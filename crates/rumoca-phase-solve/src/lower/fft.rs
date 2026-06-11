use super::function_projection::FunctionOutputProjection;
use super::*;

enum FftOutputKind {
    Info,
    Amplitudes,
    Phases,
}

struct FftOutputs {
    info: Reg,
    amplitudes: Vec<Reg>,
    phases: Vec<Reg>,
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_fft_projection_reg(
        &mut self,
        projection: &FunctionOutputProjection,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(kind) = fft_projection_kind(projection) else {
            return Ok(None);
        };
        let outputs = self.lower_fft_outputs(
            projection.base_function_name.as_str(),
            args,
            scope,
            call_depth,
        )?;
        match kind {
            FftOutputKind::Info => Ok(Some(outputs.info)),
            FftOutputKind::Amplitudes => {
                let Some(index) = projection.indices.first().copied() else {
                    return Ok(None);
                };
                Ok(outputs.amplitudes.get(index.saturating_sub(1)).copied())
            }
            FftOutputKind::Phases => {
                let Some(index) = projection.indices.first().copied() else {
                    return Ok(None);
                };
                Ok(outputs.phases.get(index.saturating_sub(1)).copied())
            }
        }
    }

    pub(super) fn lower_fft_projection_values(
        &mut self,
        projection: &FunctionOutputProjection,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let Some(kind) = fft_projection_kind(projection) else {
            return Ok(None);
        };
        if !projection.indices.is_empty() {
            return self
                .lower_fft_projection_reg(projection, args, scope, call_depth)
                .map(|reg| reg.map(|reg| vec![reg]));
        }
        let outputs = self.lower_fft_outputs(
            projection.base_function_name.as_str(),
            args,
            scope,
            call_depth,
        )?;
        let values = match kind {
            FftOutputKind::Info => vec![outputs.info],
            FftOutputKind::Amplitudes => outputs.amplitudes,
            FftOutputKind::Phases => outputs.phases,
        };
        Ok(Some(values))
    }

    fn lower_fft_outputs(
        &mut self,
        function_name: &str,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<FftOutputs, LowerError> {
        let Some(samples_expr) = args.first() else {
            return Err(LowerError::InvalidFunction {
                name: function_name.to_string(),
                reason: "FFT requires a vector input".to_string(),
            });
        };
        let samples = self.lower_array_like_values(samples_expr, scope, call_depth + 1)?;
        if samples.is_empty() || samples.len() % 2 != 0 {
            return Ok(FftOutputs {
                info: self.emit_const(1.0),
                amplitudes: Vec::new(),
                phases: Vec::new(),
            });
        }

        if is_raw_real_fft_name(function_name) {
            let (amplitudes, phases) = self.lower_raw_real_fft_values(&samples, false);
            return Ok(FftOutputs {
                info: self.emit_const(0.0),
                amplitudes,
                phases,
            });
        }

        if !is_real_fft_name(function_name) {
            return Err(LowerError::MissingFunction {
                name: function_name.to_string(),
            });
        }

        let max_width = samples.len() / 2 + 1;
        let nfi = args
            .get(1)
            .and_then(|arg| {
                self.eval_compile_time_expr(arg, &self.local_const_bindings)
                    .ok()
            })
            .map(|value| value.max(1.0) as usize)
            .unwrap_or(max_width)
            .min(max_width);

        let mean = self.lower_mean(&samples);
        let centered = samples
            .iter()
            .map(|sample| self.emit_binary(BinaryOp::Sub, *sample, mean))
            .collect::<Vec<_>>();
        let (mut amplitudes, mut phases) = self.lower_raw_real_fft_values(&centered, true);
        amplitudes.truncate(nfi);
        phases.truncate(nfi);
        if let Some(first) = amplitudes.first_mut() {
            *first = mean;
        }
        if let Some(first) = phases.first_mut() {
            *first = self.emit_const(0.0);
        }
        self.zero_noise_phases(&amplitudes, &mut phases);
        Ok(FftOutputs {
            info: self.emit_const(0.0),
            amplitudes,
            phases,
        })
    }

    pub(super) fn eval_compile_time_function_call(
        &self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        if name.as_str().starts_with(NAMED_FUNCTION_ARG_PREFIX) {
            let Some(value) = args.first() else {
                return Err(LowerError::Unsupported {
                    reason: "named structural function argument has no value".to_string(),
                });
            };
            return self.eval_compile_time_expr(value, const_scope);
        }
        if name.last_segment() != "realFFTsamplePoints" {
            return Err(LowerError::Unsupported {
                reason: "unsupported expression in for-loop range".to_string(),
            });
        }
        let f_max = eval_builtin_arg(self, args, 0, const_scope)?;
        let f_resolution = eval_builtin_arg(self, args, 1, const_scope)?;
        let f_max_factor = args
            .get(2)
            .map(|expr| self.eval_compile_time_expr(expr, const_scope))
            .unwrap_or_else(|| Ok(5.0))?;
        if f_resolution <= 0.0 || f_max <= f_resolution || f_max_factor < 1.0 {
            return Err(LowerError::Unsupported {
                reason: "invalid realFFTsamplePoints structural arguments".to_string(),
            });
        }
        Ok(real_fft_sample_points(f_max, f_resolution, f_max_factor) as f64)
    }

    pub(super) fn lower_mean(&mut self, values: &[Reg]) -> Reg {
        let mut sum = self.emit_const(0.0);
        for value in values {
            sum = self.emit_binary(BinaryOp::Add, sum, *value);
        }
        let denom = self.emit_const(values.len() as f64);
        self.emit_binary(BinaryOp::Div, sum, denom)
    }

    pub(super) fn zero_noise_phases(&mut self, amplitudes: &[Reg], phases: &mut [Reg]) {
        if amplitudes.is_empty() {
            return;
        }
        let mut max_amplitude = amplitudes[0];
        for amplitude in &amplitudes[1..] {
            max_amplitude = self.emit_binary(BinaryOp::Max, max_amplitude, *amplitude);
        }
        let eps_scale = self.emit_const(0.0001);
        let eps = self.emit_binary(BinaryOp::Mul, eps_scale, max_amplitude);
        let zero = self.emit_const(0.0);
        for (idx, phase) in phases.iter_mut().enumerate().skip(1) {
            let below = self.emit_compare(CompareOp::Lt, amplitudes[idx], eps);
            *phase = self.emit_select(below, zero, *phase);
        }
    }

    pub(super) fn lower_raw_real_fft_values(
        &mut self,
        samples: &[Reg],
        phases_in_degrees: bool,
    ) -> (Vec<Reg>, Vec<Reg>) {
        let sample_count = samples.len();
        let frequency_count = sample_count / 2 + 1;
        let inv_n = 1.0 / sample_count as f64;
        let mut amplitudes = Vec::with_capacity(frequency_count);
        let mut phases = Vec::with_capacity(frequency_count);
        for frequency_index in 0..frequency_count {
            let (re, im) = self.lower_dft_bin(samples, frequency_index);
            let re2 = self.emit_binary(BinaryOp::Mul, re, re);
            let im2 = self.emit_binary(BinaryOp::Mul, im, im);
            let magnitude2 = self.emit_binary(BinaryOp::Add, re2, im2);
            let magnitude = self.emit_unary(UnaryOp::Sqrt, magnitude2);
            let scale = if frequency_index == 0 || frequency_index == sample_count / 2 {
                inv_n
            } else {
                2.0 * inv_n
            };
            let scale = self.emit_const(scale);
            let amplitude = self.emit_binary(BinaryOp::Mul, magnitude, scale);
            let mut phase = self.emit_binary(BinaryOp::Atan2, im, re);
            if phases_in_degrees {
                let deg_scale = self.emit_const(180.0 / std::f64::consts::PI);
                phase = self.emit_binary(BinaryOp::Mul, phase, deg_scale);
            }
            amplitudes.push(amplitude);
            phases.push(phase);
        }
        (amplitudes, phases)
    }

    fn lower_dft_bin(&mut self, samples: &[Reg], frequency_index: usize) -> (Reg, Reg) {
        let n = samples.len() as f64;
        let mut re = self.emit_const(0.0);
        let mut im = self.emit_const(0.0);
        for (sample_index, sample) in samples.iter().copied().enumerate() {
            let angle =
                -2.0 * std::f64::consts::PI * frequency_index as f64 * sample_index as f64 / n;
            let cos = self.emit_const(angle.cos());
            let sin = self.emit_const(angle.sin());
            let re_term = self.emit_binary(BinaryOp::Mul, sample, cos);
            let im_term = self.emit_binary(BinaryOp::Mul, sample, sin);
            re = self.emit_binary(BinaryOp::Add, re, re_term);
            im = self.emit_binary(BinaryOp::Add, im, im_term);
        }
        (re, im)
    }
}

fn fft_projection_kind(projection: &FunctionOutputProjection) -> Option<FftOutputKind> {
    if projection.output_field.is_some() {
        return None;
    }
    let function_name = projection.base_function_name.as_str();
    if is_real_fft_name(function_name) {
        return match projection.output_name.as_str() {
            "info" => Some(FftOutputKind::Info),
            "amplitudes" => Some(FftOutputKind::Amplitudes),
            "phases" => Some(FftOutputKind::Phases),
            _ => None,
        };
    }
    if is_raw_real_fft_name(function_name) {
        return match projection.output_name.as_str() {
            "info" => Some(FftOutputKind::Info),
            "amplitudes" | "A" => Some(FftOutputKind::Amplitudes),
            "phases" | "Phi" => Some(FftOutputKind::Phases),
            _ => None,
        };
    }
    None
}

fn is_real_fft_name(name: &str) -> bool {
    crate::path_utils::leaf_segment(name) == "realFFT"
}

fn is_raw_real_fft_name(name: &str) -> bool {
    crate::path_utils::leaf_segment(name) == "rawRealFFT"
}

fn real_fft_sample_points(f_max: f64, f_resolution: f64, f_max_factor: f64) -> i64 {
    let ns1 = 2 * (f_max * f_max_factor / f_resolution).ceil() as i64;
    let mut ns = if ns1 % 2 == 0 { ns1 } else { ns1 + 1 };
    loop {
        let mut reduced = ns;
        for factor in [2, 3, 5] {
            while reduced % factor == 0 {
                reduced /= factor;
            }
        }
        if reduced <= 1 {
            return ns;
        }
        ns += 2;
    }
}
