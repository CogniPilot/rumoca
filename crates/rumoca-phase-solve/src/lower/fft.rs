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
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(kind) = fft_projection_kind(projection) else {
            return Ok(None);
        };
        let outputs = self.lower_fft_outputs(
            projection.base_function_name.as_str(),
            args,
            call_span,
            scope,
            call_depth,
        )?;
        match kind {
            FftOutputKind::Info => Ok(Some(outputs.info)),
            FftOutputKind::Amplitudes => {
                let Some(index) = projection.indices.first().copied() else {
                    return Ok(None);
                };
                let Some(index) = index.checked_sub(1) else {
                    return Err(fft_projection_error(
                        "FFT amplitude projection index must be one-based",
                        projection.span,
                    ));
                };
                Ok(outputs.amplitudes.get(index).copied())
            }
            FftOutputKind::Phases => {
                let Some(index) = projection.indices.first().copied() else {
                    return Ok(None);
                };
                let Some(index) = index.checked_sub(1) else {
                    return Err(fft_projection_error(
                        "FFT phase projection index must be one-based",
                        projection.span,
                    ));
                };
                Ok(outputs.phases.get(index).copied())
            }
        }
    }

    pub(super) fn lower_fft_projection_values(
        &mut self,
        projection: &FunctionOutputProjection,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        let Some(kind) = fft_projection_kind(projection) else {
            return Ok(None);
        };
        if !projection.indices.is_empty() {
            return self
                .lower_fft_projection_reg(projection, args, call_span, scope, call_depth)
                .map(|reg| reg.map(|reg| vec![reg]));
        }
        let outputs = self.lower_fft_outputs(
            projection.base_function_name.as_str(),
            args,
            call_span,
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
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<FftOutputs, LowerError> {
        let Some(samples_expr) = args.first() else {
            return Err(LowerError::InvalidFunction {
                name: function_name.to_string(),
                reason: "FFT requires a vector input".to_string(),
            }
            .with_fallback_span(call_span));
        };
        let samples = self.lower_array_like_values(samples_expr, scope, call_depth + 1)?;
        let span = fft_expr_or_call_span(samples_expr, call_span, "FFT sample argument")?;
        if samples.is_empty() || samples.len() % 2 != 0 {
            return Ok(FftOutputs {
                info: self.emit_const_at(1.0, span)?,
                amplitudes: Vec::new(),
                phases: Vec::new(),
            });
        }

        if is_raw_real_fft_name(function_name) {
            let (amplitudes, phases) = self.lower_raw_real_fft_values(&samples, false, span)?;
            return Ok(FftOutputs {
                info: self.emit_const_at(0.0, span)?,
                amplitudes,
                phases,
            });
        }

        if !is_real_fft_name(function_name) {
            return Err(LowerError::MissingFunction {
                name: function_name.to_string(),
            }
            .with_fallback_span(span));
        }

        let max_width = samples
            .len()
            .checked_div(2)
            .and_then(|half| half.checked_add(1))
            .ok_or_else(|| {
                LowerError::contract_violation(
                    "FFT frequency width overflows host index range",
                    span,
                )
            })?;
        let nfi = self
            .checked_fft_frequency_count_arg(args.get(1), max_width, span)?
            .unwrap_or(max_width);

        let mean = self.lower_mean(&samples, span)?;
        let mut centered = crate::lower_vec_with_capacity(
            samples.len(),
            "centered FFT expression sample count",
            span,
        )?;
        for sample in &samples {
            centered.push(self.emit_binary_at(BinaryOp::Sub, *sample, mean, span)?);
        }
        let (mut amplitudes, mut phases) = self.lower_raw_real_fft_values(&centered, true, span)?;
        amplitudes.truncate(nfi);
        phases.truncate(nfi);
        if let Some(first) = amplitudes.first_mut() {
            *first = mean;
        }
        if let Some(first) = phases.first_mut() {
            *first = self.emit_const_at(0.0, span)?;
        }
        self.zero_noise_phases(&amplitudes, &mut phases, span)?;
        Ok(FftOutputs {
            info: self.emit_const_at(0.0, span)?,
            amplitudes,
            phases,
        })
    }

    pub(super) fn eval_compile_time_function_call(
        &self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        if name.as_str().starts_with(NAMED_FUNCTION_ARG_PREFIX) {
            let Some(value) = args.first() else {
                return Err(unsupported_at(
                    "named structural function argument has no value",
                    span,
                ));
            };
            return self.eval_compile_time_expr(value, const_scope);
        }
        if name.last_segment() != "realFFTsamplePoints" {
            return Err(unsupported_at(
                "unsupported expression in for-loop range",
                span,
            ));
        }
        let f_max = eval_builtin_arg(self, args, 0, const_scope)?;
        let f_resolution = eval_builtin_arg(self, args, 1, const_scope)?;
        let f_max_factor = args
            .get(2)
            .map(|expr| self.eval_compile_time_expr(expr, const_scope))
            .unwrap_or_else(|| Ok(5.0))?;
        if f_resolution <= 0.0 || f_max <= f_resolution || f_max_factor < 1.0 {
            return Err(unsupported_at(
                "invalid realFFTsamplePoints structural arguments",
                span,
            ));
        }
        Ok(real_fft_sample_points(f_max, f_resolution, f_max_factor, span)? as f64)
    }

    fn checked_fft_frequency_count_arg(
        &self,
        arg: Option<&rumoca_core::Expression>,
        max_width: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Option<usize>, LowerError> {
        let Some(arg) = arg else {
            return Ok(None);
        };
        let span = fft_expr_or_call_span(arg, call_span, "FFT frequency count argument")?;
        let value = self
            .eval_compile_time_expr(arg, &self.local_const_bindings)
            .map_err(|err| err.with_fallback_span(span))?;
        Ok(Some(
            checked_fft_frequency_count(value, span)?.min(max_width),
        ))
    }

    pub(super) fn lower_mean(
        &mut self,
        values: &[Reg],
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let mut sum = self.emit_const_at(0.0, span)?;
        for value in values {
            sum = self.emit_binary_at(BinaryOp::Add, sum, *value, span)?;
        }
        let denom = self.emit_const_at(values.len() as f64, span)?;
        self.emit_binary_at(BinaryOp::Div, sum, denom, span)
    }

    pub(super) fn zero_noise_phases(
        &mut self,
        amplitudes: &[Reg],
        phases: &mut [Reg],
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        if amplitudes.is_empty() {
            return Ok(());
        }
        let mut max_amplitude = amplitudes[0];
        for amplitude in &amplitudes[1..] {
            max_amplitude = self.emit_binary_at(BinaryOp::Max, max_amplitude, *amplitude, span)?;
        }
        let eps_scale = self.emit_const_at(0.0001, span)?;
        let eps = self.emit_binary_at(BinaryOp::Mul, eps_scale, max_amplitude, span)?;
        let zero = self.emit_const_at(0.0, span)?;
        for (idx, phase) in phases.iter_mut().enumerate().skip(1) {
            let below = self.emit_compare_at(CompareOp::Lt, amplitudes[idx], eps, span)?;
            *phase = self.emit_select_at(below, zero, *phase, span)?;
        }
        Ok(())
    }

    pub(super) fn lower_raw_real_fft_values(
        &mut self,
        samples: &[Reg],
        phases_in_degrees: bool,
        span: rumoca_core::Span,
    ) -> Result<(Vec<Reg>, Vec<Reg>), LowerError> {
        let sample_count = samples.len();
        let frequency_count = sample_count / 2 + 1;
        let inv_n = 1.0 / sample_count as f64;
        let mut amplitudes =
            crate::lower_vec_with_capacity(frequency_count, "FFT amplitudes", span)?;
        let mut phases = crate::lower_vec_with_capacity(frequency_count, "FFT phases", span)?;
        for frequency_index in 0..frequency_count {
            let (re, im) = self.lower_dft_bin(samples, frequency_index, span)?;
            let re2 = self.emit_binary_at(BinaryOp::Mul, re, re, span)?;
            let im2 = self.emit_binary_at(BinaryOp::Mul, im, im, span)?;
            let magnitude2 = self.emit_binary_at(BinaryOp::Add, re2, im2, span)?;
            let magnitude = self.emit_unary_at(UnaryOp::Sqrt, magnitude2, span)?;
            let scale = if frequency_index == 0 || frequency_index == sample_count / 2 {
                inv_n
            } else {
                2.0 * inv_n
            };
            let scale = self.emit_const_at(scale, span)?;
            let amplitude = self.emit_binary_at(BinaryOp::Mul, magnitude, scale, span)?;
            let mut phase = self.emit_binary_at(BinaryOp::Atan2, im, re, span)?;
            if phases_in_degrees {
                let deg_scale = self.emit_const_at(180.0 / std::f64::consts::PI, span)?;
                phase = self.emit_binary_at(BinaryOp::Mul, phase, deg_scale, span)?;
            }
            amplitudes.push(amplitude);
            phases.push(phase);
        }
        Ok((amplitudes, phases))
    }

    fn lower_dft_bin(
        &mut self,
        samples: &[Reg],
        frequency_index: usize,
        span: rumoca_core::Span,
    ) -> Result<(Reg, Reg), LowerError> {
        let n = samples.len() as f64;
        let mut re = self.emit_const_at(0.0, span)?;
        let mut im = self.emit_const_at(0.0, span)?;
        for (sample_index, sample) in samples.iter().copied().enumerate() {
            let angle =
                -2.0 * std::f64::consts::PI * frequency_index as f64 * sample_index as f64 / n;
            let cos = self.emit_const_at(angle.cos(), span)?;
            let sin = self.emit_const_at(angle.sin(), span)?;
            let re_term = self.emit_binary_at(BinaryOp::Mul, sample, cos, span)?;
            let im_term = self.emit_binary_at(BinaryOp::Mul, sample, sin, span)?;
            re = self.emit_binary_at(BinaryOp::Add, re, re_term, span)?;
            im = self.emit_binary_at(BinaryOp::Add, im, im_term, span)?;
        }
        Ok((re, im))
    }
}

fn fft_projection_error(reason: impl Into<String>, span: rumoca_core::Span) -> LowerError {
    LowerError::contract_violation(reason, span)
}

fn fft_expr_or_call_span(
    expr: &rumoca_core::Expression,
    call_span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    expr.span()
        .or_else(|| (!call_span.is_dummy()).then_some(call_span))
        .ok_or_else(|| missing_fft_source_span(context))
}

fn missing_fft_source_span(context: &'static str) -> LowerError {
    LowerError::UnspannedContractViolation {
        reason: format!("{context} requires source span"),
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

pub(super) fn checked_fft_frequency_count(
    value: f64,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    if !value.is_finite() {
        return Err(unsupported_at(
            "realFFT frequency count is not finite",
            span,
        ));
    }
    let rounded = value.round();
    if (rounded - value).abs() > 1e-9 {
        return Err(unsupported_at(
            "realFFT frequency count must evaluate to an integer",
            span,
        ));
    }
    if rounded <= 0.0 {
        return Err(unsupported_at(
            "realFFT frequency count must be positive",
            span,
        ));
    }
    if rounded >= usize::MAX as f64 {
        return Err(LowerError::contract_violation(
            format!("realFFT frequency count {rounded} exceeds host index range"),
            span,
        ));
    }
    // Bounds and integrality are checked above; Rust has no TryFrom<f64>.
    Ok(rounded as usize)
}

fn real_fft_sample_points(
    f_max: f64,
    f_resolution: f64,
    f_max_factor: f64,
    span: rumoca_core::Span,
) -> Result<i64, LowerError> {
    let raw_count = f_max * f_max_factor / f_resolution;
    if !raw_count.is_finite() || raw_count <= 0.0 {
        return Err(unsupported_at(
            "realFFTsamplePoints computed a non-finite sample count",
            span,
        ));
    }
    let sample_pairs = raw_count.ceil();
    if sample_pairs > (i64::MAX / 2) as f64 {
        return Err(LowerError::contract_violation(
            "realFFTsamplePoints sample count exceeds i64 range",
            span,
        ));
    }
    // Bounds and integrality are checked above; Rust has no TryFrom<f64>.
    let sample_pairs = sample_pairs as i64;
    let ns1 = sample_pairs.checked_mul(2).ok_or_else(|| {
        LowerError::contract_violation("realFFTsamplePoints sample count overflows i64", span)
    })?;
    let mut ns = if ns1 % 2 == 0 {
        ns1
    } else {
        ns1.checked_add(1).ok_or_else(|| {
            LowerError::contract_violation(
                "realFFTsamplePoints even sample count overflows i64",
                span,
            )
        })?
    };
    loop {
        let mut reduced = ns;
        for factor in [2, 3, 5] {
            while reduced % factor == 0 {
                reduced /= factor;
            }
        }
        if reduced <= 1 {
            return Ok(ns);
        }
        ns = ns.checked_add(2).ok_or_else(|| {
            LowerError::contract_violation(
                "realFFTsamplePoints smooth sample search overflows i64",
                span,
            )
        })?;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unspanned_fft_test_span() -> rumoca_core::Span {
        rumoca_core::Span::DUMMY
    }

    #[test]
    fn checked_fft_frequency_count_rejects_fractional_value_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_lower_fft_source_31.mo"),
            5,
            9,
        );
        let err =
            checked_fft_frequency_count(2.5, span).expect_err("fractional FFT count must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "realFFT frequency count must evaluate to an integer"
        );
    }

    #[test]
    fn checked_fft_frequency_count_rejects_host_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("phase_solve_lower_fft_source_32.mo"),
            7,
            15,
        );
        let err = checked_fft_frequency_count(usize::MAX as f64, span)
            .expect_err("oversized FFT count must fail");

        assert_eq!(err.source_span(), Some(span));
        assert!(err.reason().contains("exceeds host index range"));
    }

    #[test]
    fn checked_fft_frequency_count_rejects_host_overflow_without_fabricating_span() {
        let err = checked_fft_frequency_count(usize::MAX as f64, unspanned_fft_test_span())
            .expect_err("oversized FFT count must fail");

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert!(err.reason().contains("exceeds host index range"));
    }

    #[test]
    fn real_fft_sample_points_rejects_overflowing_count() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fft_sample_points.mo"),
            4,
            26,
        );
        let err = real_fft_sample_points(f64::MAX, f64::MIN_POSITIVE, 5.0, span)
            .expect_err("overflowing sample count must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "realFFTsamplePoints computed a non-finite sample count"
        );
    }

    #[test]
    fn real_fft_sample_points_rejects_invalid_args_with_call_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fft_invalid_args.mo"),
            8,
            30,
        );
        let layout = rumoca_ir_solve::VarLayout::default();
        let functions = IndexMap::new();
        let builder = LowerBuilder::new(&layout, &functions);
        let args = [
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span,
            },
        ];

        let err = builder
            .eval_compile_time_function_call(
                &rumoca_core::Reference::new("realFFTsamplePoints"),
                &args,
                span,
                &IndexMap::new(),
            )
            .expect_err("invalid realFFTsamplePoints arguments must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid realFFTsamplePoints structural arguments"
        );
    }

    #[test]
    fn fft_projection_error_uses_projection_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("fft_projection.mo"),
            10,
            24,
        );
        let err = fft_projection_error("FFT phase projection index must be one-based", span);

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: FFT phase projection index must be one-based"
        );
    }

    #[test]
    fn fft_projection_error_is_unspanned_without_projection_span() {
        let err = fft_projection_error(
            "FFT phase projection index must be one-based",
            unspanned_fft_test_span(),
        );

        assert_eq!(err.source_span(), None);
        assert!(matches!(err, LowerError::UnspannedContractViolation { .. }));
        assert_eq!(
            err.reason(),
            "invalid IR contract: FFT phase projection index must be one-based"
        );
    }
}
