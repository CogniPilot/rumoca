use rumoca_eval_flat::eval::{self, build_env, eval_expr};
use rumoca_ir_dae as dae;
use rumoca_sim_core::{
    BackendState, EventSettleInput, OutputBuffers, SimError, SimOptions, SimulationBackend,
    SolverStartupProfile, StepUntilOutcome, TimeoutBudget, event_restart_step_hint,
    event_restart_time, initialize_output_capture, record_outputs_until,
    settle_runtime_event_updates_default, stop_time_reached_with_tol,
};

const RK45_STAGES: usize = 7;

struct Rk45Tableau {
    c: [f64; RK45_STAGES],
    a: [[f64; RK45_STAGES]; RK45_STAGES],
    b5: [f64; RK45_STAGES],
    b4: [f64; RK45_STAGES],
}

const RK45_TABLEAU: Rk45Tableau = Rk45Tableau {
    c: [0.0, 1.0 / 5.0, 3.0 / 10.0, 4.0 / 5.0, 8.0 / 9.0, 1.0, 1.0],
    a: [
        [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
        [1.0 / 5.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
        [3.0 / 40.0, 9.0 / 40.0, 0.0, 0.0, 0.0, 0.0, 0.0],
        [44.0 / 45.0, -56.0 / 15.0, 32.0 / 9.0, 0.0, 0.0, 0.0, 0.0],
        [
            19372.0 / 6561.0,
            -25360.0 / 2187.0,
            64448.0 / 6561.0,
            -212.0 / 729.0,
            0.0,
            0.0,
            0.0,
        ],
        [
            9017.0 / 3168.0,
            -355.0 / 33.0,
            46732.0 / 5247.0,
            49.0 / 176.0,
            -5103.0 / 18656.0,
            0.0,
            0.0,
        ],
        [
            35.0 / 384.0,
            0.0,
            500.0 / 1113.0,
            125.0 / 192.0,
            -2187.0 / 6784.0,
            11.0 / 84.0,
            0.0,
        ],
    ],
    b5: [
        35.0 / 384.0,
        0.0,
        500.0 / 1113.0,
        125.0 / 192.0,
        -2187.0 / 6784.0,
        11.0 / 84.0,
        0.0,
    ],
    b4: [
        5179.0 / 57600.0,
        0.0,
        7571.0 / 16695.0,
        393.0 / 640.0,
        -92097.0 / 339200.0,
        187.0 / 2100.0,
        1.0 / 40.0,
    ],
};

const RK45_ERROR_SCALE_FLOOR: f64 = 1.0e-12;
const RK45_ZERO_ERROR_GROWTH: f64 = 2.0;
const RK45_CONTROLLER_SAFETY: f64 = 0.9;
const RK45_CONTROLLER_EXPONENT: f64 = -0.2;
const RK45_CONTROLLER_FACTOR_MIN: f64 = 0.2;
const RK45_CONTROLLER_FACTOR_MAX: f64 = 5.0;

enum AdaptiveAttempt {
    Accepted { t_next: f64, h_next: f64 },
    Rejected { h_next: f64 },
}

struct Rk45Scratch {
    stages: [Vec<f64>; RK45_STAGES],
    y_tmp: Vec<f64>,
    y4: Vec<f64>,
    y5: Vec<f64>,
    error: Vec<f64>,
    sample: Vec<f64>,
}

impl Rk45Scratch {
    fn new(n: usize) -> Self {
        Self {
            stages: std::array::from_fn(|_| vec![0.0; n]),
            y_tmp: vec![0.0; n],
            y4: vec![0.0; n],
            y5: vec![0.0; n],
            error: vec![0.0; n],
            sample: vec![0.0; n],
        }
    }
}

struct Rk45Backend<'a> {
    dae_model: &'a dae::Dae,
    rhs_exprs: &'a [dae::Expression],
    params: &'a [f64],
    opts: &'a SimOptions,
    budget: &'a TimeoutBudget,
    n_x: usize,
    t: f64,
    y: Vec<f64>,
    h: f64,
    output_times: Vec<f64>,
    output_idx: usize,
    buf: OutputBuffers,
    scratch: Rk45Scratch,
}

impl<'a> Rk45Backend<'a> {
    fn new(
        dae_model: &'a dae::Dae,
        rhs_exprs: &'a [dae::Expression],
        params: &'a [f64],
        opts: &'a SimOptions,
        budget: &'a TimeoutBudget,
        y0: Vec<f64>,
        n_x: usize,
    ) -> Self {
        let (output_times, _output_len, buf, output_idx) =
            initialize_output_capture(opts, n_x, y0.as_slice());
        Self {
            dae_model,
            rhs_exprs,
            params,
            opts,
            budget,
            n_x,
            t: opts.t_start,
            y: y0,
            h: initial_step_hint(opts),
            output_times,
            output_idx,
            buf,
            scratch: Rk45Scratch::new(n_x),
        }
    }

    fn into_output(self) -> OutputBuffers {
        self.buf
    }

    fn direction(&self) -> f64 {
        if self.opts.t_end >= self.opts.t_start {
            1.0
        } else {
            -1.0
        }
    }

    fn min_step_abs(&self) -> f64 {
        1.0e-14 * (1.0 + self.t.abs().max(self.opts.t_end.abs()))
    }

    fn clamp_step_to_stop(&self, stop_time: f64, step: f64) -> f64 {
        let remaining = stop_time - self.t;
        if remaining.abs() < step.abs() {
            remaining
        } else {
            step
        }
    }

    fn normalized_next_step(&self, candidate_abs: f64) -> f64 {
        let abs_value = candidate_abs.max(self.min_step_abs());
        self.direction() * abs_value
    }

    fn evaluate_rhs(
        dae_model: &dae::Dae,
        rhs_exprs: &[dae::Expression],
        params: &[f64],
        t_eval: f64,
        y_eval: &[f64],
        out: &mut [f64],
    ) -> Result<(), SimError> {
        let env = build_env(dae_model, y_eval, params, t_eval);
        for (idx, rhs_expr) in rhs_exprs.iter().enumerate() {
            let value = eval_expr::<f64>(rhs_expr, &env);
            if !value.is_finite() {
                return Err(SimError::SolverError(format!(
                    "non-finite derivative at row {idx} and t={t_eval}"
                )));
            }
            out[idx] = value;
        }
        Ok(())
    }

    fn state_from_coeffs(
        base: &[f64],
        h: f64,
        coeffs: &[f64; RK45_STAGES],
        stages: &[Vec<f64>; RK45_STAGES],
        out: &mut [f64],
    ) {
        for (idx, out_value) in out.iter_mut().enumerate() {
            let mut value = base[idx];
            for (coef, stage) in coeffs.iter().zip(stages.iter()) {
                value += h * (*coef) * stage[idx];
            }
            *out_value = value;
        }
    }

    fn embedded_step(&mut self, h: f64) -> Result<(), SimError> {
        let t0 = self.t;
        let y0 = self.y.as_slice();
        let dae_model = self.dae_model;
        let rhs_exprs = self.rhs_exprs;
        let params = self.params;

        Self::evaluate_rhs(
            dae_model,
            rhs_exprs,
            params,
            t0,
            y0,
            &mut self.scratch.stages[0],
        )?;
        for stage_idx in 1..RK45_STAGES {
            Self::state_from_coeffs(
                y0,
                h,
                &RK45_TABLEAU.a[stage_idx],
                &self.scratch.stages,
                &mut self.scratch.y_tmp,
            );
            Self::evaluate_rhs(
                dae_model,
                rhs_exprs,
                params,
                t0 + h * RK45_TABLEAU.c[stage_idx],
                self.scratch.y_tmp.as_slice(),
                &mut self.scratch.stages[stage_idx],
            )?;
        }

        Self::state_from_coeffs(
            y0,
            h,
            &RK45_TABLEAU.b5,
            &self.scratch.stages,
            &mut self.scratch.y5,
        );
        Self::state_from_coeffs(
            y0,
            h,
            &RK45_TABLEAU.b4,
            &self.scratch.stages,
            &mut self.scratch.y4,
        );

        for idx in 0..self.scratch.error.len() {
            self.scratch.error[idx] = self.scratch.y5[idx] - self.scratch.y4[idx];
        }

        Ok(())
    }

    fn error_norm(&self, y0: &[f64], y1: &[f64], error: &[f64]) -> f64 {
        let mut accum = 0.0;
        for idx in 0..error.len() {
            let scale = self.opts.atol + self.opts.rtol * y0[idx].abs().max(y1[idx].abs());
            let normalized = error[idx] / scale.max(RK45_ERROR_SCALE_FLOOR);
            accum += normalized * normalized;
        }
        (accum / (error.len() as f64).max(1.0)).sqrt()
    }

    fn next_step_abs(current_abs: f64, error_norm: f64) -> f64 {
        if !error_norm.is_finite() || error_norm <= 0.0 {
            return current_abs * RK45_ZERO_ERROR_GROWTH;
        }
        let factor = (RK45_CONTROLLER_SAFETY * error_norm.powf(RK45_CONTROLLER_EXPONENT))
            .clamp(RK45_CONTROLLER_FACTOR_MIN, RK45_CONTROLLER_FACTOR_MAX);
        current_abs * factor
    }

    fn try_adaptive_step(&mut self, step: f64) -> Result<AdaptiveAttempt, SimError> {
        let step_abs = step.abs();
        self.embedded_step(step)?;
        let err_norm = self.error_norm(
            self.y.as_slice(),
            self.scratch.y5.as_slice(),
            self.scratch.error.as_slice(),
        );
        let h_next = self.normalized_next_step(Self::next_step_abs(step_abs, err_norm));

        if err_norm <= 1.0 {
            let t_next = self.t + step;
            return Ok(AdaptiveAttempt::Accepted { t_next, h_next });
        }

        if h_next.abs() <= self.min_step_abs() {
            return Err(SimError::SolverError(format!(
                "RK45 step underflow near t={} (requested stop at {})",
                self.t, self.opts.t_end
            )));
        }
        Ok(AdaptiveAttempt::Rejected { h_next })
    }

    fn record_segment(&mut self, t0: f64, y0: &[f64]) -> Result<(), SimError> {
        let t1 = self.t;
        let y1 = self.y.as_slice();
        let sample = &mut self.scratch.sample;
        debug_assert_eq!(y0.len(), sample.len());
        debug_assert_eq!(y1.len(), sample.len());
        record_outputs_until(
            self.output_times.as_slice(),
            &mut self.output_idx,
            t1,
            &mut self.buf,
            |t_sample, out| {
                let alpha = if (t1 - t0).abs() <= 1.0e-14 {
                    1.0
                } else {
                    ((t_sample - t0) / (t1 - t0)).clamp(0.0, 1.0)
                };
                for idx in 0..sample.len() {
                    sample[idx] = y0[idx] + alpha * (y1[idx] - y0[idx]);
                }
                out.record(t_sample, sample.as_slice());
                Ok(())
            },
        )
    }

    fn apply_accepted_step(
        &mut self,
        stop_time: f64,
        t_next: f64,
        h_next: f64,
    ) -> Result<StepUntilOutcome, SimError> {
        let t_prev = self.t;
        let y_prev = std::mem::take(&mut self.y);
        std::mem::swap(&mut self.y, &mut self.scratch.y5);
        self.t = t_next;
        self.h = h_next;
        self.record_segment(t_prev, y_prev.as_slice())?;
        self.scratch.y5 = y_prev;
        if stop_time_reached_with_tol(self.t, stop_time) {
            return Ok(StepUntilOutcome::StopReached);
        }
        Ok(StepUntilOutcome::InternalStep)
    }
}

impl SimulationBackend for Rk45Backend<'_> {
    type Error = SimError;

    fn init(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn step_until(&mut self, stop_time: f64) -> Result<StepUntilOutcome, Self::Error> {
        if stop_time_reached_with_tol(self.t, self.opts.t_end) {
            return Ok(StepUntilOutcome::Finished);
        }
        if stop_time_reached_with_tol(self.t, stop_time) {
            return Ok(StepUntilOutcome::StopReached);
        }

        let mut trial_step = self.clamp_step_to_stop(stop_time, self.h);
        if trial_step.abs() < self.min_step_abs() {
            trial_step = self.normalized_next_step(self.min_step_abs());
        }

        loop {
            self.budget.check()?;
            match self.try_adaptive_step(trial_step)? {
                AdaptiveAttempt::Accepted { t_next, h_next } => {
                    return self.apply_accepted_step(stop_time, t_next, h_next);
                }
                AdaptiveAttempt::Rejected { h_next } => {
                    trial_step = self.clamp_step_to_stop(stop_time, h_next);
                }
            }
        }
    }

    fn read_state(&self) -> BackendState {
        BackendState { t: self.t }
    }

    fn apply_event_updates(&mut self, event_time: f64) -> Result<(), Self::Error> {
        let event_env = settle_runtime_event_updates_default(EventSettleInput {
            dae: self.dae_model,
            y: self.y.as_mut_slice(),
            p: self.params,
            n_x: self.n_x,
            t_eval: event_time,
        });
        eval::seed_pre_values_from_env(&event_env);

        self.t = event_restart_time(self.opts.t_start, self.opts.t_end, event_time);
        if let Some(step_hint) =
            event_restart_step_hint(self.opts, self.t, SolverStartupProfile::Default)
        {
            self.h = self.normalized_next_step(step_hint);
        }
        Ok(())
    }
}

fn initial_step_hint(opts: &SimOptions) -> f64 {
    let span = (opts.t_end - opts.t_start).abs();
    if !span.is_finite() || span <= 0.0 {
        return 1.0e-6;
    }
    let mut hint = (span / 500.0).max(1.0e-6);
    if let Some(dt) = opts.dt
        && dt.is_finite()
        && dt > 0.0
    {
        hint = hint.min(dt.abs());
    }
    hint
}

pub fn integrate_with_rk45(
    dae_model: &dae::Dae,
    rhs_exprs: &[dae::Expression],
    params: &[f64],
    opts: &SimOptions,
    budget: &TimeoutBudget,
    y0: Vec<f64>,
    n_x: usize,
) -> Result<OutputBuffers, SimError> {
    let mut backend = Rk45Backend::new(dae_model, rhs_exprs, params, opts, budget, y0, n_x);
    rumoca_sim_core::run_with_runtime_schedule(
        &mut backend,
        dae_model,
        opts.t_start,
        opts.t_end,
        || budget.check().map_err(SimError::from),
    )?;
    Ok(backend.into_output())
}
