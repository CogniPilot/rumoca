use rumoca_eval_dae::runtime as eval;
use rumoca_eval_dae::runtime::{VarEnv, eval_array_values, eval_expr};
use rumoca_ir_dae as dae;
use rumoca_sim::equation_scalarize::{build_output_names, scalarize_equations};
use rumoca_sim::simulation::dae_prepare::{expr_contains_der_of, normalize_ode_equation_signs};
use rumoca_sim::simulation::runtime_prep::derivative_coefficient_expr;
use rumoca_sim::timeline;
use rumoca_sim::{
    BackendState, SimOptions, SimResult, SimSolverMode, SimulationBackend, StepUntilOutcome,
    TimeoutBudget, TimeoutExceeded, build_variable_meta,
};

type Dae = dae::Dae;
type Expression = dae::Expression;
type VarName = dae::VarName;
type Variable = dae::Variable;

const MIN_STEP: f64 = 1.0e-12;

#[derive(Debug, thiserror::Error)]
pub enum SimError {
    #[error("empty system: no state equations to simulate")]
    EmptySystem,

    #[error("rk45 backend does not support solver mode {requested:?}")]
    UnsupportedSolverMode { requested: SimSolverMode },

    #[error("rk45 backend only supports a narrow explicit ODE subset: {reason}")]
    UnsupportedModel { reason: String },

    #[error("missing explicit ODE row for state '{state_name}'")]
    MissingStateEquation { state_name: String },

    #[error(
        "state '{state_name}' is not an explicit single-derivative ODE row (origin '{origin}'): {reason}"
    )]
    NonExplicitStateEquation {
        state_name: String,
        origin: String,
        reason: String,
    },

    #[error("non-finite derivative evaluation for state '{state_name}'")]
    NonFiniteDerivative { state_name: String },

    #[error("derivative coefficient collapsed to zero for state '{state_name}'")]
    ZeroDerivativeCoefficient { state_name: String },

    #[error("step size underflow while advancing toward t={target_t}")]
    StepSizeUnderflow { target_t: f64 },

    #[error("timeout after {seconds:.3}s")]
    Timeout { seconds: f64 },
}

impl From<TimeoutExceeded> for SimError {
    fn from(value: TimeoutExceeded) -> Self {
        Self::Timeout {
            seconds: value.seconds,
        }
    }
}

#[derive(Clone)]
struct StateRowPlan {
    state_name: VarName,
    equation_index: usize,
    coeff_expr: Expression,
}

struct ExplicitModel {
    dae: Dae,
    state_rows: Vec<StateRowPlan>,
    params: Vec<f64>,
}

struct Rk45Backend<'a> {
    model: &'a ExplicitModel,
    time: f64,
    state: Vec<f64>,
    atol: f64,
    rtol: f64,
    next_step: f64,
    budget: TimeoutBudget,
}

struct TrialStep {
    y_next: Vec<f64>,
    error_norm: f64,
}

pub fn simulate_dae(dae_model: &Dae, opts: &SimOptions) -> Result<SimResult, SimError> {
    match opts.solver_mode {
        SimSolverMode::Auto | SimSolverMode::RkLike => {}
        requested => return Err(SimError::UnsupportedSolverMode { requested }),
    }

    let model = ExplicitModel::new(dae_model, opts.scalarize)?;
    let names = build_output_names(&model.dae);
    let sample_dt = default_output_dt(opts);
    let sample_times = timeline::build_output_times(opts.t_start, opts.t_end, sample_dt);
    let mut backend = Rk45Backend::new(&model, opts)?;
    let mut data = vec![Vec::with_capacity(sample_times.len()); names.len()];
    record_state_sample(&mut data, &backend.state);

    for &target_t in sample_times.iter().skip(1) {
        advance_backend_to(&mut backend, target_t)?;
        record_state_sample(&mut data, &backend.state);
    }

    Ok(SimResult {
        times: sample_times,
        names: names.clone(),
        data,
        n_states: names.len(),
        variable_meta: build_variable_meta(&model.dae, &names, names.len()),
    })
}

pub use simulate_dae as simulate;

impl ExplicitModel {
    fn new(source: &Dae, scalarize: bool) -> Result<Self, SimError> {
        let dae = prepare_explicit_dae(source, scalarize)?;
        let state_rows = build_state_row_plans(&dae)?;
        let params = build_parameter_values(&dae);
        Ok(Self {
            dae,
            state_rows,
            params,
        })
    }

    fn eval_state_derivatives(&self, t: f64, state: &[f64]) -> Result<Vec<f64>, SimError> {
        let env = eval::build_env(&self.dae, state, &self.params, t);
        let mut derivatives = Vec::with_capacity(self.state_rows.len());
        for row in &self.state_rows {
            let eq = &self.dae.f_x[row.equation_index];
            let residual = eval_expr::<f64>(&eq.rhs, &env);
            let coeff = eval_expr::<f64>(&row.coeff_expr, &env);
            if !residual.is_finite() {
                return Err(SimError::NonFiniteDerivative {
                    state_name: row.state_name.to_string(),
                });
            }
            if !coeff.is_finite() || coeff.abs() <= 1.0e-12 {
                return Err(SimError::ZeroDerivativeCoefficient {
                    state_name: row.state_name.to_string(),
                });
            }
            let value = -residual / coeff;
            if !value.is_finite() {
                return Err(SimError::NonFiniteDerivative {
                    state_name: row.state_name.to_string(),
                });
            }
            derivatives.push(value);
        }
        Ok(derivatives)
    }
}

impl<'a> Rk45Backend<'a> {
    fn new(model: &'a ExplicitModel, opts: &SimOptions) -> Result<Self, SimError> {
        let state = build_state_start_values(&model.dae);
        let next_step = default_step_size(opts);
        if !next_step.is_finite() || next_step <= 0.0 {
            return Err(SimError::StepSizeUnderflow {
                target_t: opts.t_end,
            });
        }
        Ok(Self {
            model,
            time: opts.t_start,
            state,
            atol: opts.atol.max(1.0e-12),
            rtol: opts.rtol.max(1.0e-12),
            next_step,
            budget: TimeoutBudget::new(opts.max_wall_seconds),
        })
    }

    fn trial_step(&self, h: f64) -> Result<TrialStep, SimError> {
        let k1 = self.model.eval_state_derivatives(self.time, &self.state)?;
        let y2 = combine_stage(&self.state, h, &[(&k1, 1.0 / 5.0)]);
        let k2 = self
            .model
            .eval_state_derivatives(self.time + h * (1.0 / 5.0), &y2)?;

        let y3 = combine_stage(&self.state, h, &[(&k1, 3.0 / 40.0), (&k2, 9.0 / 40.0)]);
        let k3 = self
            .model
            .eval_state_derivatives(self.time + h * (3.0 / 10.0), &y3)?;

        let y4 = combine_stage(
            &self.state,
            h,
            &[(&k1, 44.0 / 45.0), (&k2, -56.0 / 15.0), (&k3, 32.0 / 9.0)],
        );
        let k4 = self
            .model
            .eval_state_derivatives(self.time + h * (4.0 / 5.0), &y4)?;

        let y5 = combine_stage(
            &self.state,
            h,
            &[
                (&k1, 19372.0 / 6561.0),
                (&k2, -25360.0 / 2187.0),
                (&k3, 64448.0 / 6561.0),
                (&k4, -212.0 / 729.0),
            ],
        );
        let k5 = self
            .model
            .eval_state_derivatives(self.time + h * (8.0 / 9.0), &y5)?;

        let y6 = combine_stage(
            &self.state,
            h,
            &[
                (&k1, 9017.0 / 3168.0),
                (&k2, -355.0 / 33.0),
                (&k3, 46732.0 / 5247.0),
                (&k4, 49.0 / 176.0),
                (&k5, -5103.0 / 18656.0),
            ],
        );
        let k6 = self.model.eval_state_derivatives(self.time + h, &y6)?;

        let y_next = combine_stage(
            &self.state,
            h,
            &[
                (&k1, 35.0 / 384.0),
                (&k3, 500.0 / 1113.0),
                (&k4, 125.0 / 192.0),
                (&k5, -2187.0 / 6784.0),
                (&k6, 11.0 / 84.0),
            ],
        );
        let k7 = self.model.eval_state_derivatives(self.time + h, &y_next)?;
        let y_fourth = combine_stage(
            &self.state,
            h,
            &[
                (&k1, 5179.0 / 57600.0),
                (&k3, 7571.0 / 16695.0),
                (&k4, 393.0 / 640.0),
                (&k5, -92097.0 / 339200.0),
                (&k6, 187.0 / 2100.0),
                (&k7, 1.0 / 40.0),
            ],
        );

        Ok(TrialStep {
            error_norm: normalized_error(&self.state, &y_next, &y_fourth, self.atol, self.rtol),
            y_next,
        })
    }

    fn accepted_step_size(&self, error_norm: f64, h: f64) -> f64 {
        if error_norm <= f64::EPSILON {
            return (h * 5.0).max(MIN_STEP);
        }
        let factor = (0.9 * error_norm.powf(-0.2)).clamp(0.2, 5.0);
        (h * factor).max(MIN_STEP)
    }

    fn commit_step(
        &mut self,
        y_next: Vec<f64>,
        h: f64,
        next_h: f64,
        stop_time: f64,
    ) -> StepUntilOutcome {
        self.state = y_next;
        self.time += h;
        self.next_step = next_h;
        let reached_stop =
            timeline::sample_time_match_with_tol(self.time, stop_time) || self.time >= stop_time;
        if !reached_stop {
            return StepUntilOutcome::InternalStep;
        }
        self.time = stop_time;
        StepUntilOutcome::StopReached
    }
}

impl SimulationBackend for Rk45Backend<'_> {
    type Error = SimError;

    fn init(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn step_until(&mut self, stop_time: f64) -> Result<StepUntilOutcome, Self::Error> {
        self.budget.check()?;
        let remaining = stop_time - self.time;
        if timeline::sample_time_match_with_tol(self.time, stop_time) || remaining <= 0.0 {
            self.time = stop_time;
            return Ok(StepUntilOutcome::StopReached);
        }

        let mut h = self.next_step.min(remaining);
        loop {
            self.budget.check()?;
            if !h.is_finite() || h <= MIN_STEP {
                return Err(SimError::StepSizeUnderflow {
                    target_t: stop_time,
                });
            }

            let trial = self.trial_step(h)?;
            let next_h = self.accepted_step_size(trial.error_norm, h);
            if trial.error_norm > 1.0 {
                h = next_h.min(remaining);
                continue;
            }
            return Ok(self.commit_step(trial.y_next, h, next_h, stop_time));
        }
    }

    fn read_state(&self) -> BackendState {
        BackendState { t: self.time }
    }

    fn apply_event_updates(&mut self, event_time: f64) -> Result<(), Self::Error> {
        self.time = event_time;
        Ok(())
    }
}

fn prepare_explicit_dae(source: &Dae, scalarize: bool) -> Result<Dae, SimError> {
    if source.states.is_empty() {
        return Err(SimError::EmptySystem);
    }

    let mut dae = source.clone();
    if scalarize {
        scalarize_equations(&mut dae);
    }
    normalize_ode_equation_signs(&mut dae);

    reject_if_present(dae.inputs.is_empty(), "input variables")?;
    reject_if_present(dae.algebraics.is_empty(), "continuous algebraics")?;
    reject_if_present(dae.outputs.is_empty(), "continuous outputs")?;
    reject_if_present(dae.discrete_reals.is_empty(), "discrete Real variables")?;
    reject_if_present(dae.discrete_valued.is_empty(), "discrete-valued variables")?;
    reject_if_present(dae.derivative_aliases.is_empty(), "derivative aliases")?;

    if !dae.f_z.is_empty()
        || !dae.f_m.is_empty()
        || !dae.f_c.is_empty()
        || !dae.relation.is_empty()
        || !dae.synthetic_root_conditions.is_empty()
        || !dae.scheduled_time_events.is_empty()
        || !dae.clock_schedules.is_empty()
        || !dae.clock_constructor_exprs.is_empty()
        || !dae.triggered_clock_conditions.is_empty()
        || !dae.initial_equations.is_empty()
    {
        return Err(SimError::UnsupportedModel {
            reason: "events, clocks, and initial-equation solving are not yet supported"
                .to_string(),
        });
    }

    if dae.states.values().any(|var| var.size() != 1) {
        return Err(SimError::UnsupportedModel {
            reason: "non-scalar states require scalarization before rk45 simulation".to_string(),
        });
    }
    if dae.f_x.len() != dae.states.len() {
        return Err(SimError::UnsupportedModel {
            reason: "explicit rk45 currently requires exactly one residual row per state"
                .to_string(),
        });
    }

    Ok(dae)
}

fn reject_if_present(is_empty: bool, label: &str) -> Result<(), SimError> {
    if is_empty {
        Ok(())
    } else {
        Err(SimError::UnsupportedModel {
            reason: format!("{label} are not yet supported"),
        })
    }
}

fn build_state_row_plans(dae: &Dae) -> Result<Vec<StateRowPlan>, SimError> {
    let state_names: Vec<VarName> = dae.states.keys().cloned().collect();
    state_names
        .iter()
        .map(|state_name| build_state_row_plan(dae, &state_names, state_name))
        .collect()
}

fn build_state_row_plan(
    dae: &Dae,
    state_names: &[VarName],
    state_name: &VarName,
) -> Result<StateRowPlan, SimError> {
    let matches: Vec<usize> = dae
        .f_x
        .iter()
        .enumerate()
        .filter_map(|(idx, eq)| expr_contains_der_of(&eq.rhs, state_name).then_some(idx))
        .collect();

    if matches.len() != 1 {
        return Err(SimError::MissingStateEquation {
            state_name: state_name.to_string(),
        });
    }
    let equation_index = matches[0];
    let eq = &dae.f_x[equation_index];
    for other_state in state_names {
        if other_state == state_name {
            continue;
        }
        if expr_contains_der_of(&eq.rhs, other_state) {
            return Err(SimError::NonExplicitStateEquation {
                state_name: state_name.to_string(),
                origin: eq.origin.clone(),
                reason: format!("row also depends on der({})", other_state.as_str()),
            });
        }
    }

    let coeff_expr = derivative_coefficient_expr(&eq.rhs, state_name).map_err(|reason| {
        SimError::NonExplicitStateEquation {
            state_name: state_name.to_string(),
            origin: eq.origin.clone(),
            reason,
        }
    })?;
    let Some(coeff_expr) = coeff_expr else {
        return Err(SimError::NonExplicitStateEquation {
            state_name: state_name.to_string(),
            origin: eq.origin.clone(),
            reason: "could not isolate derivative coefficient".to_string(),
        });
    };

    Ok(StateRowPlan {
        state_name: state_name.clone(),
        equation_index,
        coeff_expr,
    })
}

fn default_output_dt(opts: &SimOptions) -> f64 {
    opts.dt.unwrap_or_else(|| {
        let span = (opts.t_end - opts.t_start).abs();
        if span <= f64::EPSILON {
            0.0
        } else {
            span / 500.0
        }
    })
}

fn default_step_size(opts: &SimOptions) -> f64 {
    let span = (opts.t_end - opts.t_start).abs();
    if span <= f64::EPSILON {
        return 1.0e-3;
    }
    let sample_dt = default_output_dt(opts);
    let reference = if sample_dt.is_finite() && sample_dt > 0.0 {
        sample_dt
    } else {
        span / 100.0
    };
    (reference / 10.0).max(1.0e-6)
}

fn advance_backend_to(backend: &mut Rk45Backend<'_>, target_t: f64) -> Result<(), SimError> {
    while backend.time < target_t && !timeline::sample_time_match_with_tol(backend.time, target_t) {
        match backend.step_until(target_t)? {
            StepUntilOutcome::InternalStep | StepUntilOutcome::StopReached => {}
            StepUntilOutcome::Finished | StepUntilOutcome::RootFound { .. } => break,
        }
    }
    Ok(())
}

fn record_state_sample(data: &mut [Vec<f64>], state: &[f64]) {
    for (series, value) in data.iter_mut().zip(state.iter().copied()) {
        series.push(value);
    }
}

fn combine_stage(base: &[f64], h: f64, terms: &[(&[f64], f64)]) -> Vec<f64> {
    let mut out = base.to_vec();
    for (idx, value) in out.iter_mut().enumerate() {
        for (k, coeff) in terms {
            *value += h * coeff * k[idx];
        }
    }
    out
}

fn normalized_error(current: &[f64], fifth: &[f64], fourth: &[f64], atol: f64, rtol: f64) -> f64 {
    current
        .iter()
        .zip(fifth.iter())
        .zip(fourth.iter())
        .fold(0.0, |max_norm, ((y0, y5), y4)| {
            let scale = atol + rtol * y0.abs().max(y5.abs());
            let norm = (y5 - y4).abs() / scale.max(1.0e-15);
            max_norm.max(norm)
        })
}

fn init_eval_env(dae: &Dae) -> VarEnv<f64> {
    let mut env = VarEnv::<f64>::new();
    if !dae.functions.is_empty() {
        env.functions = std::sync::Arc::new(eval::collect_user_functions(dae));
    }
    env.dims = std::sync::Arc::new(eval::collect_var_dims(dae));
    env.start_exprs = std::sync::Arc::new(eval::collect_var_starts(dae));
    env.enum_literal_ordinals = std::sync::Arc::new(dae.enum_literal_ordinals.clone());
    for &(fqn, value) in eval::MODELICA_CONSTANTS {
        env.set(fqn, value);
    }
    env
}

fn evaluate_constants_in_place(dae: &Dae, env: &mut VarEnv<f64>) {
    for _ in 0..2 {
        for (name, var) in &dae.constants {
            if let Some(start) = &var.start {
                env.set(name.as_str(), eval_expr::<f64>(start, env));
            }
        }
    }
}

fn build_parameter_env(dae: &Dae) -> VarEnv<f64> {
    let mut env = init_eval_env(dae);
    evaluate_constants_in_place(dae, &mut env);
    for _ in 0..2 {
        for (name, var) in &dae.parameters {
            assign_start_values(&mut env, name, var);
        }
    }
    env
}

fn assign_start_values(env: &mut VarEnv<f64>, name: &VarName, var: &Variable) {
    let size = var.size();
    if size <= 1 {
        let value = var
            .start
            .as_ref()
            .map(|expr| eval_expr::<f64>(expr, env))
            .unwrap_or(0.0);
        env.set(name.as_str(), value);
        return;
    }

    if let Some(Expression::Array { elements, .. }) = var.start.as_ref() {
        for (idx, expr) in elements.iter().enumerate() {
            let value = eval_expr::<f64>(expr, env);
            env.set(&format!("{}[{}]", name.as_str(), idx + 1), value);
        }
        return;
    }

    let value = var
        .start
        .as_ref()
        .map(|expr| eval_expr::<f64>(expr, env))
        .unwrap_or(0.0);
    for idx in 0..size {
        env.set(&format!("{}[{}]", name.as_str(), idx + 1), value);
    }
}

fn build_parameter_values(dae: &Dae) -> Vec<f64> {
    let env = build_parameter_env(dae);
    dae.parameters
        .values()
        .flat_map(|var| eval_var_start_values(var, &env))
        .collect()
}

fn build_state_start_values(dae: &Dae) -> Vec<f64> {
    let env = build_parameter_env(dae);
    dae.states
        .values()
        .flat_map(|var| eval_var_start_values(var, &env))
        .collect()
}

fn eval_var_start_values(var: &Variable, env: &VarEnv<f64>) -> Vec<f64> {
    let size = var.size();
    if size <= 1 {
        return vec![eval_scalar_start_value(var, env)];
    }
    let Some(start) = var.start.as_ref() else {
        return vec![0.0; size];
    };

    let raw = eval_array_values::<f64>(start, env);
    if raw.is_empty() {
        return vec![eval_expr::<f64>(start, env); size];
    }
    if raw.len() == 1 {
        return vec![raw[0]; size];
    }
    let last = *raw.last().unwrap_or(&0.0);
    (0..size)
        .map(|idx| raw.get(idx).copied().unwrap_or(last))
        .collect()
}

fn eval_scalar_start_value(var: &Variable, env: &VarEnv<f64>) -> f64 {
    if let Some(start) = &var.start {
        return eval_expr::<f64>(start, env);
    }
    if let Some(nominal) = &var.nominal {
        return eval_expr::<f64>(nominal, env);
    }
    0.0
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;
    use rumoca_ir_core::OpBinary;
    use rumoca_sim::run_with_runtime_schedule;

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: VarName::new(name),
            subscripts: vec![],
        }
    }

    fn real(value: f64) -> Expression {
        Expression::Literal(dae::Literal::Real(value))
    }

    fn der(name: &str) -> Expression {
        Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var(name)],
        }
    }

    fn add(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Add(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn mul(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Mul(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn sub(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }

    fn residual(rhs: Expression, origin: &str) -> dae::Equation {
        dae::Equation {
            lhs: None,
            rhs,
            span: Span::DUMMY,
            scalar_count: 1,
            origin: origin.to_string(),
        }
    }

    fn build_decay_dae() -> Dae {
        let mut dae = Dae::default();
        let mut x = Variable::new(VarName::new("x"));
        x.start = Some(real(2.0));
        dae.states.insert(VarName::new("x"), x);

        let mut k = Variable::new(VarName::new("k"));
        k.start = Some(real(3.0));
        dae.parameters.insert(VarName::new("k"), k);

        dae.f_x.push(residual(
            add(der("x"), mul(var("k"), var("x"))),
            "der(x) = -k*x",
        ));
        dae
    }

    #[test]
    fn simulates_simple_decay_model() {
        let dae = build_decay_dae();
        let result = simulate_dae(
            &dae,
            &SimOptions {
                t_end: 1.0,
                dt: Some(0.1),
                solver_mode: SimSolverMode::RkLike,
                ..SimOptions::default()
            },
        )
        .expect("rk45 simulation should succeed");

        let x_final = result
            .data
            .first()
            .and_then(|series| series.last())
            .copied()
            .expect("state trajectory");
        let expected = 2.0 * (-3.0_f64).exp();
        assert!(
            (x_final - expected).abs() < 5.0e-4,
            "expected x(1)≈{expected}, got {x_final}"
        );
    }

    #[test]
    fn rejects_models_with_continuous_algebraics() {
        let mut dae = build_decay_dae();
        dae.algebraics
            .insert(VarName::new("y"), Variable::new(VarName::new("y")));
        dae.f_x.push(residual(sub(var("y"), var("x")), "y = x"));

        let err = simulate_dae(&dae, &SimOptions::default()).expect_err("must reject algebraics");
        assert!(
            err.to_string().contains("continuous algebraics"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn backend_conforms_to_shared_runtime_schedule_surface() {
        let dae = build_decay_dae();
        let model = ExplicitModel::new(&dae, true).expect("prepare explicit model");
        let opts = SimOptions {
            t_end: 0.2,
            dt: Some(0.2),
            solver_mode: SimSolverMode::RkLike,
            ..SimOptions::default()
        };
        let mut backend = Rk45Backend::new(&model, &opts).expect("backend");

        let stats = run_with_runtime_schedule(&mut backend, &dae, 0.0, 0.2, || Ok(()))
            .expect("shared runtime schedule should drive rk45 backend");
        assert!(stats.steps > 0);
        assert!(backend.time >= 0.2);
    }
}
