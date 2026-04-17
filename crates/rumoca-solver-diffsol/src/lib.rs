pub mod eliminate {
    use rumoca_ir_dae::Dae;

    pub(crate) type EliminationResult = rumoca_phase_solve::eliminate::EliminationResult;

    pub fn eliminate_trivial(dae: &mut Dae) -> rumoca_phase_solve::eliminate::EliminationResult {
        rumoca_phase_solve::eliminate::eliminate_trivial(dae)
    }
}
pub(crate) mod integration;
mod prepare;
mod prepared_sim;
pub mod problem;
pub mod stepper;

use std::collections::{HashMap, HashSet};

use diffsol::{
    FaerSparseLU, OdeEquations, OdeSolverMethod, OdeSolverProblem, OdeSolverStopReason, VectorHost,
};
use indexmap::IndexMap;
use rumoca_ir_dae as dae;

pub(crate) type Dae = dae::Dae;
type BuiltinFunction = dae::BuiltinFunction;
type Expression = dae::Expression;
type Literal = dae::Literal;
type Subscript = dae::Subscript;
pub(crate) type VarName = dae::VarName;
type Variable = dae::Variable;

use rumoca_core::Span;
use rumoca_ir_core::OpBinary;
pub use rumoca_sim::{SimBackend, SimOptions, SimResult, SimSolverMode, SimVariableMeta};
use rumoca_sim::{
    SolverDeadlineGuard, TimeoutBudget, TimeoutExceeded, build_variable_meta,
    is_solver_timeout_panic, timeline,
};

pub(crate) use rumoca_core::{
    maybe_elapsed_seconds as trace_timer_elapsed_seconds,
    maybe_start_timer_if as trace_timer_start_if,
};
use rumoca_eval_dae::runtime::{self as eval};
pub(crate) use rumoca_sim::simulation::dae_prepare::REGULARIZATION_LEVELS;
#[cfg(test)]
pub(crate) use rumoca_sim::simulation::dae_prepare::{
    demote_alias_states_without_der, demote_coupled_derivative_states,
    demote_direct_assigned_states, demote_orphan_states_without_equation_refs,
    demote_states_without_assignable_derivative_rows, demote_states_without_derivative_refs,
    index_reduce_missing_state_derivatives, promote_der_algebraics_to_states,
};

pub(crate) type LS = FaerSparseLU<f64>;
use integration::map_solver_panic;
use integration::*;
use prepare::*;
#[cfg(test)]
pub(crate) use prepared_sim::collect_no_state_schedule_events;
use prepared_sim::validate_parameter_override;
pub use prepared_sim::{
    build_simulation, run_prepared_simulation, simulate, simulate as simulate_dae,
};
use rumoca_sim::equation_scalarize::build_output_names;
#[cfg(test)]
use rumoca_sim::equation_scalarize::{
    build_complex_field_map, build_var_dims_map, index_into_expr,
};
#[cfg(test)]
use rumoca_sim::projection_maps::{
    build_component_index_projection_map, build_function_output_projection_map,
};
pub use stepper::{SimStepper, StepperOptions, StepperState};

fn validate_simulation_function_support(dae: &Dae) -> Result<(), SimError> {
    rumoca_sim::function_validation::validate_simulation_function_support(dae).map_err(|err| {
        SimError::UnsupportedFunction {
            name: err.name,
            reason: err.reason,
        }
    })
}

pub struct PreparedSimulation {
    dae: Dae,
    elim: eliminate::EliminationResult,
    opts: SimOptions,
    parameter_overrides: IndexMap<String, Vec<f64>>,
    state: PreparedSimulationState,
}

enum PreparedSimulationState {
    Algebraic(PreparedAlgebraicSimulation),
    Dynamic(PreparedDynamicSimulation),
}

struct PreparedAlgebraicSimulation {}

struct PreparedDynamicSimulation {
    mass_matrix: MassMatrix,
    ic_blocks: Vec<rumoca_phase_solve::IcBlock>,
}

impl PreparedSimulation {
    pub fn backend(&self) -> SimBackend {
        SimBackend::Diffsol
    }

    pub fn set_parameter_value(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        self.set_parameter_values(name, &[value])
    }

    pub fn set_parameter_values(&mut self, name: &str, values: &[f64]) -> Result<(), SimError> {
        validate_parameter_override(self, name, values)?;
        self.parameter_overrides
            .insert(name.to_string(), values.to_vec());
        Ok(())
    }

    pub fn clear_parameter_overrides(&mut self) {
        self.parameter_overrides.clear();
    }

    pub fn run(&self) -> Result<SimResult, SimError> {
        run_prepared_simulation(self)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SimError {
    #[error("empty system: no equations to simulate")]
    EmptySystem,

    #[error(
        "equation/variable mismatch: {n_equations} equations but \
         {n_states} states + {n_algebraics} algebraics = {} unknowns",
        n_states + n_algebraics
    )]
    EquationMismatch {
        n_equations: usize,
        n_states: usize,
        n_algebraics: usize,
    },

    #[error(
        "no ODE equation found for state variable '{0}': \
         every state needs an equation containing der({0})"
    )]
    MissingStateEquation(String),

    #[error("solver error: {0}")]
    SolverError(String),

    #[error("unsupported function '{name}': {reason}")]
    UnsupportedFunction { name: String, reason: String },

    #[error("compiled evaluator build failed: {0}")]
    CompiledEval(String),

    #[error(
        "mass matrix form could not be derived for DiffSol at row {row} (state '{state_name}', origin '{origin}'): {reason}"
    )]
    MassMatrixForm {
        row: usize,
        state_name: String,
        origin: String,
        reason: String,
    },

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

impl From<rumoca_sim::simulation::runtime_prep::MassMatrixBuildError> for SimError {
    fn from(value: rumoca_sim::simulation::runtime_prep::MassMatrixBuildError) -> Self {
        match value {
            rumoca_sim::simulation::runtime_prep::MassMatrixBuildError::Timeout { seconds } => {
                Self::Timeout { seconds }
            }
            rumoca_sim::simulation::runtime_prep::MassMatrixBuildError::NonDerivable {
                row,
                state_name,
                origin,
                reason,
            } => Self::MassMatrixForm {
                row,
                state_name,
                origin,
                reason,
            },
        }
    }
}

pub(crate) struct OutputBuffers {
    times: Vec<f64>,
    data: Vec<Vec<f64>>,
    n_total: usize,
    runtime_names: Vec<String>,
    runtime_data: Vec<Vec<f64>>,
}

impl OutputBuffers {
    fn new(n_total: usize, capacity: usize) -> Self {
        Self {
            times: Vec::with_capacity(capacity),
            data: (0..n_total).map(|_| Vec::with_capacity(capacity)).collect(),
            n_total,
            runtime_names: Vec::new(),
            runtime_data: Vec::new(),
        }
    }

    fn record(&mut self, t: f64, y: &[f64]) {
        self.times.push(t);
        for (var_idx, val) in y[..self.n_total].iter().enumerate() {
            self.data[var_idx].push(*val);
        }
    }

    fn set_runtime_channels(&mut self, names: Vec<String>, capacity: usize) {
        self.runtime_names = names;
        self.runtime_data = self
            .runtime_names
            .iter()
            .map(|_| Vec::with_capacity(capacity))
            .collect();
    }

    fn record_runtime_values(&mut self, values: &[f64]) {
        if self.runtime_data.is_empty() {
            return;
        }
        for (idx, series) in self.runtime_data.iter_mut().enumerate() {
            series.push(values.get(idx).copied().unwrap_or(0.0));
        }
    }

    fn overwrite_runtime_values_at_time(&mut self, t: f64, values: &[f64]) -> bool {
        if self.runtime_data.is_empty() || self.times.is_empty() {
            return false;
        }
        let tol = 1.0e-9 * (1.0 + t.abs());
        let Some((row_idx, _)) = self
            .times
            .iter()
            .enumerate()
            .rev()
            .find(|(_, sample_t)| (**sample_t - t).abs() <= tol)
        else {
            return false;
        };
        for (idx, series) in self.runtime_data.iter_mut().enumerate() {
            if let Some(slot) = series.get_mut(row_idx) {
                *slot = values.get(idx).copied().unwrap_or(0.0);
            }
        }
        true
    }
}

pub(crate) fn interp_err(t: f64, e: impl std::fmt::Display) -> SimError {
    SimError::SolverError(format!("Interpolation failed at t={t}: {e}"))
}

fn scalar_channel_names_from_vars<'a>(
    vars: impl Iterator<Item = (&'a VarName, &'a Variable)>,
) -> Vec<String> {
    let mut names = Vec::new();
    for (name, var) in vars {
        let size = var.size();
        if size <= 1 {
            names.push(name.as_str().to_string());
        } else {
            for i in 1..=size {
                names.push(format!("{}[{}]", name.as_str(), i));
            }
        }
    }
    names
}

fn build_visible_result_names(dae: &Dae) -> Vec<String> {
    let mut names = build_output_names(dae);
    names.extend(scalar_channel_names_from_vars(dae.discrete_reals.iter()));
    names.extend(scalar_channel_names_from_vars(dae.discrete_valued.iter()));
    names
}

pub(crate) fn run_timeout_step<F>(budget: &TimeoutBudget, step: F) -> Result<(), SimError>
where
    F: FnOnce(),
{
    rumoca_sim::run_timeout_step::<SimError, _>(budget, step)
}

pub(crate) fn run_timeout_step_result<F>(budget: &TimeoutBudget, step: F) -> Result<(), SimError>
where
    F: FnOnce() -> Result<(), SimError>,
{
    rumoca_sim::run_timeout_step_result::<SimError, _>(budget, step)
}

fn collect_discrete_channel_names(dae: &Dae) -> Vec<String> {
    scalar_channel_names_from_vars(dae.discrete_reals.iter().chain(dae.discrete_valued.iter()))
}

fn expr_uses_event_dependent_discrete(expr: &Expression) -> bool {
    match expr {
        Expression::BuiltinCall { function, args } => {
            matches!(
                function,
                BuiltinFunction::Pre
                    | BuiltinFunction::Sample
                    | BuiltinFunction::Edge
                    | BuiltinFunction::Change
                    | BuiltinFunction::Reinit
                    | BuiltinFunction::Initial
            ) || args.iter().any(expr_uses_event_dependent_discrete)
        }
        Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            matches!(
                short,
                "previous"
                    | "hold"
                    | "Clock"
                    | "subSample"
                    | "superSample"
                    | "shiftSample"
                    | "backSample"
                    | "firstTick"
            ) || args.iter().any(expr_uses_event_dependent_discrete)
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_uses_event_dependent_discrete(lhs) || expr_uses_event_dependent_discrete(rhs)
        }
        Expression::Unary { rhs, .. } => expr_uses_event_dependent_discrete(rhs),
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expr_uses_event_dependent_discrete(cond)
                    || expr_uses_event_dependent_discrete(value)
            }) || expr_uses_event_dependent_discrete(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(expr_uses_event_dependent_discrete)
        }
        Expression::Range { start, step, end } => {
            expr_uses_event_dependent_discrete(start)
                || step
                    .as_ref()
                    .is_some_and(|value| expr_uses_event_dependent_discrete(value))
                || expr_uses_event_dependent_discrete(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expr_uses_event_dependent_discrete(expr)
                || indices
                    .iter()
                    .any(|index| expr_uses_event_dependent_discrete(&index.range))
                || filter
                    .as_ref()
                    .is_some_and(|value| expr_uses_event_dependent_discrete(value))
        }
        Expression::Index { base, subscripts } => {
            expr_uses_event_dependent_discrete(base)
                || subscripts.iter().any(|sub| match sub {
                    Subscript::Expr(value) => expr_uses_event_dependent_discrete(value),
                    _ => false,
                })
        }
        Expression::FieldAccess { base, .. } => expr_uses_event_dependent_discrete(base),
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => false,
    }
}

fn collect_recomputable_discrete_targets(dae: &Dae) -> HashSet<String> {
    let mut targets = HashSet::new();
    for eq in dae.f_z.iter().chain(dae.f_m.iter()) {
        let Some(lhs) = eq.lhs.as_ref() else {
            continue;
        };
        if expr_uses_event_dependent_discrete(&eq.rhs) {
            continue;
        }
        targets.insert(lhs.as_str().to_string());
    }
    targets
}

fn evaluate_runtime_discrete_channels(
    dae: &Dae,
    n_x: usize,
    param_values: &[f64],
    times: &[f64],
    solver_names: &[String],
    solver_data: &[Vec<f64>],
) -> (Vec<String>, Vec<Vec<f64>>) {
    let recomputable_targets = collect_recomputable_discrete_targets(dae);
    let discrete_names: Vec<String> = collect_discrete_channel_names(dae)
        .into_iter()
        .filter(|name| {
            let base = rumoca_ir_dae::component_base_name(name).unwrap_or_else(|| name.to_string());
            recomputable_targets.contains(&base)
        })
        .collect();
    if discrete_names.is_empty() || times.is_empty() {
        return (Vec::new(), Vec::new());
    }

    let solver_name_to_idx: HashMap<&str, usize> = solver_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.as_str(), idx))
        .collect();
    let solver_len = solver_data.len().min(solver_names.len());
    let mut discrete_data: Vec<Vec<f64>> = discrete_names
        .iter()
        .map(|_| Vec::with_capacity(times.len()))
        .collect();
    let use_frozen_pre =
        rumoca_sim::runtime::no_state::no_state_requires_frozen_event_pre_values(dae);
    eval::clear_pre_values();

    for (sample_idx, &t_eval) in times.iter().enumerate() {
        let mut y = vec![0.0; solver_len];
        for (col_idx, series) in solver_data.iter().enumerate().take(solver_len) {
            if let Some(value) = series.get(sample_idx).copied() {
                y[col_idx] = value;
            }
        }
        let settle_input = rumoca_sim::EventSettleInput {
            dae,
            y: &mut y,
            p: param_values,
            n_x,
            t_eval,
            is_initial: false,
        };
        let env = if use_frozen_pre {
            rumoca_sim::runtime::event::settle_runtime_event_updates_default_frozen_pre(
                settle_input,
            )
        } else {
            rumoca_sim::runtime::event::settle_runtime_event_updates_default(settle_input)
        };
        for (channel_idx, name) in discrete_names.iter().enumerate() {
            let value = env
                .vars
                .get(name.as_str())
                .copied()
                .or_else(|| {
                    solver_name_to_idx
                        .get(name.as_str())
                        .and_then(|idx| y.get(*idx).copied())
                })
                .unwrap_or(0.0);
            discrete_data[channel_idx].push(value);
        }
        eval::seed_pre_values_from_env(&env);
    }

    (discrete_names, discrete_data)
}

fn refresh_runtime_observed_solver_channels(
    dae: &Dae,
    n_x: usize,
    param_values: &[f64],
    times: &[f64],
    solver_names: &[String],
    solver_data: &mut [Vec<f64>],
) {
    if times.is_empty() || solver_names.is_empty() || solver_data.is_empty() {
        return;
    }

    let solver_len = solver_data.len().min(solver_names.len());
    if solver_len == 0 {
        return;
    }

    let n_eq = dae.f_x.len();
    let projection_masks = (n_eq > 0 && n_x < n_eq && solver_len >= n_eq)
        .then(|| problem::build_runtime_projection_masks(dae, n_x, n_eq));
    let projection_runtime_ctx = projection_masks
        .as_ref()
        .and_then(|_| problem::build_compiled_runtime_newton_context(dae, n_eq).ok());
    let projection_direct_seed_ctx = projection_masks
        .as_ref()
        .map(|_| problem::build_runtime_direct_seed_context(dae, solver_len, n_x));
    let projection_timeout = TimeoutBudget::new(None);
    let mut projection_jacobian = None;
    let mut projection_seed_env = None;
    let mut projection_scratch = problem::RuntimeProjectionScratch::default();
    let use_frozen_pre =
        rumoca_sim::runtime::no_state::no_state_requires_frozen_event_pre_values(dae);

    eval::clear_pre_values();
    for (sample_idx, &t_eval) in times.iter().enumerate() {
        let mut y = vec![0.0; solver_len];
        for (col_idx, series) in solver_data.iter().enumerate().take(solver_len) {
            if let Some(value) = series.get(sample_idx).copied() {
                y[col_idx] = value;
            }
        }

        // MLS §8 equations are equalities, and connector zero-sum equations
        // are ordinary equations as well. Re-project continuous algebraics at
        // the observation instant before refreshing runtime channels so traced
        // connector/alias algebraics do not lag stale solver interpolation.
        if let Some(masks) = projection_masks.as_ref() {
            let y_seed = y.clone();
            let _ = problem::project_algebraics_with_fixed_states_at_time_with_context_and_cache_in_place(
                dae,
                y.as_mut_slice(),
                problem::RuntimeProjectionContext {
                    p: param_values,
                    compiled_runtime: projection_runtime_ctx.as_ref(),
                    fixed_cols: &masks.fixed_cols,
                    ignored_rows: &masks.ignored_rows,
                    branch_local_analog_cols: &masks.branch_local_analog_cols,
                    direct_seed_ctx: projection_direct_seed_ctx.as_ref(),
                    direct_seed_env_cache: Some(&mut projection_seed_env),
                },
                problem::RuntimeProjectionStep {
                    y_seed: y_seed.as_slice(),
                    n_x,
                    t_eval,
                    tol: 1.0e-8,
                    timeout: &projection_timeout,
                },
                Some(&mut projection_jacobian),
                &mut projection_scratch,
            );
        }

        // MLS §16.5.1 preserves sample/hold/pre semantics at observation
        // instants. Refresh the runtime env before emitting traces so observed
        // algebraic/output channels satisfy their defining equations instead
        // of stale solver interpolation slots.
        let settle_input = rumoca_sim::EventSettleInput {
            dae,
            y: &mut y,
            p: param_values,
            n_x,
            t_eval,
            is_initial: false,
        };
        let env = if use_frozen_pre {
            rumoca_sim::runtime::event::settle_runtime_event_updates_default_frozen_pre(
                settle_input,
            )
        } else {
            rumoca_sim::runtime::event::settle_runtime_event_updates_default(settle_input)
        };

        for (col_idx, name) in solver_names.iter().enumerate().take(solver_len) {
            if col_idx < n_x {
                continue;
            }
            let Some(value) = env.vars.get(name.as_str()).copied() else {
                continue;
            };
            if let Some(slot) = solver_data
                .get_mut(col_idx)
                .and_then(|series| series.get_mut(sample_idx))
            {
                *slot = value;
            }
        }

        eval::seed_pre_values_from_env(&env);
    }
}

fn merge_runtime_discrete_channels(
    final_names: &mut Vec<String>,
    final_data: &mut Vec<Vec<f64>>,
    discrete_names: Vec<String>,
    discrete_data: Vec<Vec<f64>>,
) {
    if discrete_names.is_empty() {
        return;
    }
    let mut existing_idx: HashMap<String, usize> = final_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();

    for (name, series) in discrete_names.into_iter().zip(discrete_data) {
        if let Some(idx) = existing_idx.get(&name).copied() {
            if idx < final_data.len() {
                final_data[idx] = series;
            }
            continue;
        }
        let next = final_names.len();
        existing_idx.insert(name.clone(), next);
        final_names.push(name);
        final_data.push(series);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum SolverStartupProfile {
    Default,
    RobustTinyStep,
}

#[derive(Debug, Clone, Copy)]
struct TimeoutSolverCaps {
    max_nonlinear_iters: usize,
    max_nonlinear_failures: usize,
    max_error_failures: usize,
    min_timestep: f64,
}

fn timeout_solver_caps(
    max_wall_seconds: Option<f64>,
    profile: SolverStartupProfile,
) -> Option<TimeoutSolverCaps> {
    let secs = max_wall_seconds.filter(|s| s.is_finite() && *s > 0.0)?;
    if secs <= 1.0 {
        return Some(match profile {
            SolverStartupProfile::Default => TimeoutSolverCaps {
                max_nonlinear_iters: 10,
                max_nonlinear_failures: 30,
                max_error_failures: 20,
                min_timestep: 1e-14,
            },
            SolverStartupProfile::RobustTinyStep => TimeoutSolverCaps {
                max_nonlinear_iters: 30,
                max_nonlinear_failures: 180,
                max_error_failures: 90,
                min_timestep: 1e-16,
            },
        });
    }
    if secs <= 2.0 {
        return Some(match profile {
            SolverStartupProfile::Default => TimeoutSolverCaps {
                max_nonlinear_iters: 12,
                max_nonlinear_failures: 50,
                max_error_failures: 30,
                min_timestep: 1e-14,
            },
            SolverStartupProfile::RobustTinyStep => TimeoutSolverCaps {
                max_nonlinear_iters: 40,
                max_nonlinear_failures: 240,
                max_error_failures: 120,
                min_timestep: 1e-16,
            },
        });
    }
    if secs <= 10.0 {
        return Some(match profile {
            SolverStartupProfile::Default => TimeoutSolverCaps {
                max_nonlinear_iters: 20,
                max_nonlinear_failures: 120,
                max_error_failures: 80,
                min_timestep: 1e-14,
            },
            SolverStartupProfile::RobustTinyStep => TimeoutSolverCaps {
                max_nonlinear_iters: 40,
                max_nonlinear_failures: 800,
                max_error_failures: 400,
                min_timestep: 1e-16,
            },
        });
    }

    Some(match profile {
        SolverStartupProfile::Default => TimeoutSolverCaps {
            max_nonlinear_iters: 20,
            max_nonlinear_failures: 1000,
            max_error_failures: 600,
            min_timestep: 1e-14,
        },
        SolverStartupProfile::RobustTinyStep => TimeoutSolverCaps {
            max_nonlinear_iters: 40,
            max_nonlinear_failures: 4000,
            max_error_failures: 2000,
            min_timestep: 1e-16,
        },
    })
}

fn apply_timeout_solver_caps<Eqn>(
    problem: &mut OdeSolverProblem<Eqn>,
    max_wall_seconds: Option<f64>,
    profile: SolverStartupProfile,
) where
    Eqn: OdeEquations<T = f64>,
{
    let Some(caps) = timeout_solver_caps(max_wall_seconds, profile) else {
        return;
    };
    problem.ode_options.max_nonlinear_solver_iterations = problem
        .ode_options
        .max_nonlinear_solver_iterations
        .min(caps.max_nonlinear_iters);
    problem.ode_options.max_nonlinear_solver_failures = problem
        .ode_options
        .max_nonlinear_solver_failures
        .min(caps.max_nonlinear_failures);
    problem.ode_options.max_error_test_failures = problem
        .ode_options
        .max_error_test_failures
        .min(caps.max_error_failures);
    if problem.ode_options.min_timestep < caps.min_timestep {
        problem.ode_options.min_timestep = caps.min_timestep;
    }
}

fn startup_interval_cap(opts: &SimOptions) -> Option<f64> {
    let dt = opts.dt?;
    if !dt.is_finite() || dt <= 0.0 {
        return None;
    }
    let span = (opts.t_end - opts.t_start).abs();
    if span.is_finite() && span > 0.0 {
        let tiny_interval_threshold = span / 5000.0;
        if dt < tiny_interval_threshold {
            return None;
        }
    }
    Some((dt.abs() * 20.0).max(1e-10))
}

fn nonlinear_solver_tolerance(opts: &SimOptions, profile: SolverStartupProfile) -> f64 {
    let base = opts.atol.max(opts.rtol).max(1.0e-12);
    match profile {
        SolverStartupProfile::Default => (base * 10.0).clamp(1.0e-8, 1.0e-3),
        SolverStartupProfile::RobustTinyStep => (base * 100.0).clamp(1.0e-7, 1.0e-2),
    }
}

fn configure_solver_problem_with_profile<Eqn>(
    problem: &mut OdeSolverProblem<Eqn>,
    opts: &SimOptions,
    profile: SolverStartupProfile,
) where
    Eqn: OdeEquations<T = f64>,
{
    problem.ode_options.max_nonlinear_solver_iterations = 20;
    problem.ode_options.max_nonlinear_solver_failures = 1000;
    problem.ode_options.max_error_test_failures = 600;
    problem.ode_options.nonlinear_solver_tolerance = nonlinear_solver_tolerance(opts, profile);
    problem.ode_options.min_timestep = 1e-16;
    let span = (opts.t_end - opts.t_start).abs();
    let interval_cap = startup_interval_cap(opts);
    if span.is_finite() && span > 0.0 {
        problem.h0 = (span / 500.0).max(1e-6);
        if let Some(cap) = interval_cap {
            problem.h0 = problem.h0.min(cap);
        }
    } else if let Some(cap) = interval_cap {
        problem.h0 = cap;
    }

    if profile == SolverStartupProfile::RobustTinyStep {
        problem.ode_options.max_nonlinear_solver_iterations = 40;
        problem.ode_options.max_nonlinear_solver_failures = 4000;
        problem.ode_options.max_error_test_failures = 2000;
        problem.ode_options.nonlinear_solver_tolerance = nonlinear_solver_tolerance(opts, profile);
        if span.is_finite() && span > 0.0 {
            problem.h0 = (span / 5_000_000.0).max(1e-10);
        }
    }

    apply_timeout_solver_caps(problem, opts.max_wall_seconds, profile);
}

pub(crate) fn build_parameter_values(
    dae: &Dae,
    budget: &TimeoutBudget,
) -> Result<Vec<f64>, SimError> {
    problem::default_params_with_budget(dae, budget)
}

fn parameter_slice_range(dae: &Dae, target: &str) -> Option<(usize, usize, usize)> {
    let mut start = 0usize;
    for (name, var) in &dae.parameters {
        let size = var.size();
        if name.as_str() == target {
            return Some((start, start + size, size));
        }
        start += size;
    }
    None
}

fn apply_parameter_overrides(
    dae: &Dae,
    params: &mut [f64],
    overrides: &IndexMap<String, Vec<f64>>,
) -> Result<(), SimError> {
    for (name, values) in overrides {
        let Some((start, end, expected_len)) = parameter_slice_range(dae, name.as_str()) else {
            return Err(SimError::SolverError(format!(
                "unknown parameter override '{name}'"
            )));
        };
        if values.len() != expected_len {
            return Err(SimError::SolverError(format!(
                "parameter override '{name}' expected {expected_len} value(s), got {}",
                values.len()
            )));
        }
        params[start..end].copy_from_slice(values);
    }
    Ok(())
}

fn build_parameter_values_with_overrides(
    dae: &Dae,
    budget: &TimeoutBudget,
    overrides: &IndexMap<String, Vec<f64>>,
) -> Result<Vec<f64>, SimError> {
    let mut params = build_parameter_values(dae, budget)?;
    apply_parameter_overrides(dae, params.as_mut_slice(), overrides)?;
    Ok(params)
}

const DUMMY_STATE_NAME: &str = "_rumoca_dummy_state";

pub(crate) fn inject_dummy_state(dae: &mut Dae) {
    let var_name = VarName::new(DUMMY_STATE_NAME);
    let mut var = dae::Variable::new(var_name.clone());
    var.start = Some(Expression::Literal(Literal::Real(0.0)));
    var.fixed = Some(true);
    dae.states.insert(var_name, var);

    let der_expr = Expression::BuiltinCall {
        function: rumoca_ir_dae::BuiltinFunction::Der,
        args: vec![Expression::VarRef {
            name: VarName::new(DUMMY_STATE_NAME),
            subscripts: vec![],
        }],
    };
    let eq = dae::Equation {
        lhs: None,
        rhs: der_expr,
        span: Span::DUMMY,
        scalar_count: 1,
        origin: "dummy_state_injection".to_string(),
    };
    dae.f_x.push(eq);
}

pub(crate) type MassMatrix = rumoca_sim::simulation::pipeline::MassMatrix;

pub(crate) fn debug_print_after_expand(dae: &Dae) {
    if std::env::var("RUMOCA_DEBUG").is_err() {
        return;
    }
    eprintln!("[after expand_compound_derivatives] equations:");
    for (i, eq) in dae.f_x.iter().enumerate() {
        eprintln!("  eq[{}]: {:?}", i, eq.rhs);
    }
    eprintln!(
        "[after expand_compound_derivatives] algebraics: {:?}",
        dae.algebraics
            .keys()
            .map(|n| n.as_str())
            .collect::<Vec<_>>()
    );
    eprintln!(
        "[after expand_compound_derivatives] states: {:?}",
        dae.states.keys().map(|n| n.as_str()).collect::<Vec<_>>()
    );
}

pub(crate) fn debug_print_prepare_counts(dae: &Dae) {
    if std::env::var("RUMOCA_DEBUG").is_ok() {
        eprintln!(
            "[prepare_dae] states={}, algebraics={}, eqs={}",
            dae.states.len(),
            dae.algebraics.len(),
            dae.f_x.len()
        );
    }
}

pub(crate) fn debug_print_mass_matrix(dae: &Dae, mass_matrix: &MassMatrix) {
    if std::env::var("RUMOCA_DEBUG").is_err() {
        return;
    }
    let state_names: Vec<_> = dae.states.keys().map(|n| n.as_str()).collect();
    for (i, name) in state_names.iter().enumerate() {
        let diag = mass_matrix
            .get(i)
            .and_then(|row| row.get(i))
            .copied()
            .unwrap_or(1.0);
        if (diag - 1.0).abs() > 1e-10 {
            eprintln!("[mass_matrix] state[{i}] {name:?} diag={diag}");
        }
        if let Some(row) = mass_matrix.get(i) {
            for (j, coeff) in row
                .iter()
                .copied()
                .enumerate()
                .filter(|(j, coeff)| *j != i && coeff.abs() > 1e-10)
            {
                let other = state_names.get(j).copied().unwrap_or("<unknown>");
                eprintln!("[mass_matrix] state[{i}] {name:?} offdiag[{j}] {other:?}={coeff}");
            }
        }
    }
}

fn sim_introspect_enabled() -> bool {
    rumoca_sim::simulation::diagnostics::sim_introspect_enabled()
}

pub(crate) fn sim_trace_enabled() -> bool {
    rumoca_sim::simulation::diagnostics::sim_trace_enabled()
}

fn dump_hotpath_stats_if_enabled() {
    let Some(stats) = rumoca_sim::runtime::hotpath_stats::snapshot() else {
        return;
    };
    let per_step = |count: u64| {
        if stats.solver_steps == 0 {
            0.0
        } else {
            count as f64 / stats.solver_steps as f64
        }
    };
    let per_eval = |count: u64| {
        if stats.no_state_eval_points == 0 {
            0.0
        } else {
            count as f64 / stats.no_state_eval_points as f64
        }
    };
    eprintln!(
        concat!(
            "[sim-hotpath] solver_steps={} root_hits={} no_state_eval_points={} no_state_settles={} ",
            "clock_edge_evals={} sample_active_checks={} sample_active_true={} ",
            "held_value_reads={} left_limit_reads={} ",
            "explicit_clock_inference={} clock_alias_source_scans={} ",
            "per_step(clock_edge={:.2} sample_active={:.2} held={:.2} left_limit={:.2} infer={:.2} alias_scan={:.2}) ",
            "per_eval(clock_edge={:.2} sample_active={:.2} held={:.2} left_limit={:.2} infer={:.2} alias_scan={:.2} settles={:.2})"
        ),
        stats.solver_steps,
        stats.root_hits,
        stats.no_state_eval_points,
        stats.no_state_settles,
        stats.clock_edge_evals,
        stats.sample_active_checks,
        stats.sample_active_true,
        stats.held_value_reads,
        stats.left_limit_reads,
        stats.explicit_clock_inference,
        stats.clock_alias_source_scans,
        per_step(stats.clock_edge_evals),
        per_step(stats.sample_active_checks),
        per_step(stats.held_value_reads),
        per_step(stats.left_limit_reads),
        per_step(stats.explicit_clock_inference),
        per_step(stats.clock_alias_source_scans),
        per_eval(stats.clock_edge_evals),
        per_eval(stats.sample_active_checks),
        per_eval(stats.held_value_reads),
        per_eval(stats.left_limit_reads),
        per_eval(stats.explicit_clock_inference),
        per_eval(stats.clock_alias_source_scans),
        per_eval(stats.no_state_settles),
    );
}

fn truncate_debug(s: &str, max_chars: usize) -> String {
    rumoca_sim::simulation::diagnostics::truncate_debug(s, max_chars)
}

fn validate_no_initial_division_by_zero(
    dae: &Dae,
    param_values: &[f64],
    t_start: f64,
) -> Result<(), SimError> {
    let mut y0 = vec![0.0; dae.f_x.len()];
    problem::initialize_state_vector_with_params(dae, &mut y0, param_values);
    let pre_snapshot = eval::snapshot_pre_values();
    let env = rumoca_sim::runtime::startup::build_initial_section_env_strict(
        dae,
        y0.as_mut_slice(),
        param_values,
        t_start,
    );
    eval::restore_pre_values(pre_snapshot);
    let env = env.map_err(SimError::SolverError)?;
    // MLS §8.6: startup validation must evaluate `initial()` as true.
    let eval_initial_scalar = |expr: &dae::Expression, env: &eval::VarEnv<f64>| match expr {
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Initial,
            args,
        } if args.is_empty() => Some(1.0),
        _ => rumoca_sim::runtime::scalar_eval::eval_scalar_expr_fast(expr, env),
    };
    let eval_initial_bool = |expr: &dae::Expression, env: &eval::VarEnv<f64>| match expr {
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Initial,
            args,
        } if args.is_empty() => Some(true),
        _ => rumoca_sim::runtime::scalar_eval::eval_scalar_bool_expr_fast(expr, env),
    };
    if let Some(site) =
        rumoca_sim::simulation::diagnostics::find_initial_division_by_zero_site_with_callbacks(
            dae,
            &env,
            eval_initial_scalar,
            eval_initial_bool,
        )
    {
        let msg = format!(
            "division by zero at initialization (t={}): (a={}) / (b={}), divisor expression is: {}, equation {}[{}] origin='{}' rhs={}",
            t_start,
            site.expr_site.numerator,
            site.expr_site.denominator,
            site.expr_site.divisor_expr,
            site.equation_set,
            site.equation_index,
            site.origin,
            site.rhs_expr,
        );
        return Err(SimError::SolverError(msg));
    }
    Ok(())
}

pub(crate) fn dump_missing_state_equation_diagnostics(dae: &Dae, missing_state: &str) {
    rumoca_sim::simulation::diagnostics::dump_missing_state_equation_diagnostics(
        dae,
        missing_state,
    );
}

fn dump_transformed_dae_for_diffsol(dae: &Dae, mass_matrix: &MassMatrix) {
    rumoca_sim::simulation::diagnostics::dump_transformed_dae_for_solver(dae, mass_matrix);
}

fn dump_initial_vector_for_diffsol(dae: &Dae, param_values: &[f64]) {
    let n_total = dae.f_x.len();
    let mut y0 = vec![0.0; n_total];
    problem::initialize_state_vector_with_params(dae, &mut y0, param_values);
    let mut names = build_output_names(dae);
    names.truncate(n_total);
    rumoca_sim::simulation::diagnostics::dump_initial_vector_for_solver(&names, &y0);
}

fn dump_initial_residual_summary_for_diffsol(
    dae: &Dae,
    n_x: usize,
    param_values: &[f64],
) -> Result<(), SimError> {
    if !sim_introspect_enabled() {
        return Ok(());
    }
    let n_total = dae.f_x.len();
    let mut y0 = vec![0.0; n_total];
    problem::initialize_state_vector_with_params(dae, &mut y0, param_values);
    dump_parameter_vector_for_diffsol(dae, param_values);
    let mut rhs = vec![0.0; n_total];
    let compiled_runtime = problem::build_compiled_runtime_newton_context(dae, n_total)?;
    problem::eval_compiled_runtime_residual(&compiled_runtime, &y0, param_values, 0.0, &mut rhs);
    rumoca_sim::simulation::diagnostics::dump_initial_residual_summary(dae, &rhs, n_x);
    Ok(())
}

pub(crate) fn dump_parameter_vector_for_diffsol(dae: &Dae, params: &[f64]) {
    rumoca_sim::simulation::diagnostics::dump_parameter_vector(dae, params);
}

/// Build a [`stepper::SimStepper`] from a DAE and options.
///
/// Lives here to access `pub(super)` internals (prepare_dae, solver construction, etc.).
#[allow(clippy::too_many_lines)]
pub(crate) fn build_stepper(
    dae: &Dae,
    opts: stepper::StepperOptions,
) -> Result<stepper::SimStepper, SimError> {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    use integration::{
        SolverLoopContext, StartupSyncInput, apply_initial_sections_and_sync_startup_state,
        build_compiled_discrete_event_context,
    };
    use rumoca_sim::equation_scalarize::build_output_names;
    use rumoca_sim::runtime::layout::SimulationContext;

    eval::clear_pre_values();
    rumoca_sim::runtime::clock::reset_runtime_clock_caches();
    let budget = TimeoutBudget::new(Some(30.0));

    validate_simulation_function_support(dae)?;
    let prepared = prepare_dae(dae, opts.scalarize, &budget)?;
    let mut dae = prepared.dae;
    let elim = prepared.elimination;
    let mass_matrix = prepared.mass_matrix;
    let ic_blocks = prepared.ic_blocks;

    if prepared.has_dummy_state {
        return Err(SimError::EmptySystem);
    }

    validate_simulation_function_support(&dae)?;

    let n_x: usize = dae.states.values().map(|v| v.size()).sum();
    let n_total = dae.f_x.len();
    let param_values = build_parameter_values(&dae, &budget)?;

    solve_initial_conditions(&mut dae, &ic_blocks, n_x, &param_values, opts.atol, &budget)?;

    let input_overrides: problem::SharedInputOverrides = Rc::new(RefCell::new(HashMap::new()));

    let mut problem_obj = problem::build_problem_with_overrides_and_params(
        &dae,
        opts.rtol,
        opts.atol,
        1e-8,
        &mass_matrix,
        &param_values,
        Some(input_overrides.clone()),
    )?;

    let sim_opts = SimOptions {
        t_start: 0.0,
        t_end: 1.0, // Initial horizon; overridden per-step
        rtol: opts.rtol,
        atol: opts.atol,
        dt: None,
        scalarize: opts.scalarize,
        max_wall_seconds: opts.max_wall_seconds_per_step,
        solver_mode: SimSolverMode::Bdf,
    };

    let startup_profile = SolverStartupProfile::Default;
    configure_solver_problem_with_profile(&mut problem_obj, &sim_opts, startup_profile);

    // Leak the problem to obtain a 'static reference. The solver borrows from
    // the problem, but the stepper needs to own the solver for an unbounded
    // lifetime.  The problem is allocated once per stepper and lives for the
    // program's duration (or until the stepper is dropped — a future
    // improvement could reclaim the memory via ManuallyDrop / raw pointers).
    let problem_ref: &'static _ = Box::leak(Box::new(problem_obj));
    let mut solver = problem_ref
        .bdf::<LS>()
        .map_err(|e| SimError::SolverError(format!("Failed to create BDF solver: {e}")))?;
    let compiled_runtime = problem::build_compiled_runtime_newton_context(&dae, n_total)?;
    let compiled_synthetic_root = problem::build_compiled_synthetic_root_context(&dae, n_total)?;

    apply_initial_sections_and_sync_startup_state(
        &mut solver,
        StartupSyncInput {
            dae: &dae,
            opts: &sim_opts,
            startup_profile,
            compiled_runtime: &compiled_runtime,
            param_values: &param_values,
            n_x,
            budget: &budget,
        },
    )?;

    let mut solver_names = build_output_names(&dae);
    solver_names.truncate(n_total);

    let sim_context = SimulationContext::from_dae(&dae, n_total);

    let compiled_discrete_event_ctx = build_compiled_discrete_event_context(&dae, n_total)?;
    let dae_ref: &'static Dae = Box::leak(Box::new(dae.clone()));
    let opts_ref: &'static SimOptions = Box::leak(Box::new(sim_opts.clone()));
    let budget_ref: &'static TimeoutBudget = Box::leak(Box::new(budget));

    let ctx = SolverLoopContext {
        dae: dae_ref,
        elim: elim.clone(),
        opts: opts_ref,
        startup_profile,
        n_x,
        param_values: param_values.clone(),
        compiled_runtime,
        compiled_synthetic_root,
        discrete_event_ctx: compiled_discrete_event_ctx,
        budget: budget_ref,
    };

    // Type-erased stepper inner
    #[allow(dead_code)]
    struct ConcreteInner<'a, Eqn, S>
    where
        Eqn: diffsol::OdeEquations<T = f64> + 'a,
        Eqn::V: diffsol::VectorHost<T = f64>,
        S: diffsol::OdeSolverMethod<'a, Eqn>,
    {
        solver: S,
        ctx: SolverLoopContext<'static>,
        _phantom: std::marker::PhantomData<&'a Eqn>,
    }

    #[allow(clippy::excessive_nesting)]
    impl<'a, Eqn, S> stepper::StepperInner for ConcreteInner<'a, Eqn, S>
    where
        Eqn: diffsol::OdeEquations<T = f64> + 'a,
        Eqn::V: diffsol::VectorHost<T = f64>,
        S: diffsol::OdeSolverMethod<'a, Eqn>,
    {
        fn step(&mut self, dt: f64, _dae: &Dae, budget: &TimeoutBudget) -> Result<(), SimError> {
            use diffsol::OdeSolverStopReason;

            if dt <= 0.0 {
                return Ok(());
            }

            let t_end = self.solver.state().t + dt;

            // Guard: if t_end is not ahead of the solver's current time
            // (due to floating point accumulation), skip this step.
            if t_end <= self.solver.state().t {
                return Ok(());
            }

            self.solver.set_stop_time(t_end).map_err(|e| {
                SimError::SolverError(format!("Failed to set stop time at t_end={t_end}: {e}"))
            })?;

            loop {
                budget.check()?;
                match self.solver.step() {
                    Ok(OdeSolverStopReason::TstopReached) => break,
                    Ok(OdeSolverStopReason::InternalTimestep) => {
                        if self.solver.state().t >= t_end {
                            break;
                        }
                        continue;
                    }
                    Ok(OdeSolverStopReason::RootFound(t_root)) => {
                        // SPEC_0003 / SPEC_0022 SIM-001/SIM-008:
                        // settle event updates on the right limit before
                        // continuous integration resumes after a root hit.
                        let _ = integration::apply_event_updates_at_time::<Eqn, S>(
                            &mut self.solver,
                            t_root,
                            &self.ctx,
                        )?;
                        if integration::stop_time_reached_with_tol(self.solver.state().t, t_end) {
                            break;
                        }
                        let _ = self.solver.set_stop_time(t_end);
                        continue;
                    }
                    Err(e) => {
                        return Err(SimError::SolverError(format!("Step failed: {e}")));
                    }
                }
            }
            Ok(())
        }

        fn time(&self) -> f64 {
            self.solver.state().t
        }

        fn solver_state_y(&self) -> Vec<f64> {
            self.solver.state().y.as_slice().to_vec()
        }

        fn reset_solver_history(&mut self) {
            let state = self.solver.state_mut();
            // Clear BDF polynomial history so stale extrapolation
            // from old inputs does not cause divergence.
            for ds in state.ds.iter_mut() {
                ds.as_mut_slice().fill(0.0);
            }
            for dsg in state.dsg.iter_mut() {
                dsg.as_mut_slice().fill(0.0);
            }
            state.dg.as_mut_slice().fill(0.0);
            // Shrink step size so the solver re-establishes stability
            // with the new inputs.
            let h = *state.h;
            if h.abs() > 1e-10 {
                *state.h = h.signum() * 1e-6;
            }
        }
    }

    /// Minimal SimulationBackend adapter for the stepper (no output recording).
    #[allow(dead_code)]
    struct BackendAdapter<'b, 'a, Eqn, S>
    where
        Eqn: diffsol::OdeEquations<T = f64> + 'a,
        Eqn::V: diffsol::VectorHost<T = f64>,
        S: diffsol::OdeSolverMethod<'a, Eqn>,
    {
        solver: &'b mut S,
        ctx: &'b SolverLoopContext<'static>,
        _phantom: std::marker::PhantomData<&'a Eqn>,
    }

    impl<'b, 'a, Eqn, S> rumoca_sim::SimulationBackend for BackendAdapter<'b, 'a, Eqn, S>
    where
        Eqn: diffsol::OdeEquations<T = f64> + 'a,
        Eqn::V: diffsol::VectorHost<T = f64>,
        S: diffsol::OdeSolverMethod<'a, Eqn>,
    {
        type Error = SimError;

        fn init(&mut self) -> Result<(), SimError> {
            Ok(())
        }

        fn step_until(&mut self, stop_time: f64) -> Result<rumoca_sim::StepUntilOutcome, SimError> {
            if integration::stop_time_reached_with_tol(self.solver.state().t, self.ctx.opts.t_end) {
                return Ok(rumoca_sim::StepUntilOutcome::Finished);
            }
            self.ctx.budget.check()?;
            integration::set_solver_stop_time::<Eqn, S>(
                self.solver,
                stop_time,
                self.ctx.budget,
                "stepper step_until",
            )
            .map_err(|e| SimError::SolverError(format!("Reset stop time: {e}")))?;

            match integration::step_with_stop_recovery::<Eqn, S>(
                self.solver,
                stop_time,
                self.ctx,
                |_msg, _t, _y| {},
            )? {
                integration::StepAdvance::Advanced(reason) => match reason {
                    diffsol::OdeSolverStopReason::RootFound(t_root) => {
                        Ok(rumoca_sim::StepUntilOutcome::RootFound { t_root })
                    }
                    diffsol::OdeSolverStopReason::TstopReached => {
                        Ok(rumoca_sim::StepUntilOutcome::StopReached)
                    }
                    diffsol::OdeSolverStopReason::InternalTimestep => {
                        Ok(rumoca_sim::StepUntilOutcome::InternalStep)
                    }
                },
                integration::StepAdvance::Recovered => {
                    Ok(rumoca_sim::StepUntilOutcome::StopReached)
                }
                integration::StepAdvance::Finished => Ok(rumoca_sim::StepUntilOutcome::Finished),
            }
        }

        fn read_state(&self) -> rumoca_sim::BackendState {
            rumoca_sim::BackendState {
                t: self.solver.state().t,
            }
        }

        fn apply_event_updates(&mut self, event_time: f64) -> Result<(), SimError> {
            let _ = integration::apply_event_updates_at_time::<Eqn, S>(
                self.solver,
                event_time,
                self.ctx,
            )?;
            Ok(())
        }
    }

    let inner = ConcreteInner {
        solver,
        ctx,
        _phantom: std::marker::PhantomData,
    };

    Ok(stepper::SimStepper {
        inner: Box::new(inner),
        dae,
        sim_context,
        param_values,
        input_overrides,
        n_x,
        n_total,
        solver_names,
        max_wall_seconds_per_step: opts.max_wall_seconds_per_step,
        elim,
        inputs_dirty: false,
    })
}

#[cfg(test)]
pub(crate) mod test_support;

#[cfg(test)]
mod tests;
