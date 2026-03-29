pub mod eliminate {
    use rumoca_ir_dae::Dae;

    pub(crate) type EliminationResult = rumoca_phase_solve::eliminate::EliminationResult;

    pub fn eliminate_trivial(dae: &mut Dae) -> rumoca_phase_solve::eliminate::EliminationResult {
        rumoca_phase_solve::eliminate::eliminate_trivial(dae)
    }
}
pub(crate) mod integration;
mod prepare;
pub mod problem;
pub mod stepper;

use std::collections::{HashMap, HashSet};

use diffsol::{
    FaerSparseLU, OdeEquations, OdeSolverMethod, OdeSolverProblem, OdeSolverStopReason, VectorHost,
};
use rumoca_ir_dae as dae;

pub(crate) type Dae = dae::Dae;
type BuiltinFunction = dae::BuiltinFunction;
type Expression = dae::Expression;
type Literal = dae::Literal;
type Subscript = dae::Subscript;
pub(crate) type VarName = dae::VarName;
type Variable = dae::Variable;

use rumoca_core::Span;

pub(crate) use crate::simulation::dae_prepare::*;
use crate::{
    SolverDeadlineGuard, TimeoutBudget, TimeoutExceeded, is_solver_timeout_panic, timeline,
};
pub(crate) use rumoca_core::{
    maybe_elapsed_seconds as trace_timer_elapsed_seconds,
    maybe_start_timer_if as trace_timer_start_if,
};
pub(crate) use rumoca_eval_dae::runtime::{self as eval, build_env, eval_expr};

pub(crate) type LS = FaerSparseLU<f64>;
use crate::equation_scalarize::build_output_names;
#[cfg(test)]
use crate::equation_scalarize::{build_complex_field_map, build_var_dims_map, index_into_expr};
#[cfg(test)]
use crate::projection_maps::{
    build_component_index_projection_map, build_function_output_projection_map,
};
pub(crate) use integration::map_solver_panic;
use integration::*;
use prepare::*;

/// Prepare a DAE for simulation/codegen using the same structural passes that
/// the diffsol runtime uses before integration.
///
/// This is useful for template backends (e.g. JS residual runtime) that want
/// to consume a structurally preprocessed DAE instead of raw compile output.
pub fn prepare_dae_for_template_codegen(dae: &Dae, scalarize: bool) -> Result<Dae, SimError> {
    let budget = TimeoutBudget::new(None);
    prepare_dae_for_template_codegen_only(dae, scalarize, &budget)
}

fn validate_simulation_function_support(dae: &Dae) -> Result<(), SimError> {
    crate::function_validation::validate_simulation_function_support(dae).map_err(|err| {
        SimError::UnsupportedFunction {
            name: err.name,
            reason: err.reason,
        }
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimSolverMode {
    Auto,
    Bdf,
    RkLike,
}

impl SimSolverMode {
    pub fn from_external_name(name: &str) -> Self {
        let lower = name.trim().to_ascii_lowercase();
        if lower.is_empty() {
            return Self::Bdf;
        }
        if lower == "auto" {
            return Self::Auto;
        }

        let normalized = lower.replace(['-', '_', ' '], "");
        let rk_like = normalized.contains("rungekutta")
            || normalized.starts_with("rk")
            || normalized.contains("dopri")
            || normalized.contains("esdirk")
            || normalized.contains("trbdf2")
            || normalized.contains("euler")
            || normalized.contains("midpoint");

        if rk_like { Self::RkLike } else { Self::Bdf }
    }
}

#[derive(Debug, Clone)]
pub struct SimOptions {
    pub t_start: f64,
    pub t_end: f64,
    pub rtol: f64,
    pub atol: f64,
    pub dt: Option<f64>,
    pub scalarize: bool,
    pub max_wall_seconds: Option<f64>,
    pub solver_mode: SimSolverMode,
}

impl Default for SimOptions {
    fn default() -> Self {
        Self {
            t_start: 0.0,
            t_end: 1.0,
            rtol: 1e-6,
            atol: 1e-6,
            dt: None,
            scalarize: true,
            max_wall_seconds: None,
            solver_mode: SimSolverMode::Auto,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SimResult {
    pub times: Vec<f64>,
    pub names: Vec<String>,
    pub data: Vec<Vec<f64>>,
    pub n_states: usize,
    pub variable_meta: Vec<SimVariableMeta>,
}

#[derive(Debug, Clone)]
pub struct SimVariableMeta {
    pub name: String,
    pub role: String,
    pub is_state: bool,
    pub value_type: Option<String>,
    pub variability: Option<String>,
    pub time_domain: Option<String>,
    pub unit: Option<String>,
    pub start: Option<String>,
    pub min: Option<String>,
    pub max: Option<String>,
    pub nominal: Option<String>,
    pub fixed: Option<bool>,
    pub description: Option<String>,
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

impl From<crate::simulation::runtime_prep::MassMatrixBuildError> for SimError {
    fn from(value: crate::simulation::runtime_prep::MassMatrixBuildError) -> Self {
        match value {
            crate::simulation::runtime_prep::MassMatrixBuildError::Timeout { seconds } => {
                Self::Timeout { seconds }
            }
            crate::simulation::runtime_prep::MassMatrixBuildError::NonDerivable {
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

struct VariableSource<'a> {
    var: &'a Variable,
    role: &'static str,
    is_state: bool,
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
    crate::run_timeout_step::<SimError, _>(budget, step)
}

pub(crate) fn run_timeout_step_result<F>(budget: &TimeoutBudget, step: F) -> Result<(), SimError>
where
    F: FnOnce() -> Result<(), SimError>,
{
    crate::run_timeout_step_result::<SimError, _>(budget, step)
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
    eval::clear_pre_values();

    for (sample_idx, &t_eval) in times.iter().enumerate() {
        let mut y = vec![0.0; solver_len];
        for (col_idx, series) in solver_data.iter().enumerate().take(solver_len) {
            if let Some(value) = series.get(sample_idx).copied() {
                y[col_idx] = value;
            }
        }
        let env = crate::settle_runtime_event_updates_default(crate::EventSettleInput {
            dae,
            y: &mut y,
            p: param_values,
            n_x,
            t_eval,
        });
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

fn lookup_variable_exact<'a>(dae: &'a Dae, name: &str) -> Option<VariableSource<'a>> {
    let key = VarName::new(name.to_string());
    if let Some(var) = dae.states.get(&key) {
        return Some(VariableSource {
            var,
            role: "state",
            is_state: true,
        });
    }
    if let Some(var) = dae.algebraics.get(&key) {
        return Some(VariableSource {
            var,
            role: "algebraic",
            is_state: false,
        });
    }
    if let Some(var) = dae.outputs.get(&key) {
        return Some(VariableSource {
            var,
            role: "output",
            is_state: false,
        });
    }
    if let Some(var) = dae.inputs.get(&key) {
        return Some(VariableSource {
            var,
            role: "input",
            is_state: false,
        });
    }
    if let Some(var) = dae.parameters.get(&key) {
        return Some(VariableSource {
            var,
            role: "parameter",
            is_state: false,
        });
    }
    if let Some(var) = dae.constants.get(&key) {
        return Some(VariableSource {
            var,
            role: "constant",
            is_state: false,
        });
    }
    if let Some(var) = dae.discrete_reals.get(&key) {
        return Some(VariableSource {
            var,
            role: "discrete-real",
            is_state: false,
        });
    }
    if let Some(var) = dae.discrete_valued.get(&key) {
        return Some(VariableSource {
            var,
            role: "discrete-valued",
            is_state: false,
        });
    }
    if let Some(var) = dae.derivative_aliases.get(&key) {
        return Some(VariableSource {
            var,
            role: "derivative-alias",
            is_state: false,
        });
    }
    None
}

fn trim_trailing_scalar_indices(name: &str) -> &str {
    let mut trimmed = name;
    loop {
        if !trimmed.ends_with(']') {
            break;
        }
        let Some(open_idx) = trimmed.rfind('[') else {
            break;
        };
        let index_text = &trimmed[(open_idx + 1)..(trimmed.len() - 1)];
        if index_text.is_empty() || !index_text.chars().all(|c| c.is_ascii_digit()) {
            break;
        }
        trimmed = &trimmed[..open_idx];
    }
    trimmed
}

fn lookup_variable_source<'a>(dae: &'a Dae, name: &str) -> Option<VariableSource<'a>> {
    lookup_variable_exact(dae, name).or_else(|| {
        let base = trim_trailing_scalar_indices(name);
        if base != name {
            lookup_variable_exact(dae, base)
        } else {
            None
        }
    })
}

fn format_meta_expr(expr: &Expression) -> String {
    truncate_debug(&format!("{expr:?}"), 160)
}

fn classify_role(role: &str, is_state: bool) -> (Option<String>, Option<String>, Option<String>) {
    if is_state {
        return (
            Some("Real".to_string()),
            Some("continuous".to_string()),
            Some("continuous-time".to_string()),
        );
    }

    match role {
        "algebraic" | "output" | "input" | "derivative-alias" => (
            Some("Real".to_string()),
            Some("continuous".to_string()),
            Some("continuous-time".to_string()),
        ),
        "parameter" => (
            Some("Real".to_string()),
            Some("parameter".to_string()),
            Some("static".to_string()),
        ),
        "constant" => (
            Some("Real".to_string()),
            Some("constant".to_string()),
            Some("static".to_string()),
        ),
        "discrete-real" => (
            Some("Real".to_string()),
            Some("discrete".to_string()),
            Some("event-discrete".to_string()),
        ),
        "discrete-valued" => (
            Some("Boolean/Integer/Enum".to_string()),
            Some("discrete".to_string()),
            Some("event-discrete".to_string()),
        ),
        _ => (None, None, None),
    }
}

fn build_variable_meta(dae: &Dae, names: &[String], n_states: usize) -> Vec<SimVariableMeta> {
    names
        .iter()
        .enumerate()
        .map(|(idx, name)| {
            if let Some(source) = lookup_variable_source(dae, name) {
                let (value_type, variability, time_domain) =
                    classify_role(source.role, source.is_state);
                SimVariableMeta {
                    name: name.clone(),
                    role: source.role.to_string(),
                    is_state: source.is_state,
                    value_type,
                    variability,
                    time_domain,
                    unit: source.var.unit.clone(),
                    start: source.var.start.as_ref().map(format_meta_expr),
                    min: source.var.min.as_ref().map(format_meta_expr),
                    max: source.var.max.as_ref().map(format_meta_expr),
                    nominal: source.var.nominal.as_ref().map(format_meta_expr),
                    fixed: source.var.fixed,
                    description: source.var.description.clone(),
                }
            } else {
                let inferred_is_state = idx < n_states;
                let inferred_role = if inferred_is_state {
                    "state"
                } else {
                    "unknown"
                };
                let (value_type, variability, time_domain) =
                    classify_role(inferred_role, inferred_is_state);
                SimVariableMeta {
                    name: name.clone(),
                    role: inferred_role.to_string(),
                    is_state: inferred_is_state,
                    value_type,
                    variability,
                    time_domain,
                    unit: None,
                    start: None,
                    min: None,
                    max: None,
                    nominal: None,
                    fixed: None,
                    description: None,
                }
            }
        })
        .collect()
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

fn build_output_times(t_start: f64, t_end: f64, dt: f64) -> Vec<f64> {
    let mut times = Vec::new();
    let mut t = t_start;
    while t <= t_end {
        times.push(t);
        t += dt;
    }
    if let Some(&last) = times.last()
        && (last - t_end).abs() > 1e-12
    {
        times.push(t_end);
    }
    times
}

pub(crate) fn build_parameter_values(
    dae: &Dae,
    budget: &TimeoutBudget,
) -> Result<Vec<f64>, SimError> {
    problem::default_params_with_budget(dae, budget)
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

pub(crate) type MassMatrix = crate::simulation::pipeline::MassMatrix;

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
    crate::simulation::diagnostics::sim_introspect_enabled()
}

pub(crate) fn sim_trace_enabled() -> bool {
    crate::simulation::diagnostics::sim_trace_enabled()
}

fn truncate_debug(s: &str, max_chars: usize) -> String {
    crate::simulation::diagnostics::truncate_debug(s, max_chars)
}

fn validate_no_initial_division_by_zero(
    dae: &Dae,
    t_start: f64,
    budget: &TimeoutBudget,
) -> Result<(), SimError> {
    let mut y0 = vec![0.0; dae.f_x.len()];
    problem::initialize_state_vector(dae, &mut y0);
    let p = build_parameter_values(dae, budget)?;
    let env = build_env(dae, &y0, &p, t_start);
    if let Some(site) =
        crate::simulation::diagnostics::find_initial_division_by_zero_site(dae, &env)
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
    crate::simulation::diagnostics::dump_missing_state_equation_diagnostics(dae, missing_state);
}

fn dump_transformed_dae_for_diffsol(dae: &Dae, mass_matrix: &MassMatrix) {
    crate::simulation::diagnostics::dump_transformed_dae_for_solver(dae, mass_matrix);
}

fn dump_initial_vector_for_diffsol(dae: &Dae) {
    let n_total = dae.f_x.len();
    let mut y0 = vec![0.0; n_total];
    problem::initialize_state_vector(dae, &mut y0);
    let mut names = build_output_names(dae);
    names.truncate(n_total);
    crate::simulation::diagnostics::dump_initial_vector_for_solver(&names, &y0);
}

fn dump_initial_residual_summary_for_diffsol(
    dae: &Dae,
    n_x: usize,
    budget: &TimeoutBudget,
) -> Result<(), SimError> {
    if !sim_introspect_enabled() {
        return Ok(());
    }
    let n_total = dae.f_x.len();
    let mut y0 = vec![0.0; n_total];
    problem::initialize_state_vector(dae, &mut y0);
    let p = build_parameter_values(dae, budget)?;
    dump_parameter_vector_for_diffsol(dae, &p);
    let mut rhs = vec![0.0; n_total];
    problem::eval_rhs_equations(dae, &y0, &p, 0.0, &mut rhs, n_x);
    crate::simulation::diagnostics::dump_initial_residual_summary(dae, &rhs, n_x);
    Ok(())
}

pub(crate) fn dump_parameter_vector_for_diffsol(dae: &Dae, params: &[f64]) {
    crate::simulation::diagnostics::dump_parameter_vector(dae, params);
}

fn trace_projection_failed_at_time(t: f64) {
    if sim_trace_enabled() {
        eprintln!("[sim-trace] no-state runtime projection failed at t={}", t);
    }
}

struct AlgebraicResultSetup {
    times: Vec<f64>,
    eval_times: Vec<f64>,
    y: Vec<f64>,
    param_values: Vec<f64>,
    n_x: usize,
    all_names: Vec<String>,
    visible_name_set: HashSet<String>,
    solver_name_to_idx: HashMap<String, usize>,
    requires_projection: bool,
}

fn prepare_algebraic_result_setup(
    dae: &Dae,
    opts: &SimOptions,
    elim: &eliminate::EliminationResult,
    budget: &TimeoutBudget,
) -> Result<AlgebraicResultSetup, SimError> {
    let dt = opts.dt.unwrap_or(opts.t_end / 500.0);
    let times = build_output_times(opts.t_start, opts.t_end, dt);
    let n_total = dae.f_x.len();
    let n_x: usize = dae.states.values().map(|v| v.size()).sum();

    let mut y = vec![0.0; n_total];
    problem::initialize_state_vector(dae, &mut y);

    let param_values = build_parameter_values(dae, budget)?;
    dump_parameter_vector_for_diffsol(dae, &param_values);
    let clock_events =
        timeline::collect_periodic_clock_events(&dae.clock_schedules, opts.t_start, opts.t_end);
    if sim_introspect_enabled() {
        let preview: Vec<f64> = clock_events.iter().copied().take(12).collect();
        eprintln!(
            "[sim-introspect] no-state clock events count={} preview={:?}",
            clock_events.len(),
            preview
        );
    }
    let eval_times = timeline::merge_evaluation_times(&times, &clock_events);
    let visible_names = build_visible_result_names(dae);
    let mut all_names = visible_names.clone();
    all_names.extend(crate::collect_reconstruction_discrete_context_names(
        dae, elim, &all_names,
    ));
    let visible_name_set: HashSet<String> = visible_names
        .iter()
        .filter(|name| *name != DUMMY_STATE_NAME)
        .cloned()
        .collect();
    let mut solver_names = visible_names.clone();
    solver_names.truncate(n_total);
    let solver_name_to_idx: HashMap<String, usize> = solver_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect();
    let requires_projection = problem::runtime_projection_required(dae, n_x);
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] no-state runtime projection required={}",
            requires_projection
        );
    }

    Ok(AlgebraicResultSetup {
        times,
        eval_times,
        y,
        param_values,
        n_x,
        all_names,
        visible_name_set,
        solver_name_to_idx,
        requires_projection,
    })
}

fn collect_no_state_sample_data(
    dae: &Dae,
    opts: &SimOptions,
    elim: &eliminate::EliminationResult,
    budget: &TimeoutBudget,
    setup: &AlgebraicResultSetup,
) -> Result<Vec<Vec<f64>>, SimError> {
    let sample_ctx = crate::NoStateSampleContext {
        dae,
        elim,
        param_values: &setup.param_values,
        all_names: &setup.all_names,
        solver_name_to_idx: &setup.solver_name_to_idx,
        n_x: setup.n_x,
        t_start: opts.t_start,
        requires_projection: setup.requires_projection,
    };

    let (_, data) = crate::collect_algebraic_samples(
        &sample_ctx,
        &setup.times,
        &setup.eval_times,
        setup.y.clone(),
        || budget.check().map_err(SimError::from),
        |y_values, t, do_projection| {
            if do_projection {
                let projection = problem::project_algebraics_with_fixed_states_at_time(
                    dae,
                    y_values,
                    setup.n_x,
                    t,
                    opts.atol.max(1.0e-8),
                    budget,
                )?;
                if let Some(projected) = projection {
                    *y_values = projected;
                    return Ok(());
                }
                trace_projection_failed_at_time(t);
            }
            let _ = problem::seed_runtime_direct_assignments(
                dae,
                y_values,
                &setup.param_values,
                setup.n_x,
                t,
            );
            Ok(())
        },
    )
    .map_err(|err| match err {
        crate::NoStateSampleError::Callback(sim_err) => sim_err,
        crate::NoStateSampleError::SampleScheduleMismatch { captured, expected } => {
            SimError::SolverError(format!(
                "no-state sample schedule mismatch: captured {captured}/{expected} output samples"
            ))
        }
    })?;

    Ok(data)
}

fn filter_visible_output_series(
    recon_names: &[String],
    recon_data: &[Vec<f64>],
    visible_name_set: &HashSet<String>,
) -> (Vec<String>, Vec<Vec<f64>>) {
    let mut final_names: Vec<String> = Vec::new();
    let mut final_data: Vec<Vec<f64>> = Vec::new();
    for (name, series) in recon_names.iter().zip(recon_data.iter()) {
        if visible_name_set.contains(name) {
            final_names.push(name.clone());
            final_data.push(series.clone());
        }
    }
    (final_names, final_data)
}

fn build_algebraic_result(
    dae: &Dae,
    opts: &SimOptions,
    elim: &eliminate::EliminationResult,
    budget: &TimeoutBudget,
) -> Result<SimResult, SimError> {
    let setup = prepare_algebraic_result_setup(dae, opts, elim, budget)?;
    let data = collect_no_state_sample_data(dae, opts, elim, budget, &setup)?;
    let (recon_names, recon_data, final_n_states) =
        crate::finalize_algebraic_outputs(setup.all_names, data, setup.n_x, DUMMY_STATE_NAME);
    let (mut final_names, mut final_data) =
        filter_visible_output_series(&recon_names, &recon_data, &setup.visible_name_set);

    if !elim.substitutions.is_empty() {
        let (extra_names, extra_data) = crate::reconstruct::reconstruct_eliminated(
            elim,
            dae,
            &setup.param_values,
            &setup.times,
            &recon_names,
            &recon_data,
        );
        final_names.extend(extra_names);
        final_data.extend(extra_data);
    }

    let variable_meta = build_variable_meta(dae, &final_names, final_n_states);
    Ok(SimResult {
        times: setup.times,
        names: final_names,
        data: final_data,
        n_states: final_n_states,
        variable_meta,
    })
}

fn run_with_timeout_panic_handling<T, F>(budget: &TimeoutBudget, f: F) -> Result<T, SimError>
where
    F: FnOnce() -> Result<T, SimError>,
{
    let _solver_deadline_guard = SolverDeadlineGuard::install(budget.deadline());
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(f)) {
        Ok(result) => result,
        Err(payload) => {
            if is_solver_timeout_panic(payload.as_ref()) {
                return Err(budget.timeout_error().into());
            }
            Err(SimError::SolverError(format!(
                "integration panic: {}",
                panic_payload_message(payload)
            )))
        }
    }
}

fn finalize_dynamic_result(
    dae: &Dae,
    elim: &eliminate::EliminationResult,
    param_values: &[f64],
    n_x: usize,
    n_total: usize,
    buf: OutputBuffers,
) -> SimResult {
    let mut names = build_output_names(dae);
    names.truncate(n_total);
    let solver_names = names.clone();
    let OutputBuffers {
        times: output_times,
        data: output_data,
        n_total: _,
        runtime_names,
        runtime_data,
    } = buf;
    let (mut final_names, mut final_data, final_n_states) = (names, output_data, n_x);
    let runtime_capture_complete =
        !runtime_names.is_empty() && runtime_data.iter().all(|s| s.len() == output_times.len());
    if runtime_capture_complete {
        merge_runtime_discrete_channels(
            &mut final_names,
            &mut final_data,
            runtime_names,
            runtime_data,
        );
    }
    let (discrete_names, discrete_data) = evaluate_runtime_discrete_channels(
        dae,
        n_x,
        param_values,
        &output_times,
        &solver_names,
        &final_data,
    );
    merge_runtime_discrete_channels(
        &mut final_names,
        &mut final_data,
        discrete_names,
        discrete_data,
    );
    if !elim.substitutions.is_empty() {
        let (extra_names, extra_data) = crate::reconstruct::reconstruct_eliminated(
            elim,
            dae,
            param_values,
            &output_times,
            &final_names,
            &final_data,
        );
        final_names.extend(extra_names);
        final_data.extend(extra_data);
    }
    let variable_meta = build_variable_meta(dae, &final_names, final_n_states);
    SimResult {
        times: output_times,
        names: final_names,
        data: final_data,
        n_states: final_n_states,
        variable_meta,
    }
}

pub fn simulate(dae: &Dae, opts: &SimOptions) -> Result<SimResult, SimError> {
    eval::clear_pre_values();
    let budget = TimeoutBudget::new(opts.max_wall_seconds);
    validate_simulation_function_support(dae)?;
    let sim_start = trace_timer_start_if(sim_trace_enabled());
    let prepared = prepare_dae(dae, opts.scalarize, &budget)?;
    let mut dae = prepared.dae;
    let has_dummy = prepared.has_dummy_state;
    let elim = prepared.elimination;
    let ic_blocks = prepared.ic_blocks;
    let mass_matrix = prepared.mass_matrix;
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] stage prepare_dae {:.3}s",
            trace_timer_elapsed_seconds(sim_start)
        );
    }
    validate_simulation_function_support(&dae)?;
    dump_transformed_dae_for_diffsol(&dae, &mass_matrix);

    let n_x: usize = dae.states.values().map(|v| v.size()).sum();
    if has_dummy {
        return run_timeout_result(&budget, || {
            build_algebraic_result(&dae, opts, &elim, &budget)
        });
    }
    let n_total = dae.f_x.len();

    solve_initial_conditions(&mut dae, &ic_blocks, n_x, opts.atol, &budget)?;
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] stage solve_initial_conditions {:.3}s",
            trace_timer_elapsed_seconds(sim_start)
        );
    }
    validate_no_initial_division_by_zero(&dae, opts.t_start, &budget)?;
    dump_initial_vector_for_diffsol(&dae);
    dump_initial_residual_summary_for_diffsol(&dae, n_x, &budget)?;

    let (buf, param_values) = run_with_timeout_panic_handling(&budget, || {
        integrate_with_fallbacks(&dae, opts, n_total, &mass_matrix, &budget)
    })?;
    if sim_trace_enabled() {
        eprintln!(
            "[sim-trace] stage integrate_with_fallbacks {:.3}s",
            trace_timer_elapsed_seconds(sim_start)
        );
    }
    Ok(finalize_dynamic_result(
        &dae,
        &elim,
        &param_values,
        n_x,
        n_total,
        buf,
    ))
}

/// Build a [`stepper::SimStepper`] from a DAE and options.
///
/// Lives here to access `pub(super)` internals (prepare_dae, solver construction, etc.).
pub(crate) fn build_stepper(
    dae: &Dae,
    opts: stepper::StepperOptions,
) -> Result<stepper::SimStepper, SimError> {
    use std::cell::RefCell;
    use std::collections::HashMap;
    use std::rc::Rc;

    use crate::equation_scalarize::build_output_names;
    use crate::runtime::layout::SimulationContext;
    use integration::{
        SolverLoopContext, apply_initial_sections_and_sync_startup_state,
        build_compiled_discrete_event_context,
    };

    eval::clear_pre_values();
    let budget = TimeoutBudget::new(Some(30.0));

    validate_simulation_function_support(dae)?;
    let prepared = prepare_dae(dae, opts.scalarize, &budget)?;
    let mut dae = prepared.dae;
    let mass_matrix = prepared.mass_matrix;
    let ic_blocks = prepared.ic_blocks;

    if prepared.has_dummy_state {
        return Err(SimError::EmptySystem);
    }

    validate_simulation_function_support(&dae)?;

    let n_x: usize = dae.states.values().map(|v| v.size()).sum();
    let n_total = dae.f_x.len();

    solve_initial_conditions(&mut dae, &ic_blocks, n_x, opts.atol, &budget)?;

    let input_overrides: problem::SharedInputOverrides = Rc::new(RefCell::new(HashMap::new()));

    let param_values = build_parameter_values(&dae, &budget)?;

    let mut problem_obj = problem::build_problem_with_overrides(
        &dae,
        opts.rtol,
        opts.atol,
        1e-8,
        &mass_matrix,
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

    apply_initial_sections_and_sync_startup_state(
        &mut solver,
        &dae,
        &sim_opts,
        startup_profile,
        &param_values,
        n_x,
        &budget,
    )?;

    let mut solver_names = build_output_names(&dae);
    solver_names.truncate(n_total);

    let sim_context = SimulationContext::from_dae(&dae, n_total);

    let compiled_discrete_event_ctx = build_compiled_discrete_event_context(&dae, n_total)?;

    let ctx = SolverLoopContext {
        dae: dae.clone(),
        opts: sim_opts,
        startup_profile,
        n_x,
        param_values: param_values.clone(),
        discrete_event_ctx: compiled_discrete_event_ctx,
        budget,
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
        ctx: SolverLoopContext,
        _phantom: std::marker::PhantomData<&'a Eqn>,
    }

    impl<'a, Eqn, S> stepper::StepperInner for ConcreteInner<'a, Eqn, S>
    where
        Eqn: diffsol::OdeEquations<T = f64> + 'a,
        Eqn::V: diffsol::VectorHost<T = f64>,
        S: diffsol::OdeSolverMethod<'a, Eqn>,
    {
        fn step(&mut self, dt: f64, _dae: &Dae, budget: &TimeoutBudget) -> Result<(), SimError> {
            use diffsol::OdeSolverStopReason;

            let t_end = self.solver.state().t + dt;

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
                    Ok(OdeSolverStopReason::RootFound(_)) => {
                        // Root events (from conditional expressions) — continue
                        // but check if we've reached the target time.
                        if self.solver.state().t >= t_end {
                            break;
                        }
                        // Reset stop time after root handling
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
        ctx: &'b SolverLoopContext,
        _phantom: std::marker::PhantomData<&'a Eqn>,
    }

    impl<'b, 'a, Eqn, S> crate::SimulationBackend for BackendAdapter<'b, 'a, Eqn, S>
    where
        Eqn: diffsol::OdeEquations<T = f64> + 'a,
        Eqn::V: diffsol::VectorHost<T = f64>,
        S: diffsol::OdeSolverMethod<'a, Eqn>,
    {
        type Error = SimError;

        fn init(&mut self) -> Result<(), SimError> {
            Ok(())
        }

        fn step_until(&mut self, stop_time: f64) -> Result<crate::StepUntilOutcome, SimError> {
            if integration::stop_time_reached_with_tol(self.solver.state().t, self.ctx.opts.t_end) {
                return Ok(crate::StepUntilOutcome::Finished);
            }
            self.ctx.budget.check()?;
            integration::set_solver_stop_time::<Eqn, S>(
                self.solver,
                stop_time,
                &self.ctx.budget,
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
                        Ok(crate::StepUntilOutcome::RootFound { t_root })
                    }
                    diffsol::OdeSolverStopReason::TstopReached => {
                        Ok(crate::StepUntilOutcome::StopReached)
                    }
                    diffsol::OdeSolverStopReason::InternalTimestep => {
                        Ok(crate::StepUntilOutcome::InternalStep)
                    }
                },
                integration::StepAdvance::Recovered => Ok(crate::StepUntilOutcome::StopReached),
                integration::StepAdvance::Finished => Ok(crate::StepUntilOutcome::Finished),
            }
        }

        fn read_state(&self) -> crate::BackendState {
            crate::BackendState {
                t: self.solver.state().t,
            }
        }

        fn apply_event_updates(&mut self, event_time: f64) -> Result<(), SimError> {
            integration::apply_event_updates_at_time::<Eqn, S>(self.solver, event_time, self.ctx)
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
    })
}

#[cfg(test)]
pub(crate) mod test_support;

#[cfg(test)]
mod tests;
