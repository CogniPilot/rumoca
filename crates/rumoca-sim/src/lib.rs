//! High-level simulation facade for Rumoca.
//!
//! Re-exports the primitives crate `rumoca-solver` plus, when the
//! corresponding features are enabled, the diffsol/rk45 solver entry points
//! and the scheduled simulation module that drives scheduled scenario simulations.

use indexmap::IndexSet;
use rumoca_ir_solve as solve;
use serde::{Deserialize, Serialize};

/// NaN / non-finite runtime tracing, exposed through the sim facade so the CLI
/// (and library users) can switch it on without an environment variable. See
/// [`rumoca_eval_solve::nan_trace`].
pub use rumoca_eval_solve::nan_trace;
use rumoca_ir_dae as dae;
pub use rumoca_phase_solve::{lower_solve_artifacts, lower_solve_problem};
pub use rumoca_solver::{
    BackendState, DiffsolMethod, LoopStats, RuntimeProgressSnapshot, RuntimeStopSchedule,
    RuntimeTraceContext, SimBackend, SimOptions, SimPacingMode, SimResult, SimSolverMode,
    SimVariableMeta, SimulationBackend, SimulationRequestSummary, SimulationRunMetrics,
    SolverDeadlineGuard, StepUntilOutcome, TimeoutBudget, TimeoutExceeded,
    build_simulation_metrics_value, build_simulation_payload, is_solver_timeout_panic,
    panic_on_expired_solver_deadline, run_timeout_result, run_timeout_step,
    run_timeout_step_result, run_with_runtime_schedule, runtime_progress_snapshot,
    stop_time_reached_with_tol, time_advanced_with_tol, time_match_with_tol, trace_runtime_done,
    trace_runtime_progress, trace_runtime_start, trace_runtime_step_fail, trace_runtime_timeout,
};

mod build_timing;
pub mod bulk;
pub mod row_eval_trace;
pub mod sim_trace_compare;
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
mod simulation_session;
#[cfg(feature = "scheduled-sim")]
mod simulation_session_api;

#[cfg(feature = "solver-diffsol")]
mod diffsol;
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
mod prepared_vectors;
mod solve_lowering;
pub use build_timing::BuildSimulationTimings;
#[cfg(feature = "solver-diffsol")]
pub use diffsol::{
    PreparedSimulation, SimError, build_simulation, build_simulation_with_stage_timing,
    build_simulation_with_stage_timing_and_solve_model, check_initialization,
    check_prepared_initialization, run_prepared_simulation, simulate, simulate_dae,
};
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub use prepared_vectors::{PreparedVectorError, refresh_prepared_vectors};
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub use simulation_session::{SessionState, SimulationSession};
#[cfg(feature = "scheduled-sim")]
pub(crate) use simulation_session_api::SimulationSessionApi;
// The inspection/debug facade (probes + their named report types) is surfaced
// through `solve_lowering` so the root stays a curated same-crate facade; the
// report types are re-exported from there rather than as root cross-crate uses.
pub use solve_lowering::{
    BlockReport, EvalAtProbe, EvalAtReport, EvalAtSlot, JacobianProbe, JacobianReport,
    ObjectiveGradientProbe, ParameterJacobianProbe, SimulationDiagnosticError,
    SingularityDiagnosis, StateAndParameterJacobianProbe, SteadyStateSensitivityProbe,
    StructuralReport, TearingReport, UnmatchedEquationDiagnosis, UnmatchedUnknownDiagnosis,
    diagnose_structural_singularity, eval_dae_at, jacobian_for_dae, lower_dae_for_gpu_preparation,
    lower_dae_for_simulation, lower_for_differentiation_with_overrides,
    lower_for_simulation_with_overrides, parameter_jacobian_for_dae,
    state_and_parameter_jacobian_for_dae, steady_state_adjoint_objective_gradient_for_dae,
    steady_state_objective_gradient_for_dae, steady_state_parameter_sensitivity_for_dae,
    structural_report_for_dae, structurally_lowered_dae_for_simulation_artifact,
};

#[cfg(feature = "scenario-config")]
pub mod scenario_config;

#[cfg(feature = "solver-rk45")]
pub mod rk45;

#[cfg(all(
    feature = "scheduled-sim",
    feature = "scenario-config",
    feature = "input-keyboard",
    feature = "transport-udp",
    feature = "transport-zenoh",
    feature = "viewer-web",
    feature = "process-control"
))]
pub mod scheduled_sim;

#[cfg(feature = "report")]
pub mod report;

#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub fn simulate_with_diagnostics(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    match opts.solver_mode {
        SimSolverMode::Auto => simulate_with_auto_diagnostics(dae_model, opts),
        SimSolverMode::RkLike => simulate_with_rk45_diagnostics(dae_model, opts),
        SimSolverMode::Bdf => simulate_with_diffsol_diagnostics(dae_model, opts),
    }
}

#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub use simulate_with_diagnostics as simulate_dae_with_diagnostics;

/// Simulate an already-lowered [`rumoca_ir_solve::SolveModel`], skipping the
/// DAE→solve lowering, dispatching by `opts.solver_mode`. This is the
/// runtime-only entry the lazy diffsol WASM addon uses: the main module emits a
/// SolveModel, the addon deserializes and simulates it without carrying the
/// compiler. The skipped (`#[serde(skip)]`) layout fields are lowering-only and
/// not read here, so a serialized→deserialized SolveModel simulates identically
/// (pinned by `solve_model_round_trip` in crates/rumoca/tests).
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub fn simulate_solve_model(
    model: &rumoca_ir_solve::SolveModel,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    match opts.solver_mode {
        SimSolverMode::Auto => simulate_solve_model_auto(model, opts),
        SimSolverMode::RkLike => simulate_solve_model_rk45(model, opts),
        SimSolverMode::Bdf => simulate_solve_model_diffsol(model, opts),
    }
}

#[cfg(feature = "solver-diffsol")]
fn simulate_solve_model_auto(
    model: &rumoca_ir_solve::SolveModel,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    simulate_solve_model_diffsol(model, opts)
}

#[cfg(all(not(feature = "solver-diffsol"), feature = "solver-rk45"))]
fn simulate_solve_model_auto(
    model: &rumoca_ir_solve::SolveModel,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    simulate_solve_model_rk45(model, opts)
}

#[cfg(feature = "solver-rk45")]
fn simulate_solve_model_rk45(
    model: &rumoca_ir_solve::SolveModel,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    rumoca_solver_rk45::simulate(model, opts)
        .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))
}

#[cfg(all(
    any(feature = "solver-diffsol", feature = "solver-rk45"),
    not(feature = "solver-rk45")
))]
fn simulate_solve_model_rk45(
    _model: &rumoca_ir_solve::SolveModel,
    _opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    Err(SimulationDiagnosticError::Solver(
        "rk-like solver requested, but this build does not include the rk45 backend".to_string(),
    ))
}

#[cfg(feature = "solver-diffsol")]
fn simulate_solve_model_diffsol(
    model: &rumoca_ir_solve::SolveModel,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    rumoca_solver_diffsol::simulate(model, opts)
        .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))
}

#[cfg(all(
    any(feature = "solver-diffsol", feature = "solver-rk45"),
    not(feature = "solver-diffsol")
))]
fn simulate_solve_model_diffsol(
    _model: &rumoca_ir_solve::SolveModel,
    _opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    Err(SimulationDiagnosticError::Solver(
        "bdf/diffsol solver requested, but this build does not include the diffsol backend"
            .to_string(),
    ))
}

/// Simulate, and if it fails with an error that suggests a non-finite
/// (`NaN`/`inf`) value, automatically re-run once with NaN tracing enabled so
/// the offending model variable(s) are reported — turning an opaque
/// "step size too small" into an actionable diagnostic. Intended for
/// scheduled single-model use (the CLI); bulk callers should use
/// [`simulate_with_diagnostics`] to avoid the retry cost.
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub fn simulate_with_diagnostics_auto_nan_trace(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    let result = simulate_with_diagnostics(dae_model, opts);
    if let Err(error) = &result
        && !nan_trace::nan_trace_enabled()
        && nan_trace::error_suggests_nonfinite(&error.to_string())
    {
        eprintln!(
            "note: simulation failed with a possible non-finite (NaN/inf) value; \
             re-running with NaN tracing to locate the offending variable(s)..."
        );
        nan_trace::set_nan_trace(true);
        let _ = simulate_with_diagnostics(dae_model, opts);
        nan_trace::set_nan_trace(false);
    }
    result
}

#[cfg(feature = "solver-diffsol")]
fn simulate_with_auto_diagnostics(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    simulate_with_diffsol_diagnostics(dae_model, opts)
}

#[cfg(all(not(feature = "solver-diffsol"), feature = "solver-rk45"))]
fn simulate_with_auto_diagnostics(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    simulate_with_rk45_diagnostics(dae_model, opts)
}

#[cfg(all(test, not(feature = "solver-diffsol"), feature = "solver-rk45"))]
mod solver_mode_tests {
    use super::*;

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("solver_mode_test.mo"),
            0,
            8,
        )
    }

    #[test]
    fn auto_mode_uses_rk45_when_diffsol_is_not_built() {
        let span = test_span();
        let mut model = dae::Dae::new();
        model.variables.states.insert(
            rumoca_core::VarName::new("x"),
            dae::Variable::new(
                rumoca_core::VarName::new("x"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
        model.continuous.equations.push(dae::Equation {
            lhs: None,
            rhs: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Der,
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("x"),
                        subscripts: Vec::new(),
                        span,
                    }],
                    span,
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span,
                }),
                span,
            },
            span,
            origin: "test".to_string(),
            scalar_count: 1,
        });
        let result = simulate_with_diagnostics(
            &model,
            &SimOptions {
                solver_mode: SimSolverMode::Auto,
                t_end: 0.01,
                dt: Some(0.01),
                ..Default::default()
            },
        );
        assert!(
            !matches!(
                result,
                Err(SimulationDiagnosticError::Solver(ref message))
                    if message.contains("diffsol backend")
            ),
            "auto mode incorrectly selected diffsol stub: {result:?}"
        );
    }
}

#[cfg(feature = "solver-rk45")]
fn simulate_with_rk45_diagnostics(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    rk45::simulate_with_diagnostics(dae_model, opts)
}

#[cfg(all(
    any(feature = "solver-diffsol", feature = "solver-rk45"),
    not(feature = "solver-rk45")
))]
fn simulate_with_rk45_diagnostics(
    _dae_model: &dae::Dae,
    _opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    Err(SimulationDiagnosticError::Solver(
        "rk-like solver requested, but this build does not include the rk45 backend".to_string(),
    ))
}

#[cfg(feature = "solver-diffsol")]
fn simulate_with_diffsol_diagnostics(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    diffsol::simulate_with_diagnostics(dae_model, opts)
}

#[cfg(all(
    any(feature = "solver-diffsol", feature = "solver-rk45"),
    not(feature = "solver-diffsol")
))]
fn simulate_with_diffsol_diagnostics(
    _dae_model: &dae::Dae,
    _opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    Err(SimulationDiagnosticError::Solver(
        "bdf solver requested, but this build does not include the diffsol backend".to_string(),
    ))
}

#[cfg(feature = "report")]
pub mod web;

struct VariableSource<'a> {
    var: &'a dae::Variable,
    role: &'static str,
    is_state: bool,
}

fn lookup_variable_exact<'a>(dae_model: &'a dae::Dae, name: &str) -> Option<VariableSource<'a>> {
    let key = rumoca_core::VarName::new(name);
    if let Some(var) = dae_model.variables.states.get(&key) {
        return Some(VariableSource {
            var,
            role: "state",
            is_state: true,
        });
    }
    if let Some(var) = dae_model.variables.algebraics.get(&key) {
        return Some(VariableSource {
            var,
            role: "algebraic",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.variables.outputs.get(&key) {
        return Some(VariableSource {
            var,
            role: "output",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.variables.inputs.get(&key) {
        return Some(VariableSource {
            var,
            role: "input",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.variables.parameters.get(&key) {
        return Some(VariableSource {
            var,
            role: "parameter",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.variables.constants.get(&key) {
        return Some(VariableSource {
            var,
            role: "constant",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.variables.discrete_reals.get(&key) {
        return Some(VariableSource {
            var,
            role: "discrete-real",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.variables.discrete_valued.get(&key) {
        return Some(VariableSource {
            var,
            role: "discrete-valued",
            is_state: false,
        });
    }
    None
}

fn trim_trailing_scalar_indices(name: &str) -> &str {
    let mut trimmed = name;
    while let Some((base, index_text)) = rumoca_core::split_trailing_subscript_suffix(trimmed) {
        if index_text.is_empty() || !index_text.chars().all(|c| c.is_ascii_digit()) {
            break;
        }
        trimmed = base;
    }
    trimmed
}

fn lookup_variable_source<'a>(dae_model: &'a dae::Dae, name: &str) -> Option<VariableSource<'a>> {
    lookup_variable_exact(dae_model, name).or_else(|| {
        let base = trim_trailing_scalar_indices(name);
        if base != name {
            lookup_variable_exact(dae_model, base)
        } else {
            None
        }
    })
}

fn truncate_meta_expr(expr: &rumoca_core::Expression) -> String {
    let rendered = format!("{expr:?}");
    if rendered.len() <= 160 {
        rendered
    } else {
        format!("{}...", &rendered[..160])
    }
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
        "algebraic" | "output" | "input" => (
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

pub fn build_variable_meta(
    dae_model: &dae::Dae,
    names: &[String],
    n_states: usize,
) -> Vec<SimVariableMeta> {
    names
        .iter()
        .enumerate()
        .map(|(idx, name)| {
            if let Some(source) = lookup_variable_source(dae_model, name) {
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
                    start: source.var.start.as_ref().map(truncate_meta_expr),
                    min: source.var.min.as_ref().map(truncate_meta_expr),
                    max: source.var.max.as_ref().map(truncate_meta_expr),
                    nominal: source.var.nominal.as_ref().map(truncate_meta_expr),
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

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TunableParameterMeta {
    pub name: String,
    pub default_value: f64,
    pub unit: Option<String>,
    pub start: Option<String>,
    pub min: Option<String>,
    pub max: Option<String>,
    pub nominal: Option<String>,
    pub min_value: Option<f64>,
    pub max_value: Option<f64>,
    pub fixed: Option<bool>,
    pub description: Option<String>,
}

pub fn build_tunable_parameter_meta(
    dae_model: &dae::Dae,
    solve_model: &solve::SolveModel,
) -> Vec<TunableParameterMeta> {
    solve_model
        .problem
        .layout
        .bindings()
        .iter()
        .filter_map(|(name, slot)| {
            let solve::ScalarSlot::P { index, .. } = *slot else {
                return None;
            };
            let source = lookup_variable_source(dae_model, name)?;
            if source.role != "parameter" || !source.var.is_tunable {
                return None;
            }
            Some(TunableParameterMeta {
                name: name.to_string(),
                default_value: solve_model.parameters.get(index).copied().unwrap_or(0.0),
                unit: source.var.unit.clone(),
                start: source.var.start.as_ref().map(truncate_meta_expr),
                min: source.var.min.as_ref().map(truncate_meta_expr),
                max: source.var.max.as_ref().map(truncate_meta_expr),
                nominal: source.var.nominal.as_ref().map(truncate_meta_expr),
                min_value: source.var.min.as_ref().and_then(numeric_expression_value),
                max_value: source.var.max.as_ref().and_then(numeric_expression_value),
                fixed: source.var.fixed,
                description: source.var.description.clone(),
            })
        })
        .collect()
}

fn numeric_expression_value(expr: &rumoca_core::Expression) -> Option<f64> {
    match expr {
        rumoca_core::Expression::Literal { value, .. } => match value {
            rumoca_core::Literal::Real(value) => Some(*value),
            rumoca_core::Literal::Integer(value) => Some(*value as f64),
            _ => None,
        },
        rumoca_core::Expression::Unary { op, rhs, .. } => {
            let value = numeric_expression_value(rhs)?;
            match op {
                rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => Some(-value),
                rumoca_core::OpUnary::Plus
                | rumoca_core::OpUnary::DotPlus
                | rumoca_core::OpUnary::Empty => Some(value),
                rumoca_core::OpUnary::Not => None,
            }
        }
        _ => None,
    }
}

pub fn runtime_defined_unknown_names(dae_model: &dae::Dae) -> IndexSet<String> {
    rumoca_phase_structural::runtime_defined_unknown_names(dae_model)
}

pub fn runtime_defined_continuous_unknown_names(dae_model: &dae::Dae) -> IndexSet<String> {
    rumoca_phase_structural::runtime_defined_continuous_unknown_names(dae_model)
}

pub fn compiled_layout_binding_debug(
    dae_model: &dae::Dae,
    name: &str,
) -> Result<Option<String>, rumoca_phase_solve::LowerError> {
    let layout = rumoca_phase_solve::build_var_layout(dae_model)?;
    Ok(layout.binding(name).map(|slot| format!("{slot:?}")))
}

pub fn compiled_layout_related_bindings_debug(
    dae_model: &dae::Dae,
    prefix: &str,
) -> Result<Vec<(String, String)>, rumoca_phase_solve::LowerError> {
    let layout = rumoca_phase_solve::build_var_layout(dae_model)?;
    Ok(layout
        .bindings()
        .iter()
        .filter(|(binding_name, _)| {
            binding_name.starts_with(prefix) && binding_name.as_str() != prefix
        })
        .map(|(binding_name, slot)| (binding_name.to_string(), format!("{slot:?}")))
        .collect())
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_dae::Variable;

    #[test]
    fn trim_trailing_scalar_indices_uses_balanced_trailing_groups() {
        assert_eq!(trim_trailing_scalar_indices("x[1]"), "x");
        assert_eq!(trim_trailing_scalar_indices("x[1][2]"), "x");
        assert_eq!(trim_trailing_scalar_indices("x[1,2]"), "x[1,2]");
        assert_eq!(
            trim_trailing_scalar_indices("record[1].field[2]"),
            "record[1].field"
        );
        assert_eq!(
            trim_trailing_scalar_indices("record[index.re]"),
            "record[index.re]"
        );
    }

    #[test]
    fn build_variable_meta_resolves_scalarized_names_back_to_array_variable() {
        let mut dae_model = dae::Dae::default();
        let mut state = Variable::new(
            rumoca_core::VarName::new("x"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        state.dims = vec![2];
        state.unit = Some("m".to_string());
        dae_model
            .variables
            .states
            .insert(rumoca_core::VarName::new("x"), state);

        let meta = build_variable_meta(&dae_model, &["x[1]".to_string(), "x[2]".to_string()], 2);

        assert_eq!(meta.len(), 2);
        assert!(meta.iter().all(|entry| entry.is_state));
        assert!(meta.iter().all(|entry| entry.role == "state"));
        assert!(meta.iter().all(|entry| entry.unit.as_deref() == Some("m")));
    }
}
