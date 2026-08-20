//! High-level simulation facade for Rumoca.
//!
//! Re-exports the primitives crate `rumoca-solver` plus, when the
//! corresponding features are enabled, the diffsol/rk45 solver entry points
//! and the scheduled simulation module that drives scheduled scenario simulations.

use indexmap::IndexSet;
use rumoca_core::Span;
use rumoca_ir_solve as solve;
use serde::{Deserialize, Serialize};

/// NaN / non-finite runtime tracing, exposed through the sim facade so the CLI
/// (and library users) can switch it on without an environment variable. See
/// [`rumoca_eval_solve::nan_trace`].
pub use rumoca_eval_solve::nan_trace;
use rumoca_ir_dae as dae;
pub use rumoca_phase_solve::{
    deserialize_solve_model,
    fmi::{deserialize_fmi_component, fmi_component_wire},
    lower_solve_artifacts, lower_solve_problem, solve_model_wire,
};
pub use rumoca_solver::{
    DiffsolMethod, RuntimeProgressSnapshot, RuntimeStopSchedule, RuntimeTraceContext, SimBackend,
    SimOptions, SimPacingMode, SimResult, SimSolverMode, SimVariableMeta, SimulationRequestSummary,
    SimulationRunMetrics, SolverDeadlineGuard, TimeoutBudget, TimeoutExceeded,
    build_simulation_metrics_value, build_simulation_payload, is_solver_timeout_panic,
    panic_on_expired_solver_deadline, run_timeout_result, run_timeout_step,
    run_timeout_step_result, runtime_progress_snapshot, stop_time_reached_with_tol,
    time_advanced_with_tol, time_match_with_tol, trace_runtime_done, trace_runtime_progress,
    trace_runtime_start, trace_runtime_step_fail, trace_runtime_timeout,
};

mod build_timing;
pub mod bulk;
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
mod error;
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
mod me_backend;
pub mod row_eval_trace;
pub mod sim_trace_compare;
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
mod simulation_session;
#[cfg(feature = "scheduled-sim")]
mod simulation_session_api;

#[cfg(feature = "solver-diffsol")]
mod diffsol;
#[cfg(all(
    not(target_arch = "wasm32"),
    any(feature = "solver-rk45", feature = "solver-diffsol")
))]
mod native_execution;
/// Wasm builds carry no compiled native backend at all; the one shared
/// admission gate uniformly withholds so every caller composes through the
/// same name on every target.
#[cfg(all(
    target_arch = "wasm32",
    any(feature = "solver-rk45", feature = "solver-diffsol")
))]
mod native_execution {
    pub(crate) fn admitted_native_execution_backend(
        _opts: &rumoca_solver::SimOptions,
        _model: &rumoca_ir_solve::SolveModel,
    ) -> Option<rumoca_solver::fmi_me::MeExecutionBackend> {
        None
    }
}
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
mod prepared_vectors;
mod solve_lowering;
pub use build_timing::BuildSimulationTimings;
#[cfg(feature = "solver-diffsol")]
pub use diffsol::{
    PreparedSimulation, build_simulation, build_simulation_with_stage_timing,
    build_simulation_with_stage_timing_and_solve_model, check_initialization,
    check_prepared_initialization, run_prepared_simulation, simulate, simulate_dae,
};
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub use error::{SimError, SimFailureStage};
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub use prepared_vectors::{PreparedVectorError, refresh_prepared_vectors};
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub use simulation_session::{SessionState, SimulationSession};
#[cfg(feature = "scheduled-sim")]
pub(crate) use simulation_session_api::SimulationSessionApi;
// The inspection/debug facade (probes + their named report types) is surfaced
// through `solve_lowering` so the root stays a curated same-crate facade; the
// report types are re-exported from there rather than as root cross-crate uses.
#[cfg(feature = "fmi")]
pub use solve_lowering::lower_fmi_component;
pub use solve_lowering::{
    BlockReport, EvalAtProbe, EvalAtReport, EvalAtSlot, JacobianProbe, JacobianReport,
    ObjectiveGradientProbe, ParameterJacobianProbe, SimulationDiagnosticError,
    SingularityDiagnosis, StateAndParameterJacobianProbe, SteadyStateSensitivityProbe,
    StructuralReport, TearingReport, UnmatchedEquationDiagnosis, UnmatchedUnknownDiagnosis,
    diagnose_structural_singularity, eval_dae_at, jacobian_for_dae,
    lower_correlated_for_simulation_with_overrides, lower_dae_for_gpu_preparation,
    lower_dae_for_simulation, lower_for_differentiation_with_overrides,
    lower_for_simulation_with_overrides, parameter_jacobian_for_dae,
    state_and_parameter_jacobian_for_dae, steady_state_adjoint_objective_gradient_for_dae,
    steady_state_objective_gradient_for_dae, steady_state_parameter_sensitivity_for_dae,
    structural_report_for_dae,
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

/// Simulate one already-constructed correlated FMI component, dispatching by
/// `opts.solver_mode`.
///
/// Wire callers replay through `rumoca_phase_solve::fmi::deserialize_fmi_component`;
/// a bare Solve root is deliberately not a simulation entry because it carries
/// no proof that its FMI inventory came from the same source construction.
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub fn simulate_fmi_component(
    component: rumoca_ir_solve::fmi::FmiComponent,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    let execution_backend =
        native_execution::admitted_native_execution_backend(opts, component.runtime_view().model());
    let artifact = rumoca_solver::fmi_me::MeModelArtifact::new(component);
    match opts.solver_mode {
        SimSolverMode::Auto => simulate_artifact_auto(artifact, opts, execution_backend),
        SimSolverMode::RkLike => simulate_artifact_rk45(artifact, opts, execution_backend),
        SimSolverMode::Bdf => simulate_artifact_diffsol(artifact, opts, execution_backend),
    }
}

#[cfg(all(feature = "solver-diffsol", feature = "solver-rk45"))]
fn simulate_artifact_auto(
    artifact: rumoca_solver::fmi_me::MeModelArtifact,
    opts: &SimOptions,
    execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
) -> Result<SimResult, SimulationDiagnosticError> {
    match diffsol::assess_bdf_capability(&artifact, opts, execution_backend.clone())
        .map_err(|error| SimulationDiagnosticError::Solver(error.to_string()))?
    {
        diffsol::BdfCapability::Eligible => {
            simulate_artifact_diffsol(artifact, opts, execution_backend)
        }
        diffsol::BdfCapability::InitialLinearizationUnavailable { reason } => {
            tracing::debug!(
                target: "rumoca_sim::solver_selection",
                %reason,
                "auto selected rk-like because the initial BDF linearization is unavailable"
            );
            simulate_artifact_rk45(artifact, opts, execution_backend)
        }
    }
}

#[cfg(all(feature = "solver-diffsol", not(feature = "solver-rk45")))]
fn simulate_artifact_auto(
    artifact: rumoca_solver::fmi_me::MeModelArtifact,
    opts: &SimOptions,
    execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
) -> Result<SimResult, SimulationDiagnosticError> {
    simulate_artifact_diffsol(artifact, opts, execution_backend)
}

#[cfg(all(not(feature = "solver-diffsol"), feature = "solver-rk45"))]
fn simulate_artifact_auto(
    artifact: rumoca_solver::fmi_me::MeModelArtifact,
    opts: &SimOptions,
    execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
) -> Result<SimResult, SimulationDiagnosticError> {
    simulate_artifact_rk45(artifact, opts, execution_backend)
}

#[cfg(feature = "solver-rk45")]
fn simulate_artifact_rk45(
    artifact: rumoca_solver::fmi_me::MeModelArtifact,
    opts: &SimOptions,
    execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
) -> Result<SimResult, SimulationDiagnosticError> {
    rk45::simulate_artifact(artifact, opts, execution_backend)
        .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))
}

#[cfg(all(
    any(feature = "solver-diffsol", feature = "solver-rk45"),
    not(feature = "solver-rk45")
))]
fn simulate_artifact_rk45(
    _artifact: rumoca_solver::fmi_me::MeModelArtifact,
    _opts: &SimOptions,
    _execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
) -> Result<SimResult, SimulationDiagnosticError> {
    Err(SimulationDiagnosticError::Solver(
        "rk-like solver requested, but this build does not include the rk45 backend".to_string(),
    ))
}

#[cfg(feature = "solver-diffsol")]
fn simulate_artifact_diffsol(
    artifact: rumoca_solver::fmi_me::MeModelArtifact,
    opts: &SimOptions,
    execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
) -> Result<SimResult, SimulationDiagnosticError> {
    diffsol::simulate_artifact(artifact, opts, execution_backend)
        .map_err(|err| SimulationDiagnosticError::Solver(err.to_string()))
}

#[cfg(all(
    any(feature = "solver-diffsol", feature = "solver-rk45"),
    not(feature = "solver-diffsol")
))]
fn simulate_artifact_diffsol(
    _artifact: rumoca_solver::fmi_me::MeModelArtifact,
    _opts: &SimOptions,
    _execution_backend: Option<rumoca_solver::fmi_me::MeExecutionBackend>,
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

#[cfg(all(feature = "solver-diffsol", feature = "solver-rk45"))]
fn simulate_with_auto_diagnostics(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    let (artifact, execution_backend) =
        solve_lowering::lower_runtime_fmi_artifact(dae_model, opts)?;
    simulate_artifact_auto(artifact, opts, execution_backend)
}

#[cfg(all(feature = "solver-diffsol", not(feature = "solver-rk45")))]
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

    #[test]
    fn auto_mode_uses_rk45_when_diffsol_is_not_built() {
        let mut source_map = rumoca_core::SourceMap::new();
        let source = source_map.add("solver_mode_test.mo", "Real x(start=0); der(x)=1;");
        let declaration =
            dae::DaeProvenance::source(rumoca_core::Span::from_offsets(source, 0, 15))
                .expect("test declaration has real provenance");
        let owner = dae::DaeProvenance::source(rumoca_core::Span::from_offsets(source, 17, 26))
            .expect("test equation has real provenance");
        let model = dae::Dae::construct(source_map, |construction| {
            let real = construction.types(|types| {
                types.intern(
                    rumoca_core::TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    declaration,
                )
            })?;
            let start = construction.expressions(|expressions| {
                expressions
                    .at(declaration)
                    .literal(dae::DaeLiteral::Real(0.0))
            })?;
            let state = construction.variables(|variables| {
                variables.state(
                    rumoca_core::VarName::new("x"),
                    real,
                    declaration,
                    dae::VariableAttributes {
                        start: Some(start),
                        ..dae::VariableAttributes::default()
                    },
                )
            })?;
            let residual = construction.expressions(|expressions| {
                let derivative = expressions
                    .at(owner)
                    .coordinate(dae::CoordinateInput::Derivative(state))?;
                let one = expressions.at(owner).literal(dae::DaeLiteral::Real(1.0))?;
                expressions
                    .at(owner)
                    .binary(dae::BinaryOperator::Subtract, derivative, one)
            })?;
            construction.continuous(|continuous| continuous.value_equation(owner, residual))
        })
        .expect("test DAE is valid by construction");
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

pub fn build_variable_meta(
    dae_model: &dae::Dae,
    names: &[String],
) -> Result<Vec<SimVariableMeta>, SimulationDiagnosticError> {
    dae_model.inspect(|view| {
        let causal_definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut by_name = std::collections::HashMap::new();
        for (id, variable) in view.variables() {
            for scalar in 0..variable.scalar_count() {
                let name = variable
                    .scalar_name(scalar)
                    .expect("checked scalar variable has a name");
                by_name.insert(
                    name.clone(),
                    checked_variable_meta(
                        dae_model,
                        view,
                        variable,
                        causal_definitions.event_holds_variable(id),
                        name,
                    ),
                );
            }
        }
        names
            .iter()
            .map(|name| {
                by_name.get(name).cloned().ok_or_else(|| {
                    SimulationDiagnosticError::RuntimePreparation {
                        message: format!(
                            "Solve output `{name}` has no checked DAE variable identity"
                        ),
                        span: None,
                    }
                })
            })
            .collect()
    })
}

fn checked_variable_meta<'dae>(
    model: &dae::Dae,
    view: dae::DaeView<'dae>,
    variable: dae::VariableView<'dae>,
    event_held: bool,
    name: String,
) -> SimVariableMeta {
    SimVariableMeta {
        name,
        role: variable_role_name(variable.role()).to_string(),
        is_state: variable.role() == dae::VariableRole::State,
        value_type: Some(format!("{:?}", variable.value_type().scalar_type())),
        variability: Some(format!("{:?}", variable.variability())),
        time_domain: Some(variable_time_domain(variable.role(), event_held).to_string()),
        unit: variable.unit().map(str::to_string),
        start: variable
            .start()
            .and_then(|id| expression_source(model, view, id)),
        min: variable
            .minimum()
            .and_then(|id| expression_source(model, view, id)),
        max: variable
            .maximum()
            .and_then(|id| expression_source(model, view, id)),
        nominal: variable
            .nominal()
            .and_then(|id| expression_source(model, view, id)),
        fixed: variable.fixed(),
        description: variable.description().map(str::to_string),
    }
}

fn expression_source<'dae>(
    model: &dae::Dae,
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<String> {
    model
        .source_text(view.expression(expression)?.provenance())
        .map(str::to_string)
}

const fn variable_role_name(role: dae::VariableRole) -> &'static str {
    match role {
        dae::VariableRole::Parameter => "parameter",
        dae::VariableRole::Constant => "constant",
        dae::VariableRole::Input => "input",
        dae::VariableRole::State => "state",
        dae::VariableRole::Algebraic => "algebraic",
        dae::VariableRole::Output => "output",
        dae::VariableRole::DiscreteReal => "discrete-real",
        dae::VariableRole::DiscreteValue => "discrete-valued",
    }
}

const fn variable_time_domain(role: dae::VariableRole, event_held: bool) -> &'static str {
    match (role, event_held) {
        (dae::VariableRole::Algebraic | dae::VariableRole::Output, true) => "event-discontinuous",
        (dae::VariableRole::Parameter | dae::VariableRole::Constant, _) => "static",
        (dae::VariableRole::DiscreteReal | dae::VariableRole::DiscreteValue, _) => "event-discrete",
        (
            dae::VariableRole::Input
            | dae::VariableRole::State
            | dae::VariableRole::Algebraic
            | dae::VariableRole::Output,
            _,
        ) => "continuous-time",
    }
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
) -> Result<Vec<TunableParameterMeta>, SimulationDiagnosticError> {
    dae_model.inspect(|view| {
        let mut result = Vec::new();
        for (_, variable) in view.variables().filter(|(_, variable)| {
            variable.role() == dae::VariableRole::Parameter && variable.is_tunable()
        }) {
            result.extend(tunable_variable_meta(
                dae_model,
                view,
                solve_model,
                variable,
            )?);
        }
        Ok(result)
    })
}

fn tunable_variable_meta<'dae>(
    dae_model: &dae::Dae,
    view: dae::DaeView<'dae>,
    solve_model: &solve::SolveModel,
    variable: dae::VariableView<'dae>,
) -> Result<Vec<TunableParameterMeta>, SimulationDiagnosticError> {
    let minimum = evaluated_attribute(view, variable, variable.minimum())?;
    let maximum = evaluated_attribute(view, variable, variable.maximum())?;
    let mut result = Vec::with_capacity(variable.scalar_count());
    for scalar in 0..variable.scalar_count() {
        let name = variable
            .scalar_name(scalar)
            .expect("checked scalar variable has a name");
        let index = parameter_slot(solve_model, variable, &name)?;
        let default_value = solve_model.parameters.get(index).copied().ok_or_else(|| {
            runtime_preparation(
                format!("tunable parameter `{name}` has an invalid Solve P slot"),
                variable.declaration().span(),
            )
        })?;
        result.push(TunableParameterMeta {
            name,
            default_value,
            unit: variable.unit().map(str::to_string),
            start: variable
                .start()
                .and_then(|id| expression_source(dae_model, view, id)),
            min: variable
                .minimum()
                .and_then(|id| expression_source(dae_model, view, id)),
            max: variable
                .maximum()
                .and_then(|id| expression_source(dae_model, view, id)),
            nominal: variable
                .nominal()
                .and_then(|id| expression_source(dae_model, view, id)),
            min_value: minimum.as_ref().map(|values| values[scalar]),
            max_value: maximum.as_ref().map(|values| values[scalar]),
            fixed: variable.fixed(),
            description: variable.description().map(str::to_string),
        });
    }
    Ok(result)
}

fn evaluated_attribute<'dae>(
    view: dae::DaeView<'dae>,
    variable: dae::VariableView<'dae>,
    expression: Option<dae::ExprId<'dae>>,
) -> Result<Option<Vec<f64>>, SimulationDiagnosticError> {
    rumoca_phase_solve::fmi::numeric_attribute_values(view, variable, expression).map_err(|error| {
        SimulationDiagnosticError::RuntimePreparation {
            message: error.to_string(),
            span: error.span(),
        }
    })
}

fn parameter_slot(
    solve_model: &solve::SolveModel,
    variable: dae::VariableView<'_>,
    name: &str,
) -> Result<usize, SimulationDiagnosticError> {
    let Some(solve::ScalarSlot::P { index, .. }) = solve_model.problem.layout.binding(name) else {
        return Err(runtime_preparation(
            format!("tunable parameter `{name}` has no Solve P slot"),
            variable.declaration().span(),
        ));
    };
    Ok(index)
}

fn runtime_preparation(message: String, span: Span) -> SimulationDiagnosticError {
    SimulationDiagnosticError::RuntimePreparation {
        message,
        span: Some(span),
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
            binding_name.as_str().starts_with(prefix) && binding_name.as_str() != prefix
        })
        .map(|(binding_name, slot)| (binding_name.to_string(), format!("{slot:?}")))
        .collect())
}
