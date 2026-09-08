//! High-level simulation facade for Rumoca.
//!
//! Re-exports the primitives crate `rumoca-solver` plus the canonical DAE
//! simulation entry point and the scheduled simulation module that drives
//! scheduled scenario simulations.

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
    time_advanced_with_tol, time_match_with_tol,
    timeline::{OutputTimelineError, try_build_output_times},
    trace_runtime_done, trace_runtime_progress, trace_runtime_start, trace_runtime_step_fail,
    trace_runtime_timeout,
};

mod build_timing;
pub mod bulk;
#[cfg(any(feature = "fmi", feature = "solver-diffsol", feature = "solver-rk45"))]
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
    ) -> Result<Option<rumoca_solver::fmi_me::MeExecutionBackend>, rumoca_solver::RuntimeSolveError>
    {
        Ok(None)
    }
}
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
mod prepared_simulation;
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
mod prepared_vectors;
mod solve_lowering;
pub use build_timing::BuildSimulationTimings;
#[cfg(any(feature = "fmi", feature = "solver-diffsol", feature = "solver-rk45"))]
pub use error::{SimError, SimFailureStage};
#[cfg(any(feature = "solver-diffsol", feature = "solver-rk45"))]
pub use prepared_simulation::{PreparedSimulation, prepare_simulation};
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
    lower_correlated_for_simulation_with_overrides, lower_dae_for_simulation,
    lower_for_differentiation_with_overrides, lower_for_simulation_with_overrides,
    parameter_jacobian_for_dae, state_and_parameter_jacobian_for_dae,
    steady_state_adjoint_objective_gradient_for_dae, steady_state_objective_gradient_for_dae,
    steady_state_parameter_sensitivity_for_dae, structural_report_for_dae,
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
pub fn simulate_dae(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<SimResult, SimulationDiagnosticError> {
    let (artifact, execution_backend) =
        solve_lowering::lower_runtime_fmi_artifact(dae_model, opts)?;
    let prepared = prepared_simulation::prepare_artifact(artifact, opts, execution_backend)
        .map_err(SimulationDiagnosticError::from)?;
    prepared.run().map_err(SimulationDiagnosticError::from)
}

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
    prepared_simulation::prepare_fmi_component(component, opts)
        .and_then(PreparedSimulation::run)
        .map_err(SimulationDiagnosticError::from)
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
                    rumoca_core::InstanceId::new(1),
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
        let result = simulate_dae(
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
    pub fixed: rumoca_core::Fixity,
    pub description: Option<String>,
}

pub fn build_tunable_parameter_meta(
    solve_model: &solve::SolveModel,
) -> Result<Vec<TunableParameterMeta>, SimulationDiagnosticError> {
    let mut result = Vec::new();
    for variable in solve_model
        .variable_catalog()
        .entries()
        .iter()
        .filter(|variable| {
            variable.role() == solve::SolveVariableStorageRole::Parameter && variable.is_tunable()
        })
    {
        result.extend(tunable_variable_meta(solve_model, variable)?);
    }
    Ok(result)
}

fn tunable_variable_meta(
    solve_model: &solve::SolveModel,
    variable: &solve::SolveVariableCatalogEntry,
) -> Result<Vec<TunableParameterMeta>, SimulationDiagnosticError> {
    let solve::SolveStorageCoordinate::P(base) = variable.storage().base else {
        return Err(runtime_preparation(
            format!(
                "tunable parameter `{}` has no checked parameter storage",
                variable.name()
            ),
            variable.provenance(),
        ));
    };
    let mut result = Vec::with_capacity(variable.scalar_names().len());
    for (scalar, name) in variable.scalar_names().iter().enumerate() {
        let index = base.checked_add(scalar).ok_or_else(|| {
            runtime_preparation(
                format!("tunable parameter `{name}` storage range overflows"),
                variable.provenance(),
            )
        })?;
        let default_value = solve_model
            .parameters()
            .get(index)
            .copied()
            .ok_or_else(|| {
                runtime_preparation(
                    format!("tunable parameter `{name}` has an invalid Solve P slot"),
                    variable.provenance(),
                )
            })?;
        result.push(TunableParameterMeta {
            name: name.clone(),
            default_value,
            unit: variable.unit().map(str::to_string),
            start: scalar_catalog_attribute(variable.start(), scalar),
            min: scalar_catalog_attribute(variable.minimum(), scalar),
            max: scalar_catalog_attribute(variable.maximum(), scalar),
            nominal: scalar_catalog_attribute(variable.nominal(), scalar),
            min_value: variable
                .minimum()
                .and_then(|values| values.get(scalar))
                .copied(),
            max_value: variable
                .maximum()
                .and_then(|values| values.get(scalar))
                .copied(),
            fixed: variable.fixed(),
            description: variable.description().map(str::to_string),
        });
    }
    Ok(result)
}

fn scalar_catalog_attribute(values: Option<&[f64]>, scalar: usize) -> Option<String> {
    values
        .and_then(|values| values.get(scalar))
        .map(ToString::to_string)
}

fn runtime_preparation(message: String, span: Span) -> SimulationDiagnosticError {
    SimulationDiagnosticError::RuntimePreparation {
        message,
        span: Some(span),
    }
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
