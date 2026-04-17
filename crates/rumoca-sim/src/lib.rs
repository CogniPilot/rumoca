//! Unified simulation crate for Rumoca.
//!
//! This crate contains:
//! - solver-agnostic simulation/runtime infrastructure
//! - optional solver backend implementations (currently `diffsol`)

use indexmap::IndexMap;
use rumoca_ir_dae as dae;
use std::collections::HashSet;

pub mod equation_scalarize;
pub mod function_validation;
#[cfg(test)]
mod ic_solve;
pub mod projection_maps;
pub mod reconstruct;
pub mod runtime;
pub mod sim_trace_compare;
pub mod simulation;
pub mod sparsity;
pub mod timeline;

pub use runtime::compiled_discrete::{
    CompiledDiscreteEventContext, build_compiled_discrete_event_context,
    settle_runtime_event_updates_with_compiled_discrete,
};
pub use runtime::event::{
    EventSettleInput, build_runtime_env, event_restart_time, event_right_limit_time,
    refresh_pre_values_from_state, settle_runtime_event_updates,
    settle_runtime_event_updates_default, settle_runtime_sample_updates_default,
};
pub use runtime::no_state::{
    NoStateSampleContext, NoStateSampleError, collect_algebraic_samples,
    collect_reconstruction_discrete_context_names, finalize_algebraic_outputs,
};
pub use runtime::orchestration::{
    BackendState, LoopStats, SimulationBackend, StepUntilOutcome, run_with_runtime_schedule,
};
pub use runtime::report::{
    RuntimeProgressSnapshot, RuntimeTraceContext, runtime_progress_snapshot, trace_runtime_done,
    trace_runtime_progress, trace_runtime_start, trace_runtime_step_fail, trace_runtime_timeout,
};
pub use runtime::schedule::RuntimeStopSchedule;
pub use runtime::startup::{
    apply_initial_section_assignments, refresh_pre_values_from_state_with_initial_assignments,
};
pub use runtime::state_index::build_state_name_to_idx;
pub use runtime::time::{stop_time_reached_with_tol, time_advanced_with_tol, time_match_with_tol};
pub use runtime::timeout::{
    SolverDeadlineGuard, TimeoutBudget, TimeoutExceeded, is_solver_timeout_panic,
    panic_on_expired_solver_deadline, run_timeout_result, run_timeout_step,
    run_timeout_step_result,
};
pub use simulation::runtime_prep::{compute_mass_matrix, pin_orphaned_variables};

#[cfg(feature = "diffsol")]
pub mod with_diffsol;

#[cfg(feature = "diffsol")]
pub use with_diffsol::stepper::{SimStepper, StepperOptions, StepperState};
#[cfg(feature = "diffsol")]
pub use with_diffsol::{SimError, SimOptions, SimResult, SimSolverMode, SimVariableMeta, simulate};
#[cfg(feature = "diffsol")]
pub use with_diffsol::{eliminate, problem};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PreparedSimulationBackend {
    Diffsol,
}

/// Opaque simulator build artifact for separating simulator setup from run.
///
/// The prepared value is backend-specific and intentionally opaque so the
/// public API can expose a stable build/run boundary without leaking solver
/// internals across crate boundaries (SPEC_0029).
pub struct PreparedSimulation {
    backend: PreparedSimulationBackend,
    parameter_overrides: IndexMap<String, Vec<f64>>,
    #[cfg(feature = "diffsol")]
    diffsol: with_diffsol::PreparedSimulation,
}

impl PreparedSimulation {
    pub fn backend(&self) -> SimBackend {
        match self.backend {
            PreparedSimulationBackend::Diffsol => SimBackend::Diffsol,
        }
    }

    pub fn set_parameter_value(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        self.set_parameter_values(name, &[value])
    }

    pub fn set_parameter_values(&mut self, name: &str, values: &[f64]) -> Result<(), SimError> {
        match self.backend {
            PreparedSimulationBackend::Diffsol => {
                #[cfg(feature = "diffsol")]
                {
                    with_diffsol::validate_parameter_override(&self.diffsol, name, values)?;
                    self.parameter_overrides
                        .insert(name.to_string(), values.to_vec());
                    Ok(())
                }
                #[cfg(not(feature = "diffsol"))]
                {
                    Err(SimError)
                }
            }
        }
    }

    pub fn clear_parameter_overrides(&mut self) {
        self.parameter_overrides.clear();
    }

    pub fn run(&self) -> Result<SimResult, SimError> {
        match self.backend {
            PreparedSimulationBackend::Diffsol => {
                #[cfg(feature = "diffsol")]
                {
                    with_diffsol::run_prepared_simulation(&self.diffsol, &self.parameter_overrides)
                }
                #[cfg(not(feature = "diffsol"))]
                {
                    Err(SimError)
                }
            }
        }
    }
}

#[cfg(not(feature = "diffsol"))]
#[derive(Debug, Clone, thiserror::Error)]
#[error("diffsol backend not enabled in rumoca-sim (enable feature `diffsol`)")]
pub struct SimError;

#[cfg(not(feature = "diffsol"))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimSolverMode {
    Auto,
    Bdf,
    RkLike,
}

#[cfg(not(feature = "diffsol"))]
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

impl SimSolverMode {
    pub fn parse_request(solver: Option<&str>) -> (Self, String) {
        match solver {
            Some(raw) if !raw.trim().is_empty() => {
                let trimmed = raw.trim();
                (Self::from_external_name(trimmed), trimmed.to_string())
            }
            _ => (Self::Auto, "auto".to_string()),
        }
    }
}

#[cfg(not(feature = "diffsol"))]
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

#[cfg(not(feature = "diffsol"))]
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

#[cfg(not(feature = "diffsol"))]
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

#[cfg(not(feature = "diffsol"))]
#[derive(Debug, Clone)]
pub struct SimResult {
    pub times: Vec<f64>,
    pub names: Vec<String>,
    pub data: Vec<Vec<f64>>,
    pub n_states: usize,
    pub variable_meta: Vec<SimVariableMeta>,
}

#[cfg(not(feature = "diffsol"))]
pub fn simulate(_: &dae::Dae, _: &SimOptions) -> Result<SimResult, SimError> {
    Err(SimError)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimBackend {
    Diffsol,
}

#[cfg(feature = "diffsol")]
pub const AVAILABLE_BACKENDS: &[SimBackend] = &[SimBackend::Diffsol];

#[cfg(not(feature = "diffsol"))]
pub const AVAILABLE_BACKENDS: &[SimBackend] = &[];

pub fn available_backends() -> &'static [SimBackend] {
    AVAILABLE_BACKENDS
}

pub fn build_simulation(
    dae_model: &dae::Dae,
    opts: &SimOptions,
) -> Result<PreparedSimulation, SimError> {
    build_simulation_with_backend(dae_model, opts, SimBackend::Diffsol)
}

pub fn build_simulation_with_backend(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    backend: SimBackend,
) -> Result<PreparedSimulation, SimError> {
    match backend {
        SimBackend::Diffsol => {
            #[cfg(feature = "diffsol")]
            {
                Ok(PreparedSimulation {
                    backend: PreparedSimulationBackend::Diffsol,
                    parameter_overrides: IndexMap::new(),
                    diffsol: with_diffsol::build_simulation(dae_model, opts)?,
                })
            }
            #[cfg(not(feature = "diffsol"))]
            {
                let _ = (dae_model, opts);
                Err(SimError)
            }
        }
    }
}

pub fn run_prepared_simulation(prepared: &PreparedSimulation) -> Result<SimResult, SimError> {
    prepared.run()
}

pub fn simulate_dae(dae_model: &dae::Dae, opts: &SimOptions) -> Result<SimResult, SimError> {
    let prepared = build_simulation(dae_model, opts)?;
    run_prepared_simulation(&prepared)
}

pub fn simulate_dae_with_backend(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    backend: SimBackend,
) -> Result<SimResult, SimError> {
    let prepared = build_simulation_with_backend(dae_model, opts, backend)?;
    run_prepared_simulation(&prepared)
}

pub fn prepare_dae_for_template_codegen(
    dae_model: &dae::Dae,
    scalarize: bool,
) -> Result<dae::Dae, String> {
    prepare_dae_for_template_codegen_with_backend(dae_model, scalarize, SimBackend::Diffsol)
}

pub fn prepare_dae_for_template_codegen_with_backend(
    dae_model: &dae::Dae,
    scalarize: bool,
    backend: SimBackend,
) -> Result<dae::Dae, String> {
    match backend {
        SimBackend::Diffsol => {
            #[cfg(feature = "diffsol")]
            {
                with_diffsol::prepare_dae_for_template_codegen(dae_model, scalarize)
                    .map_err(|error| error.to_string())
            }
            #[cfg(not(feature = "diffsol"))]
            {
                let _ = (dae_model, scalarize);
                Err(
                    "diffsol backend not enabled in rumoca-sim (enable feature `diffsol`)"
                        .to_string(),
                )
            }
        }
    }
}

pub fn dae_balance(dae_model: &dae::Dae) -> i64 {
    rumoca_eval_dae::analysis::balance(dae_model)
}

pub fn dae_balance_detail(dae_model: &dae::Dae) -> dae::BalanceDetail {
    rumoca_eval_dae::analysis::balance_detail(dae_model)
}

pub fn dae_is_balanced(dae_model: &dae::Dae) -> bool {
    rumoca_eval_dae::analysis::is_balanced(dae_model)
}

pub fn runtime_defined_unknown_names(dae_model: &dae::Dae) -> HashSet<String> {
    rumoca_eval_dae::analysis::runtime_defined_unknown_names(dae_model)
}

pub fn runtime_defined_continuous_unknown_names(dae_model: &dae::Dae) -> HashSet<String> {
    rumoca_eval_dae::analysis::runtime_defined_continuous_unknown_names(dae_model)
}

pub fn compiled_layout_binding_debug(dae_model: &dae::Dae, name: &str) -> Option<String> {
    let layout = rumoca_eval_dae::compiled::layout::VarLayout::from_dae(dae_model);
    layout.binding(name).map(|slot| format!("{slot:?}"))
}

pub fn compiled_layout_related_bindings_debug(
    dae_model: &dae::Dae,
    prefix: &str,
) -> Vec<(String, String)> {
    let layout = rumoca_eval_dae::compiled::layout::VarLayout::from_dae(dae_model);
    layout
        .bindings()
        .iter()
        .filter(|(binding_name, _)| {
            binding_name.starts_with(prefix) && binding_name.as_str() != prefix
        })
        .map(|(binding_name, slot)| (binding_name.to_string(), format!("{slot:?}")))
        .collect()
}

pub fn clear_runtime_pre_values() {
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[cfg(all(test, feature = "diffsol"))]
mod tests {
    use super::*;
    use crate::with_diffsol::test_support::{real, sub, var_ref};
    use rumoca_core::Span;

    fn build_parameterized_hold_model(default_gain: f64) -> dae::Dae {
        let mut dae = dae::Dae::new();

        let mut gain = dae::Variable::new(dae::VarName::new("gain"));
        gain.start = Some(real(default_gain));
        gain.fixed = Some(true);
        dae.parameters.insert(dae::VarName::new("gain"), gain);

        let mut x = dae::Variable::new(dae::VarName::new("x"));
        x.start = Some(var_ref("gain"));
        x.fixed = Some(true);
        dae.states.insert(dae::VarName::new("x"), x);

        dae.f_x.push(dae::Equation {
            lhs: None,
            rhs: sub(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Der,
                    args: vec![var_ref("x")],
                },
                real(0.0),
            ),
            span: Span::DUMMY,
            origin: "der_x_zero".to_string(),
            scalar_count: 1,
        });

        dae
    }

    fn x_series(result: &SimResult) -> &[f64] {
        let x_idx = result
            .names
            .iter()
            .position(|name| name == "x")
            .expect("state x should be present in results");
        &result.data[x_idx]
    }

    #[test]
    fn solver_mode_request_parsing_defaults_blank_input_to_auto() {
        assert_eq!(
            SimSolverMode::parse_request(None),
            (SimSolverMode::Auto, "auto".to_string())
        );
        assert_eq!(
            SimSolverMode::parse_request(Some("   ")),
            (SimSolverMode::Auto, "auto".to_string())
        );
    }

    #[test]
    fn solver_mode_request_parsing_preserves_trimmed_label_and_maps_mode() {
        assert_eq!(
            SimSolverMode::parse_request(Some("  dopri5 ")),
            (SimSolverMode::RkLike, "dopri5".to_string())
        );
        assert_eq!(
            SimSolverMode::parse_request(Some("IDA")),
            (SimSolverMode::Bdf, "IDA".to_string())
        );
    }

    #[test]
    fn prepared_simulation_matches_direct_simulation_for_default_parameters() {
        let dae = build_parameterized_hold_model(2.0);
        let opts = SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            max_wall_seconds: Some(2.0),
            ..SimOptions::default()
        };

        let direct = simulate_dae(&dae, &opts).expect("direct simulate should succeed");
        let prepared = build_simulation(&dae, &opts).expect("build should succeed");
        let prepared_result = prepared.run().expect("prepared run should succeed");

        assert_eq!(direct.times, prepared_result.times);
        assert_eq!(x_series(&direct), x_series(&prepared_result));
    }

    #[test]
    fn prepared_simulation_reuses_build_for_parameter_overrides() {
        let dae = build_parameterized_hold_model(2.0);
        let opts = SimOptions {
            t_end: 0.2,
            dt: Some(0.1),
            max_wall_seconds: Some(2.0),
            ..SimOptions::default()
        };

        let mut prepared = build_simulation(&dae, &opts).expect("build should succeed");

        let baseline = prepared.run().expect("baseline run should succeed");
        assert!(
            x_series(&baseline)
                .iter()
                .all(|value| (*value - 2.0).abs() < 1.0e-9),
            "expected default parameter to initialize x=2, got {:?}",
            x_series(&baseline)
        );

        prepared
            .set_parameter_value("gain", 5.0)
            .expect("scalar override should validate");
        let overridden = prepared.run().expect("override run should succeed");
        assert!(
            x_series(&overridden)
                .iter()
                .all(|value| (*value - 5.0).abs() < 1.0e-9),
            "expected override to reinitialize x=5 without rebuild, got {:?}",
            x_series(&overridden)
        );

        prepared.clear_parameter_overrides();
        let reset = prepared.run().expect("reset run should succeed");
        assert!(
            x_series(&reset)
                .iter()
                .all(|value| (*value - 2.0).abs() < 1.0e-9),
            "expected cleared overrides to restore x=2, got {:?}",
            x_series(&reset)
        );
    }
}
