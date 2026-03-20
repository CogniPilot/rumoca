//! Unified simulation crate for Rumoca.
//!
//! This crate contains:
//! - solver-agnostic simulation/runtime infrastructure
//! - optional solver backend implementations (currently `diffsol`)

use rumoca_ir_dae as dae;
use std::collections::HashSet;

pub mod equation_scalarize;
pub mod function_validation;
mod ic_solve;
pub mod projection_maps;
pub mod reconstruct;
pub mod results_web;
pub mod runtime;
pub mod sim_trace_compare;
pub mod simulation;
pub mod sparsity;
pub mod timeline;

pub use ic_solve::{IcSolveError, solve_initial_blt, solve_initial_blt_with_deadline};
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
pub use with_diffsol::{SimError, SimOptions, SimResult, SimSolverMode, SimVariableMeta, simulate};
#[cfg(feature = "diffsol")]
pub use with_diffsol::{eliminate, problem};

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

pub fn simulate_dae(dae_model: &dae::Dae, opts: &SimOptions) -> Result<SimResult, SimError> {
    simulate_dae_with_backend(dae_model, opts, SimBackend::Diffsol)
}

pub fn simulate_dae_with_backend(
    dae_model: &dae::Dae,
    opts: &SimOptions,
    backend: SimBackend,
) -> Result<SimResult, SimError> {
    match backend {
        SimBackend::Diffsol => simulate(dae_model, opts),
    }
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

pub fn clear_runtime_pre_values() {
    rumoca_eval_dae::runtime::clear_pre_values();
}
