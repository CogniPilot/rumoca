//! Backend-neutral simulation contracts and runtime helpers for Rumoca.

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

#[derive(Debug, Clone)]
pub struct SimResult {
    pub times: Vec<f64>,
    pub names: Vec<String>,
    pub data: Vec<Vec<f64>>,
    pub n_states: usize,
    pub variable_meta: Vec<SimVariableMeta>,
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

#[cfg(test)]
mod tests {
    use super::*;

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
}
