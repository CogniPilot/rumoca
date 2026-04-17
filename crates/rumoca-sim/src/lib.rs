//! Backend-neutral simulation contracts and runtime helpers for Rumoca.

use rumoca_ir_dae as dae;
use std::collections::HashSet;

pub mod equation_scalarize;
pub mod function_validation;
#[cfg(test)]
mod ic_solve;
pub mod projection_maps;
pub mod reconstruct;
pub mod report_payload;
pub mod runtime;
pub mod sim_trace_compare;
pub mod simulation;
pub mod sparsity;
pub mod timeline;

pub use report_payload::{
    SimulationRequestSummary, SimulationRunMetrics, build_simulation_metrics_value,
    build_simulation_payload,
};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimBackend {
    Diffsol,
    Rk45,
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

struct VariableSource<'a> {
    var: &'a dae::Variable,
    role: &'static str,
    is_state: bool,
}

fn lookup_variable_exact<'a>(dae_model: &'a dae::Dae, name: &str) -> Option<VariableSource<'a>> {
    let key = dae::VarName::new(name);
    if let Some(var) = dae_model.states.get(&key) {
        return Some(VariableSource {
            var,
            role: "state",
            is_state: true,
        });
    }
    if let Some(var) = dae_model.algebraics.get(&key) {
        return Some(VariableSource {
            var,
            role: "algebraic",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.outputs.get(&key) {
        return Some(VariableSource {
            var,
            role: "output",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.inputs.get(&key) {
        return Some(VariableSource {
            var,
            role: "input",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.parameters.get(&key) {
        return Some(VariableSource {
            var,
            role: "parameter",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.constants.get(&key) {
        return Some(VariableSource {
            var,
            role: "constant",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.discrete_reals.get(&key) {
        return Some(VariableSource {
            var,
            role: "discrete-real",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.discrete_valued.get(&key) {
        return Some(VariableSource {
            var,
            role: "discrete-valued",
            is_state: false,
        });
    }
    if let Some(var) = dae_model.derivative_aliases.get(&key) {
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

fn truncate_meta_expr(expr: &dae::Expression) -> String {
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
    use rumoca_ir_dae::Variable;

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
    fn build_variable_meta_resolves_scalarized_names_back_to_array_variable() {
        let mut dae_model = dae::Dae::default();
        let mut state = Variable::new(dae::VarName::new("x"));
        state.dims = vec![2];
        state.unit = Some("m".to_string());
        dae_model.states.insert(dae::VarName::new("x"), state);

        let meta = build_variable_meta(&dae_model, &["x[1]".to_string(), "x[2]".to_string()], 2);

        assert_eq!(meta.len(), 2);
        assert!(meta.iter().all(|entry| entry.is_state));
        assert!(meta.iter().all(|entry| entry.role == "state"));
        assert!(meta.iter().all(|entry| entry.unit.as_deref() == Some("m")));
    }
}
