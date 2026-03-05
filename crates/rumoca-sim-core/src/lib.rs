//! Shared IC (initial condition) solving infrastructure for Rumoca simulator backends.
//!
//! Provides a BLT-based sequential runtime IC solver that walks an `IcBlock` plan
//! (produced at compile-time by `rumoca_phase_structural::build_ic_plan`) and uses
//! Levenberg-Marquardt for robust nonlinear solving with nominal-based scaling.
//!
//! # Architecture
//!
//! ```text
//! solve_initial_blt()   (runtime)
//!   ├─ ScalarDirect: evaluate symbolic solution
//!   ├─ ScalarNewton: single-variable Newton with AD
//!   ├─ TornBlock: LM on tear variables + causal sequence
//!   └─ CoupledLM: full small-block LM
//! ```

pub mod equation_scalarize;
pub mod function_validation;
mod ic_solve;
pub mod projection_maps;
pub mod reconstruct;
pub mod runtime;
pub mod sim_types;
pub mod simulation;
pub mod sparsity;
pub mod test_support;
pub mod timeline;

pub use ic_solve::{IcSolveError, solve_initial_blt, solve_initial_blt_with_deadline};
pub use runtime::compiled_discrete::{
    CompiledDiscreteEventContext, build_compiled_discrete_event_context,
    settle_runtime_event_updates_with_compiled_discrete,
};
pub use runtime::discrete_capture::{
    build_initialization_capture_env, runtime_capture_target_names,
    settle_runtime_discrete_capture_env,
};
pub use runtime::event::{
    EventSettleInput, build_runtime_env, event_restart_time, event_right_limit_time,
    refresh_pre_values_from_state, settle_runtime_event_updates,
    settle_runtime_event_updates_default, settle_runtime_sample_updates_default,
};
pub use runtime::fallback::{
    IntegrationAttemptFns, IntegrationFallbackConfig,
    integrate_with_fallbacks as integrate_with_runtime_fallbacks, is_step_size_error,
    is_step_size_t0_error, parse_solver_error_time,
};
pub use runtime::ic_startup::solve_initial_conditions;
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
pub use runtime::solver_runtime::{
    check_budget_or_trace_timeout, event_restart_step_hint, initialize_output_capture,
    integration_direction, is_interpolation_outside_step_error,
    is_interpolation_outside_step_sim_error, is_nonlinear_solver_failure,
    is_stop_time_at_current_state_time_error, map_solver_panic, near_active_stop_for_recovery,
    panic_payload_message, record_outputs_until, reset_stop_time_error, sample_state_at_stop,
    should_recover_interpolation_window_error, should_recover_nonlinear_failure_near_active_stop,
    should_recover_stop_time_error, snapshot as runtime_snapshot, startup_profile_label,
};
pub use runtime::startup::{
    apply_initial_section_assignments, initialize_state_vector,
    refresh_pre_values_from_state_with_initial_assignments,
};
pub use runtime::state_index::build_state_name_to_idx;
pub use runtime::step_diagnostics::{
    ResidualDiagnostic, collect_residual_diagnostics, jacobian_failure_introspection_enabled,
    safe_eval_expr_for_failure, sorted_value_rows, trace_function_calls_in_expr,
    trace_function_eval_diagnostics, trace_jacobian_failure_diagnostics,
    trace_residual_diagnostics, trace_state_value_diagnostics, trace_step_failure_diagnostics,
};
pub use runtime::time::{stop_time_reached_with_tol, time_advanced_with_tol, time_match_with_tol};
pub use runtime::timeout::{
    SolverDeadlineGuard, TimeoutBudget, TimeoutExceeded, is_solver_timeout_panic,
    panic_on_expired_solver_deadline, run_timeout_result, run_timeout_step,
    run_timeout_step_result,
};
pub use sim_types::{
    OutputBuffers, SimError, SimOptions, SimResult, SimSolverMode, SimVariableMeta,
    SolverStartupProfile,
};
pub use simulation::causal_ode_form::{CausalOdeForm, CausalOdeFormError, extract_causal_ode_form};
pub use simulation::equation_reorder::{EquationReorderError, reorder_equations_for_solver};
pub use simulation::mass_matrix_form::{MassMatrixFormError, validate_constant_mass_matrix_form};
pub use simulation::parameter_values::{
    ParameterValueError, default_parameter_values, default_parameter_values_with_budget,
};
pub use simulation::prepare::{
    build_ic_plan_or_empty, prepare_dae, prepare_dae_for_template_codegen_only,
    relaxed_ic_hint_has_disjoint_drop_row,
};
pub use simulation::problem_runtime::{
    count_states, default_params, default_params_with_budget, eval_jacobian_vector_ad,
    eval_jacobian_vector_ad_initial, eval_rhs_equations, eval_rhs_equations_initial,
    initial_free_residual_inf, project_algebraics_with_fixed_states_at_time,
    runtime_projection_required, seed_runtime_direct_assignments, solve_initial_algebraic,
};
pub use simulation::result_build::{build_algebraic_result, finalize_dynamic_result};
pub use simulation::results::{
    build_variable_meta, build_visible_result_names, collect_discrete_channel_names,
    evaluate_runtime_discrete_channels, merge_runtime_discrete_channels,
};
pub use simulation::runtime_alias_normalization::{
    RuntimeAliasSubstitution, apply_runtime_alias_substitutions_to_elimination,
    normalize_runtime_aliases_collect,
};
pub use simulation::runtime_prep::{compute_mass_matrix, pin_orphaned_variables};
