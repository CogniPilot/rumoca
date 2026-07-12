//! Backend-neutral simulation contracts and runtime helpers for Rumoca.

pub mod report_payload;
pub mod runtime;
pub mod solver;
pub mod sparsity;
pub mod timeline;

pub use report_payload::{
    SimulationRequestSummary, SimulationRunMetrics, build_simulation_metrics_value,
    build_simulation_payload,
};
pub use runtime::event::{
    RuntimeEventBoundary, RuntimeEventBoundaryHandler, RuntimeEventBoundaryOutcome,
    process_runtime_event_boundary, runtime_event_horizon, runtime_event_right_limit,
    runtime_root_event_application_time,
};
pub use runtime::mass_matrix::{PreparedMassMatrix, solve_mass_matrix};
pub use runtime::no_state::{
    NoStateEventStep, NoStateOrchestrationBackend, NoStateScheduledStop,
    run_no_state_output_schedule,
};
pub use runtime::orchestration::{LoopStats, run_with_runtime_schedule};
pub use runtime::pre_params::{
    clear_scheduled_root_relation_memory, commit_pre_params_after_event, update_slot,
    write_pre_params_from_sources,
};
pub use runtime::projection::{
    AlgebraicProjectionModel, implicit_residual_is_zero,
    implicit_residual_is_zero_through_interval, project_algebraics,
    project_algebraics_and_detect_changes, project_initial_algebraics, project_initial_variables,
    project_initial_variables_with_plan,
};
pub use runtime::report::{
    RuntimeProgressSnapshot, RuntimeTraceContext, runtime_progress_snapshot, trace_runtime_done,
    trace_runtime_progress, trace_runtime_start, trace_runtime_step_fail, trace_runtime_timeout,
};
pub use runtime::schedule::{
    RuntimeEventStop, RuntimeStopSchedule, SolveStopSchedule, initial_runtime_event_stop,
    initial_static_event_pre_mode, merge_runtime_event_stops,
};
pub use runtime::solve_ops::{
    EventActionOutcome, EventPreMode, EventPreSources, RootCrossing, RuntimeSolveError,
    build_sim_result_from_solve_model, convert_variable_meta, discrete_row_pre_mode,
    event_eval_params_for_pre_mode, event_eval_params_for_row_pre_mode,
    filter_scheduled_root_crossings, first_root_crossing, push_visible_values,
    relation_memory_value_from_root, replace_last_visible_values, root_crossed, root_crossings,
    root_crossings_with_relation_memory, root_value_crossed, row_reads_solver_or_time,
    update_relation_memory_slots,
};
pub use runtime::tensor_policy::{
    LinearSolveKernel, MatMulKernel, TensorPolicyError, matrix_is_diagonal, matrix_nonzeros,
    select_linear_solve_kernel, select_matmul_kernel,
};
pub use runtime::time::{
    event_solver_step_cap, stop_time_reached_with_tol, time_advanced_with_tol, time_match_with_tol,
};
pub use runtime::timeout::{
    SolverDeadlineGuard, TimeoutBudget, TimeoutExceeded, is_solver_timeout_panic,
    panic_on_expired_solver_deadline, run_timeout_result, run_timeout_step,
    run_timeout_step_result,
};
pub use solver::{
    BackendState, DiffsolMethod, SimBackend, SimOptions, SimPacingMode, SimResult, SimSolverMode,
    SimTermination, SimVariableMeta, SimulationBackend, StepUntilOutcome,
};
