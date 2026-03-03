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
    settle_runtime_event_updates_default,
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
pub use runtime::startup::apply_initial_section_assignments;
pub use runtime::state_index::build_state_name_to_idx;
pub use runtime::time::{stop_time_reached_with_tol, time_advanced_with_tol, time_match_with_tol};
pub use runtime::timeout::{
    SolverDeadlineGuard, TimeoutBudget, TimeoutExceeded, is_solver_timeout_panic,
    panic_on_expired_solver_deadline, run_timeout_result, run_timeout_step,
    run_timeout_step_result,
};
pub use simulation::runtime_prep::{compute_mass_matrix, pin_orphaned_variables};
