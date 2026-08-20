use super::*;

/// Per-model simulation timeout in seconds (shared with the OMC reference run).
pub(super) const SIM_TIMEOUT_SECS: f64 = rumoca_worker::MSL_SIM_TIMEOUT_SECS;
/// Additional parent-process grace window so worker JSON parse/write overhead
/// does not cause false parent-side timeouts when solver budget is respected.
pub(super) const SIM_WORKER_TIMEOUT_GRACE_SECS: f64 = 2.0;

/// Clamp a configured timeout override so it can only raise the budget.
fn raise_only_timeout(configured: Option<f64>, floor: f64) -> Option<f64> {
    configured
        .filter(|value| value.is_finite() && *value > 0.0)
        .map(|value| value.max(floor))
}

pub(super) fn sim_timeout_override_secs() -> Option<f64> {
    raise_only_timeout(parity_config().sim_timeout_secs, SIM_TIMEOUT_SECS)
}

pub(super) fn sim_timeout_secs() -> f64 {
    sim_timeout_override_secs().unwrap_or(SIM_TIMEOUT_SECS)
}

pub(super) fn model_worker_phase_timeouts(
    default_phase_timeout_secs: f64,
    solver_timeout_secs: f64,
) -> rumoca_worker::ModelWorkerPhaseTimeouts {
    let simulation_phase_timeout_secs =
        default_phase_timeout_secs.max(solver_timeout_secs + SIM_WORKER_TIMEOUT_GRACE_SECS);
    rumoca_worker::ModelWorkerPhaseTimeouts::new(
        default_phase_timeout_secs,
        simulation_phase_timeout_secs,
    )
}

pub(super) fn sim_worker_memory_limit_mb() -> Option<usize> {
    parity_config().sim_worker_memory_mb
}

pub(super) fn artifact_file_ready(path: &Path) -> bool {
    path.metadata().is_ok_and(|metadata| metadata.len() > 0)
}
