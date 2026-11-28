use super::*;

/// Focused simulation timeout floor in seconds (shared with the OMC reference run).
pub(super) const SIM_TIMEOUT_SECS: f64 = rumoca_worker::MSL_SIM_TIMEOUT_SECS;
/// Additional parent-process grace window so worker JSON parse/write overhead
/// does not cause false parent-side timeouts when solver budget is respected.
pub(super) const SIM_WORKER_TIMEOUT_GRACE_SECS: f64 = 2.0;

pub(super) fn sim_timeout_override_secs() -> Option<f64> {
    parity_config().simulation_timeout_override()
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
    let timeouts = rumoca_worker::ModelWorkerPhaseTimeouts::new(
        default_phase_timeout_secs,
        simulation_phase_timeout_secs,
    );
    // A raised simulation stage raises the solver's own budget instead
    // (`solver_timeout_secs`), and its watchdog follows with the grace.
    match parity_config().raised_stage {
        Some(raised) if raised.phase != rumoca_worker::WorkerProgressPhase::Sim => {
            timeouts.with_raised(raised.phase, default_phase_timeout_secs * raised.factor)
        }
        _ => timeouts,
    }
}

/// The budget a model's simulation runs under: the configured one, or a Sim
/// re-measurement's raised budget. Certification is still judged against the
/// configured budget ([`sim_timeout_secs`]).
pub(super) fn solver_timeout_secs() -> f64 {
    match parity_config().raised_stage {
        Some(raised) if raised.phase == rumoca_worker::WorkerProgressPhase::Sim => {
            sim_timeout_secs() * raised.factor
        }
        _ => sim_timeout_secs(),
    }
}

pub(super) fn sim_worker_memory_limit_mb() -> Option<usize> {
    parity_config().sim_worker_memory_mb
}

pub(super) fn artifact_file_ready(path: &Path) -> bool {
    path.metadata().is_ok_and(|metadata| metadata.len() > 0)
}
