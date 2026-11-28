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
    phase_timeouts_with(
        default_phase_timeout_secs,
        solver_timeout_secs,
        parity_config().raised_stage,
    )
}

/// [`model_worker_phase_timeouts`] for an explicit re-measurement. A raised
/// simulation stage raises the solver's own budget instead
/// ([`solver_timeout_secs`]), and its watchdog follows with the grace.
pub(super) fn phase_timeouts_with(
    default_phase_timeout_secs: f64,
    solver_timeout_secs: f64,
    raised: Option<RaisedStage>,
) -> rumoca_worker::ModelWorkerPhaseTimeouts {
    let simulation_phase_timeout_secs =
        default_phase_timeout_secs.max(solver_timeout_secs + SIM_WORKER_TIMEOUT_GRACE_SECS);
    let timeouts = rumoca_worker::ModelWorkerPhaseTimeouts::new(
        default_phase_timeout_secs,
        simulation_phase_timeout_secs,
    );
    match raised {
        Some(raised) if raised.phase != rumoca_worker::WorkerProgressPhase::Sim => timeouts
            .with_raised(raised.phase, default_phase_timeout_secs * raised.factor)
            .expect("a raised factor is at least 1, refused at config load otherwise"),
        _ => timeouts,
    }
}

/// The budget a model's simulation runs under: the configured one, or a Sim
/// re-measurement's raised budget. Certification is still judged against the
/// configured budget ([`sim_timeout_secs`]).
pub(super) fn solver_timeout_secs() -> f64 {
    raised_solver_timeout_secs(sim_timeout_secs(), parity_config().raised_stage)
}

pub(super) fn raised_solver_timeout_secs(configured: f64, raised: Option<RaisedStage>) -> f64 {
    match raised {
        Some(raised) if raised.phase == rumoca_worker::WorkerProgressPhase::Sim => {
            configured * raised.factor
        }
        _ => configured,
    }
}

/// The time a raised compile stage adds to a model's compile wall: the
/// outer budgets built from the stage budget grow by exactly what the raised
/// stage gained, so none of them kills the re-measurement first.
pub(super) fn raised_compile_wall_delta_secs(
    stage_budget_secs: f64,
    raised: Option<RaisedStage>,
) -> f64 {
    match raised {
        Some(raised) if raised.phase != rumoca_worker::WorkerProgressPhase::Sim => {
            stage_budget_secs * (raised.factor - 1.0)
        }
        _ => 0.0,
    }
}

pub(super) fn sim_worker_memory_limit_mb() -> Option<usize> {
    parity_config().sim_worker_memory_mb
}

pub(super) fn artifact_file_ready(path: &Path) -> bool {
    path.metadata().is_ok_and(|metadata| metadata.len() > 0)
}

#[cfg(test)]
mod raised_stage_tests {
    use super::*;
    use rumoca_worker::WorkerProgressPhase;

    fn raised(phase: WorkerProgressPhase, factor: f64) -> Option<RaisedStage> {
        Some(RaisedStage { phase, factor })
    }

    #[test]
    fn a_raised_compile_stage_gets_its_time_and_the_outer_budgets_grow_by_it() {
        let solve = raised(WorkerProgressPhase::Solve, 2.5);
        let timeouts = phase_timeouts_with(20.0, 45.0, solve);
        assert_eq!(
            timeouts.timeout_secs_for(Some(WorkerProgressPhase::Solve)),
            50.0
        );
        assert_eq!(
            timeouts.timeout_secs_for(Some(WorkerProgressPhase::Flatten)),
            20.0
        );
        // The compile wall (and the queue budgets built on it) grows by
        // exactly what the stage gained; the solver is untouched.
        assert_eq!(raised_compile_wall_delta_secs(20.0, solve), 30.0);
        assert_eq!(raised_solver_timeout_secs(45.0, solve), 45.0);
    }

    #[test]
    fn a_raised_simulation_raises_the_solver_and_its_watchdog_follows() {
        let sim = raised(WorkerProgressPhase::Sim, 2.0);
        let solver = raised_solver_timeout_secs(45.0, sim);
        assert_eq!(solver, 90.0);
        let timeouts = phase_timeouts_with(20.0, solver, sim);
        assert_eq!(
            timeouts.timeout_secs_for(Some(WorkerProgressPhase::Sim)),
            90.0 + SIM_WORKER_TIMEOUT_GRACE_SECS
        );
        assert_eq!(
            timeouts.timeout_secs_for(Some(WorkerProgressPhase::Solve)),
            20.0
        );
        assert_eq!(raised_compile_wall_delta_secs(20.0, sim), 0.0);
        // Nothing raised: every budget as configured.
        assert_eq!(raised_solver_timeout_secs(45.0, None), 45.0);
        assert_eq!(raised_compile_wall_delta_secs(20.0, None), 0.0);
    }

    #[test]
    fn a_factor_outside_one_to_three_is_refused_at_config_load() {
        for factor in [0.5, 3.5] {
            let raw = format!(r#"{{"phase":"Sim","factor":{factor}}}"#);
            assert!(
                serde_json::from_str::<RaisedStage>(&raw).is_err(),
                "{factor}"
            );
        }
        assert!(serde_json::from_str::<RaisedStage>(r#"{"phase":"Sim","factor":2.0}"#).is_ok());
    }
}
