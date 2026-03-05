use super::schedule::RuntimeStopSchedule;
use rumoca_ir_dae as dae;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BackendState {
    pub t: f64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StepUntilOutcome {
    InternalStep,
    RootFound { t_root: f64 },
    StopReached,
    Finished,
}

/// Minimal contract a solver backend implements to participate in Rumoca runtime orchestration.
///
/// Backends own solver-specific integration details and expose only:
/// - time advancement to a stop time (`step_until`)
/// - event update application (`apply_event_updates`)
/// - current simulation time (`read_state`)
///
/// `run_with_runtime_schedule` drives this interface and applies Rumoca's shared
/// event/time scheduling semantics independently of the concrete solver.
pub trait SimulationBackend {
    type Error;

    fn init(&mut self) -> Result<(), Self::Error>;
    fn step_until(&mut self, stop_time: f64) -> Result<StepUntilOutcome, Self::Error>;
    fn read_state(&self) -> BackendState;
    fn apply_event_updates(&mut self, event_time: f64) -> Result<(), Self::Error>;
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct LoopStats {
    pub steps: usize,
    pub root_hits: usize,
}

fn stop_time_reached_with_tol(current_t: f64, target_t: f64) -> bool {
    let tol = 1e-9 * (1.0 + current_t.abs().max(target_t.abs()));
    (current_t - target_t).abs() <= tol || current_t >= target_t
}

pub fn run_with_runtime_schedule<B, C>(
    backend: &mut B,
    dae_model: &dae::Dae,
    t_start: f64,
    t_end: f64,
    mut check_budget: C,
) -> Result<LoopStats, B::Error>
where
    B: SimulationBackend,
    C: FnMut() -> Result<(), B::Error>,
{
    backend.init()?;
    let mut stats = LoopStats::default();
    let mut schedule =
        RuntimeStopSchedule::from_dae(dae_model, t_start, backend.read_state().t, t_end);
    let mut active_stop = schedule.active_stop();

    loop {
        let state = backend.read_state();
        if stop_time_reached_with_tol(state.t, t_end) {
            break;
        }
        if stop_time_reached_with_tol(state.t, active_stop)
            && !stop_time_reached_with_tol(state.t, t_end)
        {
            active_stop = schedule.rearm(state.t, t_end);
        }

        check_budget()?;
        let outcome = backend.step_until(active_stop)?;
        if !matches!(outcome, StepUntilOutcome::Finished) {
            stats.steps += 1;
        }

        match outcome {
            StepUntilOutcome::InternalStep => {}
            StepUntilOutcome::RootFound { t_root } => {
                stats.root_hits += 1;
                backend.apply_event_updates(t_root)?;
                let state_after = backend.read_state();
                if stop_time_reached_with_tol(state_after.t, t_end) {
                    break;
                }
                active_stop = schedule.rearm(state_after.t, t_end);
            }
            StepUntilOutcome::StopReached => {
                let state_after_step = backend.read_state();
                if stop_time_reached_with_tol(state_after_step.t, t_end) {
                    break;
                }
                backend.apply_event_updates(active_stop)?;
                let state_after_event = backend.read_state();
                if stop_time_reached_with_tol(state_after_event.t, t_end) {
                    break;
                }
                active_stop = schedule.rearm(state_after_event.t, t_end);
            }
            StepUntilOutcome::Finished => break,
        }
    }

    Ok(stats)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::VecDeque;

    #[derive(Clone, Copy)]
    enum MockStep {
        Internal { t: f64 },
        Root { t: f64, root: f64 },
        Stop { t: f64 },
        Finished,
    }

    #[derive(Default)]
    struct MockBackend {
        t: f64,
        steps: VecDeque<MockStep>,
        event_updates: Vec<f64>,
        init_calls: usize,
    }

    impl SimulationBackend for MockBackend {
        type Error = String;

        fn init(&mut self) -> Result<(), Self::Error> {
            self.init_calls += 1;
            Ok(())
        }

        fn step_until(&mut self, _stop_time: f64) -> Result<StepUntilOutcome, Self::Error> {
            let step = self
                .steps
                .pop_front()
                .ok_or_else(|| "no mock steps".to_string())?;
            match step {
                MockStep::Internal { t } => {
                    self.t = t;
                    Ok(StepUntilOutcome::InternalStep)
                }
                MockStep::Root { t, root } => {
                    self.t = t;
                    Ok(StepUntilOutcome::RootFound { t_root: root })
                }
                MockStep::Stop { t } => {
                    self.t = t;
                    Ok(StepUntilOutcome::StopReached)
                }
                MockStep::Finished => Ok(StepUntilOutcome::Finished),
            }
        }

        fn read_state(&self) -> BackendState {
            BackendState { t: self.t }
        }

        fn apply_event_updates(&mut self, event_time: f64) -> Result<(), Self::Error> {
            self.event_updates.push(event_time);
            self.t = event_time + 1.0e-6;
            Ok(())
        }
    }

    #[test]
    fn orchestration_tracks_steps_and_roots() {
        let mut backend = MockBackend {
            t: 0.0,
            steps: VecDeque::from(vec![
                MockStep::Internal { t: 0.2 },
                MockStep::Root { t: 0.4, root: 0.35 },
                MockStep::Stop { t: 1.0 },
            ]),
            ..Default::default()
        };
        let dae_model = dae::Dae {
            scheduled_time_events: vec![1.0],
            ..Default::default()
        };

        let stats = run_with_runtime_schedule(&mut backend, &dae_model, 0.0, 1.0, || Ok(()))
            .expect("orchestration should succeed");
        assert_eq!(backend.init_calls, 1);
        assert_eq!(stats.steps, 3);
        assert_eq!(stats.root_hits, 1);
        assert_eq!(backend.event_updates.len(), 1);
        assert!((backend.event_updates[0] - 0.35).abs() <= 1.0e-12);
    }

    #[test]
    fn stop_reached_triggers_scheduled_event_update() {
        let mut backend = MockBackend {
            t: 0.0,
            steps: VecDeque::from(vec![MockStep::Stop { t: 0.5 }, MockStep::Stop { t: 1.0 }]),
            ..Default::default()
        };
        let dae_model = dae::Dae {
            scheduled_time_events: vec![0.5],
            ..Default::default()
        };

        let stats = run_with_runtime_schedule(&mut backend, &dae_model, 0.0, 1.0, || Ok(()))
            .expect("orchestration should succeed");
        assert_eq!(stats.steps, 2);
        assert_eq!(stats.root_hits, 0);
        assert_eq!(backend.event_updates.len(), 1);
        assert!((backend.event_updates[0] - 0.5).abs() <= 1.0e-12);
    }

    #[test]
    fn finished_outcome_exits_without_counting_step() {
        let mut backend = MockBackend {
            t: 0.0,
            steps: VecDeque::from(vec![MockStep::Finished]),
            ..Default::default()
        };
        let dae_model = dae::Dae::default();

        let stats = run_with_runtime_schedule(&mut backend, &dae_model, 0.0, 1.0, || Ok(()))
            .expect("orchestration should succeed");
        assert_eq!(stats.steps, 0);
        assert_eq!(stats.root_hits, 0);
    }
}
