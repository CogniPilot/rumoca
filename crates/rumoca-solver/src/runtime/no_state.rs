use super::{schedule::RuntimeEventStop, solve_ops::EventPreMode};
use crate::timeline::sample_time_match_with_tol;

#[derive(Debug, Clone, Copy)]
pub struct NoStateScheduledStop {
    pub stop_time: f64,
    pub event_stop: Option<RuntimeEventStop>,
}

#[derive(Debug, Clone, Copy)]
pub struct NoStateEventStep {
    pub target: f64,
    pub stop_time: f64,
    pub event_stop: Option<RuntimeEventStop>,
    pub root_event_time: Option<f64>,
    pub root_event: bool,
    pub tol: f64,
}

impl NoStateEventStep {
    pub fn event_time(self) -> f64 {
        if self.root_event {
            self.root_event_time.unwrap_or(self.stop_time)
        } else {
            self.stop_time
        }
    }

    pub fn pre_mode(self) -> EventPreMode {
        if self.root_event {
            EventPreMode::FollowCurrent
        } else {
            self.event_stop
                .map(|event| event.pre_mode)
                .unwrap_or(EventPreMode::FollowCurrent)
        }
    }
}

pub trait NoStateOrchestrationBackend {
    type Error;

    fn current_time(&self) -> f64;
    fn set_current_time(&mut self, time: f64);
    fn next_scheduled_stop(&mut self, target: f64) -> Result<NoStateScheduledStop, Self::Error>;
    fn next_root_event_time(&mut self, target: f64, tol: f64) -> Result<Option<f64>, Self::Error>;
    fn handle_event_step(&mut self, step: NoStateEventStep) -> Result<(), Self::Error>;
    fn settle_and_record_output(&mut self) -> Result<(), Self::Error>;
}

pub fn run_no_state_output_schedule<B, I>(
    backend: &mut B,
    output_times: I,
    tol: f64,
) -> Result<(), B::Error>
where
    B: NoStateOrchestrationBackend,
    I: IntoIterator<Item = f64>,
{
    for target in output_times {
        if advance_no_state_to_target(backend, target, tol)? {
            backend.settle_and_record_output()?;
        }
    }
    Ok(())
}

fn advance_no_state_to_target<B>(backend: &mut B, target: f64, tol: f64) -> Result<bool, B::Error>
where
    B: NoStateOrchestrationBackend,
{
    if no_state_output_target_is_stale(backend.current_time(), target) {
        return Ok(false);
    }
    while target > backend.current_time() + tol {
        let scheduled = backend.next_scheduled_stop(target)?;
        let root_event_time = backend.next_root_event_time(target, tol)?;
        let root_event = root_event_time
            .map(|root_time| scheduled.event_stop.is_none() || root_time < scheduled.stop_time)
            .unwrap_or(false);
        if scheduled.event_stop.is_none() && !root_event {
            backend.set_current_time(target);
            break;
        }
        let step = NoStateEventStep {
            target,
            stop_time: scheduled.stop_time,
            event_stop: scheduled.event_stop,
            root_event_time,
            root_event,
            tol,
        };
        backend.handle_event_step(step)?;
    }
    if backend.current_time() < target {
        backend.set_current_time(target);
    }
    Ok(true)
}

fn no_state_output_target_is_stale(current: f64, target: f64) -> bool {
    current > target && !sample_time_match_with_tol(current, target)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default)]
    struct RecordingBackend {
        current_time: f64,
        recorded_times: Vec<f64>,
    }

    impl NoStateOrchestrationBackend for RecordingBackend {
        type Error = ();

        fn current_time(&self) -> f64 {
            self.current_time
        }

        fn set_current_time(&mut self, time: f64) {
            self.current_time = time;
        }

        fn next_scheduled_stop(
            &mut self,
            target: f64,
        ) -> Result<NoStateScheduledStop, Self::Error> {
            Ok(NoStateScheduledStop {
                stop_time: target,
                event_stop: None,
            })
        }

        fn next_root_event_time(
            &mut self,
            _target: f64,
            _tol: f64,
        ) -> Result<Option<f64>, Self::Error> {
            Ok(None)
        }

        fn handle_event_step(&mut self, _step: NoStateEventStep) -> Result<(), Self::Error> {
            Ok(())
        }

        fn settle_and_record_output(&mut self) -> Result<(), Self::Error> {
            self.recorded_times.push(self.current_time);
            Ok(())
        }
    }

    #[test]
    fn output_schedule_skips_targets_before_current_right_limit_time() {
        let mut backend = RecordingBackend {
            current_time: 1.0e-6,
            recorded_times: Vec::new(),
        };

        run_no_state_output_schedule(&mut backend, [0.0, 0.1], 1.0e-6).unwrap();

        assert_eq!(backend.recorded_times, vec![0.1]);
    }
}
