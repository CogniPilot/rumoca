use super::{schedule::RuntimeEventStop, solve_ops::EventPreMode};
use crate::timeline::{event_right_limit_time, sample_time_match_with_tol};

#[derive(Debug, Clone, Copy)]
pub struct RuntimeEventBoundary {
    pub event_t: f64,
    pub horizon_t: f64,
    pub event: RuntimeEventStop,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct RuntimeEventBoundaryOutcome {
    pub final_t: f64,
    pub right_limit_t: Option<f64>,
}

pub trait RuntimeEventBoundaryHandler {
    type Error;

    fn on_event_time(&mut self, event_t: f64, event: RuntimeEventStop) -> Result<(), Self::Error>;

    fn on_event_right_limit(
        &mut self,
        right_t: f64,
        event: RuntimeEventStop,
    ) -> Result<(), Self::Error>;
}

pub fn process_runtime_event_boundary<H>(
    boundary: RuntimeEventBoundary,
    handler: &mut H,
) -> Result<RuntimeEventBoundaryOutcome, H::Error>
where
    H: RuntimeEventBoundaryHandler,
{
    handler.on_event_time(boundary.event_t, boundary.event)?;
    let right_t = runtime_event_right_limit(boundary);
    if should_process_right_limit(boundary.event, boundary.event_t, right_t) {
        handler.on_event_right_limit(right_t, boundary.event)?;
        return Ok(RuntimeEventBoundaryOutcome {
            final_t: right_t,
            right_limit_t: Some(right_t),
        });
    }
    Ok(RuntimeEventBoundaryOutcome {
        final_t: runtime_event_final_time(boundary.event, boundary.event_t, right_t),
        right_limit_t: None,
    })
}

pub fn runtime_event_right_limit(boundary: RuntimeEventBoundary) -> f64 {
    event_right_limit_time(boundary.event_t).min(boundary.horizon_t)
}

pub fn runtime_event_horizon(event: RuntimeEventStop, target: f64, horizon: f64) -> f64 {
    match (event.pre_mode, event.observe_right_limit) {
        (EventPreMode::EventEntry | EventPreMode::Fixed, _) => target,
        (EventPreMode::FollowCurrent, true) => horizon,
        (EventPreMode::FollowCurrent, false) => target,
    }
}

pub fn runtime_root_event_application_time(root_t: f64, target_t: f64) -> f64 {
    // A non-finite `target_t` (e.g. `f64::INFINITY` used by the interactive
    // stepper as an "unbounded next sample" sentinel) must never be reported as
    // the application time: `sample_time_match_with_tol(finite, INF)` is
    // spuriously true (its relative tolerance is itself infinite, so
    // `INF <= INF`), which would jump the event clock to infinity and NaN the
    // rebuilt solver. Guard the match on a finite target; the `.min(target_t)`
    // below is already a no-op for `INF`, leaving the right-limit of the root.
    if target_t.is_finite() && sample_time_match_with_tol(root_t, target_t) {
        target_t
    } else {
        event_right_limit_time(root_t).min(target_t)
    }
}

fn runtime_event_final_time(event: RuntimeEventStop, event_t: f64, right_t: f64) -> f64 {
    if event.observe_right_limit {
        right_t
    } else {
        event_t
    }
}

fn should_process_right_limit(event: RuntimeEventStop, event_t: f64, right_t: f64) -> bool {
    event.observe_right_limit && event.pre_mode == EventPreMode::FollowCurrent && right_t > event_t
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default)]
    struct RecordingHandler {
        event_times: Vec<f64>,
        right_limit_times: Vec<f64>,
    }

    impl RuntimeEventBoundaryHandler for RecordingHandler {
        type Error = ();

        fn on_event_time(
            &mut self,
            event_t: f64,
            _event: RuntimeEventStop,
        ) -> Result<(), Self::Error> {
            self.event_times.push(event_t);
            Ok(())
        }

        fn on_event_right_limit(
            &mut self,
            right_t: f64,
            _event: RuntimeEventStop,
        ) -> Result<(), Self::Error> {
            self.right_limit_times.push(right_t);
            Ok(())
        }
    }

    #[test]
    fn dynamic_time_event_processes_right_limit() {
        let mut handler = RecordingHandler::default();
        let outcome = process_runtime_event_boundary(
            RuntimeEventBoundary {
                event_t: 0.5,
                horizon_t: 1.0,
                event: RuntimeEventStop::dynamic_time_event(),
            },
            &mut handler,
        )
        .expect("event processing should succeed");

        assert_eq!(handler.event_times, vec![0.5]);
        assert_eq!(handler.right_limit_times.len(), 1);
        assert_eq!(
            outcome.right_limit_t,
            handler.right_limit_times.first().copied()
        );
        assert_eq!(outcome.final_t, handler.right_limit_times[0]);
    }

    #[test]
    fn event_entry_does_not_process_right_limit() {
        let mut handler = RecordingHandler::default();
        let outcome = process_runtime_event_boundary(
            RuntimeEventBoundary {
                event_t: 0.5,
                horizon_t: 1.0,
                event: RuntimeEventStop::static_event(EventPreMode::EventEntry),
            },
            &mut handler,
        )
        .expect("event processing should succeed");

        assert_eq!(handler.event_times, vec![0.5]);
        assert!(handler.right_limit_times.is_empty());
        assert_eq!(outcome.final_t, 0.5);
        assert_eq!(outcome.right_limit_t, None);
    }
}
