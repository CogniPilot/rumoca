use super::{schedule::RuntimeEventStop, solve_ops::EventPreMode};
use crate::timeline::sample_time_match_with_tol;

const ROOT_BISECTION_ITERS: usize = 64;

#[derive(Debug, Default)]
pub struct NoStateRootSearchScratch {
    start: Vec<f64>,
    end: Vec<f64>,
    lo: Vec<f64>,
    mid: Vec<f64>,
}

impl NoStateRootSearchScratch {
    pub fn new(root_count: usize) -> Self {
        Self {
            start: vec![0.0; root_count],
            end: vec![0.0; root_count],
            lo: vec![0.0; root_count],
            mid: vec![0.0; root_count],
        }
    }

    fn prepare(&mut self, root_count: usize) {
        self.start.resize(root_count, 0.0);
        self.end.resize(root_count, 0.0);
        self.lo.resize(root_count, 0.0);
        self.mid.resize(root_count, 0.0);
    }
}

pub fn first_no_state_root_crossing<E>(
    scratch: &mut NoStateRootSearchScratch,
    root_count: usize,
    t_start: f64,
    t_end: f64,
    tol: f64,
    mut evaluate: impl FnMut(f64, &mut [f64]) -> Result<(), E>,
) -> Result<Option<f64>, E> {
    if root_count == 0 {
        return Ok(None);
    }
    scratch.prepare(root_count);
    evaluate(t_start, &mut scratch.start)?;
    evaluate(t_end, &mut scratch.end)?;
    if !root_slice_crossed_after_start(&scratch.start, &scratch.end, tol) {
        return Ok(None);
    }

    scratch.lo.copy_from_slice(&scratch.start);
    let mut lo = t_start;
    let mut hi = t_end;
    for _ in 0..ROOT_BISECTION_ITERS {
        let mid = lo + 0.5 * (hi - lo);
        evaluate(mid, &mut scratch.mid)?;
        if root_slice_crossed_after_start(&scratch.lo, &scratch.mid, tol) {
            hi = mid;
        } else {
            lo = mid;
            scratch.lo.copy_from_slice(&scratch.mid);
        }
    }
    Ok(Some(hi))
}

fn root_slice_crossed_after_start(start: &[f64], end: &[f64], tol: f64) -> bool {
    start.iter().zip(end).any(|(a, b)| {
        root_surface_near_zero(*b, tol)
            || (!root_surface_near_zero(*a, tol) && a.signum() != b.signum())
    })
}

fn root_surface_near_zero(value: f64, tol: f64) -> bool {
    value.abs() <= tol
}

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
    fn max_accepted_step_size(&self) -> Option<f64>;
    fn next_scheduled_stop(&mut self, target: f64) -> Result<NoStateScheduledStop, Self::Error>;
    fn next_root_event_time(&mut self, target: f64, tol: f64) -> Result<Option<f64>, Self::Error>;
    fn handle_event_step(&mut self, step: NoStateEventStep) -> Result<(), Self::Error>;
    fn settle_accepted_step(&mut self) -> Result<(), Self::Error>;
    fn record_output(&mut self) -> Result<(), Self::Error>;
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
            backend.record_output()?;
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
    while no_state_step_is_required(
        backend.current_time(),
        target,
        tol,
        backend.max_accepted_step_size(),
    ) {
        let current = backend.current_time();
        let max_step = backend.max_accepted_step_size();
        let step_target = capped_no_state_step_target(current, target, max_step);
        let scheduled = backend.next_scheduled_stop(step_target)?;
        let root_event_time = backend.next_root_event_time(step_target, tol)?;
        let root_event = root_event_time
            .map(|root_time| scheduled.event_stop.is_none() || root_time < scheduled.stop_time)
            .unwrap_or(false);
        if scheduled.event_stop.is_none() && !root_event {
            backend.set_current_time(step_target);
            backend.settle_accepted_step()?;
            continue;
        }
        let step = NoStateEventStep {
            target: step_target,
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
        backend.settle_accepted_step()?;
    }
    Ok(true)
}

fn capped_no_state_step_target(current: f64, target: f64, max_step: Option<f64>) -> f64 {
    let Some(max_step) = max_step.filter(|step| step.is_finite() && *step > 0.0) else {
        return target;
    };
    let candidate = (current + max_step).min(target);
    if candidate > current {
        candidate
    } else {
        current.next_up().min(target)
    }
}

fn no_state_step_is_required(current: f64, target: f64, tol: f64, max_step: Option<f64>) -> bool {
    if target <= current {
        return false;
    }
    let gap = target - current;
    gap > tol
        || max_step
            .filter(|step| step.is_finite() && *step > 0.0)
            .is_some_and(|step| gap > step)
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
        max_step: Option<f64>,
        settled_times: Vec<f64>,
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

        fn max_accepted_step_size(&self) -> Option<f64> {
            self.max_step
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

        fn settle_accepted_step(&mut self) -> Result<(), Self::Error> {
            self.settled_times.push(self.current_time);
            Ok(())
        }

        fn record_output(&mut self) -> Result<(), Self::Error> {
            self.recorded_times.push(self.current_time);
            Ok(())
        }
    }

    #[test]
    fn output_schedule_skips_targets_before_current_right_limit_time() {
        let mut backend = RecordingBackend {
            current_time: 1.0e-6,
            max_step: None,
            settled_times: Vec::new(),
            recorded_times: Vec::new(),
        };

        run_no_state_output_schedule(&mut backend, [0.0, 0.1], 1.0e-6).unwrap();

        assert_eq!(backend.recorded_times, vec![0.1]);
    }

    #[test]
    fn output_schedule_settles_internal_points_at_the_accepted_step_limit() {
        let mut backend = RecordingBackend {
            max_step: Some(0.25),
            ..RecordingBackend::default()
        };

        run_no_state_output_schedule(&mut backend, [1.0], 1.0e-12).unwrap();

        assert_eq!(backend.settled_times, vec![0.25, 0.5, 0.75, 1.0]);
        assert_eq!(backend.recorded_times, vec![1.0]);
    }

    #[test]
    fn accepted_step_limit_is_honored_below_the_event_tolerance() {
        let mut backend = RecordingBackend {
            max_step: Some(0.25),
            ..RecordingBackend::default()
        };

        run_no_state_output_schedule(&mut backend, [1.0], 2.0).unwrap();

        assert_eq!(backend.settled_times, vec![0.25, 0.5, 0.75, 1.0]);
        assert_eq!(backend.recorded_times, vec![1.0]);
    }

    #[test]
    fn root_search_finds_earliest_surface_without_reallocating_scratch() {
        let mut scratch = NoStateRootSearchScratch::new(2);
        let start_ptr = scratch.start.as_ptr();
        let mid_ptr = scratch.mid.as_ptr();

        let root = first_no_state_root_crossing(&mut scratch, 2, 0.0, 1.0, 1.0e-12, |t, out| {
            out[0] = t - 0.75;
            out[1] = t - 0.25;
            Ok::<_, ()>(())
        })
        .unwrap()
        .unwrap();

        assert!((root - 0.25).abs() <= 1.0e-12);
        assert_eq!(scratch.start.as_ptr(), start_ptr);
        assert_eq!(scratch.mid.as_ptr(), mid_ptr);
    }

    #[test]
    fn root_search_does_not_retrigger_a_surface_at_the_accepted_start_time() {
        let mut scratch = NoStateRootSearchScratch::new(2);

        let root = first_no_state_root_crossing(&mut scratch, 2, 0.0, 1.0, 1.0e-12, |t, out| {
            out[0] = t;
            out[1] = t - 0.5;
            Ok::<_, ()>(())
        })
        .unwrap()
        .unwrap();

        assert!((root - 0.5).abs() <= 1.0e-12);
    }
}
