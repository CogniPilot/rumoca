use rumoca_ir_solve as solve;

const MAX_OUTPUT_TIME_COUNT: usize = 10_000_000;

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum OutputTimelineError {
    #[error("simulation time bounds must be finite and ordered")]
    InvalidBounds,
    #[error("simulation output interval must be finite and positive")]
    InvalidInterval,
    #[error("simulation output grid would contain too many samples")]
    TooManySamples,
    #[error("simulation output interval does not advance at this time magnitude")]
    NonAdvancingInterval,
}

pub fn build_output_times(t_start: f64, t_end: f64, dt: f64) -> Vec<f64> {
    try_build_output_times(t_start, t_end, dt).unwrap_or_else(|_| {
        if t_start == t_end {
            vec![t_start]
        } else {
            vec![t_start, t_end]
        }
    })
}

pub fn try_build_output_times(
    t_start: f64,
    t_end: f64,
    dt: f64,
) -> Result<Vec<f64>, OutputTimelineError> {
    if !t_start.is_finite() || !t_end.is_finite() || t_end < t_start {
        return Err(OutputTimelineError::InvalidBounds);
    }
    if t_start == t_end {
        return Ok(vec![t_start]);
    }
    if !dt.is_finite() || dt <= 0.0 {
        return Err(OutputTimelineError::InvalidInterval);
    }

    let quotient = (t_end - t_start) / dt;
    if !quotient.is_finite() || quotient > MAX_OUTPUT_TIME_COUNT as f64 {
        return Err(OutputTimelineError::TooManySamples);
    }
    let grid_count = (quotient.floor() as usize)
        .checked_add(1)
        .ok_or(OutputTimelineError::TooManySamples)?;
    // Reserve one slot for an exact endpoint when the arithmetic grid does not
    // land on it. Keeping the total bounded is more important than admitting
    // the final boundary case of the safety cap.
    if grid_count >= MAX_OUTPUT_TIME_COUNT {
        return Err(OutputTimelineError::TooManySamples);
    }
    let mut times = Vec::new();
    times
        .try_reserve_exact(
            grid_count
                .checked_add(1)
                .ok_or(OutputTimelineError::TooManySamples)?,
        )
        .map_err(|_| OutputTimelineError::TooManySamples)?;
    for k in 0..grid_count {
        let t_raw = t_start + (k as f64) * dt;
        if times
            .last()
            .is_some_and(|previous| t_raw <= *previous && t_raw != t_end)
        {
            return Err(OutputTimelineError::NonAdvancingInterval);
        }
        let t = if k > 0 && (t_raw >= t_end || output_time_match_with_ulp(t_raw, t_end)) {
            t_end
        } else {
            t_raw
        };
        if times.last().is_some_and(|previous| t <= *previous) {
            return Err(OutputTimelineError::NonAdvancingInterval);
        }
        times.push(t);
        if t == t_end {
            return Ok(times);
        }
    }
    if !times
        .last()
        .is_some_and(|last| output_time_match_with_ulp(*last, t_end))
    {
        times.push(t_end);
    } else if let Some(last) = times.last_mut() {
        *last = t_end;
    }
    Ok(times)
}

fn output_time_match_with_ulp(left: f64, right: f64) -> bool {
    if left == right {
        return true;
    }
    let ulp = [left, right]
        .into_iter()
        .flat_map(|value| {
            [
                (value.next_up() - value).abs(),
                (value - value.next_down()).abs(),
            ]
        })
        .filter(|spacing| spacing.is_finite())
        .fold(0.0, f64::max);
    ulp > 0.0 && (left - right).abs() <= ulp
}

pub fn sample_time_match_with_tol(a: f64, b: f64) -> bool {
    (a - b).abs() <= sample_time_tolerance(a, b)
}

fn sample_time_tolerance(a: f64, b: f64) -> f64 {
    rumoca_core::SCHEDULE_TIME_RELATIVE_TOLERANCE * (1.0 + a.abs().max(b.abs()))
}

#[derive(Debug, Clone)]
pub struct ScheduledTimeEvents {
    events: Vec<f64>,
    next_idx: usize,
}

impl ScheduledTimeEvents {
    pub fn new(events: Vec<f64>, t_start: f64) -> Self {
        let mut schedule = Self {
            events,
            next_idx: 0,
        };
        schedule.advance_past(t_start);
        schedule
    }

    fn advance_past(&mut self, t_current: f64) {
        while self.next_idx < self.events.len() {
            let event_t = self.events[self.next_idx];
            if event_t < t_current || sample_time_match_with_tol(event_t, t_current) {
                self.next_idx += 1;
            } else {
                break;
            }
        }
    }

    pub fn next_stop_time(&mut self, t_current: f64, t_end: f64) -> f64 {
        self.advance_past(t_current);
        if self.next_idx < self.events.len() {
            let event_t = self.events[self.next_idx];
            if event_t < t_end && !sample_time_match_with_tol(event_t, t_end) {
                return event_t;
            }
        }
        t_end
    }
}

pub fn periodic_event_times(
    schedules: &[solve::PeriodicEventSchedule],
    current_t: f64,
    target_t: f64,
) -> impl Iterator<Item = f64> + '_ {
    std::iter::successors(
        next_periodic_event_time(schedules, current_t, target_t),
        move |event_t| next_periodic_event_time(schedules, *event_t, target_t),
    )
}

pub fn next_periodic_event_time(
    schedules: &[solve::PeriodicEventSchedule],
    current_t: f64,
    target_t: f64,
) -> Option<f64> {
    schedules
        .iter()
        .filter_map(|schedule| next_schedule_event_time(schedule, current_t, target_t))
        .min_by(f64::total_cmp)
}

fn next_schedule_event_time(
    schedule: &solve::PeriodicEventSchedule,
    current_t: f64,
    target_t: f64,
) -> Option<f64> {
    let period = schedule.period_seconds();
    let phase = schedule.phase_seconds();
    if !period.is_finite()
        || !phase.is_finite()
        || period <= 0.0
        || !current_t.is_finite()
        || !target_t.is_finite()
    {
        return None;
    }
    let mut tick = ((current_t - phase) / period).floor().max(0.0);
    for _ in 0..4 {
        let event_t = schedule_tick_time(schedule, tick);
        if event_time_in_window(event_t, current_t, target_t)
            && !sample_time_match_with_tol(event_t, current_t)
        {
            return Some(event_t);
        }
        if event_t > target_t && !sample_time_match_with_tol(event_t, target_t) {
            return None;
        }
        let tolerance = sample_time_tolerance(event_t, current_t);
        let first_distinct_time = current_t + tolerance;
        let next_tick = (((first_distinct_time - phase) / period).floor() + 1.0)
            .max(tick + 1.0)
            .max(0.0);
        if !next_tick.is_finite() || next_tick <= tick {
            return None;
        }
        tick = next_tick;
    }
    None
}

/// MLS §16.3 tick instant `phase + index * period`.
///
/// This is the one place where the exact rational lattice becomes a simulation
/// time. Evaluating the tick in exact integer arithmetic and rounding once
/// keeps a long-horizon grid on the analytic instants.
pub fn schedule_tick_time(schedule: &solve::PeriodicEventSchedule, tick: f64) -> f64 {
    let fallback = schedule.phase_seconds() + tick * schedule.period_seconds();
    if tick != tick.trunc()
        || !tick.is_finite()
        || tick >= i128::MAX as f64
        || tick <= i128::MIN as f64
    {
        return fallback;
    }
    try_schedule_tick_time(schedule, tick as i128).unwrap_or(fallback)
}

/// Fallible exact form of [`schedule_tick_time`].
///
/// The integer index uses the exact MLS clock lattice and preserves its
/// structured overflow/representability error. The `f64` convenience wrapper
/// retains the seconds fallback for callers whose public API cannot return an
/// error.
pub fn try_schedule_tick_time(
    schedule: &solve::PeriodicEventSchedule,
    tick: i128,
) -> Result<f64, rumoca_core::ClockLatticeErrorKind> {
    schedule.exact_tick_time_seconds(tick)
}

pub fn event_time_in_window(event_t: f64, current_t: f64, target_t: f64) -> bool {
    event_t.is_finite()
        && event_t > current_t
        && (event_t < target_t || sample_time_match_with_tol(event_t, target_t))
}

pub fn scheduled_time_in_horizon(event_t: f64, t_start: f64, t_end: f64) -> bool {
    event_t.is_finite()
        && (event_t > t_start || sample_time_match_with_tol(event_t, t_start))
        && (event_t < t_end || sample_time_match_with_tol(event_t, t_end))
}

pub fn periodic_schedule_matches_time(schedule: &solve::PeriodicEventSchedule, t: f64) -> bool {
    let period = schedule.period_seconds();
    let phase = schedule.phase_seconds();
    if !period.is_finite() || !phase.is_finite() || period <= 0.0 || t + period < phase {
        return false;
    }
    let ticks = (t - phase) / period;
    ticks >= 0.0 && sample_time_match_with_tol(ticks, ticks.round())
}

pub fn scheduled_root_matches_time(root: &solve::ScheduledRootCondition, t: f64) -> bool {
    let Ok(lattice) =
        rumoca_core::ClockLattice::from_seconds(root.period_seconds, root.phase_seconds)
    else {
        return false;
    };
    let Ok(schedule) = solve::PeriodicEventSchedule::new(lattice) else {
        return false;
    };
    periodic_schedule_matches_time(&schedule, t)
}

pub fn scheduled_root_indices_at_time(
    roots: &[solve::ScheduledRootCondition],
    t: f64,
) -> Vec<usize> {
    roots
        .iter()
        .filter(|root| scheduled_root_matches_time(root, t))
        .map(|root| root.root_index)
        .collect()
}

pub fn scheduled_root_index_is_known(
    roots: &[solve::ScheduledRootCondition],
    root_index: usize,
) -> bool {
    roots.iter().any(|root| root.root_index == root_index)
}

pub fn runtime_parameter_index(layout: &solve::SolveLayout, name: &str) -> Option<usize> {
    layout
        .input_parameter_index(name)
        .or_else(|| layout.discrete_real_parameter_index(name))
        .or_else(|| layout.discrete_valued_parameter_index(name))
}

pub fn merge_evaluation_times(output_times: &[f64], injected_times: &[f64]) -> Vec<f64> {
    let mut merged = output_times.to_vec();
    merged.extend_from_slice(injected_times);
    merged.sort_by(f64::total_cmp);
    merged.dedup_by(|a, b| sample_time_match_with_tol(*a, *b));
    merged
}

fn next_representable_time(t_event: f64) -> f64 {
    if !t_event.is_finite() {
        return t_event;
    }
    if t_event == 0.0 {
        return f64::from_bits(1);
    }
    let bits = t_event.to_bits();
    if t_event > 0.0 {
        f64::from_bits(bits + 1)
    } else {
        f64::from_bits(bits - 1)
    }
}

fn previous_representable_time(t_event: f64) -> f64 {
    if !t_event.is_finite() {
        return t_event;
    }
    if t_event == 0.0 {
        return -f64::from_bits(1);
    }
    let bits = t_event.to_bits();
    if t_event > 0.0 {
        f64::from_bits(bits - 1)
    } else {
        f64::from_bits(bits + 1)
    }
}

pub fn event_left_limit_time(t_event: f64) -> f64 {
    previous_representable_time(t_event)
}

/// Return an evaluation time that is semantically before an event boundary.
///
/// The immediately preceding `f64` is suitable for dense continuous-state
/// interpolation, but it can still compare equal to the event instant under
/// clock and scheduler tolerances. Use this probe for time-sensitive
/// algebraic/discrete evaluation at `t-`.
pub fn event_left_probe_time(t_event: f64, tolerance: f64) -> f64 {
    if !t_event.is_finite() {
        return t_event;
    }
    let adjacent = previous_representable_time(t_event);
    let resolution = event_boundary_probe_resolution(t_event, tolerance);
    if resolution == 0.0 {
        return adjacent;
    }
    let candidate = t_event - 2.0 * resolution;
    if candidate.is_finite() && candidate < t_event {
        adjacent.min(candidate)
    } else {
        adjacent
    }
}

pub fn event_right_limit_time(t_event: f64, tolerance: f64) -> f64 {
    if !t_event.is_finite() {
        return t_event;
    }
    let adjacent = next_representable_time(t_event);
    let resolution = tolerance.abs();
    if !resolution.is_finite() || resolution == 0.0 {
        return adjacent;
    }
    adjacent.max(t_event + 2.0 * resolution)
}

/// Relative resolution of the semantic left-limit probe at an event boundary.
///
/// This is deliberately looser than [`rumoca_core::SCHEDULE_TIME_RELATIVE_TOLERANCE`],
/// and the difference is not an oversight. That constant bounds a
/// *dimensionless* tick coordinate, so the width of a tick-recognition window
/// measured in seconds scales with the clock period: for a period-`P` clock the
/// window around tick `k` is `P * tol * (1 + k)`, i.e. about `tol * (P + t)`.
/// A probe placed a fixed multiple of `tol * (1 + t)` before the event would
/// therefore sit right on the edge of that window for any clock slower than a
/// couple of seconds — including the 600 s and 1200 s clocks MSL's
/// `PeriodicExactClock` builds via `subSample(Clock(factor), resolutionFactor)`.
/// Keeping the probe three orders of magnitude coarser puts `t-` outside the
/// tick window for every clock period the scheduler can represent, while
/// staying far below any solver output resolution.
const EVENT_BOUNDARY_PROBE_RELATIVE_RESOLUTION: f64 = 1.0e-9;

fn event_boundary_probe_resolution(t_event: f64, tolerance: f64) -> f64 {
    let requested = tolerance.abs();
    let clock_resolution = EVENT_BOUNDARY_PROBE_RELATIVE_RESOLUTION * (1.0 + t_event.abs());
    if !requested.is_finite() || !clock_resolution.is_finite() {
        return 0.0;
    }
    requested.max(clock_resolution)
}

pub fn merge_output_times_with_event_observations(
    output_times: &[f64],
    event_times: &[f64],
    t_end: f64,
) -> Vec<f64> {
    let mut merged = output_times.to_vec();
    merged.extend_from_slice(event_times);
    let _ = t_end;
    merged.sort_by(f64::total_cmp);
    merged.dedup_by(|a, b| sample_time_match_with_tol(*a, *b));
    merged
}

#[cfg(test)]
mod tests {
    use super::*;

    fn periodic(period: f64, phase: f64) -> solve::PeriodicEventSchedule {
        solve::PeriodicEventSchedule::new(
            rumoca_core::ClockLattice::from_seconds(period, phase).unwrap(),
        )
        .unwrap()
    }

    #[test]
    fn scheduled_time_events_advances_exact_boundary_hits() {
        let mut schedule = ScheduledTimeEvents::new(vec![0.2, 0.5], 0.0);
        assert!((schedule.next_stop_time(0.0, 1.0) - 0.2).abs() < 1e-15);
        assert!((schedule.next_stop_time(0.2, 1.0) - 0.5).abs() < 1e-15);
        assert!((schedule.next_stop_time(0.5, 1.0) - 1.0).abs() < 1e-15);
    }

    #[test]
    fn scheduled_time_events_skips_past_event_with_tolerance() {
        let event = 0.2_f64;
        let tol = 1e-12 * (1.0 + event.abs());
        let mut schedule = ScheduledTimeEvents::new(vec![event], 0.0);
        assert!((schedule.next_stop_time(event + 0.5 * tol, 1.0) - 1.0).abs() < 1e-15);
    }

    #[test]
    fn scheduled_time_events_does_not_schedule_event_at_t_end() {
        let mut schedule = ScheduledTimeEvents::new(vec![1.0, 1.5], 0.0);
        assert!((schedule.next_stop_time(0.0, 1.0) - 1.0).abs() < 1e-15);
    }

    #[test]
    fn merge_evaluation_times_deduplicates_near_equal_entries() {
        let output = vec![0.0, 0.5, 1.0];
        let injected = vec![0.5 + 1.0e-16, 0.75];
        let merged = merge_evaluation_times(&output, &injected);
        assert_eq!(merged, vec![0.0, 0.5, 0.75, 1.0]);
    }

    #[test]
    fn merge_output_times_with_event_observations_adds_event_right_limits() {
        let output = vec![0.0, 1.0];
        let events = vec![0.0, 0.25, 1.0];
        let merged = merge_output_times_with_event_observations(&output, &events, 1.0);
        assert_eq!(merged[0], 0.0);
        assert!(merged.iter().any(|t| sample_time_match_with_tol(*t, 0.25)));
        assert_eq!(merged.last().copied(), Some(1.0));
    }

    #[test]
    fn event_right_limit_time_uses_one_representable_step() {
        let t_event = 0.001_f64;
        let t_right = event_right_limit_time(t_event, 0.0);
        assert_eq!(t_right, t_event.next_up());
        assert!(t_right - t_event < 1.0e-12);
    }

    #[test]
    fn event_right_limit_time_scales_with_solver_resolution() {
        let t_event = 1000.0_f64;
        let t_right = event_right_limit_time(t_event, 1.0e-10);
        assert!(t_right > t_event);
        assert!(t_right - t_event < 1.0e-8);
    }

    #[test]
    fn event_left_limit_time_uses_previous_representable_time() {
        let t_event = 0.5_f64;
        let t_left = event_left_limit_time(t_event);
        assert!(t_left < t_event);
        assert!(
            event_right_limit_time(t_left, 0.0)
                .total_cmp(&t_event)
                .is_ge()
        );
    }

    #[test]
    fn event_left_probe_is_outside_clock_and_scheduler_tolerances() {
        let event = 0.02_f64;
        let left = event_left_probe_time(event, 1.0e-12);

        assert!(left < event);
        assert!(!sample_time_match_with_tol(left, event));
        assert!(event - left > rumoca_core::SCHEDULE_TIME_RELATIVE_TOLERANCE * (1.0 + event.abs()));
    }

    /// Seconds width of the tick-recognition window of a period-`period` clock
    /// at the tick nearest `t_event`, using the formula shared by
    /// `rumoca_eval_dae::eval::clock_eval::is_clock_tick` and the lowered
    /// clock rows in `rumoca_phase_solve::lower::clock`.
    fn tick_window_seconds(period: f64, t_event: f64) -> f64 {
        let ticks = t_event / period;
        let nearest = ticks.round();
        period
            * rumoca_core::SCHEDULE_TIME_RELATIVE_TOLERANCE
            * (1.0 + ticks.abs().max(nearest.abs()))
    }

    #[test]
    fn event_left_probe_clears_the_tick_window_of_slow_clocks() {
        // MSL 4.1.0 `PeriodicExactClock(factor = 20, resolution = min)` is a
        // 1200 s clock. The tick tolerance is dimensionless, so its width in
        // seconds grows with the period; the probe resolution must be coarse
        // enough that `t-` is unambiguously outside it, not merely adjacent.
        let period = 1200.0_f64;
        for tick in [1.0_f64, 10.0, 1000.0] {
            let event = tick * period;
            let left = event_left_probe_time(event, 0.0);
            let window = tick_window_seconds(period, event);

            assert!(left < event);
            assert!(
                event - left > 100.0 * window,
                "left probe at t={event} must clear the {window} s tick window, gap was {}",
                event - left
            );
        }
    }

    #[test]
    fn event_left_probe_resolution_is_pinned() {
        // The probe resolution is an intentional constant, not a follower of
        // the scheduler tick tolerance; pin it so a future rename cannot move
        // it silently.
        let event = 1000.0_f64;

        assert_eq!(
            event_boundary_probe_resolution(event, 0.0),
            EVENT_BOUNDARY_PROBE_RELATIVE_RESOLUTION * (1.0 + event)
        );
        assert_eq!(EVENT_BOUNDARY_PROBE_RELATIVE_RESOLUTION, 1.0e-9);
        // A coarser solver tolerance still wins.
        assert_eq!(event_boundary_probe_resolution(event, 1.0e-3), 1.0e-3);
    }

    #[test]
    fn build_output_times_handles_zero_span_and_invalid_dt() {
        assert_eq!(build_output_times(1.0, 1.0, 0.0), vec![1.0]);
        assert_eq!(build_output_times(1.0, 2.0, 0.0), vec![1.0, 2.0]);
        assert_eq!(try_build_output_times(1.0, 1.0, 0.0), Ok(vec![1.0]));
    }

    #[test]
    fn try_build_output_times_rejects_unrepresentable_or_unbounded_grids() {
        assert_eq!(
            try_build_output_times(1.0, 2.0, 1.0e-320),
            Err(OutputTimelineError::TooManySamples)
        );
        assert_eq!(
            try_build_output_times(1.0e20, f64::from_bits(1.0e20_f64.to_bits() + 1), 1.0),
            Err(OutputTimelineError::NonAdvancingInterval)
        );
    }

    #[test]
    fn output_grid_does_not_collapse_distinct_large_absolute_times() {
        let start = 1.0e15;
        let times = try_build_output_times(start, start + 1.0, 0.25)
            .expect("representable large-time grid");

        assert_eq!(
            times,
            vec![start, start + 0.25, start + 0.5, start + 0.75, start + 1.0]
        );
    }

    #[test]
    fn next_periodic_event_is_not_limited_by_prior_tick_count() {
        let schedules = [periodic(1.0e-4, 0.0)];

        let next = next_periodic_event_time(&schedules, 20.000_05, 100.0)
            .expect("ticks after the former 200,000-event cap must remain scheduled");

        assert!(sample_time_match_with_tol(next, 20.000_1));
    }

    #[test]
    fn tick_grid_is_generated_from_the_exact_rational_lattice() {
        // MLS §16.3: tick k of a periodic clock is exactly `phase + k * period`.
        // At this horizon the f64 product misses the analytic instant.
        let schedule = periodic(0.001, 0.0);

        assert_ne!(999_983.0_f64 * 0.001, 999.983);
        assert_eq!(schedule_tick_time(&schedule, 999_983.0), 999.983);
    }

    #[test]
    fn exact_tick_scheduler_supports_two_to_the_sixty_third() {
        let schedule = periodic(1.0, 0.0);
        let boundary = 1i128 << 63;

        assert_eq!(
            try_schedule_tick_time(&schedule, boundary),
            Ok(boundary as f64)
        );
    }

    #[test]
    fn long_horizon_scheduling_lands_on_the_exact_instant() {
        let schedules = [periodic(0.001, 0.0)];

        let next = next_periodic_event_time(&schedules, 999.982_5, 1000.0)
            .expect("a tick must remain scheduled at this horizon");

        assert_eq!(next, 999.983, "the tick grid must not accumulate f64 error");
    }

    #[test]
    fn shifted_schedule_ticks_stay_on_the_lattice() {
        // phase 1/3 s, period 1/3 s: tick k lands on (k + 1) / 3 exactly.
        let schedule = periodic(1.0 / 3.0, 1.0 / 3.0);

        assert_eq!(schedule_tick_time(&schedule, 2.0), 1.0);
        assert_eq!(schedule_tick_time(&schedule, 5.0), 2.0);
    }

    #[test]
    fn schedules_without_an_exact_rational_form_are_rejected() {
        assert_eq!(
            rumoca_core::ClockLattice::from_seconds(f64::MIN_POSITIVE, 0.0),
            Err(rumoca_core::ClockLatticeErrorKind::NotRationallyRepresentable)
        );
    }

    #[test]
    fn periodic_event_iterator_merges_coincident_schedules() {
        let schedules = [periodic(0.1, 0.0), periodic(0.2, 0.0)];

        let events = periodic_event_times(&schedules, 0.0, 0.4).collect::<Vec<_>>();

        assert_eq!(events.len(), 4);
        assert!(sample_time_match_with_tol(events[0], 0.1));
        assert!(sample_time_match_with_tol(events[1], 0.2));
        assert!(sample_time_match_with_tol(events[2], 0.3));
        assert!(sample_time_match_with_tol(events[3], 0.4));
    }
}
