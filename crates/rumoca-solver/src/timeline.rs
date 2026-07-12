use rumoca_ir_solve as solve;

pub fn build_output_times(t_start: f64, t_end: f64, dt: f64) -> Vec<f64> {
    if !dt.is_finite() || dt <= 0.0 {
        if sample_time_match_with_tol(t_start, t_end) {
            return vec![t_start];
        }
        return vec![t_start, t_end];
    }

    let mut times = Vec::new();
    let mut k = 0usize;
    loop {
        let t_raw = t_start + (k as f64) * dt;
        if t_raw > t_end && !sample_time_match_with_tol(t_raw, t_end) {
            break;
        }
        // Snap to t_end when within tolerance so the endpoint is always exact.
        let t = if sample_time_match_with_tol(t_raw, t_end) {
            t_end
        } else {
            t_raw
        };
        times.push(t);
        if t >= t_end {
            return times;
        }
        k += 1;
    }
    if let Some(&last) = times.last()
        && !sample_time_match_with_tol(last, t_end)
    {
        times.push(t_end);
    }
    times
}

pub fn sample_time_match_with_tol(a: f64, b: f64) -> bool {
    let tol = 1e-12 * (1.0 + a.abs().max(b.abs()));
    (a - b).abs() <= tol
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
) -> Vec<f64> {
    let mut events = Vec::new();
    for schedule in schedules {
        collect_periodic_event_times(schedule, current_t, target_t, &mut events);
    }
    events
}

fn collect_periodic_event_times(
    schedule: &solve::PeriodicEventSchedule,
    current_t: f64,
    target_t: f64,
    events: &mut Vec<f64>,
) {
    let period = schedule.period_seconds;
    let phase = schedule.phase_seconds;
    if !period.is_finite() || !phase.is_finite() || period <= 0.0 {
        return;
    }
    let mut tick = ((current_t - phase) / period).floor().max(0.0);
    loop {
        let event_t = phase + tick * period;
        if event_t > target_t && !sample_time_match_with_tol(event_t, target_t) {
            break;
        }
        if event_time_in_window(event_t, current_t, target_t) {
            events.push(event_t);
        }
        tick += 1.0;
        if !tick.is_finite() || events.len() > 200_000 {
            break;
        }
    }
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
    let period = schedule.period_seconds;
    let phase = schedule.phase_seconds;
    if !period.is_finite() || !phase.is_finite() || period <= 0.0 || t + period < phase {
        return false;
    }
    let ticks = (t - phase) / period;
    ticks >= 0.0 && sample_time_match_with_tol(ticks, ticks.round())
}

pub fn scheduled_root_matches_time(root: &solve::ScheduledRootCondition, t: f64) -> bool {
    periodic_schedule_matches_time(
        &solve::PeriodicEventSchedule {
            period_seconds: root.period_seconds,
            phase_seconds: root.phase_seconds,
        },
        t,
    )
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

pub fn event_right_limit_time(t_event: f64) -> f64 {
    if !t_event.is_finite() {
        return t_event;
    }
    next_representable_time(t_event)
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
    fn event_right_limit_time_moves_far_enough_for_event_guard_reevaluation() {
        let t_event = 0.001_f64;
        let t_right = event_right_limit_time(t_event);
        assert!(t_right > t_event);
        assert_eq!(t_right, f64::from_bits(t_event.to_bits() + 1));
    }

    #[test]
    fn event_left_limit_time_uses_previous_representable_time() {
        let t_event = 0.5_f64;
        let t_left = event_left_limit_time(t_event);
        assert!(t_left < t_event);
        assert!(event_right_limit_time(t_left).total_cmp(&t_event).is_ge());
    }

    #[test]
    fn build_output_times_handles_zero_span_and_invalid_dt() {
        assert_eq!(build_output_times(1.0, 1.0, 0.0), vec![1.0]);
        assert_eq!(build_output_times(1.0, 2.0, 0.0), vec![1.0, 2.0]);
    }
}
