use std::collections::HashSet;

use rumoca_ir_dae as dae;

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

pub fn collect_periodic_clock_events(
    schedules: &[dae::ClockSchedule],
    t_start: f64,
    t_end: f64,
) -> Vec<f64> {
    if !(t_start.is_finite() && t_end.is_finite() && t_end > t_start) {
        return Vec::new();
    }

    let mut events = Vec::new();
    let mut seen = HashSet::new();
    for schedule in schedules {
        let period = schedule.period_seconds;
        let phase = schedule.phase_seconds;
        if !period.is_finite() || !phase.is_finite() || period <= 0.0 {
            continue;
        }

        let mut k = ((t_start - phase) / period).ceil();
        if !k.is_finite() {
            continue;
        }
        if k < 0.0 {
            k = 0.0;
        }

        let mut t = phase + k * period;
        let mut emitted = 0usize;
        while t < t_end && !sample_time_match_with_tol(t, t_end) {
            let key = format!("{t:.15e}");
            if seen.insert(key) {
                events.push(t);
            }
            emitted += 1;
            if emitted > 200_000 {
                break;
            }
            t += period;
        }
    }

    events.sort_by(f64::total_cmp);
    events.dedup_by(|a, b| sample_time_match_with_tol(*a, *b));
    events
}

pub fn collect_runtime_schedule_events(dae_model: &dae::Dae, t_start: f64, t_end: f64) -> Vec<f64> {
    let mut events: Vec<f64> = dae_model
        .scheduled_time_events
        .iter()
        .copied()
        .filter(|event_t| {
            event_t.is_finite()
                && (*event_t > t_start || sample_time_match_with_tol(*event_t, t_start))
                && *event_t < t_end
                && !sample_time_match_with_tol(*event_t, t_end)
        })
        .collect();
    events.extend(collect_periodic_clock_events(
        &dae_model.clock_schedules,
        t_start,
        t_end,
    ));
    events.sort_by(f64::total_cmp);
    events.dedup_by(|a, b| sample_time_match_with_tol(*a, *b));
    events
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

pub fn event_right_limit_time(t_event: f64) -> f64 {
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
    fn collect_periodic_clock_events_merges_duplicates() {
        let schedules = vec![
            dae::ClockSchedule {
                period_seconds: 0.5,
                phase_seconds: 0.0,
            },
            dae::ClockSchedule {
                period_seconds: 0.5,
                phase_seconds: 0.0,
            },
        ];
        let events = collect_periodic_clock_events(&schedules, 0.0, 1.0);
        assert_eq!(events, vec![0.0, 0.5]);
    }

    #[test]
    fn collect_runtime_schedule_events_merges_scheduled_and_periodic_events() {
        let dae_model = dae::Dae {
            scheduled_time_events: vec![0.2, 0.5, 1.0],
            clock_schedules: vec![dae::ClockSchedule {
                period_seconds: 0.5,
                phase_seconds: 0.0,
            }],
            ..Default::default()
        };
        let events = collect_runtime_schedule_events(&dae_model, 0.0, 1.0);
        assert_eq!(events, vec![0.0, 0.2, 0.5]);
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
    fn event_right_limit_time_uses_next_representable_float() {
        let t_event = 0.001_f64;
        let t_right = event_right_limit_time(t_event);
        assert!(t_right > t_event);
        assert!(t_right - t_event < 1.0e-12);
    }
}
