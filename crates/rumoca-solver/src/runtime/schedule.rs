use rumoca_ir_solve as solve;

use super::solve_ops::EventPreMode;
use crate::timeline::{
    ScheduledTimeEvents, periodic_event_times, periodic_schedule_matches_time,
    sample_time_match_with_tol, scheduled_time_in_horizon,
};

#[derive(Debug, Clone)]
pub struct RuntimeStopSchedule {
    scheduled_time_events: ScheduledTimeEvents,
    active_stop: f64,
}

#[derive(Debug, Clone)]
pub struct SolveStopSchedule {
    events: Vec<StopEvent>,
    next_idx: usize,
}

#[derive(Debug, Clone, Copy)]
struct StopEvent {
    time: f64,
    pre_mode: EventPreMode,
}

impl SolveStopSchedule {
    pub fn new(problem: &solve::SolveProblem, t_start: f64, t_end: f64) -> Self {
        let mut schedule = Self {
            events: collect_solve_scheduled_events(problem, t_start, t_end),
            next_idx: 0,
        };
        schedule.advance_past(t_start);
        schedule
    }

    pub fn next_stop(&mut self, t_current: f64, t_target: f64) -> (f64, Option<EventPreMode>) {
        self.advance_past(t_current);
        if let Some(event) = self.events.get(self.next_idx)
            && !sample_time_match_with_tol(event.time, t_current)
            && (event.time < t_target || sample_time_match_with_tol(event.time, t_target))
        {
            return (event.time, Some(event.pre_mode));
        }
        (t_target, None)
    }

    pub fn advance_past(&mut self, t_current: f64) {
        while let Some(event) = self.events.get(self.next_idx) {
            if event.time < t_current || sample_time_match_with_tol(event.time, t_current) {
                self.next_idx += 1;
            } else {
                break;
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RuntimeEventStop {
    pub pre_mode: EventPreMode,
    pub observe_right_limit: bool,
}

impl RuntimeEventStop {
    pub fn static_event(pre_mode: EventPreMode) -> Self {
        Self {
            pre_mode,
            observe_right_limit: false,
        }
    }

    pub fn dynamic_time_event() -> Self {
        Self {
            pre_mode: EventPreMode::FollowCurrent,
            observe_right_limit: true,
        }
    }

    pub fn merge_dynamic_time_event(self) -> Self {
        Self {
            pre_mode: self.pre_mode.merge(EventPreMode::FollowCurrent),
            observe_right_limit: true,
        }
    }
}

pub fn initial_static_event_pre_mode(
    problem: &solve::SolveProblem,
    t_start: f64,
) -> Option<EventPreMode> {
    let mut mode = None;
    for event_t in &problem.events.scheduled_time_events {
        if sample_time_match_with_tol(*event_t, t_start) {
            merge_initial_event_mode(&mut mode, EventPreMode::FollowCurrent);
        }
    }
    for schedule in &problem.clocks.periodic_event_schedules {
        if periodic_schedule_matches_time(schedule, t_start) {
            merge_initial_event_mode(&mut mode, EventPreMode::Fixed);
        }
    }
    mode
}

fn collect_solve_scheduled_events(
    problem: &solve::SolveProblem,
    t_start: f64,
    t_end: f64,
) -> Vec<StopEvent> {
    let mut events: Vec<StopEvent> = problem
        .events
        .scheduled_time_events
        .iter()
        .copied()
        .filter(|event_t| scheduled_time_in_horizon(*event_t, t_start, t_end))
        .map(|time| StopEvent {
            time,
            pre_mode: EventPreMode::FollowCurrent,
        })
        .collect();
    events.extend(collect_periodic_solve_events(
        &problem.clocks.periodic_event_schedules,
        t_start,
        t_end,
    ));
    merge_stop_events(events)
}

fn collect_periodic_solve_events(
    schedules: &[solve::PeriodicEventSchedule],
    t_start: f64,
    t_end: f64,
) -> Vec<StopEvent> {
    let events = periodic_event_times(schedules, t_start, t_end)
        .into_iter()
        .filter(|t| scheduled_time_in_horizon(*t, t_start, t_end))
        .map(|time| StopEvent {
            time,
            pre_mode: EventPreMode::Fixed,
        })
        .collect();
    merge_stop_events(events)
}

fn merge_initial_event_mode(mode: &mut Option<EventPreMode>, event_mode: EventPreMode) {
    *mode = Some(mode.map_or(event_mode, |existing| existing.merge(event_mode)));
}

fn merge_stop_events(mut events: Vec<StopEvent>) -> Vec<StopEvent> {
    events.sort_by(|a, b| a.time.total_cmp(&b.time));
    let mut merged: Vec<StopEvent> = Vec::with_capacity(events.len());
    for event in events {
        if let Some(last) = merged.last_mut()
            && sample_time_match_with_tol(last.time, event.time)
        {
            last.pre_mode = last.pre_mode.merge(event.pre_mode);
            continue;
        }
        merged.push(event);
    }
    merged
}

impl RuntimeStopSchedule {
    pub fn new(events: Vec<f64>, t_start: f64, t_current: f64, t_end: f64) -> Self {
        let mut scheduled_time_events = ScheduledTimeEvents::new(events, t_start);
        let active_stop = scheduled_time_events.next_stop_time(t_current, t_end);
        Self {
            scheduled_time_events,
            active_stop,
        }
    }

    pub fn active_stop(&self) -> f64 {
        self.active_stop
    }

    pub fn rearm(&mut self, t_current: f64, t_end: f64) -> f64 {
        self.active_stop = self.scheduled_time_events.next_stop_time(t_current, t_end);
        self.active_stop
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn runtime_stop_schedule_advances_across_discontinuities() {
        let mut schedule = RuntimeStopSchedule::new(vec![0.2, 0.5], 0.0, 0.0, 1.0);
        assert!((schedule.active_stop() - 0.2).abs() <= 1.0e-15);
        assert!((schedule.rearm(0.2, 1.0) - 0.5).abs() <= 1.0e-15);
        assert!((schedule.rearm(0.5, 1.0) - 1.0).abs() <= 1.0e-15);
    }

    #[test]
    fn runtime_stop_schedule_defaults_to_horizon_without_events() {
        let schedule = RuntimeStopSchedule::new(Vec::new(), 0.0, 0.0, 1.0);
        assert!((schedule.active_stop() - 1.0).abs() <= 1.0e-15);
    }
}
