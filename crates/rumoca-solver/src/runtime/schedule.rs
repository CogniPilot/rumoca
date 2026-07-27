use rumoca_ir_solve as solve;

use super::solve_ops::EventPreMode;
use crate::timeline::{
    ScheduledTimeEvents, next_periodic_event_time, periodic_schedule_matches_time,
    sample_time_match_with_tol, schedule_tick_time, scheduled_time_in_horizon,
};

#[derive(Debug, Clone)]
pub struct RuntimeStopSchedule {
    scheduled_time_events: ScheduledTimeEvents,
    active_stop: f64,
}

#[derive(Debug, Clone)]
pub struct SolveStopSchedule {
    events: Vec<StopEvent>,
    periodic_schedules: Vec<solve::PeriodicEventSchedule>,
    next_idx: usize,
}

#[derive(Debug, Clone, Copy)]
struct StopEvent {
    time: f64,
    pre_mode: EventPreMode,
    terminal: bool,
}

impl SolveStopSchedule {
    pub fn new(problem: &solve::SolveProblem, t_start: f64, t_end: f64) -> Self {
        let mut schedule = Self {
            events: collect_static_solve_events(problem, t_start, t_end),
            periodic_schedules: problem.clocks.periodic_event_schedules.clone(),
            next_idx: 0,
        };
        schedule.advance_past(t_start);
        schedule
    }

    pub fn next_stop(&mut self, t_current: f64, t_target: f64) -> (f64, Option<RuntimeEventStop>) {
        self.advance_past(t_current);
        let mut next = None;
        if let Some(event) = self.events.get(self.next_idx)
            && !sample_time_match_with_tol(event.time, t_current)
            && (event.time < t_target || sample_time_match_with_tol(event.time, t_target))
        {
            merge_next_stop(&mut next, *event);
        }
        if let Some(time) = next_periodic_event_time(&self.periodic_schedules, t_current, t_target)
        {
            merge_next_stop(
                &mut next,
                StopEvent {
                    time,
                    pre_mode: EventPreMode::EventEntry,
                    terminal: false,
                },
            );
        }
        if let Some(event) = next {
            return (
                event.time,
                Some(RuntimeEventStop {
                    pre_mode: event.pre_mode,
                    observe_right_limit: event.pre_mode == EventPreMode::FollowCurrent,
                    terminal: event.terminal,
                }),
            );
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
    pub terminal: bool,
}

impl RuntimeEventStop {
    pub fn static_event(pre_mode: EventPreMode) -> Self {
        Self {
            pre_mode,
            observe_right_limit: pre_mode == EventPreMode::FollowCurrent,
            terminal: false,
        }
    }

    pub fn dynamic_time_event() -> Self {
        Self {
            pre_mode: EventPreMode::FollowCurrent,
            observe_right_limit: true,
            terminal: false,
        }
    }

    pub fn merge_dynamic_time_event(self) -> Self {
        Self {
            pre_mode: self.pre_mode.merge(EventPreMode::FollowCurrent),
            observe_right_limit: true,
            terminal: self.terminal,
        }
    }
}

pub fn merge_runtime_event_stops(
    static_event: Option<RuntimeEventStop>,
    dynamic_event: Option<RuntimeEventStop>,
) -> Option<RuntimeEventStop> {
    match (static_event, dynamic_event) {
        (Some(event), Some(_)) => Some(event.merge_dynamic_time_event()),
        (Some(event), None) | (None, Some(event)) => Some(event),
        (None, None) => None,
    }
}

pub fn initial_runtime_event_stop(
    problem: &solve::SolveProblem,
    t_start: f64,
    dynamic_event: Option<RuntimeEventStop>,
) -> Option<RuntimeEventStop> {
    let static_event =
        initial_static_event_pre_mode(problem, t_start).map(RuntimeEventStop::static_event);
    merge_runtime_event_stops(static_event, dynamic_event)
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
            merge_initial_event_mode(&mut mode, EventPreMode::EventEntry);
        }
    }
    mode
}

/// The scheduled time event, if any, whose instant is `t`.
///
/// MLS 3.7 §8.5 requires every event that occurs at the same time instant to be
/// handled in a single event iteration at that instant. The driver uses this to
/// recognise a zero-crossing that lands on a scheduled instant: without it the
/// root would be applied only at its right limit, moving the simulation clock
/// past the scheduled instant so the `when sample(...)` bodies belonging to it
/// never activate.
///
/// The comparison is in *time* units. A located root is only as accurate as the
/// root solve, so it reaches the scheduled instant to within a time tolerance,
/// not to within an exact tick index.
pub fn coincident_scheduled_event(
    problem: &solve::SolveProblem,
    t: f64,
) -> Option<CoincidentScheduledEvent> {
    let mut found: Option<CoincidentScheduledEvent> = None;
    for event_t in &problem.events.scheduled_time_events {
        if sample_time_match_with_tol(*event_t, t) {
            merge_coincident_event(&mut found, *event_t, EventPreMode::FollowCurrent);
        }
    }
    for schedule in &problem.clocks.periodic_event_schedules {
        if let Some(tick_time) = periodic_tick_time_at(schedule, t) {
            merge_coincident_event(&mut found, tick_time, EventPreMode::EventEntry);
        }
    }
    found
}

/// A scheduled time event that shares its instant with a located zero-crossing.
#[derive(Debug, Clone, Copy)]
pub struct CoincidentScheduledEvent {
    /// The scheduled instant itself. The root solve only reaches it to within a
    /// time tolerance, and the `when` bodies belonging to the instant test the
    /// schedule exactly, so the event must be applied at this time, not at the
    /// located root time.
    pub time: f64,
    pub event: RuntimeEventStop,
}

fn merge_coincident_event(
    found: &mut Option<CoincidentScheduledEvent>,
    time: f64,
    pre_mode: EventPreMode,
) {
    match found {
        Some(existing) => {
            existing.event.pre_mode = existing.event.pre_mode.merge(pre_mode);
            existing.event.observe_right_limit =
                existing.event.pre_mode == EventPreMode::FollowCurrent;
        }
        None => {
            *found = Some(CoincidentScheduledEvent {
                time,
                event: RuntimeEventStop::static_event(pre_mode),
            });
        }
    }
}

/// The instant of `schedule`'s tick at `t`, if `t` is one, compared in seconds.
fn periodic_tick_time_at(schedule: &solve::PeriodicEventSchedule, t: f64) -> Option<f64> {
    let period = schedule.period_seconds;
    let phase = schedule.phase_seconds;
    if !period.is_finite() || !phase.is_finite() || period <= 0.0 || !t.is_finite() {
        return None;
    }
    let tick = ((t - phase) / period).round();
    if !tick.is_finite() || tick < 0.0 {
        return None;
    }
    let tick_time = schedule_tick_time(schedule, tick);
    sample_time_match_with_tol(tick_time, t).then_some(tick_time)
}

fn collect_static_solve_events(
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
            terminal: false,
        })
        .collect();
    if problem.events.has_terminal_event && scheduled_time_in_horizon(t_end, t_start, t_end) {
        events.push(StopEvent {
            time: t_end,
            pre_mode: EventPreMode::EventEntry,
            terminal: true,
        });
    }
    merge_stop_events(events)
}

fn merge_initial_event_mode(mode: &mut Option<EventPreMode>, event_mode: EventPreMode) {
    *mode = Some(mode.map_or(event_mode, |existing| existing.merge(event_mode)));
}

fn merge_next_stop(next: &mut Option<StopEvent>, candidate: StopEvent) {
    match next {
        Some(event) if sample_time_match_with_tol(event.time, candidate.time) => {
            event.pre_mode = event.pre_mode.merge(candidate.pre_mode);
            event.terminal |= candidate.terminal;
        }
        Some(event) if event.time < candidate.time => {}
        _ => *next = Some(candidate),
    }
}

fn merge_stop_events(mut events: Vec<StopEvent>) -> Vec<StopEvent> {
    events.sort_by(|a, b| a.time.total_cmp(&b.time));
    let mut merged: Vec<StopEvent> = Vec::with_capacity(events.len());
    for event in events {
        if let Some(last) = merged.last_mut()
            && sample_time_match_with_tol(last.time, event.time)
        {
            last.pre_mode = last.pre_mode.merge(event.pre_mode);
            last.terminal |= event.terminal;
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

    #[test]
    fn periodic_clock_stops_use_event_entry_pre_mode() {
        let mut problem = solve::SolveProblem::default();
        problem
            .clocks
            .periodic_event_schedules
            .push(solve::PeriodicEventSchedule {
                period_seconds: 0.1,
                phase_seconds: 0.0,
            });
        let mut schedule = SolveStopSchedule::new(&problem, 0.0, 0.3);

        let (stop, mode) = schedule.next_stop(0.0, 0.2);

        assert!((stop - 0.1).abs() <= 1.0e-15);
        assert_eq!(
            mode,
            Some(RuntimeEventStop::static_event(EventPreMode::EventEntry))
        );
    }

    #[test]
    fn solve_stop_schedule_includes_clock_event_at_horizon() {
        let mut problem = solve::SolveProblem::default();
        problem
            .clocks
            .periodic_event_schedules
            .push(solve::PeriodicEventSchedule {
                period_seconds: 0.05,
                phase_seconds: 0.0,
            });
        let mut schedule = SolveStopSchedule::new(&problem, 0.0, 0.1);

        let (first_stop, first_mode) = schedule.next_stop(0.0, 0.05);
        let (horizon_stop, horizon_mode) = schedule.next_stop(0.05, 0.1);

        assert!((first_stop - 0.05).abs() <= 1.0e-15);
        assert_eq!(
            first_mode,
            Some(RuntimeEventStop::static_event(EventPreMode::EventEntry))
        );
        assert!((horizon_stop - 0.1).abs() <= 1.0e-15);
        assert_eq!(
            horizon_mode,
            Some(RuntimeEventStop::static_event(EventPreMode::EventEntry))
        );
    }

    #[test]
    fn solve_stop_schedule_keeps_ticks_after_former_materialization_cap() {
        let mut problem = solve::SolveProblem::default();
        problem
            .clocks
            .periodic_event_schedules
            .push(solve::PeriodicEventSchedule {
                period_seconds: 1.0e-4,
                phase_seconds: 0.0,
            });
        let mut schedule = SolveStopSchedule::new(&problem, 0.0, 100.0);

        let (stop, mode) = schedule.next_stop(20.000_05, 100.0);

        assert!(sample_time_match_with_tol(stop, 20.000_1));
        assert_eq!(
            mode,
            Some(RuntimeEventStop::static_event(EventPreMode::EventEntry))
        );
    }

    #[test]
    fn coincident_static_and_periodic_stops_merge_pre_modes() {
        let mut problem = solve::SolveProblem::default();
        problem.events.scheduled_time_events.push(0.1);
        problem
            .clocks
            .periodic_event_schedules
            .push(solve::PeriodicEventSchedule {
                period_seconds: 0.1,
                phase_seconds: 0.0,
            });
        let mut schedule = SolveStopSchedule::new(&problem, 0.0, 1.0);

        let (stop, mode) = schedule.next_stop(0.0, 1.0);

        assert!(sample_time_match_with_tol(stop, 0.1));
        assert_eq!(
            mode,
            Some(RuntimeEventStop::static_event(EventPreMode::EventEntry))
        );
    }

    /// A located root only reaches the scheduled instant to within a time
    /// tolerance. `Modelica.Blocks.Math.Mean` inside a 50 Hz rectifier samples
    /// at exactly the diode commutation instants, and the root solve lands a
    /// fraction of a picosecond short of the tick; the instant must still be
    /// recognised as the scheduled one (MLS 3.7 §8.5).
    #[test]
    fn root_just_short_of_a_periodic_tick_is_the_scheduled_instant() {
        let mut problem = solve::SolveProblem::default();
        problem
            .clocks
            .periodic_event_schedules
            .push(solve::PeriodicEventSchedule {
                period_seconds: 0.01,
                phase_seconds: 0.01,
            });

        let found = coincident_scheduled_event(&problem, 0.009_999_999_999_642_817)
            .expect("root at the sample instant is a scheduled event");

        assert!(sample_time_match_with_tol(found.time, 0.01));
        assert_eq!(found.event.pre_mode, EventPreMode::EventEntry);
    }

    #[test]
    fn time_between_periodic_ticks_is_not_a_scheduled_instant() {
        let mut problem = solve::SolveProblem::default();
        problem
            .clocks
            .periodic_event_schedules
            .push(solve::PeriodicEventSchedule {
                period_seconds: 0.01,
                phase_seconds: 0.01,
            });

        assert!(coincident_scheduled_event(&problem, 0.010_002).is_none());
        assert!(coincident_scheduled_event(&problem, 0.015).is_none());
    }

    /// Ticks before the schedule's phase are not events, so a root there must
    /// not be mistaken for one.
    #[test]
    fn root_before_the_first_periodic_tick_is_not_a_scheduled_instant() {
        let mut problem = solve::SolveProblem::default();
        problem
            .clocks
            .periodic_event_schedules
            .push(solve::PeriodicEventSchedule {
                period_seconds: 0.01,
                phase_seconds: 0.01,
            });

        assert!(coincident_scheduled_event(&problem, 0.0).is_none());
    }

    #[test]
    fn root_at_a_static_scheduled_time_event_is_the_scheduled_instant() {
        let mut problem = solve::SolveProblem::default();
        problem.events.scheduled_time_events.push(0.17);

        let found = coincident_scheduled_event(&problem, 0.17)
            .expect("root at a static time event is a scheduled event");

        assert!(sample_time_match_with_tol(found.time, 0.17));
        assert_eq!(found.event.pre_mode, EventPreMode::FollowCurrent);
    }

    #[test]
    fn terminal_operator_schedules_terminal_event_at_horizon() {
        let mut problem = solve::SolveProblem::default();
        problem.events.has_terminal_event = true;
        let mut schedule = SolveStopSchedule::new(&problem, 0.0, 1.0);

        let (stop, event) = schedule.next_stop(0.0, 1.0);

        assert!(sample_time_match_with_tol(stop, 1.0));
        assert_eq!(
            event,
            Some(RuntimeEventStop {
                pre_mode: EventPreMode::EventEntry,
                observe_right_limit: false,
                terminal: true,
            })
        );
    }
}
