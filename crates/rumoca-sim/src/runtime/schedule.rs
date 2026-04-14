use crate::timeline::{self, ScheduledTimeEvents};
use rumoca_ir_dae as dae;

#[derive(Debug, Clone)]
pub struct RuntimeStopSchedule {
    scheduled_time_events: ScheduledTimeEvents,
    active_stop: f64,
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

    pub fn from_dae(dae_model: &dae::Dae, t_start: f64, t_current: f64, t_end: f64) -> Self {
        // MLS §16: periodic clock partitions must tick at their scheduled
        // instants, so the live solver loop needs those clock edges in the
        // same stop schedule as ordinary time discontinuities.
        let events = timeline::collect_runtime_schedule_events(dae_model, t_start, t_end);
        Self::new(events, t_start, t_current, t_end)
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
    fn runtime_stop_schedule_merges_periodic_clock_events_from_dae() {
        let dae_model = dae::Dae {
            scheduled_time_events: vec![0.2],
            clock_schedules: vec![dae::ClockSchedule {
                period_seconds: 0.5,
                phase_seconds: 0.0,
            }],
            ..Default::default()
        };
        let mut schedule = RuntimeStopSchedule::from_dae(&dae_model, 0.0, 0.0, 1.0);
        assert!((schedule.active_stop() - 0.2).abs() <= 1.0e-15);
        assert!((schedule.rearm(0.2, 1.0) - 0.5).abs() <= 1.0e-15);
        assert!((schedule.rearm(0.5, 1.0) - 1.0).abs() <= 1.0e-15);
    }
}
