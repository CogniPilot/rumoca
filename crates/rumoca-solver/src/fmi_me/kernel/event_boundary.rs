//! Component-owned processing of one Modelica event boundary.

use super::{
    MeError, SolveMeKernel, advance_states_to_event_probe, event_right_limit_state_derivatives,
    event_update_application_time,
};
use crate::{
    runtime::{
        schedule::RuntimeEventStop, solve_ops::EventPreMode, solve_runtime::EventUpdateRowFilter,
    },
    timeline::bounded_event_right_limit_time,
};

pub(super) struct EventBoundaryOutcome {
    pub(super) final_t: f64,
    pub(super) right_limit_t: Option<f64>,
}

pub(super) fn event_boundary_horizon(event: RuntimeEventStop, target: f64, horizon: f64) -> f64 {
    match (event.pre_mode, event.observe_right_limit) {
        (EventPreMode::EventEntry | EventPreMode::Fixed, _) => target,
        (EventPreMode::FollowCurrent, true) => horizon,
        (EventPreMode::FollowCurrent, false) => target,
    }
}

impl SolveMeKernel {
    pub(super) fn process_runtime_event_boundary(
        &mut self,
        event_time: f64,
        horizon: f64,
        tolerance: f64,
        event: RuntimeEventStop,
    ) -> Result<EventBoundaryOutcome, MeError> {
        self.apply_event_time(event_time, event)?;
        let right_time = bounded_event_right_limit_time(event_time, horizon, tolerance);
        let right_limit_t = if event.observe_right_limit
            && event.pre_mode == EventPreMode::FollowCurrent
            && right_time > event_time
        {
            self.apply_event_right_limit(right_time, event)?;
            Some(right_time)
        } else {
            None
        };
        let final_t = if event.observe_right_limit {
            right_time
        } else {
            event_time
        };
        Ok(EventBoundaryOutcome {
            final_t,
            right_limit_t,
        })
    }

    fn apply_event_time(
        &mut self,
        event_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), MeError> {
        if self.advance_state_to_event_right_limit {
            self.time = event_time;
        }
        if event.terminal
            && let Some(index) = self
                .runtime
                .model
                .problem
                .solve_layout
                .terminal_event_parameter_index
            && let Some(slot) = self.params.get_mut(index)
        {
            *slot = 1.0;
        }
        let (event_pre_y, event_pre_p) = self.event_pre_for_update(event_time, event)?;
        self.boundary_event_pre_y = Some(event_pre_y.clone());
        self.boundary_event_pre_p = Some(event_pre_p.clone());
        self.pending_event_pre_y = Some(event_pre_y);
        self.pending_event_pre_p = Some(event_pre_p);
        self.seed_scheduled_root_relation_overrides(event_time, event);
        let application_time = event_update_application_time(
            event_time,
            self.time,
            self.state_time_coincidence.is_some(),
        );
        let row_filter = if self.state_time_coincidence.is_consumed() {
            EventUpdateRowFilter::UnownedOnly
        } else {
            EventUpdateRowFilter::All
        };
        self.apply_discrete_event_updates(application_time, event, row_filter, None)
    }

    fn apply_event_right_limit(
        &mut self,
        right_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), MeError> {
        if self.advance_state_to_event_right_limit || self.state_time_coincidence.is_some() {
            let event_time = self.time;
            let settle = self.numerics_settle();
            let derivatives = event_right_limit_state_derivatives(
                &self.runtime,
                &self.solver_y_guess.borrow(),
                event_time,
                &self.states,
                &self.params,
                settle,
            )?;
            advance_states_to_event_probe(&mut self.states, &derivatives, event_time, right_time);
        }
        self.time = right_time;
        let iteration_y = self.current_solver_y()?;
        let event_pre_y = if let Some(event_pre_y) = self.boundary_event_pre_y.clone() {
            event_pre_y
        } else {
            self.current_solver_y()?
        };
        let event_pre_p = self
            .boundary_event_pre_p
            .clone()
            .unwrap_or_else(|| self.params.clone());
        self.pending_event_pre_y = Some(event_pre_y);
        self.pending_event_pre_p = Some(event_pre_p);
        let row_filter = if self.state_time_coincidence.is_some() {
            EventUpdateRowFilter::UnownedOnly
        } else {
            EventUpdateRowFilter::All
        };
        self.apply_discrete_event_updates(right_time, event, row_filter, Some(iteration_y))?;
        self.set_post_event_eval_time(Some(right_time));
        Ok(())
    }
}
