use rumoca_solver::{
    EventActionOutcome, EventPreMode, RuntimeEventBoundary, RuntimeEventStop, RuntimeSolveError,
    initial_runtime_event_stop, runtime_event_right_limit, timeline::sample_time_match_with_tol,
};

use super::{
    EventUpdateRowFilter, ProjectedEventUpdateInput, SolveRuntime, support::copy_runtime_values,
};

pub struct ProjectedInitialEventInput<'a> {
    pub y: &'a mut [f64],
    pub p: &'a mut [f64],
    pub t_start: f64,
    pub t_end: f64,
    pub tol: f64,
    pub event_pre_y: &'a [f64],
    pub event_pre_p: &'a [f64],
    pub max_iters: usize,
    pub dynamic_event: Option<RuntimeEventStop>,
    pub apply_without_initial_event: bool,
}

pub struct ProjectedInitialEventOutcome {
    pub final_t: f64,
    pub observations: Vec<InitialEventObservation>,
    pub action: EventActionOutcome,
}

pub struct InitialEventObservation {
    pub t: f64,
    pub y: Vec<f64>,
    pub p: Vec<f64>,
}

impl InitialEventObservation {
    fn snapshot(t: f64, y: &[f64], p: &[f64]) -> Self {
        Self {
            t,
            y: y.to_vec(),
            p: p.to_vec(),
        }
    }
}

struct InitialEventUpdate<'a> {
    y: &'a mut [f64],
    p: &'a mut [f64],
    t: f64,
    tol: f64,
    event_pre_y: &'a [f64],
    event_pre_p: &'a [f64],
    max_iters: usize,
    initial_event: Option<RuntimeEventStop>,
}

fn initial_event_right_limit(event: RuntimeEventStop, event_t: f64, horizon_t: f64) -> Option<f64> {
    if !event.observe_right_limit || event.pre_mode != EventPreMode::FollowCurrent {
        return None;
    }
    let right_t = runtime_event_right_limit(RuntimeEventBoundary {
        event_t,
        horizon_t,
        event,
    });
    (right_t > event_t && !sample_time_match_with_tol(right_t, event_t)).then_some(right_t)
}

fn trace_values_match(left: &[f64], right: &[f64], tol: f64) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| trace_value_matches(*left, *right, tol))
}

fn trace_value_matches(left: f64, right: f64, tol: f64) -> bool {
    if left == right {
        return true;
    }
    if !left.is_finite() || !right.is_finite() {
        return false;
    }
    let scale = 1.0_f64.max(left.abs()).max(right.abs());
    (left - right).abs() <= tol.max(1.0e-12) * scale
}

impl SolveRuntime {
    pub fn set_initial_event_flag(&self, p: &mut [f64], value: bool) {
        let Some(index) = self
            .model
            .problem
            .solve_layout
            .initial_event_parameter_index
        else {
            return;
        };
        if let Some(slot) = p.get_mut(index) {
            *slot = f64::from(value);
        }
    }

    pub fn apply_projected_post_initial_event_update<P>(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
        project_algebraics: P,
    ) -> Result<EventActionOutcome, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        let event_pre_y = copy_runtime_values(y, "post-initial event pre y snapshot")?;
        let event_pre_p = copy_runtime_values(p, "post-initial event pre p snapshot")?;
        self.apply_projected_event_update(
            ProjectedEventUpdateInput {
                y,
                p,
                t,
                tol,
                event_pre_y: &event_pre_y,
                event_pre_p: &event_pre_p,
                max_iters,
                row_filter: EventUpdateRowFilter::FollowCurrentOnly,
                root_relation_overrides: &[],
            },
            project_algebraics,
        )
    }

    pub fn apply_projected_initial_event_boundary<P>(
        &self,
        input: ProjectedInitialEventInput<'_>,
        mut project_algebraics: P,
    ) -> Result<ProjectedInitialEventOutcome, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64], f64) -> Result<bool, RuntimeSolveError>,
    {
        let ProjectedInitialEventInput {
            y,
            p,
            t_start,
            t_end,
            tol,
            event_pre_y,
            event_pre_p,
            max_iters,
            dynamic_event,
            apply_without_initial_event,
        } = input;
        let initial_event = initial_runtime_event_stop(&self.model.problem, t_start, dynamic_event);
        let action = if initial_event.is_some() || apply_without_initial_event {
            self.apply_initial_event_update(
                InitialEventUpdate {
                    y,
                    p,
                    t: t_start,
                    tol,
                    event_pre_y,
                    event_pre_p,
                    max_iters,
                    initial_event,
                },
                &mut project_algebraics,
            )?
        } else {
            EventActionOutcome::Continue
        };
        if action != EventActionOutcome::Continue {
            return Ok(ProjectedInitialEventOutcome {
                final_t: t_start,
                observations: Vec::new(),
                action,
            });
        }
        if initial_event.is_none() {
            self.set_initial_event_flag(p, false);
            return Ok(ProjectedInitialEventOutcome {
                final_t: t_start,
                observations: Vec::new(),
                action,
            });
        }
        let mut observations = vec![InitialEventObservation::snapshot(t_start, y, p)];
        self.set_initial_event_flag(p, false);
        let Some(event) = initial_event else {
            return Ok(ProjectedInitialEventOutcome {
                final_t: t_start,
                observations,
                action,
            });
        };
        let right_t = initial_event_right_limit(event, t_start, t_end);
        let post_t = right_t.unwrap_or(t_start);
        let post_action = self.apply_projected_post_initial_event_update(
            y,
            p,
            post_t,
            tol,
            max_iters,
            |y, p| project_algebraics(y, p, post_t),
        )?;
        if let Some(right_t) = right_t {
            let post_observation = InitialEventObservation::snapshot(right_t, y, p);
            if !self.initial_event_observations_match_trace(
                &observations[0],
                &post_observation,
                tol,
            )? {
                observations.push(post_observation);
            }
        }
        Ok(ProjectedInitialEventOutcome {
            final_t: post_t,
            observations,
            action: post_action,
        })
    }

    fn initial_event_observations_match_trace(
        &self,
        left: &InitialEventObservation,
        right: &InitialEventObservation,
        tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let left_values = self.visible_values(&left.y, &left.p, left.t)?;
        let right_values = self.visible_values(&right.y, &right.p, right.t)?;
        Ok(trace_values_match(&left_values, &right_values, tol))
    }

    fn apply_initial_event_update<P>(
        &self,
        input: InitialEventUpdate<'_>,
        project_algebraics: &mut P,
    ) -> Result<EventActionOutcome, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64], f64) -> Result<bool, RuntimeSolveError>,
    {
        let InitialEventUpdate {
            y,
            p,
            t,
            tol,
            event_pre_y,
            event_pre_p,
            max_iters,
            initial_event,
        } = input;
        if initial_event.is_some() {
            return self.apply_projected_event_update(
                ProjectedEventUpdateInput {
                    y,
                    p,
                    t,
                    tol,
                    event_pre_y,
                    event_pre_p,
                    max_iters,
                    row_filter: EventUpdateRowFilter::All,
                    root_relation_overrides: &[],
                },
                |y, p| project_algebraics(y, p, t),
            );
        }
        let event_pre_y = copy_runtime_values(y, "initial event pre y snapshot")?;
        let event_pre_p = copy_runtime_values(p, "initial event pre p snapshot")?;
        self.apply_projected_event_update(
            ProjectedEventUpdateInput {
                y,
                p,
                t,
                tol,
                event_pre_y: &event_pre_y,
                event_pre_p: &event_pre_p,
                max_iters,
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &[],
            },
            |y, p| project_algebraics(y, p, t),
        )
    }
}
