use crate::{
    EventActionOutcome, EventPreMode, RuntimeEventBoundary, RuntimeEventStop, RuntimeSolveError,
    commit_pre_params_after_event, initial_runtime_event_stop, runtime_event_right_limit,
};

use super::{
    EventUpdateRowFilter, ProjectedEventUpdateInput, SeededConditionMemory, SolveRuntime,
    support::copy_runtime_values,
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

/// The projection that follows a settled initial event.
pub struct ProjectedPostInitialEventInput<'a> {
    pub y: &'a mut [f64],
    pub p: &'a mut [f64],
    pub t: f64,
    pub tol: f64,
    pub max_iters: usize,
    pub row_filter: EventUpdateRowFilter,
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

fn initial_event_right_limit(
    event: RuntimeEventStop,
    event_t: f64,
    horizon_t: f64,
    tolerance: f64,
) -> Option<f64> {
    if !event.observe_right_limit || event.pre_mode != EventPreMode::FollowCurrent {
        return None;
    }
    let right_t = runtime_event_right_limit(RuntimeEventBoundary {
        event_t,
        horizon_t,
        tolerance,
        event,
    });
    (right_t > event_t).then_some(right_t)
}

/// `snapshot` with every seeded activation buffer replaced by its seed.
fn seeded_condition_memory_snapshot(
    snapshot: &[f64],
    seeded: &[SeededConditionMemory],
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut values = copy_runtime_values(snapshot, "seeded condition-memory event-entry snapshot")?;
    for entry in seeded {
        let slot = values.get_mut(entry.index).ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "seeded condition-memory parameter index {} is out of bounds",
                entry.index
            ))
        })?;
        *slot = entry.value;
    }
    Ok(values)
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
        super::set_initial_event_flag(&self.model, p, value);
    }

    pub fn apply_projected_post_initial_event_update<P>(
        &self,
        input: ProjectedPostInitialEventInput<'_>,
        project_algebraics: P,
    ) -> Result<EventActionOutcome, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        let ProjectedPostInitialEventInput {
            y,
            p,
            t,
            tol,
            max_iters,
            row_filter,
        } = input;
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
                row_filter,
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
        // Every backend reaches the first event instant through this boundary,
        // so the MLS §8.3.5.1 activation buffers are seeded here and only here:
        // a solver path that seeded them itself would decide by accident which
        // already-true `when` bodies run at `t_start`.
        let seeded = self.seed_condition_memory_for_initialization(y, p, t_start, tol)?;
        let seeded_event_pre_p;
        let event_pre_p: &[f64] = if seeded.is_empty() {
            event_pre_p
        } else {
            // The caller's event-entry snapshot may predate the settle that gave
            // the seed its values (the rk-like session captures it first). An
            // event action reads its activation buffer from that snapshot, so
            // carry the seed into it rather than letting the two disagree.
            seeded_event_pre_p = seeded_condition_memory_snapshot(event_pre_p, &seeded)?;
            &seeded_event_pre_p
        };
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
        let mut observations = Vec::new();
        if initial_event.is_some() {
            observations.push(InitialEventObservation::snapshot(t_start, y, p));
        }
        self.set_initial_event_flag(p, false);
        let Some(event) = initial_event else {
            commit_pre_params_after_event(&self.model, y, p, tol);
            return Ok(ProjectedInitialEventOutcome {
                final_t: t_start,
                observations,
                action,
            });
        };
        let right_t = initial_event_right_limit(event, t_start, t_end, tol);
        // The accepted initial-event value is the left endpoint of delay
        // history. A positive-delay query at the synthetic right-limit time
        // must read that accepted point, not remain in the initialization
        // identity `delay(u) = u` mode. Commit at the semantic event time
        // before evaluating the right limit; callers commit the resulting
        // right-limit point after this boundary returns.
        if right_t.is_some() {
            self.commit_delay_history(t_start, y, p)?;
        }
        // `pre(v)` remains frozen at its initialization value throughout the
        // initial event iteration. The post-event projection is the first
        // right-limit evaluation, so advance every lowered pre slot from the
        // converged current value before evaluating that limit. Keeping this
        // transition here makes the ordering identical for every backend.
        commit_pre_params_after_event(&self.model, y, p, tol);
        let post_t = right_t.unwrap_or(t_start);
        // The initial event settles at `t_start`. When `post_t` is still that
        // instant the projection is part of the same event, so rows that follow
        // current values are refreshed against the cleared `initial()` flag.
        // A synthetic right limit is a strictly later time and therefore not an
        // event instant: MLS Appendix B holds every discrete value there, and
        // only the continuous projection may move.
        let post_row_filter = if right_t.is_some() {
            EventUpdateRowFilter::Hold
        } else if event.pre_mode == EventPreMode::EventEntry {
            // A phase-zero periodic schedule ticks at the simulation start,
            // after initialization has settled. Its EventEntry rows must see
            // `initial() = false` at that same semantic instant. Fixed rows are
            // initialization actions and remain excluded from this projection.
            EventUpdateRowFilter::PostInitialClockTick
        } else {
            EventUpdateRowFilter::FollowCurrentOnly
        };
        let post_action = self.apply_projected_post_initial_event_update(
            ProjectedPostInitialEventInput {
                y,
                p,
                t: post_t,
                tol,
                max_iters,
                row_filter: post_row_filter,
            },
            |y, p| project_algebraics(y, p, post_t),
        )?;
        // The phase-zero clock pass above is the final superdense value of the
        // initial event. Its settled values become `previous()`/`pre()` for
        // the first later event, just like every other completed event.
        commit_pre_params_after_event(&self.model, y, p, tol);
        let post_observation = InitialEventObservation::snapshot(post_t, y, p);
        if !self.initial_event_observations_match_trace(&observations[0], &post_observation, tol)? {
            // This can be a synthetic right-limit point or the second
            // superdense value at `t_start` after `initial()` is cleared. The
            // recorder replaces equal-time points with the latter value.
            observations.push(post_observation);
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
        if let Some(event) = initial_event {
            let row_filter = if event.pre_mode == EventPreMode::EventEntry {
                // A periodic clock at t_start is the first superdense event
                // after initialization. Settle initialization rows now; the
                // EventEntry rows execute exactly once after initial() clears.
                EventUpdateRowFilter::UnownedOnly
            } else {
                EventUpdateRowFilter::All
            };
            return self.apply_projected_event_update(
                ProjectedEventUpdateInput {
                    y,
                    p,
                    t,
                    tol,
                    event_pre_y,
                    event_pre_p,
                    max_iters,
                    row_filter,
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
