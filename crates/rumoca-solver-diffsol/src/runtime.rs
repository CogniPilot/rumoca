use super::*;
use rumoca_solver::{
    EventActionOutcome, EventUpdateRowFilter, ProjectedEventUpdateInput, RuntimeSolveError,
};

pub(crate) fn settle_algebraics_and_relation_memory(
    runtime: &SolveRuntime,
    _model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    _state_count: usize,
    tol: f64,
) -> Result<(), SimError> {
    runtime
        .settle_projected_runtime_and_relation_memory(
            y,
            p,
            t,
            tol,
            EVENT_UPDATE_MAX_ITERS,
            move |y, p| refresh_algebraics_and_detect_changes(runtime, y, p, t, tol),
        )
        .map_err(Into::into)
}

pub(crate) fn refresh_algebraics_and_detect_changes(
    runtime: &SolveRuntime,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<bool, RuntimeSolveError> {
    let before = y.to_vec();
    runtime.refresh_delay_values(t, y, p)?;
    runtime.project_state_manifold(y, p, t, tol)?;
    runtime.refresh_algebraic_and_output_slots(t, y, p, tol, EVENT_UPDATE_MAX_ITERS)?;
    Ok(runtime_values_changed(&before, y, tol))
}

pub(crate) fn apply_event_updates(
    runtime: &SolveRuntime,
    _ode_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), SimError> {
    let event_pre_y = y.to_vec();
    let event_pre_p = p.to_vec();
    apply_event_updates_with_filter(
        EventUpdateInput {
            runtime,
            y,
            p,
            t,
            tol,
            event_pre_y: &event_pre_y,
            event_pre_p: &event_pre_p,
        },
        EventUpdateRowFilter::All,
    )
}

struct EventUpdateInput<'a> {
    runtime: &'a SolveRuntime,
    y: &'a mut [f64],
    p: &'a mut [f64],
    t: f64,
    tol: f64,
    event_pre_y: &'a [f64],
    event_pre_p: &'a [f64],
}

fn apply_event_updates_with_filter(
    input: EventUpdateInput<'_>,
    row_filter: EventUpdateRowFilter,
) -> Result<(), SimError> {
    let EventUpdateInput {
        runtime,
        y,
        p,
        t,
        tol,
        event_pre_y,
        event_pre_p,
    } = input;
    let outcome = runtime.apply_projected_event_update(
        ProjectedEventUpdateInput {
            y,
            p,
            t,
            tol,
            event_pre_y,
            event_pre_p,
            max_iters: EVENT_UPDATE_MAX_ITERS,
            row_filter,
            root_relation_overrides: &[],
        },
        project_algebraics_callback(runtime, t, tol),
    )?;
    event_action_outcome_to_result(outcome, t)
}

#[cfg(test)]
pub(crate) fn seed_initial_discrete_values(
    runtime: &SolveRuntime,
    _ode_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), SimError> {
    runtime.seed_initial_discrete_values(y, p, t, tol, EVENT_UPDATE_MAX_ITERS)?;
    Ok(())
}

fn project_algebraics_callback(
    runtime: &SolveRuntime,
    t: f64,
    tol: f64,
) -> impl FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError> + '_ {
    move |y, p| refresh_algebraics_and_detect_changes(runtime, y, p, t, tol)
}

fn event_action_outcome_to_result(
    outcome: EventActionOutcome,
    event_t: f64,
) -> Result<(), SimError> {
    match outcome {
        EventActionOutcome::Continue => Ok(()),
        EventActionOutcome::AssertionFailed { message, .. } => Err(SimError::AssertionFailed {
            time: event_t,
            message,
        }),
        EventActionOutcome::Terminated { message, .. } => Err(SimError::Terminated {
            time: event_t,
            message,
        }),
    }
}
