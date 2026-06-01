use rumoca_ir_solve as solve;
use rumoca_solver::{
    EventActionOutcome, RuntimeEventStop, RuntimeSolveError, SolveStopSchedule,
    timeline::{event_time_in_window, runtime_parameter_index, sample_time_match_with_tol},
};

use crate::{self as solve_eval, PreparedScalarProgramBlock, RowEvalContext};

pub fn next_runtime_event_stop(
    model: &solve::SolveModel,
    runtime_state: &solve_eval::SimulationRuntimeState,
    y: &[f64],
    params: &[f64],
    stop_schedule: &mut SolveStopSchedule,
    current_t: f64,
    target: f64,
) -> Result<(f64, Option<RuntimeEventStop>), RuntimeSolveError> {
    let (static_stop, static_mode) = stop_schedule.next_stop(current_t, target);
    let static_event = static_mode.map(RuntimeEventStop::static_event);
    let Some(dynamic_stop) =
        next_dynamic_time_event(model, runtime_state, y, params, current_t, target)?
    else {
        return Ok((static_stop, static_event));
    };
    if dynamic_stop < static_stop && !sample_time_match_with_tol(dynamic_stop, static_stop) {
        return Ok((dynamic_stop, Some(RuntimeEventStop::dynamic_time_event())));
    }
    if sample_time_match_with_tol(dynamic_stop, static_stop) {
        let event = static_event
            .unwrap_or_else(RuntimeEventStop::dynamic_time_event)
            .merge_dynamic_time_event();
        return Ok((static_stop, Some(event)));
    }
    Ok((static_stop, static_event))
}

pub fn current_dynamic_time_event_stop(
    model: &solve::SolveModel,
    runtime_state: &solve_eval::SimulationRuntimeState,
    y: &[f64],
    params: &[f64],
    current_t: f64,
) -> Result<Option<RuntimeEventStop>, RuntimeSolveError> {
    let named_events = model
        .problem
        .events
        .dynamic_time_event_names
        .iter()
        .filter_map(|name| dynamic_time_event_value(model, params, name));
    let row_events = dynamic_time_event_row_values(model, runtime_state, y, params, current_t)?;
    if named_events
        .chain(row_events)
        .any(|event_t| sample_time_match_with_tol(event_t, current_t))
    {
        return Ok(Some(RuntimeEventStop::dynamic_time_event()));
    }
    Ok(None)
}

pub fn visible_values_with_context(
    model: &solve::SolveModel,
    y: &[f64],
    params: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
) -> Result<Vec<f64>, RuntimeSolveError> {
    model
        .visible_names
        .iter()
        .enumerate()
        .map(|(idx, name)| visible_value_with_context(model, idx, name, y, params, t, context))
        .collect()
}

pub fn apply_discrete_slot_values(
    targets: &[solve::ScalarSlot],
    values: &[f64],
    y: &mut [f64],
    p: &mut [f64],
    tol: f64,
) -> Result<(), RuntimeSolveError> {
    solve_eval::apply_scalar_slot_values(targets, values, y, p, tol)
        .map(|_| ())
        .map_err(Into::into)
}

pub(crate) fn event_eval_params_with_relation_overrides(
    root_relation_memory_targets: &[Option<solve::ScalarSlot>],
    root_relation_overrides: &[(usize, f64)],
    p: &[f64],
) -> Vec<f64> {
    let mut event_eval_p = p.to_vec();
    for (root_idx, value) in root_relation_overrides {
        let Some(Some(solve::ScalarSlot::P { index, .. })) =
            root_relation_memory_targets.get(*root_idx).copied()
        else {
            continue;
        };
        if let Some(slot) = event_eval_p.get_mut(index) {
            *slot = *value;
        }
    }
    event_eval_p
}

pub fn eval_event_actions_with_context(
    events: &solve::SolveEventPartition,
    y: &[f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
) -> Result<EventActionOutcome, RuntimeSolveError> {
    match solve_eval::eval_event_action_request(events, y, p, t, context)? {
        solve_eval::EventActionRequest::Continue => Ok(EventActionOutcome::Continue),
        solve_eval::EventActionRequest::AssertionFailed { message } => {
            Ok(EventActionOutcome::AssertionFailed { time: t, message })
        }
        solve_eval::EventActionRequest::Terminate { message } => {
            Ok(EventActionOutcome::Terminated { time: t, message })
        }
    }
}

fn visible_value_with_context(
    model: &solve::SolveModel,
    visible_idx: usize,
    name: &str,
    y: &[f64],
    params: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
) -> Result<f64, RuntimeSolveError> {
    if model.visible_value_rows.len() == model.visible_names.len() {
        let evaluator = PreparedScalarProgramBlock::new(model.visible_value_rows.clone());
        return evaluator
            .eval_row_with_context(visible_idx, y, params, t, context)
            .map_err(Into::into);
    }
    if let Some(idx) = runtime_parameter_index(&model.problem.solve_layout, name) {
        return params.get(idx).copied().ok_or_else(|| {
            RuntimeSolveError::SolveIr(format!("runtime slot `{name}` is out of range"))
        });
    }
    if let Some(idx) = model.problem.solve_layout.solver_maps.name_to_idx.get(name) {
        return y.get(*idx).copied().ok_or_else(|| {
            RuntimeSolveError::SolveIr(format!("solver slot `{name}` is out of range"))
        });
    }
    Err(RuntimeSolveError::SolveIr(format!(
        "visible trace name `{name}` is not in solve layout"
    )))
}

fn next_dynamic_time_event(
    model: &solve::SolveModel,
    runtime_state: &solve_eval::SimulationRuntimeState,
    y: &[f64],
    params: &[f64],
    current_t: f64,
    target: f64,
) -> Result<Option<f64>, RuntimeSolveError> {
    let named_events = model
        .problem
        .events
        .dynamic_time_event_names
        .iter()
        .filter_map(|name| dynamic_time_event_value(model, params, name));
    let row_events = dynamic_time_event_row_values(model, runtime_state, y, params, current_t)?;
    Ok(named_events
        .chain(row_events)
        .filter(|event_t| event_time_in_window(*event_t, current_t, target))
        .min_by(f64::total_cmp))
}

fn dynamic_time_event_row_values(
    model: &solve::SolveModel,
    runtime_state: &solve_eval::SimulationRuntimeState,
    y: &[f64],
    params: &[f64],
    current_t: f64,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let block = &model.problem.events.dynamic_time_event_rhs;
    if block.is_empty() {
        return Ok(Vec::new());
    }
    let mut values = vec![0.0; block.len()];
    solve_eval::eval_scalar_program_block_with_context(
        block,
        y,
        params,
        current_t,
        RowEvalContext {
            external_tables: Some(model.external_tables.as_slice()),
            runtime_state: Some(runtime_state),
            ..Default::default()
        },
        &mut values,
    )?;
    Ok(values)
}

fn dynamic_time_event_value(model: &solve::SolveModel, params: &[f64], name: &str) -> Option<f64> {
    runtime_parameter_index(&model.problem.solve_layout, name)
        .and_then(|idx| params.get(idx).copied())
}
