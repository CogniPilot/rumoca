use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;

use super::solve_ops::write_clock_activation_params;

use crate::{
    EventActionOutcome, RuntimeEventStop, RuntimeSolveError, SolveStopSchedule,
    timeline::{event_time_in_window, runtime_parameter_index, sample_time_match_with_tol},
};
use rumoca_eval_solve::{PreparedScalarProgramBlock, RowEvalContext};

pub fn next_runtime_event_stop(
    model: &solve::SolveModel,
    runtime_state: &solve_eval::SimulationRuntimeState,
    y: &[f64],
    params: &[f64],
    stop_schedule: &mut SolveStopSchedule,
    current_t: f64,
    target: f64,
) -> Result<(f64, Option<RuntimeEventStop>), RuntimeSolveError> {
    let (static_stop, static_event) = stop_schedule.next_stop(current_t, target);
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
        // A periodic clock uses an exact rational lattice while a dynamic
        // deadline may reach the same mathematical instant through floating-
        // point arithmetic.  Enter the shared superdense event only after
        // both owners' deadlines have actually been reached, except when the
        // host target bounds this step. Returning the earlier value can leave
        // the later relation false and replay the clock when that dynamic
        // deadline is processed immediately after it.
        return Ok((static_stop.max(dynamic_stop).min(target), Some(event)));
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
    _tol: f64,
) -> Result<(), RuntimeSolveError> {
    solve_eval::apply_scalar_slot_values_exact(targets, values, y, p)
        .map(|_| ())
        .map_err(Into::into)
}

pub(crate) fn event_eval_params_with_relation_overrides(
    root_relation_memory_targets: &[Option<solve::ScalarSlot>],
    root_relation_overrides: &[(usize, f64)],
    p: &[f64],
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut event_eval_p = runtime_event_copy_values(p, "event relation override parameters")?;
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
    Ok(event_eval_p)
}

pub fn eval_event_actions_with_context(
    model: &solve::SolveModel,
    y: &[f64],
    p: &[f64],
    event_pre_p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
) -> Result<EventActionOutcome, RuntimeSolveError> {
    let events = &model.problem.events;
    let mut action_p = event_action_params(events, p, event_pre_p)?;
    write_clock_activation_params(model, &mut action_p, t);
    match solve_eval::eval_event_action_request(events, y, &action_p, t, context)? {
        solve_eval::EventActionRequest::Continue => Ok(EventActionOutcome::Continue),
        solve_eval::EventActionRequest::AssertionFailed { message } => {
            Ok(EventActionOutcome::AssertionFailed { time: t, message })
        }
        solve_eval::EventActionRequest::Terminate { message } => {
            Ok(EventActionOutcome::Terminated { time: t, message })
        }
    }
}

fn event_action_params(
    events: &solve::SolveEventPartition,
    p: &[f64],
    event_pre_p: &[f64],
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut action_p = runtime_event_copy_values(p, "event action parameters")?;
    for &index in &events.condition_memory_parameter_indices {
        let previous = event_pre_p.get(index).copied().ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "event-entry condition-memory parameter index {index} is out of bounds"
            ))
        })?;
        let slot = action_p.get_mut(index).ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "current condition-memory parameter index {index} is out of bounds"
            ))
        })?;
        *slot = previous;
    }
    Ok(action_p)
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
        let evaluator = PreparedScalarProgramBlock::new(model.visible_value_rows.clone())?;
        return evaluator
            .eval_row_with_context(visible_idx, y, params, t, context)
            .map_err(Into::into);
    }
    if let Some(idx) = runtime_parameter_index(&model.problem.solve_layout, name) {
        return params.get(idx).copied().ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!("runtime slot `{name}` is out of range"))
        });
    }
    if let Some(idx) = model.problem.solve_layout.solver_maps.name_to_idx.get(name) {
        return y.get(*idx).copied().ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!("solver slot `{name}` is out of range"))
        });
    }
    Err(RuntimeSolveError::solve_ir(format!(
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
    let mut candidates = dynamic_time_event_row_values(model, runtime_state, y, params, current_t)?;
    candidates.extend(named_events);
    candidates.retain(|event_t| event_time_in_window(*event_t, current_t, target));
    Ok(canonical_dynamic_time_event(&candidates))
}

/// Return the reached instant for the earliest tolerance-coincident deadline
/// cluster.  Dynamic owners can compute the same mathematical time through
/// different floating-point expressions; choosing the cluster's earliest bit
/// pattern would leave its slightly later owners armed for an immediate replay.
fn canonical_dynamic_time_event(candidates: &[f64]) -> Option<f64> {
    let earliest = candidates.iter().copied().min_by(f64::total_cmp)?;
    candidates
        .iter()
        .copied()
        .filter(|candidate| sample_time_match_with_tol(*candidate, earliest))
        .max_by(f64::total_cmp)
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
    let mut values = runtime_event_zero_values(block.len(), "dynamic time-event row values")?;
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

fn runtime_event_zero_values(
    len: usize,
    context: &'static str,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut values = runtime_event_vec_with_capacity(len, context)?;
    values.resize(len, 0.0);
    Ok(values)
}

fn runtime_event_copy_values(
    values: &[f64],
    context: &'static str,
) -> Result<Vec<f64>, RuntimeSolveError> {
    let mut copy = runtime_event_vec_with_capacity(values.len(), context)?;
    copy.extend_from_slice(values);
    Ok(copy)
}

fn runtime_event_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
) -> Result<Vec<T>, RuntimeSolveError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(capacity)
        .map_err(|_| RuntimeSolveError::solve_ir(format!("{context} capacity overflows")))?;
    Ok(values)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn periodic(period: f64, phase: f64) -> solve::PeriodicEventSchedule {
        solve::PeriodicEventSchedule::new(
            rumoca_core::ClockLattice::from_seconds(period, phase).unwrap(),
        )
        .unwrap()
    }

    #[test]
    fn coincident_periodic_and_dynamic_deadline_uses_reached_instant() {
        let period = 0.001;
        let dynamic_deadline = 9.0 * period;
        let mut model = solve::SolveModel::default();
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.solve_layout.discrete_real_scalar_names = vec!["next".to_string()];
        model.problem.events.dynamic_time_event_names = vec!["next".to_string()];
        model
            .problem
            .clocks
            .periodic_event_schedules
            .push(periodic(period, 0.0));
        model.parameters = vec![dynamic_deadline];
        let mut schedule = SolveStopSchedule::new(&model.problem, 0.0, 0.02);

        let (event_time, event) = next_runtime_event_stop(
            &model,
            &solve_eval::SimulationRuntimeState::new(),
            &[],
            &model.parameters,
            &mut schedule,
            0.008,
            0.02,
        )
        .expect("coincident periodic and dynamic event should schedule");

        assert!(sample_time_match_with_tol(event_time, 0.009));
        assert_eq!(event_time.to_bits(), dynamic_deadline.to_bits());
        assert!(event_time >= dynamic_deadline);
        assert_eq!(
            event,
            Some(RuntimeEventStop {
                pre_mode: super::super::solve_ops::EventPreMode::EventEntry,
                observe_right_limit: true,
                terminal: false,
            })
        );
    }

    #[test]
    fn dynamic_deadline_cluster_is_order_independent_and_stops_at_its_latest_owner() {
        let earliest = 0.009_f64;
        let coincident = earliest.next_up();
        let unrelated = earliest + 1.0e-5;
        let permutations = [
            [earliest, coincident, unrelated],
            [earliest, unrelated, coincident],
            [coincident, earliest, unrelated],
            [coincident, unrelated, earliest],
            [unrelated, earliest, coincident],
            [unrelated, coincident, earliest],
        ];

        for candidates in permutations {
            let stop = canonical_dynamic_time_event(&candidates)
                .expect("non-empty dynamic deadline cluster should schedule");
            assert_eq!(stop.to_bits(), coincident.to_bits());
        }
    }

    #[test]
    fn dynamic_deadline_cluster_does_not_merge_unrelated_future_event() {
        let earliest = 0.009_f64;
        let unrelated = earliest + 1.0e-5;

        let first = canonical_dynamic_time_event(&[unrelated, earliest])
            .expect("earliest dynamic deadline should schedule");

        assert_eq!(first.to_bits(), earliest.to_bits());
        assert!(!sample_time_match_with_tol(first, unrelated));
    }

    #[test]
    fn dynamic_deadline_cluster_is_anchored_at_earliest_not_chained() {
        let earliest = 0.009_f64;
        let tolerance = rumoca_core::SCHEDULE_TIME_RELATIVE_TOLERANCE * (1.0 + earliest.abs());
        let middle = earliest + 0.75 * tolerance;
        let chained = earliest + 1.5 * tolerance;
        assert!(sample_time_match_with_tol(earliest, middle));
        assert!(sample_time_match_with_tol(middle, chained));
        assert!(!sample_time_match_with_tol(earliest, chained));

        let stop = canonical_dynamic_time_event(&[chained, middle, earliest])
            .expect("anchored dynamic deadline cluster should schedule");

        assert_eq!(stop.to_bits(), middle.to_bits());
    }

    #[test]
    fn coincident_dynamic_deadline_does_not_move_terminal_horizon() {
        let target = 1.0_f64;
        let mut model = solve::SolveModel::default();
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.solve_layout.discrete_real_scalar_names = vec!["next".to_string()];
        model.problem.events.dynamic_time_event_names = vec!["next".to_string()];
        model.problem.events.has_terminal_event = true;
        model.parameters = vec![target.next_up()];
        let mut schedule = SolveStopSchedule::new(&model.problem, 0.0, target);

        let (event_time, event) = next_runtime_event_stop(
            &model,
            &solve_eval::SimulationRuntimeState::new(),
            &[],
            &model.parameters,
            &mut schedule,
            0.9,
            target,
        )
        .expect("terminal horizon and coincident dynamic deadline should schedule");

        assert_eq!(event_time.to_bits(), target.to_bits());
        assert!(event.is_some_and(|event| event.terminal));
    }

    #[test]
    fn event_eval_params_with_relation_overrides_applies_p_targets() {
        let params = event_eval_params_with_relation_overrides(
            &[
                Some(solve::ScalarSlot::P {
                    index: 1,
                    byte_offset: 8,
                }),
                Some(solve::ScalarSlot::Y {
                    index: 0,
                    byte_offset: 0,
                }),
            ],
            &[(0, 9.0), (1, 7.0), (4, 3.0)],
            &[1.0, 2.0, 3.0],
        )
        .expect("relation override params should copy");

        assert_eq!(params, vec![1.0, 9.0, 3.0]);
    }

    #[test]
    fn runtime_event_vec_with_capacity_rejects_impossible_capacity() {
        let err = runtime_event_vec_with_capacity::<u8>(usize::MAX, "runtime event test vector")
            .expect_err("impossible runtime event capacity should fail");

        assert!(
            err.to_string()
                .contains("runtime event test vector capacity overflows")
        );
    }
}
