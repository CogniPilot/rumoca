use rumoca_ir_solve as solve;

use crate::RuntimeSolveError;

#[derive(Clone, Copy, Debug)]
pub(crate) struct EventIterationLane {
    pub(crate) dest_p_index: usize,
    pub(crate) current: f64,
    pub(crate) pre: f64,
    pub(crate) value_kind: solve::EventIterationValueKind,
    pub(crate) fixed_point: bool,
}

/// Atomically advance one already-cataloged set of typed event-history lanes.
///
/// Validation completes before the first write. Fixed-point lanes advance to
/// their current value; clock-owned first-pass and excluded continuous lanes
/// remain unchanged.
pub(crate) fn advance_event_iteration_lanes(lanes: &mut [EventIterationLane]) -> Result<bool, ()> {
    if lanes.iter().any(|lane| {
        !event_iteration_value_is_valid(lane.value_kind, lane.current)
            || !event_iteration_value_is_valid(lane.value_kind, lane.pre)
    }) {
        return Err(());
    }
    let mut changed = false;
    for lane in lanes.iter_mut().filter(|lane| lane.fixed_point) {
        changed |= lane.current != lane.pre;
        lane.pre = lane.current;
    }
    Ok(changed)
}

pub fn write_pre_params_from_sources(
    model: &solve::SolveModel,
    source_y: &[f64],
    source_p: &[f64],
    params: &mut [f64],
    tol: f64,
) -> bool {
    let mut changed = false;
    for binding in &model.problem.solve_layout.pre_param_bindings {
        let value = match binding.source {
            solve::PreParamSource::Y { index } => source_y.get(index).copied(),
            solve::PreParamSource::P { index } => source_p.get(index).copied(),
        };
        if let (Some(slot), Some(value)) = (params.get_mut(binding.dest_p_index), value) {
            changed |= update_slot(slot, value, tol);
        }
    }
    changed
}

/// Advance ordinary `pre` history between two event-iteration passes.
///
/// Clock-owned bindings represent `previous` and advance only on their typed
/// clock tick when the completed event is committed.  Ordinary bindings,
/// however, are the fixed input of the next Appendix-B pass and must be copied
/// atomically from the prior pass before any equation in the next pass runs.
pub fn advance_event_iteration_pre_params(
    model: &solve::SolveModel,
    source_y: &[f64],
    source_p: &[f64],
    params: &mut [f64],
) -> Result<bool, RuntimeSolveError> {
    let plan = &model.problem.discrete.event_iteration_plan;
    let mut lanes = Vec::new();
    for (run_index, run) in plan.runs.iter().enumerate() {
        let storage = &model.problem.solve_layout.variable_storage_runs[run.variable];
        let value_kind = storage
            .event_iteration_kind()
            .expect("validated event plan references a typed discrete variable");
        let fixed_point = event_iteration_run_clock(model, run)?.is_none();
        for binding_offset in 0..storage.scalar_count {
            let binding_index = run.pre_binding_start + binding_offset;
            let binding = model
                .problem
                .solve_layout
                .pre_param_bindings
                .get(binding_index)
                .ok_or_else(|| {
                    RuntimeSolveError::solve_ir(format!(
                        "event-iteration run {run_index} binding {binding_index} is out of bounds"
                    ))
                })?;
            let value = pre_binding_source_value(binding, source_y, source_p).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "event-iteration run {run_index} current source is out of bounds"
                ))
            })?;
            validate_event_iteration_value(value_kind, value, run_index, "current")?;
            let pre = params.get(binding.dest_p_index).copied().ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "event-iteration run {run_index} pre parameter {} is outside {} parameters",
                    binding.dest_p_index,
                    params.len()
                ))
            })?;
            validate_event_iteration_value(value_kind, pre, run_index, "pre")?;
            lanes.push(EventIterationLane {
                dest_p_index: binding.dest_p_index,
                current: value,
                pre,
                value_kind,
                fixed_point,
            });
        }
    }
    let changed = advance_event_iteration_lanes(&mut lanes).map_err(|()| {
        RuntimeSolveError::solve_ir("event-iteration plan contains an invalid typed value")
    })?;
    for lane in lanes.into_iter().filter(|lane| lane.fixed_point) {
        params[lane.dest_p_index] = lane.pre;
    }
    Ok(changed)
}

/// Seed ordinary `pre` lanes from the exact event-entry snapshots.
///
/// Continuous `pre(x)` and discrete `pre(z)` both read the left limit on the
/// first whole event pass. Clocked `previous()` has a different owner and is
/// deliberately excluded.
pub fn seed_event_entry_pre_params(
    model: &solve::SolveModel,
    event_pre_y: &[f64],
    event_pre_p: &[f64],
    params: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    let mut writes = Vec::new();
    for (binding_index, binding) in model
        .problem
        .solve_layout
        .pre_param_bindings
        .iter()
        .enumerate()
        .filter(|(_, binding)| binding.clock_schedule.is_none())
    {
        let value =
            pre_binding_source_value(binding, event_pre_y, event_pre_p).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "event-entry pre binding {binding_index} source is out of bounds"
                ))
            })?;
        if binding.dest_p_index >= params.len() {
            return Err(RuntimeSolveError::solve_ir(format!(
                "event-entry pre binding {binding_index} destination {} is out of bounds",
                binding.dest_p_index
            )));
        }
        writes.push((binding.dest_p_index, value));
    }
    for (destination, value) in writes {
        params[destination] = value;
    }
    Ok(())
}

pub fn event_iteration_plan_settled(
    model: &solve::SolveModel,
    y: &[f64],
    p: &[f64],
) -> Result<bool, RuntimeSolveError> {
    for (run_index, run) in model
        .problem
        .discrete
        .event_iteration_plan
        .runs
        .iter()
        .enumerate()
    {
        if event_iteration_run_clock(model, run)?.is_some() {
            continue;
        }
        let storage = &model.problem.solve_layout.variable_storage_runs[run.variable];
        let value_kind = storage
            .event_iteration_kind()
            .expect("validated event plan references a typed discrete variable");
        for binding_offset in 0..storage.scalar_count {
            let binding_index = run.pre_binding_start + binding_offset;
            let binding = model
                .problem
                .solve_layout
                .pre_param_bindings
                .get(binding_index)
                .ok_or_else(|| {
                    RuntimeSolveError::solve_ir(format!(
                        "event-iteration run {run_index} binding {binding_index} is out of bounds"
                    ))
                })?;
            let current = pre_binding_source_value(binding, y, p).ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "event-iteration run {run_index} current source is out of bounds"
                ))
            })?;
            let pre = p.get(binding.dest_p_index).copied().ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "event-iteration run {run_index} pre parameter {} is out of bounds",
                    binding.dest_p_index
                ))
            })?;
            validate_event_iteration_value(value_kind, current, run_index, "current")?;
            validate_event_iteration_value(value_kind, pre, run_index, "pre")?;
            if current != pre {
                return Ok(false);
            }
        }
    }
    Ok(true)
}

fn event_iteration_run_clock(
    model: &solve::SolveModel,
    run: &solve::EventIterationRun,
) -> Result<Option<solve::PeriodicClockId>, RuntimeSolveError> {
    match run.owner {
        solve::EventIterationOwner::Hold => Ok(None),
        solve::EventIterationOwner::ScalarRows { start_row } => model
            .problem
            .discrete
            .clock_owners
            .get(start_row)
            .copied()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir("event-iteration scalar owner clock is out of bounds")
            }),
        solve::EventIterationOwner::StructuredUpdate { update_index } => model
            .problem
            .discrete
            .structured_updates
            .get(update_index)
            .map(|update| update.clock_owner)
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(
                    "event-iteration structured owner clock is out of bounds",
                )
            }),
    }
}

fn pre_binding_source_value(binding: &solve::PreParamBinding, y: &[f64], p: &[f64]) -> Option<f64> {
    match binding.source {
        solve::PreParamSource::Y { index } => y.get(index).copied(),
        solve::PreParamSource::P { index } => p.get(index).copied(),
    }
}

fn validate_event_iteration_value(
    kind: solve::EventIterationValueKind,
    value: f64,
    run_index: usize,
    lane: &str,
) -> Result<(), RuntimeSolveError> {
    if event_iteration_value_is_valid(kind, value) {
        Ok(())
    } else {
        Err(RuntimeSolveError::solve_ir(format!(
            "event-iteration run {run_index} has an invalid {lane} value {value}"
        )))
    }
}

pub(crate) fn event_iteration_value_is_valid(
    kind: solve::EventIterationValueKind,
    value: f64,
) -> bool {
    match kind {
        solve::EventIterationValueKind::Real => value.is_finite(),
        solve::EventIterationValueKind::Integer => value.is_finite() && value.fract() == 0.0,
        solve::EventIterationValueKind::Boolean => value == 0.0 || value == 1.0,
        solve::EventIterationValueKind::Enumeration => {
            value.is_finite() && value >= 1.0 && value.fract() == 0.0
        }
    }
}

/// MLS 3.7.3: after an event fully settles, `pre(x)` for subsequent event
/// detection is the settled post-event value.
pub fn commit_pre_params_after_event(
    model: &solve::SolveModel,
    y: &[f64],
    params: &mut [f64],
    tol: f64,
) -> bool {
    commit_pre_params_after_event_at(model, y, params, None, tol)
}

/// Commit event history after a settled semantic event boundary.
///
/// Ordinary `pre()` bindings advance after every event. MLS §16 clocked
/// `previous()` bindings advance only when their owning periodic schedule
/// matches `event_time`. Passing `None` is reserved for initialization and
/// compatibility setup, where all histories are seeded.
pub fn commit_pre_params_after_event_at(
    model: &solve::SolveModel,
    y: &[f64],
    params: &mut [f64],
    event_time: Option<f64>,
    tol: f64,
) -> bool {
    let post_event_params = params.to_vec();
    let mut changed = false;
    for binding in &model.problem.solve_layout.pre_param_bindings {
        let should_commit = match (&binding.clock_schedule, event_time) {
            (None, _) | (Some(_), None) => true,
            (Some(schedule), Some(event_time)) => {
                crate::timeline::periodic_schedule_matches_time(schedule, event_time)
            }
        };
        if !should_commit {
            continue;
        }
        let value = match binding.source {
            solve::PreParamSource::Y { index } => y.get(index).copied(),
            solve::PreParamSource::P { index } => post_event_params.get(index).copied(),
        };
        if let (Some(slot), Some(value)) = (params.get_mut(binding.dest_p_index), value) {
            changed |= update_slot(slot, value, tol);
        }
    }
    changed
}

pub fn clear_scheduled_root_relation_memory(
    model: &solve::SolveModel,
    root_indices: &[usize],
    params: &mut [f64],
) -> Result<(), String> {
    for &root_idx in root_indices {
        let Some(Some(target)) = model
            .problem
            .events
            .root_relation_memory_targets
            .get(root_idx)
            .copied()
        else {
            continue;
        };
        let solve::ScalarSlot::P { index, .. } = target else {
            return Err(format!(
                "scheduled sample root {root_idx} relation memory target is not a parameter slot"
            ));
        };
        clear_param_slot(
            params,
            index,
            format_args!(
                "scheduled sample root {root_idx} relation memory parameter index {index}"
            ),
        )?;
        clear_pre_params_from_source_p(model, params, root_idx, index)?;
    }
    Ok(())
}

fn clear_pre_params_from_source_p(
    model: &solve::SolveModel,
    params: &mut [f64],
    root_idx: usize,
    source_index: usize,
) -> Result<(), String> {
    let dest_indices: Vec<_> = model
        .problem
        .solve_layout
        .pre_param_bindings
        .iter()
        .filter_map(|binding| match binding.source {
            solve::PreParamSource::P { index } if index == source_index => {
                Some(binding.dest_p_index)
            }
            _ => None,
        })
        .collect();
    for dest_index in dest_indices {
        clear_param_slot(
            params,
            dest_index,
            format_args!("scheduled sample root {root_idx} pre parameter index {dest_index}"),
        )?;
    }
    Ok(())
}

fn clear_param_slot(
    params: &mut [f64],
    index: usize,
    label: std::fmt::Arguments<'_>,
) -> Result<(), String> {
    let param_len = params.len();
    let Some(slot) = params.get_mut(index) else {
        return Err(format!("{label} is outside {param_len} parameters"));
    };
    *slot = 0.0;
    Ok(())
}

pub fn update_slot(slot: &mut f64, value: f64, tol: f64) -> bool {
    let changed = super::solve_ops::runtime_value_changed(*slot, value, tol);
    *slot = value;
    changed
}

#[cfg(test)]
mod tests {
    use super::*;

    fn binding(
        dest_p_index: usize,
        source_y_index: usize,
        period_seconds: Option<f64>,
    ) -> solve::PreParamBinding {
        solve::PreParamBinding {
            dest_p_index,
            source: solve::PreParamSource::Y {
                index: source_y_index,
            },
            clock_schedule: period_seconds.map(|period_seconds| {
                solve::PeriodicEventSchedule::new(
                    rumoca_core::ClockLattice::from_seconds(period_seconds, 0.0).unwrap(),
                )
                .unwrap()
            }),
        }
    }

    #[test]
    fn clocked_pre_history_advances_only_on_its_own_tick() {
        let mut model = solve::SolveModel::default();
        model.problem.solve_layout.pre_param_bindings = vec![
            binding(0, 0, None),
            binding(1, 1, Some(0.1)),
            binding(2, 2, Some(0.2)),
        ];
        let y = [10.0, 11.0, 12.0];
        let mut params = [0.0, 1.0, 2.0];

        commit_pre_params_after_event_at(&model, &y, &mut params, Some(0.1), 0.0);

        assert_eq!(params, [10.0, 11.0, 2.0]);
    }

    #[test]
    fn initialization_seeds_all_pre_history() {
        let mut model = solve::SolveModel::default();
        model.problem.solve_layout.pre_param_bindings =
            vec![binding(0, 0, None), binding(1, 1, Some(0.1))];
        let mut params = [0.0, 0.0];

        commit_pre_params_after_event(&model, &[3.0, 4.0], &mut params, 0.0);

        assert_eq!(params, [3.0, 4.0]);
    }

    #[test]
    fn event_iteration_advances_ordinary_pre_but_holds_clocked_previous() {
        let mut model = solve::SolveModel::default();
        model.problem.solve_layout.pre_param_bindings = vec![
            solve::PreParamBinding {
                dest_p_index: 0,
                source: solve::PreParamSource::P { index: 2 },
                clock_schedule: None,
            },
            binding(1, 1, Some(0.1)),
        ];
        model.problem.discrete.event_iteration_plan = solve::EventIterationPlan {
            runs: vec![solve::EventIterationRun {
                variable: 0,
                pre_binding_start: 0,
                owner: solve::EventIterationOwner::ScalarRows { start_row: 0 },
            }],
        };
        model.problem.discrete.clock_owners = vec![None];
        model.problem.solve_layout.variable_storage_runs = vec![solve::SolveVariableStorageRun {
            base: solve::scalar_slot_p(2),
            scalar_count: 1,
            role: solve::SolveVariableStorageRole::DiscreteValue,
            value_kind: solve::SolveVariableValueKind::Boolean,
        }];
        let mut params = [0.0, 1.0, 1.0];

        let changed = advance_event_iteration_pre_params(
            &model,
            &[10.0, 11.0],
            &[0.0, 1.0, 1.0],
            &mut params,
        )
        .expect("valid event plan advances atomically");

        assert!(changed);
        assert_eq!(params, [1.0, 1.0, 1.0]);
    }
}
