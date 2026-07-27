use rumoca_ir_solve as solve;

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
            clock_schedule: period_seconds.map(|period_seconds| solve::PeriodicEventSchedule {
                period_seconds,
                phase_seconds: 0.0,
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
}
