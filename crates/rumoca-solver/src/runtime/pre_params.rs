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
    let post_event_params = params.to_vec();
    write_pre_params_from_sources(model, y, &post_event_params, params, tol)
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
    let changed = (*slot - value).abs() > tol;
    *slot = value;
    changed
}
