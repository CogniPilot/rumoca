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

pub fn update_slot(slot: &mut f64, value: f64, tol: f64) -> bool {
    let changed = (*slot - value).abs() > tol;
    *slot = value;
    changed
}
