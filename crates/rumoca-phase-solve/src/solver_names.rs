use super::*;

pub fn solver_vector_names(
    dae_model: &dae::Dae,
    n_total: usize,
) -> Result<Vec<String>, LowerError> {
    Ok(lower_solve_layout(dae_model, n_total)?.solver_maps.names)
}

pub fn build_solver_name_index_maps(
    dae_model: &dae::Dae,
    y_len: usize,
) -> Result<solve::SolverNameIndexMaps, LowerError> {
    let solver_names = collect_solver_names(dae_model, y_len)?;
    let span = dae_model_span(dae_model)?;
    let mut name_to_idx = IndexMap::new();
    reserve_lower_index_map_capacity(
        &mut name_to_idx,
        solver_names.len(),
        "solver name index count",
        span,
    )?;
    for (idx, name) in solver_names.iter().enumerate() {
        name_to_idx.insert(name.clone(), idx);
    }
    insert_solver_name_aliases(dae_model, y_len, &mut name_to_idx)?;
    let mut base_to_indices: IndexMap<String, Vec<usize>> = IndexMap::new();
    for (idx, name) in solver_names.iter().enumerate() {
        let base = dae::component_base_name(name).unwrap_or_else(|| name.to_string());
        if let Some(indices) = base_to_indices.get_mut(&base) {
            reserve_lower_capacity(indices, 1, "solver base-name scalar index count", span)?;
            indices.push(idx);
            continue;
        }
        reserve_lower_index_map_capacity(
            &mut base_to_indices,
            1,
            "solver base-name index count",
            span,
        )?;
        let mut indices = lower_vec_with_capacity(1, "solver base-name scalar index count", span)?;
        indices.push(idx);
        base_to_indices.insert(base, indices);
    }

    Ok(solve::SolverNameIndexMaps {
        names: solver_names,
        name_to_idx,
        base_to_indices,
    })
}

pub(super) fn variable_size(var: &dae::Variable) -> Result<usize, LowerError> {
    var.try_size()
        .map_err(|err| lower_contract_violation(err.to_string(), err.span()))
}

pub(super) fn scalar_count<'a>(
    mut vars: impl Iterator<Item = &'a dae::Variable>,
) -> Result<usize, LowerError> {
    vars.try_fold(0usize, |acc, var| {
        variable_size(var).and_then(|size| {
            acc.checked_add(size).ok_or_else(|| {
                lower_contract_violation(
                    "DAE scalar count overflows usize".to_string(),
                    var.source_span,
                )
            })
        })
    })
}

pub(super) fn var_scalar_names(name: &str, var: &dae::Variable) -> Result<Vec<String>, LowerError> {
    let size = variable_size(var)?;
    if size <= 1 && var.dims.is_empty() {
        let mut names = lower_vec_with_capacity(1, "variable scalar name count", var.source_span)?;
        names.push(name.to_string());
        return Ok(names);
    }
    let mut names = lower_vec_with_capacity(size, "variable scalar name count", var.source_span)?;
    for idx in 0..size {
        names.push(dae::scalar_name_text_for_flat_index(name, &var.dims, idx));
    }
    Ok(names)
}

pub(super) fn collect_scalar_names<'a>(
    vars: impl Iterator<Item = (&'a rumoca_core::VarName, &'a dae::Variable)>,
) -> Result<Vec<String>, LowerError> {
    let mut names = Vec::new();
    for (name, var) in vars {
        let var_names = var_scalar_names(name.as_str(), var)?;
        reserve_lower_capacity(
            &mut names,
            var_names.len(),
            "collected scalar name count",
            var.source_span,
        )?;
        names.extend(var_names);
    }
    Ok(names)
}

fn collect_solver_names(
    dae_model: &dae::Dae,
    solver_len: usize,
) -> Result<Vec<String>, LowerError> {
    let mut names = collect_scalar_names(
        dae_model
            .variables
            .states
            .iter()
            .chain(dae_model.variables.algebraics.iter())
            .chain(dae_model.variables.outputs.iter())
            .filter(|(name, _)| !layout::is_runtime_parameter_tail_variable(dae_model, name)),
    )?;
    names.truncate(solver_len);
    Ok(names)
}
