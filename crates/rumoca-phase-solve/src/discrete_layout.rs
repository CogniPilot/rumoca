use super::*;

pub(super) fn lower_discrete_update_targets(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<Vec<solve::ScalarSlot>, LowerError> {
    let equations = normalized_discrete_update_equations(dae_model)?;
    lower_update_targets_from_equations(dae_model, layout, &equations)
}

pub(super) fn lower_update_targets_from_equations(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    equations: &[dae::Equation],
) -> Result<Vec<solve::ScalarSlot>, LowerError> {
    let mut targets = lower_vec_with_capacity(
        equations.len(),
        "discrete update target count",
        dae_model_span(dae_model)?,
    )?;
    for eq in equations {
        let Some(lhs) = eq.lhs.as_ref() else {
            return Err(LowerError::Unsupported {
                reason: "discrete update equation is missing a target".to_string(),
            });
        };
        let scalar_count = eq.scalar_count.max(1);
        reserve_lower_capacity(
            &mut targets,
            scalar_count,
            "discrete update target count",
            eq.span,
        )?;
        for flat_index in 0..scalar_count {
            let name = discrete_update_scalar_name(
                dae_model,
                lhs.var_name(),
                flat_index,
                scalar_count,
                eq.span,
            )?;
            let Some(slot) = layout.binding(name.as_str()) else {
                return Err(LowerError::MissingBinding { name });
            };
            targets.push(slot);
        }
    }
    Ok(targets)
}

pub(super) fn lower_discrete_pre_modes(
    dae_model: &dae::Dae,
) -> Result<Vec<solve::DiscreteEventPreMode>, LowerError> {
    let equations = normalized_discrete_update_equations(dae_model)?;
    let mut modes = lower_vec_with_capacity(
        equations.len(),
        "discrete pre-mode count",
        dae_model_span(dae_model)?,
    )?;
    for eq in equations {
        let scalar_count = eq.scalar_count.max(1);
        let mode = discrete_pre_mode_for_equation(dae_model, &eq);
        reserve_lower_capacity(&mut modes, scalar_count, "discrete pre-mode count", eq.span)?;
        modes.extend(std::iter::repeat_n(mode, scalar_count));
    }
    Ok(modes)
}

pub(super) fn collect_expression_read_slots(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    expr: &rumoca_core::Expression,
    out: &mut Vec<solve::ScalarSlot>,
) -> Result<(), LowerError> {
    struct ReadSlotCollector<'a, 'out> {
        dae_model: &'a dae::Dae,
        layout: &'a solve::VarLayout,
        out: &'out mut Vec<solve::ScalarSlot>,
        error: Option<LowerError>,
    }

    impl ExpressionVisitor for ReadSlotCollector<'_, '_> {
        fn visit_var_ref(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
        ) {
            if self.error.is_none()
                && let Err(err) = collect_var_ref_read_slots(
                    self.dae_model,
                    self.layout,
                    name.var_name(),
                    subscripts,
                    name.span(),
                    self.out,
                )
            {
                self.error = Some(err);
            }
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
        }
    }

    let mut collector = ReadSlotCollector {
        dae_model,
        layout,
        out,
        error: None,
    };
    collector.visit_expression(expr);
    match collector.error {
        Some(err) => Err(err),
        None => Ok(()),
    }
}

pub(super) fn collect_var_ref_read_slots(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    owner_span: Option<rumoca_core::Span>,
    out: &mut Vec<solve::ScalarSlot>,
) -> Result<(), LowerError> {
    if let Some(indices) = checked_literal_positive_indices(subscripts, owner_span)? {
        let key = if indices.is_empty() {
            name.as_str().to_string()
        } else {
            dae::format_subscript_key(name.as_str(), &indices)
        };
        if let Some(slot) = layout.binding(key.as_str()) {
            reserve_lower_optional_capacity(out, 1, "expression read slot count", owner_span)?;
            out.push(slot);
        }
        return Ok(());
    }
    collect_all_var_slots(dae_model, layout, name, owner_span, out)
}

pub(super) fn collect_all_var_slots(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    name: &rumoca_core::VarName,
    owner_span: Option<rumoca_core::Span>,
    out: &mut Vec<solve::ScalarSlot>,
) -> Result<(), LowerError> {
    let Some(var) = variable_by_name(dae_model, name) else {
        if let Some(slot) = layout.binding(name.as_str()) {
            reserve_lower_optional_capacity(out, 1, "expression read slot count", owner_span)?;
            out.push(slot);
        }
        return Ok(());
    };
    let size = variable_size(var)?;
    reserve_lower_capacity(
        out,
        size.max(1),
        "expression read slot count",
        var.source_span,
    )?;
    for idx in 0..size.max(1) {
        let key = if size <= 1 && var.dims.is_empty() {
            name.as_str().to_string()
        } else {
            dae::scalar_name_text_for_flat_index(name.as_str(), &var.dims, idx)
        };
        if let Some(slot) = layout.binding(key.as_str()) {
            out.push(slot);
        }
    }
    Ok(())
}

pub(super) fn variable_by_name<'a>(
    dae_model: &'a dae::Dae,
    name: &rumoca_core::VarName,
) -> Option<&'a dae::Variable> {
    dae_model
        .variables
        .states
        .get(name)
        .or_else(|| dae_model.variables.algebraics.get(name))
        .or_else(|| dae_model.variables.outputs.get(name))
        .or_else(|| dae_model.variables.inputs.get(name))
        .or_else(|| dae_model.variables.discrete_reals.get(name))
        .or_else(|| dae_model.variables.discrete_valued.get(name))
        .or_else(|| dae_model.variables.parameters.get(name))
}

pub(super) fn condition_memory_base_name(dae_model: &dae::Dae) -> Option<String> {
    let lhs = dae_model.conditions.equations.first()?.lhs.as_ref()?;
    dae::component_base_name(lhs.as_str())
}

pub(super) fn discrete_update_scalar_name(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    flat_index: usize,
    scalar_count: usize,
    span: rumoca_core::Span,
) -> Result<String, LowerError> {
    if scalar_count <= 1 {
        return Ok(lhs.as_str().to_string());
    }
    let dims = discrete_update_dims(dae_model, lhs).ok_or_else(|| {
        lower_contract_violation(
            format!(
                "discrete update array LHS `{}` must be a known DAE variable",
                lhs.as_str()
            ),
            span,
        )
    })?;
    Ok(dae::scalar_name_text_for_flat_index(
        lhs.as_str(),
        dims,
        flat_index,
    ))
}

pub(super) fn discrete_update_dims<'a>(
    dae_model: &'a dae::Dae,
    lhs: &rumoca_core::VarName,
) -> Option<&'a [i64]> {
    dae_model
        .variables
        .states
        .get(lhs)
        .or_else(|| dae_model.variables.algebraics.get(lhs))
        .or_else(|| dae_model.variables.outputs.get(lhs))
        .or_else(|| dae_model.variables.inputs.get(lhs))
        .or_else(|| dae_model.variables.discrete_reals.get(lhs))
        .or_else(|| dae_model.variables.discrete_valued.get(lhs))
        .or_else(|| dae_model.variables.parameters.get(lhs))
        .map(|var| var.dims.as_slice())
}

pub(super) fn insert_solver_name_aliases(
    dae_model: &dae::Dae,
    solver_len: usize,
    name_to_idx: &mut IndexMap<String, usize>,
) -> Result<(), LowerError> {
    let span = dae_model_span(dae_model)?;
    let mut solver_name_set = HashSet::new();
    reserve_lower_hash_set_capacity(
        &mut solver_name_set,
        name_to_idx.len(),
        "solver name alias lookup count",
        span,
    )?;
    for name in name_to_idx.keys() {
        solver_name_set.insert(name.clone());
    }
    let mut offset = 0usize;
    for (name, var) in dae_model
        .variables
        .states
        .iter()
        .chain(dae_model.variables.algebraics.iter())
        .chain(dae_model.variables.outputs.iter())
    {
        if layout::is_runtime_parameter_tail_variable(dae_model, name) {
            continue;
        }
        let size = variable_size(var)?;
        if size == 0 {
            continue;
        }
        if offset >= solver_len {
            break;
        }

        let visible_size = size.min(solver_len - offset);
        if size > 1
            && first_visible_scalar_name(name.as_str(), var)?
                .as_deref()
                .is_some_and(|scalar| solver_name_set.contains(scalar))
            && !name_to_idx.contains_key(name.as_str())
        {
            reserve_lower_index_map_capacity(
                name_to_idx,
                1,
                "solver name alias count",
                var.source_span,
            )?;
            name_to_idx.insert(name.as_str().to_string(), offset);
        }
        for flat_idx in 0..visible_size {
            let canonical_name = if size <= 1 && var.dims.is_empty() {
                name.as_str().to_string()
            } else {
                dae::scalar_name_text_for_flat_index(name.as_str(), &var.dims, flat_idx)
            };
            if !solver_name_set.contains(canonical_name.as_str()) {
                continue;
            }
            let scalar_index =
                checked_solver_scalar_index(offset, flat_idx, canonical_name.as_str(), var)?;
            if !name_to_idx.contains_key(canonical_name.as_str()) {
                reserve_lower_index_map_capacity(
                    name_to_idx,
                    1,
                    "solver scalar name alias count",
                    var.source_span,
                )?;
                name_to_idx.insert(canonical_name, scalar_index);
            }
        }
        offset = checked_solver_scalar_offset(offset, size, name.as_str(), var)?;
    }
    Ok(())
}

pub(super) fn checked_solver_scalar_index(
    offset: usize,
    flat_idx: usize,
    canonical_name: &str,
    var: &dae::Variable,
) -> Result<usize, LowerError> {
    offset.checked_add(flat_idx).ok_or_else(|| {
        lower_contract_violation(
            format!("solver scalar index for `{canonical_name}` overflows host index range"),
            var.source_span,
        )
    })
}

pub(super) fn checked_solver_scalar_offset(
    offset: usize,
    size: usize,
    name: &str,
    var: &dae::Variable,
) -> Result<usize, LowerError> {
    offset.checked_add(size).ok_or_else(|| {
        lower_contract_violation(
            format!("solver scalar offset after `{name}` overflows host index range"),
            var.source_span,
        )
    })
}
