use super::*;

pub(super) fn lower_direct_derivative_families(
    processed: &mut [bool],
    analysis: &DerivativeRhsAnalysis,
    ctx: &DerivativeRhsLoweringContext<'_>,
    y_slot_ranges: &crate::stencil::YSlotRanges,
) -> Result<IndexMap<usize, Vec<ComputeNode>>, LowerError> {
    let state_indices_by_equation = direct_state_indices_by_dae_equation(analysis);
    let mut nodes_by_first_state = IndexMap::new();
    for family in &ctx.dae_model.continuous.structured_equations {
        // This path lifts the corner cells over the family's ENTIRE domain and
        // then marks every state it covers `processed`, so the row-based path
        // never revisits them. It may therefore run only where uniformity over
        // that domain is proven. An unproven family is left untouched here and
        // falls through to the row-based lowering, which reads every row and
        // declines to scalar when the bodies diverge.
        if !crate::stencil::compact_corner_lowering_is_proven(family) {
            continue;
        }
        let Some(state_indices) = direct_family_state_indices(family, &state_indices_by_equation)?
        else {
            continue;
        };
        if state_indices
            .iter()
            .any(|index| processed.get(*index).copied().unwrap_or(true))
        {
            continue;
        }
        let Some(mut family_nodes) =
            direct_family_tensor_nodes(family, &state_indices, analysis, ctx, y_slot_ranges)?
        else {
            continue;
        };
        let Some(first_state) = state_indices.iter().copied().min() else {
            continue;
        };
        let entry = nodes_by_first_state.entry(first_state).or_default();
        reserve_derivative_capacity(
            entry,
            family_nodes.len(),
            "direct derivative family tensor node count",
            family.span,
        )?;
        entry.append(&mut family_nodes);
        for state_index in state_indices {
            if let Some(state_processed) = processed.get_mut(state_index) {
                *state_processed = true;
            }
        }
    }
    Ok(nodes_by_first_state)
}

fn direct_state_indices_by_dae_equation(
    analysis: &DerivativeRhsAnalysis,
) -> IndexMap<usize, Vec<usize>> {
    let mut by_equation = IndexMap::<usize, Vec<usize>>::new();
    for (state_index, state) in analysis.states.iter().enumerate() {
        let Some(equation_index) = analysis
            .direct_equations
            .get(&state.name)
            .and_then(|equation| analysis.equations.get(*equation))
            .and_then(|equation| equation.dae_equation_index)
        else {
            continue;
        };
        by_equation
            .entry(equation_index)
            .or_default()
            .push(state_index);
    }
    by_equation
}

fn direct_family_state_indices(
    family: &dae::StructuredEquationFamily,
    state_indices_by_equation: &IndexMap<usize, Vec<usize>>,
) -> Result<Option<Vec<usize>>, LowerError> {
    let point_count = family.point_count().map_err(|error| {
        LowerError::contract_violation(
            format!("invalid direct derivative family domain: {error}"),
            family.span,
        )
    })?;
    let state_count = point_count
        .checked_mul(family.equations_per_point)
        .ok_or_else(|| {
            LowerError::contract_violation(
                "direct derivative family state count overflows host index range",
                family.span,
            )
        })?;
    if state_count < 2 {
        return Ok(None);
    }
    let mut scalar_indices = derivative_vec_with_capacity(
        state_count,
        "direct derivative family state count",
        family.span,
    )?;
    for offset in 0..state_count {
        let Some(equation_index) = family.first_equation_index.checked_add(offset) else {
            return Err(LowerError::contract_violation(
                "direct derivative family equation index overflows host index range",
                family.span,
            ));
        };
        let Some(indices) = state_indices_by_equation.get(&equation_index) else {
            scalar_indices.clear();
            break;
        };
        let [state_index] = indices.as_slice() else {
            scalar_indices.clear();
            break;
        };
        scalar_indices.push(*state_index);
    }
    if scalar_indices.len() == state_count {
        return Ok(Some(scalar_indices));
    }
    let Some(indices) = state_indices_by_equation.get(&family.first_equation_index) else {
        return Ok(None);
    };
    if indices.len() != state_count {
        return Ok(None);
    }
    Ok(Some(indices.clone()))
}

fn direct_family_tensor_nodes(
    family: &dae::StructuredEquationFamily,
    state_indices: &[usize],
    analysis: &DerivativeRhsAnalysis,
    ctx: &DerivativeRhsLoweringContext<'_>,
    y_slot_ranges: &crate::stencil::YSlotRanges,
) -> Result<Option<Vec<ComputeNode>>, LowerError> {
    let corner_ordinals = family.domain.corner_ordinals().map_err(|error| {
        LowerError::contract_violation(
            format!("invalid direct derivative family domain: {error}"),
            family.span,
        )
    })?;
    if corner_ordinals.len() < 2 {
        return Ok(None);
    }
    let mut nodes = derivative_vec_with_capacity(
        family.equations_per_point,
        "direct derivative family tensor node count",
        family.span,
    )?;
    for equation_position in 0..family.equations_per_point {
        let mut programs = derivative_vec_with_capacity(
            corner_ordinals.len(),
            "direct derivative family corner program count",
            family.span,
        )?;
        for ordinal in &corner_ordinals {
            let scalar_position = ordinal
                .checked_mul(family.equations_per_point)
                .and_then(|position| position.checked_add(equation_position))
                .ok_or_else(|| {
                    LowerError::contract_violation(
                        "direct derivative family scalar position overflows host index range",
                        family.span,
                    )
                })?;
            let Some(state_index) = state_indices.get(scalar_position).copied() else {
                return Ok(None);
            };
            programs.push(direct_family_corner_program(
                state_index,
                analysis,
                ctx,
                y_slot_ranges,
            )?);
        }
        // Same body-shape gate `regular_family_full_domain` applies before it
        // lowers a regular family from corners: one kernel is lifted over the
        // domain, so the corner bodies must agree on that kernel's shape.
        if !crate::stencil::compact_corner_body_shapes_match(
            &programs,
            &ctx.dae_model.continuous.equations,
            family.span,
        )? {
            return Ok(None);
        }
        let Some(node) = crate::stencil::tensor_node_from_compact_corners(
            &programs,
            &family.domain,
            family.span,
        )?
        else {
            return Ok(None);
        };
        nodes.push(node);
    }
    Ok(Some(nodes))
}

fn direct_family_corner_program(
    state_index: usize,
    analysis: &DerivativeRhsAnalysis,
    ctx: &DerivativeRhsLoweringContext<'_>,
    y_slot_ranges: &crate::stencil::YSlotRanges,
) -> Result<crate::stencil::StructuredProgram, LowerError> {
    let Some(state) = analysis.states.get(state_index) else {
        return Err(LowerError::contract_violation(
            "direct derivative family references a missing state",
            first_dae_state_span(ctx.dae_model)?,
        ));
    };
    let dae_equation_index = analysis
        .direct_equations
        .get(&state.name)
        .and_then(|equation| analysis.equations.get(*equation))
        .and_then(|equation| equation.dae_equation_index);
    let span = dae_equation_index
        .and_then(|index| ctx.dae_model.continuous.equations.get(index))
        .map(|equation| equation.span)
        .filter(|span| !span.is_dummy())
        .unwrap_or(derivative_state_or_context_span(ctx.dae_model, state)?);
    let ops = lower_state_derivative_row(state, &analysis.direct_equations, ctx)?;
    Ok(crate::stencil::StructuredProgram {
        load_y_ranges: crate::stencil::structured_load_y_ranges(&ops, y_slot_ranges, span)?,
        ops,
        output_index: state_index,
        pointwise_output_y_index: Some(state_index),
        span,
        output_y_range: state_output_y_range(ctx.dae_model, state, state_index)?,
        dae_equation_index,
        access_proof: derivative_row_access_proof(state, &analysis.direct_equations, ctx)?,
    })
}

pub(super) fn component_indices_are_contiguous_from(component: &[usize], start: usize) -> bool {
    component
        .iter()
        .copied()
        .eq(start..start.saturating_add(component.len()))
}

pub(super) fn flush_pending_derivative_programs(
    nodes: &mut Vec<ComputeNode>,
    pending: &mut Vec<crate::stencil::StructuredProgram>,
    dae_model: &dae::Dae,
    declines: &mut crate::tensor_declines::TensorDeclineJournal,
) -> Result<(), LowerError> {
    if !pending.is_empty() {
        crate::stencil::push_structured_programs(
            nodes,
            pending,
            &dae_model.continuous.structured_equations,
            &dae_model.continuous.equations,
            declines,
        )?;
    }
    Ok(())
}

pub(super) fn state_output_y_range(
    dae_model: &dae::Dae,
    state: &StateScalar,
    output_index: usize,
) -> Result<std::ops::Range<usize>, LowerError> {
    let start = output_index.checked_sub(state.component).ok_or_else(|| {
        derivative_component_contract_error(
            dae_model,
            state,
            format!(
                "derivative output index {output_index} is before component {} for state `{}`",
                state.component, state.name
            ),
        )
    })?;
    let end = start.checked_add(state.base_size).ok_or_else(|| {
        derivative_component_contract_error(
            dae_model,
            state,
            format!(
                "derivative output range overflows for state `{}` with base size {}",
                state.name, state.base_size
            ),
        )
    })?;
    Ok(start..end)
}
