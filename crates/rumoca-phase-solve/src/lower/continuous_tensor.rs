use super::*;

pub(super) fn lower_explicit_tensor_derivative_family<'dae>(
    context: ContinuousContext<'_, 'dae>,
    first_row: usize,
    family: dae::StructuredFamilyView<'dae>,
) -> Result<Option<ImplicitTensorDerivative>, LowerError> {
    if family.bodies().len() != 1 {
        return Ok(None);
    }
    let body = family
        .bodies()
        .get(0)
        .expect("single checked structured derivative body resolves");
    let checked_domain = context
        .view
        .domain(family.domain())
        .expect("checked family domain resolves");
    let domain = checked_domain.structured();
    let points = domain.index_tuples().map_err(|error| {
        LowerError::contract(
            format!("structured derivative domain is invalid: {error}"),
            family.provenance().span(),
        )
    })?;
    let Some(first_point) = points.first() else {
        return Ok(None);
    };
    let extents = checked_domain.extents();
    let first_scalar = family
        .scalar_view()
        .body_scalar(0, extents)
        .ok_or_else(|| scalar_view_overflow(family))?;
    let Some(UnknownId::Derivative {
        state,
        scalar: first_state_scalar,
    }) = context.matching.get(&first_row).copied()
    else {
        return Ok(None);
    };
    let first_state_scalar = usize::try_from(first_state_scalar).map_err(|_| {
        LowerError::contract(
            "structured derivative state-scalar ordinal exceeds host capacity",
            family.provenance().span(),
        )
    })?;
    let candidate = derivative_rhs(
        context.view,
        body,
        first_scalar,
        Some((family.domain(), first_point)),
        state,
        first_state_scalar,
    );
    let candidate = match candidate {
        Ok(candidate) => candidate,
        Err(LowerError::NonComputable { .. }) => return Ok(None),
        Err(error) => return Err(error),
    };
    let DerivativeRhs::Explicit { expression, scalar } = candidate else {
        return Ok(None);
    };
    if scalar != first_scalar {
        return Ok(None);
    }
    let Some(state_scalars) = family_points_explicit_state_scalars(
        context, first_row, family, state, expression, &points,
    )?
    else {
        return Ok(None);
    };
    build_explicit_tensor_derivative(context, family, state, expression, domain, &state_scalars)
}

/// The matched state scalar of every domain point, in domain order, when each
/// point's row explicitly defines the derivative of one scalar of `state`
/// through the same expression. Which scalar a point defines is read from the
/// matching, so a family over a strided sub-range of the state array (the
/// interior of a grid) qualifies; whether those scalars form an affine output
/// map is decided by the caller.
fn family_points_explicit_state_scalars<'dae>(
    context: ContinuousContext<'_, 'dae>,
    first_row: usize,
    family: dae::StructuredFamilyView<'dae>,
    state: dae::StateId<'dae>,
    expression: dae::ExprId<'dae>,
    points: &[Vec<i64>],
) -> Result<Option<Vec<usize>>, LowerError> {
    let body = family
        .bodies()
        .get(0)
        .expect("single checked structured derivative body resolves");
    let extents = context
        .view
        .domain(family.domain())
        .expect("checked family domain resolves")
        .extents();
    let span = family.provenance().span();
    let mut state_scalars = Vec::with_capacity(points.len());
    for (point, values) in points.iter().enumerate() {
        let row = first_row.checked_add(point).ok_or_else(|| {
            LowerError::contract("structured derivative row ordinal overflow", span)
        })?;
        let Some(UnknownId::Derivative {
            state: found_state,
            scalar: found_scalar,
        }) = context.matching.get(&row).copied()
        else {
            return Ok(None);
        };
        let Ok(state_scalar) = usize::try_from(found_scalar) else {
            return Ok(None);
        };
        if found_state != state {
            return Ok(None);
        }
        let body_scalar = family
            .scalar_view()
            .body_scalar(point, extents)
            .ok_or_else(|| scalar_view_overflow(family))?;
        let DerivativeRhs::Explicit {
            expression: found_expression,
            scalar: found_scalar,
        } = derivative_rhs(
            context.view,
            body,
            body_scalar,
            Some((family.domain(), values)),
            state,
            state_scalar,
        )?
        else {
            return Ok(None);
        };
        if found_expression != expression || found_scalar != body_scalar {
            return Ok(None);
        }
        state_scalars.push(state_scalar);
    }
    Ok(Some(state_scalars))
}

fn build_explicit_tensor_derivative<'dae>(
    context: ContinuousContext<'_, 'dae>,
    family: dae::StructuredFamilyView<'dae>,
    state: dae::StateId<'dae>,
    expression: dae::ExprId<'dae>,
    domain: &rumoca_core::StructuredIndexDomain,
    state_scalars: &[usize],
) -> Result<Option<ImplicitTensorDerivative>, LowerError> {
    let span = family.provenance().span();
    let Ok((base_ops, load_strides, const_strides)) =
        events::structured::unclocked_structured_program(
            context.view,
            context.layout,
            family.domain(),
            domain,
            family.scalar_view(),
            expression,
            span,
        )
    else {
        // A compact Map is emitted only when every point proves one affine
        // scalar program. The existing scalar path remains authoritative for
        // structured derivative families that do not admit that certificate.
        return Ok(None);
    };
    let outputs = state_scalars
        .iter()
        .map(|&scalar| state_output_slot(context.layout, state, scalar, span))
        .collect::<Result<Vec<_>, _>>()?;
    let Some(output_map) = solve::TensorOutputMap::from_outputs(domain, &outputs)
        .map_err(|_| LowerError::contract("structured derivative output map overflow", span))?
    else {
        // Points whose derivative slots are not one affine image of the
        // domain keep their scalar rows.
        return Ok(None);
    };
    let output_start = outputs
        .iter()
        .copied()
        .min()
        .expect("a nonempty family writes at least one derivative slot");
    let is_stencil = reads_neighboring_state_scalars(
        context.layout,
        state,
        &output_map,
        &base_ops,
        &load_strides,
        span,
    )?;
    let metadata = solve::TensorNodeMetadata::default();
    let node = if is_stencil {
        solve::ComputeNode::AffineStencil {
            domain: domain.clone(),
            output_map,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
        }
    } else {
        solve::ComputeNode::Map {
            domain: domain.clone(),
            output_map,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
        }
    };
    Ok(Some(ImplicitTensorDerivative {
        node,
        output_start,
        rows: outputs.len(),
        span,
    }))
}

fn state_output_slot(
    layout: &LoweredLayout<'_>,
    state: dae::StateId<'_>,
    scalar: usize,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    match variable_scalar_slot(layout, state.index(), scalar, span)? {
        solve::ScalarSlot::Y { index, .. } => Ok(index),
        _ => Err(LowerError::contract(
            "a state scalar has no Solve state slot",
            span,
        )),
    }
}

fn reads_neighboring_state_scalars(
    layout: &LoweredLayout<'_>,
    state: dae::StateId<'_>,
    output_map: &solve::TensorOutputMap,
    base_ops: &[solve::LinearOp],
    load_strides: &[solve::AffineStencilLoadStride],
    span: rumoca_core::Span,
) -> Result<bool, LowerError> {
    let state_index = usize::try_from(state.index())
        .map_err(|_| LowerError::contract("state identity exceeds host capacity", span))?;
    let state_slot = layout
        .variables
        .get(state_index)
        .ok_or_else(|| LowerError::contract("state has no Solve layout entry", span))?;
    let state_end = state_slot
        .base
        .checked_add(state_slot.count)
        .ok_or_else(|| LowerError::contract("state Solve layout range overflow", span))?;
    Ok(base_ops.iter().enumerate().any(|(position, operation)| {
        let solve::LinearOp::LoadY { index, .. } = operation else {
            return false;
        };
        if *index < state_slot.base || *index >= state_end {
            return false;
        }
        let terms = load_strides
            .iter()
            .find(|stride| stride.op_position == position)
            .map_or(&[][..], |stride| stride.terms.as_slice());
        *index != output_map.start || terms != output_map.strides
    }))
}

fn scalar_view_overflow(family: dae::StructuredFamilyView<'_>) -> LowerError {
    LowerError::contract(
        "structured derivative scalar view overflow",
        family.provenance().span(),
    )
}
