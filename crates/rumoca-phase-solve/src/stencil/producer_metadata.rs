use super::*;

pub(crate) fn producer_load_strides_for_dae_equation(
    layout: &solve::VarLayout,
    structured_equations: &[dae::StructuredEquationFamily],
    equation_index: usize,
    ops: &[solve::LinearOp],
    span: rumoca_core::Span,
) -> Result<Option<Vec<solve::AffineStencilLoadStride>>, LowerError> {
    let Some(slot) = dae::structured_equation_slot(structured_equations, equation_index) else {
        return Ok(None);
    };
    let Some(family) = structured_equations.get(slot.family_index) else {
        return Ok(None);
    };
    producer_load_strides_for_family_row(layout, family, slot.iteration_index, ops, span)
}

pub(crate) fn producer_load_strides_for_family_row(
    layout: &solve::VarLayout,
    family: &dae::StructuredEquationFamily,
    iteration_index: usize,
    ops: &[solve::LinearOp],
    span: rumoca_core::Span,
) -> Result<Option<Vec<solve::AffineStencilLoadStride>>, LowerError> {
    let Some(regular) = family.regular.as_ref() else {
        return Ok(None);
    };
    // A regular family consumes this table from its base program and validates
    // it against O(rank) corners. Repeating the same access table on every
    // scalar-view row would make metadata O(domain cardinality).
    if iteration_index != 0 {
        return Ok(None);
    }
    if regular.binders.len() != family.domain.binders.len()
        || regular
            .binders
            .iter()
            .zip(&family.domain.binders)
            .any(|(producer, domain)| producer != &domain.display_name)
    {
        return Ok(None);
    }
    let Some(index_tuple) = family
        .domain
        .index_tuple_at(iteration_index)
        .map_err(|error| {
            stencil_contract_violation(format!("structured index domain is invalid: {error}"), span)
        })?
    else {
        return Ok(None);
    };
    let mut used_positions =
        stencil_vec_with_capacity(ops.len(), "producer access op-position flags", span)?;
    used_positions.resize(ops.len(), false);
    let mut strides = stencil_vec_with_capacity(
        regular.accesses.len().min(ops.len()),
        "producer affine load stride count",
        span,
    )?;

    for access in &regular.accesses {
        let Some(shape) = layout.shape(&access.var) else {
            continue;
        };
        let Some(memory_strides) = rumoca_core::row_major_strides(shape) else {
            return Ok(None);
        };
        let Some(unit_strides) =
            access.binder_index_strides(&memory_strides, family.domain.binders.len())
        else {
            return Ok(None);
        };
        let Some(indices) = producer_access_indices(access, &index_tuple, span)? else {
            return Ok(None);
        };
        let key = dae::format_subscript_key(&access.var, &indices);
        let Some(slot) = layout.binding(&key) else {
            return Ok(None);
        };
        if matches!(slot, solve::ScalarSlot::Constant(_)) {
            return Ok(None);
        }
        let Some(terms) = producer_access_stride_terms(&unit_strides, &family.domain, span)? else {
            return Ok(None);
        };
        let Some(op_position) = matching_unused_load_op(ops, &used_positions, slot) else {
            continue;
        };
        used_positions[op_position] = true;
        if !terms.is_empty() {
            strides.push(solve::AffineStencilLoadStride { op_position, terms });
        }
    }
    strides.sort_unstable_by_key(|stride| stride.op_position);
    Ok(Some(strides))
}

fn producer_access_indices(
    access: &rumoca_core::ArrayAccess,
    index_tuple: &[i64],
    span: rumoca_core::Span,
) -> Result<Option<Vec<usize>>, LowerError> {
    let mut indices =
        stencil_vec_with_capacity(access.subscripts.len(), "producer access index rank", span)?;
    for subscript in &access.subscripts {
        if subscript.coeffs.len() != index_tuple.len() {
            return Ok(None);
        }
        let mut value = i128::from(subscript.constant);
        for (coefficient, binder) in subscript.coeffs.iter().zip(index_tuple) {
            value = value
                .checked_add(
                    i128::from(*coefficient)
                        .checked_mul(i128::from(*binder))
                        .ok_or_else(|| {
                            stencil_contract_violation(
                                "producer affine index product overflows i128",
                                span,
                            )
                        })?,
                )
                .ok_or_else(|| {
                    stencil_contract_violation("producer affine index sum overflows i128", span)
                })?;
        }
        let Ok(value) = usize::try_from(value) else {
            return Ok(None);
        };
        indices.push(value);
    }
    Ok(Some(indices))
}

fn producer_access_stride_terms(
    unit_strides: &[i64],
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<Option<Vec<solve::AffineStencilIndexStrideTerm>>, LowerError> {
    if unit_strides.len() != domain.binders.len() {
        return Ok(None);
    }
    let mut terms =
        stencil_vec_with_capacity(unit_strides.len(), "producer access stride rank", span)?;
    for (dimension, (unit_stride, binder)) in unit_strides.iter().zip(&domain.binders).enumerate() {
        let ordinal_stride = i128::from(*unit_stride)
            .checked_mul(i128::from(binder.step))
            .ok_or_else(|| {
                stencil_contract_violation("producer affine stride product overflows i128", span)
            })?;
        let ordinal_stride = isize::try_from(ordinal_stride).map_err(|_| {
            stencil_contract_violation(
                "producer affine stride does not fit host signed index range",
                span,
            )
        })?;
        if ordinal_stride != 0 {
            terms.push(solve::AffineStencilIndexStrideTerm {
                dimension,
                stride: ordinal_stride,
            });
        }
    }
    Ok(Some(terms))
}

fn matching_unused_load_op(
    ops: &[solve::LinearOp],
    used_positions: &[bool],
    slot: solve::ScalarSlot,
) -> Option<usize> {
    ops.iter().enumerate().find_map(|(position, op)| {
        if used_positions.get(position).copied().unwrap_or(true) {
            return None;
        }
        match (slot, op) {
            (
                solve::ScalarSlot::Y {
                    index: expected, ..
                },
                solve::LinearOp::LoadY { index, .. },
            )
            | (
                solve::ScalarSlot::P {
                    index: expected, ..
                },
                solve::LinearOp::LoadP { index, .. },
            ) if expected == *index => Some(position),
            _ => None,
        }
    })
}

/// Consume producer affine access metadata and use the scalar-view corners only
/// as a consistency check. No stride is inferred from numeric row differences.
pub(super) fn producer_affine_strides_from_selected_rows(
    corner_rows: &[&StructuredProgram],
    domain: &rumoca_core::StructuredIndexDomain,
    corner_tuples: &[Vec<i64>],
    span: rumoca_core::Span,
) -> Result<Option<AffineStrides>, LowerError> {
    let Some(base_row) = corner_rows.first() else {
        return Ok(None);
    };
    let Some(load_strides) = base_row.producer_load_strides.as_ref() else {
        return Ok(None);
    };
    if corner_rows.len() != corner_tuples.len() {
        return Ok(None);
    }
    let Some(base_tuple) = corner_tuples.first() else {
        return Ok(None);
    };
    let mut stride_by_position =
        stencil_vec_with_capacity(base_row.ops.len(), "producer stride op lookup", span)?;
    stride_by_position.resize(base_row.ops.len(), None::<&solve::AffineStencilLoadStride>);
    for stride in load_strides {
        let Some(entry) = stride_by_position.get_mut(stride.op_position) else {
            return Ok(None);
        };
        if entry.is_some()
            || stride
                .terms
                .iter()
                .any(|term| term.dimension >= domain.binders.len())
        {
            return Ok(None);
        }
        *entry = Some(stride);
    }

    for (row, tuple) in corner_rows.iter().zip(corner_tuples) {
        if row.ops.len() != base_row.ops.len() {
            return Ok(None);
        }
        for (op_position, (base_op, candidate_op)) in base_row.ops.iter().zip(&row.ops).enumerate()
        {
            let terms =
                stride_by_position[op_position].map_or(&[][..], |stride| stride.terms.as_slice());
            if !producer_corner_op_matches(ProducerCornerOpInput {
                base_op,
                candidate_op,
                terms,
                domain,
                base_tuple,
                tuple,
                span,
            })? {
                return Ok(None);
            }
        }
    }
    Ok(Some(AffineStrides {
        load_strides: load_strides.clone(),
        const_strides: Vec::new(),
    }))
}

struct ProducerCornerOpInput<'a> {
    base_op: &'a solve::LinearOp,
    candidate_op: &'a solve::LinearOp,
    terms: &'a [solve::AffineStencilIndexStrideTerm],
    domain: &'a rumoca_core::StructuredIndexDomain,
    base_tuple: &'a [i64],
    tuple: &'a [i64],
    span: rumoca_core::Span,
}

fn producer_corner_op_matches(input: ProducerCornerOpInput<'_>) -> Result<bool, LowerError> {
    let ProducerCornerOpInput {
        base_op,
        candidate_op,
        terms,
        domain,
        base_tuple,
        tuple,
        span,
    } = input;
    match (base_op, candidate_op) {
        (
            solve::LinearOp::LoadY {
                index: base_index, ..
            },
            solve::LinearOp::LoadY {
                index: candidate_index,
                ..
            },
        )
        | (
            solve::LinearOp::LoadP {
                index: base_index, ..
            },
            solve::LinearOp::LoadP {
                index: candidate_index,
                ..
            },
        ) => apply_producer_index_terms(*base_index, terms, domain, base_tuple, tuple, span)
            .map(|expected| expected == Some(*candidate_index)),
        (
            solve::LinearOp::Const {
                value: base_value, ..
            },
            solve::LinearOp::Const {
                value: candidate_value,
                ..
            },
        ) => Ok(base_value.to_bits() == candidate_value.to_bits()),
        (solve::LinearOp::LoadY { .. } | solve::LinearOp::LoadP { .. }, _)
        | (solve::LinearOp::Const { .. }, _) => Ok(false),
        _ => Ok(true),
    }
}

fn apply_producer_index_terms(
    base_index: usize,
    terms: &[solve::AffineStencilIndexStrideTerm],
    domain: &rumoca_core::StructuredIndexDomain,
    base_tuple: &[i64],
    tuple: &[i64],
    span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    let mut value = i128::try_from(base_index)
        .map_err(|_| stencil_contract_violation("producer access base index exceeds i128", span))?;
    for term in terms {
        let Some(ordinal) =
            ordinal_delta_with_span(term.dimension, domain, base_tuple, tuple, span)?
        else {
            return Ok(None);
        };
        let delta = i128::try_from(term.stride)
            .ok()
            .and_then(|stride| stride.checked_mul(i128::from(ordinal)))
            .ok_or_else(|| {
                stencil_contract_violation("producer access stride delta overflows i128", span)
            })?;
        value = value.checked_add(delta).ok_or_else(|| {
            stencil_contract_violation("producer access index sum overflows i128", span)
        })?;
    }
    Ok(usize::try_from(value).ok())
}
