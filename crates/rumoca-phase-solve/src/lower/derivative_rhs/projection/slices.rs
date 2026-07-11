use super::*;
use crate::lower::{
    compile_time_index_expr_with_owner, compile_time_index_raw_with_owner,
    compile_time_subscript_index_with_owner,
};

pub(in crate::lower) fn scalar_keys_for_dims(
    base: &str,
    dims: &[usize],
    span: rumoca_core::Span,
) -> Result<Vec<String>, LowerError> {
    let dims_i64 = checked_usize_dims_to_i64(dims, "scalarized binding dimension", span)?;
    let size = checked_usize_scalar_count(dims, "scalarized binding dimensions", span)?;
    let mut keys = derivative_vec_with_capacity(size, "scalarized binding key count", span)?;
    for flat_index in 0..size {
        let Some(subscripts) = dae::flat_index_to_subscripts(&dims_i64, flat_index) else {
            return Err(LowerError::contract_violation(
                format!(
                    "scalarized binding `{base}` flat index {flat_index} is outside dimensions {}",
                    format_i64_dims(&dims_i64)
                ),
                span,
            ));
        };
        keys.push(dae::format_subscript_key(base, &subscripts));
    }
    Ok(keys)
}

pub(in crate::lower) fn slice_selections(
    subscripts: &[rumoca_core::Subscript],
    dims: &[usize],
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<Vec<usize>>, LowerError> {
    let span = subscript_list_span_or_owner(subscripts, owner_span)?;
    tracing::debug!(
        target: "rumoca_phase_solve::projection",
        ?subscripts,
        ?dims,
        "projecting array slice"
    );
    if subscripts.len() > dims.len() {
        let span = subscripts
            .get(dims.len())
            .map(|subscript| span_or_owner(subscript_span(subscript), span))
            .unwrap_or(span);
        return Err(unsupported_at(
            format!(
                "array derivative slice rank {} exceeds base rank {} for dimensions {dims:?}",
                subscripts.len(),
                dims.len()
            ),
            span,
        ));
    }
    let mut selections =
        derivative_vec_with_capacity(dims.len(), "derivative slice selection count", span)?;
    for (idx, subscript) in subscripts.iter().enumerate() {
        selections.push(slice_subscript_indices(
            subscript,
            dims[idx],
            structural_bindings,
            span,
        )?);
    }
    for &dim in &dims[subscripts.len()..] {
        selections.push(one_based_index_range(
            dim,
            "derivative trailing full-slice index count",
            span,
        )?);
    }
    Ok(selections)
}

pub(in crate::lower) fn slice_subscript_indices(
    subscript: &rumoca_core::Subscript,
    dim: usize,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let span = span_or_owner(subscript_span(subscript), owner_span);
    let indices = match subscript {
        rumoca_core::Subscript::Index { value, .. } if *value > 0 => {
            let mut indices =
                derivative_vec_with_capacity(1, "derivative singleton slice index count", span)?;
            indices.push(positive_i64_index(*value, span)?);
            indices
        }
        rumoca_core::Subscript::Expr { expr, .. } => {
            slice_expr_indices(expr, structural_bindings, span)?
        }
        rumoca_core::Subscript::Colon { .. } => {
            one_based_index_range(dim, "derivative colon slice index count", span)?
        }
        _ => {
            return Err(unsupported_at(
                "non-positive derivative slice subscript is unsupported",
                span,
            ));
        }
    };
    if indices.iter().all(|index| *index > 0 && *index <= dim) {
        Ok(indices)
    } else {
        Err(unsupported_at(
            "derivative slice index is outside dimension bounds",
            span,
        ))
    }
}

pub(in crate::lower) fn slice_expr_indices(
    expr: &rumoca_core::Expression,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    match expr {
        rumoca_core::Expression::Range {
            start,
            step,
            end,
            span,
        } => compile_time_positive_range_with_owner(
            start,
            step.as_deref(),
            end,
            structural_bindings,
            span_or_owner(*span, owner_span),
        ),
        _ => {
            let span = projection_expr_span_or_owner(expr, owner_span)?;
            let mut indices =
                derivative_vec_with_capacity(1, "derivative expression slice index count", span)?;
            indices.push(compile_time_index_expr_with_owner(
                expr,
                structural_bindings,
                span,
            )?);
            Ok(indices)
        }
    }
}

#[cfg(test)]
pub(in crate::lower) fn compile_time_positive_range(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let span = projection_expr_pair_span_or_owner(start, end, owner_span)?;
    compile_time_positive_range_with_owner(start, step, end, structural_bindings, span)
}

fn compile_time_positive_range_with_owner(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    let span = projection_expr_pair_span_or_owner(start, end, owner_span)?;
    let start = compile_time_integer_with_owner(start, structural_bindings, span)?;
    let step = match step {
        Some(step) => compile_time_integer_with_owner(step, structural_bindings, span)?,
        None => 1,
    };
    let end = compile_time_integer_with_owner(end, structural_bindings, span)?;
    if step == 0 {
        return Err(unsupported_at(
            "zero derivative slice range step is unsupported",
            span,
        ));
    }
    let mut values = derivative_vec_with_capacity(0, "derivative slice range index count", span)?;
    let mut current = start;
    while range_step_keeps_going(current, end, step) {
        let value = usize::try_from(current)
            .map_err(|_| unsupported_at("derivative slice range index must be positive", span))?;
        if value == 0 {
            return Err(unsupported_at(
                "derivative slice range index must be positive",
                span,
            ));
        }
        reserve_derivative_capacity(&mut values, 1, "derivative slice range index count", span)?;
        values.push(value);
        if values.len() > 100_000 {
            return Err(unsupported_at("derivative slice range is too large", span));
        }
        if range_would_overshoot_i64(current, end, step) {
            break;
        }
        current = next_range_value(current, step, span)?;
    }
    Ok(values)
}

pub(in crate::lower) fn range_step_keeps_going(current: i64, end: i64, step: i64) -> bool {
    if step > 0 {
        current <= end
    } else {
        current >= end
    }
}

pub(super) fn next_range_value(
    current: i64,
    step: i64,
    span: rumoca_core::Span,
) -> Result<i64, LowerError> {
    current.checked_add(step).ok_or_else(|| {
        LowerError::contract_violation(
            format!("derivative slice range step overflows after index {current} with step {step}"),
            span,
        )
    })
}

pub(super) fn range_would_overshoot_i64(current: i64, end: i64, step: i64) -> bool {
    if step > 0 && current <= end {
        return end
            .checked_sub(current)
            .is_some_and(|remaining| remaining < step);
    }
    if step < 0 && current >= end {
        let Some(step_abs) = step.checked_neg() else {
            return false;
        };
        return current
            .checked_sub(end)
            .is_some_and(|remaining| remaining < step_abs);
    }
    false
}

#[cfg(test)]
pub(in crate::lower) fn compile_time_integer(
    expr: &rumoca_core::Expression,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<i64, LowerError> {
    compile_time_integer_with_owner(expr, structural_bindings, owner_span)
}

fn compile_time_integer_with_owner(
    expr: &rumoca_core::Expression,
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<i64, LowerError> {
    let span = projection_expr_span_or_owner(expr, owner_span)?;
    let raw = compile_time_index_raw_with_owner(expr, structural_bindings, span)?;
    checked_integer_f64_to_i64(raw, "derivative slice range expression", span)
}

pub(in crate::lower) fn compile_time_subscript_indices_with_owner(
    subscripts: &[rumoca_core::Subscript],
    structural_bindings: &IndexMap<String, f64>,
    owner_span: rumoca_core::Span,
) -> Result<Vec<usize>, LowerError> {
    if subscripts.is_empty() {
        return Ok(Vec::new());
    }
    let span = subscript_list_span_or_owner(subscripts, owner_span)?;
    let mut indices =
        derivative_vec_with_capacity(subscripts.len(), "compile-time subscript index count", span)?;
    for subscript in subscripts {
        indices.push(compile_time_subscript_index_with_owner(
            subscript,
            structural_bindings,
            span,
        )?);
    }
    Ok(indices)
}

fn projection_expr_span_or_owner(
    expr: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Span, LowerError> {
    let span = expr
        .span()
        .filter(|span| !span.is_dummy())
        .unwrap_or(owner_span);
    Ok(span
        .require_provenance("derivative projection expression")?
        .span())
}

fn projection_expr_pair_span_or_owner(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Span, LowerError> {
    let span = lhs
        .span()
        .or_else(|| rhs.span())
        .filter(|span| !span.is_dummy())
        .unwrap_or(owner_span);
    Ok(span
        .require_provenance("derivative projection expression pair")?
        .span())
}

fn span_or_owner(span: rumoca_core::Span, owner_span: rumoca_core::Span) -> rumoca_core::Span {
    if span.is_dummy() { owner_span } else { span }
}

pub(in crate::lower) fn collect_slice_keys(
    base: &str,
    selections: &[Vec<usize>],
    span: rumoca_core::Span,
    depth: usize,
    current: &mut Vec<usize>,
    keys: &mut Vec<String>,
) -> Result<(), LowerError> {
    if depth == selections.len() {
        reserve_derivative_capacity(keys, 1, "derivative slice key count", span)?;
        keys.push(dae::format_subscript_key(base, current));
        return Ok(());
    }
    for &index in &selections[depth] {
        reserve_derivative_capacity(current, 1, "derivative slice key depth", span)?;
        current.push(index);
        collect_slice_keys(base, selections, span, depth + 1, current, keys)?;
        current.pop();
    }
    Ok(())
}

pub(in crate::lower) fn literal_array_elements(
    expr: &rumoca_core::Expression,
) -> Result<Option<Vec<rumoca_core::Expression>>, LowerError> {
    match expr {
        rumoca_core::Expression::Array { elements, span, .. }
        | rumoca_core::Expression::Tuple { elements, span } => {
            let mut copied =
                derivative_vec_with_capacity(elements.len(), "literal array element count", *span)?;
            for element in elements {
                copied.push(element.clone());
            }
            Ok(Some(copied))
        }
        _ => Ok(None),
    }
}

pub(in crate::lower) fn literal_array_elements_flat(
    elements: &[rumoca_core::Expression],
    owner_span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    let span = projection_first_expr_or_owner_span(elements, owner_span)?;
    let mut flattened = derivative_vec_with_capacity(
        flattened_literal_array_element_count(elements, span)?,
        "flattened literal array element count",
        span,
    )?;
    for element in elements {
        match element {
            rumoca_core::Expression::Array { elements: row, .. } => {
                for value in row {
                    flattened.push(value.clone());
                }
            }
            _ => flattened.push(element.clone()),
        }
    }
    Ok(flattened)
}

fn flattened_literal_array_element_count(
    elements: &[rumoca_core::Expression],
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    elements.iter().try_fold(0usize, |count, element| {
        let additional = match element {
            rumoca_core::Expression::Array { elements: row, .. } => row.len(),
            _ => 1,
        };
        count.checked_add(additional).ok_or_else(|| {
            LowerError::contract_violation(
                "flattened literal array element count overflows host index range",
                span,
            )
        })
    })
}
