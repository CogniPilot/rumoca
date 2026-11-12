//! Assignment through an array-slice target (MLS 3.6 §10.5, §10.6.1).

use super::*;

/// Assign through a slice target (`x[a:b] := values`, `x[:] := values`,
/// `x[i, a:b] := values`).
///
/// Every index the subscript enumerates is computed in signed arithmetic before
/// anything is written, so a descending range, an empty range and an
/// out-of-range index are all decided rather than discovered mid-write. The
/// previous unsigned form panicked on `v[3:-1:1]` and divided by zero on a zero
/// step; both are subscripts the parser accepts and OMC rejects cleanly.
pub(super) fn assign_array_slice(
    env: &mut FunctionEnv,
    base_name: &str,
    subscripts: &[Subscript],
    value: Value,
    eval: &EvalState<'_>,
) -> Result<(), EvalError> {
    let target = env
        .get(base_name)
        .ok_or_else(|| EvalError::unknown_variable(base_name, eval.span))?
        .clone();
    let extents = array_extents(&target);
    if subscripts.len() > extents.len() {
        return Err(EvalError::type_mismatch(
            format!("array of at least {} dimensions", subscripts.len()),
            target.type_name(),
            eval.span,
        ));
    }
    let selections = subscripts
        .iter()
        .zip(extents)
        .map(|(subscript, extent)| target_selection(subscript, extent, env, eval))
        .collect::<Result<Vec<_>, _>>()?;
    let new_value = write_selection(target, &selections, value, eval.span)?;
    if !env.set(base_name, new_value) {
        return Err(EvalError::function_error(
            format!("cannot assign to array: {}", base_name),
            eval.span,
        ));
    }
    Ok(())
}

/// The 1-based indices a slice subscript enumerates over a container of `len`.
///
/// MLS 3.6 §10.5 gives `:` the whole dimension, and §10.4.1 gives `a:s:b` the
/// values `a, a+s, …` up to `b` — empty when the step points away from `b`, and
/// undefined for a zero step, which is reported rather than divided by.
fn slice_indices(
    subscript: &Subscript,
    len: usize,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Vec<i64>, EvalError> {
    let Subscript::Expr { expr, .. } = subscript else {
        // `x[:] := …` names every element of the dimension. It is a slice like
        // any other, so it is size-checked against the container the same way;
        // treating it as a whole-value replacement is what let
        // `v[:] := {1,2,3,4}` resize an `Integer v[2]`.
        if matches!(subscript, Subscript::Colon { .. }) {
            return Ok((1..=len as i64).collect());
        }
        return Err(EvalError::function_error(
            "expected range subscript for slice assignment".to_string(),
            eval.span,
        ));
    };
    let Expression::Range {
        start, step, end, ..
    } = expr.as_ref()
    else {
        return Err(EvalError::function_error(
            "expected range subscript for slice assignment".to_string(),
            eval.span,
        ));
    };
    let start = slice_bound(start, env, eval)?;
    let end = slice_bound(end, env, eval)?;
    let step = match step {
        Some(step) => slice_bound(step, env, eval)?,
        None => 1,
    };
    if step == 0 {
        return Err(EvalError::range_error(
            "range step must not be zero in a slice assignment target",
            eval.span,
        ));
    }
    let budget = i64::try_from(eval.limits.max_iterations).unwrap_or(i64::MAX);
    let mut indices = Vec::new();
    let mut current = start;
    while (step > 0 && current <= end) || (step < 0 && current >= end) {
        indices.push(current);
        if indices.len() as i64 > budget {
            return Err(EvalError::UnsupportedExpression {
                kind: "slice assignment target is beyond the constant-evaluation element budget"
                    .to_string(),
                span: eval.span,
            });
        }
        let Some(next) = current.checked_add(step) else {
            break;
        };
        current = next;
    }
    Ok(indices)
}

fn slice_bound(
    expr: &Expression,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<i64, EvalError> {
    let value = eval_expr_in_function(expr, env, eval)?;
    value
        .as_integer()
        .ok_or_else(|| EvalError::type_mismatch("Integer", value.type_name(), eval.span))
}

/// The extent of each dimension of an array value, outermost first.
fn array_extents(value: &Value) -> Vec<usize> {
    let mut extents = Vec::new();
    let mut current = value;
    while let Some(elements) = current.as_array() {
        extents.push(elements.len());
        let Some(first) = elements.first() else {
            break;
        };
        current = first;
    }
    extents
}

/// What one assignment-target subscript selects in its dimension.
enum Selection {
    /// A scalar index: the dimension is not part of the assigned value.
    Index(i64),
    /// The indices a range or `:` names, in order: the dimension is part of
    /// the assigned value.
    Indices(Vec<i64>),
}

/// Resolve one assignment-target subscript against a dimension of `extent`.
fn target_selection(
    subscript: &Subscript,
    extent: usize,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Selection, EvalError> {
    match subscript {
        Subscript::Index { value, .. } => Ok(Selection::Index(*value)),
        Subscript::Colon { .. } => {
            slice_indices(subscript, extent, env, eval).map(Selection::Indices)
        }
        Subscript::Expr { expr, .. } if matches!(expr.as_ref(), Expression::Range { .. }) => {
            slice_indices(subscript, extent, env, eval).map(Selection::Indices)
        }
        Subscript::Expr { expr, .. } => slice_bound(expr, env, eval).map(Selection::Index),
    }
}

/// The 0-based slot of the 1-based `index` in a dimension of `extent`.
fn selected_slot(index: i64, extent: usize, span: Span) -> Result<usize, EvalError> {
    usize::try_from(index)
        .ok()
        .filter(|slot| (1..=extent).contains(slot))
        .map(|slot| slot - 1)
        .ok_or(EvalError::IndexOutOfBounds {
            index,
            size: extent,
            span,
        })
}

/// Write `value` into the elements of `target` that `selections` name, one
/// selection per leading dimension (MLS 3.6 §10.5).
///
/// MLS 3.6 §10.6.1: an assignment requires both sides to have the same
/// number of dimensions and the same sizes, so a range selection takes an
/// array of exactly its length (an empty slice takes an empty array and nothing
/// else — `v[2:1] := {9}` is rejected, not silently written nowhere). The
/// result is built from a copy, so a refused write changes nothing.
fn write_selection(
    target: Value,
    selections: &[Selection],
    value: Value,
    span: Span,
) -> Result<Value, EvalError> {
    let Some((selection, inner)) = selections.split_first() else {
        return Ok(coerce_to_declared(&target, value));
    };
    let Value::Array(mut elements) = target else {
        return Err(EvalError::type_mismatch("Array", target.type_name(), span));
    };
    match selection {
        Selection::Index(index) => {
            let slot = selected_slot(*index, elements.len(), span)?;
            let current = std::mem::replace(&mut elements[slot], Value::Integer(0));
            elements[slot] = write_selection(current, inner, value, span)?;
        }
        Selection::Indices(indices) => {
            let Value::Array(assigned) = value else {
                return Err(EvalError::type_mismatch("Array", value.type_name(), span));
            };
            if assigned.len() != indices.len() {
                return Err(EvalError::function_error(
                    format!(
                        "slice assignment size mismatch: target names {} element(s), value has {}",
                        indices.len(),
                        assigned.len()
                    ),
                    span,
                ));
            }
            for (index, part) in indices.iter().zip(assigned) {
                let slot = selected_slot(*index, elements.len(), span)?;
                let current = std::mem::replace(&mut elements[slot], Value::Integer(0));
                elements[slot] = write_selection(current, inner, part, span)?;
            }
        }
    }
    Ok(Value::Array(elements))
}
