//! Exact lvalue and array-selection evaluation for function statements.

use rumoca_core::{ComponentReference, Expression, Span, Subscript};

use super::{EvalState, FunctionEnv, eval_expr_in_function};
use crate::constant::errors::EvalError;
use crate::constant::value::{
    Value, ValueSelection, integer_subscript_index, materialize_value_selection,
};

/// Convert a subscripted lvalue into the environment key and its exact
/// selection. Subscripts on a non-final path part are legal Modelica but are
/// outside this bounded evaluator's record/array update vocabulary, so
/// they fail closed instead of being discarded while joining identifiers.
pub(super) fn subscripted_assignment_target(
    comp: &ComponentReference,
    span: Span,
) -> Result<Option<(String, Vec<Subscript>)>, EvalError> {
    let parts = comp.parts();
    let Some(last) = parts.last() else {
        return Err(EvalError::InvalidSemanticIr {
            reason: "assignment target has no component-reference parts".to_string(),
            span,
        });
    };
    if parts[..parts.len() - 1]
        .iter()
        .any(|part| !part.subs.is_empty())
    {
        return Err(EvalError::UnsupportedExpression {
            kind: "assignment through a subscripted structured path".to_string(),
            span,
        });
    }
    if last.subs.is_empty() {
        return Ok(None);
    }
    Ok(Some((
        parts
            .iter()
            .map(|part| part.ident.as_str())
            .collect::<Vec<_>>()
            .join("."),
        last.subs.clone(),
    )))
}

/// Assign through any checked subscript selection.
///
/// Scalar indices, ranges, colons, and Integer-array selectors share this path,
/// so none can bypass the recursive shape and bounds checks in
/// `set_array_selection`.
pub(super) fn assign_array_selection(
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
    let new_value = set_array_selection(target, subscripts, value, env, eval)?;
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
            if len > eval.limits.max_iterations {
                return Err(EvalError::UnsupportedExpression {
                    kind:
                        "whole-dimension selection is beyond the constant-evaluation element budget"
                            .to_string(),
                    span: eval.span,
                });
            }
            let end = i64::try_from(len).map_err(|_| EvalError::Internal {
                message: "array extent exceeds i64 during subscript evaluation".to_string(),
            })?;
            return Ok((1..=end).collect());
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
    let start = eval_expr_in_function(start, env, eval)?;
    let end = eval_expr_in_function(end, env, eval)?;
    let step = step
        .as_ref()
        .map(|step| eval_expr_in_function(step, env, eval))
        .transpose()?;
    let values =
        crate::constant::range_eval::eval_value_range(&start, step.as_ref(), &end, eval.span)?;
    let Value::Array(values) = values else {
        return Err(EvalError::Internal {
            message: "range evaluator returned a non-array selection".to_string(),
        });
    };
    if values.len() > eval.limits.max_iterations {
        return Err(EvalError::UnsupportedExpression {
            kind: "slice assignment target is beyond the constant-evaluation element budget"
                .to_string(),
            span: eval.span,
        });
    }
    values
        .iter()
        .map(|value| integer_subscript_index(value, eval.span))
        .collect()
}

/// Write through every scalar and slice axis without flattening the shape.
pub(super) fn set_array_selection(
    target: Value,
    subscripts: &[Subscript],
    value: Value,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    let Some((subscript, remaining)) = subscripts.split_first() else {
        return Ok(coerce_to_declared(&target, value));
    };
    let mut elements = target
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", target.type_name(), eval.span))?
        .clone();
    match subscript_selection(subscript, elements.len(), env, eval)? {
        ValueSelection::Element(index) => {
            let slot = checked_subscript_slot(index, elements.len(), eval.span)?;
            elements[slot] =
                set_array_selection(elements[slot].clone(), remaining, value, env, eval)?;
        }
        ValueSelection::Slice(indices) => {
            // MLS 3.6 §10.6.1: every sliced dimension is present on the value
            // side with the same size.  Check before the local clone is
            // returned, so an empty or mismatched later dimension cannot
            // expose a partial write.
            let assigned = value
                .as_array()
                .ok_or_else(|| EvalError::type_mismatch("Array", value.type_name(), eval.span))?;
            if assigned.len() != indices.len() {
                return Err(EvalError::function_error(
                    format!(
                        "slice assignment size mismatch: target names {} element(s), value has {}",
                        indices.len(),
                        assigned.len()
                    ),
                    eval.span,
                ));
            }
            for (index, assigned_value) in indices.into_iter().zip(assigned) {
                let slot = checked_subscript_slot(index, elements.len(), eval.span)?;
                elements[slot] = set_array_selection(
                    elements[slot].clone(),
                    remaining,
                    assigned_value.clone(),
                    env,
                    eval,
                )?;
            }
        }
    }
    Ok(Value::Array(elements))
}

fn subscript_selection(
    subscript: &Subscript,
    len: usize,
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<ValueSelection, EvalError> {
    match subscript {
        Subscript::Index { value, .. } => Ok(ValueSelection::Element(*value)),
        Subscript::Colon { .. } => {
            slice_indices(subscript, len, env, eval).map(ValueSelection::Slice)
        }
        Subscript::Expr { expr, .. } if matches!(expr.as_ref(), Expression::Range { .. }) => {
            slice_indices(subscript, len, env, eval).map(ValueSelection::Slice)
        }
        Subscript::Expr { expr, .. } => selection_from_value(
            eval_expr_in_function(expr, env, eval)?,
            eval.limits.max_iterations,
            eval.span,
        ),
    }
}

fn selection_from_value(
    value: Value,
    element_budget: usize,
    span: Span,
) -> Result<ValueSelection, EvalError> {
    match value {
        Value::Array(indices) => {
            if indices.len() > element_budget {
                return Err(EvalError::UnsupportedExpression {
                    kind:
                        "Integer-vector selection is beyond the constant-evaluation element budget"
                            .to_string(),
                    span,
                });
            }
            indices
                .iter()
                .map(|index| integer_subscript_index(index, span))
                .collect::<Result<Vec<_>, _>>()
                .map(ValueSelection::Slice)
        }
        other => integer_subscript_index(&other, span).map(ValueSelection::Element),
    }
}

fn checked_subscript_slot(index: i64, len: usize, span: Span) -> Result<usize, EvalError> {
    usize::try_from(index)
        .ok()
        .filter(|slot| (1..=len).contains(slot))
        .map(|slot| slot - 1)
        .ok_or(EvalError::IndexOutOfBounds {
            index,
            size: len,
            span,
        })
}

/// MLS 3.6 §10.6.13: an Integer value written into a Real component is
/// converted to Real.
///
/// The declared element type is carried by the value already in the slot, which
/// the declaration's shaped default established. Without this,
/// `orientation[1] := 0` left an `Integer` inside a declared `Real[2]`, and the
/// mixed array compared unequal to the all-Real value the same function
/// produces on its other branch.
fn coerce_to_declared(slot: &Value, value: Value) -> Value {
    match (slot, value) {
        (Value::Real(_), Value::Integer(written)) => Value::Real(written as f64),
        (Value::Array(slots), Value::Array(written)) if slots.len() == written.len() => {
            Value::Array(
                slots
                    .iter()
                    .zip(written)
                    .map(|(slot, written)| coerce_to_declared(slot, written))
                    .collect(),
            )
        }
        (_, value) => value,
    }
}

/// Apply AST subscripts to a value.
pub(super) fn apply_subscripts_flat(
    value: Value,
    subs: &[Subscript],
    env: &FunctionEnv,
    eval: &EvalState<'_>,
) -> Result<Value, EvalError> {
    materialize_value_selection(&value, subs, eval.span, |subscript, len| {
        subscript_selection(subscript, len, env, eval)
    })
}
