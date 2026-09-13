//! MLS §10.5 indexing: scalar indices remove axes; vectors and `:` retain them.

#[cfg(test)]
mod tests;

use rumoca_core::{Expression, Span, Subscript};

use super::{EvalError, Value};

enum EvaluatedSubscript {
    Scalar(i64),
    Vector(Vec<i64>),
    Whole,
}

/// Evaluate each index once in its caller's environment, then select the
/// Cartesian product of retained axes in source order.
pub(super) fn apply_subscripts(
    value: Value,
    subscripts: &[Subscript],
    mut evaluate: impl FnMut(&Expression) -> Result<Value, EvalError>,
    span: Span,
) -> Result<Value, EvalError> {
    if subscripts.is_empty() {
        return Ok(value);
    }
    let indices = subscripts
        .iter()
        .map(|subscript| match subscript {
            Subscript::Index { value, .. } => Ok(EvaluatedSubscript::Scalar(*value)),
            Subscript::Colon { .. } => Ok(EvaluatedSubscript::Whole),
            Subscript::Expr { expr, .. } => match evaluate(expr)? {
                Value::Array(values) => values
                    .iter()
                    .map(|value| integer_index(value, span))
                    .collect::<Result<_, _>>()
                    .map(EvaluatedSubscript::Vector),
                scalar => integer_index(&scalar, span).map(EvaluatedSubscript::Scalar),
            },
        })
        .collect::<Result<Vec<_>, _>>()?;
    select(&value, &indices, span)
}

fn integer_index(value: &Value, span: Span) -> Result<i64, EvalError> {
    value
        .as_integer()
        .ok_or_else(|| EvalError::type_mismatch("Integer", value.type_name(), span))
}

fn element(values: &[Value], index: i64, span: Span) -> Result<&Value, EvalError> {
    usize::try_from(index)
        .ok()
        .and_then(|index| index.checked_sub(1))
        .and_then(|index| values.get(index))
        .ok_or(EvalError::IndexOutOfBounds {
            index,
            size: values.len(),
            span,
        })
}

fn select(value: &Value, indices: &[EvaluatedSubscript], span: Span) -> Result<Value, EvalError> {
    let Some((first, rest)) = indices.split_first() else {
        return Ok(value.clone());
    };
    let values = value
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("Array", value.type_name(), span))?;
    match first {
        EvaluatedSubscript::Scalar(index) => select(element(values, *index, span)?, rest, span),
        EvaluatedSubscript::Vector(indices) => indices
            .iter()
            .map(|index| select(element(values, *index, span)?, rest, span))
            .collect::<Result<_, _>>()
            .map(Value::Array),
        EvaluatedSubscript::Whole => values
            .iter()
            .map(|value| select(value, rest, span))
            .collect::<Result<_, _>>()
            .map(Value::Array),
    }
}
