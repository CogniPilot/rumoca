//! Value construction for the explicit source array operators.

use rumoca_core::{ArrayConstructor, Span};

use super::{EvalError, Value};

pub(super) fn construct(
    values: Vec<Value>,
    kind: ArrayConstructor,
    span: Span,
) -> Result<Value, EvalError> {
    let Some(axis) = kind.concatenation_axis() else {
        return Ok(Value::Array(values));
    };
    let shapes = values
        .iter()
        .map(|value| dimensions(value, span))
        .collect::<Result<Vec<_>, _>>()?;
    let result = kind.checked_dimensions(&shapes).ok_or_else(|| {
        EvalError::function_error("incompatible promoted concatenation dimensions", span)
    })?;
    let mut promoted = values
        .into_iter()
        .zip(shapes)
        .map(|(value, shape)| append_singleton_dimensions(value, result.len() - shape.len()));
    let mut value = promoted.next().ok_or_else(|| {
        EvalError::function_error("empty bracket concatenation is undefined", span)
    })?;
    for operand in promoted {
        concatenate(&mut value, operand, axis, span)?;
    }
    Ok(value)
}

fn dimensions(value: &Value, span: Span) -> Result<Vec<usize>, EvalError> {
    let Value::Array(elements) = value else {
        return Ok(Vec::new());
    };
    let shapes = elements
        .iter()
        .map(|value| dimensions(value, span))
        .collect::<Result<Vec<_>, _>>()?;
    ArrayConstructor::Array
        .checked_dimensions(&shapes)
        .ok_or_else(|| EvalError::function_error("array operand is not rectangular", span))
}

fn append_singleton_dimensions(value: Value, extra: usize) -> Value {
    if extra == 0 {
        return value;
    }
    match value {
        Value::Array(values) => Value::Array(
            values
                .into_iter()
                .map(|value| append_singleton_dimensions(value, extra))
                .collect(),
        ),
        scalar => (0..extra).fold(scalar, |value, _| Value::Array(vec![value])),
    }
}

fn concatenate(
    target: &mut Value,
    operand: Value,
    axis: usize,
    span: Span,
) -> Result<(), EvalError> {
    let (Value::Array(target), Value::Array(operand)) = (target, operand) else {
        return Err(EvalError::function_error(
            "concatenation operand rank mismatch",
            span,
        ));
    };
    if axis == 0 {
        target.extend(operand);
        return Ok(());
    }
    if target.len() != operand.len() {
        return Err(EvalError::function_error(
            "concatenation operand extent mismatch",
            span,
        ));
    }
    for (target, operand) in target.iter_mut().zip(operand) {
        concatenate(target, operand, axis - 1, span)?;
    }
    Ok(())
}
