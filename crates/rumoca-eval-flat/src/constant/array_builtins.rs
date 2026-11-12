//! Array builtins of MLS 3.7 §10.3 over constant values: `transpose`,
//! `identity`, `diagonal`, `outerProduct`, `cross`, `skew` and `symmetric`.
//!
//! Both call sites reach these by name through [`super::eval_builtin`]: a
//! binding expression (a `BuiltinFunction` node) and a function body, so a
//! parameter bound through one of them is evaluated the same way wherever the
//! call is written. Element arithmetic uses the evaluator's own operators, so
//! Integer elements stay Integer and overflow is reported, not wrapped.

use rumoca_core::Span;

use super::errors::EvalError;
use super::operators::{eval_mul, eval_negate, eval_sub};
use super::value::Value;

/// The names this module evaluates.
pub(super) const ARRAY_BUILTINS: [&str; 7] = [
    "transpose",
    "identity",
    "diagonal",
    "outerProduct",
    "cross",
    "skew",
    "symmetric",
];

/// `name(args)` when `name` is one of [`ARRAY_BUILTINS`].
pub(super) fn eval_array_builtin(
    name: &str,
    args: &[Value],
    span: Span,
) -> Option<Result<Value, EvalError>> {
    let result = match name {
        "transpose" => transpose(args, span),
        "identity" => identity(args, span),
        "diagonal" => diagonal(args, span),
        "outerProduct" => outer_product(args, span),
        "cross" => cross(args, span),
        "skew" => skew(args, span),
        "symmetric" => symmetric(args, span),
        _ => return None,
    };
    Some(result)
}

fn arguments<const N: usize>(args: &[Value], span: Span) -> Result<&[Value; N], EvalError> {
    args.try_into().map_err(|_| EvalError::WrongArgCount {
        expected: N,
        actual: args.len(),
        span,
    })
}

/// The elements of a vector of scalars, with exactly `extent` elements when
/// one is required.
fn vector(value: &Value, extent: Option<usize>, span: Span) -> Result<&[Value], EvalError> {
    let elements = value
        .as_array()
        .filter(|elements| elements.iter().all(|element| element.as_array().is_none()))
        .ok_or_else(|| EvalError::type_mismatch("vector", value.type_name(), span))?;
    match extent {
        Some(extent) if elements.len() != extent => Err(EvalError::function_error(
            format!(
                "expected a vector of {extent} elements, got {}",
                elements.len()
            ),
            span,
        )),
        _ => Ok(elements),
    }
}

/// The rows of a matrix: a non-empty array of rows of one common length.
fn rows(value: &Value, span: Span) -> Result<Vec<&[Value]>, EvalError> {
    let rows = value
        .as_array()
        .filter(|rows| !rows.is_empty())
        .ok_or_else(|| EvalError::type_mismatch("matrix", value.type_name(), span))?
        .iter()
        .map(|row| {
            row.as_array()
                .map(Vec::as_slice)
                .ok_or_else(|| EvalError::type_mismatch("matrix", value.type_name(), span))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let columns = rows[0].len();
    if rows.iter().any(|row| row.len() != columns) {
        return Err(EvalError::function_error(
            "matrix rows differ in length",
            span,
        ));
    }
    Ok(rows)
}

/// The zero of a numeric element's type: Integer for an Integer, else Real.
fn zero_like(element: &Value, span: Span) -> Result<Value, EvalError> {
    match element {
        Value::Integer(_) => Ok(Value::Integer(0)),
        Value::Real(_) => Ok(Value::Real(0.0)),
        other => Err(EvalError::type_mismatch(
            "Real or Integer",
            other.type_name(),
            span,
        )),
    }
}

/// MLS 3.7 §10.3.6: `transpose(A)` exchanges the first two dimensions.
fn transpose(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [matrix] = arguments(args, span)?;
    let rows = rows(matrix, span)?;
    Ok(Value::Array(
        (0..rows[0].len())
            .map(|column| Value::Array(rows.iter().map(|row| row[column].clone()).collect()))
            .collect(),
    ))
}

/// MLS 3.7 §10.3.4: `identity(n)` is the `n x n` Integer identity matrix.
fn identity(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [extent] = arguments(args, span)?;
    let extent = extent
        .as_integer()
        .and_then(|extent| usize::try_from(extent).ok())
        .ok_or_else(|| {
            EvalError::type_mismatch("non-negative Integer", extent.type_name(), span)
        })?;
    Ok(Value::Array(
        (0..extent)
            .map(|row| {
                Value::Array(
                    (0..extent)
                        .map(|column| Value::Integer(i64::from(row == column)))
                        .collect(),
                )
            })
            .collect(),
    ))
}

/// MLS 3.7 §10.3.4: `diagonal(v)` is the square matrix with `v` on its
/// diagonal and zero elsewhere.
fn diagonal(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [values] = arguments(args, span)?;
    let values = vector(values, None, span)?;
    let zeros = values
        .iter()
        .map(|value| zero_like(value, span))
        .collect::<Result<Vec<_>, _>>()?;
    let zero = zeros
        .iter()
        .find(|zero| matches!(zero, Value::Real(_)))
        .or_else(|| zeros.first())
        .cloned();
    Ok(Value::Array(
        values
            .iter()
            .enumerate()
            .map(|(row, value)| {
                Value::Array(
                    (0..values.len())
                        .map(|column| match &zero {
                            Some(zero) if column != row => zero.clone(),
                            _ => value.clone(),
                        })
                        .collect(),
                )
            })
            .collect(),
    ))
}

/// MLS 3.7 §10.3.6: `outerProduct(x, y)` is the matrix `x[i] * y[j]`.
fn outer_product(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [x, y] = arguments(args, span)?;
    let (x, y) = (vector(x, None, span)?, vector(y, None, span)?);
    x.iter()
        .map(|left| {
            y.iter()
                .map(|right| eval_mul(left, right, span))
                .collect::<Result<Vec<_>, _>>()
                .map(Value::Array)
        })
        .collect::<Result<Vec<_>, _>>()
        .map(Value::Array)
}

/// `a * b - c * d`, the term of a cross product.
fn cross_term(a: &Value, b: &Value, c: &Value, d: &Value, span: Span) -> Result<Value, EvalError> {
    eval_sub(&eval_mul(a, b, span)?, &eval_mul(c, d, span)?, span)
}

/// MLS 3.7 §10.3.6: `cross(x, y)` is `{x[2]*y[3]-x[3]*y[2],
/// x[3]*y[1]-x[1]*y[3], x[1]*y[2]-x[2]*y[1]}`.
fn cross(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [x, y] = arguments(args, span)?;
    let (x, y) = (vector(x, Some(3), span)?, vector(y, Some(3), span)?);
    Ok(Value::Array(vec![
        cross_term(&x[1], &y[2], &x[2], &y[1], span)?,
        cross_term(&x[2], &y[0], &x[0], &y[2], span)?,
        cross_term(&x[0], &y[1], &x[1], &y[0], span)?,
    ]))
}

/// MLS 3.7 §10.3.6: `skew(x)` is `{{0, -x[3], x[2]}, {x[3], 0, -x[1]},
/// {-x[2], x[1], 0}}`.
fn skew(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [x] = arguments(args, span)?;
    let x = vector(x, Some(3), span)?;
    let zero = zero_like(&x[0], span)?;
    let negated = x
        .iter()
        .map(|value| eval_negate(value, span))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Value::Array(vec![
        Value::Array(vec![zero.clone(), negated[2].clone(), x[1].clone()]),
        Value::Array(vec![x[2].clone(), zero.clone(), negated[0].clone()]),
        Value::Array(vec![negated[1].clone(), x[0].clone(), zero]),
    ]))
}

/// MLS 3.7 §10.3.6: `symmetric(A)` keeps the diagonal and the upper triangle
/// of the square matrix `A` and mirrors the upper triangle below it.
fn symmetric(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [matrix] = arguments(args, span)?;
    let rows = rows(matrix, span)?;
    if rows[0].len() != rows.len() {
        return Err(EvalError::function_error(
            "symmetric(A) requires a square matrix",
            span,
        ));
    }
    Ok(Value::Array(
        (0..rows.len())
            .map(|row| {
                Value::Array(
                    (0..rows.len())
                        .map(|column| rows[row.min(column)][row.max(column)].clone())
                        .collect(),
                )
            })
            .collect(),
    ))
}
