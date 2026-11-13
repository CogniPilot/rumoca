//! Array builtins of MLS 3.7 §10.3 over constant values: `transpose`,
//! `identity`, `diagonal`, `outerProduct`, `cross`, `skew` and `symmetric`.
//!
//! Both call sites reach these by name through [`super::eval_builtin`]: a
//! binding expression (a `BuiltinFunction` node) and a function body, so a
//! parameter bound through one of them is evaluated the same way wherever the
//! call is written.
//!
//! Each builtin checks its argument's rank, extent and element type before it
//! computes anything. `outerProduct`, `cross`, `skew` and `symmetric` have Real
//! arguments and results (§10.3.5), so an Integer argument is converted to Real
//! (§10.6.13) before the arithmetic; `identity` is Integer, and `transpose` and
//! `diagonal` keep their argument's element type. An array value here carries
//! no shape of its own, so a result whose leading extent is zero (an
//! `identity(0)`, the `0 x 2` transpose of a `2 x 0` matrix) could not keep its
//! rank: it is refused as a form this evaluator does not represent, never
//! folded to a value of another shape. A result is charged against the
//! evaluator's element budget before it is built.

use rumoca_core::Span;

use super::errors::EvalError;
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

/// `name(args)` when `name` is one of [`ARRAY_BUILTINS`], building at most
/// `budget` result elements.
pub(super) fn eval_array_builtin(
    name: &str,
    args: &[Value],
    span: Span,
    budget: usize,
) -> Option<Result<Value, EvalError>> {
    let result = match name {
        "transpose" => transpose(args, span),
        "identity" => identity(args, span, budget),
        "diagonal" => diagonal(args, span, budget),
        "outerProduct" => outer_product(args, span, budget),
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

/// A result whose leading extent is zero, which an array value cannot give a
/// rank or trailing extents.
fn zero_extent(builtin: &str, span: Span) -> EvalError {
    EvalError::UnsupportedExpression {
        kind: format!(
            "{builtin} result with a zero leading extent, whose rank a constant array \
             value cannot carry"
        ),
        span,
    }
}

/// Refuse a result of `cells` elements (`None` when the count overflows)
/// beyond `budget` before any of it is built.
fn charge(builtin: &str, cells: Option<usize>, budget: usize, span: Span) -> Result<(), EvalError> {
    match cells {
        Some(cells) if cells <= budget => Ok(()),
        _ => Err(EvalError::UnsupportedExpression {
            kind: format!("{builtin} result beyond the constant-evaluation element budget"),
            span,
        }),
    }
}

/// The elements of a vector of non-array cells, with exactly `extent`
/// elements when one is required.
fn vector(value: &Value, extent: Option<usize>, span: Span) -> Result<&[Value], EvalError> {
    let elements = value
        .as_array()
        .filter(|elements| elements.iter().all(|element| element.as_array().is_none()))
        .ok_or_else(|| EvalError::type_mismatch("vector", value.type_name(), span))?;
    match extent {
        Some(extent) if elements.len() != extent => Err(EvalError::type_mismatch(
            format!("vector of {extent} elements"),
            format!("vector of {} elements", elements.len()),
            span,
        )),
        _ => Ok(elements),
    }
}

/// A numeric element as a Real (MLS 3.7 §10.6.13 converts an Integer).
fn real(value: &Value, span: Span) -> Result<f64, EvalError> {
    match value {
        Value::Real(value) => Ok(*value),
        Value::Integer(value) => Ok(*value as f64),
        other => Err(EvalError::type_mismatch(
            "Real or Integer",
            other.type_name(),
            span,
        )),
    }
}

/// A numeric vector as Reals.
fn real_vector(value: &Value, extent: Option<usize>, span: Span) -> Result<Vec<f64>, EvalError> {
    vector(value, extent, span)?
        .iter()
        .map(|element| real(element, span))
        .collect()
}

fn reals(values: impl IntoIterator<Item = f64>) -> Value {
    Value::Array(values.into_iter().map(Value::Real).collect())
}

/// The rows of an array of rank at least two: a non-empty array of arrays of
/// one common length.
fn rows<'a>(builtin: &str, value: &'a Value, span: Span) -> Result<Vec<&'a [Value]>, EvalError> {
    let rows = value
        .as_array()
        .ok_or_else(|| EvalError::type_mismatch("matrix", value.type_name(), span))?
        .iter()
        .map(|row| {
            row.as_array()
                .map(Vec::as_slice)
                .ok_or_else(|| EvalError::type_mismatch("matrix", value.type_name(), span))
        })
        .collect::<Result<Vec<_>, _>>()?;
    let Some(first) = rows.first() else {
        return Err(zero_extent(builtin, span));
    };
    if rows.iter().any(|row| row.len() != first.len()) {
        return Err(EvalError::type_mismatch(
            "rectangular matrix",
            "rows of different lengths",
            span,
        ));
    }
    Ok(rows)
}

/// MLS 3.7 §10.3.4: `transpose(A)` exchanges the first two dimensions; the
/// element type and any further dimensions are kept.
fn transpose(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [matrix] = arguments(args, span)?;
    let rows = rows("transpose", matrix, span)?;
    if rows[0].is_empty() {
        return Err(zero_extent("transpose", span));
    }
    Ok(Value::Array(
        (0..rows[0].len())
            .map(|column| Value::Array(rows.iter().map(|row| row[column].clone()).collect()))
            .collect(),
    ))
}

/// MLS 3.7 §10.3.3: `identity(n)` is the `n x n` Integer identity matrix of an
/// Integer `n`.
fn identity(args: &[Value], span: Span, budget: usize) -> Result<Value, EvalError> {
    let [extent] = arguments(args, span)?;
    let Value::Integer(extent) = extent else {
        return Err(EvalError::type_mismatch(
            "Integer",
            extent.type_name(),
            span,
        ));
    };
    let extent = usize::try_from(*extent)
        .map_err(|_| EvalError::type_mismatch("non-negative Integer", "negative Integer", span))?;
    if extent == 0 {
        return Err(zero_extent("identity", span));
    }
    charge("identity", extent.checked_mul(extent), budget, span)?;
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

/// MLS 3.7 §10.3.3: `diagonal(v)` is the square matrix with `v` on its
/// diagonal and zero elsewhere, in the common numeric type of `v`'s elements.
fn diagonal(args: &[Value], span: Span, budget: usize) -> Result<Value, EvalError> {
    let [values] = arguments(args, span)?;
    let values = vector(values, None, span)?;
    if values.is_empty() {
        return Err(zero_extent("diagonal", span));
    }
    let (zero, values) = if values
        .iter()
        .all(|value| matches!(value, Value::Integer(_)))
    {
        (Value::Integer(0), values.to_vec())
    } else {
        let reals = values
            .iter()
            .map(|value| real(value, span).map(Value::Real))
            .collect::<Result<Vec<_>, _>>()?;
        (Value::Real(0.0), reals)
    };
    charge(
        "diagonal",
        values.len().checked_mul(values.len()),
        budget,
        span,
    )?;
    Ok(Value::Array(
        values
            .iter()
            .enumerate()
            .map(|(row, value)| {
                Value::Array(
                    (0..values.len())
                        .map(|column| if column == row { value } else { &zero }.clone())
                        .collect(),
                )
            })
            .collect(),
    ))
}

/// MLS 3.7 §10.3.5: `outerProduct(x, y)` is the Real matrix `x[i] * y[j]`.
fn outer_product(args: &[Value], span: Span, budget: usize) -> Result<Value, EvalError> {
    let [x, y] = arguments(args, span)?;
    let (x, y) = (real_vector(x, None, span)?, real_vector(y, None, span)?);
    if x.is_empty() {
        return Err(zero_extent("outerProduct", span));
    }
    charge("outerProduct", x.len().checked_mul(y.len()), budget, span)?;
    Ok(Value::Array(
        x.iter()
            .map(|left| reals(y.iter().map(|right| left * right)))
            .collect(),
    ))
}

/// MLS 3.7 §10.3.5: `cross(x, y)` is the Real vector `{x[2]*y[3]-x[3]*y[2],
/// x[3]*y[1]-x[1]*y[3], x[1]*y[2]-x[2]*y[1]}`.
fn cross(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [x, y] = arguments(args, span)?;
    let (x, y) = (
        real_vector(x, Some(3), span)?,
        real_vector(y, Some(3), span)?,
    );
    Ok(reals([
        x[1] * y[2] - x[2] * y[1],
        x[2] * y[0] - x[0] * y[2],
        x[0] * y[1] - x[1] * y[0],
    ]))
}

/// MLS 3.7 §10.3.5: `skew(x)` is the Real matrix `{{0, -x[3], x[2]},
/// {x[3], 0, -x[1]}, {-x[2], x[1], 0}}`.
fn skew(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [x] = arguments(args, span)?;
    let x = real_vector(x, Some(3), span)?;
    Ok(Value::Array(vec![
        reals([0.0, -x[2], x[1]]),
        reals([x[2], 0.0, -x[0]]),
        reals([-x[1], x[0], 0.0]),
    ]))
}

/// MLS 3.7 §10.3.5: `symmetric(A)` of a square Real matrix keeps the diagonal
/// and the upper triangle and mirrors the upper triangle below it.
fn symmetric(args: &[Value], span: Span) -> Result<Value, EvalError> {
    let [matrix] = arguments(args, span)?;
    let rows = rows("symmetric", matrix, span)?
        .into_iter()
        .map(|row| {
            row.iter()
                .map(|element| real(element, span))
                .collect::<Result<Vec<_>, _>>()
        })
        .collect::<Result<Vec<_>, _>>()?;
    if rows[0].len() != rows.len() {
        return Err(EvalError::type_mismatch(
            "square matrix",
            format!("{} x {} matrix", rows.len(), rows[0].len()),
            span,
        ));
    }
    Ok(Value::Array(
        (0..rows.len())
            .map(|row| reals((0..rows.len()).map(|column| rows[row.min(column)][row.max(column)])))
            .collect(),
    ))
}
