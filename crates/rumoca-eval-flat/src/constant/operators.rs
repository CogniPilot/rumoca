//! Scalar and element-wise operator semantics for binary and unary operations.

use rumoca_core::Span;

use super::errors::EvalError;
use super::matrix_ops::eval_matrix_mul;
use super::value::Value;
use super::{OpBinary, OpUnary};

/// Evaluate a binary operation.
pub(super) fn eval_binary_op(
    op: &OpBinary,
    lhs: &Value,
    rhs: &Value,
    span: Span,
) -> Result<Value, EvalError> {
    if let Some(refusal) = record_operand_refusal(op, lhs, rhs, span) {
        return Err(refusal);
    }
    match op {
        OpBinary::Add | OpBinary::AddElem => eval_add(lhs, rhs, span),
        OpBinary::Sub | OpBinary::SubElem => eval_sub(lhs, rhs, span),
        // MLS array semantics: `*` is linear algebra multiply; `.*` is element-wise.
        OpBinary::Mul => eval_mul(lhs, rhs, span),
        OpBinary::MulElem => eval_mul_elem(lhs, rhs, span),
        OpBinary::Div | OpBinary::DivElem => eval_div(lhs, rhs, span),
        OpBinary::Exp | OpBinary::ExpElem => eval_exp(lhs, rhs, span),
        OpBinary::Eq => eval_eq(lhs, rhs),
        OpBinary::Neq => eval_neq(lhs, rhs),
        OpBinary::Lt => eval_lt(lhs, rhs, span),
        OpBinary::Le => eval_le(lhs, rhs, span),
        OpBinary::Gt => eval_gt(lhs, rhs, span),
        OpBinary::Ge => eval_ge(lhs, rhs, span),
        OpBinary::And => eval_and(lhs, rhs, span),
        OpBinary::Or => eval_or(lhs, rhs, span),
        OpBinary::Empty | OpBinary::Assign => Err(EvalError::UnsupportedExpression {
            kind: format!("binary operator: {:?}", op),
            span,
        }),
    }
}

/// Why an arithmetic or ordering operator over a record operand has no rule
/// here, rather than proving the model wrong.
///
/// MLS 3.6 §14 defines `+`, `-`, `*`, `/`, `^` and the ordering operators over
/// an operator record only through the operator functions the record declares,
/// and resolving that overload is not something this evaluator does — Flat
/// still carries the written `Binary` node. So a record operand means "no
/// folding rule reaches here", which is an unimplemented form, not a defect in
/// the model. Reporting it as a type mismatch would blame the model for an
/// overload the compiler never resolved.
///
/// `==`/`!=` are excluded because their structural comparison is already
/// defined on every value this evaluator builds.
fn record_operand_refusal(
    op: &OpBinary,
    lhs: &Value,
    rhs: &Value,
    span: Span,
) -> Option<EvalError> {
    let overloadable = matches!(
        op,
        OpBinary::Add
            | OpBinary::AddElem
            | OpBinary::Sub
            | OpBinary::SubElem
            | OpBinary::Mul
            | OpBinary::MulElem
            | OpBinary::Div
            | OpBinary::DivElem
            | OpBinary::Exp
            | OpBinary::ExpElem
            | OpBinary::Lt
            | OpBinary::Le
            | OpBinary::Gt
            | OpBinary::Ge
    );
    let has_record = matches!(lhs, Value::Record(_)) || matches!(rhs, Value::Record(_));
    (overloadable && has_record).then(|| EvalError::UnsupportedExpression {
        kind: format!("overloaded operator `{op:?}` on an operator-record operand (MLS §14)"),
        span,
    })
}

/// Evaluate a unary operation.
pub(super) fn eval_unary_op(op: &OpUnary, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    if matches!(rhs, Value::Record(_))
        && matches!(
            op,
            OpUnary::Minus | OpUnary::DotMinus | OpUnary::Plus | OpUnary::DotPlus
        )
    {
        return Err(EvalError::UnsupportedExpression {
            kind: format!("overloaded operator `{op:?}` on an operator-record operand (MLS §14)"),
            span,
        });
    }
    match op {
        OpUnary::Minus | OpUnary::DotMinus => eval_negate(rhs, span),
        OpUnary::Plus | OpUnary::DotPlus => Ok(rhs.clone()),
        OpUnary::Not => eval_not(rhs, span),
        OpUnary::Empty => Ok(rhs.clone()),
    }
}

// Arithmetic operations

fn integer_overflow_error(op: &str, span: Span) -> EvalError {
    EvalError::function_error(
        format!("compile-time integer overflow while evaluating {op}"),
        span,
    )
}

pub(super) fn eval_add(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => a
            .checked_add(*b)
            .map(Value::Integer)
            .ok_or_else(|| integer_overflow_error("integer addition", span)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a + b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 + b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a + *b as f64)),
        (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
        (Value::Array(a), Value::Array(b)) => {
            if a.len() != b.len() {
                return Err(EvalError::function_error(
                    format!("array size mismatch: {} vs {}", a.len(), b.len()),
                    span,
                ));
            }
            let result: Vec<Value> = a
                .iter()
                .zip(b.iter())
                .map(|(x, y)| eval_add(x, y, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        _ => Err(EvalError::type_mismatch(
            "numeric or array",
            format!("{} + {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_sub(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => a
            .checked_sub(*b)
            .map(Value::Integer)
            .ok_or_else(|| integer_overflow_error("integer subtraction", span)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a - b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 - b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a - *b as f64)),
        (Value::Array(a), Value::Array(b)) => {
            if a.len() != b.len() {
                return Err(EvalError::function_error(
                    format!("array size mismatch: {} vs {}", a.len(), b.len()),
                    span,
                ));
            }
            let result: Vec<Value> = a
                .iter()
                .zip(b.iter())
                .map(|(x, y)| eval_sub(x, y, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        _ => Err(EvalError::type_mismatch(
            "numeric or array",
            format!("{} - {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_mul(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => a
            .checked_mul(*b)
            .map(Value::Integer)
            .ok_or_else(|| integer_overflow_error("integer multiplication", span)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a * b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 * b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a * *b as f64)),
        // Scalar-array scaling is shared between `*` and `.*`.
        (Value::Integer(_) | Value::Real(_), Value::Array(_))
        | (Value::Array(_), Value::Integer(_) | Value::Real(_)) => eval_mul_elem(lhs, rhs, span),
        // Array-array `*` follows matrix/vector linear algebra semantics.
        (Value::Array(_), Value::Array(_)) => eval_matrix_mul(lhs, rhs, span),
        _ => Err(EvalError::type_mismatch(
            "numeric, vector, or matrix",
            format!("{} * {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_mul_elem(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => a
            .checked_mul(*b)
            .map(Value::Integer)
            .ok_or_else(|| integer_overflow_error("integer multiplication", span)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a * b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real(*a as f64 * b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a * *b as f64)),
        // Scalar * Array
        (Value::Integer(a), Value::Array(arr)) | (Value::Array(arr), Value::Integer(a)) => {
            let result: Vec<Value> = arr
                .iter()
                .map(|v| eval_mul_elem(&Value::Integer(*a), v, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        (Value::Real(a), Value::Array(arr)) | (Value::Array(arr), Value::Real(a)) => {
            let result: Vec<Value> = arr
                .iter()
                .map(|v| eval_mul_elem(&Value::Real(*a), v, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        // Element-wise array multiplication
        (Value::Array(a), Value::Array(b)) => {
            if a.len() != b.len() {
                return Err(EvalError::function_error(
                    format!("array size mismatch: {} vs {}", a.len(), b.len()),
                    span,
                ));
            }
            let result: Vec<Value> = a
                .iter()
                .zip(b.iter())
                .map(|(x, y)| eval_mul_elem(x, y, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        _ => Err(EvalError::type_mismatch(
            "numeric or array",
            format!("{} .* {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_div(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => {
            if *b == 0 {
                return Err(EvalError::DivisionByZero { span });
            }
            // Integer division in Modelica produces Real
            Ok(Value::Real(*a as f64 / *b as f64))
        }
        (Value::Real(a), Value::Real(b)) => {
            if *b == 0.0 {
                return Err(EvalError::DivisionByZero { span });
            }
            Ok(Value::Real(a / b))
        }
        (Value::Integer(a), Value::Real(b)) => {
            if *b == 0.0 {
                return Err(EvalError::DivisionByZero { span });
            }
            Ok(Value::Real(*a as f64 / b))
        }
        (Value::Real(a), Value::Integer(b)) => {
            if *b == 0 {
                return Err(EvalError::DivisionByZero { span });
            }
            Ok(Value::Real(a / *b as f64))
        }
        (Value::Array(a), Value::Array(b)) => {
            if a.len() != b.len() {
                return Err(EvalError::function_error(
                    format!("array size mismatch: {} vs {}", a.len(), b.len()),
                    span,
                ));
            }
            let result: Vec<Value> = a
                .iter()
                .zip(b.iter())
                .map(|(x, y)| eval_div(x, y, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        // MLS 3.6 §10.6.5 "Division by Numeric Scalars": `a / s` divides every
        // element of the numeric array `a` by the numeric scalar `s`. The
        // mirrored form `s / a` has no MLS meaning and stays rejected.
        (Value::Array(a), Value::Integer(_) | Value::Real(_)) => {
            let result: Vec<Value> = a
                .iter()
                .map(|x| eval_div(x, rhs, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        _ => Err(EvalError::type_mismatch(
            "numeric",
            format!("{} / {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_exp(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => {
            if *b >= 0 {
                a.checked_pow(*b as u32)
                    .map(Value::Integer)
                    .ok_or_else(|| integer_overflow_error("integer exponentiation", span))
            } else {
                Ok(Value::Real((*a as f64).powf(*b as f64)))
            }
        }
        (Value::Real(a), Value::Real(b)) => Ok(Value::Real(a.powf(*b))),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Real((*a as f64).powf(*b))),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Real(a.powi(*b as i32))),
        _ => Err(EvalError::type_mismatch(
            "numeric",
            format!("{} ^ {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_negate(v: &Value, span: Span) -> Result<Value, EvalError> {
    match v {
        Value::Integer(x) => x
            .checked_neg()
            .map(Value::Integer)
            .ok_or_else(|| integer_overflow_error("integer negation", span)),
        Value::Real(x) => Ok(Value::Real(-x)),
        Value::Array(arr) => {
            let result: Vec<Value> = arr
                .iter()
                .map(|x| eval_negate(x, span))
                .collect::<Result<_, _>>()?;
            Ok(Value::Array(result))
        }
        _ => Err(EvalError::type_mismatch("numeric", v.type_name(), span)),
    }
}

// Comparison operations

fn eval_eq(lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    // Handle mixed Integer/Real comparisons
    match (lhs, rhs) {
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) == *b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a == (*b as f64))),
        _ => Ok(Value::Bool(lhs == rhs)),
    }
}

fn eval_neq(lhs: &Value, rhs: &Value) -> Result<Value, EvalError> {
    // Handle mixed Integer/Real comparisons
    match (lhs, rhs) {
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) != *b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a != (*b as f64))),
        _ => Ok(Value::Bool(lhs != rhs)),
    }
}

fn eval_lt(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a < b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a < b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) < *b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a < *b as f64)),
        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a < b)),
        _ => Err(EvalError::type_mismatch(
            "comparable",
            format!("{} < {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_le(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a <= b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a <= b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) <= *b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a <= *b as f64)),
        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a <= b)),
        _ => Err(EvalError::type_mismatch(
            "comparable",
            format!("{} <= {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_gt(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a > b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a > b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) > *b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a > *b as f64)),
        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a > b)),
        _ => Err(EvalError::type_mismatch(
            "comparable",
            format!("{} > {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

fn eval_ge(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Bool(a >= b)),
        (Value::Real(a), Value::Real(b)) => Ok(Value::Bool(a >= b)),
        (Value::Integer(a), Value::Real(b)) => Ok(Value::Bool((*a as f64) >= *b)),
        (Value::Real(a), Value::Integer(b)) => Ok(Value::Bool(*a >= *b as f64)),
        (Value::String(a), Value::String(b)) => Ok(Value::Bool(a >= b)),
        _ => Err(EvalError::type_mismatch(
            "comparable",
            format!("{} >= {}", lhs.type_name(), rhs.type_name()),
            span,
        )),
    }
}

// Logical operations

fn eval_and(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    let a = lhs
        .as_bool()
        .ok_or_else(|| EvalError::type_mismatch("Boolean", lhs.type_name(), span))?;
    let b = rhs
        .as_bool()
        .ok_or_else(|| EvalError::type_mismatch("Boolean", rhs.type_name(), span))?;
    Ok(Value::Bool(a && b))
}

fn eval_or(lhs: &Value, rhs: &Value, span: Span) -> Result<Value, EvalError> {
    let a = lhs
        .as_bool()
        .ok_or_else(|| EvalError::type_mismatch("Boolean", lhs.type_name(), span))?;
    let b = rhs
        .as_bool()
        .ok_or_else(|| EvalError::type_mismatch("Boolean", rhs.type_name(), span))?;
    Ok(Value::Bool(a || b))
}

fn eval_not(v: &Value, span: Span) -> Result<Value, EvalError> {
    let b = v
        .as_bool()
        .ok_or_else(|| EvalError::type_mismatch("Boolean", v.type_name(), span))?;
    Ok(Value::Bool(!b))
}
