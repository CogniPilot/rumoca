//! Range expressions: expansion of `start:step:end` into constant arrays.

use rumoca_core::Span;

use super::Expression;
use super::context::EvalContext;
use super::errors::EvalError;
use super::expr_eval::eval_expr_with_span;
use super::value::Value;

/// Evaluate a range expression to an array.
pub(super) fn eval_range(
    start: &Expression,
    step: Option<&Expression>,
    end: &Expression,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let start_val = eval_expr_with_span(start, ctx, span)?;
    let end_val = eval_expr_with_span(end, ctx, span)?;

    // Determine if we have integer or real range
    match (start_val.as_integer(), end_val.as_integer()) {
        (Some(s), Some(e)) => eval_integer_range(s, e, step, ctx, span),
        _ => eval_real_range(&start_val, &end_val, step, ctx, span),
    }
}

/// Evaluate an integer range.
fn eval_integer_range(
    s: i64,
    e: i64,
    step: Option<&Expression>,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let step_int = match step {
        Some(step_expr) => {
            let step_val = eval_expr_with_span(step_expr, ctx, span)?;
            step_val
                .as_integer()
                .ok_or_else(|| EvalError::type_mismatch("Integer", step_val.type_name(), span))?
        }
        None => 1,
    };

    if step_int == 0 {
        return Err(EvalError::range_error("step cannot be zero", span));
    }

    let values = collect_int_range(s, e, step_int);
    Ok(Value::Array(values))
}

/// Collect integer range values.
pub(super) fn collect_int_range(start: i64, end: i64, step: i64) -> Vec<Value> {
    let mut values = Vec::new();
    let mut i = start;
    if step > 0 {
        while i <= end {
            values.push(Value::Integer(i));
            let Some(next) = i.checked_add(step) else {
                break;
            };
            i = next;
        }
    } else {
        while i >= end {
            values.push(Value::Integer(i));
            let Some(next) = i.checked_add(step) else {
                break;
            };
            i = next;
        }
    }
    values
}

/// Evaluate a real range.
fn eval_real_range(
    start_val: &Value,
    end_val: &Value,
    step: Option<&Expression>,
    ctx: &EvalContext,
    span: Span,
) -> Result<Value, EvalError> {
    let s = start_val
        .to_real()
        .ok_or_else(|| EvalError::type_mismatch("Real or Integer", start_val.type_name(), span))?;
    let e = end_val
        .to_real()
        .ok_or_else(|| EvalError::type_mismatch("Real or Integer", end_val.type_name(), span))?;

    let step_f = match step {
        Some(step_expr) => {
            let step_val = eval_expr_with_span(step_expr, ctx, span)?;
            step_val.to_real().ok_or_else(|| {
                EvalError::type_mismatch("Real or Integer", step_val.type_name(), span)
            })?
        }
        None => 1.0,
    };

    if step_f == 0.0 {
        return Err(EvalError::range_error("step cannot be zero", span));
    }
    if !s.is_finite() || !e.is_finite() || !step_f.is_finite() {
        return Err(EvalError::range_error(
            "range bounds and step must be finite",
            span,
        ));
    }

    let values = collect_real_range(s, e, step_f, span)?;
    Ok(Value::Array(values))
}

/// Collect real range values.
pub(super) fn collect_real_range(
    start: f64,
    end: f64,
    step: f64,
    span: Span,
) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::new();
    let mut v = start;
    if step > 0.0 {
        while v <= end + f64::EPSILON {
            values.push(Value::Real(v));
            if v >= end {
                break;
            }
            let next = v + step;
            if next == v {
                return Err(EvalError::range_error(
                    "range step does not advance at this magnitude",
                    span,
                ));
            }
            v = next;
        }
    } else {
        while v >= end - f64::EPSILON {
            values.push(Value::Real(v));
            if v <= end {
                break;
            }
            let next = v + step;
            if next == v {
                return Err(EvalError::range_error(
                    "range step does not advance at this magnitude",
                    span,
                ));
            }
            v = next;
        }
    }
    Ok(values)
}
