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
    if !start.is_finite() || !end.is_finite() || !step.is_finite() {
        return Err(EvalError::range_error(
            "range bounds and step must be finite",
            span,
        ));
    }
    if step == 0.0 {
        return Err(EvalError::range_error("step cannot be zero", span));
    }

    // MLS §10.4.3 defines the final index as floor((end - start) / step).
    // Compute cardinality in that quotient space instead of comparing values
    // against an endpoint-scaled epsilon: endpoint scaling changes the
    // mathematical range at large offsets. Snap only a quotient that is
    // within one representable ULP of an integer, matching OMC's treatment of
    // decimal cases such as 0:0.1:0.3.
    if (step > 0.0 && start > end) || (step < 0.0 && start < end) {
        return Ok(Vec::new());
    }
    let quotient = (end - start) / step;
    if !quotient.is_finite() {
        return Err(EvalError::range_error("range has too many elements", span));
    }
    let nearest = quotient.round();
    let next = f64::from_bits(quotient.to_bits() + 1);
    let quotient_for_floor = if (quotient - nearest).abs() <= next - quotient {
        nearest
    } else {
        quotient
    };
    let last_index = quotient_for_floor.floor();
    if last_index < 0.0 || last_index >= usize::MAX as f64 {
        return Err(EvalError::range_error("range has too many elements", span));
    }
    let count = (last_index as usize)
        .checked_add(1)
        .ok_or_else(|| EvalError::range_error("range has too many elements", span))?;
    let mut values = Vec::new();
    values
        .try_reserve_exact(count)
        .map_err(|_| EvalError::range_error("range has too many elements", span))?;
    for index in 0..count {
        values.push(Value::Real(start + (index as f64) * step));
    }
    Ok(values)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn real_range_uses_indexed_values_without_accumulation_drift() {
        let values = collect_real_range(0.0, 0.3, 0.1, Span::DUMMY).unwrap();
        assert_eq!(values.len(), 4);
        assert_eq!(values[3], Value::Real(0.30000000000000004));
    }

    #[test]
    fn real_range_cardinality_tolerance_is_in_quotient_space() {
        let start = 1.0e15;
        let values = collect_real_range(start, start + 0.5, 1.0, Span::DUMMY).unwrap();
        assert_eq!(values, vec![Value::Real(start)]);
    }

    #[test]
    fn descending_real_range_uses_the_same_quotient_rule() {
        let values = collect_real_range(0.3, 0.0, -0.1, Span::DUMMY).unwrap();
        assert_eq!(values.len(), 4);
        assert_eq!(values[3], Value::Real(-5.551115123125783e-17));
    }

    #[test]
    fn real_range_rejects_non_finite_inputs() {
        let result = collect_real_range(f64::INFINITY, 1.0, 1.0, Span::DUMMY);
        assert!(result.is_err());
    }

    #[test]
    fn real_range_cardinality_does_not_require_distinct_binary64_values() {
        let start = 1.0e15;
        let values = collect_real_range(start, start + 0.5, 0.01, Span::DUMMY).unwrap();
        assert_eq!(values.len(), 51);
        assert_eq!(values[0], values[1]);
    }
}
