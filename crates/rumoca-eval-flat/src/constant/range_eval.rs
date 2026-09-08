//! Range expressions: expansion of `start:step:end` into constant arrays.

use rumoca_core::Span;

use super::context::EvalContext;
use super::errors::EvalError;
use super::expr_eval::eval_expr_with_span;
use super::value::Value;
use super::{DEFAULT_EVAL_BUDGET, Expression};

const MAX_RANGE_ELEMENTS: usize = DEFAULT_EVAL_BUDGET - 1;

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

    if start_val.as_enum().is_some() || end_val.as_enum().is_some() {
        return eval_value_range(&start_val, None, &end_val, span);
    }
    let step_val = step
        .map(|step| eval_expr_with_span(step, ctx, span))
        .transpose()?;
    eval_value_range(&start_val, step_val.as_ref(), &end_val, span)
}

/// Evaluate already-settled range values under one shared enum/numeric
/// ownership decision for direct expressions and user-function interpretation.
pub(super) fn eval_value_range(
    start: &Value,
    step: Option<&Value>,
    end: &Value,
    span: Span,
) -> Result<Value, EvalError> {
    if matches!(start, Value::Bool(_))
        || start.as_enum().is_some()
        || matches!(end, Value::Bool(_))
        || end.as_enum().is_some()
        || step.is_some_and(|value| matches!(value, Value::Bool(_)) || value.as_enum().is_some())
    {
        return Err(EvalError::UnsupportedExpression {
            kind: "Boolean and enumeration ranges require typed index-domain metadata".to_string(),
            span,
        });
    }
    if let (Some(start), Some(end)) = (start.as_integer(), end.as_integer()) {
        let step = match step {
            Some(value) => value
                .as_integer()
                .ok_or_else(|| EvalError::type_mismatch("Integer", value.type_name(), span))?,
            None => 1,
        };
        if step == 0 {
            return Err(EvalError::range_error("step cannot be zero", span));
        }
        return collect_int_range(start, end, step, span).map(Value::Array);
    }

    let start = start
        .to_real()
        .ok_or_else(|| EvalError::type_mismatch("Real or Integer", start.type_name(), span))?;
    let end = end
        .to_real()
        .ok_or_else(|| EvalError::type_mismatch("Real or Integer", end.type_name(), span))?;
    let step = match step {
        Some(value) => value
            .to_real()
            .ok_or_else(|| EvalError::type_mismatch("Real or Integer", value.type_name(), span))?,
        None => 1.0,
    };
    collect_real_range(start, end, step, span).map(Value::Array)
}

/// Collect integer range values.
pub(super) fn collect_int_range(
    start: i64,
    end: i64,
    step: i64,
    span: Span,
) -> Result<Vec<Value>, EvalError> {
    let mut values = Vec::new();
    let mut i = start;
    if step > 0 {
        while i <= end {
            if values.len() == MAX_RANGE_ELEMENTS {
                return Err(EvalError::UnsupportedExpression {
                    kind: "integer range is beyond the constant-evaluation retained-node budget"
                        .to_string(),
                    span,
                });
            }
            values.push(Value::Integer(i));
            let Some(next) = i.checked_add(step) else {
                break;
            };
            i = next;
        }
    } else {
        while i >= end {
            if values.len() == MAX_RANGE_ELEMENTS {
                return Err(EvalError::UnsupportedExpression {
                    kind: "integer range is beyond the constant-evaluation retained-node budget"
                        .to_string(),
                    span,
                });
            }
            values.push(Value::Integer(i));
            let Some(next) = i.checked_add(step) else {
                break;
            };
            i = next;
        }
    }
    Ok(values)
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
    if count
        .checked_add(1)
        .is_none_or(|nodes| nodes > DEFAULT_EVAL_BUDGET)
    {
        return Err(EvalError::UnsupportedExpression {
            kind: "real range is beyond the constant-evaluation retained-node budget".to_string(),
            span,
        });
    }
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

    #[test]
    fn range_budget_includes_the_retained_array_container() {
        let last = MAX_RANGE_ELEMENTS as i64;
        assert_eq!(
            collect_int_range(1, last, 1, Span::DUMMY)
                .expect("the largest retained range fits")
                .len(),
            MAX_RANGE_ELEMENTS
        );
        assert!(matches!(
            collect_int_range(1, last + 1, 1, Span::DUMMY),
            Err(EvalError::UnsupportedExpression { .. })
        ));
        assert!(matches!(
            collect_real_range(1.0, last as f64 + 1.0, 1.0, Span::DUMMY),
            Err(EvalError::UnsupportedExpression { .. })
        ));
    }
}
