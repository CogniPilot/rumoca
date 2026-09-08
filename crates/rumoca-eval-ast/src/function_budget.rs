use std::sync::atomic::{AtomicUsize, Ordering};

/// Maximum loop work admitted by one outermost AST function evaluation.
///
/// Nested loops and nested/recursive function calls share the same budget. This
/// prevents individually bounded loops from composing into unbounded compile-
/// time work.
pub(crate) const MAX_AST_FUNCTION_EVAL_WORK: usize = 4096;

pub(crate) struct AstFunctionWorkBudget {
    remaining: AtomicUsize,
}

impl AstFunctionWorkBudget {
    pub(crate) fn new() -> Self {
        Self {
            remaining: AtomicUsize::new(MAX_AST_FUNCTION_EVAL_WORK),
        }
    }

    pub(crate) fn try_spend(&self, amount: usize) -> bool {
        self.remaining
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |remaining| {
                remaining.checked_sub(amount)
            })
            .is_ok()
    }
}

/// Materialize an integer range only after its full cardinality fits the
/// call-owned work budget. All cardinality arithmetic is widened so extreme
/// i64 endpoints and `i64::MIN` steps cannot overflow before refusal.
pub(crate) fn materialize_integer_range(
    start: i64,
    step: i64,
    end: i64,
    budget: &AstFunctionWorkBudget,
) -> Option<Vec<i64>> {
    let count = integer_range_cardinality(start, step, end)?;
    if !budget.try_spend(count) {
        return None;
    }

    let mut values = Vec::with_capacity(count);
    let start = i128::from(start);
    let step = i128::from(step);
    for index in 0..count {
        let offset = step.checked_mul(i128::try_from(index).ok()?)?;
        values.push(i64::try_from(start.checked_add(offset)?).ok()?);
    }
    Some(values)
}

fn integer_range_cardinality(start: i64, step: i64, end: i64) -> Option<usize> {
    if step == 0 {
        return None;
    }
    let distance = if step > 0 {
        if end < start {
            return Some(0);
        }
        i128::from(end) - i128::from(start)
    } else {
        if end > start {
            return Some(0);
        }
        i128::from(start) - i128::from(end)
    };
    let magnitude = i128::from(step).abs();
    usize::try_from(distance / magnitude + 1).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extreme_ranges_are_counted_without_overflow() {
        let budget = AstFunctionWorkBudget::new();
        assert_eq!(
            materialize_integer_range(i64::MIN, i64::MAX, i64::MAX, &budget),
            Some(vec![i64::MIN, -1, i64::MAX - 1])
        );

        let budget = AstFunctionWorkBudget::new();
        assert_eq!(
            materialize_integer_range(i64::MAX, i64::MIN, i64::MIN, &budget),
            Some(vec![i64::MAX, -1])
        );
    }

    #[test]
    fn cardinality_is_charged_before_materialization() {
        let budget = AstFunctionWorkBudget::new();
        assert!(
            materialize_integer_range(1, 1, MAX_AST_FUNCTION_EVAL_WORK as i64, &budget).is_some()
        );
        assert!(materialize_integer_range(1, 1, 1, &budget).is_none());

        let budget = AstFunctionWorkBudget::new();
        assert!(
            materialize_integer_range(1, 1, MAX_AST_FUNCTION_EVAL_WORK as i64 + 1, &budget)
                .is_none()
        );
    }
}
