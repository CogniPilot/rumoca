//! Shared integer-division semantics for compile-time evaluators.
//!
//! MLS §3.7 numeric operators define `/` as division. Integer-only evaluator
//! paths can only fold `/` when the quotient is exactly representable as an
//! integer. Builtin `div(x, y)` uses truncating integer division.

/// Evaluate integer `/` for integer-only evaluator paths.
///
/// Returns `Some(q)` only when:
/// - division is valid (non-zero divisor, no overflow), and
/// - `lhs / rhs` is exact in integer arithmetic.
pub fn eval_integer_slash(lhs: i64, rhs: i64) -> Option<i64> {
    let quotient = lhs.checked_div(rhs)?;
    (lhs.checked_rem(rhs)? == 0).then_some(quotient)
}

/// Evaluate builtin `div(lhs, rhs)` (MLS §3.7.2), truncating toward zero.
pub fn eval_integer_div_builtin(lhs: i64, rhs: i64) -> Option<i64> {
    lhs.checked_div(rhs)
}

#[cfg(test)]
mod tests {
    use super::{eval_integer_div_builtin, eval_integer_slash};

    #[test]
    fn slash_requires_exact_quotient() {
        assert_eq!(eval_integer_slash(8, 2), Some(4));
        assert_eq!(eval_integer_slash(7, 2), None);
        assert_eq!(eval_integer_slash(-7, 2), None);
    }

    #[test]
    fn slash_rejects_invalid_division() {
        assert_eq!(eval_integer_slash(1, 0), None);
        assert_eq!(eval_integer_slash(i64::MIN, -1), None);
    }

    #[test]
    fn builtin_div_truncates_toward_zero() {
        assert_eq!(eval_integer_div_builtin(7, 2), Some(3));
        assert_eq!(eval_integer_div_builtin(-7, 2), Some(-3));
        assert_eq!(eval_integer_div_builtin(7, -2), Some(-3));
    }

    #[test]
    fn builtin_div_rejects_invalid_division() {
        assert_eq!(eval_integer_div_builtin(1, 0), None);
        assert_eq!(eval_integer_div_builtin(i64::MIN, -1), None);
    }
}
