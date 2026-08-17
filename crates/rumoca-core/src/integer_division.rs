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

/// Evaluate builtin `mod(lhs, rhs)` (MLS §3.7.2): `lhs - floor(lhs/rhs)*rhs`.
///
/// Floored modulo takes the sign of the divisor: `mod(-7, 3) == 2`,
/// `mod(7, -3) == -2`. This differs from Rust's `%`, which truncates.
pub fn eval_integer_mod_builtin(lhs: i64, rhs: i64) -> Option<i64> {
    let rem = lhs.checked_rem(rhs)?;
    // A truncated remainder already has the divisor's sign (or is zero)
    // exactly when it equals the floored result; only the disagreeing case
    // needs the one-divisor correction, and there `|rem| < |rhs|` with
    // opposite signs keeps the sum representable.
    if rem != 0 && (rem < 0) != (rhs < 0) {
        rem.checked_add(rhs)
    } else {
        Some(rem)
    }
}

/// Evaluate builtin `rem(lhs, rhs)` (MLS §3.7.2): `lhs - div(lhs,rhs)*rhs`.
///
/// Truncated remainder takes the sign of the dividend: `rem(-7, 3) == -1`.
pub fn eval_integer_rem_builtin(lhs: i64, rhs: i64) -> Option<i64> {
    lhs.checked_rem(rhs)
}

#[cfg(test)]
mod tests {
    use super::{
        eval_integer_div_builtin, eval_integer_mod_builtin, eval_integer_rem_builtin,
        eval_integer_slash,
    };

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

    #[test]
    fn builtin_mod_is_floored() {
        assert_eq!(eval_integer_mod_builtin(7, 3), Some(1));
        assert_eq!(eval_integer_mod_builtin(-7, 3), Some(2));
        assert_eq!(eval_integer_mod_builtin(7, -3), Some(-2));
        assert_eq!(eval_integer_mod_builtin(-7, -3), Some(-1));
    }

    #[test]
    fn builtin_mod_rejects_invalid_division() {
        assert_eq!(eval_integer_mod_builtin(1, 0), None);
        assert_eq!(eval_integer_mod_builtin(i64::MIN, -1), None);
    }

    #[test]
    fn builtin_mod_survives_extreme_magnitudes() {
        assert_eq!(
            eval_integer_mod_builtin(i64::MAX - 1, i64::MAX),
            Some(i64::MAX - 1)
        );
        assert_eq!(eval_integer_mod_builtin(i64::MAX, i64::MAX), Some(0));
        assert_eq!(
            eval_integer_mod_builtin(i64::MIN, i64::MAX),
            Some(i64::MAX - 1)
        );
        assert_eq!(eval_integer_mod_builtin(i64::MIN + 1, i64::MAX), Some(0));
        assert_eq!(eval_integer_mod_builtin(i64::MAX, i64::MIN), Some(-1));
        assert_eq!(eval_integer_mod_builtin(i64::MIN, i64::MIN), Some(0));
    }

    #[test]
    fn builtin_rem_is_truncated() {
        assert_eq!(eval_integer_rem_builtin(7, 3), Some(1));
        assert_eq!(eval_integer_rem_builtin(-7, 3), Some(-1));
        assert_eq!(eval_integer_rem_builtin(7, -3), Some(1));
        assert_eq!(eval_integer_rem_builtin(-7, -3), Some(-1));
    }

    #[test]
    fn builtin_rem_rejects_invalid_division() {
        assert_eq!(eval_integer_rem_builtin(1, 0), None);
        assert_eq!(eval_integer_rem_builtin(i64::MIN, -1), None);
    }
}
