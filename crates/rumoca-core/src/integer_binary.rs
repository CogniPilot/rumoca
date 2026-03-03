//! Shared integer binary operation semantics for AST evaluators.
//!
//! MLS §3.7 numeric operators define integer arithmetic and exponentiation
//! behavior for compile-time evaluation paths.

use crate::eval_integer_slash;

/// Integer binary operators supported by compile-time AST evaluators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntegerBinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Exp,
}

/// Evaluate an integer binary operation.
pub fn eval_integer_binary(op: IntegerBinaryOperator, lhs: i64, rhs: i64) -> Option<i64> {
    match op {
        IntegerBinaryOperator::Add => Some(lhs + rhs),
        IntegerBinaryOperator::Sub => Some(lhs - rhs),
        IntegerBinaryOperator::Mul => Some(lhs * rhs),
        IntegerBinaryOperator::Div => eval_integer_slash(lhs, rhs),
        IntegerBinaryOperator::Exp => {
            let exponent = u32::try_from(rhs).ok()?;
            Some(lhs.pow(exponent))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{IntegerBinaryOperator, eval_integer_binary};

    #[test]
    fn add_sub_mul_evaluate() {
        assert_eq!(
            eval_integer_binary(IntegerBinaryOperator::Add, 3, 4),
            Some(7)
        );
        assert_eq!(
            eval_integer_binary(IntegerBinaryOperator::Sub, 3, 4),
            Some(-1)
        );
        assert_eq!(
            eval_integer_binary(IntegerBinaryOperator::Mul, 3, 4),
            Some(12)
        );
    }

    #[test]
    fn div_uses_exact_integer_semantics() {
        assert_eq!(
            eval_integer_binary(IntegerBinaryOperator::Div, 8, 2),
            Some(4)
        );
        assert_eq!(eval_integer_binary(IntegerBinaryOperator::Div, 7, 2), None);
    }

    #[test]
    fn exp_requires_non_negative_u32_exponent() {
        assert_eq!(
            eval_integer_binary(IntegerBinaryOperator::Exp, 2, 3),
            Some(8)
        );
        assert_eq!(eval_integer_binary(IntegerBinaryOperator::Exp, 2, -1), None);
    }
}
