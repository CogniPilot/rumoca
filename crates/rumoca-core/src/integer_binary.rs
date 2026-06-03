//! Shared integer binary operation semantics for AST evaluators.
//!
//! MLS §3.7 numeric operators define integer arithmetic and exponentiation
//! behavior for compile-time evaluation paths.

use crate::{OpBinary, eval_integer_slash};

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
        IntegerBinaryOperator::Add => lhs.checked_add(rhs),
        IntegerBinaryOperator::Sub => lhs.checked_sub(rhs),
        IntegerBinaryOperator::Mul => lhs.checked_mul(rhs),
        IntegerBinaryOperator::Div => eval_integer_slash(lhs, rhs),
        IntegerBinaryOperator::Exp => {
            let exponent = u32::try_from(rhs).ok()?;
            lhs.checked_pow(exponent)
        }
    }
}

fn integer_binary_operator_from_op(op: &OpBinary) -> Option<IntegerBinaryOperator> {
    match op {
        OpBinary::Add => Some(IntegerBinaryOperator::Add),
        OpBinary::Sub => Some(IntegerBinaryOperator::Sub),
        OpBinary::Mul => Some(IntegerBinaryOperator::Mul),
        OpBinary::Div => Some(IntegerBinaryOperator::Div),
        OpBinary::Exp => Some(IntegerBinaryOperator::Exp),
        _ => None,
    }
}

pub fn eval_ast_integer_binary(op: &OpBinary, lhs: i64, rhs: i64) -> Option<i64> {
    eval_integer_binary(integer_binary_operator_from_op(op)?, lhs, rhs)
}

#[cfg(test)]
mod tests {
    use super::{IntegerBinaryOperator, eval_ast_integer_binary, eval_integer_binary};
    use crate::OpBinary;

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

    #[test]
    fn exp_overflow_returns_none() {
        assert_eq!(eval_integer_binary(IntegerBinaryOperator::Exp, 2, 63), None);
    }

    #[test]
    fn add_sub_mul_overflow_returns_none() {
        assert_eq!(
            eval_integer_binary(IntegerBinaryOperator::Add, i64::MAX, 1),
            None
        );
        assert_eq!(
            eval_integer_binary(IntegerBinaryOperator::Sub, i64::MIN, 1),
            None
        );
        assert_eq!(
            eval_integer_binary(IntegerBinaryOperator::Mul, i64::MAX, 2),
            None
        );
    }

    #[test]
    fn ast_integer_binary_handles_exponentiation() {
        assert_eq!(eval_ast_integer_binary(&OpBinary::Exp, 2, 5), Some(32));
    }
}
