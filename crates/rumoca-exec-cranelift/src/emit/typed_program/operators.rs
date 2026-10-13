//! Operator tables from the typed program vocabulary to scalar operators and
//! Cranelift condition codes.

use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use rumoca_ir_solve as solve;

pub(super) fn map_unary(operator: solve::SolveUnaryOperator) -> rumoca_ir_solve::UnaryOp {
    use rumoca_ir_solve::UnaryOp as Target;
    match operator {
        solve::SolveUnaryOperator::Negate => Target::Neg,
        solve::SolveUnaryOperator::Not => Target::Not,
        solve::SolveUnaryOperator::Abs => Target::Abs,
        solve::SolveUnaryOperator::Sign => Target::Sign,
        solve::SolveUnaryOperator::Sqrt => Target::Sqrt,
        solve::SolveUnaryOperator::Floor => Target::Floor,
        solve::SolveUnaryOperator::Ceiling => Target::Ceil,
        solve::SolveUnaryOperator::Truncate => Target::Trunc,
        solve::SolveUnaryOperator::Sin => Target::Sin,
        solve::SolveUnaryOperator::Cos => Target::Cos,
        solve::SolveUnaryOperator::Tan => Target::Tan,
        solve::SolveUnaryOperator::Asin => Target::Asin,
        solve::SolveUnaryOperator::Acos => Target::Acos,
        solve::SolveUnaryOperator::Atan => Target::Atan,
        solve::SolveUnaryOperator::Sinh => Target::Sinh,
        solve::SolveUnaryOperator::Cosh => Target::Cosh,
        solve::SolveUnaryOperator::Tanh => Target::Tanh,
        solve::SolveUnaryOperator::Exp => Target::Exp,
        solve::SolveUnaryOperator::Log => Target::Log,
        solve::SolveUnaryOperator::Log10 => Target::Log10,
    }
}

/// The Real binary operation of a typed operator; the Integer quotient has none.
pub(super) fn map_binary(
    operator: solve::SolveBinaryOperator,
) -> Option<rumoca_ir_solve::BinaryOp> {
    use rumoca_ir_solve::BinaryOp as Target;
    Some(match operator {
        solve::SolveBinaryOperator::Add => Target::Add,
        solve::SolveBinaryOperator::Subtract => Target::Sub,
        solve::SolveBinaryOperator::Multiply => Target::Mul,
        solve::SolveBinaryOperator::Divide => Target::Div,
        solve::SolveBinaryOperator::Power => Target::Pow,
        solve::SolveBinaryOperator::And => Target::And,
        solve::SolveBinaryOperator::Or => Target::Or,
        solve::SolveBinaryOperator::Atan2 => Target::Atan2,
        solve::SolveBinaryOperator::Min => Target::Min,
        solve::SolveBinaryOperator::Max => Target::Max,
        solve::SolveBinaryOperator::IntegerQuotient
        | solve::SolveBinaryOperator::IntegerModulo
        | solve::SolveBinaryOperator::IntegerRemainder => return None,
    })
}

pub(super) fn map_float_compare(operator: solve::SolveCompareOperator) -> FloatCC {
    match operator {
        solve::SolveCompareOperator::Equal => FloatCC::Equal,
        solve::SolveCompareOperator::NotEqual => FloatCC::NotEqual,
        solve::SolveCompareOperator::Less => FloatCC::LessThan,
        solve::SolveCompareOperator::LessEqual => FloatCC::LessThanOrEqual,
        solve::SolveCompareOperator::Greater => FloatCC::GreaterThan,
        solve::SolveCompareOperator::GreaterEqual => FloatCC::GreaterThanOrEqual,
    }
}

pub(super) fn map_int_compare(operator: solve::SolveCompareOperator) -> IntCC {
    match operator {
        solve::SolveCompareOperator::Equal => IntCC::Equal,
        solve::SolveCompareOperator::NotEqual => IntCC::NotEqual,
        solve::SolveCompareOperator::Less => IntCC::SignedLessThan,
        solve::SolveCompareOperator::LessEqual => IntCC::SignedLessThanOrEqual,
        solve::SolveCompareOperator::Greater => IntCC::SignedGreaterThan,
        solve::SolveCompareOperator::GreaterEqual => IntCC::SignedGreaterThanOrEqual,
    }
}
