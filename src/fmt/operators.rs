//! Operator formatting and precedence handling.

use crate::ir::ast::{OpBinary, OpUnary};

pub fn format_binary_op(op: &OpBinary) -> &'static str {
    match op {
        OpBinary::Empty => "",
        OpBinary::Add(_) => "+",
        OpBinary::Sub(_) => "-",
        OpBinary::Mul(_) => "*",
        OpBinary::Div(_) => "/",
        OpBinary::Exp(_) => "^",
        OpBinary::Eq(_) => "==",
        OpBinary::Neq(_) => "<>",
        OpBinary::Lt(_) => "<",
        OpBinary::Le(_) => "<=",
        OpBinary::Gt(_) => ">",
        OpBinary::Ge(_) => ">=",
        OpBinary::And(_) => "and",
        OpBinary::Or(_) => "or",
        OpBinary::AddElem(_) => ".+",
        OpBinary::SubElem(_) => ".-",
        OpBinary::MulElem(_) => ".*",
        OpBinary::DivElem(_) => "./",
        OpBinary::Assign(_) => "=",
    }
}

pub fn format_unary_op(op: &OpUnary) -> &'static str {
    match op {
        OpUnary::Empty => "",
        OpUnary::Minus(_) => "-",
        OpUnary::Plus(_) => "+",
        OpUnary::DotMinus(_) => ".-",
        OpUnary::DotPlus(_) => ".+",
        OpUnary::Not(_) => "not ",
    }
}

/// Get the precedence level for a binary operator.
/// Higher values bind tighter. Based on Modelica specification:
/// 0. assignment: = (lowest, used in modifications)
/// 1. or
/// 2. and
/// 3. relational: <, <=, >, >=, ==, <>
/// 4. additive: +, -, .+, .-
/// 5. multiplicative: *, /, .*, ./
/// 6. exponentiation: ^ (highest for binary ops)
pub fn binary_op_precedence(op: &OpBinary) -> u8 {
    match op {
        OpBinary::Empty => 0,
        OpBinary::Assign(_) => 0,
        OpBinary::Or(_) => 1,
        OpBinary::And(_) => 2,
        OpBinary::Eq(_)
        | OpBinary::Neq(_)
        | OpBinary::Lt(_)
        | OpBinary::Le(_)
        | OpBinary::Gt(_)
        | OpBinary::Ge(_) => 3,
        OpBinary::Add(_) | OpBinary::Sub(_) | OpBinary::AddElem(_) | OpBinary::SubElem(_) => 4,
        OpBinary::Mul(_) | OpBinary::Div(_) | OpBinary::MulElem(_) | OpBinary::DivElem(_) => 5,
        OpBinary::Exp(_) => 6,
    }
}

/// Check if an operator is right-associative.
/// In Modelica, exponentiation (^) is right-associative.
pub fn binary_op_is_right_assoc(op: &OpBinary) -> bool {
    matches!(op, OpBinary::Exp(_))
}
