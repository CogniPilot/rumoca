use super::*;

pub(super) fn subscript_expression(subscript: &Subscript) -> Option<&Expression> {
    match subscript {
        Subscript::Expr { expr, .. } => Some(expr),
        Subscript::Index { .. } | Subscript::Colon { .. } => None,
    }
}

pub(super) fn binary_operator(operator: &OpBinary) -> dae::BinaryOperator {
    match operator {
        OpBinary::Add => dae::BinaryOperator::Add,
        OpBinary::Sub => dae::BinaryOperator::Subtract,
        OpBinary::Mul => dae::BinaryOperator::Multiply,
        OpBinary::Div => dae::BinaryOperator::Divide,
        OpBinary::Exp => dae::BinaryOperator::Power,
        OpBinary::AddElem => dae::BinaryOperator::ElementwiseAdd,
        OpBinary::SubElem => dae::BinaryOperator::ElementwiseSubtract,
        OpBinary::MulElem => dae::BinaryOperator::ElementwiseMultiply,
        OpBinary::DivElem => dae::BinaryOperator::ElementwiseDivide,
        OpBinary::ExpElem => dae::BinaryOperator::ElementwisePower,
        OpBinary::Eq => dae::BinaryOperator::Equal,
        OpBinary::Neq => dae::BinaryOperator::NotEqual,
        OpBinary::Lt => dae::BinaryOperator::Less,
        OpBinary::Le => dae::BinaryOperator::LessEqual,
        OpBinary::Gt => dae::BinaryOperator::Greater,
        OpBinary::Ge => dae::BinaryOperator::GreaterEqual,
        OpBinary::And => dae::BinaryOperator::And,
        OpBinary::Or => dae::BinaryOperator::Or,
        OpBinary::Empty | OpBinary::Assign => unreachable!("analysis restricts binary operators"),
    }
}

pub(super) fn unary_operator(operator: &OpUnary) -> dae::UnaryOperator {
    match operator {
        OpUnary::Minus | OpUnary::DotMinus => dae::UnaryOperator::Negate,
        OpUnary::Plus | OpUnary::DotPlus => dae::UnaryOperator::Plus,
        OpUnary::Not => dae::UnaryOperator::Not,
        OpUnary::Empty => {
            unreachable!("analysis restricts unary operators")
        }
    }
}

pub(super) fn pure_builtin(function: BuiltinFunction) -> dae::PureBuiltin {
    match function {
        BuiltinFunction::Abs => dae::PureBuiltin::Abs,
        BuiltinFunction::Sign => dae::PureBuiltin::Sign,
        BuiltinFunction::Sqrt => dae::PureBuiltin::Sqrt,
        BuiltinFunction::Div => dae::PureBuiltin::Div,
        BuiltinFunction::Mod => dae::PureBuiltin::Mod,
        BuiltinFunction::Rem => dae::PureBuiltin::Rem,
        BuiltinFunction::Floor => dae::PureBuiltin::Floor,
        BuiltinFunction::Ceil => dae::PureBuiltin::Ceil,
        BuiltinFunction::Integer => dae::PureBuiltin::Integer,
        BuiltinFunction::Sin => dae::PureBuiltin::Sin,
        BuiltinFunction::Cos => dae::PureBuiltin::Cos,
        BuiltinFunction::Tan => dae::PureBuiltin::Tan,
        BuiltinFunction::Asin => dae::PureBuiltin::Asin,
        BuiltinFunction::Acos => dae::PureBuiltin::Acos,
        BuiltinFunction::Atan => dae::PureBuiltin::Atan,
        BuiltinFunction::Atan2 => dae::PureBuiltin::Atan2,
        BuiltinFunction::Sinh => dae::PureBuiltin::Sinh,
        BuiltinFunction::Cosh => dae::PureBuiltin::Cosh,
        BuiltinFunction::Tanh => dae::PureBuiltin::Tanh,
        BuiltinFunction::Exp => dae::PureBuiltin::Exp,
        BuiltinFunction::Log => dae::PureBuiltin::Log,
        BuiltinFunction::Log10 => dae::PureBuiltin::Log10,
        BuiltinFunction::Smooth => dae::PureBuiltin::Smooth,
        BuiltinFunction::NoEvent => dae::PureBuiltin::NoEvent,
        BuiltinFunction::Homotopy => dae::PureBuiltin::Homotopy,
        BuiltinFunction::Min => dae::PureBuiltin::Min,
        BuiltinFunction::Max => dae::PureBuiltin::Max,
        BuiltinFunction::Sum => dae::PureBuiltin::Sum,
        BuiltinFunction::Product => dae::PureBuiltin::Product,
        BuiltinFunction::Size => dae::PureBuiltin::Size,
        BuiltinFunction::Zeros => dae::PureBuiltin::Zeros,
        BuiltinFunction::Ones => dae::PureBuiltin::Ones,
        BuiltinFunction::Fill => dae::PureBuiltin::Fill,
        BuiltinFunction::Linspace => dae::PureBuiltin::Linspace,
        BuiltinFunction::Cross => dae::PureBuiltin::Cross,
        BuiltinFunction::Identity => dae::PureBuiltin::Identity,
        BuiltinFunction::Vector => dae::PureBuiltin::Vector,
        BuiltinFunction::Transpose => dae::PureBuiltin::Transpose,
        BuiltinFunction::Diagonal => dae::PureBuiltin::Diagonal,
        BuiltinFunction::OuterProduct => dae::PureBuiltin::OuterProduct,
        BuiltinFunction::Skew => dae::PureBuiltin::Skew,
        BuiltinFunction::Cat => unreachable!(
            "explicit cat remains outside the accepted DAE builtin grammar; matrix syntax uses checked promoted concatenation"
        ),
        _ => unreachable!("analysis restricts pure builtins"),
    }
}

pub(super) fn lower_literal(literal: &Literal) -> dae::DaeLiteral {
    match literal {
        Literal::Real(value) => dae::DaeLiteral::Real(*value),
        Literal::Integer(value) => dae::DaeLiteral::Integer(*value),
        Literal::Boolean(value) => dae::DaeLiteral::Boolean(*value),
        Literal::String(value) => dae::DaeLiteral::String(value.clone()),
    }
}
