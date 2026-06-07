pub(crate) fn runtime_activation(expr: &rumoca_core::Expression) -> Option<bool> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(value),
            ..
        } => Some(*value),
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Initial,
            args,
            ..
        } if args.is_empty() => Some(false),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            rhs,
            ..
        } => runtime_activation(rhs).map(|active| !active),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::And,
            lhs,
            rhs,
            ..
        } => match (runtime_activation(lhs), runtime_activation(rhs)) {
            (Some(false), _) | (_, Some(false)) => Some(false),
            (Some(true), Some(true)) => Some(true),
            _ => None,
        },
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Or,
            lhs,
            rhs,
            ..
        } => match (runtime_activation(lhs), runtime_activation(rhs)) {
            (Some(true), _) | (_, Some(true)) => Some(true),
            (Some(false), Some(false)) => Some(false),
            _ => None,
        },
        _ => None,
    }
}
