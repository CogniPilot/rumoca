use super::*;

pub(super) fn operand_projection(
    dimensions: &[u32],
    result: &[gast::Expression],
    span: Span,
) -> Result<Vec<gast::Expression>, GalecTargetError> {
    if dimensions.is_empty() {
        Ok(Vec::new())
    } else if dimensions.len() == result.len() {
        Ok(result.to_vec())
    } else {
        Err(unsupported(
            "array-projection",
            format!(
                "operand rank {} does not match projected result rank {}",
                dimensions.len(),
                result.len()
            ),
            span,
        ))
    }
}

pub(super) fn coordinate_variable<'dae>(
    coordinate: dae::CoordinateView<'dae>,
    span: Span,
) -> Result<(dae::VariableId<'dae>, bool), GalecTargetError> {
    match coordinate {
        dae::CoordinateView::Parameter(id) => Ok((dae::VariableId::from(id), false)),
        dae::CoordinateView::Input(id) => Ok((dae::VariableId::from(id), false)),
        dae::CoordinateView::State(id) => Ok((dae::VariableId::from(id), false)),
        dae::CoordinateView::Algebraic(id) => Ok((dae::VariableId::from(id), false)),
        dae::CoordinateView::DiscreteReal(id) => Ok((dae::VariableId::from(id), false)),
        dae::CoordinateView::DiscreteValue(id) => Ok((dae::VariableId::from(id), false)),
        dae::CoordinateView::PreDiscreteReal(id) => Ok((dae::VariableId::from(id), true)),
        dae::CoordinateView::PreDiscreteValue(id) => Ok((dae::VariableId::from(id), true)),
        _ => Err(unsupported(
            "runtime-coordinate",
            "runtime coordinate has no GALEC expression mapping".to_owned(),
            span,
        )),
    }
}

pub(super) fn lower_range_at(
    start: i64,
    step: i64,
    _stop: i64,
    indices: &[gast::Expression],
    scalar_type: gast::ScalarType,
    span: Span,
) -> Result<TypedExpression, GalecTargetError> {
    let [gast::Expression::Integer(ordinal)] = indices else {
        return Err(unsupported(
            "range-projection",
            "range projection requires one literal ordinal".to_owned(),
            span,
        ));
    };
    let value = start
        .checked_add((ordinal - 1).saturating_mul(step))
        .ok_or_else(|| {
            unsupported(
                "range-overflow",
                "range projection arithmetic overflowed".to_owned(),
                span,
            )
        })?;
    Ok(TypedExpression {
        expression: gast::Expression::Integer(value),
        scalar_type,
    })
}

pub(super) fn require_boolean(value: &TypedExpression, span: Span) -> Result<(), GalecTargetError> {
    if value.scalar_type == gast::ScalarType::Boolean {
        Ok(())
    } else {
        Err(type_mismatch("Boolean", value.scalar_type.keyword(), span))
    }
}

pub(super) fn lower_literal(
    literal: &dae::DaeLiteral,
    span: Span,
) -> Result<gast::Expression, GalecTargetError> {
    match literal {
        dae::DaeLiteral::Real(value) => Ok(gast::Expression::Real(*value)),
        dae::DaeLiteral::Integer(value) => Ok(gast::Expression::Integer(*value)),
        dae::DaeLiteral::Enumeration(value) => Ok(gast::Expression::Integer(*value)),
        dae::DaeLiteral::Boolean(value) => Ok(gast::Expression::Bool(*value)),
        dae::DaeLiteral::String(_) => Err(unsupported(
            "string-expression",
            "String expression has no GALEC representation".to_owned(),
            span,
        )),
    }
}

pub(super) fn lower_binary(
    operator: dae::BinaryOperator,
    lhs: TypedExpression,
    rhs: TypedExpression,
    result: gast::ScalarType,
    span: Span,
) -> Result<gast::Expression, GalecTargetError> {
    use dae::BinaryOperator as D;
    use gast::BinaryOp as G;
    let operator = match operator {
        D::Add | D::ElementwiseAdd => G::Add,
        D::Subtract | D::ElementwiseSubtract => G::Sub,
        D::Multiply | D::ElementwiseMultiply => G::Mul,
        D::Divide | D::ElementwiseDivide => G::Div,
        D::Power | D::ElementwisePower => G::Pow,
        D::Equal => G::Eq,
        D::NotEqual => G::Ne,
        D::Less => G::Lt,
        D::LessEqual => G::Le,
        D::Greater => G::Gt,
        D::GreaterEqual => G::Ge,
        D::And => G::And,
        D::Or => G::Or,
    };
    let numeric = matches!(
        operator,
        G::Add | G::Sub | G::Mul | G::Div | G::Pow | G::Lt | G::Le | G::Gt | G::Ge
    ) || matches!(operator, G::Eq | G::Ne)
        && lhs.scalar_type != gast::ScalarType::Boolean;
    let (lhs, rhs) = if numeric
        && (result == gast::ScalarType::Real
            || lhs.scalar_type == gast::ScalarType::Real
            || rhs.scalar_type == gast::ScalarType::Real
            || matches!(operator, G::Div | G::Pow))
    {
        (
            coerce(lhs, gast::ScalarType::Real, span)?,
            coerce(rhs, gast::ScalarType::Real, span)?,
        )
    } else if lhs.scalar_type == rhs.scalar_type {
        (lhs.expression, rhs.expression)
    } else {
        return Err(type_mismatch(
            lhs.scalar_type.keyword(),
            rhs.scalar_type.keyword(),
            span,
        ));
    };
    Ok(gast::Expression::binary(operator, lhs, rhs))
}

pub(super) fn lower_builtin<'dae>(
    lowerer: &mut ExpressionLowerer<'_, 'dae>,
    builtin: dae::PureBuiltin,
    arguments: dae::ExpressionOperands<'dae>,
    span: Span,
) -> Result<gast::Expression, GalecTargetError> {
    if builtin == dae::PureBuiltin::Smooth {
        return lowerer
            .lower(arguments.get(1).expect("checked smooth value argument"))
            .map(|value| value.expression);
    }
    if builtin == dae::PureBuiltin::NoEvent {
        return lowerer
            .lower(arguments.get(0).expect("checked noEvent value argument"))
            .map(|value| value.expression);
    }
    let lowered = arguments
        .iter()
        .map(|argument| lowerer.lower(argument))
        .collect::<Result<Vec<_>, _>>()?;
    lower_builtin_arguments(builtin, lowered, span)
}

pub(super) fn lower_builtin_arguments(
    builtin: dae::PureBuiltin,
    arguments: Vec<TypedExpression>,
    span: Span,
) -> Result<gast::Expression, GalecTargetError> {
    let name = match builtin {
        dae::PureBuiltin::Abs => "absolute",
        dae::PureBuiltin::Sign => "sign",
        dae::PureBuiltin::Sqrt => "sqrt",
        dae::PureBuiltin::Mod => {
            return Err(unsupported(
                "builtin:mod",
                "builtin `mod` has no scalar GALEC mapping".to_owned(),
                span,
            ));
        }
        dae::PureBuiltin::Floor => "roundDown",
        dae::PureBuiltin::Ceil => "roundUp",
        dae::PureBuiltin::Integer => "integer",
        dae::PureBuiltin::Sin => "sin",
        dae::PureBuiltin::Cos => "cos",
        dae::PureBuiltin::Tan => "tan",
        dae::PureBuiltin::Asin => "asin",
        dae::PureBuiltin::Acos => "acos",
        dae::PureBuiltin::Atan => "atan",
        dae::PureBuiltin::Atan2 => "atan2",
        dae::PureBuiltin::Sinh => "sinh",
        dae::PureBuiltin::Cosh => "cosh",
        dae::PureBuiltin::Tanh => "tanh",
        dae::PureBuiltin::Exp => "exp",
        dae::PureBuiltin::Log => "ln",
        dae::PureBuiltin::Log10 => "lg",
        dae::PureBuiltin::Smooth => unreachable!("smooth is lowered as its value"),
        dae::PureBuiltin::NoEvent => unreachable!("noEvent is lowered as its value"),
        dae::PureBuiltin::Min => "min",
        dae::PureBuiltin::Max => "max",
        dae::PureBuiltin::Sum
        | dae::PureBuiltin::Product
        | dae::PureBuiltin::Size
        | dae::PureBuiltin::Zeros
        | dae::PureBuiltin::Ones
        | dae::PureBuiltin::Fill
        | dae::PureBuiltin::Linspace
        | dae::PureBuiltin::Cross => {
            return Err(unsupported(
                "builtin",
                format!("builtin `{builtin:?}` has no scalar GALEC mapping"),
                span,
            ));
        }
    };
    let lowered = arguments
        .into_iter()
        .map(|argument| match builtin {
            dae::PureBuiltin::Min | dae::PureBuiltin::Max => {
                coerce(argument, gast::ScalarType::Real, span)
            }
            _ => Ok(argument.expression),
        })
        .collect::<Result<Vec<_>, GalecTargetError>>()?;
    Ok(gast::Expression::Call(gast::FunctionCall {
        function: with_span(gast::Name::ident(name), span),
        arguments: lowered,
    }))
}

pub(super) fn coerce(
    value: TypedExpression,
    expected: gast::ScalarType,
    span: Span,
) -> Result<gast::Expression, GalecTargetError> {
    if value.scalar_type == expected {
        return Ok(value.expression);
    }
    if expected == gast::ScalarType::Real && value.scalar_type == gast::ScalarType::Integer {
        return Ok(gast::Expression::Call(gast::FunctionCall {
            function: with_span(gast::Name::ident("real"), span),
            arguments: vec![value.expression],
        }));
    }
    Err(type_mismatch(
        expected.keyword(),
        value.scalar_type.keyword(),
        span,
    ))
}
