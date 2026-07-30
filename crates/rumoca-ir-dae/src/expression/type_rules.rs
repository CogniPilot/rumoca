use super::*;

pub(super) fn validate_static_quotient(
    storage: &Storage,
    builtin: PureBuiltin,
    arguments: &[ExprId<'_>],
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let [lhs, rhs] = arguments else {
        unreachable!("builtin result validation proves quotient arity")
    };
    let operator = quotient_name(builtin);
    let Some(lhs) = static_numeric_value(storage, lhs.index()) else {
        return Err(DaeConstructionError::NonStaticDiscontinuity {
            operator,
            span: at.span(),
        });
    };
    let Some(rhs) = static_numeric_value(storage, rhs.index()) else {
        return Err(DaeConstructionError::NonStaticDiscontinuity {
            operator,
            span: at.span(),
        });
    };
    let function = match builtin {
        PureBuiltin::Div => rumoca_core::BuiltinFunction::Div,
        PureBuiltin::Mod => rumoca_core::BuiltinFunction::Mod,
        PureBuiltin::Rem => rumoca_core::BuiltinFunction::Rem,
        _ => unreachable!("caller restricts static quotient validation"),
    };
    let result = rumoca_core::apply_scalar_binary_math(function, lhs, rhs);
    if result.is_some_and(f64::is_finite) {
        Ok(())
    } else {
        Err(DaeConstructionError::UndefinedBuiltinDomain {
            operator,
            span: at.span(),
        })
    }
}

fn quotient_name(builtin: PureBuiltin) -> &'static str {
    match builtin {
        PureBuiltin::Div => "div",
        PureBuiltin::Mod => "mod",
        PureBuiltin::Rem => "rem",
        _ => unreachable!("caller restricts quotient builtins"),
    }
}

fn static_numeric_value(storage: &Storage, expression: u32) -> Option<f64> {
    let node = storage.expressions.nodes.get(expression as usize)?;
    let value = match node {
        ExprNode::Literal(DaeLiteral::Real(value)) => *value,
        ExprNode::Literal(DaeLiteral::Integer(value) | DaeLiteral::Enumeration(value)) => {
            *value as f64
        }
        ExprNode::Unary { operator, operand } => {
            let operand = static_numeric_value(storage, *operand)?;
            match operator {
                UnaryOperator::Plus => operand,
                UnaryOperator::Negate => -operand,
                UnaryOperator::Not => return None,
            }
        }
        ExprNode::Binary { operator, lhs, rhs } => {
            let lhs = static_numeric_value(storage, *lhs)?;
            let rhs = static_numeric_value(storage, *rhs)?;
            match operator {
                BinaryOperator::Add | BinaryOperator::ElementwiseAdd => lhs + rhs,
                BinaryOperator::Subtract | BinaryOperator::ElementwiseSubtract => lhs - rhs,
                BinaryOperator::Multiply | BinaryOperator::ElementwiseMultiply => lhs * rhs,
                BinaryOperator::Divide | BinaryOperator::ElementwiseDivide if rhs != 0.0 => {
                    lhs / rhs
                }
                BinaryOperator::Power | BinaryOperator::ElementwisePower => lhs.powf(rhs),
                _ => return None,
            }
        }
        ExprNode::Builtin { builtin, operands }
            if matches!(
                builtin,
                PureBuiltin::Div | PureBuiltin::Mod | PureBuiltin::Rem
            ) =>
        {
            let mut operands = storage.expressions.operands[operands.indices()].iter();
            let lhs = static_numeric_value(storage, *operands.next()?)?;
            let rhs = static_numeric_value(storage, *operands.next()?)?;
            let function = match builtin {
                PureBuiltin::Div => rumoca_core::BuiltinFunction::Div,
                PureBuiltin::Mod => rumoca_core::BuiltinFunction::Mod,
                PureBuiltin::Rem => rumoca_core::BuiltinFunction::Rem,
                _ => unreachable!("guard restricts quotient builtins"),
            };
            rumoca_core::apply_scalar_binary_math(function, lhs, rhs)?
        }
        _ => return None,
    };
    value.is_finite().then_some(value)
}

pub(super) fn binary_result(
    operator: BinaryOperator,
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    let lhs_scalar = lhs.scalar_type();
    let rhs_scalar = rhs.scalar_type();
    match operator {
        BinaryOperator::And | BinaryOperator::Or => {
            expect_same_shape(lhs, rhs, at)?;
            if lhs_scalar != ScalarType::Boolean {
                return Err(type_mismatch(ScalarType::Boolean, lhs_scalar, at));
            }
            if rhs_scalar != ScalarType::Boolean {
                return Err(type_mismatch(ScalarType::Boolean, rhs_scalar, at));
            }
            Ok(lhs.clone())
        }
        BinaryOperator::Add
            if lhs_scalar == ScalarType::String && rhs_scalar == ScalarType::String =>
        {
            if !lhs.is_scalar() || !rhs.is_scalar() {
                return Err(DaeConstructionError::ExpectedScalar { span: at.span() });
            }
            Ok(ValueType::scalar(ScalarType::String))
        }
        BinaryOperator::Add
        | BinaryOperator::Subtract
        | BinaryOperator::ElementwiseAdd
        | BinaryOperator::ElementwiseSubtract => {
            expect_same_shape(lhs, rhs, at)?;
            expect_numeric(lhs_scalar, at)?;
            expect_numeric(rhs_scalar, at)?;
            let scalar = promoted_numeric_scalar(lhs_scalar, rhs_scalar, false);
            Ok(ValueType::array(scalar, lhs.dimensions().to_vec()))
        }
        BinaryOperator::Multiply => multiplication_result(lhs, rhs, at),
        BinaryOperator::Divide => division_result(lhs, rhs, at),
        BinaryOperator::Power => power_result(lhs, rhs, at),
        BinaryOperator::ElementwiseMultiply
        | BinaryOperator::ElementwiseDivide
        | BinaryOperator::ElementwisePower => elementwise_result(operator, lhs, rhs, at),
        BinaryOperator::Equal
        | BinaryOperator::NotEqual
        | BinaryOperator::Less
        | BinaryOperator::LessEqual
        | BinaryOperator::Greater
        | BinaryOperator::GreaterEqual => {
            expect_same_shape(lhs, rhs, at)?;
            if lhs_scalar != rhs_scalar && !(lhs_scalar.is_numeric() && rhs_scalar.is_numeric()) {
                return Err(type_mismatch(lhs_scalar, rhs_scalar, at));
            }
            Ok(ValueType::array(
                ScalarType::Boolean,
                lhs.dimensions().to_vec(),
            ))
        }
    }
}

fn expect_same_shape(
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if lhs.dimensions() == rhs.dimensions() {
        Ok(())
    } else {
        Err(DaeConstructionError::ShapeMismatch { span: at.span() })
    }
}

fn promoted_numeric_scalar(lhs: ScalarType, rhs: ScalarType, force_real: bool) -> ScalarType {
    if force_real || lhs == ScalarType::Real || rhs == ScalarType::Real {
        ScalarType::Real
    } else {
        ScalarType::Integer
    }
}

fn multiplication_result(
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_numeric(lhs.scalar_type(), at)?;
    expect_numeric(rhs.scalar_type(), at)?;
    let dimensions = match (lhs.dimensions(), rhs.dimensions()) {
        ([], rhs) => rhs.to_vec(),
        (lhs, []) => lhs.to_vec(),
        ([lhs_n], [rhs_n]) if lhs_n == rhs_n => Vec::new(),
        ([rows, inner], [rhs_inner]) if inner == rhs_inner => vec![*rows],
        ([lhs_inner], [rhs_inner, columns]) if lhs_inner == rhs_inner => vec![*columns],
        ([rows, inner], [rhs_inner, columns]) if inner == rhs_inner => {
            vec![*rows, *columns]
        }
        _ => return Err(DaeConstructionError::ShapeMismatch { span: at.span() }),
    };
    Ok(ValueType::array(
        promoted_numeric_scalar(lhs.scalar_type(), rhs.scalar_type(), false),
        dimensions,
    ))
}

fn division_result(
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_numeric(lhs.scalar_type(), at)?;
    expect_numeric(rhs.scalar_type(), at)?;
    if !rhs.is_scalar() {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    Ok(ValueType::array(
        ScalarType::Real,
        lhs.dimensions().to_vec(),
    ))
}

fn power_result(
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_numeric(lhs.scalar_type(), at)?;
    expect_numeric(rhs.scalar_type(), at)?;
    if !rhs.is_scalar()
        || !(lhs.is_scalar()
            || matches!(lhs.dimensions(), [rows, columns] if rows == columns)
                && rhs.scalar_type() == ScalarType::Integer)
    {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    Ok(ValueType::array(
        promoted_numeric_scalar(lhs.scalar_type(), rhs.scalar_type(), false),
        lhs.dimensions().to_vec(),
    ))
}

fn elementwise_result(
    operator: BinaryOperator,
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_numeric(lhs.scalar_type(), at)?;
    expect_numeric(rhs.scalar_type(), at)?;
    let dimensions = if lhs.is_scalar() {
        rhs.dimensions()
    } else if rhs.is_scalar() || lhs.dimensions() == rhs.dimensions() {
        lhs.dimensions()
    } else {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    };
    Ok(ValueType::array(
        promoted_numeric_scalar(
            lhs.scalar_type(),
            rhs.scalar_type(),
            matches!(operator, BinaryOperator::ElementwiseDivide),
        ),
        dimensions.to_vec(),
    ))
}

pub(super) fn common_value_type(
    lhs: &ValueType,
    rhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    if lhs.dimensions() != rhs.dimensions() {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    let scalar = if lhs.scalar_type() == rhs.scalar_type() {
        lhs.scalar_type()
    } else if lhs.scalar_type().is_numeric() && rhs.scalar_type().is_numeric() {
        ScalarType::Real
    } else {
        return Err(type_mismatch(lhs.scalar_type(), rhs.scalar_type(), at));
    };
    Ok(ValueType::array(scalar, lhs.dimensions().to_vec()))
}

fn expect_numeric(scalar: ScalarType, at: DaeProvenance) -> Result<(), DaeConstructionError> {
    if scalar.is_numeric() {
        Ok(())
    } else {
        Err(DaeConstructionError::ExpectedNumeric {
            found: scalar,
            span: at.span(),
        })
    }
}

pub(super) fn builtin_result<'dae>(
    storage: &Storage,
    builtin: PureBuiltin,
    arguments: &[ExprId<'dae>],
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    let Some(first) = arguments.first().copied() else {
        return Err(invalid_arity(1, 0, at));
    };
    let first = storage.expr_type(first, at)?.clone();
    if builtin.has_shaped_result() {
        return shaped_builtin_result(storage, builtin, arguments, &first, at);
    }
    if builtin == PureBuiltin::NoEvent {
        expect_arity(arguments, 1, at)?;
        return Ok(first);
    }
    if builtin == PureBuiltin::Homotopy {
        return homotopy_result(storage, arguments, first, at);
    }
    if builtin == PureBuiltin::Size {
        return size_result(storage, arguments, first, at);
    }
    expect_numeric(first.scalar_type(), at)?;
    match builtin {
        PureBuiltin::Abs
        | PureBuiltin::Sign
        | PureBuiltin::Sqrt
        | PureBuiltin::Floor
        | PureBuiltin::Ceil
        | PureBuiltin::Sin
        | PureBuiltin::Cos
        | PureBuiltin::Tan
        | PureBuiltin::Asin
        | PureBuiltin::Acos
        | PureBuiltin::Atan
        | PureBuiltin::Sinh
        | PureBuiltin::Cosh
        | PureBuiltin::Tanh
        | PureBuiltin::Exp
        | PureBuiltin::Log
        | PureBuiltin::Log10 => {
            expect_arity(arguments, 1, at)?;
            Ok(first)
        }
        PureBuiltin::Integer => {
            expect_arity(arguments, 1, at)?;
            if !first.is_scalar() {
                return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
            }
            Ok(ValueType::scalar(ScalarType::Integer))
        }
        PureBuiltin::Atan2 | PureBuiltin::Div | PureBuiltin::Mod | PureBuiltin::Rem => {
            expect_arity(arguments, 2, at)?;
            common_value_type(&first, storage.expr_type(arguments[1], at)?, at)
        }
        PureBuiltin::Homotopy => unreachable!("homotopy returns after checking both branches"),
        PureBuiltin::Smooth => {
            expect_arity(arguments, 2, at)?;
            if !first.is_scalar() || first.scalar_type() != ScalarType::Integer {
                return Err(DaeConstructionError::InvalidSubscript { span: at.span() });
            }
            Ok(storage.expr_type(arguments[1], at)?.clone())
        }
        PureBuiltin::Sum | PureBuiltin::Product => {
            expect_arity(arguments, 1, at)?;
            Ok(ValueType::scalar(first.scalar_type()))
        }
        PureBuiltin::Min | PureBuiltin::Max if arguments.len() == 1 => {
            Ok(ValueType::scalar(first.scalar_type()))
        }
        PureBuiltin::Min | PureBuiltin::Max => {
            for argument in &arguments[1..] {
                if storage.expr_type(*argument, at)? != &first {
                    return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
                }
            }
            Ok(first)
        }
        PureBuiltin::Size => unreachable!("size returns after checking its dimension argument"),
        PureBuiltin::Zeros
        | PureBuiltin::Ones
        | PureBuiltin::Fill
        | PureBuiltin::Linspace
        | PureBuiltin::Cross => {
            unreachable!("array constructors return before numeric builtins")
        }
        PureBuiltin::NoEvent => {
            unreachable!("type-preserving noEvent returns before numeric builtin checks")
        }
    }
}

fn homotopy_result(
    storage: &Storage,
    arguments: &[ExprId<'_>],
    actual: ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_arity(arguments, 2, at)?;
    let simplified = storage.expr_type(arguments[1], at)?;
    if !actual.is_scalar() || actual.scalar_type() != ScalarType::Real || simplified != &actual {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    Ok(actual)
}

fn size_result(
    storage: &Storage,
    arguments: &[ExprId<'_>],
    array: ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    if arguments.len() == 1 {
        let rank = u32::try_from(array.dimensions().len()).map_err(|_| {
            DaeConstructionError::CapacityExceeded {
                arena: "value type rank",
                attempted_index: array.dimensions().len(),
                span: at.span(),
            }
        })?;
        return Ok(ValueType::array(ScalarType::Integer, [rank]));
    }
    expect_arity(arguments, 2, at)?;
    let dimension = storage.expr_type(arguments[1], at)?;
    if !dimension.is_scalar() || dimension.scalar_type() != ScalarType::Integer {
        return Err(DaeConstructionError::InvalidSubscript { span: at.span() });
    }
    Ok(ValueType::scalar(ScalarType::Integer))
}

fn shaped_builtin_result(
    storage: &Storage,
    builtin: PureBuiltin,
    arguments: &[ExprId<'_>],
    first: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    let (scalar, extents) = match builtin {
        PureBuiltin::Zeros | PureBuiltin::Ones => (ScalarType::Real, arguments),
        PureBuiltin::Fill if arguments.len() >= 2 && first.is_scalar() => {
            (first.scalar_type(), &arguments[1..])
        }
        PureBuiltin::Fill if arguments.len() < 2 => {
            return Err(invalid_arity(2, arguments.len(), at));
        }
        PureBuiltin::Fill => return Err(DaeConstructionError::ShapeMismatch { span: at.span() }),
        PureBuiltin::Linspace => {
            expect_arity(arguments, 3, at)?;
            let endpoints = common_value_type(first, storage.expr_type(arguments[1], at)?, at)?;
            let extent = literal_array_extent(storage, arguments[2], at)?;
            if !endpoints.is_scalar() {
                return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
            }
            if extent < 2 {
                return Err(DaeConstructionError::InvalidArrayExtent { span: at.span() });
            }
            return Ok(ValueType::array(ScalarType::Real, [extent]));
        }
        PureBuiltin::Cross => {
            expect_arity(arguments, 2, at)?;
            let result = common_value_type(first, storage.expr_type(arguments[1], at)?, at)?;
            if result.dimensions() != [3] {
                return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
            }
            return Ok(result);
        }
        _ => unreachable!("only compact shaped builtins use this validator"),
    };
    let dimensions = extents
        .iter()
        .copied()
        .map(|expression| literal_array_extent(storage, expression, at))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(ValueType::array(scalar, dimensions))
}

fn literal_array_extent(
    storage: &Storage,
    expression: ExprId<'_>,
    at: DaeProvenance,
) -> Result<u32, DaeConstructionError> {
    let value_type = storage.expr_type(expression, at)?;
    if !value_type.is_scalar() || value_type.scalar_type() != ScalarType::Integer {
        return Err(DaeConstructionError::InvalidArrayExtent { span: at.span() });
    }
    u32::try_from(
        storage
            .static_integer(expression)
            .ok_or(DaeConstructionError::InvalidArrayExtent { span: at.span() })?,
    )
    .map_err(|_| DaeConstructionError::InvalidArrayExtent { span: at.span() })
}

fn expect_arity(
    arguments: &[ExprId<'_>],
    expected: usize,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if arguments.len() == expected {
        return Ok(());
    }
    Err(invalid_arity(expected, arguments.len(), at))
}

pub(super) fn range_extent(
    start: i64,
    step: i64,
    stop: i64,
    at: DaeProvenance,
) -> Result<u32, DaeConstructionError> {
    if (step > 0 && start > stop) || (step < 0 && start < stop) {
        return Ok(0);
    }
    let distance = i128::from(stop) - i128::from(start);
    let steps = distance / i128::from(step);
    u32::try_from(steps + 1)
        .map_err(|_| DaeConstructionError::RangeExtentOverflow { span: at.span() })
}

pub(super) fn type_mismatch(
    expected: ScalarType,
    found: ScalarType,
    at: DaeProvenance,
) -> DaeConstructionError {
    DaeConstructionError::TypeMismatch {
        expected,
        found,
        span: at.span(),
    }
}

pub(super) fn validate_subscript<'dae>(
    storage: &Storage,
    expression: ExprId<'dae>,
    expect_scalar: bool,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let ty = storage.expr_type(expression, at)?;
    let scalar_matches = ty.is_scalar() == expect_scalar;
    if matches!(
        ty.scalar_type(),
        ScalarType::Integer | ScalarType::Enumeration
    ) && scalar_matches
    {
        return Ok(());
    }
    Err(DaeConstructionError::InvalidSubscript { span: at.span() })
}
