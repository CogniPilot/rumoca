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

pub(super) fn validate_runtime_quotient(
    storage: &Storage,
    builtin: PureBuiltin,
    arguments: &[ExprId<'_>],
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if !matches!(
        builtin,
        PureBuiltin::Div | PureBuiltin::Mod | PureBuiltin::Rem
    ) {
        return Err(DaeConstructionError::InvalidExpressionForm { span: at.span() });
    }
    let [lhs, rhs] = arguments else {
        return Err(invalid_arity(2, arguments.len(), at));
    };
    if !storage.expr_type(*lhs, at)?.is_scalar() || !storage.expr_type(*rhs, at)?.is_scalar() {
        return Err(DaeConstructionError::ExpectedScalar { span: at.span() });
    }
    let Some(divisor) = static_numeric_value(storage, rhs.index()) else {
        return Err(DaeConstructionError::NonStaticDiscontinuity {
            operator: quotient_name(builtin),
            span: at.span(),
        });
    };
    if divisor == 0.0 || !divisor.is_finite() {
        return Err(DaeConstructionError::UndefinedBuiltinDomain {
            operator: quotient_name(builtin),
            span: at.span(),
        });
    }
    Ok(())
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
        _ => {
            return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
        }
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
    if lhs == rhs {
        return Ok(lhs.clone());
    }
    if lhs.dimensions() != rhs.dimensions() {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    if lhs.is_record() || rhs.is_record() {
        return Err(type_mismatch(lhs.scalar_type(), rhs.scalar_type(), at));
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
        let expected = if builtin == PureBuiltin::OuterProduct {
            2
        } else {
            1
        };
        return Err(invalid_arity(expected, 0, at));
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
    if builtin == PureBuiltin::Integer {
        return integer_result(arguments, &first, at);
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
            unreachable!("the Integer conversion returns before numeric builtin checks")
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
            arguments[1..].iter().try_fold(first, |common, argument| {
                common_value_type(&common, storage.expr_type(*argument, at)?, at)
            })
        }
        PureBuiltin::Size => unreachable!("size returns after checking its dimension argument"),
        PureBuiltin::Zeros
        | PureBuiltin::Ones
        | PureBuiltin::Fill
        | PureBuiltin::Linspace
        | PureBuiltin::Cross
        | PureBuiltin::PromotedCat1
        | PureBuiltin::PromotedCat2
        | PureBuiltin::Identity
        | PureBuiltin::Vector
        | PureBuiltin::Transpose
        | PureBuiltin::Diagonal
        | PureBuiltin::OuterProduct
        | PureBuiltin::Skew => {
            unreachable!("array constructors return before numeric builtins")
        }
        PureBuiltin::NoEvent => {
            unreachable!("type-preserving noEvent returns before numeric builtin checks")
        }
    }
}

/// MLS §4.9.5.2: `Integer(e)` is the Integer ordinal of an enumeration value,
/// and `integer(x)` is the Integer conversion of a numeric value. Both are
/// scalar and both produce Integer, so the enumeration operand is admitted here
/// instead of at the shared numeric gate, which every other builtin still uses.
fn integer_result(
    arguments: &[ExprId<'_>],
    actual: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_arity(arguments, 1, at)?;
    if !actual.is_scalar() {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    if !actual.scalar_type().is_numeric() && actual.scalar_type() != ScalarType::Enumeration {
        return Err(DaeConstructionError::ExpectedNumeric {
            found: actual.scalar_type(),
            span: at.span(),
        });
    }
    Ok(ValueType::scalar(ScalarType::Integer))
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
            real_three_vector(first, at)?;
            real_three_vector(storage.expr_type(arguments[1], at)?, at)?;
            return Ok(ValueType::array(ScalarType::Real, [3]));
        }
        PureBuiltin::PromotedCat1 | PureBuiltin::PromotedCat2 => {
            return promoted_concatenation_result(storage, builtin, arguments, at);
        }
        PureBuiltin::Identity => {
            expect_arity(arguments, 1, at)?;
            let extent = literal_array_extent(storage, arguments[0], at)?;
            return Ok(ValueType::array(ScalarType::Integer, [extent, extent]));
        }
        PureBuiltin::Vector => return vector_result(arguments, first, at),
        PureBuiltin::Transpose => return transpose_result(arguments, first, at),
        PureBuiltin::Diagonal => return diagonal_result(arguments, first, at),
        PureBuiltin::OuterProduct => {
            return outer_product_result(storage, arguments, first, at);
        }
        PureBuiltin::Skew => return skew_result(arguments, first, at),
        _ => unreachable!("only compact shaped builtins use this validator"),
    };
    let dimensions = extents
        .iter()
        .copied()
        .map(|expression| literal_array_extent(storage, expression, at))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(ValueType::array(scalar, dimensions))
}

/// Construct the exact rank-one type MLS §10.3.2 gives `vector(A)`.
///
/// At most one operand dimension may exceed one. Under that proof the checked
/// product is the unique vector extent, including the scalar (`1`) and empty
/// (`0`) cases. The caller supplies no result shape and the operation retains
/// only its compact operand.
fn vector_result(
    arguments: &[ExprId<'_>],
    input: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_arity(arguments, 1, at)?;
    // A record root currently has no aggregate dimensions: its field dimensions
    // cannot distinguish an array of records from a scalar record with intrinsic
    // array-valued fields. Reject until that ownership is represented explicitly.
    if input.is_record() {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    let extent = vector_extent(input.dimensions(), at)?;
    Ok(ValueType::array(input.scalar_type(), [extent]))
}

fn vector_extent(dimensions: &[u32], at: DaeProvenance) -> Result<u32, DaeConstructionError> {
    if dimensions.iter().filter(|&&extent| extent > 1).count() > 1 {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    dimensions.iter().try_fold(1_u32, |product, extent| {
        product
            .checked_mul(*extent)
            .ok_or_else(|| DaeConstructionError::CapacityExceeded {
                arena: "vector extent",
                attempted_index: (product as usize).saturating_mul(*extent as usize),
                span: at.span(),
            })
    })
}

/// Construct the exact primitive array type MLS §10.3.5 gives `transpose(A)`.
///
/// ARR-038 requires at least two dimensions. Only axes zero and one exchange
/// places, so higher-rank tensor extents remain compact and retain their order.
fn transpose_result(
    arguments: &[ExprId<'_>],
    input: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_arity(arguments, 1, at)?;
    if input.is_record() || input.dimensions().len() < 2 {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    let mut dimensions = input.dimensions().to_vec();
    dimensions.swap(0, 1);
    Ok(ValueType::array(input.scalar_type(), dimensions))
}

/// Construct the exact compact matrix type ARR-041 gives `diagonal(v)`.
///
/// The operand must be one primitive numeric vector. The constructor retains
/// that vector and derives both matrix extents from it, so neither lowering nor
/// wire replay can supply a contradictory result shape.
fn diagonal_result(
    arguments: &[ExprId<'_>],
    input: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_arity(arguments, 1, at)?;
    expect_numeric(input.scalar_type(), at)?;
    let [extent] = input.dimensions() else {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    };
    Ok(ValueType::array(input.scalar_type(), [*extent, *extent]))
}

/// Construct the exact compact matrix type ARR-042 gives
/// `outerProduct(v1, v2)`.
///
/// Both operands must be primitive numeric vectors. Their extents remain in
/// source order, while mixed Integer/Real element types promote to Real.
fn outer_product_result(
    storage: &Storage,
    arguments: &[ExprId<'_>],
    lhs: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_arity(arguments, 2, at)?;
    let rhs = storage.expr_type(arguments[1], at)?;
    expect_numeric(lhs.scalar_type(), at)?;
    expect_numeric(rhs.scalar_type(), at)?;
    let ([lhs_extent], [rhs_extent]) = (lhs.dimensions(), rhs.dimensions()) else {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    };
    Ok(ValueType::array(
        promoted_numeric_scalar(lhs.scalar_type(), rhs.scalar_type(), false),
        [*lhs_extent, *rhs_extent],
    ))
}

/// Construct the exact compact matrix type ARR-037 gives `skew(x)`.
///
/// MLS admits exactly one Real 3-vector. The operation retains that single
/// vector and derives its fixed matrix result, so no caller or wire payload can
/// claim a different rank, extent, or scalar type.
fn skew_result(
    arguments: &[ExprId<'_>],
    input: &ValueType,
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    expect_arity(arguments, 1, at)?;
    real_three_vector(input, at)?;
    Ok(ValueType::array(ScalarType::Real, [3, 3]))
}

fn real_three_vector(input: &ValueType, at: DaeProvenance) -> Result<(), DaeConstructionError> {
    expect_numeric(input.scalar_type(), at)?;
    if input.scalar_type() != ScalarType::Real {
        return Err(type_mismatch(ScalarType::Real, input.scalar_type(), at));
    }
    if input.dimensions() != [3] {
        return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
    }
    Ok(())
}

/// Construct the one exact type MLS §10.4.2.1 gives a promoted concatenation.
///
/// Promotion appends unit extents on the right until every operand has the
/// common rank `max(2, ndims(A), ndims(B), ...)`. Concatenation then requires
/// every non-concatenated extent to agree and sums only the selected extent.
/// Keeping this in the constructor means neither wire input nor a lowering
/// caller can mint a concatenation whose stored result type disagrees with its
/// operands.
fn promoted_concatenation_result(
    storage: &Storage,
    builtin: PureBuiltin,
    arguments: &[ExprId<'_>],
    at: DaeProvenance,
) -> Result<ValueType, DaeConstructionError> {
    let Some(first_id) = arguments.first().copied() else {
        return Err(invalid_arity(1, 0, at));
    };
    let axis = match builtin {
        PureBuiltin::PromotedCat1 => 0,
        PureBuiltin::PromotedCat2 => 1,
        _ => unreachable!("only promoted concatenation reaches this constructor"),
    };
    let rank = arguments.iter().try_fold(2_usize, |rank, argument| {
        let ty = storage.expr_type(*argument, at)?;
        if ty.is_record() {
            return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
        }
        Ok(rank.max(ty.dimensions().len()))
    })?;
    let first = storage.expr_type(first_id, at)?;
    let mut scalar = first.scalar_type();
    let mut result = promoted_dimensions(first.dimensions(), rank);
    for argument in &arguments[1..] {
        let ty = storage.expr_type(*argument, at)?;
        scalar = if scalar == ty.scalar_type() {
            scalar
        } else if scalar.is_numeric() && ty.scalar_type().is_numeric() {
            ScalarType::Real
        } else {
            return Err(type_mismatch(scalar, ty.scalar_type(), at));
        };
        let dimensions = promoted_dimensions(ty.dimensions(), rank);
        for dimension in 0..rank {
            if dimension != axis && result[dimension] != dimensions[dimension] {
                return Err(DaeConstructionError::ShapeMismatch { span: at.span() });
            }
        }
        let attempted_extent = (result[axis] as usize).saturating_add(dimensions[axis] as usize);
        result[axis] = result[axis].checked_add(dimensions[axis]).ok_or(
            DaeConstructionError::CapacityExceeded {
                arena: "concatenation extent",
                attempted_index: attempted_extent,
                span: at.span(),
            },
        )?;
    }
    Ok(ValueType::array(scalar, result))
}

fn promoted_dimensions(dimensions: &[u32], rank: usize) -> Vec<u32> {
    dimensions
        .iter()
        .copied()
        .chain(std::iter::repeat_n(1, rank - dimensions.len()))
        .collect()
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
