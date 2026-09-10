use rumoca_core::Span;
use rumoca_ir_solve::{
    SolveBinaryOperator, SolveCompareOperator, SolveConversionOperator, SolveIntegerDomain,
    SolveRealFormat, SolveScalarType, SolveUnaryOperator, SolveValueKind, SolveValueType,
};

use super::{TypedProgramEvalError, TypedValue, invalid};

pub(super) fn eval_unary_typed(
    operator: SolveUnaryOperator,
    value: &TypedValue,
    provenance: Span,
) -> Result<TypedValue, TypedProgramEvalError> {
    let scalar = value.value_type.element_type();
    let elements = value
        .elements
        .iter()
        .copied()
        .map(|element| eval_unary_element(operator, element, scalar, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    TypedValue::checked(value.value_type.clone(), elements, provenance)
}

fn eval_unary_element(
    operator: SolveUnaryOperator,
    value: SolveValueKind,
    scalar: SolveScalarType,
    provenance: Span,
) -> Result<SolveValueKind, TypedProgramEvalError> {
    match value {
        SolveValueKind::Real32(bits) => Ok(SolveValueKind::Real32(
            eval_real_unary_f32(operator, f32::from_bits(bits)).to_bits(),
        )),
        SolveValueKind::Real64(bits) => Ok(SolveValueKind::Real64(
            eval_real_unary_f64(operator, f64::from_bits(bits)).to_bits(),
        )),
        SolveValueKind::Integer(value) => eval_integer_unary(operator, value, scalar, provenance),
        SolveValueKind::Boolean(value) if operator == SolveUnaryOperator::Not => {
            Ok(SolveValueKind::Boolean(!value))
        }
        SolveValueKind::Boolean(_) => invalid("evaluate Boolean unary operation", provenance),
    }
}

fn eval_integer_unary(
    operator: SolveUnaryOperator,
    value: i64,
    scalar: SolveScalarType,
    provenance: Span,
) -> Result<SolveValueKind, TypedProgramEvalError> {
    let SolveScalarType::Integer(domain) = scalar else {
        return invalid("evaluate Integer unary operation", provenance);
    };
    let result = match operator {
        SolveUnaryOperator::Negate => value.checked_neg(),
        SolveUnaryOperator::Abs => value.checked_abs(),
        SolveUnaryOperator::Sign => Some(value.signum()),
        _ => None,
    }
    .filter(|result| domain.contains(*result))
    .ok_or(TypedProgramEvalError::IntegerArithmetic {
        operation: "unary operation",
        provenance,
    })?;
    Ok(SolveValueKind::Integer(result))
}

fn eval_real_unary_f32(operator: SolveUnaryOperator, value: f32) -> f32 {
    match operator {
        SolveUnaryOperator::Negate => -value,
        SolveUnaryOperator::Abs => value.abs(),
        SolveUnaryOperator::Sign => rumoca_core::modelica_sign(f64::from(value)) as f32,
        SolveUnaryOperator::Sqrt => value.sqrt(),
        SolveUnaryOperator::Floor => value.floor(),
        SolveUnaryOperator::Ceiling => value.ceil(),
        SolveUnaryOperator::Truncate => value.trunc(),
        SolveUnaryOperator::Sin => value.sin(),
        SolveUnaryOperator::Cos => value.cos(),
        SolveUnaryOperator::Tan => value.tan(),
        SolveUnaryOperator::Asin => value.asin(),
        SolveUnaryOperator::Acos => value.acos(),
        SolveUnaryOperator::Atan => value.atan(),
        SolveUnaryOperator::Sinh => value.sinh(),
        SolveUnaryOperator::Cosh => value.cosh(),
        SolveUnaryOperator::Tanh => value.tanh(),
        SolveUnaryOperator::Exp => value.exp(),
        SolveUnaryOperator::Log => value.ln(),
        SolveUnaryOperator::Log10 => value.log10(),
        SolveUnaryOperator::Not => f32::NAN,
    }
}

fn eval_real_unary_f64(operator: SolveUnaryOperator, value: f64) -> f64 {
    match operator {
        SolveUnaryOperator::Negate => -value,
        SolveUnaryOperator::Abs => value.abs(),
        SolveUnaryOperator::Sign => rumoca_core::modelica_sign(value),
        SolveUnaryOperator::Sqrt => value.sqrt(),
        SolveUnaryOperator::Floor => value.floor(),
        SolveUnaryOperator::Ceiling => value.ceil(),
        SolveUnaryOperator::Truncate => value.trunc(),
        SolveUnaryOperator::Sin => value.sin(),
        SolveUnaryOperator::Cos => value.cos(),
        SolveUnaryOperator::Tan => value.tan(),
        SolveUnaryOperator::Asin => value.asin(),
        SolveUnaryOperator::Acos => value.acos(),
        SolveUnaryOperator::Atan => value.atan(),
        SolveUnaryOperator::Sinh => value.sinh(),
        SolveUnaryOperator::Cosh => value.cosh(),
        SolveUnaryOperator::Tanh => value.tanh(),
        SolveUnaryOperator::Exp => value.exp(),
        SolveUnaryOperator::Log => value.ln(),
        SolveUnaryOperator::Log10 => value.log10(),
        SolveUnaryOperator::Not => f64::NAN,
    }
}

pub(super) fn eval_binary_typed(
    operator: SolveBinaryOperator,
    lhs: &TypedValue,
    rhs: &TypedValue,
    provenance: Span,
) -> Result<TypedValue, TypedProgramEvalError> {
    if lhs.value_type != rhs.value_type {
        return invalid("evaluate binary operation", provenance);
    }
    let scalar = lhs.value_type.element_type();
    let elements = lhs
        .elements
        .iter()
        .copied()
        .zip(rhs.elements.iter().copied())
        .map(|(lhs, rhs)| eval_binary_element(operator, lhs, rhs, scalar, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    TypedValue::checked(lhs.value_type.clone(), elements, provenance)
}

pub(super) fn eval_binary_element(
    operator: SolveBinaryOperator,
    lhs: SolveValueKind,
    rhs: SolveValueKind,
    scalar: SolveScalarType,
    provenance: Span,
) -> Result<SolveValueKind, TypedProgramEvalError> {
    match (lhs, rhs) {
        (SolveValueKind::Real32(lhs), SolveValueKind::Real32(rhs)) => Ok(SolveValueKind::Real32(
            eval_real_binary_f32(operator, f32::from_bits(lhs), f32::from_bits(rhs)).to_bits(),
        )),
        (SolveValueKind::Real64(lhs), SolveValueKind::Real64(rhs)) => Ok(SolveValueKind::Real64(
            eval_real_binary_f64(operator, f64::from_bits(lhs), f64::from_bits(rhs)).to_bits(),
        )),
        (SolveValueKind::Integer(lhs), SolveValueKind::Integer(rhs)) => {
            eval_integer_binary(operator, lhs, rhs, scalar, provenance)
        }
        (SolveValueKind::Boolean(lhs), SolveValueKind::Boolean(rhs)) => match operator {
            SolveBinaryOperator::And => Ok(SolveValueKind::Boolean(lhs && rhs)),
            SolveBinaryOperator::Or => Ok(SolveValueKind::Boolean(lhs || rhs)),
            _ => invalid("evaluate Boolean binary operation", provenance),
        },
        _ => invalid("evaluate binary operation", provenance),
    }
}

fn eval_integer_binary(
    operator: SolveBinaryOperator,
    lhs: i64,
    rhs: i64,
    scalar: SolveScalarType,
    provenance: Span,
) -> Result<SolveValueKind, TypedProgramEvalError> {
    let SolveScalarType::Integer(domain) = scalar else {
        return invalid("evaluate Integer binary operation", provenance);
    };
    let result = match operator {
        SolveBinaryOperator::Add => lhs.checked_add(rhs),
        SolveBinaryOperator::Subtract => lhs.checked_sub(rhs),
        SolveBinaryOperator::Multiply => lhs.checked_mul(rhs),
        SolveBinaryOperator::Min => Some(lhs.min(rhs)),
        SolveBinaryOperator::Max => Some(lhs.max(rhs)),
        _ => None,
    }
    .filter(|result| domain.contains(*result))
    .ok_or(TypedProgramEvalError::IntegerArithmetic {
        operation: "binary operation",
        provenance,
    })?;
    Ok(SolveValueKind::Integer(result))
}

fn eval_real_binary_f32(operator: SolveBinaryOperator, lhs: f32, rhs: f32) -> f32 {
    match operator {
        SolveBinaryOperator::Add => lhs + rhs,
        SolveBinaryOperator::Subtract => lhs - rhs,
        SolveBinaryOperator::Multiply => lhs * rhs,
        SolveBinaryOperator::Divide => lhs / rhs,
        SolveBinaryOperator::Power => lhs.powf(rhs),
        SolveBinaryOperator::Atan2 => lhs.atan2(rhs),
        SolveBinaryOperator::Min => lhs.min(rhs),
        SolveBinaryOperator::Max => lhs.max(rhs),
        SolveBinaryOperator::And | SolveBinaryOperator::Or => f32::NAN,
    }
}

fn eval_real_binary_f64(operator: SolveBinaryOperator, lhs: f64, rhs: f64) -> f64 {
    match operator {
        SolveBinaryOperator::Add => lhs + rhs,
        SolveBinaryOperator::Subtract => lhs - rhs,
        SolveBinaryOperator::Multiply => lhs * rhs,
        SolveBinaryOperator::Divide => lhs / rhs,
        SolveBinaryOperator::Power => lhs.powf(rhs),
        SolveBinaryOperator::Atan2 => lhs.atan2(rhs),
        SolveBinaryOperator::Min => lhs.min(rhs),
        SolveBinaryOperator::Max => lhs.max(rhs),
        SolveBinaryOperator::And | SolveBinaryOperator::Or => f64::NAN,
    }
}

pub(super) fn eval_compare_typed(
    operator: SolveCompareOperator,
    lhs: &TypedValue,
    rhs: &TypedValue,
    provenance: Span,
) -> Result<TypedValue, TypedProgramEvalError> {
    if lhs.value_type != rhs.value_type {
        return invalid("evaluate comparison", provenance);
    }
    let elements = lhs
        .elements
        .iter()
        .copied()
        .zip(rhs.elements.iter().copied())
        .map(|(lhs, rhs)| compare_elements(operator, lhs, rhs, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    TypedValue::checked(
        lhs.value_type.boolean_with_same_shape(),
        elements,
        provenance,
    )
}

fn compare_elements(
    operator: SolveCompareOperator,
    lhs: SolveValueKind,
    rhs: SolveValueKind,
    provenance: Span,
) -> Result<SolveValueKind, TypedProgramEvalError> {
    let result = match (lhs, rhs) {
        (SolveValueKind::Real32(lhs), SolveValueKind::Real32(rhs)) => {
            compare_ordered(operator, f32::from_bits(lhs), f32::from_bits(rhs))
        }
        (SolveValueKind::Real64(lhs), SolveValueKind::Real64(rhs)) => {
            compare_ordered(operator, f64::from_bits(lhs), f64::from_bits(rhs))
        }
        (SolveValueKind::Integer(lhs), SolveValueKind::Integer(rhs)) => {
            compare_ordered(operator, lhs, rhs)
        }
        (SolveValueKind::Boolean(lhs), SolveValueKind::Boolean(rhs)) => match operator {
            SolveCompareOperator::Equal => lhs == rhs,
            SolveCompareOperator::NotEqual => lhs != rhs,
            _ => return invalid("order Boolean values", provenance),
        },
        _ => return invalid("compare mismatched values", provenance),
    };
    Ok(SolveValueKind::Boolean(result))
}

fn compare_ordered<T: PartialOrd + PartialEq>(
    operator: SolveCompareOperator,
    lhs: T,
    rhs: T,
) -> bool {
    match operator {
        SolveCompareOperator::Equal => lhs == rhs,
        SolveCompareOperator::NotEqual => lhs != rhs,
        SolveCompareOperator::Less => lhs < rhs,
        SolveCompareOperator::LessEqual => lhs <= rhs,
        SolveCompareOperator::Greater => lhs > rhs,
        SolveCompareOperator::GreaterEqual => lhs >= rhs,
    }
}

pub(super) fn eval_convert_typed(
    operator: SolveConversionOperator,
    value: &TypedValue,
    destination_type: SolveValueType,
    provenance: Span,
) -> Result<TypedValue, TypedProgramEvalError> {
    let scalar = destination_type.element_type();
    let elements = value
        .elements
        .iter()
        .copied()
        .map(|element| convert_element(operator, element, scalar, provenance))
        .collect::<Result<Vec<_>, _>>()?;
    TypedValue::checked(destination_type, elements, provenance)
}

fn convert_element(
    operator: SolveConversionOperator,
    value: SolveValueKind,
    destination: SolveScalarType,
    provenance: Span,
) -> Result<SolveValueKind, TypedProgramEvalError> {
    match (operator, value, destination) {
        (
            SolveConversionOperator::IntegerToReal,
            SolveValueKind::Integer(value),
            SolveScalarType::Real { format, .. },
        ) => Ok(match format {
            SolveRealFormat::Binary32 => SolveValueKind::Real32((value as f32).to_bits()),
            SolveRealFormat::Binary64 => SolveValueKind::Real64((value as f64).to_bits()),
        }),
        (
            SolveConversionOperator::RealToIntegerTowardZero,
            SolveValueKind::Real32(bits),
            SolveScalarType::Integer(domain),
        ) => convert_real_to_integer(f64::from(f32::from_bits(bits)).trunc(), domain, provenance),
        (
            SolveConversionOperator::RealToIntegerTowardNegativeInfinity,
            SolveValueKind::Real32(bits),
            SolveScalarType::Integer(domain),
        ) => convert_real_to_integer(f64::from(f32::from_bits(bits)).floor(), domain, provenance),
        (
            SolveConversionOperator::RealToIntegerTowardZero,
            SolveValueKind::Real64(bits),
            SolveScalarType::Integer(domain),
        ) => convert_real_to_integer(f64::from_bits(bits).trunc(), domain, provenance),
        (
            SolveConversionOperator::RealToIntegerTowardNegativeInfinity,
            SolveValueKind::Real64(bits),
            SolveScalarType::Integer(domain),
        ) => convert_real_to_integer(f64::from_bits(bits).floor(), domain, provenance),
        _ => invalid("convert typed value", provenance),
    }
}

fn convert_real_to_integer(
    value: f64,
    domain: SolveIntegerDomain,
    provenance: Span,
) -> Result<SolveValueKind, TypedProgramEvalError> {
    let below_minimum = value < domain.minimum() as f64;
    let above_maximum = if domain.maximum() == i64::MAX {
        value >= 9_223_372_036_854_775_808.0
    } else {
        value > domain.maximum() as f64
    };
    if !value.is_finite() || below_minimum || above_maximum {
        return Err(TypedProgramEvalError::InvalidIntegerConversion { provenance });
    }
    let value = value as i64;
    if !domain.contains(value) {
        return Err(TypedProgramEvalError::InvalidIntegerConversion { provenance });
    }
    Ok(SolveValueKind::Integer(value))
}
