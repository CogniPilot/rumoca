//! Closed scalar print items: constants, selects, elementwise kernels and
//! conversions, each with the typed operator token and the fault checks the
//! checked program and its arithmetic profile issue. A printer has one fixed
//! spelling per token and decides nothing; a combination no token covers
//! stays a legacy item.

use serde::Serialize;

use super::super::{
    SolveBinaryOperator, SolveCompareOperator, SolveConversionOperator, SolveOperation,
    SolveRegisterId, SolveUnaryOperator, SolveValueType,
};
use super::SolveBodyItem;
use crate::{SolveIntegerDomain, SolveScalarType, SolveValueKind};

/// A 64-bit integer literal as its sign class and magnitude, so a printer
/// spells each class once and computes nothing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(tag = "sign", rename_all = "snake_case")]
pub enum SolveIntegerLiteral {
    /// The minimum, whose magnitude has no positive 64-bit value.
    Minimum,
    Negative {
        magnitude: u64,
    },
    Nonnegative {
        magnitude: u64,
    },
}

impl SolveIntegerLiteral {
    const fn of(value: i64) -> Self {
        if value == i64::MIN {
            Self::Minimum
        } else if value < 0 {
            Self::Negative {
                magnitude: value.unsigned_abs(),
            }
        } else {
            Self::Nonnegative {
                magnitude: value.unsigned_abs(),
            }
        }
    }
}

/// An issued literal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SolveLiteral {
    Real64 { bits: u64 },
    Int64 { value: SolveIntegerLiteral },
    Boolean { value: bool },
}

/// Register `register` read at the running element, or at element zero
/// when it is broadcast.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SolveElementOperand {
    register: usize,
    broadcast: bool,
}

impl SolveElementOperand {
    const fn element(register: SolveRegisterId) -> Self {
        Self {
            register: register.index(),
            broadcast: false,
        }
    }

    const fn broadcast(register: SolveRegisterId) -> Self {
        Self {
            register: register.index(),
            broadcast: true,
        }
    }

    #[must_use]
    pub const fn register(&self) -> usize {
        self.register
    }

    #[must_use]
    pub const fn is_broadcast(&self) -> bool {
        self.broadcast
    }
}

/// A unary operator typed by its element: integer and real `abs` are
/// distinct tokens.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveUnaryToken {
    Negate,
    Not,
    Sign,
    IntegerAbs,
    RealAbs,
    Ceiling,
    Truncate,
    Sqrt,
    Floor,
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Sinh,
    Cosh,
    Tanh,
    Exp,
    Log,
    Log10,
}

/// A real or Boolean binary operator typed by its result element.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveBinaryToken {
    Add,
    Subtract,
    Multiply,
    Divide,
    And,
    Or,
    Power,
    Atan2,
    RealMin,
    RealMax,
    BooleanMin,
    BooleanMax,
}

/// A 64-bit integer binary operator; each carries its overflow and
/// division faults.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveCheckedToken {
    Add,
    Subtract,
    Multiply,
    Quotient,
    Modulo,
    Remainder,
    Min,
    Max,
}

/// One elementwise kernel.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SolveKernel {
    /// `min_operand_check`: the integer operand's minimum has no result.
    Unary {
        token: SolveUnaryToken,
        operand: SolveElementOperand,
        min_operand_check: bool,
    },
    Binary {
        token: SolveBinaryToken,
        lhs: SolveElementOperand,
        rhs: SolveElementOperand,
    },
    Checked {
        token: SolveCheckedToken,
        lhs: SolveElementOperand,
        rhs: SolveElementOperand,
    },
    Compare {
        token: SolveCompareOperator,
        lhs: SolveElementOperand,
        rhs: SolveElementOperand,
    },
}

/// An issued conversion.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SolveConversionToken {
    IntegerToReal,
    RealToIntegerTowardZero,
    RealToIntegerTowardNegativeInfinity,
}

/// The integer domain an integer result is checked against, when narrower
/// than the full 64-bit domain.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SolveDomainCheck {
    minimum: SolveIntegerLiteral,
    maximum: SolveIntegerLiteral,
}

impl SolveDomainCheck {
    pub(in crate::typed_program) fn of(value_type: &SolveValueType) -> Option<Self> {
        match value_type.element_type() {
            SolveScalarType::Integer(domain) if domain != SolveIntegerDomain::FULL => Some(Self {
                minimum: SolveIntegerLiteral::of(domain.minimum()),
                maximum: SolveIntegerLiteral::of(domain.maximum()),
            }),
            _ => None,
        }
    }

    #[must_use]
    pub const fn minimum(&self) -> SolveIntegerLiteral {
        self.minimum
    }

    #[must_use]
    pub const fn maximum(&self) -> SolveIntegerLiteral {
        self.maximum
    }
}

/// The closed scalar item of operation `index`, when its whole printing is
/// issued; `None` keeps the operation a legacy item.
pub(super) fn scalar_item(
    index: usize,
    operation: &SolveOperation,
    register_types: &[SolveValueType],
) -> Option<SolveBodyItem> {
    let value_type = |register: &SolveRegisterId| register_types.get(register.index());
    match operation {
        SolveOperation::Constant { destination, value } => Some(SolveBodyItem::Constant {
            index,
            destination: destination.index(),
            value: literal(value.kind())?,
        }),
        SolveOperation::Select {
            destination,
            condition,
            if_true,
            if_false,
        } => Some(SolveBodyItem::Select {
            index,
            destination: destination.index(),
            condition: condition.index(),
            if_true: if_true.index(),
            if_false: if_false.index(),
        }),
        SolveOperation::Unary {
            destination,
            operator,
            operand,
        } => {
            let destination_type = value_type(destination)?;
            Some(elementwise(
                index,
                *destination,
                destination_type,
                unary_kernel(*operator, destination_type, *operand),
            ))
        }
        SolveOperation::Binary {
            destination,
            operator,
            lhs,
            rhs,
        } => {
            let destination_type = value_type(destination)?;
            let (lhs, rhs) = (
                SolveElementOperand::element(*lhs),
                SolveElementOperand::element(*rhs),
            );
            let kernel = binary_kernel(*operator, destination_type, lhs, rhs)?;
            Some(elementwise(index, *destination, destination_type, kernel))
        }
        SolveOperation::BroadcastBinary {
            destination,
            operator,
            aggregate,
            scalar,
            scalar_on_lhs,
        } => {
            let destination_type = value_type(destination)?;
            let element = SolveElementOperand::element(*aggregate);
            let broadcast = SolveElementOperand::broadcast(*scalar);
            let (lhs, rhs) = if *scalar_on_lhs {
                (broadcast, element)
            } else {
                (element, broadcast)
            };
            let kernel = binary_kernel(*operator, destination_type, lhs, rhs)?;
            Some(elementwise(index, *destination, destination_type, kernel))
        }
        SolveOperation::Compare {
            destination,
            operator,
            lhs,
            rhs,
        } => {
            let kernel = SolveKernel::Compare {
                token: *operator,
                lhs: SolveElementOperand::element(*lhs),
                rhs: SolveElementOperand::element(*rhs),
            };
            Some(elementwise(
                index,
                *destination,
                value_type(destination)?,
                kernel,
            ))
        }
        SolveOperation::Convert {
            destination,
            operator,
            operand,
        } => Some(convert(
            index,
            *destination,
            *operator,
            *operand,
            value_type(destination)?,
        )),
        _ => None,
    }
}

/// The literal of a constant, or `None` for a format this projection does
/// not issue.
const fn literal(kind: SolveValueKind) -> Option<SolveLiteral> {
    Some(match kind {
        SolveValueKind::Real64(bits) => SolveLiteral::Real64 { bits },
        SolveValueKind::Integer(value) => SolveLiteral::Int64 {
            value: SolveIntegerLiteral::of(value),
        },
        SolveValueKind::Boolean(value) => SolveLiteral::Boolean { value },
        SolveValueKind::Real32(_) => return None,
    })
}

fn unary_kernel(
    operator: SolveUnaryOperator,
    destination: &SolveValueType,
    operand: SolveRegisterId,
) -> SolveKernel {
    let integer = matches!(destination.element_type(), SolveScalarType::Integer(_));
    SolveKernel::Unary {
        token: unary_token(operator, integer),
        operand: SolveElementOperand::element(operand),
        min_operand_check: integer
            && matches!(
                operator,
                SolveUnaryOperator::Negate | SolveUnaryOperator::Abs
            ),
    }
}

fn convert(
    index: usize,
    destination: SolveRegisterId,
    operator: SolveConversionOperator,
    operand: SolveRegisterId,
    destination_type: &SolveValueType,
) -> SolveBodyItem {
    let token = match operator {
        SolveConversionOperator::IntegerToReal => SolveConversionToken::IntegerToReal,
        SolveConversionOperator::RealToIntegerTowardZero => {
            SolveConversionToken::RealToIntegerTowardZero
        }
        SolveConversionOperator::RealToIntegerTowardNegativeInfinity => {
            SolveConversionToken::RealToIntegerTowardNegativeInfinity
        }
    };
    SolveBodyItem::Convert {
        index,
        destination: destination.index(),
        operand: operand.index(),
        extent: destination_type.scalar_count(),
        token,
        domain: match token {
            SolveConversionToken::IntegerToReal => None,
            _ => SolveDomainCheck::of(destination_type),
        },
    }
}

fn elementwise(
    index: usize,
    destination: SolveRegisterId,
    destination_type: &SolveValueType,
    kernel: SolveKernel,
) -> SolveBodyItem {
    SolveBodyItem::Elementwise {
        index,
        destination: destination.index(),
        extent: destination_type.scalar_count(),
        kernel,
        domain: SolveDomainCheck::of(destination_type),
    }
}

const fn unary_token(operator: SolveUnaryOperator, integer: bool) -> SolveUnaryToken {
    match operator {
        SolveUnaryOperator::Negate => SolveUnaryToken::Negate,
        SolveUnaryOperator::Not => SolveUnaryToken::Not,
        SolveUnaryOperator::Sign => SolveUnaryToken::Sign,
        SolveUnaryOperator::Abs if integer => SolveUnaryToken::IntegerAbs,
        SolveUnaryOperator::Abs => SolveUnaryToken::RealAbs,
        SolveUnaryOperator::Ceiling => SolveUnaryToken::Ceiling,
        SolveUnaryOperator::Truncate => SolveUnaryToken::Truncate,
        SolveUnaryOperator::Sqrt => SolveUnaryToken::Sqrt,
        SolveUnaryOperator::Floor => SolveUnaryToken::Floor,
        SolveUnaryOperator::Sin => SolveUnaryToken::Sin,
        SolveUnaryOperator::Cos => SolveUnaryToken::Cos,
        SolveUnaryOperator::Tan => SolveUnaryToken::Tan,
        SolveUnaryOperator::Asin => SolveUnaryToken::Asin,
        SolveUnaryOperator::Acos => SolveUnaryToken::Acos,
        SolveUnaryOperator::Atan => SolveUnaryToken::Atan,
        SolveUnaryOperator::Sinh => SolveUnaryToken::Sinh,
        SolveUnaryOperator::Cosh => SolveUnaryToken::Cosh,
        SolveUnaryOperator::Tanh => SolveUnaryToken::Tanh,
        SolveUnaryOperator::Exp => SolveUnaryToken::Exp,
        SolveUnaryOperator::Log => SolveUnaryToken::Log,
        SolveUnaryOperator::Log10 => SolveUnaryToken::Log10,
    }
}

/// The kernel of `operator` for a `destination` result, or `None` for a
/// combination this projection does not issue.
fn binary_kernel(
    operator: SolveBinaryOperator,
    destination: &SolveValueType,
    lhs: SolveElementOperand,
    rhs: SolveElementOperand,
) -> Option<SolveKernel> {
    use SolveBinaryOperator as Operator;
    use SolveBinaryToken as Token;
    use SolveCheckedToken as Checked;
    let checked = |token| Some(SolveKernel::Checked { token, lhs, rhs });
    let token = match (destination.element_type(), operator) {
        (SolveScalarType::Integer(_), operator) => {
            return match operator {
                Operator::Add => checked(Checked::Add),
                Operator::Subtract => checked(Checked::Subtract),
                Operator::Multiply => checked(Checked::Multiply),
                Operator::IntegerQuotient => checked(Checked::Quotient),
                Operator::IntegerModulo => checked(Checked::Modulo),
                Operator::IntegerRemainder => checked(Checked::Remainder),
                Operator::Min => checked(Checked::Min),
                Operator::Max => checked(Checked::Max),
                _ => None,
            };
        }
        (SolveScalarType::Boolean, Operator::Min) => Token::BooleanMin,
        (SolveScalarType::Boolean, Operator::Max) => Token::BooleanMax,
        (_, Operator::Add) => Token::Add,
        (_, Operator::Subtract) => Token::Subtract,
        (_, Operator::Multiply) => Token::Multiply,
        (_, Operator::Divide) => Token::Divide,
        (_, Operator::And) => Token::And,
        (_, Operator::Or) => Token::Or,
        (_, Operator::Power) => Token::Power,
        (_, Operator::Atan2) => Token::Atan2,
        (_, Operator::Min) => Token::RealMin,
        (_, Operator::Max) => Token::RealMax,
        (_, Operator::IntegerQuotient | Operator::IntegerModulo | Operator::IntegerRemainder) => {
            return None;
        }
    };
    Some(SolveKernel::Binary { token, lhs, rhs })
}
