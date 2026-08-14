//! Tensor-native expression lowering.

use super::*;

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    // SPEC_0021: Exception - exhaustive binary-operator lowering dispatch.
    #[allow(clippy::too_many_lines)]
    pub(super) fn binary(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        let mut lhs_value = self.expression(lhs)?.only_register(at)?;
        let mut rhs_value = self.expression(rhs)?.only_register(at)?;
        let lhs_type = self
            .view
            .expression(lhs)
            .expect("checked lhs resolves")
            .value_type();
        let rhs_type = self
            .view
            .expression(rhs)
            .expect("checked rhs resolves")
            .value_type();
        let division = matches!(
            operator,
            dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide
        );
        let lhs_integral = matches!(
            lhs_type.scalar_type(),
            dae::ScalarType::Integer | dae::ScalarType::Enumeration
        );
        let rhs_integral = matches!(
            rhs_type.scalar_type(),
            dae::ScalarType::Integer | dae::ScalarType::Enumeration
        );
        if lhs_integral && (division || rhs_type.scalar_type() == dae::ScalarType::Real) {
            lhs_value = self.builder.convert(
                solve::SolveConversionOperator::IntegerToReal,
                lhs_value,
                at,
            )?;
        }
        if rhs_integral && (division || lhs_type.scalar_type() == dae::ScalarType::Real) {
            rhs_value = self.builder.convert(
                solve::SolveConversionOperator::IntegerToReal,
                rhs_value,
                at,
            )?;
        }
        let register = match operator {
            dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply
                if lhs_type.is_scalar() && !rhs_type.is_scalar() =>
            {
                self.builder.scale(rhs_value, lhs_value, at)
            }
            dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply
                if !lhs_type.is_scalar() && rhs_type.is_scalar() =>
            {
                self.builder.scale(lhs_value, rhs_value, at)
            }
            dae::BinaryOperator::ElementwiseAdd
            | dae::BinaryOperator::ElementwiseSubtract
            | dae::BinaryOperator::ElementwiseDivide
            | dae::BinaryOperator::ElementwisePower
                if !lhs_type.is_scalar() && rhs_type.is_scalar() =>
            {
                self.builder.broadcast_binary(
                    binary_operator(operator)?,
                    lhs_value,
                    rhs_value,
                    false,
                    at,
                )
            }
            dae::BinaryOperator::ElementwiseAdd
            | dae::BinaryOperator::ElementwiseSubtract
            | dae::BinaryOperator::ElementwiseDivide
            | dae::BinaryOperator::ElementwisePower
                if lhs_type.is_scalar() && !rhs_type.is_scalar() =>
            {
                self.builder.broadcast_binary(
                    binary_operator(operator)?,
                    rhs_value,
                    lhs_value,
                    true,
                    at,
                )
            }
            dae::BinaryOperator::Multiply if !lhs_type.is_scalar() && !rhs_type.is_scalar() => {
                self.builder.matrix_multiply(lhs_value, rhs_value, at)
            }
            dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide
                if !lhs_type.is_scalar() && rhs_type.is_scalar() =>
            {
                let one = self
                    .builder
                    .constant(solve::SolveValue::real(arithmetic_profile(), 1.0), at)?;
                let reciprocal =
                    self.builder
                        .binary(solve::SolveBinaryOperator::Divide, one, rhs_value, at)?;
                self.builder.scale(lhs_value, reciprocal, at)
            }
            dae::BinaryOperator::Equal
            | dae::BinaryOperator::NotEqual
            | dae::BinaryOperator::Less
            | dae::BinaryOperator::LessEqual
            | dae::BinaryOperator::Greater
            | dae::BinaryOperator::GreaterEqual => {
                self.builder
                    .compare(compare_operator(operator), lhs_value, rhs_value, at)
            }
            _ => self
                .builder
                .binary(binary_operator(operator)?, lhs_value, rhs_value, at),
        }?;
        Ok(LoweredValue::scalar(value_type, register))
    }

    // SPEC_0021: Exception - exhaustive pure-builtin lowering dispatch.
    #[allow(clippy::excessive_nesting, clippy::too_many_lines)]
    pub(super) fn builtin(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        at: rumoca_core::Span,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        if builtin == dae::PureBuiltin::Size {
            let aggregate = arguments.get(0).ok_or(
                solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
            )?;
            let dimension = arguments.get(1).ok_or(
                solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
            )?;
            let aggregate_type = self
                .view
                .expression(aggregate)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .value_type();
            let dimension = self
                .view
                .expression(dimension)
                .and_then(|node| match node.operation() {
                    dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(value)) => {
                        usize::try_from(*value).ok()?.checked_sub(1)
                    }
                    _ => None,
                })
                .ok_or(solve::SolveProgramConstructionError::InvalidCallInterface {
                    provenance: at,
                })?;
            let extent = aggregate_type.dimensions().get(dimension).copied().ok_or(
                solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
            )?;
            let value = solve::SolveValue::integer(arithmetic_profile(), i64::from(extent))
                .map_err(
                    |_| solve::SolveProgramConstructionError::InvalidCallInterface {
                        provenance: at,
                    },
                )?;
            let register = self.builder.constant(value, at)?;
            return Ok(LoweredValue::scalar(value_type, register));
        }
        if matches!(builtin, dae::PureBuiltin::Zeros | dae::PureBuiltin::Ones) {
            let value = if builtin == dae::PureBuiltin::Zeros {
                0.0
            } else {
                1.0
            };
            let value = self
                .builder
                .constant(solve::SolveValue::real(arithmetic_profile(), value), at)?;
            let dimensions = self
                .view
                .value_type(value_type)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .dimensions()
                .to_vec();
            let register = self.builder.fill(value, dimensions, at)?;
            return Ok(LoweredValue::scalar(value_type, register));
        }
        if builtin == dae::PureBuiltin::Fill {
            let value = arguments.get(0).ok_or(
                solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
            )?;
            let value = self.expression(value)?.only_register(at)?;
            let dimensions = self
                .view
                .value_type(value_type)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .dimensions()
                .to_vec();
            let register = self.builder.fill(value, dimensions, at)?;
            return Ok(LoweredValue::scalar(value_type, register));
        }
        if matches!(
            builtin,
            dae::PureBuiltin::PromotedCat1 | dae::PureBuiltin::PromotedCat2
        ) {
            let target_scalar = self
                .view
                .value_type(value_type)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .scalar_type();
            let mut operands = Vec::with_capacity(arguments.len());
            for argument in arguments.iter() {
                let value = self.expression(argument)?;
                let source_scalar = self
                    .view
                    .value_type(value.value_type)
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                    .scalar_type();
                let mut register = value.only_register(at)?;
                if target_scalar == dae::ScalarType::Real
                    && matches!(
                        source_scalar,
                        dae::ScalarType::Integer | dae::ScalarType::Enumeration
                    )
                {
                    register = self.builder.convert(
                        solve::SolveConversionOperator::IntegerToReal,
                        register,
                        at,
                    )?;
                }
                operands.push(register);
            }
            let axis = u32::from(builtin == dae::PureBuiltin::PromotedCat2);
            let register = self.builder.concatenate(axis, &operands, at)?;
            return Ok(LoweredValue::scalar(value_type, register));
        }
        if builtin == dae::PureBuiltin::Identity {
            let dimensions = self
                .view
                .value_type(value_type)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
                .dimensions();
            let [rows, columns] = dimensions else {
                return Err(solve::SolveProgramConstructionError::InvalidTensorAlgebra {
                    provenance: at,
                });
            };
            if rows != columns {
                return Err(solve::SolveProgramConstructionError::InvalidTensorAlgebra {
                    provenance: at,
                });
            }
            let element_type =
                lower_primitive_type(self.view, value_type, arithmetic_profile())?.element_type();
            let register = self.builder.identity(element_type, *rows, at)?;
            return Ok(LoweredValue::scalar(value_type, register));
        }
        if builtin == dae::PureBuiltin::Diagonal {
            let operand = arguments.get(0).ok_or(
                solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
            )?;
            let operand = self.expression(operand)?.only_register(at)?;
            let register = self.builder.diagonal(operand, at)?;
            return Ok(LoweredValue::scalar(value_type, register));
        }
        if builtin == dae::PureBuiltin::Cross {
            let lhs = arguments.get(0).ok_or(
                solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
            )?;
            let rhs = arguments.get(1).ok_or(
                solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
            )?;
            if arguments.len() != 2 {
                return Err(solve::SolveProgramConstructionError::InvalidCallInterface {
                    provenance: at,
                });
            }
            let lhs = self.expression(lhs)?.only_register(at)?;
            let rhs = self.expression(rhs)?.only_register(at)?;
            let register = self.builder.cross(lhs, rhs, at)?;
            return Ok(LoweredValue::scalar(value_type, register));
        }
        if matches!(
            builtin,
            dae::PureBuiltin::Atan2 | dae::PureBuiltin::Min | dae::PureBuiltin::Max
        ) && arguments.len() == 2
        {
            let lhs = self
                .expression(arguments.get(0).expect("checked binary builtin lhs"))?
                .only_register(at)?;
            let rhs = self
                .expression(arguments.get(1).expect("checked binary builtin rhs"))?
                .only_register(at)?;
            let operator = match builtin {
                dae::PureBuiltin::Atan2 => solve::SolveBinaryOperator::Atan2,
                dae::PureBuiltin::Min => solve::SolveBinaryOperator::Min,
                dae::PureBuiltin::Max => solve::SolveBinaryOperator::Max,
                _ => unreachable!("guarded above"),
            };
            let register = self.builder.binary(operator, lhs, rhs, at)?;
            return Ok(LoweredValue::scalar(value_type, register));
        }
        if builtin == dae::PureBuiltin::Integer {
            let argument = arguments.get(0).ok_or(
                solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at },
            )?;
            let value = self.expression(argument)?.only_register(at)?;
            let register = self.builder.convert(
                solve::SolveConversionOperator::RealToIntegerTowardNegativeInfinity,
                value,
                at,
            )?;
            return Ok(LoweredValue::scalar(value_type, register));
        }
        let argument = arguments
            .get(0)
            .ok_or(solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at })?;
        let value = self.expression(argument)?.only_register(at)?;
        let register = match builtin {
            dae::PureBuiltin::Abs => self
                .builder
                .unary(solve::SolveUnaryOperator::Abs, value, at),
            dae::PureBuiltin::Sign => {
                self.builder
                    .unary(solve::SolveUnaryOperator::Sign, value, at)
            }
            dae::PureBuiltin::Sqrt => {
                self.builder
                    .unary(solve::SolveUnaryOperator::Sqrt, value, at)
            }
            dae::PureBuiltin::Floor => {
                self.builder
                    .unary(solve::SolveUnaryOperator::Floor, value, at)
            }
            dae::PureBuiltin::Ceil => {
                self.builder
                    .unary(solve::SolveUnaryOperator::Ceiling, value, at)
            }
            dae::PureBuiltin::Sin => self
                .builder
                .unary(solve::SolveUnaryOperator::Sin, value, at),
            dae::PureBuiltin::Cos => self
                .builder
                .unary(solve::SolveUnaryOperator::Cos, value, at),
            dae::PureBuiltin::Tan => self
                .builder
                .unary(solve::SolveUnaryOperator::Tan, value, at),
            dae::PureBuiltin::Asin => {
                self.builder
                    .unary(solve::SolveUnaryOperator::Asin, value, at)
            }
            dae::PureBuiltin::Acos => {
                self.builder
                    .unary(solve::SolveUnaryOperator::Acos, value, at)
            }
            dae::PureBuiltin::Atan => {
                self.builder
                    .unary(solve::SolveUnaryOperator::Atan, value, at)
            }
            dae::PureBuiltin::Sinh => {
                self.builder
                    .unary(solve::SolveUnaryOperator::Sinh, value, at)
            }
            dae::PureBuiltin::Cosh => {
                self.builder
                    .unary(solve::SolveUnaryOperator::Cosh, value, at)
            }
            dae::PureBuiltin::Tanh => {
                self.builder
                    .unary(solve::SolveUnaryOperator::Tanh, value, at)
            }
            dae::PureBuiltin::Exp => self
                .builder
                .unary(solve::SolveUnaryOperator::Exp, value, at),
            dae::PureBuiltin::Log => self
                .builder
                .unary(solve::SolveUnaryOperator::Log, value, at),
            dae::PureBuiltin::Log10 => {
                self.builder
                    .unary(solve::SolveUnaryOperator::Log10, value, at)
            }
            dae::PureBuiltin::Transpose => self.builder.transpose(value, at),
            dae::PureBuiltin::Sum => {
                self.builder
                    .reduce(solve::SolveReductionOperator::Sum, value, at)
            }
            dae::PureBuiltin::Product => {
                self.builder
                    .reduce(solve::SolveReductionOperator::Product, value, at)
            }
            dae::PureBuiltin::Min if arguments.len() == 1 => {
                self.builder
                    .reduce(solve::SolveReductionOperator::Minimum, value, at)
            }
            dae::PureBuiltin::Max if arguments.len() == 1 => {
                self.builder
                    .reduce(solve::SolveReductionOperator::Maximum, value, at)
            }
            _ => Err(solve::SolveProgramConstructionError::InvalidCallInterface { provenance: at }),
        }?;
        Ok(LoweredValue::scalar(value_type, register))
    }
}

fn compare_operator(operator: dae::BinaryOperator) -> solve::SolveCompareOperator {
    match operator {
        dae::BinaryOperator::Equal => solve::SolveCompareOperator::Equal,
        dae::BinaryOperator::NotEqual => solve::SolveCompareOperator::NotEqual,
        dae::BinaryOperator::Less => solve::SolveCompareOperator::Less,
        dae::BinaryOperator::LessEqual => solve::SolveCompareOperator::LessEqual,
        dae::BinaryOperator::Greater => solve::SolveCompareOperator::Greater,
        dae::BinaryOperator::GreaterEqual => solve::SolveCompareOperator::GreaterEqual,
        _ => unreachable!("caller selects only comparison operators"),
    }
}

fn binary_operator(
    operator: dae::BinaryOperator,
) -> Result<solve::SolveBinaryOperator, solve::SolveProgramConstructionError> {
    let operator = match operator {
        dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => {
            solve::SolveBinaryOperator::Add
        }
        dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => {
            solve::SolveBinaryOperator::Subtract
        }
        dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
            solve::SolveBinaryOperator::Multiply
        }
        dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide => {
            solve::SolveBinaryOperator::Divide
        }
        dae::BinaryOperator::Power | dae::BinaryOperator::ElementwisePower => {
            solve::SolveBinaryOperator::Power
        }
        dae::BinaryOperator::And => solve::SolveBinaryOperator::And,
        dae::BinaryOperator::Or => solve::SolveBinaryOperator::Or,
        _ => return Err(solve::SolveProgramConstructionError::WireMismatch),
    };
    Ok(operator)
}
