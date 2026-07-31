//! Pure builtin lowering.
//!
//! Every builtin expands into ops Solve already owns, so a program stays a
//! flat register sequence with no runtime builtin dispatch. Reductions fold
//! their operand's scalars in declaration order under the operator this same
//! module emits for the equivalent explicit expression.

use super::*;

#[derive(Clone, Copy)]
enum ReductionKind {
    Sum,
    Product,
    Minimum,
    Maximum,
}

impl ReductionKind {
    fn operator(self) -> solve::BinaryOp {
        match self {
            Self::Sum => solve::BinaryOp::Add,
            Self::Product => solve::BinaryOp::Mul,
            Self::Minimum => solve::BinaryOp::Min,
            Self::Maximum => solve::BinaryOp::Max,
        }
    }

    fn identity(self) -> Option<f64> {
        match self {
            Self::Sum => Some(0.0),
            Self::Product => Some(1.0),
            Self::Minimum | Self::Maximum => None,
        }
    }
}

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    pub(super) fn builtin(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        match builtin {
            dae::PureBuiltin::Abs
            | dae::PureBuiltin::Sign
            | dae::PureBuiltin::Sqrt
            | dae::PureBuiltin::Floor
            | dae::PureBuiltin::Ceil
            | dae::PureBuiltin::Integer
            | dae::PureBuiltin::Sin
            | dae::PureBuiltin::Cos
            | dae::PureBuiltin::Tan
            | dae::PureBuiltin::Asin
            | dae::PureBuiltin::Acos
            | dae::PureBuiltin::Atan
            | dae::PureBuiltin::Sinh
            | dae::PureBuiltin::Cosh
            | dae::PureBuiltin::Tanh
            | dae::PureBuiltin::Exp
            | dae::PureBuiltin::Log
            | dae::PureBuiltin::Log10 => {
                let argument = self.expression(
                    arguments.get(0).expect("checked unary builtin argument"),
                    scalar,
                )?;
                let dst = self.register(span)?;
                self.ops.push(solve::LinearOp::Unary {
                    dst,
                    op: unary_builtin(builtin),
                    arg: argument,
                });
                Ok(dst)
            }
            dae::PureBuiltin::Atan2 => self.atan2(arguments, scalar, span),
            dae::PureBuiltin::Div | dae::PureBuiltin::Mod | dae::PureBuiltin::Rem => {
                self.quotient(builtin, arguments, scalar, span)
            }
            dae::PureBuiltin::Smooth => self.expression(
                arguments.get(1).expect("checked smooth value argument"),
                scalar,
            ),
            dae::PureBuiltin::NoEvent => self.expression(
                arguments.get(0).expect("checked noEvent value argument"),
                scalar,
            ),
            dae::PureBuiltin::Homotopy => self.homotopy(arguments, scalar, span),
            dae::PureBuiltin::Sum => self.reduction(
                arguments.get(0).expect("checked reduction argument"),
                ReductionKind::Sum,
                span,
            ),
            dae::PureBuiltin::Product => self.reduction(
                arguments.get(0).expect("checked reduction argument"),
                ReductionKind::Product,
                span,
            ),
            dae::PureBuiltin::Min | dae::PureBuiltin::Max => {
                let reduction = if builtin == dae::PureBuiltin::Min {
                    ReductionKind::Minimum
                } else {
                    ReductionKind::Maximum
                };
                if arguments.len() == 1 {
                    self.reduction(
                        arguments.get(0).expect("checked reduction argument"),
                        reduction,
                        span,
                    )
                } else {
                    let values = arguments
                        .iter()
                        .map(|argument| self.expression(argument, scalar))
                        .collect::<Result<Vec<_>, _>>()?;
                    self.fold_registers(values, reduction.operator(), span)
                }
            }
            dae::PureBuiltin::Size => self.size_builtin(arguments, scalar, span),
            dae::PureBuiltin::Zeros => self.constant(0.0, span),
            dae::PureBuiltin::Ones => self.constant(1.0, span),
            dae::PureBuiltin::Fill => {
                self.expression(arguments.get(0).expect("checked fill value argument"), 0)
            }
            dae::PureBuiltin::Linspace => self.linspace(arguments, scalar, span),
            dae::PureBuiltin::Cross => self.cross(arguments, scalar, span),
        }
    }

    fn atan2(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let lhs = self.expression(
            arguments.get(0).expect("checked atan2 first argument"),
            scalar,
        )?;
        let rhs = self.expression(
            arguments.get(1).expect("checked atan2 second argument"),
            scalar,
        )?;
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::Binary {
            dst,
            op: solve::BinaryOp::Atan2,
            lhs,
            rhs,
        });
        Ok(dst)
    }

    fn quotient(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let lhs = self.expression(
            arguments.get(0).expect("checked quotient first argument"),
            scalar,
        )?;
        let rhs = self.expression(
            arguments.get(1).expect("checked quotient second argument"),
            scalar,
        )?;
        let ratio = self.binary(dae::BinaryOperator::Divide, lhs, rhs, span)?;
        let quotient = self.solve_unary(
            if builtin == dae::PureBuiltin::Mod {
                solve::UnaryOp::Floor
            } else {
                solve::UnaryOp::Trunc
            },
            ratio,
            span,
        )?;
        if builtin == dae::PureBuiltin::Div {
            return Ok(quotient);
        }
        let multiple = self.binary(dae::BinaryOperator::Multiply, quotient, rhs, span)?;
        self.binary(dae::BinaryOperator::Subtract, lhs, multiple, span)
    }

    fn homotopy(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let actual = self.expression(
            arguments.get(0).expect("checked homotopy actual argument"),
            scalar,
        )?;
        let simplified = self.expression(
            arguments
                .get(1)
                .expect("checked homotopy simplified argument"),
            scalar,
        )?;
        let index = self
            .layout
            .solve_layout
            .initial_homotopy_parameter_index
            .ok_or_else(|| {
                LowerError::non_computable("homotopy expression has no checked Solve storage", span)
            })?;
        let lambda = self.load_slot(solve::scalar_slot_p(index), span)?;
        let delta = self.binary(dae::BinaryOperator::Subtract, actual, simplified, span)?;
        let scaled = self.binary(dae::BinaryOperator::Multiply, lambda, delta, span)?;
        self.binary(dae::BinaryOperator::Add, simplified, scaled, span)
    }

    fn linspace(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let start = self.expression(arguments.get(0).expect("checked linspace start"), 0)?;
        let stop = self.expression(arguments.get(1).expect("checked linspace stop"), 0)?;
        let count = u32::try_from(
            ScalarSelector::from_points(self.view, &self.domain_points)
                .integer(arguments.get(2).expect("checked linspace extent"), 0)?,
        )
        .expect("checked linspace extent is in the u32 domain");
        let ordinal = u32::try_from(scalar).expect("linspace scalar is below its u32 extent");
        let fraction = self.constant(f64::from(ordinal) / f64::from(count - 1), span)?;
        let difference = self.binary(dae::BinaryOperator::Subtract, stop, start, span)?;
        let scaled = self.binary(dae::BinaryOperator::Multiply, difference, fraction, span)?;
        self.binary(dae::BinaryOperator::Add, start, scaled, span)
    }

    fn cross(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let (first, second) = [(1, 2), (2, 0), (0, 1)][scalar];
        let lhs = self.expression(arguments.get(0).expect("checked cross lhs"), first)?;
        let rhs = self.expression(arguments.get(1).expect("checked cross rhs"), second)?;
        let positive = self.binary(dae::BinaryOperator::Multiply, lhs, rhs, span)?;
        let lhs = self.expression(arguments.get(0).expect("checked cross lhs"), second)?;
        let rhs = self.expression(arguments.get(1).expect("checked cross rhs"), first)?;
        let negative = self.binary(dae::BinaryOperator::Multiply, lhs, rhs, span)?;
        self.binary(dae::BinaryOperator::Subtract, positive, negative, span)
    }

    fn reduction(
        &mut self,
        expression: dae::ExprId<'dae>,
        reduction: ReductionKind,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let count = scalar_count(self.view, expression);
        if count == 0 {
            return reduction.identity().map_or_else(
                || {
                    Err(LowerError::non_computable(
                        "minimum and maximum require a nonempty array",
                        span,
                    ))
                },
                |identity| self.constant(identity, span),
            );
        }
        let mut values = Vec::with_capacity(count);
        for scalar in 0..count {
            values.push(self.expression(expression, scalar)?);
        }
        self.fold_registers(values, reduction.operator(), span)
    }

    fn fold_registers(
        &mut self,
        values: Vec<solve::Reg>,
        operator: solve::BinaryOp,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let mut values = values.into_iter();
        let mut result = values
            .next()
            .expect("checked reduction supplies an identity or a nonempty operand");
        for value in values {
            let dst = self.register(span)?;
            self.ops.push(solve::LinearOp::Binary {
                dst,
                op: operator,
                lhs: result,
                rhs: value,
            });
            result = dst;
        }
        Ok(result)
    }

    fn size_builtin(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let value = arguments.get(0).expect("checked size value");
        let dimensions = self.node(value).value_type().dimensions();
        let dimension = if let Some(dimension) = arguments.get(1) {
            ScalarSelector::from_points(self.view, &self.domain_points).integer(dimension, 0)?
        } else {
            i64::try_from(scalar + 1)
                .map_err(|_| LowerError::contract("size dimension overflow", span))?
        };
        let zero_based = usize::try_from(dimension - 1)
            .map_err(|_| LowerError::non_computable("size dimension must be positive", span))?;
        let extent = dimensions.get(zero_based).copied().ok_or_else(|| {
            LowerError::non_computable(
                format!(
                    "size dimension {dimension} exceeds rank {}",
                    dimensions.len()
                ),
                span,
            )
        })?;
        self.constant(f64::from(extent), span)
    }
}
