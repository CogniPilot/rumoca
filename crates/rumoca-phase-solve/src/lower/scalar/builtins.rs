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
        dims: &[u32],
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
            dae::PureBuiltin::Vector => {
                self.expression(arguments.get(0).expect("checked vector operand"), scalar)
            }
            dae::PureBuiltin::Transpose => self.transpose(arguments, scalar),
            dae::PureBuiltin::Diagonal | dae::PureBuiltin::OuterProduct => {
                self.matrix(builtin, arguments, dims, scalar, span)
            }
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
            dae::PureBuiltin::Skew => self.skew(arguments, scalar, span),
            dae::PureBuiltin::Identity => self.identity(dims, scalar, span),
            dae::PureBuiltin::PromotedCat1 | dae::PureBuiltin::PromotedCat2 => {
                let axis = usize::from(builtin == dae::PureBuiltin::PromotedCat2);
                self.promoted_concatenation(arguments, axis, scalar)
            }
        }
    }

    fn transpose(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let operand = arguments.get(0).expect("checked transpose operand");
        let selector = ScalarSelector::from_points(self.view, &self.domain_points);
        self.expression(operand, selector.transpose_scalar(operand, scalar))
    }

    fn identity(
        &mut self,
        dimensions: &[u32],
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let [_, columns] = dimensions else {
            unreachable!("checked identity result has rank two")
        };
        let diagonal = scalar / *columns as usize == scalar % *columns as usize;
        self.constant(f64::from(u8::from(diagonal)), span)
    }

    fn diagonal(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        result_dimensions: &[u32],
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let [_, columns] = result_dimensions else {
            unreachable!("checked diagonal result has rank two")
        };
        let row = scalar / *columns as usize;
        let column = scalar % *columns as usize;
        if row != column {
            return self.constant(0.0, span);
        }
        self.expression(
            arguments.get(0).expect("checked diagonal has one operand"),
            row,
        )
    }

    fn outer_product(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        result_dimensions: &[u32],
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let [_, columns] = result_dimensions else {
            unreachable!("checked outerProduct result has rank two")
        };
        let lhs = self.expression(
            arguments
                .get(0)
                .expect("checked outerProduct has a left operand"),
            scalar / *columns as usize,
        )?;
        let rhs = self.expression(
            arguments
                .get(1)
                .expect("checked outerProduct has a right operand"),
            scalar % *columns as usize,
        )?;
        self.binary(dae::BinaryOperator::Multiply, lhs, rhs, span)
    }

    fn matrix(
        &mut self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        result_dimensions: &[u32],
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        match builtin {
            dae::PureBuiltin::Diagonal => self.diagonal(arguments, result_dimensions, scalar, span),
            dae::PureBuiltin::OuterProduct => {
                self.outer_product(arguments, result_dimensions, scalar, span)
            }
            _ => unreachable!("only compact matrix products use this lowering"),
        }
    }

    fn promoted_concatenation(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        axis: usize,
        scalar: usize,
    ) -> Result<solve::Reg, LowerError> {
        let rank = arguments
            .iter()
            .map(|argument| self.node(argument).value_type().dimensions().len())
            .max()
            .unwrap_or(0)
            .max(2);
        let mut result_dimensions = vec![1_u32; rank];
        let first = arguments
            .get(0)
            .expect("checked concatenation owns an operand");
        let first_dimensions = self.node(first).value_type().dimensions();
        result_dimensions[..first_dimensions.len()].copy_from_slice(first_dimensions);
        for argument in arguments.iter().skip(1) {
            let dimensions = self.node(argument).value_type().dimensions();
            result_dimensions[axis] = result_dimensions[axis]
                .checked_add(dimensions.get(axis).copied().unwrap_or(1))
                .expect("checked concatenation extent remains in the u32 domain");
        }
        let selector = ScalarSelector::from_points(self.view, &self.domain_points);
        let (argument, argument_scalar) =
            selector.promoted_concatenation_scalar(arguments, axis, &result_dimensions, scalar)?;
        self.expression(argument, argument_scalar)
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

    /// Lower `homotopy(actual, simplified)` as the blend MLS 3.6 §3.7.4.3
    /// writes: `lambda*actual + (1 - lambda)*simplified`.
    ///
    /// The spelling matters. The algebraically equal
    /// `simplified + lambda*(actual - simplified)` loses `actual` to
    /// cancellation at λ = 1 whenever `|simplified| >> |actual|`: in IEEE
    /// double, `1e17 + 1.0*(1.0 - 1e17)` is `0.0`, not `1.0`. λ is a plain `P`
    /// slot pinned to `1.0` outside the initialization continuation, so that
    /// error would persist for the whole run, not just initialization. The form
    /// below is exact at both endpoints — `1*actual + 0*simplified` is `actual`
    /// and `0*actual + 1*simplified` is `simplified` — for the same op count.
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
        let one = self.constant(1.0, span)?;
        let complement = self.binary(dae::BinaryOperator::Subtract, one, lambda, span)?;
        let actual_term = self.binary(dae::BinaryOperator::Multiply, lambda, actual, span)?;
        let simplified_term =
            self.binary(dae::BinaryOperator::Multiply, complement, simplified, span)?;
        self.binary(dae::BinaryOperator::Add, actual_term, simplified_term, span)
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

    fn skew(
        &mut self,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let (operand_scalar, negate) = match scalar {
            0 | 4 | 8 => return self.constant(0.0, span),
            1 => (2, true),
            2 => (1, false),
            3 => (2, false),
            5 => (0, true),
            6 => (1, true),
            7 => (0, false),
            _ => unreachable!("checked skew scalar belongs to its 3x3 result"),
        };
        let value = self.expression(
            arguments.get(0).expect("checked skew has one operand"),
            operand_scalar,
        )?;
        if negate {
            self.unary(dae::UnaryOperator::Negate, value, span)
        } else {
            Ok(value)
        }
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
