//! Compile-time constant reads over checked DAE expressions.
//!
//! Solve lowering needs two different constant questions, and conflating them
//! would be unsound. Probing a coefficient's *declared* value may reject a
//! model but must never choose what a program computes; only a value a
//! simulation cannot override may select a branch. [`ConstantReach`] keeps the
//! two apart, and every fold mirrors the operator the surrounding lowering
//! emits so a probe and the run it describes cannot disagree.

use super::*;

impl<'dae> ScalarSelector<'dae> {
    pub(in crate::lower) fn constant_real(
        &self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<f64, LowerError> {
        self.constant_real_inner(expression, scalar, ConstantReach::Declared, &mut Vec::new())
    }

    fn constant_real_inner(
        &self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        reach: ConstantReach,
        active: &mut Vec<dae::ExprId<'dae>>,
    ) -> Result<f64, LowerError> {
        let node = self.node(expression);
        let span = node.provenance().span();
        check_constant_recursion(active, expression, span)?;
        active.push(expression);
        let result = match node.operation() {
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value)) => Ok(*value),
            dae::ExpressionOperation::Literal(
                dae::DaeLiteral::Integer(value) | dae::DaeLiteral::Enumeration(value),
            ) => Ok(*value as f64),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Boolean(value)) => {
                Ok(f64::from(u8::from(*value)))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) => {
                let binding = self.constant_parameter_binding(parameter, reach, span)?;
                self.constant_real_inner(binding, scalar, reach, active)
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let value = self.constant_real_inner(operand, scalar, reach, active)?;
                match operator {
                    dae::UnaryOperator::Plus => Ok(value),
                    dae::UnaryOperator::Negate => Ok(-value),
                    dae::UnaryOperator::Not => Ok(f64::from(u8::from(value == 0.0))),
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                let lhs_scalar = scalar_operand(self.view, lhs, scalar);
                let rhs_scalar = scalar_operand(self.view, rhs, scalar);
                let lhs = self.constant_real_inner(lhs, lhs_scalar, reach, active)?;
                let rhs = self.constant_real_inner(rhs, rhs_scalar, reach, active)?;
                constant_binary(operator, lhs, rhs, span)
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } => {
                self.constant_builtin(builtin, arguments, scalar, reach, span, active)
            }
            dae::ExpressionOperation::Conditional(operands) => {
                let selected =
                    self.constant_conditional_branch(operands, scalar, reach, span, active)?;
                self.constant_real_inner(selected, scalar, reach, active)
            }
            _ => Err(LowerError::non_computable(
                "affine derivative coefficient is not compile-time numeric",
                span,
            )),
        };
        active.pop();
        result.and_then(|value| finite_constant(value, span))
    }

    /// The declaration binding a compile-time constant read may follow.
    ///
    /// A translation-time read refuses a tunable parameter outright: its
    /// declared value is only a default, so folding it would bake a value a
    /// simulation is allowed to replace into the program's semantics.
    fn constant_parameter_binding(
        &self,
        parameter: dae::ParameterId<'dae>,
        reach: ConstantReach,
        span: Span,
    ) -> Result<dae::ExprId<'dae>, LowerError> {
        let variable = self
            .view
            .variable(parameter.into())
            .expect("checked parameter coordinate resolves");
        if reach == ConstantReach::Translation && variable.is_tunable() {
            return Err(LowerError::non_computable(
                "a tunable parameter cannot select a translation-time branch",
                span,
            ));
        }
        variable.binding().ok_or_else(|| {
            LowerError::non_computable(
                "affine derivative coefficient parameter has no static binding",
                span,
            )
        })
    }

    /// Fold the pure builtins whose scalar value is a function of their scalar
    /// arguments alone. Every arm mirrors the operator this same file emits for
    /// the runtime program, so the probe and the run agree by construction.
    fn constant_builtin(
        &self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        reach: ConstantReach,
        span: Span,
        active: &mut Vec<dae::ExprId<'dae>>,
    ) -> Result<f64, LowerError> {
        let argument = |index: usize| {
            arguments
                .get(index)
                .ok_or_else(|| LowerError::contract("checked builtin argument is missing", span))
        };
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
                let value = self.constant_real_inner(argument(0)?, scalar, reach, active)?;
                Ok(constant_unary_builtin(unary_builtin(builtin), value))
            }
            // `smooth` and `noEvent` are value-transparent: only event handling
            // differs. `homotopy` is not — its run-time value interpolates
            // toward the actual expression — so it stays unfolded.
            dae::PureBuiltin::Smooth => {
                self.constant_real_inner(argument(1)?, scalar, reach, active)
            }
            dae::PureBuiltin::NoEvent => {
                self.constant_real_inner(argument(0)?, scalar, reach, active)
            }
            dae::PureBuiltin::Vector => {
                self.constant_real_inner(argument(0)?, scalar, reach, active)
            }
            dae::PureBuiltin::Transpose => {
                let operand = argument(0)?;
                self.constant_real_inner(
                    operand,
                    self.transpose_scalar(operand, scalar),
                    reach,
                    active,
                )
            }
            dae::PureBuiltin::Diagonal
            | dae::PureBuiltin::OuterProduct
            | dae::PureBuiltin::Skew => {
                self.constant_matrix_builtin(builtin, arguments, scalar, reach, span, active)
            }
            dae::PureBuiltin::Atan2 => {
                let lhs = self.constant_real_inner(argument(0)?, scalar, reach, active)?;
                let rhs = self.constant_real_inner(argument(1)?, scalar, reach, active)?;
                Ok(lhs.atan2(rhs))
            }
            dae::PureBuiltin::Min | dae::PureBuiltin::Max if arguments.len() == 2 => {
                let lhs = self.constant_real_inner(argument(0)?, scalar, reach, active)?;
                let rhs = self.constant_real_inner(argument(1)?, scalar, reach, active)?;
                Ok(if builtin == dae::PureBuiltin::Min {
                    lhs.min(rhs)
                } else {
                    lhs.max(rhs)
                })
            }
            dae::PureBuiltin::Identity => {
                let extent = usize::try_from(self.integer(argument(0)?, 0)?).map_err(|_| {
                    LowerError::contract("checked identity extent is non-negative", span)
                })?;
                if extent == 0 {
                    return Err(LowerError::contract(
                        "a zero-size identity has no scalar projection",
                        span,
                    ));
                }
                Ok(f64::from(u8::from(scalar / extent == scalar % extent)))
            }
            _ => Err(LowerError::non_computable(
                "affine derivative coefficient is not compile-time numeric",
                span,
            )),
        }
    }

    fn constant_matrix_builtin(
        &self,
        builtin: dae::PureBuiltin,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        reach: ConstantReach,
        span: Span,
        active: &mut Vec<dae::ExprId<'dae>>,
    ) -> Result<f64, LowerError> {
        let argument = |index| {
            arguments
                .get(index)
                .ok_or_else(|| LowerError::contract("checked builtin argument is missing", span))
        };
        match builtin {
            dae::PureBuiltin::Diagonal => {
                let operand = argument(0)?;
                let extent = scalar_count(self.view, operand);
                let row = scalar / extent;
                if row == scalar % extent {
                    self.constant_real_inner(operand, row, reach, active)
                } else {
                    Ok(0.0)
                }
            }
            dae::PureBuiltin::OuterProduct => {
                let lhs = argument(0)?;
                let rhs = argument(1)?;
                let columns = scalar_count(self.view, rhs);
                let lhs = self.constant_real_inner(lhs, scalar / columns, reach, active)?;
                let rhs = self.constant_real_inner(rhs, scalar % columns, reach, active)?;
                Ok(lhs * rhs)
            }
            dae::PureBuiltin::Skew => {
                let (operand_scalar, negate) = match scalar {
                    0 | 4 | 8 => return Ok(0.0),
                    1 => (2, true),
                    2 => (1, false),
                    3 => (2, false),
                    5 => (0, true),
                    6 => (1, true),
                    7 => (0, false),
                    _ => unreachable!("checked skew scalar belongs to its 3x3 result"),
                };
                let value =
                    self.constant_real_inner(argument(0)?, operand_scalar, reach, active)?;
                Ok(if negate { -value } else { value })
            }
            _ => unreachable!("only compact matrix builtins use this constant evaluator"),
        }
    }

    /// Select the branch of a checked conditional whose condition this reach
    /// proves constant. Operands are `[condition, value]` pairs followed by the
    /// single `else` fallback, so a checked conditional holds an odd count of
    /// at least three.
    fn constant_conditional_branch(
        &self,
        operands: dae::ExpressionOperands<'dae>,
        scalar: usize,
        reach: ConstantReach,
        span: Span,
        active: &mut Vec<dae::ExprId<'dae>>,
    ) -> Result<dae::ExprId<'dae>, LowerError> {
        if operands.len() < 3 || operands.len().is_multiple_of(2) {
            return Err(LowerError::contract(
                "conditional operands are not condition/value pairs plus a fallback",
                span,
            ));
        }
        let fallback = operands.len() - 1;
        for index in (0..fallback).step_by(2) {
            let condition = operands
                .get(index)
                .expect("checked conditional branch has a condition");
            let condition_scalar = scalar_operand(self.view, condition, scalar);
            if self.constant_real_inner(condition, condition_scalar, reach, active)? != 0.0 {
                return Ok(operands
                    .get(index + 1)
                    .expect("checked conditional branch has a value"));
            }
        }
        Ok(operands
            .get(fallback)
            .expect("checked conditional has a fallback"))
    }

    /// Resolve the conditional selections the model fixes at translation time.
    ///
    /// Accepted conditions are literals, translation-time parameters (`final`
    /// or `annotation(Evaluate=true)`, which no simulation override can
    /// change), and the arithmetic, comparison, and logical operators over
    /// them. A run-time condition is left standing, so an expression this
    /// cannot resolve reaches its original checked rejection unchanged.
    pub(in crate::lower) fn structural_branch(
        &self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Result<dae::ExprId<'dae>, LowerError> {
        let mut selected = expression;
        for _ in 0..STRUCTURAL_BRANCH_DEPTH_LIMIT {
            let node = self.node(selected);
            let dae::ExpressionOperation::Conditional(operands) = node.operation() else {
                return Ok(selected);
            };
            match self.constant_conditional_branch(
                operands,
                scalar,
                ConstantReach::Translation,
                node.provenance().span(),
                &mut Vec::new(),
            ) {
                Ok(branch) => selected = branch,
                // A condition this reach cannot evaluate is an ordinary
                // run-time condition: the conditional stays, and the caller's
                // own rejection reports it. A broken contract still fails.
                Err(LowerError::NonComputable { .. }) => return Ok(selected),
                Err(error) => return Err(error),
            }
        }
        Err(LowerError::contract(
            "structural conditional nesting exceeded its checked limit",
            self.node(expression).provenance().span(),
        ))
    }
}

/// How far a compile-time constant read may look through parameters.
#[derive(Clone, Copy, PartialEq, Eq)]
enum ConstantReach {
    /// Read every parameter's declared binding, tunable or not. A result only
    /// describes the model as declared, so it may reject a program but must
    /// never select its semantics.
    Declared,
    /// Read only the parameters a simulation cannot override, so the result is
    /// the value every run observes and may select program semantics.
    Translation,
}

/// Nesting bound for translation-time branch selection. A conditional chain
/// deeper than this is a construction defect rather than a model.
const STRUCTURAL_BRANCH_DEPTH_LIMIT: usize = 64;

/// Apply the scalar unary operator this file emits for the same builtin, so a
/// folded coefficient and its runtime program never disagree.
fn constant_unary_builtin(op: solve::UnaryOp, value: f64) -> f64 {
    match op {
        solve::UnaryOp::Neg => -value,
        solve::UnaryOp::Not => f64::from(u8::from(value == 0.0)),
        solve::UnaryOp::Abs => value.abs(),
        solve::UnaryOp::Sign => rumoca_core::modelica_sign(value),
        solve::UnaryOp::Sqrt => value.sqrt(),
        solve::UnaryOp::Floor => value.floor(),
        solve::UnaryOp::Ceil => value.ceil(),
        solve::UnaryOp::Trunc => value.trunc(),
        solve::UnaryOp::Sin => value.sin(),
        solve::UnaryOp::Cos => value.cos(),
        solve::UnaryOp::Tan => value.tan(),
        solve::UnaryOp::Asin => value.asin(),
        solve::UnaryOp::Acos => value.acos(),
        solve::UnaryOp::Atan => value.atan(),
        solve::UnaryOp::Sinh => value.sinh(),
        solve::UnaryOp::Cosh => value.cosh(),
        solve::UnaryOp::Tanh => value.tanh(),
        solve::UnaryOp::Exp => value.exp(),
        solve::UnaryOp::Log => value.ln(),
        solve::UnaryOp::Log10 => value.log10(),
    }
}

fn check_constant_recursion<'dae>(
    active: &[dae::ExprId<'dae>],
    expression: dae::ExprId<'dae>,
    span: Span,
) -> Result<(), LowerError> {
    if active.contains(&expression) {
        return Err(LowerError::non_computable(
            "affine derivative coefficient has a cyclic parameter definition",
            span,
        ));
    }
    if active.len() >= 256 {
        return Err(LowerError::non_computable(
            "affine derivative coefficient exceeded the checked recursion limit",
            span,
        ));
    }
    Ok(())
}

fn constant_binary(
    operator: dae::BinaryOperator,
    lhs: f64,
    rhs: f64,
    span: Span,
) -> Result<f64, LowerError> {
    let value = match operator {
        dae::BinaryOperator::Add | dae::BinaryOperator::ElementwiseAdd => lhs + rhs,
        dae::BinaryOperator::Subtract | dae::BinaryOperator::ElementwiseSubtract => lhs - rhs,
        dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => lhs * rhs,
        dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide if rhs != 0.0 => {
            lhs / rhs
        }
        dae::BinaryOperator::Power | dae::BinaryOperator::ElementwisePower => lhs.powf(rhs),
        // Relations and connectives carry Boolean truth as 0.0/1.0, exactly as
        // the emitted `Compare` and `Binary` ops do at run time.
        dae::BinaryOperator::Equal => f64::from(u8::from(lhs == rhs)),
        dae::BinaryOperator::NotEqual => f64::from(u8::from(lhs != rhs)),
        dae::BinaryOperator::Less => f64::from(u8::from(lhs < rhs)),
        dae::BinaryOperator::LessEqual => f64::from(u8::from(lhs <= rhs)),
        dae::BinaryOperator::Greater => f64::from(u8::from(lhs > rhs)),
        dae::BinaryOperator::GreaterEqual => f64::from(u8::from(lhs >= rhs)),
        dae::BinaryOperator::And => f64::from(u8::from(lhs != 0.0 && rhs != 0.0)),
        dae::BinaryOperator::Or => f64::from(u8::from(lhs != 0.0 || rhs != 0.0)),
        dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide => {
            return Err(LowerError::non_computable(
                "affine derivative coefficient divides by zero",
                span,
            ));
        }
    };
    finite_constant(value, span)
}

fn finite_constant(value: f64, span: Span) -> Result<f64, LowerError> {
    if value.is_finite() {
        Ok(value)
    } else {
        Err(LowerError::non_computable(
            "affine derivative coefficient is not finite",
            span,
        ))
    }
}
