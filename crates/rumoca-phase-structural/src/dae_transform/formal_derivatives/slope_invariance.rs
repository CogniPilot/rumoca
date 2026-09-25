//! Construction-time proof that a selection stage's constraint slope is
//! constant (SPEC_0040 STRUCT-T07 constraint-fold chart rows).
//!
//! A stage whose every equation is affine in its coordinates with
//! time-invariant coefficients has a constant Jacobian, so a chart that is
//! nonsingular at construction is nonsingular everywhere and never folds. The
//! proof reuses the STRUCT-T03 time-invariance closure for the coefficients.

use rumoca_ir_dae as dae;

use super::{FormalDerivativeStage, FormalDerivativeView};
use crate::time_invariant::TimeInvariance;

impl<'map, 'source, 'target> FormalDerivativeView<'map, 'source, 'target> {
    /// Whether every equation of `stage` is affine in its coordinates with
    /// time-invariant coefficients, so its Jacobian is constant.
    ///
    /// Conservative: a coordinate read is a linear leaf, a time-invariant
    /// subexpression a constant, and only sums, differences, negations,
    /// products and quotients by time-invariant factors, projections, and
    /// arrays of such forms are accepted. Anything else, including any
    /// nonlinear function of a coordinate, refuses.
    #[must_use]
    pub fn stage_slope_is_invariant(
        &self,
        stage: FormalDerivativeStage<'map, 'source, 'target>,
    ) -> bool {
        let invariance = TimeInvariance::derive(self.view);
        stage.equations().all(|equation| {
            let affine = |expression| affine(self.view, expression, &invariance);
            match equation.value() {
                dae::ContinuousOwnerView::Residual { equation, .. } => affine(equation.residual()),
                dae::ContinuousOwnerView::Structured { family, .. } => {
                    family.bodies().iter().all(affine)
                }
            }
        })
    }
}

fn affine<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    invariance: &TimeInvariance,
) -> bool {
    if invariance.expression(view, expression) {
        return true;
    }
    let Some(node) = view.expression(expression) else {
        return false;
    };
    let affine = |operand| affine(view, operand, invariance);
    let invariant = |operand| invariance.expression(view, operand);
    match node.operation() {
        dae::ExpressionOperation::Coordinate(_) => true,
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        }
        | dae::ExpressionOperation::Field { base: operand, .. } => affine(operand),
        dae::ExpressionOperation::Binary { operator, lhs, rhs } => match operator {
            dae::BinaryOperator::Add
            | dae::BinaryOperator::Subtract
            | dae::BinaryOperator::ElementwiseAdd
            | dae::BinaryOperator::ElementwiseSubtract => affine(lhs) && affine(rhs),
            dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply => {
                (invariant(lhs) && affine(rhs)) || (invariant(rhs) && affine(lhs))
            }
            dae::BinaryOperator::Divide | dae::BinaryOperator::ElementwiseDivide => {
                affine(lhs) && invariant(rhs)
            }
            _ => false,
        },
        dae::ExpressionOperation::Array(operands) => operands.iter().all(affine),
        dae::ExpressionOperation::Index { base, subscripts } => {
            affine(base)
                && subscripts.iter().all(|subscript| match subscript {
                    dae::SubscriptView::Whole { .. } => true,
                    dae::SubscriptView::Index { expression, .. }
                    | dae::SubscriptView::Slice { expression, .. } => invariant(expression),
                })
        }
        _ => false,
    }
}
