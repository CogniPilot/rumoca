//! Shared zero-set normalization over checked residual roots (STRUCT-T08).
//!
//! The source expression is retained for numerical evaluation and provenance.
//! These borrowed operands are an equality proof, never a value substitution.

use rumoca_ir_dae as dae;

/// A view of the source equality, including `0 = a-b` and signed zero wrappers.
/// Only exact literal zeros are removed; the original residual remains its owner.
pub(crate) fn equation_sides<'dae>(
    view: dae::DaeView<'dae>,
    mut expression: dae::ExprId<'dae>,
) -> Option<(dae::ExprId<'dae>, dae::ExprId<'dae>)> {
    let mut zero = None;
    loop {
        match view.expression(expression)?.operation() {
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => expression = operand,
            dae::ExpressionOperation::Binary {
                operator: operator @ (dae::BinaryOperator::Add | dae::BinaryOperator::Subtract),
                lhs,
                rhs,
            } => {
                if is_literal_zero(view, lhs) {
                    zero = Some(lhs);
                    expression = rhs;
                } else if is_literal_zero(view, rhs) {
                    zero = Some(rhs);
                    expression = lhs;
                } else if operator == dae::BinaryOperator::Subtract {
                    return Some((lhs, rhs));
                } else {
                    return None;
                }
            }
            _ => return zero.map(|zero| (expression, zero)),
        }
    }
}

fn is_literal_zero<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> bool {
    matches!(
        view.expression(expression).map(|node| node.operation()),
        Some(dae::ExpressionOperation::Literal(
            dae::DaeLiteral::Integer(0) | dae::DaeLiteral::Real(0.0)
        ))
    )
}
