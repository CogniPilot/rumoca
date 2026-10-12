//! Conditional guards stay fixed during continuous-time differentiation.
//!
//! MLS §3.6.5 selects one branch of `if c1 then e1 elseif ... else eN` at each
//! instant. A guard that is piecewise constant in time selects the same branch
//! on every interval between the instants where it changes, so on each such
//! interval `d/dt (if c then a else b) = if c then da/dt else db/dt`; the guard
//! is retained, never differentiated. Parameters and literals are constant; a
//! relation is piecewise constant because MLS §8.5 makes it change only at its
//! event, and a discrete or condition coordinate changes only at events; Boolean
//! operators preserve the property.

use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;

pub(super) fn is_guard(index: usize, length: usize) -> bool {
    index.is_multiple_of(2) && index + 1 < length
}

pub(super) fn values(
    operands: dae::ExpressionOperands<'_>,
) -> impl Iterator<Item = dae::ExprId<'_>> {
    operands
        .iter()
        .enumerate()
        .filter_map(move |(index, value)| (!is_guard(index, operands.len())).then_some(value))
}

pub(super) fn has_parameter_guards<'dae>(
    view: dae::DaeView<'dae>,
    context: &FunctionCallContext<'dae>,
    operands: dae::ExpressionOperands<'dae>,
) -> bool {
    operands.iter().enumerate().all(|(index, operand)| {
        !is_guard(index, operands.len()) || piecewise_constant_guard(view, context, operand)
    })
}

fn piecewise_constant_guard<'dae>(
    view: dae::DaeView<'dae>,
    context: &FunctionCallContext<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    if invariant_guard(view, context, expression) {
        return true;
    }
    let context = context.scoped_to_expression(view, expression);
    let Some(node) = view.expression(expression) else {
        return false;
    };
    match node.operation() {
        dae::ExpressionOperation::Coordinate(
            dae::CoordinateView::DiscreteValue(_)
            | dae::CoordinateView::PreDiscreteValue(_)
            | dae::CoordinateView::Condition(_),
        ) => true,
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Not,
            operand,
        } => piecewise_constant_guard(view, &context, operand),
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::And | dae::BinaryOperator::Or,
            lhs,
            rhs,
        } => {
            piecewise_constant_guard(view, &context, lhs)
                && piecewise_constant_guard(view, &context, rhs)
        }
        dae::ExpressionOperation::Conditional(operands) => operands
            .iter()
            .all(|operand| piecewise_constant_guard(view, &context, operand)),
        dae::ExpressionOperation::Binary {
            operator:
                dae::BinaryOperator::Equal
                | dae::BinaryOperator::NotEqual
                | dae::BinaryOperator::Less
                | dae::BinaryOperator::LessEqual
                | dae::BinaryOperator::Greater
                | dae::BinaryOperator::GreaterEqual,
            ..
        } => context.is_empty() && relation_owns_event(view, expression, node.provenance()),
        _ => false,
    }
}

/// MLS §8.5 fixes a relation between its events only when the relation owns
/// one: a scalar or structured root, or a time event. A relation under
/// `noEvent`, or inside `smooth` without an owned root, owns none.
fn relation_owns_event<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
) -> bool {
    view.relations()
        .any(|(_, relation)| relation.expression() == expression)
        || view
            .structured_roots()
            .any(|(_, root)| root.expression() == expression)
        || view
            .time_events()
            .any(|(_, event)| event.provenance().span() == provenance.span())
}

fn invariant_guard<'dae>(
    view: dae::DaeView<'dae>,
    context: &FunctionCallContext<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    let context = context.scoped_to_expression(view, expression);
    let Some(node) = view.expression(expression) else {
        return false;
    };
    match node.operation() {
        dae::ExpressionOperation::Literal(_)
        | dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(_)) => true,
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(parameter)) => {
            context
                .parameter_argument(parameter)
                .is_some_and(|argument| invariant_guard(view, &context, argument))
        }
        dae::ExpressionOperation::Unary { operand, .. } => invariant_guard(view, &context, operand),
        dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
            invariant_guard(view, &context, lhs) && invariant_guard(view, &context, rhs)
        }
        dae::ExpressionOperation::Index { base, subscripts } => {
            invariant_guard(view, &context, base)
                && subscripts.iter().all(|subscript| match subscript {
                    dae::SubscriptView::Whole { .. } => true,
                    dae::SubscriptView::Index { expression, .. }
                    | dae::SubscriptView::Slice { expression, .. } => {
                        invariant_guard(view, &context, expression)
                    }
                })
        }
        dae::ExpressionOperation::Array(operands)
        | dae::ExpressionOperation::Conditional(operands)
        | dae::ExpressionOperation::Builtin {
            arguments: operands,
            ..
        } => operands
            .iter()
            .all(|operand| invariant_guard(view, &context, operand)),
        _ => false,
    }
}

#[cfg(test)]
mod tests;
