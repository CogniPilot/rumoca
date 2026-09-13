//! Parameter guards remain fixed during continuous-time differentiation.

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
        !is_guard(index, operands.len()) || invariant_guard(view, context, operand)
    })
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
        dae::ExpressionOperation::Array(operands)
        | dae::ExpressionOperation::Builtin {
            arguments: operands,
            ..
        } => operands
            .iter()
            .all(|operand| invariant_guard(view, &context, operand)),
        _ => false,
    }
}
