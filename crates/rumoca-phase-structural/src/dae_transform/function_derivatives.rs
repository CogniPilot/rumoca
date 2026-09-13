use rumoca_core::FunctionDerivativeInput;
use rumoca_ir_dae as dae;

use rumoca_eval_dae::FunctionCallContext;

#[derive(Clone, Copy)]
pub(super) struct SelectedFunctionDerivative<'dae> {
    pub(super) link: dae::FunctionDerivativeView<'dae>,
    pub(super) output: usize,
    pub(super) arguments: dae::ExpressionOperands<'dae>,
}

/// Select an annotation only for the derivative order its call protocol covers.
/// Other orders still require the ordinary checked source-body proof.
pub(super) fn select_derivative<'dae>(
    view: dae::DaeView<'dae>,
    context: &FunctionCallContext<'dae>,
    expression: dae::ExprId<'dae>,
    order: u8,
) -> Option<SelectedFunctionDerivative<'dae>> {
    if order != 1 {
        return None;
    }
    let dae::ExpressionOperation::Call {
        function,
        output,
        arguments,
        ..
    } = view.expression(expression)?.operation()
    else {
        return None;
    };
    let link = view
        .function(function)?
        .derivatives()
        .filter(|link| link.previous().is_none())
        .filter(|link| {
            link.inputs()
                .iter()
                .zip(arguments.iter())
                .all(|(role, argument)| {
                    *role != FunctionDerivativeInput::ZeroDerivative
                        || argument_is_invariant(view, context, argument)
                })
        })
        .min_by_key(|link| link.priority())?;
    Some(SelectedFunctionDerivative {
        link,
        output: link.result(output as usize)?,
        arguments,
    })
}

fn argument_is_invariant<'dae>(
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
                .is_some_and(|argument| argument_is_invariant(view, &context, argument))
        }
        dae::ExpressionOperation::Unary { operand, .. } => {
            argument_is_invariant(view, &context, operand)
        }
        dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
            argument_is_invariant(view, &context, lhs) && argument_is_invariant(view, &context, rhs)
        }
        dae::ExpressionOperation::Array(arguments)
        | dae::ExpressionOperation::Record(arguments)
        | dae::ExpressionOperation::Builtin { arguments, .. }
        | dae::ExpressionOperation::Call { arguments, .. } => arguments
            .iter()
            .all(|argument| argument_is_invariant(view, &context, argument)),
        dae::ExpressionOperation::Field { base, field } => context
            .projected_field(view, base, field)
            .is_some_and(|(projected, nested)| argument_is_invariant(view, &nested, projected)),
        dae::ExpressionOperation::Index { base, subscripts } => {
            super::tensor_maps::has_invariant_subscripts(view, subscripts)
                && argument_is_invariant(view, &context, base)
        }
        _ => false,
    }
}
