use rumoca_core::FunctionDerivativeInput;
use rumoca_ir_dae as dae;

use rumoca_eval_dae::FunctionCallContext;

#[derive(Clone, Copy)]
pub(super) struct DerivativeArgument<'dae> {
    pub(super) source: dae::ExprId<'dae>,
    pub(super) order: u8,
}

pub(super) struct SelectedFunctionDerivative<'dae> {
    pub(super) source: dae::ExprId<'dae>,
    pub(super) chain: Vec<dae::FunctionDerivativeView<'dae>>,
    pub(super) arguments: Vec<DerivativeArgument<'dae>>,
}

impl SelectedFunctionDerivative<'_> {
    fn append_tangents(&mut self) {
        for ordinal in self
            .chain
            .last()
            .expect("selected nonempty chain")
            .tangent_inputs()
        {
            let previous = self.arguments[ordinal];
            self.arguments.push(DerivativeArgument {
                source: previous.source,
                order: previous.order + 1,
            });
        }
    }
}

/// Select an annotation only for the derivative order its call protocol covers.
/// Other orders still require the ordinary checked source-body proof.
pub(super) fn select_derivative<'dae>(
    view: dae::DaeView<'dae>,
    context: &FunctionCallContext<'dae>,
    expression: dae::ExprId<'dae>,
    order: u8,
) -> Option<SelectedFunctionDerivative<'dae>> {
    if order == 0 {
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
    let previous = view
        .expression(expression)?
        .call_derivative()
        .map(|(_, link)| link);
    let link = view
        .function(function)?
        .derivatives()
        .filter(|link| link.previous().is_none() || link.previous() == previous)
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
    let mut selected = SelectedFunctionDerivative {
        source: expression,
        chain: vec![link],
        arguments: arguments
            .iter()
            .map(|source| DerivativeArgument { source, order: 0 })
            .collect(),
    };
    let mut output = link.result(output as usize)?;
    selected.append_tangents();
    for _ in 1..order {
        let last = selected.chain.last()?;
        let next = view
            .function(last.target())?
            .derivatives()
            .filter(|link| link.previous() == Some(last.id()))
            .filter(|link| {
                link.inputs()
                    .iter()
                    .zip(&selected.arguments)
                    .all(|(role, argument)| {
                        *role != FunctionDerivativeInput::ZeroDerivative
                            || argument_is_invariant(view, context, argument.source)
                    })
            })
            .min_by_key(|link| link.priority())?;
        output = next.result(output)?;
        selected.chain.push(next);
        selected.append_tangents();
    }
    Some(selected)
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
