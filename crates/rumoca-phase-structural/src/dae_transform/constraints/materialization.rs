//! Exact value materialization and its source-state dependency proof.

use super::super::builtin_profiles::is_materializable_builtin;
use super::{
    DifferentiationFacts, FunctionCallContext, Visit, has_invariant_subscripts,
    is_differentiable_binary, projected_element,
};
use rumoca_ir_dae as dae;

/// Whether the retained position-level residual can be reconstructed entirely
/// from exact state/invariant value anchors.
pub(super) fn can_materialize_holonomic_value<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExprId<'dae>,
    visited: &mut [Visit],
) -> bool {
    can_materialize_holonomic_value_in_context(
        view,
        facts,
        expression,
        visited,
        &FunctionCallContext::default(),
        &mut Vec::new(),
    )
}

pub(super) fn can_materialize_holonomic_value_in_context<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExprId<'dae>,
    visited: &mut [Visit],
    context: &FunctionCallContext<'dae>,
    states: &mut Vec<u32>,
) -> bool {
    let scoped_context = context.scoped_to_expression(view, expression);
    let context = &scoped_context;
    if let Some(branch) = context.selected_branch(view, expression) {
        return can_materialize_holonomic_value_in_context(
            view, facts, branch, visited, context, states,
        );
    }
    if let Some(element) = projected_element(view, facts, expression) {
        return can_materialize_holonomic_value_in_context(
            view, facts, element, visited, context, states,
        );
    }
    let index = expression.index() as usize;
    if context.is_empty() {
        match visited[index] {
            Visit::Differentiable => return true,
            Visit::InProgress => return false,
            Visit::Pending => visited[index] = Visit::InProgress,
        }
    }
    let Some(expression) = view.expression(expression) else {
        return false;
    };
    let materializable = materialize_operation(view, facts, expression, visited, context, states);
    if context.is_empty() {
        visited[index] = if materializable {
            Visit::Differentiable
        } else {
            Visit::Pending
        };
    }
    materializable
}

fn materialize_operation<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExpressionView<'dae>,
    visited: &mut [Visit],
    context: &FunctionCallContext<'dae>,
    states: &mut Vec<u32>,
) -> bool {
    match expression.operation() {
        dae::ExpressionOperation::Literal(_)
        | dae::ExpressionOperation::Coordinate(
            dae::CoordinateView::Parameter(_) | dae::CoordinateView::Time,
        ) => true,
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) => {
            states.push(state.index());
            true
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(parameter)) => {
            context
                .parameter_argument(parameter)
                .is_some_and(|argument| {
                    can_materialize_holonomic_value_in_context(
                        view, facts, argument, visited, context, states,
                    )
                })
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => {
            materialize_algebraic(view, facts, algebraic, visited, context, states)
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) => facts
            .derivative_definitions[state.index() as usize]
            .and_then(|definition| view.expression_id(definition.expression as usize))
            .is_some_and(|definition| {
                can_materialize_holonomic_value_in_context(
                    view, facts, definition, visited, context, states,
                )
            }),
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => can_materialize_holonomic_value_in_context(
            view, facts, operand, visited, context, states,
        ),
        dae::ExpressionOperation::Binary { operator, lhs, rhs }
            if is_differentiable_binary(operator) =>
        {
            can_materialize_holonomic_value_in_context(view, facts, lhs, visited, context, states)
                && can_materialize_holonomic_value_in_context(
                    view, facts, rhs, visited, context, states,
                )
        }
        dae::ExpressionOperation::Array(elements)
        | dae::ExpressionOperation::Call {
            arguments: elements,
            ..
        } => elements.iter().all(|element| {
            can_materialize_holonomic_value_in_context(
                view, facts, element, visited, context, states,
            )
        }),
        dae::ExpressionOperation::Conditional(operands) => {
            super::super::parameter_conditionals::has_parameter_guards(view, context, operands)
                && super::super::parameter_conditionals::values(operands).all(|value| {
                    can_materialize_holonomic_value_in_context(
                        view, facts, value, visited, context, states,
                    )
                })
        }
        dae::ExpressionOperation::Field { base, field } => context
            .projected_field(view, base, field)
            .is_some_and(|(projected, nested)| {
                can_materialize_holonomic_value_in_context(
                    view, facts, projected, visited, &nested, states,
                )
            }),
        dae::ExpressionOperation::Index { base, subscripts } => {
            has_invariant_subscripts(view, subscripts)
                && can_materialize_holonomic_value_in_context(
                    view, facts, base, visited, context, states,
                )
        }
        dae::ExpressionOperation::Builtin { builtin, arguments }
            if is_materializable_builtin(builtin) =>
        {
            arguments.iter().all(|argument| {
                can_materialize_holonomic_value_in_context(
                    view, facts, argument, visited, context, states,
                )
            })
        }
        _ => false,
    }
}

fn materialize_algebraic<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    algebraic: dae::AlgebraicId<'dae>,
    visited: &mut [Visit],
    context: &FunctionCallContext<'dae>,
    states: &mut Vec<u32>,
) -> bool {
    if let Some(block) = &facts.auxiliary_blocks[algebraic.index() as usize] {
        states.extend_from_slice(&block.state_anchors);
        return true;
    }
    if let Some(definition) = &facts.component_definitions[algebraic.index() as usize] {
        return definition.leaves().into_iter().all(|leaf| {
            can_materialize_holonomic_value_in_context(
                view,
                facts,
                view.expression_id(leaf as usize).unwrap(),
                visited,
                context,
                states,
            )
        });
    }
    facts
        .equalities
        .value_anchor_of(algebraic.index())
        .and_then(|(anchor, _)| facts.equalities.payload_anchor_expression(anchor))
        .and_then(|anchor| view.expression_id(anchor as usize))
        .or_else(|| facts.algebraic_definition(view, algebraic))
        .is_some_and(|definition| {
            can_materialize_holonomic_value_in_context(
                view, facts, definition, visited, context, states,
            )
        })
}
