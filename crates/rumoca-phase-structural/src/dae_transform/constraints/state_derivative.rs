//! Proof that a first derivative can be retained as a state-only manifold.

use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;

use super::{
    DifferentiationFacts, EqualityAnchor, Visit, can_materialize_holonomic_value,
    can_materialize_holonomic_value_in_context, has_invariant_subscripts, is_differentiable_binary,
    is_differentiable_builtin, projected_element,
};

pub(super) fn has_state_only_first_derivative<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExprId<'dae>,
    visited: &mut [Visit],
    state_visited: &mut [Visit],
    value_visited: &mut [Visit],
) -> bool {
    StateDerivativeWalk {
        view,
        facts,
        visited,
        state_visited,
        value_visited,
    }
    .expression(expression, &FunctionCallContext::default())
}

struct StateDerivativeWalk<'facts, 'dae> {
    view: dae::DaeView<'dae>,
    facts: &'facts DifferentiationFacts,
    visited: &'facts mut [Visit],
    state_visited: &'facts mut [Visit],
    value_visited: &'facts mut [Visit],
}

impl<'dae> StateDerivativeWalk<'_, 'dae> {
    fn expression(
        &mut self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> bool {
        let context = context.scoped_to_expression(self.view, expression);
        if let Some(branch) = context.selected_branch(self.view, expression) {
            return self.expression(branch, &context);
        }
        if let Some(element) = projected_element(self.view, self.facts, expression) {
            return self.expression(element, &context);
        }
        if let Some(selected) = super::super::function_derivatives::select_derivative(
            self.view, &context, expression, 1,
        ) {
            return selected
                .arguments
                .iter()
                .all(|argument| match argument.order {
                    0 => can_materialize_holonomic_value_in_context(
                        self.view,
                        self.facts,
                        argument.source,
                        self.value_visited,
                        &context,
                        &mut Vec::new(),
                    ),
                    1 => self.expression(argument.source, &context),
                    _ => false,
                });
        }
        let index = expression.index() as usize;
        if context.is_empty() {
            match self.visited[index] {
                Visit::Differentiable => return true,
                Visit::InProgress => return false,
                Visit::Pending => self.visited[index] = Visit::InProgress,
            }
        }
        let materializable =
            if let Some((result, nested)) = context.call_result(self.view, expression) {
                self.expression(result, &nested)
            } else {
                self.view
                    .expression(expression)
                    .is_some_and(|node| self.operation(node.operation(), &context))
            };
        if context.is_empty() {
            self.visited[index] = visit_result(materializable);
        }
        materializable
    }

    fn operation(
        &mut self,
        operation: dae::ExpressionOperation<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> bool {
        match operation {
            dae::ExpressionOperation::Array(elements) => elements
                .iter()
                .all(|element| self.expression(element, context)),
            dae::ExpressionOperation::Conditional(operands) => {
                super::super::parameter_conditionals::has_parameter_guards(
                    self.view, context, operands,
                ) && super::super::parameter_conditionals::values(operands)
                    .all(|value| self.expression(value, context))
            }
            dae::ExpressionOperation::Literal(_)
            | dae::ExpressionOperation::Coordinate(
                dae::CoordinateView::Parameter(_) | dae::CoordinateView::Time,
            ) => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => context
                .parameter_argument(parameter)
                .is_some_and(|argument| self.expression(argument, context)),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) => {
                self.state(state.index())
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => {
                self.algebraic(algebraic, context)
            }
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => self.expression(operand, context),
            dae::ExpressionOperation::Index { base, subscripts } => {
                has_invariant_subscripts(self.view, subscripts) && self.expression(base, context)
            }
            dae::ExpressionOperation::Field { base, field } => context
                .projected_field(self.view, base, field)
                .is_some_and(|(projected, nested)| self.expression(projected, &nested)),
            dae::ExpressionOperation::Builtin { builtin, arguments }
                if is_differentiable_builtin(builtin, 1) =>
            {
                arguments
                    .iter()
                    .all(|argument| self.expression(argument, context))
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs }
                if is_differentiable_binary(operator) =>
            {
                self.expression(lhs, context) && self.expression(rhs, context)
            }
            _ => false,
        }
    }

    fn algebraic(
        &mut self,
        algebraic: dae::AlgebraicId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> bool {
        if let Some(block) = &self.facts.auxiliary_blocks[algebraic.index() as usize] {
            return block.operands().all(|operand| {
                self.expression(
                    self.view
                        .expression_id(operand.expression as usize)
                        .unwrap(),
                    &operand.context(self.view),
                )
            });
        }
        match self.facts.equalities.value_anchor_of(algebraic.index()) {
            Some((EqualityAnchor::Invariant { .. }, _)) => true,
            Some((EqualityAnchor::State(state), _)) => self.state(state),
            None => self
                .facts
                .algebraic_definition(self.view, algebraic)
                .is_some_and(|definition| self.expression(definition, context)),
        }
    }

    fn state(&mut self, state: u32) -> bool {
        match self.state_visited[state as usize] {
            Visit::Differentiable => return true,
            Visit::InProgress => return false,
            Visit::Pending => self.state_visited[state as usize] = Visit::InProgress,
        }
        let materializable = self.facts.derivative_definitions[state as usize]
            .and_then(|definition| self.view.expression_id(definition as usize))
            .is_some_and(|definition| {
                can_materialize_holonomic_value(
                    self.view,
                    self.facts,
                    definition,
                    self.value_visited,
                )
            });
        self.state_visited[state as usize] = visit_result(materializable);
        materializable
    }
}

fn visit_result(materializable: bool) -> Visit {
    if materializable {
        Visit::Differentiable
    } else {
        Visit::Pending
    }
}
