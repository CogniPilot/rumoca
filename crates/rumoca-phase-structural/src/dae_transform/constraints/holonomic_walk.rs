//! The differentiability walk behind a holonomic or lifted-algebraic proof.
//!
//! It mirrors the reconstruction's differentiation one operation at a time
//! under the current function-call substitution, recording the states the
//! derivative is anchored to.

use super::super::smooth_order::smooth_operands;
use super::*;

pub(super) struct HolonomicProofWalk<'facts, 'dae> {
    pub(super) view: dae::DaeView<'dae>,
    pub(super) facts: &'facts DifferentiationFacts,
    pub(super) excluded_residual: u32,
    pub(super) derivative_anchors: DerivativeAnchors,
    pub(super) anchored_states: Vec<u32>,
    pub(super) function_context: FunctionCallContext<'dae>,
    pub(super) scratch: &'facts mut HolonomicProofScratch,
    /// The differentiability order certified by the innermost enclosing
    /// `smooth(p, ..)` of the operand being walked (MLS §3.7.5); zero outside
    /// one. It does not extend past a coordinate or a function argument, which
    /// the certificate does not cover.
    pub(super) smooth_order: u8,
}

impl<'facts, 'dae> HolonomicProofWalk<'facts, 'dae> {
    fn can_apply_derivative(
        &mut self,
        selected: super::super::function_derivatives::SelectedFunctionDerivative<'dae>,
        on_residual: bool,
    ) -> bool {
        let mut value_visited = VisitMarks::default();
        selected.arguments.iter().all(|argument| {
            if argument.order == 0 {
                can_materialize_holonomic_value(
                    self.view,
                    self.facts,
                    argument.source,
                    &mut value_visited,
                    &self.function_context,
                )
            } else {
                self.can_differentiate_order(argument.source, argument.order, on_residual)
            }
        })
    }

    pub(super) fn can_differentiate_order(
        &mut self,
        expression: dae::ExprId<'dae>,
        order: u8,
        on_residual: bool,
    ) -> bool {
        let scoped_context = self
            .function_context
            .scoped_to_expression(self.view, expression);
        let previous_context = std::mem::replace(&mut self.function_context, scoped_context);
        let differentiable = self.can_differentiate_order_scoped(expression, order, on_residual);
        self.function_context = previous_context;
        differentiable
    }

    fn can_differentiate_order_scoped(
        &mut self,
        expression: dae::ExprId<'dae>,
        order: u8,
        on_residual: bool,
    ) -> bool {
        if let Some(branch) = self.function_context.selected_branch(self.view, expression) {
            return self.can_differentiate_order(branch, order, on_residual);
        }
        if let Some(element) = projected_element(self.view, self.facts, expression) {
            return self.can_differentiate_order(element, order, on_residual);
        }
        let index = expression.index() as usize;
        let context = usize::from(on_residual);
        if let Some(selected) = super::super::function_derivatives::select_derivative(
            self.view,
            &self.function_context,
            expression,
            order,
        ) {
            return self
                .facts
                .expression_is_zero(self.view, expression, &self.function_context)
                || self.can_apply_derivative(selected, on_residual);
        }
        if self.function_context.is_empty() && self.smooth_order == 0 {
            match self.scratch.state(index, order as usize, context) {
                Visit::Differentiable => return true,
                Visit::InProgress => return false,
                Visit::Pending => {
                    self.scratch
                        .set_state(index, order as usize, context, Visit::InProgress);
                }
            }
        }
        if let Some((result, nested)) = self.function_context.call_result(self.view, expression) {
            let previous = std::mem::replace(&mut self.function_context, nested);
            let differentiable = self.can_differentiate_order(result, order, on_residual);
            self.function_context = previous;
            self.cache_differentiability(index, order, context, differentiable);
            return differentiable;
        }
        if self
            .facts
            .expression_is_zero(self.view, expression, &self.function_context)
        {
            self.cache_differentiability(index, order, context, true);
            return true;
        }
        if let Some(argument) = forwarded_call_argument(self.view, expression) {
            let differentiable = self.can_differentiate_order(argument, order, on_residual);
            self.cache_differentiability(index, order, context, differentiable);
            return differentiable;
        }
        let expression_id = expression;
        let expression = self
            .view
            .expression(expression)
            .expect("checked differentiability expression resolves");
        if self.function_context.is_empty() && is_time_invariant(self.view, expression_id) {
            self.cache_differentiability(index, order, context, true);
            return true;
        }
        let differentiable = self.can_differentiate_operation(expression, order, on_residual);
        self.cache_differentiability(index, order, context, differentiable);
        differentiable
    }

    fn can_differentiate_operation(
        &mut self,
        expression: dae::ExpressionView<'dae>,
        order: u8,
        on_residual: bool,
    ) -> bool {
        match expression.operation() {
            dae::ExpressionOperation::Literal(_) => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self
                .function_context
                .parameter_argument(parameter)
                .is_some_and(|argument| {
                    self.with_smooth_order(0, |walk| {
                        walk.can_differentiate_order(argument, order, on_residual)
                    })
                }),
            dae::ExpressionOperation::Coordinate(coordinate) => self.with_smooth_order(0, |walk| {
                walk.can_differentiate_coordinate(coordinate, order, on_residual)
            }),
            dae::ExpressionOperation::Builtin {
                builtin: dae::PureBuiltin::Smooth,
                arguments,
            } => smooth_operands(self.view, arguments).is_some_and(|(smoothness, value)| {
                let certified = self.smooth_order.max(smoothness);
                self.with_smooth_order(certified, |walk| {
                    walk.can_differentiate_order(value, order, on_residual)
                })
            }),
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => self.can_differentiate_order(operand, order, on_residual),
            dae::ExpressionOperation::Binary { operator, lhs, rhs }
                if is_differentiable_power(self.view, operator, lhs, rhs) =>
            {
                self.can_differentiate_order(lhs, order, on_residual)
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                is_differentiable_binary(operator)
                    && self.can_differentiate_order(lhs, order, on_residual)
                    && self.can_differentiate_order(rhs, order, on_residual)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                has_invariant_subscripts(self.view, subscripts)
                    && self.can_differentiate_order(base, order, on_residual)
            }
            dae::ExpressionOperation::Builtin { builtin, arguments }
                if is_differentiable_builtin(builtin, order) =>
            {
                arguments
                    .iter()
                    .all(|argument| self.can_differentiate_order(argument, order, on_residual))
            }
            dae::ExpressionOperation::Array(elements) => elements
                .iter()
                .all(|element| self.can_differentiate_order(element, order, on_residual)),
            dae::ExpressionOperation::Conditional(operands) => {
                (self.smooth_order >= order
                    || super::super::parameter_conditionals::has_parameter_guards(
                        self.view,
                        &self.function_context,
                        operands,
                    ))
                    && super::super::parameter_conditionals::values(operands)
                        .all(|value| self.can_differentiate_order(value, order, on_residual))
            }
            dae::ExpressionOperation::Field { base, field } => self
                .function_context
                .projected_field(self.view, base, field)
                .is_some_and(|(projected, projected_context)| {
                    let previous = std::mem::replace(&mut self.function_context, projected_context);
                    let differentiable =
                        self.can_differentiate_order(projected, order, on_residual);
                    self.function_context = previous;
                    differentiable
                }),
            _ => false,
        }
    }

    /// Walk under the smoothness certificate `smooth_order`, restoring the
    /// enclosing certificate afterwards.
    fn with_smooth_order<T>(&mut self, smooth_order: u8, walk: impl FnOnce(&mut Self) -> T) -> T {
        let enclosing = std::mem::replace(&mut self.smooth_order, smooth_order);
        let result = walk(self);
        self.smooth_order = enclosing;
        result
    }

    fn cache_differentiability(
        &mut self,
        expression: usize,
        order: u8,
        context: usize,
        differentiable: bool,
    ) {
        if self.function_context.is_empty() && self.smooth_order == 0 {
            self.scratch.set_state(
                expression,
                order as usize,
                context,
                if differentiable {
                    Visit::Differentiable
                } else {
                    Visit::Pending
                },
            );
        }
    }

    fn can_differentiate_coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        order: u8,
        on_residual: bool,
    ) -> bool {
        match coordinate {
            dae::CoordinateView::Parameter(_) | dae::CoordinateView::Time => true,
            dae::CoordinateView::State(state) => {
                self.can_differentiate_state(state.index(), order, on_residual)
            }
            dae::CoordinateView::Derivative(state) => self.facts.derivative_definitions
                [state.index() as usize]
                .is_some_and(|definition| {
                    definition.residual != self.excluded_residual
                        && self.can_differentiate_order(
                            self.view
                                .expression_id(definition.expression as usize)
                                .unwrap(),
                            order,
                            on_residual,
                        )
                }),
            dae::CoordinateView::Algebraic(algebraic) => {
                self.can_differentiate_algebraic(algebraic, order, on_residual)
            }
            _ => false,
        }
    }

    /// An algebraic is differentiable through its auxiliary block, its component
    /// definition, its equality anchor, or its explicit definition.
    fn can_differentiate_algebraic(
        &mut self,
        algebraic: dae::AlgebraicId<'dae>,
        order: u8,
        on_residual: bool,
    ) -> bool {
        if let Some(block) = self.facts.auxiliary_blocks[algebraic.index() as usize].clone() {
            return self.can_differentiate_auxiliary(&block, order, on_residual);
        }
        if let Some(definition) =
            self.facts.component_definitions[algebraic.index() as usize].clone()
        {
            return definition.leaves().into_iter().all(|leaf| {
                self.can_differentiate_order(
                    self.view.expression_id(leaf as usize).unwrap(),
                    order,
                    on_residual,
                )
            });
        }
        match self
            .facts
            .equalities
            .derivative_anchor(algebraic.index(), self.derivative_anchors)
        {
            Some((EqualityAnchor::Invariant { .. }, _)) => true,
            Some((anchor @ EqualityAnchor::State(state), _)) => {
                self.can_differentiate_equality_anchor(anchor, state, order, on_residual)
            }
            None => self
                .facts
                .algebraic_definition(self.view, algebraic)
                .is_some_and(|definition| {
                    self.can_differentiate_order(definition, order, on_residual)
                }),
        }
    }

    fn can_differentiate_auxiliary(
        &mut self,
        block: &super::super::auxiliary_blocks::AuxiliaryBlock,
        order: u8,
        on_residual: bool,
    ) -> bool {
        if block.contains_residual(self.excluded_residual) {
            return false;
        }
        if on_residual {
            self.anchored_states.extend_from_slice(&block.state_anchors);
        }
        block.operands().all(|operand| {
            let previous =
                std::mem::replace(&mut self.function_context, operand.context(self.view));
            let result = self.can_differentiate_order(
                self.view
                    .expression_id(operand.expression as usize)
                    .unwrap(),
                order,
                false,
            );
            self.function_context = previous;
            result
        })
    }

    fn can_differentiate_equality_anchor(
        &mut self,
        anchor: EqualityAnchor,
        state: u32,
        order: u8,
        on_residual: bool,
    ) -> bool {
        let Some(expression) = self
            .facts
            .equalities
            .anchor_expression(
                self.view,
                anchor,
                self.view
                    .variable(self.view.variable_id(state as usize).unwrap())
                    .unwrap()
                    .value_type(),
            )
            .and_then(|expression| self.view.expression_id(expression as usize))
            .and_then(|expression| self.view.expression(expression))
        else {
            return false;
        };
        matches!(
            expression.operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(candidate))
                if candidate.index() == state
        ) && self.can_differentiate_state(state, order, on_residual)
    }

    fn can_differentiate_state(&mut self, state: u32, order: u8, on_residual: bool) -> bool {
        if on_residual {
            self.anchored_states.push(state);
        }
        order == 1
            || self.facts.derivative_definitions[state as usize].is_some_and(|definition| {
                self.view
                    .expression_id(definition.expression as usize)
                    .is_some_and(|definition| {
                        self.can_differentiate_order(definition, order - 1, false)
                    })
            })
    }
}
