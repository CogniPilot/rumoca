//! Exact value materialization and its source-state dependency proof.

use std::sync::Arc;

use super::super::builtin_profiles::is_materializable_builtin;
use super::{
    DifferentiationFacts, FunctionCallContext, Visit, VisitMarks, has_invariant_subscripts,
    is_differentiable_binary, projected_element,
};
use rumoca_ir_dae as dae;

/// The materialization witnesses of one immutable [`DifferentiationFacts`]
/// snapshot, shared by every root proved against it.
///
/// Every route the materialization walk follows from a coordinate is fixed by
/// the facts: an algebraic takes its auxiliary block, its component
/// definition, its zero pin, or its value definition, in that order, and every
/// operand list is a conjunction. Whether a coordinate's value materializes,
/// and from which states, is therefore a property of the facts alone and never
/// of the root that reached it: it fails exactly when its dependency closure is
/// cyclic or reaches a non-materializable leaf. Each algebraic value and state
/// derivative is proved once here and every later root reuses that witness, so
/// proving many roots over shared definition chains costs one walk of each
/// chain rather than one walk per root. The table borrows the facts it proves
/// against, so they cannot change while a witness is held.
pub(in crate::dae_transform) struct MaterializedAnchors<'facts> {
    facts: &'facts DifferentiationFacts,
    witnesses: rustc_hash::FxHashMap<WitnessKey, Witness>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum WitnessKey {
    Algebraic(u32),
    Derivative(u32),
}

enum Witness {
    /// On the current proof path; reaching it again closes a cycle.
    InProgress,
    Failed,
    /// The sorted, deduplicated states the value materializes from.
    Proved(Arc<[u32]>),
}

impl<'facts> MaterializedAnchors<'facts> {
    pub(in crate::dae_transform) fn new(facts: &'facts DifferentiationFacts) -> Self {
        Self {
            facts,
            witnesses: rustc_hash::FxHashMap::default(),
        }
    }

    /// The sorted, deduplicated states `expression` materializes from in
    /// `context`, or `None` when its value does not materialize.
    pub(in crate::dae_transform) fn state_anchors_in_context<'dae>(
        &mut self,
        view: dae::DaeView<'dae>,
        expression: u32,
        context: &FunctionCallContext<'dae>,
    ) -> Option<Vec<u32>> {
        let expression = view.expression_id(expression as usize)?;
        let mut states = Vec::new();
        if !can_materialize_holonomic_value_in_context(
            view,
            self,
            expression,
            &mut VisitMarks::default(),
            context,
            &mut states,
        ) {
            return None;
        }
        states.sort_unstable();
        states.dedup();
        Some(states)
    }

    pub(in crate::dae_transform) fn state_anchors(
        &mut self,
        view: dae::DaeView<'_>,
        expression: u32,
    ) -> Option<Vec<u32>> {
        self.state_anchors_in_context(view, expression, &FunctionCallContext::default())
    }

    pub(in crate::dae_transform) fn can_materialize(
        &mut self,
        view: dae::DaeView<'_>,
        expression: u32,
    ) -> bool {
        self.state_anchors(view, expression).is_some()
    }

    /// Prove `key` once with `prove`, which walks from fresh expression marks
    /// so the witness records every state of the closure, then extend `states`
    /// with the recorded witness.
    fn witnessed(
        &mut self,
        key: WitnessKey,
        states: &mut Vec<u32>,
        prove: impl FnOnce(&mut Self, &mut VisitMarks, &mut Vec<u32>) -> bool,
    ) -> bool {
        match self.witnesses.get(&key) {
            Some(Witness::Proved(proved)) => {
                states.extend_from_slice(proved);
                return true;
            }
            Some(Witness::Failed | Witness::InProgress) => return false,
            None => {}
        }
        #[cfg(test)]
        WITNESS_PROOFS.with(|count| count.set(count.get() + 1));
        self.witnesses.insert(key, Witness::InProgress);
        let mut proved = Vec::new();
        let witness = if prove(self, &mut VisitMarks::default(), &mut proved) {
            proved.sort_unstable();
            proved.dedup();
            states.extend_from_slice(&proved);
            Witness::Proved(proved.into())
        } else {
            Witness::Failed
        };
        let materializable = matches!(witness, Witness::Proved(_));
        self.witnesses.insert(key, witness);
        materializable
    }
}

#[cfg(test)]
thread_local! {
    static WITNESS_PROOFS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// How many coordinate witnesses this thread has proved.
#[cfg(test)]
pub(in crate::dae_transform) fn witness_proofs() -> usize {
    WITNESS_PROOFS.with(std::cell::Cell::get)
}

/// Whether the retained position-level residual can be reconstructed entirely
/// from exact state/invariant value anchors.
pub(super) fn can_materialize_holonomic_value<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExprId<'dae>,
    visited: &mut VisitMarks,
    context: &FunctionCallContext<'dae>,
) -> bool {
    can_materialize_holonomic_value_in_context(
        view,
        &mut MaterializedAnchors::new(facts),
        expression,
        visited,
        context,
        &mut Vec::new(),
    )
}

fn can_materialize_holonomic_value_in_context<'dae>(
    view: dae::DaeView<'dae>,
    anchors: &mut MaterializedAnchors<'_>,
    expression: dae::ExprId<'dae>,
    visited: &mut VisitMarks,
    context: &FunctionCallContext<'dae>,
    states: &mut Vec<u32>,
) -> bool {
    let scoped_context = context.scoped_to_expression(view, expression);
    let context = &scoped_context;
    if let Some(branch) = context.selected_branch(view, expression) {
        return can_materialize_holonomic_value_in_context(
            view, anchors, branch, visited, context, states,
        );
    }
    if let Some(element) = projected_element(view, anchors.facts, expression) {
        return can_materialize_holonomic_value_in_context(
            view, anchors, element, visited, context, states,
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
    let materializable = materialize_operation(view, anchors, expression, visited, context, states);
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
    anchors: &mut MaterializedAnchors<'_>,
    expression: dae::ExpressionView<'dae>,
    visited: &mut VisitMarks,
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
                        view, anchors, argument, visited, context, states,
                    )
                })
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => {
            if context.is_empty() {
                anchors.witnessed(
                    WitnessKey::Algebraic(algebraic.index()),
                    states,
                    |anchors, visited, states| {
                        materialize_algebraic(view, anchors, algebraic, visited, context, states)
                    },
                )
            } else {
                materialize_algebraic(view, anchors, algebraic, visited, context, states)
            }
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) => {
            if context.is_empty() {
                anchors.witnessed(
                    WitnessKey::Derivative(state.index()),
                    states,
                    |anchors, visited, states| {
                        materialize_derivative(view, anchors, state, visited, context, states)
                    },
                )
            } else {
                materialize_derivative(view, anchors, state, visited, context, states)
            }
        }
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => can_materialize_holonomic_value_in_context(
            view, anchors, operand, visited, context, states,
        ),
        dae::ExpressionOperation::Binary { operator, lhs, rhs }
            if is_differentiable_binary(operator) || operator == dae::BinaryOperator::Power =>
        {
            can_materialize_holonomic_value_in_context(view, anchors, lhs, visited, context, states)
                && can_materialize_holonomic_value_in_context(
                    view, anchors, rhs, visited, context, states,
                )
        }
        dae::ExpressionOperation::Array(elements)
        | dae::ExpressionOperation::Call {
            arguments: elements,
            ..
        } => elements.iter().all(|element| {
            can_materialize_holonomic_value_in_context(
                view, anchors, element, visited, context, states,
            )
        }),
        dae::ExpressionOperation::Conditional(operands) => {
            super::super::parameter_conditionals::has_parameter_guards(view, context, operands)
                && super::super::parameter_conditionals::values(operands).all(|value| {
                    can_materialize_holonomic_value_in_context(
                        view, anchors, value, visited, context, states,
                    )
                })
        }
        dae::ExpressionOperation::Field { base, field } => context
            .projected_field(view, base, field)
            .is_some_and(|(projected, nested)| {
                can_materialize_holonomic_value_in_context(
                    view, anchors, projected, visited, &nested, states,
                )
            }),
        dae::ExpressionOperation::Index { base, subscripts } => {
            has_invariant_subscripts(view, subscripts)
                && can_materialize_holonomic_value_in_context(
                    view, anchors, base, visited, context, states,
                )
        }
        dae::ExpressionOperation::Builtin { builtin, arguments }
            if is_materializable_builtin(builtin) =>
        {
            arguments.iter().all(|argument| {
                can_materialize_holonomic_value_in_context(
                    view, anchors, argument, visited, context, states,
                )
            })
        }
        _ => false,
    }
}

fn materialize_derivative<'dae>(
    view: dae::DaeView<'dae>,
    anchors: &mut MaterializedAnchors<'_>,
    state: dae::StateId<'dae>,
    visited: &mut VisitMarks,
    context: &FunctionCallContext<'dae>,
    states: &mut Vec<u32>,
) -> bool {
    anchors.facts.derivative_definitions[state.index() as usize]
        .and_then(|definition| view.expression_id(definition.expression as usize))
        .is_some_and(|definition| {
            can_materialize_holonomic_value_in_context(
                view, anchors, definition, visited, context, states,
            )
        })
}

fn materialize_algebraic<'dae>(
    view: dae::DaeView<'dae>,
    anchors: &mut MaterializedAnchors<'_>,
    algebraic: dae::AlgebraicId<'dae>,
    visited: &mut VisitMarks,
    context: &FunctionCallContext<'dae>,
    states: &mut Vec<u32>,
) -> bool {
    let facts = anchors.facts;
    if let Some(block) = &facts.auxiliary_blocks[algebraic.index() as usize] {
        states.extend_from_slice(&block.state_anchors);
        return true;
    }
    if let Some(definition) = &facts.component_definitions[algebraic.index() as usize] {
        return definition.leaves().into_iter().all(|leaf| {
            can_materialize_holonomic_value_in_context(
                view,
                anchors,
                view.expression_id(leaf as usize).unwrap(),
                visited,
                context,
                states,
            )
        });
    }
    if facts.is_zero_pinned(algebraic.index()) {
        return true;
    }
    facts
        .algebraic_value_definition(view, algebraic)
        .is_some_and(|(definition, _)| {
            can_materialize_holonomic_value_in_context(
                view, anchors, definition, visited, context, states,
            )
        })
}
