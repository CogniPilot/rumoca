//! The typed literal an expression reduces to by substitution.
//!
//! A function body is a sequence of definitions, each reading the ones before
//! it, so the literal of a long chain depends on a dependency per link. The
//! reduction therefore runs over an explicit work stack: a step that reads a
//! dependency not yet reduced stops and names it, the driver reduces that one
//! first, and the step then runs again over the completed reductions. The
//! native stack stays constant whatever the chain length, and a step requests
//! its dependencies in the order a recursive reduction would, so the answer is
//! the same.

use super::*;

/// A reduction a step needs before it can finish.
#[derive(Clone, Copy)]
enum Needed<'frame, 'dae> {
    Expression(dae::ExprId<'dae>, Option<&'frame CallFrame<'frame, 'dae>>),
    State(
        dae::FunctionConditionalId<'dae>,
        dae::FunctionValueId<'dae>,
        Option<&'frame CallFrame<'frame, 'dae>>,
    ),
}

impl<'dae> Needed<'_, 'dae> {
    fn completed(&self, query: &ProofQuery<'dae>) -> Option<Option<NumericSourceScalar>> {
        match *self {
            Self::Expression(expression, frame) => query
                .literal
                .completed(&proof::expression_key(expression, frame)),
            Self::State(conditional, value, frame) => query
                .literal_states
                .completed(&proof::state_key(conditional, value, frame)),
        }
    }

    fn complete(&self, query: &ProofQuery<'dae>, answer: Option<NumericSourceScalar>) {
        match *self {
            Self::Expression(expression, frame) => query
                .literal
                .complete(proof::expression_key(expression, frame), answer),
            Self::State(conditional, value, frame) => query
                .literal_states
                .complete(proof::state_key(conditional, value, frame), answer),
        }
    }
}

impl<'frame, 'dae> Needed<'frame, 'dae> {
    /// The reduction when its dependencies are complete, else the first one
    /// that is not.
    fn reduce(
        &self,
        view: dae::DaeView<'dae>,
        query: &ProofQuery<'dae>,
    ) -> Result<Option<NumericSourceScalar>, Self> {
        match *self {
            Self::Expression(expression, frame) => {
                reduce_expression(view, expression, frame, query)
            }
            Self::State(conditional, value, frame) => {
                reduce_state(view, conditional, value, frame, query)
            }
        }
    }
}

/// The typed literal `expression` reduces to by substitution; `None` for
/// anything that reads a runtime coordinate or requires unsupported arithmetic.
pub(super) fn literal<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    frame: Option<&CallFrame<'_, 'dae>>,
    query: &ProofQuery<'dae>,
) -> Option<NumericSourceScalar> {
    let root = Needed::Expression(expression, frame);
    let mut pending = vec![root];
    while let Some(top) = pending.last().copied() {
        if top.completed(query).is_some() {
            pending.pop();
            continue;
        }
        match top.reduce(view, query) {
            Ok(answer) => {
                top.complete(query, answer);
                pending.pop();
            }
            Err(needed) => pending.push(needed),
        }
    }
    root.completed(query).flatten()
}

fn ask<'frame, 'dae>(
    needed: Needed<'frame, 'dae>,
    query: &ProofQuery<'dae>,
) -> Result<Option<NumericSourceScalar>, Needed<'frame, 'dae>> {
    needed.completed(query).ok_or(needed)
}

fn reduce_expression<'frame, 'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    frame: Option<&'frame CallFrame<'frame, 'dae>>,
    query: &ProofQuery<'dae>,
) -> Result<Option<NumericSourceScalar>, Needed<'frame, 'dae>> {
    let Some(node) = view.expression(expression) else {
        return Ok(None);
    };
    let value = match forward(view, expression, frame) {
        Forward::Expression(forwarded, frame) => ask(Needed::Expression(forwarded, frame), query)?,
        Forward::Opaque => match node.operation() {
            dae::ExpressionOperation::FunctionConditionalOutput {
                conditional, value, ..
            } => ask(Needed::State(conditional, value, frame), query)?,
            dae::ExpressionOperation::Literal(value) => match value {
                dae::DaeLiteral::Real(value) if value.is_finite() => {
                    Some(NumericSourceScalar::Real(*value))
                }
                dae::DaeLiteral::Integer(value) => Some(NumericSourceScalar::Integer(*value)),
                dae::DaeLiteral::Boolean(value) => Some(NumericSourceScalar::Boolean(*value)),
                _ => None,
            },
            dae::ExpressionOperation::Unary { operator, operand } => {
                ask(Needed::Expression(operand, frame), query)?
                    .and_then(|value| proof::unary_literal(operator, value))
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                match ask(Needed::Expression(lhs, frame), query)? {
                    Some(lhs) => ask(Needed::Expression(rhs, frame), query)?
                        .and_then(|rhs| proof::binary_literal(operator, lhs, rhs)),
                    None => None,
                }
            }
            _ => None,
        },
    };
    Ok(value.and_then(|value| proof::typed_literal(value, node.value_type().scalar_type())))
}

fn reduce_state<'frame, 'dae>(
    view: dae::DaeView<'dae>,
    conditional: dae::FunctionConditionalId<'dae>,
    value: dae::FunctionValueId<'dae>,
    frame: Option<&'frame CallFrame<'frame, 'dae>>,
    query: &ProofQuery<'dae>,
) -> Result<Option<NumericSourceScalar>, Needed<'frame, 'dae>> {
    let Some(owner) = view.function_conditional(conditional) else {
        return Ok(None);
    };
    scoped::reduce_outgoing(
        owner,
        value,
        |condition| ask(Needed::Expression(condition, frame), query),
        |assigned| match assigned {
            dae::FunctionAssignedState::Assigned(definition) => {
                match view.function_definition(definition) {
                    Some(definition) => ask(Needed::Expression(definition.rhs(), frame), query),
                    None => Ok(None),
                }
            }
            dae::FunctionAssignedState::Conditional { owner, value } => {
                ask(Needed::State(owner, value, frame), query)
            }
            dae::FunctionAssignedState::Unassigned => Ok(None),
        },
    )
}
