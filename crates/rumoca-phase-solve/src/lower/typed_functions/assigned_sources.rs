//! Source-owned diagnostic identity for an executed-path assignedness action.

use super::*;

#[derive(Clone, Copy)]
pub enum FunctionAssertionMessage<'dae> {
    Expression(dae::ExprId<'dae>),
    UnassignedValue(dae::FunctionValueId<'dae>),
}

impl<'dae> FunctionAssertionMessage<'dae> {
    pub fn expression(self) -> Option<dae::ExprId<'dae>> {
        match self {
            Self::Expression(expression) => Some(expression),
            Self::UnassignedValue(_) => None,
        }
    }

    pub fn literal(self, view: dae::DaeView<'dae>) -> Option<String> {
        match self {
            Self::Expression(expression) => match view.expression(expression)?.operation() {
                dae::ExpressionOperation::Literal(dae::DaeLiteral::String(message)) => {
                    Some(message.clone())
                }
                _ => None,
            },
            Self::UnassignedValue(value) => {
                let source = view
                    .function(value.function())?
                    .values()
                    .find(|source| source.id() == value)?;
                Some(format!("`{}` is used without a value", source.name()))
            }
        }
    }
}

#[derive(Clone, Copy)]
pub(super) enum FunctionAssertionCondition<'dae> {
    Expression(dae::ExprId<'dae>),
    Assigned {
        conditional: dae::FunctionConditionalBodyView<'dae>,
        value: dae::FunctionValueId<'dae>,
    },
}

impl<'dae> FunctionAssertionCondition<'dae> {
    pub(super) fn inputs(self) -> Vec<dae::ExprId<'dae>> {
        match self {
            Self::Expression(expression) => vec![expression],
            Self::Assigned { conditional, value } => {
                let mut inputs = Vec::new();
                conditional.visit_value_inputs(value, |input| inputs.push(input));
                inputs
            }
        }
    }
}

pub(super) fn retain_required_states<'dae>(
    view: dae::DaeView<'dae>,
    statements: dae::FunctionStatements<'dae>,
    owners: &mut HashMap<dae::FunctionConditionalId<'dae>, assertions::ConditionalStatements<'dae>>,
) -> Result<(), solve::SolveProgramConstructionError> {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::RequireAssigned {
                conditional, value, ..
            } => {
                retain_state_source(view, conditional, value, owners)?;
            }
            dae::FunctionStatementView::Conditional { conditional } => {
                for branch in conditional
                    .branches()
                    .chain(std::iter::once(conditional.fallback()))
                {
                    retain_required_states(view, branch.statements(), owners)?;
                }
            }
            dae::FunctionStatementView::For { statements, .. } => {
                retain_required_states(view, statements, owners)?
            }
            _ => {}
        }
    }
    Ok(())
}

fn retain_state_source<'dae>(
    view: dae::DaeView<'dae>,
    source: dae::FunctionConditionalBodyView<'dae>,
    value: dae::FunctionValueId<'dae>,
    owners: &mut HashMap<dae::FunctionConditionalId<'dae>, assertions::ConditionalStatements<'dae>>,
) -> Result<(), solve::SolveProgramConstructionError> {
    let owner = owners
        .get_mut(&source.id())
        .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
    if owner.states.contains(&value) {
        return Ok(());
    }
    owner.states.push(value);
    for branch in source.branches().chain(std::iter::once(source.fallback())) {
        if let Some(dae::FunctionAssignedState::Conditional { owner, value }) =
            branch.outgoing(value)
        {
            let source = view
                .function_conditional(owner)
                .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
            retain_state_source(view, source, value, owners)?;
        }
    }
    Ok(())
}
