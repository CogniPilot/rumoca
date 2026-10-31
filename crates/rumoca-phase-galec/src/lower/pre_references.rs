//! Collection of exact `pre(...)` dependencies required by GALEC state projection.

use std::collections::HashSet;

use super::*;

pub(super) fn referenced_pre_variables<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
) -> Result<Vec<dae::VariableId<'dae>>, Vec<GalecTargetError>> {
    let mut ids = Vec::new();
    let mut seen = HashSet::new();
    collect_discrete_real_pre(view, &mut seen, &mut ids)?;
    collect_event_action_pre(view, &mut seen, &mut ids)?;
    collect_discrete_value_pre(view, &mut seen, &mut ids)?;
    // Last, so a model whose pre() reads all sit in event-owned equations
    // keeps its previous-value order and therefore its bytes.
    collect_causal_definition_pre(view, definitions, &mut seen, &mut ids)?;
    Ok(ids)
}

/// When DoStep commits each previous value.
pub(super) struct SettledCommitSchedule<'dae> {
    /// Read by an equation-section definition: committed after the event's
    /// sampled actions and before the causal assignments.
    pub(super) before_definitions: Vec<dae::VariableId<'dae>>,
    /// Everything else, committed at the end of DoStep in collection order.
    pub(super) after_definitions: Vec<dae::VariableId<'dae>>,
}

/// Splits the previous-value commits around the causal assignments.
///
/// An equation-section `pre(x)` outside any `when` is settled by event
/// iteration (MLS 3.7.5): `sample()` is true only on the first iteration, and
/// the iteration ends when every `pre(x)` equals `x`. So the published
/// definition reads `x` as the event left it, while a sampled action reads the
/// entry snapshot. Committing only the values those definitions read before
/// them gives that settled point in one pass without running any sampled
/// action again, and leaves models without such a read byte-identical.
///
/// A definition reading the previous value of a variable the causal
/// assignments themselves define would need the iteration itself, so it is
/// refused.
pub(super) fn settled_commit_schedule<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    referenced: &[dae::VariableId<'dae>],
    by_id: &HashMap<u32, ClassifiedVariable<'dae>>,
) -> Result<SettledCommitSchedule<'dae>, Vec<GalecTargetError>> {
    let mut read = Vec::new();
    collect_causal_definition_pre(view, definitions, &mut HashSet::new(), &mut read)?;
    if let Some(defined) = read
        .iter()
        .find(|id| defined_by_causal_assignment(definitions, **id))
    {
        let base = by_id
            .get(&defined.index())
            .expect("pre variable was resolved while collecting names");
        return Err(vec![unsupported(
            "pre-of-causal-definition",
            format!(
                "equation-section pre({}) reads a variable the settled definitions assign, \
                 which needs event iteration",
                base.variable.name()
            ),
            base.variable.declaration().span(),
        )]);
    }
    let read = read.iter().map(|id| id.index()).collect::<HashSet<_>>();
    let (before_definitions, after_definitions) = referenced
        .iter()
        .copied()
        .partition(|id| read.contains(&id.index()));
    Ok(SettledCommitSchedule {
        before_definitions,
        after_definitions,
    })
}

fn defined_by_causal_assignment<'dae>(
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    id: dae::VariableId<'dae>,
) -> bool {
    definitions.definition_for_variable(id).is_some()
        || definitions.fully_defines_variable(id)
        || definitions
            .order()
            .iter()
            .any(|algebraic| dae::VariableId::from(*algebraic) == id)
}

/// An equation-section `pre(x)` outside any `when` (MLS 3.7.5) is part of a
/// causal output or local definition, which DoStep lowers after the event
/// assignments and after committing the previous values it reads.
fn collect_causal_definition_pre<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &rumoca_phase_structural::CausalDefinitions<'dae>,
    seen: &mut HashSet<u32>,
    ids: &mut Vec<dae::VariableId<'dae>>,
) -> Result<(), Vec<GalecTargetError>> {
    for expression in definitions.defining_expressions() {
        collect_pre(view, expression, seen, ids)?;
    }
    Ok(())
}

fn collect_discrete_real_pre<'dae>(
    view: dae::DaeView<'dae>,
    seen: &mut HashSet<u32>,
    ids: &mut Vec<dae::VariableId<'dae>>,
) -> Result<(), Vec<GalecTargetError>> {
    for equation in view.discrete_real_equations() {
        collect_pre(view, equation.residual(), seen, ids)?;
        if let dae::DiscreteRealActivation::When { trigger, guard } = equation.activation() {
            collect_condition_pre(view, trigger, seen, ids)?;
            collect_condition_pre(view, guard, seen, ids)?;
        }
    }
    Ok(())
}

fn collect_event_action_pre<'dae>(
    view: dae::DaeView<'dae>,
    seen: &mut HashSet<u32>,
    ids: &mut Vec<dae::VariableId<'dae>>,
) -> Result<(), Vec<GalecTargetError>> {
    for (_, action) in view.event_actions() {
        collect_condition_pre(view, action.trigger(), seen, ids)?;
        collect_condition_pre(view, action.guard(), seen, ids)?;
        let value = match action.operation() {
            dae::EventActionOperation::Reinitialize { value, .. } => Some(value),
            dae::EventActionOperation::Assert { message } => Some(message),
            // A warning has no GALEC representation (MLS §8.3.7).
            dae::EventActionOperation::Warning { .. } => None,
            dae::EventActionOperation::Terminate { message } => Some(message),
            dae::EventActionOperation::Print { message } => Some(message),
        };
        if let Some(value) = value {
            collect_pre(view, value, seen, ids)?;
        }
    }
    Ok(())
}

fn collect_discrete_value_pre<'dae>(
    view: dae::DaeView<'dae>,
    seen: &mut HashSet<u32>,
    ids: &mut Vec<dae::VariableId<'dae>>,
) -> Result<(), Vec<GalecTargetError>> {
    for index in 0..view.discrete_value_owner_count() {
        let owner = view
            .discrete_value_owner(
                view.discrete_value_owner_id(index)
                    .expect("dense checked B.1c owner identity"),
            )
            .expect("checked B.1c owner resolves");
        for branch in owner.branches().iter() {
            if let dae::DiscreteBranchActivation::When { trigger, guard } = branch.activation() {
                collect_condition_pre(view, trigger, seen, ids)?;
                collect_condition_pre(view, guard, seen, ids)?;
            }
            for (value, _) in branch.values().iter() {
                collect_pre(view, value, seen, ids)?;
            }
        }
    }
    Ok(())
}

fn collect_condition_pre<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    seen_variables: &mut HashSet<u32>,
    ids: &mut Vec<dae::VariableId<'dae>>,
) -> Result<(), Vec<GalecTargetError>> {
    let mut pending = vec![root];
    let mut seen_conditions = HashSet::new();
    while let Some(condition) = pending.pop() {
        if !seen_conditions.insert(condition.index()) {
            continue;
        }
        match view
            .condition(condition)
            .expect("checked condition identity resolves")
            .operation()
        {
            dae::ConditionOperation::Initial
            | dae::ConditionOperation::Always
            | dae::ConditionOperation::Clock(_) => {}
            dae::ConditionOperation::Relation(relation) => {
                let expression = view
                    .relation(relation)
                    .expect("checked relation identity resolves")
                    .expression();
                collect_pre(view, expression, seen_variables, ids)?;
            }
            dae::ConditionOperation::Discrete(expression) => {
                collect_pre(view, expression, seen_variables, ids)?;
            }
            dae::ConditionOperation::Not(inner) => pending.push(inner),
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => pending.extend([lhs, rhs]),
        }
    }
    Ok(())
}

fn collect_pre<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    seen: &mut HashSet<u32>,
    ids: &mut Vec<dae::VariableId<'dae>>,
) -> Result<(), Vec<GalecTargetError>> {
    dae::for_each_expression(view, expression, |_id, node| {
        let variable = match node.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::PreDiscreteReal(id)) => {
                Some(dae::VariableId::from(id))
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::PreDiscreteValue(id)) => {
                Some(dae::VariableId::from(id))
            }
            _ => None,
        };
        if let Some(variable) = variable
            && seen.insert(variable.index())
        {
            ids.push(variable);
        }
    });
    Ok(())
}
