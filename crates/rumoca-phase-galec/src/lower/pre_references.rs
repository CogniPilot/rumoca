//! Collection of exact `pre(...)` dependencies required by GALEC state projection.

use std::collections::HashSet;

use super::*;

pub(super) fn referenced_pre_variables<'dae>(
    view: dae::DaeView<'dae>,
) -> Result<Vec<dae::VariableId<'dae>>, Vec<GalecTargetError>> {
    let mut ids = Vec::new();
    let mut seen = HashSet::new();
    collect_discrete_real_pre(view, &mut seen, &mut ids)?;
    collect_event_action_pre(view, &mut seen, &mut ids)?;
    collect_discrete_value_pre(view, &mut seen, &mut ids)?;
    Ok(ids)
}

fn collect_discrete_real_pre<'dae>(
    view: dae::DaeView<'dae>,
    seen: &mut HashSet<u32>,
    ids: &mut Vec<dae::VariableId<'dae>>,
) -> Result<(), Vec<GalecTargetError>> {
    for index in 0..view.discrete_real_equation_count() {
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
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
    for index in 0..view.event_action_count() {
        let action = view
            .event_action(
                view.event_action_id(index)
                    .expect("dense checked action identity"),
            )
            .expect("checked action resolves");
        collect_condition_pre(view, action.trigger(), seen, ids)?;
        collect_condition_pre(view, action.guard(), seen, ids)?;
        let value = match action.operation() {
            dae::EventActionOperation::Reinitialize { value, .. } => Some(value),
            dae::EventActionOperation::Assert { message, level } => {
                collect_pre(view, message, seen, ids)?;
                level
            }
            dae::EventActionOperation::Terminate { message } => Some(message),
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
