//! Prove a checked activation cannot run outside one periodic task.

use super::*;

pub(super) fn condition_requires_clock<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
) -> bool {
    condition_value(view, root, expected, false, &mut HashMap::new()) == Some(false)
}

/// Prove a guard adds no restriction inside the already selected periodic task.
pub(super) fn condition_true_on_tick<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
) -> bool {
    condition_value(view, root, expected, true, &mut HashMap::new()) == Some(true)
}

fn condition_value<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
    on_tick: bool,
    values: &mut HashMap<u32, Option<bool>>,
) -> Option<bool> {
    if let Some(value) = values.get(&condition.index()) {
        return *value;
    }
    values.insert(condition.index(), None);
    let value = match view
        .condition(condition)
        .expect("checked condition resolves")
        .operation()
    {
        dae::ConditionOperation::Clock(found) if found == expected => Some(on_tick),
        dae::ConditionOperation::Always => Some(true),
        dae::ConditionOperation::Not(inner) => {
            condition_value(view, inner, expected, on_tick, values).map(|value| !value)
        }
        dae::ConditionOperation::And(lhs, rhs) => {
            match (
                condition_value(view, lhs, expected, on_tick, values),
                condition_value(view, rhs, expected, on_tick, values),
            ) {
                (Some(false), _) | (_, Some(false)) => Some(false),
                (Some(true), Some(true)) => Some(true),
                _ => None,
            }
        }
        dae::ConditionOperation::Or(lhs, rhs) | dae::ConditionOperation::AnyRise(lhs, rhs) => {
            match (
                condition_value(view, lhs, expected, on_tick, values),
                condition_value(view, rhs, expected, on_tick, values),
            ) {
                (Some(true), _) | (_, Some(true)) => Some(true),
                (Some(false), Some(false)) => Some(false),
                _ => None,
            }
        }
        dae::ConditionOperation::Clock(_)
        | dae::ConditionOperation::Initial
        | dae::ConditionOperation::Relation(_)
        | dae::ConditionOperation::Discrete(_) => None,
    };
    values.insert(condition.index(), value);
    value
}
