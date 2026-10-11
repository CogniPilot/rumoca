//! Prove a checked activation cannot run outside one periodic task.

use super::*;

pub(super) fn condition_requires_clock<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
) -> bool {
    off_tick_value(view, root, expected, &mut HashMap::new()) == Some(false)
}

fn off_tick_value<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
    expected: dae::ClockId<'dae>,
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
        dae::ConditionOperation::Clock(found) if found == expected => Some(false),
        dae::ConditionOperation::Always => Some(true),
        dae::ConditionOperation::Not(inner) => {
            off_tick_value(view, inner, expected, values).map(|value| !value)
        }
        dae::ConditionOperation::And(lhs, rhs) => {
            match (
                off_tick_value(view, lhs, expected, values),
                off_tick_value(view, rhs, expected, values),
            ) {
                (Some(false), _) | (_, Some(false)) => Some(false),
                (Some(true), Some(true)) => Some(true),
                _ => None,
            }
        }
        dae::ConditionOperation::Or(lhs, rhs) | dae::ConditionOperation::AnyRise(lhs, rhs) => {
            match (
                off_tick_value(view, lhs, expected, values),
                off_tick_value(view, rhs, expected, values),
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
