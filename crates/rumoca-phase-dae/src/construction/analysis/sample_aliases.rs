//! Exact ownership of Boolean aliases of the periodic `sample` event operator.
//!
//! MLS §3.7.5 defines `sample(start, interval)` as true only at its periodic
//! event instants. A scalar Boolean equation may give that event expression a
//! name and use the name as a `when` condition. Treating that use as an
//! ordinary discrete coordinate buffers the true value between events and
//! consequently deletes every later rising edge. This analysis retains the
//! authoritative periodic schedule through exact, unconditional,
//! whole-coordinate aliases. It deliberately recognizes no Boolean algebra,
//! subscripting, source names, or approximate periods.

use super::*;

#[derive(Clone)]
enum AliasDefinition {
    Schedule(PeriodicClockSchedule),
    Coordinate(VarName),
}

pub(super) fn analyze_sample_aliases(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    expression_events: &ExpressionEventPlans,
    connection_ranks: &HashMap<VarName, usize>,
    aggregate_connections: &AggregateDiscreteConnections,
) -> Result<HashMap<VarName, PeriodicClockSchedule>, ToDaeError> {
    let mut definitions = HashMap::new();

    for (name, variable) in &flat.variables {
        let Some(binding) = variable.binding.as_ref() else {
            continue;
        };
        if let Some(definition) = alias_definition(flat, roles, expression_events, name, binding) {
            definitions.insert(name.clone(), definition);
        }
    }

    for (row, equation) in flat.equations.iter().enumerate() {
        let EquationPartition::DiscreteValue(plan) = equation_partition(
            flat,
            row,
            equation,
            roles,
            connection_ranks,
            aggregate_connections,
        )?
        else {
            continue;
        };
        if let Some(definition) = alias_definition(
            flat,
            roles,
            expression_events,
            plan.target,
            plan.value.as_ref(),
        ) {
            definitions.insert(plan.target.clone(), definition);
        }
    }

    // Exact alias closure supports connection chains such as
    // `tick = internalTick; internalTick = sample(...)`. Cycles and aliases
    // whose source is not itself proven remain ordinary discrete coordinates.
    let mut schedules = definitions
        .iter()
        .filter_map(|(target, definition)| match definition {
            AliasDefinition::Schedule(schedule) => Some((target.clone(), *schedule)),
            AliasDefinition::Coordinate(_) => None,
        })
        .collect::<HashMap<_, _>>();
    loop {
        let mut changed = false;
        for (target, definition) in &definitions {
            if schedules.contains_key(target) {
                continue;
            }
            let AliasDefinition::Coordinate(source) = definition else {
                continue;
            };
            let Some(schedule) = schedules.get(source).copied() else {
                continue;
            };
            schedules.insert(target.clone(), schedule);
            changed = true;
        }
        if !changed {
            break;
        }
    }
    Ok(schedules)
}

fn alias_definition(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    expression_events: &ExpressionEventPlans,
    target: &VarName,
    value: &Expression,
) -> Option<AliasDefinition> {
    if !is_scalar_boolean_discrete(flat, roles, target) {
        return None;
    }
    match value {
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
            span,
        } => {
            let operands = args.iter().collect::<Vec<_>>();
            match expression_events.plan(*span, &operands)? {
                ExpressionEventPlan::SampleClock(schedule) => {
                    Some(AliasDefinition::Schedule(schedule))
                }
                ExpressionEventPlan::StateRelation | ExpressionEventPlan::TimeEvent(_) => None,
            }
        }
        Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() && is_scalar_boolean_discrete(flat, roles, name.var_name()) => {
            Some(AliasDefinition::Coordinate(name.var_name().clone()))
        }
        _ => None,
    }
}

fn is_scalar_boolean_discrete(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    name: &VarName,
) -> bool {
    let Some(variable) = flat.variables.get(name) else {
        return false;
    };
    variable.dims.is_empty()
        && matches!(roles.get(name), Some(PlannedRole::DiscreteValue))
        && effective_variable_scalar_type(flat, variable) == Some(dae::ScalarType::Boolean)
}
