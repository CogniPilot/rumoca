//! Orientation of plain discrete alias equations `a = b` by their producers.
//!
//! MLS 3.7 Appendix B requires a discrete-valued equation to be an assignment
//! after at most flipping its sides. Written left to right, an alias `a = b`
//! defines `a`. When `a` already has another defining owner and `b` has none,
//! the left-to-right reading would give `a` two definitions and leave `b`
//! undefined, while the flipped reading `b := a` gives every coordinate
//! exactly one. `Modelica.Media.Water` states `phase = state.phase` after an
//! `if` equation that defines `phase`, so the alias defines the record field.
//!
//! Only that case is flipped: an alias whose left side has no other producer
//! keeps its written orientation, so no model that already has one owner per
//! coordinate changes.

use super::*;

/// Owners for the alias rows whose written orientation would define a
/// coordinate twice while leaving the other side without a definition.
pub(super) fn reversed_alias_owners(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    connection_ranks: &HashMap<VarName, usize>,
    result: &mut AggregateDiscreteConnections,
) -> Result<(), ToDaeError> {
    let producers = producer_counts(flat, roles);
    for (row, equation) in flat.equations.iter().enumerate() {
        if result.owners.contains_key(&row) || result.members.contains(&row) {
            continue;
        }
        let Some((target, source)) = plain_alias(equation, roles) else {
            continue;
        };
        let defined_elsewhere = producers.get(source).copied().unwrap_or(0) > 1;
        // Rank 0 without a producer is a connection source this alias feeds
        // (`alias_fed_connection_sources`).
        let undefined = !producers.contains_key(target)
            && connection_ranks.get(target).is_none_or(|rank| *rank == 0);
        if !(defined_elsewhere && undefined) {
            continue;
        }
        let scalar_count =
            checked_connection_shape_size(&flat.variables[target].dims, equation.span)?;
        let value = match &equation.residual {
            Expression::Binary { lhs, .. } => lhs.as_ref().clone(),
            _ => unreachable!("a plain alias is a difference"),
        };
        result.owners.insert(
            row,
            AggregateDiscreteConnection {
                target: target.clone(),
                value,
                scalar_count,
                ordered_scalar_self_dependencies: false,
            },
        );
    }
    Ok(())
}

/// `(b, a)` for a non-connection residual `a - b` over two whole
/// discrete-valued coordinates: the flipped target and its value's name.
fn plain_alias<'flat>(
    equation: &'flat flat::Equation,
    roles: &HashMap<VarName, PlannedRole>,
) -> Option<(&'flat VarName, &'flat VarName)> {
    if matches!(equation.origin, flat::EquationOrigin::Connection { .. }) {
        return None;
    }
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        return None;
    };
    let (written, written_subscripts) = discrete_value_base_reference(lhs, roles)?;
    let (source, source_subscripts) = discrete_value_base_reference(rhs, roles)?;
    (written_subscripts.is_empty() && source_subscripts.is_empty() && written != source)
        .then_some((source, written))
}

/// How many owners define each discrete-valued coordinate when every
/// equation keeps its written orientation.
fn producer_counts(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> HashMap<VarName, usize> {
    let mut counts = HashMap::<VarName, usize>::new();
    let mut count = |name: &VarName| *counts.entry(name.clone()).or_default() += 1;
    for (name, variable) in &flat.variables {
        if variable.binding.is_some() && matches!(roles.get(name), Some(PlannedRole::DiscreteValue))
        {
            count(name);
        }
    }
    for equation in &flat.equations {
        if matches!(equation.origin, flat::EquationOrigin::Connection { .. }) {
            continue;
        }
        if let Ok(Some(plan)) = discrete_value_assignment(&equation.residual, roles, equation.span)
        {
            count(plan.target);
        }
        if let Some((target, _, _)) = discrete_element_assignment(equation, roles) {
            count(target);
        }
    }
    for name in event_targets(flat).iter().chain(&algorithm_targets(flat)) {
        count(name);
    }
    counts
}

/// The connection coordinates a reversed alias feeds: `b` of an alias
/// `a = b` whose written side `a` has another defining owner, when no
/// producer reaches `b` through connections and `b` is connected.
pub(super) fn alias_fed_connection_sources(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    ranks: &HashMap<VarName, usize>,
    neighbors: &HashMap<VarName, Vec<VarName>>,
) -> Vec<VarName> {
    let producers = producer_counts(flat, roles);
    let mut sources = flat
        .equations
        .iter()
        .filter_map(|equation| plain_alias(equation, roles))
        .filter(|(target, source)| {
            producers.get(*source).copied().unwrap_or(0) > 1
                && !producers.contains_key(*target)
                && !ranks.contains_key(*target)
                && neighbors.contains_key(*target)
        })
        .map(|(target, _)| target.clone())
        .collect::<Vec<_>>();
    sources.sort_unstable();
    sources.dedup();
    sources
}
