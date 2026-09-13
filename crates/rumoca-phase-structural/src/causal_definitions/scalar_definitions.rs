//! Source-owned definitions of exact indexed array coordinates.

use super::{DefinitionCandidate, HashMap, HashSet, dae, equation_sides, expression_dependencies};

/// A scalar/element equality gets its direction from independent definitions.
/// A claimed scalar leaves the equality for component coverage; otherwise a
/// unique equality may define the scalar from the still-independent array.
pub(super) fn append_indexed_aliases<'dae>(
    candidates: &mut Vec<DefinitionCandidate<'dae>>,
    aliases: Vec<DefinitionCandidate<'dae>>,
) {
    let claimed = candidates
        .iter()
        .map(|(_, target, _)| target.index())
        .collect::<HashSet<_>>();
    let mut counts = HashMap::<u32, usize>::new();
    for (_, target, _) in &aliases {
        *counts.entry(target.index()).or_default() += 1;
    }
    candidates.extend(aliases.into_iter().filter(|(_, target, _)| {
        !claimed.contains(&target.index()) && counts[&target.index()] == 1
    }));
}

type ScalarDefinitionMap<'dae> = HashMap<(u32, u32), dae::ExprId<'dae>>;

pub(super) fn derive_scalar_definitions<'dae>(
    view: dae::DaeView<'dae>,
    whole: &HashMap<u32, dae::ExprId<'dae>>,
    closed: &HashSet<u32>,
    whole_event_held: &HashSet<u32>,
    already_consumed: &HashSet<u32>,
) -> (
    ScalarDefinitionMap<'dae>,
    HashSet<u32>,
    HashSet<u32>,
    HashSet<u32>,
) {
    let mut candidates = Vec::new();
    let mut counts = HashMap::<(u32, u32), usize>::new();
    for owner in view.continuous_owners() {
        let dae::ContinuousOwnerView::Residual { id, equation } = owner else {
            continue;
        };
        if already_consumed.contains(&id.index()) {
            continue;
        }
        let Some((variable, scalar, value)) = scalar_direct_definition(view, equation.residual())
        else {
            continue;
        };
        let dependencies = expression_dependencies(view, value);
        if whole.contains_key(&variable.index())
            || !dependencies.algebraic.iter().all(|id| closed.contains(id))
        {
            continue;
        }
        *counts.entry((variable.index(), scalar)).or_default() += 1;
        candidates.push((
            id,
            variable,
            scalar,
            value,
            !dependencies.has_continuous_source
                && dependencies
                    .algebraic
                    .iter()
                    .all(|id| whole_event_held.contains(id)),
        ));
    }
    candidates.retain(|(_, variable, scalar, _, _)| counts[&(variable.index(), *scalar)] == 1);
    let candidate_definitions = candidates
        .iter()
        .map(|(_, variable, scalar, value, _)| ((variable.index(), *scalar), *value))
        .collect::<HashMap<_, _>>();
    let candidate_event_held = candidates
        .iter()
        .map(|(_, variable, scalar, _, event_held)| ((variable.index(), *scalar), *event_held))
        .collect::<HashMap<_, _>>();
    let fully_defined = view
        .variables()
        .filter(|(variable, entry)| {
            let Ok(scalar_count) = u32::try_from(entry.scalar_count()) else {
                return false;
            };
            matches!(
                entry.role(),
                dae::VariableRole::Algebraic | dae::VariableRole::Output
            ) && scalar_count > 0
                && (0..scalar_count)
                    .all(|scalar| candidate_definitions.contains_key(&(variable.index(), scalar)))
        })
        .map(|(variable, _)| variable.index())
        .collect::<HashSet<_>>();
    let definitions = candidate_definitions
        .into_iter()
        .filter(|((variable, _), _)| fully_defined.contains(variable))
        .collect();
    let consumed = candidates
        .iter()
        .filter(|(_, variable, _, _, _)| fully_defined.contains(&variable.index()))
        .map(|(equation, _, _, _, _)| equation.index())
        .collect::<HashSet<_>>();
    let event_held = fully_defined
        .iter()
        .copied()
        .filter(|variable| {
            let declaration = view
                .variable_id(
                    usize::try_from(*variable)
                        .expect("checked variable ordinal is representable as usize"),
                )
                .and_then(|id| view.variable(id))
                .expect("complete scalar definition target resolves in its branded DAE");
            u32::try_from(declaration.scalar_count()).is_ok_and(|scalar_count| {
                (0..scalar_count).all(|scalar| {
                    candidate_event_held
                        .get(&(*variable, scalar))
                        .copied()
                        .unwrap_or(false)
                })
            })
        })
        .collect();
    (definitions, fully_defined, consumed, event_held)
}

fn scalar_direct_definition<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Option<(dae::AlgebraicId<'dae>, u32, dae::ExprId<'dae>)> {
    let (lhs, rhs) = equation_sides(view, residual)?;
    match (scalar_algebraic(view, lhs), scalar_algebraic(view, rhs)) {
        (Some((target, scalar)), None) => scalar_compatible_definition(view, target, scalar, rhs),
        (None, Some((target, scalar))) => scalar_compatible_definition(view, target, scalar, lhs),
        _ => None,
    }
}

fn scalar_compatible_definition<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::AlgebraicId<'dae>,
    scalar: u32,
    value: dae::ExprId<'dae>,
) -> Option<(dae::AlgebraicId<'dae>, u32, dae::ExprId<'dae>)> {
    let variable = view.variable(dae::VariableId::from(target))?;
    let value_type = view.expression(value)?.value_type();
    (value_type.is_scalar() && variable.value_type().scalar_type() == value_type.scalar_type())
        .then_some((target, scalar, value))
}

pub(super) fn scalar_algebraic<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<(dae::AlgebraicId<'dae>, u32)> {
    let node = view.expression(expression)?;
    let dae::ExpressionOperation::Index { base, subscripts } = node.operation() else {
        return None;
    };
    let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable)) =
        view.expression(base)?.operation()
    else {
        return None;
    };
    let dimensions = view
        .variable(dae::VariableId::from(variable))?
        .value_type()
        .dimensions();
    if dimensions.len() != subscripts.len() {
        return None;
    }
    let mut scalar = 0_u32;
    for (subscript, extent) in subscripts.iter().zip(dimensions) {
        let dae::SubscriptView::Index { expression, .. } = subscript else {
            return None;
        };
        let dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(index)) =
            view.expression(expression)?.operation()
        else {
            return None;
        };
        let coordinate = u32::try_from(*index).ok()?.checked_sub(1)?;
        if coordinate >= *extent {
            return None;
        }
        scalar = scalar.checked_mul(*extent)?.checked_add(coordinate)?;
    }
    Some((variable, scalar))
}
