//! Exact inverse scalar definitions derived from authoritative array equalities.

use std::collections::BTreeSet;
use std::sync::Arc;

use rumoca_core::StateSelect;

use super::super::constraints::exact_state_anchor;
use super::super::equalities::EqualitySign;
use super::{
    ComponentConstraint, ComponentExpression, ComponentProjection, DifferentiationFacts,
    component_indices, dae,
};
use crate::residual_normalization::equation_sides;

#[derive(Clone, Copy)]
pub(super) struct StateArrayDefinition {
    pub(super) expression: u32,
    pub(super) sign: EqualitySign,
}

pub(in super::super) fn derive_definitions(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
) -> Vec<Option<Arc<ComponentConstraint>>> {
    let state_arrays = explicit_state_arrays(view, facts);
    let mut definitions = vec![None; view.variable_count()];
    let targets = inverse_state_coordinates(view, &state_arrays);
    if targets.is_empty() {
        return definitions;
    }
    for residual in candidate_residuals(view) {
        for (target, definition) in
            residual_definitions(view, facts, &state_arrays, &targets, residual)
        {
            definitions[target as usize].get_or_insert(definition);
        }
    }
    definitions
}

fn residual_definitions<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    state_arrays: &[Option<StateArrayDefinition>],
    targets: &BTreeSet<u32>,
    residual: dae::ExprId<'dae>,
) -> Vec<(u32, Arc<ComponentConstraint>)> {
    let mut definitions = Vec::new();
    let node = view.expression(residual).unwrap();
    if node.binder_domain().is_some()
        || node.value_type().dimensions().is_empty()
        || node.value_type().scalar_type() != dae::ScalarType::Real
    {
        return definitions;
    }
    for scalar in 0..node.value_type().scalar_count().unwrap_or(0) {
        let Some(expression) =
            ComponentProjection::new(view, facts, Some(state_arrays)).derive(residual, scalar)
        else {
            continue;
        };
        for target in expression.scalar_algebraics(view) {
            if !targets.contains(&target)
                || facts.algebraic_definitions[target as usize].is_some()
                || facts.auxiliary_blocks[target as usize].is_some()
                || facts.equalities.value_anchor_of(target).is_some()
            {
                continue;
            }
            let Some(rhs) = expression.isolate(view, target) else {
                continue;
            };
            if independent_rhs(view, facts, state_arrays, &rhs) {
                definitions.push((
                    target,
                    Arc::new(ComponentConstraint {
                        scalar,
                        indices: component_indices(node.value_type(), scalar).unwrap(),
                        expression: rhs,
                    }),
                ));
            }
        }
    }
    definitions
}

fn independent_rhs(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
    state_arrays: &[Option<StateArrayDefinition>],
    rhs: &ComponentExpression,
) -> bool {
    let mut leaves = Vec::new();
    rhs.collect_leaves(&mut leaves);
    leaves.into_iter().all(|leaf| {
        facts
            .materialized_state_anchors(view, leaf)
            .is_some_and(|anchors| {
                anchors
                    .into_iter()
                    .all(|state| state_arrays[state as usize].is_none())
            })
    })
}

fn inverse_state_coordinates(
    view: dae::DaeView<'_>,
    arrays: &[Option<StateArrayDefinition>],
) -> BTreeSet<u32> {
    let mut targets = BTreeSet::new();
    for expression in arrays.iter().flatten() {
        dae::for_each_expression(
            view,
            view.expression_id(expression.expression as usize).unwrap(),
            |_, node| {
                if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) =
                    node.operation()
                    && node.value_type().dimensions().is_empty()
                {
                    targets.insert(id.index());
                }
            },
        );
    }
    targets
}

fn explicit_state_arrays(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
) -> Vec<Option<StateArrayDefinition>> {
    let mut definitions = vec![None; view.variable_count()];
    let mut duplicates = vec![false; view.variable_count()];
    for residual in candidate_residuals(view) {
        let Some((lhs, rhs)) = equation_sides(view, residual) else {
            continue;
        };
        for (target, value) in [(lhs, rhs), (rhs, lhs)] {
            let Some((state, sign)) = exact_state_anchor(view, &facts.equalities, target) else {
                continue;
            };
            let variable = view
                .variable(view.variable_id(state.index() as usize).unwrap())
                .unwrap();
            if variable.state_select() == StateSelect::Always
                || variable.value_type().dimensions()
                    != view.expression(value).unwrap().value_type().dimensions()
                || !matches!(
                    view.expression(value).unwrap().operation(),
                    dae::ExpressionOperation::Array(_)
                )
            {
                continue;
            }
            let slot = &mut definitions[state.index() as usize];
            if slot
                .replace(StateArrayDefinition {
                    expression: value.index(),
                    sign,
                })
                .is_some()
            {
                duplicates[state.index() as usize] = true;
            }
        }
    }
    for (slot, duplicate) in definitions.iter_mut().zip(duplicates) {
        if duplicate {
            *slot = None;
        }
    }
    definitions
}

impl ComponentExpression {
    fn scalar_algebraics(&self, view: dae::DaeView<'_>) -> BTreeSet<u32> {
        match self {
            Self::Source {
                expression,
                indices,
            } if indices.is_empty() => {
                let node = view
                    .expression(view.expression_id(*expression as usize).unwrap())
                    .unwrap();
                match node.operation() {
                    dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) => {
                        BTreeSet::from([id.index()])
                    }
                    _ => BTreeSet::new(),
                }
            }
            Self::Sum { lhs, rhs, .. } => {
                let mut result = lhs.scalar_algebraics(view);
                result.extend(rhs.scalar_algebraics(view));
                result
            }
            _ => BTreeSet::new(),
        }
    }

    fn isolate(&self, view: dae::DaeView<'_>, target: u32) -> Option<Self> {
        let (count, coefficient, remainder) = self.split_target(view, target);
        match (count, coefficient) {
            (1, 1) => Some(Self::sum(
                dae::BinaryOperator::Subtract,
                Self::Zero,
                remainder,
            )),
            (1, -1) => Some(remainder),
            _ => None,
        }
    }

    fn split_target(&self, view: dae::DaeView<'_>, target: u32) -> (usize, i64, Self) {
        match self {
            Self::Sum { operator, lhs, rhs } => {
                let (lc, ls, lr) = lhs.split_target(view, target);
                let (rc, rs, rr) = rhs.split_target(view, target);
                let sign = if *operator == dae::BinaryOperator::Subtract {
                    -1
                } else {
                    1
                };
                (lc + rc, ls + sign * rs, Self::sum(*operator, lr, rr))
            }
            _ if self.scalar_algebraics(view).contains(&target) => (1, 1, Self::Zero),
            _ => (0, 0, self.clone()),
        }
    }

    fn sum(operator: dae::BinaryOperator, lhs: Self, rhs: Self) -> Self {
        Self::Sum {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

fn candidate_residuals<'dae>(view: dae::DaeView<'dae>) -> impl Iterator<Item = dae::ExprId<'dae>> {
    view.continuous_owners().flat_map(|owner| {
        let bodies: Box<dyn Iterator<Item = dae::ExprId<'dae>>> = match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                Box::new(std::iter::once(equation.residual()))
            }
            dae::ContinuousOwnerView::Structured { family, .. }
                if family.scalar_view()
                    == rumoca_core::ComprehensionScalarView::RowMajorProjection =>
            {
                Box::new(family.bodies().iter())
            }
            dae::ContinuousOwnerView::Structured { .. } => Box::new(std::iter::empty()),
        };
        bodies
    })
}
