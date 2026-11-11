//! Equalities of source-owned scalar coordinates preserve an issued phasor.
use super::*;
use std::collections::HashSet;

pub(super) fn inherit(
    view: dae::DaeView<'_>,
    sources: &mut HashMap<ScalarCoordinate, Option<PhasorFact>>,
) {
    let query = ProofQuery::default();
    let mut adjacent: HashMap<ScalarCoordinate, Vec<ScalarCoordinate>> = HashMap::new();
    for owner in view.continuous_owners() {
        let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
            continue;
        };
        let Some((lhs, rhs)) = crate::model_values::equation_sides(view, equation.residual())
        else {
            continue;
        };
        let Some(lhs) = coordinate(view, lhs, &query) else {
            continue;
        };
        let Some(rhs) = coordinate(view, rhs, &query) else {
            continue;
        };
        if lhs != rhs {
            adjacent.entry(lhs).or_default().push(rhs);
            adjacent.entry(rhs).or_default().push(lhs);
        }
    }
    let mut seen = HashSet::new();
    for &first in adjacent.keys() {
        if !seen.insert(first) {
            continue;
        }
        let component = connected_component(first, &adjacent, &mut seen);
        if !component.iter().any(|member| sources.contains_key(member)) {
            continue;
        }
        let fact = agreed(&component, sources);
        for member in component {
            sources.insert(member, fact.clone());
        }
    }
}

fn connected_component(
    first: ScalarCoordinate,
    adjacent: &HashMap<ScalarCoordinate, Vec<ScalarCoordinate>>,
    seen: &mut HashSet<ScalarCoordinate>,
) -> Vec<ScalarCoordinate> {
    let mut component = vec![first];
    let mut cursor = 0;
    while cursor < component.len() {
        for &next in &adjacent[&component[cursor]] {
            if seen.insert(next) {
                component.push(next);
            }
        }
        cursor += 1;
    }
    component
}

fn agreed(
    component: &[ScalarCoordinate],
    sources: &HashMap<ScalarCoordinate, Option<PhasorFact>>,
) -> Option<PhasorFact> {
    let members = component.iter().copied().collect::<HashSet<_>>();
    let mut answer = None;
    for member in component {
        if let Some(fact) = sources.get(member) {
            let fact = fact.as_ref()?;
            if members.contains(&fact.operands.im) || members.contains(&fact.operands.re) {
                return None;
            }
            if answer.as_ref().is_some_and(|previous| previous != fact) {
                return None;
            }
            answer = Some(fact.clone());
        }
    }
    answer
}

/// Strip only checked source sign/unit operations. Parameters and values read
/// from the run do not establish a unit gain, even when their default is one.
fn coordinate<'dae>(
    view: dae::DaeView<'dae>,
    mut expression: dae::ExprId<'dae>,
    query: &ProofQuery<'dae>,
) -> Option<ScalarCoordinate> {
    for _ in 0..view.expression_count() {
        match view.expression(expression)?.operation() {
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => expression = operand,
            dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Multiply,
                lhs,
                rhs,
            } => {
                expression = if unit(literal(view, lhs, None, query)) {
                    rhs
                } else if unit(literal(view, rhs, None, query)) {
                    lhs
                } else {
                    return None;
                };
            }
            _ => return scalar_coordinate(view, expression, None, query),
        }
    }
    None
}

fn unit(value: Option<NumericSourceScalar>) -> bool {
    match value {
        Some(NumericSourceScalar::Real(value)) => value == 1.0 || value == -1.0,
        Some(NumericSourceScalar::Integer(value)) => value == 1 || value == -1,
        _ => false,
    }
}
