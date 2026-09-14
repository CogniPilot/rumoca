//! Source-bound selection through literal array constructors, MLS §10.5.

use std::collections::{BTreeSet, VecDeque};

use rumoca_ir_dae as dae;

use super::constraints::DifferentiationFacts;

/// Resolve a checked index to an existing element expression. This only
/// follows array construction and proved whole-coordinate definitions; it
/// neither evaluates tunable indices nor enters function calls. The original
/// array equations remain owners in the reconstructed system.
pub(super) fn projected_element<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExprId<'dae>,
) -> Option<dae::ExprId<'dae>> {
    let dae::ExpressionOperation::Index {
        mut base,
        subscripts,
    } = view.expression(expression)?.operation()
    else {
        return None;
    };
    if !matches!(
        view.expression(base)?.operation(),
        dae::ExpressionOperation::Array(_)
            | dae::ExpressionOperation::Index { .. }
            | dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(_))
    ) {
        return None;
    }
    let mut remaining = literal_indices(view, subscripts)?;
    let mut visited = BTreeSet::new();
    while let Some(&index) = remaining.front() {
        match view.expression(base)?.operation() {
            dae::ExpressionOperation::Array(elements) => {
                base = elements.get(index)?;
                remaining.pop_front();
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => {
                if !visited.insert(base.index()) {
                    return None;
                }
                base = facts.algebraic_definition(view, algebraic)?;
            }
            dae::ExpressionOperation::Index {
                base: inner,
                subscripts,
            } => {
                let mut indices = literal_indices(view, subscripts)?;
                indices.append(&mut remaining);
                remaining = indices;
                base = inner;
            }
            _ => return None,
        }
    }
    Some(base)
}

pub(super) fn literal_indices<'dae>(
    view: dae::DaeView<'dae>,
    subscripts: dae::SubscriptsView<'dae>,
) -> Option<VecDeque<usize>> {
    subscripts
        .iter()
        .map(|subscript| {
            let dae::SubscriptView::Index { expression, .. } = subscript else {
                return None;
            };
            let dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(index)) =
                view.expression(expression)?.operation()
            else {
                return None;
            };
            usize::try_from(*index).ok()?.checked_sub(1)
        })
        .collect()
}
