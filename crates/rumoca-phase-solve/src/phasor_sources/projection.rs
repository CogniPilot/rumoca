//! Scalar views of original array equations; no new DAE nodes are issued.
use super::*;

/// The exact scalar coordinate selected from an original array expression.
pub(super) fn coordinate<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    scalar: usize,
    frame: Option<&CallFrame<'_, 'dae>>,
    query: &ProofQuery<'dae>,
) -> Option<ScalarCoordinate> {
    if let Forward::Expression(next, frame) = forward(view, expression, frame) {
        return coordinate(view, next, scalar, frame, query);
    }
    let node = view.expression(expression)?;
    if scalar >= node.value_type().scalar_count()? {
        return None;
    }
    match node.operation() {
        dae::ExpressionOperation::Coordinate(value) => {
            let variable = visible_value_variable(view, expression, value)?;
            Some((variable.index(), scalar))
        }
        dae::ExpressionOperation::Array(elements) => {
            let (element, scalar) = array_element(view, elements, scalar)?;
            coordinate(view, element, scalar, frame, query)
        }
        _ if scalar == 0 && node.value_type().is_scalar() => {
            scalar_coordinate(view, expression, frame, query)
        }
        _ => None,
    }
}

/// Resolve full scalar subscripts only from the query's original literals and
/// lexical binder point; a runtime coordinate cannot establish a selection.
pub(super) fn index<'dae>(
    view: dae::DaeView<'dae>,
    base: dae::ExprId<'dae>,
    subscripts: dae::SubscriptsView<'dae>,
    frame: Option<&CallFrame<'_, 'dae>>,
    query: &ProofQuery<'dae>,
) -> Option<ScalarCoordinate> {
    let dimensions = view.expression(base)?.value_type().dimensions();
    if subscripts.len() != dimensions.len() {
        return None;
    }
    let coordinates = (0..subscripts.len())
        .map(|axis| {
            let dae::SubscriptView::Index { expression, .. } = subscripts.get(axis)? else {
                return None;
            };
            let NumericSourceScalar::Integer(value) = literal(view, expression, frame, query)?
            else {
                return None;
            };
            u32::try_from(value.checked_sub(1)?).ok()
        })
        .collect::<Option<Vec<_>>>()?;
    let scalar = rumoca_core::flatten_coordinates(dimensions, &coordinates)?;
    coordinate(view, base, scalar, frame, query)
}

fn array_element<'dae>(
    view: dae::DaeView<'dae>,
    elements: dae::ExpressionOperands<'dae>,
    mut scalar: usize,
) -> Option<(dae::ExprId<'dae>, usize)> {
    for element in elements.iter() {
        let count = view.expression(element)?.value_type().scalar_count()?;
        if scalar < count {
            return Some((element, scalar));
        }
        scalar = scalar.checked_sub(count)?;
    }
    None
}

/// Select an original scalar RHS and freeze its lexical points before any
/// memoized proof runs. Each scalar owns a fresh query: no result can leak
/// across binder values.
fn select<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    scalar: usize,
    query: &mut ProofQuery<'dae>,
) -> Option<dae::ExprId<'dae>> {
    let node = view.expression(expression)?;
    if scalar >= node.value_type().scalar_count()? {
        return None;
    }
    match node.operation() {
        dae::ExpressionOperation::Array(elements) => {
            let (element, scalar) = array_element(view, elements, scalar)?;
            select(view, element, scalar, query)
        }
        dae::ExpressionOperation::Comprehension { domain, body } => {
            let count = view.expression(body)?.value_type().scalar_count()?;
            let point = view
                .domain(domain)?
                .structured()
                .index_tuple_at(scalar.checked_div(count)?)
                .ok()??;
            query.binders.insert(domain, point);
            select(view, body, scalar.checked_rem(count)?, query)
        }
        _ if scalar == 0 && node.value_type().is_scalar() => Some(expression),
        _ => None,
    }
}

fn sides<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
) -> Option<(dae::ExprId<'dae>, dae::ExprId<'dae>)> {
    if family.scalar_view() != rumoca_core::ComprehensionScalarView::RowMajorProjection
        || family.bodies().len() != 1
    {
        return None;
    }
    let (lhs, rhs) = crate::model_values::equation_sides(view, family.bodies().get(0)?)?;
    [lhs, rhs]
        .into_iter()
        .all(|side| {
            view.expression(side)
                .and_then(|node| node.value_type().scalar_count())
                == Some(family.scalar_rows() as usize)
        })
        .then_some((lhs, rhs))
}

pub(super) fn facts<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
) -> impl Iterator<Item = (ScalarCoordinate, PhasorFact)> + 'dae {
    sides(view, family)
        .into_iter()
        .flat_map(|(lhs, rhs)| [(lhs, rhs), (rhs, lhs)])
        .filter(move |(_, value)| {
            view.expression(*value).is_some_and(|node| {
                node.value_type().is_scalar()
                    || matches!(
                        node.operation(),
                        dae::ExpressionOperation::Array(_)
                            | dae::ExpressionOperation::Comprehension { .. }
                    )
            })
        })
        .flat_map(move |(target, value)| {
            (0..family.scalar_rows() as usize).filter_map(move |scalar| {
                let mut query = ProofQuery::default();
                let target = coordinate(view, target, scalar, None, &query)?;
                let value = select(view, value, scalar, &mut query)?;
                Some((target, phasor_of(view, value, target, &query)?))
            })
        })
}
