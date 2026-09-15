//! Sparse identities for authored projections; tensor declarations remain whole.

use std::collections::BTreeMap;

use rumoca_core::flatten_coordinates;
use rumoca_ir_dae as dae;

use super::super::component_projection::literal_indices;

#[derive(Clone, Copy)]
pub(super) struct Coordinate {
    pub variable: u32,
    pub scalar: u32,
    pub projected: bool,
}

pub(super) struct Coordinates {
    items: Vec<Coordinate>,
    projections: BTreeMap<u32, u32>,
}

impl Coordinates {
    pub(super) fn collect(view: dae::DaeView<'_>) -> Self {
        let mut catalog = Self {
            items: view
                .variables()
                .map(|(variable, _)| Coordinate {
                    variable: variable.index(),
                    scalar: 0,
                    projected: false,
                })
                .collect(),
            projections: BTreeMap::new(),
        };
        let mut components = BTreeMap::new();
        for ordinal in 0..view.expression_count() {
            let Some(expression) = view.expression_id(ordinal) else {
                continue;
            };
            let Some((variable, scalar)) = literal_component(view, expression) else {
                continue;
            };
            let count = view
                .variable(view.variable_id(variable as usize).unwrap())
                .unwrap()
                .scalar_count();
            let key = if count == 1 {
                Some(variable)
            } else {
                catalog.insert_component(&mut components, variable, scalar)
            };
            if let Some(key) = key {
                catalog.projections.insert(expression.index(), key);
            }
        }
        catalog
    }

    fn insert_component(
        &mut self,
        components: &mut BTreeMap<(u32, u32), u32>,
        variable: u32,
        scalar: u32,
    ) -> Option<u32> {
        if let Some(&key) = components.get(&(variable, scalar)) {
            return Some(key);
        }
        let key = u32::try_from(self.items.len()).ok()?;
        self.items.push(Coordinate {
            variable,
            scalar,
            projected: true,
        });
        components.insert((variable, scalar), key);
        Some(key)
    }

    pub(super) fn len(&self) -> usize {
        self.items.len()
    }

    pub(super) fn get(&self, key: u32) -> Coordinate {
        self.items[key as usize]
    }

    pub(super) fn iter(&self) -> impl Iterator<Item = (u32, Coordinate)> + '_ {
        self.items
            .iter()
            .copied()
            .enumerate()
            .map(|(i, c)| (i as u32, c))
    }

    pub(super) fn projection(&self, expression: dae::ExprId<'_>) -> Option<u32> {
        self.projections.get(&expression.index()).copied()
    }
}

/// Literal indexing of a coordinate is an exact value identity. A unique
/// dependency of a general expression would not establish this fact.
fn literal_component<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<(u32, u32)> {
    let node = view.expression(expression)?;
    if node.binder_domain().is_some()
        || !node.value_type().is_scalar()
        || node.value_type().scalar_type() != dae::ScalarType::Real
    {
        return None;
    }
    let dae::ExpressionOperation::Index {
        mut base,
        subscripts,
    } = node.operation()
    else {
        return None;
    };
    let mut indices = literal_indices(view, subscripts)?;
    while let dae::ExpressionOperation::Index {
        base: inner,
        subscripts,
    } = view.expression(base)?.operation()
    {
        let mut outer = literal_indices(view, subscripts)?;
        outer.append(&mut indices);
        indices = outer;
        base = inner;
    }
    let variable = match view.expression(base)?.operation() {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(id)) => id.index(),
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) => id.index(),
        _ => return None,
    };
    let indices = indices
        .into_iter()
        .map(u32::try_from)
        .collect::<Result<Vec<_>, _>>()
        .ok()?;
    let scalar = flatten_coordinates(view.expression(base)?.value_type().dimensions(), &indices)?;
    Some((variable, u32::try_from(scalar).ok()?))
}
