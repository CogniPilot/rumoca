//! Element-wise initial-equation definitions of discrete array coordinates.
//!
//! An initial equation may determine one element of a discrete array, such as
//! `pre(aboveLevel[i]) = level_start >= height[i]` inside a `for` loop. MLS
//! 3.7 §8.6 gives the coordinate one initialization value, so the elements
//! become one aggregate definition exactly when every element of the declared
//! shape is determined once. An incomplete or repeated element set claims no
//! row and stays an ordinary initialization residual.

use std::collections::BTreeMap;

use super::*;

/// The element of a discrete array an initial equation determines.
pub(super) struct ElementTarget<'flat> {
    pub(super) name: &'flat VarName,
    /// Row-major ordinal of the element within the declared shape.
    pub(super) ordinal: usize,
}

/// One determined element: its owning row and its defining value.
struct ElementDefinition<'flat> {
    row: usize,
    value: &'flat Expression,
    span: Span,
}

/// Element definitions collected per discrete array coordinate.
#[derive(Default)]
pub(super) struct ElementDefinitions<'flat> {
    targets: BTreeMap<&'flat VarName, BTreeMap<usize, ElementDefinition<'flat>>>,
    repeated: HashSet<&'flat VarName>,
}

impl<'flat> ElementDefinitions<'flat> {
    pub(super) fn record(
        &mut self,
        target: ElementTarget<'flat>,
        row: usize,
        value: &'flat Expression,
        span: Span,
    ) {
        let elements = self.targets.entry(target.name).or_default();
        if elements
            .insert(target.ordinal, ElementDefinition { row, value, span })
            .is_some()
        {
            self.repeated.insert(target.name);
        }
    }

    /// The aggregate definition of every coordinate whose declared shape is
    /// covered exactly once, with the rows that determine it.
    pub(super) fn complete(
        self,
        flat: &flat::Model,
    ) -> Vec<(&'flat VarName, InitialDiscreteValue, Vec<usize>)> {
        let mut complete = Vec::new();
        for (name, elements) in self.targets {
            if self.repeated.contains(name) {
                continue;
            }
            let Some(dims) = declared_extents(flat, name) else {
                continue;
            };
            if dims.iter().product::<usize>() != elements.len() {
                continue;
            }
            let Some(span) = elements.values().next().map(|element| element.span) else {
                continue;
            };
            let rows = elements.values().map(|element| element.row).collect();
            let values = elements
                .into_values()
                .map(|element| element.value.clone())
                .collect::<Vec<_>>();
            let Some(value) = nested_array(&dims, values, span) else {
                continue;
            };
            complete.push((name, InitialDiscreteValue { value, span }, rows));
        }
        complete
    }
}

/// The element a fully literal subscript selects within the declared shape.
pub(super) fn element_ordinal(
    flat: &flat::Model,
    name: &VarName,
    subscripts: &[Subscript],
) -> Option<usize> {
    let dims = declared_extents(flat, name)?;
    if subscripts.len() != dims.len() || dims.is_empty() {
        return None;
    }
    subscripts
        .iter()
        .zip(&dims)
        .try_fold(0usize, |ordinal, (subscript, extent)| {
            let Subscript::Index { value, .. } = subscript else {
                return None;
            };
            let index = usize::try_from(*value).ok()?;
            if index == 0 || index > *extent {
                return None;
            }
            ordinal.checked_mul(*extent)?.checked_add(index - 1)
        })
}

fn declared_extents(flat: &flat::Model, name: &VarName) -> Option<Vec<usize>> {
    flat.variables
        .get(name)?
        .dims
        .iter()
        .map(|extent| usize::try_from(*extent).ok())
        .collect()
}

/// The row-major array literal of `values` over the nonzero extents `dims`,
/// grouping the innermost axis first.
fn nested_array(dims: &[usize], values: Vec<Expression>, span: Span) -> Option<Expression> {
    let mut level = values;
    for &extent in dims.iter().rev() {
        if extent == 0 || !level.len().is_multiple_of(extent) {
            return None;
        }
        let mut grouped = Vec::with_capacity(level.len() / extent);
        let mut elements = level.into_iter();
        while elements.len() > 0 {
            grouped.push(Expression::Array {
                elements: elements.by_ref().take(extent).collect(),
                kind: rumoca_core::ArrayConstructor::Array,
                span,
            });
        }
        level = grouped;
    }
    let mut whole = level.into_iter();
    let value = whole.next()?;
    whole.next().is_none().then_some(value)
}
