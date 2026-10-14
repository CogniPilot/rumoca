use rustc_hash::{FxHashMap, FxHashSet};

use crate::model::{Storage, unknown};
use crate::{DaeConstructionError, DaeProvenance, DiscreteValueId, VariableId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, serde::Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct DiscreteValueTargetEntry {
    pub(crate) variable: u32,
    pub(crate) scalar_selection: Option<u32>,
}

/// A whole discrete coordinate or a construction-checked scalar selection.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DiscreteValueTarget<'dae> {
    pub(crate) entry: DiscreteValueTargetEntry,
    pub(crate) marker: std::marker::PhantomData<&'dae mut &'dae ()>,
}

impl<'dae> DiscreteValueTarget<'dae> {
    pub const fn variable(self) -> DiscreteValueId<'dae> {
        DiscreteValueId::from_raw(self.entry.variable)
    }

    pub const fn scalar_selection(self) -> Option<u32> {
        self.entry.scalar_selection
    }

    pub const fn index(self) -> u32 {
        self.entry.variable
    }
}

impl<'dae> From<DiscreteValueId<'dae>> for DiscreteValueTarget<'dae> {
    fn from(variable: DiscreteValueId<'dae>) -> Self {
        Self {
            entry: DiscreteValueTargetEntry {
                variable: variable.index(),
                scalar_selection: None,
            },
            marker: std::marker::PhantomData,
        }
    }
}

impl<'dae> From<DiscreteValueTarget<'dae>> for VariableId<'dae> {
    fn from(target: DiscreteValueTarget<'dae>) -> Self {
        target.variable().into()
    }
}

pub(crate) fn select_element<'dae>(
    storage: &Storage,
    variable: DiscreteValueId<'dae>,
    indices: &[i64],
    provenance: DaeProvenance,
) -> Result<DiscreteValueTarget<'dae>, DaeConstructionError> {
    storage.expect_discrete_value_target_id(variable.index(), provenance)?;
    let entry = &storage.variables[variable.index() as usize];
    let dimensions = storage
        .value_type_at(entry.value_type, provenance)?
        .dimensions();
    let selection = scalar_selection(dimensions, indices).ok_or(
        DaeConstructionError::InvalidDiscreteElementSelection {
            target: variable.index(),
            span: provenance.span(),
        },
    )?;
    Ok(DiscreteValueTarget {
        entry: DiscreteValueTargetEntry {
            variable: variable.index(),
            scalar_selection: Some(selection),
        },
        marker: std::marker::PhantomData,
    })
}

pub(super) fn scalar_selection(dimensions: &[u32], indices: &[i64]) -> Option<u32> {
    if dimensions.is_empty() || dimensions.len() != indices.len() {
        return None;
    }
    dimensions
        .iter()
        .zip(indices)
        .try_fold(0u32, |offset, (&extent, &index)| {
            let index = u32::try_from(index).ok()?;
            if index == 0 || index > extent {
                return None;
            }
            offset.checked_mul(extent)?.checked_add(index - 1)
        })
}

pub(crate) fn target_indices(
    storage: &Storage,
    target: DiscreteValueTarget<'_>,
    provenance: DaeProvenance,
) -> Result<Option<Vec<i64>>, DaeConstructionError> {
    let Some(offset) = target.scalar_selection() else {
        return Ok(None);
    };
    let entry = storage
        .variables
        .get(target.index() as usize)
        .ok_or_else(|| unknown("variable", target.index(), provenance))?;
    let dimensions = storage
        .value_type_at(entry.value_type, provenance)?
        .dimensions();
    selection_indices(dimensions, offset).map(Some).ok_or(
        DaeConstructionError::InvalidDiscreteElementSelection {
            target: target.index(),
            span: provenance.span(),
        },
    )
}

pub(crate) fn selection_indices(dimensions: &[u32], mut offset: u32) -> Option<Vec<i64>> {
    if dimensions.is_empty() {
        return None;
    }
    let mut indices = vec![0; dimensions.len()];
    for (index, &extent) in indices.iter_mut().zip(dimensions).rev() {
        if extent == 0 {
            return None;
        }
        *index = i64::from(offset % extent) + 1;
        offset /= extent;
    }
    (offset == 0).then_some(indices)
}

#[derive(Default)]
pub(super) struct TargetCoverage {
    variables: FxHashMap<u32, Coverage>,
}

enum Coverage {
    Whole,
    Elements(FxHashSet<u32>),
}

impl TargetCoverage {
    pub(super) fn contains_union(
        &self,
        other: &Self,
        variable: u32,
        selection: Option<u32>,
        storage: &Storage,
        provenance: DaeProvenance,
    ) -> Result<bool, DaeConstructionError> {
        if self.contains(variable, selection, storage, provenance)?
            || other.contains(variable, selection, storage, provenance)?
        {
            return Ok(true);
        }
        let (Some(Coverage::Elements(a)), Some(Coverage::Elements(b))) = (
            self.variables.get(&variable),
            other.variables.get(&variable),
        ) else {
            return Ok(false);
        };
        let entry = &storage.variables[variable as usize];
        // The checked plan proves the issued and owner-prefix sets disjoint.
        Ok(selection.is_none()
            && storage
                .value_type_at(entry.value_type, provenance)?
                .scalar_count()
                == a.len().checked_add(b.len()))
    }

    pub(super) fn insert(&mut self, target: DiscreteValueTargetEntry) -> bool {
        use std::collections::hash_map::Entry;
        match self.variables.entry(target.variable) {
            Entry::Vacant(entry) => {
                entry.insert(match target.scalar_selection {
                    None => Coverage::Whole,
                    Some(offset) => Coverage::Elements(FxHashSet::from_iter([offset])),
                });
                true
            }
            Entry::Occupied(mut entry) => match (entry.get_mut(), target.scalar_selection) {
                (Coverage::Elements(elements), Some(offset)) => elements.insert(offset),
                _ => false,
            },
        }
    }

    pub(super) fn contains(
        &self,
        variable: u32,
        selection: Option<u32>,
        storage: &Storage,
        provenance: DaeProvenance,
    ) -> Result<bool, DaeConstructionError> {
        match self.variables.get(&variable) {
            None => Ok(false),
            Some(Coverage::Whole) => Ok(true),
            Some(Coverage::Elements(elements)) => match selection {
                Some(offset) => Ok(elements.contains(&offset)),
                None => {
                    let entry = storage
                        .variables
                        .get(variable as usize)
                        .ok_or_else(|| unknown("variable", variable, provenance))?;
                    Ok(storage
                        .value_type_at(entry.value_type, provenance)?
                        .scalar_count()
                        == Some(elements.len()))
                }
            },
        }
    }
}
