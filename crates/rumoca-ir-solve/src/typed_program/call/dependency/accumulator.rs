//! Exact dependency union without scanning prior coordinate relations.

use std::collections::{BTreeMap, btree_map::Entry};

use indexmap::IndexSet;

use super::{Coordinates, SolveCallDependency};

/// Construction-only state: input order is sorted, coordinate order is the
/// first source occurrence, and a whole-input dependency absorbs every exact
/// coordinate of that input. Final summaries retain their existing wire form.
#[derive(Default)]
pub(super) struct Dependencies {
    inputs: BTreeMap<usize, Option<IndexSet<Coordinates>>>,
}

impl Dependencies {
    pub(super) fn insert(&mut self, dependency: SolveCallDependency) {
        match self.inputs.entry(dependency.input) {
            Entry::Vacant(entry) => {
                entry.insert(dependency.coordinates.map(|value| IndexSet::from([value])));
            }
            Entry::Occupied(mut entry) => match (entry.get_mut(), dependency.coordinates) {
                (None, _) => {}
                (target @ Some(_), None) => *target = None,
                (Some(coordinates), Some(value)) => {
                    coordinates.insert(value);
                }
            },
        }
    }

    pub(super) fn finish(self) -> Vec<SolveCallDependency> {
        let mut dependencies = Vec::new();
        for (input, coordinates) in self.inputs {
            let Some(coordinates) = coordinates else {
                dependencies.push(SolveCallDependency::whole(input));
                continue;
            };
            dependencies.extend(
                coordinates
                    .into_iter()
                    .map(|coordinates| SolveCallDependency {
                        input,
                        coordinates: Some(coordinates),
                    }),
            );
        }
        dependencies
    }
}

#[cfg(test)]
mod tests;
