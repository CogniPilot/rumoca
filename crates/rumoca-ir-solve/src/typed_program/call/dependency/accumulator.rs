//! Exact dependency union without scanning prior coordinate relations.

use std::collections::BTreeMap;

use indexmap::IndexSet;

use super::{Coordinates, SolveCallDependency};

/// Construction-only state. Whole-input coverage is held as maximal disjoint
/// runs of consecutive input leaves, so a dependency on a block of leaves is
/// one run from the moment it is inserted. A whole-input run absorbs every
/// exact coordinate relation on a leaf it covers. Exact relations keep their
/// first source occurrence order per leaf; consecutive leaves that carry the
/// same ordered relations are emitted as one run per relation, so the
/// relation is stored once per run. The final summary lists runs and
/// relations by ascending first input.
#[derive(Default)]
pub(super) struct Dependencies {
    /// Run start to exclusive run end; never overlapping, never adjacent.
    whole: BTreeMap<usize, usize>,
    /// Exact relations on leaves that no whole-input run covers.
    exact: BTreeMap<usize, IndexSet<Coordinates>>,
}

/// Consecutive leaves `start..start + count` carrying the same ordered relations.
struct ExactGroup {
    start: usize,
    count: usize,
    coordinates: IndexSet<Coordinates>,
}

impl ExactGroup {
    fn into_dependencies(self) -> impl Iterator<Item = SolveCallDependency> {
        let (start, end) = (self.start, self.start + self.count);
        self.coordinates
            .into_iter()
            .map(move |coordinates| SolveCallDependency::exact_run(start, end, coordinates))
    }
}

impl Dependencies {
    pub(super) fn insert(&mut self, dependency: SolveCallDependency) {
        let inputs = dependency.inputs();
        match dependency.coordinates {
            None => self.insert_whole(inputs.start, inputs.end),
            Some(coordinates) => self.insert_exact(inputs, &coordinates),
        }
    }

    fn insert_exact(&mut self, inputs: std::ops::Range<usize>, coordinates: &Coordinates) {
        for input in inputs.filter(|input| !Self::covered(&self.whole, *input)) {
            self.exact
                .entry(input)
                .or_default()
                .insert(coordinates.clone());
        }
    }

    fn covered(whole: &BTreeMap<usize, usize>, input: usize) -> bool {
        whole
            .range(..=input)
            .next_back()
            .is_some_and(|(_, end)| *end > input)
    }

    fn insert_whole(&mut self, mut start: usize, mut end: usize) {
        if let Some((&before, &before_end)) = self.whole.range(..=start).next_back()
            && before_end >= start
        {
            start = before;
            end = end.max(before_end);
        }
        let merged: Vec<usize> = self.whole.range(start..=end).map(|(&key, _)| key).collect();
        for key in merged {
            if let Some(merged_end) = self.whole.remove(&key) {
                end = end.max(merged_end);
            }
        }
        let absorbed: Vec<usize> = self.exact.range(start..end).map(|(&key, _)| key).collect();
        for key in absorbed {
            self.exact.remove(&key);
        }
        self.whole.insert(start, end);
    }

    fn exact_groups(exact: BTreeMap<usize, IndexSet<Coordinates>>) -> Vec<ExactGroup> {
        let mut groups: Vec<ExactGroup> = Vec::new();
        for (input, coordinates) in exact {
            match groups.last_mut() {
                Some(group)
                    if group.start + group.count == input
                        && group.coordinates.iter().eq(coordinates.iter()) =>
                {
                    group.count += 1;
                }
                _ => groups.push(ExactGroup {
                    start: input,
                    count: 1,
                    coordinates,
                }),
            }
        }
        groups
    }

    pub(super) fn finish(self) -> Vec<SolveCallDependency> {
        let groups = Self::exact_groups(self.exact);
        let mut dependencies = Vec::with_capacity(self.whole.len() + groups.len());
        let mut runs = self.whole.into_iter().peekable();
        let mut exact = groups.into_iter().peekable();
        loop {
            let take_run = match (runs.peek(), exact.peek()) {
                (Some((start, _)), Some(group)) => *start < group.start,
                (Some(_), None) => true,
                (None, Some(_)) => false,
                (None, None) => break,
            };
            if take_run {
                dependencies.extend(
                    runs.next()
                        .map(|(start, end)| SolveCallDependency::whole_run(start, end)),
                );
            } else if let Some(group) = exact.next() {
                dependencies.extend(group.into_dependencies());
            }
        }
        dependencies
    }
}

#[cfg(test)]
mod tests;
