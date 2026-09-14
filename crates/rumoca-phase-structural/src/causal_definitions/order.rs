//! Stable elimination order of whole-coordinate definition candidates.

use std::cmp::Reverse;
use std::collections::BinaryHeap;

use super::{DefinitionCandidate, ExpressionDependencies, HashMap};

pub(super) fn candidate_order(
    candidates: &[DefinitionCandidate<'_>],
    dependencies: &HashMap<u32, ExpressionDependencies>,
) -> Vec<usize> {
    let target_positions = candidates
        .iter()
        .enumerate()
        .map(|(position, (_, target, _))| (target.index(), position))
        .collect::<HashMap<_, _>>();
    let prerequisites = candidates
        .iter()
        .map(|(_, target, _)| {
            dependencies[&target.index()]
                .algebraic
                .iter()
                .filter_map(|dependency| target_positions.get(dependency).copied())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    elimination_order(&prerequisites)
}

fn elimination_order(prerequisites: &[Vec<usize>]) -> Vec<usize> {
    let mut remaining = Vec::with_capacity(prerequisites.len());
    let mut dependents = vec![Vec::new(); prerequisites.len()];
    let mut ready = BinaryHeap::new();
    for (candidate, dependencies) in prerequisites.iter().enumerate() {
        remaining.push(dependencies.len());
        if dependencies.is_empty() {
            ready.push(Reverse(candidate));
        }
        for &dependency in dependencies {
            dependents[dependency].push(candidate);
        }
    }
    let mut order = Vec::with_capacity(prerequisites.len());
    while let Some(Reverse(candidate)) = ready.pop() {
        order.push(candidate);
        for &dependent in &dependents[candidate] {
            remaining[dependent] -= 1;
            if remaining[dependent] == 0 {
                ready.push(Reverse(dependent));
            }
        }
    }
    order
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn queue_matches_first_ready_semantics_for_every_three_candidate_graph() {
        for edges in 0..1_u32 << 9 {
            let prerequisites = (0..3)
                .map(|candidate| {
                    (0..3)
                        .filter(|dependency| edges & (1 << (candidate * 3 + dependency)) != 0)
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();
            let mut expected = Vec::new();
            while let Some(next) = (0..3).find(|candidate| {
                !expected.contains(candidate)
                    && prerequisites[*candidate]
                        .iter()
                        .all(|dependency| expected.contains(dependency))
            }) {
                expected.push(next);
            }
            assert_eq!(elimination_order(&prerequisites), expected, "edges={edges}");
        }
    }
}
