//! Exact completed dependency sets; identical sets share storage, never owners.
mod components;
use crate::projection::{HashMap, HashSet};
use std::sync::Arc;

use super::FunctionParameterDependency;

type Set = Arc<[usize]>;

#[derive(Debug, Default)]
pub(super) struct ClosureMetrics {
    pub(super) unions: u64,
    pub(super) comparisons: u64,
    pub(super) unique_sets: usize,
    pub(super) retained_ids: usize,
    pub(super) output_occurrences: usize,
}

pub(super) fn complete(
    edges: &[Vec<usize>],
    direct: &[Arc<[FunctionParameterDependency]>],
) -> (Vec<Arc<[FunctionParameterDependency]>>, ClosureMetrics) {
    let (parameters, direct_sets, mut inventory) = initial_sets(direct);
    let mut metrics = ClosureMetrics::default();
    let components = components::derive(edges);
    let sets = completed_components(
        edges,
        &direct_sets,
        &components,
        &mut inventory,
        &mut metrics,
    );
    metrics.unique_sets = inventory.len();
    metrics.retained_ids = inventory.iter().map(|set| set.len()).sum();
    // Keys identify live, already interned allocations, not source identities.
    // Inventory retains those allocations throughout conversion, so no address
    // can be recycled. Unequal allocations never imply equal dependencies.
    let mut outputs = HashMap::<*const usize, Arc<[FunctionParameterDependency]>>::default();
    let completed = components
        .of
        .into_iter()
        .map(|component| {
            let set = &sets[component];
            metrics.output_occurrences += set.len();
            Arc::clone(
                outputs
                    .entry(Arc::as_ptr(set).cast::<usize>())
                    .or_insert_with(|| {
                        set.iter()
                            .map(|id| parameters[*id].clone())
                            .collect::<Vec<_>>()
                            .into()
                    }),
            )
        })
        .collect();
    (completed, metrics)
}

fn completed_components(
    edges: &[Vec<usize>],
    direct: &[Set],
    components: &components::Components,
    inventory: &mut HashSet<Set>,
    metrics: &mut ClosureMetrics,
) -> Vec<Set> {
    let mut completed = vec![Arc::from([]); components.nodes.len()];
    // Component ids are source-before-successor. Each cyclic component has the
    // union of all its direct dependencies and already completed successors.
    // Only completed sets enter inventory; historical partial unions do not.
    for component in (0..components.nodes.len()).rev() {
        let mut current = Arc::from([]);
        for node in &components.nodes[component] {
            absorb(&mut current, &direct[*node], metrics);
            for target in &edges[*node] {
                let successor = components.of[*target];
                absorb_successor(&mut current, component, successor, &completed, metrics);
            }
        }
        completed[component] = if let Some(issued) = inventory.get(current.as_ref()) {
            Arc::clone(issued)
        } else {
            inventory.insert(Arc::clone(&current));
            current
        };
    }
    completed
}

fn absorb_successor(
    current: &mut Set,
    component: usize,
    successor: usize,
    completed: &[Set],
    metrics: &mut ClosureMetrics,
) {
    if successor != component {
        assert!(successor > component, "checked component order is acyclic");
        absorb(current, &completed[successor], metrics);
    }
}

fn absorb(current: &mut Set, incoming: &Set, metrics: &mut ClosureMetrics) {
    metrics.unions += 1;
    if Arc::ptr_eq(current, incoming) || incoming.is_empty() {
        return;
    }
    if current.is_empty() || subset(current, incoming, &mut metrics.comparisons) {
        *current = Arc::clone(incoming);
    } else if !subset(incoming, current, &mut metrics.comparisons) {
        *current = merged(current, incoming, &mut metrics.comparisons).into();
    }
}

fn subset(candidate: &[usize], containing: &[usize], comparisons: &mut u64) -> bool {
    if candidate.len() > containing.len() {
        return false;
    }
    candidate.iter().all(|value| {
        containing
            .binary_search_by(|current| {
                *comparisons += 1;
                current.cmp(value)
            })
            .is_ok()
    })
}

fn initial_sets(
    direct: &[Arc<[FunctionParameterDependency]>],
) -> (Vec<FunctionParameterDependency>, Vec<Set>, HashSet<Set>) {
    let mut parameters = Vec::new();
    let mut parameter_ids = HashMap::default();
    let mut inventory = HashSet::default();
    // Completed direct inventories remain live in `direct`. Their shared
    // allocation is only a shortcut for reusing the exact same checked slice.
    let mut direct_sets = HashMap::<*const FunctionParameterDependency, Set>::default();
    let sets = direct
        .iter()
        .map(|values| {
            let identity = Arc::as_ptr(values).cast::<FunctionParameterDependency>();
            if let Some(set) = direct_sets.get(&identity) {
                return Arc::clone(set);
            }
            let mut ids = values
                .iter()
                .map(|value| {
                    let next = parameters.len();
                    *parameter_ids.entry(value.clone()).or_insert_with(|| {
                        parameters.push(value.clone());
                        next
                    })
                })
                .collect::<Vec<_>>();
            ids.sort_unstable();
            ids.dedup();
            let set = intern(ids, &mut inventory);
            direct_sets.insert(identity, Arc::clone(&set));
            set
        })
        .collect();
    (parameters, sets, inventory)
}

fn intern(values: Vec<usize>, inventory: &mut HashSet<Set>) -> Set {
    if let Some(set) = inventory.get(values.as_slice()) {
        return Arc::clone(set);
    }
    let set: Set = values.into();
    inventory.insert(Arc::clone(&set));
    set
}

fn merged(current: &Set, incoming: &Set, comparisons: &mut u64) -> Vec<usize> {
    let mut union = Vec::with_capacity(current.len() + incoming.len());
    let (mut left, mut right) = (0, 0);
    while left < current.len() && right < incoming.len() {
        *comparisons += 1;
        match current[left].cmp(&incoming[right]) {
            std::cmp::Ordering::Less => {
                union.push(current[left]);
                left += 1;
            }
            std::cmp::Ordering::Greater => {
                union.push(incoming[right]);
                right += 1;
            }
            std::cmp::Ordering::Equal => {
                union.push(current[left]);
                left += 1;
                right += 1;
            }
        }
    }
    union.extend_from_slice(&current[left..]);
    union.extend_from_slice(&incoming[right..]);
    union
}

#[cfg(test)]
mod tests;
