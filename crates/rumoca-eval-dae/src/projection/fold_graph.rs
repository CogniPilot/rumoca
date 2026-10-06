mod closure;
mod fragments;

use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use rumoca_ir_dae as dae;

use super::FunctionParameterDependency;
use super::dependencies::OrderedDependencies;
use super::domain_context::Context;

/// One source transition at an exact lexical parent environment. The selected
/// initial/update expressions retain the construction-issued read versions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct FoldNode<'dae> {
    pub(super) activation: super::Activation,
    pub(super) fold: dae::FunctionFoldId<'dae>,
    pub(super) carried: u32,
    pub(super) field: Option<usize>,
    pub(super) scalar: usize,
    pub(super) initial: dae::ExprId<'dae>,
    pub(super) update: dae::ExprId<'dae>,
    pub(super) parent: Arc<Context>,
}

/// A reachable dependency graph, not a collection of reusable partial walks.
/// Pending and cyclic edges are retained until every node has been checked.
#[derive(Debug, Default)]
pub(super) struct FoldGraph<'dae> {
    inventory: HashMap<FoldNode<'dae>, usize>,
    nodes: Vec<FoldNode<'dae>>,
    edges: Vec<Vec<usize>>,
    direct: Vec<Arc<[FunctionParameterDependency]>>,
    direct_inventory: fragments::Inventories,
    active_dependencies: fragments::Recording,
    edge_membership: Vec<HashSet<usize>>,
    active: Option<usize>,
    cursor: usize,
    #[cfg(debug_assertions)]
    edge_count: usize,
    #[cfg(test)]
    pub(super) repeated_edges: usize,
}

impl<'dae> FoldGraph<'dae> {
    pub(super) fn enqueue(&mut self, node: FoldNode<'dae>) {
        let index = if let Some(index) = self.inventory.get(&node) {
            #[cfg(test)]
            {
                self.repeated_edges += 1;
            }
            *index
        } else {
            let index = self.nodes.len();
            self.inventory.insert(node.clone(), index);
            self.nodes.push(node);
            self.edges.push(Vec::new());
            self.edge_membership.push(HashSet::default());
            self.direct.push(Arc::from([]));
            index
        };
        if let Some(active) = self.active
            && self.edge_membership[active].insert(index)
        {
            self.edges[active].push(index);
            #[cfg(debug_assertions)]
            {
                self.edge_count += 1;
            }
        }
    }

    pub(super) fn begin_next(&mut self) -> Option<FoldNode<'dae>> {
        let node = self.nodes.get(self.cursor)?.clone();
        #[cfg(debug_assertions)]
        super::profile::graph(&node, self.cursor, self.nodes.len(), self.edge_count);
        self.active = Some(self.cursor);
        self.cursor += 1;
        Some(node)
    }

    pub(super) fn finish_node(&mut self) {
        let index = self.active.expect("only an active checked node can finish");
        let completed = self
            .direct_inventory
            .finish(std::mem::take(&mut self.active_dependencies));
        self.direct[index] = completed;
        self.active = None;
    }

    pub(super) fn capture(&mut self, dependency: &FunctionParameterDependency) {
        if self.active.is_some() {
            self.active_dependencies.scalar(dependency);
        }
    }

    pub(super) fn capture_completed(&mut self, dependencies: &Arc<[FunctionParameterDependency]>) {
        if self.active.is_some() {
            self.active_dependencies.completed(dependencies);
        }
    }

    #[cfg(test)]
    pub(super) fn ordered_edges(&self) -> Vec<(FoldNode<'dae>, Vec<FoldNode<'dae>>)> {
        self.nodes
            .iter()
            .cloned()
            .zip(self.edges.iter().map(|edges| {
                edges
                    .iter()
                    .map(|index| self.nodes[*index].clone())
                    .collect()
            }))
            .collect()
    }

    /// Complete every reachable closure before publishing reusable results.
    pub(super) fn completed(self) -> Vec<(FoldNode<'dae>, Arc<[FunctionParameterDependency]>)> {
        assert_eq!(
            self.cursor,
            self.nodes.len(),
            "the whole reachable graph was checked"
        );
        assert!(self.active.is_none(), "no active node can be completed");
        let (closures, _) = closure::complete(&self.edges, &self.direct);
        self.nodes.into_iter().zip(closures).collect()
    }
}
