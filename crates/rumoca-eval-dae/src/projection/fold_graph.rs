#[cfg(test)]
mod closure;
mod fragments;

use crate::projection::{HashMap, HashSet};
use std::sync::Arc;

use rumoca_ir_dae as dae;

use super::dependencies::OrderedDependencies;
use super::domain_context::Context;
use super::fold_nodes::Node;
use super::{FieldPath, FunctionParameterDependency};

/// One source transition at an exact lexical parent environment. The selected
/// initial/update expressions retain the construction-issued read versions.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) struct FoldNode<'dae> {
    pub(super) activation: super::Activation,
    pub(super) integer_profile: Arc<[super::IntegerBinding]>,
    pub(super) fold: dae::FunctionFoldId<'dae>,
    pub(super) carried: u32,
    pub(super) field: Option<FieldPath>,
    pub(super) scalar: usize,
    pub(super) initial: dae::ExprId<'dae>,
    pub(super) update: dae::ExprId<'dae>,
    pub(super) parent: Arc<Context>,
}

/// One fully checked node body, preserving both direct reads and edge order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct FoldRecipe<'dae> {
    direct: Arc<[FunctionParameterDependency]>,
    edges: Arc<[Node<'dae>]>,
}

impl<'dae> FoldRecipe<'dae> {
    pub(super) fn direct(&self) -> &[FunctionParameterDependency] {
        &self.direct
    }
}

/// A reachable dependency graph, not a collection of reusable partial walks.
/// Pending and cyclic edges are retained until every node has been checked.
#[derive(Debug, Default)]
pub(super) struct FoldGraph<'dae> {
    inventory: HashMap<Node<'dae>, usize>,
    nodes: Vec<Node<'dae>>,
    edges: Vec<Vec<usize>>,
    direct: Vec<Arc<[FunctionParameterDependency]>>,
    direct_inventory: fragments::Inventories,
    active_dependencies: fragments::Recording,
    edge_membership: Vec<HashSet<usize>>,
    active: Option<usize>,
    cursor: usize,
    #[cfg(test)]
    pub(super) repeated_edges: usize,
    #[cfg(test)]
    pub(super) recorded_edges: usize,
    #[cfg(test)]
    pub(super) replayed_edges: usize,
}

impl<'dae> FoldGraph<'dae> {
    pub(super) fn enqueue(&mut self, node: Node<'dae>) {
        let index = self.node_index(node);
        if let Some(active) = self.active
            && self.edge_membership[active].insert(index)
        {
            self.edges[active].push(index);
            #[cfg(test)]
            {
                self.recorded_edges += 1;
            }
        }
    }

    fn node_index(&mut self, node: Node<'dae>) -> usize {
        if let Some(index) = self.inventory.get(&node) {
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
        }
    }

    /// A recipe's constructor has already issued its unique ordered edges.
    /// Re-interning their immutable handles preserves that uniqueness, so the
    /// replay arm needs no second edge-membership proof or allocation.
    pub(super) fn replay_edges(&mut self, recipe: &FoldRecipe<'dae>) {
        let active = self.active.expect("a replay has one active node");
        assert!(
            self.edges[active].is_empty(),
            "replay replaces no source walk"
        );
        self.edges[active] = recipe
            .edges
            .iter()
            .cloned()
            .map(|node| self.node_index(node))
            .collect();
        #[cfg(test)]
        {
            self.replayed_edges += recipe.edges.len();
        }
    }

    pub(super) fn begin_next(&mut self) -> Option<Node<'dae>> {
        let node = self.nodes.get(self.cursor)?.clone();
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
            .map(|node| (**node).clone())
            .zip(self.edges.iter().map(|edges| {
                edges
                    .iter()
                    .map(|index| (*self.nodes[*index]).clone())
                    .collect()
            }))
            .collect()
    }

    /// Publish only after every node and outgoing edge has been checked.
    pub(super) fn recipes(&self) -> Vec<(Node<'dae>, FoldRecipe<'dae>)> {
        assert_eq!(
            self.cursor,
            self.nodes.len(),
            "the whole reachable graph was checked"
        );
        assert!(self.active.is_none(), "an active node cannot be published");
        self.nodes
            .iter()
            .cloned()
            .enumerate()
            .map(|(index, node)| {
                let edges = self.edges[index]
                    .iter()
                    .map(|edge| self.nodes[*edge].clone())
                    .collect::<Vec<_>>();
                (
                    node,
                    FoldRecipe {
                        direct: Arc::clone(&self.direct[index]),
                        edges: edges.into(),
                    },
                )
            })
            .collect()
    }

    /// Completed closures are a test oracle, never a production replay path.
    #[cfg(test)]
    pub(super) fn completed(self) -> Vec<(FoldNode<'dae>, Arc<[FunctionParameterDependency]>)> {
        assert_eq!(
            self.cursor,
            self.nodes.len(),
            "the whole reachable graph was checked"
        );
        assert!(self.active.is_none(), "no active node can be completed");
        let (closures, _) = closure::complete(&self.edges, &self.direct);
        self.nodes
            .into_iter()
            .map(|node| (*node).clone())
            .zip(closures)
            .collect()
    }
}
