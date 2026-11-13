//! Exact-value construction issues handles; graph replay only carries them.
use super::{HashMap, fold_graph::FoldNode};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::sync::Arc;

#[derive(Debug, Clone)]
enum Identity<'dae> {
    Retained(Arc<FoldNode<'dae>>),
    Local(Arc<FoldNode<'dae>>),
}

/// Retained allocation identity is issued only by the exact-value interner.
/// Its Arc keeps that identity live in every graph and completed recipe.
#[derive(Debug, Clone)]
pub(super) struct Node<'dae>(Identity<'dae>);

impl<'dae> Deref for Node<'dae> {
    type Target = FoldNode<'dae>;

    fn deref(&self) -> &Self::Target {
        match &self.0 {
            Identity::Retained(value) | Identity::Local(value) => value,
        }
    }
}

impl PartialEq for Node<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (&self.0, &other.0) {
            (Identity::Retained(a), Identity::Retained(b)) => Arc::ptr_eq(a, b),
            (Identity::Local(a), Identity::Local(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Node<'_> {}

impl Hash for Node<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        std::mem::discriminant(&self.0).hash(state);
        match &self.0 {
            Identity::Retained(value) => Arc::as_ptr(value).hash(state),
            Identity::Local(value) => value.hash(state),
        }
    }
}

#[derive(Debug, Default)]
pub(super) struct Nodes<'dae> {
    exact: HashMap<FoldNode<'dae>, Arc<FoldNode<'dae>>>,
}

impl<'dae> Nodes<'dae> {
    pub(super) fn intern(&mut self, value: FoldNode<'dae>) -> Node<'dae> {
        self.intern_with_limit(value, 65_536)
    }

    fn intern_with_limit(&mut self, value: FoldNode<'dae>, limit: usize) -> Node<'dae> {
        if let Some(issued) = self.exact.get(&value) {
            return Node(Identity::Retained(Arc::clone(issued)));
        }
        if self.exact.len() < limit {
            let issued = Arc::new(value.clone());
            self.exact.insert(value, Arc::clone(&issued));
            Node(Identity::Retained(issued))
        } else {
            // Existing identities never change. New values use the original
            // full-value membership path and do not acquire a recipe cache.
            Node(Identity::Local(Arc::new(value)))
        }
    }

    #[cfg(test)]
    pub(super) fn with_test_limit(&mut self, value: FoldNode<'dae>, limit: usize) -> Node<'dae> {
        self.intern_with_limit(value, limit)
    }
}

impl Node<'_> {
    pub(super) fn retained(&self) -> bool {
        matches!(self.0, Identity::Retained(_))
    }
}
