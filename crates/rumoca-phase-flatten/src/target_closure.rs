//! The classes whose type identities a flat model carries.
//!
//! A resolved tree may hold classes unrelated to the compiled target (a
//! session loads every document). The flat model of a target carries only the
//! types, enumerations and enumeration literals of the target's closure, so its
//! content does not depend on which other classes the session happens to hold.

use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use rustc_hash::FxHashSet;

/// Classes of a tree that lie outside a target's closure.
#[derive(Debug, Clone, Default)]
pub struct TargetClosure {
    outside: FxHashSet<DefId>,
}

impl TargetClosure {
    /// Every class of the tree is part of the closure.
    pub fn whole_tree() -> Self {
        Self::default()
    }

    /// The closure made of `reachable_classes`, their nested classes, and the
    /// predefined types. A class is outside when neither it nor any lexical
    /// ancestor is reachable.
    pub fn new(tree: &ast::ClassTree, reachable_classes: &[String]) -> Self {
        let reachable: FxHashSet<&str> = reachable_classes.iter().map(String::as_str).collect();
        let predefined: FxHashSet<DefId> = ["Real", "Integer", "Boolean", "String", "Clock"]
            .into_iter()
            .filter_map(|name| {
                tree.scope_tree
                    .predefined_member(&rumoca_core::ComponentPath::from_flat_path(name))
            })
            .collect();
        let outside = tree
            .def_map
            .iter()
            .filter(|(def_id, name)| {
                !predefined.contains(def_id) && !is_in_closure(name, &reachable)
            })
            .map(|(def_id, _)| *def_id)
            .collect();
        Self { outside }
    }

    /// Whether the declaration belongs to the closure.
    pub fn contains(&self, def_id: DefId) -> bool {
        !self.outside.contains(&def_id)
    }

    pub(crate) fn is_whole_tree(&self) -> bool {
        self.outside.is_empty()
    }
}

fn is_in_closure(name: &str, reachable: &FxHashSet<&str>) -> bool {
    reachable.contains(name)
        || name
            .match_indices('.')
            .any(|(dot, _)| reachable.contains(&name[..dot]))
}
