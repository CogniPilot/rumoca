//! Extends-chain lookups shared by redeclaration handling.
//!
//! MLS §7.1 inheritance makes base classes and their nested classes reachable
//! from a derived class, so redeclare targets must be searched along the whole
//! extends chain instead of a single class body.

use crate::find_class_in_tree;
use rumoca_core::DefId;
use rumoca_ir_ast as ast;

pub(super) fn extends_base_classes<'a>(
    tree: &'a ast::ClassTree,
    class: &'a ast::ClassDef,
) -> Vec<&'a ast::ClassDef> {
    class
        .extends
        .iter()
        .filter_map(|ext| {
            let base_name = ext.base_name.to_string();
            ext.base_def_id
                .and_then(|def_id| tree.get_class_by_def_id(def_id))
                .or_else(|| find_class_in_tree(tree, &base_name))
        })
        .collect()
}

/// Find a nested class by name in a class and its extends chain.
///
/// MLS §7.3 redeclare targets can be inherited via extends, so component-level
/// redeclare modifiers must recognize replaceable nested classes from base types.
pub(crate) fn find_nested_class_in_hierarchy<'a>(
    tree: &'a ast::ClassTree,
    root: &'a ast::ClassDef,
    nested_name: &str,
) -> Option<&'a ast::ClassDef> {
    const MAX_DEPTH: usize = 32;

    let mut to_visit = vec![root];
    let mut visited_def_ids = std::collections::HashSet::<DefId>::new();
    let mut visited_names = std::collections::HashSet::<String>::new();

    for _ in 0..MAX_DEPTH {
        if to_visit.is_empty() {
            break;
        }

        let mut next = Vec::new();
        for class in to_visit.drain(..) {
            let already_seen = match class.def_id {
                Some(def_id) => !visited_def_ids.insert(def_id),
                None => !visited_names.insert(class.name.text.to_string()),
            };
            if already_seen {
                continue;
            }

            if let Some(nested) = class.classes.get(nested_name) {
                return Some(nested);
            }

            next.extend(class.extends.iter().filter_map(|ext| {
                let base_name = ext.base_name.to_string();
                ext.base_def_id
                    .and_then(|def_id| tree.get_class_by_def_id(def_id))
                    .or_else(|| find_class_in_tree(tree, &base_name))
            }));
        }
        to_visit = next;
    }

    None
}
