//! Extends-chain lookups shared by redeclaration handling.
//!
//! MLS §7.1 inheritance makes base classes and their nested classes reachable
//! from a derived class, so redeclare targets must be searched along the whole
//! extends chain instead of a single class body.

use crate::{InstantiateError, InstantiateResult};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;
use std::collections::{HashSet, VecDeque};

pub(super) fn exact_base_class<'a>(
    tree: &'a ast::ClassTree,
    extend: &ast::Extend,
) -> InstantiateResult<Option<&'a ast::ClassDef>> {
    if crate::inheritance::predefined_extend_name(tree, extend)?.is_some() {
        return Ok(None);
    }
    let base_def_id = extend
        .base_def_id
        .or(extend.base_name.def_id)
        .ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("extends edge `{}`", extend.base_name),
                extend.location.span(),
            ))
        })?;
    tree.get_class_by_def_id(base_def_id)
        .map(Some)
        .ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("extends edge `{}` ({base_def_id:?})", extend.base_name),
                extend.location.span(),
            ))
        })
}

pub(super) fn extends_base_classes<'a>(
    tree: &'a ast::ClassTree,
    class: &'a ast::ClassDef,
) -> InstantiateResult<Vec<&'a ast::ClassDef>> {
    let mut bases = Vec::new();
    for extend in &class.extends {
        if let Some(base) = exact_base_class(tree, extend)? {
            bases.push(base);
        }
    }
    Ok(bases)
}

/// Find a nested class by name in a class and its extends chain.
///
/// MLS §7.3 redeclare targets can be inherited via extends, so component-level
/// redeclare modifiers must recognize replaceable nested classes from base
/// types. Resolve identity is authoritative; spelling is display evidence only.
pub(crate) fn find_nested_class_in_hierarchy<'a>(
    tree: &'a ast::ClassTree,
    root: &'a ast::ClassDef,
    nested_name: &str,
) -> InstantiateResult<Option<&'a ast::ClassDef>> {
    let mut to_visit = VecDeque::from([(root, 0_usize)]);
    let mut complete = HashSet::<DefId>::new();
    let mut candidate_depth = None;
    let mut candidates = ast::AstIndexMap::<DefId, &ast::ClassDef>::default();
    while let Some((class, depth)) = to_visit.pop_front() {
        let def_id = class.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("nested-class lookup owner `{}`", class.name.text),
                class.location.span(),
            ))
        })?;
        if !complete.insert(def_id) {
            continue;
        }
        if let Some(nested) = class.classes.get(nested_name) {
            let nested_def_id = nested.def_id.ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!("nested redeclare target `{nested_name}`"),
                    nested.location.span(),
                ))
            })?;
            if candidate_depth.is_none_or(|candidate_depth| depth <= candidate_depth) {
                candidate_depth = Some(depth);
                candidates.entry(nested_def_id).or_insert(nested);
            }
        }
        for extend in &class.extends {
            if let Some(base) = exact_base_class(tree, extend)? {
                to_visit.push_back((base, depth + 1));
            }
        }
    }
    if candidates.len() > 1 {
        return Err(Box::new(InstantiateError::redeclare_error(
            nested_name,
            format!(
                "inherited nested-class lookup is ambiguous between exact identities {:?}",
                candidates.keys().collect::<Vec<_>>()
            ),
            root.location.span(),
        )));
    }
    Ok(candidates.into_values().next())
}

/// Find the inherited nested-class slot selected by Resolve's exact identity.
///
/// Multiple bases may legally declare the same spelling. The resolved LHS
/// identity is therefore the selector; names are never used to rediscover it.
pub(crate) fn find_nested_class_by_def_id_in_hierarchy<'a>(
    tree: &'a ast::ClassTree,
    root: &'a ast::ClassDef,
    target_def_id: DefId,
) -> InstantiateResult<Option<&'a ast::ClassDef>> {
    let mut to_visit = VecDeque::from([root]);
    let mut complete = HashSet::<DefId>::new();
    let mut selected = None;
    while let Some(class) = to_visit.pop_front() {
        let class_def_id = class.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("exact nested-class lookup owner `{}`", class.name.text),
                class.location.span(),
            ))
        })?;
        if !complete.insert(class_def_id) {
            continue;
        }
        for nested in class.classes.values() {
            let nested_def_id = nested.def_id.ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!("nested class `{}`", nested.name.text),
                    nested.location.span(),
                ))
            })?;
            if nested_def_id == target_def_id {
                selected.get_or_insert(nested);
            }
        }
        for extend in &class.extends {
            if let Some(base) = exact_base_class(tree, extend)? {
                to_visit.push_back(base);
            }
        }
    }
    Ok(selected)
}

/// Find the direct or inherited component slot selected by Resolve's exact
/// declaration identity.
pub(crate) fn find_component_by_def_id_in_hierarchy<'a>(
    tree: &'a ast::ClassTree,
    root: &'a ast::ClassDef,
    target_def_id: DefId,
) -> InstantiateResult<Option<&'a ast::Component>> {
    let mut to_visit = VecDeque::from([root]);
    let mut complete = HashSet::<DefId>::new();
    while let Some(class) = to_visit.pop_front() {
        let class_def_id = class.def_id.ok_or_else(|| {
            Box::new(InstantiateError::missing_resolved_identity(
                format!("exact component-slot lookup owner `{}`", class.name.text),
                class.location.span(),
            ))
        })?;
        if !complete.insert(class_def_id) {
            continue;
        }
        for component in class.components.values() {
            let component_def_id = component.def_id.ok_or_else(|| {
                Box::new(InstantiateError::missing_resolved_identity(
                    format!("component slot `{}`", component.name),
                    component.location.span(),
                ))
            })?;
            if component_def_id == target_def_id {
                return Ok(Some(component));
            }
        }
        for extend in &class.extends {
            if let Some(base) = exact_base_class(tree, extend)? {
                to_visit.push_back(base);
            }
        }
    }
    Ok(None)
}

/// Prove that an exact declaration identity belongs to an inherited component
/// slot rather than to the nested-class namespace.
pub(crate) fn contains_component_by_def_id_in_hierarchy(
    tree: &ast::ClassTree,
    root: &ast::ClassDef,
    target_def_id: DefId,
) -> InstantiateResult<bool> {
    Ok(find_component_by_def_id_in_hierarchy(tree, root, target_def_id)?.is_some())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: std::sync::Arc::from(text),
            ..Default::default()
        }
    }

    fn resolved_name(text: &str, def_id: DefId) -> ast::Name {
        ast::Name {
            name: vec![token(text)],
            def_id: Some(def_id),
        }
    }

    fn class(name: &str, def_id: DefId) -> ast::ClassDef {
        ast::ClassDef {
            name: token(name),
            def_id: Some(def_id),
            ..Default::default()
        }
    }

    fn extend(name: &str, def_id: DefId) -> ast::Extend {
        ast::Extend {
            base_name: resolved_name(name, def_id),
            base_def_id: Some(def_id),
            ..Default::default()
        }
    }

    fn insert(tree: &mut ast::ClassTree, key: &str, class: ast::ClassDef) {
        let def_id = class.def_id.expect("test class identity");
        tree.name_map.insert(key.to_string(), def_id);
        tree.def_map.insert(def_id, key.to_string());
        tree.definitions.classes.insert(key.to_string(), class);
    }

    #[test]
    fn nested_lookup_follows_more_than_the_old_depth_limit() {
        let mut tree = ast::ClassTree::default();
        for index in (0..40_u32).rev() {
            let def_id = DefId::new(5_000 + index);
            let mut current = class(&format!("C{index}"), def_id);
            if index < 39 {
                current.extends.push(extend(
                    &format!("C{}", index + 1),
                    DefId::new(5_001 + index),
                ));
            } else {
                current
                    .classes
                    .insert("Target".to_string(), class("Target", DefId::new(5_100)));
            }
            insert(&mut tree, &format!("C{index}"), current);
        }

        let root = tree
            .get_class_by_def_id(DefId::new(5_000))
            .expect("root identity");
        let target = find_nested_class_in_hierarchy(&tree, root, "Target")
            .expect("long exact hierarchy is valid")
            .expect("deep nested class is found");
        assert_eq!(target.def_id, Some(DefId::new(5_100)));
    }

    #[test]
    fn direct_base_materialization_refuses_missing_later_edge_atomically() {
        let root_id = DefId::new(5_300);
        let good_id = DefId::new(5_301);
        let missing_id = DefId::new(5_302);
        let mut root = class("Root", root_id);
        root.extends.push(extend("Good", good_id));
        root.extends.push(extend("Missing", missing_id));
        let mut tree = ast::ClassTree::default();
        insert(&mut tree, "Root", root);
        insert(&mut tree, "Good", class("Good", good_id));

        let root = tree.get_class_by_def_id(root_id).expect("root identity");
        assert!(extends_base_classes(&tree, root).is_err());
    }

    #[test]
    fn nested_lookup_does_not_follow_same_spelled_different_identity() {
        let tree = crate::test_support::resolved_tree(
            "nested_lookup_identity.mo",
            r"
package P
  model Base end Base;
end P;
package Q
  model Base
    model Target end Target;
  end Base;
end Q;
model Root
  extends P.Base;
end Root;
",
        );
        let root = tree
            .get_class_by_qualified_name("Root")
            .expect("root identity");
        assert_eq!(
            find_nested_class_in_hierarchy(&tree, root, "Target")
                .expect("selected exact graph is valid"),
            None
        );
    }

    #[test]
    fn nested_lookup_refuses_ambiguous_same_depth_identities() {
        let root_id = DefId::new(5_500);
        let a_id = DefId::new(5_501);
        let b_id = DefId::new(5_502);
        let mut root = class("Root", root_id);
        root.extends.push(extend("A", a_id));
        root.extends.push(extend("B", b_id));
        let mut a = class("A", a_id);
        a.classes
            .insert("Target".to_string(), class("Target", DefId::new(5_503)));
        let mut b = class("B", b_id);
        b.classes
            .insert("Target".to_string(), class("Target", DefId::new(5_504)));
        let mut tree = ast::ClassTree::default();
        insert(&mut tree, "Root", root);
        insert(&mut tree, "A", a);
        insert(&mut tree, "B", b);

        let root = tree.get_class_by_def_id(root_id).expect("root identity");
        assert!(find_nested_class_in_hierarchy(&tree, root, "Target").is_err());
    }

    #[test]
    fn exact_nested_lookup_selects_one_of_two_same_spelled_slots() {
        let root_id = DefId::new(5_600);
        let a_id = DefId::new(5_601);
        let b_id = DefId::new(5_602);
        let a_target_id = DefId::new(5_603);
        let b_target_id = DefId::new(5_604);
        let mut root = class("Root", root_id);
        root.extends.push(extend("A", a_id));
        root.extends.push(extend("B", b_id));
        let mut a = class("A", a_id);
        a.classes
            .insert("Target".to_string(), class("Target", a_target_id));
        let mut b = class("B", b_id);
        b.classes
            .insert("Target".to_string(), class("Target", b_target_id));
        let mut tree = ast::ClassTree::default();
        insert(&mut tree, "Root", root);
        insert(&mut tree, "A", a);
        insert(&mut tree, "B", b);

        let root = tree.get_class_by_def_id(root_id).expect("root identity");
        let selected = find_nested_class_by_def_id_in_hierarchy(&tree, root, b_target_id)
            .expect("exact hierarchy is valid")
            .expect("selected inherited slot exists");
        assert_eq!(selected.def_id, Some(b_target_id));
        assert_eq!(
            find_nested_class_by_def_id_in_hierarchy(&tree, root, DefId::new(5_699))
                .expect("absence is distinguished from malformed hierarchy"),
            None
        );
    }

    #[test]
    fn exact_component_lookup_classifies_an_inherited_redeclare_slot() {
        let root_id = DefId::new(5_700);
        let base_id = DefId::new(5_701);
        let selected_id = DefId::new(5_702);
        let other_id = DefId::new(5_703);
        let mut root = class("Root", root_id);
        root.extends.push(extend("Base", base_id));
        let mut base = class("Base", base_id);
        for (name, def_id) in [("selected", selected_id), ("same_spelling", other_id)] {
            let mut component = ast::Component::empty_with_span(rumoca_core::Span::DUMMY);
            component.name = name.to_string();
            component.def_id = Some(def_id);
            base.components.insert(name.to_string(), component);
        }
        let mut tree = ast::ClassTree::default();
        insert(&mut tree, "Root", root);
        insert(&mut tree, "Base", base);

        let root = tree.get_class_by_def_id(root_id).expect("root identity");
        assert!(
            contains_component_by_def_id_in_hierarchy(&tree, root, selected_id)
                .expect("exact inherited component graph is complete")
        );
        assert!(
            !contains_component_by_def_id_in_hierarchy(&tree, root, DefId::new(5_799))
                .expect("exact absence is valid")
        );
    }
}
