//! Collection of the redeclarations effective for one class context.
//!
//! MLS §7.3 redeclarations reach a class through its own nested declarations,
//! through its enclosing package and that package's extends chain, and through
//! extends-modifications. This module walks those sources into a
//! [`TypeOverrideMap`].

use super::class_hierarchy::{extends_base_classes, find_nested_class_in_hierarchy};
use super::override_map::TypeOverrideMap;
use super::redeclare_values::resolve_redeclare_value_def_id;
use crate::traversal_adapter::{
    redeclare_target_value, walk_class_extends_modifications, walk_nested_classes,
};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;

/// Build a type override map for replaceable type redeclarations (MLS §7.3).
///
/// When a class redeclares a replaceable type (e.g.,
/// `redeclare record extends ThermodynamicState`), inherited components
/// referencing the original type should use the redeclared version.
///
/// This collects type name -> DefId mappings from:
/// 1. The class's own nested classes (redeclared types in this class)
/// 2. The enclosing class's nested classes (sibling type redeclarations)
///
/// Returns a map from unqualified type name to the DefId of the local version.
pub(crate) fn build_type_override_map(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> TypeOverrideMap {
    let mut overrides = TypeOverrideMap::new();

    // 1. Collect from the class's own nested classes
    walk_nested_classes(class, |name, nested| {
        if let Some(def_id) = nested.def_id {
            overrides.insert_alias(ast::QualifiedName::from_ident(name), Some(def_id), def_id);
        }
    });

    // 2. Collect from the enclosing class's nested classes.
    // This handles the pattern where a record type (like ThermodynamicState)
    // is redeclared in the enclosing package, and components in the model
    // reference it by its short name.
    collect_enclosing_type_overrides(tree, class, mod_env, &mut overrides);

    // 3. Collect package/type redeclarations from extends-modifications
    // (e.g., extends Base(redeclare replaceable package Medium = ...)).
    collect_extends_redeclare_overrides(tree, class, mod_env, &mut overrides);

    overrides
}

/// Collect type overrides from the enclosing class's nested classes.
///
/// Helper for [`build_type_override_map`] to reduce nesting depth.
fn collect_enclosing_type_overrides(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut TypeOverrideMap,
) {
    let Some(class_def_id) = class.def_id else {
        return;
    };
    let Some(qualified_name) = tree.def_map.get(&class_def_id) else {
        return;
    };
    let Some(parent_name) = tree.enclosing_class_names_of(qualified_name).next() else {
        return;
    };
    let Some(parent_class) = tree.get_class_by_qualified_name(parent_name) else {
        return;
    };
    collect_nested_overrides_in_extends_chain(tree, parent_class, mod_env, overrides);
}

/// Collect nested class overrides from a class and all of its base classes.
///
/// MLS §7.3 redeclarations are inherited through extends-chains, so a derived
/// package can provide the effective type used by descendant models even when
/// the redeclare is not declared directly in the immediate parent package.
fn collect_nested_overrides_in_extends_chain(
    tree: &ast::ClassTree,
    root: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut TypeOverrideMap,
) {
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
            if is_visited_class(class, &mut visited_def_ids, &mut visited_names) {
                continue;
            }

            insert_nested_class_overrides(class, overrides);
            insert_extends_redeclare_overrides(tree, class, mod_env, overrides);
            next.extend(extends_base_classes(tree, class));
        }
        to_visit = next;
    }
}

fn insert_extends_redeclare_overrides(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut TypeOverrideMap,
) {
    walk_class_extends_modifications(class, |_, ext_mod| {
        let Some((target_name, value_expr)) = redeclare_target_value(ext_mod) else {
            return;
        };
        let Some(def_id) = resolve_redeclare_value_def_id(tree, value_expr, mod_env) else {
            return;
        };
        let alias_def_id = find_nested_class_in_hierarchy(tree, class, target_name)
            .and_then(|nested| nested.def_id);
        overrides.insert_alias_if_absent(
            ast::QualifiedName::from_ident(target_name),
            alias_def_id,
            def_id,
        );
    });
}

fn is_visited_class(
    class: &ast::ClassDef,
    visited_def_ids: &mut std::collections::HashSet<DefId>,
    visited_names: &mut std::collections::HashSet<String>,
) -> bool {
    match class.def_id {
        Some(def_id) => !visited_def_ids.insert(def_id),
        None => !visited_names.insert(class.name.text.to_string()),
    }
}

fn insert_nested_class_overrides(class: &ast::ClassDef, overrides: &mut TypeOverrideMap) {
    walk_nested_classes(class, |name, nested| {
        if let Some(def_id) = nested.def_id {
            let alias_path = ast::QualifiedName::from_ident(name);
            let target_def_id = overrides.target_for_path(&alias_path).unwrap_or(def_id);
            overrides.insert_alias_if_absent(alias_path, Some(def_id), target_def_id);
        }
    });
}

/// Collect redeclared type/package overrides from extends clause modifications.
///
/// MLS §7.3: A redeclare in an extends-modification overrides inherited replaceable
/// declarations in the derived class context.
fn collect_extends_redeclare_overrides(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut TypeOverrideMap,
) {
    walk_class_extends_modifications(class, |_, ext_mod| {
        let Some((target_name, value_expr)) = redeclare_target_value(ext_mod) else {
            return;
        };
        if let Some(def_id) = resolve_redeclare_value_def_id(tree, value_expr, mod_env) {
            let alias_def_id = find_nested_class_in_hierarchy(tree, class, target_name)
                .and_then(|nested| nested.def_id);
            overrides.insert_alias(
                ast::QualifiedName::from_ident(target_name),
                alias_def_id,
                def_id,
            );
        }
    });
}
