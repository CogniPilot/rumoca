//! Collection of the redeclarations effective for one class context.
//!
//! MLS §7.3 redeclarations reach a class through its own nested declarations,
//! through its enclosing package and that package's extends chain, and through
//! extends-modifications. This module walks those sources into a
//! [`TypeOverrideMap`].

use super::class_hierarchy::{
    contains_component_by_def_id_in_hierarchy, exact_base_class,
    find_nested_class_by_def_id_in_hierarchy,
};
use super::override_map::TypeOverrideMap;
use super::redeclare_values::{resolve_cref_def_id, resolve_redeclare_value_def_id};
use crate::traversal_adapter::redeclare_target_value;
use rumoca_ir_ast as ast;
use std::collections::HashSet;

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
/// Returns a map from each exact alias declaration DefId to its selected DefId.
pub(crate) fn build_type_override_map(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> crate::InstantiateResult<TypeOverrideMap> {
    let mut overrides = TypeOverrideMap::new();

    // 1. Collect from the class's own nested classes.
    insert_nested_class_overrides(tree, class, &mut overrides)?;

    // 2. Collect from the enclosing class's nested classes.
    // This handles the pattern where a record type (like ThermodynamicState)
    // is redeclared in the enclosing package, and components in the model
    // reference it by its short name.
    collect_enclosing_type_overrides(tree, class, mod_env, &mut overrides)?;

    // 3. Collect package/type redeclarations from extends-modifications
    // (e.g., extends Base(redeclare replaceable package Medium = ...)).
    collect_extends_redeclare_overrides(tree, class, mod_env, &mut overrides)?;

    Ok(overrides)
}

/// Collect type overrides from the enclosing class's nested classes.
///
/// Helper for [`build_type_override_map`] to reduce nesting depth.
fn collect_enclosing_type_overrides(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut TypeOverrideMap,
) -> crate::InstantiateResult<()> {
    let Some(class_def_id) = class.def_id else {
        return Err(override_collection_error(
            tree,
            class,
            "class context has no exact DefId",
        )?);
    };
    let Some(qualified_name) = tree.def_map.get(&class_def_id) else {
        return Err(override_collection_error(
            tree,
            class,
            "class context DefId is absent from the declaration map",
        )?);
    };
    let Some(parent_name) = tree.enclosing_class_names_of(qualified_name).next() else {
        return Ok(());
    };
    let Some(parent_class) = tree.get_class_by_qualified_name(parent_name) else {
        return Err(override_collection_error(
            tree,
            class,
            "enclosing class identity is absent from the resolved tree",
        )?);
    };
    collect_nested_overrides_in_extends_chain(tree, parent_class, mod_env, overrides)
}

/// Collect nested class overrides from a class and all of its base classes.
///
/// MLS §7.3 redeclarations are inherited through extends-chains, so a derived
/// package can provide the effective type used by descendant models even when
/// the redeclare is not declared directly in the immediate parent package.
pub(super) fn collect_nested_overrides_in_extends_chain(
    tree: &ast::ClassTree,
    root: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut TypeOverrideMap,
) -> crate::InstantiateResult<()> {
    let mut planned = overrides.clone();
    collect_nested_overrides_in_extends_chain_inner(tree, root, mod_env, &mut planned)?;
    *overrides = planned;
    Ok(())
}

fn collect_nested_overrides_in_extends_chain_inner(
    tree: &ast::ClassTree,
    root: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut TypeOverrideMap,
) -> crate::InstantiateResult<()> {
    let mut to_visit = vec![root];
    let mut visited = HashSet::new();
    while let Some(class) = to_visit.pop() {
        let Some(def_id) = class.def_id else {
            return Err(override_collection_error(
                tree,
                class,
                "extends-chain class has no exact DefId",
            )?);
        };
        if !visited.insert(def_id) {
            continue;
        }
        insert_nested_class_overrides(tree, class, overrides)?;
        insert_extends_redeclare_overrides(tree, class, mod_env, overrides)?;
        for extend in class.extends.iter().rev() {
            if crate::inheritance::predefined_extend_name(tree, extend)?.is_some() {
                continue;
            }
            let Some(base_def_id) = extend.base_def_id.or(extend.base_name.def_id) else {
                return Err(override_collection_error(
                    tree,
                    class,
                    "extends edge has no exact base-class DefId",
                )?);
            };
            let Some(base) = tree.get_class_by_def_id(base_def_id) else {
                return Err(override_collection_error(
                    tree,
                    class,
                    "extends base DefId is absent from the resolved tree",
                )?);
            };
            if !visited.contains(&base_def_id) {
                to_visit.push(base);
            }
        }
    }
    Ok(())
}

fn insert_extends_redeclare_overrides(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    mod_env: Option<&ast::ModificationEnvironment>,
    overrides: &mut TypeOverrideMap,
) -> crate::InstantiateResult<()> {
    for extend in &class.extends {
        for ext_mod in &extend.modifications {
            if !ext_mod.redeclare {
                continue;
            }
            let Some((alias_def_id, def_id)) =
                exact_class_redeclare_override(tree, class, extend, ext_mod, mod_env)?
            else {
                continue;
            };
            // The derived class is visited before its bases. Refuse to replace
            // its exact selection while replaying inherited evidence.
            overrides.insert_alias_if_absent(alias_def_id, def_id);
        }
    }
    Ok(())
}

fn insert_nested_class_overrides(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    overrides: &mut TypeOverrideMap,
) -> crate::InstantiateResult<()> {
    for nested in class.classes.values() {
        let Some(def_id) = nested.def_id else {
            return Err(override_collection_error(
                tree,
                nested,
                "nested override declaration has no exact DefId",
            )?);
        };
        let target_def_id = overrides
            .checked_target_for_alias_def_id(tree, def_id, nested.location.span())?
            .unwrap_or(def_id);
        // The derived class is visited before its bases. `if_absent` preserves
        // that deterministic MLS override precedence.
        overrides.insert_alias_if_absent(def_id, target_def_id);
    }
    Ok(())
}

fn override_collection_error(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    reason: &str,
) -> crate::InstantiateResult<Box<crate::InstantiateError>> {
    let span = crate::location_to_span(
        &class.location,
        &tree.source_map,
        "type override collection",
    )?;
    Ok(Box::new(crate::InstantiateError::redeclare_error(
        class.name.text.as_ref(),
        reason,
        span,
    )))
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
) -> crate::InstantiateResult<()> {
    for extend in &class.extends {
        for ext_mod in &extend.modifications {
            if !ext_mod.redeclare {
                continue;
            }
            let Some((alias_def_id, def_id)) =
                exact_class_redeclare_override(tree, class, extend, ext_mod, mod_env)?
            else {
                continue;
            };
            overrides.insert_alias(alias_def_id, def_id);
        }
    }
    Ok(())
}

/// Class and component redeclarations share the `redeclare` source marker but
/// have distinct semantic owners. This collector owns only class/package
/// overrides; a component slot is positively identified and left to the
/// inheritance component-redeclare transaction.
fn exact_class_redeclare_override(
    tree: &ast::ClassTree,
    class: &ast::ClassDef,
    extend: &ast::Extend,
    modification: &ast::ExtendModification,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> crate::InstantiateResult<Option<(rumoca_core::DefId, rumoca_core::DefId)>> {
    let Some((target_name, target_ref, value_expr)) = redeclare_target_value(modification) else {
        return Err(override_collection_error(
            tree,
            class,
            "redeclare extends modifier has no exact target/value structure",
        )?);
    };
    let Some(alias_def_id) = resolve_cref_def_id(target_ref) else {
        return Err(override_collection_error(
            tree,
            class,
            "redeclare extends modifier LHS has no exact resolved DefId",
        )?);
    };
    let Some(base_class) = exact_base_class(tree, extend)? else {
        return Err(override_collection_error(
            tree,
            class,
            "predefined extends edge cannot own a redeclare slot",
        )?);
    };
    if contains_component_by_def_id_in_hierarchy(tree, base_class, alias_def_id)? {
        return Ok(None);
    }
    if find_nested_class_by_def_id_in_hierarchy(tree, base_class, alias_def_id)?.is_some() {
        let Some(def_id) = resolve_redeclare_value_def_id(tree, value_expr, mod_env)? else {
            return Err(override_collection_error(
                tree,
                class,
                "class redeclare extends modifier RHS has no exact resolved DefId",
            )?);
        };
        return Ok(Some((alias_def_id, def_id)));
    }
    Err(override_collection_error(
        tree,
        class,
        &format!(
            "redeclare extends modifier target `{target_name}` identifies {alias_def_id:?}, which is neither an exact inherited class nor component slot"
        ),
    )?)
}
