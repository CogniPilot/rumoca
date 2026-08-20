//! Extraction of component-level class overrides from resolved modifiers.
//!
//! Persisting the selections proved here lets downstream phases evaluate
//! instance-scoped constants of a redeclared class or package.

use super::class_hierarchy::find_nested_class_in_hierarchy;
use super::component_redeclare_validation::{
    reject_unmarked_component_class_replacement, validate_component_class_redeclare_target,
    validate_component_source_modifier_metadata,
};
use super::redeclare_modifiers::{
    class_redeclare_modifier_args, class_redeclare_target_ref,
    component_source_modifier_target_name, is_forwarding_component_redeclare,
};
use super::redeclare_values::{resolve_cref_def_id, resolve_redeclare_value_def_id};
use super::selected_class_members::resolve_class_override_modifier_targets;
use crate::{InstantiateError, InstantiateResult, location_to_span};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

/// MLS §7.3: component-level redeclare modifiers can target replaceable nested
/// classes declared in base classes (via extends). Persisting these resolved
/// overrides enables downstream phases to evaluate instance-scoped constants.
pub(crate) fn extract_component_class_overrides(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    target_class: Option<&ast::ClassDef>,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> InstantiateResult<ast::ClassOverrideMap> {
    let mut overrides = IndexMap::default();
    let Some(target_class) = target_class else {
        return Ok(overrides);
    };

    validate_component_source_modifier_metadata(tree, comp)?;
    for (index, mod_expr) in comp.source_modifications.iter().enumerate() {
        let is_redeclare = comp
            .source_modification_redeclare_flags
            .get(index)
            .copied()
            .unwrap_or(false);
        let Some(target_name) = component_source_modifier_target_name(mod_expr) else {
            continue;
        };

        let Some(nested_class) = find_nested_class_in_hierarchy(tree, target_class, &target_name)
        else {
            continue;
        };
        // `source_modifications` owns the keyword and source span; the
        // corresponding normalized modification owns resolved identities.
        let resolved_mod_expr = comp.modifications.get(&target_name).unwrap_or(mod_expr);
        if !is_redeclare {
            reject_unmarked_component_class_replacement(
                tree,
                &target_name,
                mod_expr,
                resolved_mod_expr,
            )?;
            continue;
        }
        let Some(alias_def_id) = nested_class.def_id else {
            return Err(Box::new(InstantiateError::redeclare_error(
                &target_name,
                "resolved redeclare target has no DefId",
                location_to_span(
                    &nested_class.location,
                    &tree.source_map,
                    "resolved component class redeclare target",
                )?,
            )));
        };
        if is_forwarding_component_redeclare(mod_expr, &target_name) {
            // The enclosing override is instance-local and is applied by
            // `resolve_component_nested_type_overrides`; validating the
            // lexical alias itself would compare the wrong class identity.
            continue;
        }
        let Some(def_id) = resolve_redeclare_value_def_id(tree, resolved_mod_expr, mod_env)
            .or_else(|| {
                class_redeclare_target_ref(resolved_mod_expr)
                    .and_then(|target| resolve_cref_def_id(&target))
            })
            .or_else(|| resolve_redeclare_value_def_id(tree, mod_expr, mod_env))
            .or_else(|| {
                class_redeclare_target_ref(mod_expr).and_then(|target| resolve_cref_def_id(&target))
            })
        else {
            return Err(Box::new(InstantiateError::redeclare_error(
                &target_name,
                "component redeclare value did not resolve to a class",
                mod_expr.span(),
            )));
        };
        validate_component_class_redeclare_target(
            tree,
            &target_name,
            nested_class,
            mod_expr,
            def_id,
        )?;
        let modifier_args = resolve_class_override_modifier_targets(
            tree,
            def_id,
            class_redeclare_modifier_args(mod_expr),
        )?;
        overrides.insert(
            alias_def_id,
            ast::ClassOverride::new(
                target_name,
                alias_def_id,
                def_id,
                class_redeclare_target_ref(mod_expr),
            )
            .with_modifier_args(modifier_args),
        );
    }

    Ok(overrides)
}
