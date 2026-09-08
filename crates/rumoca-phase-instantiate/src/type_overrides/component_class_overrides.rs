//! Extraction of component-level class overrides from resolved modifiers.
//!
//! Persisting the selections proved here lets downstream phases evaluate
//! instance-scoped constants of a redeclared class or package.

use super::class_hierarchy::{
    contains_component_by_def_id_in_hierarchy, find_nested_class_by_def_id_in_hierarchy,
    find_nested_class_in_hierarchy,
};
use super::component_redeclare_validation::{
    reject_unmarked_component_class_replacement, validate_component_class_redeclare_target,
    validate_component_source_modifier_metadata,
};
use super::redeclare_modifiers::{
    class_redeclare_alias_ref, class_redeclare_modifier_args, class_redeclare_target_ref,
    component_source_modifier_target_name,
};
use super::redeclare_values::{resolve_cref_def_id, resolve_redeclare_value_def_id};
use super::selected_class_members::resolve_class_override_modifier_targets;
use super::{
    SourceForwardingEvidence, SourceForwardingEvidenceError, TypeOverrideMap,
    checked_source_forwarding_witness,
};
use crate::{InstantiateError, InstantiateResult};
use rumoca_ir_ast as ast;
use rumoca_ir_ast::AstIndexMap as IndexMap;

struct ComponentClassOverrideContext<'a> {
    tree: &'a ast::ClassTree,
    comp: &'a ast::Component,
    target_class: &'a ast::ClassDef,
    mod_env: Option<&'a ast::ModificationEnvironment>,
    type_overrides: &'a TypeOverrideMap,
}

/// MLS §7.3: component-level redeclare modifiers can target replaceable nested
/// classes declared in base classes (via extends). Persisting these resolved
/// overrides enables downstream phases to evaluate instance-scoped constants.
pub(crate) fn extract_component_class_overrides(
    tree: &ast::ClassTree,
    comp: &ast::Component,
    target_class: Option<&ast::ClassDef>,
    mod_env: Option<&ast::ModificationEnvironment>,
    type_overrides: &TypeOverrideMap,
) -> InstantiateResult<ast::ClassOverrideMap> {
    let mut overrides = IndexMap::default();
    let Some(target_class) = target_class else {
        return Ok(overrides);
    };

    validate_component_source_modifier_metadata(tree, comp)?;
    let context = ComponentClassOverrideContext {
        tree,
        comp,
        target_class,
        mod_env,
        type_overrides,
    };
    for (mod_expr, is_redeclare) in comp
        .source_modifications
        .iter()
        .zip(&comp.source_modification_redeclare_flags)
    {
        let Some((alias_def_id, class_override)) =
            extract_one_component_class_override(&context, mod_expr, *is_redeclare)?
        else {
            continue;
        };
        overrides.insert(alias_def_id, class_override);
    }

    Ok(overrides)
}

fn extract_one_component_class_override(
    context: &ComponentClassOverrideContext<'_>,
    mod_expr: &ast::Expression,
    is_redeclare: bool,
) -> InstantiateResult<Option<(rumoca_core::DefId, ast::ClassOverride)>> {
    let Some(target_name) = component_source_modifier_target_name(mod_expr) else {
        return Ok(None);
    };
    // `source_modifications` owns the keyword, source span, and exact LHS
    // identity; the normalized modification supplies the resolved RHS.
    let resolved_mod_expr = match context.comp.modifications.get(&target_name) {
        Some(resolved) => resolved,
        None if !is_redeclare => mod_expr,
        None => {
            return Err(Box::new(InstantiateError::redeclare_error(
                &target_name,
                "direct redeclare has no normalized semantic modifier",
                mod_expr.span(),
            )));
        }
    };
    if !is_redeclare {
        if find_nested_class_in_hierarchy(context.tree, context.target_class, &target_name)?
            .is_none()
        {
            return Ok(None);
        }
        reject_unmarked_component_class_replacement(
            context.tree,
            &target_name,
            mod_expr,
            resolved_mod_expr,
        )?;
        return Ok(None);
    }
    let alias_def_id = class_redeclare_alias_ref(mod_expr)
        .and_then(ast::ComponentReference::target_def_id)
        .ok_or_else(|| {
            Box::new(InstantiateError::redeclare_error(
                &target_name,
                "direct redeclare LHS has no Resolve-issued receiver-slot identity",
                mod_expr.span(),
            ))
        })?;
    let Some(nested_class) =
        find_nested_class_by_def_id_in_hierarchy(context.tree, context.target_class, alias_def_id)?
    else {
        if contains_component_by_def_id_in_hierarchy(
            context.tree,
            context.target_class,
            alias_def_id,
        )? {
            return Ok(None);
        }
        return Err(Box::new(InstantiateError::redeclare_error(
            &target_name,
            format!(
                "Resolve-issued LHS {alias_def_id:?} is not an exact direct or inherited receiver slot"
            ),
            mod_expr.span(),
        )));
    };
    if is_instance_local_forwarding(
        context,
        mod_expr,
        resolved_mod_expr,
        &target_name,
        alias_def_id,
    )? {
        return Ok(None);
    }
    let def_id = resolve_component_redeclare_identity(
        context.tree,
        mod_expr,
        resolved_mod_expr,
        context.mod_env,
        &target_name,
    )?;
    validate_component_class_redeclare_target(
        context.tree,
        &target_name,
        nested_class,
        mod_expr,
        def_id,
    )?;
    let modifier_args = resolve_class_override_modifier_targets(
        context.tree,
        def_id,
        class_redeclare_modifier_args(mod_expr),
    )?;
    let class_override = ast::ClassOverride::new(
        target_name,
        alias_def_id,
        def_id,
        class_redeclare_target_ref(mod_expr),
    )
    .with_modifier_args(modifier_args);
    Ok(Some((alias_def_id, class_override)))
}

fn is_instance_local_forwarding(
    context: &ComponentClassOverrideContext<'_>,
    source: &ast::Expression,
    resolved: &ast::Expression,
    target_name: &str,
    alias_def_id: rumoca_core::DefId,
) -> InstantiateResult<bool> {
    checked_source_forwarding_witness(SourceForwardingEvidence {
        tree: context.tree,
        type_overrides: context.type_overrides,
        is_redeclare: true,
        source,
        resolved,
        target_name,
        alias_def_id,
    })
    .map_err(|reason| match reason {
        SourceForwardingEvidenceError::Resolution(error) => error,
        other => Box::new(InstantiateError::redeclare_error(
            target_name,
            other.to_string(),
            source.span(),
        )),
    })
    .map(|witness| witness.is_some())
}

fn resolve_component_redeclare_identity(
    tree: &ast::ClassTree,
    source: &ast::Expression,
    resolved: &ast::Expression,
    mod_env: Option<&ast::ModificationEnvironment>,
    target_name: &str,
) -> InstantiateResult<rumoca_core::DefId> {
    let resolved_identity = resolve_redeclare_value_def_id(tree, resolved, mod_env)?;
    let resolved_target_identity =
        class_redeclare_target_ref(resolved).and_then(|target| resolve_cref_def_id(&target));
    let source_identity = if resolved_identity.is_none() && resolved_target_identity.is_none() {
        resolve_redeclare_value_def_id(tree, source, mod_env)?
    } else {
        None
    };
    let source_target_identity =
        class_redeclare_target_ref(source).and_then(|target| resolve_cref_def_id(&target));
    resolved_identity
        .or(resolved_target_identity)
        .or(source_identity)
        .or(source_target_identity)
        .ok_or_else(|| {
            Box::new(InstantiateError::redeclare_error(
                target_name,
                "component redeclare value did not resolve to a class",
                source.span(),
            ))
        })
}
