//! MLS §7.3 validity checks for component-level class redeclarations.
//!
//! A component redeclare must target a replaceable, non-final nested class, the
//! replacement must be a subtype of the constraining type (MLS §7.3.2), and a
//! class replacement without the `redeclare` keyword is rejected instead of
//! being inferred from the modifier shape.

use super::redeclare_modifiers::class_redeclare_alias_ref;
use super::redeclare_values::resolve_redeclare_value_def_id;
use crate::{InstantiateError, InstantiateResult, location_to_span};
use rumoca_core::DefId;
use rumoca_ir_ast as ast;

/// Extract active class/package redeclare overrides from a component's modifiers.
///
pub(crate) fn validate_component_class_redeclare_target(
    tree: &ast::ClassTree,
    target_name: &str,
    nested_class: &ast::ClassDef,
    mod_expr: &ast::Expression,
    replacement_def_id: DefId,
) -> InstantiateResult<()> {
    let Some(target_ref) = class_redeclare_alias_ref(mod_expr) else {
        return Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            "redeclare target is missing source span",
            location_to_span(
                &nested_class.location,
                &tree.source_map,
                "component class redeclare target",
            )?,
        )));
    };
    let Some(part) = target_ref.parts.first() else {
        return Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            "redeclare target is missing source span",
            location_to_span(
                &nested_class.location,
                &tree.source_map,
                "component class redeclare target",
            )?,
        )));
    };
    let span = location_to_span(
        &part.ident.location,
        &tree.source_map,
        "component class redeclare name",
    )?;

    if nested_class.is_final {
        return Err(Box::new(InstantiateError::redeclare_final(
            target_name,
            span,
        )));
    }
    if !nested_class.is_replaceable {
        return Err(Box::new(InstantiateError::redeclare_non_replaceable(
            target_name,
            span,
        )));
    }

    validate_component_redeclare_constraint(
        tree,
        target_name,
        nested_class,
        replacement_def_id,
        span,
    )?;

    Ok(())
}

fn validate_component_redeclare_constraint(
    tree: &ast::ClassTree,
    target_name: &str,
    nested_class: &ast::ClassDef,
    replacement_def_id: DefId,
    span: rumoca_core::Span,
) -> InstantiateResult<()> {
    let Some(constraint_def_id) = component_redeclare_constraint_def_id(nested_class) else {
        return Err(Box::new(InstantiateError::redeclare_error(
            target_name,
            "resolved replaceable declaration has no constraining-type identity",
            span,
        )));
    };
    let replacement_name = tree
        .def_map
        .get(&replacement_def_id)
        .cloned()
        .or_else(|| {
            tree.get_class_by_def_id(replacement_def_id)
                .map(|class| class.name.text.to_string())
        })
        .ok_or_else(|| {
            Box::new(InstantiateError::redeclare_error(
                target_name,
                "resolved redeclare value has no class identity",
                span,
            ))
        })?;
    let constraint_name = tree
        .def_map
        .get(&constraint_def_id)
        .cloned()
        .or_else(|| {
            tree.get_class_by_def_id(constraint_def_id)
                .map(|class| class.name.text.to_string())
        })
        .ok_or_else(|| {
            Box::new(InstantiateError::redeclare_error(
                target_name,
                "resolved constraining type has no class identity",
                span,
            ))
        })?;

    if !crate::inheritance::is_type_subtype(tree, &replacement_name, &constraint_name) {
        return Err(Box::new(InstantiateError::redeclare_constraint_violation(
            target_name,
            &replacement_name,
            &constraint_name,
            span,
        )));
    }

    Ok(())
}

fn component_redeclare_constraint_def_id(nested_class: &ast::ClassDef) -> Option<DefId> {
    nested_class
        .constrainedby
        .as_ref()
        .and_then(|constraint| constraint.def_id)
        .or_else(|| {
            nested_class
                .extends
                .first()
                .and_then(|extend| extend.base_def_id.or(extend.base_name.def_id))
        })
        .or(nested_class.def_id)
}

pub(super) fn validate_component_source_modifier_metadata(
    tree: &ast::ClassTree,
    comp: &ast::Component,
) -> InstantiateResult<()> {
    if comp.source_modifications.len() == comp.source_modification_redeclare_flags.len() {
        return Ok(());
    }
    Err(Box::new(InstantiateError::redeclare_error(
        &comp.name,
        "component source modifiers lost their redeclare metadata",
        location_to_span(
            &comp.location,
            &tree.source_map,
            "component source modifier metadata",
        )?,
    )))
}

pub(super) fn reject_unmarked_component_class_replacement(
    tree: &ast::ClassTree,
    target_name: &str,
    source_mod_expr: &ast::Expression,
    resolved_mod_expr: &ast::Expression,
) -> InstantiateResult<()> {
    if !matches!(source_mod_expr, ast::Expression::Modification { .. }) {
        return Ok(());
    }
    if resolve_redeclare_value_def_id(tree, resolved_mod_expr, None).is_none() {
        return Ok(());
    }
    let span = class_redeclare_alias_ref(source_mod_expr)
        .expect("source modifier target was validated before replacement checking")
        .span;
    Err(Box::new(InstantiateError::redeclare_error(
        target_name,
        "changing a class or package requires the `redeclare` keyword",
        span,
    )))
}
