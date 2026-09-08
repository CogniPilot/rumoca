//! Structural reads over redeclare modifier expressions.
//!
//! Modifiers keep their source shape through instantiation, so the alias being
//! redeclared, the replacement reference, and the replacement's own modifier
//! arguments are recovered by matching that shape.

use rumoca_ir_ast as ast;

pub(crate) fn is_forwarding_component_redeclare(
    mod_expr: &ast::Expression,
    target_name: &str,
) -> bool {
    let Some(target) = class_redeclare_target_ref(mod_expr) else {
        return false;
    };
    let [part] = target.parts.as_slice() else {
        return false;
    };
    part.subs.is_none() && part.ident.text.as_ref() == target_name
}

pub(crate) fn component_source_modifier_target_name(mod_expr: &ast::Expression) -> Option<String> {
    class_redeclare_alias_ref(mod_expr)?
        .parts
        .first()
        .map(|part| part.ident.text.to_string())
}

/// The exact source modifier that directly redeclares `target_name`.
///
/// This is a non-authoritative shape probe: Resolve owns the LHS identity and
/// Instantiate callers must reject a recognized redeclare whose returned
/// reference lacks that identity.
pub(crate) fn direct_source_redeclare<'a>(
    comp: &'a ast::Component,
    target_name: &str,
) -> Option<&'a ast::Expression> {
    comp.source_modifications
        .iter()
        .zip(&comp.source_modification_redeclare_flags)
        .find_map(|(source, is_redeclare)| {
            (*is_redeclare
                && component_source_modifier_target_name(source).as_deref() == Some(target_name))
            .then_some(source)
        })
}

pub(crate) fn class_redeclare_alias_ref(
    mod_expr: &ast::Expression,
) -> Option<&ast::ComponentReference> {
    match mod_expr {
        ast::Expression::Modification { target, .. }
        | ast::Expression::ClassModification { target, .. } => Some(target),
        _ => None,
    }
}

pub(super) fn class_redeclare_target_ref(
    mod_expr: &ast::Expression,
) -> Option<ast::ComponentReference> {
    match mod_expr {
        ast::Expression::Modification { target, value, .. } => value
            .as_deref()
            .and_then(class_redeclare_target_ref)
            .or_else(|| Some(target.clone())),
        ast::Expression::ClassModification { target, .. } => Some(target.clone()),
        ast::Expression::FunctionCall { comp, .. } => Some(comp.clone()),
        ast::Expression::ComponentReference(cref) => Some(cref.clone()),
        _ => None,
    }
}

pub(crate) fn class_redeclare_modifier_args(mod_expr: &ast::Expression) -> Vec<ast::Expression> {
    match mod_expr {
        ast::Expression::Modification {
            value: Some(value), ..
        } => class_redeclare_modifier_args(value),
        ast::Expression::ClassModification { modifications, .. } => modifications.clone(),
        ast::Expression::FunctionCall { args, .. } => args.clone(),
        _ => Vec::new(),
    }
}
