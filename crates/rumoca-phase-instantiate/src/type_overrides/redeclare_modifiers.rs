//! Structural reads over redeclare modifier expressions.
//!
//! Modifiers keep their source shape through instantiation, so the alias being
//! redeclared, the replacement reference, and the replacement's own modifier
//! arguments are recovered by matching that shape.

use rumoca_ir_ast as ast;

pub(crate) fn component_source_modifier_target_name(mod_expr: &ast::Expression) -> Option<String> {
    class_redeclare_alias_ref(mod_expr)?
        .parts
        .first()
        .map(|part| part.ident.text.to_string())
}

pub(super) fn class_redeclare_alias_ref(
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
) -> Option<&ast::ComponentReference> {
    match mod_expr {
        ast::Expression::Modification { value, .. } => class_redeclare_target_ref(value),
        ast::Expression::ClassModification { target, .. } => Some(target),
        ast::Expression::FunctionCall { comp, .. } => Some(comp),
        ast::Expression::ComponentReference(cref) => Some(cref),
        _ => None,
    }
}

pub(crate) fn class_redeclare_modifier_args(mod_expr: &ast::Expression) -> Vec<ast::Expression> {
    match mod_expr {
        ast::Expression::Modification { value, .. } => class_redeclare_modifier_args(value),
        ast::Expression::ClassModification { modifications, .. } => modifications.clone(),
        ast::Expression::FunctionCall { args, .. } => args.clone(),
        _ => Vec::new(),
    }
}
