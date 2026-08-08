//! Class identity proofs for redeclare values.
//!
//! A redeclare value arrives as a modification, class-modification, function
//! call, or plain reference expression, optionally forwarded through the
//! modification environment. Each shape is unwrapped until the exact class
//! declaration identity proved by Resolve is reached.

use rumoca_core::DefId;
use rumoca_ir_ast as ast;

pub(crate) fn resolve_redeclare_value_def_id(
    tree: &ast::ClassTree,
    value: &ast::Expression,
    mod_env: Option<&ast::ModificationEnvironment>,
) -> Option<DefId> {
    resolve_redeclare_value_def_id_with_depth(tree, value, mod_env, 0)
}

fn resolve_redeclare_value_def_id_with_depth(
    tree: &ast::ClassTree,
    value: &ast::Expression,
    mod_env: Option<&ast::ModificationEnvironment>,
    depth: usize,
) -> Option<DefId> {
    const MAX_REDECLARE_RESOLVE_DEPTH: usize = 8;
    if depth > MAX_REDECLARE_RESOLVE_DEPTH {
        return None;
    }

    match value {
        ast::Expression::Modification { value, .. } => {
            resolve_redeclare_value_def_id_with_depth(tree, value, mod_env, depth + 1)
        }
        ast::Expression::ClassModification { target, .. } => resolve_cref_def_id(target)
            .or_else(|| resolve_cref_via_mod_env(tree, target, mod_env, depth)),
        ast::Expression::FunctionCall { comp, .. } => resolve_cref_def_id(comp)
            .or_else(|| resolve_cref_via_mod_env(tree, comp, mod_env, depth)),
        ast::Expression::ComponentReference(cref) => resolve_cref_def_id(cref)
            .or_else(|| resolve_cref_via_mod_env(tree, cref, mod_env, depth)),
        _ => None,
    }
}

fn resolve_cref_via_mod_env(
    tree: &ast::ClassTree,
    cref: &ast::ComponentReference,
    mod_env: Option<&ast::ModificationEnvironment>,
    depth: usize,
) -> Option<DefId> {
    let mod_env = mod_env?;
    let qn = cref_to_qualified_name(cref)?;
    let mod_value = mod_env.get(&qn)?;
    if mod_value.value == ast::Expression::ComponentReference(cref.clone()) {
        return None;
    }
    resolve_redeclare_value_def_id_with_depth(tree, &mod_value.value, Some(mod_env), depth + 1)
}

pub(super) fn cref_to_qualified_name(cref: &ast::ComponentReference) -> Option<ast::QualifiedName> {
    let mut parts = cref.parts.iter();
    let first = parts.next()?;
    let mut qn = ast::QualifiedName::from_ident(first.ident.text.as_ref());
    for part in parts {
        qn = qn.child(part.ident.text.as_ref());
    }
    Some(qn)
}

pub(super) fn resolve_cref_def_id(cref: &ast::ComponentReference) -> Option<DefId> {
    // Resolve has already proved class-reference identity. Prefer its exact
    // final target because a one-token reference may itself contain a dotted
    // class path. Only a truly one-part reference can use its root identity.
    if let Some(target_def_id) = cref.target_def_id() {
        return Some(target_def_id);
    }
    (cref.parts.len() == 1)
        .then_some(cref.root_def_id())
        .flatten()
}
