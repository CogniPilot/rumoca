use super::*;

pub(super) fn selected_user_function_name<'a>(
    function: &rumoca_ir_ast::ComponentReference,
    rendered: &str,
    ctx: &'a TypeCheckEvalContext,
) -> Option<&'a str> {
    if let Some(target) = function.target_def_id() {
        return ctx
            .functions
            .iter()
            .find(|(_, definition)| definition.def_id == Some(target))
            .map(|(name, _)| name.as_str());
    }
    // An identity-free call reference selects a user function only in the
    // delimited pre-identity structural category; a post-Resolve environment
    // refuses it (SPEC_0036), so a spelling can never become call authority.
    match ctx.call_identity_policy() {
        CallIdentityPolicy::PreIdentityStructural => ctx
            .functions
            .get_key_value(rendered)
            .map(|(name, _)| name.as_str()),
        CallIdentityPolicy::RequireResolvedIdentity => None,
    }
}

pub(super) fn call_targets_predefined(
    function: &rumoca_ir_ast::ComponentReference,
    rendered: &str,
    ctx: &TypeCheckEvalContext,
) -> bool {
    let Some(short_name) = function.parts.last().map(|part| part.ident.text.as_ref()) else {
        return false;
    };
    if rendered != short_name || selected_user_function_name(function, rendered, ctx).is_some() {
        return false;
    }
    match function.target_def_id() {
        Some(target) => ctx.predefined_functions.get(short_name) == Some(&target),
        // Builtin selection by bare spelling is likewise confined to the
        // pre-identity structural category.
        None => matches!(
            ctx.call_identity_policy(),
            CallIdentityPolicy::PreIdentityStructural
        ),
    }
}
