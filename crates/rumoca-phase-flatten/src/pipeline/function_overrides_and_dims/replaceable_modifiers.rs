use super::*;

pub(super) fn append_replaceable_function_modifier_args(
    current_ref: &rumoca_core::Reference,
    resolved_name: &str,
    mut args: Vec<Expression>,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Vec<Expression> {
    let receiver_alias = receiver_alias_for_member_function(current_ref, ctx);
    let existing_names = named_function_arg_names(&args);
    let declaration_receiver_scope = receiver_alias
        .as_deref()
        .map(ComponentPath::from_flat_path)
        .unwrap_or_else(|| ctx.active_scope.clone());
    if let Some(default_args) = replaceable_function_modifier_args(
        current_ref.as_str(),
        resolved_name,
        &declaration_receiver_scope,
        ctx,
    ) {
        args.extend(
            default_args
                .into_iter()
                .filter(|(name, _, _)| !existing_names.contains(name))
                .map(|(name, value, span)| named_function_arg(&name, value, span)),
        );
    }
    let existing_names = named_function_arg_names(&args);
    if let Some((override_target, receiver_scope)) =
        override_function_target_and_receiver_scope(current_ref, ctx)
    {
        args.extend(
            override_target
                .modifier_args
                .iter()
                .filter(|arg| !existing_names.contains(arg.name.as_str()))
                .map(|arg| {
                    named_function_arg(
                        &arg.name,
                        qualify_redeclare_function_arg(&arg.value, &receiver_scope, ctx),
                        arg.span,
                    )
                }),
        );
    }
    args
}

fn override_function_target_and_receiver_scope<'a>(
    current_ref: &rumoca_core::Reference,
    ctx: &'a FunctionOverrideRewriteContext<'a>,
) -> Option<(&'a OverrideTarget, ComponentPath)> {
    let alias = current_ref.last_segment();
    if let Some(target) = ctx.override_functions.get(alias) {
        return Some((
            target,
            receiver_scope_for_function_modifier(current_ref, ctx),
        ));
    }
    None
}

fn receiver_scope_for_function_modifier(
    current_ref: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> ComponentPath {
    if let Some(scope) = current_ref.component_scope() {
        let prefix_parts = scope.prefix_parts();
        if !prefix_parts.is_empty() {
            let receiver_scope =
                ComponentPath::from_parts(prefix_parts.iter().map(|part| part.ident.as_str()));
            if ctx
                .class_index
                .get_by_qualified_name(receiver_scope.as_str())
                .is_some()
            {
                return ctx.active_scope.clone();
            }
            return receiver_scope;
        }
    }
    ctx.active_scope.clone()
}

fn qualify_redeclare_function_arg(
    value: &rumoca_ir_ast::Expression,
    receiver_scope: &ComponentPath,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Expression {
    let value = QualifyReplaceableFunctionModifier {
        receiver_alias: receiver_scope,
    }
    .transform_expression(value.clone());
    crate::ast_lower::expression_from_ast_with_def_map(&value, Some(&ctx.tree.def_map))
        .expect("redeclare function modifier expression lowering failed")
}

fn receiver_alias_for_member_function(
    current_ref: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<String> {
    if let Some(alias) = current_ref
        .component_scope()
        .and_then(rumoca_core::ComponentReferenceScope::parent_ident)
        .filter(|alias| ctx.override_functions.contains_key(*alias))
    {
        return Some(alias.to_string());
    }

    let current_name = current_ref.as_str();
    let leaf = current_ref.last_segment();
    let mut matches = ctx
        .override_functions
        .iter()
        .filter_map(|(alias, receiver_type)| {
            let function_name =
                resolve_function_in_package_chain(ctx.tree, ctx.class_index, receiver_type, leaf)?;
            (function_name == current_name).then(|| alias.clone())
        });
    let first = matches.next()?;
    matches.next().is_none().then_some(first)
}

fn replaceable_function_modifier_args(
    current_name: &str,
    resolved_name: &str,
    receiver_scope: &ComponentPath,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<Vec<(String, Expression, rumoca_core::Span)>> {
    let class_def = ctx.class_index.get_by_qualified_name(current_name)?;
    let mut result = Vec::new();
    for ext in &class_def.extends {
        let base_name = ext.base_name.to_string();
        let resolved_base = ext
            .base_def_id
            .and_then(|def_id| ctx.tree.def_map.get(&def_id).cloned())
            .or_else(|| resolve_class_in_scope_indexed(ctx.class_index, &base_name, current_name).1)
            .or_else(|| {
                ctx.class_index
                    .get_by_qualified_name(&base_name)
                    .map(|_| base_name.clone())
            });
        if resolved_base.as_deref() != Some(resolved_name) {
            continue;
        }
        for modifier in &ext.modifications {
            if let Some(arg) =
                replaceable_function_modifier_arg(&modifier.expr, receiver_scope, ctx)
            {
                result.push(arg);
            }
        }
    }
    (!result.is_empty()).then_some(result)
}

fn replaceable_function_modifier_arg(
    expr: &rumoca_ir_ast::Expression,
    receiver_scope: &ComponentPath,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> Option<(String, Expression, rumoca_core::Span)> {
    let (name, value) = match expr {
        rumoca_ir_ast::Expression::NamedArgument { name, value, .. } => {
            (name.text.to_string(), value.as_ref().clone())
        }
        rumoca_ir_ast::Expression::Modification { target, value, .. } => {
            (single_component_ref_name(target)?, value.as_ref().clone())
        }
        _ => return None,
    };
    let value = QualifyReplaceableFunctionModifier {
        receiver_alias: receiver_scope,
    }
    .transform_expression(value);
    Some((
        name,
        crate::ast_lower::expression_from_ast_with_def_map(&value, Some(&ctx.tree.def_map))
            .expect("replaceable function modifier expression lowering failed"),
        expr.span(),
    ))
}

pub(super) fn single_component_ref_name(
    comp: &rumoca_ir_ast::ComponentReference,
) -> Option<String> {
    let [part] = comp.parts.as_slice() else {
        return None;
    };
    Some(part.ident.text.to_string())
}
