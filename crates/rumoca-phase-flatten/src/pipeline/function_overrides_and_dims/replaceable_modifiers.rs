use super::*;

pub(super) fn append_replaceable_function_modifier_args(
    current_ref: &rumoca_core::Reference,
    selection: FunctionSelection,
    mut args: Vec<Expression>,
    ctx: &FunctionOverrideRewriteContext<'_>,
    span: rumoca_core::Span,
) -> Result<Vec<Expression>, FlattenError> {
    let existing_names = named_function_arg_names(&args);
    if let Some((override_target, receiver_scope)) =
        exact_override_function_target_and_receiver_scope(current_ref, selection, ctx, span)?
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
    let existing_names = named_function_arg_names(&args);
    let declaration_receiver_scope = exact_receiver_scope_for_function_modifier(current_ref, ctx);
    args.extend(
        replaceable_function_modifier_args(
            selection,
            current_ref,
            &declaration_receiver_scope,
            ctx,
            span,
        )?
        .into_iter()
        .filter(|(name, _, _)| !existing_names.contains(name))
        .map(|(name, value, span)| named_function_arg(&name, value, span)),
    );
    Ok(args)
}

fn replaceable_function_modifier_args(
    selection: FunctionSelection,
    current_ref: &rumoca_core::Reference,
    receiver_scope: &ComponentPath,
    ctx: &FunctionOverrideRewriteContext<'_>,
    span: rumoca_core::Span,
) -> Result<Vec<(String, Expression, rumoca_core::Span)>, FlattenError> {
    let class_def = ctx.class_index.get(selection.exposure).ok_or_else(|| {
        FlattenError::missing_function_selection_identity(
            current_ref.as_str(),
            "exposed function DefId is absent from the resolved class index",
            span,
        )
    })?;
    let mut result = Vec::new();
    for ext in &class_def.extends {
        let Some(base_def_id) = ext.base_def_id else {
            return Err(FlattenError::missing_function_selection_identity(
                current_ref.as_str(),
                "function base has no resolved DefId",
                span,
            ));
        };
        if base_def_id != selection.implementation {
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
    Ok(result)
}

fn exact_override_function_target_and_receiver_scope<'a>(
    current_ref: &rumoca_core::Reference,
    selection: FunctionSelection,
    ctx: &'a FunctionOverrideRewriteContext<'a>,
    span: rumoca_core::Span,
) -> Result<Option<(&'a OverrideTarget, ComponentPath)>, FlattenError> {
    let mut matches = ctx.override_functions.values().filter(|target| {
        target.class_type == rumoca_core::ClassType::Function
            && target.def_id == selection.implementation
            && !target.modifier_args.is_empty()
    });
    let target = matches.next();
    if matches.next().is_some() {
        return Err(FlattenError::missing_function_selection_identity(
            current_ref.as_str(),
            "selected function has multiple exact modifier owners",
            span,
        ));
    }
    Ok(target.map(|target| {
        (
            target,
            exact_receiver_scope_for_function_modifier(current_ref, ctx),
        )
    }))
}

fn exact_receiver_scope_for_function_modifier(
    current_ref: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> ComponentPath {
    let Some(scope) = current_ref.component_scope() else {
        return ctx.active_scope.clone();
    };
    let prefix_parts = scope.prefix_parts();
    let Some(prefix) = prefix_parts.last() else {
        return ctx.active_scope.clone();
    };
    if ctx.class_index.get(prefix.def_id).is_some() {
        return ctx.active_scope.clone();
    }
    ComponentPath::from_parts(prefix_parts.iter().map(|part| part.ident.as_str()))
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
    crate::ast_lower::expression_from_ast_with_intrinsics(
        &value,
        crate::ast_lower::PredefinedIntrinsicIds::from_tree(ctx.tree),
    )
    .expect("redeclare function modifier expression lowering failed")
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
        crate::ast_lower::expression_from_ast_with_intrinsics(
            &value,
            crate::ast_lower::PredefinedIntrinsicIds::from_tree(ctx.tree),
        )
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
