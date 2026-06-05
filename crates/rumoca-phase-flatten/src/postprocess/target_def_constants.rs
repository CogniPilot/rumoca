use super::*;

pub(super) fn substitute_target_def_scalar_var_ref(
    name: &rumoca_core::Reference,
    key: &str,
    span: rumoca_core::Span,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    locals: &HashSet<String>,
    scope: &str,
) -> Option<rumoca_core::Expression> {
    if reference_root_is_local(name, locals) {
        return None;
    }
    let target_name = reference_target_name(name, ctx)?;
    if target_name == key
        || target_def_targets_live_array(name, key, target_name.as_ref(), ctx, live_vars, scope)
    {
        return None;
    }

    let target_has_array_shape = reference_key_has_array_shape(target_name.as_ref(), ctx, scope);
    if live_vars.contains(target_name.as_ref()) {
        return Some(rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new(target_name.to_string()),
            subscripts: vec![],
            span,
        });
    }
    if live_vars.contains(key) && !target_name_is_well_known_package_constant(target_name.as_ref())
    {
        return None;
    }
    if let Some(value) = resolve_constant_value_expr(target_name.as_ref(), ctx) {
        if !target_has_array_shape {
            if let Some(literal) = eval_constant_expr_scalar_literal(value, span, ctx) {
                return Some(literal);
            }
            if let Some(literal) = resolved_constant_alias_scalar_literal(value, span, ctx) {
                return Some(literal);
            }
        }
        if target_has_array_shape && !constant_expr_preserves_array_shape(value) {
            return None;
        }
        return Some(substitute_resolved_constant_expr(
            target_name.as_ref(),
            value,
            span,
            ctx,
            live_vars,
            locals,
            scope,
        ));
    }
    if !target_has_array_shape
        && let Some(literal) =
            scalar_parameter_or_resolved_alias_literal(target_name.as_ref(), span, ctx)
    {
        return Some(literal);
    }
    if !target_has_array_shape
        && let Some(literal) = eval_constant_scalar_literal(target_name.as_ref(), span, ctx)
    {
        return Some(literal);
    }
    resolve_inline_indexed_constant(target_name.as_ref(), span, ctx)
}

fn target_name_is_well_known_package_constant(name: &str) -> bool {
    let path = rumoca_core::ComponentPath::from_flat_path(name);
    path.starts_with(&rumoca_core::ComponentPath::from_flat_path(
        "Modelica.Constants",
    )) || path.starts_with(&rumoca_core::ComponentPath::from_flat_path(
        "ModelicaServices.Machine",
    ))
}

fn scalar_parameter_or_resolved_alias_literal(
    name: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let expr = scalar_parameter_literal(name, span, ctx)?;
    Some(resolve_scalar_parameter_alias_expr(expr, span, ctx))
}

fn resolve_scalar_parameter_alias_expr(
    expr: rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &Context,
) -> rumoca_core::Expression {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => resolved_constant_alias_scalar_literal(
            &rumoca_core::Expression::VarRef {
                name: name.clone(),
                subscripts: vec![],
                span,
            },
            span,
            ctx,
        )
        .unwrap_or(rumoca_core::Expression::VarRef {
            name,
            subscripts: vec![],
            span,
        }),
        other => other,
    }
}

fn reference_target_name<'a>(
    name: &'a rumoca_core::Reference,
    ctx: &'a Context,
) -> Option<std::borrow::Cow<'a, str>> {
    let component_target = name.component_ref().map(|component_ref| {
        rumoca_core::ComponentPath::from_component_reference(component_ref).to_flat_string()
    });
    if let Some(component_target) = component_target.as_deref()
        && component_target != name.as_str()
        && target_name_is_well_known_package_constant(component_target)
    {
        return Some(std::borrow::Cow::Owned(component_target.to_string()));
    }
    if let Some(target_name) = name
        .target_def_id()
        .and_then(|def_id| ctx.target_def_names.get(&def_id))
        && target_name != name.as_str()
    {
        return Some(std::borrow::Cow::Borrowed(target_name.as_str()));
    }
    let target_name = component_target?;
    (target_name != name.as_str()).then_some(std::borrow::Cow::Owned(target_name))
}

pub(super) fn eval_constant_scalar_literal(
    name: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let expr = ctx.constant_values.get(name)?;
    eval_constant_expr_scalar_literal(expr, span, ctx)
}

pub(super) fn eval_constant_expr_scalar_literal(
    expr: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let value = rumoca_eval_flat::constant::eval_expr(expr, ctx.eval_fallback_context()).ok()?;
    match value {
        rumoca_eval_flat::constant::Value::Real(v) if v.is_finite() => {
            Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(v),
                span,
            })
        }
        rumoca_eval_flat::constant::Value::Integer(v) => Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(v),
            span,
        }),
        rumoca_eval_flat::constant::Value::Bool(v) => Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(v),
            span,
        }),
        rumoca_eval_flat::constant::Value::Enum(type_name, literal) => {
            Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(format!("{type_name}.{literal}")),
                subscripts: vec![],
                span,
            })
        }
        _ => None,
    }
}

pub(super) fn resolved_constant_alias_scalar_literal(
    expr: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    scalar_parameter_literal(name.as_str(), span, ctx)
        .or_else(|| eval_constant_scalar_literal(name.as_str(), span, ctx))
}

pub(super) fn well_known_package_constant_alias_scalar_literal(
    expr: &rumoca_core::Expression,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() || !target_name_is_well_known_package_constant(name.as_str()) {
        return None;
    }
    resolved_constant_alias_scalar_literal(expr, span, ctx)
}

pub(super) fn live_package_constant_alias_scalar_literal(
    key: &str,
    constant_value: Option<&rumoca_core::Expression>,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let value = ctx.constant_values.get(key).or(constant_value)?;
    well_known_package_constant_alias_scalar_literal(value, span, ctx)
}

pub(super) fn constant_expr_preserves_array_shape(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::Array { .. }
            | rumoca_core::Expression::Tuple { .. }
            | rumoca_core::Expression::Range { .. }
            | rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Fill
                    | rumoca_core::BuiltinFunction::Zeros
                    | rumoca_core::BuiltinFunction::Ones,
                ..
            }
    )
}

pub(super) fn reference_has_array_shape(
    name: &rumoca_core::Reference,
    key: &str,
    ctx: &Context,
    scope: &str,
) -> bool {
    reference_key_has_array_shape(key, ctx, scope)
        || name
            .target_def_id()
            .and_then(|def_id| ctx.target_def_names.get(&def_id))
            .is_some_and(|target_name| reference_key_has_array_shape(target_name, ctx, scope))
}

fn target_def_targets_live_array(
    name: &rumoca_core::Reference,
    key: &str,
    target_name: &str,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
    scope: &str,
) -> bool {
    if live_vars.contains(target_name) {
        return true;
    }
    let leaf = name.last_segment();
    if !live_vars.contains(leaf) || !key_targets_leaf(key, leaf) {
        return false;
    }
    reference_key_has_array_shape(target_name, ctx, scope)
        || reference_key_has_array_shape(leaf, ctx, scope)
}

fn key_targets_leaf(key: &str, leaf: &str) -> bool {
    key == leaf
        || key
            .strip_prefix(leaf)
            .is_some_and(|suffix| suffix.starts_with('['))
}
