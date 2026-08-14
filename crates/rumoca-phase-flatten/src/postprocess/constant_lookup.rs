//! Look up the value of a `constant`/`parameter` declaration recorded on the
//! flatten context.
//!
//! A reference reaches this module either with an exact declaration identity
//! (`DefId`, optionally narrowed to a constant occurrence) or as a generated
//! flat key that must be resolved through the enclosing scopes, constant
//! aliases, inline subscripts, and record field projections. Every lookup here
//! is a pure function of the context: deciding *whether* a reference may be
//! folded is the substituter's job.

use super::constant_expansion::SemanticConstantId;
use super::*;

pub(super) fn resolve_indexed_constant_field_access(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    field: &str,
    span: rumoca_core::Span,
    ctx: &Context,
    live_vars: &rustc_hash::FxHashSet<String>,
) -> Option<rumoca_core::Expression> {
    if let rumoca_core::Expression::VarRef { name, .. } = base
        && live_vars.contains(name.as_str())
    {
        return None;
    }
    let selected = select_constant_index(base, subscripts, span, ctx)?;
    resolve_field_on_constant_expr(&selected, field, span, ctx)
}

fn select_constant_index(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    if subscripts.is_empty() {
        return Some(base.clone().with_span(span));
    }

    let base = resolve_constant_expr_alias(base, ctx)?;
    let (first, rest) = subscripts.split_first()?;
    match first {
        rumoca_core::Subscript::Index { value, .. } => {
            select_constant_index_element(&base, *value, rest, span, ctx)
        }
        rumoca_core::Subscript::Expr { expr, .. } => {
            let index = literal_integer(expr)?;
            select_constant_index_element(&base, index, rest, span, ctx)
        }
        rumoca_core::Subscript::Colon { .. } => {
            let rumoca_core::Expression::Array { elements, .. } = base else {
                return None;
            };
            let projected = elements
                .iter()
                .map(|element| select_constant_index(element, rest, span, ctx))
                .collect::<Option<Vec<_>>>()?;
            Some(rumoca_core::Expression::Array {
                elements: projected,
                is_matrix: false,
                span,
            })
        }
    }
}

fn select_constant_index_element(
    base: &rumoca_core::Expression,
    index: i64,
    rest: &[rumoca_core::Subscript],
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::Array { elements, .. } = base else {
        return None;
    };
    let zero_based = usize::try_from(index.checked_sub(1)?).ok()?;
    let element = elements.get(zero_based)?;
    select_constant_index(element, rest, span, ctx)
}

fn resolve_constant_expr_alias(
    expr: &rumoca_core::Expression,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => resolve_constant_value_expr_for_ref(name, ctx).cloned(),
        other => Some(other.clone()),
    }
}

fn resolve_field_on_constant_expr(
    expr: &rumoca_core::Expression,
    field: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    match expr {
        rumoca_core::Expression::Array { elements, .. } => {
            let projected = elements
                .iter()
                .map(|element| resolve_field_on_constant_expr(element, field, span, ctx))
                .collect::<Option<Vec<_>>>()?;
            Some(rumoca_core::Expression::Array {
                elements: projected,
                is_matrix: false,
                span,
            })
        }
        rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            ..
        } => named_constructor_arg(args, field)
            .cloned()
            .map(|expr| expr.with_span(span))
            .or_else(|| {
                args.is_empty()
                    .then(|| resolve_constant_field_access(name.as_str(), field, span, ctx))
                    .flatten()
            }),
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() && name.is_generated() => {
            resolve_constant_field_access(name.as_str(), field, span, ctx)
        }
        _ => None,
    }
}

fn literal_integer(expr: &rumoca_core::Expression) -> Option<i64> {
    match expr {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some(*value),
        _ => None,
    }
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

pub(super) fn scalar_parameter_literal(
    key: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    if let Some(v) = ctx.real_parameter_values.get(key)
        && v.is_finite()
    {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(*v),
            span,
        });
    }
    if let Some(v) = ctx.parameter_values.get(key) {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(*v),
            span,
        });
    }
    if let Some(v) = ctx.boolean_parameter_values.get(key) {
        return Some(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Boolean(*v),
            span,
        });
    }
    ctx.enum_parameter_values
        .get(key)
        .map(|v| rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::generated(v.clone()),
            subscripts: vec![],
            span,
        })
}

pub(super) fn reference_key_has_array_shape(key: &str, ctx: &Context, scope: &str) -> bool {
    ctx.array_dimensions
        .get(key)
        .is_some_and(|dims| !dims.is_empty())
        || scoped_lookup_candidates(key, scope)
            .into_iter()
            .any(|candidate| {
                ctx.array_dimensions
                    .get(&candidate)
                    .is_some_and(|dims| !dims.is_empty())
            })
}

pub(super) fn resolve_projected_constant_path(
    name: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let path = rumoca_core::ComponentPath::from_flat_path(name);
    let parts = path.parts();
    if parts.len() < 2 {
        return None;
    }

    for split in (1..parts.len()).rev() {
        let prefix = path.prefix(split)?.to_flat_string();
        let Some(mut expr) = resolve_constant_value_expr(&prefix, ctx).cloned() else {
            continue;
        };
        let mut resolved = true;
        for field in &parts[split..] {
            let Some(field_expr) = resolve_field_on_constant_expr(&expr, field, span, ctx) else {
                resolved = false;
                break;
            };
            expr = field_expr;
        }
        if resolved {
            return Some(expr.with_span(span));
        }
    }

    None
}

pub(super) fn resolve_constant_value_expr<'a>(
    name: &str,
    ctx: &'a Context,
) -> Option<&'a rumoca_core::Expression> {
    let mut current = name.to_string();
    let mut visited = rustc_hash::FxHashSet::default();
    loop {
        if !visited.insert(current.clone()) {
            return None;
        }
        let expr = ctx.constant_values.get(&current)?;
        let rumoca_core::Expression::VarRef {
            name: alias_name,
            subscripts,
            ..
        } = expr
        else {
            return Some(expr);
        };
        if !subscripts.is_empty() || alias_name.as_str() == current {
            return Some(expr);
        }
        let alias_scope = parent_component_scope(&current);
        let Some(resolved_key) =
            resolve_constant_key_with_scope(alias_name.as_str(), &alias_scope, ctx)
        else {
            return Some(expr);
        };
        if resolved_key == current {
            return Some(expr);
        }
        current = resolved_key;
    }
}

pub(super) fn resolve_constant_value_expr_for_ref<'a>(
    name: &rumoca_core::Reference,
    ctx: &'a Context,
) -> Option<&'a rumoca_core::Expression> {
    if !name.is_generated() {
        return resolve_source_constant(name, ctx).map(|(_, value)| value);
    }
    resolve_constant_value_expr(name.as_str(), ctx)
}

pub(super) fn resolve_source_constant<'a>(
    name: &rumoca_core::Reference,
    ctx: &'a Context,
) -> Option<(SemanticConstantId, &'a rumoca_core::Expression)> {
    let declaration = name.target_def_id()?;
    let occurrence = name
        .instance_id()
        .and_then(|class_instance| ctx.constant_owner_for_class(class_instance))
        .map(|owner| crate::ConstantOccurrenceId::new(owner, declaration));
    if let Some(occurrence) = occurrence
        && let Some(value) = ctx.constant_values_by_occurrence.get(&occurrence)
    {
        return Some((SemanticConstantId::Occurrence(occurrence), value));
    }
    ctx.constant_values_by_def_id
        .get(&declaration)
        .map(|value| (SemanticConstantId::Declaration(declaration), value))
}

pub(super) fn generated_constant_candidate_exists(name: &str, ctx: &Context, scope: &str) -> bool {
    let direct_or_scoped = std::iter::once(name.to_string())
        .chain(scoped_lookup_candidates(name, scope))
        .any(|candidate| constant_key_or_prefix_exists(&candidate, ctx));
    if direct_or_scoped {
        return true;
    }
    split_inline_indexed_name(name)
        .is_some_and(|(base, _)| constant_key_or_prefix_exists(base, ctx))
        || resolve_varref_through_constant_aliases(name, ctx, scope).is_some()
}

fn resolve_constant_key_with_scope(name: &str, scope: &str, ctx: &Context) -> Option<String> {
    scoped_lookup_candidates_with_scope(name, scope)
        .into_iter()
        .map(|(candidate, _candidate_scope)| candidate)
        .find(|candidate| ctx.constant_values.contains_key(candidate))
}

pub(super) fn resolve_varref_through_constant_aliases(
    name: &str,
    ctx: &Context,
    scope: &str,
) -> Option<String> {
    let mut current = name.to_string();
    let mut visited = rustc_hash::FxHashSet::default();
    loop {
        if !visited.insert(current.clone()) {
            return None;
        }

        let mut replaced = false;
        for (idx, ch) in current.char_indices().rev() {
            if ch != '.' {
                continue;
            }
            let prefix = &current[..idx];
            let suffix = &current[idx..];
            let alias_key = if ctx.constant_values.contains_key(prefix) {
                Some(prefix.to_string())
            } else {
                resolve_constant_key_with_scope(prefix, scope, ctx)
            };
            let Some(alias_key) = alias_key else {
                continue;
            };
            let Some(alias_expr) = ctx.constant_values.get(&alias_key) else {
                continue;
            };
            let rumoca_core::Expression::VarRef {
                name: alias_name,
                subscripts,
                ..
            } = alias_expr
            else {
                continue;
            };
            if !subscripts.is_empty() {
                continue;
            }
            let alias_scope = parent_component_scope(&alias_key);
            let alias_target =
                resolve_alias_target_with_scope(alias_name.as_str(), &alias_scope, ctx)
                    .or_else(|| resolve_alias_target_with_scope(alias_name.as_str(), scope, ctx))
                    .unwrap_or_else(|| alias_name.as_str().to_string());
            current = format!("{alias_target}{suffix}");
            replaced = true;
            break;
        }

        if !replaced {
            return if current == name { None } else { Some(current) };
        }
    }
}

fn resolve_alias_target_with_scope(name: &str, scope: &str, ctx: &Context) -> Option<String> {
    scoped_lookup_candidates_with_scope(name, scope)
        .into_iter()
        .map(|(candidate, _candidate_scope)| candidate)
        .find(|candidate| constant_key_or_prefix_exists(candidate, ctx))
}

fn constant_key_or_prefix_exists(name: &str, ctx: &Context) -> bool {
    ctx.constant_values.contains_key(name)
        || ctx.real_parameter_values.contains_key(name)
        || ctx.parameter_values.contains_key(name)
        || ctx.boolean_parameter_values.contains_key(name)
        || ctx.enum_parameter_values.contains_key(name)
        || map_has_key_prefix(&ctx.constant_values, name)
        || map_has_key_prefix(&ctx.real_parameter_values, name)
        || map_has_key_prefix(&ctx.parameter_values, name)
        || map_has_key_prefix(&ctx.boolean_parameter_values, name)
        || map_has_key_prefix(&ctx.enum_parameter_values, name)
}

fn map_has_key_prefix<T>(map: &rustc_hash::FxHashMap<String, T>, prefix: &str) -> bool {
    map.keys().any(|key| {
        key.strip_prefix(prefix)
            .is_some_and(|suffix| suffix.starts_with('.'))
    })
}

pub(super) fn resolve_inline_indexed_constant(
    name: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Result<Option<rumoca_core::Expression>, FlattenError> {
    let Some((base, indices)) = split_inline_indexed_name(name) else {
        return Ok(None);
    };
    let Some(base_expr) = resolve_constant_value_expr(base, ctx).cloned() else {
        return Ok(None);
    };
    let subscripts = indices
        .into_iter()
        .map(|index| {
            rumoca_core::Subscript::try_generated_expr(
                Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(index),
                    span,
                }),
                span,
                "flatten inline indexed constant",
            )
            .map_err(|err| FlattenError::missing_source_context(err.to_string()))
        })
        .collect::<Result<Vec<_>, FlattenError>>()?;
    Ok(Some(rumoca_core::Expression::Index {
        base: Box::new(base_expr),
        subscripts,
        span,
    }))
}

pub(super) fn named_constructor_arg<'a>(
    args: &'a [rumoca_core::Expression],
    field: &str,
) -> Option<&'a rumoca_core::Expression> {
    for arg in args {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: true,
            ..
        } = arg
            && name.as_str().strip_prefix("__rumoca_named_arg__.") == Some(field)
        {
            return args.first();
        }
    }
    None
}

pub(super) fn resolve_constant_field_access(
    base_name: &str,
    field: &str,
    span: rumoca_core::Span,
    ctx: &Context,
) -> Option<rumoca_core::Expression> {
    let mut current = base_name.to_string();
    let mut visited = rustc_hash::FxHashSet::default();
    loop {
        if !visited.insert(current.clone()) {
            return None;
        }
        let key = format!("{}.{}", current, field);
        if let Some(value) = ctx.constant_values.get(&key) {
            return Some(value.clone().with_span(span));
        }
        if let Some(value) = ctx.real_parameter_values.get(&key)
            && value.is_finite()
        {
            return Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(*value),
                span,
            });
        }
        if let Some(value) = ctx.parameter_values.get(&key) {
            return Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(*value),
                span,
            });
        }
        if let Some(value) = ctx.boolean_parameter_values.get(&key) {
            return Some(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(*value),
                span,
            });
        }
        if let Some(value) = ctx.enum_parameter_values.get(&key) {
            return Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::generated(value.clone()),
                subscripts: vec![],
                span,
            });
        }

        let alias_expr = ctx.constant_values.get(&current)?;
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = alias_expr
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        current = name.as_str().to_string();
    }
}
