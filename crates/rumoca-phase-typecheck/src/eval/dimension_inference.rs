use super::*;

/// Try to infer array dimensions from a binding expression.
pub fn infer_dimensions_from_binding(
    expr: &Expression,
    ctx: &TypeCheckEvalContext,
) -> Option<Vec<usize>> {
    infer_dimensions_from_binding_with_scope(expr, ctx, "")
}

/// Try to infer array dimensions from a binding expression with scope context.
///
/// The scope is used for resolving component references. For example, when
/// evaluating `combiTimeTable.table` with binding `table`, the scope is
/// the parent component path so we can resolve `table` correctly.
pub fn infer_dimensions_from_binding_with_scope(
    expr: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<Vec<usize>> {
    match expr {
        Expression::Array {
            elements,
            is_matrix,
        } => infer_array_dims(elements, *is_matrix, ctx),

        Expression::FunctionCall { comp, args } => {
            let func_name = comp
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            infer_dims_from_func_with_scope(&func_name, args, ctx, scope)
        }

        Expression::Range { start, step, end } => {
            infer_range_len_numeric(start, step.as_deref(), end, ctx, scope).map(|n| vec![n])
        }

        Expression::ComponentReference(cr) => {
            let indexed_path = cr.to_string();
            if let Some(dims) = lookup_structural_with_scope(
                &indexed_path,
                scope,
                &ctx.dimensions,
                ctx.suffix_index.as_ref(),
            ) {
                return Some(dims.clone());
            }

            let unindexed_path = cr
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            let base_dims = lookup_structural_with_scope(
                &unindexed_path,
                scope,
                &ctx.dimensions,
                ctx.suffix_index.as_ref(),
            )?
            .clone();
            Some(apply_component_subscripts_to_dims(
                base_dims, cr, ctx, scope,
            ))
        }

        Expression::Parenthesized { inner } => {
            infer_dimensions_from_binding_with_scope(inner, ctx, scope)
        }

        // Handle if-expressions by checking branch consistency or evaluating condition.
        Expression::If {
            branches,
            else_branch,
        } => infer_dims_from_if_with_scope(branches, else_branch, ctx, scope),

        // Binary expressions: element-wise and regular ops preserve shape.
        Expression::Binary { op, lhs, rhs } => {
            infer_dims_from_binary_with_scope(op, lhs, rhs, ctx, scope)
        }

        // Unary expressions (`-A`, `not A`) preserve shape.
        Expression::Unary { rhs, .. } => infer_dimensions_from_binding_with_scope(rhs, ctx, scope),

        // FieldAccess: `base.field` resolves as a full path in scope.
        Expression::FieldAccess { base, field } => {
            let base_path = extract_simple_component_path(base)?;
            let full_path = format!("{base_path}.{field}");
            lookup_structural_with_scope(
                &full_path,
                scope,
                &ctx.dimensions,
                ctx.suffix_index.as_ref(),
            )
            .cloned()
        }

        // ArrayComprehension: `{expr for i in range}` -> `[range_len, inner_dims...]`.
        Expression::ArrayComprehension {
            expr: inner_expr,
            indices,
            ..
        } => infer_dims_from_array_comprehension(inner_expr, indices, ctx, scope),

        _ => None,
    }
}

/// Apply component-reference subscripts to a base dimension vector.
///
/// MLS §10.1: scalar indexing consumes one dimension (`a[i]` -> scalar from `[n]`),
/// while range/colon indexing preserves that dimension (`a[2:4]`, `a[:]`).
fn apply_component_subscripts_to_dims(
    mut dims: Vec<usize>,
    cr: &rumoca_ir_ast::ComponentReference,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Vec<usize> {
    for part in &cr.parts {
        let Some(subs) = &part.subs else { continue };
        for sub in subs {
            if dims.is_empty() {
                return Vec::new();
            }
            apply_subscript_to_dims(sub, &mut dims, ctx, scope);
        }
    }
    dims
}

fn apply_subscript_to_dims(
    sub: &Subscript,
    dims: &mut Vec<usize>,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) {
    match sub {
        Subscript::Expression(expr) if matches!(expr, Expression::Range { .. }) => {
            if let Some(first_dim) = dims.first_mut() {
                *first_dim = infer_range_length(expr, ctx, scope).unwrap_or(*first_dim);
            }
        }
        // Scalar indexing consumes one dimension.
        Subscript::Expression(_) => {
            dims.remove(0);
        }
        // `:` keeps the current dimension unchanged.
        Subscript::Range { .. } | Subscript::Empty => {}
    }
}

fn extract_simple_component_path(expr: &Expression) -> Option<String> {
    match expr {
        Expression::ComponentReference(cr) => (!cr.parts.is_empty()).then(|| cr.to_string()),
        Expression::FieldAccess { base, field } => {
            let base_path = extract_simple_component_path(base)?;
            Some(format!("{base_path}.{field}"))
        }
        Expression::ArrayIndex { base, subscripts } => {
            let base_path = extract_simple_component_path(base)?;
            let rendered_subs: Vec<String> = subscripts
                .iter()
                .map(|sub| match sub {
                    rumoca_ir_ast::Subscript::Expression(sub_expr) => sub_expr.to_string(),
                    rumoca_ir_ast::Subscript::Range { token } => token.text.to_string(),
                    rumoca_ir_ast::Subscript::Empty => ":".to_string(),
                })
                .collect();
            Some(format!("{base_path}[{}]", rendered_subs.join(",")))
        }
        _ => None,
    }
}

fn infer_dims_from_array_comprehension(
    inner_expr: &Expression,
    indices: &[rumoca_ir_ast::ForIndex],
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<Vec<usize>> {
    if indices.is_empty() {
        return None;
    }
    let range = &indices[0].range;
    let outer_len = infer_range_length(range, ctx, scope)?;
    let mut dims = vec![outer_len];
    if let Some(inner_dims) = infer_dimensions_from_binding_with_scope(inner_expr, ctx, scope) {
        dims.extend(inner_dims);
    }
    Some(dims)
}

fn infer_range_length(
    range: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<usize> {
    if let Expression::Range { start, step, end } = range {
        infer_range_len_numeric(start, step.as_deref(), end, ctx, scope)
    } else {
        eval_integer_with_scope(range, ctx, scope).map(|n| n as usize)
    }
}

fn infer_dims_from_binary_with_scope(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<Vec<usize>> {
    let lhs_dims = infer_dimensions_from_binding_with_scope(lhs, ctx, scope);
    let rhs_dims = infer_dimensions_from_binding_with_scope(rhs, ctx, scope);

    match op {
        // Matrix multiply: `[m,n] * [n,p]` -> `[m,p]`.
        OpBinary::Mul(_) => match (&lhs_dims, &rhs_dims) {
            (Some(ld), Some(rd)) if ld.len() == 2 && rd.len() == 2 => Some(vec![ld[0], rd[1]]),
            (Some(ld), Some(rd)) if ld.len() == 2 && rd.len() == 1 => Some(vec![ld[0]]),
            (Some(ld), None) => Some(ld.clone()),
            (None, Some(rd)) => Some(rd.clone()),
            (Some(ld), Some(rd)) if ld.is_empty() => Some(rd.clone()),
            (Some(ld), Some(rd)) if rd.is_empty() => Some(ld.clone()),
            _ => lhs_dims.or(rhs_dims),
        },
        OpBinary::Add(_)
        | OpBinary::Sub(_)
        | OpBinary::AddElem(_)
        | OpBinary::SubElem(_)
        | OpBinary::MulElem(_)
        | OpBinary::DivElem(_)
        | OpBinary::ExpElem(_) => lhs_dims.or(rhs_dims),
        OpBinary::Div(_) => lhs_dims.or(rhs_dims),
        _ => None,
    }
}

fn infer_dims_from_if_with_scope(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<Vec<usize>> {
    if let Some(dims) = try_eval_if_condition_with_scope(branches, else_branch, ctx, scope) {
        return Some(dims);
    }

    let else_dims = infer_dimensions_from_binding_with_scope(else_branch, ctx, scope)?;
    if all_branches_consistent_with_scope(branches, &else_dims, ctx, scope) {
        Some(else_dims)
    } else {
        None
    }
}

fn try_eval_if_condition_with_scope(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    ctx: &TypeCheckEvalContext,
    scope: &str,
) -> Option<Vec<usize>> {
    for (cond, then_expr) in branches {
        match eval_boolean_with_scope(cond, ctx, scope) {
            Some(true) => return infer_dimensions_from_binding_with_scope(then_expr, ctx, scope),
            Some(false) => continue,
            None => return None,
        }
    }
    infer_dimensions_from_binding_with_scope(else_branch, ctx, scope)
}
