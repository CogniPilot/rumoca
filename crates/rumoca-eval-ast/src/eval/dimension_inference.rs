use super::*;

/// Try to infer array dimensions from a binding expression.
pub fn infer_dimensions_from_binding(
    expr: &Expression,
    ctx: &(impl DimensionInferenceContext + ?Sized),
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
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<Vec<usize>> {
    if rumoca_ir_ast::expression_required_value_violation(expr).is_some() {
        return None;
    }
    infer_dimensions_from_binding_inner(expr, ctx, scope)
}

fn infer_dimensions_from_binding_inner(
    expr: &Expression,
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<Vec<usize>> {
    match expr {
        Expression::Terminal { .. } => Some(Vec::new()),

        Expression::Array {
            elements,
            is_matrix,
            ..
        } => infer_array_dims(elements, *is_matrix, ctx, scope),

        Expression::FunctionCall { comp, args, .. } => {
            infer_dims_from_func_with_scope(comp, args, ctx, scope)
        }

        Expression::DerivativeCall { args, .. } => match args.as_slice() {
            [arg] => infer_dimensions_from_binding_inner(arg, ctx, scope),
            _ => None,
        },

        Expression::Range {
            start, step, end, ..
        } => infer_range_len_numeric(start, step.as_deref(), end, ctx, scope).map(|n| vec![n]),

        Expression::ComponentReference(cr) => {
            validate_component_subscripts(cr, ctx, scope)?;
            let indexed_path = cr.to_string();
            if let Some(dims) = ctx.lookup_dimensions(&indexed_path, scope) {
                return Some(dims);
            }

            let unindexed_path = cr
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            let Some(base_dims) = ctx.lookup_dimensions(&unindexed_path, scope) else {
                return ctx
                    .scalar_value_known(&unindexed_path, scope)
                    .then(Vec::new);
            };
            apply_component_subscripts_to_dims(base_dims, cr, ctx, scope)
        }

        Expression::Parenthesized { inner, .. } => {
            infer_dimensions_from_binding_inner(inner, ctx, scope)
        }

        // Handle if-expressions by checking branch consistency or evaluating condition.
        Expression::If {
            branches,
            else_branch,
            ..
        } => infer_dims_from_if_with_scope(branches, else_branch, ctx, scope),

        // Binary expressions: element-wise and regular ops preserve shape.
        Expression::Binary { op, lhs, rhs, .. } => {
            infer_dims_from_binary_with_scope(op, lhs, rhs, ctx, scope)
        }

        // Unary expressions (`-A`, `not A`) preserve shape.
        Expression::Unary { rhs, .. } => infer_dimensions_from_binding_inner(rhs, ctx, scope),

        // FieldAccess: `base.field` resolves as a full path in scope.
        Expression::FieldAccess { base, field, .. } => {
            let base_path = extract_simple_component_path(base)?;
            let full_path = format!("{base_path}.{field}");
            ctx.lookup_dimensions(&full_path, scope)
        }

        // ArrayComprehension: `{expr for i in range}` -> `[range_len, inner_dims...]`.
        Expression::ArrayComprehension {
            expr: inner_expr,
            indices,
            filter,
            ..
        } => {
            infer_dims_from_array_comprehension(inner_expr, indices, filter.as_deref(), ctx, scope)
        }

        _ => None,
    }
}

fn validate_component_subscripts(
    cr: &rumoca_ir_ast::ComponentReference,
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<()> {
    for subscript in cr
        .parts
        .iter()
        .filter_map(|part| part.subs.as_deref())
        .flatten()
    {
        match subscript {
            Subscript::Expression(expression @ Expression::Range { .. }) => {
                infer_range_length(expression, ctx, scope)?;
            }
            Subscript::Expression(expression) => {
                if !infer_dimensions_from_binding_inner(expression, ctx, scope)?.is_empty() {
                    return None;
                }
            }
            Subscript::Range { .. } => {}
            Subscript::Empty => return None,
        }
    }
    Some(())
}

/// Apply component-reference subscripts to a base dimension vector.
///
/// MLS §10.1: scalar indexing consumes one dimension (`a[i]` -> scalar from `[n]`),
/// while range/colon indexing preserves that dimension (`a[2:4]`, `a[:]`).
fn apply_component_subscripts_to_dims(
    mut dims: Vec<usize>,
    cr: &rumoca_ir_ast::ComponentReference,
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<Vec<usize>> {
    // `pos` tracks the dimension the next subscript applies to. Scalar
    // indexing removes that dimension (so the cursor stays put, now pointing
    // at the following dimension); slice/colon indexing keeps it and advances
    // the cursor. This positional walk is what lets `a[:, i]` on `[3, 4]`
    // drop dimension 1 and yield `[3]` rather than removing dimension 0.
    let mut pos = 0usize;
    for part in &cr.parts {
        let Some(subs) = &part.subs else { continue };
        for sub in subs {
            if matches!(sub, Subscript::Empty) || pos >= dims.len() {
                return None;
            }
            apply_subscript_to_dims(sub, &mut dims, &mut pos, ctx, scope)?;
        }
    }
    Some(dims)
}

fn apply_subscript_to_dims(
    sub: &Subscript,
    dims: &mut Vec<usize>,
    pos: &mut usize,
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<()> {
    match sub {
        Subscript::Expression(expr) if matches!(expr, Expression::Range { .. }) => {
            dims[*pos] = infer_range_length(expr, ctx, scope)?;
            *pos += 1;
            Some(())
        }
        // Scalar indexing consumes the dimension at the cursor.
        Subscript::Expression(expression) => {
            if !infer_dimensions_from_binding_inner(expression, ctx, scope)?.is_empty() {
                return None;
            }
            dims.remove(*pos);
            Some(())
        }
        // `:` keeps the current dimension unchanged.
        Subscript::Range { .. } => {
            *pos += 1;
            Some(())
        }
        Subscript::Empty => None,
    }
}

fn extract_simple_component_path(expr: &Expression) -> Option<String> {
    rumoca_ir_ast::expression_component_path(expr).map(|path| path.to_flat_string())
}

fn infer_dims_from_array_comprehension(
    inner_expr: &Expression,
    indices: &[rumoca_ir_ast::ForIndex],
    filter: Option<&Expression>,
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<Vec<usize>> {
    let [index] = indices else {
        return None;
    };
    if filter.is_some() {
        return None;
    }
    let outer_len = infer_range_length(&index.range, ctx, scope)?;
    let mut dims = vec![outer_len];
    dims.extend(infer_dimensions_from_binding_inner(inner_expr, ctx, scope)?);
    Some(dims)
}

fn infer_range_length(
    range: &Expression,
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<usize> {
    if let Expression::Range {
        start, step, end, ..
    } = range
    {
        infer_range_len_numeric(start, step.as_deref(), end, ctx, scope)
    } else {
        ctx.eval_integer(range, scope)
            .and_then(|n| usize::try_from(n).ok())
    }
}

fn infer_dims_from_binary_with_scope(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<Vec<usize>> {
    let lhs_dims = infer_dimensions_from_binding_inner(lhs, ctx, scope);
    let rhs_dims = infer_dimensions_from_binding_inner(rhs, ctx, scope);

    match op {
        // Matrix multiply: `[m,n] * [n,p]` -> `[m,p]`.
        OpBinary::Mul => match (&lhs_dims, &rhs_dims) {
            (Some(ld), Some(rd)) if ld.len() == 2 && rd.len() == 2 => Some(vec![ld[0], rd[1]]),
            (Some(ld), Some(rd)) if ld.len() == 2 && rd.len() == 1 => Some(vec![ld[0]]),
            (Some(ld), None) => Some(ld.clone()),
            (None, Some(rd)) => Some(rd.clone()),
            (Some(ld), Some(rd)) if ld.is_empty() => Some(rd.clone()),
            (Some(ld), Some(rd)) if rd.is_empty() => Some(ld.clone()),
            _ => lhs_dims.or(rhs_dims),
        },
        OpBinary::Add
        | OpBinary::Sub
        | OpBinary::AddElem
        | OpBinary::SubElem
        | OpBinary::MulElem
        | OpBinary::DivElem
        | OpBinary::ExpElem => lhs_dims.or(rhs_dims),
        OpBinary::Div => lhs_dims.or(rhs_dims),
        _ => None,
    }
}

fn infer_dims_from_if_with_scope(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<Vec<usize>> {
    if let Some(dims) = try_eval_if_condition_with_scope(branches, else_branch, ctx, scope) {
        return Some(dims);
    }

    let else_dims = infer_dimensions_from_binding_inner(else_branch, ctx, scope)?;
    if all_branches_consistent_with_scope(branches, &else_dims, ctx, scope) {
        Some(else_dims)
    } else {
        None
    }
}

fn try_eval_if_condition_with_scope(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    ctx: &(impl DimensionInferenceContext + ?Sized),
    scope: &str,
) -> Option<Vec<usize>> {
    for (cond, then_expr) in branches {
        match ctx.eval_boolean(cond, scope) {
            Some(true) => return infer_dimensions_from_binding_inner(then_expr, ctx, scope),
            Some(false) => continue,
            None => return None,
        }
    }
    infer_dimensions_from_binding_inner(else_branch, ctx, scope)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::TypeCheckEvalContext;
    use std::sync::Arc;

    fn indexed(subscripts: Vec<Subscript>) -> Expression {
        Expression::ComponentReference(rumoca_ir_ast::ComponentReference {
            local: false,
            parts: vec![rumoca_ir_ast::ComponentRefPart {
                ident: rumoca_core::Token {
                    text: Arc::from("a"),
                    ..rumoca_core::Token::default()
                },
                subs: Some(subscripts),
                def_id: None,
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        })
    }

    fn integer(value: i64) -> Expression {
        Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token: rumoca_core::Token {
                text: Arc::from(value.to_string()),
                ..rumoca_core::Token::default()
            },
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn reference(name: &str) -> Expression {
        Expression::ComponentReference(rumoca_ir_ast::ComponentReference {
            local: false,
            parts: vec![rumoca_ir_ast::ComponentRefPart {
                ident: rumoca_core::Token {
                    text: Arc::from(name),
                    ..rumoca_core::Token::default()
                },
                subs: None,
                def_id: None,
            }],
            span: rumoca_core::Span::DUMMY,
            qualified_display_name: None,
        })
    }

    #[test]
    fn dimension_inference_distinguishes_colon_from_recovery_subscripts() {
        let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
        ctx.dimensions.insert("a".to_string(), vec![3, 4]);
        let colon = indexed(vec![Subscript::Range {
            token: rumoca_core::Token::default(),
        }]);
        let recovery = indexed(vec![Subscript::Empty]);

        assert_eq!(
            infer_dimensions_from_binding(&colon, &ctx),
            Some(vec![3, 4])
        );
        assert_eq!(infer_dimensions_from_binding(&recovery, &ctx), None);

        ctx.dimensions.insert("a".to_string(), Vec::new());
        assert_eq!(
            infer_dimensions_from_binding(&indexed(vec![Subscript::Empty]), &ctx),
            None,
            "a scalar base must not hide a recovery subscript"
        );

        ctx.dimensions.insert("a".to_string(), vec![3]);
        let scalar_index = || {
            Subscript::Expression(Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token: rumoca_core::Token {
                    text: Arc::from("1"),
                    ..rumoca_core::Token::default()
                },
                span: rumoca_core::Span::DUMMY,
            })
        };
        assert_eq!(
            infer_dimensions_from_binding(&indexed(vec![scalar_index(), scalar_index()]), &ctx),
            None,
            "excess subscripts must not silently succeed"
        );
    }

    #[test]
    fn dimension_inference_rejects_recovery_before_any_shape_or_exact_path_lookup() {
        let array = Expression::Array {
            elements: vec![Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token: rumoca_core::Token::default(),
                span: rumoca_core::Span::DUMMY,
            }],
            is_matrix: false,
            span: rumoca_core::Span::DUMMY,
        };
        let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
        ctx.dimensions.insert("a[]".to_string(), vec![9]);

        assert_eq!(
            infer_dimensions_from_binding(
                &Expression::Terminal {
                    terminal_type: rumoca_ir_ast::TerminalType::Empty,
                    token: rumoca_core::Token::default(),
                    span: rumoca_core::Span::DUMMY,
                },
                &ctx,
            ),
            None,
        );
        assert_eq!(
            infer_dimensions_from_binding(
                &Expression::Unary {
                    op: rumoca_core::OpUnary::Empty,
                    rhs: Arc::new(array.clone()),
                    span: rumoca_core::Span::DUMMY,
                },
                &ctx,
            ),
            None,
        );
        assert_eq!(
            infer_dimensions_from_binding(
                &Expression::Binary {
                    op: OpBinary::Add,
                    lhs: Arc::new(array),
                    rhs: Arc::new(Expression::Empty {
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                },
                &ctx,
            ),
            None,
        );
        assert_eq!(
            infer_dimensions_from_binding(&indexed(vec![Subscript::Empty]), &ctx),
            None,
            "an exact rendered-path hit must not bypass recovery validation",
        );
    }

    #[test]
    fn dimension_inference_enforces_assignment_and_end_contexts() {
        let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
        let assignment = Expression::Binary {
            op: OpBinary::Assign,
            lhs: Arc::new(Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token: rumoca_core::Token::default(),
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Arc::new(Expression::Terminal {
                terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
                token: rumoca_core::Token::default(),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        };
        let end = Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::End,
            token: rumoca_core::Token::default(),
            span: rumoca_core::Span::DUMMY,
        };
        assert_eq!(infer_dimensions_from_binding(&assignment, &ctx), None);
        assert_eq!(infer_dimensions_from_binding(&end, &ctx), None);
        ctx.dimensions.insert("a".to_string(), vec![3]);
        assert_eq!(
            infer_dimensions_from_binding(&indexed(vec![Subscript::Expression(end)]), &ctx,),
            Some(Vec::new()),
            "`end` is a legal scalar index only while nested in a subscript",
        );
    }

    #[test]
    fn explicit_slice_requires_exact_bounds_while_colon_preserves_extent() {
        let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
        ctx.dimensions.insert("a".to_string(), vec![10]);
        let explicit = |end| {
            indexed(vec![Subscript::Expression(Expression::Range {
                start: Arc::new(integer(2)),
                step: None,
                end: Arc::new(end),
                span: rumoca_core::Span::DUMMY,
            })])
        };

        assert_eq!(
            infer_dimensions_from_binding(&explicit(integer(4)), &ctx),
            Some(vec![3])
        );
        assert_eq!(
            infer_dimensions_from_binding(&explicit(reference("n")), &ctx),
            None,
            "an unknown explicit range is not equivalent to full-colon selection"
        );
        assert_eq!(
            infer_dimensions_from_binding(
                &indexed(vec![Subscript::Range {
                    token: rumoca_core::Token::default()
                }]),
                &ctx
            ),
            Some(vec![10])
        );
    }

    #[test]
    fn vector_selector_refuses_even_when_an_exact_rendered_shape_is_cached() {
        let mut ctx = TypeCheckEvalContext::for_pre_identity_structural();
        ctx.dimensions.insert("a".to_string(), vec![10]);
        ctx.dimensions.insert("indices".to_string(), vec![2]);
        let selected = indexed(vec![Subscript::Expression(reference("indices"))]);
        let Expression::ComponentReference(reference) = &selected else {
            unreachable!();
        };
        ctx.dimensions.insert(reference.to_string(), vec![2]);

        assert_eq!(infer_dimensions_from_binding(&selected, &ctx), None);
    }
}
