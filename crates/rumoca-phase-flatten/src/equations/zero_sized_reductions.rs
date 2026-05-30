use super::*;

// SPEC_0021: Exception - exhaustive expression-tree rewrite over AST variants.
#[allow(clippy::too_many_lines)]
pub(super) fn simplify_zero_sized_reductions(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> ast::Expression {
    match expr {
        ast::Expression::FunctionCall { comp, args, span } => {
            if args.len() == 1
                && expression_is_statically_zero_sized(ctx, &args[0], prefix)
                && let Some(neutral) = zero_sized_reduction_neutral(comp, *span)
            {
                return neutral;
            }

            ast::Expression::FunctionCall {
                comp: comp.clone(),
                args: args
                    .iter()
                    .map(|arg| simplify_zero_sized_reductions(ctx, arg, prefix))
                    .collect(),
                span: *span,
            }
        }
        ast::Expression::Binary { op, lhs, rhs, span } => ast::Expression::Binary {
            op: op.clone(),
            lhs: Arc::new(simplify_zero_sized_reductions(ctx, lhs, prefix)),
            rhs: Arc::new(simplify_zero_sized_reductions(ctx, rhs, prefix)),
            span: *span,
        },
        ast::Expression::Unary { op, rhs, span } => ast::Expression::Unary {
            op: op.clone(),
            rhs: Arc::new(simplify_zero_sized_reductions(ctx, rhs, prefix)),
            span: *span,
        },
        ast::Expression::Parenthesized { inner, span } => ast::Expression::Parenthesized {
            inner: Arc::new(simplify_zero_sized_reductions(ctx, inner, prefix)),
            span: *span,
        },
        ast::Expression::If {
            branches,
            else_branch,
            span,
        } => ast::Expression::If {
            branches: branches
                .iter()
                .map(|(condition, value)| {
                    (
                        simplify_zero_sized_reductions(ctx, condition, prefix),
                        simplify_zero_sized_reductions(ctx, value, prefix),
                    )
                })
                .collect(),
            else_branch: Arc::new(simplify_zero_sized_reductions(ctx, else_branch, prefix)),
            span: *span,
        },
        ast::Expression::Array {
            elements,
            is_matrix,
            span,
        } => ast::Expression::Array {
            elements: elements
                .iter()
                .map(|element| simplify_zero_sized_reductions(ctx, element, prefix))
                .collect(),
            is_matrix: *is_matrix,
            span: *span,
        },
        ast::Expression::Tuple { elements, span } => ast::Expression::Tuple {
            elements: elements
                .iter()
                .map(|element| simplify_zero_sized_reductions(ctx, element, prefix))
                .collect(),
            span: *span,
        },
        ast::Expression::Range {
            start,
            step,
            end,
            span,
        } => ast::Expression::Range {
            start: Arc::new(simplify_zero_sized_reductions(ctx, start, prefix)),
            step: step
                .as_ref()
                .map(|step| Arc::new(simplify_zero_sized_reductions(ctx, step, prefix))),
            end: Arc::new(simplify_zero_sized_reductions(ctx, end, prefix)),
            span: *span,
        },
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            span,
        } => ast::Expression::ArrayComprehension {
            expr: Arc::new(simplify_zero_sized_reductions(ctx, expr, prefix)),
            indices: indices.clone(),
            filter: filter
                .as_ref()
                .map(|filter| Arc::new(simplify_zero_sized_reductions(ctx, filter, prefix))),
            span: *span,
        },
        ast::Expression::ArrayIndex {
            base,
            subscripts,
            span,
        } => ast::Expression::ArrayIndex {
            base: Arc::new(simplify_zero_sized_reductions(ctx, base, prefix)),
            subscripts: subscripts.clone(),
            span: *span,
        },
        ast::Expression::FieldAccess { base, field, span } => ast::Expression::FieldAccess {
            base: Arc::new(simplify_zero_sized_reductions(ctx, base, prefix)),
            field: field.clone(),
            span: *span,
        },
        ast::Expression::NamedArgument { name, value, span } => ast::Expression::NamedArgument {
            name: name.clone(),
            value: Arc::new(simplify_zero_sized_reductions(ctx, value, prefix)),
            span: *span,
        },
        ast::Expression::Modification {
            target,
            value,
            span,
        } => ast::Expression::Modification {
            target: target.clone(),
            value: Arc::new(simplify_zero_sized_reductions(ctx, value, prefix)),
            span: *span,
        },
        ast::Expression::ClassModification { .. }
        | ast::Expression::ComponentReference(_)
        | ast::Expression::Terminal { .. }
        | ast::Expression::Empty { .. } => expr.clone(),
    }
}

fn expression_is_statically_zero_sized(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> bool {
    match expr {
        ast::Expression::ComponentReference(component) => {
            component_ref_is_statically_zero_sized(ctx, component, prefix)
        }
        _ => find_array_refs_needing_expansion(expr, prefix, ctx)
            .iter()
            .any(|array_ref| array_ref.dims.iter().any(|dim| *dim <= 0)),
    }
}

fn component_ref_is_statically_zero_sized(
    ctx: &Context,
    component: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
) -> bool {
    if matches!(
        infer_component_ref_shape(component, prefix, ctx),
        ExpressionShape::Vector(0) | ExpressionShape::Matrix(0, _) | ExpressionShape::Matrix(_, 0)
    ) {
        return true;
    }

    find_array_refs_needing_expansion(
        &ast::Expression::ComponentReference(component.clone()),
        prefix,
        ctx,
    )
    .iter()
    .any(|array_ref| array_ref.dims.iter().any(|dim| *dim <= 0))
}

fn zero_sized_reduction_neutral(
    comp: &ast::ComponentReference,
    span: rumoca_core::Span,
) -> Option<ast::Expression> {
    if comp.local || comp.parts.len() != 1 {
        return None;
    }
    let name = comp.parts.last()?.ident.text.as_ref();
    match name {
        "sum" => Some(real_literal_expr(0.0, span)),
        "product" => Some(real_literal_expr(1.0, span)),
        _ => None,
    }
}

fn real_literal_expr(value: f64, span: rumoca_core::Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedReal,
        token: rumoca_core::Token {
            text: std::sync::Arc::from(value.to_string()),
            ..Default::default()
        },
        span,
    }
}

pub(super) fn expand_reduction_over_array_ref(
    ctx: &Context,
    comp: &ast::ComponentReference,
    args: &[ast::Expression],
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> Result<Option<ast::Expression>, FlattenError> {
    let Some((neutral, op)) = scalar_reduction_neutral_and_op(comp, span) else {
        return Ok(None);
    };
    let [ast::Expression::ComponentReference(arg_ref)] = args else {
        return Ok(None);
    };
    let Some(array_ref) = find_array_refs_needing_expansion(&args[0], prefix, ctx)
        .into_iter()
        .next()
    else {
        return Ok(None);
    };
    let [count] = array_ref.dims.as_slice() else {
        return Ok(None);
    };
    if *count <= 0 {
        return Ok(Some(neutral));
    }
    let Some(component_part_index) = array_ref.component_part_index else {
        return Ok(None);
    };

    let mut terms = Vec::with_capacity(*count as usize);
    for index in 1..=*count {
        let Some(indexed_ref) =
            component_ref_with_array_index(arg_ref, component_part_index, index)
        else {
            return Ok(None);
        };
        terms.push(ast::Expression::ComponentReference(indexed_ref));
    }
    Ok(Some(
        fold_reduction_terms(terms, op, span).unwrap_or(neutral),
    ))
}

fn scalar_reduction_neutral_and_op(
    comp: &ast::ComponentReference,
    span: rumoca_core::Span,
) -> Option<(ast::Expression, OpBinary)> {
    if comp.local || comp.parts.len() != 1 {
        return None;
    }
    match comp.parts[0].ident.text.as_ref() {
        "sum" => Some((real_literal_expr(0.0, span), OpBinary::Add)),
        "product" => Some((real_literal_expr(1.0, span), OpBinary::Mul)),
        _ => None,
    }
}

fn component_ref_with_array_index(
    component: &ast::ComponentReference,
    component_part_index: usize,
    index: i64,
) -> Option<ast::ComponentReference> {
    let mut indexed = component.clone();
    let part = indexed.parts.get_mut(component_part_index)?;
    part.subs = Some(vec![ast::Subscript::Expression(integer_literal_expr(
        index,
        component.span,
    ))]);
    Some(indexed)
}

fn integer_literal_expr(value: i64, span: rumoca_core::Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: std::sync::Arc::from(value.to_string()),
            ..Default::default()
        },
        span,
    }
}

fn fold_reduction_terms(
    mut terms: Vec<ast::Expression>,
    op: OpBinary,
    span: rumoca_core::Span,
) -> Option<ast::Expression> {
    let first = terms.drain(..1).next()?;
    Some(
        terms
            .into_iter()
            .fold(first, |lhs, rhs| ast::Expression::Binary {
                op: op.clone(),
                lhs: Arc::new(lhs),
                rhs: Arc::new(rhs),
                span,
            }),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn part(name: &str) -> ast::ComponentRefPart {
        ast::ComponentRefPart {
            ident: rumoca_core::Token {
                text: std::sync::Arc::from(name),
                ..Default::default()
            },
            subs: None,
        }
    }

    fn component_ref(path: &[&str]) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: path.iter().map(|name| part(name)).collect(),
            span: rumoca_core::Span::DUMMY,
            def_id: None,
        }
    }

    fn var_ref(path: &[&str]) -> ast::Expression {
        ast::Expression::ComponentReference(component_ref(path))
    }

    fn call(name: &str, args: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression::FunctionCall {
            comp: component_ref(&[name]),
            args,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn qualified_call(path: &[&str], args: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression::FunctionCall {
            comp: component_ref(path),
            args,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn literal_text(expr: &ast::Expression) -> Option<&str> {
        match expr {
            ast::Expression::Terminal { token, .. } => Some(token.text.as_ref()),
            _ => None,
        }
    }

    fn first_subscript_integer(expr: &ast::Expression) -> Option<&str> {
        let ast::Expression::ComponentReference(component) = expr else {
            return None;
        };
        let subscript = component.parts.first()?.subs.as_ref()?.first()?;
        let ast::Subscript::Expression(ast::Expression::Terminal { token, .. }) = subscript else {
            return None;
        };
        Some(token.text.as_ref())
    }

    #[test]
    fn simplifies_sum_over_zero_sized_component_array_field() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("tank.topPorts".to_string(), vec![0]);

        let expr = call("sum", vec![var_ref(&["topPorts", "m_flow"])]);
        let simplified =
            simplify_zero_sized_reductions(&ctx, &expr, &ast::QualifiedName::from_dotted("tank"));

        assert_eq!(literal_text(&simplified), Some("0"));
    }

    #[test]
    fn preserves_sum_over_nonzero_component_array_field() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("tank.topPorts".to_string(), vec![1]);

        let expr = call("sum", vec![var_ref(&["topPorts", "m_flow"])]);
        let simplified =
            simplify_zero_sized_reductions(&ctx, &expr, &ast::QualifiedName::from_dotted("tank"));

        assert!(matches!(simplified, ast::Expression::FunctionCall { .. }));
    }

    #[test]
    fn expands_sum_over_component_array_field() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("tank.topPorts".to_string(), vec![2]);

        let expr = call("sum", vec![var_ref(&["topPorts", "m_flow"])]);
        let ast::Expression::FunctionCall { comp, args, span } = expr else {
            panic!("expected function call");
        };
        let expanded = expand_reduction_over_array_ref(
            &ctx,
            &comp,
            &args,
            &ast::QualifiedName::from_dotted("tank"),
            span,
        )
        .expect("reduction expansion should succeed")
        .expect("reduction should expand");

        let ast::Expression::Binary { lhs, rhs, .. } = expanded else {
            panic!("expected binary sum");
        };
        assert_eq!(first_subscript_integer(&lhs), Some("1"));
        assert_eq!(first_subscript_integer(&rhs), Some("2"));
    }

    #[test]
    fn expands_product_over_component_array_field() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("tank.topPorts".to_string(), vec![2]);

        let expr = call("product", vec![var_ref(&["topPorts", "m_flow"])]);
        let ast::Expression::FunctionCall { comp, args, span } = expr else {
            panic!("expected function call");
        };
        let expanded = expand_reduction_over_array_ref(
            &ctx,
            &comp,
            &args,
            &ast::QualifiedName::from_dotted("tank"),
            span,
        )
        .expect("reduction expansion should succeed")
        .expect("reduction should expand");

        assert!(matches!(
            expanded,
            ast::Expression::Binary {
                op: rumoca_core::OpBinary::Mul,
                ..
            }
        ));
    }

    #[test]
    fn preserves_qualified_sum_over_zero_sized_component_array_field() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("tank.topPorts".to_string(), vec![0]);

        let expr = qualified_call(&["Pkg", "sum"], vec![var_ref(&["topPorts", "m_flow"])]);
        let simplified =
            simplify_zero_sized_reductions(&ctx, &expr, &ast::QualifiedName::from_dotted("tank"));

        assert!(matches!(simplified, ast::Expression::FunctionCall { .. }));
    }

    #[test]
    fn simplifies_product_over_zero_sized_component_array_field() {
        let mut ctx = Context::new();
        ctx.array_dimensions
            .insert("tank.topPorts".to_string(), vec![0]);

        let expr = call("product", vec![var_ref(&["topPorts", "m_flow"])]);
        let simplified =
            simplify_zero_sized_reductions(&ctx, &expr, &ast::QualifiedName::from_dotted("tank"));

        assert_eq!(literal_text(&simplified), Some("1"));
    }
}
