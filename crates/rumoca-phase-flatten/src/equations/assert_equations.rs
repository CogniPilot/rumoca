use std::sync::Arc;

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

use crate::boolean_eval::{try_eval_integer_for_comparison, try_eval_structural_boolean};
use crate::errors::FlattenError;
use crate::{Context, qualify_expression_imports_with_def_map_ctx};

use super::FlattenedEquations;

pub(super) struct AssertEquationLowering<'a> {
    ctx: &'a Context,
    prefix: &'a ast::QualifiedName,
    span: rumoca_core::Span,
    def_map: Option<&'a crate::ResolveDefMap>,
    origin: flat::EquationOrigin,
}

impl<'a> AssertEquationLowering<'a> {
    pub(super) fn new(
        ctx: &'a Context,
        prefix: &'a ast::QualifiedName,
        span: rumoca_core::Span,
        def_map: Option<&'a crate::ResolveDefMap>,
        origin: flat::EquationOrigin,
    ) -> Self {
        Self {
            ctx,
            prefix,
            span,
            def_map,
            origin,
        }
    }
}

pub(super) fn flatten_assert_equation(
    lowering: AssertEquationLowering<'_>,
    condition: &ast::Expression,
    message: &ast::Expression,
    level: Option<&ast::Expression>,
) -> Result<FlattenedEquations, FlattenError> {
    // MLS §8.3.7: preserve assert-equations for runtime checks in flat output.
    // They do not contribute to the DAE residual equation system.
    let assert_eq = flat::AssertEquation::new(
        qualify_assert_condition(lowering.ctx, condition, lowering.prefix, lowering.def_map)?,
        qualify_expression_imports_with_def_map_ctx(
            message,
            lowering.prefix,
            &lowering.ctx.current_imports,
            lowering.def_map,
            lowering.ctx,
        )?,
        level
            .map(|expr| {
                qualify_expression_imports_with_def_map_ctx(
                    expr,
                    lowering.prefix,
                    &lowering.ctx.current_imports,
                    lowering.def_map,
                    lowering.ctx,
                )
            })
            .transpose()?,
        lowering.span,
        lowering.origin,
    );
    Ok(assert_equation_result(assert_eq))
}

pub(super) fn is_assert_function_call(comp: &ast::ComponentReference) -> bool {
    comp.parts
        .last()
        .map(|part| part.ident.text.as_ref() == "assert")
        .unwrap_or(false)
}

pub(super) fn flatten_assert_function_call(
    lowering: AssertEquationLowering<'_>,
    args: &[ast::Expression],
) -> Result<FlattenedEquations, FlattenError> {
    let positional: Vec<&ast::Expression> = args
        .iter()
        .filter(|arg| !matches!(arg, ast::Expression::NamedArgument { .. }))
        .collect();
    let condition = named_call_arg(args, "condition").or_else(|| positional.first().copied());
    let message = named_call_arg(args, "message").or_else(|| positional.get(1).copied());
    let level = named_call_arg(args, "level").or_else(|| positional.get(2).copied());

    let (condition, message) = match (condition, message) {
        (Some(condition), Some(message)) => (condition, message),
        _ => {
            return Err(FlattenError::unsupported_equation(
                "assert() equation requires at least condition and message arguments",
                lowering.span,
            ));
        }
    };

    let assert_eq = flat::AssertEquation::new(
        qualify_assert_condition(lowering.ctx, condition, lowering.prefix, lowering.def_map)?,
        qualify_expression_imports_with_def_map_ctx(
            message,
            lowering.prefix,
            &lowering.ctx.current_imports,
            lowering.def_map,
            lowering.ctx,
        )?,
        level
            .map(|expr| {
                qualify_expression_imports_with_def_map_ctx(
                    expr,
                    lowering.prefix,
                    &lowering.ctx.current_imports,
                    lowering.def_map,
                    lowering.ctx,
                )
            })
            .transpose()?,
        lowering.span,
        lowering.origin,
    );

    Ok(assert_equation_result(assert_eq))
}

fn assert_equation_result(assert_eq: flat::AssertEquation) -> FlattenedEquations {
    FlattenedEquations {
        equations: vec![],
        structured_equations: vec![],
        assert_equations: vec![assert_eq],
        when_clauses: vec![],
        definite_roots: vec![],
        branches: vec![],
        potential_roots: vec![],
    }
}

fn named_call_arg<'a>(args: &'a [ast::Expression], name: &str) -> Option<&'a ast::Expression> {
    args.iter().find_map(|arg| {
        if let ast::Expression::NamedArgument {
            name: arg_name,
            value,
            ..
        } = arg
            && arg_name.text.as_ref() == name
        {
            Some(value.as_ref())
        } else {
            None
        }
    })
}

fn qualify_assert_condition(
    ctx: &Context,
    condition: &ast::Expression,
    prefix: &ast::QualifiedName,
    def_map: Option<&crate::ResolveDefMap>,
) -> Result<rumoca_core::Expression, FlattenError> {
    if !contains_structural_assert_intrinsic(condition) {
        return qualify_expression_imports_with_def_map_ctx(
            condition,
            prefix,
            &ctx.current_imports,
            def_map,
            ctx,
        );
    }
    let rewritten = rewrite_structural_assert_condition(ctx, condition, prefix);
    let condition = match try_eval_structural_boolean(ctx, &rewritten, prefix) {
        Some(value) => ast_boolean_literal(value, condition.span()),
        None => rewritten,
    };
    qualify_expression_imports_with_def_map_ctx(
        &condition,
        prefix,
        &ctx.current_imports,
        def_map,
        ctx,
    )
}

fn contains_structural_assert_intrinsic(expr: &ast::Expression) -> bool {
    match expr {
        ast::Expression::FunctionCall { comp, args, .. } => {
            is_structural_assert_intrinsic(comp)
                || args.iter().any(contains_structural_assert_intrinsic)
        }
        ast::Expression::Unary { rhs, .. } => contains_structural_assert_intrinsic(rhs),
        ast::Expression::Binary { lhs, rhs, .. } => {
            contains_structural_assert_intrinsic(lhs) || contains_structural_assert_intrinsic(rhs)
        }
        ast::Expression::Parenthesized { inner, .. } => contains_structural_assert_intrinsic(inner),
        ast::Expression::Array { elements, .. } | ast::Expression::Tuple { elements, .. } => {
            elements.iter().any(contains_structural_assert_intrinsic)
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(cond, value)| {
                contains_structural_assert_intrinsic(cond)
                    || contains_structural_assert_intrinsic(value)
            }) || contains_structural_assert_intrinsic(else_branch)
        }
        ast::Expression::Range {
            start, step, end, ..
        } => {
            contains_structural_assert_intrinsic(start)
                || step
                    .as_ref()
                    .is_some_and(|step| contains_structural_assert_intrinsic(step))
                || contains_structural_assert_intrinsic(end)
        }
        ast::Expression::NamedArgument { value, .. } => contains_structural_assert_intrinsic(value),
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            contains_structural_assert_intrinsic(expr)
                || indices
                    .iter()
                    .any(|index| contains_structural_assert_intrinsic(&index.range))
                || filter
                    .as_ref()
                    .is_some_and(|filter| contains_structural_assert_intrinsic(filter))
        }
        ast::Expression::ArrayIndex {
            base, subscripts, ..
        } => {
            contains_structural_assert_intrinsic(base)
                || subscripts
                    .iter()
                    .any(subscript_contains_structural_assert_intrinsic)
        }
        ast::Expression::FieldAccess { base, .. } => contains_structural_assert_intrinsic(base),
        ast::Expression::ClassModification { modifications, .. } => modifications
            .iter()
            .any(contains_structural_assert_intrinsic),
        ast::Expression::Modification { value, .. } => contains_structural_assert_intrinsic(value),
        ast::Expression::ComponentReference(_)
        | ast::Expression::Empty { .. }
        | ast::Expression::Terminal { .. } => false,
    }
}

fn is_structural_assert_intrinsic(comp: &ast::ComponentReference) -> bool {
    match comp.parts.as_slice() {
        [name] => name.ident.text.as_ref() == "cardinality",
        [package, name] if package.ident.text.as_ref() == "Connections" => {
            matches!(name.ident.text.as_ref(), "isRoot" | "rooted")
        }
        _ => false,
    }
}

fn is_cardinality_call(comp: &ast::ComponentReference) -> bool {
    matches!(
        comp.parts.as_slice(),
        [name] if name.ident.text.as_ref() == "cardinality"
    )
}

fn subscript_contains_structural_assert_intrinsic(subscript: &ast::Subscript) -> bool {
    match subscript {
        ast::Subscript::Expression(expr) => contains_structural_assert_intrinsic(expr),
        ast::Subscript::Empty | ast::Subscript::Range { .. } => false,
    }
}

fn rewrite_structural_assert_condition(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> ast::Expression {
    if let Some(value) = cardinality_integer_value(ctx, expr, prefix) {
        return ast_integer_literal(value, expr.span());
    }
    match expr {
        ast::Expression::Range { .. } => rewrite_assert_range(ctx, expr, prefix),
        ast::Expression::Unary { op, rhs, span } => ast::Expression::Unary {
            op: op.clone(),
            rhs: Arc::new(rewrite_structural_assert_condition(ctx, rhs, prefix)),
            span: *span,
        },
        ast::Expression::Binary { op, lhs, rhs, span } => ast::Expression::Binary {
            op: op.clone(),
            lhs: Arc::new(rewrite_structural_assert_condition(ctx, lhs, prefix)),
            rhs: Arc::new(rewrite_structural_assert_condition(ctx, rhs, prefix)),
            span: *span,
        },
        ast::Expression::ComponentReference(reference) => ast::Expression::ComponentReference(
            rewrite_assert_component_ref(ctx, reference, prefix),
        ),
        ast::Expression::FunctionCall { comp, args, span } => ast::Expression::FunctionCall {
            comp: rewrite_assert_component_ref(ctx, comp, prefix),
            args: args
                .iter()
                .map(|arg| rewrite_structural_assert_condition(ctx, arg, prefix))
                .collect(),
            span: *span,
        },
        ast::Expression::NamedArgument { name, value, span } => ast::Expression::NamedArgument {
            name: name.clone(),
            value: Arc::new(rewrite_structural_assert_condition(ctx, value, prefix)),
            span: *span,
        },
        ast::Expression::Array {
            elements,
            is_matrix,
            span,
        } => ast::Expression::Array {
            elements: rewrite_assert_elements(ctx, elements, prefix),
            is_matrix: *is_matrix,
            span: *span,
        },
        ast::Expression::Tuple { elements, span } => ast::Expression::Tuple {
            elements: rewrite_assert_elements(ctx, elements, prefix),
            span: *span,
        },
        ast::Expression::If { .. } => rewrite_assert_if(ctx, expr, prefix),
        ast::Expression::Parenthesized { inner, span } => ast::Expression::Parenthesized {
            inner: Arc::new(rewrite_structural_assert_condition(ctx, inner, prefix)),
            span: *span,
        },
        ast::Expression::ArrayComprehension { .. } => {
            rewrite_assert_array_comprehension(ctx, expr, prefix)
        }
        ast::Expression::ArrayIndex {
            base,
            subscripts,
            span,
        } => ast::Expression::ArrayIndex {
            base: Arc::new(rewrite_structural_assert_condition(ctx, base, prefix)),
            subscripts: subscripts
                .iter()
                .map(|subscript| rewrite_assert_subscript(ctx, subscript, prefix))
                .collect(),
            span: *span,
        },
        ast::Expression::FieldAccess { base, field, span } => ast::Expression::FieldAccess {
            base: Arc::new(rewrite_structural_assert_condition(ctx, base, prefix)),
            field: field.clone(),
            span: *span,
        },
        ast::Expression::ClassModification { .. } => {
            rewrite_assert_class_modification(ctx, expr, prefix)
        }
        ast::Expression::Modification { .. } => rewrite_assert_modification(ctx, expr, prefix),
        ast::Expression::Empty { .. } | ast::Expression::Terminal { .. } => expr.clone(),
    }
}

fn rewrite_assert_range(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> ast::Expression {
    match expr {
        ast::Expression::Range {
            start,
            step,
            end,
            span,
        } => ast::Expression::Range {
            start: Arc::new(rewrite_structural_assert_condition(ctx, start, prefix)),
            step: step
                .as_ref()
                .map(|step| Arc::new(rewrite_structural_assert_condition(ctx, step, prefix))),
            end: Arc::new(rewrite_structural_assert_condition(ctx, end, prefix)),
            span: *span,
        },
        _ => expr.clone(),
    }
}

fn rewrite_assert_elements(
    ctx: &Context,
    elements: &[ast::Expression],
    prefix: &ast::QualifiedName,
) -> Vec<ast::Expression> {
    elements
        .iter()
        .map(|element| rewrite_structural_assert_condition(ctx, element, prefix))
        .collect()
}

fn rewrite_assert_if(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> ast::Expression {
    match expr {
        ast::Expression::If {
            branches,
            else_branch,
            span,
        } => ast::Expression::If {
            branches: branches
                .iter()
                .map(|(cond, value)| {
                    (
                        rewrite_structural_assert_condition(ctx, cond, prefix),
                        rewrite_structural_assert_condition(ctx, value, prefix),
                    )
                })
                .collect(),
            else_branch: Arc::new(rewrite_structural_assert_condition(
                ctx,
                else_branch,
                prefix,
            )),
            span: *span,
        },
        _ => expr.clone(),
    }
}

fn rewrite_assert_array_comprehension(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> ast::Expression {
    match expr {
        ast::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            span,
        } => ast::Expression::ArrayComprehension {
            expr: Arc::new(rewrite_structural_assert_condition(ctx, expr, prefix)),
            indices: indices.clone(),
            filter: filter
                .as_ref()
                .map(|filter| Arc::new(rewrite_structural_assert_condition(ctx, filter, prefix))),
            span: *span,
        },
        _ => expr.clone(),
    }
}

fn rewrite_assert_class_modification(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> ast::Expression {
    match expr {
        ast::Expression::ClassModification {
            target,
            modifications,
            each_flags,
            final_flags,
            redeclare_flags,
            span,
        } => ast::Expression::ClassModification {
            target: rewrite_assert_component_ref(ctx, target, prefix),
            modifications: rewrite_assert_elements(ctx, modifications, prefix),
            each_flags: each_flags.clone(),
            final_flags: final_flags.clone(),
            redeclare_flags: redeclare_flags.clone(),
            span: *span,
        },
        _ => expr.clone(),
    }
}

fn rewrite_assert_modification(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> ast::Expression {
    match expr {
        ast::Expression::Modification {
            target,
            value,
            span,
        } => ast::Expression::Modification {
            target: rewrite_assert_component_ref(ctx, target, prefix),
            value: Arc::new(rewrite_structural_assert_condition(ctx, value, prefix)),
            span: *span,
        },
        _ => expr.clone(),
    }
}

fn cardinality_integer_value(
    ctx: &Context,
    expr: &ast::Expression,
    prefix: &ast::QualifiedName,
) -> Option<i64> {
    match expr {
        ast::Expression::FunctionCall { comp, .. } if is_cardinality_call(comp) => {
            try_eval_integer_for_comparison(Some(ctx), expr, prefix)
        }
        _ => None,
    }
}

fn rewrite_assert_component_ref(
    ctx: &Context,
    reference: &ast::ComponentReference,
    prefix: &ast::QualifiedName,
) -> ast::ComponentReference {
    let mut reference = reference.clone();
    for part in &mut reference.parts {
        if let Some(subscripts) = &mut part.subs {
            *subscripts = subscripts
                .iter()
                .map(|subscript| rewrite_assert_subscript(ctx, subscript, prefix))
                .collect();
        }
    }
    reference
}

fn rewrite_assert_subscript(
    ctx: &Context,
    subscript: &ast::Subscript,
    prefix: &ast::QualifiedName,
) -> ast::Subscript {
    match subscript {
        ast::Subscript::Expression(expr) => {
            ast::Subscript::Expression(rewrite_structural_assert_condition(ctx, expr, prefix))
        }
        ast::Subscript::Empty | ast::Subscript::Range { .. } => subscript.clone(),
    }
}

fn ast_integer_literal(value: i64, span: rumoca_core::Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::UnsignedInteger,
        token: rumoca_core::Token {
            text: Arc::from(value.to_string()),
            ..Default::default()
        },
        span,
    }
}

fn ast_boolean_literal(value: bool, span: rumoca_core::Span) -> ast::Expression {
    ast::Expression::Terminal {
        terminal_type: ast::TerminalType::Bool,
        token: rumoca_core::Token {
            text: Arc::from(value.to_string()),
            ..Default::default()
        },
        span,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn token(text: &str) -> rumoca_core::Token {
        rumoca_core::Token {
            text: Arc::from(text.to_string()),
            ..Default::default()
        }
    }

    fn test_span() -> rumoca_core::Span {
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("assert_condition_test.mo"),
            10,
            20,
        )
    }

    fn component_ref(parts: &[&str]) -> ast::ComponentReference {
        ast::ComponentReference {
            local: false,
            parts: parts
                .iter()
                .map(|part| ast::ComponentRefPart {
                    ident: token(part),
                    subs: None,
                })
                .collect(),
            span: test_span(),
            def_id: None,
        }
    }

    fn var_expr(parts: &[&str]) -> ast::Expression {
        ast::Expression::ComponentReference(component_ref(parts))
    }

    fn cardinality_expr(parts: &[&str]) -> ast::Expression {
        ast::Expression::FunctionCall {
            comp: component_ref(&["cardinality"]),
            args: vec![var_expr(parts)],
            span: test_span(),
        }
    }

    fn int_expr(value: i64) -> ast::Expression {
        ast_integer_literal(value, test_span())
    }

    fn binary_expr(
        op: rumoca_core::OpBinary,
        lhs: ast::Expression,
        rhs: ast::Expression,
    ) -> ast::Expression {
        ast::Expression::Binary {
            op,
            lhs: Arc::new(lhs),
            rhs: Arc::new(rhs),
            span: test_span(),
        }
    }

    fn assert_context() -> (Context, ast::QualifiedName) {
        let mut ctx = Context::default();
        ctx.cardinality_counts.insert("sg.inPort".to_string(), 1);
        (ctx, ast::QualifiedName::from_ident("sg"))
    }

    fn contains_cardinality_call(expr: &rumoca_core::Expression) -> bool {
        match expr {
            rumoca_core::Expression::FunctionCall { name, args, .. } => {
                name.as_str() == "cardinality" || args.iter().any(contains_cardinality_call)
            }
            rumoca_core::Expression::Binary { lhs, rhs, .. } => {
                contains_cardinality_call(lhs) || contains_cardinality_call(rhs)
            }
            rumoca_core::Expression::Unary { rhs, .. } => contains_cardinality_call(rhs),
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                branches.iter().any(|(cond, value)| {
                    contains_cardinality_call(cond) || contains_cardinality_call(value)
                }) || contains_cardinality_call(else_branch)
            }
            rumoca_core::Expression::Array { elements, .. }
            | rumoca_core::Expression::Tuple { elements, .. } => {
                elements.iter().any(contains_cardinality_call)
            }
            rumoca_core::Expression::Range {
                start, step, end, ..
            } => {
                contains_cardinality_call(start)
                    || step
                        .as_ref()
                        .is_some_and(|step| contains_cardinality_call(step))
                    || contains_cardinality_call(end)
            }
            rumoca_core::Expression::BuiltinCall { args, .. } => {
                args.iter().any(contains_cardinality_call)
            }
            rumoca_core::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => {
                contains_cardinality_call(expr)
                    || indices
                        .iter()
                        .any(|index| contains_cardinality_call(&index.range))
                    || filter
                        .as_ref()
                        .is_some_and(|filter| contains_cardinality_call(filter))
            }
            rumoca_core::Expression::Index {
                base, subscripts, ..
            } => {
                contains_cardinality_call(base)
                    || subscripts.iter().any(subscript_contains_cardinality_call)
            }
            rumoca_core::Expression::FieldAccess { base, .. } => contains_cardinality_call(base),
            rumoca_core::Expression::VarRef { subscripts, .. } => {
                subscripts.iter().any(subscript_contains_cardinality_call)
            }
            rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => {
                false
            }
        }
    }

    fn subscript_contains_cardinality_call(subscript: &rumoca_core::Subscript) -> bool {
        match subscript {
            rumoca_core::Subscript::Expr { expr, .. } => contains_cardinality_call(expr),
            rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => false,
        }
    }

    #[test]
    fn assert_condition_folds_structural_cardinality_check() {
        let (ctx, prefix) = assert_context();
        let condition = binary_expr(
            rumoca_core::OpBinary::Eq,
            cardinality_expr(&["inPort"]),
            int_expr(1),
        );

        let qualified = qualify_assert_condition(&ctx, &condition, &prefix, None)
            .expect("structural assertion condition should qualify");

        assert!(matches!(
            qualified,
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(true),
                ..
            }
        ));
    }

    #[test]
    fn assert_condition_rewrites_cardinality_inside_runtime_condition() {
        let (ctx, prefix) = assert_context();
        let cardinality_check = binary_expr(
            rumoca_core::OpBinary::Eq,
            cardinality_expr(&["inPort"]),
            int_expr(1),
        );
        let runtime_check = binary_expr(rumoca_core::OpBinary::Gt, var_expr(&["x"]), int_expr(0));
        let condition = binary_expr(rumoca_core::OpBinary::And, cardinality_check, runtime_check);

        let qualified = qualify_assert_condition(&ctx, &condition, &prefix, None)
            .expect("mixed assertion condition should qualify");

        assert!(
            !contains_cardinality_call(&qualified),
            "cardinality() must not survive into Flat assert conditions"
        );
    }

    #[test]
    fn assert_condition_preserves_runtime_parameter_comparison() {
        let mut ctx = Context::default();
        ctx.real_parameter_values.insert("m.k".to_string(), 1.0);
        let prefix = ast::QualifiedName::from_ident("m");
        let condition = binary_expr(rumoca_core::OpBinary::Gt, var_expr(&["k"]), int_expr(0));

        let qualified = qualify_assert_condition(&ctx, &condition, &prefix, None)
            .expect("runtime parameter assertion condition should qualify");

        assert!(matches!(
            qualified,
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Gt,
                ..
            }
        ));
    }
}
