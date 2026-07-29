use super::*;

pub(super) fn expression_contains_function_call(expr: &Expression) -> bool {
    match expr {
        Expression::FunctionCall { .. } => true,
        Expression::Binary { lhs, rhs, .. } => {
            expression_contains_function_call(lhs) || expression_contains_function_call(rhs)
        }
        Expression::Unary { rhs, .. } => expression_contains_function_call(rhs),
        Expression::BuiltinCall { args, .. }
        | Expression::Array { elements: args, .. }
        | Expression::Tuple { elements: args, .. } => {
            args.iter().any(expression_contains_function_call)
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            branches.iter().any(|(condition, value)| {
                expression_contains_function_call(condition)
                    || expression_contains_function_call(value)
            }) || expression_contains_function_call(else_branch)
        }
        Expression::Range {
            start, step, end, ..
        } => {
            expression_contains_function_call(start)
                || step
                    .as_deref()
                    .is_some_and(expression_contains_function_call)
                || expression_contains_function_call(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            expression_contains_function_call(expr)
                || indices
                    .iter()
                    .any(|index| expression_contains_function_call(&index.range))
                || filter
                    .as_deref()
                    .is_some_and(expression_contains_function_call)
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            expression_contains_function_call(base)
                || subscripts.iter().any(subscript_contains_function_call)
        }
        Expression::FieldAccess { base, .. } => expression_contains_function_call(base),
        Expression::VarRef { subscripts, .. } => {
            subscripts.iter().any(subscript_contains_function_call)
        }
        Expression::Literal { .. } | Expression::Empty { .. } => false,
    }
}

fn subscript_contains_function_call(subscript: &rumoca_core::Subscript) -> bool {
    match subscript {
        rumoca_core::Subscript::Expr { expr, .. } => expression_contains_function_call(expr),
        rumoca_core::Subscript::Index { .. } | rumoca_core::Subscript::Colon { .. } => false,
    }
}

pub(super) struct FunctionOverrideExpressionRewriter<'a> {
    ctx: &'a FunctionOverrideRewriteContext<'a>,
    active_comprehension_binders: Vec<String>,
}

impl<'a> FunctionOverrideExpressionRewriter<'a> {
    pub(super) fn new(ctx: &'a FunctionOverrideRewriteContext<'a>) -> Self {
        Self {
            ctx,
            active_comprehension_binders: Vec::new(),
        }
    }

    fn reference_is_active_comprehension_binder(&self, reference: &rumoca_core::Reference) -> bool {
        let path = ComponentPath::from_reference(reference);
        let [part] = path.parts() else {
            return false;
        };
        self.active_comprehension_binders
            .iter()
            .rev()
            .any(|binder| binder == part)
    }
}

impl ExpressionRewriter for FunctionOverrideExpressionRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        if let Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
        {
            let rewritten_subscripts = self.rewrite_subscripts(subscripts);
            if self.reference_is_active_comprehension_binder(name)
                || reference_targets_function_local_def(name, self.ctx)
            {
                return Expression::VarRef {
                    name: name.clone(),
                    subscripts: rewritten_subscripts,
                    span: *span,
                };
            }
            if let Some(resolved_name) = resolve_override_member_name(name, self.ctx) {
                return Expression::VarRef {
                    name: rewritten_reference(name, resolved_name, self.ctx),
                    subscripts: rewritten_subscripts,
                    span: *span,
                };
            }
            if let Some(canonical_name) = canonical_instance_reference_name(name, self.ctx) {
                return Expression::VarRef {
                    name: canonical_name,
                    subscripts: rewritten_subscripts,
                    span: *span,
                };
            }
            return Expression::VarRef {
                name: name.clone(),
                subscripts: rewritten_subscripts,
                span: *span,
            };
        }

        if let Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        {
            let rewritten_args = self.rewrite_expressions(args);
            if reference_targets_function_local_def(name, self.ctx) {
                return Expression::FunctionCall {
                    name: name.clone(),
                    args: rewritten_args,
                    is_constructor: *is_constructor,
                    span: *span,
                };
            }
            let Some(resolved_name) =
                resolve_function_override_name(name, *is_constructor, self.ctx)
            else {
                return Expression::FunctionCall {
                    name: name.clone(),
                    args: rewritten_args,
                    is_constructor: *is_constructor,
                    span: *span,
                };
            };
            let args = append_replaceable_function_modifier_args(
                name,
                &resolved_name,
                rewritten_args,
                self.ctx,
            );
            return Expression::FunctionCall {
                name: rewritten_reference(name, resolved_name, self.ctx),
                args,
                is_constructor: *is_constructor,
                span: *span,
            };
        }
        self.walk_expression(expr)
    }

    fn walk_array_comprehension_expression(
        &mut self,
        expr: &Expression,
        indices: &[rumoca_core::ComprehensionIndex],
        filter: Option<&Expression>,
        span: rumoca_core::Span,
    ) -> Expression {
        let binder_depth = self.active_comprehension_binders.len();
        let mut rewritten_indices = Vec::with_capacity(indices.len());
        for index in indices {
            rewritten_indices.push(rumoca_core::ComprehensionIndex {
                name: index.name.clone(),
                range: self.rewrite_expression(&index.range),
            });
            self.active_comprehension_binders.push(index.name.clone());
        }
        let rewritten_expr = Box::new(self.rewrite_expression(expr));
        let rewritten_filter = filter.map(|filter| Box::new(self.rewrite_expression(filter)));
        self.active_comprehension_binders.truncate(binder_depth);
        Expression::ArrayComprehension {
            expr: rewritten_expr,
            indices: rewritten_indices,
            filter: rewritten_filter,
            span,
        }
    }
}

impl StatementRewriter for FunctionOverrideExpressionRewriter<'_> {}

pub(super) fn function_local_def_ids(
    function: &rumoca_core::Function,
) -> FxHashSet<rumoca_core::DefId> {
    function
        .inputs
        .iter()
        .chain(function.outputs.iter())
        .chain(function.locals.iter())
        .filter_map(|param| param.def_id)
        .collect()
}

fn reference_targets_function_local_def(
    reference: &rumoca_core::Reference,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> bool {
    reference
        .target_def_id()
        .is_some_and(|def_id| ctx.local_def_ids.contains(&def_id))
}

fn rewritten_reference(
    original: &rumoca_core::Reference,
    resolved_name: String,
    ctx: &FunctionOverrideRewriteContext<'_>,
) -> rumoca_core::Reference {
    rewritten_function_reference(original, resolved_name, ctx.tree, ctx.class_index)
}

pub(super) fn rewritten_function_reference(
    original: &rumoca_core::Reference,
    resolved_name: String,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> rumoca_core::Reference {
    let Some(mut component_ref) = original.component_ref().cloned() else {
        return rumoca_core::Reference::new(resolved_name);
    };
    component_ref.def_id = tree.name_map.get(&resolved_name).copied().or_else(|| {
        class_index
            .get_by_qualified_name(&resolved_name)
            .and_then(|class_def| class_def.def_id)
    });
    component_ref.parts = ComponentPath::from_flat_path(&resolved_name)
        .parts()
        .iter()
        .map(|part| rumoca_core::ComponentRefPart {
            ident: part.clone(),
            span: component_ref.span,
            subs: Vec::new(),
        })
        .collect();
    rumoca_core::Reference::with_component_reference(resolved_name, component_ref)
}
