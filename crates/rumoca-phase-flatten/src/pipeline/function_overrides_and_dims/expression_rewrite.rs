use super::*;

pub(super) fn expression_contains_function_call(expr: &Expression) -> bool {
    match expr {
        Expression::FunctionCall { .. } => true,
        Expression::StringConversion { value, format, .. } => {
            expression_contains_function_call(value)
                || format.operands().any(expression_contains_function_call)
        }
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
    error: Option<FlattenError>,
}

impl<'a> FunctionOverrideExpressionRewriter<'a> {
    pub(super) fn new(ctx: &'a FunctionOverrideRewriteContext<'a>) -> Self {
        Self {
            ctx,
            active_comprehension_binders: Vec::new(),
            error: None,
        }
    }

    pub(super) fn finish(self) -> Result<(), FlattenError> {
        match self.error {
            Some(error) => Err(error),
            None => Ok(()),
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

    fn rewrite_variable_reference(&mut self, expr: &Expression) -> Option<Expression> {
        let Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
        else {
            return None;
        };
        let subscripts = self.rewrite_subscripts(subscripts);
        // Only an override projection renames a value reference. Every other
        // occurrence keeps the flat name its instantiation produced: the
        // declaration segments are relative to their declaring class, so
        // rebuilding a name from them would drop the instance prefix.
        let name = if self.reference_is_active_comprehension_binder(name)
            || reference_targets_function_local_def(name, self.ctx)
        {
            name.clone()
        } else {
            resolve_override_member_name(name, self.ctx).map_or_else(
                || name.clone(),
                |resolved| rewritten_value_reference(name, resolved),
            )
        };
        Some(Expression::VarRef {
            name,
            subscripts,
            span: *span,
        })
    }

    fn rewrite_function_call(&mut self, expr: &Expression) -> Option<Expression> {
        let Expression::FunctionCall {
            name,
            args,
            is_constructor,
            span,
        } = expr
        else {
            return None;
        };
        let args = self.rewrite_expressions(args);
        if reference_targets_function_local_def(name, self.ctx) {
            return Some(function_call(name.clone(), args, *is_constructor, *span));
        }
        let rewrite = match resolve_exact_function_rewrite(name, *is_constructor, self.ctx, *span) {
            Ok(Some(rewrite)) => rewrite,
            Ok(None) => return Some(function_call(name.clone(), args, *is_constructor, *span)),
            Err(error) => {
                self.error.get_or_insert(error);
                return Some(function_call(name.clone(), args, *is_constructor, *span));
            }
        };
        Some(self.rewrite_selected_call(name, args, *is_constructor, *span, &rewrite))
    }

    fn rewrite_selected_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: Vec<Expression>,
        is_constructor: bool,
        span: rumoca_core::Span,
        rewrite: &ResolvedFunctionRewrite,
    ) -> Expression {
        let args = if is_constructor {
            args
        } else {
            match append_replaceable_function_modifier_args(
                name,
                rewrite.selection,
                args.clone(),
                self.ctx,
                span,
            ) {
                Ok(args) => args,
                Err(error) => {
                    self.error.get_or_insert(error);
                    args
                }
            }
        };
        function_call(
            rewritten_selected_function_reference(name, rewrite),
            args,
            is_constructor,
            span,
        )
    }
}

impl ExpressionRewriter for FunctionOverrideExpressionRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        if let Some(rewritten) = self.rewrite_variable_reference(expr) {
            return rewritten;
        }
        if let Some(rewritten) = self.rewrite_function_call(expr) {
            return rewritten;
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

fn function_call(
    name: rumoca_core::Reference,
    args: Vec<Expression>,
    is_constructor: bool,
    span: rumoca_core::Span,
) -> Expression {
    Expression::FunctionCall {
        name,
        args,
        is_constructor,
        span,
    }
}

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

fn rewritten_value_reference(
    original: &rumoca_core::Reference,
    resolved_name: String,
) -> rumoca_core::Reference {
    original.with_var_name(rumoca_core::VarName::new(resolved_name))
}

pub(super) fn rewritten_function_reference(
    original: &rumoca_core::Reference,
    resolved_name: String,
    tree: &ClassTree,
    class_index: &rumoca_ir_ast::ClassDefIndex<'_>,
) -> rumoca_core::Reference {
    let target_def_id = tree
        .name_map
        .get(&resolved_name)
        .copied()
        .or_else(|| {
            class_index
                .get_by_qualified_name(&resolved_name)
                .and_then(|class_def| class_def.def_id)
        })
        .expect("resolved function rewrite requires an exact target DefId");
    retarget_function_reference(original, resolved_name, target_def_id)
}

fn rewritten_selected_function_reference(
    original: &rumoca_core::Reference,
    rewrite: &ResolvedFunctionRewrite,
) -> rumoca_core::Reference {
    let target_def_id = match rewrite.occurrence_identity {
        CallOccurrenceIdentity::SelectedImplementation => rewrite.selection.implementation,
        CallOccurrenceIdentity::ExposedDeclaration => rewrite.selection.exposure,
    };
    retarget_function_reference(original, rewrite.display_name.clone(), target_def_id)
}

fn retarget_function_reference(
    original: &rumoca_core::Reference,
    resolved_name: String,
    target_def_id: rumoca_core::DefId,
) -> rumoca_core::Reference {
    let Some(component_ref) = original.component_ref() else {
        return original.with_var_name(rumoca_core::VarName::new(resolved_name));
    };
    let mut parts = component_ref.parts().to_vec();
    parts
        .last_mut()
        .expect("checked component references are nonempty")
        .def_id = target_def_id;
    let component_ref = component_ref
        .with_replaced_parts(parts)
        .expect("rewriting one exact target preserves a checked component reference");
    original.with_rewritten_component_reference(resolved_name, component_ref)
}
