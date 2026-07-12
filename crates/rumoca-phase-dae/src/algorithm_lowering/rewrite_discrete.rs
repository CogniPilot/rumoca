use super::*;
use rumoca_core::ExpressionRewriter;

pub(super) fn discrete_assignment_rhs_var_name(expr: &Expression) -> Option<VarName> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    Some(varref_with_subscripts(name, subscripts))
}

pub(super) fn rewrite_discrete_self_refs_to_pre(
    expr: &Expression,
    target: &VarName,
    owner_span: Span,
) -> Result<Expression, ToDaeError> {
    let mut rewriter = DiscreteSelfRefRewriter {
        target,
        owner_span,
        error: None,
    };
    let rewritten = rewriter.rewrite_expression(expr);
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(rewritten),
    }
}

struct DiscreteSelfRefRewriter<'a> {
    target: &'a VarName,
    owner_span: Span,
    error: Option<ToDaeError>,
}

impl ExpressionRewriter for DiscreteSelfRefRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        if self.error.is_some() {
            return expr.clone();
        }
        match expr {
            Expression::VarRef {
                name,
                subscripts,
                span,
            } if name.var_name() == self.target => {
                match pre_target_expr_with_subscripts(
                    self.target,
                    subscripts,
                    self.self_ref_provenance_span(*span),
                ) {
                    Ok(expr) => expr,
                    Err(error) => {
                        self.error = Some(error);
                        expr.clone()
                    }
                }
            }
            Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                ..
            } => expr.clone(),
            Expression::FunctionCall { name, .. }
                if matches!(
                    rumoca_core::source_temporal_function_short_name(name.as_str()),
                    Some("pre" | "previous")
                ) =>
            {
                expr.clone()
            }
            Expression::If {
                branches,
                else_branch,
                span,
            } => self.rewrite_if(branches, else_branch, *span),
            Expression::Index {
                base,
                subscripts,
                span,
            } => Expression::Index {
                base: Box::new(self.rewrite_expression(base)),
                subscripts: subscripts.to_vec(),
                span: *span,
            },
            _ => self.walk_expression(expr),
        }
    }
}

impl DiscreteSelfRefRewriter<'_> {
    fn self_ref_provenance_span(&self, span: Span) -> Span {
        if span.is_dummy() {
            self.owner_span
        } else {
            span
        }
    }

    fn rewrite_if(
        &mut self,
        branches: &[(Expression, Expression)],
        else_branch: &Expression,
        span: rumoca_core::Span,
    ) -> Expression {
        Expression::If {
            branches: branches
                .iter()
                .map(|(cond, value)| {
                    (
                        self.rewrite_expression(cond),
                        self.rewrite_branch_value(cond, value),
                    )
                })
                .collect(),
            else_branch: Box::new(self.rewrite_expression(else_branch)),
            span,
        }
    }

    fn rewrite_branch_value(&mut self, cond: &Expression, value: &Expression) -> Expression {
        if is_initial_builtin(cond) && is_direct_target_var_ref(value, self.target) {
            // MLS §8.6: the preserved initial-section value in
            // `if initial() then x else pre(x)` must stay as the current `x`
            // instead of being rewritten back to `pre(x)`.
            return value.clone();
        }
        self.rewrite_expression(value)
    }
}

fn pre_target_expr_with_subscripts(
    target: &VarName,
    subscripts: &[Subscript],
    span: Span,
) -> Result<Expression, ToDaeError> {
    Ok(Expression::BuiltinCall {
        function: BuiltinFunction::Pre,
        args: vec![current_target_expr_with_subscripts(
            target, subscripts, span,
        )?],
        span,
    })
}

fn current_target_expr_with_subscripts(
    target: &VarName,
    subscripts: &[Subscript],
    span: Span,
) -> Result<Expression, ToDaeError> {
    Ok(Expression::VarRef {
        name: structured_target_reference(target, span)?,
        subscripts: subscripts.to_vec(),
        span,
    })
}

fn is_initial_builtin(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::BuiltinCall {
            function: BuiltinFunction::Initial,
            args, ..
        } if args.is_empty()
    )
}

fn is_direct_target_var_ref(expr: &Expression, target: &VarName) -> bool {
    matches!(
        expr,
        Expression::VarRef { name, subscripts, .. }
            if name.var_name() == target && subscripts.is_empty()
    )
}
