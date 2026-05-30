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

pub(super) fn rewrite_discrete_self_refs_to_pre(expr: &Expression, target: &VarName) -> Expression {
    DiscreteSelfRefRewriter { target }.rewrite_expression(expr)
}

struct DiscreteSelfRefRewriter<'a> {
    target: &'a VarName,
}

impl ExpressionRewriter for DiscreteSelfRefRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        match expr {
            Expression::VarRef { name, .. } if name.var_name() == self.target => {
                pre_target_expr(self.target)
            }
            Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                ..
            } => expr.clone(),
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
