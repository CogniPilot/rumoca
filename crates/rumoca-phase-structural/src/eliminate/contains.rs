use super::{Expression, VarName, var_ref_matches_unknown};

/// Check if an expression references a variable (by base name).
pub fn expr_contains_var(expr: &Expression, var: &VarName) -> bool {
    match expr {
        Expression::VarRef { name, subscripts } => {
            if var_ref_matches_unknown(name, subscripts, var) {
                return true;
            }
            subscripts.iter().any(|s| match s {
                rumoca_ir_dae::Subscript::Expr(e) => expr_contains_var(e, var),
                _ => false,
            })
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_contains_var(lhs, var) || expr_contains_var(rhs, var)
        }
        Expression::Unary { rhs, .. } => expr_contains_var(rhs, var),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(|a| expr_contains_var(a, var))
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            branches
                .iter()
                .any(|(c, v)| expr_contains_var(c, var) || expr_contains_var(v, var))
                || expr_contains_var(else_branch, var)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(|e| expr_contains_var(e, var))
        }
        Expression::Range { start, step, end } => {
            expr_contains_var(start, var)
                || step.as_ref().is_some_and(|s| expr_contains_var(s, var))
                || expr_contains_var(end, var)
        }
        Expression::Index { base, subscripts } => {
            expr_contains_var(base, var)
                || subscripts.iter().any(|s| match s {
                    rumoca_ir_dae::Subscript::Expr(e) => expr_contains_var(e, var),
                    _ => false,
                })
        }
        Expression::ArrayComprehension { expr, filter, .. } => {
            expr_contains_var(expr, var)
                || filter.as_ref().is_some_and(|f| expr_contains_var(f, var))
        }
        Expression::FieldAccess { base, .. } => expr_contains_var(base, var),
        Expression::Literal(_) | Expression::Empty => false,
    }
}
