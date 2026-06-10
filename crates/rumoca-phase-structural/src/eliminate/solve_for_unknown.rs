use super::*;

/// Try to solve `0 = rhs` for `unknown` symbolically.
///
/// Handles the common residual patterns produced by todae:
/// - `0 = z`              -> `z = 0`
/// - `0 = z - expr`       -> `z = expr`
/// - `0 = expr - z`       -> `z = expr`
/// - `0 = -(z - expr)`    -> `z = expr`
/// - `0 = -(expr - z)`    -> `z = expr`
pub fn try_solve_for_unknown(rhs: &Expression, unknown: &VarName) -> Option<Expression> {
    match rhs {
        // Pattern: 0 = z  ->  z = 0
        Expression::VarRef { span, .. } if is_symbolic_solve_target(rhs, unknown) => {
            Some(Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span: *span,
            })
        }
        // Pattern: 0 = lhs - rhs_inner (Binary Sub)
        Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs: rhs_inner,
            ..
        } => {
            // 0 = z - expr -> z = expr
            if is_symbolic_solve_target(lhs, unknown) && !expr_contains_var(rhs_inner, unknown) {
                return Some(*rhs_inner.clone());
            }
            // 0 = expr - z -> z = expr
            if is_symbolic_solve_target(rhs_inner, unknown) && !expr_contains_var(lhs, unknown) {
                return Some(*lhs.clone());
            }
            None
        }
        // Pattern: 0 = -(something) (Unary Minus)
        Expression::Unary {
            op: OpUnary::Minus,
            rhs: inner,
            ..
        } => {
            // Recurse into the negated expression.
            // -(z - expr) has the same solutions as (z - expr).
            try_solve_for_unknown(inner, unknown)
        }
        _ => None,
    }
}

fn is_symbolic_solve_target(expr: &Expression, unknown: &VarName) -> bool {
    match expr {
        Expression::VarRef {
            name, subscripts, ..
        } => {
            var_ref_matches_unknown(name, subscripts, unknown)
                && !rumoca_ir_dae::complex_base_alias_match(name.as_str(), unknown.as_str())
        }
        _ => false,
    }
}
