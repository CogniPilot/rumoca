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
            span,
        } => {
            // 0 = z - expr -> z = expr
            if is_symbolic_solve_target(lhs, unknown) && !expr_contains_var(rhs_inner, unknown) {
                return Some(*rhs_inner.clone());
            }
            // 0 = expr - z -> z = expr
            if is_symbolic_solve_target(rhs_inner, unknown) && !expr_contains_var(lhs, unknown) {
                return Some(*lhs.clone());
            }
            solve_unit_affine_residual(rhs, unknown, *span)
        }
        Expression::Binary {
            op: OpBinary::Add,
            span,
            ..
        } => solve_unit_affine_residual(rhs, unknown, *span),
        Expression::Unary {
            op: OpUnary::Plus,
            rhs: inner,
            ..
        } => try_solve_for_unknown(inner, unknown),
        Expression::Unary {
            op: OpUnary::Minus,
            rhs: inner,
            ..
        } => {
            // Recurse into the negated expression.
            // -(z - expr) has the same solutions as (z - expr).
            try_solve_for_unknown(inner, unknown)
        }
        Expression::If {
            branches,
            else_branch,
            span,
        } => solve_if_residual(branches, else_branch, unknown, *span),
        _ => None,
    }
}

fn solve_if_residual(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    unknown: &VarName,
    span: rumoca_core::Span,
) -> Option<Expression> {
    let mut solved_branches = Vec::with_capacity(branches.len());
    for (condition, branch_residual) in branches {
        if expr_contains_var(condition, unknown) {
            return None;
        }
        solved_branches.push((
            condition.clone(),
            try_solve_for_unknown(branch_residual, unknown)?,
        ));
    }
    Some(Expression::If {
        branches: solved_branches,
        else_branch: Box::new(try_solve_for_unknown(else_branch, unknown)?),
        span,
    })
}

fn solve_unit_affine_residual(
    rhs: &Expression,
    unknown: &VarName,
    residual_span: rumoca_core::Span,
) -> Option<Expression> {
    let (coef, remainder) = split_unit_affine_residual(rhs, unknown, residual_span)?;
    match coef {
        1 => Some(negate_expr(remainder, residual_span)),
        -1 => Some(remainder),
        _ => None,
    }
}

fn split_unit_affine_residual(
    expr: &Expression,
    unknown: &VarName,
    span: rumoca_core::Span,
) -> Option<(i32, Expression)> {
    if is_symbolic_solve_target(expr, unknown) {
        return Some((
            1,
            Expression::Literal {
                value: rumoca_core::Literal::Real(0.0),
                span,
            },
        ));
    }
    let Expression::Binary { op, lhs, rhs, .. } = expr else {
        return None;
    };
    match op {
        OpBinary::Add | OpBinary::AddElem => {
            if let Some((coef, rem)) = split_unit_affine_residual(lhs, unknown, span)
                && !expr_contains_var(rhs, unknown)
            {
                return Some((coef, add_expr(rem, *rhs.clone(), span)));
            }
            if let Some((coef, rem)) = split_unit_affine_residual(rhs, unknown, span)
                && !expr_contains_var(lhs, unknown)
            {
                return Some((coef, add_expr(*lhs.clone(), rem, span)));
            }
            None
        }
        OpBinary::Sub | OpBinary::SubElem => {
            if let Some((coef, rem)) = split_unit_affine_residual(lhs, unknown, span)
                && !expr_contains_var(rhs, unknown)
            {
                return Some((coef, sub_expr(rem, *rhs.clone(), span)));
            }
            if let Some((coef, rem)) = split_unit_affine_residual(rhs, unknown, span)
                && !expr_contains_var(lhs, unknown)
            {
                return Some((-coef, sub_expr(*lhs.clone(), rem, span)));
            }
            None
        }
        _ => None,
    }
}

fn add_expr(lhs: Expression, rhs: Expression, span: rumoca_core::Span) -> Expression {
    if is_zero_literal(&lhs) {
        return rhs;
    }
    if is_zero_literal(&rhs) {
        return lhs;
    }
    Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn sub_expr(lhs: Expression, rhs: Expression, span: rumoca_core::Span) -> Expression {
    if is_zero_literal(&rhs) {
        return lhs;
    }
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn negate_expr(expr: Expression, span: rumoca_core::Span) -> Expression {
    if let Expression::Unary {
        op: OpUnary::Minus,
        rhs,
        ..
    } = expr
    {
        return *rhs;
    }
    Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(expr),
        span,
    }
}

fn is_zero_literal(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } if *value == 0.0
    ) || matches!(
        expr,
        Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } if *value == 0
    )
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
