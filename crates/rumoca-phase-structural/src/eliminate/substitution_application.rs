use rumoca_core::{ExpressionRewriter, Literal, OpUnary};
use rumoca_ir_dae::DaeExpressionRewriter;

use super::{Dae, Expression, OpBinary, Substitution, apply_substitutions_to_expr};

pub(super) fn equation_analysis_expr(eq: &rumoca_ir_dae::Equation) -> Expression {
    let Some(lhs) = eq.lhs.as_ref() else {
        return eq.rhs.clone();
    };
    subtraction(
        Expression::VarRef {
            name: lhs.clone(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        },
        eq.rhs.clone(),
    )
}

pub(super) fn apply_substitutions_to_remaining_once(
    dae: &mut Dae,
    eliminated_eq_flags: &[bool],
    substitutions: &[Substitution],
) {
    if substitutions.is_empty() {
        return;
    }
    for (i, eq) in dae.continuous.equations.iter_mut().enumerate() {
        if *eliminated_eq_flags
            .get(i)
            .expect("eliminated equation flags must cover every continuous equation")
        {
            continue;
        }
        let rhs = apply_substitutions_in_order(&eq.rhs, substitutions);
        let Some(lhs) = eq.lhs.as_ref() else {
            eq.rhs = rhs;
            continue;
        };
        let lhs_expr = Expression::VarRef {
            name: lhs.clone(),
            subscripts: Vec::new(),
            span: rumoca_core::Span::DUMMY,
        };
        let substituted_lhs = apply_substitutions_in_order(&lhs_expr, substitutions);
        if substituted_lhs == lhs_expr {
            eq.rhs = rhs;
        } else {
            eq.lhs = None;
            eq.rhs = subtraction(substituted_lhs, rhs);
        }
    }
}

pub(super) fn apply_substitutions_to_dae_partitions(dae: &mut Dae, substitutions: &[Substitution]) {
    if substitutions.is_empty() {
        return;
    }
    SubstitutionDaeRewriter { substitutions }.rewrite_dae(dae);
}

pub(super) fn apply_substitutions_in_order(
    expr: &Expression,
    substitutions: &[Substitution],
) -> Expression {
    let substituted = apply_substitutions_to_expr(expr, substitutions);
    simplify_arithmetic_identities(substituted)
}

/// Fold exact arithmetic identities introduced by substitution.
///
/// Substituting a literal 0 into `x - s_support` produces `x - 0`, which the
/// symbolic solver in `try_solve_for_unknown` cannot see through (it only
/// matches the unknown when it's the direct lhs/rhs of a top-level Sub). This
/// pass canonicalises the expression so equation residues like `x - (y - 0)`
/// reduce to the solvable `x - y` form.
///
/// Only handles identities that are mathematically exact across all numeric
/// types — division-by-zero and `0^0` are intentionally not folded.
fn simplify_arithmetic_identities(expr: Expression) -> Expression {
    match expr {
        Expression::Binary { op, lhs, rhs, span } => {
            let lhs = simplify_arithmetic_identities(*lhs);
            let rhs = simplify_arithmetic_identities(*rhs);
            match op {
                OpBinary::Add => {
                    if is_numeric_zero(&lhs) {
                        return rhs;
                    }
                    if is_numeric_zero(&rhs) {
                        return lhs;
                    }
                }
                OpBinary::Sub => {
                    if is_numeric_zero(&rhs) {
                        return lhs;
                    }
                    if is_numeric_zero(&lhs) {
                        return negate(rhs, span);
                    }
                }
                OpBinary::Mul => {
                    if is_numeric_zero(&lhs) || is_numeric_zero(&rhs) {
                        return zero_literal(span);
                    }
                    if is_numeric_one(&lhs) {
                        return rhs;
                    }
                    if is_numeric_one(&rhs) {
                        return lhs;
                    }
                }
                OpBinary::Div if is_numeric_one(&rhs) => {
                    return lhs;
                }
                _ => {}
            }
            Expression::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span,
            }
        }
        Expression::Unary { op, rhs, span } => {
            let inner = simplify_arithmetic_identities(*rhs);
            if matches!(op, OpUnary::Minus) {
                // -(-x) → x
                if let Expression::Unary {
                    op: OpUnary::Minus,
                    rhs: inner_inner,
                    ..
                } = inner
                {
                    return *inner_inner;
                }
                // -0 → 0
                if is_numeric_zero(&inner) {
                    return inner;
                }
            }
            Expression::Unary {
                op,
                rhs: Box::new(inner),
                span,
            }
        }
        _ => expr,
    }
}

fn is_numeric_zero(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Literal {
            value: Literal::Real(v),
            ..
        } if *v == 0.0
    ) || matches!(
        expr,
        Expression::Literal {
            value: Literal::Integer(0),
            ..
        }
    )
}

fn is_numeric_one(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::Literal {
            value: Literal::Real(v),
            ..
        } if *v == 1.0
    ) || matches!(
        expr,
        Expression::Literal {
            value: Literal::Integer(1),
            ..
        }
    )
}

fn negate(expr: Expression, span: rumoca_core::Span) -> Expression {
    Expression::Unary {
        op: OpUnary::Minus,
        rhs: Box::new(expr),
        span,
    }
}

fn zero_literal(span: rumoca_core::Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(0.0),
        span,
    }
}

fn apply_substitutions_to_equation(
    eq: &mut rumoca_ir_dae::Equation,
    substitutions: &[Substitution],
) {
    let rhs = apply_substitutions_in_order(&eq.rhs, substitutions);
    let Some(lhs) = eq.lhs.as_ref() else {
        eq.rhs = rhs;
        return;
    };
    let lhs_expr = Expression::VarRef {
        name: lhs.clone(),
        subscripts: Vec::new(),
        span: rumoca_core::Span::DUMMY,
    };
    let substituted_lhs = apply_substitutions_in_order(&lhs_expr, substitutions);
    if substituted_lhs == lhs_expr {
        eq.rhs = rhs;
    } else {
        eq.lhs = None;
        eq.rhs = subtraction(substituted_lhs, rhs);
    }
}

struct SubstitutionDaeRewriter<'a> {
    substitutions: &'a [Substitution],
}

impl ExpressionRewriter for SubstitutionDaeRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &Expression) -> Expression {
        apply_substitutions_in_order(expr, self.substitutions)
    }
}

impl DaeExpressionRewriter for SubstitutionDaeRewriter<'_> {
    fn rewrite_equation(&mut self, equation: &mut rumoca_ir_dae::Equation) {
        apply_substitutions_to_equation(equation, self.substitutions);
    }
}

fn subtraction(lhs: Expression, rhs: Expression) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span: rumoca_core::Span::DUMMY,
    }
}
