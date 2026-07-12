use std::collections::HashMap;

use rumoca_core::{Literal, OpUnary};

use super::{
    Dae, Expression, OpBinary, Substitution, VarName,
    apply_substitutions_to_expr_with_derivatives_and_dae, exact_subscript_index_in_dae,
    project_index_expr_with_exact_subscripts_in_dae,
};
use crate::StructuralError;

pub(super) fn equation_analysis_expr(eq: &rumoca_ir_dae::Equation) -> Expression {
    let Some(lhs) = eq.lhs.as_ref() else {
        return eq.rhs.clone();
    };
    subtraction(
        Expression::VarRef {
            name: lhs.clone(),
            subscripts: Vec::new(),
            span: eq.span,
        },
        eq.rhs.clone(),
        eq.span,
    )
}

pub(super) fn canonicalize_exact_indexing_in_continuous_equations(
    dae: &mut Dae,
) -> Result<(), StructuralError> {
    let dae_context = dae.clone();
    let mut touched_equations = Vec::new();
    for (index, equation) in dae.continuous.equations.iter_mut().enumerate() {
        let original_rhs = equation.rhs.clone();
        equation.rhs = simplify_after_substitution(original_rhs.clone(), &dae_context);
        if equation.rhs != original_rhs {
            touched_equations.push(index);
        }
    }
    super::drop_structured_families_touching_equations(dae, &touched_equations);
    Ok(())
}

pub(super) fn apply_substitutions_to_remaining_once(
    dae: &mut Dae,
    eliminated_eq_flags: &[bool],
    substitutions: &[Substitution],
) -> Result<(), StructuralError> {
    if substitutions.is_empty() {
        return Ok(());
    }
    let derivative_source = dae.clone();
    let mut derivative_replacements = DerivativeReplacementCache::new(&derivative_source);
    let mut touched_equations = Vec::new();
    for (i, eq) in dae.continuous.equations.iter_mut().enumerate() {
        let eliminated = eliminated_eq_flags.get(i).ok_or_else(|| {
            StructuralError::ContractViolation {
                reason: format!(
                    "eliminated equation flags have length {} but continuous equation {i} exists",
                    eliminated_eq_flags.len()
                ),
                span: eq.span,
            }
        })?;
        if *eliminated {
            continue;
        }
        let original_lhs = eq.lhs.clone();
        let original_rhs = eq.rhs.clone();
        let rhs = apply_substitutions_in_order_with_derivatives(
            &eq.rhs,
            substitutions,
            &derivative_source,
            &mut derivative_replacements,
        )?;
        let Some(lhs) = eq.lhs.as_ref() else {
            eq.rhs = rhs;
            if eq.rhs != original_rhs {
                touched_equations.push(i);
            }
            continue;
        };
        let lhs_expr = Expression::VarRef {
            name: lhs.clone(),
            subscripts: Vec::new(),
            span: eq.span,
        };
        let substituted_lhs = apply_substitutions_in_order_with_derivatives_and_dae(
            &lhs_expr,
            substitutions,
            &derivative_source,
            &mut derivative_replacements,
        )?;
        if substituted_lhs == lhs_expr {
            eq.rhs = rhs;
        } else {
            eq.lhs = None;
            eq.rhs = subtraction(substituted_lhs, rhs, eq.span);
        }
        if eq.lhs != original_lhs || eq.rhs != original_rhs {
            touched_equations.push(i);
        }
    }
    super::drop_structured_families_touching_equations(dae, &touched_equations);
    Ok(())
}

pub(super) fn apply_substitutions_to_dae_partitions(
    dae: &mut Dae,
    substitutions: &[Substitution],
) -> Result<(), StructuralError> {
    if substitutions.is_empty() {
        return Ok(());
    }
    let derivative_source = dae.clone();
    let mut rewriter = SubstitutionDaeRewriter {
        substitutions,
        derivative_replacements: DerivativeReplacementCache::new(&derivative_source),
    };
    rewriter.rewrite_dae(dae)
}

fn apply_substitutions_in_order_with_derivatives_and_dae(
    expr: &Expression,
    substitutions: &[Substitution],
    dae_context: &Dae,
    derivative_replacements: &mut DerivativeReplacementCache<'_>,
) -> Result<Expression, StructuralError> {
    let substituted = apply_substitutions_to_expr_with_derivatives_and_dae(
        expr,
        substitutions,
        Some(dae_context),
        |sub| derivative_replacements.replacement_for(sub),
    )?;
    Ok(simplify_after_substitution(substituted, dae_context))
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
fn simplify_after_substitution(expr: Expression, dae_context: &Dae) -> Expression {
    match expr {
        Expression::Binary { op, lhs, rhs, span } => {
            let lhs = simplify_after_substitution(*lhs, dae_context);
            let rhs = simplify_after_substitution(*rhs, dae_context);
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
            let inner = simplify_after_substitution(*rhs, dae_context);
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
        Expression::Index {
            base,
            subscripts,
            span,
        } => {
            let base = simplify_after_substitution(*base, dae_context);
            let subscripts = subscripts
                .into_iter()
                .map(|subscript| simplify_subscript_after_substitution(subscript, dae_context))
                .collect::<Vec<_>>();
            project_index_expr_with_exact_subscripts_in_dae(dae_context, &base, &subscripts, span)
                .unwrap_or_else(|| Expression::Index {
                    base: Box::new(base),
                    subscripts,
                    span,
                })
        }
        Expression::VarRef {
            name,
            subscripts,
            span,
        } => Expression::VarRef {
            name,
            subscripts: subscripts
                .into_iter()
                .map(|subscript| simplify_subscript_after_substitution(subscript, dae_context))
                .collect(),
            span,
        },
        _ => expr,
    }
}

fn simplify_subscript_after_substitution(
    subscript: rumoca_core::Subscript,
    dae_context: &Dae,
) -> rumoca_core::Subscript {
    match subscript {
        rumoca_core::Subscript::Expr { expr, span } => {
            let expr = simplify_after_substitution(*expr, dae_context);
            let subscript = rumoca_core::Subscript::Expr {
                expr: Box::new(expr),
                span,
            };
            if let Some(value) = exact_subscript_index_in_dae(dae_context, &subscript) {
                rumoca_core::Subscript::Index { value, span }
            } else {
                subscript
            }
        }
        other => other,
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
    derivative_replacements: &mut DerivativeReplacementCache<'_>,
) -> Result<(), StructuralError> {
    let rhs = apply_substitutions_in_order_with_derivatives_and_dae(
        &eq.rhs,
        substitutions,
        derivative_replacements.dae,
        derivative_replacements,
    )?;
    let Some(lhs) = eq.lhs.as_ref() else {
        eq.rhs = rhs;
        return Ok(());
    };
    let lhs_expr = Expression::VarRef {
        name: lhs.clone(),
        subscripts: Vec::new(),
        span: eq.span,
    };
    let substituted_lhs = apply_substitutions_in_order_with_derivatives_and_dae(
        &lhs_expr,
        substitutions,
        derivative_replacements.dae,
        derivative_replacements,
    )?;
    if substituted_lhs == lhs_expr {
        eq.rhs = rhs;
    } else {
        eq.lhs = None;
        eq.rhs = subtraction(substituted_lhs, rhs, eq.span);
    }
    Ok(())
}

struct SubstitutionDaeRewriter<'a> {
    substitutions: &'a [Substitution],
    derivative_replacements: DerivativeReplacementCache<'a>,
}

impl SubstitutionDaeRewriter<'_> {
    fn rewrite_dae(&mut self, dae: &mut Dae) -> Result<(), StructuralError> {
        let touched_equations =
            self.rewrite_equations_collecting_changes(&mut dae.continuous.equations)?;
        super::drop_structured_families_touching_equations(dae, &touched_equations);
        self.rewrite_equations(&mut dae.initialization.equations)?;
        self.rewrite_equations(&mut dae.discrete.real_updates)?;
        self.rewrite_equations(&mut dae.discrete.valued_updates)?;
        self.rewrite_equations(&mut dae.conditions.equations)?;
        self.rewrite_expression_slots(&mut dae.conditions.relations)?;
        self.rewrite_expression_slots(&mut dae.events.synthetic_root_conditions)?;
        self.rewrite_event_actions(&mut dae.events.event_actions)?;
        self.rewrite_expression_slots(&mut dae.clocks.constructor_exprs)?;
        self.rewrite_expression_slots(&mut dae.clocks.triggered_conditions)?;
        Ok(())
    }

    fn rewrite_equations(
        &mut self,
        equations: &mut [rumoca_ir_dae::Equation],
    ) -> Result<(), StructuralError> {
        for equation in equations {
            self.rewrite_equation(equation)?;
        }
        Ok(())
    }

    fn rewrite_equations_collecting_changes(
        &mut self,
        equations: &mut [rumoca_ir_dae::Equation],
    ) -> Result<Vec<usize>, StructuralError> {
        let mut touched = Vec::new();
        for (index, equation) in equations.iter_mut().enumerate() {
            let original_lhs = equation.lhs.clone();
            let original_rhs = equation.rhs.clone();
            self.rewrite_equation(equation)?;
            if equation.lhs != original_lhs || equation.rhs != original_rhs {
                touched.push(index);
            }
        }
        Ok(touched)
    }

    fn rewrite_equation(
        &mut self,
        equation: &mut rumoca_ir_dae::Equation,
    ) -> Result<(), StructuralError> {
        apply_substitutions_to_equation(
            equation,
            self.substitutions,
            &mut self.derivative_replacements,
        )
    }

    fn rewrite_expression(&mut self, expr: &Expression) -> Result<Expression, StructuralError> {
        apply_substitutions_in_order_with_derivatives_and_dae(
            expr,
            self.substitutions,
            self.derivative_replacements.dae,
            &mut self.derivative_replacements,
        )
    }

    fn rewrite_expression_slots(
        &mut self,
        expressions: &mut [Expression],
    ) -> Result<(), StructuralError> {
        for expression in expressions {
            *expression = self.rewrite_expression(expression)?;
        }
        Ok(())
    }

    fn rewrite_event_actions(
        &mut self,
        actions: &mut [rumoca_ir_dae::DaeEventAction],
    ) -> Result<(), StructuralError> {
        for action in actions {
            action.condition = self.rewrite_expression(&action.condition)?;
        }
        Ok(())
    }
}

struct DerivativeReplacementCache<'a> {
    dae: &'a Dae,
    replacements: HashMap<VarName, Option<Expression>>,
}

impl<'a> DerivativeReplacementCache<'a> {
    fn new(dae: &'a Dae) -> Self {
        Self {
            dae,
            replacements: HashMap::new(),
        }
    }

    fn replacement_for(
        &mut self,
        substitution: &Substitution,
    ) -> Result<Option<Expression>, StructuralError> {
        if !substitution.var_dims.is_empty() {
            return Ok(None);
        }
        if !self.replacements.contains_key(&substitution.var_name) {
            let derivative = crate::dae_prepare::symbolic_time_derivative_for_expr(
                self.dae,
                &substitution.expr,
            )?;
            self.replacements
                .insert(substitution.var_name.clone(), derivative);
        }
        Ok(self
            .replacements
            .get(&substitution.var_name)
            .and_then(Clone::clone))
    }
}

fn subtraction(lhs: Expression, rhs: Expression, span: rumoca_core::Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Sub,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}
