//! Prepare invariant qualification and shape work for scalar arithmetic loop bodies.
//!
//! This only derives the same materialized scalar views. Family/domain ownership,
//! loop range evaluation, and nested-loop expansion stay with `expand_for_equation`.

use rumoca_core::{
    Expression, ExpressionRewriter, Literal, OpBinary, OpUnary, Reference, Subscript,
};

use super::*;

#[cfg(test)]
mod tests;

#[cfg(test)]
thread_local! {
    static ENABLED: std::cell::Cell<bool> = const { std::cell::Cell::new(true) };
    static PREPARED_ROWS: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static PREPARED_BODIES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

pub(super) fn collect_prepared_iterations(
    env: &ForIterationEnv<'_>,
    index: &ast::ForIndex,
    equations: &[ast::Equation],
    values: &[i64],
    outer_values: &[i64],
    iterations: &mut Vec<SourceStructuredIteration>,
    out: &mut FlattenedEquations,
) -> Result<bool, FlattenError> {
    #[cfg(test)]
    if !ENABLED.get() {
        return Ok(false);
    }
    // Empty domains must not qualify a body the original collector never reads.
    if values.is_empty() {
        return Ok(false);
    }
    let Some(body) = prepare_body(env, equations, &index.ident.text)? else {
        return Ok(false);
    };
    #[cfg(test)]
    PREPARED_BODIES.set(PREPARED_BODIES.get() + 1);
    let mut projector = ScalarLoopProjection {
        binder: &index.ident.text,
        value: 0,
        constants: EvalContext::new(),
    };
    for &value in values {
        projector.value = value;
        let mut index_values = outer_values.to_vec();
        index_values.push(value);
        iterations.push(SourceStructuredIteration {
            index_values,
            equation_count: body.len(),
        });
        for residual in &body {
            out.equations.push(flat::Equation::new(
                projector.rewrite_expression(residual),
                env.span,
                env.origin.clone(),
            ));
        }
    }
    #[cfg(test)]
    PREPARED_ROWS.set(PREPARED_ROWS.get() + values.len() * body.len());
    Ok(true)
}

fn prepare_body(
    env: &ForIterationEnv<'_>,
    equations: &[ast::Equation],
    binder: &str,
) -> Result<Option<Vec<Expression>>, FlattenError> {
    if !equations.iter().all(|equation| match equation {
        ast::Equation::Simple { lhs, rhs } => {
            stable_scalar_expression(env, lhs, binder) && stable_scalar_expression(env, rhs, binder)
        }
        _ => false,
    }) {
        return Ok(None);
    }
    let locals = HashSet::from([binder.to_owned()]);
    let mut body = Vec::with_capacity(equations.len());
    for equation in equations {
        let ast::Equation::Simple { lhs, rhs } = equation else {
            return Ok(None);
        };
        body.push(make_residual(
            env.ctx,
            lhs,
            rhs,
            env.prefix,
            env.def_map,
            Some(&locals),
        )?);
    }
    Ok(Some(body))
}

fn stable_scalar_expression(
    env: &ForIterationEnv<'_>,
    expr: &ast::Expression,
    binder: &str,
) -> bool {
    match expr {
        ast::Expression::Terminal { terminal_type, .. } => matches!(
            terminal_type,
            ast::TerminalType::UnsignedInteger | ast::TerminalType::UnsignedReal
        ),
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            arithmetic_operator(op)
                && stable_scalar_expression(env, lhs, binder)
                && stable_scalar_expression(env, rhs, binder)
        }
        ast::Expression::Unary { op, rhs, .. } => {
            arithmetic_unary(op) && stable_scalar_expression(env, rhs, binder)
        }
        ast::Expression::Parenthesized { inner, .. } => {
            stable_scalar_expression(env, inner, binder)
        }
        ast::Expression::ComponentReference(reference) => {
            stable_scalar_reference(env, reference, binder)
        }
        _ => false,
    }
}

fn stable_scalar_reference(
    env: &ForIterationEnv<'_>,
    reference: &ast::ComponentReference,
    binder: &str,
) -> bool {
    // A member of an indexed structured component can select a different occurrence;
    // only single-part scalar/array references in this unchanged scope are reusable.
    let [part] = reference.parts.as_slice() else {
        return false;
    };
    if part.ident.text.as_ref() == binder {
        // Index lowering separates the array base from its subscripts. Refuse a
        // same-spelled indexed base rather than confusing it with the local binder.
        return part.subs.is_none();
    }
    if !part.subs.as_deref().unwrap_or_default().iter().all(|sub| {
        matches!(sub, ast::Subscript::Expression(expr) if stable_integer_subscript(expr, binder))
    }) {
        return false;
    }
    infer_component_ref_shape(reference, env.prefix, env.ctx) == ExpressionShape::Scalar
}

fn stable_integer_subscript(expr: &ast::Expression, binder: &str) -> bool {
    match expr {
        ast::Expression::Terminal { terminal_type, .. } => {
            *terminal_type == ast::TerminalType::UnsignedInteger
        }
        ast::Expression::ComponentReference(reference) => {
            matches!(reference.parts.as_slice(), [part] if part.subs.is_none() && part.ident.text.as_ref() == binder)
        }
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            matches!(op, OpBinary::Add | OpBinary::Sub | OpBinary::Mul)
                && stable_integer_subscript(lhs, binder)
                && stable_integer_subscript(rhs, binder)
        }
        ast::Expression::Unary { op, rhs, .. } => {
            arithmetic_unary(op) && stable_integer_subscript(rhs, binder)
        }
        ast::Expression::Parenthesized { inner, .. } => stable_integer_subscript(inner, binder),
        _ => false,
    }
}

fn arithmetic_operator(op: &OpBinary) -> bool {
    matches!(
        op,
        OpBinary::Add | OpBinary::Sub | OpBinary::Mul | OpBinary::Div | OpBinary::Exp
    )
}

fn arithmetic_unary(op: &OpUnary) -> bool {
    matches!(
        op,
        OpUnary::Plus | OpUnary::Minus | OpUnary::DotPlus | OpUnary::DotMinus | OpUnary::Empty
    )
}

struct ScalarLoopProjection<'a> {
    binder: &'a str,
    value: i64,
    constants: EvalContext,
}

impl ExpressionRewriter for ScalarLoopProjection<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &Reference,
        subscripts: &[Subscript],
        span: rumoca_core::Span,
    ) -> Expression {
        if name.as_str() == self.binder && subscripts.is_empty() {
            return Expression::Literal {
                value: Literal::Integer(self.value),
                span,
            };
        }
        self.walk_var_ref_expression(name, subscripts, span)
    }

    fn rewrite_subscript(&mut self, subscript: &Subscript) -> Subscript {
        let Subscript::Expr { expr, span } = subscript else {
            return subscript.clone();
        };
        let expr = self.rewrite_expression(expr);
        // The gate admits only integer literals/binder arithmetic using the same
        // checked +,-,* semantics as AST static-subscript evaluation. Failure keeps
        // the expression, exactly as `subscript_from_ast_for_base` does.
        match rumoca_eval_flat::constant::try_eval_integer(&expr, &self.constants) {
            Some(value) => Subscript::index(value, *span),
            None => Subscript::expr(Box::new(expr), *span),
        }
    }
}
