//! `for` ranges fixed at translation, and the iterations they expand to.
//!
//! MLS 3.7 §11.2.2 evaluates a `for` range once, before the first iteration.
//! A range whose bounds read only literals, constants, and evaluable
//! parameters has the same iterations at every run the compiled model can
//! make, so the loop is exactly its unrolled sequence: each iteration is the
//! body with the index replaced by its value. A range that reads a settable
//! parameter could change between runs, and a range that is not an Integer
//! range has no such sequence; both are refused.

use super::*;
use rumoca_core::{ExpressionRewriter, StatementRewriter};

/// The exact `(start, step, end)` of a range whose bounds read only literals,
/// constants, and evaluable parameters. `section` names the algorithm kind in
/// the refusal (`"initial"` or `"model"`).
pub(super) fn fixed_range(
    flat: &flat::Model,
    shapes: &ShapeEnvironment,
    range: &Expression,
    section: &'static str,
    span: Span,
) -> Result<(i64, i64, i64), ToDaeError> {
    let refuse = |detail: &str| ToDaeError::unsupported_algorithm(section, detail, span);
    let Expression::Range {
        start, step, end, ..
    } = range
    else {
        return Err(refuse("an algorithm `for` iterates over an Integer range"));
    };
    let bound = |expression: &Expression| {
        reads_only_fixed(flat, shapes, expression)
            .then(|| shapes.proven_integer_bounds(expression))
            .flatten()
            .and_then(|(lower, upper)| (lower == upper).then_some(lower))
            .ok_or_else(|| {
                refuse(
                    "an algorithm `for` range must read only literals, constants, and evaluable \
                     parameters, so its iterations are fixed at translation",
                )
            })
    };
    let step = step.as_deref().map(bound).transpose()?.unwrap_or(1);
    if step == 0 {
        return Err(refuse("an algorithm `for` range has a zero step"));
    }
    Ok((bound(start)?, step, bound(end)?))
}

/// The index values of a fixed `(start, step, end)` range, in order.
pub(super) fn range_values((start, step, end): (i64, i64, i64)) -> impl Iterator<Item = i64> {
    let mut current = Some(start);
    std::iter::from_fn(move || {
        let value =
            current.filter(|value| (step > 0 && *value <= end) || (step < 0 && *value >= end))?;
        current = value.checked_add(step);
        Some(value)
    })
}

fn reads_only_fixed(
    flat: &flat::Model,
    shapes: &ShapeEnvironment,
    expression: &Expression,
) -> bool {
    let evaluable = shapes.evaluable();
    let mut reads = Vec::new();
    expression.collect_var_refs(&mut reads);
    reads.iter().all(|name| {
        evaluable.is_some_and(|evaluable| evaluable.contains(name))
            || flat
                .variables
                .get(name)
                .is_some_and(|variable| matches!(variable.variability, Variability::Constant(_)))
    })
}

/// Replaces one loop index by its iteration value; a nested loop that rebinds
/// the same name keeps its own binding.
pub(super) struct IndexBinding<'name> {
    pub(super) name: &'name str,
    pub(super) value: i64,
    pub(super) span: Span,
}

impl ExpressionRewriter for IndexBinding<'_> {
    fn rewrite_var_ref_expression(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[Subscript],
        span: Span,
    ) -> Expression {
        if subscripts.is_empty() && name.var_name().as_str() == self.name {
            return Expression::Literal {
                value: rumoca_core::Literal::Integer(self.value),
                span: self.span,
            };
        }
        Expression::VarRef {
            name: name.clone(),
            subscripts: self.rewrite_subscripts(subscripts),
            span,
        }
    }
}

impl StatementRewriter for IndexBinding<'_> {
    fn rewrite_statement(&mut self, statement: &rumoca_core::Statement) -> rumoca_core::Statement {
        if let rumoca_core::Statement::For { indices, .. } = statement
            && indices.iter().any(|index| index.ident == self.name)
        {
            return statement.clone();
        }
        self.walk_statement(statement)
    }
}
