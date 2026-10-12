//! Exact finite prefixes selected by a leading, settled binder-only break.

#[cfg(test)]
mod tests;

use super::super::function_ranges::static_integer_expression;
use super::*;
use rumoca_core::{Literal, OpBinary, Progression, Statement};

pub(super) fn truncate_prefix_breaks(statements: &[Statement]) -> Vec<Statement> {
    PrefixBreaks.rewrite_statements(statements)
}

struct PrefixBreaks;

impl ExpressionRewriter for PrefixBreaks {}

impl StatementRewriter for PrefixBreaks {
    fn rewrite_statements(&mut self, statements: &[Statement]) -> Vec<Statement> {
        statements
            .iter()
            .filter_map(|statement| truncate_loop(self.walk_statement(statement)))
            .collect()
    }
}

/// A proof of the exact source-domain prefix before its first break.
///
/// Only the loop's own binder and settled numeric literals are inspected. The
/// break must be the first action, so truncation omits no executed prefix and
/// evaluates no suffix on the breaking iteration. Unsupported conditions keep
/// their source and reach the ordinary typed statement refusal.
struct Prefix {
    last: Option<i64>,
}

fn truncate_loop(mut statement: Statement) -> Option<Statement> {
    let Statement::For {
        indices, equations, ..
    } = &mut statement
    else {
        return Some(statement);
    };
    let [index] = indices.as_mut_slice() else {
        return Some(statement);
    };
    let Some(condition) = equations.first().and_then(break_condition) else {
        return Some(statement);
    };
    let Some(prefix) = prove_prefix(index, condition) else {
        return Some(statement);
    };
    let last = prefix.last?;
    let Expression::Range { end, span, .. } = &mut index.range else {
        unreachable!("prefix proof requires an Integer range")
    };
    **end = Expression::Literal {
        value: Literal::Integer(last),
        span: *span,
    };
    equations.remove(0);
    Some(statement)
}

fn break_condition(statement: &Statement) -> Option<&Expression> {
    let Statement::If {
        cond_blocks,
        else_block: None,
        ..
    } = statement
    else {
        return None;
    };
    let [block] = cond_blocks.as_slice() else {
        return None;
    };
    matches!(block.stmts.as_slice(), [Statement::Break { .. }]).then_some(&block.cond)
}

fn literal_integer(expression: &Expression) -> Option<i64> {
    static_integer_expression(expression, &HashMap::new())
}

fn prove_prefix(index: &rumoca_core::ForIndex, condition: &Expression) -> Option<Prefix> {
    let Expression::Range {
        start, step, end, ..
    } = &index.range
    else {
        return None;
    };
    let first = literal_integer(start)?;
    let stride = match step.as_deref() {
        Some(step) => literal_integer(step)?,
        None => 1,
    };
    let (operator, threshold) = binder_comparison(condition, &index.ident)?;
    let domain = checked_progression(first, stride, literal_integer(end)?)?;
    if domain.is_empty() {
        return Some(Prefix { last: None });
    }
    let stop = first_break(domain, first, stride, operator, threshold)?;
    let last = match stop {
        Some(value) if value == first => None,
        Some(value) => Some(value.checked_sub(stride)?),
        None if stride > 0 => domain.last(),
        None => domain.first(),
    };
    Some(Prefix { last })
}

fn binder_comparison(condition: &Expression, binder: &str) -> Option<(OpBinary, i64)> {
    let Expression::Binary { op, lhs, rhs, .. } = condition else {
        return None;
    };
    if !op.is_relational() {
        return None;
    }
    let own_binder = |expression: &Expression| {
        matches!(expression,
        Expression::VarRef { name, subscripts, .. }
            if name.as_str() == binder && subscripts.is_empty())
    };
    if own_binder(lhs) {
        return Some((op.clone(), literal_integer(rhs)?));
    }
    if !own_binder(rhs) {
        return None;
    }
    let reversed = match op {
        OpBinary::Eq => OpBinary::Eq,
        OpBinary::Neq => OpBinary::Neq,
        OpBinary::Lt => OpBinary::Gt,
        OpBinary::Le => OpBinary::Ge,
        OpBinary::Gt => OpBinary::Lt,
        OpBinary::Ge => OpBinary::Le,
        _ => return None,
    };
    Some((reversed, literal_integer(lhs)?))
}

/// Locate the first satisfying point using compact set intersection, retaining
/// the source's execution direction rather than the set's ascending order.
fn first_break(
    domain: Progression,
    first: i64,
    stride: i64,
    operator: OpBinary,
    threshold: i64,
) -> Option<Option<i64>> {
    let (lower, upper) = (domain.first()?, domain.last()?);
    let interval = match operator {
        OpBinary::Eq => Progression::single(threshold),
        OpBinary::Neq if first != threshold => return Some(Some(first)),
        OpBinary::Neq if domain.len() == 1 => return Some(None),
        OpBinary::Neq => return Some(Some(first.checked_add(stride)?)),
        OpBinary::Lt => match threshold.checked_sub(1) {
            Some(limit) => checked_progression(lower, 1, upper.min(limit))?,
            None => return Some(None),
        },
        OpBinary::Le => checked_progression(lower, 1, upper.min(threshold))?,
        OpBinary::Gt => match threshold.checked_add(1) {
            Some(limit) => checked_progression(lower.max(limit), 1, upper)?,
            None => return Some(None),
        },
        OpBinary::Ge => checked_progression(lower.max(threshold), 1, upper)?,
        _ => return None,
    };
    let broken = domain.intersect(interval)?;
    Some(if stride > 0 {
        broken.first()
    } else {
        broken.last()
    })
}

fn checked_progression(lower: i64, step: i64, upper: i64) -> Option<Progression> {
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: String::new(),
            lower,
            upper,
            step,
        }],
    };
    i64::try_from(domain.scalar_count().ok()?).ok()?;
    Progression::range(lower, step, upper)
}
