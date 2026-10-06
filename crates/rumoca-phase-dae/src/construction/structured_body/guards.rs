//! Retain the source-selected domain of a structural equation conditional.
//!
//! Flat selects these guards after substituting each lexical point. A compact
//! family keeps the point symbolic, but its exact settled declaration operands
//! still belong to that translation, rather than a new runtime guard profile.

#[cfg(test)]
mod tests;

use super::*;

pub(super) fn settle<'dae>(
    body: &Expression,
    shapes: &ShapeEnvironment,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
) -> Option<Expression> {
    if binders.is_empty() || shapes.is_attribute_scope() || shapes.is_specialization() {
        return None;
    }
    let Expression::If {
        branches,
        else_branch,
        span,
    } = body
    else {
        return None;
    };
    let mut changed = false;
    let branches = branches
        .iter()
        .map(|(condition, value)| {
            let settled = predicate(condition, shapes, binders);
            changed |= settled.is_some();
            (settled.unwrap_or_else(|| condition.clone()), value.clone())
        })
        .collect();
    changed.then(|| Expression::If {
        branches,
        else_branch: else_branch.clone(),
        span: *span,
    })
}

fn predicate<'dae>(
    expression: &Expression,
    shapes: &ShapeEnvironment,
    binders: &HashMap<VarName, dae::DomainBinderId<'dae>>,
) -> Option<Expression> {
    match expression {
        Expression::Literal {
            value: Literal::Integer(_) | Literal::Boolean(_),
            ..
        } => Some(expression.clone()),
        Expression::VarRef {
            name,
            subscripts,
            span,
        } if subscripts.is_empty() => {
            if name.resolved_function().is_some()
                || name.component_ref().is_some_and(|reference| {
                    reference.parts().len() != 1
                        || reference.parts().iter().any(|part| !part.subs.is_empty())
                })
            {
                return None;
            }
            if binders.contains_key(name.var_name()) {
                if name
                    .instance_id()
                    .is_some_and(|instance| !shapes.slice_reference_scope(instance))
                {
                    return None;
                }
                shapes.slice_binder_bounds(name.var_name())?;
                return Some(expression.clone());
            }
            let value = shapes.slice_constant(name)?;
            Some(Expression::Literal {
                value: Literal::Integer(value),
                span: *span,
            })
        }
        Expression::Unary { op, rhs, span } => Some(Expression::Unary {
            op: op.clone(),
            rhs: Box::new(predicate(rhs, shapes, binders)?),
            span: *span,
        }),
        Expression::Binary { op, lhs, rhs, span } if permitted(op) => Some(Expression::Binary {
            op: op.clone(),
            lhs: Box::new(predicate(lhs, shapes, binders)?),
            rhs: Box::new(predicate(rhs, shapes, binders)?),
            span: *span,
        }),
        // One runtime read declines the complete guard: in particular a
        // threshold parameter is never frozen inside a state/input relation.
        _ => None,
    }
}

fn permitted(operator: &OpBinary) -> bool {
    matches!(
        operator,
        OpBinary::Add
            | OpBinary::Sub
            | OpBinary::Mul
            | OpBinary::Lt
            | OpBinary::Le
            | OpBinary::Gt
            | OpBinary::Ge
            | OpBinary::Eq
            | OpBinary::Neq
            | OpBinary::And
            | OpBinary::Or
    )
}
