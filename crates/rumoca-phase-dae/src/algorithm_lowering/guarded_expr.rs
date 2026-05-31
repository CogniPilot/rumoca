use super::*;

pub(super) fn guarded_expr(
    guard: Expression,
    value: Expression,
    fallback: Expression,
) -> Expression {
    if is_bool_expr(&guard, true) {
        return value;
    }
    if is_bool_expr(&guard, false) {
        return fallback;
    }
    if let Expression::If {
        mut branches,
        else_branch,
        span,
    } = fallback
    {
        // MLS §11.1 / §11.2: later algorithm assignments take precedence over
        // earlier values. Keep that ordering as a flat branch list so guarded
        // loop lowering does not repeatedly clone the full previous fallback
        // tree for every branch and iteration.
        branches.insert(0, (guard, value));
        return Expression::If {
            branches,
            else_branch,
            span,
        };
    }
    Expression::If {
        branches: vec![(guard, value)],
        else_branch: Box::new(fallback),
        span: rumoca_core::Span::DUMMY,
    }
}

pub(super) const MAX_GUARDED_ALGORITHM_EXPR_NODES: usize = 1_000_000;

pub(super) fn expression_exceeds_node_budget(expr: &Expression, max_nodes: usize) -> bool {
    expression_node_count_exceeds(expr, max_nodes)
}

pub(super) fn expression_node_count(expr: &Expression) -> usize {
    let mut count = 0usize;
    let mut stack = vec![expr];
    while let Some(expr) = stack.pop() {
        count = count.saturating_add(1);
        push_expression_children(expr, &mut stack);
    }
    count
}

fn expression_node_count_exceeds(expr: &Expression, max_nodes: usize) -> bool {
    let mut remaining = max_nodes;
    let mut stack = vec![expr];
    while let Some(expr) = stack.pop() {
        if remaining == 0 {
            return true;
        }
        remaining -= 1;
        push_expression_children(expr, &mut stack);
    }
    false
}

fn push_subscript_exprs<'a>(subscripts: &'a [Subscript], stack: &mut Vec<&'a Expression>) {
    for subscript in subscripts {
        if let Subscript::Expr { expr, .. } = subscript {
            stack.push(expr);
        }
    }
}

fn push_expression_children<'a>(expr: &'a Expression, stack: &mut Vec<&'a Expression>) {
    match expr {
        Expression::Binary { lhs, rhs, .. } => {
            stack.push(lhs);
            stack.push(rhs);
        }
        Expression::Unary { rhs, .. } => stack.push(rhs),
        Expression::VarRef { subscripts, .. } => push_subscript_exprs(subscripts, stack),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            stack.extend(args);
        }
        Expression::Literal { value: _, .. } | Expression::Empty { .. } => {}
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                stack.push(condition);
                stack.push(value);
            }
            stack.push(else_branch);
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements, .. } => {
            stack.extend(elements);
        }
        Expression::Range {
            start, step, end, ..
        } => {
            stack.push(start);
            if let Some(step) = step {
                stack.push(step);
            }
            stack.push(end);
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            stack.push(expr);
            stack.extend(indices.iter().map(|index| &index.range));
            if let Some(filter) = filter {
                stack.push(filter);
            }
        }
        Expression::Index {
            base, subscripts, ..
        } => {
            stack.push(base);
            push_subscript_exprs(subscripts, stack);
        }
        Expression::FieldAccess { base, .. } => stack.push(base),
    }
}
