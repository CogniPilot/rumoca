use super::*;

pub(super) fn discrete_assignment_rhs_var_name(expr: &Expression) -> Option<VarName> {
    let Expression::VarRef { name, subscripts } = expr else {
        return None;
    };
    Some(varref_with_subscripts(name, subscripts))
}

pub(super) fn rewrite_discrete_self_refs_to_pre(expr: &Expression, target: &VarName) -> Expression {
    match expr {
        Expression::VarRef { name, subscripts } => {
            rewrite_discrete_var_ref(name, subscripts, target)
        }
        Expression::Binary { op, lhs, rhs } => rewrite_discrete_binary_expr(op, lhs, rhs, target),
        Expression::Unary { op, rhs } => rewrite_discrete_unary_expr(op, rhs, target),
        Expression::BuiltinCall { function, args } => {
            rewrite_discrete_builtin_call(*function, args, target)
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => rewrite_discrete_function_call(name, args, *is_constructor, target),
        Expression::If {
            branches,
            else_branch,
        } => rewrite_discrete_if_expr(branches, else_branch, target),
        Expression::Array {
            elements,
            is_matrix,
        } => rewrite_discrete_array_expr(elements, *is_matrix, target),
        Expression::Tuple { elements } => rewrite_discrete_tuple_expr(elements, target),
        Expression::Range { start, step, end } => {
            rewrite_discrete_range_expr(start, step, end, target)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => rewrite_discrete_array_comprehension(expr, indices, filter, target),
        Expression::Index { base, subscripts } => {
            rewrite_discrete_index_expr(base, subscripts, target)
        }
        Expression::FieldAccess { base, field } => {
            rewrite_discrete_field_access_expr(base, field, target)
        }
        Expression::Literal(literal) => Expression::Literal(literal.clone()),
        Expression::Empty => Expression::Empty,
    }
}

fn rewrite_discrete_var_ref(
    name: &VarName,
    subscripts: &[Subscript],
    target: &VarName,
) -> Expression {
    if name == target {
        pre_target_expr(target)
    } else {
        Expression::VarRef {
            name: name.clone(),
            subscripts: subscripts.to_vec(),
        }
    }
}

fn rewrite_discrete_binary_expr(
    op: &rumoca_ir_core::OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    target: &VarName,
) -> Expression {
    Expression::Binary {
        op: op.clone(),
        lhs: Box::new(rewrite_discrete_self_refs_to_pre(lhs, target)),
        rhs: Box::new(rewrite_discrete_self_refs_to_pre(rhs, target)),
    }
}

fn rewrite_discrete_unary_expr(
    op: &rumoca_ir_core::OpUnary,
    rhs: &Expression,
    target: &VarName,
) -> Expression {
    Expression::Unary {
        op: op.clone(),
        rhs: Box::new(rewrite_discrete_self_refs_to_pre(rhs, target)),
    }
}

fn rewrite_discrete_builtin_call(
    function: BuiltinFunction,
    args: &[Expression],
    target: &VarName,
) -> Expression {
    if matches!(function, BuiltinFunction::Pre) {
        return Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: args.to_vec(),
        };
    }
    Expression::BuiltinCall {
        function,
        args: args
            .iter()
            .map(|arg| rewrite_discrete_self_refs_to_pre(arg, target))
            .collect(),
    }
}

fn rewrite_discrete_function_call(
    name: &VarName,
    args: &[Expression],
    is_constructor: bool,
    target: &VarName,
) -> Expression {
    Expression::FunctionCall {
        name: name.clone(),
        args: args
            .iter()
            .map(|arg| rewrite_discrete_self_refs_to_pre(arg, target))
            .collect(),
        is_constructor,
    }
}

fn rewrite_discrete_if_expr(
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    target: &VarName,
) -> Expression {
    Expression::If {
        branches: branches
            .iter()
            .map(|(cond, value)| {
                (
                    rewrite_discrete_self_refs_to_pre(cond, target),
                    rewrite_discrete_branch_value(cond, value, target),
                )
            })
            .collect(),
        else_branch: Box::new(rewrite_discrete_self_refs_to_pre(else_branch, target)),
    }
}

fn rewrite_discrete_branch_value(
    cond: &Expression,
    value: &Expression,
    target: &VarName,
) -> Expression {
    if is_initial_builtin(cond) && is_direct_target_var_ref(value, target) {
        // MLS §8.6: the preserved initial-section value in
        // `if initial() then x else pre(x)` must stay as the current `x`
        // instead of being rewritten back to `pre(x)`.
        return value.clone();
    }
    rewrite_discrete_self_refs_to_pre(value, target)
}

fn is_initial_builtin(expr: &Expression) -> bool {
    matches!(
        expr,
        Expression::BuiltinCall {
            function: BuiltinFunction::Initial,
            args,
        } if args.is_empty()
    )
}

fn is_direct_target_var_ref(expr: &Expression, target: &VarName) -> bool {
    matches!(
        expr,
        Expression::VarRef { name, subscripts }
            if name == target && subscripts.is_empty()
    )
}

fn rewrite_discrete_array_expr(
    elements: &[Expression],
    is_matrix: bool,
    target: &VarName,
) -> Expression {
    Expression::Array {
        elements: elements
            .iter()
            .map(|element| rewrite_discrete_self_refs_to_pre(element, target))
            .collect(),
        is_matrix,
    }
}

fn rewrite_discrete_tuple_expr(elements: &[Expression], target: &VarName) -> Expression {
    Expression::Tuple {
        elements: elements
            .iter()
            .map(|element| rewrite_discrete_self_refs_to_pre(element, target))
            .collect(),
    }
}

fn rewrite_discrete_range_expr(
    start: &Expression,
    step: &Option<Box<Expression>>,
    end: &Expression,
    target: &VarName,
) -> Expression {
    Expression::Range {
        start: Box::new(rewrite_discrete_self_refs_to_pre(start, target)),
        step: step
            .as_ref()
            .map(|step_expr| Box::new(rewrite_discrete_self_refs_to_pre(step_expr, target))),
        end: Box::new(rewrite_discrete_self_refs_to_pre(end, target)),
    }
}

fn rewrite_discrete_array_comprehension(
    expr: &Expression,
    indices: &[ComprehensionIndex],
    filter: &Option<Box<Expression>>,
    target: &VarName,
) -> Expression {
    Expression::ArrayComprehension {
        expr: Box::new(rewrite_discrete_self_refs_to_pre(expr, target)),
        indices: indices
            .iter()
            .map(|index| ComprehensionIndex {
                name: index.name.clone(),
                range: rewrite_discrete_self_refs_to_pre(&index.range, target),
            })
            .collect(),
        filter: filter
            .as_ref()
            .map(|filter_expr| Box::new(rewrite_discrete_self_refs_to_pre(filter_expr, target))),
    }
}

fn rewrite_discrete_index_expr(
    base: &Expression,
    subscripts: &[Subscript],
    target: &VarName,
) -> Expression {
    Expression::Index {
        base: Box::new(rewrite_discrete_self_refs_to_pre(base, target)),
        subscripts: subscripts.to_vec(),
    }
}

fn rewrite_discrete_field_access_expr(
    base: &Expression,
    field: &str,
    target: &VarName,
) -> Expression {
    Expression::FieldAccess {
        base: Box::new(rewrite_discrete_self_refs_to_pre(base, target)),
        field: field.to_string(),
    }
}
