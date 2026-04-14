use super::*;

fn is_relational_binary_op(op: &rumoca_ir_core::OpBinary) -> bool {
    matches!(
        op,
        rumoca_ir_core::OpBinary::Lt(_)
            | rumoca_ir_core::OpBinary::Le(_)
            | rumoca_ir_core::OpBinary::Gt(_)
            | rumoca_ir_core::OpBinary::Ge(_)
            | rumoca_ir_core::OpBinary::Eq(_)
            | rumoca_ir_core::OpBinary::Neq(_)
    )
}

fn expression_contains_relational_operator(expr: &Expression) -> bool {
    match expr {
        Expression::Binary { op, lhs, rhs } => {
            is_relational_binary_op(op)
                || expression_contains_relational_operator(lhs)
                || expression_contains_relational_operator(rhs)
        }
        Expression::Unary { rhs, .. } => expression_contains_relational_operator(rhs),
        Expression::BuiltinCall { args, .. } | Expression::FunctionCall { args, .. } => {
            args.iter().any(expression_contains_relational_operator)
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expression_contains_relational_operator(cond)
                    || expression_contains_relational_operator(value)
            }) || expression_contains_relational_operator(else_branch)
        }
        Expression::Array { elements, .. } | Expression::Tuple { elements } => {
            elements.iter().any(expression_contains_relational_operator)
        }
        Expression::Range { start, step, end } => {
            expression_contains_relational_operator(start)
                || step
                    .as_deref()
                    .is_some_and(expression_contains_relational_operator)
                || expression_contains_relational_operator(end)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expression_contains_relational_operator(expr)
                || indices
                    .iter()
                    .any(|idx| expression_contains_relational_operator(&idx.range))
                || filter
                    .as_deref()
                    .is_some_and(expression_contains_relational_operator)
        }
        Expression::Index { base, subscripts } => {
            expression_contains_relational_operator(base)
                || subscripts.iter().any(|sub| match sub {
                    Subscript::Expr(expr) => expression_contains_relational_operator(expr),
                    Subscript::Index(_) | Subscript::Colon => false,
                })
        }
        Expression::FieldAccess { base, .. } => expression_contains_relational_operator(base),
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => false,
    }
}

fn discrete_valued_name_exists(dae: &Dae, name: &VarName) -> bool {
    dae.discrete_valued
        .contains_key(&flat_to_dae_var_name(name))
        || subscript_fallback_chain(name).into_iter().any(|candidate| {
            dae.discrete_valued
                .contains_key(&flat_to_dae_var_name(&candidate))
        })
}

fn find_condition_definition_rhs(dae: &Dae, name: &VarName) -> Option<Expression> {
    dae.f_m
        .iter()
        .chain(dae.f_z.iter())
        .chain(dae.f_x.iter())
        .find_map(|equation| {
            let lhs = equation.lhs.as_ref()?;
            let lhs = dae_to_flat_var_name(lhs);
            if lhs != *name {
                return None;
            }
            if equation.origin.starts_with("guarded ") {
                return None;
            }
            Some(dae_to_flat_expression(&equation.rhs))
        })
}

fn unfold_boolean_alias_condition_expr(
    dae: &Dae,
    expr: &Expression,
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Option<Expression> {
    if depth > 8 {
        return None;
    }
    let Expression::VarRef { name, subscripts } = expr else {
        return None;
    };
    if !subscripts.is_empty() || !discrete_valued_name_exists(dae, name) {
        return None;
    }
    if !visiting.insert(name.clone()) {
        return None;
    }
    let result = find_condition_definition_rhs(dae, name).and_then(|rhs| {
        let unfolded = unfold_boolean_aliases_in_expr(dae, &rhs, visiting, depth + 1);
        if expression_contains_relational_operator(&unfolded) {
            Some(unfolded)
        } else {
            None
        }
    });
    visiting.remove(name);
    result
}

fn unfold_boolean_aliases_in_expr(
    dae: &Dae,
    expr: &Expression,
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Expression {
    if let Some(unfolded) = unfold_boolean_alias_condition_expr(dae, expr, visiting, depth) {
        return unfolded;
    }
    match expr {
        Expression::Binary { op, lhs, rhs } => {
            unfold_boolean_aliases_binary(dae, op, lhs, rhs, visiting, depth)
        }
        Expression::Unary { op, rhs } => {
            unfold_boolean_aliases_unary(dae, op, rhs, visiting, depth)
        }
        Expression::BuiltinCall { function, args } => {
            unfold_boolean_aliases_builtin_call(dae, *function, args, visiting, depth)
        }
        Expression::FunctionCall {
            name,
            args,
            is_constructor,
        } => {
            unfold_boolean_aliases_function_call(dae, name, args, *is_constructor, visiting, depth)
        }
        Expression::If {
            branches,
            else_branch,
        } => unfold_boolean_aliases_if(dae, branches, else_branch, visiting, depth),
        Expression::Array {
            elements,
            is_matrix,
        } => unfold_boolean_aliases_array(dae, elements, *is_matrix, visiting, depth),
        Expression::Tuple { elements } => {
            unfold_boolean_aliases_tuple(dae, elements, visiting, depth)
        }
        Expression::Range { start, step, end } => {
            unfold_boolean_aliases_range(dae, start, step, end, visiting, depth)
        }
        Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            unfold_boolean_aliases_array_comprehension(dae, expr, indices, filter, visiting, depth)
        }
        Expression::Index { base, subscripts } => {
            unfold_boolean_aliases_index(dae, base, subscripts, visiting, depth)
        }
        Expression::FieldAccess { base, field } => {
            unfold_boolean_aliases_field_access(dae, base, field, visiting, depth)
        }
        Expression::VarRef { .. } | Expression::Literal(_) | Expression::Empty => expr.clone(),
    }
}

fn unfold_boolean_aliases_expr_vec(
    dae: &Dae,
    args: &[Expression],
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Vec<Expression> {
    args.iter()
        .map(|arg| unfold_boolean_aliases_in_expr(dae, arg, visiting, depth + 1))
        .collect()
}

fn unfold_boolean_aliases_binary(
    dae: &Dae,
    op: &rumoca_ir_core::OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Expression {
    Expression::Binary {
        op: op.clone(),
        lhs: Box::new(unfold_boolean_aliases_in_expr(
            dae,
            lhs,
            visiting,
            depth + 1,
        )),
        rhs: Box::new(unfold_boolean_aliases_in_expr(
            dae,
            rhs,
            visiting,
            depth + 1,
        )),
    }
}

fn unfold_boolean_aliases_unary(
    dae: &Dae,
    op: &rumoca_ir_core::OpUnary,
    rhs: &Expression,
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Expression {
    Expression::Unary {
        op: op.clone(),
        rhs: Box::new(unfold_boolean_aliases_in_expr(
            dae,
            rhs,
            visiting,
            depth + 1,
        )),
    }
}

fn unfold_boolean_aliases_builtin_call(
    dae: &Dae,
    function: BuiltinFunction,
    args: &[Expression],
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Expression {
    Expression::BuiltinCall {
        function,
        args: unfold_boolean_aliases_expr_vec(dae, args, visiting, depth),
    }
}

fn unfold_boolean_aliases_function_call(
    dae: &Dae,
    name: &VarName,
    args: &[Expression],
    is_constructor: bool,
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Expression {
    Expression::FunctionCall {
        name: name.clone(),
        args: unfold_boolean_aliases_expr_vec(dae, args, visiting, depth),
        is_constructor,
    }
}

fn unfold_boolean_aliases_if(
    dae: &Dae,
    branches: &[(Expression, Expression)],
    else_branch: &Expression,
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Expression {
    Expression::If {
        branches: branches
            .iter()
            .map(|(cond, value)| {
                (
                    unfold_boolean_aliases_in_expr(dae, cond, visiting, depth + 1),
                    unfold_boolean_aliases_in_expr(dae, value, visiting, depth + 1),
                )
            })
            .collect(),
        else_branch: Box::new(unfold_boolean_aliases_in_expr(
            dae,
            else_branch,
            visiting,
            depth + 1,
        )),
    }
}

fn unfold_boolean_aliases_array(
    dae: &Dae,
    elements: &[Expression],
    is_matrix: bool,
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Expression {
    Expression::Array {
        elements: unfold_boolean_aliases_expr_vec(dae, elements, visiting, depth),
        is_matrix,
    }
}

fn unfold_boolean_aliases_tuple(
    dae: &Dae,
    elements: &[Expression],
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Expression {
    Expression::Tuple {
        elements: unfold_boolean_aliases_expr_vec(dae, elements, visiting, depth),
    }
}

fn unfold_boolean_aliases_range(
    dae: &Dae,
    start: &Expression,
    step: &Option<Box<Expression>>,
    end: &Expression,
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Expression {
    Expression::Range {
        start: Box::new(unfold_boolean_aliases_in_expr(
            dae,
            start,
            visiting,
            depth + 1,
        )),
        step: step.as_deref().map(|step_expr| {
            Box::new(unfold_boolean_aliases_in_expr(
                dae,
                step_expr,
                visiting,
                depth + 1,
            ))
        }),
        end: Box::new(unfold_boolean_aliases_in_expr(
            dae,
            end,
            visiting,
            depth + 1,
        )),
    }
}

fn unfold_boolean_aliases_array_comprehension(
    dae: &Dae,
    expr: &Expression,
    indices: &[ComprehensionIndex],
    filter: &Option<Box<Expression>>,
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Expression {
    Expression::ArrayComprehension {
        expr: Box::new(unfold_boolean_aliases_in_expr(
            dae,
            expr,
            visiting,
            depth + 1,
        )),
        indices: indices
            .iter()
            .map(|index| ComprehensionIndex {
                name: index.name.clone(),
                range: unfold_boolean_aliases_in_expr(dae, &index.range, visiting, depth + 1),
            })
            .collect(),
        filter: filter.as_deref().map(|filter_expr| {
            Box::new(unfold_boolean_aliases_in_expr(
                dae,
                filter_expr,
                visiting,
                depth + 1,
            ))
        }),
    }
}

fn unfold_boolean_aliases_index(
    dae: &Dae,
    base: &Expression,
    subscripts: &[Subscript],
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Expression {
    Expression::Index {
        base: Box::new(unfold_boolean_aliases_in_expr(
            dae,
            base,
            visiting,
            depth + 1,
        )),
        subscripts: subscripts.to_vec(),
    }
}

fn unfold_boolean_aliases_field_access(
    dae: &Dae,
    base: &Expression,
    field: &str,
    visiting: &mut HashSet<VarName>,
    depth: usize,
) -> Expression {
    Expression::FieldAccess {
        base: Box::new(unfold_boolean_aliases_in_expr(
            dae,
            base,
            visiting,
            depth + 1,
        )),
        field: field.to_string(),
    }
}

fn when_guard_scalar_activation_expr(dae: &Dae, when_condition: &Expression) -> Expression {
    let unfolded = unfold_boolean_aliases_in_expr(dae, when_condition, &mut HashSet::new(), 0);
    // MLS §8.3.5.1: when-equation assignments fire on the false->true edge
    // of the condition, not while the condition remains true. Keep the
    // canonical relational subexpressions visible under edge(...) so the
    // relation analysis still exposes the underlying condition roots.
    match when_condition {
        Expression::VarRef { .. } | Expression::Index { .. } | Expression::FieldAccess { .. }
            if !expression_contains_relational_operator(&unfolded) =>
        {
            Expression::BuiltinCall {
                function: BuiltinFunction::Edge,
                args: vec![when_condition.clone()],
            }
        }
        _ => Expression::BuiltinCall {
            function: BuiltinFunction::Edge,
            args: vec![unfolded],
        },
    }
}

pub(super) fn when_guard_activation_expr(dae: &Dae, when_condition: &Expression) -> Expression {
    match when_condition {
        // MLS §8.3.5: vectorized when-conditions trigger when any listed
        // condition becomes true, so each scalar Boolean guard needs its own
        // activation edge instead of leaving the aggregate guard level-active.
        Expression::Array {
            elements,
            is_matrix,
        } => Expression::Array {
            elements: elements
                .iter()
                .map(|element| when_guard_activation_expr(dae, element))
                .collect(),
            is_matrix: *is_matrix,
        },
        Expression::Tuple { elements } => Expression::Tuple {
            elements: elements
                .iter()
                .map(|element| when_guard_activation_expr(dae, element))
                .collect(),
        },
        _ => when_guard_scalar_activation_expr(dae, when_condition),
    }
}
