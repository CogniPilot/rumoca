use super::*;

pub(super) fn validate_integer_reduction(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
) -> Result<Option<FunctionPlan>, ToDaeError> {
    if function
        .body
        .iter()
        .any(|statement| matches!(statement, rumoca_core::Statement::While { .. }))
    {
        return validate_while_sum(function, context).map(Some);
    }
    if contains_break(&function.body) {
        return validate_capped_for_sum(function, context).map(Some);
    }
    Ok(None)
}

fn validate_while_sum(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionPlan, ToDaeError> {
    let [
        rumoca_core::Statement::Assignment {
            value: result_zero, ..
        },
        rumoca_core::Statement::Assignment {
            value: index_zero, ..
        },
        rumoca_core::Statement::While { block, span },
    ] = function.body.as_slice()
    else {
        return Err(unsupported_reduction(function));
    };
    let initial = validate_function_statements(&function.body[..2], context)?;
    let [
        FunctionStatementPlan::Assignment(result_assignment),
        FunctionStatementPlan::Assignment(index_assignment),
    ] = initial.as_slice()
    else {
        return Err(unsupported_reduction(function));
    };
    if !result_assignment.is_whole() || !index_assignment.is_whole() {
        return Err(unsupported_reduction(function));
    }
    let result = result_assignment.target().clone();
    let index = index_assignment.target().clone();
    let [
        rumoca_core::Statement::Assignment {
            value: result_update,
            ..
        },
        rumoca_core::Statement::Assignment {
            value: index_update,
            ..
        },
    ] = block.stmts.as_slice()
    else {
        return Err(unsupported_reduction(function));
    };
    let updates = validate_function_statements(&block.stmts, context)?;
    if !is_output_integer(function, &result)
        || !is_local_integer(function, &index)
        || !is_integer_zero(result_zero)
        || !is_integer_zero(index_zero)
        || !matches!(
            updates.as_slice(),
            [
                FunctionStatementPlan::Assignment(target),
                FunctionStatementPlan::Assignment(next)
            ] if target.is_whole()
                && next.is_whole()
                && target.target() == &result
                && next.target() == &index
        )
        || !is_sum_update(result_update, &result, &index)
        || !is_increment(index_update, &index)
    {
        return Err(unsupported_reduction(function));
    }
    validate_function_expression_with_roles(&block.cond, context.roles, context.flat)?;
    if !is_exclusive_bound(&block.cond, &index, function) {
        return Err(unsupported_reduction(function));
    }
    require_span(*span, "function while reduction")?;
    Ok(FunctionPlan::IntegerReduction {
        initial,
        result,
        reduction: FunctionIntegerReduction::WhileExclusive,
    })
}

fn validate_capped_for_sum(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionPlan, ToDaeError> {
    let [
        rumoca_core::Statement::Assignment {
            value: initial_value,
            ..
        },
        rumoca_core::Statement::For {
            indices,
            equations,
            span,
        },
    ] = function.body.as_slice()
    else {
        return Err(unsupported_reduction(function));
    };
    let [index] = indices.as_slice() else {
        return Err(unsupported_reduction(function));
    };
    let [
        rumoca_core::Statement::If {
            cond_blocks,
            else_block: None,
            ..
        },
        update_statement @ rumoca_core::Statement::Assignment { value: update, .. },
    ] = equations.as_slice()
    else {
        return Err(unsupported_reduction(function));
    };
    let [block] = cond_blocks.as_slice() else {
        return Err(unsupported_reduction(function));
    };
    let [rumoca_core::Statement::Break { span: break_span }] = block.stmts.as_slice() else {
        return Err(unsupported_reduction(function));
    };
    let initial = validate_function_statements(&function.body[..1], context)?;
    let [FunctionStatementPlan::Assignment(result_assignment)] = initial.as_slice() else {
        return Err(unsupported_reduction(function));
    };
    if !result_assignment.is_whole() {
        return Err(unsupported_reduction(function));
    }
    let result = result_assignment.target().clone();
    let mut loop_roles = context.roles.clone();
    let binder = VarName::new(&index.ident);
    loop_roles.insert(binder.clone(), PlannedRole::Parameter);
    let loop_context = FunctionValidationContext {
        roles: &loop_roles,
        ..context
    };
    let update_plan =
        validate_function_statements(std::slice::from_ref(update_statement), loop_context)?;
    validate_function_range_expression(&index.range, context.roles, context.flat)?;
    validate_function_expression_with_roles(&block.cond, &loop_roles, context.flat)?;
    if !is_output_integer(function, &result)
        || !is_integer_zero(initial_value)
        || !matches!(
            update_plan.as_slice(),
            [FunctionStatementPlan::Assignment(target)]
                if target.is_whole() && target.target() == &result
        )
        || !is_unit_runtime_range(&index.range, function)
        || !is_break_after_cap(&block.cond, &binder)
        || !is_sum_update(update, &result, &binder)
    {
        return Err(unsupported_reduction(function));
    }
    require_span(*span, "function capped for reduction")?;
    require_span(*break_span, "function break statement")?;
    Ok(FunctionPlan::IntegerReduction {
        initial,
        result,
        reduction: FunctionIntegerReduction::ForInclusiveCapped,
    })
}

fn is_exclusive_bound(
    expression: &Expression,
    index: &VarName,
    function: &rumoca_core::Function,
) -> bool {
    matches!(
        expression,
        Expression::Binary {
            op: OpBinary::Lt,
            lhs,
            rhs,
            ..
        } if is_reference(lhs, index) && is_integer_input_reference(rhs, function)
    )
}

fn is_unit_runtime_range(expression: &Expression, function: &rumoca_core::Function) -> bool {
    matches!(
        expression,
        Expression::Range {
            start,
            step,
            end,
            ..
        } if is_integer_one(start)
            && step.as_deref().is_none_or(is_integer_one)
            && is_integer_input_reference(end, function)
    )
}

fn is_break_after_cap(expression: &Expression, binder: &VarName) -> bool {
    matches!(
        expression,
        Expression::Binary {
            op: OpBinary::Gt,
            lhs,
            rhs,
            ..
        } if is_reference(lhs, binder)
            && matches!(
                rhs.as_ref(),
                Expression::Literal {
                    value: Literal::Integer(value),
                    ..
                } if *value >= 1
            )
    )
}

fn is_sum_update(expression: &Expression, target: &VarName, term: &VarName) -> bool {
    matches!(
        expression,
        Expression::Binary {
            op: OpBinary::Add | OpBinary::AddElem,
            lhs,
            rhs,
            ..
        } if is_reference(lhs, target) && is_reference(rhs, term)
    )
}

fn is_increment(expression: &Expression, target: &VarName) -> bool {
    matches!(
        expression,
        Expression::Binary {
            op: OpBinary::Add | OpBinary::AddElem,
            lhs,
            rhs,
            ..
        } if is_reference(lhs, target) && is_integer_one(rhs)
    )
}

fn is_reference(expression: &Expression, target: &VarName) -> bool {
    matches!(
        expression,
        Expression::VarRef {
            name, subscripts, ..
        } if name.var_name() == target && subscripts.is_empty()
    )
}

fn is_integer_input_reference(expression: &Expression, function: &rumoca_core::Function) -> bool {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return false;
    };
    subscripts.is_empty()
        && function.inputs.iter().any(|input| {
            input.name == name.as_str() && input.type_name == "Integer" && input.dims.is_empty()
        })
}

fn is_output_integer(function: &rumoca_core::Function, name: &VarName) -> bool {
    function.outputs.iter().any(|output| {
        output.name == name.as_str() && output.type_name == "Integer" && output.dims.is_empty()
    })
}

fn is_local_integer(function: &rumoca_core::Function, name: &VarName) -> bool {
    function.locals.iter().any(|local| {
        local.name == name.as_str() && local.type_name == "Integer" && local.dims.is_empty()
    })
}

fn is_integer_zero(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::Literal {
            value: Literal::Integer(0),
            ..
        }
    )
}

fn is_integer_one(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::Literal {
            value: Literal::Integer(1),
            ..
        }
    )
}

fn contains_break(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Break { .. } => true,
        rumoca_core::Statement::For { equations, .. } => contains_break(equations),
        rumoca_core::Statement::While { block, .. } => contains_break(&block.stmts),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| contains_break(&block.stmts))
                || else_block.as_deref().is_some_and(contains_break)
        }
        _ => false,
    })
}

fn unsupported_reduction(function: &rumoca_core::Function) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "function loop reduction",
        format!(
            "`{}` does not have a proved finite arithmetic-series form",
            function.name
        ),
        function.span,
    )
}
