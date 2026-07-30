use super::*;

pub(super) fn validate_guarded_function_return(
    function: &rumoca_core::Function,
    context: FunctionValidationContext<'_>,
) -> Result<Option<FunctionPlan>, ToDaeError> {
    if !contains_return(&function.body) {
        return Ok(None);
    }
    let Some((first, tail)) = function.body.split_first() else {
        unreachable!("a function containing return has a statement")
    };
    let rumoca_core::Statement::If {
        cond_blocks,
        else_block: None,
        span,
    } = first
    else {
        return unsupported_return_shape(function, first);
    };
    require_span(*span, "function return conditional")?;
    if cond_blocks.is_empty() || tail.is_empty() {
        return unsupported_return_shape(function, first);
    }

    let targets = function
        .outputs
        .iter()
        .map(|output| VarName::new(&output.name))
        .collect::<Vec<_>>();
    let mut branches = Vec::with_capacity(cond_blocks.len());
    for block in cond_blocks {
        validate_function_expression_with_roles(&block.cond, context.roles, context.flat)?;
        let Some((rumoca_core::Statement::Return { span }, statements)) = block.stmts.split_last()
        else {
            return unsupported_return_shape(function, first);
        };
        require_span(*span, "function return statement")?;
        let plans = validate_function_statements(statements, context)?;
        validate_return_definitions(function, statements, &plans, &targets, *span)?;
        branches.push(plans);
    }
    let tail = validate_function_statements(tail, context)?;
    if targets
        .iter()
        .any(|target| !sequence_defines_target(&tail, target))
    {
        return Err(ToDaeError::unsupported_flat(
            "function return",
            format!(
                "`{}` must define every output on its non-returning path",
                function.name
            ),
            *span,
        ));
    }
    Ok(Some(FunctionPlan::GuardedReturn {
        branches,
        tail,
        targets,
    }))
}

fn validate_return_definitions(
    function: &rumoca_core::Function,
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    targets: &[VarName],
    span: Span,
) -> Result<(), ToDaeError> {
    let mut assigned = HashSet::new();
    for (statement, plan) in statements.iter().zip(plans) {
        let statement_span =
            required_statement_span(statement, "guarded function return definition")?;
        let (
            rumoca_core::Statement::Assignment { value, .. },
            FunctionStatementPlan::Assignment(assignment),
        ) = (statement, plan)
        else {
            return unsupported_return_shape(function, statement);
        };
        if !assignment.is_whole() {
            return unsupported_return_shape(function, statement);
        }
        let target = assignment.target();
        let mut references = Vec::new();
        value.collect_var_refs(&mut references);
        if references
            .iter()
            .any(|reference| assigned.contains(reference))
            || !targets.contains(target)
            || !assigned.insert(target.clone())
        {
            return Err(ToDaeError::unsupported_flat(
                "function return",
                format!(
                    "`{}` requires independent whole-output definitions before return",
                    function.name
                ),
                statement_span,
            ));
        }
    }
    if assigned.len() != targets.len() {
        return Err(ToDaeError::unsupported_flat(
            "function return",
            format!(
                "`{}` must define every output before returning",
                function.name
            ),
            span,
        ));
    }
    Ok(())
}

fn sequence_defines_target(plans: &[FunctionStatementPlan], target: &VarName) -> bool {
    plans.iter().any(|plan| match plan {
        FunctionStatementPlan::Assignment(assignment) if assignment.is_whole() => {
            assignment.target() == target
        }
        FunctionStatementPlan::If { targets, .. } => targets.contains(target),
        _ => false,
    })
}

fn contains_return(statements: &[rumoca_core::Statement]) -> bool {
    statements.iter().any(|statement| match statement {
        rumoca_core::Statement::Return { .. } => true,
        rumoca_core::Statement::For { equations, .. } => contains_return(equations),
        rumoca_core::Statement::While { block, .. } => contains_return(&block.stmts),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks
                .iter()
                .any(|block| contains_return(&block.stmts))
                || else_block.as_deref().is_some_and(contains_return)
        }
        _ => false,
    })
}

fn unsupported_return_shape<T>(
    function: &rumoca_core::Function,
    statement: &rumoca_core::Statement,
) -> Result<T, ToDaeError> {
    let span = required_statement_span(statement, "unsupported guarded function return statement")?;
    Err(ToDaeError::unsupported_flat(
        "function return",
        format!(
            "`{}` requires a leading guarded return with total output definitions",
            function.name
        ),
        span,
    ))
}
