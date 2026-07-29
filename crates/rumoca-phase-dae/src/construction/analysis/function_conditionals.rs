use super::*;

pub(super) fn validate_function_conditional(
    blocks: &[rumoca_core::StatementBlock],
    fallback: Option<&[rumoca_core::Statement]>,
    span: Span,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionStatementPlan, ToDaeError> {
    require_span(span, "function if statement")?;
    if blocks.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "function conditional",
            "a function conditional must contain at least one condition branch",
            span,
        ));
    }
    let mut branch_plans = Vec::with_capacity(blocks.len());
    let mut expected_targets = None;
    for block in blocks {
        validate_function_expression_with_roles(&block.cond, context.roles, context.flat)?;
        let plans = validate_function_statements(&block.stmts, context)?;
        let targets = total_conditional_targets(&block.stmts, &plans, context, span)?;
        require_same_conditional_targets(&mut expected_targets, &targets, context, span)?;
        branch_plans.push(plans);
    }
    let fallback_plans = fallback
        .map(|fallback| {
            let plans = validate_function_statements(fallback, context)?;
            let fallback_targets = total_conditional_targets(fallback, &plans, context, span)?;
            require_same_conditional_targets(
                &mut expected_targets,
                &fallback_targets,
                context,
                span,
            )?;
            Ok::<_, ToDaeError>(plans)
        })
        .transpose()?;
    Ok(FunctionStatementPlan::If {
        branches: branch_plans,
        fallback: fallback_plans,
        targets: expected_targets.expect("nonempty condition branches establish targets"),
    })
}

fn total_conditional_targets(
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    span: Span,
) -> Result<Vec<VarName>, ToDaeError> {
    let mut targets = Vec::with_capacity(plans.len());
    let mut assigned = HashSet::new();
    for (statement, plan) in statements.iter().zip(plans) {
        let (
            rumoca_core::Statement::Assignment { value, .. },
            FunctionStatementPlan::Assignment(assignment),
        ) = (statement, plan)
        else {
            return Err(ToDaeError::unsupported_flat(
                "function conditional",
                format!(
                    "`{}` requires direct whole-value assignments in every checked branch",
                    context.function.name
                ),
                statement.source_span().unwrap_or(span),
            ));
        };
        let target = assignment.target();
        let mut references = Vec::new();
        value.collect_var_refs(&mut references);
        if let Some(dependency) = references
            .iter()
            .find(|reference| assigned.contains(*reference))
        {
            return Err(ToDaeError::unsupported_flat(
                "function conditional",
                format!(
                    "`{}` branch assignment reads earlier branch-local value `{dependency}`",
                    context.function.name
                ),
                statement.source_span().unwrap_or(span),
            ));
        }
        if !assigned.insert(target.clone()) {
            return Err(ToDaeError::unsupported_flat(
                "function conditional",
                format!(
                    "`{}` assigns `{target}` more than once in one conditional branch",
                    context.function.name
                ),
                statement.source_span().unwrap_or(span),
            ));
        }
        targets.push(target.clone());
    }
    if targets.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "function conditional",
            format!(
                "`{}` has a conditional branch without a value definition",
                context.function.name
            ),
            span,
        ));
    }
    Ok(targets)
}

fn require_same_conditional_targets(
    expected: &mut Option<Vec<VarName>>,
    found: &[VarName],
    context: FunctionValidationContext<'_>,
    span: Span,
) -> Result<(), ToDaeError> {
    let found_set = found.iter().cloned().collect::<HashSet<_>>();
    if let Some(expected) = expected {
        let expected_set = expected.iter().cloned().collect::<HashSet<_>>();
        if expected_set != found_set {
            return Err(ToDaeError::unsupported_flat(
                "function conditional",
                format!(
                    "`{}` must define the same function values in every branch",
                    context.function.name
                ),
                span,
            ));
        }
    } else {
        *expected = Some(found.to_vec());
    }
    Ok(())
}
