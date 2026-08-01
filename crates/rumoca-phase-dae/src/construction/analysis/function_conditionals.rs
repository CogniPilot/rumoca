use super::*;

/// Prove the checked owner shape of one MLS §11.5 function conditional.
pub(super) fn plan_function_conditional(
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
    let mut branches = Vec::with_capacity(blocks.len());
    for block in blocks {
        validate_function_expression_with_roles(
            &block.cond,
            context.roles,
            context.flat,
            context.shapes,
        )?;
        branches.push(plan_function_statements(&block.stmts, context)?);
    }
    let fallback = fallback
        .map(|statements| plan_function_statements(statements, context))
        .transpose()?;
    Ok(FunctionStatementPlan::If {
        branches,
        fallback,
        targets: Vec::new(),
    })
}

/// Prove which values one function conditional defines on all of its paths.
///
/// Each branch is an ordinary MLS §11 algorithm section: its statements run in
/// order and the last write to a value wins, so a branch may assign the same
/// value repeatedly and may read what it already assigned. The branch keeps its
/// own definedness certificate, and the join keeps exactly the values the
/// conditional owns once it finishes.
pub(super) fn resolve_function_conditional(
    blocks: &[rumoca_core::StatementBlock],
    fallback_statements: Option<&[rumoca_core::Statement]>,
    branches: &mut [Vec<FunctionStatementPlan>],
    fallback_plans: Option<&mut Vec<FunctionStatementPlan>>,
    span: Span,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<Vec<VarName>, ToDaeError> {
    let mut branch_states = Vec::with_capacity(branches.len() + 1);
    let mut ordered = Vec::new();
    for (block, plans) in blocks.iter().zip(branches.iter_mut()) {
        definitions.require_readable(&block.cond, context, span)?;
        let mut state = definitions.clone();
        resolve_conditional_branch(&block.stmts, plans, context, &mut state)?;
        collect_branch_targets(plans, &mut ordered);
        branch_states.push(state);
    }
    let exhaustive = match (fallback_statements, fallback_plans) {
        (Some(statements), Some(plans)) => {
            let mut state = definitions.clone();
            resolve_conditional_branch(statements, plans, context, &mut state)?;
            collect_branch_targets(plans, &mut ordered);
            branch_states.push(state);
            true
        }
        (None, None) => false,
        _ => unreachable!("a planned function conditional keeps its source fallback shape"),
    };
    if ordered.is_empty() {
        return Err(ToDaeError::unsupported_flat(
            "function conditional",
            format!(
                "`{}` has a conditional branch without a value definition",
                context.function.name
            ),
            span,
        ));
    }
    definitions.join_branches(&branch_states, exhaustive, &ordered, context, span)
}

/// A branch value has no owner outside the conditional expression that selects
/// it, so a branch may only contain direct assignments: a nested loop or
/// conditional would need its own unconditional owner in the function body.
fn resolve_conditional_branch(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    for (statement, plan) in statements.iter().zip(plans.iter()) {
        if matches!(plan, FunctionStatementPlan::Assignment(_)) {
            continue;
        }
        let span = required_statement_span(statement, "function conditional branch statement")?;
        return Err(ToDaeError::unsupported_flat(
            "function conditional",
            format!(
                "`{}` requires direct value assignments in every checked branch",
                context.function.name
            ),
            span,
        ));
    }
    resolve_function_definitions(statements, plans, context, definitions)
}

fn collect_branch_targets(plans: &[FunctionStatementPlan], ordered: &mut Vec<VarName>) {
    for plan in plans {
        let FunctionStatementPlan::Assignment(assignment) = plan else {
            continue;
        };
        if !ordered.contains(assignment.target()) {
            ordered.push(assignment.target().clone());
        }
    }
}
