use super::*;

pub(super) fn seed_guarded_sequence_scratch(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    for outer_index in 0..plans.len() {
        let Some((guard, branch)) = guarded_sequence(&statements[outer_index]) else {
            continue;
        };
        let Some(inner_len) = guarded_plan_len(&plans[outer_index]) else {
            continue;
        };
        for inner_index in 0..inner_len {
            let candidates = guarded_plan_targets(&plans[outer_index], inner_index);
            for target in candidates {
                seed_guarded_candidate(
                    (statements, branch),
                    (outer_index, inner_index),
                    &target,
                    guard,
                    context,
                    definitions,
                    &mut plans[outer_index],
                )?;
            }
        }
    }
    Ok(())
}

fn seed_guarded_candidate(
    sources: (&[rumoca_core::Statement], &[rumoca_core::Statement]),
    indices: (usize, usize),
    target: &VarName,
    guard: &Expression,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
    plan: &mut FunctionStatementPlan,
) -> Result<(), ToDaeError> {
    let (statements, branch) = sources;
    if guarded_scratch_is_observable(
        statements,
        branch,
        indices,
        target,
        guard,
        context,
        definitions,
    ) {
        return Ok(());
    }
    let span = required_statement_span(&branch[indices.1], "guarded function-loop scratch")?;
    let seed = definitions.whole_loop_seed(target, context, span)?;
    attach_guarded_seed(plan, indices.1, target, seed);
    definitions.define_whole(target);
    Ok(())
}

fn guarded_sequence(
    statement: &rumoca_core::Statement,
) -> Option<(&Expression, &[rumoca_core::Statement])> {
    let rumoca_core::Statement::If {
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
    Some((&block.cond, &block.stmts))
}

fn guarded_plan_len(plan: &FunctionStatementPlan) -> Option<usize> {
    match plan {
        FunctionStatementPlan::If {
            branches,
            fallback: None,
            ..
        } if branches.len() == 1 => Some(branches[0].len()),
        _ => None,
    }
}

fn guarded_plan_targets(plan: &FunctionStatementPlan, inner_index: usize) -> Vec<VarName> {
    let FunctionStatementPlan::If { branches, .. } = plan else {
        unreachable!("uniform guard proof aligns source and plan conditionals")
    };
    whole_definition_targets(&branches[0][inner_index])
}

fn guarded_scratch_is_observable(
    statements: &[rumoca_core::Statement],
    branch: &[rumoca_core::Statement],
    indices: (usize, usize),
    target: &VarName,
    guard: &Expression,
    context: FunctionValidationContext<'_>,
    definitions: &FunctionDefinitions,
) -> bool {
    definitions.is_defined(target)
        || context
            .function
            .outputs
            .iter()
            .any(|output| output.name == target.as_str())
        || guarded_prefix_reads_name(statements, indices.0, branch, indices.1, target)
        || statements
            .iter()
            .any(|statement| has_unguarded_target_read(statement, target, guard, false))
}

fn attach_guarded_seed(
    plan: &mut FunctionStatementPlan,
    inner_index: usize,
    target: &VarName,
    seed: FunctionValueSeed,
) {
    let FunctionStatementPlan::If { branches, .. } = plan else {
        unreachable!("uniform guard proof aligns source and plan conditionals")
    };
    attach_whole_definition_seed(&mut branches[0][inner_index], target, seed);
}

fn whole_definition_targets(plan: &FunctionStatementPlan) -> Vec<VarName> {
    match plan {
        FunctionStatementPlan::Assignment(assignment) if assignment.is_whole() => {
            vec![assignment.target().clone()]
        }
        FunctionStatementPlan::MultiOutputCall { outputs } => outputs
            .iter()
            .flatten()
            .filter(|output| output.is_whole())
            .map(|output| output.target().clone())
            .collect(),
        FunctionStatementPlan::RecordAssembly(assembly) => vec![assembly.target.clone()],
        FunctionStatementPlan::ArrayAssembly(assembly) => vec![assembly.target.clone()],
        _ => Vec::new(),
    }
}

fn attach_whole_definition_seed(
    plan: &mut FunctionStatementPlan,
    target: &VarName,
    seed: FunctionValueSeed,
) {
    match plan {
        FunctionStatementPlan::Assignment(assignment) if assignment.target() == target => {
            assignment.seed = Some(seed);
        }
        FunctionStatementPlan::MultiOutputCall { outputs } => {
            let output = outputs
                .iter_mut()
                .flatten()
                .find(|output| output.target() == target)
                .expect("whole-definition candidate remains in its plan");
            output.seed = Some(seed);
        }
        FunctionStatementPlan::RecordAssembly(assembly) if &assembly.target == target => {
            assembly.seed = Some(seed);
        }
        FunctionStatementPlan::ArrayAssembly(assembly) if &assembly.target == target => {
            assembly.seed = Some(seed);
        }
        _ => unreachable!("whole-definition candidate remains in its plan"),
    }
}

fn guarded_prefix_reads_name(
    guarded: &[rumoca_core::Statement],
    outer_index: usize,
    branch: &[rumoca_core::Statement],
    inner_index: usize,
    target: &VarName,
) -> bool {
    guarded[..outer_index]
        .iter()
        .any(|statement| statement_reads_target(statement, target))
        || branch[..=inner_index]
            .iter()
            .any(|statement| statement_reads_target(statement, target))
}

fn has_unguarded_target_read(
    statement: &rumoca_core::Statement,
    target: &VarName,
    guard: &Expression,
    guarded: bool,
) -> bool {
    match statement {
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            (!guarded
                && indices
                    .iter()
                    .any(|index| expression_reads_target(&index.range, target)))
                || equations
                    .iter()
                    .any(|statement| has_unguarded_target_read(statement, target, guard, guarded))
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| {
                (!guarded && expression_reads_target(&block.cond, target))
                    || block.stmts.iter().any(|statement| {
                        let branch_guarded = guarded || condition_implies_guard(&block.cond, guard);
                        has_unguarded_target_read(statement, target, guard, branch_guarded)
                    })
            }) || else_block.as_ref().is_some_and(|statements| {
                statements
                    .iter()
                    .any(|statement| has_unguarded_target_read(statement, target, guard, guarded))
            })
        }
        _ => !guarded && statement_reads_target(statement, target),
    }
}

fn condition_implies_guard(condition: &Expression, guard: &Expression) -> bool {
    if rumoca_core::expressions_semantically_equal(condition, guard) {
        return true;
    }
    matches!(
        condition,
        Expression::Binary {
            op: OpBinary::And,
            lhs,
            rhs,
            ..
        } if condition_implies_guard(lhs, guard) || condition_implies_guard(rhs, guard)
    )
}

pub(super) fn statement_reads_target(statement: &rumoca_core::Statement, target: &VarName) -> bool {
    match statement {
        rumoca_core::Statement::Assignment { comp, value, .. } => {
            expression_reads_target(value, target) || component_subscripts_read_target(comp, target)
        }
        rumoca_core::Statement::FunctionCall { args, outputs, .. } => {
            args.iter()
                .any(|argument| expression_reads_target(argument, target))
                || outputs
                    .iter()
                    .flatten()
                    .any(|output| component_subscripts_read_target(output, target))
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            indices
                .iter()
                .any(|index| expression_reads_target(&index.range, target))
                || equations
                    .iter()
                    .any(|statement| statement_reads_target(statement, target))
        }
        rumoca_core::Statement::While { block, .. } => {
            expression_reads_target(&block.cond, target)
                || block
                    .stmts
                    .iter()
                    .any(|statement| statement_reads_target(statement, target))
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| {
                expression_reads_target(&block.cond, target)
                    || block
                        .stmts
                        .iter()
                        .any(|statement| statement_reads_target(statement, target))
            }) || else_block.as_ref().is_some_and(|statements| {
                statements
                    .iter()
                    .any(|statement| statement_reads_target(statement, target))
            })
        }
        rumoca_core::Statement::When { blocks, .. } => blocks.iter().any(|block| {
            expression_reads_target(&block.cond, target)
                || block
                    .stmts
                    .iter()
                    .any(|statement| statement_reads_target(statement, target))
        }),
        rumoca_core::Statement::Reinit {
            variable, value, ..
        } => {
            component_subscripts_read_target(variable, target)
                || expression_reads_target(value, target)
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            expression_reads_target(condition, target)
                || expression_reads_target(message, target)
                || level
                    .as_deref()
                    .is_some_and(|level| expression_reads_target(level, target))
        }
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => false,
    }
}

fn component_subscripts_read_target(
    component: &rumoca_core::ComponentReference,
    target: &VarName,
) -> bool {
    component.parts().iter().any(|part| {
        part.subs.iter().any(|subscript| match subscript {
            Subscript::Expr { expr, .. } => expression_reads_target(expr, target),
            Subscript::Index { .. } | Subscript::Colon { .. } => false,
        })
    })
}

fn expression_reads_target(expression: &Expression, target: &VarName) -> bool {
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    references.iter().any(|reference| reference == target)
}
