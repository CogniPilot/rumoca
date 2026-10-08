use super::*;
use crate::construction::analysis::function_definitions::{
    condition_implies_guard, reads_only_immutable,
};

/// Seed loop scratch that is written and read only under one guard, and return
/// each seeded value with that guard. The seed is dead only while the guard
/// selects every read; the caller owns that proof past the sequence.
/// A guard that reads a value the function can write is never seeded: the
/// proof would name the value the guard had at the guarded statement while a
/// later read sees the value it has now, so such a conditional is left to the
/// value-fact join, which forgets a fact when its value is written.
pub(super) fn seed_guarded_sequence_scratch(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<Vec<GuardedSeed>, ToDaeError> {
    let mut seeded = Vec::new();
    for outer_index in 0..plans.len() {
        let Some((guard, branch)) = guarded_sequence(&statements[outer_index]) else {
            continue;
        };
        if !reads_only_immutable(guard, context) {
            continue;
        }
        let Some(inner_len) = guarded_plan_len(&plans[outer_index]) else {
            continue;
        };
        for inner_index in 0..inner_len {
            let candidates = guarded_plan_targets(&plans[outer_index], inner_index);
            for target in candidates {
                seeded.extend(seed_guarded_candidate(
                    (statements, branch),
                    (outer_index, inner_index),
                    &target,
                    guard,
                    context,
                    definitions,
                    &mut plans[outer_index],
                )?);
            }
        }
    }
    Ok(seeded)
}

fn seed_guarded_candidate(
    sources: (&[rumoca_core::Statement], &[rumoca_core::Statement]),
    indices: (usize, usize),
    target: &VarName,
    guard: &Expression,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
    plan: &mut FunctionStatementPlan,
) -> Result<Option<GuardedSeed>, ToDaeError> {
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
        return Ok(None);
    }
    let span = required_statement_span(&branch[indices.1], "guarded function-loop scratch")?;
    let seed = definitions.whole_loop_seed(target, context, span)?;
    attach_guarded_seed(plan, indices.1, target, seed);
    definitions.define_whole(target);
    Ok(Some(GuardedSeed {
        target: target.clone(),
        guard: guard.clone(),
        span: required_statement_span(&statements[indices.0], "guarded function-loop scratch")?,
    }))
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
            .any(|statement| has_unguarded_target_read(statement, (target, guard), context, false))
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
    (target, guard): (&VarName, &Expression),
    context: FunctionValidationContext<'_>,
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
                || equations.iter().any(|statement| {
                    has_unguarded_target_read(statement, (target, guard), context, guarded)
                })
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| {
                (!guarded && expression_reads_target(&block.cond, target))
                    || block.stmts.iter().any(|statement| {
                        let branch_guarded =
                            guarded || condition_implies_guard(&block.cond, guard, context, 0);
                        has_unguarded_target_read(
                            statement,
                            (target, guard),
                            context,
                            branch_guarded,
                        )
                    })
            }) || else_block.as_ref().is_some_and(|statements| {
                statements.iter().any(|statement| {
                    has_unguarded_target_read(statement, (target, guard), context, guarded)
                })
            })
        }
        _ => !guarded && statement_reads_target(statement, target),
    }
}

pub(in crate::construction::analysis) fn statement_reads_target(
    statement: &rumoca_core::Statement,
    target: &VarName,
) -> bool {
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

/// A dead seed issued for loop scratch that one guard both writes and reads.
pub(super) struct GuardedSeed {
    target: VarName,
    guard: Expression,
    /// The guarded conditional whose false path keeps the seed.
    span: Span,
}

/// Confine each seed to the paths its guard selects once `statements` end.
///
/// MLS §12.4.4 leaves a function local undefined until it is assigned. The
/// seed only gives the compact transition a carried slot; on the path where
/// the guard is false it is no definition. Past the region a value no
/// statement there writes outside the guard is therefore defined only under
/// that guard, and an unguarded read of it is refused as an undefined read.
pub(super) fn confine_guarded_seeds(
    statements: &[rumoca_core::Statement],
    seeds: Vec<GuardedSeed>,
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) {
    for seed in seeds {
        let unguarded_write = statements.iter().any(|statement| {
            has_unguarded_target_write(statement, (&seed.target, &seed.guard), context, false)
        });
        if !unguarded_write {
            definitions.guard_seeded_value(&seed.target, seed.guard, seed.span);
        }
    }
}

fn has_unguarded_target_write(
    statement: &rumoca_core::Statement,
    (target, guard): (&VarName, &Expression),
    context: FunctionValidationContext<'_>,
    guarded: bool,
) -> bool {
    match statement {
        rumoca_core::Statement::For { equations, .. } => equations.iter().any(|statement| {
            has_unguarded_target_write(statement, (target, guard), context, guarded)
        }),
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            cond_blocks.iter().any(|block| {
                let branch_guarded =
                    guarded || condition_implies_guard(&block.cond, guard, context, 0);
                block.stmts.iter().any(|statement| {
                    has_unguarded_target_write(statement, (target, guard), context, branch_guarded)
                })
            }) || else_block.iter().flatten().any(|statement| {
                has_unguarded_target_write(statement, (target, guard), context, guarded)
            })
        }
        _ => !guarded && statement_assigns_target(statement, target),
    }
}
