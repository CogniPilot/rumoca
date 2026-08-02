use super::*;

/// The statements MLS §11.5 executes when `selected` is the proven branch.
///
/// `Some(ordinal)` names the first condition branch whose condition holds and
/// `None` names the else part, which is empty when the conditional declares
/// none — exactly the two outcomes MLS §11.5 defines for a settled condition
/// sequence.
pub(in crate::construction) fn selected_conditional_statements<'statement>(
    blocks: &'statement [rumoca_core::StatementBlock],
    fallback: Option<&'statement [rumoca_core::Statement]>,
    selected: Option<usize>,
) -> &'statement [rumoca_core::Statement] {
    match selected {
        Some(ordinal) => &blocks[ordinal].stmts,
        None => fallback.unwrap_or_default(),
    }
}

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
    if let Some(selected) = proven_conditional_branch(blocks, context.shapes) {
        return plan_proven_conditional_branch(blocks, fallback, selected, context);
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

/// Plan the one branch this specialization proves MLS §11.5 executes.
///
/// # Acceptance contract (SPEC_0008 §"Acceptance Contract Before Rejection")
///
/// MLS §11.5 executes the statements of the first branch whose condition
/// evaluates to `true`, and the else part when none does. When the enclosing
/// value-proven specialization settles every condition it has to evaluate, that
/// choice is fixed at translation time: the selected statements are the
/// function's behaviour and the others are unreachable. MLS §12.2's rule that a
/// body is written over the inputs is what makes the choice settled — the
/// specialization exists precisely because those inputs carry proven values.
///
/// **Accepted.** The selected statements, planned as the ordinary MLS §11
/// algorithm section they are: nested loops, nested conditionals, and array
/// assemblies all keep their usual owners, because nothing about them is
/// conditional any more. This is what the "direct value assignments in every
/// checked branch" rule below cannot admit, and it does not have to: that rule
/// exists because a *runtime* branch value has no owner outside the conditional
/// expression that selects it, and a proven branch has no such expression.
///
/// **Typed-rejected.** Nothing new about the executed path. A conditional this
/// scope does not settle keeps the checked-branch rule unchanged, reported at
/// the same statement.
///
/// **Gate.** This rule reaches only a conditional whose conditions the
/// *specialization key* settles, and `ValueReadInputs` puts an input in that key
/// only when a declared dimension, compact range, or `zeros`/`ones`/`fill`
/// extent reads its value. So `f(m)` with `output Real y[m]` folds `if m == 3`
/// and `f(m)` with `output Real y[3]` does not, even though both calls pass a
/// literal. That gate is deliberate and load-bearing in two directions: it is
/// what keeps MLS §4.5 non-structural parameters — values the initialization
/// problem may establish — out of translation-time control flow, and it is what
/// bounds the specialization count so a recursive callee reaches a repeated key.
/// The cost is that an unrelated edit to a declared dimension changes whether a
/// conditional folds, which is a visible property of the language rule and not
/// an accident of this analysis.
///
/// **Not proven, and never guessed.** The statements of the branches MLS §11.5
/// does not execute contribute no value, no call, and no shape to this
/// specialization: no certificate is minted for a callee only they reach, and
/// no extent proof is demanded of them. They are *not* thereby unchecked —
/// `check_unexecuted_branches` still proves every statement well formed under
/// MLS §11.2.1, which is what the base compiler's lowering did before the fold
/// existed.
///
/// **Owner.** `plan_function_conditional`, the only producer of a function
/// conditional's plan.
///
/// **Evidence.** `rumoca/tests/function_proven_branch_test.rs`:
/// `a_proven_false_condition_selects_the_else_arm` and
/// `a_proven_true_condition_selects_a_nested_arm` (accepted, over the MSL
/// `symmetricOrientation` shape, whose unexecuted arm keeps a recursive call
/// and a `fill` extent that nothing proves), and
/// `an_unproven_condition_keeps_the_checked_branch_rule` (rejected),
/// `a_declared_dimension_that_reads_the_input_is_what_enables_the_fold`
/// (the gate, both directions), and
/// `a_dead_arm_shape_error_is_still_rejected`.
fn plan_proven_conditional_branch(
    blocks: &[rumoca_core::StatementBlock],
    fallback: Option<&[rumoca_core::Statement]>,
    selected: Option<usize>,
    context: FunctionValidationContext<'_>,
) -> Result<FunctionStatementPlan, ToDaeError> {
    check_unexecuted_branches(blocks, fallback, selected, context)?;
    let statements = selected_conditional_statements(blocks, fallback, selected);
    Ok(FunctionStatementPlan::ProvenBranch {
        selected,
        statements: plan_function_statements(statements, context)?,
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

/// Prove that every statement in a runtime branch has an expression owner.
///
/// A nested conditional has exactly such an owner: its own checked conditional
/// expressions, derived from the branch-local definition state.  A loop still
/// needs a compact transition owner, which cannot be embedded in an expression
/// branch, so it remains rejected here.
fn resolve_conditional_branch(
    statements: &[rumoca_core::Statement],
    plans: &mut [FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
    definitions: &mut FunctionDefinitions,
) -> Result<(), ToDaeError> {
    validate_conditional_branch_shape(statements, plans, context)?;
    resolve_function_definitions(statements, plans, context, definitions)
}

fn validate_conditional_branch_shape(
    statements: &[rumoca_core::Statement],
    plans: &[FunctionStatementPlan],
    context: FunctionValidationContext<'_>,
) -> Result<(), ToDaeError> {
    for (statement, plan) in statements.iter().zip(plans) {
        match (statement, plan) {
            (_, FunctionStatementPlan::ProvenAssertion) => continue,
            (_, FunctionStatementPlan::Assignment(_)) => continue,
            (
                rumoca_core::Statement::If {
                    cond_blocks,
                    else_block,
                    ..
                },
                FunctionStatementPlan::If {
                    branches, fallback, ..
                },
            ) => {
                for (block, branch) in cond_blocks.iter().zip(branches) {
                    validate_conditional_branch_shape(&block.stmts, branch, context)?;
                }
                if let (Some(source), Some(branch)) = (else_block.as_deref(), fallback.as_deref()) {
                    validate_conditional_branch_shape(source, branch, context)?;
                }
                continue;
            }
            (
                rumoca_core::Statement::If {
                    cond_blocks,
                    else_block,
                    ..
                },
                FunctionStatementPlan::ProvenBranch {
                    selected,
                    statements,
                },
            ) => {
                let source =
                    selected_conditional_statements(cond_blocks, else_block.as_deref(), *selected);
                validate_conditional_branch_shape(source, statements, context)?;
                continue;
            }
            _ => {}
        }
        let span = required_statement_span(statement, "function conditional branch statement")?;
        return Err(ToDaeError::unsupported_flat(
            "function conditional",
            format!(
                "`{}` requires assignments or nested conditionals in every checked branch",
                context.function.name
            ),
            span,
        ));
    }
    Ok(())
}

fn collect_branch_targets(plans: &[FunctionStatementPlan], ordered: &mut Vec<VarName>) {
    for plan in plans {
        match plan {
            FunctionStatementPlan::Assignment(assignment) => {
                collect_branch_target(assignment.target(), ordered);
            }
            FunctionStatementPlan::If { targets, .. } => {
                for target in targets {
                    collect_branch_target(target, ordered);
                }
            }
            FunctionStatementPlan::ProvenBranch { statements, .. } => {
                collect_branch_targets(statements, ordered);
            }
            _ => {}
        }
    }
}

fn collect_branch_target(target: &VarName, ordered: &mut Vec<VarName>) {
    if !ordered.contains(target) {
        ordered.push(target.clone());
    }
}
