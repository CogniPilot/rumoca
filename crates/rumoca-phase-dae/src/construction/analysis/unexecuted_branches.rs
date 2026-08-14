//! Statement checking for the branches MLS §11.5 does not execute.
//!
//! When a value-proven specialization settles a function conditional's
//! conditions, `plan_function_conditional` plans only the selected branch and
//! the DAE lowering never sees the others. MLS §11.5 justifies that for
//! *execution* — but well-formedness is not execution. This module carries the
//! part of the base compiler's strength that the fold would otherwise drop.

use super::*;

/// Prove every unexecuted branch is still a legal algorithm section.
///
/// # Acceptance contract (SPEC_0008 §"Acceptance Contract Before Rejection")
///
/// MLS §11.2.1 makes an assignment statement's compatibility between its
/// right-hand-side expression and its component reference a property of *the
/// statement*. MLS §11.5 says which statements run; it does not say which are
/// well formed. Before this phase folded proven conditionals, an unexecuted
/// branch was lowered like any other and the DAE constructor rejected a
/// mismatched assignment there (`ED020`, "expression shape mismatch"). Folding
/// removes the lowering, so the check moves here and the strength is kept.
///
/// **Rejected.** An assignment whose target shape and value shape this scope
/// both proves and which disagree — checked with the same `expression_shape`
/// rule every executed statement is proven with, so the two cannot drift.
///
/// **Exempt — not an error, and deliberately so.** Anything this scope cannot
/// answer *without owning a proof the executed path never needs*:
///
/// * a value containing a function call. Specializing the callee is what would
///   answer its result shape, and an unexecuted branch must mint no
///   certificate: `symmetricOrientation(3)`'s unexecuted arm calls
///   `symmetricOrientation(1)`, which the program never evaluates.
/// * an extent only the unexecuted path settles — a comprehension, a compact
///   range, or a subscript over a value this specialization does not fix. MLS
///   §4.4.2 evaluability is required of the extents a *constructed* function
///   declares, and no function is constructed from this branch.
/// * every statement form other than an assignment, plus the nested bodies of
///   loops and conditionals, which are walked for their assignments only.
///
/// Both exemptions are one-sided: they can only fail to report, never accept a
/// mismatch this scope proved. A branch whose two sides are both proven is
/// checked exactly as if it were executed.
///
/// **Owner.** `plan_proven_conditional_branch`, the only place a branch becomes
/// unexecuted.
///
/// **Evidence.** `rumoca/tests/function_proven_branch_test.rs`:
/// `a_dead_arm_shape_error_is_still_rejected` (rejected) and
/// `a_dead_arm_call_no_specialization_can_prove_stays_accepted` (exempt).
pub(super) fn check_unexecuted_branches(
    blocks: &[rumoca_core::StatementBlock],
    fallback: Option<&[rumoca_core::Statement]>,
    selected: Option<usize>,
    context: FunctionValidationContext<'_>,
) -> Result<(), ToDaeError> {
    for (ordinal, block) in blocks.iter().enumerate() {
        if selected == Some(ordinal) {
            continue;
        }
        check_statements(&block.stmts, context, context.shapes)?;
    }
    if selected.is_some()
        && let Some(fallback) = fallback
    {
        check_statements(fallback, context, context.shapes)?;
    }
    Ok(())
}

/// Check assignment compatibility before semantics-preserving normalization.
///
/// Loop and scratch compaction may remove a source statement whose value is
/// dead, but MLS §11.2.1 well-formedness is a property of that statement, not
/// of its liveness. Running this call-free proof before normalization retains
/// every mismatch the specialization can establish without minting a callee
/// certificate for an unexecuted path.
pub(super) fn check_function_assignment_shapes(
    statements: &[rumoca_core::Statement],
    context: FunctionValidationContext<'_>,
) -> Result<(), ToDaeError> {
    check_statements(statements, context, context.shapes)
}

fn check_statements(
    statements: &[rumoca_core::Statement],
    context: FunctionValidationContext<'_>,
    shapes: &ShapeEnvironment,
) -> Result<(), ToDaeError> {
    for statement in statements {
        check_statement(statement, context, shapes)?;
    }
    Ok(())
}

fn check_statement(
    statement: &rumoca_core::Statement,
    context: FunctionValidationContext<'_>,
    shapes: &ShapeEnvironment,
) -> Result<(), ToDaeError> {
    match statement {
        rumoca_core::Statement::Assignment { comp, value, span } => {
            check_assignment(comp, value, *span, context, shapes)
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            // MLS §11.2.2 opens each index as a fresh scalar of the loop, so the
            // body is checked in a scope where the index shadows any enclosing
            // coordinate and carries no proven value.
            let mut loop_shapes = shapes.clone();
            for index in indices {
                loop_shapes.insert(VarName::new(&index.ident), Vec::new());
            }
            check_statements(equations, context, &loop_shapes)
        }
        rumoca_core::Statement::While { block, .. } => {
            check_statements(&block.stmts, context, shapes)
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                check_statements(&block.stmts, context, shapes)?;
            }
            check_statements(else_block.as_deref().unwrap_or_default(), context, shapes)
        }
        rumoca_core::Statement::When { blocks, .. } => {
            for block in blocks {
                check_statements(&block.stmts, context, shapes)?;
            }
            Ok(())
        }
        rumoca_core::Statement::FunctionCall { .. }
        | rumoca_core::Statement::Reinit { .. }
        | rumoca_core::Statement::Assert { .. }
        | rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => Ok(()),
    }
}

fn check_assignment(
    component: &rumoca_core::ComponentReference,
    value: &Expression,
    span: Span,
    context: FunctionValidationContext<'_>,
    shapes: &ShapeEnvironment,
) -> Result<(), ToDaeError> {
    let (Some(target), Some(found)) = (
        call_free_target_shape(component, shapes),
        call_free_expression_shape(value, shapes),
    ) else {
        return Ok(());
    };
    // MLS §10.6.13 lets a scalar right-hand side fill an array target, which is
    // the one shape relation an assignment admits beyond equality.
    if target == found || found.is_empty() {
        return Ok(());
    }
    Err(ToDaeError::unsupported_flat(
        "function conditional",
        format!(
            "`{}` assigns a value of shape {found:?} to a target of shape {target:?}; MLS \
             §11.2.1 requires the two to be compatible in every branch, including the ones \
             this specialization proves unreachable",
            context.function.name
        ),
        span,
    ))
}
