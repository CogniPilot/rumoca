//! Tearing over proven causal coefficients (SPEC_0043 §4).
//!
//! A causal step divides by its isolated coefficient on every call, so it is
//! admissible only with a construction proof that the coefficient is bounded
//! away from zero. A block whose structural tearing uses a step without such a
//! proof is torn again over the proven candidates alone, so an alternative
//! causal step replaces it where one exists; a step that still has none is
//! promoted to a tear when the refresh plan is built.

use std::collections::HashSet;

use rumoca_eval_solve::refresh_plan::causal_step_is_proven;
use rumoca_eval_solve::{EvalSolveError, PreparedScalarProgramBlock};
use rumoca_ir_solve as solve;

/// Re-tear every block whose tearing isolates a variable without a proof.
pub(crate) fn retear_unproven_blocks(
    problem: &mut solve::SolveProblem,
) -> Result<(), EvalSolveError> {
    let blocks = &problem.continuous.algebraic_projection_plan.blocks;
    if blocks
        .explicit_blocks()
        .filter_map(|block| block.tearing.as_ref())
        .all(|tearing| tearing.causal_steps.is_empty())
    {
        return Ok(());
    }
    let prepared = PreparedScalarProgramBlock::new(
        rumoca_eval_solve::to_scalar_program_projection(&problem.continuous.implicit_rhs)?
            .into_block(),
    )?;
    let blocks = &problem.continuous.algebraic_projection_plan.blocks;
    let targets_shared = problem
        .continuous
        .implicit_row_targets
        .shares_projection_owner(blocks);
    let rewritten = blocks.map_explicit_blocks(|block| {
        retear_block(block, &prepared);
        Ok::<_, EvalSolveError>(())
    })?;
    if targets_shared {
        problem.continuous.implicit_row_targets =
            solve::ImplicitRowTargets::from_projection(&rewritten);
    }
    problem.continuous.algebraic_projection_plan.blocks = rewritten;
    Ok(())
}

fn retear_block(
    block: &mut solve::AlgebraicProjectionBlock,
    prepared: &PreparedScalarProgramBlock,
) {
    let Some(tearing) = block.tearing.as_ref() else {
        return;
    };
    if tearing
        .causal_steps
        .iter()
        .all(|step| causal_step_is_proven(prepared, step.row, step.y_index))
    {
        return;
    }
    let n = block.rows.len();
    if block.y_indices.len() != n {
        return;
    }
    // One dependency analysis per row serves every unknown of the block.
    let reads = |row: usize| {
        prepared
            .row_output_position(row)
            .and_then(|(program, output)| {
                Some(solve::output_y_reads(
                    prepared.block().program(program)?,
                    output,
                ))
            })
            .unwrap_or(solve::OutputYReads::Absent)
    };
    let mut eq_unknowns = vec![HashSet::new(); n];
    let mut candidates = vec![HashSet::new(); n];
    for (equation, &row) in block.rows.iter().enumerate() {
        let row_reads = reads(row);
        for (unknown, &y_index) in block.y_indices.iter().enumerate() {
            if !row_reads.contains(y_index) {
                continue;
            }
            eq_unknowns[equation].insert(unknown);
            if causal_step_is_proven(prepared, row, y_index) {
                candidates[equation].insert(unknown);
            }
        }
    }
    // Promoting the unproven steps keeps the issued tearing valid with this
    // many tears; the re-tearing replaces it only when it needs no more.
    let promoted = tearing.tear_y_indices.len()
        + tearing
            .causal_steps
            .iter()
            .filter(|step| !causal_step_is_proven(prepared, step.row, step.y_index))
            .count();
    if let Some(result) = rumoca_phase_structural::tear_algebraic_loop_with_causal_candidates(
        n,
        &eq_unknowns,
        &candidates,
    ) && result.tear_var_local_indices.len() <= promoted
    {
        block.tearing = Some(crate::lower::solve_block_tearing(
            &result,
            &block.rows,
            &block.y_indices,
        ));
    }
}
