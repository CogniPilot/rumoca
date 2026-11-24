//! Block order of the algebraic projection under execution reads.
//!
//! Structural analysis orders the blocks by value dependencies. Executing a
//! row reads more than its value does: one tensor program computes every lane
//! of `der(x[i]) = r[i]*(x[i] - uu[i])` and loads `uu` whole, so the row of
//! lane 1 also loads `uu[2]`. A block that reads a target of a later block
//! refuses the staged refresh schedule (`RefreshExecutionRefusal`) and marks
//! that block as invalidating an earlier one, and every refresh then projects
//! block by block. Where the later block does not depend on the reader, the
//! reader can simply run after it.

#[cfg(test)]
mod tests;

use std::collections::{BTreeMap, BTreeSet};

use rumoca_ir_solve::{
    AlgebraicProjectionBlock, ImplicitRowTargets, ProjectionStage, SolveProblem,
};

use super::reverse_invalidations::execution_row_reads;
use crate::{EvalSolveError, to_scalar_program_block};

/// Reorder the algebraic projection blocks so the targets a block's execution
/// reads are produced before it, wherever such an order exists. Value
/// dependencies are execution reads too, so the issued order stays a valid
/// one; blocks keep their issued order unless a read moves them, and a read
/// that closes a cycle keeps the issued order and its refusal.
///
/// A plan with a ranged declaration run is left as issued.
pub fn order_algebraic_projection_by_execution_reads(
    problem: &mut SolveProblem,
) -> Result<(), EvalSolveError> {
    let plan = &problem.continuous.algebraic_projection_plan.blocks;
    let Some(blocks) = plan
        .stages()
        .iter()
        .map(|stage| match stage {
            ProjectionStage::Explicit(block) => Some(block),
            ProjectionStage::SingletonDeclarationRun { .. } => None,
        })
        .collect::<Option<Vec<_>>>()
    else {
        return Ok(());
    };
    if blocks.len() < 2 {
        return Ok(());
    }
    let source = to_scalar_program_block(&problem.continuous.implicit_rhs)?;
    let reads = execution_row_reads(&source, None, source.program_span(0))?;
    let Some(order) = execution_order(&blocks, &reads) else {
        return Ok(());
    };
    let shared_targets = problem
        .continuous
        .implicit_row_targets
        .shares_projection_owner(plan);
    let reordered = order
        .into_iter()
        .map(|block| blocks[block].clone())
        .collect::<Vec<_>>()
        .into();
    if shared_targets {
        problem.continuous.implicit_row_targets = ImplicitRowTargets::from_projection(&reordered);
    }
    problem.continuous.algebraic_projection_plan.blocks = reordered;
    Ok(())
}

/// The positions of `blocks` in execution order, or `None` when the issued
/// order already is one or cannot be improved.
fn execution_order(
    blocks: &[&AlgebraicProjectionBlock],
    reads: &[Option<std::sync::Arc<rumoca_ir_solve::IndexIntervals>>],
) -> Option<Vec<usize>> {
    let producers = blocks
        .iter()
        .enumerate()
        .flat_map(|(block, plan)| plan.y_indices.iter().map(move |&target| (target, block)))
        .collect::<BTreeMap<_, _>>();
    // `needs[block]`: the blocks producing a target its rows' execution reads.
    let needs = blocks
        .iter()
        .enumerate()
        .map(|(block, plan)| {
            let mut needed = BTreeSet::new();
            for reads in plan.rows.iter().filter_map(|&row| reads.get(row)?.as_ref()) {
                for interval in reads.intervals() {
                    needed.extend(producers.range(interval).map(|(_, &producer)| producer));
                }
            }
            needed.remove(&block);
            needed
        })
        .collect::<Vec<_>>();
    if needs
        .iter()
        .enumerate()
        .all(|(block, needed)| needed.iter().all(|&producer| producer < block))
    {
        return None;
    }
    let mut placed = vec![false; blocks.len()];
    let mut order = Vec::with_capacity(blocks.len());
    for block in 0..blocks.len() {
        // Hoist the producers this block still waits for, each as soon as its
        // own reads are produced. What is left waiting is an execution cycle:
        // the block then keeps its issued place, which every dependency on an
        // earlier block already allows.
        while !placed[block] {
            let next = ready_producer(block, &needs, &placed).unwrap_or(block);
            placed[next] = true;
            order.push(next);
        }
    }
    order
        .iter()
        .enumerate()
        .any(|(position, &block)| position != block)
        .then_some(order)
}

/// The first block in issued order, among `block` and the unplaced producers
/// it waits for directly or through them, whose own producers are all placed.
fn ready_producer(block: usize, needs: &[BTreeSet<usize>], placed: &[bool]) -> Option<usize> {
    let mut reached = BTreeSet::from([block]);
    let mut pending = vec![block];
    let mut ready = None::<usize>;
    while let Some(waiting) = pending.pop() {
        let mut unmet = needs[waiting].iter().filter(|&&producer| !placed[producer]);
        if unmet.clone().next().is_none() {
            ready = Some(ready.map_or(waiting, |ready| ready.min(waiting)));
            continue;
        }
        pending.extend(unmet.by_ref().filter(|&&producer| reached.insert(producer)));
    }
    ready
}
