//! Execution view of the MLS §8.6 initialization projection.
//!
//! The runtime settles initialization through
//! `SolveRuntime::settle_initialization_system`: the parameter bindings and
//! the initialization projection plan alternate until the bindings stop
//! changing, and every residual row is evaluated on the settled coordinates
//! (bindings plus the algebraic refresh applied to a copy). The generated
//! component runs the same loop through the shared kernel; this module reads
//! the checked initialization owner once and records its rows, blocks,
//! combined unknowns, and Jacobian patterns for the C initialization kernel.
//! Admission (`rumoca_ir_solve::fmi` C profile) has already refused every
//! initialization shape this view does not describe.

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use minijinja::Value;
use rumoca_eval_solve::to_scalar_program_projection;
use rumoca_ir_solve as solve;
use serde::Serialize;

use super::super::scalar_program_plan::ScalarProgramPlan;
use super::block::check_seed_loads;
use super::table::ProgramTable;
use crate::errors::CodegenError;

/// One initialization block: pool offsets of its residual rows, combined
/// unknowns (a solver-Y index, or the solver length plus a parameter index),
/// per-row fallback targets (a solver-Y index plus one, zero for none),
/// per-row relaxation columns (the block column of the row's own target, `n`
/// for none), and its compressed-row Jacobian pattern when one is issued.
#[derive(Serialize, Default)]
struct InitBlockRecord {
    n: usize,
    rows: usize,
    unknowns: usize,
    fallback: usize,
    relax: usize,
    row_ptr: usize,
    col_idx: usize,
    pattern: bool,
}

fn refuse(reason: &str) -> CodegenError {
    CodegenError::dae_preparation_failed(
        format!("unsupported-feature:initialization: {reason}"),
        None,
    )
}

/// The initialization kernel's view, or `rows = 0` when the initialization
/// system is parameter bindings alone.
pub(super) fn initialization_value(
    problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
    tangent: (&mut ProgramTable, &[(usize, usize)], usize),
) -> Result<(Value, usize), CodegenError> {
    let init = &problem.initialization;
    let rows = init
        .residual()
        .len()
        .map_err(|error| refuse(&error.to_string()))?;
    if rows == 0 {
        return Ok((minijinja::context! { rows => 0 }, 0));
    }
    let y_len = problem.solve_layout.solver_scalar_count();
    if y_len != problem.layout.y_scalars() {
        return Err(refuse("the solver coordinates do not fill the Y storage"));
    }
    let projection = to_scalar_program_projection(init.residual())?.into_block();
    let residual = Value::from_object(ScalarProgramPlan::new(Arc::new(projection))?);
    let combined = |slot: solve::ScalarSlot| match slot {
        solve::ScalarSlot::Y { index, .. } if index < y_len => Ok(index),
        solve::ScalarSlot::P { index, .. } if index < problem.layout.p_scalars() => {
            Ok(y_len + index)
        }
        _ => Err(refuse(
            "an initialization unknown is outside the solver and parameter storage",
        )),
    };
    let row_target = |row: usize| match init.row_targets().get(row).copied().flatten() {
        Some(solve::ScalarSlot::Y { index, .. }) => index + 1,
        _ => 0,
    };
    let structures = artifacts.initialization.structural.projection();
    let mut pool = Vec::new();
    let mut blocks = Vec::with_capacity(init.projection_plan().blocks.len());
    let (mut max_n, mut unknown_count) = (1, 0);
    for (index, block) in init.projection_plan().blocks.iter().enumerate() {
        let n = block.rows.len();
        if n != block.unknowns.len() || block.rows.iter().any(|&row| row >= rows) {
            return Err(refuse(
                "an initialization block is not square over its residual",
            ));
        }
        let unknowns = block
            .unknowns
            .iter()
            .map(|&slot| combined(slot))
            .collect::<Result<Vec<_>, _>>()?;
        let relax = block.rows.iter().map(|&row| {
            let target = row_target(row);
            (target != 0)
                .then(|| unknowns.iter().position(|&unknown| unknown == target - 1))
                .flatten()
                .unwrap_or(n)
        });
        let mut record = InitBlockRecord {
            n,
            rows: push(&mut pool, block.rows.iter().copied()),
            relax: 0,
            ..InitBlockRecord::default()
        };
        record.relax = push(&mut pool, relax.collect::<Vec<_>>());
        record.fallback = push(&mut pool, block.rows.iter().map(|&row| row_target(row)));
        record.unknowns = push(&mut pool, unknowns);
        if let Some(pattern) = structures
            .get(index)
            .map(solve::JacobianStructure::pattern)
            .filter(|pattern| pattern.rows() as usize == n && pattern.columns() as usize == n)
        {
            let (mut row_ptr, mut col_idx) = (vec![0], Vec::new());
            for row in 0..n {
                pattern.visit_row_columns(row, |column| col_idx.push(column));
                row_ptr.push(col_idx.len());
            }
            record.pattern = true;
            record.row_ptr = push(&mut pool, row_ptr);
            record.col_idx = push(&mut pool, col_idx);
        }
        max_n = max_n.max(n);
        unknown_count += n;
        blocks.push(record);
    }
    let (tangent, tangent_doubles) = tangent_value(problem, artifacts, tangent, &mut pool)?;
    // The deepest initialization frame chain: the plan's parameter scales and
    // residual rows, one block's scaled Newton system with its dense factor
    // and SVD factors, the settled residual's saved coordinates, and slack.
    let doubles =
        y_len + 2 * problem.layout.p_scalars() + 5 * rows + 5 * max_n * max_n + 12 * max_n + 64;
    let doubles = doubles + tangent_doubles;
    let pool = if pool.is_empty() { vec![0] } else { pool };
    Ok((
        minijinja::context! {
            rows => rows,
            residual => residual,
            y_len => y_len,
            blocks => Value::from_serialize(&blocks),
            pool => pool,
            unknown_count => unknown_count,
            row_targets => (0..rows).map(row_target).collect::<Vec<_>>(),
            sizes => 2 * max_n + 8,
            tangent => tangent,
        },
        doubles,
    ))
}

fn push(pool: &mut Vec<usize>, values: impl IntoIterator<Item = usize>) -> usize {
    let start = pool.len();
    pool.extend(values);
    start
}

/// Output position of every stored output of `block`: output index ->
/// (program, offset).
fn output_positions(block: &solve::ScalarProgramBlock) -> BTreeMap<usize, (usize, usize)> {
    let mut positions = BTreeMap::new();
    let mut ordinal = 0;
    for (program, ops) in block.programs().iter().enumerate() {
        for offset in 0..solve::ScalarProgramBlock::program_output_count(ops) {
            if let Some(&output) = block.output_indices().get(ordinal) {
                positions.insert(output, (program, offset));
            }
            ordinal += 1;
        }
    }
    positions
}

/// The JVP functions of one directional-derivative block over
/// `[solver-y | parameter]` seeds, interned into the component table.
struct JvpRows<'a> {
    block: &'a solve::ScalarProgramBlock,
    source: usize,
    positions: BTreeMap<usize, (usize, usize)>,
    seed_len: usize,
}

impl<'a> JvpRows<'a> {
    fn new(
        table: &mut ProgramTable,
        block: &'a solve::ScalarProgramBlock,
        seed_len: usize,
    ) -> Self {
        Self {
            source: table.jvp_source(block),
            positions: output_positions(block),
            block,
            seed_len,
        }
    }

    /// `(function, output offset)` of output `output`, and the function's
    /// output count.
    fn entry(
        &self,
        table: &mut ProgramTable,
        output: usize,
        what: &str,
    ) -> Result<([usize; 2], usize), CodegenError> {
        let &(program, offset) = self
            .positions
            .get(&output)
            .ok_or_else(|| refuse(&format!("{what} has no directional derivative")))?;
        let block = self.block;
        let seed_len = self.seed_len;
        let function = table.jvp.intern((self.source, program), || {
            let operations = block.programs()[program].clone();
            check_seed_loads(0, &operations, seed_len, 1)?;
            let span = block
                .program_span(program)
                .ok_or_else(|| refuse(&format!("{what} has no source provenance")))?;
            Ok((operations, span))
        })?;
        Ok(([function, offset], table.jvp.output_count(function)))
    }
}

/// The settled initialization tangent: the primary chart's complete algebraic
/// plan (`seed_blocks`, interned block ids in plan order) with each block
/// row's JVP over `[solver-y | parameter]` seeds, the update rows' JVP with
/// their combined targets (plus one, zero for none), and the residual rows'
/// JVP, all as pool offsets.
fn tangent_value(
    problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
    (table, seed_blocks, seed_len): (&mut ProgramTable, &[(usize, usize)], usize),
    pool: &mut Vec<usize>,
) -> Result<(Value, usize), CodegenError> {
    let init = &problem.initialization;
    let mut max_outputs = 1;
    let full = JvpRows::new(
        table,
        &artifacts.continuous.implicit_jacobian_v_scalar,
        seed_len,
    );
    let residual_block =
        rumoca_eval_solve::to_scalar_program_block(&artifacts.initialization.residual_jacobian_v)?;
    let cone = tangent_cone(problem, artifacts, seed_blocks, &residual_block)?;
    let seed_blocks = seed_blocks
        .iter()
        .zip(&cone.blocks)
        .filter_map(|(block, kept)| kept.then_some(*block))
        .collect::<Vec<_>>();
    let seed_blocks = seed_blocks.as_slice();
    let mut rows = Vec::new();
    let mut nrows = 0;
    for &(_, canonical) in seed_blocks {
        let block = problem
            .continuous
            .algebraic_projection_plan
            .blocks
            .get(canonical)
            .ok_or_else(|| refuse("the algebraic plan names a block outside the projection"))?;
        for &row in &block.rows {
            rows.extend(jvp_entry(
                table,
                &full,
                row,
                "an algebraic row",
                &mut max_outputs,
            )?);
            nrows += 1;
        }
    }
    let (updates, nupdates) = tangent_updates(
        problem,
        artifacts,
        (table, &cone.updates, seed_len),
        &mut max_outputs,
    )?;
    let residual_rows = JvpRows::new(table, &residual_block, seed_len);
    let count = init
        .residual()
        .len()
        .map_err(|error| refuse(&error.to_string()))?;
    let mut residual = Vec::with_capacity(2 * count);
    for row in 0..count {
        residual.extend(jvp_entry(
            table,
            &residual_rows,
            row,
            "an initialization residual row",
            &mut max_outputs,
        )?);
    }
    let max_n = seed_blocks
        .iter()
        .filter_map(|&(_, canonical)| {
            problem
                .continuous
                .algebraic_projection_plan
                .blocks
                .get(canonical)
        })
        .map(|block| block.rows.len())
        .max()
        .unwrap_or(1);
    // The tangent frame: its seed and unit direction, update values, all plan
    // rows' residual and scales, one block's right-hand side, column,
    // Jacobian, dense matrix and factor, and one JVP output buffer.
    let doubles = 2 * seed_len
        + nupdates
        + 3 * nrows
        + 5 * max_n
        + 3 * max_n * max_n
        + max_outputs
        + count
        + 64;
    Ok((
        minijinja::context! {
            nblocks => seed_blocks.len(),
            blocks => push(pool, seed_blocks.iter().map(|&(id, _)| id)),
            rows => push(pool, rows),
            nrows => nrows,
            nupdates => nupdates,
            updates => push(pool, updates),
            residual => push(pool, residual),
            max_outputs => max_outputs,
            max_n => max_n,
        },
        doubles,
    ))
}

/// The seed indices (solver-Y, then parameters after `y_len`) each output of
/// `block` depends on, by output index.
/// A directional derivative whose seed reads cannot be derived is refused.
fn output_seed_reads(
    block: &solve::ScalarProgramBlock,
) -> Result<BTreeMap<usize, BTreeSet<usize>>, CodegenError> {
    output_positions(block)
        .into_iter()
        .map(|(output, (program, offset))| {
            solve::StructuralPattern::derive_output_seed_index_dependencies(
                &block.programs()[program],
                None,
            )
            .ok()
            .and_then(|outputs| outputs.get(offset).cloned())
            .map(|reads| (output, reads))
            .ok_or_else(|| refuse("a directional derivative's seed reads are not derivable"))
        })
        .collect()
}

/// The part of the settled initialization tangent that can reach the
/// initialization residual: the plan blocks and update rows both reachable
/// from an initialization unknown and read, through the plan and the
/// bindings, by a residual row. Every other block's tangent is exactly zero or
/// unread, so its rows need no directional derivative.
struct TangentCone {
    blocks: Vec<bool>,
    updates: Vec<bool>,
}

fn meets(set: &BTreeSet<usize>, values: impl IntoIterator<Item = usize>) -> bool {
    values.into_iter().any(|value| set.contains(&value))
}

/// One pass of the forward closure from the initialization unknowns through the
/// bindings and the plan; whether it grew.
fn forward_pass(inputs: &ConeInputs<'_>, forward: &mut BTreeSet<usize>) -> bool {
    let before = forward.len();
    for (target, reads) in &inputs.updates {
        let reached = meets(forward, reads.iter().copied());
        forward.extend(target.filter(|_| reached));
    }
    for (unknowns, reads) in &inputs.blocks {
        let reached = meets(forward, reads.iter().copied())
            || meets(&inputs.unknowns, unknowns.iter().copied());
        forward.extend(unknowns.iter().copied().filter(|_| reached));
    }
    forward.len() != before
}

/// One pass of the backward closure from the residual rows' reads; whether it
/// grew.
fn backward_pass(inputs: &ConeInputs<'_>, backward: &mut BTreeSet<usize>) -> bool {
    let before = backward.len();
    for (unknowns, reads) in inputs.blocks.iter().rev() {
        let read = meets(backward, unknowns.iter().copied());
        backward.extend(reads.iter().copied().filter(|_| read));
    }
    for (target, reads) in &inputs.updates {
        let read = target.is_some_and(|target| backward.contains(&target));
        backward.extend(reads.iter().copied().filter(|_| read));
    }
    backward.len() != before
}

struct ConeInputs<'a> {
    unknowns: BTreeSet<usize>,
    residual_reads: BTreeSet<usize>,
    blocks: Vec<(&'a [usize], BTreeSet<usize>)>,
    updates: Vec<(Option<usize>, BTreeSet<usize>)>,
}

impl TangentCone {
    fn derive(inputs: &ConeInputs<'_>) -> Self {
        let mut forward = inputs.unknowns.clone();
        while forward_pass(inputs, &mut forward) {}
        let mut backward = inputs.residual_reads.clone();
        while backward_pass(inputs, &mut backward) {}
        Self {
            blocks: inputs
                .blocks
                .iter()
                .map(|(unknowns, _)| {
                    meets(&forward, unknowns.iter().copied())
                        && meets(&backward, unknowns.iter().copied())
                })
                .collect(),
            updates: inputs
                .updates
                .iter()
                .map(|(target, reads)| {
                    target.is_some_and(|target| backward.contains(&target))
                        && meets(&forward, reads.iter().copied())
                })
                .collect(),
        }
    }
}

/// The cone of the settled initialization tangent over the primary chart's
/// complete algebraic plan (`seed_blocks`).
fn tangent_cone(
    problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
    seed_blocks: &[(usize, usize)],
    residual_block: &solve::ScalarProgramBlock,
) -> Result<TangentCone, CodegenError> {
    let init = &problem.initialization;
    let y_len = problem.solve_layout.solver_scalar_count();
    let combined = |slot: solve::ScalarSlot| match slot {
        solve::ScalarSlot::Y { index, .. } => Some(index),
        solve::ScalarSlot::P { index, .. } => Some(y_len + index),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => None,
    };
    let row_reads = output_seed_reads(&artifacts.continuous.implicit_jacobian_v_scalar)?;
    let plan = &problem.continuous.algebraic_projection_plan;
    let blocks = seed_blocks
        .iter()
        .map(|&(_, canonical)| {
            let block = plan
                .blocks
                .get(canonical)
                .ok_or_else(|| refuse("the algebraic plan names a block outside the projection"))?;
            let reads = block
                .rows
                .iter()
                .filter_map(|row| row_reads.get(row))
                .flatten()
                .copied()
                .collect();
            Ok((block.y_indices.as_slice(), reads))
        })
        .collect::<Result<Vec<_>, CodegenError>>()?;
    let targets = init.update_targets();
    let update_reads = match &artifacts.initialization.update_jacobian_v {
        Some(block) => output_seed_reads(block)?,
        None if targets.is_empty() => BTreeMap::new(),
        None => {
            return Err(refuse(
                "an initialization update row has no directional derivative",
            ));
        }
    };
    let updates = targets
        .iter()
        .enumerate()
        .map(|(row, target)| {
            let reads = update_reads.get(&row).cloned().ok_or_else(|| {
                refuse("an initialization update row has no directional derivative")
            })?;
            Ok((combined(*target), reads))
        })
        .collect::<Result<_, CodegenError>>()?;
    let residual_reads = output_seed_reads(residual_block)?
        .into_values()
        .flatten()
        .collect();
    let unknowns = init
        .projection_plan()
        .blocks
        .iter()
        .flat_map(|block| block.unknowns.iter().copied())
        .filter_map(combined)
        .collect();
    Ok(TangentCone::derive(&ConeInputs {
        unknowns,
        residual_reads,
        blocks,
        updates,
    }))
}

/// One JVP output's `(function, offset)` pool entry; `max_outputs` keeps the
/// widest function's output count.
fn jvp_entry(
    table: &mut ProgramTable,
    rows: &JvpRows<'_>,
    output: usize,
    what: &str,
    max_outputs: &mut usize,
) -> Result<[usize; 2], CodegenError> {
    let (entry, outputs) = rows.entry(table, output, what)?;
    *max_outputs = (*max_outputs).max(outputs);
    Ok(entry)
}

/// The kept update rows' `(function, offset, combined target + 1)` entries
/// and their count.
fn tangent_updates(
    problem: &solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
    (table, kept, seed_len): (&mut ProgramTable, &[bool], usize),
    max_outputs: &mut usize,
) -> Result<(Vec<usize>, usize), CodegenError> {
    let y_len = problem.solve_layout.solver_scalar_count();
    let targets = problem.initialization.update_targets();
    let mut updates = Vec::new();
    if !kept.contains(&true) {
        return Ok((updates, 0));
    }
    let block = artifacts
        .initialization
        .update_jacobian_v
        .as_ref()
        .ok_or_else(|| refuse("an initialization update row has no directional derivative"))?;
    let update = JvpRows::new(table, block, seed_len);
    let mut count = 0;
    for (row, target) in targets.iter().enumerate() {
        if !kept[row] {
            continue;
        }
        count += 1;
        updates.extend(jvp_entry(
            table,
            &update,
            row,
            "an initialization update row",
            max_outputs,
        )?);
        updates.push(match *target {
            solve::ScalarSlot::Y { index, .. } => index + 1,
            solve::ScalarSlot::P { index, .. } => y_len + index + 1,
            solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => 0,
        });
    }
    Ok((updates, count))
}
