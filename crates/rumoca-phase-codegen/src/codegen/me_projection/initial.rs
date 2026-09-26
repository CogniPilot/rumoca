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

use std::collections::BTreeMap;
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
    let y_len = problem.solve_layout.solver_scalar_count();
    let mut max_outputs = 1;
    let mut entry = |table: &mut ProgramTable, rows: &JvpRows<'_>, output: usize, what: &str| {
        let (entry, outputs) = rows.entry(table, output, what)?;
        max_outputs = max_outputs.max(outputs);
        Ok::<_, CodegenError>(entry)
    };
    let full = JvpRows::new(
        table,
        &artifacts.continuous.implicit_jacobian_v_scalar,
        seed_len,
    );
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
            rows.extend(entry(table, &full, row, "an algebraic row")?);
            nrows += 1;
        }
    }
    let targets = init.update_targets();
    let mut updates = Vec::with_capacity(3 * targets.len());
    if !targets.is_empty() {
        let block = artifacts
            .initialization
            .update_jacobian_v
            .as_ref()
            .ok_or_else(|| refuse("an initialization update row has no directional derivative"))?;
        let update = JvpRows::new(table, block, seed_len);
        for (row, target) in targets.iter().enumerate() {
            updates.extend(entry(table, &update, row, "an initialization update row")?);
            updates.push(match *target {
                solve::ScalarSlot::Y { index, .. } => index + 1,
                solve::ScalarSlot::P { index, .. } => y_len + index + 1,
                solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => 0,
            });
        }
    }
    let residual_block =
        rumoca_eval_solve::to_scalar_program_block(&artifacts.initialization.residual_jacobian_v)?;
    let residual_rows = JvpRows::new(table, &residual_block, seed_len);
    let count = init
        .residual()
        .len()
        .map_err(|error| refuse(&error.to_string()))?;
    let mut residual = Vec::with_capacity(2 * count);
    for row in 0..count {
        residual.extend(entry(
            table,
            &residual_rows,
            row,
            "an initialization residual row",
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
    // The tangent frame: its seed, update values, all plan rows' residual and
    // scales, one block's right-hand side, Jacobian, dense matrix and factor,
    // and one JVP output buffer.
    let doubles = seed_len
        + targets.len()
        + 3 * nrows
        + 3 * max_n
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
            nupdates => targets.len(),
            updates => push(pool, updates),
            residual => push(pool, residual),
            max_outputs => max_outputs,
            max_n => max_n,
        },
        doubles,
    ))
}
