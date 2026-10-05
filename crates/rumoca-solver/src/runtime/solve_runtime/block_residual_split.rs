//! Block residual splits of the linked kernel (SPEC_0043 §6a): each
//! projection block's residual programs divided into an invariant part,
//! evaluated once per block projection call, and a dependent part evaluated
//! per residual pass over the invariant part's values. With a compiled
//! residual path the parts run as compiled programs: the invariant program
//! stores its live-out values and the dependent program reads them as its
//! seed vector.

use std::cell::{Cell, RefCell};
use std::rc::Rc;

use rumoca_eval_solve::{
    JacobianEvalInputs, PreparedBlockResidualSplit, PreparedScalarProgramBlock,
};
use rumoca_ir_solve::{self as solve, BlockResidualSplit};
use rustc_hash::FxHashMap;

use super::{
    CompiledSolveExpression, CompiledSolveJacobianExpression, SolveExecutionBackend, SolveRuntime,
};
use crate::RuntimeSolveError;

/// The prepared splits of one projection block, in residual program order.
pub(crate) struct BlockSplits {
    /// Residual program index to split ordinal.
    ordinals: FxHashMap<usize, usize>,
    splits: Vec<PreparedBlockResidualSplit>,
    compiled: Option<CompiledBlockSplits>,
    /// Start of each split's live-out values in `values`, and their end.
    offsets: Vec<usize>,
    /// Live-out values of the call in progress, split after split: the seed
    /// vector of every compiled dependent program of the block.
    values: RefCell<Vec<f64>>,
    /// One split's invariant outputs before they are placed in `values`.
    scratch: RefCell<Vec<f64>>,
}

/// The block's split programs compiled by the execution backend, program
/// `i` of each block being split ordinal `i`.
struct CompiledBlockSplits {
    invariant: Rc<dyn CompiledSolveExpression>,
    dependent: Rc<dyn CompiledSolveJacobianExpression>,
}

/// The block projection call in progress with evaluated invariant values.
#[derive(Clone, Copy)]
pub(crate) struct ActiveBlockSplit {
    block: usize,
    compiled: bool,
}

/// Counts of the linked kernel's block residual split on this thread.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct BlockResidualSplitCounts {
    /// Block projection calls that evaluated their invariant parts.
    pub calls: u64,
    /// Invariant-part evaluations (one per split program per call).
    pub invariant_evaluations: u64,
    /// Dependent-part evaluations (one per split program per residual pass).
    pub dependent_evaluations: u64,
    /// Calls whose invariant parts failed and fell back to the unsplit programs.
    pub fallbacks: u64,
    /// Residual rows evaluated by a batched compiled residual evaluation,
    /// which calls each compiled entry once for all its rows.
    pub batched_rows: u64,
}

thread_local! {
    static COUNTS: Cell<BlockResidualSplitCounts> =
        const { Cell::new(BlockResidualSplitCounts {
            calls: 0,
            invariant_evaluations: 0,
            dependent_evaluations: 0,
            fallbacks: 0,
            batched_rows: 0,
        }) };
}

/// The block residual split counts on this thread since the last
/// [`reset_block_residual_split_counts`].
#[must_use]
pub fn block_residual_split_counts() -> BlockResidualSplitCounts {
    COUNTS.with(Cell::get)
}

pub fn reset_block_residual_split_counts() {
    COUNTS.with(|counts| counts.set(BlockResidualSplitCounts::default()));
}

fn count(update: impl FnOnce(&mut BlockResidualSplitCounts)) {
    COUNTS.with(|counts| {
        let mut value = counts.get();
        update(&mut value);
        counts.set(value);
    });
}

/// The splits of the residual programs of every affine projection block with
/// more than one row, aligned with `plan.blocks` (the affine residual passes
/// evaluate residual programs; a torn sweep evaluates its own): the programs of its residual
/// output selection, or of its rows when it has none. A one-row block settles
/// by its singleton assignment first, and a program the interpreter evaluates
/// lazily keeps its unsplit evaluation. With an execution `backend`, each
/// block's parts are also compiled; a block whose compilation declines keeps
/// the unsplit compiled programs.
pub(super) fn block_residual_splits(
    owners: &solve::ContinuousRefreshOwners,
    plan: &solve::AlgebraicProjectionPlan,
    structures: &solve::ContinuousStructuralArtifacts,
    implicit: &PreparedScalarProgramBlock,
    backend: Option<&dyn SolveExecutionBackend>,
    shared: Option<SharedSplits<'_>>,
) -> Box<[Option<Rc<BlockSplits>>]> {
    plan.blocks
        .iter()
        .zip(structures.algebraic_projection())
        .enumerate()
        .map(|(index, (block, structure))| {
            if block.rows.len() < 2 || !owners.algebraic_projection_block_is_affine(index) {
                return None;
            }
            if let Some(splits) = shared.as_ref().and_then(|shared| shared.splits_of(block)) {
                return splits;
            }
            let splits = block_splits(block, structure, implicit)?;
            Some(Rc::new(prepared_block_splits(splits, implicit, backend)))
        })
        .collect()
}

/// The primary runtime's block splits an alternate chart reuses: a block with
/// the primary's rows, unknowns, and tearing whose residual programs none of
/// the alternate replaced evaluates exactly the primary's splits, program
/// indices included.
pub(super) struct SharedSplits<'a> {
    plan: &'a solve::AlgebraicProjectionPlan,
    splits: &'a [Option<Rc<BlockSplits>>],
    replaced_rows: &'a std::collections::BTreeSet<usize>,
    by_rows: std::collections::BTreeMap<&'a [usize], usize>,
}

impl<'a> SharedSplits<'a> {
    pub(super) fn new(
        plan: &'a solve::AlgebraicProjectionPlan,
        splits: &'a [Option<Rc<BlockSplits>>],
        replaced_rows: &'a std::collections::BTreeSet<usize>,
    ) -> Self {
        let by_rows = plan
            .blocks
            .iter()
            .enumerate()
            .map(|(index, block)| (block.rows.as_slice(), index))
            .collect();
        Self {
            plan,
            splits,
            replaced_rows,
            by_rows,
        }
    }

    /// The primary's splits of `block`, `None` when the block is not shared.
    fn splits_of(
        &self,
        block: &solve::AlgebraicProjectionBlock,
    ) -> Option<Option<Rc<BlockSplits>>> {
        if block
            .rows
            .iter()
            .any(|row| self.replaced_rows.contains(row))
        {
            return None;
        }
        let index = *self.by_rows.get(block.rows.as_slice())?;
        (self.plan.blocks[index] == *block).then(|| self.splits.get(index).cloned())?
    }
}

fn prepared_block_splits(
    splits: Vec<(usize, PreparedBlockResidualSplit)>,
    implicit: &PreparedScalarProgramBlock,
    backend: Option<&dyn SolveExecutionBackend>,
) -> BlockSplits {
    let mut offsets = vec![0];
    for (_, split) in &splits {
        offsets.push(offsets[offsets.len() - 1] + split.split().live_out().len());
    }
    let compiled =
        backend.and_then(|backend| compile_block_splits(&splits, &offsets, implicit, backend));
    let values = RefCell::new(vec![0.0; offsets[offsets.len() - 1]]);
    let ordinals = splits
        .iter()
        .enumerate()
        .map(|(ordinal, (program, _))| (*program, ordinal))
        .collect();
    BlockSplits {
        ordinals,
        splits: splits.into_iter().map(|(_, split)| split).collect(),
        compiled,
        offsets,
        values,
        scratch: RefCell::new(Vec::new()),
    }
}

fn block_splits(
    block: &solve::AlgebraicProjectionBlock,
    structure: &solve::JacobianStructure,
    implicit: &PreparedScalarProgramBlock,
) -> Option<Vec<(usize, PreparedBlockResidualSplit)>> {
    let sources: Vec<usize> = match structure.residual_output_evaluation() {
        Some(selection) => selection
            .programs()
            .iter()
            .map(|program| program.program())
            .collect(),
        None => block
            .rows
            .iter()
            .filter_map(|&row| Some(implicit.row_output_position(row)?.0))
            .collect(),
    };
    let mut splits: Vec<(usize, PreparedBlockResidualSplit)> = Vec::new();
    for index in sources {
        if splits.iter().any(|(program, _)| *program == index) || implicit.has_lazy_row_plan(index)
        {
            continue;
        }
        let ops = implicit.block().programs().get(index)?;
        let Some(split) = BlockResidualSplit::derive(ops, &block.y_indices) else {
            continue;
        };
        split.check(ops, &block.y_indices).ok()?;
        let outputs = solve::ScalarProgramBlock::program_output_count(ops);
        let span = implicit.block().program_span(index);
        splits.push((index, PreparedBlockResidualSplit::new(split, outputs, span)));
    }
    (!splits.is_empty()).then_some(splits)
}

fn compile_block_splits(
    splits: &[(usize, PreparedBlockResidualSplit)],
    offsets: &[usize],
    implicit: &PreparedScalarProgramBlock,
    backend: &dyn SolveExecutionBackend,
) -> Option<CompiledBlockSplits> {
    let spans = splits
        .iter()
        .map(|(program, _)| implicit.block().program_span(*program))
        .collect::<Option<Vec<_>>>()?;
    let block = |programs: Vec<Vec<solve::LinearOp>>| {
        solve::ScalarProgramBlock::with_program_spans(programs, spans.clone()).ok()
    };
    let invariant = block(
        splits
            .iter()
            .map(|(_, split)| split.split().invariant_program())
            .collect(),
    )?;
    let dependent = block(
        splits
            .iter()
            .zip(offsets)
            .map(|((_, split), &offset)| split.split().dependent_program(offset))
            .collect(),
    )?;
    let invariant = backend.compile_selectable_expression(&invariant).ok()?;
    let dependent = backend.compile_jacobian_expression(&dependent).ok()?;
    Some(CompiledBlockSplits {
        invariant,
        dependent,
    })
}

impl SolveRuntime {
    /// Evaluate the invariant parts of plan block `block` at the call's
    /// incoming point, compiled when the residual programs are. A failure
    /// reports nothing and leaves no values, so the call evaluates the unsplit
    /// programs and raises their error; a declining compiled call leaves the
    /// call unsplit.
    pub(super) fn begin_block_residual_split(&self, block: usize, y: &[f64], p: &[f64], t: f64) {
        self.active_split.set(None);
        if !rumoca_eval_solve::projection_policy::block_residual_split() {
            return;
        }
        let Some(Some(splits)) = self.block_splits.get(block) else {
            return;
        };
        let compiled = self.compiled_implicit_rhs.is_some();
        if compiled && splits.compiled.is_none() {
            return;
        }
        let mut values = splits.values.borrow_mut();
        let mut scratch = splits.scratch.borrow_mut();
        for (ordinal, split) in splits.splits.iter().enumerate() {
            let evaluated = match &splits.compiled {
                Some(programs) if compiled => programs
                    .invariant
                    .call_program_outputs(
                        ordinal,
                        y,
                        p,
                        t,
                        self.model.external_tables.as_slice(),
                        &mut scratch,
                    )
                    .unwrap_or(false),
                _ => split
                    .eval_invariant((y, p, t), self.row_eval_context(), &mut scratch)
                    .is_ok(),
            };
            let range = splits.offsets[ordinal]..splits.offsets[ordinal + 1];
            if !evaluated || scratch.len() != range.len() {
                count(|counts| counts.fallbacks += 1);
                return;
            }
            values[range].copy_from_slice(&scratch);
        }
        count(|counts| {
            counts.calls += 1;
            counts.invariant_evaluations += splits.splits.len() as u64;
        });
        self.active_split
            .set(Some(ActiveBlockSplit { block, compiled }));
    }

    /// End the block projection call: its invariant values are discarded.
    pub(super) fn end_block_residual_split(&self) {
        self.active_split.set(None);
    }

    /// Evaluate residual program `program` through the active split, or
    /// `None` when the call in progress has no split of it, or when the
    /// dependent part declines or fails: the unsplit program then evaluates
    /// the pass and raises its own error.
    pub(super) fn eval_split_residual_program(
        &self,
        program: usize,
        (y, p, t): (&[f64], &[f64], f64),
        out: &mut Vec<f64>,
    ) -> Option<()> {
        let active = self.active_split.get()?;
        let splits = self.block_splits.get(active.block)?.as_ref()?;
        let &ordinal = splits.ordinals.get(&program)?;
        let values = splits.values.borrow();
        let evaluated = match &splits.compiled {
            // The compiled dependent programs share the block's values as
            // their seed vector.
            Some(programs) if active.compiled => programs
                .dependent
                .call_program_outputs(
                    ordinal,
                    JacobianEvalInputs {
                        y,
                        p,
                        t,
                        seed: &values,
                    },
                    self.model.external_tables.as_slice(),
                    out,
                )
                .unwrap_or(false),
            _ => {
                let range = splits.offsets[ordinal]..splits.offsets[ordinal + 1];
                splits.splits[ordinal]
                    .eval_dependent(&values[range], (y, p, t), self.row_eval_context(), out)
                    .is_ok()
            }
        };
        evaluated.then(|| count(|counts| counts.dependent_evaluations += 1))
    }

    /// Evaluate the single output of residual program `program` through the
    /// active split, or `None` as [`Self::eval_split_residual_program`] or
    /// when the program stores more than one output (a single-row evaluation
    /// of a multi-output program keeps its own path).
    pub(super) fn eval_split_residual_row(
        &self,
        program: usize,
        (y, p, t): (&[f64], &[f64], f64),
    ) -> Option<f64> {
        if self.implicit_scalar_rhs.row_output_count(program) != Some(1) {
            return None;
        }
        let mut out = self.split_row_scratch.borrow_mut();
        self.eval_split_residual_program(program, (y, p, t), &mut out)?;
        out.first().copied()
    }
}

/// The active split slot of a runtime.
pub(super) type ActiveSplitSlot = Cell<Option<ActiveBlockSplit>>;

/// Residual rows of one batched evaluation that run the same compiled
/// entry: their `(program, output offset)` coordinates, their positions in
/// the batch, and the values of the call in progress.
#[derive(Clone, Default)]
struct BatchedRows {
    coordinates: Vec<(usize, usize)>,
    slots: Vec<usize>,
    values: Vec<f64>,
}

impl BatchedRows {
    fn clear(&mut self) {
        self.coordinates.clear();
        self.slots.clear();
    }

    fn push(&mut self, coordinate: (usize, usize), slot: usize) {
        self.coordinates.push(coordinate);
        self.slots.push(slot);
    }

    fn scatter(&self, out: &mut [f64]) {
        for (&slot, &value) in self.slots.iter().zip(&self.values) {
            out[slot] = value;
        }
    }
}

/// The entries the most recent batched residual evaluation assigned its
/// rows: a pure function of the rows and the active compiled split's block,
/// which every later evaluation of the same rows under the same split reuses.
#[derive(Clone, Default)]
pub(super) struct ResidualRowBatch {
    rows: Vec<usize>,
    split: Option<usize>,
    /// Whether every row has a scalar view; the per-row evaluation serves
    /// the rows otherwise.
    batched: bool,
    dependent: BatchedRows,
    direct: BatchedRows,
}

impl ResidualRowBatch {
    /// Assign `rows` to their entries under `split` (its block and splits)
    /// unless they are the rows and split already assigned.
    fn assign(
        &mut self,
        runtime: &SolveRuntime,
        rows: &[usize],
        split: Option<(usize, &BlockSplits)>,
    ) {
        let block = split.map(|(block, _)| block);
        if self.rows == rows && self.split == block {
            return;
        }
        self.rows.clear();
        self.rows.extend_from_slice(rows);
        self.split = block;
        self.dependent.clear();
        self.direct.clear();
        self.batched = rows.iter().enumerate().all(|(slot, &row)| {
            let Some((program, offset)) = runtime.implicit_scalar_rhs.row_output_position(row)
            else {
                return false;
            };
            let ordinal = split
                .filter(|_| runtime.implicit_scalar_rhs.row_output_count(program) == Some(1))
                .and_then(|(_, splits)| splits.ordinals.get(&program).copied());
            match ordinal {
                Some(ordinal) => self.dependent.push((ordinal, 0), slot),
                None => self.direct.push((program, offset), slot),
            }
            true
        });
        self.dependent
            .values
            .resize(self.dependent.coordinates.len(), 0.0);
        self.direct
            .values
            .resize(self.direct.coordinates.len(), 0.0);
    }
}

impl SolveRuntime {
    /// The compiled residuals of `rows`, in order, into `out`: exactly what
    /// the per-row evaluation yields, each row through the entry it would
    /// take there (the active compiled split's dependent program for a
    /// one-output program the split holds, else the compiled residual
    /// program), with each entry called once for all its rows. `false`, with
    /// `out` unspecified, leaves the rows to that per-row evaluation: a row
    /// without a scalar view, an interpreted active split, or a compiled
    /// entry that declines.
    pub(super) fn eval_compiled_residual_rows(
        &self,
        rows: &[usize],
        (y, p, t): (&[f64], &[f64], f64),
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let Some(compiled) = &self.compiled_implicit_rhs else {
            return Ok(false);
        };
        let split = match self.active_split.get() {
            None => None,
            Some(active) if active.compiled => {
                match self.block_splits.get(active.block).and_then(Option::as_ref) {
                    Some(splits) if splits.compiled.is_none() => return Ok(false),
                    splits => splits.map(|splits| (active.block, splits.as_ref())),
                }
            }
            Some(_) => return Ok(false),
        };
        let mut batch = self.residual_row_batch.borrow_mut();
        batch.assign(self, rows, split);
        if !batch.batched {
            return Ok(false);
        }
        let tables = self.model.external_tables.as_slice();
        let ResidualRowBatch {
            dependent, direct, ..
        } = &mut *batch;
        if let Some((_, splits)) = split
            && let Some(programs) = &splits.compiled
            && !dependent.coordinates.is_empty()
        {
            let seed = splits.values.borrow();
            let inputs = JacobianEvalInputs {
                y,
                p,
                t,
                seed: &seed,
            };
            // The per-row evaluation takes a failed dependent call as a
            // decline and evaluates the row unsplit; so does a declined batch.
            let called = programs.dependent.call_program_outputs_at(
                &dependent.coordinates,
                inputs,
                tables,
                &mut dependent.values,
            );
            if !matches!(called, Ok(true)) {
                return Ok(false);
            }
        }
        if !direct.coordinates.is_empty()
            && !compiled
                .call_program_outputs_at(&direct.coordinates, (y, p, t), tables, &mut direct.values)
                .map_err(RuntimeSolveError::solve_ir)?
        {
            return Ok(false);
        }
        dependent.scatter(out);
        direct.scatter(out);
        let evaluated = dependent.coordinates.len() as u64;
        count(|counts| {
            counts.dependent_evaluations += evaluated;
            counts.batched_rows += rows.len() as u64;
        });
        for (&row, &value) in rows.iter().zip(out.iter()) {
            self.report_nonfinite_implicit_residual_row_inputs(t, y, row, value);
        }
        Ok(true)
    }
}
