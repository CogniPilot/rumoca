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
}

thread_local! {
    static COUNTS: Cell<BlockResidualSplitCounts> =
        const { Cell::new(BlockResidualSplitCounts {
            calls: 0,
            invariant_evaluations: 0,
            dependent_evaluations: 0,
            fallbacks: 0,
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
) -> Box<[Option<BlockSplits>]> {
    plan.blocks
        .iter()
        .zip(structures.algebraic_projection())
        .enumerate()
        .map(|(index, (block, structure))| {
            if block.rows.len() < 2 || !owners.algebraic_projection_block_is_affine(index) {
                return None;
            }
            let splits = block_splits(block, structure, implicit)?;
            let mut offsets = vec![0];
            for (_, split) in &splits {
                offsets.push(offsets[offsets.len() - 1] + split.split().live_out().len());
            }
            let compiled = backend
                .and_then(|backend| compile_block_splits(&splits, &offsets, implicit, backend));
            let values = RefCell::new(vec![0.0; offsets[offsets.len() - 1]]);
            let ordinals = splits
                .iter()
                .enumerate()
                .map(|(ordinal, (program, _))| (*program, ordinal))
                .collect();
            Some(BlockSplits {
                ordinals,
                splits: splits.into_iter().map(|(_, split)| split).collect(),
                compiled,
                offsets,
                values,
                scratch: RefCell::new(Vec::new()),
            })
        })
        .collect()
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
