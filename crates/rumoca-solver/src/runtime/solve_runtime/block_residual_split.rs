//! Block residual splits of the linked kernel (SPEC_0043 §6a): each
//! projection block's residual programs divided into an invariant part,
//! evaluated once per block projection call, and a dependent part evaluated
//! per residual pass over the invariant part's values.

use std::cell::{Cell, RefCell};

use rumoca_eval_solve::{PreparedBlockResidualSplit, PreparedScalarProgramBlock};
use rumoca_ir_solve::{self as solve, BlockResidualSplit};
use rustc_hash::FxHashMap;

use super::SolveRuntime;
use crate::RuntimeSolveError;

/// The prepared splits of one projection block, keyed by residual program.
pub(crate) struct BlockSplits {
    programs: FxHashMap<usize, PreparedBlockResidualSplit>,
}

/// The invariant values of the block projection call in progress.
#[derive(Clone)]
pub(crate) struct ActiveBlockSplit {
    block: usize,
    values: FxHashMap<usize, Vec<f64>>,
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

/// The splits of the residual programs of every projection block with more
/// than one row, aligned with `plan.blocks`: the programs of its residual
/// output selection, or of its rows when it has none. A one-row block settles
/// by its singleton assignment first, and a program the interpreter evaluates
/// lazily keeps its unsplit evaluation.
pub(super) fn block_residual_splits(
    plan: &solve::AlgebraicProjectionPlan,
    structures: &solve::ContinuousStructuralArtifacts,
    implicit: &PreparedScalarProgramBlock,
) -> Box<[Option<BlockSplits>]> {
    plan.blocks
        .iter()
        .zip(structures.algebraic_projection())
        .map(|(block, structure)| {
            if block.rows.len() < 2 {
                return None;
            }
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
            let mut programs = FxHashMap::default();
            for index in sources {
                if programs.contains_key(&index) || implicit.has_lazy_row_plan(index) {
                    continue;
                }
                let ops = implicit.block().programs().get(index)?;
                let Some(split) = BlockResidualSplit::derive(ops, &block.y_indices) else {
                    continue;
                };
                split.check(ops, &block.y_indices).ok()?;
                let outputs = solve::ScalarProgramBlock::program_output_count(ops);
                let span = implicit.block().program_span(index);
                programs.insert(index, PreparedBlockResidualSplit::new(split, outputs, span));
            }
            (!programs.is_empty()).then_some(BlockSplits { programs })
        })
        .collect()
}

impl SolveRuntime {
    /// Evaluate the invariant parts of plan block `block` at the call's
    /// incoming point. A failure reports nothing and leaves no values, so the
    /// call evaluates the unsplit programs and raises their error.
    pub(super) fn begin_block_residual_split(&self, block: usize, y: &[f64], p: &[f64], t: f64) {
        *self.active_split.borrow_mut() = None;
        if self.compiled_implicit_rhs.is_some()
            || !rumoca_eval_solve::projection_policy::block_residual_split()
        {
            return;
        }
        let Some(Some(splits)) = self.block_splits.get(block) else {
            return;
        };
        let mut values = FxHashMap::default();
        for (&program, split) in &splits.programs {
            let mut program_values = Vec::new();
            if split
                .eval_invariant((y, p, t), self.row_eval_context(), &mut program_values)
                .is_err()
            {
                count(|counts| counts.fallbacks += 1);
                return;
            }
            values.insert(program, program_values);
        }
        count(|counts| {
            counts.calls += 1;
            counts.invariant_evaluations += splits.programs.len() as u64;
        });
        *self.active_split.borrow_mut() = Some(ActiveBlockSplit { block, values });
    }

    /// End the block projection call: its invariant values are discarded.
    pub(super) fn end_block_residual_split(&self) {
        *self.active_split.borrow_mut() = None;
    }

    /// Evaluate residual program `program` through the active split, or
    /// `None` when the call in progress has no split of it.
    pub(super) fn eval_split_residual_program(
        &self,
        program: usize,
        (y, p, t): (&[f64], &[f64], f64),
        out: &mut Vec<f64>,
    ) -> Option<Result<(), RuntimeSolveError>> {
        let active = self.active_split.borrow();
        let active = active.as_ref()?;
        let values = active.values.get(&program)?;
        let split = self
            .block_splits
            .get(active.block)?
            .as_ref()?
            .programs
            .get(&program)?;
        count(|counts| counts.dependent_evaluations += 1);
        Some(
            split
                .eval_dependent(values, (y, p, t), self.row_eval_context(), out)
                .map_err(Into::into),
        )
    }
}

/// The active split slot of a runtime.
pub(super) type ActiveSplitSlot = RefCell<Option<ActiveBlockSplit>>;

impl SolveRuntime {
    /// Evaluate the single output of residual program `program` through the
    /// active split, or `None` when the call in progress has no split of it
    /// or the program stores more than one output (a single-row evaluation
    /// of a multi-output program keeps its own path).
    pub(super) fn eval_split_residual_row(
        &self,
        program: usize,
        (y, p, t): (&[f64], &[f64], f64),
    ) -> Option<Result<f64, RuntimeSolveError>> {
        if self.implicit_scalar_rhs.row_output_count(program) != Some(1) {
            return None;
        }
        let mut out = self.split_row_scratch.borrow_mut();
        Some(
            self.eval_split_residual_program(program, (y, p, t), &mut out)?
                .map(|()| out[0]),
        )
    }
}
