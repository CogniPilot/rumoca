//! Executable program form of one target isolation query.
//!
//! [`PreparedScalarProgramBlock::eval_target_assignment_output_unchecked_with_context`]
//! answers "what value does this residual output assign to that target" one
//! evaluation at a time. A code-generation backend that must reproduce the
//! same answer without this evaluator needs the answer in executable form,
//! decided by the same rule: a recognized assignment shape materializes its
//! isolator; an output that another shape owns, or that reads the target
//! without a shape, has no isolation; any other output is its own value.

use rumoca_ir_solve::{LinearOp, materialize_target_assignment};

use super::{PreparedScalarProgramBlock, row_output_depends_on_y_index};

/// Executable form of one unchecked target isolation.
#[derive(Clone, Debug, PartialEq)]
pub enum TargetIsolationProgram {
    /// The evaluator returns no isolated value for this output and target.
    Unavailable,
    /// The evaluator returns the output's own value, which does not read the
    /// target.
    OutputValue,
    /// A single-output isolator program. A singular or non-finite coefficient
    /// stores a non-finite value exactly where the evaluator reports a
    /// singular assignment.
    Isolator(Vec<LinearOp>),
    /// The evaluator answers, but no scalar program in this vocabulary
    /// reproduces its answer; a backend must refuse rather than guess.
    Unrepresentable,
}

impl PreparedScalarProgramBlock {
    /// Executable form of the unchecked isolation of one output.
    pub fn target_isolation_output_program(
        &self,
        row_idx: usize,
        output_offset: usize,
        target_y_index: usize,
    ) -> TargetIsolationProgram {
        let Some(row) = self.block.programs().get(row_idx) else {
            return TargetIsolationProgram::Unavailable;
        };
        if let Some(shape) =
            self.assignment_shape_for_output(row_idx, output_offset, target_y_index)
        {
            let Some(prefix) = row.get(..shape.expr_eval_len()) else {
                return TargetIsolationProgram::Unrepresentable;
            };
            let mut program = prefix
                .iter()
                .filter(|op| {
                    !matches!(
                        op,
                        LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. }
                    )
                })
                .cloned()
                .collect::<Vec<_>>();
            return match materialize_target_assignment(shape, &mut program) {
                Some((result, _)) => {
                    program.push(LinearOp::StoreOutput { src: result });
                    TargetIsolationProgram::Isolator(program)
                }
                None => TargetIsolationProgram::Unrepresentable,
            };
        }
        let owned_by_other_shape = self
            .row_assignment_shapes
            .get(row_idx)
            .is_some_and(|shapes| shapes.iter().any(|(output, _)| *output == output_offset));
        if owned_by_other_shape || row_output_depends_on_y_index(row, output_offset, target_y_index)
        {
            TargetIsolationProgram::Unavailable
        } else {
            TargetIsolationProgram::OutputValue
        }
    }
}

impl PreparedScalarProgramBlock {
    /// Length of the row prefix the isolator of one (output, target) pair
    /// evaluates, or `None` when the pair has no assignment shape.
    pub fn target_isolation_prefix_len(
        &self,
        row_idx: usize,
        output_offset: usize,
        target_y_index: usize,
    ) -> Option<usize> {
        self.assignment_shape_for_output(row_idx, output_offset, target_y_index)
            .map(|shape| shape.expr_eval_len())
    }

    /// One program storing the isolated values of several (output, target)
    /// pairs of one row, in the given order, from one shared evaluation of
    /// the row's expression prefix.
    ///
    /// Every pair must have an assignment shape (an
    /// [`TargetIsolationProgram::Isolator`]) over the same prefix length, so
    /// the group evaluates exactly the operations each single isolator does
    /// and fails exactly where one of them fails; each stored value equals
    /// that pair's single isolator, because every materialization reads only
    /// the unmodified prefix registers. Returns `None` when a pair has no
    /// shape, the prefix lengths differ, or a materialization does not fit.
    pub fn target_isolation_group_program(
        &self,
        row_idx: usize,
        pairs: &[(usize, usize)],
    ) -> Option<Vec<LinearOp>> {
        let row = self.block.programs().get(row_idx)?;
        let shapes = pairs
            .iter()
            .map(|&(output, target)| self.assignment_shape_for_output(row_idx, output, target))
            .collect::<Option<Vec<_>>>()?;
        let prefix_len = shapes.first()?.expr_eval_len();
        if shapes
            .iter()
            .any(|shape| shape.expr_eval_len() != prefix_len)
        {
            return None;
        }
        let mut program = row
            .get(..prefix_len)?
            .iter()
            .filter(|op| {
                !matches!(
                    op,
                    LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. }
                )
            })
            .cloned()
            .collect::<Vec<_>>();
        let mut results = Vec::with_capacity(shapes.len());
        for shape in shapes {
            results.push(materialize_target_assignment(shape, &mut program)?.0);
        }
        program.extend(results.into_iter().map(|src| LinearOp::StoreOutput { src }));
        Some(program)
    }
}

/// Consecutive causal steps recovered from one residual program: the program,
/// its `(output, target)` pairs in sweep order, and the first step. A run of
/// several pairs evaluates as one
/// [`PreparedScalarProgramBlock::target_isolation_chain_program`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TornSweepRun {
    pub program_row: usize,
    pub pairs: Vec<(usize, usize)>,
    pub first_step: usize,
}

impl PreparedScalarProgramBlock {
    /// The causal sweep `(row, target)` as runs: a step joins the run before
    /// it when both recover through the same residual program and the chain
    /// program answers every pair of the run in order. `None` when a step has
    /// no isolator program. The linked kernel and every backend group a torn
    /// sweep through this one construction.
    pub fn torn_sweep_runs(&self, causal: &[(usize, usize)]) -> Option<Vec<TornSweepRun>> {
        let mut runs: Vec<TornSweepRun> = Vec::new();
        for (step, &(row, target)) in causal.iter().enumerate() {
            let (program_row, offset) = self.row_output_position(row)?;
            if !matches!(
                self.target_isolation_output_program(program_row, offset, target),
                TargetIsolationProgram::Isolator(_)
            ) {
                return None;
            }
            if let Some(run) = runs.last_mut().filter(|run| run.program_row == program_row)
                && self.extends_run(run, (offset, target))
            {
                continue;
            }
            runs.push(TornSweepRun {
                program_row,
                pairs: vec![(offset, target)],
                first_step: step,
            });
        }
        Some(runs)
    }
}

impl PreparedScalarProgramBlock {
    /// Append `pair` to `run` when the chain program still answers it.
    fn extends_run(&self, run: &mut TornSweepRun, pair: (usize, usize)) -> bool {
        run.pairs.push(pair);
        let extends = self
            .target_isolation_chain_program(run.program_row, &run.pairs)
            .is_some();
        if !extends {
            run.pairs.pop();
        }
        extends
    }
}

impl PreparedScalarProgramBlock {
    /// One program answering several (output, target) isolations of one row
    /// in the given order, storing each value as its own output right after
    /// it is computed.
    ///
    /// The row prefix is evaluated once, extended between stores up to each
    /// pair's own prefix length, so every stored value equals that pair's
    /// single isolator on the same inputs, and a consumer that stops at a
    /// non-finite store has executed exactly the operations the single
    /// isolators before it execute: each materialization writes only
    /// registers no earlier prefix operation wrote, and its value is stored
    /// before the prefix continues. A sequential consumer writes each value
    /// before the next isolation, and a per-step isolator would evaluate its
    /// whole prefix after those writes, so no pair's isolated value may depend
    /// on the target of an earlier pair; the prefix may still read that target
    /// elsewhere, as every residual output reads its own target, because
    /// those registers do not reach the value. Returns `None` when a pair has
    /// no shape, the prefix lengths decrease, an isolated value depends on an
    /// earlier target, or a materialization does not fit.
    pub fn target_isolation_chain_program(
        &self,
        row_idx: usize,
        pairs: &[(usize, usize)],
    ) -> Option<Vec<LinearOp>> {
        let row = self.block.programs().get(row_idx)?;
        let mut program = Vec::new();
        let mut evaluated = 0;
        for (position, &(output, target)) in pairs.iter().enumerate() {
            let shape = self.assignment_shape_for_output(row_idx, output, target)?;
            let length = shape.expr_eval_len();
            let prefix = row.get(..length)?;
            if length < evaluated || value_reads_earlier_target(prefix, shape, &pairs[..position]) {
                return None;
            }
            program.extend(
                row.get(evaluated..length)?
                    .iter()
                    .filter(|op| {
                        !matches!(
                            op,
                            LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. }
                        )
                    })
                    .cloned(),
            );
            evaluated = length;
            let (result, _) = materialize_target_assignment(shape, &mut program)?;
            program.push(LinearOp::StoreOutput { src: result });
        }
        Some(program)
    }
}

/// Whether the isolated value of `shape` depends on the target of any
/// earlier pair within its row prefix.
fn value_reads_earlier_target(
    prefix: &[LinearOp],
    shape: &rumoca_ir_solve::TargetAssignmentShape,
    earlier: &[(usize, usize)],
) -> bool {
    let depends =
        |register, target| super::dependency::reg_depends_on_y_index(prefix, register, target);
    shape
        .value_registers()
        .any(|register| earlier.iter().any(|&(_, target)| depends(register, target)))
}
