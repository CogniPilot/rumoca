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
