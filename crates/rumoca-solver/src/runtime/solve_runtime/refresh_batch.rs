use crate::RuntimeSolveError;

use super::{InterpreterPermit, PreparedRefreshRow, PreparedRefreshRows, SolveRuntime};
use rumoca_eval_solve::ComputeNodeOutputRangeRequest;

pub(super) struct RefreshSegmentEvaluation<'a> {
    pub(super) t: f64,
    pub(super) solver_y: &'a mut [f64],
    pub(super) params: &'a [f64],
}

impl SolveRuntime {
    pub(super) fn try_refresh_tensor_output_segment(
        &self,
        selected_arm: &super::ExactAssignmentPermit,
        plan: PreparedRefreshRows<'_>,
        start: usize,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<Option<usize>, RuntimeSolveError> {
        let first = plan
            .get(start)
            .expect("prepared refresh segment starts inside its checked selection");
        if !self.can_refresh_from_tensor_output(first) {
            return Ok(None);
        }
        let Some(first_output) = self
            .implicit_scalar_rhs
            .row_output_index(first.program_row(), first.output_offset())
        else {
            return Ok(None);
        };

        let mut end = start + 1;
        let mut next_output = first_output + 1;
        while end < plan.len() {
            let row = plan
                .get(end)
                .expect("prepared refresh segment remains inside its checked selection");
            if !self.can_refresh_from_tensor_output(row) {
                break;
            }
            let Some(output_index) = self
                .implicit_scalar_rhs
                .row_output_index(row.program_row(), row.output_offset())
            else {
                break;
            };
            if output_index != next_output {
                break;
            }
            end += 1;
            next_output += 1;
        }
        if end == start + 1 {
            return Ok(None);
        }

        let mut tensor_out = self.refresh_tensor_scratch.borrow_mut();
        let len = end - start;
        let covered = self
            .implicit_rhs
            .eval_node_covering_output_range_with_context(ComputeNodeOutputRangeRequest {
                start: first_output,
                len,
                y: solver_y,
                p: params,
                t,
                context: selected_arm.row_eval_context(self),
                out: &mut tensor_out,
            })?;
        if !covered {
            return Ok(None);
        }

        for position in start..end {
            let refresh_row = plan
                .get(position)
                .expect("prepared refresh segment remains inside its checked selection");
            let Some(output_index) = self
                .implicit_scalar_rhs
                .row_output_index(refresh_row.program_row(), refresh_row.output_offset())
            else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "tensor refresh row {} output offset {} has no scalar output index",
                    refresh_row.equation_index(),
                    refresh_row.output_offset()
                )));
            };
            let Some(value) = tensor_out.get(output_index).copied() else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "tensor refresh output {} is outside {} computed outputs",
                    output_index,
                    tensor_out.len()
                )));
            };
            if !value.is_finite() {
                return Err(self.non_finite_value_error(refresh_row.target_index(), value));
            }
            solver_y[refresh_row.target_index()] = value;
        }
        Ok(Some(end))
    }

    pub(super) fn try_refresh_shapeless_output_segment(
        &self,
        selected_arm: &super::ExactAssignmentPermit,
        plan: PreparedRefreshRows<'_>,
        start: usize,
        evaluation: RefreshSegmentEvaluation<'_>,
        row_outputs: &mut Vec<f64>,
    ) -> Result<Option<usize>, RuntimeSolveError> {
        let RefreshSegmentEvaluation {
            t,
            solver_y,
            params,
        } = evaluation;
        let first = plan
            .get(start)
            .expect("prepared refresh segment starts inside its checked selection");
        let row_idx = first.program_row();
        let Some(output_count) = self.implicit_scalar_rhs.row_output_count(row_idx) else {
            return Ok(None);
        };
        if output_count <= 1 || !self.can_batch_shapeless_output_refresh(first) {
            return Ok(None);
        }
        let mut end = start + 1;
        while end < plan.len() {
            let row = plan
                .get(end)
                .expect("prepared refresh segment remains inside its checked selection");
            if row.source() != first.source() || !self.can_batch_shapeless_output_refresh(row) {
                break;
            }
            end += 1;
        }
        row_outputs.resize(output_count, 0.0);
        self.implicit_scalar_rhs
            .eval_row_outputs_unchecked_with_context(
                row_idx,
                solver_y,
                params,
                t,
                selected_arm.row_eval_context(self),
                row_outputs,
            )?;
        for position in start..end {
            let refresh_row = plan
                .get(position)
                .expect("prepared refresh segment remains inside its checked selection");
            let Some(value) = row_outputs.get(refresh_row.output_offset()).copied() else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "refresh row {} requested output offset {} from {} outputs",
                    refresh_row.equation_index(),
                    refresh_row.output_offset(),
                    row_outputs.len()
                )));
            };
            if !value.is_finite() {
                return Err(self.non_finite_value_error(refresh_row.target_index(), value));
            }
            solver_y[refresh_row.target_index()] = value;
        }
        Ok(Some(end))
    }

    pub(super) fn can_batch_assignment_refresh(&self, plan: PreparedRefreshRows<'_>) -> bool {
        plan.iter().all(|row| {
            row.assignment_target() == Some(row.target_index())
                && row
                    .assignment_shape()
                    .is_some_and(|shape| shape.target_y_index() == row.target_index())
        })
    }

    fn can_batch_shapeless_output_refresh(&self, row: PreparedRefreshRow<'_>) -> bool {
        row.assignment_target() == Some(row.target_index())
            && !self
                .implicit_scalar_rhs
                .row_has_assignment_shape(row.program_row())
            && !self
                .implicit_scalar_rhs
                .row_reads_y(row.program_row(), row.target_index())
    }

    fn can_refresh_from_tensor_output(&self, row: PreparedRefreshRow<'_>) -> bool {
        row.assignment_target() == Some(row.target_index())
            && !self
                .implicit_scalar_rhs
                .row_reads_y(row.program_row(), row.target_index())
    }
}
