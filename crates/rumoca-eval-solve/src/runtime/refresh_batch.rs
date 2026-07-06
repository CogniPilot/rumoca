use rumoca_solver::RuntimeSolveError;

use crate::prepared::ComputeNodeOutputRangeRequest;
use crate::refresh_plan::AlgebraicRefreshRow;
use crate::runtime::SolveRuntime;

impl SolveRuntime {
    pub(super) fn try_refresh_tensor_output_segment(
        &self,
        plan: &[AlgebraicRefreshRow],
        start: usize,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<Option<usize>, RuntimeSolveError> {
        let first = &plan[start];
        if !self.can_refresh_from_tensor_output(first) {
            return Ok(None);
        }
        let Some(first_output) = self
            .implicit_scalar_rhs
            .row_output_index(first.row_idx, first.output_offset)
        else {
            return Ok(None);
        };

        let mut end = start + 1;
        let mut next_output = first_output + 1;
        while end < plan.len() && self.can_refresh_from_tensor_output(&plan[end]) {
            let Some(output_index) = self
                .implicit_scalar_rhs
                .row_output_index(plan[end].row_idx, plan[end].output_offset)
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
                context: self.row_eval_context(),
                out: &mut tensor_out,
            })?;
        if !covered {
            return Ok(None);
        }

        for refresh_row in &plan[start..end] {
            let Some(output_index) = self
                .implicit_scalar_rhs
                .row_output_index(refresh_row.row_idx, refresh_row.output_offset)
            else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "tensor refresh row {} output offset {} has no scalar output index",
                    refresh_row.row_idx, refresh_row.output_offset
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
                return Err(self.non_finite_value_error(refresh_row.target_index, value));
            }
            solver_y[refresh_row.target_index] = value;
        }
        Ok(Some(end))
    }

    pub(super) fn try_refresh_shapeless_output_segment(
        &self,
        plan: &[AlgebraicRefreshRow],
        start: usize,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        row_outputs: &mut Vec<f64>,
    ) -> Result<Option<usize>, RuntimeSolveError> {
        let first = &plan[start];
        let Some(output_count) = self.implicit_scalar_rhs.row_output_count(first.row_idx) else {
            return Ok(None);
        };
        if output_count <= 1 || !self.can_batch_shapeless_output_refresh(first) {
            return Ok(None);
        }
        let row_idx = first.row_idx;
        let mut end = start + 1;
        while end < plan.len()
            && plan[end].row_idx == row_idx
            && self.can_batch_shapeless_output_refresh(&plan[end])
        {
            end += 1;
        }
        self.implicit_scalar_rhs
            .eval_row_outputs_unchecked_with_context(
                row_idx,
                solver_y,
                params,
                t,
                self.row_eval_context(),
                row_outputs,
            )?;
        for refresh_row in &plan[start..end] {
            let Some(value) = row_outputs.get(refresh_row.output_offset).copied() else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "refresh row {} requested output offset {} from {} outputs",
                    refresh_row.row_idx,
                    refresh_row.output_offset,
                    row_outputs.len()
                )));
            };
            if !value.is_finite() {
                return Err(self.non_finite_value_error(refresh_row.target_index, value));
            }
            solver_y[refresh_row.target_index] = value;
        }
        Ok(Some(end))
    }

    pub(super) fn can_batch_assignment_refresh(&self, plan: &[AlgebraicRefreshRow]) -> bool {
        plan.iter().all(|row| {
            row.assignment_target == Some(row.target_index)
                && row.output_offset == 0
                && self
                    .implicit_scalar_rhs
                    .row_has_assignment_shape(row.row_idx)
                && self
                    .implicit_scalar_rhs
                    .can_evaluate_target_assignment(row.row_idx, row.target_index)
        })
    }

    fn can_batch_shapeless_output_refresh(&self, row: &AlgebraicRefreshRow) -> bool {
        row.assignment_target == Some(row.target_index)
            && !self
                .implicit_scalar_rhs
                .row_has_assignment_shape(row.row_idx)
            && !self
                .implicit_scalar_rhs
                .row_reads_y(row.row_idx, row.target_index)
    }

    fn can_refresh_from_tensor_output(&self, row: &AlgebraicRefreshRow) -> bool {
        row.assignment_target == Some(row.target_index)
            && !self
                .implicit_scalar_rhs
                .row_reads_y(row.row_idx, row.target_index)
    }
}
