//! Numeric reference capability issued only by an explicit Reference owner.
use super::{PrimalExecution, PrimalProgram, RowEvalPoint, RuntimeSolveError};
use rumoca_eval_solve::reverse::{ReverseCotangents, ReverseInputs, ReverseScratch};
use rumoca_eval_solve::{
    ComputeNodeOutputRangeRequest, PreparedComputeBlock, PreparedScalarProgramBlock,
    PreparedTornSweep, RowEvalContext, TargetAssignmentOutputRequest, TornSweepStatus,
};
use rumoca_ir_solve::AlgebraicRefreshRow;

pub(in crate::runtime::solve_runtime) struct ReferenceProgram<'source> {
    source: &'source PreparedScalarProgramBlock,
    compute: Option<&'source PreparedComputeBlock>,
}

impl PrimalProgram {
    pub(in crate::runtime::solve_runtime) fn reference(&self) -> Option<ReferenceProgram<'_>> {
        match &self.execution {
            PrimalExecution::Reference { compute } => Some(ReferenceProgram {
                source: &self.source,
                compute: compute.as_deref(),
            }),
            PrimalExecution::Compiled { .. } => None,
        }
    }
}

impl ReferenceProgram<'_> {
    pub(in crate::runtime::solve_runtime) fn eval_node_covering_output_range_with_context(
        &self,
        request: ComputeNodeOutputRangeRequest<'_>,
    ) -> Result<Option<bool>, RuntimeSolveError> {
        self.compute
            .map(|compute| {
                compute
                    .eval_node_covering_output_range_with_context(request)
                    .map_err(RuntimeSolveError::from)
            })
            .transpose()
    }

    pub(in crate::runtime::solve_runtime) fn eval_target_assignment_row_unchecked_with_context(
        &self,
        row: usize,
        target: usize,
        point: RowEvalPoint<'_>,
        context: RowEvalContext<'_>,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.source
            .eval_target_assignment_row_unchecked_with_context(
                row, target, point.y, point.p, point.t, context,
            )
            .map_err(Into::into)
    }

    pub(in crate::runtime::solve_runtime) fn eval_target_assignment_output_unchecked_with_context(
        &self,
        request: TargetAssignmentOutputRequest<'_>,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.source
            .eval_target_assignment_output_unchecked_with_context(request)
            .map_err(RuntimeSolveError::from)
    }

    pub(in crate::runtime::solve_runtime) fn apply_target_assignment_rows_unchecked_with_context<
        'row,
        I,
    >(
        &self,
        rows: I,
        program_row: impl FnMut(&AlgebraicRefreshRow) -> Option<usize>,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
    ) -> Result<(), RuntimeSolveError>
    where
        I: IntoIterator<Item = &'row AlgebraicRefreshRow>,
        I::IntoIter: ExactSizeIterator,
    {
        self.source
            .apply_target_assignment_rows_unchecked_with_context(
                rows,
                program_row,
                y,
                p,
                t,
                context,
            )
            .map_err(RuntimeSolveError::from)
    }

    pub(in crate::runtime::solve_runtime) fn eval_torn_sweep_unchecked_with_context(
        &self,
        sweep: &PreparedTornSweep,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        residual_out: &mut Vec<Option<f64>>,
    ) -> Result<TornSweepStatus, RuntimeSolveError> {
        self.source
            .eval_torn_sweep_unchecked_with_context(sweep, y, p, t, context, residual_out)
            .map_err(RuntimeSolveError::from)
    }

    pub(in crate::runtime::solve_runtime) fn reverse_row_y_gradient(
        &self,
        program: usize,
        inputs: &ReverseInputs<'_>,
        gradient: &mut [f64],
        scratch: &mut ReverseScratch,
    ) -> Result<bool, RuntimeSolveError> {
        self.source
            .reverse_row_y_gradient(program, inputs, gradient, scratch)
            .map_err(RuntimeSolveError::from)
    }

    pub(in crate::runtime::solve_runtime) fn reverse_vjp(
        &self,
        inputs: &ReverseInputs<'_>,
        output_cotangents: &[f64],
        cotangents: &mut ReverseCotangents<'_>,
        scratch: &mut ReverseScratch,
    ) -> Result<(), RuntimeSolveError> {
        self.source
            .reverse_vjp(inputs, output_cotangents, cotangents, scratch)
            .map_err(RuntimeSolveError::from)
    }
}
