//! Source queries that cannot execute the prepared reference program.

use super::*;

/// Read-only source facts for an execution owner.
///
/// Numeric evaluation and learned reference specialization are deliberately
/// available only on the separately owned reference evaluator.
///
/// ```
/// use rumoca_eval_solve::{PreparedScalarProgramBlock, ScalarProgramAnalysis};
/// use rumoca_ir_solve::ScalarProgramBlock;
/// let prepared = PreparedScalarProgramBlock::new(ScalarProgramBlock::default()).unwrap();
/// let source: ScalarProgramAnalysis<'_> = prepared.analysis();
/// assert!(source.is_empty());
/// assert_eq!(source.len(), 0);
/// assert_eq!(source.row_count(), 0);
/// assert_eq!(source.program_span(0), None);
/// ```
///
/// ```compile_fail
/// use rumoca_eval_solve::{PreparedScalarProgramBlock, RowEvalContext};
/// use rumoca_ir_solve::ScalarProgramBlock;
/// let prepared = PreparedScalarProgramBlock::new(ScalarProgramBlock::default()).unwrap();
/// let source = prepared.analysis();
/// source.eval_with_context(&[], &[], 0.0, RowEvalContext::default(), &mut []).unwrap();
/// ```
///
/// ```compile_fail
/// use rumoca_eval_solve::PreparedScalarProgramBlock;
/// use rumoca_ir_solve::ScalarProgramBlock;
/// let prepared = PreparedScalarProgramBlock::new(ScalarProgramBlock::default()).unwrap();
/// let source = prepared.analysis();
/// let _reference_source = source.block();
/// ```
#[derive(Clone, Copy)]
pub struct ScalarProgramAnalysis<'source> {
    source: &'source PreparedScalarProgramBlock,
}

impl PreparedScalarProgramBlock {
    pub fn analysis(&self) -> ScalarProgramAnalysis<'_> {
        ScalarProgramAnalysis { source: self }
    }
}

impl ScalarProgramAnalysis<'_> {
    pub fn row_count(&self) -> usize {
        self.source.block().row_count()
    }

    pub fn program_span(&self, program: usize) -> Option<rumoca_core::Span> {
        self.source.block().program_span(program)
    }

    pub fn program_spans(&self) -> &[rumoca_core::Span] {
        self.source.block().program_spans()
    }

    pub fn len(&self) -> usize {
        self.source.len()
    }

    pub fn is_empty(&self) -> bool {
        self.source.is_empty()
    }

    pub fn requirements(&self) -> RowInputRequirements {
        self.source.requirements()
    }

    pub fn row_reads_y(&self, row: usize, index: usize) -> bool {
        self.source.row_reads_y(row, index)
    }

    pub fn row_has_assignment_shape(&self, row: usize) -> bool {
        self.source.row_has_assignment_shape(row)
    }

    pub fn row_output_count(&self, row: usize) -> Option<usize> {
        self.source.row_output_count(row)
    }

    pub fn row_output_index(&self, row: usize, offset: usize) -> Option<usize> {
        self.source.row_output_index(row, offset)
    }

    pub fn row_output_position(&self, output: usize) -> Option<(usize, usize)> {
        self.source.row_output_position(output)
    }

    pub fn single_output_row_for_output_index(&self, output: usize) -> Option<usize> {
        self.source.single_output_row_for_output_index(output)
    }

    pub fn row_parameter_indices(&self, row: usize) -> Option<&[usize]> {
        self.source.row_parameter_indices(row)
    }

    pub fn parameter_static_y_gradient_params(&self, row: usize) -> Option<&[usize]> {
        self.source.parameter_static_y_gradient_params(row)
    }

    pub fn reverse_row_y_gradient_supported(&self, row: usize) -> bool {
        self.source.reverse_row_y_gradient_supported(row)
    }

    pub fn reverse_row_unsupported_op_kinds(
        &self,
        row: usize,
    ) -> impl Iterator<Item = &'static str> + '_ {
        self.source.reverse_row_unsupported_op_kinds(row)
    }

    pub fn program_requires_checked_observation(&self, row: usize) -> bool {
        self.source.program_requires_checked_observation(row)
    }

    pub fn certifies_exact_target_assignment_output(
        &self,
        row: usize,
        offset: usize,
        target: usize,
    ) -> bool {
        self.source
            .certifies_exact_target_assignment_output(row, offset, target)
    }

    pub fn can_evaluate_target_assignment_output(
        &self,
        row: usize,
        offset: usize,
        target: usize,
    ) -> bool {
        self.source
            .can_evaluate_target_assignment_output(row, offset, target)
    }

    pub fn portable_target_value_plan(
        &self,
        row: usize,
        offset: usize,
        target: usize,
    ) -> Result<Option<PreparedTargetValuePlan>, EvalSolveError> {
        self.source.portable_target_value_plan(row, offset, target)
    }

    pub fn exact_target_assignment_output_program(
        &self,
        row: usize,
        offset: usize,
        target: usize,
    ) -> Option<Vec<LinearOp>> {
        self.source
            .exact_target_assignment_output_program(row, offset, target)
    }

    pub fn prepare_torn_sweep(
        &self,
        causal: &[(usize, usize)],
        residual: &[usize],
    ) -> Option<PreparedTornSweep> {
        self.source.prepare_torn_sweep(causal, residual)
    }

    pub fn torn_sweep_composite(&self, sweep: &PreparedTornSweep) -> Option<TornSweepComposite> {
        self.source.torn_sweep_composite(sweep)
    }
}
