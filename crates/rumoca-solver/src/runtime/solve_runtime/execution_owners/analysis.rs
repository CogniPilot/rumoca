//! Private source facts issued by a runtime execution owner.
//! No reference evaluator or whole source getter is exposed.
use rumoca_eval_solve::{
    EvalSolveError, PreparedScalarProgramBlock, PreparedTargetValuePlan, PreparedTornSweep,
    RowInputRequirements, TornSweepComposite,
};
use rumoca_ir_solve::LinearOp;

#[derive(Clone, Copy)]
pub(in crate::runtime::solve_runtime) struct ProgramAnalysis<'source> {
    source: &'source PreparedScalarProgramBlock,
}

impl<'source> ProgramAnalysis<'source> {
    pub(super) fn new(source: &'source PreparedScalarProgramBlock) -> Self {
        Self { source }
    }

    pub(in crate::runtime::solve_runtime) fn row_count(&self) -> usize {
        self.source.block().row_count()
    }

    pub(in crate::runtime::solve_runtime) fn program_span(
        &self,
        program: usize,
    ) -> Option<rumoca_core::Span> {
        self.source.block().program_span(program)
    }

    pub(in crate::runtime::solve_runtime) fn shares_program_owner(
        &self,
        source: &rumoca_ir_solve::ScalarProgramBlock,
    ) -> bool {
        self.source.block().shares_program_owner(source)
    }

    pub(in crate::runtime::solve_runtime) fn len(&self) -> usize {
        self.source.len()
    }

    pub(in crate::runtime::solve_runtime) fn requirements(&self) -> RowInputRequirements {
        self.source.requirements()
    }

    pub(in crate::runtime::solve_runtime) fn row_reads_y(&self, row: usize, index: usize) -> bool {
        self.source.row_reads_y(row, index)
    }

    pub(in crate::runtime::solve_runtime) fn row_has_assignment_shape(&self, row: usize) -> bool {
        self.source.row_has_assignment_shape(row)
    }

    pub(in crate::runtime::solve_runtime) fn has_lazy_row_plan(&self, program: usize) -> bool {
        self.source.has_lazy_row_plan(program)
    }

    pub(in crate::runtime::solve_runtime) fn direct_y_reads(
        &self,
        program: usize,
    ) -> Option<impl Iterator<Item = usize> + 'source> {
        let ops = self.source.block().program(program)?;
        Some(ops.iter().filter_map(|op| match op {
            LinearOp::LoadY { index, .. } => Some(*index),
            _ => None,
        }))
    }

    pub(in crate::runtime::solve_runtime) fn nonfinite_y_reads(
        &self,
        program: usize,
    ) -> Option<impl Iterator<Item = usize> + 'source> {
        let ops = self.source.block().program(program)?;
        let direct = ops.iter().filter_map(|op| match op {
            LinearOp::LoadY { index, .. } => Some(*index),
            _ => None,
        });
        let tensors = ops.iter().filter_map(|op| match op {
            LinearOp::TensorLoad {
                input: rumoca_ir_solve::TensorInputKind::Y,
                input_start,
                count,
                ..
            } => Some(*input_start..input_start.saturating_add(*count)),
            _ => None,
        });
        Some(direct.chain(tensors.flatten()))
    }

    pub(in crate::runtime::solve_runtime) fn program_selects_branch(
        &self,
        program: usize,
    ) -> Option<bool> {
        let ops = self.source.block().program(program)?;
        Some(ops.iter().any(|op| {
            matches!(
                op,
                LinearOp::Select { .. } | LinearOp::FunctionConditional { .. }
            )
        }))
    }

    pub(in crate::runtime::solve_runtime) fn mode_parameter_reads(
        &self,
        program: usize,
    ) -> Option<Vec<usize>> {
        let ops = self.source.block().program(program)?;
        let mut reads = Vec::new();
        collect_mode_parameter_reads(ops, &mut |index| reads.push(index));
        Some(reads)
    }

    pub(in crate::runtime::solve_runtime) fn row_output_count(&self, row: usize) -> Option<usize> {
        self.source.row_output_count(row)
    }

    pub(in crate::runtime::solve_runtime) fn row_output_index(
        &self,
        row: usize,
        offset: usize,
    ) -> Option<usize> {
        self.source.row_output_index(row, offset)
    }

    pub(in crate::runtime::solve_runtime) fn row_output_position(
        &self,
        output: usize,
    ) -> Option<(usize, usize)> {
        self.source.row_output_position(output)
    }

    pub(in crate::runtime::solve_runtime) fn single_output_row_for_output_index(
        &self,
        output: usize,
    ) -> Option<usize> {
        self.source.single_output_row_for_output_index(output)
    }

    pub(in crate::runtime::solve_runtime) fn row_parameter_indices(
        &self,
        row: usize,
    ) -> Option<&'source [usize]> {
        self.source.row_parameter_indices(row)
    }

    pub(in crate::runtime::solve_runtime) fn parameter_static_y_gradient_params(
        &self,
        row: usize,
    ) -> Option<&'source [usize]> {
        self.source.parameter_static_y_gradient_params(row)
    }

    pub(in crate::runtime::solve_runtime) fn reverse_row_y_gradient_supported(
        &self,
        row: usize,
    ) -> bool {
        self.source.reverse_row_y_gradient_supported(row)
    }

    pub(in crate::runtime::solve_runtime) fn reverse_row_unsupported_op_kinds(
        &self,
        row: usize,
    ) -> impl Iterator<Item = &'static str> + 'source {
        self.source.reverse_row_unsupported_op_kinds(row)
    }

    pub(in crate::runtime::solve_runtime) fn certifies_exact_target_assignment_output(
        &self,
        row: usize,
        offset: usize,
        target: usize,
    ) -> bool {
        self.source
            .certifies_exact_target_assignment_output(row, offset, target)
    }

    pub(in crate::runtime::solve_runtime) fn portable_target_value_plan(
        &self,
        row: usize,
        offset: usize,
        target: usize,
    ) -> Result<Option<PreparedTargetValuePlan>, EvalSolveError> {
        self.source.portable_target_value_plan(row, offset, target)
    }

    pub(in crate::runtime::solve_runtime) fn exact_target_assignment_output_program(
        &self,
        row: usize,
        offset: usize,
        target: usize,
    ) -> Option<Vec<LinearOp>> {
        self.source
            .exact_target_assignment_output_program(row, offset, target)
    }

    pub(in crate::runtime::solve_runtime) fn prepare_torn_sweep(
        &self,
        causal: &[(usize, usize)],
        residual: &[usize],
    ) -> Option<PreparedTornSweep> {
        self.source.prepare_torn_sweep(causal, residual)
    }

    pub(in crate::runtime::solve_runtime) fn torn_sweep_composite(
        &self,
        sweep: &PreparedTornSweep,
    ) -> Option<TornSweepComposite> {
        self.source.torn_sweep_composite(sweep)
    }
}

fn collect_mode_parameter_reads(program: &[LinearOp], visit: &mut dyn FnMut(usize)) {
    for op in program {
        match op {
            LinearOp::LoadP { index, .. } => visit(*index),
            LinearOp::LoadIndexedP { base, count, .. } => {
                (*base..*base + *count).for_each(&mut *visit)
            }
            LinearOp::TensorLoad {
                input: rumoca_ir_solve::TensorInputKind::P,
                input_start,
                count,
                ..
            } => (*input_start..*input_start + *count).for_each(&mut *visit),
            LinearOp::FunctionConditional { program, .. } => {
                collect_mode_conditional_reads(program, visit);
            }
            _ => {}
        }
    }
}

fn collect_mode_conditional_reads(
    program: &rumoca_ir_solve::FunctionConditionalProgram,
    visit: &mut dyn FnMut(usize),
) {
    for arm in &program.arms {
        collect_mode_parameter_reads(&arm.condition, visit);
        collect_mode_parameter_reads(&arm.result, visit);
    }
    collect_mode_parameter_reads(&program.fallback, visit);
}
