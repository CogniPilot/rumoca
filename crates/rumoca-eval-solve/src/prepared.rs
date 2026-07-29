//! Prepared Solve-IR evaluation and tensor-node orchestration.

mod assignment_shape;
#[cfg(test)]
mod assignment_shape_tests;
mod dependency;
#[cfg(test)]
mod prepared_compute_block_tests;

use std::cell::RefCell;

use crate::refresh_plan::AlgebraicRefreshRow;
use crate::tensor_policy::{
    LinearSolveKernel, MatMulKernel, select_linear_solve_kernel, select_matmul_kernel,
};
use crate::{
    EvalSolveError, OutputCursor, PreparedRowEval, RowEvalContext, RowEvalScratch,
    RowInputRequirements, SimulationRuntimeState,
    compute_block_scalarize::{
        checked_contiguous_output_count, scalar_program_output_count,
        scalar_program_output_indices, tensor_output_count, validate_affine_stride_metadata,
    },
    eval_program_no_output, eval_program_single, eval_row_prepared_maybe_fast,
    linear_solve::solve_all_unchecked,
    record_solve_block_eval, required_registers, row_input_requirements,
    validate_input_requirements, validate_input_requirements_with_span, validate_output_len,
};
#[cfg(test)]
use assignment_shape::checked_expr_eval_len;
pub use assignment_shape::{
    TargetAssignmentShape, target_assignment_shape, target_assignment_shapes,
};
use rumoca_core::StructuredIndexDomain;
use rumoca_ir_solve::{
    AffineStencilConstStride, AffineStencilLoadStride, ComputeBlock, ComputeNode, LinearOp,
    ScalarProgramBlock, StructuralPattern, TensorOutputMap,
};

/// Reusable evaluator for one Solve-IR row block.
pub struct PreparedScalarProgramBlock {
    block: ScalarProgramBlock,
    output_count: usize,
    row_outputs: Box<PreparedRowOutputMetadata>,
    row_registers: Vec<usize>,
    row_requirements: Vec<RowInputRequirements>,
    row_seed_loads: Vec<Box<[PreparedSeedLoad]>>,
    row_assignment_shapes: Vec<Box<[TargetAssignmentShape]>>,
    requirements: RowInputRequirements,
    scratch: RefCell<RowEvalScratch>,
    row_output_scratch: RefCell<Vec<f64>>,
}

#[derive(Clone, Copy)]
enum PreparedSeedLoad {
    Direct(usize),
    Indexed { base: usize, count: usize },
}

impl Clone for PreparedScalarProgramBlock {
    fn clone(&self) -> Self {
        Self {
            block: self.block.clone(),
            output_count: self.output_count,
            row_outputs: self.row_outputs.clone(),
            row_registers: self.row_registers.clone(),
            row_requirements: self.row_requirements.clone(),
            row_seed_loads: self.row_seed_loads.clone(),
            row_assignment_shapes: self.row_assignment_shapes.clone(),
            requirements: self.requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
            row_output_scratch: RefCell::new(Vec::new()),
        }
    }
}

impl PreparedScalarProgramBlock {
    pub fn new(block: ScalarProgramBlock) -> Result<Self, EvalSolveError> {
        let row_count = block.programs().len();
        let block_span = block.program_span(0);
        let output_count = checked_prepared_output_count(&block)?;
        let row_outputs = Box::new(prepare_row_output_metadata(&block, output_count)?);
        let mut row_registers =
            prepared_vec_with_capacity(row_count, "prepared row register count", block_span)?;
        let mut row_requirements =
            prepared_vec_with_capacity(row_count, "prepared row requirement count", block_span)?;
        let mut row_seed_loads =
            prepared_vec_with_capacity(row_count, "prepared row seed load count", block_span)?;
        let mut row_assignment_shapes = prepared_vec_with_capacity(
            row_count,
            "prepared row assignment shape count",
            block_span,
        )?;
        let mut requirements = RowInputRequirements::default();
        for (row_idx, row) in block.programs().iter().enumerate() {
            let span = block.program_span(row_idx);
            let row_requirement =
                row_input_requirements(row).map_err(|error| error.with_source_span(span))?;
            row_registers
                .push(required_registers(row).map_err(|error| error.with_source_span(span))?);
            row_requirements.push(row_requirement);
            row_seed_loads.push(prepared_seed_loads(row, span)?);
            row_assignment_shapes.push(
                target_assignment_shapes(row)
                    .map_err(|error| error.with_source_span(span))?
                    .into_boxed_slice(),
            );
            requirements = requirements.merge(row_requirement);
        }
        Ok(Self {
            block,
            output_count,
            row_outputs,
            row_registers,
            row_requirements,
            row_seed_loads,
            row_assignment_shapes,
            requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
            row_output_scratch: RefCell::new(Vec::new()),
        })
    }

    pub fn from_compute_block(block: &ComputeBlock) -> Result<Self, EvalSolveError> {
        Self::new(crate::to_scalar_program_block(block)?)
    }

    pub fn block(&self) -> &ScalarProgramBlock {
        &self.block
    }

    /// Number of outputs this block produces (one per `StoreOutput`), which a
    /// matmul/linsolve program may exceed its program count for. Consumers size
    /// their output buffers from this.
    pub fn len(&self) -> usize {
        self.output_count
    }

    pub fn is_empty(&self) -> bool {
        self.block.is_empty()
    }

    pub fn requirements(&self) -> RowInputRequirements {
        self.requirements
    }

    pub fn reverse_row_y_gradient_supported(&self, row_idx: usize) -> bool {
        self.block.programs().get(row_idx).is_some_and(|row| {
            row.iter()
                .filter(|op| matches!(op, LinearOp::StoreOutput { .. }))
                .count()
                == 1
                && row.iter().all(crate::reverse::reverse_row_op_supported)
        })
    }

    pub fn reverse_row_unsupported_op_kinds(
        &self,
        row_idx: usize,
    ) -> impl Iterator<Item = &'static str> + '_ {
        self.block
            .programs()
            .get(row_idx)
            .into_iter()
            .flatten()
            .filter(|op| !crate::reverse::reverse_row_op_supported(op))
            .map(LinearOp::kind_name)
    }

    /// Reverse-mode VJP: accumulate `Jᵀ · output_cotangents` of this block into
    /// `cot` at the `LoadY` / `LoadP` / `LoadSeed` input sites (Track A scalar
    /// reverse core). `scratch` is caller-owned so a hot loop stays
    /// allocation-free. See [`crate::reverse`].
    pub fn reverse_vjp(
        &self,
        inputs: &crate::reverse::ReverseInputs<'_>,
        output_cotangents: &[f64],
        cot: &mut crate::reverse::ReverseCotangents<'_>,
        scratch: &mut crate::reverse::ReverseScratch,
    ) -> Result<(), EvalSolveError> {
        crate::reverse::reverse_scalar_block_vjp(
            &crate::reverse::ScalarVjpProgram {
                block: &self.block,
                row_registers: &self.row_registers,
                requirements: self.requirements,
            },
            inputs,
            output_cotangents,
            cot,
            scratch,
        )
    }

    /// Evaluate the complete solver-`y` gradient of one scalar residual row.
    /// Returns `false` when that row contains an operation without a reverse AD
    /// rule, allowing the projection solver to retain its exact forward-JVP
    /// fallback.
    pub fn reverse_row_y_gradient(
        &self,
        row_idx: usize,
        inputs: &crate::reverse::ReverseInputs<'_>,
        gradient: &mut [f64],
        scratch: &mut crate::reverse::ReverseScratch,
    ) -> Result<bool, EvalSolveError> {
        let Some(requirements) = self.row_requirements.get(row_idx).copied() else {
            return Ok(false);
        };
        if !self.reverse_row_y_gradient_supported(row_idx) {
            return Ok(false);
        }
        validate_output_len(gradient, inputs.y.len())?;
        validate_input_requirements(requirements, inputs.y, inputs.p, inputs.context.seed)?;
        record_solve_block_eval("scalar_reverse_row", self.output_count, 1);
        crate::reverse::reverse_scalar_row_y_gradient(
            &crate::reverse::ScalarVjpProgram {
                block: &self.block,
                row_registers: &self.row_registers,
                requirements,
            },
            row_idx,
            inputs,
            gradient,
            scratch,
        )
    }

    pub fn eval_with_context(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), EvalSolveError> {
        let local_runtime_state;
        let context = match context.runtime_state {
            Some(_) => context,
            None => {
                local_runtime_state = SimulationRuntimeState::new();
                context.with_runtime_state(&local_runtime_state)
            }
        };
        validate_output_len(out, self.output_count)?;
        validate_input_requirements(self.requirements, y, p, context.seed)?;
        out.fill(0.0);
        let mut scratch = self.scratch.borrow_mut();
        self.eval_rows_unchecked(y, p, t, context, out, &mut scratch)
    }

    pub fn eval_prefix_with_context(
        &self,
        rows: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), EvalSolveError> {
        let rows = rows.min(self.block.row_count());
        let prefix = &self.block.programs()[..rows];
        let stored_output_count = self.row_outputs.offsets[rows];
        let local_runtime_state;
        let context = match context.runtime_state {
            Some(_) => context,
            None => {
                local_runtime_state = SimulationRuntimeState::new();
                context.with_runtime_state(&local_runtime_state)
            }
        };
        let prefix_output_indices = self
            .block
            .output_indices()
            .get(..stored_output_count)
            .ok_or_else(|| EvalSolveError::ShapeContract {
                message: format!(
                    "prepared prefix has {stored_output_count} stored outputs but only {} output indices",
                    self.block.output_indices().len()
                ),
                span: self.block.program_span(0),
            })?;
        let output_count = prefix_output_indices
            .iter()
            .copied()
            .max()
            .map_or(0, |index| index + 1);
        validate_output_len(out, output_count)?;
        let requirements = self
            .row_requirements
            .iter()
            .take(rows)
            .copied()
            .fold(RowInputRequirements::default(), RowInputRequirements::merge);
        validate_input_requirements(requirements, y, p, context.seed)?;
        out[..output_count].fill(0.0);
        let mut scratch = self.scratch.borrow_mut();
        record_solve_block_eval("scalar_prefix", self.output_count, output_count);
        let mut sink = OutputCursor::with_output_indices(out, prefix_output_indices);
        for (row_idx, row) in prefix.iter().enumerate() {
            eval_row_prepared_maybe_fast(
                PreparedRowEval::new(row, self.row_registers[row_idx], y, p, t, context)
                    .with_source_span(self.block.program_span(row_idx)),
                true,
                &mut scratch,
                &mut sink,
            )
            .map_err(|error| error.with_source_span(self.block.program_span(row_idx)))?;
        }
        Ok(())
    }

    pub fn eval_row_with_context(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
    ) -> Result<f64, EvalSolveError> {
        self.eval_row_inner(RowEvalRequest {
            row_idx,
            y,
            p,
            t,
            context,
            validate_inputs: true,
            label: "scalar_row",
        })
    }

    pub fn eval_row_unchecked_with_context(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
    ) -> Result<f64, EvalSolveError> {
        self.eval_row_inner(RowEvalRequest {
            row_idx,
            y,
            p,
            t,
            context,
            validate_inputs: false,
            label: "scalar_row_unchecked",
        })
    }

    pub fn eval_row_output_unchecked_with_context(
        &self,
        row_idx: usize,
        output_offset: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
    ) -> Result<f64, EvalSolveError> {
        self.eval_row_output_inner(RowOutputRequest {
            row_idx,
            output_offset,
            y,
            p,
            t,
            context,
            validate_inputs: false,
            label: "scalar_row_output_unchecked",
        })
    }

    pub fn eval_single_output_rows_unchecked_with_context(
        &self,
        row_indices: &[usize],
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), EvalSolveError> {
        let mut scratch = self.scratch.borrow_mut();
        record_solve_block_eval(
            "scalar_selected_rows_unchecked",
            self.output_count,
            row_indices.len(),
        );
        let out_len = out.len();
        for &row_idx in row_indices {
            let row = self
                .block
                .programs()
                .get(row_idx)
                .ok_or(EvalSolveError::OutputTooSmall {
                    required: checked_required_row_count(row_idx)?,
                    len: self.block.row_count(),
                    span: self.block.program_span(row_idx),
                })?;
            let slot = out.get_mut(row_idx).ok_or(EvalSolveError::OutputTooSmall {
                required: checked_required_row_count(row_idx)?,
                len: out_len,
                span: self.block.program_span(row_idx),
            })?;
            let mut sink = OutputCursor::new(std::slice::from_mut(slot));
            eval_row_prepared_maybe_fast(
                PreparedRowEval::new(row, self.row_registers[row_idx], y, p, t, context)
                    .with_source_span(self.block.program_span(row_idx)),
                true,
                &mut scratch,
                &mut sink,
            )
            .map_err(|error| error.with_source_span(self.block.program_span(row_idx)))?;
        }
        Ok(())
    }

    fn eval_row_inner(&self, request: RowEvalRequest<'_>) -> Result<f64, EvalSolveError> {
        let row =
            self.block
                .programs()
                .get(request.row_idx)
                .ok_or(EvalSolveError::OutputTooSmall {
                    required: checked_required_row_count(request.row_idx)?,
                    len: self.block.row_count(),
                    span: self.block.program_span(request.row_idx),
                })?;
        if request.validate_inputs {
            validate_input_requirements_with_span(
                self.row_requirements[request.row_idx],
                request.y,
                request.p,
                request.context.seed,
                self.block.program_span(request.row_idx),
            )?;
        }
        let mut scratch = self.scratch.borrow_mut();
        record_solve_block_eval(request.label, self.output_count, 1);
        eval_program_single(
            PreparedRowEval::new(
                row,
                self.row_registers[request.row_idx],
                request.y,
                request.p,
                request.t,
                request.context,
            )
            .with_source_span(self.block.program_span(request.row_idx)),
            true,
            &mut scratch,
        )
        .map_err(|error| error.with_source_span(self.block.program_span(request.row_idx)))
    }

    fn eval_row_output_inner(&self, request: RowOutputRequest<'_>) -> Result<f64, EvalSolveError> {
        let row =
            self.block
                .programs()
                .get(request.row_idx)
                .ok_or(EvalSolveError::OutputTooSmall {
                    required: checked_required_row_count(request.row_idx)?,
                    len: self.block.row_count(),
                    span: self.block.program_span(request.row_idx),
                })?;
        if request.validate_inputs {
            validate_input_requirements_with_span(
                self.row_requirements[request.row_idx],
                request.y,
                request.p,
                request.context.seed,
                self.block.program_span(request.row_idx),
            )?;
        }
        let output_count = self.row_output_count(request.row_idx).ok_or_else(|| {
            invalid_prepared_row("prepared row output metadata is missing the requested row")
        })?;
        if request.output_offset >= output_count {
            return Err(EvalSolveError::OutputTooSmall {
                required: request.output_offset.checked_add(1).ok_or_else(|| {
                    invalid_prepared_row("row output offset overflows output count")
                })?,
                len: output_count,
                span: self.block.program_span(request.row_idx),
            });
        }
        let mut out = self.row_output_scratch.borrow_mut();
        reserve_prepared_vec_capacity(
            &mut out,
            output_count,
            "prepared row output scratch count",
            self.block.program_span(request.row_idx),
        )?;
        out.resize(output_count, 0.0);
        out[..output_count].fill(0.0);
        let mut scratch = self.scratch.borrow_mut();
        record_solve_block_eval(request.label, self.output_count, output_count);
        let mut sink = OutputCursor::new(&mut out);
        eval_row_prepared_maybe_fast(
            PreparedRowEval::new(
                row,
                self.row_registers[request.row_idx],
                request.y,
                request.p,
                request.t,
                request.context,
            )
            .with_source_span(self.block.program_span(request.row_idx)),
            true,
            &mut scratch,
            &mut sink,
        )
        .map_err(|error| error.with_source_span(self.block.program_span(request.row_idx)))?;
        Ok(out[request.output_offset])
    }

    pub fn eval_target_assignment_row_with_context(
        &self,
        row_idx: usize,
        target_y_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
    ) -> Result<Option<f64>, EvalSolveError> {
        self.eval_target_assignment_row_inner(TargetAssignmentRowRequest {
            row_idx,
            target_y_index,
            y,
            p,
            t,
            context,
            validate_inputs: true,
            label: "target_row",
        })
    }

    /// True when the row's program loads the given solver-Y slot.
    pub fn row_reads_y(&self, row_idx: usize, y_index: usize) -> bool {
        self.block
            .programs()
            .get(row_idx)
            .is_some_and(|row| row_loads_y_index(row, y_index))
    }

    pub fn row_seed_depends_on(&self, row_idx: usize, seed_index: usize) -> bool {
        self.row_seed_loads.get(row_idx).is_none_or(|loads| {
            loads.iter().any(|load| match *load {
                PreparedSeedLoad::Direct(index) => index == seed_index,
                PreparedSeedLoad::Indexed { base, count } => seed_index
                    .checked_sub(base)
                    .is_some_and(|offset| offset < count),
            })
        })
    }

    /// True when the row was lowered with an explicit assignment shape
    /// (`target = expr`); its full program then evaluates the residual, while
    /// shapeless rows with an implicit target evaluate the target value.
    pub fn row_has_assignment_shape(&self, row_idx: usize) -> bool {
        self.row_assignment_shapes
            .get(row_idx)
            .is_some_and(|shapes| !shapes.is_empty())
    }

    pub fn row_output_count(&self, row_idx: usize) -> Option<usize> {
        let start = *self.row_outputs.offsets.get(row_idx)?;
        let end = *self.row_outputs.offsets.get(row_idx.checked_add(1)?)?;
        end.checked_sub(start)
    }

    pub fn row_output_index(&self, row_idx: usize, output_offset: usize) -> Option<usize> {
        if output_offset >= self.row_output_count(row_idx)? {
            return None;
        }
        let stored_ordinal = self.row_outputs.offsets[row_idx].checked_add(output_offset)?;
        self.block.output_indices().get(stored_ordinal).copied()
    }

    /// Resolve a logical block output to its sole scalar program row.
    /// Assignment-shape evaluation is row-based, while tensor/scalarized
    /// compute blocks may place rows through a non-identity output map.
    pub fn single_output_row_for_output_index(&self, output_index: usize) -> Option<usize> {
        self.row_outputs
            .single_rows
            .get(output_index)
            .copied()
            .flatten()
    }

    pub fn can_evaluate_target_assignment(&self, row_idx: usize, target_y_index: usize) -> bool {
        let Some(row) = self.block.programs().get(row_idx) else {
            return false;
        };
        self.assignment_shape(row_idx, target_y_index).is_some()
            || !row_loads_y_index(row, target_y_index)
    }

    pub(crate) fn certifies_direct_target_assignment(
        &self,
        row_idx: usize,
        target_y_index: usize,
    ) -> bool {
        let Some(row) = self.block.programs().get(row_idx) else {
            return false;
        };
        if row.iter().any(non_causal_linear_op) {
            return false;
        }
        matches!(
            self.assignment_shape(row_idx, target_y_index),
            Some(TargetAssignmentShape::Direct { .. })
        )
    }

    pub(crate) fn certifies_exact_target_assignment(
        &self,
        row_idx: usize,
        target_y_index: usize,
    ) -> bool {
        let Some(row) = self.block.programs().get(row_idx) else {
            return false;
        };
        !row.iter().any(non_causal_linear_op)
            && self.assignment_shape(row_idx, target_y_index).is_some()
    }

    pub fn eval_target_assignment_row_unchecked_with_context(
        &self,
        row_idx: usize,
        target_y_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
    ) -> Result<Option<f64>, EvalSolveError> {
        self.eval_target_assignment_row_inner(TargetAssignmentRowRequest {
            row_idx,
            target_y_index,
            y,
            p,
            t,
            context,
            validate_inputs: false,
            label: "target_row_unchecked",
        })
    }

    pub fn eval_row_outputs_unchecked_with_context(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut Vec<f64>,
    ) -> Result<(), EvalSolveError> {
        let row = self
            .block
            .programs()
            .get(row_idx)
            .ok_or(EvalSolveError::OutputTooSmall {
                required: checked_required_row_count(row_idx)?,
                len: self.block.row_count(),
                span: self.block.program_span(row_idx),
            })?;
        let output_count = self.row_output_count(row_idx).ok_or_else(|| {
            invalid_prepared_row("prepared row output metadata is missing the requested row")
        })?;
        out.resize(output_count, 0.0);
        out.fill(0.0);
        let mut scratch = self.scratch.borrow_mut();
        record_solve_block_eval(
            "scalar_row_outputs_unchecked",
            self.block.len(),
            output_count,
        );
        let mut sink = OutputCursor::new(out.as_mut_slice());
        eval_row_prepared_maybe_fast(
            PreparedRowEval::new(row, self.row_registers[row_idx], y, p, t, context)
                .with_source_span(self.block.program_span(row_idx)),
            true,
            &mut scratch,
            &mut sink,
        )
        .map_err(|error| error.with_source_span(self.block.program_span(row_idx)))
    }

    pub fn apply_target_assignment_rows_unchecked_with_context(
        &self,
        rows: &[AlgebraicRefreshRow],
        y: &mut [f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
    ) -> Result<(), EvalSolveError> {
        let local_runtime_state;
        let context = match context.runtime_state {
            Some(_) => context,
            None => {
                local_runtime_state = SimulationRuntimeState::new();
                context.with_runtime_state(&local_runtime_state)
            }
        };
        let mut scratch = self.scratch.borrow_mut();
        record_solve_block_eval("target_rows_batch", self.block.len(), rows.len());
        for row in rows {
            let value =
                self.eval_target_assignment_row_with_scratch(TargetAssignmentScratchRequest {
                    row_idx: row.row_idx,
                    target_y_index: row.target_index,
                    y,
                    p,
                    t,
                    context,
                    scratch: &mut scratch,
                })?;
            y[row.target_index] = value;
        }
        Ok(())
    }

    fn eval_target_assignment_row_inner(
        &self,
        request: TargetAssignmentRowRequest<'_>,
    ) -> Result<Option<f64>, EvalSolveError> {
        let row =
            self.block
                .programs()
                .get(request.row_idx)
                .ok_or(EvalSolveError::OutputTooSmall {
                    required: checked_required_row_count(request.row_idx)?,
                    len: self.block.row_count(),
                    span: self.block.program_span(request.row_idx),
                })?;
        if request.validate_inputs {
            validate_input_requirements_with_span(
                self.row_requirements[request.row_idx],
                request.y,
                request.p,
                request.context.seed,
                self.block.program_span(request.row_idx),
            )?;
        }
        let mut scratch = self.scratch.borrow_mut();
        record_solve_block_eval(request.label, self.output_count, 1);
        let Some(shape) = self.assignment_shape(request.row_idx, request.target_y_index) else {
            // No assignment shape means the row is an ordinary residual. It is
            // only reusable for a target update when it does not read that same
            // target slot; otherwise the parent receives None and tries another row.
            if !self.row_assignment_shapes[request.row_idx].is_empty() {
                return Ok(None);
            }
            let output = eval_program_single(
                PreparedRowEval::new(
                    row,
                    self.row_registers[request.row_idx],
                    request.y,
                    request.p,
                    request.t,
                    request.context,
                )
                .with_source_span(self.block.program_span(request.row_idx)),
                true,
                &mut scratch,
            )
            .map_err(|error| error.with_source_span(self.block.program_span(request.row_idx)))?;
            return Ok((!row_loads_y_index(row, request.target_y_index)).then_some(output));
        };
        eval_program_no_output(
            PreparedRowEval::new(
                &row[..shape.expr_eval_len()],
                self.row_registers[request.row_idx],
                request.y,
                request.p,
                request.t,
                request.context,
            )
            .with_source_span(self.block.program_span(request.row_idx)),
            true,
            &mut scratch,
        )
        .map_err(|error| error.with_source_span(self.block.program_span(request.row_idx)))?;
        let value = shape
            .eval_value(
                request.row_idx,
                &scratch.regs,
                self.block.program_span(request.row_idx),
            )
            .map_err(|error| error.with_source_span(self.block.program_span(request.row_idx)))?;
        Ok(Some(value))
    }

    fn eval_target_assignment_row_with_scratch(
        &self,
        request: TargetAssignmentScratchRequest<'_>,
    ) -> Result<f64, EvalSolveError> {
        let row =
            self.block
                .programs()
                .get(request.row_idx)
                .ok_or(EvalSolveError::OutputTooSmall {
                    required: checked_required_row_count(request.row_idx)?,
                    len: self.block.row_count(),
                    span: self.block.program_span(request.row_idx),
                })?;
        let Some(shape) = self.assignment_shape(request.row_idx, request.target_y_index) else {
            return Err(invalid_prepared_row_with_span(
                "batched target assignment row has no matching assignment shape",
                self.block.program_span(request.row_idx),
            ));
        };
        eval_program_no_output(
            PreparedRowEval::new(
                &row[..shape.expr_eval_len()],
                self.row_registers[request.row_idx],
                request.y,
                request.p,
                request.t,
                request.context,
            )
            .with_source_span(self.block.program_span(request.row_idx)),
            true,
            &mut *request.scratch,
        )
        .map_err(|error| error.with_source_span(self.block.program_span(request.row_idx)))?;
        shape
            .eval_value(
                request.row_idx,
                &request.scratch.regs,
                self.block.program_span(request.row_idx),
            )
            .map_err(|error| error.with_source_span(self.block.program_span(request.row_idx)))
    }

    fn assignment_shape(
        &self,
        row_idx: usize,
        target_y_index: usize,
    ) -> Option<TargetAssignmentShape> {
        self.row_assignment_shapes
            .get(row_idx)?
            .iter()
            .copied()
            .find(|shape| shape.target_y_index() == target_y_index)
    }

    fn eval_rows_unchecked(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut [f64],
        scratch: &mut RowEvalScratch,
    ) -> Result<(), EvalSolveError> {
        record_solve_block_eval(
            "scalar_rows_unchecked",
            self.output_count,
            self.output_count,
        );
        let mut sink = OutputCursor::with_output_indices(out, self.block.output_indices());
        for (row_idx, row) in self.block.programs().iter().enumerate() {
            eval_row_prepared_maybe_fast(
                PreparedRowEval::new(row, self.row_registers[row_idx], y, p, t, context)
                    .with_source_span(self.block.program_span(row_idx)),
                true,
                scratch,
                &mut sink,
            )
            .map_err(|error| error.with_source_span(self.block.program_span(row_idx)))?;
        }
        Ok(())
    }
}

struct RowEvalRequest<'a> {
    row_idx: usize,
    y: &'a [f64],
    p: &'a [f64],
    t: f64,
    context: RowEvalContext<'a>,
    validate_inputs: bool,
    label: &'static str,
}

struct RowOutputRequest<'a> {
    row_idx: usize,
    output_offset: usize,
    y: &'a [f64],
    p: &'a [f64],
    t: f64,
    context: RowEvalContext<'a>,
    validate_inputs: bool,
    label: &'static str,
}

struct TargetAssignmentRowRequest<'a> {
    row_idx: usize,
    target_y_index: usize,
    y: &'a [f64],
    p: &'a [f64],
    t: f64,
    context: RowEvalContext<'a>,
    validate_inputs: bool,
    label: &'static str,
}

struct TargetAssignmentScratchRequest<'a> {
    row_idx: usize,
    target_y_index: usize,
    y: &'a [f64],
    p: &'a [f64],
    t: f64,
    context: RowEvalContext<'a>,
    scratch: &'a mut RowEvalScratch,
}

fn row_loads_y_index(row: &[LinearOp], target_y_index: usize) -> bool {
    row.iter().any(|op| {
        matches!(
            *op,
            LinearOp::LoadY { index, .. } if index == target_y_index
        )
    })
}

fn producer(row: &[LinearOp], dst_reg: u32) -> Option<&LinearOp> {
    row.iter()
        .rev()
        .find(|op| op.dst_register() == Some(dst_reg))
}

/// Reusable evaluator for a full tensor-aware Solve-IR compute block.
///
/// This is an execution preparation, not another lowering phase: it preserves
/// the original `ComputeNode` structure and only precomputes validation data.
pub struct PreparedComputeBlock {
    label: &'static str,
    nodes: Vec<PreparedComputeNode>,
    len: usize,
    requirements: RowInputRequirements,
    scratch: RefCell<RowEvalScratch>,
}

/// Output-range refresh request for a prepared compute node.
///
/// `pub` for `rumoca_solver::runtime::solve_runtime`, which batches algebraic
/// refreshes through this entry point.
pub struct ComputeNodeOutputRangeRequest<'a> {
    pub start: usize,
    pub len: usize,
    pub y: &'a [f64],
    pub p: &'a [f64],
    pub t: f64,
    pub context: RowEvalContext<'a>,
    pub out: &'a mut Vec<f64>,
}

impl Clone for PreparedComputeBlock {
    fn clone(&self) -> Self {
        Self {
            label: self.label,
            nodes: self.nodes.clone(),
            len: self.len,
            requirements: self.requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
        }
    }
}

impl PreparedComputeBlock {
    pub fn new(block: &ComputeBlock) -> Result<Self, EvalSolveError> {
        Self::new_with_label(block, "compute_block")
    }

    pub fn new_with_label(
        block: &ComputeBlock,
        label: &'static str,
    ) -> Result<Self, EvalSolveError> {
        let declared_len = block.len().map_err(EvalSolveError::from)?;
        let mut requirements = RowInputRequirements::default();
        let mut output_cursor = 0usize;
        let mut nodes = prepared_vec_with_capacity(
            block.nodes.len(),
            "prepared compute node count",
            first_compute_node_span(block),
        )?;
        for node in &block.nodes {
            let (prepared, next_output_cursor) =
                PreparedComputeNode::new_at_output_cursor(node, output_cursor)?;
            output_cursor = next_output_cursor;
            requirements = requirements.merge(prepared.requirements());
            nodes.push(prepared);
        }
        if output_cursor > declared_len {
            return Err(EvalSolveError::ShapeContract {
                message: format!(
                    "prepared {label} advanced to {output_cursor} outputs, beyond declared \
                     ComputeBlock length {declared_len}"
                ),
                span: first_compute_node_span(block),
            });
        }
        Ok(Self {
            label,
            nodes,
            len: declared_len,
            requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
        })
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn eval_with_context(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), EvalSolveError> {
        let local_runtime_state;
        let context = match context.runtime_state {
            Some(_) => context,
            None => {
                local_runtime_state = SimulationRuntimeState::new();
                context.with_runtime_state(&local_runtime_state)
            }
        };
        validate_output_len(out, self.len)?;
        validate_input_requirements(self.requirements, y, p, context.seed)?;
        out.fill(0.0);
        record_solve_block_eval(self.label, self.len, self.len);
        let mut scratch = self.scratch.borrow_mut();
        for node in &self.nodes {
            node.eval_into(ComputeNodeEvalRequest {
                y,
                p,
                t,
                context,
                out,
                scratch: &mut scratch,
                block_label: self.label,
            })?;
        }
        Ok(())
    }

    pub fn eval_node_covering_output_range_with_context(
        &self,
        request: ComputeNodeOutputRangeRequest<'_>,
    ) -> Result<bool, EvalSolveError> {
        let Some(end) = request.start.checked_add(request.len) else {
            return Err(EvalSolveError::ShapeContract {
                message: "prepared compute node output range overflows".to_string(),
                span: None,
            });
        };
        let Some(node) = self
            .nodes
            .iter()
            .find(|node| node.contiguous_output_range_covers(request.start, end))
        else {
            return Ok(false);
        };

        let local_runtime_state;
        let context = match request.context.runtime_state {
            Some(_) => request.context,
            None => {
                local_runtime_state = SimulationRuntimeState::new();
                request.context.with_runtime_state(&local_runtime_state)
            }
        };
        validate_input_requirements(self.requirements, request.y, request.p, context.seed)?;
        request.out.resize(self.len, 0.0);
        record_solve_block_eval(self.label, self.len, request.len);
        let mut scratch = self.scratch.borrow_mut();
        node.eval_into(ComputeNodeEvalRequest {
            y: request.y,
            p: request.p,
            t: request.t,
            context,
            out: request.out,
            scratch: &mut scratch,
            block_label: self.label,
        })?;
        Ok(true)
    }
}

#[derive(Clone)]
enum PreparedComputeNode {
    ScalarPrograms(Box<PreparedScalarProgramBlock>),
    Affine {
        program: PreparedLinearOps,
        scalar_count: usize,
        extents: Vec<usize>,
        ordinal_strides: Vec<usize>,
        output_start: usize,
        output_strides: Vec<i128>,
        load_adjustments: Vec<PreparedAffineLoadAdjustment>,
        const_adjustments: Vec<PreparedAffineConstAdjustment>,
        contiguous_output_range: Option<(usize, usize)>,
        span: rumoca_core::Span,
        requirements: RowInputRequirements,
    },
    MatMul {
        setup: PreparedLinearOps,
        lhs_start: u32,
        rhs_start: u32,
        output_start: usize,
        lhs_len: usize,
        rhs_len: usize,
        output_len: usize,
        m: usize,
        k: usize,
        n: usize,
        kernel: MatMulKernel,
    },
    LinSolve {
        setup: PreparedLinearOps,
        matrix_start: u32,
        rhs_start: u32,
        output_start: usize,
        matrix_len: usize,
        n: usize,
        kernel: LinearSolveKernel,
        span: rumoca_core::Span,
    },
}

#[derive(Clone)]
struct PreparedAffineLoadAdjustment {
    op_position: usize,
    strides: Vec<i128>,
}

#[derive(Clone)]
struct PreparedAffineConstAdjustment {
    op_position: usize,
    strides: Vec<f64>,
}

struct ComputeNodeEvalRequest<'a> {
    y: &'a [f64],
    p: &'a [f64],
    t: f64,
    context: RowEvalContext<'a>,
    out: &'a mut [f64],
    scratch: &'a mut RowEvalScratch,
    block_label: &'static str,
}

struct PreparedMatMulInput<'a> {
    lhs_ops: &'a [LinearOp],
    lhs_start: u32,
    rhs_ops: &'a [LinearOp],
    rhs_start: u32,
    m: usize,
    k: usize,
    n: usize,
    lhs_pattern: &'a StructuralPattern,
    rhs_pattern: &'a StructuralPattern,
    span: rumoca_core::Span,
}

fn prepared_scalar_programs(
    block: &ScalarProgramBlock,
    output_cursor: usize,
) -> Result<(PreparedComputeNode, usize), EvalSolveError> {
    let output_indices =
        scalar_program_output_indices(block, output_cursor, "prepared scalar programs")?;
    let next_output_cursor =
        scalar_program_output_count(block, output_cursor, "prepared scalar programs")?;
    let placed = ScalarProgramBlock::with_output_indices(
        block.programs().to_vec(),
        block.program_spans().to_vec(),
        output_indices,
    )?;
    Ok((
        PreparedComputeNode::ScalarPrograms(Box::new(PreparedScalarProgramBlock::new(placed)?)),
        next_output_cursor,
    ))
}

fn prepared_matmul(
    input: PreparedMatMulInput<'_>,
    output_cursor: usize,
) -> Result<(PreparedComputeNode, usize), EvalSolveError> {
    let PreparedMatMulInput {
        lhs_ops,
        lhs_start,
        rhs_ops,
        rhs_start,
        m,
        k,
        n,
        lhs_pattern,
        rhs_pattern,
        span,
    } = input;
    let setup_op_count = checked_prepared_sum(
        lhs_ops.len(),
        rhs_ops.len(),
        "prepared matmul setup op count",
        Some(span),
    )?;
    let mut setup_ops =
        prepared_vec_with_capacity(setup_op_count, "prepared matmul setup op count", Some(span))?;
    setup_ops.extend_from_slice(lhs_ops);
    setup_ops.extend_from_slice(rhs_ops);
    let lhs_len = checked_product(m, k, "prepared matmul lhs", span)?;
    let rhs_len = checked_product(k, n, "prepared matmul rhs", span)?;
    let output_len = checked_product(m, n, "prepared matmul output", span)?;
    let next_output_cursor =
        checked_contiguous_output_count(output_cursor, output_len, "prepared matmul output", span)?;
    let kernel = select_matmul_kernel(m, k, n, lhs_pattern, rhs_pattern).map_err(|err| {
        EvalSolveError::ShapeContract {
            message: format!("prepared MatMul tensor policy failed: {err}"),
            span: Some(span),
        }
    })?;
    Ok((
        PreparedComputeNode::MatMul {
            setup: PreparedLinearOps::new(setup_ops)?,
            lhs_start,
            rhs_start,
            output_start: output_cursor,
            lhs_len,
            rhs_len,
            output_len,
            m,
            k,
            n,
            kernel,
        },
        next_output_cursor,
    ))
}

fn prepared_linsolve(
    setup_ops: &[LinearOp],
    matrix_start: u32,
    rhs_start: u32,
    n: usize,
    matrix_pattern: &StructuralPattern,
    span: rumoca_core::Span,
    output_cursor: usize,
) -> Result<(PreparedComputeNode, usize), EvalSolveError> {
    let matrix_len = checked_product(n, n, "prepared linsolve matrix", span)?;
    let next_output_cursor =
        checked_contiguous_output_count(output_cursor, n, "prepared linsolve output", span)?;
    let kernel = select_linear_solve_kernel(n, matrix_pattern).map_err(|error| {
        EvalSolveError::ShapeContract {
            message: format!("prepared LinSolve policy failed: {error}"),
            span: Some(span),
        }
    })?;
    Ok((
        PreparedComputeNode::LinSolve {
            setup: PreparedLinearOps::new(setup_ops.to_vec())?,
            matrix_start,
            rhs_start,
            output_start: output_cursor,
            matrix_len,
            n,
            kernel,
            span,
        },
        next_output_cursor,
    ))
}

fn prepared_affine(
    domain: &StructuredIndexDomain,
    output_map: &TensorOutputMap,
    base_ops: &[LinearOp],
    load_strides: &[AffineStencilLoadStride],
    const_strides: &[AffineStencilConstStride],
    span: rumoca_core::Span,
    output_cursor: usize,
) -> Result<(PreparedComputeNode, usize), EvalSolveError> {
    validate_affine_stride_metadata(
        domain,
        base_ops,
        load_strides,
        const_strides,
        "prepared affine",
        span,
    )?;
    let scalar_count = prepared_domain_scalar_count(domain, span)?;
    let extents = prepared_domain_extents(domain, span)?;
    let ordinal_strides = prepared_domain_ordinal_strides(domain, span)?;
    let output_count = tensor_output_count(domain, output_map, "prepared affine", span)?;
    let next_output_cursor = output_cursor.max(output_count);
    let output_strides = prepared_output_strides(output_map, domain.binders.len(), span)?;
    let load_adjustments =
        prepared_load_adjustments(load_strides, base_ops.len(), domain.binders.len(), span)?;
    let const_adjustments =
        prepared_const_adjustments(const_strides, base_ops.len(), domain.binders.len(), span)?;
    let requirements = if scalar_count == 0 {
        RowInputRequirements::default()
    } else {
        prepared_affine_requirements(base_ops, &load_adjustments, &extents, span)?
    };
    let contiguous_output_range = prepared_affine_contiguous_output_range(
        output_map.start,
        scalar_count,
        &extents,
        &ordinal_strides,
        &output_strides,
    );
    Ok((
        PreparedComputeNode::Affine {
            program: PreparedLinearOps::new_with_requirements(base_ops.to_vec(), requirements)?,
            scalar_count,
            extents,
            ordinal_strides,
            output_start: output_map.start,
            output_strides,
            load_adjustments,
            const_adjustments,
            contiguous_output_range,
            span,
            requirements,
        },
        next_output_cursor,
    ))
}

fn prepared_domain_extents(
    domain: &StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, EvalSolveError> {
    domain
        .extents()
        .map_err(|err| prepared_domain_error(err, span))
}

fn prepared_domain_ordinal_strides(
    domain: &StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, EvalSolveError> {
    domain
        .ordinal_strides()
        .map_err(|err| prepared_domain_error(err, span))
}

fn prepared_domain_error(
    error: rumoca_core::StructuredIndexDomainError,
    span: rumoca_core::Span,
) -> EvalSolveError {
    EvalSolveError::ShapeContract {
        message: format!("prepared affine structured index domain is invalid: {error}"),
        span: Some(span),
    }
}

fn prepared_output_strides(
    output_map: &TensorOutputMap,
    rank: usize,
    span: rumoca_core::Span,
) -> Result<Vec<i128>, EvalSolveError> {
    let mut strides = vec![0i128; rank];
    for term in &output_map.strides {
        let Some(stride) = strides.get_mut(term.dimension) else {
            return Err(prepared_affine_dimension_error(
                "output",
                term.dimension,
                rank,
                span,
            ));
        };
        *stride = stride.checked_add(term.stride as i128).ok_or_else(|| {
            prepared_affine_arithmetic_error("output stride accumulation overflows", span)
        })?;
    }
    Ok(strides)
}

fn prepared_load_adjustments(
    load_strides: &[AffineStencilLoadStride],
    op_count: usize,
    rank: usize,
    span: rumoca_core::Span,
) -> Result<Vec<PreparedAffineLoadAdjustment>, EvalSolveError> {
    let mut by_op = vec![None::<Vec<i128>>; op_count];
    for load_stride in load_strides {
        let Some(strides) = by_op.get_mut(load_stride.op_position) else {
            return Err(prepared_affine_op_error(
                "load",
                load_stride.op_position,
                op_count,
                span,
            ));
        };
        let strides = strides.get_or_insert_with(|| vec![0i128; rank]);
        for term in &load_stride.terms {
            let Some(stride) = strides.get_mut(term.dimension) else {
                return Err(prepared_affine_dimension_error(
                    "load",
                    term.dimension,
                    rank,
                    span,
                ));
            };
            *stride = stride.checked_add(term.stride as i128).ok_or_else(|| {
                prepared_affine_arithmetic_error("load stride accumulation overflows", span)
            })?;
        }
    }
    Ok(by_op
        .into_iter()
        .enumerate()
        .filter_map(|(op_position, strides)| {
            strides.map(|strides| PreparedAffineLoadAdjustment {
                op_position,
                strides,
            })
        })
        .collect())
}

fn prepared_const_adjustments(
    const_strides: &[AffineStencilConstStride],
    op_count: usize,
    rank: usize,
    span: rumoca_core::Span,
) -> Result<Vec<PreparedAffineConstAdjustment>, EvalSolveError> {
    let mut by_op = vec![None::<Vec<f64>>; op_count];
    for const_stride in const_strides {
        let Some(strides) = by_op.get_mut(const_stride.op_position) else {
            return Err(prepared_affine_op_error(
                "constant",
                const_stride.op_position,
                op_count,
                span,
            ));
        };
        let strides = strides.get_or_insert_with(|| vec![0.0; rank]);
        for term in &const_stride.terms {
            let Some(stride) = strides.get_mut(term.dimension) else {
                return Err(prepared_affine_dimension_error(
                    "constant",
                    term.dimension,
                    rank,
                    span,
                ));
            };
            *stride += term.stride;
            if !stride.is_finite() {
                return Err(prepared_affine_arithmetic_error(
                    "constant stride accumulation is non-finite",
                    span,
                ));
            }
        }
    }
    Ok(by_op
        .into_iter()
        .enumerate()
        .filter_map(|(op_position, strides)| {
            strides.map(|strides| PreparedAffineConstAdjustment {
                op_position,
                strides,
            })
        })
        .collect())
}

fn prepared_affine_requirements(
    base_ops: &[LinearOp],
    adjustments: &[PreparedAffineLoadAdjustment],
    extents: &[usize],
    span: rumoca_core::Span,
) -> Result<RowInputRequirements, EvalSolveError> {
    let mut requirements = row_input_requirements(base_ops)?;
    for adjustment in adjustments {
        let Some(op) = base_ops.get(adjustment.op_position) else {
            return Err(prepared_affine_op_error(
                "load",
                adjustment.op_position,
                base_ops.len(),
                span,
            ));
        };
        let (requirements_len, base_index) = match *op {
            LinearOp::LoadY { index, .. } => (&mut requirements.y_len, index),
            LinearOp::LoadP { index, .. } => (&mut requirements.p_len, index),
            LinearOp::LoadSeed { index, .. } => (&mut requirements.seed_len, index),
            _ => {
                return Err(prepared_affine_arithmetic_error(
                    "load adjustment does not target LoadY, LoadP, or LoadSeed",
                    span,
                ));
            }
        };
        let (_, maximum) =
            prepared_affine_index_bounds(base_index, &adjustment.strides, extents, span)?;
        let required = maximum.checked_add(1).ok_or_else(|| {
            prepared_affine_arithmetic_error("affine input requirement overflows", span)
        })?;
        *requirements_len = (*requirements_len).max(required);
    }
    Ok(requirements)
}

fn prepared_affine_index_bounds(
    base_index: usize,
    strides: &[i128],
    extents: &[usize],
    span: rumoca_core::Span,
) -> Result<(usize, usize), EvalSolveError> {
    let start = i128::try_from(base_index)
        .map_err(|_| prepared_affine_arithmetic_error("base input index overflows", span))?;
    let mut minimum = start;
    let mut maximum = start;
    for (stride, extent) in strides.iter().copied().zip(extents.iter().copied()) {
        let last_position = i128::try_from(extent.saturating_sub(1))
            .map_err(|_| prepared_affine_arithmetic_error("domain extent overflows", span))?;
        let offset = last_position
            .checked_mul(stride)
            .ok_or_else(|| prepared_affine_arithmetic_error("input stride overflows", span))?;
        if offset < 0 {
            minimum = minimum.checked_add(offset).ok_or_else(|| {
                prepared_affine_arithmetic_error("minimum input index overflows", span)
            })?;
        } else {
            maximum = maximum.checked_add(offset).ok_or_else(|| {
                prepared_affine_arithmetic_error("maximum input index overflows", span)
            })?;
        }
    }
    if minimum < 0 {
        return Err(EvalSolveError::Scalarization {
            message: format!("prepared affine output produced negative load index {minimum}"),
            span: Some(span),
        });
    }
    let minimum = usize::try_from(minimum)
        .map_err(|_| prepared_affine_arithmetic_error("minimum input index overflows", span))?;
    let maximum = usize::try_from(maximum)
        .map_err(|_| prepared_affine_arithmetic_error("maximum input index overflows", span))?;
    Ok((minimum, maximum))
}

fn prepared_affine_contiguous_output_range(
    output_start: usize,
    scalar_count: usize,
    extents: &[usize],
    ordinal_strides: &[usize],
    output_strides: &[i128],
) -> Option<(usize, usize)> {
    if scalar_count == 0 {
        return None;
    }
    let dense = extents
        .iter()
        .copied()
        .zip(ordinal_strides.iter().copied())
        .zip(output_strides.iter().copied())
        .all(|((extent, ordinal_stride), output_stride)| {
            extent <= 1 || i128::try_from(ordinal_stride) == Ok(output_stride)
        });
    dense.then_some((output_start, scalar_count))
}

fn prepared_affine_dimension_error(
    kind: &'static str,
    dimension: usize,
    rank: usize,
    span: rumoca_core::Span,
) -> EvalSolveError {
    EvalSolveError::ShapeContract {
        message: format!(
            "prepared affine {kind} stride dimension {dimension} is outside domain rank {rank}"
        ),
        span: Some(span),
    }
}

fn prepared_affine_op_error(
    kind: &'static str,
    op_position: usize,
    op_count: usize,
    span: rumoca_core::Span,
) -> EvalSolveError {
    EvalSolveError::ShapeContract {
        message: format!(
            "prepared affine {kind} stride operation {op_position} is outside {op_count} operations"
        ),
        span: Some(span),
    }
}

fn prepared_affine_arithmetic_error(
    message: &'static str,
    span: rumoca_core::Span,
) -> EvalSolveError {
    EvalSolveError::ShapeContract {
        message: format!("prepared affine {message}"),
        span: Some(span),
    }
}

impl PreparedComputeNode {
    fn new_at_output_cursor(
        node: &ComputeNode,
        output_cursor: usize,
    ) -> Result<(Self, usize), EvalSolveError> {
        Ok(match node {
            ComputeNode::ScalarPrograms(block) => prepared_scalar_programs(block, output_cursor)?,
            ComputeNode::MatMul {
                lhs_ops,
                lhs_start,
                rhs_ops,
                rhs_start,
                m,
                k,
                n,
                lhs_pattern,
                rhs_pattern,
                span,
                ..
            } => prepared_matmul(
                PreparedMatMulInput {
                    lhs_ops,
                    lhs_start: *lhs_start,
                    rhs_ops,
                    rhs_start: *rhs_start,
                    m: *m,
                    k: *k,
                    n: *n,
                    lhs_pattern,
                    rhs_pattern,
                    span: *span,
                },
                output_cursor,
            )?,
            ComputeNode::LinSolve {
                setup_ops,
                matrix_start,
                rhs_start,
                n,
                matrix_pattern,
                span,
                ..
            } => prepared_linsolve(
                setup_ops,
                *matrix_start,
                *rhs_start,
                *n,
                matrix_pattern,
                *span,
                output_cursor,
            )?,
            ComputeNode::Map {
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                span,
                ..
            }
            | ComputeNode::AffineStencil {
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                span,
                ..
            } => prepared_affine(
                domain,
                output_map,
                base_ops,
                load_strides,
                const_strides,
                *span,
                output_cursor,
            )?,
        })
    }

    fn requirements(&self) -> RowInputRequirements {
        match self {
            Self::ScalarPrograms(block) => block.requirements(),
            Self::Affine { requirements, .. } => *requirements,
            Self::MatMul { setup, .. } | Self::LinSolve { setup, .. } => setup.requirements,
        }
    }

    fn contiguous_output_range_covers(&self, start: usize, end: usize) -> bool {
        let Some((node_start, node_len)) = self.contiguous_output_range() else {
            return false;
        };
        let Some(node_end) = node_start.checked_add(node_len) else {
            return false;
        };
        start >= node_start && end <= node_end
    }

    fn contiguous_output_range(&self) -> Option<(usize, usize)> {
        match self {
            Self::MatMul {
                output_start,
                output_len,
                ..
            } => Some((*output_start, *output_len)),
            Self::LinSolve {
                output_start, n, ..
            } => Some((*output_start, *n)),
            Self::Affine {
                contiguous_output_range,
                ..
            } => *contiguous_output_range,
            Self::ScalarPrograms(_) => None,
        }
    }

    fn eval_into(&self, request: ComputeNodeEvalRequest<'_>) -> Result<(), EvalSolveError> {
        let ComputeNodeEvalRequest {
            y,
            p,
            t,
            context,
            out,
            scratch,
            block_label,
        } = request;
        match self {
            Self::ScalarPrograms(block) => {
                block.eval_rows_unchecked(y, p, t, context, out, scratch)
            }
            Self::Affine { .. } => eval_prepared_affine_node(self, y, p, t, context, out, scratch),
            Self::MatMul {
                setup,
                lhs_start,
                rhs_start,
                output_start,
                lhs_len,
                rhs_len,
                output_len,
                m,
                k,
                n,
                kernel,
            } => {
                setup.eval(y, p, t, context, scratch)?;
                ensure_register_range(&scratch.regs, "read", *lhs_start, *lhs_len)?;
                ensure_register_range(&scratch.regs, "read", *rhs_start, *rhs_len)?;
                let output_end = output_start.checked_add(*output_len).ok_or_else(|| {
                    invalid_prepared_row("prepared matmul output range overflows")
                })?;
                eval_matmul_with_policy(
                    &scratch.regs,
                    MatMulEvalSpec {
                        lhs_start: *lhs_start as usize,
                        rhs_start: *rhs_start as usize,
                        m: *m,
                        k: *k,
                        n: *n,
                        kernel: *kernel,
                    },
                    &mut out[*output_start..output_end],
                )
            }
            Self::LinSolve {
                setup,
                matrix_start,
                rhs_start,
                output_start,
                matrix_len,
                n,
                kernel,
                span,
            } => {
                setup.eval(y, p, t, context, scratch)?;
                ensure_register_range(&scratch.regs, "read", *matrix_start, *matrix_len)?;
                ensure_register_range(&scratch.regs, "read", *rhs_start, *n)?;
                let output_end = output_start.checked_add(*n).ok_or_else(|| {
                    invalid_prepared_row("prepared linsolve output range overflows")
                })?;
                solve_all_unchecked(
                    &scratch.regs,
                    *matrix_start,
                    *rhs_start,
                    *n,
                    *kernel,
                    &mut out[*output_start..output_end],
                )
                .map_err(|error| {
                    tracing::debug!(
                        target: "rumoca_eval_solve::linsolve",
                        label = block_label,
                        output_start,
                        size = n,
                        matrix = ?&scratch.regs[*matrix_start as usize
                            ..*matrix_start as usize + *matrix_len],
                        rhs = ?&scratch.regs[*rhs_start as usize..*rhs_start as usize + *n],
                        span = ?span,
                        "prepared linear solve failed"
                    );
                    error.with_source_span(Some(*span))
                })
            }
        }
    }
}

mod affine_eval;
use affine_eval::*;

mod support;
use support::*;
