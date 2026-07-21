mod assignment_shape;
#[cfg(test)]
mod assignment_shape_tests;
mod dependency;
#[cfg(test)]
mod prepared_compute_block_tests;

use std::cell::RefCell;

use rumoca_core::StructuredIndexDomain;
use rumoca_ir_solve::{
    AffineStencilConstStride, AffineStencilLoadStride, ComputeBlock, ComputeNode, LinearOp,
    ScalarProgramBlock, SparsityPattern, TensorOutputMap,
};
use rumoca_solver::{MatMulKernel, select_matmul_kernel};

use crate::refresh_plan::AlgebraicRefreshRow;
use crate::{
    EvalSolveError, OutputCursor, PreparedRowEval, RowEvalContext, RowEvalScratch,
    RowInputRequirements, SimulationRuntimeState,
    compute_block_scalarize::{
        checked_contiguous_output_count, checked_tensor_output_count, scalar_program_output_count,
        scalar_program_output_indices, scalarize_affine_rows, tensor_output_indices,
    },
    eval_program_single, eval_row_prepared_maybe_fast,
    linear_solve::solve_all_unchecked,
    record_solve_block_eval, required_registers, row_input_requirements,
    row_register_flow_is_valid, validate_input_requirements, validate_input_requirements_with_span,
    validate_output_len,
};
#[cfg(test)]
use assignment_shape::checked_expr_eval_len;
use assignment_shape::target_assignment_shapes;
pub use assignment_shape::{TargetAssignmentShape, target_assignment_shape};

/// Reusable evaluator for one Solve-IR row block.
pub struct PreparedScalarProgramBlock {
    block: ScalarProgramBlock,
    output_count: usize,
    row_outputs: Box<PreparedRowOutputMetadata>,
    row_registers: Vec<usize>,
    row_requirements: Vec<RowInputRequirements>,
    row_register_safe: Vec<bool>,
    row_assignment_shapes: Vec<Box<[TargetAssignmentShape]>>,
    requirements: RowInputRequirements,
    scratch: RefCell<RowEvalScratch>,
    row_output_scratch: RefCell<Vec<f64>>,
}

impl Clone for PreparedScalarProgramBlock {
    fn clone(&self) -> Self {
        Self {
            block: self.block.clone(),
            output_count: self.output_count,
            row_outputs: self.row_outputs.clone(),
            row_registers: self.row_registers.clone(),
            row_requirements: self.row_requirements.clone(),
            row_register_safe: self.row_register_safe.clone(),
            row_assignment_shapes: self.row_assignment_shapes.clone(),
            requirements: self.requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
            row_output_scratch: RefCell::new(Vec::new()),
        }
    }
}

impl PreparedScalarProgramBlock {
    pub fn new(block: ScalarProgramBlock) -> Result<Self, EvalSolveError> {
        let row_count = block.programs.len();
        let block_span = block.program_span(0);
        let output_count = checked_prepared_output_count(&block)?;
        let row_outputs = Box::new(prepare_row_output_metadata(&block, output_count)?);
        let mut row_registers =
            prepared_vec_with_capacity(row_count, "prepared row register count", block_span)?;
        let mut row_requirements =
            prepared_vec_with_capacity(row_count, "prepared row requirement count", block_span)?;
        let mut row_register_safe =
            prepared_vec_with_capacity(row_count, "prepared row flow metadata count", block_span)?;
        let mut row_assignment_shapes = prepared_vec_with_capacity(
            row_count,
            "prepared row assignment shape count",
            block_span,
        )?;
        let mut requirements = RowInputRequirements::default();
        for (row_idx, row) in block.programs.iter().enumerate() {
            let span = block.program_span(row_idx);
            let row_requirement =
                row_input_requirements(row).map_err(|error| error.with_source_span(span))?;
            row_registers
                .push(required_registers(row).map_err(|error| error.with_source_span(span))?);
            row_requirements.push(row_requirement);
            row_register_safe.push(
                row_register_flow_is_valid(row).map_err(|error| error.with_source_span(span))?,
            );
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
            row_register_safe,
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

    /// Reverse-mode VJP: accumulate `Jᵀ · output_cotangents` of this block into
    /// `cot` at the `LoadY` / `LoadP` / `LoadSeed` input sites (Track A scalar
    /// reverse core). `scratch` is caller-owned so a hot loop stays
    /// allocation-free. See [`crate::reverse`].
    pub(crate) fn reverse_vjp(
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
        let prefix = &self.block.programs[..rows];
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
            .output_indices
            .get(..stored_output_count)
            .ok_or_else(|| EvalSolveError::ShapeContract {
                message: format!(
                    "prepared prefix has {stored_output_count} stored outputs but only {} output indices",
                    self.block.output_indices.len()
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
                self.row_register_safe[row_idx],
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

    pub(crate) fn eval_single_output_rows_unchecked_with_context(
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
                .programs
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
                self.row_register_safe[row_idx],
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
                .programs
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
            self.row_register_safe[request.row_idx],
            &mut scratch,
        )
        .map_err(|error| error.with_source_span(self.block.program_span(request.row_idx)))
    }

    fn eval_row_output_inner(&self, request: RowOutputRequest<'_>) -> Result<f64, EvalSolveError> {
        let row =
            self.block
                .programs
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
            self.row_register_safe[request.row_idx],
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
            .programs
            .get(row_idx)
            .is_some_and(|row| row_loads_y_index(row, y_index))
    }

    /// True when the row was lowered with an explicit assignment shape
    /// (`target = expr`); its full program then evaluates the residual, while
    /// shapeless rows with an implicit target evaluate the target value.
    pub fn row_has_assignment_shape(&self, row_idx: usize) -> bool {
        self.row_assignment_shapes
            .get(row_idx)
            .is_some_and(|shapes| !shapes.is_empty())
    }

    pub(crate) fn row_output_count(&self, row_idx: usize) -> Option<usize> {
        let start = *self.row_outputs.offsets.get(row_idx)?;
        let end = *self.row_outputs.offsets.get(row_idx.checked_add(1)?)?;
        end.checked_sub(start)
    }

    pub(crate) fn row_output_index(&self, row_idx: usize, output_offset: usize) -> Option<usize> {
        if output_offset >= self.row_output_count(row_idx)? {
            return None;
        }
        let stored_ordinal = self.row_outputs.offsets[row_idx].checked_add(output_offset)?;
        self.block.output_indices.get(stored_ordinal).copied()
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
        let Some(row) = self.block.programs.get(row_idx) else {
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
        let Some(row) = self.block.programs.get(row_idx) else {
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

    pub(crate) fn eval_row_outputs_unchecked_with_context(
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
            .programs
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
            self.row_register_safe[row_idx],
            &mut scratch,
            &mut sink,
        )
        .map_err(|error| error.with_source_span(self.block.program_span(row_idx)))
    }

    pub(crate) fn apply_target_assignment_rows_unchecked_with_context(
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
                .programs
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
                self.row_register_safe[request.row_idx],
                &mut scratch,
            )
            .map_err(|error| error.with_source_span(self.block.program_span(request.row_idx)))?;
            return Ok((!row_loads_y_index(row, request.target_y_index)).then_some(output));
        };
        eval_program_single(
            PreparedRowEval::new(
                &row[..shape.expr_eval_len()],
                self.row_registers[request.row_idx],
                request.y,
                request.p,
                request.t,
                request.context,
            )
            .with_source_span(self.block.program_span(request.row_idx)),
            self.row_register_safe[request.row_idx],
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
                .programs
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
        eval_program_single(
            PreparedRowEval::new(
                &row[..shape.expr_eval_len()],
                self.row_registers[request.row_idx],
                request.y,
                request.p,
                request.t,
                request.context,
            )
            .with_source_span(self.block.program_span(request.row_idx)),
            self.row_register_safe[request.row_idx],
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
        let mut sink = OutputCursor::with_output_indices(out, &self.block.output_indices);
        for (row_idx, row) in self.block.programs.iter().enumerate() {
            eval_row_prepared_maybe_fast(
                PreparedRowEval::new(row, self.row_registers[row_idx], y, p, t, context)
                    .with_source_span(self.block.program_span(row_idx)),
                self.row_register_safe[row_idx],
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

pub(crate) struct ComputeNodeOutputRangeRequest<'a> {
    pub(crate) start: usize,
    pub(crate) len: usize,
    pub(crate) y: &'a [f64],
    pub(crate) p: &'a [f64],
    pub(crate) t: f64,
    pub(crate) context: RowEvalContext<'a>,
    pub(crate) out: &'a mut Vec<f64>,
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
            node.eval_into(y, p, t, context, out, &mut scratch)?;
        }
        Ok(())
    }

    pub(crate) fn eval_node_covering_output_range_with_context(
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
        node.eval_into(
            request.y,
            request.p,
            request.t,
            context,
            request.out,
            &mut scratch,
        )?;
        Ok(true)
    }
}

#[derive(Clone)]
enum PreparedComputeNode {
    ScalarPrograms(PreparedScalarProgramBlock),
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
    },
}

struct PreparedMatMulInput<'a> {
    lhs_ops: &'a [LinearOp],
    lhs_start: u32,
    rhs_ops: &'a [LinearOp],
    rhs_start: u32,
    m: usize,
    k: usize,
    n: usize,
    lhs_sparsity: &'a SparsityPattern,
    rhs_sparsity: &'a SparsityPattern,
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
        block.programs.clone(),
        block.program_spans.clone(),
        output_indices,
    )?;
    Ok((
        PreparedComputeNode::ScalarPrograms(PreparedScalarProgramBlock::new(placed)?),
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
        lhs_sparsity,
        rhs_sparsity,
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
    let kernel = select_matmul_kernel(m, k, n, lhs_sparsity, rhs_sparsity).map_err(|err| {
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
    span: rumoca_core::Span,
    output_cursor: usize,
) -> Result<(PreparedComputeNode, usize), EvalSolveError> {
    let matrix_len = checked_product(n, n, "prepared linsolve matrix", span)?;
    let next_output_cursor =
        checked_contiguous_output_count(output_cursor, n, "prepared linsolve output", span)?;
    Ok((
        PreparedComputeNode::LinSolve {
            setup: PreparedLinearOps::new(setup_ops.to_vec())?,
            matrix_start,
            rhs_start,
            output_start: output_cursor,
            matrix_len,
            n,
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
    let output_indices = tensor_output_indices(domain, output_map, "prepared affine", span)?;
    let next_output_cursor = output_cursor.max(checked_tensor_output_count(
        &output_indices,
        output_cursor,
        "prepared affine",
        span,
    )?);
    let scalar_count = prepared_domain_scalar_count(domain, span)?;
    let mut spans = prepared_vec_with_capacity(
        scalar_count,
        "prepared affine scalar span count",
        Some(span),
    )?;
    spans.extend(std::iter::repeat_n(span, scalar_count));
    let rows = scalarize_affine_rows(domain, base_ops, load_strides, const_strides, span)?;
    let block = ScalarProgramBlock::with_output_indices(rows, spans, output_indices)?;
    Ok((
        PreparedComputeNode::ScalarPrograms(PreparedScalarProgramBlock::new(block)?),
        next_output_cursor,
    ))
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
                lhs_sparsity,
                rhs_sparsity,
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
                    lhs_sparsity,
                    rhs_sparsity,
                    span: *span,
                },
                output_cursor,
            )?,
            ComputeNode::LinSolve {
                setup_ops,
                matrix_start,
                rhs_start,
                n,
                span,
                ..
            } => prepared_linsolve(
                setup_ops,
                *matrix_start,
                *rhs_start,
                *n,
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
            Self::ScalarPrograms(_) => None,
        }
    }

    fn eval_into(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut [f64],
        scratch: &mut RowEvalScratch,
    ) -> Result<(), EvalSolveError> {
        match self {
            Self::ScalarPrograms(block) => {
                block.eval_rows_unchecked(y, p, t, context, out, scratch)
            }
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
                    *lhs_start as usize,
                    *rhs_start as usize,
                    *m,
                    *k,
                    *n,
                    *kernel,
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
                    &mut out[*output_start..output_end],
                )
            }
        }
    }
}

fn prepared_domain_scalar_count(
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<usize, EvalSolveError> {
    domain
        .scalar_count()
        .map_err(|err| EvalSolveError::ShapeContract {
            message: format!("prepared affine structured index domain is invalid: {err}"),
            span: Some(span),
        })
}

#[derive(Clone)]
struct PreparedLinearOps {
    ops: Vec<LinearOp>,
    register_count: usize,
    register_safe: bool,
    requirements: RowInputRequirements,
}

impl PreparedLinearOps {
    fn new(ops: Vec<LinearOp>) -> Result<Self, EvalSolveError> {
        Ok(Self {
            register_count: required_registers(&ops)?,
            register_safe: row_register_flow_is_valid(&ops)?,
            requirements: row_input_requirements(&ops)?,
            ops,
        })
    }

    fn eval(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        scratch: &mut RowEvalScratch,
    ) -> Result<(), EvalSolveError> {
        // Operand setup ops compute matrix/rhs entries into the register file
        // and contain no `StoreOutput`; the matmul/linsolve kernel reads the
        // registers afterward. The single-output helper drives the op loop and
        // its (unused) return value is discarded.
        eval_program_single(
            PreparedRowEval::new(&self.ops, self.register_count, y, p, t, context),
            self.register_safe,
            scratch,
        )?;
        Ok(())
    }
}

// SPEC_0021: Exception - matrix multiply evaluation keeps dimensions and
// register slices explicit to avoid per-row allocation in the hot row evaluator.
#[allow(clippy::too_many_arguments)]
fn eval_matmul_with_policy(
    regs: &[f64],
    lhs_start: usize,
    rhs_start: usize,
    m: usize,
    k: usize,
    n: usize,
    kernel: MatMulKernel,
    out: &mut [f64],
) -> Result<(), EvalSolveError> {
    let output_len = m
        .checked_mul(n)
        .ok_or_else(|| EvalSolveError::Scalarization {
            message: format!("matmul output shape {m}x{n} overflows output vector length"),
            span: None,
        })?;
    validate_output_len(out, output_len)?;
    match kernel {
        MatMulKernel::DiagonalLeft => {
            return eval_left_diagonal_matmul(regs, lhs_start, rhs_start, m, n, out);
        }
        MatMulKernel::DiagonalRight => {
            return eval_right_diagonal_matmul(regs, lhs_start, rhs_start, m, k, out);
        }
        MatMulKernel::SmallDense | MatMulKernel::Dense | MatMulKernel::SparseCandidate => {}
    }
    for row in 0..m {
        for col in 0..n {
            let mut sum = 0.0;
            for inner in 0..k {
                sum += regs[lhs_start + row * k + inner] * regs[rhs_start + inner * n + col];
            }
            out[row * n + col] = sum;
        }
    }
    Ok(())
}

fn checked_product(
    lhs: usize,
    rhs: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, crate::ScalarizeError> {
    lhs.checked_mul(rhs)
        .ok_or(crate::ScalarizeError::ProductOverflow {
            kind,
            lhs,
            rhs,
            span,
        })
}

fn eval_left_diagonal_matmul(
    regs: &[f64],
    lhs_start: usize,
    rhs_start: usize,
    m: usize,
    n: usize,
    out: &mut [f64],
) -> Result<(), EvalSolveError> {
    for row in 0..m {
        let scale = regs[lhs_start + row * m + row];
        for col in 0..n {
            out[row * n + col] = scale * regs[rhs_start + row * n + col];
        }
    }
    Ok(())
}

fn eval_right_diagonal_matmul(
    regs: &[f64],
    lhs_start: usize,
    rhs_start: usize,
    m: usize,
    k: usize,
    out: &mut [f64],
) -> Result<(), EvalSolveError> {
    for row in 0..m {
        for col in 0..k {
            out[row * k + col] = regs[lhs_start + row * k + col] * regs[rhs_start + col * k + col];
        }
    }
    Ok(())
}

fn ensure_register_range(
    regs: &[f64],
    access: &'static str,
    start: u32,
    len: usize,
) -> Result<(), EvalSolveError> {
    let start_index = start as usize;
    if start_index
        .checked_add(len)
        .is_some_and(|end| end <= regs.len())
    {
        return Ok(());
    }
    Err(EvalSolveError::RegisterOutOfBounds {
        access,
        register: checked_register_range_last(start, len)?,
        len: regs.len(),
        span: None,
    })
}

fn checked_required_row_count(row_idx: usize) -> Result<usize, EvalSolveError> {
    row_idx
        .checked_add(1)
        .ok_or_else(|| invalid_prepared_row("row index overflows row count"))
}

fn checked_register_range_last(start: u32, len: usize) -> Result<u32, EvalSolveError> {
    let Some(offset) = len.checked_sub(1) else {
        return Ok(start);
    };
    let offset = u32::try_from(offset).map_err(|_| {
        invalid_prepared_row(format!(
            "register range offset {offset} exceeds register index type"
        ))
    })?;
    start.checked_add(offset).ok_or_else(|| {
        invalid_prepared_row(format!("register range starting at {start} overflows"))
    })
}

fn prepare_row_output_metadata(
    block: &ScalarProgramBlock,
    output_count: usize,
) -> Result<PreparedRowOutputMetadata, EvalSolveError> {
    let span = block.program_span(0);
    let offset_count = checked_prepared_sum(
        block.row_count(),
        1,
        "prepared row output offset count",
        span,
    )?;
    let mut offsets =
        prepared_vec_with_capacity(offset_count, "prepared row output offsets", span)?;
    offsets.push(0usize);
    for row in &block.programs {
        let next = checked_prepared_sum(
            *offsets.last().unwrap_or(&0),
            ScalarProgramBlock::program_output_count(row),
            "prepared row stored output count",
            span,
        )?;
        offsets.push(next);
    }

    if offsets.last().copied() != Some(block.output_indices.len()) {
        return Err(invalid_prepared_row_with_span(
            format!(
                "prepared block has {} stored outputs but {} logical output indices",
                offsets.last().copied().unwrap_or(0),
                block.output_indices.len()
            ),
            span,
        ));
    }
    let mut owners =
        prepared_vec_with_capacity(output_count, "prepared single-output ownership", span)?;
    owners.resize(output_count, PreparedOutputOwner::Unseen);
    for (row_idx, range) in offsets.windows(2).enumerate() {
        let row_output_count = range[1] - range[0];
        for stored_ordinal in range[0]..range[1] {
            let output_index = *block.output_indices.get(stored_ordinal).ok_or_else(|| {
                invalid_prepared_row_with_span(
                    format!(
                        "prepared row output ordinal {stored_ordinal} has no logical output index"
                    ),
                    block.program_span(row_idx),
                )
            })?;
            let owner = owners.get_mut(output_index).ok_or_else(|| {
                invalid_prepared_row_with_span(
                    format!("logical output index {output_index} exceeds prepared output count"),
                    block.program_span(row_idx),
                )
            })?;
            *owner = match (*owner, row_output_count) {
                (PreparedOutputOwner::Unseen, 1) => PreparedOutputOwner::Single(row_idx),
                _ => PreparedOutputOwner::Ambiguous,
            };
        }
    }
    let single_output_rows = owners
        .into_iter()
        .map(|owner| match owner {
            PreparedOutputOwner::Single(row_idx) => Some(row_idx),
            PreparedOutputOwner::Unseen | PreparedOutputOwner::Ambiguous => None,
        })
        .collect();
    Ok(PreparedRowOutputMetadata {
        offsets,
        single_rows: single_output_rows,
    })
}

#[derive(Clone)]
struct PreparedRowOutputMetadata {
    offsets: Vec<usize>,
    single_rows: Vec<Option<usize>>,
}

#[derive(Clone, Copy)]
enum PreparedOutputOwner {
    Unseen,
    Single(usize),
    Ambiguous,
}

fn checked_prepared_output_count(block: &ScalarProgramBlock) -> Result<usize, EvalSolveError> {
    block
        .output_indices
        .iter()
        .copied()
        .max()
        .map_or(Ok(0), |index| {
            checked_prepared_sum(
                index,
                1,
                "prepared logical output count",
                block.program_span(0),
            )
        })
}

fn non_causal_linear_op(op: &LinearOp) -> bool {
    matches!(
        op,
        LinearOp::LoadSeed { .. }
            | LinearOp::LoadIndexedSeed { .. }
            | LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. }
    )
}

fn prepared_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<T>, EvalSolveError> {
    let mut values = Vec::new();
    values.try_reserve_exact(capacity).map_err(|_| {
        invalid_prepared_row_with_span(format!("{context} exceeds host memory limits"), span)
    })?;
    Ok(values)
}

fn reserve_prepared_vec_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    if values.capacity() >= capacity {
        return Ok(());
    }
    values
        .try_reserve_exact(capacity - values.capacity())
        .map_err(|_| {
            invalid_prepared_row_with_span(format!("{context} exceeds host memory limits"), span)
        })
}

fn checked_prepared_sum(
    lhs: usize,
    rhs: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<usize, EvalSolveError> {
    lhs.checked_add(rhs).ok_or_else(|| {
        invalid_prepared_row_with_span(format!("{context} overflows host index range"), span)
    })
}

fn first_compute_node_span(block: &ComputeBlock) -> Option<rumoca_core::Span> {
    block.nodes.iter().find_map(compute_node_span)
}

fn compute_node_span(node: &ComputeNode) -> Option<rumoca_core::Span> {
    match node {
        ComputeNode::ScalarPrograms(block) => block.program_span(0),
        ComputeNode::MatMul { span, .. }
        | ComputeNode::LinSolve { span, .. }
        | ComputeNode::Map { span, .. }
        | ComputeNode::AffineStencil { span, .. } => Some(*span),
    }
}

fn invalid_prepared_row(message: impl Into<String>) -> EvalSolveError {
    invalid_prepared_row_with_span(message, None)
}

fn invalid_prepared_row_with_span(
    message: impl Into<String>,
    span: Option<rumoca_core::Span>,
) -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: message.into(),
        span,
    }
}
