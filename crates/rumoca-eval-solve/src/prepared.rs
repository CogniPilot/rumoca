use std::cell::RefCell;

use rumoca_ir_solve::{ComputeBlock, ComputeNode, LinearOp, ScalarProgramBlock};
use rumoca_solver::{MatMulKernel, select_matmul_kernel};

use crate::{
    EvalSolveError, OutputCursor, PreparedRowEval, RowEvalContext, RowEvalScratch,
    RowInputRequirements, SimulationRuntimeState,
    compute_block_scalarize::scalarize_affine_stencil, eval_program_single,
    eval_row_prepared_maybe_fast, linear_solve::solve_all_unchecked, record_solve_block_eval,
    required_registers, row_input_requirements, row_register_flow_is_valid,
    to_scalar_program_block, validate_input_requirements, validate_output_len,
};

/// Reusable evaluator for one Solve-IR row block.
pub struct PreparedScalarProgramBlock {
    block: ScalarProgramBlock,
    row_registers: Vec<usize>,
    row_requirements: Vec<RowInputRequirements>,
    row_register_safe: Vec<bool>,
    row_assignment_shapes: Vec<Option<TargetAssignmentShape>>,
    requirements: RowInputRequirements,
    scratch: RefCell<RowEvalScratch>,
}

impl Clone for PreparedScalarProgramBlock {
    fn clone(&self) -> Self {
        Self {
            block: self.block.clone(),
            row_registers: self.row_registers.clone(),
            row_requirements: self.row_requirements.clone(),
            row_register_safe: self.row_register_safe.clone(),
            row_assignment_shapes: self.row_assignment_shapes.clone(),
            requirements: self.requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
        }
    }
}

impl PreparedScalarProgramBlock {
    pub fn new(block: ScalarProgramBlock) -> Self {
        let mut row_registers = Vec::with_capacity(block.programs.len());
        let mut row_requirements = Vec::with_capacity(block.programs.len());
        let mut row_register_safe = Vec::with_capacity(block.programs.len());
        let mut row_assignment_shapes = Vec::with_capacity(block.programs.len());
        let mut requirements = RowInputRequirements::default();
        for row in &block.programs {
            let row_requirement = row_input_requirements(row);
            row_registers.push(required_registers(row));
            row_requirements.push(row_requirement);
            row_register_safe.push(row_register_flow_is_valid(row));
            row_assignment_shapes.push(target_assignment_shape(row));
            requirements = requirements.merge(row_requirement);
        }
        Self {
            block,
            row_registers,
            row_requirements,
            row_register_safe,
            row_assignment_shapes,
            requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
        }
    }

    pub fn from_compute_block(block: &ComputeBlock) -> Self {
        Self::new(to_scalar_program_block(block))
    }

    pub fn block(&self) -> &ScalarProgramBlock {
        &self.block
    }

    /// Number of outputs this block produces (one per `StoreOutput`), which a
    /// matmul/linsolve program may exceed its program count for. Consumers size
    /// their output buffers from this.
    pub fn len(&self) -> usize {
        self.block.output_count()
    }

    pub fn is_empty(&self) -> bool {
        self.block.is_empty()
    }

    pub fn requirements(&self) -> RowInputRequirements {
        self.requirements
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
        validate_output_len(out, self.block.output_count())?;
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
        // `rows` counts programs (the prefix to evaluate); the produced output
        // count may exceed it when a program emits several outputs.
        let rows = rows.min(self.block.len());
        let prefix = &self.block.programs[..rows];
        let output_count: usize = prefix
            .iter()
            .map(|program| ScalarProgramBlock::program_output_count(program))
            .sum();
        let local_runtime_state;
        let context = match context.runtime_state {
            Some(_) => context,
            None => {
                local_runtime_state = SimulationRuntimeState::new();
                context.with_runtime_state(&local_runtime_state)
            }
        };
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
        record_solve_block_eval("scalar_prefix", self.block.len(), output_count);
        let mut sink = OutputCursor::new(&mut out[..output_count]);
        for (row_idx, row) in prefix.iter().enumerate() {
            eval_row_prepared_maybe_fast(
                PreparedRowEval::new(row, self.row_registers[row_idx], y, p, t, context),
                self.row_register_safe[row_idx],
                &mut scratch,
                &mut sink,
            )?;
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

    fn eval_row_inner(&self, request: RowEvalRequest<'_>) -> Result<f64, EvalSolveError> {
        let row =
            self.block
                .programs
                .get(request.row_idx)
                .ok_or(EvalSolveError::OutputTooSmall {
                    required: request.row_idx.saturating_add(1),
                    len: self.block.len(),
                })?;
        if request.validate_inputs {
            validate_input_requirements(
                self.row_requirements[request.row_idx],
                request.y,
                request.p,
                request.context.seed,
            )?;
        }
        let mut scratch = self.scratch.borrow_mut();
        record_solve_block_eval(request.label, self.block.len(), 1);
        eval_program_single(
            PreparedRowEval::new(
                row,
                self.row_registers[request.row_idx],
                request.y,
                request.p,
                request.t,
                request.context,
            ),
            self.row_register_safe[request.row_idx],
            &mut scratch,
        )
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
            .is_some_and(|shape| shape.is_some())
    }

    pub fn can_evaluate_target_assignment(&self, row_idx: usize, target_y_index: usize) -> bool {
        let Some(row) = self.block.programs.get(row_idx) else {
            return false;
        };
        match self.row_assignment_shapes[row_idx] {
            Some(shape) => shape.target_y_index() == target_y_index,
            None => !row_loads_y_index(row, target_y_index),
        }
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

    fn eval_target_assignment_row_inner(
        &self,
        request: TargetAssignmentRowRequest<'_>,
    ) -> Result<Option<f64>, EvalSolveError> {
        let row =
            self.block
                .programs
                .get(request.row_idx)
                .ok_or(EvalSolveError::OutputTooSmall {
                    required: request.row_idx.saturating_add(1),
                    len: self.block.len(),
                })?;
        if request.validate_inputs {
            validate_input_requirements(
                self.row_requirements[request.row_idx],
                request.y,
                request.p,
                request.context.seed,
            )?;
        }
        let mut scratch = self.scratch.borrow_mut();
        record_solve_block_eval(request.label, self.block.len(), 1);
        let Some(shape) = self.row_assignment_shapes[request.row_idx] else {
            // No assignment shape means the row is an ordinary residual. It is
            // only reusable for a target update when it does not read that same
            // target slot; otherwise the parent receives None and tries another row.
            let output = eval_program_single(
                PreparedRowEval::new(
                    row,
                    self.row_registers[request.row_idx],
                    request.y,
                    request.p,
                    request.t,
                    request.context,
                ),
                self.row_register_safe[request.row_idx],
                &mut scratch,
            )?;
            return Ok((!row_loads_y_index(row, request.target_y_index)).then_some(output));
        };
        if shape.target_y_index() != request.target_y_index {
            return Ok(None);
        }
        eval_program_single(
            PreparedRowEval::new(
                &row[..shape.expr_eval_len()],
                self.row_registers[request.row_idx],
                request.y,
                request.p,
                request.t,
                request.context,
            ),
            self.row_register_safe[request.row_idx],
            &mut scratch,
        )?;
        Ok(Some(shape.eval_value(request.row_idx, &scratch.regs)?))
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
            self.block.len(),
            self.block.output_count(),
        );
        let mut sink = OutputCursor::new(out);
        for (row_idx, row) in self.block.programs.iter().enumerate() {
            eval_row_prepared_maybe_fast(
                PreparedRowEval::new(row, self.row_registers[row_idx], y, p, t, context),
                self.row_register_safe[row_idx],
                scratch,
                &mut sink,
            )?;
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

#[derive(Clone, Copy)]
enum TargetAssignmentShape {
    Direct {
        target_y_index: usize,
        expr_reg: u32,
        expr_eval_len: usize,
    },
    Affine {
        target_y_index: usize,
        offset_reg: u32,
        coefficient_reg: Option<u32>,
        offset_scale: f64,
        coefficient_scale: f64,
        expr_eval_len: usize,
    },
}

impl TargetAssignmentShape {
    fn target_y_index(self) -> usize {
        match self {
            Self::Direct { target_y_index, .. } | Self::Affine { target_y_index, .. } => {
                target_y_index
            }
        }
    }

    fn expr_eval_len(self) -> usize {
        match self {
            Self::Direct { expr_eval_len, .. } | Self::Affine { expr_eval_len, .. } => {
                expr_eval_len
            }
        }
    }

    fn eval_value(self, row_idx: usize, regs: &[f64]) -> Result<f64, EvalSolveError> {
        match self {
            Self::Direct { expr_reg, .. } => read_shape_reg(regs, expr_reg),
            Self::Affine {
                target_y_index,
                offset_reg,
                coefficient_reg,
                offset_scale,
                coefficient_scale,
                ..
            } => {
                let offset = offset_scale * read_shape_reg(regs, offset_reg)?;
                let coefficient = coefficient_scale
                    * coefficient_reg.map_or(Ok(1.0), |reg| read_shape_reg(regs, reg))?;
                if coefficient == 0.0 || !coefficient.is_finite() {
                    return Err(EvalSolveError::SingularTargetAssignment {
                        row: row_idx,
                        target_y_index,
                        coefficient,
                    });
                }
                Ok(-offset / coefficient)
            }
        }
    }
}

fn target_assignment_shape(row: &[LinearOp]) -> Option<TargetAssignmentShape> {
    // Affine/direct target back-solving is only meaningful for single-output
    // residual programs. Multi-output programs (matmul/linsolve) have no single
    // target slot, so they are never reusable for target assignment.
    if ScalarProgramBlock::program_output_count(row) > 1 {
        return None;
    }
    let output_reg = store_output_reg(row)?;
    direct_assignment_shape(row, output_reg).or_else(|| affine_assignment_shape(row, output_reg))
}

fn read_shape_reg(regs: &[f64], reg: u32) -> Result<f64, EvalSolveError> {
    regs.get(reg as usize)
        .copied()
        .ok_or(EvalSolveError::RegisterOutOfBounds {
            access: "read",
            register: reg,
            len: regs.len(),
        })
}

fn direct_assignment_shape(row: &[LinearOp], output_reg: u32) -> Option<TargetAssignmentShape> {
    let (target_reg, expr_reg) = assignment_expr_reg(row, output_reg)?;
    let target_y_index = target_load_index(row, target_reg)?;
    let expr_eval_len = producer_pos(row, expr_reg)?.saturating_add(1);
    Some(TargetAssignmentShape::Direct {
        target_y_index,
        expr_reg,
        expr_eval_len,
    })
}

fn affine_assignment_shape(row: &[LinearOp], output_reg: u32) -> Option<TargetAssignmentShape> {
    let output_op = producer(row, output_reg)?;
    let (lhs, rhs, lhs_scale, rhs_scale) = match *output_op {
        LinearOp::Binary {
            op: rumoca_ir_solve::BinaryOp::Add,
            lhs,
            rhs,
            ..
        } => (lhs, rhs, 1.0, 1.0),
        LinearOp::Binary {
            op: rumoca_ir_solve::BinaryOp::Sub,
            lhs,
            rhs,
            ..
        } => (lhs, rhs, 1.0, -1.0),
        _ => return None,
    };
    affine_sum_shape(row, lhs, rhs, lhs_scale, rhs_scale)
}

fn affine_sum_shape(
    row: &[LinearOp],
    lhs: u32,
    rhs: u32,
    lhs_scale: f64,
    rhs_scale: f64,
) -> Option<TargetAssignmentShape> {
    if let Some((target_reg, coefficient_reg)) = affine_target_term(row, lhs) {
        return affine_sum_side_shape(row, target_reg, coefficient_reg, lhs_scale, rhs, rhs_scale);
    }
    let (target_reg, coefficient_reg) = affine_target_term(row, rhs)?;
    affine_sum_side_shape(row, target_reg, coefficient_reg, rhs_scale, lhs, lhs_scale)
}

fn affine_sum_side_shape(
    row: &[LinearOp],
    target_reg: u32,
    coefficient_reg: Option<u32>,
    coefficient_scale: f64,
    offset_reg: u32,
    offset_scale: f64,
) -> Option<TargetAssignmentShape> {
    let target_y_index = target_load_index(row, target_reg)?;
    if coefficient_reg.is_some_and(|reg| reg_depends_on_y_index(row, reg, target_y_index))
        || reg_depends_on_y_index(row, offset_reg, target_y_index)
    {
        return None;
    }
    let coefficient_pos = coefficient_reg
        .and_then(|reg| producer_pos(row, reg))
        .unwrap_or(0);
    let expr_eval_len = coefficient_pos
        .max(producer_pos(row, offset_reg)?)
        .saturating_add(1);
    Some(TargetAssignmentShape::Affine {
        target_y_index,
        offset_reg,
        coefficient_reg,
        offset_scale,
        coefficient_scale,
        expr_eval_len,
    })
}

fn affine_target_term(row: &[LinearOp], reg: u32) -> Option<(u32, Option<u32>)> {
    if is_y_load(row, reg) {
        return Some((reg, None));
    }
    let LinearOp::Binary {
        op: rumoca_ir_solve::BinaryOp::Mul,
        lhs,
        rhs,
        ..
    } = *producer(row, reg)?
    else {
        return None;
    };
    match (is_y_load(row, lhs), is_y_load(row, rhs)) {
        (true, false) => Some((lhs, Some(rhs))),
        (false, true) => Some((rhs, Some(lhs))),
        _ => None,
    }
}

fn store_output_reg(row: &[LinearOp]) -> Option<u32> {
    row.iter().rev().find_map(|op| match *op {
        LinearOp::StoreOutput { src } => Some(src),
        _ => None,
    })
}

fn target_load_index(row: &[LinearOp], target_reg: u32) -> Option<usize> {
    row.iter().find_map(|op| match *op {
        LinearOp::LoadY { dst, index } if dst == target_reg => Some(index),
        _ => None,
    })
}

fn row_loads_y_index(row: &[LinearOp], target_y_index: usize) -> bool {
    row.iter().any(|op| {
        matches!(
            *op,
            LinearOp::LoadY { index, .. } if index == target_y_index
        )
    })
}

fn reg_depends_on_y_index(row: &[LinearOp], reg: u32, target_y_index: usize) -> bool {
    // Register programs form a DAG: a register computed once can feed many
    // downstream ops, so the naive recursion below re-traverses shared
    // sub-expressions exponentially (a 1700-op matrix-product row never
    // finishes). Memoize by register — dependence on a fixed `y` index is a
    // pure function of the register — to make the walk linear in the row.
    let mut memo: std::collections::HashMap<u32, bool> = std::collections::HashMap::new();
    reg_depends_on_y_index_memo(row, reg, target_y_index, &mut memo)
}

fn reg_depends_on_y_index_memo(
    row: &[LinearOp],
    reg: u32,
    target_y_index: usize,
    memo: &mut std::collections::HashMap<u32, bool>,
) -> bool {
    if let Some(&cached) = memo.get(&reg) {
        return cached;
    }
    // Guard against accidental cycles (register programs are acyclic in
    // practice): seed `false` before recursing so a back-edge terminates.
    memo.insert(reg, false);
    let result = producer(row, reg).is_some_and(|op| match *op {
        LinearOp::LoadY { index, .. } => index == target_y_index,
        // Indexed loads structurally depend on y exactly when their index
        // register does — preserving the sparsity the equivalent select chain
        // (whose `cond` carried that dependency) would have produced.
        LinearOp::Move { src, .. }
        | LinearOp::Unary { arg: src, .. }
        | LinearOp::LoadIndexedP { index: src, .. }
        | LinearOp::LoadIndexedSeed { index: src, .. } => {
            reg_depends_on_y_index_memo(row, src, target_y_index, memo)
        }
        LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
            reg_depends_on_y_index_memo(row, lhs, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, rhs, target_y_index, memo)
        }
        LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => {
            reg_depends_on_y_index_memo(row, cond, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, if_true, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, if_false, target_y_index, memo)
        }
        LinearOp::LinearSolveComponent {
            matrix_start,
            rhs_start,
            n,
            ..
        } => {
            reg_range_depends_on_y_index(row, matrix_start, n * n, target_y_index, memo)
                || reg_range_depends_on_y_index(row, rhs_start, n, target_y_index, memo)
        }
        LinearOp::TableBounds { table_id, .. } => {
            reg_depends_on_y_index_memo(row, table_id, target_y_index, memo)
        }
        LinearOp::TableLookup {
            table_id,
            column,
            input,
            ..
        }
        | LinearOp::TableLookupSlope {
            table_id,
            column,
            input,
            ..
        } => {
            reg_depends_on_y_index_memo(row, table_id, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, column, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, input, target_y_index, memo)
        }
        LinearOp::TableNextEvent { table_id, time, .. } => {
            reg_depends_on_y_index_memo(row, table_id, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, time, target_y_index, memo)
        }
        LinearOp::RandomInitialState {
            local_seed,
            global_seed,
            ..
        } => {
            reg_depends_on_y_index_memo(row, local_seed, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, global_seed, target_y_index, memo)
        }
        LinearOp::RandomResult {
            state_start,
            state_len,
            ..
        }
        | LinearOp::RandomState {
            state_start,
            state_len,
            ..
        } => reg_range_depends_on_y_index(row, state_start, state_len, target_y_index, memo),
        LinearOp::ImpureRandomInit { seed, .. } => {
            reg_depends_on_y_index_memo(row, seed, target_y_index, memo)
        }
        LinearOp::ImpureRandom { id, .. } => {
            reg_depends_on_y_index_memo(row, id, target_y_index, memo)
        }
        LinearOp::ImpureRandomInteger { id, imin, imax, .. } => {
            reg_depends_on_y_index_memo(row, id, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, imin, target_y_index, memo)
                || reg_depends_on_y_index_memo(row, imax, target_y_index, memo)
        }
        LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::LoadSeed { .. }
        | LinearOp::StoreOutput { .. } => false,
    });
    memo.insert(reg, result);
    result
}

fn reg_range_depends_on_y_index(
    row: &[LinearOp],
    start: u32,
    len: usize,
    target_y_index: usize,
    memo: &mut std::collections::HashMap<u32, bool>,
) -> bool {
    (0..len).any(|offset| {
        reg_depends_on_y_index_memo(
            row,
            start.saturating_add(u32::try_from(offset).unwrap_or(u32::MAX)),
            target_y_index,
            memo,
        )
    })
}

fn assignment_expr_reg(row: &[LinearOp], output_reg: u32) -> Option<(u32, u32)> {
    let output_op = producer(row, output_reg)?;
    match *output_op {
        LinearOp::Binary {
            op: rumoca_ir_solve::BinaryOp::Sub,
            lhs,
            rhs,
            ..
        } => sub_assignment_expr_reg(row, lhs, rhs),
        LinearOp::Unary {
            op: rumoca_ir_solve::UnaryOp::Neg,
            arg,
            ..
        } => {
            let inner = producer(row, arg)?;
            let LinearOp::Binary {
                op: rumoca_ir_solve::BinaryOp::Sub,
                lhs,
                rhs,
                ..
            } = *inner
            else {
                return None;
            };
            sub_assignment_expr_reg(row, lhs, rhs)
        }
        _ => None,
    }
}

fn producer(row: &[LinearOp], dst_reg: u32) -> Option<&LinearOp> {
    row.iter()
        .rev()
        .find(|op| op.dst_register() == Some(dst_reg))
}

fn producer_pos(row: &[LinearOp], dst_reg: u32) -> Option<usize> {
    row.iter()
        .rposition(|op| op.dst_register() == Some(dst_reg))
}

fn sub_assignment_expr_reg(row: &[LinearOp], lhs: u32, rhs: u32) -> Option<(u32, u32)> {
    if is_y_load(row, lhs) {
        Some((lhs, rhs))
    } else if is_y_load(row, rhs) {
        Some((rhs, lhs))
    } else {
        None
    }
}

fn is_y_load(row: &[LinearOp], reg: u32) -> bool {
    matches!(producer(row, reg), Some(LinearOp::LoadY { .. }))
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
    pub fn new(block: &ComputeBlock) -> Self {
        Self::new_with_label(block, "compute_block")
    }

    pub fn new_with_label(block: &ComputeBlock, label: &'static str) -> Self {
        let mut len = 0;
        let mut requirements = RowInputRequirements::default();
        let nodes = block
            .nodes
            .iter()
            .map(|node| {
                let prepared = PreparedComputeNode::new(node);
                len += prepared.len();
                requirements = requirements.merge(prepared.requirements());
                prepared
            })
            .collect();
        Self {
            label,
            nodes,
            len,
            requirements,
            scratch: RefCell::new(RowEvalScratch::default()),
        }
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
        let mut offset = 0;
        for node in &self.nodes {
            let node_len = node.len();
            node.eval_into(
                y,
                p,
                t,
                context,
                &mut out[offset..offset + node_len],
                &mut scratch,
            )?;
            offset += node_len;
        }
        Ok(())
    }
}

#[derive(Clone)]
enum PreparedComputeNode {
    ScalarPrograms(PreparedScalarProgramBlock),
    MatMul {
        setup: PreparedLinearOps,
        lhs_start: u32,
        rhs_start: u32,
        m: usize,
        k: usize,
        n: usize,
        kernel: MatMulKernel,
    },
    LinSolve {
        setup: PreparedLinearOps,
        matrix_start: u32,
        rhs_start: u32,
        n: usize,
    },
}

impl PreparedComputeNode {
    fn new(node: &ComputeNode) -> Self {
        match node {
            ComputeNode::ScalarPrograms(block) => {
                Self::ScalarPrograms(PreparedScalarProgramBlock::new(block.clone()))
            }
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
                ..
            } => {
                let mut setup_ops = Vec::with_capacity(lhs_ops.len() + rhs_ops.len());
                setup_ops.extend_from_slice(lhs_ops);
                setup_ops.extend_from_slice(rhs_ops);
                Self::MatMul {
                    setup: PreparedLinearOps::new(setup_ops),
                    lhs_start: *lhs_start,
                    rhs_start: *rhs_start,
                    m: *m,
                    k: *k,
                    n: *n,
                    kernel: select_matmul_kernel(*m, *k, *n, lhs_sparsity, rhs_sparsity),
                }
            }
            ComputeNode::LinSolve {
                setup_ops,
                matrix_start,
                rhs_start,
                n,
                ..
            } => Self::LinSolve {
                setup: PreparedLinearOps::new(setup_ops.clone()),
                matrix_start: *matrix_start,
                rhs_start: *rhs_start,
                n: *n,
            },
            ComputeNode::AffineStencil {
                count,
                base_ops,
                load_strides,
                const_strides,
                span,
                ..
            } => Self::ScalarPrograms(PreparedScalarProgramBlock::new(
                ScalarProgramBlock::with_program_spans(
                    scalarize_affine_stencil(base_ops, load_strides, const_strides, *count),
                    std::iter::repeat_n(*span, *count).collect(),
                ),
            )),
        }
    }

    fn len(&self) -> usize {
        match self {
            Self::ScalarPrograms(block) => block.len(),
            Self::MatMul { m, n, .. } => m * n,
            Self::LinSolve { n, .. } => *n,
        }
    }

    fn requirements(&self) -> RowInputRequirements {
        match self {
            Self::ScalarPrograms(block) => block.requirements(),
            Self::MatMul { setup, .. } | Self::LinSolve { setup, .. } => setup.requirements,
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
                m,
                k,
                n,
                kernel,
            } => {
                setup.eval(y, p, t, context, scratch)?;
                ensure_register_range(&scratch.regs, "read", *lhs_start, m.saturating_mul(*k))?;
                ensure_register_range(&scratch.regs, "read", *rhs_start, k.saturating_mul(*n))?;
                eval_matmul_with_policy(
                    &scratch.regs,
                    *lhs_start as usize,
                    *rhs_start as usize,
                    *m,
                    *k,
                    *n,
                    *kernel,
                    out,
                )
            }
            Self::LinSolve {
                setup,
                matrix_start,
                rhs_start,
                n,
            } => {
                setup.eval(y, p, t, context, scratch)?;
                ensure_register_range(&scratch.regs, "read", *matrix_start, n.saturating_mul(*n))?;
                ensure_register_range(&scratch.regs, "read", *rhs_start, *n)?;
                solve_all_unchecked(&scratch.regs, *matrix_start, *rhs_start, *n, out);
                Ok(())
            }
        }
    }
}

#[derive(Clone)]
struct PreparedLinearOps {
    ops: Vec<LinearOp>,
    register_count: usize,
    register_safe: bool,
    requirements: RowInputRequirements,
}

impl PreparedLinearOps {
    fn new(ops: Vec<LinearOp>) -> Self {
        Self {
            register_count: required_registers(&ops),
            register_safe: row_register_flow_is_valid(&ops),
            requirements: row_input_requirements(&ops),
            ops,
        }
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
    validate_output_len(out, m.saturating_mul(n))?;
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
        register: start.saturating_add(len.saturating_sub(1) as u32),
        len: regs.len(),
    })
}

#[cfg(test)]
mod assignment_shape_tests {
    use super::*;
    use rumoca_ir_solve::{BinaryOp, LinearOp};

    // Regression: `reg_depends_on_y_index` used to recurse over the register DAG
    // without memoization, so a row whose affine coefficient/offset is a deeply
    // shared sub-expression (typical of inlined matrix products) took O(2^depth)
    // and hung `PreparedScalarProgramBlock::new`. A 40-deep doubling chain has
    // 2^40 distinct root-to-leaf paths; the memoized walk must still finish
    // instantly and classify the row correctly.
    #[test]
    fn affine_shape_with_deep_shared_dag_terminates() {
        let depth: u32 = 40;
        let mut ops = vec![LinearOp::Const { dst: 0, value: 1.0 }];
        // reg i = reg(i-1) + reg(i-1): a register reused twice at every level.
        for i in 1..=depth {
            ops.push(LinearOp::Binary {
                dst: i,
                op: BinaryOp::Add,
                lhs: i - 1,
                rhs: i - 1,
            });
        }
        let deep = depth; // root of the shared DAG (no LoadY inside -> full traversal)
        let y_reg = depth + 1;
        let mul_reg = depth + 2;
        let out_reg = depth + 3;
        // out = (y[7] * deep) + deep  -> affine: coefficient `deep`, offset `deep`.
        ops.push(LinearOp::LoadY {
            dst: y_reg,
            index: 7,
        });
        ops.push(LinearOp::Binary {
            dst: mul_reg,
            op: BinaryOp::Mul,
            lhs: y_reg,
            rhs: deep,
        });
        ops.push(LinearOp::Binary {
            dst: out_reg,
            op: BinaryOp::Add,
            lhs: mul_reg,
            rhs: deep,
        });
        ops.push(LinearOp::StoreOutput { src: out_reg });

        // Would hang pre-fix; must return promptly now.
        let shape = target_assignment_shape(&ops);
        match shape {
            Some(TargetAssignmentShape::Affine { target_y_index, .. }) => {
                assert_eq!(target_y_index, 7);
            }
            _ => panic!("expected Affine shape for y[7]"),
        }

        // And the public preparation path must also complete.
        let _ =
            PreparedScalarProgramBlock::new(rumoca_ir_solve::ScalarProgramBlock::new(vec![ops]));
    }
}
