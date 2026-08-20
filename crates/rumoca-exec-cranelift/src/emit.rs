// SPEC_0021 file-size exception: Cranelift emission still keeps builder state,
// intrinsic lowering, and ABI glue together. split plan: move intrinsic
// lowering and memory/ABI helpers into focused emitter submodules.
use super::CompileError;
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::{
    AbiParam, InstBuilder, MemFlags, StackSlot, StackSlotData, StackSlotKind, types,
};
use cranelift_codegen::settings;
use cranelift_codegen::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use rumoca_core::ExternalTableData;
use rumoca_eval_solve::{
    eval_table_bound_value_in, eval_table_lookup_slope_value_in, eval_table_lookup_value_in,
    eval_time_table_next_event_value_in,
};
use rumoca_ir_solve::{
    BinaryOp, CompareOp, LinearOp, ScalarProgramBlock, UnaryOp, resolve_indexed_slot,
};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

mod host_runtime;
mod input_validation;
mod interpreter;
pub(crate) mod typed_program;

use host_runtime::{register_math_symbols, with_active_external_tables};
pub(crate) use input_validation::InputRequirements as EmitInputRequirements;
use input_validation::{
    InputRequirements, input_compile_error, input_requirements_for_linear_ops,
    input_requirements_for_plans, row_input_requirements, validate_input_requirements,
    validate_output_len,
};
use interpreter::execute_row;

// Each compiled program writes its outputs through the trailing `*mut f64`
// pointer (one program may emit several outputs via consecutive StoreOutputs).
type ResidualRowFn = unsafe extern "C" fn(*const f64, *const f64, f64, *mut f64);
type ResidualChunkFn = unsafe extern "C" fn(*const f64, *const f64, f64, *mut f64, *mut f64);
type JacobianRowFn = unsafe extern "C" fn(*const f64, *const f64, f64, *const f64, *mut f64);
type AssignmentScheduleFn = unsafe extern "C" fn(*mut f64, *const f64, f64);

#[derive(Clone)]
enum RowPlan {
    Simple(SimpleRowPlan),
    General(GeneralRowPlan),
}

struct CompiledResidualRow {
    plan: RowPlan,
    validate_with_interpreter: bool,
    interpreter_supported: bool,
}

struct CompiledResidualJit {
    jit: ResidualJit,
    register_count: usize,
    output_start: usize,
    output_count: usize,
}

enum ResidualJit {
    Whole(ResidualRowFn),
    Chunked(Box<[ResidualChunkFn]>),
}

enum LoweredConditionalOutput {
    Value(cranelift_codegen::ir::Value),
    TapeRange {
        pointer: cranelift_codegen::ir::Value,
        start: u32,
        count: usize,
        stride: usize,
    },
}

struct CompiledJacobianRow {
    plan: RowPlan,
    jit: JacobianRowFn,
    validate_with_interpreter: bool,
    interpreter_supported: bool,
}

struct JacobianCallContext<'a> {
    y: &'a [f64],
    p: &'a [f64],
    t: f64,
    v: &'a [f64],
    external_tables: &'a [ExternalTableData],
}

/// The runtime inputs read while interpreting one row: state `y`, parameters
/// `p`, time `t`, the optional directional `seed` (present for Jacobian rows),
/// and the active external tables. Grouped so the interpreter entry points take
/// one input bundle instead of five separate arguments.
#[derive(Clone, Copy)]
struct RowInputs<'a> {
    y: &'a [f64],
    p: &'a [f64],
    t: f64,
    seed: Option<&'a [f64]>,
    external_tables: &'a [ExternalTableData],
}

#[derive(Clone)]
struct SimpleRowPlan {
    ops: Box<[SimpleOp]>,
    reg_count: usize,
    output_srcs: Box<[usize]>,
    input_requirements: InputRequirements,
}

#[derive(Clone)]
struct GeneralRowPlan {
    ops: Box<[LinearOp]>,
    reg_count: usize,
    output_srcs: Box<[usize]>,
    input_requirements: InputRequirements,
}

impl RowPlan {
    /// Number of outputs this program writes (one per StoreOutput).
    fn output_count(&self) -> usize {
        match self {
            RowPlan::Simple(plan) => plan.output_srcs.len(),
            RowPlan::General(plan) => plan.output_srcs.len(),
        }
    }

    fn register_count(&self) -> usize {
        match self {
            RowPlan::Simple(plan) => plan.reg_count,
            RowPlan::General(plan) => plan.reg_count,
        }
    }
}

#[derive(Clone, Copy)]
enum SimpleOp {
    Const {
        dst: u32,
        value: f64,
    },
    LoadTime {
        dst: u32,
    },
    LoadY {
        dst: u32,
        index: u32,
    },
    LoadP {
        dst: u32,
        index: u32,
    },
    Unary {
        dst: u32,
        op: UnaryOp,
        arg: u32,
    },
    Binary {
        dst: u32,
        op: BinaryOp,
        lhs: u32,
        rhs: u32,
    },
    Compare {
        dst: u32,
        op: CompareOp,
        lhs: u32,
        rhs: u32,
    },
    Select {
        dst: u32,
        cond: u32,
        if_true: u32,
        if_false: u32,
    },
}

pub(crate) struct CompiledResidualRows {
    _module: JITModule,
    _pure_calls: Option<Rc<typed_program::CompiledPureCallTable>>,
    rows: Vec<CompiledResidualRow>,
    jits: Vec<CompiledResidualJit>,
    input_requirements: InputRequirements,
    regs_scratch: RefCell<Vec<f64>>,
    jit_call_count: Cell<usize>,
}

pub(crate) struct CompiledAssignmentSchedule {
    _module: JITModule,
    _pure_calls: Option<Rc<typed_program::CompiledPureCallTable>>,
    jit: AssignmentScheduleFn,
    input_requirements: InputRequirements,
    required_y_len: usize,
    rows: usize,
}

impl CompiledAssignmentSchedule {
    pub(crate) fn call(&self, y: &mut [f64], p: &[f64], t: f64) -> Result<(), CompileError> {
        self.call_with_external_tables(y, p, t, &[])
    }

    pub(crate) fn call_with_external_tables(
        &self,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
    ) -> Result<(), CompileError> {
        validate_input_requirements(self.input_requirements, y, p, None)?;
        if y.len() < self.required_y_len {
            return Err(CompileError::Input(format!(
                "assignment schedule requires {} y values, got {}",
                self.required_y_len,
                y.len()
            )));
        }
        with_active_external_tables(external_tables, || {
            // SAFETY: compilation validates every input load and target offset;
            // the checks above establish both against these exact slices.
            unsafe { (self.jit)(y.as_mut_ptr(), p.as_ptr(), t) };
        });
        Ok(())
    }

    pub(crate) fn rows(&self) -> usize {
        self.rows
    }
}

impl CompiledResidualRows {
    #[cfg(test)]
    pub(crate) fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        self.call_with_external_tables(y, p, t, &[], out)
    }

    pub(crate) fn call_with_external_tables(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        let output_count: usize = self.rows.iter().map(|row| row.plan.output_count()).sum();
        validate_output_len(out, output_count)?;
        validate_input_requirements(self.input_requirements, y, p, None)?;
        let inputs = RowInputs {
            y,
            p,
            t,
            seed: None,
            external_tables,
        };
        with_active_external_tables(external_tables, || {
            self.call_active(inputs, output_count, out)
        })
    }

    pub(crate) fn rows(&self) -> usize {
        self.rows.len()
    }

    pub(crate) fn input_requirements(&self) -> InputRequirements {
        self.input_requirements
    }

    fn record_jit_call(&self) {
        self.jit_call_count
            .set(self.jit_call_count.get().saturating_add(1));
    }

    fn call_active(
        &self,
        inputs: RowInputs<'_>,
        output_count: usize,
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        let mut regs_scratch = self.regs_scratch.borrow_mut();
        let expected = self.interpreted_outputs(&mut regs_scratch, inputs, output_count)?;
        for compiled in &self.jits {
            let output =
                &mut out[compiled.output_start..compiled.output_start + compiled.output_count];
            // SAFETY: compile-time row validation proves the register and output
            // extents used by this finalized function.
            unsafe {
                call_residual_jit(
                    &compiled.jit,
                    compiled.register_count,
                    &mut regs_scratch,
                    inputs.y,
                    inputs.p,
                    inputs.t,
                    output,
                )
            };
            self.record_jit_call();
        }
        if let Some(expected) = expected {
            validate_interpreted_outputs(out, &expected)?;
        }
        Ok(())
    }

    fn interpreted_outputs(
        &self,
        regs_scratch: &mut Vec<f64>,
        inputs: RowInputs<'_>,
        output_count: usize,
    ) -> Result<Option<Vec<f64>>, CompileError> {
        let validate = self.rows.iter().all(|row| row.interpreter_supported)
            && self
                .rows
                .iter()
                .any(|row| should_validate_jit_row(row.validate_with_interpreter));
        if !validate {
            return Ok(None);
        }
        let mut expected = vec![0.0; output_count];
        let mut base = 0;
        for row in &self.rows {
            let count = row.plan.output_count();
            execute_row(
                &row.plan,
                regs_scratch,
                inputs,
                &mut expected[base..base + count],
            )?;
            base += count;
        }
        Ok(Some(expected))
    }

    #[cfg(test)]
    fn jit_call_count(&self) -> usize {
        self.jit_call_count.get()
    }
}

fn validate_interpreted_outputs(actual: &[f64], expected: &[f64]) -> Result<(), CompileError> {
    for (&actual, &expected) in actual.iter().zip(expected) {
        validate_jit_matches_interpreter("residual", actual, expected)?;
    }
    Ok(())
}

pub(crate) struct CompiledJacobianRows {
    _module: JITModule,
    _pure_calls: Option<Rc<typed_program::CompiledPureCallTable>>,
    rows: Vec<CompiledJacobianRow>,
    input_requirements: InputRequirements,
    regs_scratch: RefCell<Vec<f64>>,
    jit_call_count: Cell<usize>,
}

impl CompiledJacobianRows {
    #[cfg(test)]
    pub(crate) fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        self.call_with_external_tables(y, p, t, v, &[], out)
    }

    pub(crate) fn call_with_external_tables(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        external_tables: &[ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        let output_count: usize = self.rows.iter().map(|row| row.plan.output_count()).sum();
        validate_output_len(out, output_count)?;
        validate_input_requirements(self.input_requirements, y, p, Some(v))?;
        with_active_external_tables(external_tables, || {
            let mut regs_scratch = self.regs_scratch.borrow_mut();
            let ctx = JacobianCallContext {
                y,
                p,
                t,
                v,
                external_tables,
            };
            let mut base = 0;
            for row in self.rows.iter() {
                let k = row.plan.output_count();
                self.call_jacobian_row(row, &mut regs_scratch, &ctx, &mut out[base..base + k])?;
                base += k;
            }
            Ok(())
        })
    }

    fn call_jacobian_row(
        &self,
        row: &CompiledJacobianRow,
        regs_scratch: &mut Vec<f64>,
        ctx: &JacobianCallContext<'_>,
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        if row.interpreter_supported && should_validate_jit_row(row.validate_with_interpreter) {
            let mut expected = vec![0.0; out.len()];
            let inputs = RowInputs {
                y: ctx.y,
                p: ctx.p,
                t: ctx.t,
                seed: Some(ctx.v),
                external_tables: ctx.external_tables,
            };
            execute_row(&row.plan, regs_scratch, inputs, &mut expected)?;
            unsafe { call_jacobian_jit(row.jit, ctx.y, ctx.p, ctx.t, ctx.v, out) };
            self.record_jit_call();
            for (actual, expected) in out.iter().zip(&expected) {
                validate_jit_matches_interpreter("jacobian", *actual, *expected)?;
            }
            return Ok(());
        }
        unsafe { call_jacobian_jit(row.jit, ctx.y, ctx.p, ctx.t, ctx.v, out) };
        self.record_jit_call();
        Ok(())
    }

    pub(crate) fn rows(&self) -> usize {
        self.rows.len()
    }

    pub(crate) fn input_requirements(&self) -> InputRequirements {
        self.input_requirements
    }

    fn record_jit_call(&self) {
        self.jit_call_count
            .set(self.jit_call_count.get().saturating_add(1));
    }

    #[cfg(test)]
    fn jit_call_count(&self) -> usize {
        self.jit_call_count.get()
    }
}

#[derive(Clone, Copy)]
enum RowKind {
    Residual,
    JacobianV,
}

enum ResidualFuncIds {
    Whole(FuncId),
    Chunked(Vec<FuncId>),
}

const RESIDUAL_CHUNK_THRESHOLD: usize = 16_000;
const RESIDUAL_CHUNK_OPS: usize = 4_000;
const RESIDUAL_BATCH_OPS: usize = 12_000;
static NEXT_FOLD_KERNEL_ID: AtomicUsize = AtomicUsize::new(0);
static NEXT_RESIDUAL_KERNEL_ID: AtomicUsize = AtomicUsize::new(0);
static NEXT_ASSIGNMENT_KERNEL_ID: AtomicUsize = AtomicUsize::new(0);

// Final native emission keeps small fixed folds in their caller. Eight points
// covers the short dot-product reductions used by fixed-size estimator
// matrices without turning larger tensor domains into straight-line code.
const INLINE_FIXED_FOLD_POINT_LIMIT: usize = 8;

impl RowKind {
    fn has_seed(self) -> bool {
        matches!(self, Self::JacobianV)
    }
}

fn fold_signature(
    module: &JITModule,
    pointer_type: cranelift_codegen::ir::Type,
    kind: RowKind,
) -> cranelift_codegen::ir::Signature {
    let mut signature = module.make_signature();
    signature.params.push(AbiParam::new(pointer_type));
    signature.params.push(AbiParam::new(pointer_type));
    signature.params.push(AbiParam::new(types::F64));
    if kind.has_seed() {
        signature.params.push(AbiParam::new(pointer_type));
    }
    signature.params.push(AbiParam::new(pointer_type));
    signature.params.push(AbiParam::new(pointer_type));
    signature
}

pub(crate) fn compile_residual_rows(
    rows: &[Vec<LinearOp>],
) -> Result<CompiledResidualRows, CompileError> {
    compile_residual_rows_attached(rows, None)
}

pub(crate) fn compile_residual_rows_with_pure_calls(
    rows: &[Vec<LinearOp>],
    pure_calls: Rc<typed_program::CompiledPureCallTable>,
) -> Result<CompiledResidualRows, CompileError> {
    compile_residual_rows_attached(rows, Some(pure_calls))
}

fn compile_residual_rows_attached(
    rows: &[Vec<LinearOp>],
    pure_calls: Option<Rc<typed_program::CompiledPureCallTable>>,
) -> Result<CompiledResidualRows, CompileError> {
    let mut emitter = CraneliftEmitter::new(pure_calls.as_deref())?;
    let plans = plan_rows(rows)?;
    for row in rows {
        validate_row_supported_by_jit(row, RowKind::Residual)?;
    }
    let pending = compile_residual_functions(&mut emitter, rows, &plans)?;
    emitter
        .module
        .finalize_definitions()
        .map_err(to_backend_err)?;
    let input_requirements = input_requirements_for_plans(&plans);
    let compiled_rows = build_compiled_residual_rows(rows, plans)?;
    let jits = finalize_residual_jits(&emitter.module, pending)?;
    Ok(CompiledResidualRows {
        _module: emitter.module,
        _pure_calls: pure_calls,
        rows: compiled_rows,
        jits,
        input_requirements,
        regs_scratch: RefCell::new(Vec::new()),
        jit_call_count: Cell::new(0),
    })
}

type PendingResidual = (ResidualFuncIds, usize, usize, usize);

fn compile_residual_functions(
    emitter: &mut CraneliftEmitter,
    rows: &[Vec<LinearOp>],
    plans: &[RowPlan],
) -> Result<Vec<PendingResidual>, CompileError> {
    let mut pending = Vec::new();
    let mut row_start = 0usize;
    let mut output_start = 0usize;
    if rows.iter().flatten().any(|operation| {
        matches!(
            operation,
            LinearOp::FunctionConditional { program, .. } if program.owner.is_some()
        )
    }) {
        let output_count = plans.iter().map(RowPlan::output_count).sum();
        let func_id = emitter.compile_residual_batch(rows)?;
        pending.push((
            ResidualFuncIds::Whole(func_id),
            0,
            output_start,
            output_count,
        ));
        output_start = output_start.saturating_add(output_count);
        row_start = rows.len();
    }
    while row_start < rows.len() {
        if residual_program_cost(&rows[row_start]) > RESIDUAL_CHUNK_THRESHOLD {
            let output_count = plans[row_start].output_count();
            pending.push((
                emitter.compile_residual_program(&rows[row_start], row_start)?,
                plans[row_start].register_count(),
                output_start,
                output_count,
            ));
            output_start += output_count;
            row_start += 1;
            continue;
        }
        let mut row_end = row_start;
        let mut operation_count = 0usize;
        while row_end < rows.len()
            && residual_program_cost(&rows[row_end]) <= RESIDUAL_CHUNK_THRESHOLD
        {
            let next = operation_count.saturating_add(residual_program_cost(&rows[row_end]));
            if row_end > row_start && next > RESIDUAL_BATCH_OPS {
                break;
            }
            operation_count = next;
            row_end += 1;
        }
        let output_count = plans[row_start..row_end]
            .iter()
            .map(RowPlan::output_count)
            .sum();
        let func_id = emitter.compile_residual_batch(&rows[row_start..row_end])?;
        pending.push((
            ResidualFuncIds::Whole(func_id),
            0,
            output_start,
            output_count,
        ));
        output_start += output_count;
        row_start = row_end;
    }
    Ok(pending)
}

fn build_compiled_residual_rows(
    rows: &[Vec<LinearOp>],
    plans: Vec<RowPlan>,
) -> Result<Vec<CompiledResidualRow>, CompileError> {
    let mut compiled_rows = checked_vec_with_capacity(rows.len(), "compiled residual rows")?;
    for (index, plan) in plans.into_iter().enumerate() {
        compiled_rows.push(CompiledResidualRow {
            plan,
            validate_with_interpreter: row_uses_table_ops(&rows[index]),
            interpreter_supported: !row_uses_pure_calls(&rows[index]),
        });
    }
    Ok(compiled_rows)
}

fn finalize_residual_jits(
    module: &JITModule,
    pending: Vec<PendingResidual>,
) -> Result<Vec<CompiledResidualJit>, CompileError> {
    let mut jits = checked_vec_with_capacity(pending.len(), "compiled residual batches")?;
    for (func_ids, register_count, output_start, output_count) in pending {
        let jit = match func_ids {
            ResidualFuncIds::Whole(func_id) => {
                ResidualJit::Whole(finalized_residual_fn(module, func_id)?)
            }
            ResidualFuncIds::Chunked(func_ids) => ResidualJit::Chunked(
                func_ids
                    .into_iter()
                    .map(|func_id| finalized_residual_chunk_fn(module, func_id))
                    .collect::<Result<Vec<_>, _>>()?
                    .into_boxed_slice(),
            ),
        };
        jits.push(CompiledResidualJit {
            jit,
            register_count,
            output_start,
            output_count,
        });
    }
    Ok(jits)
}

fn residual_program_cost(program: &[LinearOp]) -> usize {
    program.iter().fold(0usize, |cost, operation| {
        cost.saturating_add(match operation {
            LinearOp::FunctionFold { program, .. }
            | LinearOp::GuardedFunctionFold { program, .. }
            | LinearOp::StoreOutputFunctionFold { program, .. } => {
                1usize.saturating_add(residual_program_cost(&program.update))
            }
            LinearOp::FunctionConditional { program, .. } => {
                let arm_cost = program.arms.iter().fold(0usize, |cost, arm| {
                    cost.saturating_add(residual_program_cost(&arm.condition))
                        .saturating_add(residual_program_cost(&arm.result))
                });
                1usize
                    .saturating_add(arm_cost)
                    .saturating_add(residual_program_cost(&program.fallback))
            }
            _ => 1,
        })
    })
}

pub(crate) fn compile_assignment_schedule(
    rows: &[Vec<LinearOp>],
    target_y_indices: &[usize],
) -> Result<CompiledAssignmentSchedule, CompileError> {
    compile_assignment_schedule_attached(rows, target_y_indices, None)
}

pub(crate) fn compile_assignment_schedule_with_pure_calls(
    rows: &[Vec<LinearOp>],
    target_y_indices: &[usize],
    pure_calls: Rc<typed_program::CompiledPureCallTable>,
) -> Result<CompiledAssignmentSchedule, CompileError> {
    compile_assignment_schedule_attached(rows, target_y_indices, Some(pure_calls))
}

fn compile_assignment_schedule_attached(
    rows: &[Vec<LinearOp>],
    target_y_indices: &[usize],
    pure_calls: Option<Rc<typed_program::CompiledPureCallTable>>,
) -> Result<CompiledAssignmentSchedule, CompileError> {
    let row_refs = rows.iter().map(Vec::as_slice).collect::<Vec<_>>();
    compile_assignment_schedule_slices(&row_refs, target_y_indices, pure_calls)
}

pub(crate) fn compile_exact_assignment_schedule(
    source: &rumoca_ir_solve::ComputeBlock,
    owners: &rumoca_ir_solve::ContinuousRefreshOwners,
    schedule: &rumoca_ir_solve::ExactRefreshAssignmentSchedule,
    pure_calls: Option<Rc<typed_program::CompiledPureCallTable>>,
) -> Result<CompiledAssignmentSchedule, CompileError> {
    let mut blocks =
        checked_vec_with_capacity(schedule.program_ids().len(), "exact assignment rows")?;
    let mut targets = Vec::new();
    for id in schedule.program_ids() {
        let program = owners.exact_assignment_program(*id).ok_or_else(|| {
            CompileError::Input(
                "exact assignment schedule refers to a missing constructed program".to_string(),
            )
        })?;
        let block = program.final_scalar_program(source).map_err(|error| {
            CompileError::Input(format!("exact assignment final projection failed: {error}"))
        })?;
        let [_] = block.programs() else {
            return Err(CompileError::Input(
                "exact assignment owner must contain one program".to_string(),
            ));
        };
        blocks.push(block);
        targets
            .try_reserve_exact(program.target_indices().len())
            .map_err(|_| {
                CompileError::Input("exact assignment targets exceed memory".to_string())
            })?;
        targets.extend_from_slice(program.target_indices());
    }
    let rows = blocks
        .iter()
        .map(|block| block.programs()[0].as_slice())
        .collect::<Vec<_>>();
    compile_assignment_schedule_slices(&rows, &targets, pure_calls)
}

fn compile_assignment_schedule_slices(
    rows: &[&[LinearOp]],
    target_y_indices: &[usize],
    pure_calls: Option<Rc<typed_program::CompiledPureCallTable>>,
) -> Result<CompiledAssignmentSchedule, CompileError> {
    let output_count = rows.iter().try_fold(0usize, |count, row| {
        count.checked_add(ScalarProgramBlock::program_output_count(row))
    });
    if output_count != Some(target_y_indices.len()) {
        return Err(CompileError::Input(format!(
            "assignment schedule has {} outputs but {} targets",
            output_count.map_or(usize::MAX, |count| count),
            target_y_indices.len()
        )));
    }
    let plans = plan_rows(rows)?;
    for (row, plan) in rows.iter().zip(&plans) {
        validate_row_supported_by_jit(row, RowKind::Residual)?;
        if plan.output_count() == 0 {
            return Err(CompileError::Input(
                "assignment schedule programs must have at least one output".to_string(),
            ));
        }
    }
    let input_requirements = input_requirements_for_plans(&plans);
    let required_y_len = target_y_indices
        .iter()
        .copied()
        .max()
        .map_or(0, |index| index.saturating_add(1));
    let mut emitter = CraneliftEmitter::new(pure_calls.as_deref())?;
    let func_id = emitter.compile_assignment_schedule(rows, target_y_indices)?;
    emitter
        .module
        .finalize_definitions()
        .map_err(to_backend_err)?;
    let jit = finalized_assignment_schedule_fn(&emitter.module, func_id)?;
    Ok(CompiledAssignmentSchedule {
        _module: emitter.module,
        _pure_calls: pure_calls,
        jit,
        input_requirements,
        required_y_len,
        rows: rows.len(),
    })
}

pub(crate) fn compile_jacobian_rows(
    rows: &[Vec<LinearOp>],
) -> Result<CompiledJacobianRows, CompileError> {
    compile_jacobian_rows_attached(rows, None)
}

pub(crate) fn compile_jacobian_rows_with_pure_calls(
    rows: &[Vec<LinearOp>],
    pure_calls: Rc<typed_program::CompiledPureCallTable>,
) -> Result<CompiledJacobianRows, CompileError> {
    compile_jacobian_rows_attached(rows, Some(pure_calls))
}

fn compile_jacobian_rows_attached(
    rows: &[Vec<LinearOp>],
    pure_calls: Option<Rc<typed_program::CompiledPureCallTable>>,
) -> Result<CompiledJacobianRows, CompileError> {
    let mut emitter = CraneliftEmitter::new(pure_calls.as_deref())?;
    let plans = plan_rows(rows)?;
    let mut func_ids = checked_vec_with_capacity(rows.len(), "Jacobian row function ids")?;
    for (index, row) in rows.iter().enumerate() {
        validate_row_supported_by_jit(row, RowKind::JacobianV)?;
        let func_id = emitter.compile_row(
            row,
            RowKind::JacobianV,
            &format!("rumoca_jacobian_row_{index}"),
        )?;
        func_ids.push(func_id);
    }
    emitter
        .module
        .finalize_definitions()
        .map_err(to_backend_err)?;
    let input_requirements = input_requirements_for_plans(&plans);
    let mut compiled_rows = checked_vec_with_capacity(rows.len(), "compiled Jacobian rows")?;
    for (index, (plan, func_id)) in plans.into_iter().zip(func_ids).enumerate() {
        let jit = finalized_jacobian_fn(&emitter.module, func_id)?;
        compiled_rows.push(CompiledJacobianRow {
            plan,
            jit,
            validate_with_interpreter: row_uses_table_ops(&rows[index]),
            interpreter_supported: !row_uses_pure_calls(&rows[index]),
        });
    }
    Ok(CompiledJacobianRows {
        _module: emitter.module,
        _pure_calls: pure_calls,
        rows: compiled_rows,
        input_requirements,
        regs_scratch: RefCell::new(Vec::new()),
        jit_call_count: Cell::new(0),
    })
}

fn plan_rows<R: AsRef<[LinearOp]>>(rows: &[R]) -> Result<Vec<RowPlan>, CompileError> {
    let mut plans = checked_vec_with_capacity(rows.len(), "row plans")?;
    for row in rows {
        plans.push(plan_row(row.as_ref())?);
    }
    Ok(plans)
}

fn checked_vec_with_capacity<T>(
    capacity: usize,
    kind: &'static str,
) -> Result<Vec<T>, CompileError> {
    let mut values = Vec::new();
    values.try_reserve(capacity).map_err(|_| {
        CompileError::Backend(format!("{kind} allocation overflow for {capacity} entries"))
    })?;
    Ok(values)
}

fn finalized_residual_fn(
    module: &JITModule,
    func_id: FuncId,
) -> Result<ResidualRowFn, CompileError> {
    let ptr = module.get_finalized_function(func_id);
    if ptr.is_null() {
        return Err(CompileError::Backend(
            "Cranelift returned a null residual function pointer".to_string(),
        ));
    }
    // SAFETY: compile_row declared this function with the ResidualRowFn ABI:
    // (y pointer, p pointer, time) -> f64. The JITModule owns the code memory
    // for at least as long as the CompiledResidualRows containing this pointer.
    Ok(unsafe { std::mem::transmute::<*const u8, ResidualRowFn>(ptr) })
}

fn finalized_jacobian_fn(
    module: &JITModule,
    func_id: FuncId,
) -> Result<JacobianRowFn, CompileError> {
    let ptr = module.get_finalized_function(func_id);
    if ptr.is_null() {
        return Err(CompileError::Backend(
            "Cranelift returned a null Jacobian function pointer".to_string(),
        ));
    }
    // SAFETY: compile_row declared this function with the JacobianRowFn ABI:
    // (y pointer, p pointer, time, seed pointer) -> f64. The JITModule owns the
    // code memory for at least as long as the CompiledJacobianRows containing
    // this pointer.
    Ok(unsafe { std::mem::transmute::<*const u8, JacobianRowFn>(ptr) })
}

fn finalized_residual_chunk_fn(
    module: &JITModule,
    func_id: FuncId,
) -> Result<ResidualChunkFn, CompileError> {
    let ptr = module.get_finalized_function(func_id);
    if ptr.is_null() {
        return Err(CompileError::Backend(
            "Cranelift returned a null residual chunk function pointer".to_string(),
        ));
    }
    // SAFETY: compile_residual_chunk declared this function with the
    // ResidualChunkFn ABI and the owning JIT module outlives the pointer.
    Ok(unsafe { std::mem::transmute::<*const u8, ResidualChunkFn>(ptr) })
}

fn finalized_assignment_schedule_fn(
    module: &JITModule,
    func_id: FuncId,
) -> Result<AssignmentScheduleFn, CompileError> {
    let ptr = module.get_finalized_function(func_id);
    if ptr.is_null() {
        return Err(CompileError::Backend(
            "Cranelift returned a null assignment schedule function pointer".to_string(),
        ));
    }
    // SAFETY: compile_assignment_schedule declares the AssignmentScheduleFn ABI.
    Ok(unsafe { std::mem::transmute::<*const u8, AssignmentScheduleFn>(ptr) })
}

unsafe fn call_residual_jit(
    jit: &ResidualJit,
    register_count: usize,
    regs: &mut Vec<f64>,
    y: &[f64],
    p: &[f64],
    t: f64,
    out: &mut [f64],
) {
    match jit {
        ResidualJit::Whole(jit) => {
            // SAFETY: prevalidation proves all input/output accesses.
            unsafe { jit(y.as_ptr(), p.as_ptr(), t, out.as_mut_ptr()) }
        }
        ResidualJit::Chunked(chunks) => {
            // Chunk functions exchange compiler registers through this tape.
            // Each register is written before its first read by validated SSA.
            regs.resize(register_count, 0.0);
            for jit in chunks {
                // SAFETY: compilation validates row register flow and embeds
                // output offsets against the already validated output slice.
                unsafe {
                    jit(
                        y.as_ptr(),
                        p.as_ptr(),
                        t,
                        out.as_mut_ptr(),
                        regs.as_mut_ptr(),
                    )
                }
            }
        }
    }
}

unsafe fn call_jacobian_jit(
    jit: JacobianRowFn,
    y: &[f64],
    p: &[f64],
    t: f64,
    v: &[f64],
    out: &mut [f64],
) {
    // SAFETY: the caller guarantees the function pointer came from
    // finalized_jacobian_fn, prevalidation ensures any loaded y/p/seed indices
    // are in bounds, and `out` has room for this program's outputs.
    unsafe { jit(y.as_ptr(), p.as_ptr(), t, v.as_ptr(), out.as_mut_ptr()) }
}

fn validate_jit_matches_interpreter(
    row_kind: &'static str,
    actual: f64,
    expected: f64,
) -> Result<(), CompileError> {
    if actual.to_bits() == expected.to_bits() || (actual.is_nan() && expected.is_nan()) {
        return Ok(());
    }
    let scale = actual.abs().max(expected.abs()).max(1.0);
    if (actual - expected).abs() <= f64::EPSILON * 64.0 * scale {
        return Ok(());
    }
    Err(CompileError::Backend(format!(
        "Cranelift {row_kind} row result {actual} diverged from interpreter result {expected}"
    )))
}

fn should_validate_jit_row(row_requires_validation: bool) -> bool {
    cfg!(test) || row_requires_validation
}

fn validate_row_supported_by_jit(row: &[LinearOp], kind: RowKind) -> Result<(), CompileError> {
    for op in row {
        if matches!(
            op,
            LinearOp::RandomInitialState { .. }
                | LinearOp::RandomResult { .. }
                | LinearOp::RandomState { .. }
                | LinearOp::ImpureRandomInit { .. }
                | LinearOp::ImpureRandom { .. }
                | LinearOp::ImpureRandomInteger { .. }
        ) {
            return Err(CompileError::Backend(
                "cranelift row compiler does not support discrete random solve-IR ops".to_string(),
            ));
        }
        if matches!(op, LinearOp::LoadSeed { .. }) && !kind.has_seed() {
            return Err(CompileError::Backend(
                "LoadSeed in residual row without seed input".to_string(),
            ));
        }
    }
    Ok(())
}

fn row_uses_table_ops(row: &[LinearOp]) -> bool {
    row.iter().any(|op| {
        matches!(
            op,
            LinearOp::TableBounds { .. }
                | LinearOp::TableLookup { .. }
                | LinearOp::TableLookupSlope { .. }
                | LinearOp::TableNextEvent { .. }
        )
    })
}

fn row_uses_pure_calls(row: &[LinearOp]) -> bool {
    row.iter().any(|operation| {
        matches!(
            operation,
            LinearOp::PureCall { .. } | LinearOp::PureCallDirectional { .. }
        )
    })
}

struct CraneliftEmitter {
    module: JITModule,
    math: MathImports,
    fold_functions: HashMap<usize, FuncId>,
    canonical_fold_functions: Vec<(Arc<rumoca_ir_solve::FunctionFoldProgram>, FuncId)>,
    conditional_functions: HashMap<rumoca_ir_solve::FunctionConditionalOwnerId, FuncId>,
    pure_call_functions:
        HashMap<rumoca_ir_solve::SolvePureCallOwnerId, typed_program::PureCallImport>,
}

impl CraneliftEmitter {
    fn new(
        pure_calls: Option<&typed_program::CompiledPureCallTable>,
    ) -> Result<Self, CompileError> {
        let mut builder = JITBuilder::with_flags(
            &[("opt_level", "speed")],
            cranelift_module::default_libcall_names(),
        )
        .map_err(to_backend_err)?;
        register_math_symbols(&mut builder);
        if let Some(pure_calls) = pure_calls {
            pure_calls.register_symbols(&mut builder);
        }
        let mut module = JITModule::new(builder);
        let pure_call_functions = pure_calls
            .map(|pure_calls| pure_calls.declare_imports(&mut module))
            .transpose()?
            .unwrap_or_default();
        Ok(Self {
            module,
            math: MathImports::default(),
            fold_functions: HashMap::new(),
            canonical_fold_functions: Vec::new(),
            conditional_functions: HashMap::new(),
            pure_call_functions,
        })
    }

    fn ensure_fold_programs(
        &mut self,
        operations: &[LinearOp],
        kind: RowKind,
    ) -> Result<(), CompileError> {
        let mut programs = Vec::new();
        for operation in operations {
            match operation {
                LinearOp::FunctionFold { program, .. }
                | LinearOp::GuardedFunctionFold { program, .. }
                | LinearOp::StoreOutputFunctionFold { program, .. } => {
                    programs.push(program.clone());
                }
                LinearOp::FunctionConditional { program, .. } => {
                    self.ensure_conditional_fold_programs(program, kind)?;
                }
                _ => {}
            }
        }
        for program in programs {
            self.ensure_fold_program(program, kind)?;
        }
        Ok(())
    }

    fn ensure_conditional_fold_programs(
        &mut self,
        program: &rumoca_ir_solve::FunctionConditionalProgram,
        kind: RowKind,
    ) -> Result<(), CompileError> {
        for arm in &program.arms {
            self.ensure_fold_programs(&arm.condition, kind)?;
            self.ensure_fold_programs(&arm.result, kind)?;
        }
        self.ensure_fold_programs(&program.fallback, kind)
    }

    fn ensure_conditional_programs(
        &mut self,
        operations: &[LinearOp],
        kind: RowKind,
    ) -> Result<(), CompileError> {
        for operation in operations {
            match operation {
                LinearOp::FunctionFold { program, .. }
                | LinearOp::GuardedFunctionFold { program, .. }
                | LinearOp::StoreOutputFunctionFold { program, .. } => {
                    self.ensure_conditional_programs(&program.update, kind)?;
                }
                LinearOp::FunctionConditional { program, .. } => {
                    self.ensure_nested_conditional_program(program, kind)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn ensure_nested_conditional_program(
        &mut self,
        program: &Arc<rumoca_ir_solve::FunctionConditionalProgram>,
        kind: RowKind,
    ) -> Result<(), CompileError> {
        if program.owner.is_some() {
            self.ensure_conditional_program(program.clone(), kind)?;
            return Ok(());
        }
        for arm in &program.arms {
            self.ensure_conditional_programs(&arm.condition, kind)?;
            self.ensure_conditional_programs(&arm.result, kind)?;
        }
        self.ensure_conditional_programs(&program.fallback, kind)
    }

    fn ensure_conditional_program(
        &mut self,
        program: Arc<rumoca_ir_solve::FunctionConditionalProgram>,
        kind: RowKind,
    ) -> Result<FuncId, CompileError> {
        let owner = program.owner.ok_or_else(|| {
            CompileError::Backend(
                "unowned function conditional cannot define a native helper".to_string(),
            )
        })?;
        if let Some(&function) = self.conditional_functions.get(&owner) {
            return Ok(function);
        }
        let pointer_type = self.module.target_config().pointer_type();
        let mut signature = self.module.make_signature();
        signature.params.push(AbiParam::new(pointer_type)); // y
        signature.params.push(AbiParam::new(pointer_type)); // p
        signature.params.push(AbiParam::new(types::F64)); // t
        if kind.has_seed() {
            signature.params.push(AbiParam::new(pointer_type)); // v
        }
        signature.params.push(AbiParam::new(pointer_type)); // captures
        signature.params.push(AbiParam::new(pointer_type)); // results
        let function = self
            .module
            .declare_function(
                &format!("rumoca_conditional_owner_{}", owner.get()),
                Linkage::Local,
                &signature,
            )
            .map_err(to_backend_err)?;
        self.conditional_functions.insert(owner, function);

        for arm in &program.arms {
            self.ensure_fold_programs(&arm.condition, kind)?;
            self.ensure_fold_programs(&arm.result, kind)?;
            self.ensure_conditional_programs(&arm.condition, kind)?;
            self.ensure_conditional_programs(&arm.result, kind)?;
        }
        self.ensure_fold_programs(&program.fallback, kind)?;
        self.ensure_conditional_programs(&program.fallback, kind)?;

        let mut context = self.module.make_context();
        context.func.signature = signature;
        let mut fb_ctx = FunctionBuilderContext::new();
        {
            let mut fb = FunctionBuilder::new(&mut context.func, &mut fb_ctx);
            let entry = fb.create_block();
            fb.append_block_params_for_function_params(entry);
            fb.switch_to_block(entry);
            fb.seal_block(entry);
            let params = fb.block_params(entry).to_vec();
            let y_ptr = params[0];
            let p_ptr = params[1];
            let t_value = params[2];
            let (v_ptr, captures_ptr, results_ptr) = if kind.has_seed() {
                (Some(params[3]), params[4], params[5])
            } else {
                (None, params[3], params[4])
            };
            let mut regs = HashMap::new();
            let mut lower = RowLowerCtx {
                fb: &mut fb,
                module: &mut self.module,
                math: &mut self.math,
                regs: &mut regs,
                y_ptr,
                p_ptr,
                t_value,
                v_ptr,
                backing_regs_ptr: None,
                flags: MemFlags::new(),
                loaded_y: None,
                loaded_p: None,
                fold_carried: None,
                fold_indices: None,
                fold_index_constants: None,
                fold_captures: None,
                fold_captures_ptr: None,
                conditional_captures: None,
                conditional_captures_ptr: Some(captures_ptr),
                fold_functions: &self.fold_functions,
                conditional_functions: &self.conditional_functions,
                pure_call_functions: &self.pure_call_functions,
                pure_call_results: HashMap::new(),
                nested_fold_results: HashMap::new(),
                conditional_results: HashMap::new(),
                fold_carried_versions: Vec::new(),
                known_constants: HashMap::new(),
            };
            let result = lower.lower_function_conditional_inline(0, &program)?;
            lower.copy_stack_to_pointer(result, results_ptr, program.result_count)?;
            lower.fb.ins().return_(&[]);
            drop(lower);
            fb.finalize();
        }
        let flags = settings::Flags::new(settings::builder());
        verify_function(&context.func, &flags).map_err(to_backend_err)?;
        self.module
            .define_function(function, &mut context)
            .map_err(to_backend_err)?;
        self.module.clear_context(&mut context);
        Ok(function)
    }

    fn ensure_fold_program(
        &mut self,
        program: Arc<rumoca_ir_solve::FunctionFoldProgram>,
        kind: RowKind,
    ) -> Result<FuncId, CompileError> {
        let key = Arc::as_ptr(&program) as usize;
        if let Some(function) = self.existing_fold_function(key, &program) {
            return Ok(function);
        }
        let profile_id = NEXT_FOLD_KERNEL_ID.fetch_add(1, Ordering::Relaxed);
        let pointer_type = self.module.target_config().pointer_type();
        let signature = fold_signature(&self.module, pointer_type, kind);
        let function = self
            .module
            .declare_function(
                &format!("rumoca_fold_kernel_{profile_id}"),
                Linkage::Local,
                &signature,
            )
            .map_err(to_backend_err)?;
        self.fold_functions.insert(key, function);
        self.canonical_fold_functions
            .push((program.clone(), function));
        self.ensure_fold_programs(&program.update, kind)?;
        self.ensure_conditional_programs(&program.update, kind)?;

        let mut context = self.module.make_context();
        context.func.signature = signature;
        let mut fb_ctx = FunctionBuilderContext::new();
        {
            let mut fb = FunctionBuilder::new(&mut context.func, &mut fb_ctx);
            let entry = fb.create_block();
            fb.append_block_params_for_function_params(entry);
            fb.switch_to_block(entry);
            fb.seal_block(entry);
            let params = fb.block_params(entry).to_vec();
            let y_ptr = params[0];
            let p_ptr = params[1];
            let t_value = params[2];
            let (v_ptr, carried_ptr, captures_ptr) = if kind.has_seed() {
                (Some(params[3]), params[4], params[5])
            } else {
                (None, params[3], params[4])
            };
            let mut regs = HashMap::new();
            let flags = MemFlags::new();
            let captures = load_fold_captures(&mut fb, flags, captures_ptr, program.capture_count)?;
            let bytes = program
                .carried_count
                .checked_mul(std::mem::size_of::<f64>())
                .and_then(|bytes| u32::try_from(bytes).ok())
                .ok_or_else(|| {
                    CompileError::Backend("fold kernel carried size overflow".to_string())
                })?;
            let carried = fb.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                bytes,
                3,
            ));
            let backing_regs_ptr =
                create_fold_register_tape(&mut fb, pointer_type, &program.update)?;
            let mut lower = RowLowerCtx {
                fb: &mut fb,
                module: &mut self.module,
                math: &mut self.math,
                regs: &mut regs,
                y_ptr,
                p_ptr,
                t_value,
                v_ptr,
                backing_regs_ptr,
                flags,
                loaded_y: None,
                loaded_p: None,
                fold_carried: None,
                fold_indices: None,
                fold_index_constants: None,
                fold_captures: None,
                fold_captures_ptr: None,
                conditional_captures: None,
                conditional_captures_ptr: None,
                fold_functions: &self.fold_functions,
                conditional_functions: &self.conditional_functions,
                pure_call_functions: &self.pure_call_functions,
                pure_call_results: HashMap::new(),
                nested_fold_results: HashMap::new(),
                conditional_results: HashMap::new(),
                fold_carried_versions: Vec::new(),
                known_constants: HashMap::new(),
            };
            lower.copy_pointer_to_stack(carried_ptr, carried, program.carried_count)?;
            lower.lower_function_fold_on_stack(carried, &captures, Some(captures_ptr), &program)?;
            lower.copy_stack_to_pointer(carried, carried_ptr, program.carried_count)?;
            lower.fb.ins().return_(&[]);
            drop(lower);
            fb.finalize();
        }
        let flags = settings::Flags::new(settings::builder());
        verify_function(&context.func, &flags).map_err(to_backend_err)?;
        self.module
            .define_function(function, &mut context)
            .map_err(to_backend_err)?;
        self.module.clear_context(&mut context);
        Ok(function)
    }

    fn existing_fold_function(
        &mut self,
        key: usize,
        program: &rumoca_ir_solve::FunctionFoldProgram,
    ) -> Option<FuncId> {
        if let Some(&function) = self.fold_functions.get(&key) {
            return Some(function);
        }
        let function = self
            .canonical_fold_functions
            .iter()
            .find_map(|(canonical, function)| {
                (canonical.as_ref() == program).then_some(*function)
            })?;
        self.fold_functions.insert(key, function);
        Some(function)
    }

    fn compile_row(
        &mut self,
        row: &[LinearOp],
        kind: RowKind,
        name: &str,
    ) -> Result<FuncId, CompileError> {
        self.ensure_fold_programs(row, kind)?;
        self.ensure_conditional_programs(row, kind)?;
        let pointer_type = self.module.target_config().pointer_type();

        let mut signature = self.module.make_signature();
        signature.params.push(AbiParam::new(pointer_type)); // y
        signature.params.push(AbiParam::new(pointer_type)); // p
        signature.params.push(AbiParam::new(types::F64)); // t
        if kind.has_seed() {
            signature.params.push(AbiParam::new(pointer_type)); // v
        }
        signature.params.push(AbiParam::new(pointer_type)); // out (written, not returned)

        let func_id = self
            .module
            .declare_function(name, Linkage::Local, &signature)
            .map_err(to_backend_err)?;

        let mut context = self.module.make_context();
        context.func.signature = signature;
        let mut fb_ctx = FunctionBuilderContext::new();
        {
            let mut fb = FunctionBuilder::new(&mut context.func, &mut fb_ctx);
            let entry = fb.create_block();
            fb.append_block_params_for_function_params(entry);
            fb.switch_to_block(entry);
            fb.seal_block(entry);

            let params = fb.block_params(entry).to_vec();
            let y_ptr = params[0];
            let p_ptr = params[1];
            let t_value = params[2];
            let (v_ptr, out_ptr) = if kind.has_seed() {
                (Some(params[3]), params[4])
            } else {
                (None, params[3])
            };

            let mut regs: HashMap<u32, cranelift_codegen::ir::Value> = HashMap::new();
            let mut loaded_y = HashMap::new();
            let mut loaded_p = HashMap::new();
            let flags = MemFlags::new();
            let backing_regs_ptr = create_row_register_tape(&mut fb, pointer_type, row)?;
            let mut row_lower = RowLowerCtx {
                fb: &mut fb,
                module: &mut self.module,
                math: &mut self.math,
                regs: &mut regs,
                y_ptr,
                p_ptr,
                t_value,
                v_ptr,
                backing_regs_ptr,
                flags,
                loaded_y: backing_regs_ptr.is_none().then_some(&mut loaded_y),
                loaded_p: backing_regs_ptr.is_none().then_some(&mut loaded_p),
                fold_carried: None,
                fold_indices: None,
                fold_index_constants: None,
                fold_captures: None,
                fold_captures_ptr: None,
                conditional_captures: None,
                conditional_captures_ptr: None,
                fold_functions: &self.fold_functions,
                conditional_functions: &self.conditional_functions,
                pure_call_functions: &self.pure_call_functions,
                pure_call_results: HashMap::new(),
                nested_fold_results: HashMap::new(),
                conditional_results: HashMap::new(),
                fold_carried_versions: Vec::new(),
                known_constants: HashMap::new(),
            };

            row_lower.lower_row_outputs(row, out_ptr)?;
            fb.ins().return_(&[]);
            fb.finalize();
        }

        let flags = settings::Flags::new(settings::builder());
        verify_function(&context.func, &flags).map_err(to_backend_err)?;
        self.module
            .define_function(func_id, &mut context)
            .map_err(to_backend_err)?;
        self.module.clear_context(&mut context);
        Ok(func_id)
    }

    fn compile_residual_program(
        &mut self,
        row: &[LinearOp],
        row_index: usize,
    ) -> Result<ResidualFuncIds, CompileError> {
        let profile_id = NEXT_RESIDUAL_KERNEL_ID.fetch_add(1, Ordering::Relaxed);
        if row.len() <= RESIDUAL_CHUNK_THRESHOLD || !row.iter().cloned().all(chunkable_op) {
            return self
                .compile_row(
                    row,
                    RowKind::Residual,
                    &format!("rumoca_residual_program_{profile_id}_row_{row_index}"),
                )
                .map(ResidualFuncIds::Whole);
        }
        let mut output_base = 0usize;
        let mut ids = checked_vec_with_capacity(
            row.len().div_ceil(RESIDUAL_CHUNK_OPS),
            "residual chunk function ids",
        )?;
        for (chunk_index, chunk) in row.chunks(RESIDUAL_CHUNK_OPS).enumerate() {
            ids.push(self.compile_residual_chunk(
                chunk,
                output_base,
                &format!(
                    "rumoca_residual_program_{profile_id}_row_{row_index}_chunk_{chunk_index}"
                ),
            )?);
            output_base = output_base.saturating_add(
                chunk
                    .iter()
                    .filter(|op| matches!(op, LinearOp::StoreOutput { .. }))
                    .count(),
            );
        }
        Ok(ResidualFuncIds::Chunked(ids))
    }

    fn compile_residual_batch(&mut self, rows: &[Vec<LinearOp>]) -> Result<FuncId, CompileError> {
        for row in rows {
            self.ensure_fold_programs(row, RowKind::Residual)?;
            self.ensure_conditional_programs(row, RowKind::Residual)?;
        }
        let pointer_type = self.module.target_config().pointer_type();
        let mut signature = self.module.make_signature();
        signature.params.push(AbiParam::new(pointer_type));
        signature.params.push(AbiParam::new(pointer_type));
        signature.params.push(AbiParam::new(types::F64));
        signature.params.push(AbiParam::new(pointer_type));
        let profile_id = NEXT_RESIDUAL_KERNEL_ID.fetch_add(1, Ordering::Relaxed);
        let func_id = self
            .module
            .declare_function(
                &format!("rumoca_residual_batch_{profile_id}"),
                Linkage::Local,
                &signature,
            )
            .map_err(to_backend_err)?;
        let mut context = self.module.make_context();
        context.func.signature = signature;
        let mut fb_ctx = FunctionBuilderContext::new();
        {
            let mut fb = FunctionBuilder::new(&mut context.func, &mut fb_ctx);
            let entry = fb.create_block();
            fb.append_block_params_for_function_params(entry);
            fb.switch_to_block(entry);
            fb.seal_block(entry);
            let params = fb.block_params(entry).to_vec();
            let (y_ptr, p_ptr, t_value, out_ptr) = (params[0], params[1], params[2], params[3]);
            let mut loaded_y = HashMap::new();
            let mut loaded_p = HashMap::new();
            let mut conditional_results = HashMap::new();
            let mut output_base = 0usize;
            for row in rows {
                let mut regs = HashMap::new();
                let backing_regs_ptr = create_row_register_tape(&mut fb, pointer_type, row)?;
                let mut row_lower = RowLowerCtx {
                    fb: &mut fb,
                    module: &mut self.module,
                    math: &mut self.math,
                    regs: &mut regs,
                    y_ptr,
                    p_ptr,
                    t_value,
                    v_ptr: None,
                    backing_regs_ptr,
                    flags: MemFlags::new(),
                    loaded_y: backing_regs_ptr.is_none().then_some(&mut loaded_y),
                    loaded_p: backing_regs_ptr.is_none().then_some(&mut loaded_p),
                    fold_carried: None,
                    fold_indices: None,
                    fold_index_constants: None,
                    fold_captures: None,
                    fold_captures_ptr: None,
                    conditional_captures: None,
                    conditional_captures_ptr: None,
                    fold_functions: &self.fold_functions,
                    conditional_functions: &self.conditional_functions,
                    pure_call_functions: &self.pure_call_functions,
                    pure_call_results: HashMap::new(),
                    nested_fold_results: HashMap::new(),
                    conditional_results: std::mem::take(&mut conditional_results),
                    fold_carried_versions: Vec::new(),
                    known_constants: HashMap::new(),
                };
                row_lower.lower_row_outputs_from(row, out_ptr, output_base)?;
                conditional_results = std::mem::take(&mut row_lower.conditional_results);
                output_base += ScalarProgramBlock::program_output_count(row);
            }
            fb.ins().return_(&[]);
            fb.finalize();
        }
        let flags = settings::Flags::new(settings::builder());
        verify_function(&context.func, &flags).map_err(to_backend_err)?;
        self.module
            .define_function(func_id, &mut context)
            .map_err(to_backend_err)?;
        self.module.clear_context(&mut context);
        Ok(func_id)
    }

    fn compile_residual_chunk(
        &mut self,
        chunk: &[LinearOp],
        output_base: usize,
        name: &str,
    ) -> Result<FuncId, CompileError> {
        let pointer_type = self.module.target_config().pointer_type();
        let mut signature = self.module.make_signature();
        signature.params.push(AbiParam::new(pointer_type)); // y
        signature.params.push(AbiParam::new(pointer_type)); // p
        signature.params.push(AbiParam::new(types::F64)); // t
        signature.params.push(AbiParam::new(pointer_type)); // out
        signature.params.push(AbiParam::new(pointer_type)); // register tape
        let func_id = self
            .module
            .declare_function(name, Linkage::Local, &signature)
            .map_err(to_backend_err)?;
        let mut context = self.module.make_context();
        context.func.signature = signature;
        let mut fb_ctx = FunctionBuilderContext::new();
        {
            let mut fb = FunctionBuilder::new(&mut context.func, &mut fb_ctx);
            let entry = fb.create_block();
            fb.append_block_params_for_function_params(entry);
            fb.switch_to_block(entry);
            fb.seal_block(entry);
            let params = fb.block_params(entry).to_vec();
            let mut regs = HashMap::new();
            let mut loaded_y = HashMap::new();
            let mut loaded_p = HashMap::new();
            let mut row_lower = RowLowerCtx {
                fb: &mut fb,
                module: &mut self.module,
                math: &mut self.math,
                regs: &mut regs,
                y_ptr: params[0],
                p_ptr: params[1],
                t_value: params[2],
                v_ptr: None,
                backing_regs_ptr: Some(params[4]),
                flags: MemFlags::new(),
                loaded_y: Some(&mut loaded_y),
                loaded_p: Some(&mut loaded_p),
                fold_carried: None,
                fold_indices: None,
                fold_index_constants: None,
                fold_captures: None,
                fold_captures_ptr: None,
                conditional_captures: None,
                conditional_captures_ptr: None,
                fold_functions: &self.fold_functions,
                conditional_functions: &self.conditional_functions,
                pure_call_functions: &self.pure_call_functions,
                pure_call_results: HashMap::new(),
                nested_fold_results: HashMap::new(),
                conditional_results: HashMap::new(),
                fold_carried_versions: Vec::new(),
                known_constants: HashMap::new(),
            };
            row_lower.lower_row_outputs_from(chunk, params[3], output_base)?;
            fb.ins().return_(&[]);
            fb.finalize();
        }
        let flags = settings::Flags::new(settings::builder());
        verify_function(&context.func, &flags).map_err(to_backend_err)?;
        self.module
            .define_function(func_id, &mut context)
            .map_err(to_backend_err)?;
        self.module.clear_context(&mut context);
        Ok(func_id)
    }

    fn compile_assignment_schedule<R: AsRef<[LinearOp]>>(
        &mut self,
        rows: &[R],
        target_y_indices: &[usize],
    ) -> Result<FuncId, CompileError> {
        for row in rows {
            self.ensure_fold_programs(row.as_ref(), RowKind::Residual)?;
            self.ensure_conditional_programs(row.as_ref(), RowKind::Residual)?;
        }
        let pointer_type = self.module.target_config().pointer_type();
        let mut signature = self.module.make_signature();
        signature.params.push(AbiParam::new(pointer_type)); // mutable y
        signature.params.push(AbiParam::new(pointer_type)); // p
        signature.params.push(AbiParam::new(types::F64)); // t
        let profile_id = NEXT_ASSIGNMENT_KERNEL_ID.fetch_add(1, Ordering::Relaxed);
        let func_id = self
            .module
            .declare_function(
                &format!("rumoca_assignment_schedule_{profile_id}"),
                Linkage::Local,
                &signature,
            )
            .map_err(to_backend_err)?;
        let mut context = self.module.make_context();
        context.func.signature = signature;
        let mut fb_ctx = FunctionBuilderContext::new();
        {
            let mut fb = FunctionBuilder::new(&mut context.func, &mut fb_ctx);
            let entry = fb.create_block();
            fb.append_block_params_for_function_params(entry);
            fb.switch_to_block(entry);
            fb.seal_block(entry);
            let params = fb.block_params(entry).to_vec();
            let (y_ptr, p_ptr, t_value) = (params[0], params[1], params[2]);
            let flags = MemFlags::new();
            let mut target_offset = 0usize;
            for row in rows {
                let row = row.as_ref();
                // Assignment programs execute sequentially and may overwrite
                // solver-Y slots read by later programs. Keep load CSE local
                // to one program: cross-program SSA values become stale after
                // a tape-backed tensor program commits its targets, and long
                // lived values also increase register-allocation pressure.
                let mut loaded_y = HashMap::new();
                let mut loaded_p = HashMap::new();
                let output_count = ScalarProgramBlock::program_output_count(row);
                let targets = &target_y_indices[target_offset..target_offset + output_count];
                let mut regs = HashMap::new();
                let backing_regs_ptr = create_row_register_tape(&mut fb, pointer_type, row)?;
                let mut row_lower = RowLowerCtx {
                    fb: &mut fb,
                    module: &mut self.module,
                    math: &mut self.math,
                    regs: &mut regs,
                    y_ptr,
                    p_ptr,
                    t_value,
                    v_ptr: None,
                    backing_regs_ptr,
                    flags,
                    loaded_y: backing_regs_ptr.is_none().then_some(&mut loaded_y),
                    loaded_p: backing_regs_ptr.is_none().then_some(&mut loaded_p),
                    fold_carried: None,
                    fold_indices: None,
                    fold_index_constants: None,
                    fold_captures: None,
                    fold_captures_ptr: None,
                    conditional_captures: None,
                    conditional_captures_ptr: None,
                    fold_functions: &self.fold_functions,
                    conditional_functions: &self.conditional_functions,
                    pure_call_functions: &self.pure_call_functions,
                    pure_call_results: HashMap::new(),
                    nested_fold_results: HashMap::new(),
                    conditional_results: HashMap::new(),
                    fold_carried_versions: Vec::new(),
                    known_constants: HashMap::new(),
                };
                row_lower.lower_assignments(row, targets)?;
                target_offset += output_count;
            }
            fb.ins().return_(&[]);
            fb.finalize();
        }
        let flags = settings::Flags::new(settings::builder());
        verify_function(&context.func, &flags).map_err(to_backend_err)?;
        self.module
            .define_function(func_id, &mut context)
            .map_err(to_backend_err)?;
        self.module.clear_context(&mut context);
        Ok(func_id)
    }
}

fn create_row_register_tape(
    fb: &mut FunctionBuilder<'_>,
    pointer_type: cranelift_codegen::ir::Type,
    row: &[LinearOp],
) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
    if !row.iter().any(|operation| {
        matches!(
            operation,
            LinearOp::DotProduct { .. }
                | LinearOp::MatrixMultiply { .. }
                | LinearOp::TensorBinary { .. }
                | LinearOp::TensorTranspose { .. }
                | LinearOp::TensorConcatenate { .. }
                | LinearOp::TensorUpdate { .. }
                | LinearOp::TensorFill { .. }
                | LinearOp::TensorIdentity { .. }
                | LinearOp::TensorLoad { .. }
        )
    }) {
        return Ok(None);
    }
    create_register_tape(fb, pointer_type, row)
}

fn create_fold_register_tape(
    fb: &mut FunctionBuilder<'_>,
    pointer_type: cranelift_codegen::ir::Type,
    row: &[LinearOp],
) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
    // Fixed-shape tensor operations lower directly to scalar SSA here, at the
    // final machine-code boundary.  A register tape is only required when an
    // operation must dynamically address a run of intermediate registers.
    // Runtime scalar indices do not require one: their lowering materializes
    // just the indexed source run in a local stack slot.  Tensor-update slices
    // are the sole operation whose compact scan currently addresses the row's
    // register namespace dynamically.
    if !row.iter().any(|operation| {
        matches!(
            operation,
            LinearOp::TensorUpdate { subscripts, .. }
                if subscripts.iter().any(|subscript| matches!(
                    subscript,
                    rumoca_ir_solve::TensorUpdateSubscript::Slice { .. }
                ))
        )
    }) {
        return Ok(None);
    }
    create_register_tape(fb, pointer_type, row)
}

fn create_register_tape(
    fb: &mut FunctionBuilder<'_>,
    pointer_type: cranelift_codegen::ir::Type,
    row: &[LinearOp],
) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
    let register_count =
        row.iter().try_fold(0usize, |count, operation| {
            Ok::<_, CompileError>(match max_reg_index(operation.clone())? {
                Some(index) => count.max(index.checked_add(1).ok_or_else(|| {
                    CompileError::Backend("row register tape count overflow".into())
                })?),
                None => count,
            })
        })?;
    let bytes = register_count
        .max(1)
        .checked_mul(std::mem::size_of::<f64>())
        .and_then(|bytes| u32::try_from(bytes).ok())
        .ok_or_else(|| CompileError::Backend("row register tape size overflow".into()))?;
    let tape =
        fb.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, bytes, 3));
    Ok(Some(fb.ins().stack_addr(pointer_type, tape, 0)))
}

fn create_typed_cell_slot(
    fb: &mut FunctionBuilder<'_>,
    cells: usize,
    context: &str,
) -> Result<StackSlot, CompileError> {
    let bytes = cells
        .max(1)
        .checked_mul(std::mem::size_of::<u64>())
        .and_then(|bytes| u32::try_from(bytes).ok())
        .ok_or_else(|| CompileError::Backend(format!("{context} stack size overflows")))?;
    Ok(fb.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, bytes, 3)))
}

fn typed_cell_type(scalar: rumoca_ir_solve::SolveScalarType) -> cranelift_codegen::ir::Type {
    match scalar {
        rumoca_ir_solve::SolveScalarType::Real {
            format: rumoca_ir_solve::SolveRealFormat::Binary32,
            ..
        } => types::F32,
        rumoca_ir_solve::SolveScalarType::Real {
            format: rumoca_ir_solve::SolveRealFormat::Binary64,
            ..
        } => types::F64,
        rumoca_ir_solve::SolveScalarType::Integer(_)
        | rumoca_ir_solve::SolveScalarType::Boolean => types::I64,
    }
}

fn encode_typed_cell(
    fb: &mut FunctionBuilder<'_>,
    scalar: rumoca_ir_solve::SolveScalarType,
    value: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    match scalar {
        rumoca_ir_solve::SolveScalarType::Real {
            format: rumoca_ir_solve::SolveRealFormat::Binary32,
            ..
        } => fb.ins().fdemote(types::F32, value),
        rumoca_ir_solve::SolveScalarType::Real {
            format: rumoca_ir_solve::SolveRealFormat::Binary64,
            ..
        } => value,
        rumoca_ir_solve::SolveScalarType::Integer(_) => fb.ins().fcvt_to_sint(types::I64, value),
        rumoca_ir_solve::SolveScalarType::Boolean => {
            let zero = fb.ins().f64const(0.0);
            let condition = fb.ins().fcmp(FloatCC::NotEqual, value, zero);
            fb.ins().uextend(types::I64, condition)
        }
    }
}

fn decode_typed_cell(
    fb: &mut FunctionBuilder<'_>,
    scalar: rumoca_ir_solve::SolveScalarType,
    value: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    match scalar {
        rumoca_ir_solve::SolveScalarType::Real {
            format: rumoca_ir_solve::SolveRealFormat::Binary32,
            ..
        } => fb.ins().fpromote(types::F64, value),
        rumoca_ir_solve::SolveScalarType::Real {
            format: rumoca_ir_solve::SolveRealFormat::Binary64,
            ..
        } => value,
        rumoca_ir_solve::SolveScalarType::Integer(_) => fb.ins().fcvt_from_sint(types::F64, value),
        rumoca_ir_solve::SolveScalarType::Boolean => fb.ins().fcvt_from_uint(types::F64, value),
    }
}

struct RowLowerCtx<'a, 'b> {
    fb: &'a mut FunctionBuilder<'b>,
    module: &'a mut JITModule,
    math: &'a mut MathImports,
    regs: &'a mut HashMap<u32, cranelift_codegen::ir::Value>,
    y_ptr: cranelift_codegen::ir::Value,
    p_ptr: cranelift_codegen::ir::Value,
    t_value: cranelift_codegen::ir::Value,
    v_ptr: Option<cranelift_codegen::ir::Value>,
    backing_regs_ptr: Option<cranelift_codegen::ir::Value>,
    flags: MemFlags,
    loaded_y: Option<&'a mut HashMap<usize, cranelift_codegen::ir::Value>>,
    loaded_p: Option<&'a mut HashMap<usize, cranelift_codegen::ir::Value>>,
    fold_carried: Option<StackSlot>,
    fold_indices: Option<&'a [cranelift_codegen::ir::Value]>,
    fold_index_constants: Option<&'a [f64]>,
    fold_captures: Option<&'a [cranelift_codegen::ir::Value]>,
    fold_captures_ptr: Option<cranelift_codegen::ir::Value>,
    conditional_captures: Option<&'a HashMap<usize, cranelift_codegen::ir::Value>>,
    conditional_captures_ptr: Option<cranelift_codegen::ir::Value>,
    fold_functions: &'a HashMap<usize, FuncId>,
    conditional_functions: &'a HashMap<rumoca_ir_solve::FunctionConditionalOwnerId, FuncId>,
    pure_call_functions:
        &'a HashMap<rumoca_ir_solve::SolvePureCallOwnerId, typed_program::PureCallImport>,
    pure_call_results: HashMap<PureCallResultKey, StackSlot>,
    nested_fold_results: HashMap<NestedFoldCallKey, StackSlot>,
    conditional_results: HashMap<rumoca_ir_solve::FunctionConditionalOwnerId, StackSlot>,
    fold_carried_versions: Vec<u32>,
    known_constants: HashMap<u32, f64>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum NestedFoldInitialKey {
    Registers(Box<[cranelift_codegen::ir::Value]>),
    ParentCarried {
        base: usize,
        count: usize,
        versions: Box<[u32]>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct NestedFoldCallKey {
    program: FuncId,
    initial: Box<[NestedFoldInitialKey]>,
    captures: Box<[cranelift_codegen::ir::Value]>,
    guard: Option<(cranelift_codegen::ir::Value, bool)>,
}

/// One native pure-call result is reusable only for the exact SSA inputs that
/// produced it. The same owner may occur repeatedly with different primal or
/// tangent values, as it does while constructing an affine derivative basis.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct PureCallResultKey {
    owner: rumoca_ir_solve::SolvePureCallOwnerId,
    directional: bool,
    inputs: Box<[u32]>,
}

struct NestedFoldOutput<'a> {
    parent_carried: StackSlot,
    output_base: usize,
    initial: &'a [rumoca_ir_solve::FoldInitialSource],
    capture_start: u32,
    program: &'a Arc<rumoca_ir_solve::FunctionFoldProgram>,
    result_base: usize,
    result_count: usize,
    condition: Option<u32>,
    nested_when_true: bool,
}

#[derive(Clone, Copy)]
struct StaticMatrixOutput {
    dst_start: u32,
    lhs_start: u32,
    rhs_start: u32,
    row: usize,
    column: usize,
    inner: usize,
    columns: usize,
    lanes: usize,
}

#[derive(Clone, Copy)]
struct StaticMatrixMultiply {
    dst_start: u32,
    lhs_start: u32,
    rhs_start: u32,
    rows: usize,
    inner: usize,
    columns: usize,
    lanes: usize,
}

struct TensorCrossComponent<'a> {
    dst_start: u32,
    lhs: &'a [cranelift_codegen::ir::Value],
    rhs: &'a [cranelift_codegen::ir::Value],
    component: usize,
    first: usize,
    second: usize,
    lanes: usize,
}

struct StaticTransposeCell {
    dst_start: u32,
    src_start: u32,
    rows: usize,
    columns: usize,
    value_width: usize,
    row: usize,
    column: usize,
}

#[derive(Clone, Copy)]
struct TensorBinaryRequest {
    dst_start: u32,
    op: BinaryOp,
    lhs_start: u32,
    rhs_start: u32,
    count: usize,
    lhs_stride: usize,
    rhs_stride: usize,
    lanes: usize,
}

#[derive(Clone, Copy)]
struct TensorConcatenateCopy<'a> {
    dst_start: u32,
    source: &'a rumoca_ir_solve::TensorConcatenateSource,
    axis: usize,
    axis_offset: usize,
    inner: usize,
    result_block: usize,
    lanes: usize,
}

impl<'a, 'b> RowLowerCtx<'a, 'b> {
    fn lower_assignments(
        &mut self,
        row: &[LinearOp],
        targets: &[usize],
    ) -> Result<(), CompileError> {
        let mut outputs = Vec::with_capacity(targets.len());
        for op in row.iter().cloned() {
            self.lower_assignment_output(op, &mut outputs)?;
        }
        if outputs.len() != targets.len() {
            return Err(CompileError::Input(format!(
                "assignment schedule program has {} outputs but {} targets",
                outputs.len(),
                targets.len()
            )));
        }
        for (value, &target) in outputs.into_iter().zip(targets) {
            let offset = target
                .checked_mul(std::mem::size_of::<f64>())
                .and_then(|value| i32::try_from(value).ok())
                .ok_or_else(|| {
                    CompileError::Input("assignment target byte offset exceeds i32".to_string())
                })?;
            self.fb.ins().store(self.flags, value, self.y_ptr, offset);
            if let Some(loaded_y) = self.loaded_y.as_deref_mut() {
                loaded_y.insert(target, value);
            }
        }
        Ok(())
    }

    fn lower_assignment_output(
        &mut self,
        operation: LinearOp,
        outputs: &mut Vec<cranelift_codegen::ir::Value>,
    ) -> Result<(), CompileError> {
        if let LinearOp::StoreOutputRange {
            start,
            count,
            stride,
        } = operation
        {
            outputs.extend(self.lower_output_range(start, count, stride)?);
            return Ok(());
        }
        if let Some(value) = self.lower_op(operation)? {
            outputs.push(value);
        }
        Ok(())
    }

    /// Lower every op in `row`, writing each `StoreOutput` result to the next
    /// consecutive `out[k]` slot so a multi-output program fills consecutive
    /// output slots. (`lower_op` returns `Some(value)` exactly for StoreOutput.)
    fn lower_row_outputs(
        &mut self,
        row: &[LinearOp],
        out_ptr: cranelift_codegen::ir::Value,
    ) -> Result<(), CompileError> {
        self.lower_row_outputs_from(row, out_ptr, 0)
    }

    fn lower_row_outputs_from(
        &mut self,
        row: &[LinearOp],
        out_ptr: cranelift_codegen::ir::Value,
        output_base: usize,
    ) -> Result<(), CompileError> {
        let mut out_idx = i32::try_from(output_base)
            .map_err(|_| CompileError::Backend("residual output index exceeds i32".to_string()))?;
        for op in row.iter().cloned() {
            let values = match op {
                LinearOp::StoreOutputRange {
                    start,
                    count,
                    stride,
                } => self.lower_output_range(start, count, stride)?,
                operation => self.lower_op(operation)?.into_iter().collect(),
            };
            for value in values {
                self.fb.ins().store(self.flags, value, out_ptr, out_idx * 8);
                out_idx += 1;
            }
        }
        Ok(())
    }

    fn lower_output_range(
        &mut self,
        start: u32,
        count: usize,
        stride: usize,
    ) -> Result<Vec<cranelift_codegen::ir::Value>, CompileError> {
        let mut values = Vec::with_capacity(count);
        for ordinal in 0..count {
            values.push(self.lookup(checked_strided_register(
                start,
                ordinal,
                stride,
                "output range",
            )?)?);
        }
        Ok(values)
    }

    // SPEC_0021: exhaustive dispatch over every LinearOp native lowering contract.
    #[expect(
        clippy::too_many_lines,
        clippy::excessive_nesting,
        reason = "one exhaustive native dispatcher makes unsupported LinearOp variants fail closed"
    )]
    fn lower_op(
        &mut self,
        op: LinearOp,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        match op {
            LinearOp::Const { dst, value } => {
                self.known_constants.insert(dst, value);
                let value = self.fb.ins().f64const(value);
                self.insert(dst, value)
            }
            LinearOp::LoadTime { dst } => self.insert(dst, self.t_value),
            LinearOp::LoadY { dst, index } => self.lower_loaded_y_reg(dst, index),
            LinearOp::LoadP { dst, index } => self.lower_loaded_p_reg(dst, index),
            LinearOp::LoadIndexedP {
                dst,
                base,
                count,
                index,
            } => self.lower_indexed_loaded_reg(dst, self.p_ptr, base, count, index),
            LinearOp::LoadIndexedRegister {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => self.lower_indexed_register(dst, base, stride, &dimensions, &indices),
            LinearOp::LoadSeed { dst, index } => self.lower_seed_reg(dst, index),
            LinearOp::LoadIndexedSeed {
                dst,
                base,
                count,
                index,
            } => {
                let base_ptr = self.v_ptr.ok_or_else(|| {
                    CompileError::Backend("LoadIndexedSeed in row without seed input".to_string())
                })?;
                self.lower_indexed_loaded_reg(dst, base_ptr, base, count, index)
            }
            LinearOp::LoadFoldCarried { dst, index } => {
                let carried = self.fold_carried.ok_or_else(|| {
                    CompileError::Backend("invalid function-fold carried load".to_string())
                })?;
                let offset = i32::try_from(index.checked_mul(8).ok_or_else(|| {
                    CompileError::Backend("function-fold carried offset overflow".to_string())
                })?)
                .map_err(|_| {
                    CompileError::Backend("function-fold carried offset exceeds i32".to_string())
                })?;
                let value = self.fb.ins().stack_load(types::F64, carried, offset);
                self.insert(dst, value)
            }
            LinearOp::LoadIndexedFoldCarried {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => self.lower_indexed_fold_carried(dst, base, stride, &dimensions, &indices),
            LinearOp::LoadIndexedFoldCapture {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => self.lower_indexed_fold_capture(dst, base, stride, &dimensions, &indices),
            LinearOp::LoadFoldIndex { dst, dimension } => {
                let value = self
                    .fold_indices
                    .and_then(|values| values.get(dimension))
                    .copied()
                    .ok_or_else(|| {
                        CompileError::Backend("invalid function-fold binder load".to_string())
                    })?;
                if let Some(constant) = self
                    .fold_index_constants
                    .and_then(|values| values.get(dimension))
                    .copied()
                {
                    self.known_constants.insert(dst, constant);
                }
                self.insert(dst, value)
            }
            LinearOp::LoadFoldCapture { dst, index } => {
                let value = self
                    .fold_captures
                    .and_then(|values| values.get(index))
                    .copied()
                    .ok_or_else(|| {
                        CompileError::Backend("invalid function-fold capture load".to_string())
                    })?;
                self.insert(dst, value)
            }
            LinearOp::LoadFunctionConditionalCapture { dst, index } => {
                let value = self
                    .conditional_captures
                    .and_then(|values| values.get(&index))
                    .copied()
                    .ok_or_else(|| {
                        CompileError::Backend(
                            "invalid function-conditional capture load".to_string(),
                        )
                    })?;
                self.insert(dst, value)
            }
            LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start,
                index_start,
                count,
            } => {
                let captures = self.conditional_captures.ok_or_else(|| {
                    CompileError::Backend(
                        "invalid function-conditional capture range load".to_string(),
                    )
                })?;
                let mut output = None;
                for offset in 0..count {
                    let value =
                        captures
                            .get(&(index_start + offset))
                            .copied()
                            .ok_or_else(|| {
                                CompileError::Backend(
                                    "invalid function-conditional capture range load".to_string(),
                                )
                            })?;
                    output = self.insert(
                        checked_reg_offset(
                            dst_start,
                            offset,
                            "function conditional capture range destination",
                        )?,
                        value,
                    )?;
                }
                Ok(output)
            }
            LinearOp::Move { dst, src } => {
                let value = self.lookup(src)?;
                if let Some(constant) = self.known_constants.get(&src).copied() {
                    self.known_constants.insert(dst, constant);
                }
                self.insert(dst, value)
            }
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                component,
            } => self.lower_linear_solve_component(dst, matrix_start, rhs_start, n, component),
            LinearOp::DotProduct {
                dst,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
            } => self.lower_dot_product(dst, lhs_start, rhs_start, count, lhs_stride, rhs_stride),
            LinearOp::MatrixMultiply {
                dst_start,
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes,
            } => self.lower_matrix_multiply(
                dst_start, lhs_start, rhs_start, rows, inner, columns, lanes,
            ),
            LinearOp::TensorBinary {
                dst_start,
                op,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                lanes,
            } => self.lower_tensor_binary(
                dst_start, op, lhs_start, rhs_start, count, lhs_stride, rhs_stride, lanes,
            ),
            LinearOp::TensorCross {
                dst_start,
                lhs_start,
                rhs_start,
                lanes,
            } => self.lower_tensor_cross(dst_start, lhs_start, rhs_start, lanes),
            LinearOp::TensorTranspose {
                dst_start,
                src_start,
                rows,
                columns,
                element_width,
                lanes,
            } => self.lower_tensor_transpose(
                dst_start,
                src_start,
                rows,
                columns,
                element_width,
                lanes,
            ),
            LinearOp::TensorConcatenate {
                dst_start,
                sources,
                dimensions,
                axis,
                lanes,
            } => self.lower_tensor_concatenate(dst_start, &sources, &dimensions, axis, lanes),
            LinearOp::TensorUpdate {
                dst_start,
                base_start,
                value_start,
                dimensions,
                subscripts,
                lanes,
            } => self.lower_tensor_update(
                dst_start,
                base_start,
                value_start,
                &dimensions,
                &subscripts,
                lanes,
            ),
            LinearOp::TensorFill {
                dst_start,
                value_start,
                count,
                lanes,
            } => self.lower_tensor_fill(dst_start, value_start, count, lanes),
            LinearOp::TensorIdentity {
                dst_start,
                size,
                lanes,
            } => self.lower_tensor_identity(dst_start, size, lanes),
            LinearOp::TensorLoad {
                dst_start,
                input,
                input_start,
                count,
                seed_start,
                lanes,
            } => self.lower_tensor_load(dst_start, input, input_start, count, seed_start, lanes),
            LinearOp::TableBounds { dst, table_id, max } => {
                self.lower_table_bounds(dst, table_id, max)
            }
            LinearOp::TableLookup {
                dst,
                table_id,
                column,
                input,
            } => self.lower_table_lookup(dst, table_id, column, input, TableHostFn::Lookup),
            LinearOp::TableLookupSlope {
                dst,
                table_id,
                column,
                input,
            } => self.lower_table_lookup(dst, table_id, column, input, TableHostFn::LookupSlope),
            LinearOp::TableNextEvent {
                dst,
                table_id,
                time,
            } => self.lower_table_next_event(dst, table_id, time),
            LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. } => Err(CompileError::Backend(
                "cranelift row compiler does not support discrete random solve-IR ops".to_string(),
            )),
            LinearOp::Unary { dst, op, arg } => {
                let x = self.lookup(arg)?;
                let value = emit_unary_op(self.fb, self.module, self.math, op, x)?;
                if let Some(constant) = self.known_constants.get(&arg).copied()
                    && let Some(result) = fold_unary_constant(op, constant)
                {
                    self.known_constants.insert(dst, result);
                }
                self.insert(dst, value)
            }
            LinearOp::Binary { dst, op, lhs, rhs } => {
                let l = self.lookup(lhs)?;
                let r = self.lookup(rhs)?;
                let value = emit_binary_op(self.fb, self.module, self.math, op, l, r)?;
                if let (Some(lhs), Some(rhs)) = (
                    self.known_constants.get(&lhs).copied(),
                    self.known_constants.get(&rhs).copied(),
                ) && let Some(result) = fold_binary_constant(op, lhs, rhs)
                {
                    self.known_constants.insert(dst, result);
                }
                self.insert(dst, value)
            }
            LinearOp::Compare { dst, op, lhs, rhs } => {
                let l = self.lookup(lhs)?;
                let r = self.lookup(rhs)?;
                let cond = emit_compare_op(self.fb, op, l, r);
                let value = bool_to_f64(self.fb, cond);
                self.insert(dst, value)
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                let condition = self.lookup(cond)?;
                let when_true = self.lookup(if_true)?;
                let when_false = self.lookup(if_false)?;
                let zero = self.fb.ins().f64const(0.0);
                let is_true = self.fb.ins().fcmp(FloatCC::NotEqual, condition, zero);
                let value = self.fb.ins().select(is_true, when_true, when_false);
                self.insert(dst, value)
            }
            LinearOp::FunctionFold {
                dst_start,
                initial_start,
                capture_start,
                program,
            } => self.lower_function_fold(dst_start, initial_start, capture_start, &program),
            LinearOp::GuardedFunctionFold {
                dst_start,
                initial_start,
                capture_start,
                activation,
                program,
            } => self.lower_guarded_function_fold(
                dst_start,
                initial_start,
                capture_start,
                activation,
                &program,
            ),
            LinearOp::FunctionConditional {
                dst_start,
                capture_start,
                program,
            } => self.lower_function_conditional(dst_start, capture_start, &program),
            LinearOp::PureCall {
                dst_start,
                input_starts,
                site,
            } => self.lower_pure_call(dst_start, &input_starts, &site),
            LinearOp::PureCallDirectional {
                dst_start,
                input_starts,
                site,
            } => self.lower_pure_call_directional(dst_start, &input_starts, &site),
            LinearOp::StoreOutputFoldTensorUpdate { .. }
            | LinearOp::StoreOutputFunctionFold { .. } => Err(CompileError::Backend(
                "aggregate fold output escaped function-fold lowering".to_string(),
            )),
            LinearOp::StoreOutputRange { .. } => Err(CompileError::Backend(
                "output range escaped its owning program boundary".to_string(),
            )),
            LinearOp::StoreOutput { src } => Ok(Some(self.lookup(src)?)),
        }
    }

    fn lower_function_fold(
        &mut self,
        dst_start: u32,
        initial_start: u32,
        capture_start: u32,
        program: &Arc<rumoca_ir_solve::FunctionFoldProgram>,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let carried = (0..program.carried_count)
            .map(|offset| {
                checked_reg_offset(initial_start, offset, "function fold initial")
                    .and_then(|register| self.lookup(register))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let captures = (0..program.capture_count)
            .map(|offset| {
                checked_reg_offset(capture_start, offset, "function fold capture")
                    .and_then(|register| self.lookup(register))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let program_key = self
            .fold_functions
            .get(&(Arc::as_ptr(program) as usize))
            .copied()
            .ok_or_else(|| {
                CompileError::Backend("function-fold kernel was not precompiled".to_string())
            })?;
        let call_key = NestedFoldCallKey {
            program: program_key,
            initial: vec![NestedFoldInitialKey::Registers(
                carried.clone().into_boxed_slice(),
            )]
            .into_boxed_slice(),
            captures: captures.clone().into_boxed_slice(),
            guard: None,
        };
        let carried_slot = if let Some(cached) = self.nested_fold_results.get(&call_key).copied() {
            cached
        } else {
            let carried_bytes = program
                .carried_count
                .checked_mul(std::mem::size_of::<f64>())
                .and_then(|bytes| u32::try_from(bytes).ok())
                .ok_or_else(|| {
                    CompileError::Backend("function-fold carried storage size overflow".to_string())
                })?;
            let carried_slot = self.fb.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                carried_bytes,
                3,
            ));
            for (offset, value) in carried.iter().copied().enumerate() {
                self.fb.ins().stack_store(
                    value,
                    carried_slot,
                    stack_element_byte_offset(offset, "function-fold initial")?,
                );
            }
            self.call_fold_kernel(program, carried_slot, &captures)?;
            self.nested_fold_results.insert(call_key, carried_slot);
            carried_slot
        };
        for offset in 0..program.carried_count {
            let value = self.fb.ins().stack_load(
                types::F64,
                carried_slot,
                stack_element_byte_offset(offset, "function-fold result")?,
            );
            self.insert(
                checked_reg_offset(dst_start, offset, "function fold destination")?,
                value,
            )?;
        }
        Ok(None)
    }

    fn lower_pure_call(
        &mut self,
        dst_start: u32,
        input_starts: &[u32],
        site: &rumoca_ir_solve::SolvePureCallSite,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let import = self
            .pure_call_functions
            .get(&site.owner())
            .cloned()
            .ok_or_else(|| {
                CompileError::Backend(format!(
                    "typed pure-call owner {} has no native helper",
                    site.owner().index()
                ))
            })?;
        if input_starts.len() != site.inputs().len()
            || import.inputs.as_ref() != site.inputs()
            || import.outputs.as_ref() != site.outputs()
        {
            return Err(CompileError::Backend(
                "typed pure-call native interface mismatch".to_string(),
            ));
        }
        self.lower_typed_pure_call(
            dst_start,
            input_starts,
            site.owner(),
            site.inputs(),
            site.outputs(),
            import.function,
            false,
        )
    }

    fn lower_pure_call_directional(
        &mut self,
        dst_start: u32,
        input_starts: &[u32],
        site: &rumoca_ir_solve::SolvePureCallDirectionalSite,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let import = self
            .pure_call_functions
            .get(&site.owner())
            .cloned()
            .ok_or_else(|| {
                CompileError::Backend(format!(
                    "typed directional owner {} has no native helper",
                    site.owner().index()
                ))
            })?;
        let directional = import.directional.ok_or_else(|| {
            CompileError::Backend(format!(
                "typed owner {} has no directional native helper",
                site.owner().index()
            ))
        })?;
        if input_starts.len() != site.inputs().len()
            || directional.inputs.as_ref() != site.inputs()
            || directional.outputs.as_ref() != site.outputs()
        {
            return Err(CompileError::Backend(
                "typed directional native interface mismatch".to_string(),
            ));
        }
        self.lower_typed_pure_call(
            dst_start,
            input_starts,
            site.owner(),
            site.inputs(),
            site.outputs(),
            directional.function,
            true,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_typed_pure_call(
        &mut self,
        dst_start: u32,
        input_starts: &[u32],
        owner: rumoca_ir_solve::SolvePureCallOwnerId,
        inputs: &[rumoca_ir_solve::SolveValueType],
        outputs: &[rumoca_ir_solve::SolvePureCallOutput],
        function: FuncId,
        directional: bool,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let cache_key = PureCallResultKey {
            owner,
            directional,
            inputs: input_starts.into(),
        };
        if let Some(output_slot) = self.pure_call_results.get(&cache_key).copied() {
            self.load_typed_call_results(dst_start, outputs, output_slot)?;
            return Ok(None);
        }
        let input_cells = inputs.iter().try_fold(0usize, |count, value_type| {
            count.checked_add(value_type.scalar_count() as usize)
        });
        let output_cells = outputs.iter().try_fold(0usize, |count, output| {
            count.checked_add(output.value_type().scalar_count() as usize)
        });
        let input_cells = input_cells
            .ok_or_else(|| CompileError::Backend("typed pure-call input width overflows".into()))?;
        let output_cells = output_cells.ok_or_else(|| {
            CompileError::Backend("typed pure-call output width overflows".into())
        })?;
        let input_slot = create_typed_cell_slot(self.fb, input_cells, "pure-call input")?;
        let output_slot = create_typed_cell_slot(self.fb, output_cells, "pure-call output")?;
        let pointer_type = self.module.target_config().pointer_type();
        let input_ptr = self.fb.ins().stack_addr(pointer_type, input_slot, 0);
        let output_ptr = self.fb.ins().stack_addr(pointer_type, output_slot, 0);
        let mut input_cell = 0usize;
        for (start, value_type) in input_starts.iter().zip(inputs) {
            for offset in 0..value_type.scalar_count() as usize {
                let register = checked_reg_offset(*start, offset, "typed pure-call input")?;
                let value = self.lookup(register)?;
                let value = encode_typed_cell(self.fb, value_type.element_type(), value);
                self.fb.ins().stack_store(
                    value,
                    input_slot,
                    stack_element_byte_offset(input_cell, "typed pure-call input")?,
                );
                input_cell += 1;
            }
        }
        let function = self.module.declare_func_in_func(function, self.fb.func);
        self.fb.ins().call(function, &[input_ptr, output_ptr]);
        self.pure_call_results.insert(cache_key, output_slot);
        self.load_typed_call_results(dst_start, outputs, output_slot)?;
        Ok(None)
    }

    fn load_typed_call_results(
        &mut self,
        dst_start: u32,
        outputs: &[rumoca_ir_solve::SolvePureCallOutput],
        output_slot: StackSlot,
    ) -> Result<(), CompileError> {
        let mut output_cell = 0usize;
        for output in outputs {
            for _ in 0..output.value_type().scalar_count() {
                let value = self.fb.ins().stack_load(
                    typed_cell_type(output.value_type().element_type()),
                    output_slot,
                    stack_element_byte_offset(output_cell, "typed pure-call output")?,
                );
                let value = decode_typed_cell(self.fb, output.value_type().element_type(), value);
                self.insert(
                    checked_reg_offset(dst_start, output_cell, "typed pure-call output")?,
                    value,
                )?;
                output_cell += 1;
            }
        }
        Ok(())
    }

    fn lower_function_conditional(
        &mut self,
        dst_start: u32,
        capture_start: u32,
        program: &Arc<rumoca_ir_solve::FunctionConditionalProgram>,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        if let Some(owner) = program.owner
            && let Some(result_slot) = self.conditional_results.get(&owner).copied()
        {
            self.load_function_conditional_results(dst_start, result_slot, program.result_count)?;
            return Ok(None);
        }
        let result_slot = if let Some(owner) = program.owner {
            let function = self
                .conditional_functions
                .get(&owner)
                .copied()
                .ok_or_else(|| {
                    CompileError::Backend(format!(
                        "function-conditional owner {} was not precompiled",
                        owner.get()
                    ))
                })?;
            self.call_function_conditional(function, capture_start, program)?
        } else {
            self.lower_function_conditional_inline(capture_start, program)?
        };
        if let Some(owner) = program.owner {
            self.conditional_results.insert(owner, result_slot);
        }
        self.load_function_conditional_results(dst_start, result_slot, program.result_count)?;
        Ok(None)
    }

    fn lower_function_conditional_inline(
        &mut self,
        capture_start: u32,
        program: &rumoca_ir_solve::FunctionConditionalProgram,
    ) -> Result<StackSlot, CompileError> {
        let result_bytes = program
            .result_count
            .checked_mul(std::mem::size_of::<f64>())
            .and_then(|bytes| u32::try_from(bytes).ok())
            .ok_or_else(|| {
                CompileError::Backend(
                    "function-conditional result storage size overflow".to_string(),
                )
            })?;
        let result_slot = self.fb.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            result_bytes,
            3,
        ));
        let continuation = self.fb.create_block();
        for arm in &program.arms {
            let condition = self.lower_function_conditional_condition(
                &arm.condition,
                capture_start,
                program.capture_count,
            )?;
            let selected = self.fb.create_block();
            let next = self.fb.create_block();
            let zero = self.fb.ins().f64const(0.0);
            let condition = self.fb.ins().fcmp(FloatCC::NotEqual, condition, zero);
            self.fb.ins().brif(condition, selected, &[], next, &[]);

            self.fb.switch_to_block(selected);
            self.fb.seal_block(selected);
            let values = self.lower_function_conditional_region(
                &arm.result,
                capture_start,
                program.capture_count,
            )?;
            self.store_function_conditional_results(result_slot, &values, program.result_count)?;
            self.fb.ins().jump(continuation, &[]);

            self.fb.switch_to_block(next);
            self.fb.seal_block(next);
        }
        let fallback = self.lower_function_conditional_region(
            &program.fallback,
            capture_start,
            program.capture_count,
        )?;
        self.store_function_conditional_results(result_slot, &fallback, program.result_count)?;
        self.fb.ins().jump(continuation, &[]);

        self.fb.switch_to_block(continuation);
        self.fb.seal_block(continuation);
        Ok(result_slot)
    }

    fn call_function_conditional(
        &mut self,
        function: FuncId,
        capture_start: u32,
        program: &rumoca_ir_solve::FunctionConditionalProgram,
    ) -> Result<StackSlot, CompileError> {
        let captures_ptr = if let Some(registers) = self.backing_regs_ptr {
            let offset = i64::from(register_byte_offset(capture_start)?);
            self.fb.ins().iadd_imm(registers, offset)
        } else {
            let capture_bytes = program
                .capture_count
                .max(1)
                .checked_mul(std::mem::size_of::<f64>())
                .and_then(|bytes| u32::try_from(bytes).ok())
                .ok_or_else(|| {
                    CompileError::Backend(
                        "function-conditional capture storage size overflow".to_string(),
                    )
                })?;
            let captures = self.fb.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                capture_bytes,
                3,
            ));
            for offset in 0..program.capture_count {
                let value = self.lookup(checked_reg_offset(
                    capture_start,
                    offset,
                    "function conditional capture",
                )?)?;
                self.fb.ins().stack_store(
                    value,
                    captures,
                    stack_element_byte_offset(offset, "function-conditional capture")?,
                );
            }
            self.fb.ins().stack_addr(types::I64, captures, 0)
        };
        let result_bytes = program
            .result_count
            .checked_mul(std::mem::size_of::<f64>())
            .and_then(|bytes| u32::try_from(bytes).ok())
            .ok_or_else(|| {
                CompileError::Backend(
                    "function-conditional result storage size overflow".to_string(),
                )
            })?;
        let results = self.fb.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            result_bytes,
            3,
        ));
        let results_ptr = self.fb.ins().stack_addr(types::I64, results, 0);
        let function = self.module.declare_func_in_func(function, self.fb.func);
        let mut arguments = vec![self.y_ptr, self.p_ptr, self.t_value];
        if let Some(seed) = self.v_ptr {
            arguments.push(seed);
        }
        arguments.extend([captures_ptr, results_ptr]);
        self.fb.ins().call(function, &arguments);
        Ok(results)
    }

    fn load_function_conditional_results(
        &mut self,
        dst_start: u32,
        result_slot: StackSlot,
        result_count: usize,
    ) -> Result<(), CompileError> {
        if let Some(registers) = self.backing_regs_ptr {
            let source = self.fb.ins().stack_addr(types::I64, result_slot, 0);
            let destination_offset = i64::from(register_byte_offset(dst_start)?);
            let destination = self.fb.ins().iadd_imm(registers, destination_offset);
            return self.copy_pointer_range(source, destination, result_count);
        }
        for offset in 0..result_count {
            let value = self.fb.ins().stack_load(
                types::F64,
                result_slot,
                stack_element_byte_offset(offset, "function-conditional result")?,
            );
            self.insert(
                checked_reg_offset(dst_start, offset, "function conditional destination")?,
                value,
            )?;
        }
        Ok(())
    }

    fn lower_function_conditional_region(
        &mut self,
        region: &[LinearOp],
        capture_start: u32,
        capture_count: usize,
    ) -> Result<Vec<LoweredConditionalOutput>, CompileError> {
        // Place parent-register loads in this region's native block.  Loading
        // the union before the first condition would make all branch captures
        // eager and defeats the checked conditional's lazy semantics.
        let captures =
            self.load_function_conditional_captures(region, capture_start, capture_count)?;
        let mut registers = HashMap::new();
        // Conditional regions own an isolated register namespace.  A parent
        // row tape is sized only for the parent's registers, so reusing it for
        // region-local registers can write beyond the stack slot.  Allocate a
        // region tape only when this exact region's final lowering needs one.
        let pointer_type = self.module.target_config().pointer_type();
        let backing_regs_ptr = create_row_register_tape(self.fb, pointer_type, region)?;
        let mut nested = RowLowerCtx {
            fb: self.fb,
            module: self.module,
            math: self.math,
            regs: &mut registers,
            y_ptr: self.y_ptr,
            p_ptr: self.p_ptr,
            t_value: self.t_value,
            v_ptr: self.v_ptr,
            backing_regs_ptr,
            flags: self.flags,
            loaded_y: None,
            loaded_p: None,
            fold_carried: self.fold_carried,
            fold_indices: self.fold_indices,
            fold_index_constants: self.fold_index_constants,
            fold_captures: self.fold_captures,
            fold_captures_ptr: self.fold_captures_ptr,
            conditional_captures: Some(&captures),
            conditional_captures_ptr: self.conditional_captures_ptr,
            fold_functions: self.fold_functions,
            conditional_functions: self.conditional_functions,
            pure_call_functions: self.pure_call_functions,
            // Conditional regions own isolated register namespaces, so their
            // register-number call keys cannot reuse parent-row results.
            pure_call_results: HashMap::new(),
            nested_fold_results: HashMap::new(),
            conditional_results: self.conditional_results.clone(),
            fold_carried_versions: self.fold_carried_versions.clone(),
            known_constants: HashMap::new(),
        };
        nested.lower_conditional_region_outputs(region)
    }

    fn load_function_conditional_captures(
        &mut self,
        region: &[LinearOp],
        capture_start: u32,
        capture_count: usize,
    ) -> Result<HashMap<usize, cranelift_codegen::ir::Value>, CompileError> {
        let mut captures = HashMap::new();
        for operation in region {
            let Some((start, count)) = conditional_capture_range(operation) else {
                continue;
            };
            let end = start
                .checked_add(count)
                .filter(|end| *end <= capture_count)
                .ok_or_else(|| {
                    CompileError::Backend(
                        "function-conditional capture range exceeds checked ABI".to_string(),
                    )
                })?;
            self.load_function_conditional_capture_range(&mut captures, capture_start, start..end)?;
        }
        Ok(captures)
    }

    fn load_function_conditional_capture_range(
        &mut self,
        captures: &mut HashMap<usize, cranelift_codegen::ir::Value>,
        capture_start: u32,
        range: std::ops::Range<usize>,
    ) -> Result<(), CompileError> {
        for index in range {
            if captures.contains_key(&index) {
                continue;
            }
            let value = self.load_function_conditional_capture(capture_start, index)?;
            captures.insert(index, value);
        }
        Ok(())
    }

    fn load_function_conditional_capture(
        &mut self,
        capture_start: u32,
        index: usize,
    ) -> Result<cranelift_codegen::ir::Value, CompileError> {
        if let Some(pointer) = self.conditional_captures_ptr {
            return load_f64(self.fb, self.flags, pointer, index);
        }
        let register = checked_reg_offset(capture_start, index, "function conditional capture")?;
        self.lookup(register)
    }

    fn lower_conditional_region_outputs(
        &mut self,
        region: &[LinearOp],
    ) -> Result<Vec<LoweredConditionalOutput>, CompileError> {
        let mut outputs = Vec::new();
        for operation in region.iter().cloned() {
            self.lower_conditional_region_output(operation, &mut outputs)?;
        }
        Ok(outputs)
    }

    fn lower_conditional_region_output(
        &mut self,
        operation: LinearOp,
        outputs: &mut Vec<LoweredConditionalOutput>,
    ) -> Result<(), CompileError> {
        let LinearOp::StoreOutputRange {
            start,
            count,
            stride,
        } = operation
        else {
            if let Some(value) = self.lower_op(operation)? {
                outputs.push(LoweredConditionalOutput::Value(value));
            }
            return Ok(());
        };
        if let Some(pointer) = self.backing_regs_ptr {
            outputs.push(LoweredConditionalOutput::TapeRange {
                pointer,
                start,
                count,
                stride,
            });
        } else {
            outputs.extend(
                self.lower_output_range(start, count, stride)?
                    .into_iter()
                    .map(LoweredConditionalOutput::Value),
            );
        }
        Ok(())
    }

    fn lower_function_conditional_condition(
        &mut self,
        region: &[LinearOp],
        capture_start: u32,
        capture_count: usize,
    ) -> Result<cranelift_codegen::ir::Value, CompileError> {
        let outputs =
            self.lower_function_conditional_region(region, capture_start, capture_count)?;
        let [output] = outputs.as_slice() else {
            return Err(CompileError::Backend(
                "function-conditional condition output count mismatch".to_string(),
            ));
        };
        match *output {
            LoweredConditionalOutput::Value(value) => Ok(value),
            LoweredConditionalOutput::TapeRange {
                pointer,
                start,
                count: 1,
                ..
            } => Ok(self.fb.ins().load(
                types::F64,
                self.flags,
                pointer,
                register_byte_offset(start)?,
            )),
            LoweredConditionalOutput::TapeRange { .. } => Err(CompileError::Backend(
                "function-conditional condition range is not scalar".to_string(),
            )),
        }
    }

    fn store_function_conditional_results(
        &mut self,
        result_slot: StackSlot,
        values: &[LoweredConditionalOutput],
        expected: usize,
    ) -> Result<(), CompileError> {
        let actual = values.iter().try_fold(0usize, |count, value| {
            count.checked_add(match value {
                LoweredConditionalOutput::Value(_) => 1,
                LoweredConditionalOutput::TapeRange { count, .. } => *count,
            })
        });
        if actual != Some(expected) {
            return Err(CompileError::Backend(
                "function-conditional result output count mismatch".to_string(),
            ));
        }
        let mut output = 0usize;
        for value in values {
            output = self.store_function_conditional_output(result_slot, value, output)?;
        }
        Ok(())
    }

    fn store_function_conditional_output(
        &mut self,
        result_slot: StackSlot,
        value: &LoweredConditionalOutput,
        mut output: usize,
    ) -> Result<usize, CompileError> {
        match *value {
            LoweredConditionalOutput::Value(value) => {
                self.fb.ins().stack_store(
                    value,
                    result_slot,
                    stack_element_byte_offset(output, "function-conditional result")?,
                );
                output += 1;
            }
            LoweredConditionalOutput::TapeRange {
                pointer,
                start,
                count,
                stride: 1,
            } => {
                let source_offset = i64::from(register_byte_offset(start)?);
                let source = self.fb.ins().iadd_imm(pointer, source_offset);
                let destination = self.fb.ins().stack_addr(
                    types::I64,
                    result_slot,
                    stack_element_byte_offset(output, "function-conditional result")?,
                );
                self.copy_pointer_range(source, destination, count)?;
                output += count;
            }
            LoweredConditionalOutput::TapeRange {
                pointer,
                start,
                count,
                stride,
            } => {
                for ordinal in 0..count {
                    let source = checked_strided_register(
                        start,
                        ordinal,
                        stride,
                        "function-conditional output",
                    )?;
                    let value = self.fb.ins().load(
                        types::F64,
                        self.flags,
                        pointer,
                        register_byte_offset(source)?,
                    );
                    self.fb.ins().stack_store(
                        value,
                        result_slot,
                        stack_element_byte_offset(output, "function-conditional result")?,
                    );
                    output += 1;
                }
            }
        }
        Ok(output)
    }

    fn lower_guarded_function_fold(
        &mut self,
        dst_start: u32,
        initial_start: u32,
        capture_start: u32,
        activation: u32,
        program: &Arc<rumoca_ir_solve::FunctionFoldProgram>,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let carried = (0..program.carried_count)
            .map(|offset| {
                checked_reg_offset(initial_start, offset, "guarded function fold initial")
                    .and_then(|register| self.lookup(register))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let captures = (0..program.capture_count)
            .map(|offset| {
                checked_reg_offset(capture_start, offset, "guarded function fold capture")
                    .and_then(|register| self.lookup(register))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let activation = self.lookup(activation)?;
        let carried_bytes = program
            .carried_count
            .checked_mul(std::mem::size_of::<f64>())
            .and_then(|bytes| u32::try_from(bytes).ok())
            .ok_or_else(|| {
                CompileError::Backend(
                    "guarded function-fold carried storage size overflow".to_string(),
                )
            })?;
        let carried_slot = self.fb.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            carried_bytes,
            3,
        ));
        for (offset, value) in carried.iter().copied().enumerate() {
            self.fb.ins().stack_store(
                value,
                carried_slot,
                stack_element_byte_offset(offset, "guarded function-fold initial")?,
            );
        }

        // The Solve IR retains the activation and tensor fold as first-class
        // objects. This is their final native control-flow boundary: the fold
        // kernel is absent from the inactive execution path, while the shared
        // carried slot makes the inactive result exactly the initial tuple.
        let execute = self.fb.create_block();
        let continuation = self.fb.create_block();
        let zero = self.fb.ins().f64const(0.0);
        let active = self.fb.ins().fcmp(FloatCC::NotEqual, activation, zero);
        self.fb.ins().brif(active, execute, &[], continuation, &[]);

        self.fb.switch_to_block(execute);
        self.fb.seal_block(execute);
        self.call_fold_kernel(program, carried_slot, &captures)?;
        self.fb.ins().jump(continuation, &[]);

        self.fb.switch_to_block(continuation);
        self.fb.seal_block(continuation);
        for offset in 0..program.carried_count {
            let value = self.fb.ins().stack_load(
                types::F64,
                carried_slot,
                stack_element_byte_offset(offset, "guarded function-fold result")?,
            );
            self.insert(
                checked_reg_offset(dst_start, offset, "guarded function fold destination")?,
                value,
            )?;
        }
        Ok(None)
    }

    fn lower_function_fold_on_stack(
        &mut self,
        carried_slot: StackSlot,
        captures: &[cranelift_codegen::ir::Value],
        captures_ptr: Option<cranelift_codegen::ir::Value>,
        program: &rumoca_ir_solve::FunctionFoldProgram,
    ) -> Result<(), CompileError> {
        let count = program.domain.scalar_count().map_err(|error| {
            CompileError::Backend(format!("invalid function-fold domain: {error}"))
        })?;
        let extents = program.domain.extents().map_err(|error| {
            CompileError::Backend(format!("invalid function-fold domain: {error}"))
        })?;
        let strides = program.domain.ordinal_strides().map_err(|error| {
            CompileError::Backend(format!("invalid function-fold domain: {error}"))
        })?;
        // Shape is still compact in Solve IR; this is the final native-code
        // emission boundary.  Tiny static domains are cheaper as straight-line
        // code even when runtime indexing requires a register tape: the tape
        // remains addressable, while we eliminate loop control and integer
        // div/rem from every estimator matrix reduction.
        if count <= INLINE_FIXED_FOLD_POINT_LIMIT && program.update.len() <= 128 {
            for ordinal in 0..count {
                let constants = fold_index_constants(&program.domain, &extents, &strides, ordinal);
                let indices = constants
                    .iter()
                    .map(|&coordinate| self.fb.ins().f64const(coordinate))
                    .collect::<Vec<_>>();
                self.lower_function_fold_iteration(
                    carried_slot,
                    captures,
                    captures_ptr,
                    program,
                    &indices,
                    Some(&constants),
                )?;
            }
            return Ok(());
        }
        let header = self.fb.create_block();
        let body = self.fb.create_block();
        let exit = self.fb.create_block();
        self.fb.append_block_param(header, types::I64);
        let zero = self.fb.ins().iconst(types::I64, 0);
        self.fb.ins().jump(header, &[zero.into()]);

        self.fb.switch_to_block(header);
        let ordinal = self.fb.block_params(header)[0];
        let in_range = self.fb.ins().icmp_imm(
            IntCC::UnsignedLessThan,
            ordinal,
            i64::try_from(count).map_err(|_| {
                CompileError::Backend("function-fold domain count exceeds i64".to_string())
            })?,
        );
        self.fb.ins().brif(in_range, body, &[], exit, &[]);

        self.fb.switch_to_block(body);
        self.fb.seal_block(body);
        let mut indices = Vec::with_capacity(program.domain.binders.len());
        for ((binder, extent), stride) in program.domain.binders.iter().zip(extents).zip(strides) {
            let position = if stride == 1 {
                ordinal
            } else {
                self.fb.ins().udiv_imm(ordinal, stride as i64)
            };
            let position = if extent == 1 {
                self.fb.ins().iconst(types::I64, 0)
            } else {
                self.fb.ins().urem_imm(position, extent as i64)
            };
            let scaled = self.fb.ins().imul_imm(position, binder.step);
            let coordinate = self.fb.ins().iadd_imm(scaled, binder.lower);
            indices.push(self.fb.ins().fcvt_from_sint(types::F64, coordinate));
        }
        self.lower_function_fold_iteration(
            carried_slot,
            captures,
            captures_ptr,
            program,
            &indices,
            None,
        )?;
        let next_ordinal = self.fb.ins().iadd_imm(ordinal, 1);
        self.fb.ins().jump(header, &[next_ordinal.into()]);
        self.fb.seal_block(header);

        self.fb.switch_to_block(exit);
        self.fb.seal_block(exit);
        Ok(())
    }

    fn lower_function_fold_iteration(
        &mut self,
        carried_slot: StackSlot,
        captures: &[cranelift_codegen::ir::Value],
        captures_ptr: Option<cranelift_codegen::ir::Value>,
        program: &rumoca_ir_solve::FunctionFoldProgram,
        indices: &[cranelift_codegen::ir::Value],
        index_constants: Option<&[f64]>,
    ) -> Result<(), CompileError> {
        let mut update_regs = HashMap::new();
        let mut update = RowLowerCtx {
            fb: self.fb,
            module: self.module,
            math: self.math,
            regs: &mut update_regs,
            y_ptr: self.y_ptr,
            p_ptr: self.p_ptr,
            t_value: self.t_value,
            v_ptr: self.v_ptr,
            backing_regs_ptr: self.backing_regs_ptr,
            flags: self.flags,
            loaded_y: None,
            loaded_p: None,
            fold_carried: Some(carried_slot),
            fold_indices: Some(indices),
            fold_index_constants: index_constants,
            fold_captures: Some(captures),
            fold_captures_ptr: captures_ptr,
            conditional_captures: self.conditional_captures,
            conditional_captures_ptr: self.conditional_captures_ptr,
            fold_functions: self.fold_functions,
            conditional_functions: self.conditional_functions,
            pure_call_functions: self.pure_call_functions,
            pure_call_results: HashMap::new(),
            nested_fold_results: HashMap::new(),
            conditional_results: HashMap::new(),
            fold_carried_versions: vec![0; program.carried_count],
            known_constants: HashMap::new(),
        };
        let output_ops = update.lower_fold_update_body(&program.update)?;
        let mut output_cursor = 0usize;
        for operation in output_ops {
            let output_start = output_cursor;
            output_cursor = update.lower_fold_output(carried_slot, output_cursor, operation)?;
            for version in &mut update.fold_carried_versions[output_start..output_cursor] {
                *version = version.wrapping_add(1);
            }
        }
        drop(update);
        if output_cursor != program.carried_count {
            return Err(CompileError::Backend(
                "function-fold update output count mismatch".to_string(),
            ));
        }
        Ok(())
    }

    fn lower_fold_update_body(
        &mut self,
        operations: &[LinearOp],
    ) -> Result<Vec<LinearOp>, CompileError> {
        let mut outputs = Vec::new();
        for operation in operations.iter().cloned() {
            if is_fold_output(&operation) {
                outputs.push(operation);
            } else {
                self.lower_op(operation)?;
            }
        }
        Ok(outputs)
    }

    fn lower_fold_output(
        &mut self,
        carried: StackSlot,
        cursor: usize,
        operation: LinearOp,
    ) -> Result<usize, CompileError> {
        match operation {
            LinearOp::StoreOutput { src } => {
                let value = self.lookup(src)?;
                self.fb.ins().stack_store(
                    value,
                    carried,
                    stack_element_byte_offset(cursor, "function-fold update")?,
                );
                checked_fold_output_cursor(cursor, 1)
            }
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                let values = self.lower_output_range(start, count, stride)?;
                for (offset, value) in values.into_iter().enumerate() {
                    self.fb.ins().stack_store(
                        value,
                        carried,
                        stack_element_byte_offset(cursor + offset, "function-fold update")?,
                    );
                }
                checked_fold_output_cursor(cursor, count)
            }
            LinearOp::StoreOutputFoldTensorUpdate {
                source_base,
                source_stride,
                dimensions,
                updates,
                nodes,
                result,
                lanes,
            } => {
                self.lower_fold_tensor_update(
                    carried,
                    cursor,
                    source_base,
                    source_stride,
                    &dimensions,
                    &updates,
                    &nodes,
                    result,
                    lanes,
                )?;
                checked_fold_output_cursor(cursor, tensor_output_count(&dimensions, lanes)?)
            }
            LinearOp::StoreOutputFunctionFold {
                initial,
                capture_start,
                program,
                result_base,
                count,
                condition,
                nested_when_true,
            } => {
                let request = NestedFoldOutput {
                    parent_carried: carried,
                    output_base: cursor,
                    initial: &initial,
                    capture_start,
                    program: &program,
                    result_base,
                    result_count: count,
                    condition,
                    nested_when_true,
                };
                self.lower_nested_fold_output(request)?;
                checked_fold_output_cursor(cursor, count)
            }
            _ => unreachable!("fold outputs are filtered before dispatch"),
        }
    }

    fn call_fold_kernel(
        &mut self,
        program: &Arc<rumoca_ir_solve::FunctionFoldProgram>,
        carried: StackSlot,
        captures: &[cranelift_codegen::ir::Value],
    ) -> Result<(), CompileError> {
        let key = Arc::as_ptr(program) as usize;
        let function = self.fold_functions.get(&key).copied().ok_or_else(|| {
            CompileError::Backend(format!(
                "function-fold kernel {key:#x} was not precompiled ({} kernels available)",
                self.fold_functions.len()
            ))
        })?;
        let capture_bytes = captures
            .len()
            .max(1)
            .checked_mul(std::mem::size_of::<f64>())
            .and_then(|bytes| u32::try_from(bytes).ok())
            .ok_or_else(|| CompileError::Backend("fold capture size overflow".to_string()))?;
        let capture_slot = self.fb.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            capture_bytes,
            3,
        ));
        for (offset, value) in captures.iter().copied().enumerate() {
            self.fb.ins().stack_store(
                value,
                capture_slot,
                stack_element_byte_offset(offset, "fold capture")?,
            );
        }
        let carried_ptr = self.fb.ins().stack_addr(types::I64, carried, 0);
        let captures_ptr = self.fb.ins().stack_addr(types::I64, capture_slot, 0);
        let function = self.module.declare_func_in_func(function, self.fb.func);
        let mut arguments = vec![self.y_ptr, self.p_ptr, self.t_value];
        if let Some(seed) = self.v_ptr {
            arguments.push(seed);
        }
        arguments.extend([carried_ptr, captures_ptr]);
        self.fb.ins().call(function, &arguments);
        Ok(())
    }

    fn lower_nested_fold_output(
        &mut self,
        request: NestedFoldOutput<'_>,
    ) -> Result<(), CompileError> {
        let NestedFoldOutput {
            parent_carried,
            output_base,
            initial,
            capture_start,
            program,
            result_base,
            result_count,
            condition,
            nested_when_true,
        } = request;
        let initial_key = self.nested_fold_initial_key(initial)?;
        let captures = (0..program.capture_count)
            .map(|offset| {
                checked_reg_offset(capture_start, offset, "nested function fold capture")
                    .and_then(|register| self.lookup(register))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let program_key = self
            .fold_functions
            .get(&(Arc::as_ptr(program) as usize))
            .copied()
            .ok_or_else(|| {
                CompileError::Backend("nested function-fold kernel was not precompiled".to_string())
            })?;
        let guard = condition
            .map(|condition| {
                self.lookup(condition)
                    .map(|value| (value, nested_when_true))
            })
            .transpose()?;
        let call_key = NestedFoldCallKey {
            program: program_key,
            initial: initial_key.into_boxed_slice(),
            captures: captures.clone().into_boxed_slice(),
            guard,
        };
        // A Modelica algorithm branch is semantically lazy. Keep the compact
        // nested fold in Solve IR, then introduce control flow only here at the
        // final native-emission boundary so an inactive estimator correction
        // does not execute its Cholesky and covariance kernels.
        let continuation = self.enter_nested_fold_guard(guard);
        let nested_carried =
            self.cached_nested_fold_carried(parent_carried, program, &captures, call_key)?;
        self.copy_stack_range(
            nested_carried,
            result_base,
            parent_carried,
            output_base,
            result_count,
        )?;
        if let Some(continuation) = continuation {
            self.fb.ins().jump(continuation, &[]);
            self.fb.switch_to_block(continuation);
            self.fb.seal_block(continuation);
        }
        Ok(())
    }

    fn nested_fold_initial_key(
        &mut self,
        initial: &[rumoca_ir_solve::FoldInitialSource],
    ) -> Result<Vec<NestedFoldInitialKey>, CompileError> {
        initial
            .iter()
            .map(|source| self.nested_fold_initial_source(source))
            .collect()
    }

    fn nested_fold_initial_source(
        &mut self,
        source: &rumoca_ir_solve::FoldInitialSource,
    ) -> Result<NestedFoldInitialKey, CompileError> {
        match *source {
            rumoca_ir_solve::FoldInitialSource::Registers { start, count } => {
                let values = (0..count)
                    .map(|offset| {
                        checked_reg_offset(start, offset, "nested function fold initial")
                            .and_then(|register| self.lookup(register))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(NestedFoldInitialKey::Registers(values.into_boxed_slice()))
            }
            rumoca_ir_solve::FoldInitialSource::ParentCarried { base, count } => {
                let end = base.checked_add(count).ok_or_else(|| {
                    CompileError::Backend("nested function-fold initial range overflow".into())
                })?;
                let versions = self
                    .fold_carried_versions
                    .get(base..end)
                    .ok_or_else(|| {
                        CompileError::Backend(
                            "nested function-fold initial range is out of bounds".into(),
                        )
                    })?
                    .to_vec()
                    .into_boxed_slice();
                Ok(NestedFoldInitialKey::ParentCarried {
                    base,
                    count,
                    versions,
                })
            }
        }
    }

    fn enter_nested_fold_guard(
        &mut self,
        guard: Option<(cranelift_codegen::ir::Value, bool)>,
    ) -> Option<cranelift_codegen::ir::Block> {
        let (condition, nested_when_true) = guard?;
        let zero = self.fb.ins().f64const(0.0);
        let condition = self.fb.ins().fcmp(FloatCC::NotEqual, condition, zero);
        let use_nested = if nested_when_true {
            condition
        } else {
            self.fb.ins().bnot(condition)
        };
        let nested = self.fb.create_block();
        let continuation = self.fb.create_block();
        self.fb
            .ins()
            .brif(use_nested, nested, &[], continuation, &[]);
        self.fb.switch_to_block(nested);
        self.fb.seal_block(nested);
        Some(continuation)
    }

    fn cached_nested_fold_carried(
        &mut self,
        parent_carried: StackSlot,
        program: &Arc<rumoca_ir_solve::FunctionFoldProgram>,
        captures: &[cranelift_codegen::ir::Value],
        call_key: NestedFoldCallKey,
    ) -> Result<StackSlot, CompileError> {
        if let Some(cached) = self.nested_fold_results.get(&call_key).copied() {
            return Ok(cached);
        }
        let carried_bytes = checked_f64_stack_bytes(
            program.carried_count,
            "nested function-fold storage size overflow",
        )?;
        let nested_carried = self.fb.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            carried_bytes,
            3,
        ));
        let count =
            self.initialize_nested_fold_carried(parent_carried, nested_carried, &call_key.initial)?;
        if count != program.carried_count {
            return Err(CompileError::Backend(
                "nested function-fold initial count mismatch".into(),
            ));
        }
        self.execute_nested_fold(program, nested_carried, captures)?;
        self.nested_fold_results.insert(call_key, nested_carried);
        Ok(nested_carried)
    }

    fn initialize_nested_fold_carried(
        &mut self,
        parent: StackSlot,
        nested: StackSlot,
        initial: &[NestedFoldInitialKey],
    ) -> Result<usize, CompileError> {
        let mut destination = 0usize;
        for source in initial {
            let count = self.initialize_nested_fold_source(parent, nested, destination, source)?;
            destination = checked_usize_add(
                destination,
                count,
                "nested function-fold initial count overflow",
            )?;
        }
        Ok(destination)
    }

    fn initialize_nested_fold_source(
        &mut self,
        parent: StackSlot,
        nested: StackSlot,
        destination: usize,
        source: &NestedFoldInitialKey,
    ) -> Result<usize, CompileError> {
        match source {
            NestedFoldInitialKey::Registers(values) => {
                for (offset, value) in values.iter().copied().enumerate() {
                    self.fb.ins().stack_store(
                        value,
                        nested,
                        stack_element_byte_offset(
                            checked_usize_add(
                                destination,
                                offset,
                                "nested function-fold initial offset overflow",
                            )?,
                            "nested function-fold initial",
                        )?,
                    );
                }
                Ok(values.len())
            }
            NestedFoldInitialKey::ParentCarried { base, count, .. } => {
                self.copy_stack_range(parent, *base, nested, destination, *count)?;
                Ok(*count)
            }
        }
    }

    fn execute_nested_fold(
        &mut self,
        program: &Arc<rumoca_ir_solve::FunctionFoldProgram>,
        carried: StackSlot,
        captures: &[cranelift_codegen::ir::Value],
    ) -> Result<(), CompileError> {
        let point_count = program.domain.scalar_count().map_err(|error| {
            CompileError::Backend(format!("invalid nested function-fold domain: {error}"))
        })?;
        let inline = point_count <= INLINE_FIXED_FOLD_POINT_LIMIT
            && program.carried_count <= 2
            && program.update.len() <= 24
            && self.backing_regs_ptr.is_none();
        if !inline {
            return self.call_fold_kernel(program, carried, captures);
        }
        let capture_bytes =
            checked_f64_stack_bytes(captures.len().max(1), "inline fold capture size overflow")?;
        let capture_slot = self.fb.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            capture_bytes,
            3,
        ));
        for (offset, value) in captures.iter().copied().enumerate() {
            self.fb.ins().stack_store(
                value,
                capture_slot,
                stack_element_byte_offset(offset, "inline fold capture")?,
            );
        }
        let captures_ptr = self.fb.ins().stack_addr(types::I64, capture_slot, 0);
        self.lower_function_fold_on_stack(carried, captures, Some(captures_ptr), program)
    }

    fn copy_stack_range(
        &mut self,
        source: StackSlot,
        source_base: usize,
        destination: StackSlot,
        destination_base: usize,
        count: usize,
    ) -> Result<(), CompileError> {
        if count == 0 {
            return Ok(());
        }
        let source_base = stack_element_byte_offset(source_base, "stack range source")?;
        let destination_base =
            stack_element_byte_offset(destination_base, "stack range destination")?;
        let count = i64::try_from(count)
            .map_err(|_| CompileError::Backend("stack range count exceeds i64".to_string()))?;
        let header = self.fb.create_block();
        let body = self.fb.create_block();
        let exit = self.fb.create_block();
        self.fb.append_block_param(header, types::I64);
        let zero = self.fb.ins().iconst(types::I64, 0);
        self.fb.ins().jump(header, &[zero.into()]);

        self.fb.switch_to_block(header);
        let ordinal = self.fb.block_params(header)[0];
        let in_range = self
            .fb
            .ins()
            .icmp_imm(IntCC::UnsignedLessThan, ordinal, count);
        self.fb.ins().brif(in_range, body, &[], exit, &[]);

        self.fb.switch_to_block(body);
        self.fb.seal_block(body);
        let byte_offset = self
            .fb
            .ins()
            .imul_imm(ordinal, std::mem::size_of::<f64>() as i64);
        let source_address = self.fb.ins().stack_addr(types::I64, source, source_base);
        let source_address = self.fb.ins().iadd(source_address, byte_offset);
        let value = self
            .fb
            .ins()
            .load(types::F64, self.flags, source_address, 0);
        let destination_address =
            self.fb
                .ins()
                .stack_addr(types::I64, destination, destination_base);
        let destination_address = self.fb.ins().iadd(destination_address, byte_offset);
        self.fb
            .ins()
            .store(self.flags, value, destination_address, 0);
        let next = self.fb.ins().iadd_imm(ordinal, 1);
        self.fb.ins().jump(header, &[next.into()]);
        self.fb.seal_block(header);

        self.fb.switch_to_block(exit);
        self.fb.seal_block(exit);
        Ok(())
    }

    fn copy_pointer_to_stack(
        &mut self,
        source: cranelift_codegen::ir::Value,
        destination: StackSlot,
        count: usize,
    ) -> Result<(), CompileError> {
        let destination = self.fb.ins().stack_addr(types::I64, destination, 0);
        self.copy_pointer_range(source, destination, count)
    }

    fn copy_stack_to_pointer(
        &mut self,
        source: StackSlot,
        destination: cranelift_codegen::ir::Value,
        count: usize,
    ) -> Result<(), CompileError> {
        let source = self.fb.ins().stack_addr(types::I64, source, 0);
        self.copy_pointer_range(source, destination, count)
    }

    fn copy_pointer_range(
        &mut self,
        source: cranelift_codegen::ir::Value,
        destination: cranelift_codegen::ir::Value,
        count: usize,
    ) -> Result<(), CompileError> {
        if count == 0 {
            return Ok(());
        }
        let count = i64::try_from(count)
            .map_err(|_| CompileError::Backend("pointer range count exceeds i64".to_string()))?;
        let header = self.fb.create_block();
        let body = self.fb.create_block();
        let exit = self.fb.create_block();
        self.fb.append_block_param(header, types::I64);
        let zero = self.fb.ins().iconst(types::I64, 0);
        self.fb.ins().jump(header, &[zero.into()]);
        self.fb.switch_to_block(header);
        let ordinal = self.fb.block_params(header)[0];
        let in_range = self
            .fb
            .ins()
            .icmp_imm(IntCC::UnsignedLessThan, ordinal, count);
        self.fb.ins().brif(in_range, body, &[], exit, &[]);
        self.fb.switch_to_block(body);
        self.fb.seal_block(body);
        let byte_offset = self
            .fb
            .ins()
            .imul_imm(ordinal, std::mem::size_of::<f64>() as i64);
        let source_address = self.fb.ins().iadd(source, byte_offset);
        let value = self
            .fb
            .ins()
            .load(types::F64, self.flags, source_address, 0);
        let destination_address = self.fb.ins().iadd(destination, byte_offset);
        self.fb
            .ins()
            .store(self.flags, value, destination_address, 0);
        let next = self.fb.ins().iadd_imm(ordinal, 1);
        self.fb.ins().jump(header, &[next.into()]);
        self.fb.seal_block(header);
        self.fb.switch_to_block(exit);
        self.fb.seal_block(exit);
        Ok(())
    }

    fn lower_loaded_reg(
        &mut self,
        dst: u32,
        base: cranelift_codegen::ir::Value,
        index: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let value = load_f64(self.fb, self.flags, base, index)?;
        self.insert(dst, value)
    }

    fn lower_loaded_y_reg(
        &mut self,
        dst: u32,
        index: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        if let Some(value) = self
            .loaded_y
            .as_deref()
            .and_then(|values| values.get(&index))
            .copied()
        {
            return self.insert(dst, value);
        }
        let value = load_f64(self.fb, self.flags, self.y_ptr, index)?;
        if let Some(values) = self.loaded_y.as_deref_mut() {
            values.insert(index, value);
        }
        self.insert(dst, value)
    }

    fn lower_loaded_p_reg(
        &mut self,
        dst: u32,
        index: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        if let Some(value) = self
            .loaded_p
            .as_deref()
            .and_then(|values| values.get(&index))
            .copied()
        {
            return self.insert(dst, value);
        }
        let value = load_f64(self.fb, self.flags, self.p_ptr, index)?;
        if let Some(values) = self.loaded_p.as_deref_mut() {
            values.insert(index, value);
        }
        self.insert(dst, value)
    }

    fn lower_dot_product(
        &mut self,
        dst: u32,
        lhs_start: u32,
        rhs_start: u32,
        count: usize,
        lhs_stride: usize,
        rhs_stride: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let mut terms = checked_vec_with_capacity(count, "dot product schedule terms")?;
        for term in 0..count {
            let lhs = checked_strided_register(lhs_start, term, lhs_stride, "dot lhs")?;
            let rhs = checked_strided_register(rhs_start, term, rhs_stride, "dot rhs")?;
            terms.push((self.lookup(lhs)?, self.lookup(rhs)?));
        }
        let mut value = self.fb.ins().f64const(0.0);
        for (lhs, rhs) in terms {
            let product = self.fb.ins().fmul(lhs, rhs);
            value = self.fb.ins().fadd(value, product);
        }
        self.insert(dst, value)
    }

    #[allow(clippy::too_many_arguments, clippy::too_many_lines)]
    fn lower_matrix_multiply(
        &mut self,
        dst_start: u32,
        lhs_start: u32,
        rhs_start: u32,
        rows: usize,
        inner: usize,
        columns: usize,
        lanes: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let static_work = rows
            .checked_mul(columns)
            .and_then(|outputs| outputs.checked_mul(inner))
            .and_then(|work| work.checked_mul(lanes));
        if self.backing_regs_ptr.is_none() || static_work.is_some_and(|work| work <= 64) {
            self.lower_static_matrix_multiply(StaticMatrixMultiply {
                dst_start,
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes,
            })?;
            return Ok(None);
        }
        let regs_ptr = self.backing_regs_ptr.expect("checked above");

        let output_count = rows.checked_mul(columns).ok_or_else(|| {
            CompileError::Backend("matrix multiply output extent overflow".to_string())
        })?;
        let outer_header = self.fb.create_block();
        let outer_body = self.fb.create_block();
        let outer_exit = self.fb.create_block();
        let term_header = self.fb.create_block();
        let term_body = self.fb.create_block();
        let term_exit = self.fb.create_block();
        self.fb.append_block_param(outer_header, types::I64);
        self.fb.append_block_param(term_header, types::I64);
        self.fb.append_block_param(term_header, types::F64);
        self.fb.append_block_param(term_header, types::F64);

        let zero_index = self.fb.ins().iconst(types::I64, 0);
        self.fb.ins().jump(outer_header, &[zero_index.into()]);
        self.fb.switch_to_block(outer_header);
        let output = self.fb.block_params(outer_header)[0];
        let in_range = self.fb.ins().icmp_imm(
            IntCC::UnsignedLessThan,
            output,
            i64::try_from(output_count).map_err(|_| {
                CompileError::Backend("matrix multiply output count exceeds i64".to_string())
            })?,
        );
        self.fb
            .ins()
            .brif(in_range, outer_body, &[], outer_exit, &[]);

        self.fb.switch_to_block(outer_body);
        self.fb.seal_block(outer_body);
        let row = self.fb.ins().udiv_imm(output, columns as i64);
        let column = self.fb.ins().urem_imm(output, columns as i64);
        let zero_re = self.fb.ins().f64const(0.0);
        let zero_du = self.fb.ins().f64const(0.0);
        self.fb.ins().jump(
            term_header,
            &[zero_index.into(), zero_re.into(), zero_du.into()],
        );

        self.fb.switch_to_block(term_header);
        let term = self.fb.block_params(term_header)[0];
        let accumulated_re = self.fb.block_params(term_header)[1];
        let accumulated_du = self.fb.block_params(term_header)[2];
        let term_in_range = self.fb.ins().icmp_imm(
            IntCC::UnsignedLessThan,
            term,
            i64::try_from(inner).map_err(|_| {
                CompileError::Backend("matrix multiply inner extent exceeds i64".to_string())
            })?,
        );
        self.fb
            .ins()
            .brif(term_in_range, term_body, &[], term_exit, &[]);

        self.fb.switch_to_block(term_body);
        self.fb.seal_block(term_body);
        let lhs_element = self.fb.ins().imul_imm(row, inner as i64);
        let lhs_element = self.fb.ins().iadd(lhs_element, term);
        let lhs_element = self.fb.ins().imul_imm(lhs_element, lanes as i64);
        let rhs_element = self.fb.ins().imul_imm(term, columns as i64);
        let rhs_element = self.fb.ins().iadd(rhs_element, column);
        let rhs_element = self.fb.ins().imul_imm(rhs_element, lanes as i64);
        let lhs_register = self.fb.ins().iadd_imm(lhs_element, i64::from(lhs_start));
        let rhs_register = self.fb.ins().iadd_imm(rhs_element, i64::from(rhs_start));
        let lhs_offset = self.fb.ins().imul_imm(lhs_register, 8);
        let rhs_offset = self.fb.ins().imul_imm(rhs_register, 8);
        let lhs_address = self.fb.ins().iadd(regs_ptr, lhs_offset);
        let rhs_address = self.fb.ins().iadd(regs_ptr, rhs_offset);
        let lhs_re = self.fb.ins().load(types::F64, self.flags, lhs_address, 0);
        let rhs_re = self.fb.ins().load(types::F64, self.flags, rhs_address, 0);
        let product = self.fb.ins().fmul(lhs_re, rhs_re);
        let next_re = self.fb.ins().fadd(accumulated_re, product);
        let next_du = if lanes == 2 {
            let lhs_du = self.fb.ins().load(types::F64, self.flags, lhs_address, 8);
            let rhs_du = self.fb.ins().load(types::F64, self.flags, rhs_address, 8);
            let lhs_term = self.fb.ins().fmul(lhs_du, rhs_re);
            let rhs_term = self.fb.ins().fmul(lhs_re, rhs_du);
            let tangent = self.fb.ins().fadd(lhs_term, rhs_term);
            self.fb.ins().fadd(accumulated_du, tangent)
        } else {
            accumulated_du
        };
        let next_term = self.fb.ins().iadd_imm(term, 1);
        self.fb.ins().jump(
            term_header,
            &[next_term.into(), next_re.into(), next_du.into()],
        );
        self.fb.seal_block(term_header);

        self.fb.switch_to_block(term_exit);
        self.fb.seal_block(term_exit);
        let output_element = self.fb.ins().imul_imm(output, lanes as i64);
        let output_register = self.fb.ins().iadd_imm(output_element, i64::from(dst_start));
        let output_offset = self.fb.ins().imul_imm(output_register, 8);
        let output_address = self.fb.ins().iadd(regs_ptr, output_offset);
        self.fb
            .ins()
            .store(self.flags, accumulated_re, output_address, 0);
        if lanes == 2 {
            self.fb
                .ins()
                .store(self.flags, accumulated_du, output_address, 8);
        }
        let next_output = self.fb.ins().iadd_imm(output, 1);
        self.fb.ins().jump(outer_header, &[next_output.into()]);
        self.fb.seal_block(outer_header);

        self.fb.switch_to_block(outer_exit);
        self.fb.seal_block(outer_exit);
        Ok(None)
    }

    fn lower_static_matrix_multiply(
        &mut self,
        request: StaticMatrixMultiply,
    ) -> Result<(), CompileError> {
        for row in 0..request.rows {
            for column in 0..request.columns {
                self.lower_static_matrix_output(StaticMatrixOutput {
                    dst_start: request.dst_start,
                    lhs_start: request.lhs_start,
                    rhs_start: request.rhs_start,
                    row,
                    column,
                    inner: request.inner,
                    columns: request.columns,
                    lanes: request.lanes,
                })?;
            }
        }
        Ok(())
    }

    fn lower_static_matrix_output(
        &mut self,
        request: StaticMatrixOutput,
    ) -> Result<(), CompileError> {
        let StaticMatrixOutput {
            dst_start,
            lhs_start,
            rhs_start,
            row,
            column,
            inner,
            columns,
            lanes,
        } = request;
        let output = (row * columns + column) * lanes;
        let mut re = self.fb.ins().f64const(0.0);
        let mut du = self.fb.ins().f64const(0.0);
        for term in 0..inner {
            let lhs = (row * inner + term) * lanes;
            let rhs = (term * columns + column) * lanes;
            let lhs_re = self.lookup(checked_reg_offset(lhs_start, lhs, "matrix multiply lhs")?)?;
            let rhs_re = self.lookup(checked_reg_offset(rhs_start, rhs, "matrix multiply rhs")?)?;
            let product = self.fb.ins().fmul(lhs_re, rhs_re);
            re = self.fb.ins().fadd(re, product);
            if lanes == 2 {
                let lhs_du = self.lookup(checked_reg_offset(
                    lhs_start,
                    lhs + 1,
                    "matrix multiply lhs tangent",
                )?)?;
                let rhs_du = self.lookup(checked_reg_offset(
                    rhs_start,
                    rhs + 1,
                    "matrix multiply rhs tangent",
                )?)?;
                let lhs_term = self.fb.ins().fmul(lhs_du, rhs_re);
                let rhs_term = self.fb.ins().fmul(lhs_re, rhs_du);
                let tangent = self.fb.ins().fadd(lhs_term, rhs_term);
                du = self.fb.ins().fadd(du, tangent);
            }
        }
        self.insert(
            checked_reg_offset(dst_start, output, "matrix multiply output")?,
            re,
        )?;
        if lanes == 2 {
            self.insert(
                checked_reg_offset(dst_start, output + 1, "matrix multiply output tangent")?,
                du,
            )?;
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_tensor_binary(
        &mut self,
        dst_start: u32,
        op: BinaryOp,
        lhs_start: u32,
        rhs_start: u32,
        count: usize,
        lhs_stride: usize,
        rhs_stride: usize,
        lanes: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let request = TensorBinaryRequest {
            dst_start,
            op,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes,
        };
        let Some(regs_ptr) = self.backing_regs_ptr else {
            self.lower_static_tensor_binary(request)?;
            return Ok(None);
        };

        let lhs_step = i64::try_from(lhs_stride.saturating_mul(lanes)).map_err(|_| {
            CompileError::Backend("tensor binary lhs stride exceeds i64".to_string())
        })?;
        let rhs_step = i64::try_from(rhs_stride.saturating_mul(lanes)).map_err(|_| {
            CompileError::Backend("tensor binary rhs stride exceeds i64".to_string())
        })?;
        let dst_step = i64::try_from(lanes).map_err(|_| {
            CompileError::Backend("tensor binary lane count exceeds i64".to_string())
        })?;
        let header = self.fb.create_block();
        let body = self.fb.create_block();
        let exit = self.fb.create_block();
        self.fb.append_block_param(header, types::I64);
        let zero = self.fb.ins().iconst(types::I64, 0);
        self.fb.ins().jump(header, &[zero.into()]);
        self.fb.switch_to_block(header);
        let element = self.fb.block_params(header)[0];
        let in_range = self.fb.ins().icmp_imm(
            IntCC::UnsignedLessThan,
            element,
            i64::try_from(count).map_err(|_| {
                CompileError::Backend("tensor binary extent exceeds i64".to_string())
            })?,
        );
        self.fb.ins().brif(in_range, body, &[], exit, &[]);

        self.fb.switch_to_block(body);
        self.fb.seal_block(body);
        let register_address = |fb: &mut FunctionBuilder<'_>, start: u32, step: i64| {
            let register = fb.ins().imul_imm(element, step);
            let register = fb.ins().iadd_imm(register, i64::from(start));
            let offset = fb.ins().imul_imm(register, 8);
            fb.ins().iadd(regs_ptr, offset)
        };
        let lhs_address = register_address(self.fb, lhs_start, lhs_step);
        let rhs_address = register_address(self.fb, rhs_start, rhs_step);
        let dst_address = register_address(self.fb, dst_start, dst_step);
        let lhs_re = self.fb.ins().load(types::F64, self.flags, lhs_address, 0);
        let rhs_re = self.fb.ins().load(types::F64, self.flags, rhs_address, 0);
        let primal =
            emit_tensor_binary_primal(self.fb, self.module, self.math, op, lhs_re, rhs_re, lanes)?;
        self.fb.ins().store(self.flags, primal, dst_address, 0);
        if lanes == 2 {
            let lhs_du = self.fb.ins().load(types::F64, self.flags, lhs_address, 8);
            let rhs_du = self.fb.ins().load(types::F64, self.flags, rhs_address, 8);
            let tangent = emit_tensor_binary_tangent(self.fb, op, lhs_re, lhs_du, rhs_re, rhs_du)?;
            self.fb.ins().store(self.flags, tangent, dst_address, 8);
        }
        let next = self.fb.ins().iadd_imm(element, 1);
        self.fb.ins().jump(header, &[next.into()]);
        self.fb.seal_block(header);
        self.fb.switch_to_block(exit);
        self.fb.seal_block(exit);
        Ok(None)
    }

    fn lower_static_tensor_binary(
        &mut self,
        request: TensorBinaryRequest,
    ) -> Result<(), CompileError> {
        for element in 0..request.count {
            let lhs = checked_reg_offset(
                request.lhs_start,
                element
                    .saturating_mul(request.lhs_stride)
                    .saturating_mul(request.lanes),
                "tensor binary lhs",
            )?;
            let rhs = checked_reg_offset(
                request.rhs_start,
                element
                    .saturating_mul(request.rhs_stride)
                    .saturating_mul(request.lanes),
                "tensor binary rhs",
            )?;
            let dst = checked_reg_offset(
                request.dst_start,
                element.saturating_mul(request.lanes),
                "tensor binary output",
            )?;
            let lhs_re = self.lookup(lhs)?;
            let rhs_re = self.lookup(rhs)?;
            let primal = emit_tensor_binary_primal(
                self.fb,
                self.module,
                self.math,
                request.op,
                lhs_re,
                rhs_re,
                request.lanes,
            )?;
            self.insert(dst, primal)?;
            if request.lanes == 2 {
                let lhs_du = self.lookup(lhs + 1)?;
                let rhs_du = self.lookup(rhs + 1)?;
                let tangent = emit_tensor_binary_tangent(
                    self.fb, request.op, lhs_re, lhs_du, rhs_re, rhs_du,
                )?;
                self.insert(dst + 1, tangent)?;
            }
        }
        Ok(())
    }

    fn lower_tensor_cross(
        &mut self,
        dst_start: u32,
        lhs_start: u32,
        rhs_start: u32,
        lanes: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let count = 3usize
            .checked_mul(lanes)
            .ok_or_else(|| CompileError::Backend("tensor cross width overflow".into()))?;
        let mut lhs = Vec::with_capacity(count);
        let mut rhs = Vec::with_capacity(count);
        for offset in 0..count {
            let lhs_reg = checked_reg_offset(lhs_start, offset, "tensor cross lhs")?;
            let rhs_reg = checked_reg_offset(rhs_start, offset, "tensor cross rhs")?;
            if let Some(regs_ptr) = self.backing_regs_ptr {
                let lhs_address = self.fb.ins().iadd_imm(regs_ptr, i64::from(lhs_reg) * 8);
                let rhs_address = self.fb.ins().iadd_imm(regs_ptr, i64::from(rhs_reg) * 8);
                lhs.push(self.fb.ins().load(types::F64, self.flags, lhs_address, 0));
                rhs.push(self.fb.ins().load(types::F64, self.flags, rhs_address, 0));
            } else {
                lhs.push(self.lookup(lhs_reg)?);
                rhs.push(self.lookup(rhs_reg)?);
            }
        }
        for (component, (first, second)) in
            [(1usize, 2usize), (2, 0), (0, 1)].into_iter().enumerate()
        {
            self.lower_tensor_cross_component(TensorCrossComponent {
                dst_start,
                lhs: &lhs,
                rhs: &rhs,
                component,
                first,
                second,
                lanes,
            })?;
        }
        Ok(None)
    }

    fn lower_tensor_cross_component(
        &mut self,
        request: TensorCrossComponent<'_>,
    ) -> Result<(), CompileError> {
        let TensorCrossComponent {
            dst_start,
            lhs,
            rhs,
            component,
            first,
            second,
            lanes,
        } = request;
        let first = first * lanes;
        let second = second * lanes;
        let positive = self.fb.ins().fmul(lhs[first], rhs[second]);
        let negative = self.fb.ins().fmul(lhs[second], rhs[first]);
        let primal = self.fb.ins().fsub(positive, negative);
        let dst = checked_reg_offset(dst_start, component * lanes, "tensor cross destination")?;
        self.store_tensor_cross_value(dst, primal)?;
        if lanes == 2 {
            let first_lhs = self.fb.ins().fmul(lhs[first + 1], rhs[second]);
            let first_rhs = self.fb.ins().fmul(lhs[first], rhs[second + 1]);
            let second_lhs = self.fb.ins().fmul(lhs[second + 1], rhs[first]);
            let second_rhs = self.fb.ins().fmul(lhs[second], rhs[first + 1]);
            let positive = self.fb.ins().fadd(first_lhs, first_rhs);
            let negative = self.fb.ins().fadd(second_lhs, second_rhs);
            let tangent = self.fb.ins().fsub(positive, negative);
            self.store_tensor_cross_value(dst + 1, tangent)?;
        }
        Ok(())
    }

    fn store_tensor_cross_value(
        &mut self,
        dst: u32,
        value: cranelift_codegen::ir::Value,
    ) -> Result<(), CompileError> {
        if let Some(regs_ptr) = self.backing_regs_ptr {
            let address = self.fb.ins().iadd_imm(regs_ptr, i64::from(dst) * 8);
            self.fb.ins().store(self.flags, value, address, 0);
            Ok(())
        } else {
            self.insert(dst, value).map(|_| ())
        }
    }

    fn lower_tensor_transpose(
        &mut self,
        dst_start: u32,
        src_start: u32,
        rows: usize,
        columns: usize,
        element_width: usize,
        lanes: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let count = rows
            .checked_mul(columns)
            .ok_or_else(|| CompileError::Backend("tensor transpose extent overflow".into()))?;
        let value_width = element_width
            .checked_mul(lanes)
            .ok_or_else(|| CompileError::Backend("tensor transpose value width overflow".into()))?;
        let Some(regs_ptr) = self.backing_regs_ptr else {
            self.lower_static_tensor_transpose(dst_start, src_start, rows, columns, value_width)?;
            return Ok(None);
        };
        let header = self.fb.create_block();
        let body = self.fb.create_block();
        let exit = self.fb.create_block();
        self.fb.append_block_param(header, types::I64);
        let zero = self.fb.ins().iconst(types::I64, 0);
        self.fb.ins().jump(header, &[zero.into()]);
        self.fb.switch_to_block(header);
        let output = self.fb.block_params(header)[0];
        let in_range = self.fb.ins().icmp_imm(
            IntCC::UnsignedLessThan,
            output,
            i64::try_from(count)
                .map_err(|_| CompileError::Backend("tensor transpose extent exceeds i64".into()))?,
        );
        self.fb.ins().brif(in_range, body, &[], exit, &[]);
        self.fb.switch_to_block(body);
        self.fb.seal_block(body);
        let row = self.fb.ins().udiv_imm(output, columns as i64);
        let column = self.fb.ins().urem_imm(output, columns as i64);
        let source = self.fb.ins().imul_imm(column, rows as i64);
        let source = self.fb.ins().iadd(source, row);
        let source = self.fb.ins().imul_imm(source, value_width as i64);
        let destination = self.fb.ins().imul_imm(output, value_width as i64);
        let source = self.fb.ins().iadd_imm(source, i64::from(src_start));
        let destination = self.fb.ins().iadd_imm(destination, i64::from(dst_start));
        let source_offset = self.fb.ins().imul_imm(source, 8);
        let destination_offset = self.fb.ins().imul_imm(destination, 8);
        let source_address = self.fb.ins().iadd(regs_ptr, source_offset);
        let destination_address = self.fb.ins().iadd(regs_ptr, destination_offset);
        for value in 0..value_width {
            let offset = i32::try_from(value.checked_mul(8).ok_or_else(|| {
                CompileError::Backend("tensor transpose value offset overflow".into())
            })?)
            .map_err(|_| CompileError::Backend("tensor transpose offset exceeds i32".into()))?;
            let value = self
                .fb
                .ins()
                .load(types::F64, self.flags, source_address, offset);
            self.fb
                .ins()
                .store(self.flags, value, destination_address, offset);
        }
        let next = self.fb.ins().iadd_imm(output, 1);
        self.fb.ins().jump(header, &[next.into()]);
        self.fb.seal_block(header);
        self.fb.switch_to_block(exit);
        self.fb.seal_block(exit);
        Ok(None)
    }

    fn lower_static_tensor_transpose(
        &mut self,
        dst_start: u32,
        src_start: u32,
        rows: usize,
        columns: usize,
        value_width: usize,
    ) -> Result<(), CompileError> {
        for row in 0..rows {
            for column in 0..columns {
                self.lower_static_tensor_transpose_cell(StaticTransposeCell {
                    dst_start,
                    src_start,
                    rows,
                    columns,
                    value_width,
                    row,
                    column,
                })?;
            }
        }
        Ok(())
    }

    fn lower_static_tensor_transpose_cell(
        &mut self,
        request: StaticTransposeCell,
    ) -> Result<(), CompileError> {
        let StaticTransposeCell {
            dst_start,
            src_start,
            rows,
            columns,
            value_width,
            row,
            column,
        } = request;
        for value in 0..value_width {
            let dst = (row * columns + column) * value_width + value;
            let src = (column * rows + row) * value_width + value;
            let value = self.lookup(checked_reg_offset(
                src_start,
                src,
                "tensor transpose source",
            )?)?;
            self.insert(
                checked_reg_offset(dst_start, dst, "tensor transpose output")?,
                value,
            )?;
        }
        Ok(())
    }

    fn lower_tensor_concatenate(
        &mut self,
        dst_start: u32,
        sources: &[rumoca_ir_solve::TensorConcatenateSource],
        dimensions: &[u32],
        axis: usize,
        lanes: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let inner = dimensions[axis + 1..]
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
            .ok_or_else(|| CompileError::Backend("tensor concatenate inner overflow".into()))?;
        let result_axis = dimensions[axis] as usize;
        let result_block = result_axis
            .checked_mul(inner)
            .ok_or_else(|| CompileError::Backend("tensor concatenate block overflow".into()))?;
        let mut axis_offset = 0usize;
        for source in sources {
            let source_axis = self.lower_tensor_concatenate_source(TensorConcatenateCopy {
                dst_start,
                source,
                axis,
                axis_offset,
                inner,
                result_block,
                lanes,
            })?;
            axis_offset += source_axis;
        }
        Ok(None)
    }

    fn lower_tensor_concatenate_source(
        &mut self,
        request: TensorConcatenateCopy<'_>,
    ) -> Result<usize, CompileError> {
        let TensorConcatenateCopy {
            dst_start,
            source,
            axis,
            axis_offset,
            inner,
            result_block,
            lanes,
        } = request;
        let source_axis = source.dimensions[axis] as usize;
        let source_count = source
            .dimensions
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
            .ok_or_else(|| CompileError::Backend("tensor concatenate source overflow".into()))?;
        let source_block = source_axis.checked_mul(inner).ok_or_else(|| {
            CompileError::Backend("tensor concatenate source block overflow".into())
        })?;
        if self.backing_regs_ptr.is_none() {
            self.lower_static_tensor_concatenate_source(request, source_count, source_block)?;
            return Ok(source_axis);
        }
        let regs_ptr = self.backing_regs_ptr.expect("checked above");
        let header = self.fb.create_block();
        let body = self.fb.create_block();
        let exit = self.fb.create_block();
        self.fb.append_block_param(header, types::I64);
        let zero = self.fb.ins().iconst(types::I64, 0);
        self.fb.ins().jump(header, &[zero.into()]);
        self.fb.switch_to_block(header);
        let element = self.fb.block_params(header)[0];
        let in_range = self.fb.ins().icmp_imm(
            IntCC::UnsignedLessThan,
            element,
            i64::try_from(source_count).map_err(|_| {
                CompileError::Backend("tensor concatenate source exceeds i64".into())
            })?,
        );
        self.fb.ins().brif(in_range, body, &[], exit, &[]);
        self.fb.switch_to_block(body);
        self.fb.seal_block(body);
        let outer = self.fb.ins().udiv_imm(element, source_block as i64);
        let within = self.fb.ins().urem_imm(element, source_block as i64);
        let destination = self.fb.ins().imul_imm(outer, result_block as i64);
        let destination = self.fb.ins().iadd(destination, within);
        let destination = self.fb.ins().iadd_imm(
            destination,
            i64::try_from(axis_offset * inner).map_err(|_| {
                CompileError::Backend("tensor concatenate axis offset exceeds i64".into())
            })?,
        );
        let source_register = self.fb.ins().imul_imm(element, lanes as i64);
        let source_register = self
            .fb
            .ins()
            .iadd_imm(source_register, i64::from(source.start));
        let destination_register = self.fb.ins().imul_imm(destination, lanes as i64);
        let destination_register = self
            .fb
            .ins()
            .iadd_imm(destination_register, i64::from(dst_start));
        let source_address = self.fb.ins().imul_imm(source_register, 8);
        let source_address = self.fb.ins().iadd(regs_ptr, source_address);
        let destination_address = self.fb.ins().imul_imm(destination_register, 8);
        let destination_address = self.fb.ins().iadd(regs_ptr, destination_address);
        let primal = self
            .fb
            .ins()
            .load(types::F64, self.flags, source_address, 0);
        self.fb
            .ins()
            .store(self.flags, primal, destination_address, 0);
        if lanes == 2 {
            let tangent = self
                .fb
                .ins()
                .load(types::F64, self.flags, source_address, 8);
            self.fb
                .ins()
                .store(self.flags, tangent, destination_address, 8);
        }
        let next = self.fb.ins().iadd_imm(element, 1);
        self.fb.ins().jump(header, &[next.into()]);
        self.fb.seal_block(header);
        self.fb.switch_to_block(exit);
        self.fb.seal_block(exit);
        Ok(source_axis)
    }

    fn lower_static_tensor_concatenate_source(
        &mut self,
        request: TensorConcatenateCopy<'_>,
        source_count: usize,
        source_block: usize,
    ) -> Result<(), CompileError> {
        for element in 0..source_count {
            let outer = element / source_block;
            let within = element % source_block;
            let destination =
                outer * request.result_block + request.axis_offset * request.inner + within;
            for lane in 0..request.lanes {
                let value = self.lookup(checked_reg_offset(
                    request.source.start,
                    element * request.lanes + lane,
                    "tensor concatenate source",
                )?)?;
                self.insert(
                    checked_reg_offset(
                        request.dst_start,
                        destination * request.lanes + lane,
                        "tensor concatenate output",
                    )?,
                    value,
                )?;
            }
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    // SPEC_0021: exhaustive static/dynamic dispatch over TensorUpdateSubscript.
    #[expect(
        clippy::too_many_lines,
        clippy::excessive_nesting,
        reason = "tensor updates exhaustively lower every checked subscript representation"
    )]
    fn lower_tensor_update(
        &mut self,
        dst_start: u32,
        base_start: u32,
        value_start: u32,
        dimensions: &[u32],
        subscripts: &[rumoca_ir_solve::TensorUpdateSubscript],
        lanes: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let count = dimensions
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
            .ok_or_else(|| CompileError::Backend("tensor update extent overflow".into()))?;
        let Some(regs_ptr) = self.backing_regs_ptr else {
            if subscripts.iter().any(|subscript| {
                matches!(
                    subscript,
                    rumoca_ir_solve::TensorUpdateSubscript::Slice { .. }
                )
            }) {
                return Err(CompileError::Backend(
                    "compact tensor-update slices require register-tape lowering".to_string(),
                ));
            }
            for element in 0..count {
                let mut valid = self.fb.ins().iconst(types::I8, 1);
                let mut value_element = 0usize;
                let mut axis_stride = count;
                for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
                    axis_stride /= extent as usize;
                    let coordinate = (element / axis_stride) % extent as usize;
                    match *subscript {
                        rumoca_ir_solve::TensorUpdateSubscript::Whole => {
                            value_element = value_element * extent as usize + coordinate;
                        }
                        rumoca_ir_solve::TensorUpdateSubscript::Index(
                            rumoca_ir_solve::TensorIndex::Constant(selected),
                        ) => {
                            if selected as usize != coordinate {
                                valid = self.fb.ins().iconst(types::I8, 0);
                            }
                        }
                        rumoca_ir_solve::TensorUpdateSubscript::Index(
                            rumoca_ir_solve::TensorIndex::Runtime(register),
                        ) => {
                            let selected = self.lookup(register)?;
                            let coordinate = self.fb.ins().f64const((coordinate + 1) as f64);
                            let matches = self.fb.ins().fcmp(FloatCC::Equal, selected, coordinate);
                            valid = self.fb.ins().band(valid, matches);
                        }
                        rumoca_ir_solve::TensorUpdateSubscript::Slice { .. } => unreachable!(),
                    }
                }
                for lane in 0..lanes {
                    let base = self.lookup(checked_reg_offset(
                        base_start,
                        element * lanes + lane,
                        "tensor update base",
                    )?)?;
                    let value = self.lookup(checked_reg_offset(
                        value_start,
                        value_element * lanes + lane,
                        "tensor update value",
                    )?)?;
                    let selected = self.fb.ins().select(valid, value, base);
                    self.insert(
                        checked_reg_offset(
                            dst_start,
                            element * lanes + lane,
                            "tensor update output",
                        )?,
                        selected,
                    )?;
                }
            }
            return Ok(None);
        };

        let header = self.fb.create_block();
        let body = self.fb.create_block();
        let exit = self.fb.create_block();
        self.fb.append_block_param(header, types::I64);
        let zero = self.fb.ins().iconst(types::I64, 0);
        self.fb.ins().jump(header, &[zero.into()]);
        self.fb.switch_to_block(header);
        let element = self.fb.block_params(header)[0];
        let in_range = self.fb.ins().icmp_imm(
            IntCC::UnsignedLessThan,
            element,
            i64::try_from(count)
                .map_err(|_| CompileError::Backend("tensor update extent exceeds i64".into()))?,
        );
        self.fb.ins().brif(in_range, body, &[], exit, &[]);
        self.fb.switch_to_block(body);
        self.fb.seal_block(body);
        let mut valid = self.fb.ins().iconst(types::I8, 1);
        let mut value_element = self.fb.ins().iconst(types::I64, 0);
        let mut axis_stride = count;
        for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
            axis_stride /= extent as usize;
            let coordinate = if axis_stride == 1 {
                element
            } else {
                self.fb.ins().udiv_imm(element, axis_stride as i64)
            };
            let coordinate = if extent == 1 {
                self.fb.ins().iconst(types::I64, 0)
            } else {
                self.fb.ins().urem_imm(coordinate, i64::from(extent))
            };
            match *subscript {
                rumoca_ir_solve::TensorUpdateSubscript::Whole => {
                    value_element = self.fb.ins().imul_imm(value_element, i64::from(extent));
                    value_element = self.fb.ins().iadd(value_element, coordinate);
                }
                rumoca_ir_solve::TensorUpdateSubscript::Index(index) => {
                    let selected = match index {
                        rumoca_ir_solve::TensorIndex::Constant(selected) => {
                            self.fb.ins().iconst(types::I64, i64::from(selected))
                        }
                        rumoca_ir_solve::TensorIndex::Runtime(register) => {
                            let value = self.lookup(register)?;
                            let rounded = self.fb.ins().nearest(value);
                            let one = self.fb.ins().f64const(1.0);
                            let upper = self.fb.ins().f64const(f64::from(extent));
                            let integral = self.fb.ins().fcmp(FloatCC::Equal, value, rounded);
                            let above =
                                self.fb
                                    .ins()
                                    .fcmp(FloatCC::GreaterThanOrEqual, rounded, one);
                            let below =
                                self.fb.ins().fcmp(FloatCC::LessThanOrEqual, rounded, upper);
                            valid = self.fb.ins().band(valid, integral);
                            valid = self.fb.ins().band(valid, above);
                            valid = self.fb.ins().band(valid, below);
                            let clamped = self.fb.ins().fmax(rounded, one);
                            let clamped = self.fb.ins().fmin(clamped, upper);
                            let selected = self.fb.ins().fcvt_to_sint(types::I64, clamped);
                            self.fb.ins().iadd_imm(selected, -1)
                        }
                    };
                    let matches = self.fb.ins().icmp(IntCC::Equal, coordinate, selected);
                    valid = self.fb.ins().band(valid, matches);
                }
                rumoca_ir_solve::TensorUpdateSubscript::Slice {
                    start,
                    ref dimensions,
                } => {
                    let slice_count = dimensions
                        .iter()
                        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
                        .ok_or_else(|| {
                            CompileError::Backend("tensor update slice overflow".into())
                        })?;
                    let scan_header = self.fb.create_block();
                    let scan_body = self.fb.create_block();
                    let scan_next = self.fb.create_block();
                    let scan_done = self.fb.create_block();
                    self.fb.append_block_param(scan_header, types::I64);
                    self.fb.append_block_param(scan_done, types::I8);
                    self.fb.append_block_param(scan_done, types::I64);
                    let zero_position = self.fb.ins().iconst(types::I64, 0);
                    self.fb.ins().jump(scan_header, &[zero_position.into()]);

                    self.fb.switch_to_block(scan_header);
                    let position = self.fb.block_params(scan_header)[0];
                    let in_slice = self.fb.ins().icmp_imm(
                        IntCC::UnsignedLessThan,
                        position,
                        i64::try_from(slice_count).map_err(|_| {
                            CompileError::Backend("tensor update slice extent exceeds i64".into())
                        })?,
                    );
                    let not_found = self.fb.ins().iconst(types::I8, 0);
                    let no_position = self.fb.ins().iconst(types::I64, 0);
                    self.fb.ins().brif(
                        in_slice,
                        scan_body,
                        &[],
                        scan_done,
                        &[not_found.into(), no_position.into()],
                    );

                    self.fb.switch_to_block(scan_body);
                    self.fb.seal_block(scan_body);
                    let register = self.fb.ins().iadd_imm(position, i64::from(start));
                    let byte_offset = self.fb.ins().imul_imm(register, 8);
                    let address = self.fb.ins().iadd(regs_ptr, byte_offset);
                    let candidate = self.fb.ins().load(types::F64, self.flags, address, 0);
                    let expected = self.fb.ins().iadd_imm(coordinate, 1);
                    let expected = self.fb.ins().fcvt_from_uint(types::F64, expected);
                    let matches = self.fb.ins().fcmp(FloatCC::Equal, candidate, expected);
                    let found = self.fb.ins().iconst(types::I8, 1);
                    self.fb.ins().brif(
                        matches,
                        scan_done,
                        &[found.into(), position.into()],
                        scan_next,
                        &[],
                    );

                    self.fb.switch_to_block(scan_next);
                    self.fb.seal_block(scan_next);
                    let next_position = self.fb.ins().iadd_imm(position, 1);
                    self.fb.ins().jump(scan_header, &[next_position.into()]);
                    self.fb.seal_block(scan_header);

                    self.fb.switch_to_block(scan_done);
                    self.fb.seal_block(scan_done);
                    let found = self.fb.block_params(scan_done)[0];
                    let selected = self.fb.block_params(scan_done)[1];
                    valid = self.fb.ins().band(valid, found);
                    value_element = self.fb.ins().imul_imm(value_element, slice_count as i64);
                    value_element = self.fb.ins().iadd(value_element, selected);
                }
            }
        }
        let base_register = self.fb.ins().imul_imm(element, lanes as i64);
        let base_register = self.fb.ins().iadd_imm(base_register, i64::from(base_start));
        let value_register = self.fb.ins().imul_imm(value_element, lanes as i64);
        let value_register = self
            .fb
            .ins()
            .iadd_imm(value_register, i64::from(value_start));
        let dst_register = self.fb.ins().imul_imm(element, lanes as i64);
        let dst_register = self.fb.ins().iadd_imm(dst_register, i64::from(dst_start));
        let base_address = self.fb.ins().imul_imm(base_register, 8);
        let base_address = self.fb.ins().iadd(regs_ptr, base_address);
        let value_address = self.fb.ins().imul_imm(value_register, 8);
        let value_address = self.fb.ins().iadd(regs_ptr, value_address);
        let dst_address = self.fb.ins().imul_imm(dst_register, 8);
        let dst_address = self.fb.ins().iadd(regs_ptr, dst_address);
        for lane in 0..lanes {
            let byte_offset = i32::try_from(lane * 8).map_err(|_| {
                CompileError::Backend("tensor update lane offset exceeds i32".into())
            })?;
            let base = self
                .fb
                .ins()
                .load(types::F64, self.flags, base_address, byte_offset);
            let value = self
                .fb
                .ins()
                .load(types::F64, self.flags, value_address, byte_offset);
            let selected = self.fb.ins().select(valid, value, base);
            self.fb
                .ins()
                .store(self.flags, selected, dst_address, byte_offset);
        }
        let next = self.fb.ins().iadd_imm(element, 1);
        self.fb.ins().jump(header, &[next.into()]);
        self.fb.seal_block(header);
        self.fb.switch_to_block(exit);
        self.fb.seal_block(exit);
        Ok(None)
    }

    fn lower_tensor_fill(
        &mut self,
        dst_start: u32,
        value_start: u32,
        count: usize,
        lanes: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let Some(regs_ptr) = self.backing_regs_ptr else {
            self.lower_static_tensor_fill(dst_start, value_start, count, lanes)?;
            return Ok(None);
        };
        let header = self.fb.create_block();
        let body = self.fb.create_block();
        let exit = self.fb.create_block();
        self.fb.append_block_param(header, types::I64);
        let zero = self.fb.ins().iconst(types::I64, 0);
        self.fb.ins().jump(header, &[zero.into()]);
        self.fb.switch_to_block(header);
        let element = self.fb.block_params(header)[0];
        let in_range = self.fb.ins().icmp_imm(
            IntCC::UnsignedLessThan,
            element,
            i64::try_from(count)
                .map_err(|_| CompileError::Backend("tensor fill extent exceeds i64".into()))?,
        );
        self.fb.ins().brif(in_range, body, &[], exit, &[]);
        self.fb.switch_to_block(body);
        self.fb.seal_block(body);
        let dst_register = self.fb.ins().imul_imm(element, lanes as i64);
        let dst_register = self.fb.ins().iadd_imm(dst_register, i64::from(dst_start));
        let dst_offset = self.fb.ins().imul_imm(dst_register, 8);
        let dst_address = self.fb.ins().iadd(regs_ptr, dst_offset);
        for lane in 0..lanes {
            let value_register = checked_reg_offset(value_start, lane, "tensor fill value")?;
            let value_offset = self
                .fb
                .ins()
                .iconst(types::I64, i64::from(value_register) * 8);
            let value_address = self.fb.ins().iadd(regs_ptr, value_offset);
            let value = self.fb.ins().load(types::F64, self.flags, value_address, 0);
            self.fb.ins().store(
                self.flags,
                value,
                dst_address,
                i32::try_from(lane * 8).map_err(|_| {
                    CompileError::Backend("tensor fill lane offset exceeds i32".into())
                })?,
            );
        }
        let next = self.fb.ins().iadd_imm(element, 1);
        self.fb.ins().jump(header, &[next.into()]);
        self.fb.seal_block(header);
        self.fb.switch_to_block(exit);
        self.fb.seal_block(exit);
        Ok(None)
    }

    fn lower_static_tensor_fill(
        &mut self,
        dst_start: u32,
        value_start: u32,
        count: usize,
        lanes: usize,
    ) -> Result<(), CompileError> {
        let values = (0..lanes)
            .map(|lane| self.lookup(checked_reg_offset(value_start, lane, "tensor fill value")?))
            .collect::<Result<Vec<_>, CompileError>>()?;
        for element in 0..count {
            for (lane, &value) in values.iter().enumerate() {
                self.insert(
                    checked_reg_offset(dst_start, element * lanes + lane, "tensor fill output")?,
                    value,
                )?;
            }
        }
        Ok(())
    }

    fn lower_tensor_identity(
        &mut self,
        dst_start: u32,
        size: usize,
        lanes: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let count = size
            .checked_mul(size)
            .ok_or_else(|| CompileError::Backend("tensor identity extent overflow".into()))?;
        let Some(regs_ptr) = self.backing_regs_ptr else {
            self.lower_static_tensor_identity(dst_start, size, lanes)?;
            return Ok(None);
        };
        let header = self.fb.create_block();
        let body = self.fb.create_block();
        let exit = self.fb.create_block();
        self.fb.append_block_param(header, types::I64);
        let zero = self.fb.ins().iconst(types::I64, 0);
        self.fb.ins().jump(header, &[zero.into()]);
        self.fb.switch_to_block(header);
        let element = self.fb.block_params(header)[0];
        let in_range = self.fb.ins().icmp_imm(
            IntCC::UnsignedLessThan,
            element,
            i64::try_from(count)
                .map_err(|_| CompileError::Backend("tensor identity extent exceeds i64".into()))?,
        );
        self.fb.ins().brif(in_range, body, &[], exit, &[]);
        self.fb.switch_to_block(body);
        self.fb.seal_block(body);
        let row = self.fb.ins().udiv_imm(element, size as i64);
        let column = self.fb.ins().urem_imm(element, size as i64);
        let diagonal = self.fb.ins().icmp(IntCC::Equal, row, column);
        let one = self.fb.ins().f64const(1.0);
        let zero_value = self.fb.ins().f64const(0.0);
        let primal = self.fb.ins().select(diagonal, one, zero_value);
        let dst_register = self.fb.ins().imul_imm(element, lanes as i64);
        let dst_register = self.fb.ins().iadd_imm(dst_register, i64::from(dst_start));
        let dst_offset = self.fb.ins().imul_imm(dst_register, 8);
        let dst_address = self.fb.ins().iadd(regs_ptr, dst_offset);
        self.fb.ins().store(self.flags, primal, dst_address, 0);
        if lanes == 2 {
            self.fb.ins().store(self.flags, zero_value, dst_address, 8);
        }
        let next = self.fb.ins().iadd_imm(element, 1);
        self.fb.ins().jump(header, &[next.into()]);
        self.fb.seal_block(header);
        self.fb.switch_to_block(exit);
        self.fb.seal_block(exit);
        Ok(None)
    }

    fn lower_static_tensor_identity(
        &mut self,
        dst_start: u32,
        size: usize,
        lanes: usize,
    ) -> Result<(), CompileError> {
        for row in 0..size {
            for column in 0..size {
                self.lower_static_tensor_identity_cell(dst_start, size, lanes, row, column)?;
            }
        }
        Ok(())
    }

    fn lower_static_tensor_identity_cell(
        &mut self,
        dst_start: u32,
        size: usize,
        lanes: usize,
        row: usize,
        column: usize,
    ) -> Result<(), CompileError> {
        for lane in 0..lanes {
            let value = self
                .fb
                .ins()
                .f64const(f64::from(lane == 0 && row == column));
            self.insert(
                checked_reg_offset(
                    dst_start,
                    (row * size + column) * lanes + lane,
                    "tensor identity output",
                )?,
                value,
            )?;
        }
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn lower_tensor_load(
        &mut self,
        dst_start: u32,
        input: rumoca_ir_solve::TensorInputKind,
        input_start: usize,
        count: usize,
        seed_start: Option<usize>,
        lanes: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let input_ptr = match input {
            rumoca_ir_solve::TensorInputKind::Y => self.y_ptr,
            rumoca_ir_solve::TensorInputKind::P => self.p_ptr,
        };
        let Some(regs_ptr) = self.backing_regs_ptr else {
            self.lower_static_tensor_load(dst_start, input, input_start, count, seed_start, lanes)?;
            return Ok(None);
        };
        let header = self.fb.create_block();
        let body = self.fb.create_block();
        let exit = self.fb.create_block();
        self.fb.append_block_param(header, types::I64);
        let zero = self.fb.ins().iconst(types::I64, 0);
        self.fb.ins().jump(header, &[zero.into()]);
        self.fb.switch_to_block(header);
        let element = self.fb.block_params(header)[0];
        let in_range = self.fb.ins().icmp_imm(
            IntCC::UnsignedLessThan,
            element,
            i64::try_from(count)
                .map_err(|_| CompileError::Backend("tensor load extent exceeds i64".into()))?,
        );
        self.fb.ins().brif(in_range, body, &[], exit, &[]);
        self.fb.switch_to_block(body);
        self.fb.seal_block(body);
        let input_element = self.fb.ins().iadd_imm(
            element,
            i64::try_from(input_start)
                .map_err(|_| CompileError::Backend("tensor load input exceeds i64".into()))?,
        );
        let input_offset = self.fb.ins().imul_imm(input_element, 8);
        let input_address = self.fb.ins().iadd(input_ptr, input_offset);
        let value = self.fb.ins().load(types::F64, self.flags, input_address, 0);
        let dst_register = self.fb.ins().imul_imm(element, lanes as i64);
        let dst_register = self.fb.ins().iadd_imm(dst_register, i64::from(dst_start));
        let dst_offset = self.fb.ins().imul_imm(dst_register, 8);
        let dst_address = self.fb.ins().iadd(regs_ptr, dst_offset);
        self.fb.ins().store(self.flags, value, dst_address, 0);
        if lanes == 2 {
            let tangent = self.dynamic_tensor_load_tangent(seed_start, element)?;
            self.fb.ins().store(self.flags, tangent, dst_address, 8);
        }
        let next = self.fb.ins().iadd_imm(element, 1);
        self.fb.ins().jump(header, &[next.into()]);
        self.fb.seal_block(header);
        self.fb.switch_to_block(exit);
        self.fb.seal_block(exit);
        Ok(None)
    }

    fn dynamic_tensor_load_tangent(
        &mut self,
        seed_start: Option<usize>,
        element: cranelift_codegen::ir::Value,
    ) -> Result<cranelift_codegen::ir::Value, CompileError> {
        let Some(seed_start) = seed_start else {
            return Ok(self.fb.ins().f64const(0.0));
        };
        let seed_ptr = self.v_ptr.ok_or_else(|| {
            CompileError::Backend("seeded tensor load escaped a seed-aware kernel".into())
        })?;
        let seed_start = i64::try_from(seed_start)
            .map_err(|_| CompileError::Backend("tensor load seed exceeds i64".into()))?;
        let seed_element = self.fb.ins().iadd_imm(element, seed_start);
        let seed_offset = self.fb.ins().imul_imm(seed_element, 8);
        let seed_address = self.fb.ins().iadd(seed_ptr, seed_offset);
        Ok(self.fb.ins().load(types::F64, self.flags, seed_address, 0))
    }

    fn lower_static_tensor_load(
        &mut self,
        dst_start: u32,
        input: rumoca_ir_solve::TensorInputKind,
        input_start: usize,
        count: usize,
        seed_start: Option<usize>,
        lanes: usize,
    ) -> Result<(), CompileError> {
        for element in 0..count {
            let dst = checked_reg_offset(dst_start, element * lanes, "tensor load output")?;
            match input {
                rumoca_ir_solve::TensorInputKind::Y => {
                    self.lower_loaded_y_reg(dst, input_start + element)?;
                }
                rumoca_ir_solve::TensorInputKind::P => {
                    self.lower_loaded_p_reg(dst, input_start + element)?;
                }
            }
            if lanes == 2 {
                let tangent = self.static_tensor_load_tangent(seed_start, element)?;
                self.insert(dst + 1, tangent)?;
            }
        }
        Ok(())
    }

    fn static_tensor_load_tangent(
        &mut self,
        seed_start: Option<usize>,
        element: usize,
    ) -> Result<cranelift_codegen::ir::Value, CompileError> {
        let Some(seed_start) = seed_start else {
            return Ok(self.fb.ins().f64const(0.0));
        };
        let seed_ptr = self.v_ptr.ok_or_else(|| {
            CompileError::Backend("seeded tensor load escaped a seed-aware kernel".into())
        })?;
        load_f64(self.fb, self.flags, seed_ptr, seed_start + element)
    }

    /// Lower `mem[base_ptr + 8*(base + clamp(round(index_reg), 0, count-1))]`,
    /// matching [`rumoca_ir_solve::resolve_indexed_slot`]: round the runtime
    /// f64 index to nearest, clamp into `[0, count-1]`, convert to an integer
    /// byte offset, then add the run-relative `base` and dereference.
    fn lower_indexed_loaded_reg(
        &mut self,
        dst: u32,
        base_ptr: cranelift_codegen::ir::Value,
        base: usize,
        count: usize,
        index: u32,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let idx = self.lookup(index)?;
        let rounded = self.fb.ins().nearest(idx);
        let zero = self.fb.ins().f64const(0.0);
        let lo = self.fb.ins().fmax(rounded, zero);
        let last_index = if count == 0 { 0 } else { count - 1 };
        let last = self.fb.ins().f64const(last_index as f64);
        let clamped = self.fb.ins().fmin(lo, last);
        let idx_i = self.fb.ins().fcvt_to_sint(types::I64, clamped);
        let byte_off = self
            .fb
            .ins()
            .imul_imm(idx_i, std::mem::size_of::<f64>() as i64);
        let base_bytes = base
            .checked_mul(std::mem::size_of::<f64>())
            .ok_or_else(|| {
                CompileError::Backend("indexed load base byte offset overflow".to_string())
            })?;
        let base_bytes = i64::try_from(base_bytes)
            .map_err(|_| CompileError::Backend("indexed load base exceeds i64".to_string()))?;
        let abs_off = self.fb.ins().iadd_imm(byte_off, base_bytes);
        let addr = self.fb.ins().iadd(base_ptr, abs_off);
        let value = self.fb.ins().load(types::F64, self.flags, addr, 0);
        self.insert(dst, value)
    }

    fn lower_indexed_register(
        &mut self,
        dst: u32,
        base: u32,
        stride: usize,
        dimensions: &[u32],
        indices: &[rumoca_ir_solve::TensorIndex],
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        if dimensions.len() != indices.len() {
            return Err(CompileError::Backend(
                "runtime tensor projection rank mismatch".to_string(),
            ));
        }
        if let Some(offset) = self.constant_tensor_offset(dimensions, indices) {
            let value = match offset {
                Some(offset) => self.lookup(checked_strided_register(
                    base,
                    offset,
                    stride,
                    "constant tensor projection source",
                )?)?,
                None => self.fb.ins().f64const(f64::NAN),
            };
            return self.insert(dst, value);
        }
        let count = dimensions
            .iter()
            .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
            .ok_or_else(|| {
                CompileError::Backend("runtime tensor projection extent overflow".to_string())
            })?;
        let byte_count = count
            .checked_mul(std::mem::size_of::<f64>())
            .and_then(|bytes| u32::try_from(bytes).ok())
            .ok_or_else(|| {
                CompileError::Backend("runtime tensor projection stack size overflow".to_string())
            })?;
        let slot = self.fb.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            byte_count,
            3,
        ));
        for offset in 0..count {
            let register =
                checked_strided_register(base, offset, stride, "runtime tensor projection source")?;
            let value = self.lookup(register)?;
            let byte_offset = i32::try_from(offset * std::mem::size_of::<f64>()).map_err(|_| {
                CompileError::Backend(
                    "runtime tensor projection stack offset exceeds i32".to_string(),
                )
            })?;
            self.fb.ins().stack_store(value, slot, byte_offset);
        }

        let (flat, valid) = self.lower_tensor_offset(dimensions, indices)?;
        let byte_offset = self
            .fb
            .ins()
            .imul_imm(flat, std::mem::size_of::<f64>() as i64);
        let stack = self.fb.ins().stack_addr(types::I64, slot, 0);
        let address = self.fb.ins().iadd(stack, byte_offset);
        let projected = self.fb.ins().load(types::F64, self.flags, address, 0);
        let out_of_range = self.fb.ins().f64const(f64::NAN);
        let value = self.fb.ins().select(valid, projected, out_of_range);
        self.insert(dst, value)
    }

    fn lower_indexed_fold_carried(
        &mut self,
        dst: u32,
        base: usize,
        stride: usize,
        dimensions: &[u32],
        indices: &[rumoca_ir_solve::TensorIndex],
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let carried = self.fold_carried.ok_or_else(|| {
            CompileError::Backend("indexed function-fold carried load outside fold".to_string())
        })?;
        if dimensions.len() != indices.len() {
            return Err(CompileError::Backend(
                "runtime tensor projection rank mismatch".to_string(),
            ));
        }
        if let Some(offset) = self.constant_tensor_offset(dimensions, indices) {
            let value = self.constant_fold_carried_value(carried, base, stride, offset)?;
            return self.insert(dst, value);
        }
        let (flat, valid) = self.lower_tensor_offset(dimensions, indices)?;
        let stride = i64::try_from(stride).map_err(|_| {
            CompileError::Backend("function-fold tensor stride exceeds i64".to_string())
        })?;
        let base = i64::try_from(base).map_err(|_| {
            CompileError::Backend("function-fold tensor base exceeds i64".to_string())
        })?;
        let element_offset = self.fb.ins().imul_imm(flat, stride);
        let element_offset = self.fb.ins().iadd_imm(element_offset, base);
        let byte_offset = self
            .fb
            .ins()
            .imul_imm(element_offset, std::mem::size_of::<f64>() as i64);
        let stack = self.fb.ins().stack_addr(types::I64, carried, 0);
        let address = self.fb.ins().iadd(stack, byte_offset);
        let projected = self.fb.ins().load(types::F64, self.flags, address, 0);
        let out_of_range = self.fb.ins().f64const(f64::NAN);
        let value = self.fb.ins().select(valid, projected, out_of_range);
        self.insert(dst, value)
    }

    fn constant_fold_carried_value(
        &mut self,
        carried: StackSlot,
        base: usize,
        stride: usize,
        offset: Option<usize>,
    ) -> Result<cranelift_codegen::ir::Value, CompileError> {
        let Some(offset) = offset else {
            return Ok(self.fb.ins().f64const(f64::NAN));
        };
        let offset = offset
            .checked_mul(stride)
            .ok_or_else(|| CompileError::Backend("function-fold tensor offset overflow".into()))?;
        let element = base
            .checked_add(offset)
            .ok_or_else(|| CompileError::Backend("function-fold tensor offset overflow".into()))?;
        Ok(self.fb.ins().stack_load(
            types::F64,
            carried,
            stack_element_byte_offset(element, "function-fold tensor")?,
        ))
    }

    fn lower_indexed_fold_capture(
        &mut self,
        dst: u32,
        base: usize,
        stride: usize,
        dimensions: &[u32],
        indices: &[rumoca_ir_solve::TensorIndex],
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        if dimensions.len() != indices.len() {
            return Err(CompileError::Backend(
                "runtime tensor projection rank mismatch".to_string(),
            ));
        }
        if let Some(offset) = self.constant_tensor_offset(dimensions, indices) {
            let value = match offset {
                Some(offset) => self
                    .fold_captures
                    .and_then(|captures| captures.get(base + offset * stride))
                    .copied()
                    .ok_or_else(|| {
                        CompileError::Backend(
                            "constant function-fold capture projection is out of range".to_string(),
                        )
                    })?,
                None => self.fb.ins().f64const(f64::NAN),
            };
            return self.insert(dst, value);
        }
        let captures_ptr = self.fold_captures_ptr.ok_or_else(|| {
            CompileError::Backend(
                "indexed function-fold capture has no addressable capture tuple".to_string(),
            )
        })?;
        let (flat, valid) = self.lower_tensor_offset(dimensions, indices)?;
        let element = self.fb.ins().imul_imm(
            flat,
            i64::try_from(stride).map_err(|_| {
                CompileError::Backend("function-fold capture stride exceeds i64".to_string())
            })?,
        );
        let element = self.fb.ins().iadd_imm(
            element,
            i64::try_from(base).map_err(|_| {
                CompileError::Backend("function-fold capture base exceeds i64".to_string())
            })?,
        );
        let byte_offset = self.fb.ins().imul_imm(element, 8);
        let address = self.fb.ins().iadd(captures_ptr, byte_offset);
        let projected = self.fb.ins().load(types::F64, self.flags, address, 0);
        let out_of_range = self.fb.ins().f64const(f64::NAN);
        let value = self.fb.ins().select(valid, projected, out_of_range);
        self.insert(dst, value)
    }

    /// Resolve binder-derived tensor projections while a tiny fixed fold is
    /// unrolled at final machine-code emission. `Some(None)` is a statically
    /// invalid Modelica index; `None` means at least one coordinate is dynamic.
    fn constant_tensor_offset(
        &self,
        dimensions: &[u32],
        indices: &[rumoca_ir_solve::TensorIndex],
    ) -> Option<Option<usize>> {
        let mut flat = 0usize;
        for (&extent, index) in dimensions.iter().zip(indices) {
            let Some(coordinate) = self.constant_tensor_coordinate(extent, *index)? else {
                return Some(None);
            };
            if coordinate >= extent as usize {
                return Some(None);
            }
            flat = flat.checked_mul(extent as usize)?.checked_add(coordinate)?;
        }
        Some(Some(flat))
    }

    fn constant_tensor_coordinate(
        &self,
        extent: u32,
        index: rumoca_ir_solve::TensorIndex,
    ) -> Option<Option<usize>> {
        match index {
            rumoca_ir_solve::TensorIndex::Constant(coordinate) => Some(Some(coordinate as usize)),
            rumoca_ir_solve::TensorIndex::Runtime(register) => {
                let value = self.known_constants.get(&register).copied()?;
                let rounded = value.round_ties_even();
                let valid = value.is_finite()
                    && value == rounded
                    && rounded >= 1.0
                    && rounded <= f64::from(extent);
                Some(valid.then(|| rounded as usize - 1))
            }
        }
    }

    fn lower_tensor_offset(
        &mut self,
        dimensions: &[u32],
        indices: &[rumoca_ir_solve::TensorIndex],
    ) -> Result<(cranelift_codegen::ir::Value, cranelift_codegen::ir::Value), CompileError> {
        if dimensions.len() != indices.len() {
            return Err(CompileError::Backend(
                "runtime tensor projection rank mismatch".to_string(),
            ));
        }
        let mut flat = self.fb.ins().iconst(types::I64, 0);
        let mut valid = self.fb.ins().iconst(types::I8, 1);
        for (&extent, index) in dimensions.iter().zip(indices) {
            let coordinate = match *index {
                rumoca_ir_solve::TensorIndex::Constant(coordinate) => {
                    self.fb.ins().iconst(types::I64, i64::from(coordinate))
                }
                rumoca_ir_solve::TensorIndex::Runtime(register) => {
                    let value = self.lookup(register)?;
                    let rounded = self.fb.ins().nearest(value);
                    let one = self.fb.ins().f64const(1.0);
                    let upper = self.fb.ins().f64const(f64::from(extent));
                    let integral = self.fb.ins().fcmp(FloatCC::Equal, value, rounded);
                    let above_lower = self
                        .fb
                        .ins()
                        .fcmp(FloatCC::GreaterThanOrEqual, rounded, one);
                    let below_upper = self.fb.ins().fcmp(FloatCC::LessThanOrEqual, rounded, upper);
                    let axis_valid = self.fb.ins().band(integral, above_lower);
                    let axis_valid = self.fb.ins().band(axis_valid, below_upper);
                    valid = self.fb.ins().band(valid, axis_valid);
                    let clamped = self.fb.ins().fmax(rounded, one);
                    let clamped = self.fb.ins().fmin(clamped, upper);
                    let one_based = self.fb.ins().fcvt_to_sint(types::I64, clamped);
                    self.fb.ins().iadd_imm(one_based, -1)
                }
            };
            flat = self.fb.ins().imul_imm(flat, i64::from(extent));
            flat = self.fb.ins().iadd(flat, coordinate);
        }
        Ok((flat, valid))
    }

    #[allow(clippy::too_many_arguments)]
    // SPEC_0021: exhaustive dispatch over checked fold tensor-update nodes.
    #[expect(
        clippy::too_many_lines,
        clippy::excessive_nesting,
        reason = "fold tensor updates exhaustively lower every checked node and subscript"
    )]
    fn lower_fold_tensor_update(
        &mut self,
        carried: StackSlot,
        output_base: usize,
        source_base: usize,
        source_stride: usize,
        dimensions: &[u32],
        updates: &[rumoca_ir_solve::FoldTensorUpdate],
        nodes: &[rumoca_ir_solve::FoldTensorNode],
        result: u32,
        lanes: usize,
    ) -> Result<(), CompileError> {
        if output_base != source_base || updates.is_empty() {
            return Err(CompileError::Backend(
                "invalid compact function-fold tensor update".to_string(),
            ));
        }
        let count = dimensions
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
            .ok_or_else(|| {
                CompileError::Backend("function-fold tensor update extent overflow".to_string())
            })?;
        if self.try_lower_scalar_fold_tensor_update(
            carried,
            source_base,
            source_stride,
            dimensions,
            updates,
            nodes,
            result,
            lanes,
        )? {
            return Ok(());
        }
        let mut update_values = Vec::with_capacity(updates.len());
        for update in updates {
            if update.subscripts.len() != dimensions.len() {
                return Err(CompileError::Backend(
                    "function-fold tensor patch rank mismatch".to_string(),
                ));
            }
            let value_count = dimensions
                .iter()
                .zip(update.subscripts.iter())
                .try_fold(1usize, |count, (&extent, subscript)| {
                    if matches!(subscript, rumoca_ir_solve::TensorSubscript::Whole) {
                        count.checked_mul(extent as usize)
                    } else {
                        Some(count)
                    }
                })
                .ok_or_else(|| {
                    CompileError::Backend(
                        "function-fold tensor update value extent overflow".to_string(),
                    )
                })?;
            let value_bytes = value_count
                .checked_mul(lanes)
                .and_then(|count| count.checked_mul(std::mem::size_of::<f64>()))
                .and_then(|bytes| u32::try_from(bytes).ok())
                .ok_or_else(|| {
                    CompileError::Backend(
                        "function-fold tensor update value storage overflow".to_string(),
                    )
                })?;
            let values = self.fb.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                value_bytes,
                3,
            ));
            for element in 0..value_count {
                for lane in 0..lanes {
                    let register = checked_strided_register(
                        update.value_start,
                        element,
                        update.value_stride,
                        "function-fold tensor update value",
                    )?
                    .checked_add(u32::try_from(lane).map_err(|_| {
                        CompileError::Backend(
                            "function-fold tensor update lane exceeds register identity"
                                .to_string(),
                        )
                    })?)
                    .ok_or_else(|| {
                        CompileError::Backend(
                            "function-fold tensor update value register overflow".to_string(),
                        )
                    })?;
                    let value = self.lookup(register)?;
                    self.fb.ins().stack_store(
                        value,
                        values,
                        i32::try_from((element * lanes + lane) * std::mem::size_of::<f64>())
                            .map_err(|_| {
                                CompileError::Backend(
                                    "function-fold tensor update value offset exceeds i32"
                                        .to_string(),
                                )
                            })?,
                    );
                }
            }
            update_values.push(values);
        }

        let header = self.fb.create_block();
        let body = self.fb.create_block();
        let exit = self.fb.create_block();
        self.fb.append_block_param(header, types::I64);
        let zero = self.fb.ins().iconst(types::I64, 0);
        self.fb.ins().jump(header, &[zero.into()]);

        self.fb.switch_to_block(header);
        let element = self.fb.block_params(header)[0];
        let in_range = self.fb.ins().icmp_imm(
            IntCC::UnsignedLessThan,
            element,
            i64::try_from(count).map_err(|_| {
                CompileError::Backend("function-fold tensor update count exceeds i64".to_string())
            })?,
        );
        self.fb.ins().brif(in_range, body, &[], exit, &[]);

        self.fb.switch_to_block(body);
        self.fb.seal_block(body);
        let mut patches = Vec::with_capacity(updates.len());
        for (update, values) in updates.iter().zip(update_values.iter().copied()) {
            let mut valid = self.fb.ins().iconst(types::I8, 1);
            if let Some(condition) = update.condition {
                let condition = self.lookup(condition)?;
                let zero = self.fb.ins().f64const(0.0);
                let enabled = self.fb.ins().fcmp(FloatCC::NotEqual, condition, zero);
                valid = self.fb.ins().band(valid, enabled);
            }
            let mut value_element = self.fb.ins().iconst(types::I64, 0);
            let mut axis_stride = count;
            for (&extent, subscript) in dimensions.iter().zip(update.subscripts.iter()) {
                axis_stride /= extent as usize;
                let coordinate = if axis_stride == 1 {
                    element
                } else {
                    self.fb.ins().udiv_imm(element, axis_stride as i64)
                };
                let coordinate = if extent == 1 {
                    self.fb.ins().iconst(types::I64, 0)
                } else {
                    self.fb.ins().urem_imm(coordinate, i64::from(extent))
                };
                match *subscript {
                    rumoca_ir_solve::TensorSubscript::Whole => {
                        value_element = self.fb.ins().imul_imm(value_element, i64::from(extent));
                        value_element = self.fb.ins().iadd(value_element, coordinate);
                    }
                    rumoca_ir_solve::TensorSubscript::Index(index) => {
                        let selected = match index {
                            rumoca_ir_solve::TensorIndex::Constant(selected) => {
                                self.fb.ins().iconst(types::I64, i64::from(selected))
                            }
                            rumoca_ir_solve::TensorIndex::Runtime(register) => {
                                let value = self.lookup(register)?;
                                let rounded = self.fb.ins().nearest(value);
                                let one = self.fb.ins().f64const(1.0);
                                let upper = self.fb.ins().f64const(f64::from(extent));
                                let integral = self.fb.ins().fcmp(FloatCC::Equal, value, rounded);
                                let above =
                                    self.fb
                                        .ins()
                                        .fcmp(FloatCC::GreaterThanOrEqual, rounded, one);
                                let below =
                                    self.fb.ins().fcmp(FloatCC::LessThanOrEqual, rounded, upper);
                                valid = self.fb.ins().band(valid, integral);
                                valid = self.fb.ins().band(valid, above);
                                valid = self.fb.ins().band(valid, below);
                                let clamped = self.fb.ins().fmax(rounded, one);
                                let clamped = self.fb.ins().fmin(clamped, upper);
                                let selected = self.fb.ins().fcvt_to_sint(types::I64, clamped);
                                self.fb.ins().iadd_imm(selected, -1)
                            }
                        };
                        let matches = self.fb.ins().icmp(IntCC::Equal, coordinate, selected);
                        valid = self.fb.ins().band(valid, matches);
                    }
                }
            }
            patches.push((valid, value_element, values));
        }
        let carried_base = self.fb.ins().stack_addr(types::I64, carried, 0);
        for lane in 0..lanes {
            let source_element = self.fb.ins().imul_imm(
                element,
                i64::try_from(source_stride).map_err(|_| {
                    CompileError::Backend(
                        "function-fold tensor update source stride exceeds i64".to_string(),
                    )
                })?,
            );
            let source_element = self.fb.ins().iadd_imm(
                source_element,
                i64::try_from(source_base + lane).map_err(|_| {
                    CompileError::Backend(
                        "function-fold tensor update source offset exceeds i64".to_string(),
                    )
                })?,
            );
            let source_bytes = self
                .fb
                .ins()
                .imul_imm(source_element, std::mem::size_of::<f64>() as i64);
            let source_address = self.fb.ins().iadd(carried_base, source_bytes);
            let source = self
                .fb
                .ins()
                .load(types::F64, self.flags, source_address, 0);
            let mut node_values = Vec::with_capacity(nodes.len() + 1);
            node_values.push(source);
            for node in nodes {
                let value = match *node {
                    rumoca_ir_solve::FoldTensorNode::Update { base, update } => {
                        let (valid, value_element, values) = patches[update as usize];
                        let selected_element = self.fb.ins().imul_imm(
                            value_element,
                            i64::try_from(lanes).map_err(|_| {
                                CompileError::Backend(
                                    "function-fold tensor update lane count exceeds i64"
                                        .to_string(),
                                )
                            })?,
                        );
                        let selected_element = self.fb.ins().iadd_imm(
                            selected_element,
                            i64::try_from(lane).map_err(|_| {
                                CompileError::Backend(
                                    "function-fold tensor update value lane exceeds i64"
                                        .to_string(),
                                )
                            })?,
                        );
                        let selected_bytes = self
                            .fb
                            .ins()
                            .imul_imm(selected_element, std::mem::size_of::<f64>() as i64);
                        let values_base = self.fb.ins().stack_addr(types::I64, values, 0);
                        let selected_address = self.fb.ins().iadd(values_base, selected_bytes);
                        let selected =
                            self.fb
                                .ins()
                                .load(types::F64, self.flags, selected_address, 0);
                        self.fb
                            .ins()
                            .select(valid, selected, node_values[base as usize])
                    }
                    rumoca_ir_solve::FoldTensorNode::Select {
                        condition,
                        if_true,
                        if_false,
                    } => {
                        let condition = self.lookup(condition)?;
                        let zero = self.fb.ins().f64const(0.0);
                        let condition = self.fb.ins().fcmp(FloatCC::NotEqual, condition, zero);
                        self.fb.ins().select(
                            condition,
                            node_values[if_true as usize],
                            node_values[if_false as usize],
                        )
                    }
                };
                node_values.push(value);
            }
            self.fb
                .ins()
                .store(self.flags, node_values[result as usize], source_address, 0);
        }
        let next = self.fb.ins().iadd_imm(element, 1);
        self.fb.ins().jump(header, &[next.into()]);
        self.fb.seal_block(header);
        self.fb.switch_to_block(exit);
        self.fb.seal_block(exit);
        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    fn try_lower_scalar_fold_tensor_update(
        &mut self,
        carried: StackSlot,
        source_base: usize,
        source_stride: usize,
        dimensions: &[u32],
        updates: &[rumoca_ir_solve::FoldTensorUpdate],
        nodes: &[rumoca_ir_solve::FoldTensorNode],
        result: u32,
        lanes: usize,
    ) -> Result<bool, CompileError> {
        let Some(first) = updates.first() else {
            return Ok(false);
        };
        if updates
            .iter()
            .any(|update| update.subscripts != first.subscripts)
        {
            return Ok(false);
        }
        let indices = first
            .subscripts
            .iter()
            .map(|subscript| match subscript {
                rumoca_ir_solve::TensorSubscript::Index(index) => Some(*index),
                rumoca_ir_solve::TensorSubscript::Whole => None,
            })
            .collect::<Option<Vec<_>>>();
        let Some(indices) = indices else {
            return Ok(false);
        };

        let (element, address_valid) = self.lower_tensor_offset(dimensions, &indices)?;
        let carried_base = self.fb.ins().stack_addr(types::I64, carried, 0);
        let source_element = self.fb.ins().imul_imm(
            element,
            i64::try_from(source_stride).map_err(|_| {
                CompileError::Backend(
                    "function-fold scalar update source stride exceeds i64".to_string(),
                )
            })?,
        );
        for lane in 0..lanes {
            let lane_element = self.fb.ins().iadd_imm(
                source_element,
                i64::try_from(source_base + lane).map_err(|_| {
                    CompileError::Backend(
                        "function-fold scalar update source offset exceeds i64".to_string(),
                    )
                })?,
            );
            let byte_offset = self.fb.ins().imul_imm(lane_element, 8);
            let address = self.fb.ins().iadd(carried_base, byte_offset);
            let source = self.fb.ins().load(types::F64, self.flags, address, 0);
            let mut node_values = Vec::with_capacity(nodes.len() + 1);
            node_values.push(source);
            for node in nodes {
                let value = self.lower_scalar_fold_tensor_node(
                    node,
                    updates,
                    &node_values,
                    address_valid,
                    lane,
                )?;
                node_values.push(value);
            }
            self.fb
                .ins()
                .store(self.flags, node_values[result as usize], address, 0);
        }
        Ok(true)
    }

    fn lower_scalar_fold_tensor_node(
        &mut self,
        node: &rumoca_ir_solve::FoldTensorNode,
        updates: &[rumoca_ir_solve::FoldTensorUpdate],
        values: &[cranelift_codegen::ir::Value],
        address_valid: cranelift_codegen::ir::Value,
        lane: usize,
    ) -> Result<cranelift_codegen::ir::Value, CompileError> {
        match *node {
            rumoca_ir_solve::FoldTensorNode::Update { base, update } => {
                let update = &updates[update as usize];
                let mut enabled = address_valid;
                if let Some(condition) = update.condition {
                    let condition = self.lookup(condition)?;
                    let zero = self.fb.ins().f64const(0.0);
                    let condition = self.fb.ins().fcmp(FloatCC::NotEqual, condition, zero);
                    enabled = self.fb.ins().band(enabled, condition);
                }
                let lane = u32::try_from(lane).map_err(|_| {
                    CompileError::Backend(
                        "function-fold scalar update lane exceeds register identity".into(),
                    )
                })?;
                let register = update.value_start.checked_add(lane).ok_or_else(|| {
                    CompileError::Backend("function-fold scalar update register overflow".into())
                })?;
                let selected = self.lookup(register)?;
                Ok(self
                    .fb
                    .ins()
                    .select(enabled, selected, values[base as usize]))
            }
            rumoca_ir_solve::FoldTensorNode::Select {
                condition,
                if_true,
                if_false,
            } => {
                let condition = self.lookup(condition)?;
                let zero = self.fb.ins().f64const(0.0);
                let condition = self.fb.ins().fcmp(FloatCC::NotEqual, condition, zero);
                Ok(self.fb.ins().select(
                    condition,
                    values[if_true as usize],
                    values[if_false as usize],
                ))
            }
        }
    }

    fn lower_seed_reg(
        &mut self,
        dst: u32,
        index: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let base = self.v_ptr.ok_or_else(|| {
            CompileError::Backend("LoadSeed in row without seed input".to_string())
        })?;
        self.lower_loaded_reg(dst, base, index)
    }

    fn lower_table_bounds(
        &mut self,
        dst: u32,
        table_id: u32,
        max: bool,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let table_id = self.lookup(table_id)?;
        let kind = if max {
            TableHostFn::BoundsMax
        } else {
            TableHostFn::BoundsMin
        };
        let value = call_table_host(self.fb, self.module, self.math, kind, &[table_id])?;
        self.insert(dst, value)
    }

    fn lower_table_lookup(
        &mut self,
        dst: u32,
        table_id: u32,
        column: u32,
        input: u32,
        kind: TableHostFn,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let table_id = self.lookup(table_id)?;
        let column = self.lookup(column)?;
        let input = self.lookup(input)?;
        let value = call_table_host(
            self.fb,
            self.module,
            self.math,
            kind,
            &[table_id, column, input],
        )?;
        self.insert(dst, value)
    }

    fn lower_table_next_event(
        &mut self,
        dst: u32,
        table_id: u32,
        time: u32,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let table_id = self.lookup(table_id)?;
        let time = self.lookup(time)?;
        let value = call_table_host(
            self.fb,
            self.module,
            self.math,
            TableHostFn::NextEvent,
            &[table_id, time],
        )?;
        self.insert(dst, value)
    }

    fn lower_linear_solve_component(
        &mut self,
        dst: u32,
        matrix_start: u32,
        rhs_start: u32,
        n: usize,
        component: usize,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        if n == 0 || component >= n {
            let value = self.fb.ins().f64const(f64::NAN);
            return self.insert(dst, value);
        }

        let matrix_len = n.checked_mul(n).ok_or_else(|| {
            CompileError::Backend("linear solve matrix size overflow".to_string())
        })?;

        let mut matrix = checked_vec_with_capacity(matrix_len, "linear solve matrix values")?;
        for offset in 0..matrix_len {
            let reg = checked_reg_offset(matrix_start, offset, "matrix")?;
            matrix.push(self.lookup(reg)?);
        }
        let mut rhs = checked_vec_with_capacity(n, "linear solve rhs values")?;
        for offset in 0..n {
            let reg = checked_reg_offset(rhs_start, offset, "rhs")?;
            rhs.push(self.lookup(reg)?);
        }

        let solution = emit_dense_linear_solve(self.fb, &mut matrix, &mut rhs, n)?;
        self.insert(dst, solution[component])
    }

    fn insert(
        &mut self,
        dst: u32,
        value: cranelift_codegen::ir::Value,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        if let Some(regs_ptr) = self.backing_regs_ptr {
            let offset = register_byte_offset(dst)?;
            self.fb.ins().store(self.flags, value, regs_ptr, offset);
            return Ok(None);
        }
        self.regs.insert(dst, value);
        Ok(None)
    }

    fn lookup(&mut self, register: u32) -> Result<cranelift_codegen::ir::Value, CompileError> {
        if let Some(regs_ptr) = self.backing_regs_ptr {
            return Ok(self.fb.ins().load(
                types::F64,
                self.flags,
                regs_ptr,
                register_byte_offset(register)?,
            ));
        }
        if let Some(value) = self.regs.get(&register).copied() {
            return Ok(value);
        }
        Err(CompileError::Backend(format!(
            "register r{register} used before definition"
        )))
    }
}

#[derive(Default)]
struct MathImports {
    unary: HashMap<UnaryMathFn, FuncId>,
    binary: HashMap<BinaryMathFn, FuncId>,
    table: HashMap<TableHostFn, FuncId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum UnaryMathFn {
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Sinh,
    Cosh,
    Tanh,
    Exp,
    Log,
    Log10,
    Floor,
    Ceil,
    Trunc,
}

impl UnaryMathFn {
    fn symbol(self) -> &'static str {
        match self {
            Self::Sin => "rumoca_host_sin",
            Self::Cos => "rumoca_host_cos",
            Self::Tan => "rumoca_host_tan",
            Self::Asin => "rumoca_host_asin",
            Self::Acos => "rumoca_host_acos",
            Self::Atan => "rumoca_host_atan",
            Self::Sinh => "rumoca_host_sinh",
            Self::Cosh => "rumoca_host_cosh",
            Self::Tanh => "rumoca_host_tanh",
            Self::Exp => "rumoca_host_exp",
            Self::Log => "rumoca_host_log",
            Self::Log10 => "rumoca_host_log10",
            Self::Floor => "rumoca_host_floor",
            Self::Ceil => "rumoca_host_ceil",
            Self::Trunc => "rumoca_host_trunc",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum BinaryMathFn {
    Pow,
    Atan2,
}

impl BinaryMathFn {
    fn symbol(self) -> &'static str {
        match self {
            Self::Pow => "rumoca_host_powf",
            Self::Atan2 => "rumoca_host_atan2",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum TableHostFn {
    BoundsMin,
    BoundsMax,
    Lookup,
    LookupSlope,
    NextEvent,
}

impl TableHostFn {
    fn symbol(self) -> &'static str {
        match self {
            Self::BoundsMin => "rumoca_host_table_bounds_min",
            Self::BoundsMax => "rumoca_host_table_bounds_max",
            Self::Lookup => "rumoca_host_table_lookup",
            Self::LookupSlope => "rumoca_host_table_lookup_slope",
            Self::NextEvent => "rumoca_host_table_next_event",
        }
    }

    fn arity(self) -> usize {
        match self {
            Self::BoundsMin | Self::BoundsMax => 1,
            Self::Lookup | Self::LookupSlope => 3,
            Self::NextEvent => 2,
        }
    }
}

fn fold_unary_constant(op: UnaryOp, value: f64) -> Option<f64> {
    Some(match op {
        UnaryOp::Neg => -value,
        UnaryOp::Abs => value.abs(),
        UnaryOp::Sign => rumoca_core::modelica_sign(value),
        UnaryOp::Floor => value.floor(),
        UnaryOp::Ceil => value.ceil(),
        UnaryOp::Trunc => value.trunc(),
        _ => return None,
    })
}

fn fold_binary_constant(op: BinaryOp, lhs: f64, rhs: f64) -> Option<f64> {
    Some(match op {
        BinaryOp::Add => lhs + rhs,
        BinaryOp::Sub => lhs - rhs,
        BinaryOp::Mul => lhs * rhs,
        BinaryOp::Div => lhs / rhs,
        BinaryOp::Min => lhs.min(rhs),
        BinaryOp::Max => lhs.max(rhs),
        _ => return None,
    })
}

fn emit_unary_op(
    fb: &mut FunctionBuilder<'_>,
    module: &mut JITModule,
    math: &mut MathImports,
    op: UnaryOp,
    x: cranelift_codegen::ir::Value,
) -> Result<cranelift_codegen::ir::Value, CompileError> {
    let value = match op {
        UnaryOp::Neg => fb.ins().fneg(x),
        UnaryOp::Not => {
            let zero = fb.ins().f64const(0.0);
            let is_false = fb.ins().fcmp(FloatCC::Equal, x, zero);
            bool_to_f64(fb, is_false)
        }
        UnaryOp::Abs => fb.ins().fabs(x),
        UnaryOp::Sign => {
            let zero = fb.ins().f64const(0.0);
            let one = fb.ins().f64const(1.0);
            let neg_one = fb.ins().f64const(-1.0);
            let gt = fb.ins().fcmp(FloatCC::GreaterThan, x, zero);
            let lt = fb.ins().fcmp(FloatCC::LessThan, x, zero);
            let lt_value = fb.ins().select(lt, neg_one, zero);
            fb.ins().select(gt, one, lt_value)
        }
        UnaryOp::Sqrt => fb.ins().sqrt(x),
        UnaryOp::Floor => call_unary_math(fb, module, math, UnaryMathFn::Floor, x)?,
        UnaryOp::Ceil => call_unary_math(fb, module, math, UnaryMathFn::Ceil, x)?,
        UnaryOp::Trunc => call_unary_math(fb, module, math, UnaryMathFn::Trunc, x)?,
        UnaryOp::Sin => call_unary_math(fb, module, math, UnaryMathFn::Sin, x)?,
        UnaryOp::Cos => call_unary_math(fb, module, math, UnaryMathFn::Cos, x)?,
        UnaryOp::Tan => call_unary_math(fb, module, math, UnaryMathFn::Tan, x)?,
        UnaryOp::Asin => call_unary_math(fb, module, math, UnaryMathFn::Asin, x)?,
        UnaryOp::Acos => call_unary_math(fb, module, math, UnaryMathFn::Acos, x)?,
        UnaryOp::Atan => call_unary_math(fb, module, math, UnaryMathFn::Atan, x)?,
        UnaryOp::Sinh => call_unary_math(fb, module, math, UnaryMathFn::Sinh, x)?,
        UnaryOp::Cosh => call_unary_math(fb, module, math, UnaryMathFn::Cosh, x)?,
        UnaryOp::Tanh => call_unary_math(fb, module, math, UnaryMathFn::Tanh, x)?,
        UnaryOp::Exp => call_unary_math(fb, module, math, UnaryMathFn::Exp, x)?,
        UnaryOp::Log => call_unary_math(fb, module, math, UnaryMathFn::Log, x)?,
        UnaryOp::Log10 => call_unary_math(fb, module, math, UnaryMathFn::Log10, x)?,
    };
    Ok(value)
}

fn emit_tensor_binary_primal(
    fb: &mut FunctionBuilder<'_>,
    module: &mut JITModule,
    math: &mut MathImports,
    op: BinaryOp,
    lhs: cranelift_codegen::ir::Value,
    rhs: cranelift_codegen::ir::Value,
    lanes: usize,
) -> Result<cranelift_codegen::ir::Value, CompileError> {
    let raw = emit_binary_op(fb, module, math, op, lhs, rhs)?;
    if lanes != 2 || op != BinaryOp::Div {
        return Ok(raw);
    }
    let zero = fb.ins().f64const(0.0);
    let denominator_zero = fb.ins().fcmp(FloatCC::Equal, rhs, zero);
    let numerator_zero = fb.ins().fcmp(FloatCC::Equal, lhs, zero);
    let zero_over_zero = fb.ins().select(numerator_zero, zero, raw);
    Ok(fb.ins().select(denominator_zero, zero_over_zero, raw))
}

fn emit_tensor_binary_tangent(
    fb: &mut FunctionBuilder<'_>,
    op: BinaryOp,
    lhs_re: cranelift_codegen::ir::Value,
    lhs_du: cranelift_codegen::ir::Value,
    rhs_re: cranelift_codegen::ir::Value,
    rhs_du: cranelift_codegen::ir::Value,
) -> Result<cranelift_codegen::ir::Value, CompileError> {
    Ok(match op {
        BinaryOp::Add => fb.ins().fadd(lhs_du, rhs_du),
        BinaryOp::Sub => fb.ins().fsub(lhs_du, rhs_du),
        BinaryOp::Mul => {
            let lhs_term = fb.ins().fmul(lhs_du, rhs_re);
            let rhs_term = fb.ins().fmul(lhs_re, rhs_du);
            fb.ins().fadd(lhs_term, rhs_term)
        }
        BinaryOp::Div => {
            let lhs_term = fb.ins().fmul(lhs_du, rhs_re);
            let rhs_term = fb.ins().fmul(lhs_re, rhs_du);
            let numerator = fb.ins().fsub(lhs_term, rhs_term);
            let denominator = fb.ins().fmul(rhs_re, rhs_re);
            let quotient = fb.ins().fdiv(numerator, denominator);
            let zero = fb.ins().f64const(0.0);
            let denominator_zero = fb.ins().fcmp(FloatCC::Equal, rhs_re, zero);
            fb.ins().select(denominator_zero, zero, quotient)
        }
        _ => {
            return Err(CompileError::Backend(
                "invalid compact tensor binary operator".to_string(),
            ));
        }
    })
}

fn emit_binary_op(
    fb: &mut FunctionBuilder<'_>,
    module: &mut JITModule,
    math: &mut MathImports,
    op: BinaryOp,
    lhs: cranelift_codegen::ir::Value,
    rhs: cranelift_codegen::ir::Value,
) -> Result<cranelift_codegen::ir::Value, CompileError> {
    let value = match op {
        BinaryOp::Add => fb.ins().fadd(lhs, rhs),
        BinaryOp::Sub => fb.ins().fsub(lhs, rhs),
        BinaryOp::Mul => fb.ins().fmul(lhs, rhs),
        BinaryOp::Div => fb.ins().fdiv(lhs, rhs),
        BinaryOp::Pow => call_binary_math(fb, module, math, BinaryMathFn::Pow, lhs, rhs)?,
        BinaryOp::And => {
            let zero = fb.ins().f64const(0.0);
            let l = fb.ins().fcmp(FloatCC::NotEqual, lhs, zero);
            let r = fb.ins().fcmp(FloatCC::NotEqual, rhs, zero);
            let and_bits = fb.ins().band(l, r);
            bool_to_f64(fb, and_bits)
        }
        BinaryOp::Or => {
            let zero = fb.ins().f64const(0.0);
            let l = fb.ins().fcmp(FloatCC::NotEqual, lhs, zero);
            let r = fb.ins().fcmp(FloatCC::NotEqual, rhs, zero);
            let or_bits = fb.ins().bor(l, r);
            bool_to_f64(fb, or_bits)
        }
        BinaryOp::Atan2 => call_binary_math(fb, module, math, BinaryMathFn::Atan2, lhs, rhs)?,
        BinaryOp::Min => fb.ins().fmin(lhs, rhs),
        BinaryOp::Max => fb.ins().fmax(lhs, rhs),
    };
    Ok(value)
}

fn emit_compare_op(
    fb: &mut FunctionBuilder<'_>,
    op: CompareOp,
    lhs: cranelift_codegen::ir::Value,
    rhs: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    match op {
        CompareOp::Lt => fb.ins().fcmp(FloatCC::LessThan, lhs, rhs),
        CompareOp::Le => fb.ins().fcmp(FloatCC::LessThanOrEqual, lhs, rhs),
        CompareOp::Gt => fb.ins().fcmp(FloatCC::GreaterThan, lhs, rhs),
        CompareOp::Ge => fb.ins().fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs),
        CompareOp::Eq => fb.ins().fcmp(FloatCC::Equal, lhs, rhs),
        CompareOp::Ne => fb.ins().fcmp(FloatCC::NotEqual, lhs, rhs),
    }
}

fn emit_dense_linear_solve(
    fb: &mut FunctionBuilder<'_>,
    matrix: &mut [cranelift_codegen::ir::Value],
    rhs: &mut [cranelift_codegen::ir::Value],
    n: usize,
) -> Result<Vec<cranelift_codegen::ir::Value>, CompileError> {
    for col in 0..n {
        emit_partial_pivot(fb, matrix, rhs, n, col);
        let pivot = emit_nonzero_pivot(fb, matrix[dense_index(n, col, col)]);
        for row in col + 1..n {
            let factor = fb.ins().fdiv(matrix[dense_index(n, row, col)], pivot);
            matrix[dense_index(n, row, col)] = fb.ins().f64const(0.0);
            for entry in col + 1..n {
                let term = fb.ins().fmul(factor, matrix[dense_index(n, col, entry)]);
                matrix[dense_index(n, row, entry)] =
                    fb.ins().fsub(matrix[dense_index(n, row, entry)], term);
            }
            let rhs_term = fb.ins().fmul(factor, rhs[col]);
            rhs[row] = fb.ins().fsub(rhs[row], rhs_term);
        }
    }

    let zero = fb.ins().f64const(0.0);
    let mut solution = checked_vec_with_capacity(n, "linear solve solution values")?;
    solution.resize(n, zero);
    for row in (0..n).rev() {
        let mut tail = zero;
        for col in row + 1..n {
            let term = fb
                .ins()
                .fmul(matrix[dense_index(n, row, col)], solution[col]);
            tail = fb.ins().fadd(tail, term);
        }
        let numerator = fb.ins().fsub(rhs[row], tail);
        let pivot = emit_nonzero_pivot(fb, matrix[dense_index(n, row, row)]);
        solution[row] = fb.ins().fdiv(numerator, pivot);
    }
    Ok(solution)
}

fn emit_partial_pivot(
    fb: &mut FunctionBuilder<'_>,
    matrix: &mut [cranelift_codegen::ir::Value],
    rhs: &mut [cranelift_codegen::ir::Value],
    n: usize,
    col: usize,
) {
    for row in col + 1..n {
        let pivot_abs = fb.ins().fabs(matrix[dense_index(n, col, col)]);
        let candidate_abs = fb.ins().fabs(matrix[dense_index(n, row, col)]);
        let use_candidate = fb
            .ins()
            .fcmp(FloatCC::GreaterThan, candidate_abs, pivot_abs);
        for entry in 0..n {
            let pivot_index = dense_index(n, col, entry);
            let candidate_index = dense_index(n, row, entry);
            let pivot_value = matrix[pivot_index];
            let candidate_value = matrix[candidate_index];
            matrix[pivot_index] = fb.ins().select(use_candidate, candidate_value, pivot_value);
            matrix[candidate_index] = fb.ins().select(use_candidate, pivot_value, candidate_value);
        }
        let pivot_rhs = rhs[col];
        let candidate_rhs = rhs[row];
        rhs[col] = fb.ins().select(use_candidate, candidate_rhs, pivot_rhs);
        rhs[row] = fb.ins().select(use_candidate, pivot_rhs, candidate_rhs);
    }
}

fn emit_nonzero_pivot(
    fb: &mut FunctionBuilder<'_>,
    pivot: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    let abs = fb.ins().fabs(pivot);
    let eps = fb.ins().f64const(f64::EPSILON);
    let valid = fb.ins().fcmp(FloatCC::GreaterThan, abs, eps);
    let nan = fb.ins().f64const(f64::NAN);
    fb.ins().select(valid, pivot, nan)
}

fn dense_index(n: usize, row: usize, col: usize) -> usize {
    row * n + col
}

fn bool_to_f64(
    fb: &mut FunctionBuilder<'_>,
    value: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    let one = fb.ins().f64const(1.0);
    let zero = fb.ins().f64const(0.0);
    fb.ins().select(value, one, zero)
}

fn load_f64(
    fb: &mut FunctionBuilder<'_>,
    flags: MemFlags,
    base: cranelift_codegen::ir::Value,
    index: usize,
) -> Result<cranelift_codegen::ir::Value, CompileError> {
    let byte_offset = index
        .checked_mul(std::mem::size_of::<f64>())
        .ok_or_else(|| CompileError::Backend("load index overflow".to_string()))?;
    let offset = i32::try_from(byte_offset)
        .map_err(|_| CompileError::Backend("load offset exceeds i32".to_string()))?;
    Ok(fb.ins().load(types::F64, flags, base, offset))
}

fn load_fold_captures(
    builder: &mut FunctionBuilder<'_>,
    flags: MemFlags,
    pointer: cranelift_codegen::ir::Value,
    count: usize,
) -> Result<Vec<cranelift_codegen::ir::Value>, CompileError> {
    (0..count)
        .map(|index| load_f64(builder, flags, pointer, index))
        .collect()
}

fn register_byte_offset(register: u32) -> Result<i32, CompileError> {
    register
        .checked_mul(std::mem::size_of::<f64>() as u32)
        .and_then(|offset| i32::try_from(offset).ok())
        .ok_or_else(|| CompileError::Backend("register tape offset exceeds i32".to_string()))
}

fn call_unary_math(
    fb: &mut FunctionBuilder<'_>,
    module: &mut JITModule,
    math: &mut MathImports,
    function: UnaryMathFn,
    arg: cranelift_codegen::ir::Value,
) -> Result<cranelift_codegen::ir::Value, CompileError> {
    let func_id = if let Some(existing) = math.unary.get(&function).copied() {
        existing
    } else {
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(types::F64));
        sig.returns.push(AbiParam::new(types::F64));
        let func_id = module
            .declare_function(function.symbol(), Linkage::Import, &sig)
            .map_err(to_backend_err)?;
        math.unary.insert(function, func_id);
        func_id
    };
    let callee = module.declare_func_in_func(func_id, fb.func);
    let call = fb.ins().call(callee, &[arg]);
    let values = fb.inst_results(call);
    values
        .first()
        .copied()
        .ok_or_else(|| CompileError::Backend(format!("no return value for {}", function.symbol())))
}

fn call_binary_math(
    fb: &mut FunctionBuilder<'_>,
    module: &mut JITModule,
    math: &mut MathImports,
    function: BinaryMathFn,
    lhs: cranelift_codegen::ir::Value,
    rhs: cranelift_codegen::ir::Value,
) -> Result<cranelift_codegen::ir::Value, CompileError> {
    let func_id = if let Some(existing) = math.binary.get(&function).copied() {
        existing
    } else {
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(types::F64));
        sig.params.push(AbiParam::new(types::F64));
        sig.returns.push(AbiParam::new(types::F64));
        let func_id = module
            .declare_function(function.symbol(), Linkage::Import, &sig)
            .map_err(to_backend_err)?;
        math.binary.insert(function, func_id);
        func_id
    };
    let callee = module.declare_func_in_func(func_id, fb.func);
    let call = fb.ins().call(callee, &[lhs, rhs]);
    let values = fb.inst_results(call);
    values
        .first()
        .copied()
        .ok_or_else(|| CompileError::Backend(format!("no return value for {}", function.symbol())))
}

fn call_table_host(
    fb: &mut FunctionBuilder<'_>,
    module: &mut JITModule,
    math: &mut MathImports,
    function: TableHostFn,
    args: &[cranelift_codegen::ir::Value],
) -> Result<cranelift_codegen::ir::Value, CompileError> {
    let func_id = if let Some(existing) = math.table.get(&function).copied() {
        existing
    } else {
        let mut sig = module.make_signature();
        for _ in 0..function.arity() {
            sig.params.push(AbiParam::new(types::F64));
        }
        sig.returns.push(AbiParam::new(types::F64));
        let func_id = module
            .declare_function(function.symbol(), Linkage::Import, &sig)
            .map_err(to_backend_err)?;
        math.table.insert(function, func_id);
        func_id
    };
    let callee = module.declare_func_in_func(func_id, fb.func);
    let call = fb.ins().call(callee, args);
    let values = fb.inst_results(call);
    values
        .first()
        .copied()
        .ok_or_else(|| CompileError::Backend(format!("no return value for {}", function.symbol())))
}

fn checked_reg_offset(base: u32, offset: usize, kind: &str) -> Result<u32, CompileError> {
    let offset = u32::try_from(offset)
        .map_err(|_| CompileError::Backend(format!("linear solve {kind} register overflow")))?;
    base.checked_add(offset)
        .ok_or_else(|| CompileError::Backend(format!("linear solve {kind} register overflow")))
}

fn stack_element_byte_offset(element: usize, kind: &str) -> Result<i32, CompileError> {
    element
        .checked_mul(std::mem::size_of::<f64>())
        .and_then(|offset| i32::try_from(offset).ok())
        .ok_or_else(|| CompileError::Backend(format!("{kind} stack offset exceeds i32")))
}

fn checked_f64_stack_bytes(count: usize, message: &str) -> Result<u32, CompileError> {
    count
        .checked_mul(std::mem::size_of::<f64>())
        .and_then(|bytes| u32::try_from(bytes).ok())
        .ok_or_else(|| CompileError::Backend(message.to_string()))
}

fn fold_index_constants(
    domain: &rumoca_core::StructuredIndexDomain,
    extents: &[usize],
    strides: &[usize],
    ordinal: usize,
) -> Vec<f64> {
    domain
        .binders
        .iter()
        .zip(extents.iter().copied())
        .zip(strides.iter().copied())
        .map(|((binder, extent), stride)| {
            let position = (ordinal / stride) % extent;
            (position as i64 * binder.step + binder.lower) as f64
        })
        .collect()
}

fn conditional_capture_range(operation: &LinearOp) -> Option<(usize, usize)> {
    match operation {
        LinearOp::LoadFunctionConditionalCapture { index, .. } => Some((*index, 1)),
        LinearOp::LoadFunctionConditionalCaptureRange {
            index_start, count, ..
        } => Some((*index_start, *count)),
        _ => None,
    }
}

fn is_fold_output(operation: &LinearOp) -> bool {
    matches!(
        operation,
        LinearOp::StoreOutput { .. }
            | LinearOp::StoreOutputRange { .. }
            | LinearOp::StoreOutputFoldTensorUpdate { .. }
            | LinearOp::StoreOutputFunctionFold { .. }
    )
}

fn checked_fold_output_cursor(cursor: usize, count: usize) -> Result<usize, CompileError> {
    checked_usize_add(cursor, count, "function-fold output cursor overflow")
}

fn checked_usize_add(lhs: usize, rhs: usize, message: &str) -> Result<usize, CompileError> {
    lhs.checked_add(rhs)
        .ok_or_else(|| CompileError::Backend(message.to_string()))
}

fn tensor_output_count(dimensions: &[u32], lanes: usize) -> Result<usize, CompileError> {
    dimensions
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
        .and_then(|count| count.checked_mul(lanes))
        .ok_or_else(|| {
            CompileError::Backend("function-fold aggregate output count overflow".to_string())
        })
}

// SPEC_0021: exhaustive classification of the complete LinearOp row vocabulary.
#[expect(
    clippy::too_many_lines,
    clippy::excessive_nesting,
    reason = "row planning must classify every LinearOp variant in one dispatch"
)]
fn plan_row(row: &[LinearOp]) -> Result<RowPlan, CompileError> {
    let mut reg_count = 0usize;
    let input_requirements = input_requirements_for_linear_ops(row)?;
    for op in row {
        if let Some(index) = max_reg_index(op.clone())? {
            reg_count = reg_count.max(index.checked_add(1).ok_or_else(|| {
                CompileError::Backend("compiled row register count overflow".to_string())
            })?);
        }
    }
    let mut defined = checked_vec_with_capacity(reg_count, "defined register flags")?;
    defined.resize(reg_count, false);
    for op in row {
        validate_row_sources(&defined, op.clone())?;
        if let Some(dst) = dst_reg(op.clone()) {
            let count = match op {
                LinearOp::FunctionFold { program, .. }
                | LinearOp::GuardedFunctionFold { program, .. } => program.carried_count,
                LinearOp::FunctionConditional { program, .. } => program.result_count,
                LinearOp::PureCall { site, .. } => site.output_scalar_count().ok_or_else(|| {
                    CompileError::Backend("typed pure-call output width overflows".to_string())
                })?,
                LinearOp::PureCallDirectional { site, .. } => {
                    site.output_scalar_count().ok_or_else(|| {
                        CompileError::Backend(
                            "typed directional output width overflows".to_string(),
                        )
                    })?
                }
                LinearOp::MatrixMultiply {
                    rows,
                    columns,
                    lanes,
                    ..
                } => rows.saturating_mul(*columns).saturating_mul(*lanes),
                LinearOp::TensorBinary { count, lanes, .. } => count.saturating_mul(*lanes),
                LinearOp::TensorCross { lanes, .. } => 3usize.saturating_mul(*lanes),
                LinearOp::TensorTranspose {
                    rows,
                    columns,
                    element_width,
                    lanes,
                    ..
                } => rows
                    .saturating_mul(*columns)
                    .saturating_mul(*element_width)
                    .saturating_mul(*lanes),
                LinearOp::TensorConcatenate {
                    dimensions, lanes, ..
                } => dimensions.iter().fold(*lanes, |count, extent| {
                    count.saturating_mul(*extent as usize)
                }),
                LinearOp::TensorUpdate {
                    dimensions, lanes, ..
                } => dimensions.iter().fold(*lanes, |count, extent| {
                    count.saturating_mul(*extent as usize)
                }),
                LinearOp::TensorFill { count, lanes, .. } => count.saturating_mul(*lanes),
                LinearOp::TensorIdentity { size, lanes, .. } => {
                    size.saturating_mul(*size).saturating_mul(*lanes)
                }
                LinearOp::TensorLoad { count, lanes, .. } => count.saturating_mul(*lanes),
                _ => 1,
            };
            defined[dst..dst + count].fill(true);
        }
    }
    // Collect every checked output projection in order and keep compute ops as
    // the body. Compact ranges remain one Solve op until this final native
    // row-plan boundary, where target output slots become explicit.
    let mut output_srcs = Vec::new();
    let mut body: Vec<LinearOp> = Vec::with_capacity(row.len());
    for op in row {
        match op {
            LinearOp::StoreOutput { src } => output_srcs.push(*src as usize),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                if *count == 0 {
                    return Err(CompileError::Backend(
                        "compiled row has an empty StoreOutputRange".to_string(),
                    ));
                }
                for ordinal in 0..*count {
                    output_srcs.push(checked_strided_register(
                        *start,
                        ordinal,
                        *stride,
                        "output range",
                    )? as usize);
                }
            }
            _ => body.push(op.clone()),
        }
    }
    if output_srcs.is_empty() {
        return Err(CompileError::Backend(
            "compiled row is missing an output projection".to_string(),
        ));
    }
    let output_srcs = output_srcs.into_boxed_slice();

    if body.iter().cloned().all(is_simple_linear_op) {
        let mut ops = checked_vec_with_capacity(body.len(), "simple runtime ops")?;
        for op in body.iter().cloned() {
            ops.push(lower_simple_op(op)?);
        }
        return Ok(RowPlan::Simple(SimpleRowPlan {
            ops: ops.into_boxed_slice(),
            reg_count,
            output_srcs,
            input_requirements,
        }));
    }

    Ok(RowPlan::General(GeneralRowPlan {
        ops: body.into_boxed_slice(),
        reg_count,
        output_srcs,
        input_requirements,
    }))
}

fn chunkable_op(op: LinearOp) -> bool {
    matches!(
        op,
        LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadY { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::Move { .. }
            | LinearOp::DotProduct { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
            | LinearOp::StoreOutput { .. }
    )
}

fn is_simple_linear_op(op: LinearOp) -> bool {
    !matches!(
        op,
        LinearOp::LoadSeed { .. }
            | LinearOp::LoadIndexedP { .. }
            | LinearOp::LoadIndexedRegister { .. }
            | LinearOp::LoadIndexedFoldCarried { .. }
            | LinearOp::LoadIndexedFoldCapture { .. }
            | LinearOp::LoadIndexedSeed { .. }
            | LinearOp::LoadFoldCarried { .. }
            | LinearOp::LoadFoldIndex { .. }
            | LinearOp::LoadFoldCapture { .. }
            | LinearOp::LoadFunctionConditionalCapture { .. }
            | LinearOp::LoadFunctionConditionalCaptureRange { .. }
            | LinearOp::FunctionFold { .. }
            | LinearOp::GuardedFunctionFold { .. }
            | LinearOp::FunctionConditional { .. }
            | LinearOp::PureCall { .. }
            | LinearOp::PureCallDirectional { .. }
            | LinearOp::Move { .. }
            | LinearOp::LinearSolveComponent { .. }
            | LinearOp::DotProduct { .. }
            | LinearOp::MatrixMultiply { .. }
            | LinearOp::TensorBinary { .. }
            | LinearOp::TensorCross { .. }
            | LinearOp::TensorTranspose { .. }
            | LinearOp::TensorConcatenate { .. }
            | LinearOp::TensorUpdate { .. }
            | LinearOp::TensorFill { .. }
            | LinearOp::TensorIdentity { .. }
            | LinearOp::TensorLoad { .. }
            | LinearOp::TableBounds { .. }
            | LinearOp::TableLookup { .. }
            | LinearOp::TableLookupSlope { .. }
            | LinearOp::TableNextEvent { .. }
            | LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. }
            | LinearOp::StoreOutputFoldTensorUpdate { .. }
            | LinearOp::StoreOutputFunctionFold { .. }
            | LinearOp::StoreOutputRange { .. }
            | LinearOp::StoreOutput { .. }
    )
}

fn lower_simple_op(op: LinearOp) -> Result<SimpleOp, CompileError> {
    match op {
        LinearOp::Const { dst, value } => Ok(SimpleOp::Const { dst, value }),
        LinearOp::LoadTime { dst } => Ok(SimpleOp::LoadTime { dst }),
        LinearOp::LoadY { dst, index } => Ok(SimpleOp::LoadY {
            dst,
            index: lower_runtime_index(index, "LoadY")?,
        }),
        LinearOp::LoadP { dst, index } => Ok(SimpleOp::LoadP {
            dst,
            index: lower_runtime_index(index, "LoadP")?,
        }),
        LinearOp::Unary { dst, op, arg } => Ok(SimpleOp::Unary { dst, op, arg }),
        LinearOp::Binary { dst, op, lhs, rhs } => Ok(SimpleOp::Binary { dst, op, lhs, rhs }),
        LinearOp::Compare { dst, op, lhs, rhs } => Ok(SimpleOp::Compare { dst, op, lhs, rhs }),
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => Ok(SimpleOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        }),
        LinearOp::LoadSeed { .. }
        | LinearOp::LoadIndexedP { .. }
        | LinearOp::LoadIndexedRegister { .. }
        | LinearOp::LoadIndexedFoldCarried { .. }
        | LinearOp::LoadIndexedFoldCapture { .. }
        | LinearOp::LoadIndexedSeed { .. }
        | LinearOp::LoadFoldCarried { .. }
        | LinearOp::LoadFoldIndex { .. }
        | LinearOp::LoadFoldCapture { .. }
        | LinearOp::LoadFunctionConditionalCapture { .. }
        | LinearOp::LoadFunctionConditionalCaptureRange { .. }
        | LinearOp::FunctionFold { .. }
        | LinearOp::GuardedFunctionFold { .. }
        | LinearOp::FunctionConditional { .. }
        | LinearOp::PureCall { .. }
        | LinearOp::PureCallDirectional { .. }
        | LinearOp::Move { .. }
        | LinearOp::LinearSolveComponent { .. }
        | LinearOp::DotProduct { .. }
        | LinearOp::MatrixMultiply { .. }
        | LinearOp::TensorBinary { .. }
        | LinearOp::TensorCross { .. }
        | LinearOp::TensorTranspose { .. }
        | LinearOp::TensorConcatenate { .. }
        | LinearOp::TensorUpdate { .. }
        | LinearOp::TensorFill { .. }
        | LinearOp::TensorIdentity { .. }
        | LinearOp::TensorLoad { .. }
        | LinearOp::TableBounds { .. }
        | LinearOp::TableLookup { .. }
        | LinearOp::TableLookupSlope { .. }
        | LinearOp::TableNextEvent { .. }
        | LinearOp::RandomInitialState { .. }
        | LinearOp::RandomResult { .. }
        | LinearOp::RandomState { .. }
        | LinearOp::ImpureRandomInit { .. }
        | LinearOp::ImpureRandom { .. }
        | LinearOp::ImpureRandomInteger { .. }
        | LinearOp::StoreOutputFoldTensorUpdate { .. }
        | LinearOp::StoreOutputFunctionFold { .. }
        | LinearOp::StoreOutputRange { .. }
        | LinearOp::StoreOutput { .. } => Err(CompileError::Backend(
            "attempted to lower non-simple runtime op onto the simple row path".to_string(),
        )),
    }
}

fn lower_runtime_index(index: usize, kind: &str) -> Result<u32, CompileError> {
    u32::try_from(index)
        .map_err(|_| CompileError::Backend(format!("{kind} index exceeds u32 runtime plan")))
}

fn checked_strided_register(
    start: u32,
    term: usize,
    stride: usize,
    kind: &str,
) -> Result<u32, CompileError> {
    let offset = term
        .checked_mul(stride)
        .and_then(|offset| u32::try_from(offset).ok())
        .ok_or_else(|| CompileError::Backend(format!("{kind} register offset overflows u32")))?;
    start
        .checked_add(offset)
        .ok_or_else(|| CompileError::Backend(format!("{kind} register index overflows u32")))
}

// SPEC_0021: exhaustive register-footprint dispatch over every LinearOp variant.
#[expect(
    clippy::too_many_lines,
    clippy::excessive_nesting,
    reason = "one exhaustive dispatch proves the register footprint of every LinearOp"
)]
fn max_reg_index(op: LinearOp) -> Result<Option<usize>, CompileError> {
    match op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::LoadSeed { dst, .. }
        | LinearOp::LoadFoldCarried { dst, .. }
        | LinearOp::LoadFoldIndex { dst, .. }
        | LinearOp::LoadFoldCapture { dst, .. }
        | LinearOp::LoadFunctionConditionalCapture { dst, .. }
        | LinearOp::TableBounds { dst, .. }
        | LinearOp::TableLookup { dst, .. }
        | LinearOp::TableLookupSlope { dst, .. }
        | LinearOp::TableNextEvent { dst, .. } => Ok(Some(dst as usize)),
        LinearOp::LoadFunctionConditionalCaptureRange {
            dst_start, count, ..
        } => Ok(Some(checked_range_last_reg(
            dst_start,
            count,
            "function conditional capture range destination",
        )? as usize)),
        LinearOp::LoadIndexedP { dst, index, .. }
        | LinearOp::LoadIndexedSeed { dst, index, .. } => Ok(Some(dst.max(index) as usize)),
        LinearOp::LoadIndexedRegister {
            dst,
            base,
            stride,
            dimensions,
            indices,
        } => {
            let count = dimensions
                .iter()
                .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
                .ok_or_else(|| {
                    CompileError::Backend("runtime tensor projection extent overflow".to_string())
                })?;
            let mut last = dst.max(checked_strided_register(
                base,
                count.saturating_sub(1),
                stride,
                "runtime tensor projection source",
            )?);
            for index in indices {
                if let rumoca_ir_solve::TensorIndex::Runtime(register) = index {
                    last = last.max(register);
                }
            }
            Ok(Some(last as usize))
        }
        LinearOp::LoadIndexedFoldCarried { dst, indices, .. }
        | LinearOp::LoadIndexedFoldCapture { dst, indices, .. } => {
            let mut last = dst;
            for index in indices {
                if let rumoca_ir_solve::TensorIndex::Runtime(register) = index {
                    last = last.max(register);
                }
            }
            Ok(Some(last as usize))
        }
        LinearOp::RandomInitialState {
            dst,
            local_seed,
            global_seed,
            ..
        } => Ok(Some(dst.max(local_seed).max(global_seed) as usize)),
        LinearOp::RandomResult {
            dst,
            state_start,
            state_len,
            ..
        }
        | LinearOp::RandomState {
            dst,
            state_start,
            state_len,
            ..
        } => Ok(Some(dst.max(checked_range_last_reg(
            state_start,
            state_len,
            "random state",
        )?) as usize)),
        LinearOp::ImpureRandomInit { dst, seed } => Ok(Some(dst.max(seed) as usize)),
        LinearOp::ImpureRandom { dst, id, .. } => Ok(Some(dst.max(id) as usize)),
        LinearOp::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            ..
        } => Ok(Some(dst.max(id).max(imin).max(imax) as usize)),
        LinearOp::Move { dst, src } => Ok(Some(dst.max(src) as usize)),
        LinearOp::LinearSolveComponent {
            dst,
            matrix_start,
            rhs_start,
            n,
            ..
        } => Ok(Some(
            dst.max(checked_range_last_reg(
                matrix_start,
                checked_square_len(n, "linear solve matrix")?,
                "linear solve matrix",
            )?)
            .max(checked_range_last_reg(rhs_start, n, "linear solve rhs")?) as usize,
        )),
        LinearOp::DotProduct {
            dst,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
        } => {
            let last = count.saturating_sub(1);
            let lhs = checked_strided_register(lhs_start, last, lhs_stride, "dot lhs")?;
            let rhs = checked_strided_register(rhs_start, last, rhs_stride, "dot rhs")?;
            Ok(Some(dst.max(lhs).max(rhs) as usize))
        }
        LinearOp::MatrixMultiply {
            dst_start,
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes,
        } => {
            let output = rows
                .checked_mul(columns)
                .and_then(|count| count.checked_mul(lanes))
                .ok_or_else(|| {
                    CompileError::Backend("matrix multiply output range overflow".to_string())
                })?;
            let lhs = rows
                .checked_mul(inner)
                .and_then(|count| count.checked_mul(lanes))
                .ok_or_else(|| {
                    CompileError::Backend("matrix multiply lhs range overflow".to_string())
                })?;
            let rhs = inner
                .checked_mul(columns)
                .and_then(|count| count.checked_mul(lanes))
                .ok_or_else(|| {
                    CompileError::Backend("matrix multiply rhs range overflow".to_string())
                })?;
            Ok(Some(
                checked_range_last_reg(dst_start, output, "matrix multiply output")?
                    .max(checked_range_last_reg(
                        lhs_start,
                        lhs,
                        "matrix multiply lhs",
                    )?)
                    .max(checked_range_last_reg(
                        rhs_start,
                        rhs,
                        "matrix multiply rhs",
                    )?) as usize,
            ))
        }
        LinearOp::TensorBinary {
            dst_start,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes,
            ..
        } => {
            let output_count = count.checked_mul(lanes).ok_or_else(|| {
                CompileError::Backend("tensor binary output range overflow".to_string())
            })?;
            let source_last = |start, stride, kind| {
                let element_offset = count
                    .saturating_sub(1)
                    .checked_mul(stride)
                    .and_then(|offset| offset.checked_mul(lanes))
                    .and_then(|offset| offset.checked_add(lanes.saturating_sub(1)))
                    .ok_or_else(|| CompileError::Backend(format!("{kind} range overflow")))?;
                checked_reg_offset(start, element_offset, kind)
            };
            Ok(Some(
                checked_range_last_reg(dst_start, output_count, "tensor binary output")?
                    .max(source_last(lhs_start, lhs_stride, "tensor binary lhs")?)
                    .max(source_last(rhs_start, rhs_stride, "tensor binary rhs")?)
                    as usize,
            ))
        }
        LinearOp::TensorCross {
            dst_start,
            lhs_start,
            rhs_start,
            lanes,
        } => {
            let count = 3usize.checked_mul(lanes).ok_or_else(|| {
                CompileError::Backend("tensor cross product range overflow".to_string())
            })?;
            Ok(Some(
                checked_range_last_reg(dst_start, count, "tensor cross product output")?
                    .max(checked_range_last_reg(
                        lhs_start,
                        count,
                        "tensor cross product lhs",
                    )?)
                    .max(checked_range_last_reg(
                        rhs_start,
                        count,
                        "tensor cross product rhs",
                    )?) as usize,
            ))
        }
        LinearOp::TensorTranspose {
            dst_start,
            src_start,
            rows,
            columns,
            element_width,
            lanes,
        } => {
            let count = rows
                .checked_mul(columns)
                .and_then(|count| count.checked_mul(element_width))
                .and_then(|count| count.checked_mul(lanes))
                .ok_or_else(|| {
                    CompileError::Backend("tensor transpose range overflow".to_string())
                })?;
            Ok(Some(
                checked_range_last_reg(dst_start, count, "tensor transpose output")?.max(
                    checked_range_last_reg(src_start, count, "tensor transpose source")?,
                ) as usize,
            ))
        }
        LinearOp::TensorConcatenate {
            dst_start,
            sources,
            dimensions,
            lanes,
            ..
        } => {
            let output_count = dimensions
                .iter()
                .try_fold(lanes, |count, extent| count.checked_mul(*extent as usize))
                .ok_or_else(|| {
                    CompileError::Backend("tensor concatenate output overflow".to_string())
                })?;
            let mut last =
                checked_range_last_reg(dst_start, output_count, "tensor concatenate output")?;
            for source in sources {
                let count = source
                    .dimensions
                    .iter()
                    .try_fold(lanes, |count, extent| count.checked_mul(*extent as usize))
                    .ok_or_else(|| {
                        CompileError::Backend("tensor concatenate source overflow".to_string())
                    })?;
                last = last.max(checked_range_last_reg(
                    source.start,
                    count,
                    "tensor concatenate source",
                )?);
            }
            Ok(Some(last as usize))
        }
        LinearOp::TensorUpdate {
            dst_start,
            base_start,
            value_start,
            dimensions,
            subscripts,
            lanes,
        } => {
            let output_count = dimensions
                .iter()
                .try_fold(lanes, |count, extent| count.checked_mul(*extent as usize))
                .ok_or_else(|| CompileError::Backend("tensor update output overflow".into()))?;
            let mut last =
                checked_range_last_reg(dst_start, output_count, "tensor update output")?.max(
                    checked_range_last_reg(base_start, output_count, "tensor update base")?,
                );
            let mut value_count = lanes;
            for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
                match subscript {
                    rumoca_ir_solve::TensorUpdateSubscript::Whole => {
                        value_count =
                            value_count.checked_mul(extent as usize).ok_or_else(|| {
                                CompileError::Backend("tensor update value overflow".into())
                            })?;
                    }
                    rumoca_ir_solve::TensorUpdateSubscript::Index(
                        rumoca_ir_solve::TensorIndex::Runtime(register),
                    ) => last = last.max(*register),
                    rumoca_ir_solve::TensorUpdateSubscript::Index(
                        rumoca_ir_solve::TensorIndex::Constant(_),
                    ) => {}
                    rumoca_ir_solve::TensorUpdateSubscript::Slice { start, dimensions } => {
                        let count = dimensions
                            .iter()
                            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
                            .ok_or_else(|| {
                                CompileError::Backend("tensor update slice overflow".into())
                            })?;
                        last = last.max(checked_range_last_reg(
                            *start,
                            count,
                            "tensor update slice",
                        )?);
                        value_count = value_count.checked_mul(count).ok_or_else(|| {
                            CompileError::Backend("tensor update sliced value overflow".into())
                        })?;
                    }
                }
            }
            last = last.max(checked_range_last_reg(
                value_start,
                value_count,
                "tensor update value",
            )?);
            Ok(Some(last as usize))
        }
        LinearOp::TensorFill {
            dst_start,
            value_start,
            count,
            lanes,
        } => Ok(Some(
            checked_range_last_reg(
                dst_start,
                count
                    .checked_mul(lanes)
                    .ok_or_else(|| CompileError::Backend("tensor fill overflow".into()))?,
                "tensor fill output",
            )?
            .max(checked_range_last_reg(
                value_start,
                lanes,
                "tensor fill value",
            )?) as usize,
        )),
        LinearOp::TensorIdentity {
            dst_start,
            size,
            lanes,
        } => Ok(Some(checked_range_last_reg(
            dst_start,
            size.checked_mul(size)
                .and_then(|count| count.checked_mul(lanes))
                .ok_or_else(|| CompileError::Backend("tensor identity overflow".into()))?,
            "tensor identity output",
        )? as usize)),
        LinearOp::TensorLoad {
            dst_start,
            count,
            lanes,
            ..
        } => Ok(Some(checked_range_last_reg(
            dst_start,
            count
                .checked_mul(lanes)
                .ok_or_else(|| CompileError::Backend("tensor load overflow".into()))?,
            "tensor load output",
        )? as usize)),
        LinearOp::Unary { dst, arg, .. } => Ok(Some((dst.max(arg)) as usize)),
        LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } => {
            Ok(Some(dst.max(lhs).max(rhs) as usize))
        }
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => Ok(Some(dst.max(cond).max(if_true).max(if_false) as usize)),
        LinearOp::FunctionFold {
            dst_start,
            initial_start,
            capture_start,
            program,
        } => {
            let output =
                checked_range_last_reg(dst_start, program.carried_count, "function fold output")?;
            let initial = checked_range_last_reg(
                initial_start,
                program.carried_count,
                "function fold initial",
            )?;
            let mut last = output.max(initial);
            if program.capture_count != 0 {
                last = last.max(checked_range_last_reg(
                    capture_start,
                    program.capture_count,
                    "function fold capture",
                )?);
            }
            Ok(Some(last as usize))
        }
        LinearOp::GuardedFunctionFold {
            dst_start,
            initial_start,
            capture_start,
            activation,
            program,
        } => {
            let output = checked_range_last_reg(
                dst_start,
                program.carried_count,
                "guarded function fold output",
            )?;
            let initial = checked_range_last_reg(
                initial_start,
                program.carried_count,
                "guarded function fold initial",
            )?;
            let mut last = output.max(initial).max(activation);
            if program.capture_count != 0 {
                last = last.max(checked_range_last_reg(
                    capture_start,
                    program.capture_count,
                    "guarded function fold capture",
                )?);
            }
            Ok(Some(last as usize))
        }
        LinearOp::FunctionConditional {
            dst_start,
            capture_start,
            program,
        } => {
            let mut last = checked_range_last_reg(
                dst_start,
                program.result_count,
                "function conditional output",
            )?;
            if program.capture_count != 0 {
                last = last.max(checked_range_last_reg(
                    capture_start,
                    program.capture_count,
                    "function conditional capture",
                )?);
            }
            Ok(Some(last as usize))
        }
        LinearOp::StoreOutputFoldTensorUpdate {
            dimensions,
            updates,
            nodes,
            lanes,
            ..
        } => {
            let mut last = 0;
            for update in updates {
                let value_count = dimensions
                    .iter()
                    .zip(update.subscripts.iter())
                    .try_fold(1usize, |count, (&extent, subscript)| {
                        if matches!(subscript, rumoca_ir_solve::TensorSubscript::Whole) {
                            count.checked_mul(extent as usize)
                        } else {
                            Some(count)
                        }
                    })
                    .ok_or_else(|| {
                        CompileError::Backend("tensor update value extent overflow".to_string())
                    })?;
                let value_last = checked_strided_register(
                    update.value_start,
                    value_count.saturating_sub(1),
                    update.value_stride,
                    "tensor update value",
                )?
                .checked_add(u32::try_from(lanes.saturating_sub(1)).map_err(|_| {
                    CompileError::Backend("tensor update lane exceeds u32".to_string())
                })?)
                .ok_or_else(|| {
                    CompileError::Backend("tensor update value register overflow".to_string())
                })?;
                last = last.max(value_last);
                for subscript in update.subscripts {
                    if let rumoca_ir_solve::TensorSubscript::Index(
                        rumoca_ir_solve::TensorIndex::Runtime(register),
                    ) = subscript
                    {
                        last = last.max(register);
                    }
                }
                if let Some(condition) = update.condition {
                    last = last.max(condition);
                }
            }
            for node in nodes {
                if let rumoca_ir_solve::FoldTensorNode::Select { condition, .. } = node {
                    last = last.max(condition);
                }
            }
            Ok(Some(last as usize))
        }
        LinearOp::StoreOutputFunctionFold {
            initial,
            capture_start,
            program,
            condition,
            ..
        } => {
            let mut last = 0;
            for source in initial {
                if let rumoca_ir_solve::FoldInitialSource::Registers { start, count } = source
                    && count != 0
                {
                    last = last.max(checked_range_last_reg(
                        start,
                        count,
                        "nested function fold initial",
                    )?);
                }
            }
            if program.capture_count != 0 {
                last = last.max(checked_range_last_reg(
                    capture_start,
                    program.capture_count,
                    "nested function fold capture",
                )?);
            }
            if let Some(condition) = condition {
                last = last.max(condition);
            }
            Ok(Some(last as usize))
        }
        LinearOp::StoreOutputRange {
            start,
            count,
            stride,
        } => Ok(Some(checked_strided_register(
            start,
            count.saturating_sub(1),
            stride,
            "output range",
        )? as usize)),
        LinearOp::PureCall {
            dst_start,
            input_starts,
            site,
        } => {
            let output_count = site.output_scalar_count().ok_or_else(|| {
                CompileError::Backend("typed pure-call output width overflows".to_string())
            })?;
            let mut last = checked_range_last_reg(dst_start, output_count, "pure call output")?;
            for (start, value_type) in input_starts.iter().zip(site.inputs()) {
                last = last.max(checked_range_last_reg(
                    *start,
                    value_type.scalar_count() as usize,
                    "pure call input",
                )?);
            }
            Ok(Some(last as usize))
        }
        LinearOp::PureCallDirectional {
            dst_start,
            input_starts,
            site,
        } => {
            let output_count = site.output_scalar_count().ok_or_else(|| {
                CompileError::Backend(
                    "typed directional pure-call output width overflows".to_string(),
                )
            })?;
            let mut last =
                checked_range_last_reg(dst_start, output_count, "directional pure call output")?;
            for (start, value_type) in input_starts.iter().zip(site.inputs()) {
                last = last.max(checked_range_last_reg(
                    *start,
                    value_type.scalar_count() as usize,
                    "directional pure call input",
                )?);
            }
            Ok(Some(last as usize))
        }
        LinearOp::StoreOutput { src } => Ok(Some(src as usize)),
    }
}

fn checked_square_len(n: usize, kind: &str) -> Result<usize, CompileError> {
    n.checked_mul(n)
        .ok_or_else(|| CompileError::Backend(format!("{kind} size overflow")))
}

fn checked_range_last_reg(base: u32, count: usize, kind: &str) -> Result<u32, CompileError> {
    let Some(offset) = count.checked_sub(1) else {
        return Ok(base);
    };
    checked_reg_offset(base, offset, kind)
}

fn dst_reg(op: LinearOp) -> Option<usize> {
    match op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::LoadSeed { dst, .. }
        | LinearOp::LoadFoldCarried { dst, .. }
        | LinearOp::LoadFoldIndex { dst, .. }
        | LinearOp::LoadFoldCapture { dst, .. }
        | LinearOp::LoadFunctionConditionalCapture { dst, .. }
        | LinearOp::TableBounds { dst, .. }
        | LinearOp::TableLookup { dst, .. }
        | LinearOp::TableLookupSlope { dst, .. }
        | LinearOp::TableNextEvent { dst, .. }
        | LinearOp::RandomInitialState { dst, .. }
        | LinearOp::RandomResult { dst, .. }
        | LinearOp::RandomState { dst, .. }
        | LinearOp::ImpureRandomInit { dst, .. }
        | LinearOp::ImpureRandom { dst, .. }
        | LinearOp::ImpureRandomInteger { dst, .. }
        | LinearOp::Move { dst, .. }
        | LinearOp::LinearSolveComponent { dst, .. }
        | LinearOp::DotProduct { dst, .. }
        | LinearOp::Unary { dst, .. }
        | LinearOp::Binary { dst, .. }
        | LinearOp::Compare { dst, .. }
        | LinearOp::LoadIndexedP { dst, .. }
        | LinearOp::LoadIndexedRegister { dst, .. }
        | LinearOp::LoadIndexedFoldCarried { dst, .. }
        | LinearOp::LoadIndexedFoldCapture { dst, .. }
        | LinearOp::LoadIndexedSeed { dst, .. }
        | LinearOp::Select { dst, .. } => Some(dst as usize),
        LinearOp::LoadFunctionConditionalCaptureRange { dst_start, .. } => Some(dst_start as usize),
        LinearOp::FunctionFold { dst_start, .. }
        | LinearOp::GuardedFunctionFold { dst_start, .. }
        | LinearOp::FunctionConditional { dst_start, .. }
        | LinearOp::PureCall { dst_start, .. }
        | LinearOp::PureCallDirectional { dst_start, .. }
        | LinearOp::MatrixMultiply { dst_start, .. }
        | LinearOp::TensorBinary { dst_start, .. }
        | LinearOp::TensorCross { dst_start, .. } => Some(dst_start as usize),
        LinearOp::TensorTranspose { dst_start, .. } => Some(dst_start as usize),
        LinearOp::TensorConcatenate { dst_start, .. }
        | LinearOp::TensorUpdate { dst_start, .. }
        | LinearOp::TensorFill { dst_start, .. }
        | LinearOp::TensorIdentity { dst_start, .. }
        | LinearOp::TensorLoad { dst_start, .. } => Some(dst_start as usize),
        LinearOp::StoreOutputFoldTensorUpdate { .. }
        | LinearOp::StoreOutputFunctionFold { .. }
        | LinearOp::StoreOutputRange { .. }
        | LinearOp::StoreOutput { .. } => None,
    }
}

// SPEC_0021: exhaustive source-validation dispatch over every LinearOp variant.
#[expect(
    clippy::too_many_lines,
    clippy::excessive_nesting,
    reason = "one exhaustive dispatch validates every LinearOp source contract"
)]
fn validate_row_sources(defined: &[bool], op: LinearOp) -> Result<(), CompileError> {
    match op {
        LinearOp::PureCall {
            input_starts, site, ..
        } => {
            if input_starts.len() != site.inputs().len() {
                return Err(CompileError::Backend(
                    "typed pure-call input ABI mismatch".to_string(),
                ));
            }
            for (start, value_type) in input_starts.iter().zip(site.inputs()) {
                validate_reg_range_defined(defined, *start, value_type.scalar_count() as usize)?;
            }
            Ok(())
        }
        LinearOp::PureCallDirectional {
            input_starts, site, ..
        } => {
            if input_starts.len() != site.inputs().len() {
                return Err(CompileError::Backend(
                    "typed directional pure-call input ABI mismatch".to_string(),
                ));
            }
            for (start, value_type) in input_starts.iter().zip(site.inputs()) {
                validate_reg_range_defined(defined, *start, value_type.scalar_count() as usize)?;
            }
            Ok(())
        }
        LinearOp::LoadIndexedP { index, .. } | LinearOp::LoadIndexedSeed { index, .. } => {
            validate_reg_defined(defined, index)
        }
        LinearOp::LoadIndexedRegister {
            base,
            stride,
            dimensions,
            indices,
            ..
        } => {
            let count = dimensions
                .iter()
                .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
                .ok_or_else(|| {
                    CompileError::Backend("runtime tensor projection extent overflow".to_string())
                })?;
            for offset in 0..count {
                validate_reg_defined(
                    defined,
                    checked_strided_register(
                        base,
                        offset,
                        stride,
                        "runtime tensor projection source",
                    )?,
                )?;
            }
            for index in indices {
                if let rumoca_ir_solve::TensorIndex::Runtime(register) = index {
                    validate_reg_defined(defined, register)?;
                }
            }
            Ok(())
        }
        LinearOp::LoadIndexedFoldCarried { indices, .. }
        | LinearOp::LoadIndexedFoldCapture { indices, .. } => {
            for index in indices {
                if let rumoca_ir_solve::TensorIndex::Runtime(register) = index {
                    validate_reg_defined(defined, register)?;
                }
            }
            Ok(())
        }
        LinearOp::StoreOutputFoldTensorUpdate {
            dimensions,
            updates,
            nodes,
            lanes,
            ..
        } => {
            for update in updates {
                if let Some(condition) = update.condition {
                    validate_reg_defined(defined, condition)?;
                }
                let value_count = dimensions
                    .iter()
                    .zip(update.subscripts.iter())
                    .try_fold(1usize, |count, (&extent, subscript)| {
                        if matches!(subscript, rumoca_ir_solve::TensorSubscript::Whole) {
                            count.checked_mul(extent as usize)
                        } else {
                            Some(count)
                        }
                    })
                    .ok_or_else(|| {
                        CompileError::Backend("tensor update value extent overflow".to_string())
                    })?;
                for element in 0..value_count {
                    let base = checked_strided_register(
                        update.value_start,
                        element,
                        update.value_stride,
                        "tensor update value",
                    )?;
                    for lane in 0..lanes {
                        validate_reg_defined(
                            defined,
                            base.checked_add(u32::try_from(lane).map_err(|_| {
                                CompileError::Backend("tensor update lane exceeds u32".to_string())
                            })?)
                            .ok_or_else(|| {
                                CompileError::Backend(
                                    "tensor update value register overflow".to_string(),
                                )
                            })?,
                        )?;
                    }
                }
                for subscript in update.subscripts {
                    if let rumoca_ir_solve::TensorSubscript::Index(
                        rumoca_ir_solve::TensorIndex::Runtime(register),
                    ) = subscript
                    {
                        validate_reg_defined(defined, register)?;
                    }
                }
            }
            for node in nodes {
                if let rumoca_ir_solve::FoldTensorNode::Select { condition, .. } = node {
                    validate_reg_defined(defined, condition)?;
                }
            }
            Ok(())
        }
        LinearOp::TableBounds { table_id, .. } => validate_reg_defined(defined, table_id),
        LinearOp::TableLookup {
            table_id,
            column,
            input,
            ..
        } => {
            validate_reg_defined(defined, table_id)?;
            validate_reg_defined(defined, column)?;
            validate_reg_defined(defined, input)
        }
        LinearOp::TableLookupSlope {
            table_id,
            column,
            input,
            ..
        } => {
            validate_reg_defined(defined, table_id)?;
            validate_reg_defined(defined, column)?;
            validate_reg_defined(defined, input)
        }
        LinearOp::TableNextEvent { table_id, time, .. } => {
            validate_reg_defined(defined, table_id)?;
            validate_reg_defined(defined, time)
        }
        LinearOp::RandomInitialState {
            local_seed,
            global_seed,
            ..
        } => {
            validate_reg_defined(defined, local_seed)?;
            validate_reg_defined(defined, global_seed)
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
        } => validate_reg_range_defined(defined, state_start, state_len),
        LinearOp::ImpureRandomInit { seed, .. } => validate_reg_defined(defined, seed),
        LinearOp::ImpureRandom { id, .. } => validate_reg_defined(defined, id),
        LinearOp::ImpureRandomInteger { id, imin, imax, .. } => {
            validate_reg_defined(defined, id)?;
            validate_reg_defined(defined, imin)?;
            validate_reg_defined(defined, imax)
        }
        LinearOp::Move { src, .. } => validate_reg_defined(defined, src),
        LinearOp::LinearSolveComponent {
            matrix_start,
            rhs_start,
            n,
            ..
        } => {
            validate_reg_range_defined(
                defined,
                matrix_start,
                checked_square_len(n, "linear solve matrix")?,
            )?;
            validate_reg_range_defined(defined, rhs_start, n)
        }
        LinearOp::DotProduct {
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            ..
        } => {
            for term in 0..count {
                validate_reg_defined(
                    defined,
                    checked_strided_register(lhs_start, term, lhs_stride, "dot lhs")?,
                )?;
                validate_reg_defined(
                    defined,
                    checked_strided_register(rhs_start, term, rhs_stride, "dot rhs")?,
                )?;
            }
            Ok(())
        }
        LinearOp::MatrixMultiply {
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes,
            ..
        } => {
            let lhs_count = rows
                .checked_mul(inner)
                .and_then(|count| count.checked_mul(lanes))
                .ok_or_else(|| {
                    CompileError::Backend("matrix multiply lhs range overflow".to_string())
                })?;
            let rhs_count = inner
                .checked_mul(columns)
                .and_then(|count| count.checked_mul(lanes))
                .ok_or_else(|| {
                    CompileError::Backend("matrix multiply rhs range overflow".to_string())
                })?;
            validate_reg_range_defined(defined, lhs_start, lhs_count)?;
            validate_reg_range_defined(defined, rhs_start, rhs_count)
        }
        LinearOp::TensorBinary {
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes,
            ..
        } => {
            for element in 0..count {
                let lhs = checked_strided_register(
                    lhs_start,
                    element,
                    lhs_stride.saturating_mul(lanes),
                    "tensor binary lhs",
                )?;
                let rhs = checked_strided_register(
                    rhs_start,
                    element,
                    rhs_stride.saturating_mul(lanes),
                    "tensor binary rhs",
                )?;
                validate_reg_range_defined(defined, lhs, lanes)?;
                validate_reg_range_defined(defined, rhs, lanes)?;
            }
            Ok(())
        }
        LinearOp::TensorCross {
            lhs_start,
            rhs_start,
            lanes,
            ..
        } => {
            let count = 3usize.checked_mul(lanes).ok_or_else(|| {
                CompileError::Backend("tensor cross product range overflow".to_string())
            })?;
            validate_reg_range_defined(defined, lhs_start, count)?;
            validate_reg_range_defined(defined, rhs_start, count)
        }
        LinearOp::TensorTranspose {
            src_start,
            rows,
            columns,
            element_width,
            lanes,
            ..
        } => {
            let count = rows
                .checked_mul(columns)
                .and_then(|count| count.checked_mul(element_width))
                .and_then(|count| count.checked_mul(lanes))
                .ok_or_else(|| {
                    CompileError::Backend("tensor transpose range overflow".to_string())
                })?;
            validate_reg_range_defined(defined, src_start, count)
        }
        LinearOp::TensorConcatenate { sources, lanes, .. } => {
            for source in sources {
                let count = source
                    .dimensions
                    .iter()
                    .try_fold(lanes, |count, extent| count.checked_mul(*extent as usize))
                    .ok_or_else(|| {
                        CompileError::Backend("tensor concatenate source overflow".to_string())
                    })?;
                validate_reg_range_defined(defined, source.start, count)?;
            }
            Ok(())
        }
        LinearOp::TensorUpdate {
            base_start,
            value_start,
            dimensions,
            subscripts,
            lanes,
            ..
        } => {
            let output_count = dimensions
                .iter()
                .try_fold(lanes, |count, extent| count.checked_mul(*extent as usize))
                .ok_or_else(|| CompileError::Backend("tensor update output overflow".into()))?;
            validate_reg_range_defined(defined, base_start, output_count)?;
            let mut value_count = lanes;
            for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
                match subscript {
                    rumoca_ir_solve::TensorUpdateSubscript::Whole => {
                        value_count =
                            value_count.checked_mul(extent as usize).ok_or_else(|| {
                                CompileError::Backend("tensor update value overflow".into())
                            })?;
                    }
                    rumoca_ir_solve::TensorUpdateSubscript::Index(
                        rumoca_ir_solve::TensorIndex::Runtime(register),
                    ) => validate_reg_defined(defined, *register)?,
                    rumoca_ir_solve::TensorUpdateSubscript::Index(
                        rumoca_ir_solve::TensorIndex::Constant(_),
                    ) => {}
                    rumoca_ir_solve::TensorUpdateSubscript::Slice { start, dimensions } => {
                        let count = dimensions
                            .iter()
                            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
                            .ok_or_else(|| {
                                CompileError::Backend("tensor update slice overflow".into())
                            })?;
                        validate_reg_range_defined(defined, *start, count)?;
                        value_count = value_count.checked_mul(count).ok_or_else(|| {
                            CompileError::Backend("tensor update sliced value overflow".into())
                        })?;
                    }
                }
            }
            validate_reg_range_defined(defined, value_start, value_count)
        }
        LinearOp::TensorFill {
            value_start, lanes, ..
        } => validate_reg_range_defined(defined, value_start, lanes),
        LinearOp::Unary { arg, .. } => validate_reg_defined(defined, arg),
        LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
            validate_reg_defined(defined, lhs)?;
            validate_reg_defined(defined, rhs)
        }
        LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => {
            validate_reg_defined(defined, cond)?;
            validate_reg_defined(defined, if_true)?;
            validate_reg_defined(defined, if_false)
        }
        LinearOp::StoreOutput { src } => validate_reg_defined(defined, src),
        LinearOp::FunctionFold {
            initial_start,
            capture_start,
            program,
            ..
        } => {
            validate_reg_range_defined(defined, initial_start, program.carried_count)?;
            validate_reg_range_defined(defined, capture_start, program.capture_count)
        }
        LinearOp::GuardedFunctionFold {
            initial_start,
            capture_start,
            activation,
            program,
            ..
        } => {
            validate_reg_defined(defined, activation)?;
            validate_reg_range_defined(defined, initial_start, program.carried_count)?;
            validate_reg_range_defined(defined, capture_start, program.capture_count)
        }
        LinearOp::FunctionConditional {
            capture_start,
            program,
            ..
        } => validate_reg_range_defined(defined, capture_start, program.capture_count),
        LinearOp::StoreOutputFunctionFold {
            initial,
            capture_start,
            program,
            condition,
            ..
        } => {
            for source in initial {
                if let rumoca_ir_solve::FoldInitialSource::Registers { start, count } = source {
                    validate_reg_range_defined(defined, start, count)?;
                }
            }
            validate_reg_range_defined(defined, capture_start, program.capture_count)?;
            if let Some(condition) = condition {
                validate_reg_defined(defined, condition)?;
            }
            Ok(())
        }
        LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadY { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::LoadSeed { .. }
        | LinearOp::LoadFoldCarried { .. }
        | LinearOp::LoadFoldIndex { .. }
        | LinearOp::LoadFoldCapture { .. }
        | LinearOp::LoadFunctionConditionalCapture { .. }
        | LinearOp::LoadFunctionConditionalCaptureRange { .. }
        | LinearOp::TensorIdentity { .. }
        | LinearOp::TensorLoad { .. } => Ok(()),
        LinearOp::StoreOutputRange {
            start,
            count,
            stride,
        } => {
            for ordinal in 0..count {
                validate_reg_defined(
                    defined,
                    checked_strided_register(start, ordinal, stride, "output range")?,
                )?;
            }
            Ok(())
        }
    }
}

fn validate_reg_defined(defined: &[bool], reg: u32) -> Result<(), CompileError> {
    if defined.get(reg as usize).copied().unwrap_or(false) {
        return Ok(());
    }
    Err(CompileError::Backend(format!(
        "compiled row references undefined register r{reg}"
    )))
}

fn validate_reg_range_defined(
    defined: &[bool],
    start: u32,
    count: usize,
) -> Result<(), CompileError> {
    for offset in 0..count {
        validate_reg_defined(defined, checked_reg_offset(start, offset, "source range")?)?;
    }
    Ok(())
}

fn to_backend_err<E: std::fmt::Display>(err: E) -> CompileError {
    CompileError::Backend(err.to_string())
}

#[cfg(test)]
mod emit_tests;
#[cfg(test)]
mod interpreter_tests;
