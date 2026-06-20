// SPEC_0021 file-size exception: Cranelift emission still keeps builder state,
// intrinsic lowering, and ABI glue together. split plan: move intrinsic
// lowering and memory/ABI helpers into focused emitter submodules.
use super::CompileError;
use cranelift_codegen::ir::condcodes::FloatCC;
use cranelift_codegen::ir::{AbiParam, InstBuilder, MemFlags, types};
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
use rumoca_ir_solve::{BinaryOp, CompareOp, LinearOp, UnaryOp, resolve_indexed_slot};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

mod host_runtime;
mod input_validation;
mod interpreter;

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
type JacobianRowFn = unsafe extern "C" fn(*const f64, *const f64, f64, *const f64, *mut f64);

#[derive(Clone)]
enum RowPlan {
    Simple(SimpleRowPlan),
    General(GeneralRowPlan),
}

struct CompiledResidualRow {
    plan: RowPlan,
    jit: ResidualRowFn,
    validate_with_interpreter: bool,
}

struct CompiledJacobianRow {
    plan: RowPlan,
    jit: JacobianRowFn,
    validate_with_interpreter: bool,
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
    rows: Vec<CompiledResidualRow>,
    input_requirements: InputRequirements,
    regs_scratch: RefCell<Vec<f64>>,
    jit_call_count: Cell<usize>,
}

impl CompiledResidualRows {
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
            let mut regs_scratch = self.regs_scratch.borrow_mut();
            let mut base = 0;
            for row in self.rows.iter() {
                let k = row.plan.output_count();
                self.call_residual_row(row, &mut regs_scratch, inputs, &mut out[base..base + k])?;
                base += k;
            }
            Ok(())
        })
    }

    fn call_residual_row(
        &self,
        row: &CompiledResidualRow,
        regs_scratch: &mut Vec<f64>,
        inputs: RowInputs<'_>,
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        let (y, p, t) = (inputs.y, inputs.p, inputs.t);
        if should_validate_jit_row(row.validate_with_interpreter) {
            let mut expected = vec![0.0; out.len()];
            execute_row(&row.plan, regs_scratch, inputs, &mut expected)?;
            unsafe { call_residual_jit(row.jit, y, p, t, out) };
            self.record_jit_call();
            for (actual, expected) in out.iter().zip(&expected) {
                validate_jit_matches_interpreter("residual", *actual, *expected)?;
            }
            return Ok(());
        }
        unsafe { call_residual_jit(row.jit, y, p, t, out) };
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

pub(crate) struct CompiledJacobianRows {
    _module: JITModule,
    rows: Vec<CompiledJacobianRow>,
    input_requirements: InputRequirements,
    regs_scratch: RefCell<Vec<f64>>,
    jit_call_count: Cell<usize>,
}

impl CompiledJacobianRows {
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
        if should_validate_jit_row(row.validate_with_interpreter) {
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

impl RowKind {
    fn has_seed(self) -> bool {
        matches!(self, Self::JacobianV)
    }
}

pub(crate) fn compile_residual_rows(
    rows: &[Vec<LinearOp>],
) -> Result<CompiledResidualRows, CompileError> {
    let mut emitter = CraneliftEmitter::new()?;
    let plans = plan_rows(rows)?;
    let mut func_ids = checked_vec_with_capacity(rows.len(), "residual row function ids")?;
    for (index, row) in rows.iter().enumerate() {
        validate_row_supported_by_jit(row, RowKind::Residual)?;
        let func_id = emitter.compile_row(
            row,
            RowKind::Residual,
            &format!("rumoca_residual_row_{index}"),
        )?;
        func_ids.push(func_id);
    }
    emitter
        .module
        .finalize_definitions()
        .map_err(to_backend_err)?;
    let input_requirements = input_requirements_for_plans(&plans);
    let mut compiled_rows = checked_vec_with_capacity(rows.len(), "compiled residual rows")?;
    for (index, (plan, func_id)) in plans.into_iter().zip(func_ids).enumerate() {
        let jit = finalized_residual_fn(&emitter.module, func_id)?;
        compiled_rows.push(CompiledResidualRow {
            plan,
            jit,
            validate_with_interpreter: row_uses_table_ops(&rows[index]),
        });
    }
    Ok(CompiledResidualRows {
        _module: emitter.module,
        rows: compiled_rows,
        input_requirements,
        regs_scratch: RefCell::new(Vec::new()),
        jit_call_count: Cell::new(0),
    })
}

pub(crate) fn compile_jacobian_rows(
    rows: &[Vec<LinearOp>],
) -> Result<CompiledJacobianRows, CompileError> {
    let mut emitter = CraneliftEmitter::new()?;
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
        });
    }
    Ok(CompiledJacobianRows {
        _module: emitter.module,
        rows: compiled_rows,
        input_requirements,
        regs_scratch: RefCell::new(Vec::new()),
        jit_call_count: Cell::new(0),
    })
}

fn plan_rows(rows: &[Vec<LinearOp>]) -> Result<Vec<RowPlan>, CompileError> {
    let mut plans = checked_vec_with_capacity(rows.len(), "row plans")?;
    for row in rows {
        plans.push(plan_row(row)?);
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

unsafe fn call_residual_jit(jit: ResidualRowFn, y: &[f64], p: &[f64], t: f64, out: &mut [f64]) {
    // SAFETY: the caller guarantees the function pointer came from
    // finalized_residual_fn, prevalidation ensures any loaded y/p indices are in
    // bounds, and `out` has room for this program's output_count outputs.
    unsafe { jit(y.as_ptr(), p.as_ptr(), t, out.as_mut_ptr()) }
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
    row_requires_validation || cfg!(debug_assertions)
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

struct CraneliftEmitter {
    module: JITModule,
    math: MathImports,
}

impl CraneliftEmitter {
    fn new() -> Result<Self, CompileError> {
        let mut builder =
            JITBuilder::new(cranelift_module::default_libcall_names()).map_err(to_backend_err)?;
        register_math_symbols(&mut builder);
        let module = JITModule::new(builder);
        Ok(Self {
            module,
            math: MathImports::default(),
        })
    }

    fn compile_row(
        &mut self,
        row: &[LinearOp],
        kind: RowKind,
        name: &str,
    ) -> Result<FuncId, CompileError> {
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
            let flags = MemFlags::new();
            let mut row_lower = RowLowerCtx {
                fb: &mut fb,
                module: &mut self.module,
                math: &mut self.math,
                regs: &mut regs,
                y_ptr,
                p_ptr,
                t_value,
                v_ptr,
                flags,
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
    flags: MemFlags,
}

impl<'a, 'b> RowLowerCtx<'a, 'b> {
    /// Lower every op in `row`, writing each `StoreOutput` result to the next
    /// consecutive `out[k]` slot so a multi-output program fills consecutive
    /// output slots. (`lower_op` returns `Some(value)` exactly for StoreOutput.)
    fn lower_row_outputs(
        &mut self,
        row: &[LinearOp],
        out_ptr: cranelift_codegen::ir::Value,
    ) -> Result<(), CompileError> {
        let mut out_idx: i32 = 0;
        for &op in row {
            if let Some(value) = self.lower_op(op)? {
                self.fb.ins().store(self.flags, value, out_ptr, out_idx * 8);
                out_idx += 1;
            }
        }
        Ok(())
    }

    fn lower_op(
        &mut self,
        op: LinearOp,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        match op {
            LinearOp::Const { dst, value } => {
                let value = self.fb.ins().f64const(value);
                self.insert(dst, value)
            }
            LinearOp::LoadTime { dst } => self.insert(dst, self.t_value),
            LinearOp::LoadY { dst, index } => self.lower_loaded_reg(dst, self.y_ptr, index),
            LinearOp::LoadP { dst, index } => self.lower_loaded_reg(dst, self.p_ptr, index),
            LinearOp::LoadIndexedP {
                dst,
                base,
                count,
                index,
            } => self.lower_indexed_loaded_reg(dst, self.p_ptr, base, count, index),
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
            LinearOp::Move { dst, src } => {
                let value = lookup_reg(self.regs, src)?;
                self.insert(dst, value)
            }
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                component,
            } => self.lower_linear_solve_component(dst, matrix_start, rhs_start, n, component),
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
                let x = lookup_reg(self.regs, arg)?;
                let value = emit_unary_op(self.fb, self.module, self.math, op, x)?;
                self.insert(dst, value)
            }
            LinearOp::Binary { dst, op, lhs, rhs } => {
                let l = lookup_reg(self.regs, lhs)?;
                let r = lookup_reg(self.regs, rhs)?;
                let value = emit_binary_op(self.fb, self.module, self.math, op, l, r)?;
                self.insert(dst, value)
            }
            LinearOp::Compare { dst, op, lhs, rhs } => {
                let l = lookup_reg(self.regs, lhs)?;
                let r = lookup_reg(self.regs, rhs)?;
                let cond = emit_compare_op(self.fb, op, l, r);
                let value = bool_to_f64(self.fb, cond);
                self.insert(dst, value)
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => self.lower_select(dst, cond, if_true, if_false),
            LinearOp::StoreOutput { src } => Ok(Some(lookup_reg(self.regs, src)?)),
        }
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
        let idx = lookup_reg(self.regs, index)?;
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
        let table_id = lookup_reg(self.regs, table_id)?;
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
        let table_id = lookup_reg(self.regs, table_id)?;
        let column = lookup_reg(self.regs, column)?;
        let input = lookup_reg(self.regs, input)?;
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
        let table_id = lookup_reg(self.regs, table_id)?;
        let time = lookup_reg(self.regs, time)?;
        let value = call_table_host(
            self.fb,
            self.module,
            self.math,
            TableHostFn::NextEvent,
            &[table_id, time],
        )?;
        self.insert(dst, value)
    }

    fn lower_select(
        &mut self,
        dst: u32,
        cond: u32,
        if_true: u32,
        if_false: u32,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        let cond_value = lookup_reg(self.regs, cond)?;
        let t = lookup_reg(self.regs, if_true)?;
        let f = lookup_reg(self.regs, if_false)?;
        let zero = self.fb.ins().f64const(0.0);
        let is_true = self.fb.ins().fcmp(FloatCC::NotEqual, cond_value, zero);
        let value = self.fb.ins().select(is_true, t, f);
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
            matrix.push(lookup_reg(self.regs, reg)?);
        }
        let mut rhs = checked_vec_with_capacity(n, "linear solve rhs values")?;
        for offset in 0..n {
            let reg = checked_reg_offset(rhs_start, offset, "rhs")?;
            rhs.push(lookup_reg(self.regs, reg)?);
        }

        let solution = emit_dense_linear_solve(self.fb, &mut matrix, &mut rhs, n)?;
        self.insert(dst, solution[component])
    }

    fn insert(
        &mut self,
        dst: u32,
        value: cranelift_codegen::ir::Value,
    ) -> Result<Option<cranelift_codegen::ir::Value>, CompileError> {
        self.regs.insert(dst, value);
        Ok(None)
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
        BinaryOp::Div => guarded_division(fb, lhs, rhs),
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

fn guarded_division(
    fb: &mut FunctionBuilder<'_>,
    lhs: cranelift_codegen::ir::Value,
    rhs: cranelift_codegen::ir::Value,
) -> cranelift_codegen::ir::Value {
    let zero = fb.ins().f64const(0.0);
    let inf = fb.ins().f64const(f64::INFINITY);
    let rhs_zero = fb.ins().fcmp(FloatCC::Equal, rhs, zero);
    let lhs_zero = fb.ins().fcmp(FloatCC::Equal, lhs, zero);
    let safe = fb.ins().fdiv(lhs, rhs);
    let rhs_zero_value = fb.ins().select(lhs_zero, zero, inf);
    fb.ins().select(rhs_zero, rhs_zero_value, safe)
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

fn lookup_reg(
    regs: &HashMap<u32, cranelift_codegen::ir::Value>,
    reg: u32,
) -> Result<cranelift_codegen::ir::Value, CompileError> {
    regs.get(&reg)
        .copied()
        .ok_or_else(|| CompileError::Backend(format!("missing source register r{reg}")))
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

fn plan_row(row: &[LinearOp]) -> Result<RowPlan, CompileError> {
    let mut reg_count = 0usize;
    let input_requirements = input_requirements_for_linear_ops(row)?;
    for op in row {
        if let Some(index) = max_reg_index(*op)? {
            reg_count = reg_count.max(index.checked_add(1).ok_or_else(|| {
                CompileError::Backend("compiled row register count overflow".to_string())
            })?);
        }
    }
    let mut defined = checked_vec_with_capacity(reg_count, "defined register flags")?;
    defined.resize(reg_count, false);
    for op in row {
        validate_row_sources(&defined, *op)?;
        if let Some(dst) = dst_reg(*op) {
            defined[dst] = true;
        }
    }
    // Collect every output (one per StoreOutput, in order) and keep the
    // compute ops as the body. A program may emit several outputs.
    let mut output_srcs = Vec::new();
    let mut body: Vec<LinearOp> = Vec::with_capacity(row.len());
    for op in row {
        if let LinearOp::StoreOutput { src } = op {
            output_srcs.push(*src as usize);
        } else {
            body.push(*op);
        }
    }
    if output_srcs.is_empty() {
        return Err(CompileError::Backend(
            "compiled row is missing StoreOutput".to_string(),
        ));
    }
    let output_srcs = output_srcs.into_boxed_slice();

    if body.iter().copied().all(is_simple_linear_op) {
        let mut ops = checked_vec_with_capacity(body.len(), "simple runtime ops")?;
        for op in body.iter().copied() {
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

fn is_simple_linear_op(op: LinearOp) -> bool {
    !matches!(
        op,
        LinearOp::LoadSeed { .. }
            | LinearOp::LoadIndexedP { .. }
            | LinearOp::LoadIndexedSeed { .. }
            | LinearOp::Move { .. }
            | LinearOp::LinearSolveComponent { .. }
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
        | LinearOp::LoadIndexedSeed { .. }
        | LinearOp::Move { .. }
        | LinearOp::LinearSolveComponent { .. }
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
        | LinearOp::StoreOutput { .. } => Err(CompileError::Backend(
            "attempted to lower non-simple runtime op onto the simple row path".to_string(),
        )),
    }
}

fn lower_runtime_index(index: usize, kind: &str) -> Result<u32, CompileError> {
    u32::try_from(index)
        .map_err(|_| CompileError::Backend(format!("{kind} index exceeds u32 runtime plan")))
}

fn max_reg_index(op: LinearOp) -> Result<Option<usize>, CompileError> {
    match op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::LoadSeed { dst, .. }
        | LinearOp::TableBounds { dst, .. }
        | LinearOp::TableLookup { dst, .. }
        | LinearOp::TableLookupSlope { dst, .. }
        | LinearOp::TableNextEvent { dst, .. } => Ok(Some(dst as usize)),
        LinearOp::LoadIndexedP { dst, index, .. }
        | LinearOp::LoadIndexedSeed { dst, index, .. } => Ok(Some(dst.max(index) as usize)),
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
        | LinearOp::Unary { dst, .. }
        | LinearOp::Binary { dst, .. }
        | LinearOp::Compare { dst, .. }
        | LinearOp::LoadIndexedP { dst, .. }
        | LinearOp::LoadIndexedSeed { dst, .. }
        | LinearOp::Select { dst, .. } => Some(dst as usize),
        LinearOp::StoreOutput { .. } => None,
    }
}

fn validate_row_sources(defined: &[bool], op: LinearOp) -> Result<(), CompileError> {
    match op {
        LinearOp::LoadIndexedP { index, .. } | LinearOp::LoadIndexedSeed { index, .. } => {
            validate_reg_defined(defined, index)
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
        LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadY { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::LoadSeed { .. } => Ok(()),
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
