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
    try_eval_table_bound_value_in, try_eval_table_lookup_slope_value_in,
    try_eval_table_lookup_value_in, try_eval_time_table_next_event_value_in,
};
use rumoca_ir_solve::{BinaryOp, CompareOp, ExternalFunctionKind, LinearOp, UnaryOp};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;

mod host_runtime;
mod input_validation;

use host_runtime::{register_math_symbols, with_active_external_tables};
pub(crate) use input_validation::InputRequirements as EmitInputRequirements;
use input_validation::{
    InputRequirements, input_requirements_for_linear_ops, input_requirements_for_plans,
    row_input_requirements, validate_input_requirements, validate_output_len,
};

type ResidualRowFn = unsafe extern "C" fn(*const f64, *const f64, f64) -> f64;
type JacobianRowFn = unsafe extern "C" fn(*const f64, *const f64, f64, *const f64) -> f64;

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

#[derive(Clone)]
struct SimpleRowPlan {
    ops: Box<[SimpleOp]>,
    reg_count: usize,
    output_src: usize,
    input_requirements: InputRequirements,
}

#[derive(Clone)]
struct GeneralRowPlan {
    ops: Box<[LinearOp]>,
    reg_count: usize,
    output_src: usize,
    input_requirements: InputRequirements,
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
        validate_output_len(out, self.rows.len())?;
        validate_input_requirements(self.input_requirements, y, p, None)?;
        with_active_external_tables(external_tables, || {
            let mut regs_scratch = self.regs_scratch.borrow_mut();
            for (index, row) in self.rows.iter().enumerate() {
                out[index] =
                    self.call_residual_row(row, &mut regs_scratch, y, p, t, external_tables)?;
            }
            Ok(())
        })
    }

    fn call_residual_row(
        &self,
        row: &CompiledResidualRow,
        regs_scratch: &mut Vec<f64>,
        y: &[f64],
        p: &[f64],
        t: f64,
        external_tables: &[ExternalTableData],
    ) -> Result<f64, CompileError> {
        if should_validate_jit_row(row.validate_with_interpreter) {
            let expected = execute_row(&row.plan, regs_scratch, y, p, t, None, external_tables)?;
            let actual = unsafe { call_residual_jit(row.jit, y, p, t) };
            self.record_jit_call();
            validate_jit_matches_interpreter("residual", actual, expected)?;
            return Ok(actual);
        }
        let actual = unsafe { call_residual_jit(row.jit, y, p, t) };
        self.record_jit_call();
        Ok(actual)
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
        validate_output_len(out, self.rows.len())?;
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
            for (index, row) in self.rows.iter().enumerate() {
                out[index] = self.call_jacobian_row(row, &mut regs_scratch, &ctx)?;
            }
            Ok(())
        })
    }

    fn call_jacobian_row(
        &self,
        row: &CompiledJacobianRow,
        regs_scratch: &mut Vec<f64>,
        ctx: &JacobianCallContext<'_>,
    ) -> Result<f64, CompileError> {
        if should_validate_jit_row(row.validate_with_interpreter) {
            let expected = execute_row(
                &row.plan,
                regs_scratch,
                ctx.y,
                ctx.p,
                ctx.t,
                Some(ctx.v),
                ctx.external_tables,
            )?;
            let actual = unsafe { call_jacobian_jit(row.jit, ctx.y, ctx.p, ctx.t, ctx.v) };
            self.record_jit_call();
            validate_jit_matches_interpreter("jacobian", actual, expected)?;
            return Ok(actual);
        }
        let actual = unsafe { call_jacobian_jit(row.jit, ctx.y, ctx.p, ctx.t, ctx.v) };
        self.record_jit_call();
        Ok(actual)
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
    let plans = rows
        .iter()
        .map(|row| plan_row(row))
        .collect::<Result<Vec<_>, _>>()?;
    let mut func_ids = Vec::with_capacity(rows.len());
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
    let rows = plans
        .into_iter()
        .zip(func_ids)
        .enumerate()
        .map(|(index, (plan, func_id))| {
            let jit = finalized_residual_fn(&emitter.module, func_id)?;
            Ok(CompiledResidualRow {
                plan,
                jit,
                validate_with_interpreter: row_uses_table_ops(&rows[index]),
            })
        })
        .collect::<Result<Vec<_>, CompileError>>()?;
    Ok(CompiledResidualRows {
        _module: emitter.module,
        rows,
        input_requirements,
        regs_scratch: RefCell::new(Vec::new()),
        jit_call_count: Cell::new(0),
    })
}

pub(crate) fn compile_jacobian_rows(
    rows: &[Vec<LinearOp>],
) -> Result<CompiledJacobianRows, CompileError> {
    let mut emitter = CraneliftEmitter::new()?;
    let plans = rows
        .iter()
        .map(|row| plan_row(row))
        .collect::<Result<Vec<_>, _>>()?;
    let mut func_ids = Vec::with_capacity(rows.len());
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
    let rows = plans
        .into_iter()
        .zip(func_ids)
        .enumerate()
        .map(|(index, (plan, func_id))| {
            let jit = finalized_jacobian_fn(&emitter.module, func_id)?;
            Ok(CompiledJacobianRow {
                plan,
                jit,
                validate_with_interpreter: row_uses_table_ops(&rows[index]),
            })
        })
        .collect::<Result<Vec<_>, CompileError>>()?;
    Ok(CompiledJacobianRows {
        _module: emitter.module,
        rows,
        input_requirements,
        regs_scratch: RefCell::new(Vec::new()),
        jit_call_count: Cell::new(0),
    })
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

unsafe fn call_residual_jit(jit: ResidualRowFn, y: &[f64], p: &[f64], t: f64) -> f64 {
    // SAFETY: the caller guarantees the function pointer came from
    // finalized_residual_fn, and prevalidation ensures any loaded y/p indices
    // are in bounds before the JIT reads through these pointers.
    unsafe { jit(y.as_ptr(), p.as_ptr(), t) }
}

unsafe fn call_jacobian_jit(jit: JacobianRowFn, y: &[f64], p: &[f64], t: f64, v: &[f64]) -> f64 {
    // SAFETY: the caller guarantees the function pointer came from
    // finalized_jacobian_fn, and prevalidation ensures any loaded y/p/seed
    // indices are in bounds before the JIT reads through these pointers.
    unsafe { jit(y.as_ptr(), p.as_ptr(), t, v.as_ptr()) }
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
        signature.returns.push(AbiParam::new(types::F64));

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
            let v_ptr = if kind.has_seed() {
                Some(params[3])
            } else {
                None
            };

            let mut regs: HashMap<u32, cranelift_codegen::ir::Value> = HashMap::new();
            let mut output = fb.ins().f64const(0.0);
            let mut row_lower = RowLowerCtx {
                fb: &mut fb,
                module: &mut self.module,
                math: &mut self.math,
                regs: &mut regs,
                y_ptr,
                p_ptr,
                t_value,
                v_ptr,
                flags: MemFlags::new(),
            };

            for &op in row {
                output = row_lower.lower_op(op)?.unwrap_or(output);
            }
            fb.ins().return_(&[output]);
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
            LinearOp::LoadSeed { dst, index } => self.lower_seed_reg(dst, index),
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
            LinearOp::ExternalCall { function, .. } => Err(external_call_compile_error(function)),
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

        let mut matrix = Vec::with_capacity(matrix_len);
        for offset in 0..matrix_len {
            let reg = checked_reg_offset(matrix_start, offset, "matrix")?;
            matrix.push(lookup_reg(self.regs, reg)?);
        }
        let mut rhs = Vec::with_capacity(n);
        for offset in 0..n {
            let reg = checked_reg_offset(rhs_start, offset, "rhs")?;
            rhs.push(lookup_reg(self.regs, reg)?);
        }

        let solution = emit_dense_linear_solve(self.fb, &mut matrix, &mut rhs, n);
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
) -> Vec<cranelift_codegen::ir::Value> {
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
    let mut solution = vec![zero; n];
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
    solution
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
    let input_requirements = input_requirements_for_linear_ops(row);
    for op in row {
        reg_count = reg_count.max(max_reg_index(*op).map_or(0, |index| index + 1));
    }
    let mut defined = vec![false; reg_count];
    for op in row {
        validate_row_sources(&defined, *op)?;
        if let Some(dst) = dst_reg(*op) {
            defined[dst] = true;
        }
    }
    let output_src = match row.last().copied() {
        Some(LinearOp::StoreOutput { src }) => src as usize,
        _ => {
            return Err(CompileError::Backend(
                "compiled row is missing final StoreOutput".to_string(),
            ));
        }
    };

    let body = &row[..row.len().saturating_sub(1)];
    if body.iter().copied().all(is_simple_linear_op) {
        let ops = body
            .iter()
            .copied()
            .map(lower_simple_op)
            .collect::<Result<Vec<_>, _>>()?;
        return Ok(RowPlan::Simple(SimpleRowPlan {
            ops: ops.into_boxed_slice(),
            reg_count,
            output_src,
            input_requirements,
        }));
    }

    Ok(RowPlan::General(GeneralRowPlan {
        ops: body.to_vec().into_boxed_slice(),
        reg_count,
        output_src,
        input_requirements,
    }))
}

fn is_simple_linear_op(op: LinearOp) -> bool {
    !matches!(
        op,
        LinearOp::LoadSeed { .. }
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
            | LinearOp::ExternalCall { .. }
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
        | LinearOp::ExternalCall { .. }
        | LinearOp::StoreOutput { .. } => Err(CompileError::Backend(
            "attempted to lower non-simple runtime op onto the simple row path".to_string(),
        )),
    }
}

fn lower_runtime_index(index: usize, kind: &str) -> Result<u32, CompileError> {
    u32::try_from(index)
        .map_err(|_| CompileError::Backend(format!("{kind} index exceeds u32 runtime plan")))
}

fn max_reg_index(op: LinearOp) -> Option<usize> {
    match op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::LoadSeed { dst, .. }
        | LinearOp::TableBounds { dst, .. }
        | LinearOp::TableLookup { dst, .. }
        | LinearOp::TableLookupSlope { dst, .. }
        | LinearOp::TableNextEvent { dst, .. } => Some(dst as usize),
        LinearOp::RandomInitialState {
            dst,
            local_seed,
            global_seed,
            ..
        } => Some(dst.max(local_seed).max(global_seed) as usize),
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
        } => Some(dst.max(state_start.saturating_add(state_len.saturating_sub(1) as u32)) as usize),
        LinearOp::ImpureRandomInit { dst, seed } => Some(dst.max(seed) as usize),
        LinearOp::ImpureRandom { dst, id, .. } => Some(dst.max(id) as usize),
        LinearOp::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            ..
        } => Some(dst.max(id).max(imin).max(imax) as usize),
        LinearOp::ExternalCall {
            dst,
            args,
            arg_count,
            ..
        } => Some(args.iter().copied().take(arg_count).fold(dst, u32::max) as usize),
        LinearOp::Move { dst, src } => Some(dst.max(src) as usize),
        LinearOp::LinearSolveComponent {
            dst,
            matrix_start,
            rhs_start,
            n,
            ..
        } => Some(
            dst.max(matrix_start.saturating_add((n.saturating_mul(n)).saturating_sub(1) as u32))
                .max(rhs_start.saturating_add(n.saturating_sub(1) as u32)) as usize,
        ),
        LinearOp::Unary { dst, arg, .. } => Some((dst.max(arg)) as usize),
        LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } => {
            Some(dst.max(lhs).max(rhs) as usize)
        }
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => Some(dst.max(cond).max(if_true).max(if_false) as usize),
        LinearOp::StoreOutput { src } => Some(src as usize),
    }
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
        | LinearOp::ExternalCall { dst, .. }
        | LinearOp::Move { dst, .. }
        | LinearOp::LinearSolveComponent { dst, .. }
        | LinearOp::Unary { dst, .. }
        | LinearOp::Binary { dst, .. }
        | LinearOp::Compare { dst, .. }
        | LinearOp::Select { dst, .. } => Some(dst as usize),
        LinearOp::StoreOutput { .. } => None,
    }
}

fn validate_row_sources(defined: &[bool], op: LinearOp) -> Result<(), CompileError> {
    match op {
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
        LinearOp::ExternalCall {
            args, arg_count, ..
        } => args
            .iter()
            .copied()
            .take(arg_count)
            .try_for_each(|arg| validate_reg_defined(defined, arg)),
        LinearOp::Move { src, .. } => validate_reg_defined(defined, src),
        LinearOp::LinearSolveComponent {
            matrix_start,
            rhs_start,
            n,
            ..
        } => {
            validate_reg_range_defined(defined, matrix_start, n.saturating_mul(n))?;
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
        validate_reg_defined(defined, start.saturating_add(offset as u32))?;
    }
    Ok(())
}

#[inline(always)]
fn execute_row(
    row: &RowPlan,
    regs_scratch: &mut Vec<f64>,
    y: &[f64],
    p: &[f64],
    t: f64,
    seed: Option<&[f64]>,
    external_tables: &[ExternalTableData],
) -> Result<f64, CompileError> {
    validate_input_requirements(row_input_requirements(row), y, p, seed)?;
    match row {
        RowPlan::Simple(row) => execute_simple_row(row, regs_scratch, y, p, t),
        RowPlan::General(row) => {
            execute_general_row(row, regs_scratch, y, p, t, seed, external_tables)
        }
    }
}

#[inline(always)]
fn execute_simple_row(
    row: &SimpleRowPlan,
    regs_scratch: &mut Vec<f64>,
    y: &[f64],
    p: &[f64],
    t: f64,
) -> Result<f64, CompileError> {
    let regs = runtime_reg_slice(regs_scratch, row.reg_count);
    for op in row.ops.iter().copied() {
        match op {
            SimpleOp::Const { dst, value } => set_reg_value(regs, dst as usize, value),
            SimpleOp::LoadTime { dst } => set_reg_value(regs, dst as usize, t),
            SimpleOp::LoadY { dst, index } => {
                let value = read_input_value("y", y, index as usize)?;
                set_reg_value(regs, dst as usize, value)
            }
            SimpleOp::LoadP { dst, index } => {
                let value = read_input_value("p", p, index as usize)?;
                set_reg_value(regs, dst as usize, value)
            }
            SimpleOp::Unary { dst, op, arg } => {
                let x = read_reg_value(regs, arg as usize);
                set_reg_value(regs, dst as usize, apply_unary(op, x));
            }
            SimpleOp::Binary { dst, op, lhs, rhs } => {
                let lhs = read_reg_value(regs, lhs as usize);
                let rhs = read_reg_value(regs, rhs as usize);
                set_reg_value(regs, dst as usize, apply_binary(op, lhs, rhs));
            }
            SimpleOp::Compare { dst, op, lhs, rhs } => {
                let lhs = read_reg_value(regs, lhs as usize);
                let rhs = read_reg_value(regs, rhs as usize);
                set_reg_value(regs, dst as usize, apply_compare(op, lhs, rhs));
            }
            SimpleOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                let cond = read_reg_value(regs, cond as usize);
                let if_true = read_reg_value(regs, if_true as usize);
                let if_false = read_reg_value(regs, if_false as usize);
                set_reg_value(
                    regs,
                    dst as usize,
                    if cond != 0.0 { if_true } else { if_false },
                );
            }
        }
    }
    Ok(read_reg_value(regs, row.output_src))
}

#[inline(always)]
fn execute_general_row(
    row: &GeneralRowPlan,
    regs_scratch: &mut Vec<f64>,
    y: &[f64],
    p: &[f64],
    t: f64,
    seed: Option<&[f64]>,
    external_tables: &[ExternalTableData],
) -> Result<f64, CompileError> {
    let regs = runtime_reg_slice(regs_scratch, row.reg_count);
    for op in row.ops.iter().copied() {
        execute_general_op(regs, op, y, p, t, seed, external_tables)?;
    }
    Ok(read_reg_value(regs, row.output_src))
}

#[inline(always)]
fn execute_general_op(
    regs: &mut [f64],
    op: LinearOp,
    y: &[f64],
    p: &[f64],
    t: f64,
    seed: Option<&[f64]>,
    external_tables: &[ExternalTableData],
) -> Result<(), CompileError> {
    match op {
        LinearOp::Const { dst, value } => set_reg_value(regs, dst as usize, value),
        LinearOp::LoadTime { dst } => set_reg_value(regs, dst as usize, t),
        LinearOp::LoadY { dst, index } => {
            let value = read_input_value("y", y, index)?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadP { dst, index } => {
            let value = read_input_value("p", p, index)?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LoadSeed { dst, index } => {
            let seed = seed.ok_or_else(|| input_compile_error("seed", index, 0))?;
            let value = read_input_value("seed", seed, index)?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::Move { dst, src } => {
            let value = read_reg_value(regs, src as usize);
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::LinearSolveComponent { dst, .. } => {
            set_reg_value(regs, dst as usize, eval_linear_solve_component(regs, op));
        }
        LinearOp::TableBounds { dst, table_id, max } => {
            let table_id = read_reg_value(regs, table_id as usize);
            let value =
                try_eval_table_bound_value_in(table_id, max, external_tables).ok_or_else(|| {
                    table_compile_error(
                        if max { "bounds max" } else { "bounds min" },
                        table_id,
                        None,
                    )
                })?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::TableLookup { .. }
        | LinearOp::TableLookupSlope { .. }
        | LinearOp::TableNextEvent { .. } => execute_general_table_op(regs, op, external_tables)?,
        LinearOp::RandomInitialState { dst, .. }
        | LinearOp::RandomResult { dst, .. }
        | LinearOp::RandomState { dst, .. }
        | LinearOp::ImpureRandomInit { dst, .. }
        | LinearOp::ImpureRandom { dst, .. }
        | LinearOp::ImpureRandomInteger { dst, .. } => set_reg_value(regs, dst as usize, 1.0),
        LinearOp::ExternalCall { function, .. } => {
            return Err(external_call_compile_error(function));
        }
        LinearOp::Unary { dst, op, arg } => {
            let x = read_reg_value(regs, arg as usize);
            set_reg_value(regs, dst as usize, apply_unary(op, x));
        }
        LinearOp::Binary { dst, op, lhs, rhs } => {
            let lhs = read_reg_value(regs, lhs as usize);
            let rhs = read_reg_value(regs, rhs as usize);
            set_reg_value(regs, dst as usize, apply_binary(op, lhs, rhs));
        }
        LinearOp::Compare { dst, op, lhs, rhs } => {
            let lhs = read_reg_value(regs, lhs as usize);
            let rhs = read_reg_value(regs, rhs as usize);
            set_reg_value(regs, dst as usize, apply_compare(op, lhs, rhs));
        }
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => {
            let cond = read_reg_value(regs, cond as usize);
            let if_true = read_reg_value(regs, if_true as usize);
            let if_false = read_reg_value(regs, if_false as usize);
            set_reg_value(
                regs,
                dst as usize,
                if cond != 0.0 { if_true } else { if_false },
            );
        }
        LinearOp::StoreOutput { .. } => {}
    }
    Ok(())
}

fn external_call_compile_error(function: ExternalFunctionKind) -> CompileError {
    CompileError::Backend(format!(
        "external function {function:?} requires a native runtime bridge not provided by rumoca-exec-cranelift"
    ))
}

fn execute_general_table_op(
    regs: &mut [f64],
    op: LinearOp,
    external_tables: &[ExternalTableData],
) -> Result<(), CompileError> {
    match op {
        LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        } => {
            let table_id = read_reg_value(regs, table_id as usize);
            let column = read_reg_value(regs, column as usize);
            let input = read_reg_value(regs, input as usize);
            let value = try_eval_table_lookup_value_in(table_id, column, input, external_tables)
                .ok_or_else(|| table_compile_error("lookup", table_id, Some(column)))?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        } => {
            let table_id = read_reg_value(regs, table_id as usize);
            let column = read_reg_value(regs, column as usize);
            let input = read_reg_value(regs, input as usize);
            let value =
                try_eval_table_lookup_slope_value_in(table_id, column, input, external_tables)
                    .ok_or_else(|| table_compile_error("lookup slope", table_id, Some(column)))?;
            set_reg_value(regs, dst as usize, value);
        }
        LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        } => {
            let table_id = read_reg_value(regs, table_id as usize);
            let time = read_reg_value(regs, time as usize);
            let value = try_eval_time_table_next_event_value_in(table_id, time, external_tables)
                .ok_or_else(|| table_compile_error("next event", table_id, None))?;
            set_reg_value(regs, dst as usize, value);
        }
        _ => {}
    }
    Ok(())
}

fn table_compile_error(
    operation: &'static str,
    table_id: f64,
    column: Option<f64>,
) -> CompileError {
    let message = if let Some(column) = column {
        format!("external table {operation} failed for table id {table_id} column {column}")
    } else {
        format!("external table {operation} failed for table id {table_id}")
    };
    CompileError::Input(message)
}

fn read_input_value(
    vector: &'static str,
    values: &[f64],
    index: usize,
) -> Result<f64, CompileError> {
    values
        .get(index)
        .copied()
        .ok_or_else(|| input_compile_error(vector, index, values.len()))
}

fn input_compile_error(vector: &'static str, index: usize, len: usize) -> CompileError {
    CompileError::Input(format!(
        "missing {vector}[{index}] while evaluating Cranelift row; vector length is {len}"
    ))
}

#[inline(always)]
fn runtime_reg_slice(regs_scratch: &mut Vec<f64>, reg_count: usize) -> &mut [f64] {
    if regs_scratch.len() < reg_count {
        regs_scratch.resize(reg_count, 0.0);
    }
    &mut regs_scratch[..reg_count]
}

#[inline(always)]
fn set_reg_value(regs: &mut [f64], reg: usize, value: f64) {
    regs[reg] = value;
}

#[inline(always)]
fn read_reg_value(regs: &[f64], reg: usize) -> f64 {
    regs[reg]
}

fn eval_linear_solve_component(regs: &[f64], op: LinearOp) -> f64 {
    let LinearOp::LinearSolveComponent {
        matrix_start,
        rhs_start,
        n,
        component,
        ..
    } = op
    else {
        return f64::NAN;
    };
    if n == 0 || component >= n {
        return f64::NAN;
    }
    let mut matrix = vec![0.0; n * n];
    let mut rhs = vec![0.0; n];
    for row in 0..n {
        rhs[row] = read_reg_value(regs, rhs_start as usize + row);
        for col in 0..n {
            matrix[row * n + col] = read_reg_value(regs, matrix_start as usize + row * n + col);
        }
    }
    solve_dense_component(&mut matrix, &mut rhs, n, component)
}

fn solve_dense_component(matrix: &mut [f64], rhs: &mut [f64], n: usize, component: usize) -> f64 {
    for col in 0..n {
        let Some(pivot) = pivot_row(matrix, n, col) else {
            return f64::NAN;
        };
        swap_dense_rows(matrix, rhs, n, col, pivot);
        let pivot_value = matrix[col * n + col];
        if pivot_value.abs() <= f64::EPSILON {
            return f64::NAN;
        }
        for row in col + 1..n {
            let factor = matrix[row * n + col] / pivot_value;
            matrix[row * n + col] = 0.0;
            for entry in col + 1..n {
                matrix[row * n + entry] -= factor * matrix[col * n + entry];
            }
            rhs[row] -= factor * rhs[col];
        }
    }

    let mut solution = vec![0.0; n];
    for row in (0..n).rev() {
        let tail = ((row + 1)..n)
            .map(|col| matrix[row * n + col] * solution[col])
            .sum::<f64>();
        solution[row] = (rhs[row] - tail) / matrix[row * n + row];
    }
    solution[component]
}

fn pivot_row(matrix: &[f64], n: usize, col: usize) -> Option<usize> {
    (col..n).max_by(|&lhs, &rhs| {
        matrix[lhs * n + col]
            .abs()
            .total_cmp(&matrix[rhs * n + col].abs())
    })
}

fn swap_dense_rows(matrix: &mut [f64], rhs: &mut [f64], n: usize, lhs: usize, rhs_row: usize) {
    if lhs == rhs_row {
        return;
    }
    for col in 0..n {
        matrix.swap(lhs * n + col, rhs_row * n + col);
    }
    rhs.swap(lhs, rhs_row);
}

#[inline(always)]
fn apply_unary(op: UnaryOp, value: f64) -> f64 {
    match op {
        UnaryOp::Neg => -value,
        UnaryOp::Not => {
            if value == 0.0 {
                1.0
            } else {
                0.0
            }
        }
        UnaryOp::Abs => value.abs(),
        UnaryOp::Sign => {
            if value > 0.0 {
                1.0
            } else if value < 0.0 {
                -1.0
            } else {
                0.0
            }
        }
        UnaryOp::Sqrt => value.sqrt(),
        UnaryOp::Floor => value.floor(),
        UnaryOp::Ceil => value.ceil(),
        UnaryOp::Trunc => value.trunc(),
        UnaryOp::Sin => value.sin(),
        UnaryOp::Cos => value.cos(),
        UnaryOp::Tan => value.tan(),
        UnaryOp::Asin => value.asin(),
        UnaryOp::Acos => value.acos(),
        UnaryOp::Atan => value.atan(),
        UnaryOp::Sinh => value.sinh(),
        UnaryOp::Cosh => value.cosh(),
        UnaryOp::Tanh => value.tanh(),
        UnaryOp::Exp => value.exp(),
        UnaryOp::Log => value.ln(),
        UnaryOp::Log10 => value.log10(),
    }
}

#[inline(always)]
fn apply_binary(op: BinaryOp, lhs: f64, rhs: f64) -> f64 {
    match op {
        BinaryOp::Add => lhs + rhs,
        BinaryOp::Sub => lhs - rhs,
        BinaryOp::Mul => lhs * rhs,
        BinaryOp::Div => {
            if rhs == 0.0 {
                if lhs == 0.0 { 0.0 } else { f64::INFINITY }
            } else {
                lhs / rhs
            }
        }
        BinaryOp::Pow => lhs.powf(rhs),
        BinaryOp::And => {
            if lhs != 0.0 && rhs != 0.0 {
                1.0
            } else {
                0.0
            }
        }
        BinaryOp::Or => {
            if lhs != 0.0 || rhs != 0.0 {
                1.0
            } else {
                0.0
            }
        }
        BinaryOp::Atan2 => lhs.atan2(rhs),
        BinaryOp::Min => lhs.min(rhs),
        BinaryOp::Max => lhs.max(rhs),
    }
}

#[inline(always)]
fn apply_compare(op: CompareOp, lhs: f64, rhs: f64) -> f64 {
    op.compare_as_f64(lhs, rhs)
}

fn to_backend_err<E: std::fmt::Display>(err: E) -> CompileError {
    CompileError::Backend(err.to_string())
}

#[cfg(test)]
mod emit_tests;
