use super::CompileError;
use crate::runtime::eval::{
    eval_table_bound_value, eval_table_lookup_slope_value, eval_table_lookup_value,
    eval_time_table_next_event_value,
};
use cranelift_codegen::ir::condcodes::FloatCC;
use cranelift_codegen::ir::{AbiParam, InstBuilder, MemFlags, types};
use cranelift_codegen::settings;
use cranelift_codegen::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use rumoca_ir_solve::{BinaryOp, CompareOp, LinearOp, UnaryOp};
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Clone)]
enum RowPlan {
    Simple(SimpleRowPlan),
    General(GeneralRowPlan),
}

#[derive(Clone)]
struct SimpleRowPlan {
    ops: Box<[SimpleOp]>,
    reg_count: usize,
    output_src: usize,
}

#[derive(Clone)]
struct GeneralRowPlan {
    ops: Box<[LinearOp]>,
    reg_count: usize,
    output_src: usize,
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
    rows: Vec<RowPlan>,
    regs_scratch: RefCell<Vec<f64>>,
}

impl CompiledResidualRows {
    pub(crate) fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        let mut regs_scratch = self.regs_scratch.borrow_mut();
        validate_output_len(out, self.rows.len())?;
        for (index, row) in self.rows.iter().enumerate() {
            out[index] = execute_row(row, &mut regs_scratch, y, p, t, None);
        }
        Ok(())
    }

    pub(crate) fn rows(&self) -> usize {
        self.rows.len()
    }
}

pub(crate) struct CompiledJacobianRows {
    _module: JITModule,
    rows: Vec<RowPlan>,
    regs_scratch: RefCell<Vec<f64>>,
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
        if out.len() < self.rows.len() {
            return Err(CompileError::Input(format!(
                "output buffer too small: {} < {}",
                out.len(),
                self.rows.len()
            )));
        }
        let mut regs_scratch = self.regs_scratch.borrow_mut();
        for (index, row) in self.rows.iter().enumerate() {
            out[index] = execute_row(row, &mut regs_scratch, y, p, t, Some(v));
        }
        Ok(())
    }

    pub(crate) fn rows(&self) -> usize {
        self.rows.len()
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
    for (index, row) in rows.iter().enumerate() {
        emitter.compile_row(
            row,
            RowKind::Residual,
            &format!("rumoca_residual_row_{index}"),
        )?;
    }
    emitter
        .module
        .finalize_definitions()
        .map_err(to_backend_err)?;
    let rows = rows
        .iter()
        .map(|row| plan_row(row))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(CompiledResidualRows {
        _module: emitter.module,
        rows,
        regs_scratch: RefCell::new(Vec::new()),
    })
}

pub(crate) fn compile_jacobian_rows(
    rows: &[Vec<LinearOp>],
) -> Result<CompiledJacobianRows, CompileError> {
    let mut emitter = CraneliftEmitter::new()?;
    for (index, row) in rows.iter().enumerate() {
        emitter.compile_row(
            row,
            RowKind::JacobianV,
            &format!("rumoca_jacobian_row_{index}"),
        )?;
    }
    emitter
        .module
        .finalize_definitions()
        .map_err(to_backend_err)?;
    Ok(CompiledJacobianRows {
        _module: emitter.module,
        rows: rows
            .iter()
            .map(|row| plan_row(row))
            .collect::<Result<_, _>>()?,
        regs_scratch: RefCell::new(Vec::new()),
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
    ) -> Result<(), CompileError> {
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
        Ok(())
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
            let is_true = fb.ins().fcmp(FloatCC::NotEqual, x, zero);
            let is_false = fb.ins().bnot(is_true);
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
        CompareOp::Eq => {
            let diff = fb.ins().fsub(lhs, rhs);
            let abs = fb.ins().fabs(diff);
            let eps = fb.ins().f64const(f64::EPSILON);
            fb.ins().fcmp(FloatCC::LessThan, abs, eps)
        }
        CompareOp::Ne => {
            let diff = fb.ins().fsub(lhs, rhs);
            let abs = fb.ins().fabs(diff);
            let eps = fb.ins().f64const(f64::EPSILON);
            fb.ins().fcmp(FloatCC::GreaterThanOrEqual, abs, eps)
        }
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

fn plan_row(row: &[LinearOp]) -> Result<RowPlan, CompileError> {
    let mut reg_count = 0usize;
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
        }));
    }

    Ok(RowPlan::General(GeneralRowPlan {
        ops: body.to_vec().into_boxed_slice(),
        reg_count,
        output_src,
    }))
}

fn is_simple_linear_op(op: LinearOp) -> bool {
    !matches!(
        op,
        LinearOp::LoadSeed { .. }
            | LinearOp::TableBounds { .. }
            | LinearOp::TableLookup { .. }
            | LinearOp::TableLookupSlope { .. }
            | LinearOp::TableNextEvent { .. }
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
        | LinearOp::TableBounds { .. }
        | LinearOp::TableLookup { .. }
        | LinearOp::TableLookupSlope { .. }
        | LinearOp::TableNextEvent { .. }
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

#[inline(always)]
fn execute_row(
    row: &RowPlan,
    regs_scratch: &mut Vec<f64>,
    y: &[f64],
    p: &[f64],
    t: f64,
    seed: Option<&[f64]>,
) -> f64 {
    match row {
        RowPlan::Simple(row) => execute_simple_row(row, regs_scratch, y, p, t),
        RowPlan::General(row) => execute_general_row(row, regs_scratch, y, p, t, seed),
    }
}

#[inline(always)]
fn validate_output_len(out: &[f64], row_count: usize) -> Result<(), CompileError> {
    if out.len() >= row_count {
        return Ok(());
    }
    Err(CompileError::Input(format!(
        "output buffer too small: {} < {}",
        out.len(),
        row_count
    )))
}

#[inline(always)]
fn execute_simple_row(
    row: &SimpleRowPlan,
    regs_scratch: &mut Vec<f64>,
    y: &[f64],
    p: &[f64],
    t: f64,
) -> f64 {
    let regs = runtime_reg_slice(regs_scratch, row.reg_count);
    for op in row.ops.iter().copied() {
        match op {
            SimpleOp::Const { dst, value } => set_reg_value(regs, dst as usize, value),
            SimpleOp::LoadTime { dst } => set_reg_value(regs, dst as usize, t),
            SimpleOp::LoadY { dst, index } => {
                set_reg_value(regs, dst as usize, *y.get(index as usize).unwrap_or(&0.0))
            }
            SimpleOp::LoadP { dst, index } => {
                set_reg_value(regs, dst as usize, *p.get(index as usize).unwrap_or(&0.0))
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
    read_reg_value(regs, row.output_src)
}

#[inline(always)]
fn execute_general_row(
    row: &GeneralRowPlan,
    regs_scratch: &mut Vec<f64>,
    y: &[f64],
    p: &[f64],
    t: f64,
    seed: Option<&[f64]>,
) -> f64 {
    let regs = runtime_reg_slice(regs_scratch, row.reg_count);
    for op in row.ops.iter().copied() {
        match op {
            LinearOp::Const { dst, value } => set_reg_value(regs, dst as usize, value),
            LinearOp::LoadTime { dst } => set_reg_value(regs, dst as usize, t),
            LinearOp::LoadY { dst, index } => {
                set_reg_value(regs, dst as usize, *y.get(index).unwrap_or(&0.0))
            }
            LinearOp::LoadP { dst, index } => {
                set_reg_value(regs, dst as usize, *p.get(index).unwrap_or(&0.0))
            }
            LinearOp::LoadSeed { dst, index } => {
                let value = seed
                    .and_then(|values| values.get(index))
                    .copied()
                    .unwrap_or(0.0);
                set_reg_value(regs, dst as usize, value);
            }
            LinearOp::TableBounds { dst, table_id, max } => {
                let table_id = read_reg_value(regs, table_id as usize);
                set_reg_value(regs, dst as usize, eval_table_bound_value(table_id, max));
            }
            LinearOp::TableLookup {
                dst,
                table_id,
                column,
                input,
            } => {
                let table_id = read_reg_value(regs, table_id as usize);
                let column = read_reg_value(regs, column as usize);
                let input = read_reg_value(regs, input as usize);
                set_reg_value(
                    regs,
                    dst as usize,
                    eval_table_lookup_value(table_id, column, input),
                );
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
                set_reg_value(
                    regs,
                    dst as usize,
                    eval_table_lookup_slope_value(table_id, column, input),
                );
            }
            LinearOp::TableNextEvent {
                dst,
                table_id,
                time,
            } => {
                let table_id = read_reg_value(regs, table_id as usize);
                let time = read_reg_value(regs, time as usize);
                set_reg_value(
                    regs,
                    dst as usize,
                    eval_time_table_next_event_value(table_id, time),
                );
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
    }
    read_reg_value(regs, row.output_src)
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
    let value = match op {
        CompareOp::Lt => lhs < rhs,
        CompareOp::Le => lhs <= rhs,
        CompareOp::Gt => lhs > rhs,
        CompareOp::Ge => lhs >= rhs,
        CompareOp::Eq => (lhs - rhs).abs() < f64::EPSILON,
        CompareOp::Ne => (lhs - rhs).abs() >= f64::EPSILON,
    };
    if value { 1.0 } else { 0.0 }
}

fn to_backend_err<E: std::fmt::Display>(err: E) -> CompileError {
    CompileError::Backend(err.to_string())
}

fn register_math_symbols(builder: &mut JITBuilder) {
    builder.symbol("rumoca_host_sin", rumoca_host_sin as *const u8);
    builder.symbol("rumoca_host_cos", rumoca_host_cos as *const u8);
    builder.symbol("rumoca_host_tan", rumoca_host_tan as *const u8);
    builder.symbol("rumoca_host_asin", rumoca_host_asin as *const u8);
    builder.symbol("rumoca_host_acos", rumoca_host_acos as *const u8);
    builder.symbol("rumoca_host_atan", rumoca_host_atan as *const u8);
    builder.symbol("rumoca_host_atan2", rumoca_host_atan2 as *const u8);
    builder.symbol("rumoca_host_sinh", rumoca_host_sinh as *const u8);
    builder.symbol("rumoca_host_cosh", rumoca_host_cosh as *const u8);
    builder.symbol("rumoca_host_tanh", rumoca_host_tanh as *const u8);
    builder.symbol("rumoca_host_exp", rumoca_host_exp as *const u8);
    builder.symbol("rumoca_host_log", rumoca_host_log as *const u8);
    builder.symbol("rumoca_host_log10", rumoca_host_log10 as *const u8);
    builder.symbol("rumoca_host_floor", rumoca_host_floor as *const u8);
    builder.symbol("rumoca_host_ceil", rumoca_host_ceil as *const u8);
    builder.symbol("rumoca_host_trunc", rumoca_host_trunc as *const u8);
    builder.symbol("rumoca_host_powf", rumoca_host_powf as *const u8);
    builder.symbol(
        "rumoca_host_table_bounds_min",
        rumoca_host_table_bounds_min as *const u8,
    );
    builder.symbol(
        "rumoca_host_table_bounds_max",
        rumoca_host_table_bounds_max as *const u8,
    );
    builder.symbol(
        "rumoca_host_table_lookup",
        rumoca_host_table_lookup as *const u8,
    );
    builder.symbol(
        "rumoca_host_table_lookup_slope",
        rumoca_host_table_lookup_slope as *const u8,
    );
    builder.symbol(
        "rumoca_host_table_next_event",
        rumoca_host_table_next_event as *const u8,
    );
}

extern "C" fn rumoca_host_sin(x: f64) -> f64 {
    x.sin()
}
extern "C" fn rumoca_host_cos(x: f64) -> f64 {
    x.cos()
}
extern "C" fn rumoca_host_tan(x: f64) -> f64 {
    x.tan()
}
extern "C" fn rumoca_host_asin(x: f64) -> f64 {
    x.asin()
}
extern "C" fn rumoca_host_acos(x: f64) -> f64 {
    x.acos()
}
extern "C" fn rumoca_host_atan(x: f64) -> f64 {
    x.atan()
}
extern "C" fn rumoca_host_atan2(y: f64, x: f64) -> f64 {
    y.atan2(x)
}
extern "C" fn rumoca_host_sinh(x: f64) -> f64 {
    x.sinh()
}
extern "C" fn rumoca_host_cosh(x: f64) -> f64 {
    x.cosh()
}
extern "C" fn rumoca_host_tanh(x: f64) -> f64 {
    x.tanh()
}
extern "C" fn rumoca_host_exp(x: f64) -> f64 {
    x.exp()
}
extern "C" fn rumoca_host_log(x: f64) -> f64 {
    x.ln()
}
extern "C" fn rumoca_host_log10(x: f64) -> f64 {
    x.log10()
}
extern "C" fn rumoca_host_floor(x: f64) -> f64 {
    x.floor()
}
extern "C" fn rumoca_host_ceil(x: f64) -> f64 {
    x.ceil()
}
extern "C" fn rumoca_host_trunc(x: f64) -> f64 {
    x.trunc()
}
extern "C" fn rumoca_host_powf(x: f64, y: f64) -> f64 {
    x.powf(y)
}

extern "C" fn rumoca_host_table_bounds_min(table_id: f64) -> f64 {
    eval_table_bound_value(table_id, false)
}

extern "C" fn rumoca_host_table_bounds_max(table_id: f64) -> f64 {
    eval_table_bound_value(table_id, true)
}

extern "C" fn rumoca_host_table_lookup(table_id: f64, column: f64, input: f64) -> f64 {
    eval_table_lookup_value(table_id, column, input)
}

extern "C" fn rumoca_host_table_lookup_slope(table_id: f64, column: f64, input: f64) -> f64 {
    eval_table_lookup_slope_value(table_id, column, input)
}

extern "C" fn rumoca_host_table_next_event(table_id: f64, time: f64) -> f64 {
    eval_time_table_next_event_value(table_id, time)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn plan_row_uses_simple_runtime_plan_for_plain_residual_rows() {
        let row = vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ];

        let plan = plan_row(&row).expect("simple plan");
        assert!(matches!(plan, RowPlan::Simple(_)));
    }

    #[test]
    fn plan_row_keeps_seed_rows_on_general_runtime_plan() {
        let row = vec![
            LinearOp::LoadSeed { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ];

        let plan = plan_row(&row).expect("general plan");
        assert!(matches!(plan, RowPlan::General(_)));
    }
}
