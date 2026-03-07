use super::CompileError;
use crate::compiled::linear_op::{BinaryOp, CompareOp, LinearOp, UnaryOp};
use cranelift_codegen::ir::condcodes::FloatCC;
use cranelift_codegen::ir::{AbiParam, InstBuilder, MemFlags, types};
use cranelift_codegen::isa::CallConv;
use cranelift_codegen::settings;
use cranelift_codegen::verify_function;
use cranelift_frontend::{FunctionBuilder, FunctionBuilderContext};
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{FuncId, Linkage, Module};
use std::collections::HashMap;

pub(crate) struct CompiledResidualRows {
    _module: JITModule,
    rows: Vec<Vec<LinearOp>>,
}

impl CompiledResidualRows {
    pub(crate) fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), CompileError> {
        if out.len() < self.rows.len() {
            return Err(CompileError::Input(format!(
                "output buffer too small: {} < {}",
                out.len(),
                self.rows.len()
            )));
        }
        for (index, row) in self.rows.iter().enumerate() {
            out[index] = execute_row(row, y, p, t, None)?;
        }
        Ok(())
    }

    pub(crate) fn rows(&self) -> usize {
        self.rows.len()
    }
}

pub(crate) struct CompiledJacobianRows {
    _module: JITModule,
    rows: Vec<Vec<LinearOp>>,
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
        for (index, row) in self.rows.iter().enumerate() {
            out[index] = execute_row(row, y, p, t, Some(v))?;
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
    Ok(CompiledResidualRows {
        _module: emitter.module,
        rows: rows.to_vec(),
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
        rows: rows.to_vec(),
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
        signature.call_conv = CallConv::SystemV;
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
            LinearOp::LoadY { dst, index } => {
                let value = load_f64(self.fb, self.flags, self.y_ptr, index)?;
                self.insert(dst, value)
            }
            LinearOp::LoadP { dst, index } => {
                let value = load_f64(self.fb, self.flags, self.p_ptr, index)?;
                self.insert(dst, value)
            }
            LinearOp::LoadSeed { dst, index } => {
                let base = self.v_ptr.ok_or_else(|| {
                    CompileError::Backend("LoadSeed in row without seed input".to_string())
                })?;
                let value = load_f64(self.fb, self.flags, base, index)?;
                self.insert(dst, value)
            }
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
            } => {
                let cond_value = lookup_reg(self.regs, cond)?;
                let t = lookup_reg(self.regs, if_true)?;
                let f = lookup_reg(self.regs, if_false)?;
                let zero = self.fb.ins().f64const(0.0);
                let is_true = self.fb.ins().fcmp(FloatCC::NotEqual, cond_value, zero);
                let value = self.fb.ins().select(is_true, t, f);
                self.insert(dst, value)
            }
            LinearOp::StoreOutput { src } => Ok(Some(lookup_reg(self.regs, src)?)),
        }
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
        sig.call_conv = CallConv::SystemV;
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
        sig.call_conv = CallConv::SystemV;
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

fn execute_row(
    row: &[LinearOp],
    y: &[f64],
    p: &[f64],
    t: f64,
    seed: Option<&[f64]>,
) -> Result<f64, CompileError> {
    let mut regs: Vec<f64> = Vec::new();
    let mut out = 0.0;

    for op in row {
        match *op {
            LinearOp::Const { dst, value } => set_reg_value(&mut regs, dst, value),
            LinearOp::LoadTime { dst } => set_reg_value(&mut regs, dst, t),
            LinearOp::LoadY { dst, index } => {
                set_reg_value(&mut regs, dst, *y.get(index).unwrap_or(&0.0))
            }
            LinearOp::LoadP { dst, index } => {
                set_reg_value(&mut regs, dst, *p.get(index).unwrap_or(&0.0))
            }
            LinearOp::LoadSeed { dst, index } => {
                let v = seed.and_then(|s| s.get(index)).copied().unwrap_or(0.0);
                set_reg_value(&mut regs, dst, v);
            }
            LinearOp::Unary { dst, op, arg } => {
                let x = read_reg_value(&regs, arg)?;
                set_reg_value(&mut regs, dst, apply_unary(op, x));
            }
            LinearOp::Binary { dst, op, lhs, rhs } => {
                let l = read_reg_value(&regs, lhs)?;
                let r = read_reg_value(&regs, rhs)?;
                set_reg_value(&mut regs, dst, apply_binary(op, l, r));
            }
            LinearOp::Compare { dst, op, lhs, rhs } => {
                let l = read_reg_value(&regs, lhs)?;
                let r = read_reg_value(&regs, rhs)?;
                set_reg_value(&mut regs, dst, apply_compare(op, l, r));
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                let c = read_reg_value(&regs, cond)?;
                let t_val = read_reg_value(&regs, if_true)?;
                let f_val = read_reg_value(&regs, if_false)?;
                set_reg_value(&mut regs, dst, if c != 0.0 { t_val } else { f_val });
            }
            LinearOp::StoreOutput { src } => {
                out = read_reg_value(&regs, src)?;
            }
        }
    }

    Ok(out)
}

fn set_reg_value(regs: &mut Vec<f64>, reg: u32, value: f64) {
    let idx = reg as usize;
    if idx >= regs.len() {
        regs.resize(idx + 1, 0.0);
    }
    regs[idx] = value;
}

fn read_reg_value(regs: &[f64], reg: u32) -> Result<f64, CompileError> {
    regs.get(reg as usize)
        .copied()
        .ok_or_else(|| CompileError::Backend(format!("missing source register r{reg}")))
}

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
