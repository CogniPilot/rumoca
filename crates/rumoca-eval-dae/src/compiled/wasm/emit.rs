//! WASM emitter for residual linear-op rows.

use crate::compiled::linear_op::{BinaryOp, CompareOp, LinearOp, Reg, UnaryOp};
use std::collections::{BTreeMap, BTreeSet};
use wasm_encoder::BlockType;
use wasm_encoder::CodeSection;
use wasm_encoder::EntityType;
use wasm_encoder::ExportKind;
use wasm_encoder::ExportSection;
use wasm_encoder::Function;
use wasm_encoder::FunctionSection;
use wasm_encoder::ImportSection;
use wasm_encoder::Instruction;
use wasm_encoder::MemArg;
use wasm_encoder::MemoryType;
use wasm_encoder::Module;
use wasm_encoder::TypeSection;
use wasm_encoder::ValType;

const Y_PTR_PARAM: u32 = 0;
const P_PTR_PARAM: u32 = 1;
const TIME_PARAM: u32 = 2;
const SEED_PTR_PARAM: u32 = 3;
const OUT_PTR_PARAM: u32 = 4;
const LOCAL_BASE: u32 = 5;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum MathImport {
    Abs,
    Sign,
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
    Pow,
    Atan2,
}

impl MathImport {
    fn symbol(self) -> &'static str {
        match self {
            Self::Abs => "abs",
            Self::Sign => "sign",
            Self::Sin => "sin",
            Self::Cos => "cos",
            Self::Tan => "tan",
            Self::Asin => "asin",
            Self::Acos => "acos",
            Self::Atan => "atan",
            Self::Sinh => "sinh",
            Self::Cosh => "cosh",
            Self::Tanh => "tanh",
            Self::Exp => "exp",
            Self::Log => "log",
            Self::Log10 => "log10",
            Self::Pow => "pow",
            Self::Atan2 => "atan2",
        }
    }

    fn is_binary(self) -> bool {
        matches!(self, Self::Pow | Self::Atan2)
    }
}

#[derive(Debug, Clone)]
struct ImportCatalog {
    function_indices: BTreeMap<MathImport, u32>,
    eval_function_index: u32,
}

pub(super) fn emit_residual_module(rows: &[Vec<LinearOp>]) -> Result<Vec<u8>, String> {
    let imports = collect_imports(rows);
    let max_registers = max_registers(rows);

    let mut module = Module::new();
    let type_ids = add_type_section(&mut module);
    let import_catalog = add_import_section(&mut module, &imports, &type_ids);
    add_function_section(&mut module, type_ids.eval_type);
    add_export_section(&mut module, import_catalog.eval_function_index);
    add_code_section(&mut module, rows, max_registers, &import_catalog)?;
    Ok(module.finish())
}

#[derive(Debug, Clone, Copy)]
struct TypeIds {
    eval_type: u32,
    unary_type: u32,
    binary_type: u32,
}

fn add_type_section(module: &mut Module) -> TypeIds {
    let mut types = TypeSection::new();
    let eval_type = types.len();
    types.ty().function(
        [
            ValType::I32,
            ValType::I32,
            ValType::F64,
            ValType::I32,
            ValType::I32,
        ],
        [],
    );
    let unary_type = types.len();
    types.ty().function([ValType::F64], [ValType::F64]);
    let binary_type = types.len();
    types
        .ty()
        .function([ValType::F64, ValType::F64], [ValType::F64]);
    module.section(&types);
    TypeIds {
        eval_type,
        unary_type,
        binary_type,
    }
}

fn add_import_section(
    module: &mut Module,
    imports: &[MathImport],
    type_ids: &TypeIds,
) -> ImportCatalog {
    let mut import_section = ImportSection::new();
    import_section.import(
        "env",
        "memory",
        EntityType::Memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        }),
    );
    let mut function_indices = BTreeMap::new();
    for import in imports {
        let type_id = if import.is_binary() {
            type_ids.binary_type
        } else {
            type_ids.unary_type
        };
        let function_index = function_indices.len() as u32;
        import_section.import("env", import.symbol(), EntityType::Function(type_id));
        function_indices.insert(*import, function_index);
    }
    module.section(&import_section);
    ImportCatalog {
        eval_function_index: function_indices.len() as u32,
        function_indices,
    }
}

fn add_function_section(module: &mut Module, eval_type: u32) {
    let mut functions = FunctionSection::new();
    functions.function(eval_type);
    module.section(&functions);
}

fn add_export_section(module: &mut Module, eval_function_index: u32) {
    let mut exports = ExportSection::new();
    exports.export("memory", ExportKind::Memory, 0);
    exports.export("eval_residual", ExportKind::Func, eval_function_index);
    module.section(&exports);
}

fn add_code_section(
    module: &mut Module,
    rows: &[Vec<LinearOp>],
    max_registers: usize,
    imports: &ImportCatalog,
) -> Result<(), String> {
    let mut code = CodeSection::new();
    let mut function = Function::new(locals_for_register_count(max_registers));
    let mut emitter = BodyEmitter::new(imports, &mut function);
    emitter.emit_rows(rows)?;
    function.instruction(&Instruction::End);
    code.function(&function);
    module.section(&code);
    Ok(())
}

fn locals_for_register_count(max_registers: usize) -> Vec<(u32, ValType)> {
    if max_registers == 0 {
        Vec::new()
    } else {
        vec![(max_registers as u32, ValType::F64)]
    }
}

fn collect_imports(rows: &[Vec<LinearOp>]) -> Vec<MathImport> {
    let mut imports = BTreeSet::new();
    for row in rows {
        for op in row {
            if let Some(import) = unary_import(op) {
                imports.insert(import);
            }
            if let Some(import) = binary_import(op) {
                imports.insert(import);
            }
        }
    }
    imports.into_iter().collect()
}

fn unary_import(op: &LinearOp) -> Option<MathImport> {
    match op {
        LinearOp::Unary {
            op: UnaryOp::Abs, ..
        } => Some(MathImport::Abs),
        LinearOp::Unary {
            op: UnaryOp::Sign, ..
        } => Some(MathImport::Sign),
        LinearOp::Unary {
            op: UnaryOp::Sin, ..
        } => Some(MathImport::Sin),
        LinearOp::Unary {
            op: UnaryOp::Cos, ..
        } => Some(MathImport::Cos),
        LinearOp::Unary {
            op: UnaryOp::Tan, ..
        } => Some(MathImport::Tan),
        LinearOp::Unary {
            op: UnaryOp::Asin, ..
        } => Some(MathImport::Asin),
        LinearOp::Unary {
            op: UnaryOp::Acos, ..
        } => Some(MathImport::Acos),
        LinearOp::Unary {
            op: UnaryOp::Atan, ..
        } => Some(MathImport::Atan),
        LinearOp::Unary {
            op: UnaryOp::Sinh, ..
        } => Some(MathImport::Sinh),
        LinearOp::Unary {
            op: UnaryOp::Cosh, ..
        } => Some(MathImport::Cosh),
        LinearOp::Unary {
            op: UnaryOp::Tanh, ..
        } => Some(MathImport::Tanh),
        LinearOp::Unary {
            op: UnaryOp::Exp, ..
        } => Some(MathImport::Exp),
        LinearOp::Unary {
            op: UnaryOp::Log, ..
        } => Some(MathImport::Log),
        LinearOp::Unary {
            op: UnaryOp::Log10, ..
        } => Some(MathImport::Log10),
        _ => None,
    }
}

fn binary_import(op: &LinearOp) -> Option<MathImport> {
    match op {
        LinearOp::Binary {
            op: BinaryOp::Pow, ..
        } => Some(MathImport::Pow),
        LinearOp::Binary {
            op: BinaryOp::Atan2,
            ..
        } => Some(MathImport::Atan2),
        _ => None,
    }
}

fn max_registers(rows: &[Vec<LinearOp>]) -> usize {
    rows.iter()
        .flat_map(|row| row.iter())
        .map(max_register_for_op)
        .max()
        .map_or(0, |reg| reg.saturating_add(1))
}

fn max_register_for_op(op: &LinearOp) -> usize {
    match *op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::LoadSeed { dst, .. } => dst as usize,
        LinearOp::Unary { dst, arg, .. } => (dst.max(arg)) as usize,
        LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } => {
            dst.max(lhs).max(rhs) as usize
        }
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => dst.max(cond).max(if_true).max(if_false) as usize,
        LinearOp::StoreOutput { src } => src as usize,
    }
}

struct BodyEmitter<'a> {
    imports: &'a ImportCatalog,
    function: &'a mut Function,
    next_output_slot: u64,
}

impl<'a> BodyEmitter<'a> {
    fn new(imports: &'a ImportCatalog, function: &'a mut Function) -> Self {
        Self {
            imports,
            function,
            next_output_slot: 0,
        }
    }

    fn emit_rows(&mut self, rows: &[Vec<LinearOp>]) -> Result<(), String> {
        for row in rows {
            for op in row {
                self.emit_op(*op)?;
            }
        }
        Ok(())
    }

    fn emit_op(&mut self, op: LinearOp) -> Result<(), String> {
        match op {
            LinearOp::Const { dst, value } => {
                self.push(Instruction::F64Const(value.into()));
                self.set_reg(dst);
            }
            LinearOp::LoadTime { dst } => {
                self.push(Instruction::LocalGet(TIME_PARAM));
                self.set_reg(dst);
            }
            LinearOp::LoadY { dst, index } => {
                self.push(Instruction::LocalGet(Y_PTR_PARAM));
                self.push(Instruction::F64Load(memarg_for_index(index)?));
                self.set_reg(dst);
            }
            LinearOp::LoadP { dst, index } => {
                self.push(Instruction::LocalGet(P_PTR_PARAM));
                self.push(Instruction::F64Load(memarg_for_index(index)?));
                self.set_reg(dst);
            }
            LinearOp::LoadSeed { dst, index } => {
                self.push(Instruction::LocalGet(SEED_PTR_PARAM));
                self.push(Instruction::F64Load(memarg_for_index(index)?));
                self.set_reg(dst);
            }
            LinearOp::Unary { dst, op, arg } => self.emit_unary(dst, op, arg)?,
            LinearOp::Binary { dst, op, lhs, rhs } => self.emit_binary(dst, op, lhs, rhs)?,
            LinearOp::Compare { dst, op, lhs, rhs } => self.emit_compare(dst, op, lhs, rhs),
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => self.emit_select(dst, cond, if_true, if_false),
            LinearOp::StoreOutput { src } => self.emit_store_output(src)?,
        }
        Ok(())
    }

    fn emit_unary(&mut self, dst: Reg, op: UnaryOp, arg: Reg) -> Result<(), String> {
        match op {
            UnaryOp::Not => {
                self.push_reg(arg);
                self.push(Instruction::F64Const(0.0f64.into()));
                self.push(Instruction::F64Eq);
                self.push(Instruction::F64ConvertI32S);
            }
            UnaryOp::Neg => {
                self.push_reg(arg);
                self.push(Instruction::F64Neg);
            }
            UnaryOp::Sqrt => {
                self.push_reg(arg);
                self.push(Instruction::F64Sqrt);
            }
            UnaryOp::Floor => {
                self.push_reg(arg);
                self.push(Instruction::F64Floor);
            }
            UnaryOp::Ceil => {
                self.push_reg(arg);
                self.push(Instruction::F64Ceil);
            }
            UnaryOp::Trunc => {
                self.push_reg(arg);
                self.push(Instruction::F64Trunc);
            }
            UnaryOp::Abs
            | UnaryOp::Sign
            | UnaryOp::Sin
            | UnaryOp::Cos
            | UnaryOp::Tan
            | UnaryOp::Asin
            | UnaryOp::Acos
            | UnaryOp::Atan
            | UnaryOp::Sinh
            | UnaryOp::Cosh
            | UnaryOp::Tanh
            | UnaryOp::Exp
            | UnaryOp::Log
            | UnaryOp::Log10 => {
                let import = unary_import(&LinearOp::Unary { dst, op, arg })
                    .ok_or_else(|| format!("unsupported unary op import mapping: {op:?}"))?;
                self.push_reg(arg);
                self.call_import(import)?;
            }
        }
        self.set_reg(dst);
        Ok(())
    }

    fn emit_binary(&mut self, dst: Reg, op: BinaryOp, lhs: Reg, rhs: Reg) -> Result<(), String> {
        match op {
            BinaryOp::Add => self.emit_arith2(lhs, rhs, Instruction::F64Add),
            BinaryOp::Sub => self.emit_arith2(lhs, rhs, Instruction::F64Sub),
            BinaryOp::Mul => self.emit_arith2(lhs, rhs, Instruction::F64Mul),
            BinaryOp::Div => self.emit_arith2(lhs, rhs, Instruction::F64Div),
            BinaryOp::Min => self.emit_arith2(lhs, rhs, Instruction::F64Min),
            BinaryOp::Max => self.emit_arith2(lhs, rhs, Instruction::F64Max),
            BinaryOp::And => self.emit_logic2(lhs, rhs, true),
            BinaryOp::Or => self.emit_logic2(lhs, rhs, false),
            BinaryOp::Pow | BinaryOp::Atan2 => {
                let import = binary_import(&LinearOp::Binary { dst, op, lhs, rhs })
                    .ok_or_else(|| format!("unsupported binary op import mapping: {op:?}"))?;
                self.push_reg(lhs);
                self.push_reg(rhs);
                self.call_import(import)?;
            }
        }
        self.set_reg(dst);
        Ok(())
    }

    fn emit_arith2(&mut self, lhs: Reg, rhs: Reg, op: Instruction<'static>) {
        self.push_reg(lhs);
        self.push_reg(rhs);
        self.push(op);
    }

    fn emit_logic2(&mut self, lhs: Reg, rhs: Reg, is_and: bool) {
        self.push_reg(lhs);
        self.push(Instruction::F64Const(0.0f64.into()));
        self.push(Instruction::F64Ne);
        self.push_reg(rhs);
        self.push(Instruction::F64Const(0.0f64.into()));
        self.push(Instruction::F64Ne);
        if is_and {
            self.push(Instruction::I32And);
        } else {
            self.push(Instruction::I32Or);
        }
        self.push(Instruction::F64ConvertI32S);
    }

    fn emit_compare(&mut self, dst: Reg, op: CompareOp, lhs: Reg, rhs: Reg) {
        self.push_reg(lhs);
        self.push_reg(rhs);
        self.push(match op {
            CompareOp::Lt => Instruction::F64Lt,
            CompareOp::Le => Instruction::F64Le,
            CompareOp::Gt => Instruction::F64Gt,
            CompareOp::Ge => Instruction::F64Ge,
            CompareOp::Eq => Instruction::F64Eq,
            CompareOp::Ne => Instruction::F64Ne,
        });
        self.push(Instruction::F64ConvertI32S);
        self.set_reg(dst);
    }

    fn emit_select(&mut self, dst: Reg, cond: Reg, if_true: Reg, if_false: Reg) {
        self.push_reg(cond);
        self.push(Instruction::F64Const(0.0f64.into()));
        self.push(Instruction::F64Ne);
        self.push(Instruction::If(BlockType::Result(ValType::F64)));
        self.push_reg(if_true);
        self.push(Instruction::Else);
        self.push_reg(if_false);
        self.push(Instruction::End);
        self.set_reg(dst);
    }

    fn emit_store_output(&mut self, src: Reg) -> Result<(), String> {
        self.push(Instruction::LocalGet(OUT_PTR_PARAM));
        self.push_reg(src);
        self.push(Instruction::F64Store(MemArg {
            offset: self.next_output_slot.saturating_mul(8),
            align: 3,
            memory_index: 0,
        }));
        self.next_output_slot = self.next_output_slot.saturating_add(1);
        if self.next_output_slot == u64::MAX {
            return Err("output slot overflow".to_string());
        }
        Ok(())
    }

    fn call_import(&mut self, import: MathImport) -> Result<(), String> {
        let index = self
            .imports
            .function_indices
            .get(&import)
            .copied()
            .ok_or_else(|| format!("missing imported function index: {}", import.symbol()))?;
        self.push(Instruction::Call(index));
        Ok(())
    }

    fn push(&mut self, instruction: Instruction<'_>) {
        self.function.instruction(&instruction);
    }

    fn push_reg(&mut self, reg: Reg) {
        self.push(Instruction::LocalGet(local_for_reg(reg)));
    }

    fn set_reg(&mut self, reg: Reg) {
        self.push(Instruction::LocalSet(local_for_reg(reg)));
    }
}

fn memarg_for_index(index: usize) -> Result<MemArg, String> {
    let offset = (index as u64)
        .checked_mul(8)
        .ok_or_else(|| "memory offset overflow".to_string())?;
    Ok(MemArg {
        offset,
        align: 3,
        memory_index: 0,
    })
}

fn local_for_reg(reg: Reg) -> u32 {
    LOCAL_BASE.saturating_add(reg)
}
