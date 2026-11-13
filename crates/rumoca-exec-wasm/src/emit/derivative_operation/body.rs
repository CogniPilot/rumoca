//! One-to-one printing of the constructor-issued memory32 control stream.
use super::*;
use rumoca_ir_solve::fmi::{FmiDerivativeControlInstruction as Op, FmiDerivativeHelper as Helper};

/// Function indices of the imported helpers an entry may call.
#[derive(Clone, Copy)]
pub(super) struct Helpers {
    pub(super) stat: Option<u32>,
    pub(super) dynamic: Option<u32>,
    pub(super) values: u32,
    pub(super) kinematic: Option<u32>,
}

pub(super) fn emit(instructions: &[Op], i32_locals: u32, helpers: Helpers) -> Function {
    let mut function = Function::new([(i32_locals, ValType::I32)]);
    for &op in instructions {
        let instruction = match op {
            Op::I32Const(n) => Instruction::I32Const(n),
            Op::I64Const(n) => Instruction::I64Const(n),
            Op::F64Const(bits) => Instruction::F64Const(f64::from_bits(bits).into()),
            Op::LocalGet(n) => Instruction::LocalGet(n),
            Op::LocalSet(n) => Instruction::LocalSet(n),
            Op::I32Add => Instruction::I32Add,
            Op::I32Mul => Instruction::I32Mul,
            Op::I32Eqz => Instruction::I32Eqz,
            Op::I32GeU => Instruction::I32GeU,
            Op::I32GtU => Instruction::I32GtU,
            Op::I32And => Instruction::I32And,
            Op::I64Add => Instruction::I64Add,
            Op::I64ExtendI32U => Instruction::I64ExtendI32U,
            Op::I64Shl => Instruction::I64Shl,
            Op::I64GtU => Instruction::I64GtU,
            Op::I64Ne => Instruction::I64Ne,
            Op::F64Abs => Instruction::F64Abs,
            Op::F64Le => Instruction::F64Le,
            Op::I32Store => Instruction::I32Store(mem(2)),
            Op::I32Load8U => Instruction::I32Load8U(mem(0)),
            Op::I64Load => Instruction::I64Load(mem(3)),
            Op::F64Load => Instruction::F64Load(mem(3)),
            Op::MemorySize => Instruction::MemorySize(0),
            Op::MemoryCopy => Instruction::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            },
            Op::MemoryFill => Instruction::MemoryFill(0),
            Op::Block => Instruction::Block(BlockType::Empty),
            Op::Loop => Instruction::Loop(BlockType::Empty),
            Op::If => Instruction::If(BlockType::Empty),
            Op::Else => Instruction::Else,
            Op::End => Instruction::End,
            Op::BrIf(n) => Instruction::BrIf(n),
            Op::Br(n) => Instruction::Br(n),
            Op::Return => Instruction::Return,
            Op::Call(helper) => Instruction::Call(match helper {
                Helper::Static => helpers.stat.expect("issued static helper"),
                Helper::Dynamic => helpers.dynamic.expect("issued dynamic helper"),
                Helper::Values => helpers.values,
                Helper::Kinematic => helpers.kinematic.expect("issued kinematic helper"),
            }),
        };
        function.instruction(&instruction);
    }
    function
}
fn mem(align: u32) -> MemArg {
    MemArg {
        offset: 0,
        align,
        memory_index: 0,
    }
}
