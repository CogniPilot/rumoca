//! Private bounded register storage for compact native assignment programs.

mod arithmetic;
pub(in crate::emit) mod layouts;
pub(in crate::emit) mod register_layout;
mod storage;

pub(in crate::emit) use storage::ArenaStorage;

use super::*;
use wasm_encoder::MemorySection;

#[derive(Clone)]
pub(super) struct ArenaPlan {
    registers: usize,
    layouts: std::rc::Rc<[IssuedRegisterLayout]>,
    pub(in crate::emit) root_registers: usize,
    storage: ArenaStorage,
    pub(super) counter: u32,
    pub(super) inner_counter: u32,
}

struct IssuedRegisterLayout {
    program: std::rc::Rc<[LinearOp]>,
    layout: std::rc::Rc<register_layout::RegisterLayout>,
}

impl ArenaPlan {
    pub(super) fn new(rows: &[Vec<LinearOp>], rank: usize) -> Result<Option<Self>, String> {
        if !rows.iter().flatten().any(needs_arena) {
            return Ok(None);
        }
        Self::required(rows, rank).map(Some)
    }

    pub(super) fn required(rows: &[Vec<LinearOp>], rank: usize) -> Result<Self, String> {
        Self::required_with_policy(
            rows,
            rank,
            crate::CheckedNativeWasmCapacityPolicy::default(),
        )
    }

    pub(super) fn required_with_policy(
        rows: &[Vec<LinearOp>],
        rank: usize,
        policy: crate::CheckedNativeWasmCapacityPolicy,
    ) -> Result<Self, String> {
        conditional::visit_operations(rows, validate_profile)?;
        let layouts = rows
            .iter()
            .map(|program| {
                Ok(IssuedRegisterLayout {
                    program: program.clone().into(),
                    layout: std::rc::Rc::new(register_layout::RegisterLayout::derive(program)?),
                })
            })
            .collect::<Result<Vec<_>, String>>()?;
        let root_registers = layouts
            .iter()
            .map(|issued| issued.layout.extent())
            .max()
            .unwrap_or(0);
        let registers = conditional::workspace::register_extent(rows, root_registers)?;
        let bytes = registers
            .checked_mul(8)
            .ok_or("native register size overflow")?;
        policy.require_private_bytes(bytes)?;
        let counter = u32::try_from(rank)
            .ok()
            .and_then(|rank| (LOCAL_BASE + 3).checked_add(rank))
            .ok_or("native arena loop rank overflow")?;
        let inner_counter = counter
            .checked_add(1)
            .ok_or("native arena local overflow")?;
        inner_counter
            .checked_add(1)
            .ok_or("native arena local overflow")?;
        Ok(Self {
            layouts: layouts.into(),
            registers,
            root_registers,
            storage: ArenaStorage::Defined,
            counter,
            inner_counter,
        })
    }

    fn root_program(&self, ordinal: usize) -> Result<&IssuedRegisterLayout, String> {
        self.layouts
            .get(ordinal)
            .ok_or_else(|| "native root program is outside its issued placement".to_owned())
    }

    pub(super) fn locals(&self) -> Vec<(u32, ValType)> {
        vec![
            (2, ValType::F64),
            (self.inner_counter - LOCAL_BASE - 1, ValType::I32),
        ]
    }

    pub(super) fn add_memory(&self, module: &mut Module) {
        if self.storage == ArenaStorage::Pooled {
            return;
        }
        let pages = (self.registers * 8).div_ceil(65536).max(1) as u64;
        let mut memory = MemorySection::new();
        memory.memory(MemoryType {
            minimum: pages,
            maximum: Some(pages),
            memory64: false,
            shared: false,
            page_size_log2: None,
        });
        module.section(&memory);
    }
}

/// A range store of at most this many values is emitted as one store per value.
/// A longer one is a loop over the arena, so a range that comes from a compact
/// tensor never expands the emitted code by its extent.
const MAX_UNROLLED_RANGE_STORE: usize = 64;

fn needs_arena(op: &LinearOp) -> bool {
    match op {
        LinearOp::TensorLoad { count, lanes, .. } => {
            count.saturating_mul(*lanes) > tensor::MAX_PACKED_VALUES
        }
        LinearOp::StoreOutputRange { count, .. } => *count > MAX_UNROLLED_RANGE_STORE,
        LinearOp::LoadIndexedRegister { .. }
        | LinearOp::MatrixMultiply { .. }
        | LinearOp::FunctionConditional { .. }
        | LinearOp::PureCall { .. }
        | LinearOp::TensorBinary { .. }
        | LinearOp::TensorCross { .. }
        | LinearOp::TensorTranspose { .. }
        | LinearOp::TensorConcatenate { .. }
        | LinearOp::TensorFill { .. }
        | LinearOp::TensorIdentity { .. } => true,
        _ => false,
    }
}

pub(super) fn is_tensor(op: &LinearOp) -> bool {
    matches!(
        op,
        LinearOp::TensorLoad { .. }
            | LinearOp::MatrixMultiply { .. }
            | LinearOp::TensorBinary { .. }
            | LinearOp::TensorCross { .. }
            | LinearOp::TensorTranspose { .. }
            | LinearOp::TensorConcatenate { .. }
            | LinearOp::TensorFill { .. }
            | LinearOp::TensorIdentity { .. }
    )
}

fn validate_profile(op: &LinearOp) -> Result<(), String> {
    let lanes = match op {
        LinearOp::TensorLoad {
            lanes, seed_start, ..
        } => {
            if seed_start.is_some() {
                return Err("native private register arena does not support seeds".into());
            }
            *lanes
        }
        LinearOp::MatrixMultiply { lanes, .. }
        | LinearOp::TensorBinary { lanes, .. }
        | LinearOp::TensorCross { lanes, .. }
        | LinearOp::TensorTranspose { lanes, .. }
        | LinearOp::TensorConcatenate { lanes, .. }
        | LinearOp::TensorFill { lanes, .. }
        | LinearOp::TensorIdentity { lanes, .. } => *lanes,
        _ => return Ok(()),
    };
    if lanes != 1 {
        return Err("native private register arena requires one primal lane".into());
    }
    Ok(())
}

pub(super) fn memarg() -> MemArg {
    MemArg {
        offset: 0,
        align: 3,
        memory_index: 1,
    }
}

impl BodyEmitter<'_> {
    pub(in crate::emit) fn bind_root_program(
        &mut self,
    ) -> Result<Option<std::rc::Rc<[LinearOp]>>, String> {
        let Some(arena) = &self.arena else {
            return Ok(None);
        };
        let issued = arena.root_program(self.root_program_cursor)?;
        self.root_register_layout = Some(std::rc::Rc::clone(&issued.layout));
        self.root_program_cursor = self
            .root_program_cursor
            .checked_add(1)
            .ok_or("native root program ordinal overflows")?;
        Ok(Some(std::rc::Rc::clone(&issued.program)))
    }

    pub(in crate::emit) fn require_complete_root_programs(&self) -> Result<(), String> {
        if let Some(arena) = &self.arena
            && self.root_program_cursor != arena.layouts.len()
        {
            return Err("native emission did not consume its complete issued programs".into());
        }
        Ok(())
    }

    pub(in crate::emit) fn physical_arena_register(&self, register: Reg) -> Result<Reg, String> {
        let relative = if self.conditional_region.is_some() {
            register
        } else {
            self.root_register_layout
                .as_ref()
                .ok_or("native root register layout is unavailable")?
                .physical(register)?
        };
        relative
            .checked_add(self.register_base)
            .ok_or_else(|| "native frame offset overflow".into())
    }

    pub(super) fn push_arena_address(
        &mut self,
        start: Reg,
        counter: Option<u32>,
        stride: usize,
    ) -> Result<(), String> {
        let start = self.physical_arena_register(start)?;
        self.push_absolute_arena_address(start, counter, stride)
    }

    pub(in crate::emit) fn push_absolute_arena_address(
        &mut self,
        start: Reg,
        counter: Option<u32>,
        stride: usize,
    ) -> Result<(), String> {
        let offset = start
            .checked_mul(8)
            .ok_or("native register address overflow")?;
        let stride = stride
            .checked_mul(8)
            .and_then(|n| u32::try_from(n).ok())
            .ok_or("native register stride overflow")?;
        self.push(Instruction::I32Const(offset as i32));
        if let Some(counter) = counter {
            self.push(Instruction::LocalGet(counter));
            self.push(Instruction::I32Const(stride as i32));
            self.push(Instruction::I32Mul);
            self.push(Instruction::I32Add);
        }
        self.relocate_arena_address()?;
        Ok(())
    }

    fn push_arena_index(&mut self, start: Reg) -> Result<(), String> {
        // A checked runtime element offset is on the stack.
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        let offset = self
            .physical_arena_register(start)?
            .checked_mul(8)
            .ok_or("native register address overflow")?;
        self.push(Instruction::I32Const(offset as i32));
        self.push(Instruction::I32Add);
        self.relocate_arena_address()?;
        Ok(())
    }

    pub(in crate::emit) fn arena_plan(&self) -> Result<ArenaPlan, String> {
        self.arena
            .clone()
            .ok_or_else(|| "native tensor operation requires private register storage".into())
    }

    pub(super) fn emit_arena_range(
        &mut self,
        start: Reg,
        count: usize,
        stride: usize,
        output: usize,
    ) -> Result<(), String> {
        let arena = self.arena_plan()?;
        let bytes = output
            .checked_mul(8)
            .and_then(|n| u32::try_from(n).ok())
            .ok_or("native output address overflow")?;
        self.loop_start(arena.counter, count)?;
        self.push(Instruction::LocalGet(OUT_PTR_PARAM));
        self.push(Instruction::LocalGet(arena.counter));
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
        self.push_arena_address(start, Some(arena.counter), stride)?;
        self.push(Instruction::F64Load(memarg()));
        self.push(Instruction::F64Store(MemArg {
            offset: u64::from(bytes),
            align: 3,
            memory_index: 0,
        }));
        self.loop_end(arena.counter);
        Ok(())
    }
}
