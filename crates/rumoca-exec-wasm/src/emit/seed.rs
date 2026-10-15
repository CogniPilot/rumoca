//! Original seed coordinates in the prepared private scratch suffix.
use super::*;

impl BodyEmitter<'_> {
    pub(super) fn emit_synthetic_index_slot(
        &mut self,
        index: Reg,
        count: usize,
    ) -> Result<(), String> {
        let last = i32::try_from(count.saturating_sub(1))
            .map_err(|_| "WASM synthetic index extent exceeds i32")?;
        // resolve_indexed_slot uses round(), saturating conversion and clamping.
        // Branch before conversion so NaN and both infinities retain that contract.
        self.push_reg(index)?;
        self.push(Instruction::F64Const(0.0.into()));
        self.push(Instruction::F64Ge);
        self.push(Instruction::If(BlockType::Result(ValType::I32)));
        self.push_reg(index)?;
        self.push(Instruction::F64Const(f64::from(last).into()));
        self.push(Instruction::F64Ge);
        self.push(Instruction::If(BlockType::Result(ValType::I32)));
        self.push(Instruction::I32Const(last));
        self.push(Instruction::Else);
        self.push_reg(index)?;
        self.push(Instruction::F64Floor);
        self.push(Instruction::I32TruncF64U);
        self.push_reg(index)?;
        self.push_reg(index)?;
        self.push(Instruction::F64Floor);
        self.push(Instruction::F64Sub);
        self.push(Instruction::F64Const(0.5.into()));
        self.push(Instruction::F64Ge);
        self.push(Instruction::I32Add);
        self.push(Instruction::End);
        self.push(Instruction::Else);
        self.push(Instruction::I32Const(0));
        self.push(Instruction::End);
        Ok(())
    }

    pub(super) fn seed_index(&self, index: usize) -> Result<usize, String> {
        index
            .checked_add(self.private_seed_offset.unwrap_or(0) as usize / 8)
            .ok_or_else(|| "WASM private seed address overflows".into())
    }

    pub(super) fn emit_seed_load(&mut self, dst: Reg, index: usize) -> Result<(), String> {
        self.emit_array_load(dst, self.seed_index(index)?, SEED_PTR_PARAM)
    }

    pub(super) fn emit_indexed_seed_load(
        &mut self,
        dst: Reg,
        base: usize,
        count: usize,
        index: Reg,
    ) -> Result<(), String> {
        self.emit_indexed_load(dst, self.seed_index(base)?, count, index, SEED_PTR_PARAM)
    }

    pub(super) fn seed_array_address(&mut self, start: usize, counter: u32) -> Result<(), String> {
        self.push(Instruction::LocalGet(SEED_PTR_PARAM));
        self.push(Instruction::I32Const(arena::layouts::cell_bytes(
            self.seed_index(start)?,
        )?));
        self.push(Instruction::I32Add);
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
        Ok(())
    }
}
