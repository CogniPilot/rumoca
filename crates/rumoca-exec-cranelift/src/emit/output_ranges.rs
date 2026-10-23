//! Preserve contiguous output transfers over the native register tape.

use super::*;

pub(super) enum TypedRangeDirection {
    Input,
    Output,
}

impl RowLowerCtx<'_, '_> {
    pub(super) fn pack_typed_call_inputs(
        &mut self,
        input_starts: &[u32],
        inputs: &[rumoca_ir_solve::SolveValueType],
        input_slot: StackSlot,
    ) -> Result<(), CompileError> {
        let mut input_cell = 0usize;
        for (start, value_type) in input_starts.iter().zip(inputs) {
            if self.copy_taped_real_cells(
                *start,
                value_type,
                (input_slot, input_cell),
                TypedRangeDirection::Input,
            )? {
                input_cell += value_type.scalar_count() as usize;
                continue;
            }
            for offset in 0..value_type.scalar_count() as usize {
                let register = checked_reg_offset(*start, offset, "typed pure-call input")?;
                let value = self.lookup(register)?;
                let value = encode_typed_cell(self.fb, value_type.element_type(), value);
                scratch::stack_store(
                    self.fb,
                    value,
                    input_slot,
                    stack_element_byte_offset(input_cell, "typed pure-call input")?,
                );
                input_cell += 1;
            }
        }
        Ok(())
    }

    pub(super) fn copy_taped_real_cells(
        &mut self,
        register: u32,
        value_type: &rumoca_ir_solve::SolveValueType,
        (slot, first_cell): (StackSlot, usize),
        direction: TypedRangeDirection,
    ) -> Result<bool, CompileError> {
        let Some(tape) = self.backing_regs_ptr else {
            return Ok(false);
        };
        if !matches!(
            value_type.element_type(),
            rumoca_ir_solve::SolveScalarType::Real {
                format: rumoca_ir_solve::SolveRealFormat::Binary64,
                ..
            }
        ) {
            return Ok(false);
        }
        let count = value_type.scalar_count() as usize;
        if count == 0 {
            return Ok(true);
        }
        let last = checked_reg_offset(register, count - 1, "typed range transfer")?;
        register_byte_offset(last)?;
        let register_offset = register_byte_offset(register)?;
        let registers = self.fb.ins().iadd_imm(tape, i64::from(register_offset));
        let cells = scratch::stack_addr(
            self.fb,
            types::I64,
            slot,
            stack_element_byte_offset(first_cell, "typed range transfer")?,
        );
        match direction {
            TypedRangeDirection::Input => self.copy_pointer_range(registers, cells, count)?,
            TypedRangeDirection::Output => self.copy_pointer_range(cells, registers, count)?,
        }
        Ok(true)
    }

    pub(super) fn copy_taped_output_range(
        &mut self,
        start: u32,
        count: usize,
        output: cranelift_codegen::ir::Value,
        output_index: i32,
    ) -> Result<Option<i32>, CompileError> {
        let Some(tape) = self.backing_regs_ptr else {
            return Ok(None);
        };
        if count == 0 {
            return Ok(Some(output_index));
        }
        let count_i32 = i32::try_from(count)
            .map_err(|_| CompileError::Backend("output range count exceeds i32".into()))?;
        let next = output_index
            .checked_add(count_i32)
            .filter(|next| (next - 1).checked_mul(8).is_some())
            .ok_or_else(|| CompileError::Backend("output range byte offset exceeds i32".into()))?;
        let source_offset = register_byte_offset(start)?;
        let last = checked_strided_register(start, count - 1, 1, "output range")?;
        register_byte_offset(last)?;
        let destination_offset = output_index
            .checked_mul(8)
            .ok_or_else(|| CompileError::Backend("output range byte offset exceeds i32".into()))?;
        let source = self.fb.ins().iadd_imm(tape, i64::from(source_offset));
        let destination = self
            .fb
            .ins()
            .iadd_imm(output, i64::from(destination_offset));
        self.copy_pointer_range(source, destination, count)?;
        Ok(Some(next))
    }
}
