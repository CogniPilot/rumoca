//! Compact tensor loads, replication and row-major permutations.

use super::*;
use rumoca_ir_solve::TensorInputKind;

impl BodyEmitter<'_> {
    pub(super) fn arena_load(&mut self, operation: LinearOp) -> Result<(), String> {
        let LinearOp::TensorLoad {
            dst_start,
            input,
            input_start,
            count,
            ..
        } = operation
        else {
            return Err("expected TensorLoad".into());
        };
        let pointer = match input {
            TensorInputKind::Y => Y_PTR_PARAM,
            TensorInputKind::P => P_PTR_PARAM,
        };
        let counter = self.arena_plan()?.counter;
        self.loop_start(counter, count)?;
        self.push_arena_address(dst_start, Some(counter), 1)?;
        self.push(Instruction::LocalGet(pointer));
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(8));
        self.push(Instruction::I32Mul);
        self.push(Instruction::I32Add);
        self.push(Instruction::F64Load(memarg_for_index(input_start)?));
        self.push(Instruction::F64Store(memarg()));
        self.loop_end(counter);
        Ok(())
    }

    pub(super) fn arena_fill(&mut self, operation: LinearOp) -> Result<(), String> {
        let LinearOp::TensorFill {
            dst_start,
            value_start,
            count,
            ..
        } = operation
        else {
            return Err("expected TensorFill".into());
        };
        let counter = self.arena_plan()?.counter;
        self.loop_start(counter, count)?;
        self.push_arena_address(dst_start, Some(counter), 1)?;
        self.push_reg(value_start)?;
        self.push(Instruction::F64Store(memarg()));
        self.loop_end(counter);
        Ok(())
    }

    pub(super) fn arena_identity(&mut self, operation: LinearOp) -> Result<(), String> {
        let LinearOp::TensorIdentity {
            dst_start, size, ..
        } = operation
        else {
            return Err("expected TensorIdentity".into());
        };
        let counter = self.arena_plan()?.counter;
        let count = size
            .checked_mul(size)
            .ok_or("native identity count overflow")?;
        let size = i32::try_from(size).map_err(|_| "native identity extent exceeds i32")?;
        self.loop_start(counter, count)?;
        self.push_arena_address(dst_start, Some(counter), 1)?;
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(size));
        self.push(Instruction::I32DivU);
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(size));
        self.push(Instruction::I32RemU);
        self.push(Instruction::I32Eq);
        self.push(Instruction::F64ConvertI32U);
        self.push(Instruction::F64Store(memarg()));
        self.loop_end(counter);
        Ok(())
    }

    pub(super) fn arena_transpose(&mut self, operation: LinearOp) -> Result<(), String> {
        let LinearOp::TensorTranspose {
            dst_start,
            src_start,
            rows,
            columns,
            element_width,
            ..
        } = operation
        else {
            return Err("expected TensorTranspose".into());
        };
        let count = rows
            .checked_mul(columns)
            .and_then(|n| n.checked_mul(element_width))
            .ok_or("native transpose count overflow")?;
        let row_width = columns
            .checked_mul(element_width)
            .and_then(|n| i32::try_from(n).ok())
            .ok_or("native transpose row width overflow")?;
        let rows = i32::try_from(rows).map_err(|_| "native transpose rows overflow")?;
        let width =
            i32::try_from(element_width).map_err(|_| "native transpose trailing width overflow")?;
        let counter = self.arena_plan()?.counter;
        self.loop_start(counter, count)?;
        self.push_arena_address(dst_start, Some(counter), 1)?;
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(width));
        self.push(Instruction::I32DivU);
        self.push(Instruction::I32Const(columns as i32));
        self.push(Instruction::I32RemU);
        self.push(Instruction::I32Const(rows));
        self.push(Instruction::I32Mul);
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(row_width));
        self.push(Instruction::I32DivU);
        self.push(Instruction::I32Add);
        self.push(Instruction::I32Const(width));
        self.push(Instruction::I32Mul);
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I32Const(width));
        self.push(Instruction::I32RemU);
        self.push(Instruction::I32Add);
        self.push_arena_index(src_start)?;
        self.push(Instruction::F64Load(memarg()));
        self.push(Instruction::F64Store(memarg()));
        self.loop_end(counter);
        Ok(())
    }

    pub(super) fn arena_concatenate(&mut self, operation: LinearOp) -> Result<(), String> {
        let LinearOp::TensorConcatenate {
            dst_start,
            sources,
            dimensions,
            axis,
            ..
        } = operation
        else {
            return Err("expected TensorConcatenate".into());
        };
        let inner = dimensions[axis + 1..]
            .iter()
            .try_fold(1usize, |n, d| n.checked_mul(*d as usize))
            .ok_or("native concatenate inner width overflow")?;
        let result_block = (dimensions[axis] as usize)
            .checked_mul(inner)
            .and_then(|n| i32::try_from(n).ok())
            .ok_or("native concatenate result width overflow")?;
        let counter = self.arena_plan()?.counter;
        let mut axis_offset = 0usize;
        for source in &sources {
            let source_block = (source.dimensions[axis] as usize)
                .checked_mul(inner)
                .and_then(|n| i32::try_from(n).ok())
                .ok_or("native concatenate source width overflow")?;
            let count = source
                .dimensions
                .iter()
                .try_fold(1usize, |n, d| n.checked_mul(*d as usize))
                .ok_or("native concatenate count overflow")?;
            let offset = axis_offset
                .checked_mul(inner)
                .and_then(|n| i32::try_from(n).ok())
                .ok_or("native concatenate offset overflow")?;
            self.loop_start(counter, count)?;
            self.push(Instruction::LocalGet(counter));
            self.push(Instruction::I32Const(source_block));
            self.push(Instruction::I32DivU);
            self.push(Instruction::I32Const(result_block));
            self.push(Instruction::I32Mul);
            self.push(Instruction::I32Const(offset));
            self.push(Instruction::I32Add);
            self.push(Instruction::LocalGet(counter));
            self.push(Instruction::I32Const(source_block));
            self.push(Instruction::I32RemU);
            self.push(Instruction::I32Add);
            self.push_arena_index(dst_start)?;
            self.push_arena_address(source.start, Some(counter), 1)?;
            self.push(Instruction::F64Load(memarg()));
            self.push(Instruction::F64Store(memarg()));
            self.loop_end(counter);
            axis_offset = axis_offset
                .checked_add(source.dimensions[axis] as usize)
                .ok_or("native concatenate axis overflow")?;
        }
        Ok(())
    }
}
