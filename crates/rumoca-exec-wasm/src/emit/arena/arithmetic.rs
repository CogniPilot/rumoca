//! Ordered arithmetic over compact primal register ranges.

use super::*;

impl BodyEmitter<'_> {
    pub(in crate::emit) fn emit_arena_tensor(&mut self, op: LinearOp) -> Result<(), String> {
        match op {
            op @ LinearOp::TensorBinary { .. } => self.arena_binary(op),
            op @ LinearOp::MatrixMultiply { .. } => self.arena_matrix(op),
            op @ LinearOp::TensorCross { .. } => self.arena_cross(op),
            op @ LinearOp::TensorLoad { .. } => self.arena_load(op),
            op @ LinearOp::TensorFill { .. } => self.arena_fill(op),
            op @ LinearOp::TensorIdentity { .. } => self.arena_identity(op),
            op @ LinearOp::TensorTranspose { .. } => self.arena_transpose(op),
            op @ LinearOp::TensorConcatenate { .. } => self.arena_concatenate(op),
            _ => Err("unsupported native private arena operation".into()),
        }
    }

    fn arena_binary(&mut self, operation: LinearOp) -> Result<(), String> {
        let LinearOp::TensorBinary {
            dst_start,
            op,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            ..
        } = operation
        else {
            return Err("expected TensorBinary".into());
        };
        let arithmetic = match op {
            BinaryOp::Add => Instruction::F64Add,
            BinaryOp::Sub => Instruction::F64Sub,
            BinaryOp::Mul => Instruction::F64Mul,
            BinaryOp::Div => Instruction::F64Div,
            _ => {
                return Err(
                    "native arena tensor binary supports add/subtract/multiply/divide".into(),
                );
            }
        };
        let counter = self.arena_plan()?.counter;
        self.loop_start(counter, count)?;
        self.push_arena_address(dst_start, Some(counter), 1)?;
        self.push_arena_address(lhs_start, Some(counter), lhs_stride)?;
        self.push(Instruction::F64Load(memarg()));
        self.push_arena_address(rhs_start, Some(counter), rhs_stride)?;
        self.push(Instruction::F64Load(memarg()));
        self.push(arithmetic);
        self.push(Instruction::F64Store(memarg()));
        self.loop_end(counter);
        Ok(())
    }

    fn arena_matrix(&mut self, operation: LinearOp) -> Result<(), String> {
        let LinearOp::MatrixMultiply {
            dst_start,
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            ..
        } = operation
        else {
            return Err("expected MatrixMultiply".into());
        };
        let count = rows
            .checked_mul(columns)
            .ok_or("native matrix count overflow")?;
        let arena = self.arena_plan()?;
        let cols = i32::try_from(columns).map_err(|_| "native matrix columns exceed i32")?;
        let width = i32::try_from(inner).map_err(|_| "native matrix inner extent exceeds i32")?;
        self.loop_start(arena.counter, count)?;
        self.push(Instruction::F64Const(0.0.into()));
        self.push(Instruction::LocalSet(LOCAL_BASE + 1));
        self.loop_start(arena.inner_counter, inner)?;
        self.push(Instruction::LocalGet(LOCAL_BASE + 1));
        self.push(Instruction::LocalGet(arena.counter));
        self.push(Instruction::I32Const(cols));
        self.push(Instruction::I32DivU);
        self.push(Instruction::I32Const(width));
        self.push(Instruction::I32Mul);
        self.push(Instruction::LocalGet(arena.inner_counter));
        self.push(Instruction::I32Add);
        self.push_arena_index(lhs_start)?;
        self.push(Instruction::F64Load(memarg()));
        self.push(Instruction::LocalGet(arena.inner_counter));
        self.push(Instruction::I32Const(cols));
        self.push(Instruction::I32Mul);
        self.push(Instruction::LocalGet(arena.counter));
        self.push(Instruction::I32Const(cols));
        self.push(Instruction::I32RemU);
        self.push(Instruction::I32Add);
        self.push_arena_index(rhs_start)?;
        self.push(Instruction::F64Load(memarg()));
        self.push(Instruction::F64Mul);
        self.push(Instruction::F64Add);
        self.push(Instruction::LocalSet(LOCAL_BASE + 1));
        self.loop_end(arena.inner_counter);
        self.push_arena_address(dst_start, Some(arena.counter), 1)?;
        self.push(Instruction::LocalGet(LOCAL_BASE + 1));
        self.push(Instruction::F64Store(memarg()));
        self.loop_end(arena.counter);
        Ok(())
    }

    fn arena_cross(&mut self, operation: LinearOp) -> Result<(), String> {
        let LinearOp::TensorCross {
            dst_start,
            lhs_start,
            rhs_start,
            ..
        } = operation
        else {
            return Err("expected TensorCross".into());
        };
        // Fixed vector rank, independent of any tensor extent.
        for (index, left, right) in [(0, 1, 2), (1, 2, 0), (2, 0, 1)] {
            self.push_arena_address(dst_start + index, None, 0)?;
            self.push_reg(lhs_start + left)?;
            self.push_reg(rhs_start + right)?;
            self.push(Instruction::F64Mul);
            self.push_reg(lhs_start + right)?;
            self.push_reg(rhs_start + left)?;
            self.push(Instruction::F64Mul);
            self.push(Instruction::F64Sub);
            self.push(Instruction::F64Store(memarg()));
        }
        Ok(())
    }
}
