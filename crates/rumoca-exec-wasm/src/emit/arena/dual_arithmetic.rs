//! Ordered arithmetic of the original two-lane tensor programs.
use super::*;

impl BodyEmitter<'_> {
    pub(super) fn arena_dual_binary(&mut self, operation: LinearOp) -> Result<(), String> {
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
            return Err("expected two-lane TensorBinary".into());
        };
        let counter = self.arena_plan()?.counter;
        self.loop_start(counter, count)?;
        for lane in 0..2 {
            self.push_arena_address(dst_start + lane, Some(counter), 2)?;
            self.dual_binary_lane(
                op,
                (lhs_start, lhs_stride),
                (rhs_start, rhs_stride),
                lane,
                counter,
            )?;
            self.push(Instruction::F64Store(memarg()));
        }
        self.loop_end(counter);
        Ok(())
    }

    fn dual_binary_lane(
        &mut self,
        op: BinaryOp,
        lhs: (Reg, usize),
        rhs: (Reg, usize),
        lane: u32,
        counter: u32,
    ) -> Result<(), String> {
        match (op, lane) {
            (BinaryOp::Add | BinaryOp::Sub, _) => {
                self.dual_read(lhs, lane, counter)?;
                self.dual_read(rhs, lane, counter)?;
                self.push(if op == BinaryOp::Add {
                    Instruction::F64Add
                } else {
                    Instruction::F64Sub
                });
            }
            (BinaryOp::Mul | BinaryOp::Div, 0) => {
                self.dual_read(lhs, 0, counter)?;
                self.dual_read(rhs, 0, counter)?;
                self.push(if op == BinaryOp::Mul {
                    Instruction::F64Mul
                } else {
                    Instruction::F64Div
                });
                if op == BinaryOp::Div {
                    self.push(Instruction::F64Const(0.0.into()));
                    self.dual_read(lhs, 0, counter)?;
                    self.push(Instruction::F64Const(0.0.into()));
                    self.push(Instruction::F64Ne);
                    self.dual_read(rhs, 0, counter)?;
                    self.push(Instruction::F64Const(0.0.into()));
                    self.push(Instruction::F64Ne);
                    self.push(Instruction::I32Or);
                    self.push(Instruction::Select);
                }
            }
            (BinaryOp::Mul, 1) => {
                self.dual_read(lhs, 1, counter)?;
                self.dual_read(rhs, 0, counter)?;
                self.push(Instruction::F64Mul);
                self.dual_read(lhs, 0, counter)?;
                self.dual_read(rhs, 1, counter)?;
                self.push(Instruction::F64Mul);
                self.push(Instruction::F64Add);
            }
            (BinaryOp::Div, 1) => {
                self.dual_division_tangent(lhs, rhs, counter)?;
            }
            _ => {
                return Err(
                    "two-lane tensor arithmetic requires add/subtract/multiply/divide".into(),
                );
            }
        }
        Ok(())
    }

    fn dual_read(
        &mut self,
        (start, stride): (Reg, usize),
        lane: u32,
        counter: u32,
    ) -> Result<(), String> {
        self.push_arena_address(
            start + lane,
            Some(counter),
            stride
                .checked_mul(2)
                .ok_or("dual tensor stride overflows")?,
        )?;
        self.push(Instruction::F64Load(memarg()));
        Ok(())
    }

    fn dual_division_tangent(
        &mut self,
        lhs: (Reg, usize),
        rhs: (Reg, usize),
        counter: u32,
    ) -> Result<(), String> {
        self.dual_read(lhs, 1, counter)?;
        self.push(Instruction::F64Const(1.0.into()));
        self.dual_read(rhs, 0, counter)?;
        self.push(Instruction::F64Div);
        self.dual_finite_or_zero();
        self.push(Instruction::F64Mul);
        self.dual_read(rhs, 1, counter)?;
        self.dual_read(lhs, 0, counter)?;
        self.push(Instruction::F64Neg);
        self.dual_read(rhs, 0, counter)?;
        self.dual_read(rhs, 0, counter)?;
        self.push(Instruction::F64Mul);
        self.push(Instruction::F64Div);
        self.dual_finite_or_zero();
        self.push(Instruction::F64Mul);
        self.push(Instruction::F64Add);
        Ok(())
    }

    fn dual_finite_or_zero(&mut self) {
        self.push(Instruction::LocalSet(LOCAL_BASE));
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::F64Const(0.0.into()));
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::LocalGet(LOCAL_BASE));
        self.push(Instruction::F64Sub);
        self.push(Instruction::F64Const(0.0.into()));
        self.push(Instruction::F64Eq);
        self.push(Instruction::Select);
    }

    pub(super) fn arena_dual_matrix(&mut self, operation: LinearOp) -> Result<(), String> {
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
            return Err("expected two-lane MatrixMultiply".into());
        };
        let count = rows
            .checked_mul(columns)
            .ok_or("dual matrix count overflow")?;
        let arena = self.arena_plan()?;
        let columns = i32::try_from(columns).map_err(|_| "dual matrix columns overflow")?;
        let width = i32::try_from(inner).map_err(|_| "dual matrix inner extent overflow")?;
        self.loop_start(arena.counter, count)?;
        for lane in 0..2 {
            self.push(Instruction::F64Const(0.0.into()));
            self.push(Instruction::LocalSet(LOCAL_BASE + 1));
            self.loop_start(arena.inner_counter, inner)?;
            self.push(Instruction::LocalGet(LOCAL_BASE + 1));
            self.dual_matrix_read(lhs_start, lane, columns, width, true)?;
            self.dual_matrix_read(rhs_start, 0, columns, width, false)?;
            self.push(Instruction::F64Mul);
            if lane == 1 {
                self.dual_matrix_read(lhs_start, 0, columns, width, true)?;
                self.dual_matrix_read(rhs_start, 1, columns, width, false)?;
                self.push(Instruction::F64Mul);
                self.push(Instruction::F64Add);
            }
            self.push(Instruction::F64Add);
            self.push(Instruction::LocalSet(LOCAL_BASE + 1));
            self.loop_end(arena.inner_counter);
            self.push_arena_address(dst_start + lane, Some(arena.counter), 2)?;
            self.push(Instruction::LocalGet(LOCAL_BASE + 1));
            self.push(Instruction::F64Store(memarg()));
        }
        self.loop_end(arena.counter);
        Ok(())
    }

    fn dual_matrix_read(
        &mut self,
        start: Reg,
        lane: u32,
        columns: i32,
        width: i32,
        lhs: bool,
    ) -> Result<(), String> {
        let arena = self.arena_plan()?;
        if lhs {
            self.push(Instruction::LocalGet(arena.counter));
            self.push(Instruction::I32Const(columns));
            self.push(Instruction::I32DivU);
            self.push(Instruction::I32Const(width));
            self.push(Instruction::I32Mul);
            self.push(Instruction::LocalGet(arena.inner_counter));
        } else {
            self.push(Instruction::LocalGet(arena.inner_counter));
            self.push(Instruction::I32Const(columns));
            self.push(Instruction::I32Mul);
            self.push(Instruction::LocalGet(arena.counter));
            self.push(Instruction::I32Const(columns));
            self.push(Instruction::I32RemU);
        }
        self.push(Instruction::I32Add);
        self.push(Instruction::I32Const(2));
        self.push(Instruction::I32Mul);
        self.push_arena_index(start + lane)?;
        self.push(Instruction::F64Load(memarg()));
        Ok(())
    }

    pub(super) fn arena_dual_cross(&mut self, operation: LinearOp) -> Result<(), String> {
        let LinearOp::TensorCross {
            dst_start,
            lhs_start,
            rhs_start,
            ..
        } = operation
        else {
            return Err("expected two-lane TensorCross".into());
        };
        for indices in [(0, 1, 2), (1, 2, 0), (2, 0, 1)] {
            for lane in 0..2 {
                self.dual_cross_lane(dst_start, lhs_start, rhs_start, indices, lane)?;
            }
        }
        Ok(())
    }

    fn dual_cross_lane(
        &mut self,
        dst_start: Reg,
        lhs_start: Reg,
        rhs_start: Reg,
        (index, left, right): (u32, u32, u32),
        lane: u32,
    ) -> Result<(), String> {
        self.push_arena_address(dst_start + 2 * index + lane, None, 0)?;
        self.push_reg(lhs_start + 2 * left + lane)?;
        self.push_reg(rhs_start + 2 * right)?;
        self.push(Instruction::F64Mul);
        if lane == 1 {
            self.push_reg(lhs_start + 2 * left)?;
            self.push_reg(rhs_start + 2 * right + 1)?;
            self.push(Instruction::F64Mul);
            self.push(Instruction::F64Add);
        }
        self.push_reg(lhs_start + 2 * right + lane)?;
        self.push_reg(rhs_start + 2 * left)?;
        self.push(Instruction::F64Mul);
        self.push(Instruction::F64Sub);
        if lane == 1 {
            self.push_reg(lhs_start + 2 * right)?;
            self.push_reg(rhs_start + 2 * left + 1)?;
            self.push(Instruction::F64Mul);
            self.push(Instruction::F64Sub);
        }
        self.push(Instruction::F64Store(memarg()));
        Ok(())
    }
}
