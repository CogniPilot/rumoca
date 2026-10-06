//! Fail before external writes; publish all target values only after success.
use super::*;
impl BodyEmitter<'_> {
    pub(in crate::emit) fn begin_call_program(&mut self, layout: &VarLayout) -> Result<(), String> {
        let plan = self.calls.ok_or("missing native call layout")?;
        self.guard_call_spans(layout)?;
        self.push(Instruction::LocalGet(Y_PTR_PARAM));
        self.push(Instruction::LocalSet(plan.saved_y));
        self.push(Instruction::LocalGet(SEED_PTR_PARAM));
        self.push(Instruction::LocalGet(Y_PTR_PARAM));
        self.push(Instruction::I32Const(plan.work_bytes as i32));
        self.push(Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
        self.push(Instruction::LocalGet(SEED_PTR_PARAM));
        self.push(Instruction::LocalSet(Y_PTR_PARAM));
        self.reset_call_memos();
        Ok(())
    }

    pub(in crate::emit) fn begin_exact_schedule(
        &mut self,
        layout: &VarLayout,
    ) -> Result<(), String> {
        self.guard_call_spans(layout)?;
        self.reset_call_memos();
        Ok(())
    }

    fn guard_call_spans(&mut self, layout: &VarLayout) -> Result<(), String> {
        let plan = self.calls.ok_or("missing native call layout")?;
        self.push(Instruction::LocalGet(OUT_PTR_PARAM));
        self.return_status_if(1);
        let spans = [
            (
                Y_PTR_PARAM,
                u32::try_from(
                    layout
                        .y_scalars()
                        .checked_mul(8)
                        .ok_or("native Y overflow")?,
                )
                .map_err(|_| "native Y overflow")?,
            ),
            (
                P_PTR_PARAM,
                u32::try_from(
                    layout
                        .p_scalars()
                        .checked_mul(8)
                        .ok_or("native P overflow")?,
                )
                .map_err(|_| "native P overflow")?,
            ),
            (SEED_PTR_PARAM, plan.bytes),
        ];
        for &(pointer, bytes) in &spans {
            self.guard_call_buffer(pointer, bytes);
        }
        for (i, &(left, bytes)) in spans.iter().enumerate() {
            for &(right, other) in &spans[..i] {
                self.guard_call_disjoint(left, bytes, right, other);
            }
        }
        Ok(())
    }

    fn reset_call_memos(&mut self) {
        let plan = self.calls.expect("checked call layout");
        for memo in &plan.memos {
            self.call_address(memo.flag);
            self.push(Instruction::I32Const(0));
            self.push(Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
        }
    }

    pub(in crate::emit) fn finish_call_program(&mut self) {
        let plan = self.calls.expect("checked call layout");
        self.push(Instruction::LocalGet(plan.saved_y));
        self.push(Instruction::LocalGet(Y_PTR_PARAM));
        self.push(Instruction::I32Const(plan.work_bytes as i32));
        self.push(Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
        self.push(Instruction::I32Const(0));
    }

    fn guard_call_buffer(&mut self, pointer: u32, bytes: u32) {
        if bytes == 0 {
            return;
        }
        self.push(Instruction::LocalGet(pointer));
        self.push(Instruction::I32Const(7));
        self.push(Instruction::I32And);
        self.return_status_if(1);
        self.buffer_end(pointer, bytes);
        self.push(Instruction::MemorySize(0));
        self.push(Instruction::I64ExtendI32U);
        self.push(Instruction::I64Const(16));
        self.push(Instruction::I64Shl);
        self.push(Instruction::I64GtU);
        self.return_status_if(1);
    }

    fn guard_call_disjoint(&mut self, left: u32, bytes: u32, right: u32, other: u32) {
        if bytes == 0 || other == 0 {
            return;
        }
        self.push(Instruction::LocalGet(left));
        self.push(Instruction::I64ExtendI32U);
        self.buffer_end(right, other);
        self.push(Instruction::I64LtU);
        self.push(Instruction::LocalGet(right));
        self.push(Instruction::I64ExtendI32U);
        self.buffer_end(left, bytes);
        self.push(Instruction::I64LtU);
        self.push(Instruction::I32And);
        self.return_status_if(1);
    }

    fn buffer_end(&mut self, pointer: u32, bytes: u32) {
        self.push(Instruction::LocalGet(pointer));
        self.push(Instruction::I64ExtendI32U);
        self.push(Instruction::I64Const(i64::from(bytes)));
        self.push(Instruction::I64Add);
    }

    pub(in crate::emit) fn return_status_if(&mut self, status: i32) {
        self.push(Instruction::If(BlockType::Empty));
        self.push(Instruction::I32Const(status));
        self.push(Instruction::Return);
        self.push(Instruction::End);
    }
}
