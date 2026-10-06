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
        self.push(Instruction::I32Const(plan.host_y_bytes as i32));
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
        // The output pointer is reserved (zero) unless the program publishes
        // typed output lanes through it.
        self.push(Instruction::LocalGet(OUT_PTR_PARAM));
        if plan.lane_bytes != 0 {
            self.push(Instruction::I32Eqz);
        }
        self.return_status_if(1);
        let host_y = u32::try_from(
            layout
                .y_scalars()
                .checked_mul(8)
                .ok_or("native Y overflow")?,
        )
        .map_err(|_| "native Y overflow")?;
        let spans = [
            (Y_PTR_PARAM, host_y),
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
            (OUT_PTR_PARAM, plan.lane_bytes),
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

    /// Publish the host Y scalars and every typed output lane after the
    /// last stage succeeded. Real and Boolean lanes take the value of their
    /// work slot; an Integer lane already holds its call cell or literal.
    pub(in crate::emit) fn finish_call_program(
        &mut self,
        outputs: &[solve::NativeDerivedOutput],
    ) -> Result<(), String> {
        let plan = self.calls.ok_or("missing native call layout")?;
        self.push(Instruction::LocalGet(plan.saved_y));
        self.push(Instruction::LocalGet(Y_PTR_PARAM));
        self.push(Instruction::I32Const(plan.host_y_bytes as i32));
        self.push(Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
        for output in outputs {
            let lane = lane_address(plan, output)?;
            let work = u32::try_from(output.work_index())
                .ok()
                .and_then(|n| n.checked_mul(8))
                .ok_or("native work slot overflow")?;
            match (output.lane(), output.integer_source()) {
                (solve::NativeOutputLane::Real, _) => {
                    self.call_address(lane);
                    self.call_address(work);
                    self.push(Instruction::F64Load(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                    self.push(Instruction::F64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                (solve::NativeOutputLane::Boolean, _) => {
                    self.call_address(lane);
                    self.call_address(work);
                    self.push(Instruction::F64Load(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                    self.push(Instruction::F64Const(0.0.into()));
                    self.push(Instruction::F64Ne);
                    self.push(Instruction::I32Store8(MemArg {
                        offset: 0,
                        align: 0,
                        memory_index: 0,
                    }));
                }
                (
                    solve::NativeOutputLane::Integer,
                    Some(solve::NativeIntegerSource::Literal(value)),
                ) => {
                    self.call_address(lane);
                    self.push(Instruction::I64Const(value));
                    self.push(Instruction::I64Store(MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));
                }
                (
                    solve::NativeOutputLane::Integer,
                    Some(solve::NativeIntegerSource::CallCell { .. }),
                ) => {}
                (solve::NativeOutputLane::Integer, None) => {
                    return Err("native Integer lane has no exact source".into());
                }
            }
        }
        if plan.lane_bytes != 0 {
            self.push(Instruction::LocalGet(OUT_PTR_PARAM));
            self.call_address(plan.lanes);
            self.push(Instruction::I32Const(plan.lane_bytes as i32));
            self.push(Instruction::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
        }
        self.push(Instruction::I32Const(0));
        Ok(())
    }

    /// Copy the exact i64 output cell of the call just emitted into its
    /// Integer lane, before any later call reuses the output buffer.
    pub(in crate::emit) fn capture_integer_cell(
        &mut self,
        capture: crate::emit::IntegerCapture,
    ) -> Result<(), String> {
        let plan = self.calls.ok_or("missing native call layout")?;
        let cell = capture
            .cell
            .checked_mul(8)
            .and_then(|n| n.checked_add(plan.output))
            .ok_or("native call cell overflow")?;
        self.call_address(capture.lane);
        self.call_address(cell);
        self.push(Instruction::I64Load(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        self.push(Instruction::I64Store(MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        Ok(())
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

/// Staging address of one derived output's typed lane.
pub(in crate::emit) fn lane_address(
    plan: &CallProgramPlan,
    output: &solve::NativeDerivedOutput,
) -> Result<u32, String> {
    u32::try_from(output.lane_offset())
        .ok()
        .and_then(|offset| plan.lanes.checked_add(offset))
        .ok_or_else(|| "native output lane overflow".into())
}
