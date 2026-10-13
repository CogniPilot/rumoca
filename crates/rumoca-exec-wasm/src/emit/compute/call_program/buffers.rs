//! Fail before external writes; publish all target values only after success.
use super::*;
impl BodyEmitter<'_> {
    /// The exported entry: guards the buffers, saves the P cells the stages
    /// patch with typed-input views, runs the program body, and restores
    /// those cells whatever its status, so the host P is unchanged on return.
    pub(in crate::emit) fn emit_call_program_entry(
        &mut self,
        layout: &VarLayout,
        schedule: &solve::NativeRefreshAssignmentSchedule,
        body: u32,
    ) -> Result<(), String> {
        let status = self
            .calls
            .ok_or("missing native call layout")?
            .view()
            .status;
        self.guard_call_spans(layout)?;
        self.copy_patched_inputs(schedule, true)?;
        for argument in 0..LOCAL_BASE {
            self.push(Instruction::LocalGet(argument));
        }
        self.push(Instruction::Call(body));
        self.push(Instruction::LocalSet(status));
        self.copy_patched_inputs(schedule, false)?;
        self.push(Instruction::LocalGet(status));
        Ok(())
    }

    /// Copy the typed-input P cells to (`save`) or back from their save region.
    fn copy_patched_inputs(
        &mut self,
        schedule: &solve::NativeRefreshAssignmentSchedule,
        save: bool,
    ) -> Result<(), String> {
        let mut saved = self
            .calls
            .ok_or("missing native call layout")?
            .view()
            .p_save;
        let bytes_of = |cells: usize| {
            cells
                .checked_mul(8)
                .and_then(|n| u32::try_from(n).ok())
                .ok_or("native typed input cells overflow")
        };
        for run in schedule
            .input_runs()
            .iter()
            .filter(|run| run.view_is_read())
        {
            let bytes = bytes_of(run.scalar_count())?;
            let cell = bytes_of(run.p_range().start)?;
            let host = |this: &mut Self| {
                this.push(Instruction::LocalGet(P_PTR_PARAM));
                this.push(Instruction::I32Const(cell as i32));
                this.push(Instruction::I32Add);
            };
            if save {
                self.call_address(saved);
                host(self);
            } else {
                host(self);
                self.call_address(saved);
            }
            self.push(Instruction::I32Const(bytes as i32));
            self.push(Instruction::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
            saved = saved.checked_add(bytes).ok_or("native P save overflow")?;
        }
        Ok(())
    }

    /// Copy the Y cells the stages read into the work region before the first
    /// stage runs.
    pub(in crate::emit) fn begin_call_program(
        &mut self,
        schedule: &solve::NativeRefreshAssignmentSchedule,
    ) -> Result<(), String> {
        let plan = self.calls.ok_or("missing native call layout")?;
        self.push(Instruction::LocalGet(Y_PTR_PARAM));
        self.push(Instruction::LocalSet(plan.view().saved_y));
        for range in &plan.view().y_reads {
            for pointer in [SEED_PTR_PARAM, Y_PTR_PARAM] {
                self.push(Instruction::LocalGet(pointer));
                self.push(Instruction::I32Const(range.start as i32));
                self.push(Instruction::I32Add);
            }
            self.push(Instruction::I32Const((range.end - range.start) as i32));
            self.push(Instruction::MemoryCopy {
                src_mem: 0,
                dst_mem: 0,
            });
        }
        self.push(Instruction::LocalGet(SEED_PTR_PARAM));
        self.push(Instruction::LocalSet(Y_PTR_PARAM));
        if !schedule.input_runs().is_empty() {
            // Stages patch the typed-input cells of P in place; the entry
            // restores them.
            self.call_address(plan.view().typed_lanes_cell);
            self.push(Instruction::LocalGet(OUT_PTR_PARAM));
            self.push(Instruction::I32Store(MemArg {
                offset: 0,
                align: 2,
                memory_index: 0,
            }));
        }
        Ok(())
    }

    pub(in crate::emit) fn begin_exact_schedule(
        &mut self,
        layout: &VarLayout,
    ) -> Result<(), String> {
        self.guard_call_spans(layout)
    }

    fn guard_call_spans(&mut self, layout: &VarLayout) -> Result<(), String> {
        let plan = self.calls.ok_or("missing native call layout")?;
        // The output pointer is reserved (zero) unless the program reads typed
        // input lanes or publishes typed output lanes through it.
        let typed_lanes = plan
            .view()
            .input_lane_bytes
            .checked_add(plan.view().lane_bytes)
            .ok_or("native typed lanes overflow")?;
        self.push(Instruction::LocalGet(OUT_PTR_PARAM));
        if typed_lanes != 0 {
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
            (SEED_PTR_PARAM, plan.view().bytes),
            (OUT_PTR_PARAM, typed_lanes),
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

    /// Publish the host Y scalars and the staged typed output lanes after the
    /// last stage succeeded.
    pub(in crate::emit) fn finish_call_program(&mut self) -> Result<(), String> {
        self.publish_work_y()?;
        self.publish_lanes()?;
        self.push(Instruction::I32Const(0));
        Ok(())
    }

    /// Push the host typed-lane base. Stage bodies reuse the output-pointer
    /// parameter as their store cursor, so the base is read from the cell the
    /// entry filled before any stage ran.
    pub(in crate::emit) fn push_typed_lanes_base(&mut self) -> Result<(), String> {
        let cell = self
            .calls
            .ok_or("missing native call layout")?
            .view()
            .typed_lanes_cell;
        self.call_address(cell);
        self.push(Instruction::I32Load(MemArg {
            offset: 0,
            align: 2,
            memory_index: 0,
        }));
        Ok(())
    }

    /// Stage the typed lanes of a run of derived outputs (see [`derived_runs`])
    /// from the finished work region: one copy for a run of Real or input-lane
    /// cells, one loop for a run of Boolean cells, and the scalar form for the
    /// rest.
    pub(in crate::emit) fn publish_derived_run(
        &mut self,
        run: &[solve::NativeDerivedOutput],
    ) -> Result<(), String> {
        let [first, _, ..] = run else {
            return run
                .first()
                .map_or(Ok(()), |output| self.publish_derived_output(output));
        };
        let plan = self.calls.ok_or("missing native call layout")?;
        let lane = lane_address(plan, first)?;
        let count = run.len();
        let work = u32::try_from(first.work_index())
            .ok()
            .and_then(|n| n.checked_mul(8))
            .ok_or("native work slot overflow")?;
        let bytes =
            i32::try_from(count * first.lane().width()).map_err(|_| "native lane run overflow")?;
        match (first.lane(), first.integer_source()) {
            (solve::NativeOutputLane::Real, _) => {
                self.call_address(lane);
                self.call_address(work);
                self.push(Instruction::I32Const(bytes));
                self.push(Instruction::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
            }
            (
                solve::NativeOutputLane::Integer,
                Some(solve::NativeIntegerSource::Input { lane_offset }),
            ) => {
                self.call_address(lane);
                self.push_typed_lanes_base()?;
                self.push(Instruction::I32Const(
                    i32::try_from(lane_offset).map_err(|_| "native input lane overflow")?,
                ));
                self.push(Instruction::I32Add);
                self.push(Instruction::I32Const(bytes));
                self.push(Instruction::MemoryCopy {
                    src_mem: 0,
                    dst_mem: 0,
                });
            }
            (solve::NativeOutputLane::Boolean, _) => self.publish_boolean_run(lane, work, count)?,
            _ => return Err("native lane run mixes sources".into()),
        }
        Ok(())
    }

    /// One loop over the cells of a run of Boolean lanes.
    fn publish_boolean_run(&mut self, lane: u32, work: u32, count: usize) -> Result<(), String> {
        let counter = self
            .arena
            .as_ref()
            .ok_or("missing native register arena")?
            .counter;
        self.cells(counter, count, |this| {
            this.call_address(lane);
            this.push(Instruction::LocalGet(counter));
            this.push(Instruction::I32Add);
            this.call_cell_address(work, counter);
            this.push(Instruction::F64Load(MemArg {
                offset: 0,
                align: 3,
                memory_index: 0,
            }));
            this.push(Instruction::F64Const(0.0.into()));
            this.push(Instruction::F64Ne);
            this.push(Instruction::I32Store8(MemArg {
                offset: 0,
                align: 0,
                memory_index: 0,
            }));
            Ok(())
        })
    }

    /// Stage one derived output's typed lane from the finished work region.
    fn publish_derived_output(
        &mut self,
        output: &solve::NativeDerivedOutput,
    ) -> Result<(), String> {
        let plan = self.calls.ok_or("missing native call layout")?;
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
                Some(solve::NativeIntegerSource::Input { lane_offset }),
            ) => {
                self.call_address(lane);
                self.push_typed_lanes_base()?;
                self.push(Instruction::I64Load(MemArg {
                    offset: lane_offset as u64,
                    align: 3,
                    memory_index: 0,
                }));
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
        Ok(())
    }

    /// Copy the exact i64 output cells of the call just emitted, the first at
    /// offset `cell`, into their Integer lane, before any later call reuses
    /// the output buffer.
    pub(in crate::emit) fn capture_integer_cell(
        &mut self,
        capture: crate::emit::IntegerCapture,
        cell: u32,
    ) -> Result<(), String> {
        let last_offset = capture
            .count
            .checked_sub(1)
            .and_then(|count| count.checked_mul(8))
            .ok_or("native Integer capture extent overflow")?;
        cell.checked_add(last_offset)
            .ok_or("native call cell overflow")?;
        capture
            .lane
            .checked_add(last_offset)
            .ok_or("native Integer lane overflow")?;
        // The cells are consecutive in the call output and in the lane, so one
        // copy moves them all.
        let bytes = last_offset
            .checked_add(8)
            .and_then(|bytes| i32::try_from(bytes).ok())
            .ok_or("native Integer capture extent overflow")?;
        self.call_address(capture.lane);
        self.call_address(cell);
        self.push(Instruction::I32Const(bytes));
        self.push(Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
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
        .and_then(|offset| plan.view().lanes.checked_add(offset))
        .ok_or_else(|| "native output lane overflow".into())
}

/// The maximal runs of derived outputs `outputs` that publish consecutive work
/// slots into consecutive lane cells of one kind, and for Integer inputs read
/// consecutive input lane cells. Only the cells of such a run share one copy
/// or loop; an output with any other source stands alone.
pub(super) fn derived_runs(
    outputs: &[solve::NativeDerivedOutput],
) -> Vec<&[solve::NativeDerivedOutput]> {
    let follows = |previous: &solve::NativeDerivedOutput, next: &solve::NativeDerivedOutput| {
        let cells = next.work_index() == previous.work_index() + 1
            && next.lane() == previous.lane()
            && next.lane_offset() == previous.lane_offset() + previous.lane().width();
        let source = match (previous.integer_source(), next.integer_source()) {
            (None, None) => true,
            (
                Some(solve::NativeIntegerSource::Input {
                    lane_offset: before,
                }),
                Some(solve::NativeIntegerSource::Input { lane_offset: after }),
            ) => after == before + previous.lane().width(),
            _ => false,
        };
        cells && source
    };
    let mut runs = Vec::new();
    let mut start = 0usize;
    for at in 1..=outputs.len() {
        if at == outputs.len() || !follows(&outputs[at - 1], &outputs[at]) {
            runs.push(&outputs[start..at]);
            start = at;
        }
    }
    if outputs.is_empty() {
        runs.clear();
    }
    runs
}
