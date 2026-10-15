//! Emit predicate-only observation calls and preserve their exact carrier frames.
use super::*;
use crate::typed_call::retained_event::InvocationStorage;

impl BodyEmitter<'_> {
    pub(in crate::emit) fn reset_event_invocations(&mut self) {
        let Some(records) = self.event_invocations else {
            return;
        };
        for record in records {
            self.call_address(record.status);
            self.push(Instruction::I64Const(-1));
            self.push(Instruction::I64Store(cell()));
        }
    }

    pub(in crate::emit) fn emit_event_observation(
        &mut self,
        operation: &LinearOp,
    ) -> Result<(), String> {
        let LinearOp::PureCallObservation {
            dst_start,
            input_starts,
            site,
        } = operation
        else {
            return Err("expected an issued predicate-only observation".into());
        };
        let record = self
            .event_invocations
            .and_then(|records| {
                records.iter().find(|record| {
                    record.operation == self.operation_ordinal && record.path == self.region_path
                })
            })
            .ok_or("observation has no retained source/carrier binding")?;
        let plan = self.calls.ok_or("observation has no compiled call table")?;
        let call = plan.call(site.value_site())?;
        self.pass_call_inputs(site.value_site().inputs(), input_starts, &call, None)?;
        self.pass_observation_buffers(&call)?;
        for offset in [plan.view().input, plan.view().output, plan.view().scratch] {
            self.call_address(offset);
        }
        self.push(Instruction::Call(call.function));
        self.push(Instruction::LocalSet(plan.view().status));
        self.call_address(record.status);
        self.push(Instruction::LocalGet(plan.view().status));
        self.push(Instruction::I64ExtendI32U);
        self.push(Instruction::I64Store(cell()));
        self.push(Instruction::LocalGet(plan.view().status));
        self.push(Instruction::I32Eqz);
        for status in self.event_stops {
            self.push(Instruction::LocalGet(plan.view().status));
            self.push(Instruction::I32Const(*status as i32));
            self.push(Instruction::I32Eq);
            self.push(Instruction::I32Or);
        }
        self.push(Instruction::I32Eqz);
        self.push(Instruction::If(BlockType::Empty));
        self.push(Instruction::LocalGet(plan.view().status));
        self.push(Instruction::Return);
        self.push(Instruction::End);
        for (to, from, bytes) in [
            (record.input, plan.view().input, record.input_bytes),
            (record.output, plan.view().output, record.output_bytes),
            (record.scratch, plan.view().scratch, record.scratch_bytes),
        ] {
            self.copy_event_cells(to, from, bytes);
        }
        for index in 0..site.output_scalar_count() {
            self.emit_const(
                dst_start
                    .checked_add(index as u32)
                    .ok_or("event predicate register overflows")?,
                1.0,
            )?;
        }
        self.emit_failed_predicates(record, *dst_start, site.predicate_outputs())
    }

    fn copy_event_cells(&mut self, to: u32, from: u32, bytes: u32) {
        self.call_address(to);
        self.call_address(from);
        self.push(Instruction::I32Const(bytes as i32));
        self.push(Instruction::MemoryCopy {
            src_mem: 0,
            dst_mem: 0,
        });
    }

    fn emit_failed_predicates(
        &mut self,
        record: &InvocationStorage,
        destination: Reg,
        predicates: &[usize],
    ) -> Result<(), String> {
        let Some(carrier) = &record.carrier else {
            return Ok(());
        };
        let base = record
            .scratch
            .checked_add(carrier.range.offset)
            .ok_or("event record offset overflows")?;
        let counter = self.arena_plan()?.counter;
        self.loop_start(counter, carrier.capacity as usize)?;
        self.push(Instruction::LocalGet(counter));
        self.push(Instruction::I64ExtendI32U);
        self.call_address(base);
        self.push(Instruction::I64Load(cell()));
        self.push(Instruction::I64LtU);
        self.push(Instruction::If(BlockType::Empty));
        for (tag, source) in carrier.sources.iter().enumerate() {
            let Some(index) = predicates
                .iter()
                .position(|predicate| *predicate == source.predicate)
            else {
                return Err("record predicate is foreign to its issued observation site".into());
            };
            self.call_address(base.checked_add(8).ok_or("event tag offset overflows")?);
            self.push(Instruction::LocalGet(counter));
            self.push(Instruction::I32Const(carrier.stride as i32));
            self.push(Instruction::I32Mul);
            self.push(Instruction::I32Add);
            self.push(Instruction::I64Load(cell()));
            self.push(Instruction::I64Const(tag as i64));
            self.push(Instruction::I64Eq);
            self.push(Instruction::If(BlockType::Empty));
            self.emit_const(
                destination
                    .checked_add(index as u32)
                    .ok_or("event predicate register overflows")?,
                0.0,
            )?;
            self.push(Instruction::End);
        }
        self.push(Instruction::End);
        self.loop_end(counter);
        Ok(())
    }
}

fn cell() -> MemArg {
    MemArg {
        offset: 0,
        align: 3,
        memory_index: 0,
    }
}
