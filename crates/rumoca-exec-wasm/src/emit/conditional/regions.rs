//! A region has its own register namespace and private complete output cursor.
use super::*;
impl BodyEmitter<'_> {
    pub(super) fn emit_conditional_region(
        &mut self,
        operations: &[LinearOp],
        registers: usize,
        frame: workspace::Frame,
        capture_count: usize,
        output_count: usize,
    ) -> Result<(), String> {
        let previous = (
            self.register_base,
            self.region_frame_end,
            self.conditional_region,
        );
        self.register_base = frame.registers;
        self.region_frame_end = Reg::try_from(registers)
            .ok()
            .and_then(|count| frame.registers.checked_add(count));
        self.conditional_region = Some(Region {
            capture: frame.capture,
            capture_count,
            output: frame.output,
            output_count,
            written: 0,
        });
        let result = (|| {
            if self.region_frame_end.is_none() {
                return Err("native region frame overflows".into());
            }
            for (index, operation) in operations.iter().enumerate() {
                self.operation_ordinal = index;
                self.emit_conditional_region_operation(operation)?;
            }
            if self
                .conditional_region
                .is_none_or(|region| region.written != output_count)
            {
                return Err("native conditional region output cardinality differs".into());
            }
            Ok(())
        })();
        (
            self.register_base,
            self.region_frame_end,
            self.conditional_region,
        ) = previous;
        result
    }

    fn emit_conditional_region_operation(&mut self, operation: &LinearOp) -> Result<(), String> {
        if matches!(operation, LinearOp::StoreOutputRange { .. }) {
            return Err("native conditional range publication is unsupported".into());
        }
        self.emit_op(operation.clone())
    }

    pub(in crate::emit) fn emit_conditional_capture(
        &mut self,
        dst: Reg,
        index: usize,
    ) -> Result<(), String> {
        let region = self
            .conditional_region
            .ok_or("capture outside its conditional region")?;
        if index >= region.capture_count {
            return Err("native capture exceeds its exact owner".into());
        }
        let source = Reg::try_from(index)
            .ok()
            .and_then(|index| region.capture.checked_add(index))
            .ok_or("native capture offset overflows")?;
        self.push_absolute_arena_address(source, None, 0)?;
        self.push(Instruction::F64Load(arena::memarg()));
        self.set_reg(dst)
    }

    pub(in crate::emit) fn emit_conditional_capture_range(
        &mut self,
        dst: Reg,
        start: usize,
        count: usize,
    ) -> Result<(), String> {
        let region = self
            .conditional_region
            .ok_or("capture outside its conditional region")?;
        if start
            .checked_add(count)
            .is_none_or(|end| end > region.capture_count)
        {
            return Err("native capture range exceeds its exact owner".into());
        }
        let source = Reg::try_from(start)
            .ok()
            .and_then(|start| region.capture.checked_add(start))
            .ok_or("native capture offset overflows")?;
        let target = dst
            .checked_add(self.register_base)
            .ok_or("native capture destination overflows")?;
        self.copy_conditional_cells(target, source, count)
    }

    pub(in crate::emit) fn emit_conditional_output(&mut self, source: Reg) -> Result<(), String> {
        let mut region = self
            .conditional_region
            .ok_or("output outside its conditional region")?;
        if region.written >= region.output_count {
            return Err("native conditional has excess outputs".into());
        }
        let target = Reg::try_from(region.written)
            .ok()
            .and_then(|offset| region.output.checked_add(offset))
            .ok_or("native conditional output offset overflows")?;
        self.push_absolute_arena_address(target, None, 0)?;
        self.push_reg(source)?;
        self.push(Instruction::F64Store(arena::memarg()));
        region.written += 1;
        self.conditional_region = Some(region);
        Ok(())
    }
}
