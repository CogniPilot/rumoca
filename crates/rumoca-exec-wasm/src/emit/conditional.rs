//! Final native control flow for the existing checked conditional owner.
mod discovery;
mod regions;
pub(in crate::emit) mod workspace;
use super::*;
pub(in crate::emit) use discovery::{call_sites, visit_operations};
use rumoca_ir_solve::FunctionConditionalProgram;

#[derive(Clone, Copy)]
pub(super) struct Region {
    capture: Reg,
    capture_count: usize,
    output: Reg,
    output_count: usize,
    written: usize,
}

impl BodyEmitter<'_> {
    pub(super) fn emit_conditional_operation(
        &mut self,
        operation: &LinearOp,
    ) -> Option<Result<(), String>> {
        match operation {
            LinearOp::FunctionConditional {
                dst_start,
                capture_start,
                program,
            } if self.arena.is_some() && self.calls.is_some() => {
                Some(self.emit_function_conditional(*dst_start, *capture_start, program))
            }
            LinearOp::LoadFunctionConditionalCapture { dst, index }
                if self.conditional_region.is_some() =>
            {
                Some(self.emit_conditional_capture(*dst, *index))
            }
            LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start,
                index_start,
                count,
            } if self.conditional_region.is_some() => {
                Some(self.emit_conditional_capture_range(*dst_start, *index_start, *count))
            }
            _ => None,
        }
    }

    pub(super) fn emit_function_conditional(
        &mut self,
        destination: Reg,
        capture_start: Reg,
        program: &FunctionConditionalProgram,
    ) -> Result<(), String> {
        let parent_end = self.region_frame_end.unwrap_or(
            Reg::try_from(self.arena_plan()?.root_registers)
                .map_err(|_| "native root frame overflows")?,
        );
        let frame = workspace::Frame::new(parent_end, program)?;
        let source_capture = capture_start
            .checked_add(self.register_base)
            .ok_or("native capture source overflows")?;
        self.copy_conditional_cells(frame.capture, source_capture, program.capture_count)?;
        let parent_operation = self.operation_ordinal;
        for (index, arm) in program.arms.iter().enumerate() {
            self.region_path.push((parent_operation, 2 * index));
            self.emit_conditional_region(
                &arm.condition,
                arm.condition_register_count,
                frame,
                program.capture_count,
                1,
            )?;
            self.region_path.pop();
            self.push_absolute_arena_address(frame.output, None, 0)?;
            self.push(Instruction::F64Load(arena::memarg()));
            self.push(Instruction::F64Const(0.0f64.into()));
            self.push(Instruction::F64Ne);
            self.push(Instruction::If(BlockType::Empty));
            self.region_path.push((parent_operation, 2 * index + 1));
            self.emit_conditional_region(
                &arm.result,
                arm.result_register_count,
                frame,
                program.capture_count,
                program.result_count,
            )?;
            self.region_path.pop();
            self.push(Instruction::Else);
        }
        self.region_path
            .push((parent_operation, 2 * program.arms.len()));
        self.emit_conditional_region(
            &program.fallback,
            program.fallback_register_count,
            frame,
            program.capture_count,
            program.result_count,
        )?;
        self.region_path.pop();
        self.operation_ordinal = parent_operation;
        for _ in &program.arms {
            self.push(Instruction::End);
        }
        let destination = destination
            .checked_add(self.register_base)
            .ok_or("native conditional destination overflows")?;
        // Captures and the complete selected tuple occupy disjoint private
        // frames. No parent result register changes before selected-arm success.
        self.copy_conditional_cells(destination, frame.output, program.result_count)
    }

    fn copy_conditional_cells(&mut self, dst: Reg, src: Reg, count: usize) -> Result<(), String> {
        if count == 0 {
            return Ok(());
        }
        let counter = self.arena_plan()?.counter;
        self.loop_start(counter, count)?;
        self.push_absolute_arena_address(dst, Some(counter), 1)?;
        self.push_absolute_arena_address(src, Some(counter), 1)?;
        self.push(Instruction::F64Load(arena::memarg()));
        self.push(Instruction::F64Store(arena::memarg()));
        self.loop_end(counter);
        Ok(())
    }
}
