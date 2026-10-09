//! Arguments/results are copied to private disjoint callee spans once per call.
use super::{TypedCallCompileError, emit::Emitter, layout::CellRange};
use rumoca_ir_solve as solve;
use wasm_encoder::{BlockType, Instruction as I};

impl Emitter<'_> {
    pub(super) fn call_operation(
        &mut self,
        index: usize,
        operation: &solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        let solve::SolveOperation::Call {
            owner,
            arguments,
            destinations,
            ..
        } = operation.operation()
        else {
            unreachable!("checked call dispatch")
        };
        let frame = self.plan.calls[index]
            .as_ref()
            .ok_or(TypedCallCompileError::SiteMismatch)?;
        let (input, output, scratch) = (frame.input, frame.output, frame.scratch);
        let function = self.linked.functions[owner.index() as usize]
            .ok_or(TypedCallCompileError::SiteMismatch)?;
        self.copy_tuple(input, arguments, true);
        self.address(input);
        self.address(output);
        self.address(scratch);
        self.push(I::Call(function));
        self.push(I::LocalTee(3));
        self.push(I::If(BlockType::Empty));
        self.push(I::LocalGet(3));
        self.push(I::Return);
        self.push(I::End);
        // A failed helper never transfers even part of its result tuple.
        self.copy_tuple(output, destinations, false);
        Ok(())
    }

    fn copy_tuple(
        &mut self,
        tuple: CellRange,
        registers: &[solve::SolveRegisterId],
        to_tuple: bool,
    ) {
        let mut offset = tuple.offset;
        for register in registers {
            let register = self.reg(*register);
            let cell = CellRange {
                offset,
                bytes: register.bytes,
                ..tuple
            };
            if to_tuple {
                self.copy(cell, register);
            } else {
                self.copy(register, cell);
            }
            offset += register.bytes;
        }
    }
}
