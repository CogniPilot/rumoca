//! Small arguments are copied to private callee spans once per call and large
//! ones are passed by reference; results are copied back once per call.
use super::{
    TypedCallCompileError,
    emit::{CELL, Emitter},
    layout::{CellRange, PointerCell, RefOutput},
};
use rumoca_ir_solve as solve;
use wasm_encoder::{BlockType, Instruction as I};

impl Emitter<'_> {
    pub(super) fn call_operation(
        &mut self,
        index: usize,
        operation: &solve::SolveSpannedOperation,
    ) -> Result<(), TypedCallCompileError> {
        let solve::SolveOperation::Call {
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
        let results_in_place = frame.results_in_place;
        let unit = frame.unit;
        let function = self.linked.functions[unit].ok_or(TypedCallCompileError::SiteMismatch)?;
        self.pass_arguments(input, arguments, unit)?;
        self.pass_buffers(index, input);
        self.address(input);
        self.address(output);
        self.address(scratch);
        self.push(I::Call(function));
        self.push(I::LocalSet(3));
        // The root guards cover every constructed child span. Its header is
        // therefore initialized before input validation even on child failure.
        self.forward_observations(index, unit, input, scratch, operation.provenance())?;
        self.push(I::LocalGet(3));
        self.push(I::If(BlockType::Empty));
        self.push(I::LocalGet(3));
        self.push(I::Return);
        self.push(I::End);
        // A failed helper never transfers even part of its result tuple.
        if !results_in_place {
            self.copy_results(output, destinations);
        }
        Ok(())
    }

    /// Pack small arguments into the callee's input tuple and store the
    /// address of each large one (see `InputConvention::ByReference`).
    fn pass_arguments(
        &mut self,
        tuple: CellRange,
        arguments: &[solve::SolveRegisterId],
        unit: usize,
    ) -> Result<(), TypedCallCompileError> {
        let cells = self.linked.unit_plan(unit)?.input_cells.clone();
        if cells.len() != arguments.len() {
            return Err(TypedCallCompileError::SiteMismatch);
        }
        for (cell, argument) in cells.iter().zip(arguments) {
            let argument = self.reg(*argument);
            let at = CellRange {
                offset: tuple.offset + cell.offset,
                bytes: argument.bytes,
                ..tuple
            };
            if cell.by_reference {
                self.address(at);
                self.address(argument);
                self.push(I::I64ExtendI32U);
                self.push(I::I64Store(CELL));
            } else {
                self.copy(at, argument);
            }
        }
        Ok(())
    }

    /// Store the address of each buffer the callee builds a by-reference result
    /// in, in the callee's input tuple.
    fn pass_buffers(&mut self, index: usize, tuple: CellRange) {
        let Some(frame) = self.plan.calls[index].as_ref() else {
            return;
        };
        for destination in frame.buffers.clone() {
            let at = CellRange {
                offset: tuple.offset + destination.cell,
                bytes: 8,
                ..tuple
            };
            self.address(at);
            self.address(destination.buffer);
            self.push(I::I64ExtendI32U);
            self.push(I::I64Store(CELL));
        }
    }

    fn copy_results(&mut self, tuple: CellRange, registers: &[solve::SolveRegisterId]) {
        let mut offset = tuple.offset;
        for source in registers.iter().copied() {
            let register = self.reg(source);
            let cell = CellRange {
                offset,
                bytes: register.bytes,
                ..tuple
            };
            if self.plan.liveness.needs(source) {
                self.copy(register, cell);
            }
            offset += register.bytes;
        }
    }
}

impl Emitter<'_> {
    /// Leave the address of an output's value in its result cell (see
    /// `RefOutput`). The value is already where the cell will name it when the
    /// body built it in the caller's buffer, when it is that buffer's own
    /// reference result, or when it is an input the helper returns unchanged;
    /// otherwise it is copied into the buffer.
    pub(super) fn store_reference_output(
        &mut self,
        output: RefOutput,
        slot: usize,
        source: solve::SolveRegisterId,
    ) {
        let buffer = self.plan.slots[slot];
        let value = self.reg(source);
        let result = CellRange {
            base: 2,
            offset: output.result_cell,
            bytes: 8,
        };
        self.address(result);
        let returned_reference = self
            .plan
            .register_hints
            .get(&source.index())
            .is_some_and(|(returned, _)| self.plan.output_slots.get(*returned) == Some(&slot));
        match value.pointer_cell() {
            Some(PointerCell::Scratch(cell)) if returned_reference => {
                self.push(I::LocalGet(2));
                self.push(I::I32Const(cell as i32));
                self.push(I::I32Add);
                self.push(I::I64Load(CELL));
            }
            Some(PointerCell::Input(_)) if value != buffer => {
                self.address(value);
                self.push(I::I64ExtendI32U);
            }
            _ => {
                self.copy(buffer, value);
                self.address(buffer);
                self.push(I::I64ExtendI32U);
            }
        }
        self.push(I::I64Store(CELL));
    }
}
