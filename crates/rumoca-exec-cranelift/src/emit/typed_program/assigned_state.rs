//! Internal optional payload transport, separate from normal function results.

use super::{CompileError, ProgramLowerer, StorageBase, ValueLocation, checked_cells, create_tape};
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{InstBuilder, Value, types};
use rumoca_ir_solve as solve;

#[derive(Clone)]
pub(super) struct AssignedLocation {
    presence: ValueLocation,
    payload: ValueLocation,
}

pub(super) fn layout(
    value_types: &[solve::SolveValueType],
    base: StorageBase,
    cursor: &mut u32,
) -> Result<Box<[AssignedLocation]>, CompileError> {
    let mut states = Vec::with_capacity(value_types.len());
    for value_type in value_types {
        let presence = ValueLocation {
            base,
            cell: *cursor,
            value_type: solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
        };
        *cursor = checked_cells(*cursor, 1, "assigned-state presence")?;
        let payload = ValueLocation {
            base,
            cell: *cursor,
            value_type: value_type.clone(),
        };
        *cursor = checked_cells(*cursor, value_type.scalar_count(), "assigned-state payload")?;
        states.push(AssignedLocation { presence, payload });
    }
    Ok(states.into_boxed_slice())
}

impl ProgramLowerer<'_, '_> {
    pub(super) fn lower_assigned_state(
        &mut self,
        operation: &solve::SolveAssignedStateOperation,
    ) -> Result<(), CompileError> {
        use solve::SolveAssignedStateOperation as O;
        let zero = self.builder.ins().iconst(types::I64, 0);
        match operation {
            O::Absent { destination, .. } => {
                let state = self.layout.assigned_states[destination.index()].clone();
                self.store_scalar(&state.presence, zero, zero)
            }
            O::Present {
                destination,
                source,
            } => {
                let state = self.layout.assigned_states[destination.index()].clone();
                let source = self.register(*source)?.clone();
                self.copy(&source, &state.payload)?;
                let one = self.builder.ins().iconst(types::I64, 1);
                self.store_scalar(&state.presence, zero, one)
            }
            O::Input { destination, input } => {
                let source = self.layout.assigned_inputs[*input].clone();
                let destination = self.layout.assigned_states[destination.index()].clone();
                self.copy_assigned(&source, &destination)
            }
            O::Output { output, source } => {
                let source = self.layout.assigned_states[source.index()].clone();
                let destination = self.layout.assigned_outputs[*output].clone();
                self.copy_assigned(&source, &destination)
            }
            O::IsPresent {
                destination,
                source,
            } => {
                let source = self.layout.assigned_states[source.index()].presence.clone();
                let destination = self.register(*destination)?.clone();
                self.copy(&source, &destination)
            }
            O::AllPresent {
                destination,
                sources,
            } => {
                let mut present = self.builder.ins().iconst(types::I64, 1);
                for source in sources {
                    let source = self.layout.assigned_states[source.index()].presence.clone();
                    let value = self.load_scalar(&source, zero)?;
                    present = self.builder.ins().band(present, value);
                }
                let destination = self.register(*destination)?.clone();
                self.store_scalar(&destination, zero, present)
            }
            O::ProjectChecked {
                destination,
                source,
                ..
            } => {
                let source = self.layout.assigned_states[source.index()].payload.clone();
                let destination = self.register(*destination)?.clone();
                self.copy(&source, &destination)
            }
        }
    }

    fn copy_assigned(
        &mut self,
        source: &AssignedLocation,
        destination: &AssignedLocation,
    ) -> Result<(), CompileError> {
        self.copy_assigned_between(
            source,
            destination,
            self.base(&source.presence),
            self.base(&destination.presence),
        )
    }

    pub(super) fn lower_assigned_conditional(
        &mut self,
        conditional: &solve::SolveAssignedConditional,
    ) -> Result<(), CompileError> {
        let normal_inputs =
            self.register_cell_count(conditional.captures(), "assigned captures")?;
        let normal_outputs =
            self.register_cell_count(conditional.destinations(), "assigned results")?;
        let mut input_cells = normal_inputs;
        let input_states = layout(
            conditional.if_true().body().assigned_state_inputs(),
            StorageBase::Input,
            &mut input_cells,
        )?;
        let mut output_cells = normal_outputs;
        let output_states = layout(
            conditional.if_true().body().assigned_state_outputs(),
            StorageBase::Output,
            &mut output_cells,
        )?;
        let input = create_tape(self.builder, self.pointer_type, input_cells)?;
        let output = create_tape(self.builder, self.pointer_type, output_cells)?;
        self.pack_registers(conditional.captures(), input)?;
        for (source, destination) in conditional.state_captures().iter().zip(&input_states) {
            let source = self.layout.assigned_states[source.index()].clone();
            self.copy_assigned_to(&source, destination, input)?;
        }
        let condition = self.register(conditional.condition())?.clone();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let condition = self.load_scalar(&condition, zero)?;
        let selected = self.builder.ins().icmp_imm(IntCC::NotEqual, condition, 0);
        let yes = self.builder.create_block();
        let no = self.builder.create_block();
        let join = self.builder.create_block();
        self.builder.ins().brif(selected, yes, &[], no, &[]);
        self.builder.switch_to_block(yes);
        self.builder.seal_block(yes);
        self.lower_region(conditional.if_true(), input, output)?;
        self.builder.ins().jump(join, &[]);
        self.builder.switch_to_block(no);
        self.builder.seal_block(no);
        self.lower_region(conditional.if_false(), input, output)?;
        self.builder.ins().jump(join, &[]);
        self.builder.switch_to_block(join);
        self.builder.seal_block(join);
        self.unpack_registers(output, conditional.destinations())?;
        for (source, destination) in output_states.iter().zip(conditional.state_destinations()) {
            let destination = self.layout.assigned_states[destination.index()].clone();
            self.copy_assigned_between(
                source,
                &destination,
                output,
                self.base(&destination.presence),
            )?;
        }
        Ok(())
    }

    pub(super) fn pack_assigned_inputs(
        &mut self,
        captures: &[solve::SolveAssignedStateId],
        input_types: &[solve::SolveValueType],
        pointer: Value,
        start: u32,
    ) -> Result<(), CompileError> {
        let mut cursor = start;
        let locations = layout(input_types, StorageBase::Input, &mut cursor)?;
        for (source, destination) in captures.iter().zip(&locations) {
            let source = self.layout.assigned_states[source.index()].clone();
            self.copy_assigned_to(&source, destination, pointer)?;
        }
        Ok(())
    }

    pub(super) fn copy_frame_cells(
        &mut self,
        source: Value,
        destination: Value,
        count: u32,
    ) -> Result<(), CompileError> {
        self.for_each_element(count, |this, index| {
            let from = this.cell_address(source, 0, index);
            let to = this.cell_address(destination, 0, index);
            let value = this.builder.ins().load(types::I64, this.flags, from, 0);
            this.builder.ins().store(this.flags, value, to, 0);
            Ok(())
        })
    }

    pub(super) fn assigned_input_cells(
        &self,
        types: &[solve::SolveValueType],
        start: u32,
    ) -> Result<u32, CompileError> {
        types.iter().try_fold(start, |cursor, value_type| {
            let cursor = checked_cells(cursor, 1, "assigned capture presence")?;
            checked_cells(
                cursor,
                value_type.scalar_count(),
                "assigned capture payload",
            )
        })
    }

    fn copy_assigned_to(
        &mut self,
        source: &AssignedLocation,
        destination: &AssignedLocation,
        pointer: Value,
    ) -> Result<(), CompileError> {
        self.copy_assigned_between(source, destination, self.base(&source.presence), pointer)
    }

    fn copy_assigned_between(
        &mut self,
        source: &AssignedLocation,
        destination: &AssignedLocation,
        source_base: Value,
        destination_base: Value,
    ) -> Result<(), CompileError> {
        let zero = self.builder.ins().iconst(types::I64, 0);
        let flag = self.load_scalar_from(&source.presence, source_base, zero)?;
        self.store_scalar_to(&destination.presence, destination_base, zero, flag)?;
        let present = self.builder.ins().icmp_imm(IntCC::NotEqual, flag, 0);
        let copy = self.builder.create_block();
        let done = self.builder.create_block();
        self.builder.ins().brif(present, copy, &[], done, &[]);
        self.builder.switch_to_block(copy);
        self.builder.seal_block(copy);
        self.copy_between_bases(
            &source.payload,
            &destination.payload,
            source_base,
            destination_base,
        )?;
        self.builder.ins().jump(done, &[]);
        self.builder.switch_to_block(done);
        self.builder.seal_block(done);
        Ok(())
    }
}
