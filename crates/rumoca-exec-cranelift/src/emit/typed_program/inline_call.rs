//! Calls lowered in place of the call: the callee's body runs on the
//! caller's argument and destination cells (see [`super::placement`]).

use super::*;

impl ProgramLowerer<'_, '_> {
    /// Lower `callee`'s body here with its inputs placed on the arguments and
    /// its outputs on the destinations when that placement is sound; otherwise
    /// it writes a private tape unpacked afterwards, as a call does.
    pub(super) fn lower_inline_call(
        &mut self,
        callee: &solve::SolvePureCallOwner,
        arguments: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
    ) -> Result<(), CompileError> {
        let program = callee.body();
        let shape = placement::Shape {
            body: program,
            inputs: arguments.len(),
            outputs: destinations.len(),
        };
        let inputs = self.placed_slots(arguments)?;
        let outputs = self.placed_slots(destinations)?;
        let input_cells = placement::slot_cells(&inputs);
        let output_cells = placement::slot_cells(&outputs);
        let mut layout = ProgramLayout::new(program, (shape.inputs, shape.outputs), &[])?;
        placement::relocate_updates_onto_placed(shape, &mut layout, &input_cells, &output_cells);
        let mut direct = placement::placement_is_safe(shape, &layout, &input_cells, &output_cells)?;
        if !direct {
            layout = ProgramLayout::new(program, (shape.inputs, shape.outputs), &[])?;
            direct = placement::placement_is_safe(shape, &layout, &input_cells, &output_cells)?;
        }
        let output_tape = if direct {
            None
        } else {
            let cells = self.register_cell_count(destinations, "inline call results")?;
            Some(create_tape(self.builder, self.pointer_type, cells)?)
        };
        let tape = create_tape(self.builder, self.pointer_type, layout.tape_cells)?;
        let table = self.table;
        let mut nested = ProgramLowerer {
            builder: self.builder,
            module: self.module,
            math: self.math,
            pointer_type: self.pointer_type,
            input: self.tape,
            output: output_tape.unwrap_or(self.tape),
            tape,
            layout: &layout,
            outputs: callee.outputs(),
            functions: self.functions,
            recursive_calls: None,
            flags: self.flags,
            observation: None,
            operation: std::ptr::null(),
            input_placement: Some(inputs),
            output_placement: if direct { Some(outputs) } else { None },
            table,
            inline_depth: self.inline_depth + 1,
        };
        nested.lower(program)?;
        if let Some(output_tape) = output_tape {
            self.unpack_registers(output_tape, destinations)?;
        }
        Ok(())
    }
}

/// The callee of a call that is lowered in place of the call: a
/// non-recursive owner that publishes no predicates and asserts nothing,
/// below a size that keeps the caller's code bounded.
pub(super) fn inline_callee<'t>(
    table: &'t solve::SolvePureCallTable,
    owner: solve::SolvePureCallOwnerId,
    arguments: &[solve::SolveRegisterId],
    destinations: &[solve::SolveRegisterId],
) -> Option<&'t solve::SolvePureCallOwner> {
    if table.recursive_group(owner).is_some() {
        return None;
    }
    let callee = table.owner(owner)?;
    let body = callee.body();
    let asserts = body.operations().iter().any(|operation| {
        matches!(
            operation.operation(),
            solve::SolveOperation::CheckAssertion { .. }
                | solve::SolveOperation::AssignedState { .. }
                | solve::SolveOperation::AssignedConditional { .. }
        )
    });
    (!asserts
        && !publishes_predicates(callee)
        && body.operations().len() <= 4000
        && body.assigned_state_inputs().is_empty()
        && body.assigned_state_outputs().is_empty()
        && callee.inputs().len() == arguments.len()
        && callee.outputs().len() == destinations.len())
    .then_some(callee)
}
