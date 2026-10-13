//! Source-ordered assertions inside native typed owner bodies.
//!
//! A value row (the primal form) publishes the inactive message tuple for a
//! passing condition and evaluates its message region for a failing one, then
//! continues at every level: native owners are value rows, and the scheduled
//! assertion owner alone raises the authored failure.
//!
//! The observation form of an owner answers a different question (SOLVE-C25):
//! which of the owner's assertions fail before the first failed Error-level
//! one. It keeps every predicate cell eagerly, clearing the cell of each
//! failed assertion it executes, and stops at the first failed Error-level
//! assertion, so no later operation is evaluated and no later fault can
//! replace the assertion outcome.

use super::{
    CompileError, ProgramLowerer, StorageBase, ValueLocation, observation::ObservationLowering,
    status,
};
use cranelift_codegen::ir::{InstBuilder, Value, types};
use rumoca_ir_solve as solve;

impl ProgramLowerer<'_, '_> {
    pub(super) fn lower_assertion(
        &mut self,
        (predicate_output, condition): (usize, solve::SolveRegisterId),
        (captures, destinations): (&[solve::SolveRegisterId], &[solve::SolveRegisterId]),
        message: &solve::SolveAssertionMessage,
    ) -> Result<(), CompileError> {
        let condition = self.register(condition)?.clone();
        let zero = self.builder.ins().iconst(types::I64, 0);
        let holds = self.load_scalar(&condition, zero)?;
        let failed = self.builder.create_block();
        let passed = self.builder.create_block();
        let continuation = self.builder.create_block();
        self.builder.ins().brif(holds, passed, &[], failed, &[]);

        self.builder.switch_to_block(failed);
        self.builder.seal_block(failed);
        if let solve::SolveAssertionMessage::Captures { program } = message {
            let input = self.packed_tape(captures, "assertion message captures")?;
            let cells = self.register_cell_count(destinations, "assertion message results")?;
            let output = super::create_tape(self.builder, self.pointer_type, cells)?;
            self.lower_region(program, input, output)?;
            self.unpack_registers(output, destinations)?;
        }
        if self.observation.is_some() {
            self.clear_predicate(predicate_output)?;
        }
        if self.stops_at(predicate_output) {
            status::stop(self.builder);
        } else {
            self.builder.ins().jump(continuation, &[]);
        }

        self.builder.switch_to_block(passed);
        self.builder.seal_block(passed);
        for destination in destinations {
            let destination = self.register(*destination)?.clone();
            let inactive = solve::SolveValue::inactive_assertion_message(
                destination.value_type.element_type(),
            );
            let scalar = self.scalar_constant(&inactive);
            self.for_each_element(destination.value_type.scalar_count(), |this, index| {
                this.store_scalar(&destination, index, scalar)
            })?;
        }
        self.builder.ins().jump(continuation, &[]);

        self.builder.switch_to_block(continuation);
        self.builder.seal_block(continuation);
        Ok(())
    }

    /// Whether a failed assertion on `predicate_output` ends this form.
    fn stops_at(&self, predicate_output: usize) -> bool {
        self.observation.is_some()
            && self.outputs.get(predicate_output).is_some_and(|output| {
                output.assertion_level() == Some(solve::SolveAssertionLevel::Error)
            })
    }

    /// Every predicate cell starts as passed; only an executed failed
    /// assertion clears it.
    pub(super) fn initialize_observation(&mut self) -> Result<(), CompileError> {
        let Some(observation) = self.observation.clone() else {
            return Ok(());
        };
        for output in 0..observation.cells.len() {
            self.write_predicate(&observation, output, true)?;
        }
        Ok(())
    }

    fn clear_predicate(&mut self, predicate_output: usize) -> Result<(), CompileError> {
        let observation = self.observation_lowering()?;
        self.write_predicate(&observation, predicate_output, false)
    }

    fn observation_lowering(&self) -> Result<ObservationLowering, CompileError> {
        self.observation
            .clone()
            .ok_or_else(|| CompileError::Backend("observation form state is missing".into()))
    }

    fn predicate_location(
        observation: &ObservationLowering,
        predicate_output: usize,
    ) -> Result<Option<ValueLocation>, CompileError> {
        let cell = observation
            .cells
            .get(predicate_output)
            .ok_or_else(|| CompileError::Backend("assertion predicate output is missing".into()))?;
        Ok(cell.map(|cell| ValueLocation {
            base: StorageBase::Output,
            cell,
            value_type: solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
        }))
    }

    fn write_predicate(
        &mut self,
        observation: &ObservationLowering,
        predicate_output: usize,
        holds: bool,
    ) -> Result<(), CompileError> {
        let Some(location) = Self::predicate_location(observation, predicate_output)? else {
            return Ok(());
        };
        let zero = self.builder.ins().iconst(types::I64, 0);
        let value = self.builder.ins().iconst(types::I64, i64::from(holds));
        self.store_scalar_to(&location, observation.owner_output, zero, value)
    }

    /// Call `owner`'s observation form: forward each child predicate into this
    /// owner's predicate cell, then stop here when the child stopped.
    pub(super) fn observe_call(
        &mut self,
        owner: solve::SolvePureCallOwnerId,
        arguments: &[solve::SolveRegisterId],
        destinations: &[solve::SolveRegisterId],
        forwarding: &[solve::SolveAssertionForwarding],
        output: Value,
    ) -> Result<(), CompileError> {
        let status = self.emit_owner_call(owner, arguments, output)?;
        let (stopped, failure) = status::separate_stop(self.builder, status);
        status::propagate(self.builder, failure);
        let observation = self.observation_lowering()?;
        for forwarded in forwarding {
            self.forward_predicate(&observation, destinations, forwarded, output)?;
        }
        let exit = self.builder.create_block();
        let continuation = self.builder.create_block();
        self.builder
            .ins()
            .brif(stopped, exit, &[], continuation, &[]);
        self.builder.switch_to_block(exit);
        self.builder.seal_block(exit);
        status::stop(self.builder);
        self.builder.switch_to_block(continuation);
        self.builder.seal_block(continuation);
        Ok(())
    }

    /// The parent predicate fails exactly when it or the child's did.
    fn forward_predicate(
        &mut self,
        observation: &ObservationLowering,
        destinations: &[solve::SolveRegisterId],
        forwarding: &solve::SolveAssertionForwarding,
        child_output: Value,
    ) -> Result<(), CompileError> {
        let Some(parent) = Self::predicate_location(observation, forwarding.parent_predicate())?
        else {
            return Err(CompileError::Backend(
                "forwarded assertion has no parent predicate".into(),
            ));
        };
        let mut child_cell = 0u32;
        for register in destinations.iter().take(forwarding.child_predicate()) {
            child_cell = super::checked_cells(
                child_cell,
                self.register(*register)?.value_type.scalar_count(),
                "forwarded predicate layout",
            )?;
        }
        let child = ValueLocation {
            base: StorageBase::Tape,
            cell: child_cell,
            value_type: parent.value_type.clone(),
        };
        let zero = self.builder.ins().iconst(types::I64, 0);
        let child_holds = self.load_scalar_from(&child, child_output, zero)?;
        let parent_holds = self.load_scalar_from(&parent, observation.owner_output, zero)?;
        let holds = self.builder.ins().band(parent_holds, child_holds);
        self.store_scalar_to(&parent, observation.owner_output, zero, holds)
    }
}
