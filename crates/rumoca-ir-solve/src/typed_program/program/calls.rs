//! One canonical call emitter and branded source assertion forwarding.

use super::*;
use crate::typed_program::call::assertion_message_outputs;

/// An exact call operation emitted in this construction context.
#[derive(Debug)]
pub struct ProgramCall<'program> {
    pub(super) operation: usize,
    pub(super) owner: SolvePureCallOwnerId,
    pub(super) registers: Vec<ProgramRegister<'program>>,
    pub(super) marker: PhantomData<&'program mut &'program ()>,
}

impl<'program> ProgramCall<'program> {
    #[must_use]
    pub fn registers(&self) -> &[ProgramRegister<'program>] {
        &self.registers
    }

    #[must_use]
    pub fn into_registers(self) -> Vec<ProgramRegister<'program>> {
        self.registers
    }
}

/// Source-issued child predicate to enclosing-owner predicate association.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub struct SolveAssertionForwarding {
    child_predicate: usize,
    parent_predicate: usize,
}

impl SolveAssertionForwarding {
    #[must_use]
    pub const fn child_predicate(&self) -> usize {
        self.child_predicate
    }
    #[must_use]
    pub const fn parent_predicate(&self) -> usize {
        self.parent_predicate
    }
}

impl<'program> TypedProgramBuilder<'program> {
    pub fn forward_assertion(
        &mut self,
        call: &ProgramCall<'program>,
        child_predicate: usize,
        parent: ProgramAssertion<'program>,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let child = self
            .available_calls
            .get(call.owner.index() as usize)
            .filter(|interface| interface.id == call.owner)
            .ok_or(SolveProgramConstructionError::UnknownCallOwner { provenance })?;
        let parent_predicate = parent.output;
        let level = child
            .outputs
            .get(child_predicate)
            .and_then(SolvePureCallOutput::assertion_level);
        if level.is_none()
            || self
                .assertion_outputs
                .get(parent_predicate)
                .and_then(SolvePureCallOutput::assertion_level)
                != level
        {
            return Err(SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        let child_messages = assertion_message_outputs(child.outputs, child_predicate);
        let parent_messages = assertion_message_outputs(self.assertion_outputs, parent_predicate);
        if child_messages.len() != parent_messages.len()
            || child_messages
                .iter()
                .zip(&parent_messages)
                .any(|(child_output, parent_output)| {
                    child.outputs[*child_output].value_type()
                        != self.assertion_outputs[*parent_output].value_type()
                })
        {
            return Err(SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        let Some(operation) = self.operations.get_mut(call.operation) else {
            return Err(SolveProgramConstructionError::InvalidCallOutput { provenance });
        };
        let SolveOperation::Call {
            owner,
            destinations,
            assertion_forwarding,
            ..
        } = &mut operation.operation
        else {
            return Err(SolveProgramConstructionError::InvalidCallOutput { provenance });
        };
        if *owner != call.owner
            || destinations.len() != call.registers.len()
            || !destinations
                .iter()
                .zip(&call.registers)
                .all(|(destination, register)| *destination == register.id)
            || assertion_forwarding.iter().any(|forwarding| {
                forwarding.child_predicate == child_predicate
                    || forwarding.parent_predicate == parent_predicate
            })
        {
            return Err(SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        let mut forwarding = assertion_forwarding.to_vec();
        forwarding.push(SolveAssertionForwarding {
            child_predicate,
            parent_predicate,
        });
        *assertion_forwarding = forwarding.into_boxed_slice();
        Ok(())
    }
}

#[cfg(test)]
mod tests;
