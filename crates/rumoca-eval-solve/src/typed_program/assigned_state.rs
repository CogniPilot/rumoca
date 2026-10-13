//! Internal assigned state has no payload on an absent path.

use super::*;
use rumoca_ir_solve::{
    SolveAssignedConditional, SolveAssignedStateId, SolveAssignedStateOperation,
};

impl<'model> EvalFrame<'model, '_> {
    pub(super) fn read_assigned(
        &self,
        id: SolveAssignedStateId,
        at: Span,
    ) -> Result<&Option<TypedValue>, TypedProgramEvalError> {
        self.storage
            .assigned_states
            .get(id.index())
            .and_then(Option::as_ref)
            .ok_or_else(|| invalid_error("read internal assigned state", at))
    }

    fn write_assigned(
        &mut self,
        id: SolveAssignedStateId,
        value: Option<TypedValue>,
        at: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let expected = self
            .program
            .assigned_state_types()
            .get(id.index())
            .ok_or_else(|| invalid_error("write internal assigned state", at))?;
        if value
            .as_ref()
            .is_some_and(|value| value.value_type() != expected)
        {
            return invalid("internal assigned-state payload type", at);
        }
        let destination = self
            .storage
            .assigned_states
            .get_mut(id.index())
            .ok_or_else(|| invalid_error("write internal assigned state", at))?;
        if destination.is_some() {
            return invalid("duplicate internal assigned-state write", at);
        }
        *destination = Some(value);
        Ok(())
    }

    pub(super) fn eval_assigned_state(
        &mut self,
        operation: &SolveAssignedStateOperation,
        at: Span,
    ) -> Result<(), TypedProgramEvalError> {
        match operation {
            SolveAssignedStateOperation::Absent { destination, .. } => {
                self.write_assigned(*destination, None, at)
            }
            SolveAssignedStateOperation::Present {
                destination,
                source,
            } => {
                let value = self.read_owned(*source, at)?;
                self.write_assigned(*destination, Some(value), at)
            }
            SolveAssignedStateOperation::Input { destination, input } => {
                let value = self
                    .storage
                    .assigned_inputs
                    .get(*input)
                    .cloned()
                    .ok_or_else(|| invalid_error("load internal assigned-state input", at))?;
                self.write_assigned(*destination, value, at)
            }
            SolveAssignedStateOperation::Output { output, source } => {
                let value = self.read_assigned(*source, at)?.clone();
                let destination = self
                    .storage
                    .assigned_outputs
                    .get_mut(*output)
                    .ok_or_else(|| invalid_error("store internal assigned-state output", at))?;
                if destination.is_some() {
                    return invalid("duplicate internal assigned-state output", at);
                }
                *destination = Some(value);
                Ok(())
            }
            SolveAssignedStateOperation::IsPresent {
                destination,
                source,
            } => {
                let present = self.read_assigned(*source, at)?.is_some();
                self.write(
                    *destination,
                    TypedValue::scalar(&SolveValue::boolean(present)),
                    at,
                )
            }
            SolveAssignedStateOperation::AllPresent {
                destination,
                sources,
            } => {
                let mut present = true;
                for source in sources {
                    present &= self.read_assigned(*source, at)?.is_some();
                }
                self.write(
                    *destination,
                    TypedValue::scalar(&SolveValue::boolean(present)),
                    at,
                )
            }
            SolveAssignedStateOperation::ProjectChecked {
                destination,
                source,
                ..
            } => {
                let value = match self.read_assigned(*source, at)?.clone() {
                    Some(value) => value,
                    // A value row publishes the neutral value of a read the
                    // preceding presence check proved absent; the scheduled
                    // assertion owner raises the authored failure.
                    None if self.mode.policy == AssertionPolicy::Value => {
                        self.neutral_value(*destination, at)?
                    }
                    None => return invalid("checked assigned-state projection", at),
                };
                self.write(*destination, value, at)
            }
        }
    }

    pub(super) fn eval_assigned_conditional(
        &mut self,
        program: &'model SolveAssignedConditional,
        at: Span,
    ) -> Result<(), TypedProgramEvalError> {
        let selected = if scalar_boolean(self.read(program.condition(), at)?, at)? {
            program.if_true()
        } else {
            program.if_false()
        };
        let captures = program
            .captures()
            .iter()
            .map(|capture| self.read_owned(*capture, at))
            .collect::<Result<Vec<_>, _>>()?;
        let states = program
            .state_captures()
            .iter()
            .map(|capture| self.read_assigned(*capture, at).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        let completion = eval_region_assigned(
            self.table,
            selected,
            (captures, states),
            &mut FrameStorage::default(),
            self.mode,
            self.chain,
            self.owner,
        )?;
        let Some(values) = self.accept_completion(completion) else {
            return Ok(());
        };
        if values.normal.len() != program.destinations().len()
            || values.states.len() != program.state_destinations().len()
        {
            return invalid("transfer internal assigned conditional results", at);
        }
        for (destination, value) in program.destinations().iter().zip(values.normal) {
            self.write(*destination, value, at)?;
        }
        for (destination, value) in program.state_destinations().iter().zip(values.states) {
            self.write_assigned(*destination, value, at)?;
        }
        Ok(())
    }
}

impl EvalFrame<'_, '_> {
    /// The typed zero of one register, the value of an absent checked read in a
    /// value row.
    fn neutral_value(
        &self,
        register: SolveRegisterId,
        at: Span,
    ) -> Result<TypedValue, TypedProgramEvalError> {
        let value_type = self.destination_type(register, at)?.clone();
        let neutral = SolveValue::inactive_assertion_message(value_type.element_type()).kind();
        let elements = vec![neutral; value_type.scalar_count() as usize];
        TypedValue::construct(value_type, elements)
            .map_err(|_| invalid_error("neutral value of an absent checked read", at))
    }
}
