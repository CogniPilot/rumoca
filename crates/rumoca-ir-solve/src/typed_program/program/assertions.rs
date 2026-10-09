//! Assertion checks borrow the issuing owner's canonical output interface.

use super::*;

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum SolveAssertionMessage {
    NoCaptures,
    Captures { program: Box<SolveProgramRegion> },
}

/// One predicate output of this exact construction context.
#[derive(Debug, Clone, Copy)]
pub struct ProgramAssertion<'program> {
    pub(super) output: usize,
    marker: PhantomData<&'program mut &'program ()>,
}

impl<'program> TypedProgramBuilder<'program> {
    pub fn assertion_output(
        &self,
        output: usize,
        provenance: Span,
    ) -> Result<ProgramAssertion<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        if self
            .assertion_outputs
            .get(output)
            .and_then(SolvePureCallOutput::assertion_level)
            .is_none()
        {
            return Err(SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        Ok(ProgramAssertion {
            output,
            marker: PhantomData,
        })
    }

    pub fn check_assertion(
        &mut self,
        assertion: ProgramAssertion<'program>,
        condition: ProgramRegister<'program>,
        captures: &[ProgramRegister<'program>],
        provenance: Span,
        build: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let inputs = self.register_types_for(captures, provenance)?;
        let outputs = self.message_types(assertion.output);
        let message = if outputs.is_empty() {
            let body = TypedProgram::construct_with_owner(
                self.arithmetic,
                self.available_calls,
                self.assertion_outputs,
                |builder| build(builder, &[], &[]),
            )?;
            if !inputs.is_empty() || !body.slots().is_empty() || !body.operations().is_empty() {
                return Err(SolveProgramConstructionError::InvalidCallOutput { provenance });
            }
            SolveAssertionMessage::NoCaptures
        } else {
            SolveAssertionMessage::Captures {
                program: Box::new(self.build_region(inputs, outputs, provenance, build)?),
            }
        };
        self.check_assertion_from_region(assertion, condition, captures, message, provenance)
    }

    pub(super) fn check_assertion_from_region(
        &mut self,
        assertion: ProgramAssertion<'program>,
        condition: ProgramRegister<'program>,
        captures: &[ProgramRegister<'program>],
        message: SolveAssertionMessage,
        provenance: Span,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        let output_types = self.message_types(assertion.output);
        let inputs = self.register_types_for(captures, provenance)?;
        let invalid_message = match &message {
            SolveAssertionMessage::NoCaptures => !inputs.is_empty() || !output_types.is_empty(),
            SolveAssertionMessage::Captures { program } => {
                program.inputs.as_ref() != inputs
                    || program.outputs.as_ref() != output_types
                    || program.body.arithmetic() != self.arithmetic
            }
        };
        if self.register_type(condition, provenance)?
            != &SolveValueType::scalar(SolveScalarType::Boolean)
            || invalid_message
        {
            return Err(SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        let destinations = output_types
            .iter()
            .map(|value_type| self.issue_register(value_type.clone(), provenance))
            .collect::<Result<Vec<_>, _>>()?;
        self.push(
            SolveOperation::CheckAssertion {
                predicate_output: assertion.output,
                message_outputs: self.message_outputs(assertion.output).into_boxed_slice(),
                condition: condition.id,
                captures: captures.iter().map(|capture| capture.id).collect(),
                destinations: destinations
                    .iter()
                    .map(|destination| destination.id)
                    .collect(),
                message,
            },
            provenance,
        );
        Ok(destinations)
    }

    fn message_types(&self, predicate: usize) -> Vec<SolveValueType> {
        self.message_outputs(predicate)
            .into_iter()
            .map(|index| self.assertion_outputs[index].value_type().clone())
            .collect()
    }

    pub(super) fn message_outputs(&self, predicate: usize) -> Vec<usize> {
        crate::typed_program::call::assertion_message_outputs(self.assertion_outputs, predicate)
    }
}
