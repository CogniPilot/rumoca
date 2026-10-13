//! Constructor-issued internal assigned state, separate from the value ABI.

mod conditional;
mod fold;

use super::*;
use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SolveAssignedStateId(u32);

impl SolveAssignedStateId {
    #[must_use]
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ProgramAssignedState<'program> {
    pub(super) id: SolveAssignedStateId,
    marker: PhantomData<&'program mut &'program ()>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "state", rename_all = "snake_case")]
pub enum SolveAssignedStateOperation {
    Absent {
        destination: SolveAssignedStateId,
        payload_type: SolveValueType,
    },
    Present {
        destination: SolveAssignedStateId,
        source: SolveRegisterId,
    },
    Input {
        destination: SolveAssignedStateId,
        input: usize,
    },
    Output {
        output: usize,
        source: SolveAssignedStateId,
    },
    IsPresent {
        destination: SolveRegisterId,
        source: SolveAssignedStateId,
    },
    AllPresent {
        destination: SolveRegisterId,
        sources: Box<[SolveAssignedStateId]>,
    },
    /// A preceding checked error-level assertion tested this exact state.
    ProjectChecked {
        destination: SolveRegisterId,
        source: SolveAssignedStateId,
        predicate_output: usize,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Default)]
pub(super) struct AssignedStateLayout {
    pub(super) types: Vec<SolveValueType>,
    pub(super) inputs: Vec<SolveValueType>,
    pub(super) outputs: Vec<SolveValueType>,
    #[serde(skip)]
    assigned_read_checks: Vec<(SolveRegisterId, usize)>,
}

#[derive(Default)]
pub(super) struct AssignedStateConstruction {
    pub(super) layout: AssignedStateLayout,
    output_written: Vec<bool>,
    presence: Vec<Option<Box<[SolveAssignedStateId]>>>,
    checked: HashSet<(SolveAssignedStateId, usize)>,
}

impl AssignedStateConstruction {
    pub(super) fn interface(inputs: Vec<SolveValueType>, outputs: Vec<SolveValueType>) -> Self {
        let output_written = vec![false; outputs.len()];
        Self {
            layout: AssignedStateLayout {
                types: Vec::new(),
                inputs,
                outputs,
                ..AssignedStateLayout::default()
            },
            output_written,
            ..Self::default()
        }
    }

    pub(super) fn complete(
        &self,
        provenance: Option<Span>,
    ) -> Result<(), SolveProgramConstructionError> {
        if self.output_written.iter().any(|written| !written) {
            let provenance = provenance
                .filter(|span| !span.is_dummy())
                .ok_or(SolveProgramConstructionError::MissingProvenance)?;
            return Err(SolveProgramConstructionError::InvalidRegion { provenance });
        }
        Ok(())
    }

    pub(super) fn checked_assertion(
        &mut self,
        condition: SolveRegisterId,
        predicate: usize,
        capture_free: bool,
    ) {
        if let Some(states) = self
            .presence
            .get(condition.index())
            .and_then(Option::as_ref)
        {
            self.checked
                .extend(states.iter().map(|state| (*state, predicate)));
            if capture_free && !states.is_empty() {
                self.layout
                    .assigned_read_checks
                    .push((condition, predicate));
            }
        }
    }
}

impl<'program> TypedProgramBuilder<'program> {
    pub(super) fn replay_assigned_state(
        &self,
        id: SolveAssignedStateId,
        provenance: Span,
    ) -> Result<ProgramAssignedState<'program>, SolveProgramConstructionError> {
        let state = ProgramAssignedState {
            id,
            marker: PhantomData,
        };
        self.assigned_state_type(state, provenance)?;
        Ok(state)
    }

    fn issue_assigned_state(
        &mut self,
        payload_type: SolveValueType,
        provenance: Span,
    ) -> Result<ProgramAssignedState<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        if !payload_type.belongs_to(self.arithmetic) {
            return Err(SolveProgramConstructionError::ProfileMismatch { provenance });
        }
        let id = SolveAssignedStateId(checked_ordinal(
            self.assigned_states.layout.types.len(),
            provenance,
        )?);
        self.assigned_states.layout.types.push(payload_type);
        Ok(ProgramAssignedState {
            id,
            marker: PhantomData,
        })
    }

    pub fn absent_assigned_state(
        &mut self,
        payload_type: SolveValueType,
        provenance: Span,
    ) -> Result<ProgramAssignedState<'program>, SolveProgramConstructionError> {
        let destination = self.issue_assigned_state(payload_type.clone(), provenance)?;
        self.push(
            SolveOperation::AssignedState {
                operation: SolveAssignedStateOperation::Absent {
                    destination: destination.id,
                    payload_type,
                },
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn present_assigned_state(
        &mut self,
        source: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramAssignedState<'program>, SolveProgramConstructionError> {
        let payload_type = self.register_type(source, provenance)?.clone();
        let destination = self.issue_assigned_state(payload_type, provenance)?;
        self.push(
            SolveOperation::AssignedState {
                operation: SolveAssignedStateOperation::Present {
                    destination: destination.id,
                    source: source.id,
                },
            },
            provenance,
        );
        Ok(destination)
    }

    pub(super) fn assigned_input(
        &mut self,
        input: usize,
        provenance: Span,
    ) -> Result<ProgramAssignedState<'program>, SolveProgramConstructionError> {
        let payload_type = self
            .assigned_states
            .layout
            .inputs
            .get(input)
            .cloned()
            .ok_or(SolveProgramConstructionError::InvalidRegion { provenance })?;
        let destination = self.issue_assigned_state(payload_type, provenance)?;
        self.push(
            SolveOperation::AssignedState {
                operation: SolveAssignedStateOperation::Input {
                    destination: destination.id,
                    input,
                },
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn store_assigned_state(
        &mut self,
        output: usize,
        source: ProgramAssignedState<'program>,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let payload_type = self.assigned_state_type(source, provenance)?;
        if self.assigned_states.layout.outputs.get(output) != Some(payload_type)
            || self.assigned_states.output_written.get(output) != Some(&false)
        {
            return Err(SolveProgramConstructionError::InvalidRegion { provenance });
        }
        self.assigned_states.output_written[output] = true;
        self.push(
            SolveOperation::AssignedState {
                operation: SolveAssignedStateOperation::Output {
                    output,
                    source: source.id,
                },
            },
            provenance,
        );
        Ok(())
    }

    pub(super) fn assigned_state_type(
        &self,
        state: ProgramAssignedState<'program>,
        provenance: Span,
    ) -> Result<&SolveValueType, SolveProgramConstructionError> {
        self.assigned_states
            .layout
            .types
            .get(state.id.index())
            .ok_or(SolveProgramConstructionError::UnknownRegister { provenance })
    }

    pub fn assigned_state_is_present(
        &mut self,
        source: ProgramAssignedState<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        self.assigned_state_type(source, provenance)?;
        let destination =
            self.issue_register(SolveValueType::scalar(SolveScalarType::Boolean), provenance)?;
        self.assigned_states
            .presence
            .resize(self.register_types.len(), None);
        self.assigned_states.presence[destination.id.index()] = Some(Box::new([source.id]));
        self.push(
            SolveOperation::AssignedState {
                operation: SolveAssignedStateOperation::IsPresent {
                    destination: destination.id,
                    source: source.id,
                },
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn assigned_states_all_present(
        &mut self,
        sources: &[ProgramAssignedState<'program>],
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        if sources.is_empty() {
            return Err(SolveProgramConstructionError::InvalidProjection { provenance });
        }
        for source in sources {
            self.assigned_state_type(*source, provenance)?;
        }
        let destination =
            self.issue_register(SolveValueType::scalar(SolveScalarType::Boolean), provenance)?;
        let sources: Box<[_]> = sources.iter().map(|source| source.id).collect();
        self.assigned_states
            .presence
            .resize(self.register_types.len(), None);
        self.assigned_states.presence[destination.id.index()] = Some(sources.clone());
        self.push(
            SolveOperation::AssignedState {
                operation: SolveAssignedStateOperation::AllPresent {
                    destination: destination.id,
                    sources,
                },
            },
            provenance,
        );
        Ok(destination)
    }

    pub fn require_assigned_states(
        &mut self,
        assertion: ProgramAssertion<'program>,
        states: &[ProgramAssignedState<'program>],
        provenance: Span,
    ) -> Result<
        (ProgramRegister<'program>, Vec<ProgramRegister<'program>>),
        SolveProgramConstructionError,
    > {
        if self
            .assertion_outputs
            .get(assertion.output)
            .and_then(SolvePureCallOutput::assertion_level)
            != Some(crate::typed_program::call::SolveAssertionLevel::Error)
        {
            return Err(SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        let condition = self.assigned_states_all_present(states, provenance)?;
        self.check_assertion(assertion, condition, &[], provenance, |_, _, _| Ok(()))?;
        let values = states
            .iter()
            .map(|state| self.project_checked_assigned_state(*state, assertion.output, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        Ok((condition, values))
    }

    pub fn require_assigned_state(
        &mut self,
        assertion: ProgramAssertion<'program>,
        state: ProgramAssignedState<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        let (_, values) = self.require_assigned_states(assertion, &[state], provenance)?;
        Ok(values[0])
    }

    pub(super) fn project_checked_assigned_state(
        &mut self,
        source: ProgramAssignedState<'program>,
        predicate_output: usize,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let payload_type = self.assigned_state_type(source, provenance)?.clone();
        if !self
            .assigned_states
            .checked
            .contains(&(source.id, predicate_output))
        {
            return Err(SolveProgramConstructionError::InvalidProjection { provenance });
        }
        let destination = self.issue_register(payload_type, provenance)?;
        self.push(
            SolveOperation::AssignedState {
                operation: SolveAssignedStateOperation::ProjectChecked {
                    destination: destination.id,
                    source: source.id,
                    predicate_output,
                },
            },
            provenance,
        );
        Ok(destination)
    }
}

impl TypedProgram {
    pub(in crate::typed_program) fn is_assigned_read_check(
        &self,
        condition: SolveRegisterId,
        predicate: usize,
    ) -> bool {
        self.assigned_states
            .assigned_read_checks
            .contains(&(condition, predicate))
    }

    #[must_use]
    pub fn assigned_state_types(&self) -> &[SolveValueType] {
        &self.assigned_states.types
    }
    #[must_use]
    pub fn assigned_state_inputs(&self) -> &[SolveValueType] {
        &self.assigned_states.inputs
    }
    #[must_use]
    pub fn assigned_state_outputs(&self) -> &[SolveValueType] {
        &self.assigned_states.outputs
    }
}

/// One conditional publishes a complete normal tuple and complete tagged states.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct SolveAssignedConditional {
    condition: SolveRegisterId,
    captures: Box<[SolveRegisterId]>,
    state_captures: Box<[SolveAssignedStateId]>,
    destinations: Box<[SolveRegisterId]>,
    state_destinations: Box<[SolveAssignedStateId]>,
    if_true: SolveProgramRegion,
    if_false: SolveProgramRegion,
}

impl SolveAssignedConditional {
    #[must_use]
    pub const fn condition(&self) -> SolveRegisterId {
        self.condition
    }
    #[must_use]
    pub fn captures(&self) -> &[SolveRegisterId] {
        &self.captures
    }
    #[must_use]
    pub fn state_captures(&self) -> &[SolveAssignedStateId] {
        &self.state_captures
    }
    #[must_use]
    pub fn destinations(&self) -> &[SolveRegisterId] {
        &self.destinations
    }
    #[must_use]
    pub fn state_destinations(&self) -> &[SolveAssignedStateId] {
        &self.state_destinations
    }
    #[must_use]
    pub const fn if_true(&self) -> &SolveProgramRegion {
        &self.if_true
    }
    #[must_use]
    pub const fn if_false(&self) -> &SolveProgramRegion {
        &self.if_false
    }
}
