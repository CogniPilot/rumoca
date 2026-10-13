//! Directional transport preserves the selected path's optional payload state.

use super::*;

pub(super) fn interface_offsets(types: &[SolveValueType]) -> Vec<usize> {
    let mut offset = 0;
    types
        .iter()
        .map(|value_type| {
            let current = offset;
            offset += 1 + usize::from(is_real(value_type));
            current
        })
        .collect()
}

impl<'primal, 'program> DirectionalBuilder<'primal, 'program> {
    fn state(
        &self,
        id: SolveAssignedStateId,
        provenance: Span,
    ) -> Result<Directional<ProgramAssignedState<'program>>, SolveProgramConstructionError> {
        self.assigned_states
            .get(id.index())
            .copied()
            .flatten()
            .ok_or(SolveProgramConstructionError::InvalidRegion { provenance })
    }

    fn bind_state(
        &mut self,
        id: SolveAssignedStateId,
        state: Directional<ProgramAssignedState<'program>>,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let target = self
            .assigned_states
            .get_mut(id.index())
            .ok_or(SolveProgramConstructionError::InvalidRegion { provenance })?;
        if target.replace(state).is_some() {
            return Err(SolveProgramConstructionError::InvalidRegion { provenance });
        }
        Ok(())
    }

    pub(super) fn expanded_states(
        &self,
        states: &[SolveAssignedStateId],
        provenance: Span,
    ) -> Result<Vec<ProgramAssignedState<'program>>, SolveProgramConstructionError> {
        let mut expanded = Vec::new();
        for state in states {
            let state = self.state(*state, provenance)?;
            expanded.push(state.primal);
            expanded.extend(state.tangent);
        }
        Ok(expanded)
    }

    pub(super) fn derive_assigned_state(
        &mut self,
        operation: &SolveAssignedStateOperation,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        use SolveAssignedStateOperation as O;
        match operation {
            O::Absent {
                destination,
                payload_type,
            } => {
                let primal = self
                    .builder
                    .absent_assigned_state(payload_type.clone(), provenance)?;
                let tangent = is_real(payload_type)
                    .then(|| {
                        self.builder
                            .absent_assigned_state(payload_type.clone(), provenance)
                    })
                    .transpose()?;
                self.bind_state(*destination, Directional { primal, tangent }, provenance)
            }
            O::Present {
                destination,
                source,
            } => {
                let source_type = self.primal.register_types()[source.index()].clone();
                let source = self.get(*source, provenance)?;
                let primal = self
                    .builder
                    .present_assigned_state(source.primal, provenance)?;
                let tangent = if is_real(&source_type) {
                    let tangent = self.tangent_or_zero(source, &source_type, provenance)?;
                    Some(self.builder.present_assigned_state(tangent, provenance)?)
                } else {
                    None
                };
                self.bind_state(*destination, Directional { primal, tangent }, provenance)
            }
            O::Input { destination, input } => {
                let ordinal = self.state_inputs[*input];
                let primal = self.builder.assigned_input(ordinal, provenance)?;
                let tangent = is_real(&self.primal.assigned_state_inputs()[*input])
                    .then(|| self.builder.assigned_input(ordinal + 1, provenance))
                    .transpose()?;
                self.bind_state(*destination, Directional { primal, tangent }, provenance)
            }
            O::Output { output, source } => {
                let source = self.state(*source, provenance)?;
                let ordinal = self.state_outputs[*output];
                self.builder
                    .store_assigned_state(ordinal, source.primal, provenance)?;
                if let Some(tangent) = source.tangent {
                    self.builder
                        .store_assigned_state(ordinal + 1, tangent, provenance)?;
                }
                Ok(())
            }
            O::IsPresent {
                destination,
                source,
            } => {
                let sources = self.expanded_states(&[*source], provenance)?;
                let value = self
                    .builder
                    .assigned_states_all_present(&sources, provenance)?;
                self.bind_nonreal(*destination, value, provenance)
            }
            O::AllPresent {
                destination,
                sources,
            } => {
                let sources = self.expanded_states(sources, provenance)?;
                let value = self
                    .builder
                    .assigned_states_all_present(&sources, provenance)?;
                self.bind_nonreal(*destination, value, provenance)
            }
            O::ProjectChecked {
                destination,
                source,
                predicate_output,
            } => {
                let source = self.state(*source, provenance)?;
                let predicate = *self
                    .assertions
                    .locations
                    .get(*predicate_output)
                    .ok_or(SolveProgramConstructionError::InvalidCallOutput { provenance })?;
                let primal = self.builder.project_checked_assigned_state(
                    source.primal,
                    predicate,
                    provenance,
                )?;
                let tangent = source
                    .tangent
                    .map(|state| {
                        self.builder
                            .project_checked_assigned_state(state, predicate, provenance)
                    })
                    .transpose()?;
                self.bind(*destination, Directional { primal, tangent }, provenance)
            }
        }
    }

    pub(super) fn derive_assigned_conditional(
        &mut self,
        conditional: &SolveAssignedConditional,
        provenance: Span,
    ) -> Result<(), SolveProgramConstructionError> {
        let condition = self.get(conditional.condition(), provenance)?.primal;
        let captures = self.expanded_registers(conditional.captures(), provenance)?;
        let states = self.expanded_states(conditional.state_captures(), provenance)?;
        let if_true = derive_region(conditional.if_true(), self.available, self.assertions)?;
        let if_false = derive_region(conditional.if_false(), self.available, self.assertions)?;
        let (values, states) = self.builder.conditional_assigned_from_regions(
            condition, &captures, &states, if_true, if_false, provenance,
        )?;
        self.bind_expanded(conditional.destinations(), &values, provenance)?;
        let mut offset = 0;
        for destination in conditional.state_destinations() {
            let primal = *states
                .get(offset)
                .ok_or(SolveProgramConstructionError::InvalidRegion { provenance })?;
            offset += 1;
            let tangent = if is_real(&self.primal.assigned_state_types()[destination.index()]) {
                let tangent = *states
                    .get(offset)
                    .ok_or(SolveProgramConstructionError::InvalidRegion { provenance })?;
                offset += 1;
                Some(tangent)
            } else {
                None
            };
            self.bind_state(*destination, Directional { primal, tangent }, provenance)?;
        }
        if offset != states.len() {
            return Err(SolveProgramConstructionError::InvalidRegion { provenance });
        }
        Ok(())
    }
}
