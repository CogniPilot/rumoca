//! Lower only the canonical source action that admits a partial value.

use super::*;

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    pub(super) fn require_assigned_statement(
        &mut self,
        conditional: dae::FunctionConditionalBodyView<'dae>,
        value: dae::FunctionValueId<'dae>,
        definition: dae::FunctionDefinitionView<'dae>,
        provenance: rumoca_core::Span,
    ) -> Result<(), solve::SolveProgramConstructionError> {
        let declaration = self
            .direct_assertions
            .get(self.next_direct_assertion)
            .ok_or(solve::SolveProgramConstructionError::InvalidCallOutput { provenance })?;
        if !matches!(declaration.message, FunctionAssertionMessage::UnassignedValue(source) if source == value)
            || declaration.provenance.span() != provenance
        {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        let states = self
            .assigned_values
            .get(&(conditional.id(), value))
            .cloned()
            .ok_or(solve::SolveProgramConstructionError::InvalidProjection { provenance })?;
        let capability = self
            .builder
            .assertion_output(declaration.predicate_output, provenance)?;
        let (condition, leaves) = self
            .builder
            .require_assigned_states(capability, &states, provenance)?;
        let slot = declaration.predicate_output - self.assertion_output_base;
        if self.predicate_values[slot].replace(condition).is_some() {
            return Err(solve::SolveProgramConstructionError::InvalidCallOutput { provenance });
        }
        let value_type = function_value_type(self.view, value, provenance)?;
        self.function_values
            .insert(definition.id(), LoweredValue { value_type, leaves });
        self.next_direct_assertion += 1;
        Ok(())
    }

    pub(super) fn assigned_branch_value(
        &mut self,
        branch: dae::FunctionBranchView<'dae>,
        value: dae::FunctionValueId<'dae>,
        provenance: rumoca_core::Span,
    ) -> Result<Vec<solve::ProgramAssignedState<'program>>, solve::SolveProgramConstructionError>
    {
        match branch
            .outgoing(value)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?
        {
            dae::FunctionAssignedState::Unassigned => {
                let value_type = function_value_type(self.view, value, provenance)?;
                lower_value_type_leaves(self.view, value_type, arithmetic_profile())?
                    .into_iter()
                    .map(|value_type| self.builder.absent_assigned_state(value_type, provenance))
                    .collect()
            }
            dae::FunctionAssignedState::Assigned(id) => {
                let definition = self
                    .view
                    .function_definition(id)
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
                let value = self.function_definition_value(definition)?;
                value
                    .leaves
                    .into_iter()
                    .map(|source| self.builder.present_assigned_state(source, provenance))
                    .collect()
            }
            dae::FunctionAssignedState::Conditional { owner, value } => self
                .assigned_values
                .get(&(owner, value))
                .cloned()
                .ok_or(solve::SolveProgramConstructionError::InvalidProjection { provenance }),
        }
    }
}
