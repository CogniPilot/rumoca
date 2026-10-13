use super::*;

impl<'program> TypedProgramBuilder<'program> {
    pub fn conditional_assigned(
        &mut self,
        condition: ProgramRegister<'program>,
        captures: (
            &[ProgramRegister<'program>],
            &[ProgramAssignedState<'program>],
        ),
        output_types: (Vec<SolveValueType>, Vec<SolveValueType>),
        provenance: Span,
        build_true: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramSlot<'region>],
            &[ProgramAssignedState<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
        build_false: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramSlot<'region>],
            &[ProgramAssignedState<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<
        (
            Vec<ProgramRegister<'program>>,
            Vec<ProgramAssignedState<'program>>,
        ),
        SolveProgramConstructionError,
    > {
        let (captures, state_captures) = captures;
        let (output_types, state_output_types) = output_types;
        require_provenance(provenance)?;
        let input_types = self.register_types_for(captures, provenance)?;
        let state_inputs = state_captures
            .iter()
            .map(|state| self.assigned_state_type(*state, provenance).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        let if_true = self.build_assigned_region(
            input_types.clone(),
            output_types.clone(),
            state_inputs.clone(),
            state_output_types.clone(),
            provenance,
            build_true,
        )?;
        let if_false = self.build_assigned_region(
            input_types,
            output_types,
            state_inputs,
            state_output_types,
            provenance,
            build_false,
        )?;
        self.conditional_assigned_from_regions(
            condition,
            captures,
            state_captures,
            if_true,
            if_false,
            provenance,
        )
    }

    pub(in crate::typed_program::program) fn build_assigned_region(
        &self,
        inputs: Vec<SolveValueType>,
        outputs: Vec<SolveValueType>,
        state_inputs: Vec<SolveValueType>,
        state_outputs: Vec<SolveValueType>,
        provenance: Span,
        build: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramSlot<'region>],
            &[ProgramAssignedState<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<SolveProgramRegion, SolveProgramConstructionError> {
        let state_input_count = state_inputs.len();
        let body = TypedProgram::construct_with_assigned_interface(
            self.arithmetic,
            self.available_calls,
            self.assertion_outputs,
            state_inputs,
            state_outputs,
            Some(provenance),
            |builder| {
                let input_slots = declare_interface_slots(
                    builder,
                    &inputs,
                    SolveStorageClass::Input,
                    SolveSlotAccess::ReadOnly,
                    provenance,
                )?;
                let output_slots = declare_interface_slots(
                    builder,
                    &outputs,
                    SolveStorageClass::Output,
                    SolveSlotAccess::ReadWrite,
                    provenance,
                )?;
                let state_inputs = (0..state_input_count)
                    .map(|input| builder.assigned_input(input, provenance))
                    .collect::<Result<Vec<_>, _>>()?;
                build(builder, &input_slots, &state_inputs, &output_slots)
            },
        )?;
        construct_region(inputs, outputs, body, provenance)
    }

    pub(in crate::typed_program::program) fn conditional_assigned_from_regions(
        &mut self,
        condition: ProgramRegister<'program>,
        captures: &[ProgramRegister<'program>],
        state_captures: &[ProgramAssignedState<'program>],
        if_true: SolveProgramRegion,
        if_false: SolveProgramRegion,
        provenance: Span,
    ) -> Result<
        (
            Vec<ProgramRegister<'program>>,
            Vec<ProgramAssignedState<'program>>,
        ),
        SolveProgramConstructionError,
    > {
        self.check_conditional_regions(condition, captures, &if_true, &if_false, provenance)?;
        let state_inputs = state_captures
            .iter()
            .map(|state| self.assigned_state_type(*state, provenance).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        if state_inputs != if_true.body.assigned_states.inputs
            || if_true.body.assigned_states.inputs != if_false.body.assigned_states.inputs
            || if_true.body.assigned_states.outputs != if_false.body.assigned_states.outputs
        {
            return Err(SolveProgramConstructionError::InvalidRegion { provenance });
        }
        let destinations = if_true
            .outputs
            .iter()
            .cloned()
            .map(|value_type| self.issue_register(value_type, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        let state_destinations = if_true
            .body
            .assigned_states
            .outputs
            .iter()
            .cloned()
            .map(|value_type| self.issue_assigned_state(value_type, provenance))
            .collect::<Result<Vec<_>, _>>()?;
        self.push(
            SolveOperation::AssignedConditional {
                program: Box::new(SolveAssignedConditional {
                    condition: condition.id,
                    captures: captures.iter().map(|capture| capture.id).collect(),
                    state_captures: state_captures.iter().map(|state| state.id).collect(),
                    destinations: destinations.iter().map(|value| value.id).collect(),
                    state_destinations: state_destinations.iter().map(|value| value.id).collect(),
                    if_true,
                    if_false,
                }),
            },
            provenance,
        );
        Ok((destinations, state_destinations))
    }
}
