//! One construction lifecycle hands its completion product to the caller.
use super::*;

impl TypedProgram {
    pub fn construct(
        arithmetic: SolveArithmeticProfile,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        Self::construct_with_calls(arithmetic, SolvePureCallTableView::default(), build)
    }

    pub(in crate::typed_program) fn construct_with_calls(
        arithmetic: SolveArithmeticProfile,
        available_calls: SolvePureCallTableView<'_>,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        Self::construct_with_owner(arithmetic, available_calls, &[], build)
    }

    pub(in crate::typed_program) fn construct_with_owner(
        arithmetic: SolveArithmeticProfile,
        available_calls: SolvePureCallTableView<'_>,
        assertion_outputs: &[SolvePureCallOutput],
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        Self::construct_with_assigned_interface(
            arithmetic,
            available_calls,
            assertion_outputs,
            Vec::new(),
            Vec::new(),
            None,
            build,
        )
    }

    pub(super) fn construct_with_assigned_interface(
        arithmetic: SolveArithmeticProfile,
        available_calls: SolvePureCallTableView<'_>,
        assertion_outputs: &[SolvePureCallOutput],
        state_inputs: Vec<SolveValueType>,
        state_outputs: Vec<SolveValueType>,
        region_provenance: Option<Span>,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Self, SolveProgramConstructionError> {
        Self::construct_with_completion(
            arithmetic,
            available_calls,
            assertion_outputs,
            state_inputs,
            state_outputs,
            region_provenance,
            build,
        )
        .map(|(program, ())| program)
    }

    pub(in crate::typed_program) fn construct_complete_entry(
        available_calls: SolvePureCallTableView<'_>,
        arithmetic: SolveArithmeticProfile,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<(Self, crate::typed_program::entry::EntryInterface), SolveProgramConstructionError>
    {
        Self::construct_with_completion(
            arithmetic,
            available_calls,
            &[],
            Vec::new(),
            Vec::new(),
            None,
            |builder| {
                builder.entry_interface.enable();
                build(builder)?;
                std::mem::take(&mut builder.entry_interface).finish()
            },
        )
    }

    fn construct_with_completion<R>(
        arithmetic: SolveArithmeticProfile,
        available_calls: SolvePureCallTableView<'_>,
        assertion_outputs: &[SolvePureCallOutput],
        state_inputs: Vec<SolveValueType>,
        state_outputs: Vec<SolveValueType>,
        region_provenance: Option<Span>,
        build: impl for<'program> FnOnce(
            &mut TypedProgramBuilder<'program>,
        ) -> Result<R, SolveProgramConstructionError>,
    ) -> Result<(Self, R), SolveProgramConstructionError> {
        if state_inputs
            .iter()
            .chain(&state_outputs)
            .any(|value_type| !value_type.belongs_to(arithmetic))
        {
            return Err(SolveProgramConstructionError::WireMismatch);
        }
        let mut builder = TypedProgramBuilder {
            arithmetic,
            slots: Vec::new(),
            slot_initialized: Vec::new(),
            entry_interface: entry_interface::EntryConstruction::default(),
            register_types: Vec::new(),
            operations: Vec::new(),
            numbering: std::collections::HashMap::new(),
            numbered_values: true,
            arithmetic_facts: arithmetic::ArithmeticFacts::default(),
            assigned_states: AssignedStateConstruction::interface(state_inputs, state_outputs),
            available_calls,
            assertion_outputs,
            marker: PhantomData,
        };
        let completion = build(&mut builder)?;
        builder.assigned_states.complete(
            builder
                .operations
                .last()
                .map(|op| op.provenance)
                .or(region_provenance),
        )?;
        let program = Self::assemble(
            arithmetic,
            builder.slots,
            builder.register_types,
            builder.operations,
            builder.assigned_states.layout,
        );
        Ok((program, completion))
    }
}
