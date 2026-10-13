//! Complete carried folds with separate immutable assigned-state captures.

use super::*;

impl<'program> TypedProgramBuilder<'program> {
    pub fn fold_with_assigned(
        &mut self,
        domain: StructuredIndexDomain,
        initial: &[ProgramRegister<'program>],
        captures: &[ProgramRegister<'program>],
        state_captures: &[ProgramAssignedState<'program>],
        provenance: Span,
        build: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramAssignedState<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        require_fold_domain(&domain, self.arithmetic, provenance)?;
        if initial.is_empty() {
            return Err(SolveProgramConstructionError::InvalidFold { provenance });
        }
        let state_types = state_captures
            .iter()
            .map(|state| self.assigned_state_type(*state, provenance).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        let carried_types = self.register_types_for(initial, provenance)?;
        let capture_types = self.register_types_for(captures, provenance)?;
        let binder_types = vec![
            SolveValueType::scalar(SolveScalarType::integer(self.arithmetic));
            domain.binders.len()
        ];
        let mut input_types = carried_types.clone();
        input_types.extend(capture_types.iter().cloned());
        input_types.extend(binder_types.iter().cloned());
        let carried_count = carried_types.len();
        let capture_count = capture_types.len();
        let transition = self.build_assigned_region(
            input_types,
            carried_types,
            state_types.clone(),
            Vec::new(),
            provenance,
            |builder, inputs, states, outputs| {
                let (carried, remaining) = inputs.split_at(carried_count);
                let (captures, binders) = remaining.split_at(capture_count);
                build(builder, states, carried, captures, binders, outputs)
            },
        )?;
        self.fold_from_region(
            domain,
            initial,
            (captures, state_captures),
            transition,
            None,
            provenance,
        )
    }

    /// A fold that ends at the first pass whose `continuation` predicate,
    /// built over the carried tuple, the captures and the binder values of
    /// that pass, is false.
    pub fn fold_while_assigned(
        &mut self,
        domain: StructuredIndexDomain,
        initial: &[ProgramRegister<'program>],
        (captures, state_captures): (
            &[ProgramRegister<'program>],
            &[ProgramAssignedState<'program>],
        ),
        provenance: Span,
        continuation: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramAssignedState<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
        build: impl for<'region> FnOnce(
            &mut TypedProgramBuilder<'region>,
            &[ProgramAssignedState<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
            &[ProgramSlot<'region>],
        ) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<Vec<ProgramRegister<'program>>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let state_types = state_captures
            .iter()
            .map(|state| self.assigned_state_type(*state, provenance).cloned())
            .collect::<Result<Vec<_>, _>>()?;
        let carried_types = self.register_types_for(initial, provenance)?;
        let capture_types = self.register_types_for(captures, provenance)?;
        let binder_types = vec![
            SolveValueType::scalar(SolveScalarType::integer(self.arithmetic));
            domain.binders.len()
        ];
        let carried_count = carried_types.len();
        let capture_count = capture_types.len();
        let mut input_types = carried_types.clone();
        input_types.extend(capture_types);
        input_types.extend(binder_types);
        let predicate = self.build_assigned_region(
            input_types.clone(),
            vec![SolveValueType::scalar(SolveScalarType::Boolean)],
            state_types.clone(),
            Vec::new(),
            provenance,
            |builder, inputs, states, outputs| {
                let (carried, remaining) = inputs.split_at(carried_count);
                let (captures, binders) = remaining.split_at(capture_count);
                continuation(builder, states, carried, captures, binders, outputs)
            },
        )?;
        let transition = self.build_assigned_region(
            input_types,
            carried_types,
            state_types,
            Vec::new(),
            provenance,
            |builder, inputs, states, outputs| {
                let (carried, remaining) = inputs.split_at(carried_count);
                let (captures, binders) = remaining.split_at(capture_count);
                build(builder, states, carried, captures, binders, outputs)
            },
        )?;
        self.fold_from_region(
            domain,
            initial,
            (captures, state_captures),
            transition,
            Some(predicate),
            provenance,
        )
    }
}
