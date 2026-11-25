use super::*;

impl<'dae> Functions<'_, 'dae> {
    /// Check the exact executed-path state before admitting a partial value.
    /// The constructor supplies the ordered action; a Boolean assertion is
    /// neither an input nor a certificate for this operation.
    pub fn require_assigned(
        &mut self,
        body: &mut FunctionBody<'dae>,
        target: FunctionValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionDefinitionId<'dae>, DaeConstructionError> {
        self.require_assigned_region(body, target, provenance, false)
    }

    /// Carry the exact executed-path state of a partial value without a
    /// check: the issued definition holds the value or its absence. Only a
    /// field of the record assigned to a function output may read it; the
    /// check moves to the read of that field (MLS §12.4.4).
    pub fn carry_assigned(
        &mut self,
        body: &mut FunctionBody<'dae>,
        target: FunctionValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionDefinitionId<'dae>, DaeConstructionError> {
        self.require_assigned_region(body, target, provenance, true)
    }

    /// The same assigned-state operation inside an existing compact loop.
    pub fn require_assigned_loop(
        &mut self,
        body: &mut FunctionLoop<'dae>,
        target: FunctionValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionDefinitionId<'dae>, DaeConstructionError> {
        self.require_assigned_region(body.body(), target, provenance, false)
    }

    fn require_assigned_region(
        &mut self,
        body: &FunctionBody<'dae>,
        target: FunctionValueId<'dae>,
        provenance: DaeProvenance,
        carried: bool,
    ) -> Result<FunctionDefinitionId<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        check_function_value_owner(body.function, target, provenance)?;
        function_value_entry(self.storage, target, provenance)?;
        let state =
            function_build_state(self.storage, body).current_values[target.ordinal() as usize];
        let FunctionValueState::Conditional { owner, value } = state else {
            return Err(incomplete(
                "function partial assigned state",
                target.ordinal() as usize,
                provenance,
            ));
        };
        if value != target.ordinal() {
            return Err(incomplete(
                "function partial assigned target",
                target.ordinal() as usize,
                provenance,
            ));
        }
        let owner = FunctionConditionalId::from_raw(body.function.index(), owner);
        let definition = next_function_definition_id(self.storage, body.function, provenance)?;
        let expression = crate::expression::insert_function_conditional_output(
            self.source_map,
            self.storage,
            owner,
            target,
            definition,
            provenance,
        )?;
        let issued = insert_function_definition(self.storage, target, expression, provenance)?;
        if issued != definition {
            return Err(DaeConstructionError::ShapeMismatch {
                span: provenance.span(),
            });
        }
        if carried {
            self.storage.functions[body.function.index() as usize]
                .carried_definitions
                .push(definition.ordinal());
        }
        let build = function_build_state_mut(self.storage, body);
        build
            .statements
            .push(FunctionStatementWire::RequireAssigned {
                owner: owner.ordinal(),
                target: target.ordinal(),
                definition: definition.ordinal(),
                carried,
            });
        build.current_values[target.ordinal() as usize] =
            FunctionValueState::Assigned(definition.ordinal());
        Ok(definition)
    }
}
