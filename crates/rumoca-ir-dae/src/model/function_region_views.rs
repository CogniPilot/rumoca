use super::*;

#[derive(Clone, Copy)]
pub struct FunctionConditionalBodyView<'dae> {
    dae: &'dae Dae,
    id: FunctionConditionalId<'dae>,
    entry: &'dae ScopedFunctionConditionalEntry,
}

#[derive(Clone, Copy)]
pub struct FunctionBranchView<'dae> {
    dae: &'dae Dae,
    function: FunctionId<'dae>,
    entry: &'dae FunctionBranchEntry,
}

impl<'dae> DaeView<'dae> {
    pub fn function_conditional(
        self,
        id: FunctionConditionalId<'dae>,
    ) -> Option<FunctionConditionalBodyView<'dae>> {
        let function = self
            .dae
            .storage
            .functions
            .get(id.function().index() as usize)?;
        Some(FunctionConditionalBodyView {
            dae: self.dae,
            id,
            entry: function.conditionals.get(id.ordinal() as usize)?,
        })
    }
}

impl<'dae> FunctionConditionalBodyView<'dae> {
    pub const fn id(self) -> FunctionConditionalId<'dae> {
        self.id
    }

    pub const fn declaration(self) -> DaeProvenance {
        self.entry.declaration
    }

    pub fn conditions(self) -> impl ExactSizeIterator<Item = ExprId<'dae>> + 'dae {
        self.entry.conditions.iter().copied().map(ExprId::from_raw)
    }

    pub fn branches(self) -> impl ExactSizeIterator<Item = FunctionBranchView<'dae>> + 'dae {
        self.entry
            .branches
            .iter()
            .copied()
            .map(move |ordinal| self.branch(ordinal))
    }

    pub fn fallback(self) -> FunctionBranchView<'dae> {
        self.branch(self.entry.fallback)
    }

    /// Complete changed outputs issued atomically by this conditional constructor.
    /// Unchanged incoming values and partial definedness states are excluded.
    pub fn outputs(self) -> impl ExactSizeIterator<Item = FunctionDefinitionView<'dae>> + 'dae {
        self.entry.outputs.clone().map(move |ordinal| {
            super::view::function_definition_view(self.dae, self.id.function(), ordinal as u32)
        })
    }

    /// Visit the exact assigned source expressions and conditions of one value.
    /// Unassigned paths contribute no value expression.
    pub fn visit_value_inputs(
        self,
        value: FunctionValueId<'dae>,
        visitor: impl FnMut(ExprId<'dae>),
    ) -> bool {
        self.visit_value_inputs_pruned(value, |_, _| false, visitor)
    }

    /// Visit original assigned sources, stopping at caller-owned exact state interfaces.
    pub fn visit_value_inputs_pruned(
        self,
        value: FunctionValueId<'dae>,
        prune: impl FnMut(FunctionConditionalId<'dae>, FunctionValueId<'dae>) -> bool,
        visitor: impl FnMut(ExprId<'dae>),
    ) -> bool {
        if value.function() != self.id.function()
            || value.ordinal() as usize >= self.entry.outgoing.len()
        {
            return false;
        }
        super::scoped_conditionals::visit_conditional_inputs_pruned(
            &self.dae.storage.functions[self.id.function().index() as usize],
            self.id,
            value,
            prune,
            visitor,
        );
        true
    }

    pub fn outgoing(self, value: FunctionValueId<'dae>) -> Option<FunctionAssignedState<'dae>> {
        boundary_value(self.id.function(), value, &self.entry.outgoing)
    }

    fn branch(self, ordinal: u32) -> FunctionBranchView<'dae> {
        FunctionBranchView {
            dae: self.dae,
            function: self.id.function(),
            entry: &self.dae.storage.functions[self.id.function().index() as usize].branches
                [ordinal as usize],
        }
    }
}

impl<'dae> FunctionBranchView<'dae> {
    pub const fn declaration(self) -> DaeProvenance {
        self.entry.declaration
    }

    pub fn body_scope(self) -> FunctionScopeView<'dae> {
        FunctionScopeView::new(self.dae, self.function, None, Some(self.entry.scope))
    }

    pub fn statements(self) -> FunctionStatements<'dae> {
        FunctionStatements::new(self.dae, self.function, &self.entry.statements)
    }

    pub fn incoming(self, value: FunctionValueId<'dae>) -> Option<FunctionAssignedState<'dae>> {
        boundary_value(self.function, value, &self.entry.incoming)
    }

    pub fn outgoing(self, value: FunctionValueId<'dae>) -> Option<FunctionAssignedState<'dae>> {
        boundary_value(self.function, value, &self.entry.outgoing)
    }
}

fn boundary_value<'dae>(
    function: FunctionId<'dae>,
    value: FunctionValueId<'dae>,
    states: &[FunctionValueState],
) -> Option<FunctionAssignedState<'dae>> {
    if value.function() != function {
        return None;
    }
    Some(FunctionAssignedState::from_state(
        function,
        *states.get(value.ordinal() as usize)?,
    ))
}
