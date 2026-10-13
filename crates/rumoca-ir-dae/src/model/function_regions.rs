//! Linear construction of ordered function branch bodies.

use super::*;

#[cfg(test)]
mod tests;

/// Explicit definedness of a function value at a branch boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionAssignedState<'dae> {
    Unassigned,
    Assigned(FunctionDefinitionId<'dae>),
    Conditional {
        owner: FunctionConditionalId<'dae>,
        value: FunctionValueId<'dae>,
    },
}

impl<'dae> FunctionAssignedState<'dae> {
    pub(super) fn from_state(function: FunctionId<'dae>, state: FunctionValueState) -> Self {
        match state {
            FunctionValueState::Unassigned => Self::Unassigned,
            FunctionValueState::Assigned(definition) => {
                Self::Assigned(FunctionDefinitionId::from_raw(function.index(), definition))
            }
            FunctionValueState::Conditional { owner, value } => Self::Conditional {
                owner: FunctionConditionalId::from_raw(function.index(), owner),
                value: FunctionValueId::from_raw(function.index(), value),
            },
        }
    }
}

/// One aggregate-owned, source-ordered branch awaiting conditional attachment.
///
/// This capability is consumed by attachment; it cannot be cloned or assembled
/// from expression equality. An unattached owner prevents function completion.
pub struct FunctionBranchOwner<'dae> {
    pub(super) function: FunctionId<'dae>,
    pub(super) ordinal: u32,
}

/// Exact linear parent retained while its child branch is constructed.
pub struct FunctionBranchFinish<Parent> {
    parent: Parent,
    state: FunctionBuildState,
    scope: u32,
    declaration: DaeProvenance,
}

pub(super) trait FunctionRegion<'dae> {
    fn function_body(&self) -> &FunctionBody<'dae>;
}

impl<'dae> FunctionRegion<'dae> for FunctionBody<'dae> {
    fn function_body(&self) -> &FunctionBody<'dae> {
        self
    }
}

impl<'dae> FunctionRegion<'dae> for FunctionLoop<'dae> {
    fn function_body(&self) -> &FunctionBody<'dae> {
        &self.body
    }
}

impl<'dae> Functions<'_, 'dae> {
    pub fn begin_branch(
        &mut self,
        parent: FunctionBody<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(FunctionBody<'dae>, FunctionBranchFinish<FunctionBody<'dae>>), DaeConstructionError>
    {
        self.begin_branch_region(parent, provenance)
    }

    pub fn begin_branch_loop(
        &mut self,
        parent: FunctionLoop<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(FunctionBody<'dae>, FunctionBranchFinish<FunctionLoop<'dae>>), DaeConstructionError>
    {
        self.begin_branch_region(parent, provenance)
    }

    pub fn finish_branch(
        &mut self,
        finish: FunctionBranchFinish<FunctionBody<'dae>>,
        body: FunctionBody<'dae>,
    ) -> Result<(FunctionBody<'dae>, FunctionBranchOwner<'dae>), DaeConstructionError> {
        self.finish_branch_region(finish, body)
    }

    pub fn finish_branch_loop(
        &mut self,
        finish: FunctionBranchFinish<FunctionLoop<'dae>>,
        body: FunctionBody<'dae>,
    ) -> Result<(FunctionLoop<'dae>, FunctionBranchOwner<'dae>), DaeConstructionError> {
        self.finish_branch_region(finish, body)
    }

    /// Suspend a body and begin a branch against its current definitions.
    ///
    /// The token owns the suspended parent until the exact child is finished.
    /// Dropping it cannot expose a parent whose region is still suspended.
    /// Existing assignment and compact-loop constructors operate on the child.
    fn begin_branch_region<Parent: FunctionRegion<'dae>>(
        &mut self,
        parent: Parent,
        provenance: DaeProvenance,
    ) -> Result<(FunctionBody<'dae>, FunctionBranchFinish<Parent>), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let body = parent.function_body();
        let active_scope = function_build_state(self.storage, body).active_scope;
        let entry = &self.storage.functions[body.function.index() as usize];
        let scope = checked_u32(entry.scopes.len(), "function region", provenance)?;
        let current = function_build_state(self.storage, body);
        let child_state = FunctionBuildState {
            current_values: current.current_values.clone(),
            statements: Vec::new(),
            carried_targets: current.carried_targets.clone(),
            iteration_local_targets: current.iteration_local_targets.clone(),
            active_fold: current.active_fold,
            active_scope: Some(scope),
        };
        let state = std::mem::replace(function_build_state_mut(self.storage, body), child_state);
        let entry = &mut self.storage.functions[body.function.index() as usize];
        entry.scopes.push(FunctionScopeEntry {
            parent: active_scope,
        });
        entry.unfilled_branches += 1;
        let child = FunctionBody {
            function: body.function,
            construction: body.construction,
            domain: body.domain,
            scope: Some(scope),
        };
        let finish = FunctionBranchFinish {
            parent,
            state,
            scope,
            declaration: provenance,
        };
        Ok((child, finish))
    }

    /// Seal the exact child and restore its parent without advancing that parent.
    fn finish_branch_region<Parent: FunctionRegion<'dae>>(
        &mut self,
        finish: FunctionBranchFinish<Parent>,
        body: FunctionBody<'dae>,
    ) -> Result<(Parent, FunctionBranchOwner<'dae>), DaeConstructionError> {
        let parent = finish.parent.function_body();
        let function = parent.function;
        let valid = body.function == function
            && body.scope == Some(finish.scope)
            && body.domain == parent.domain
            && self.storage.functions[function.index() as usize]
                .build
                .as_ref()
                .is_some_and(|state| state.active_scope == Some(finish.scope));
        if !valid {
            return Err(incomplete(
                "function branch body",
                finish.scope as usize,
                finish.declaration,
            ));
        }
        let entry = &self.storage.functions[function.index() as usize];
        let ordinal = checked_u32(entry.branches.len(), "function branch", finish.declaration)?;
        let incoming = finish.state.current_values.clone();
        let child = std::mem::replace(function_build_state_mut(self.storage, &body), finish.state);
        let entry = &mut self.storage.functions[function.index() as usize];
        entry.branches.push(FunctionBranchEntry {
            scope: finish.scope,
            incoming,
            outgoing: child.current_values,
            statements: child.statements,
            declaration: finish.declaration,
        });
        Ok((finish.parent, FunctionBranchOwner { function, ordinal }))
    }

    /// Inspect definedness without fabricating a value for an unassigned target.
    pub fn branch_outgoing(
        &self,
        owner: &FunctionBranchOwner<'dae>,
        target: FunctionValueId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionAssignedState<'dae>, DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        check_function_value_owner(owner.function, target, provenance)?;
        function_value_entry(self.storage, target, provenance)?;
        let branch = self.branch_entry(owner, provenance)?;
        Ok(FunctionAssignedState::from_state(
            owner.function,
            branch.outgoing[target.ordinal() as usize],
        ))
    }

    pub(super) fn branch_entry(
        &self,
        owner: &FunctionBranchOwner<'dae>,
        provenance: DaeProvenance,
    ) -> Result<&FunctionBranchEntry, DaeConstructionError> {
        self.storage.functions[owner.function.index() as usize]
            .branches
            .get(owner.ordinal as usize)
            .ok_or_else(|| unknown("function branch", owner.ordinal, provenance))
    }
}
