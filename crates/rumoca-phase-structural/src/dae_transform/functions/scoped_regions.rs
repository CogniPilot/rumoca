use super::*;

/// Dispatch concrete linear parents through one reconstruction implementation.
pub(super) trait RebuildRegion<'dae>: Sized {
    fn body(&self) -> &dae::FunctionBody<'dae>;

    fn begin_branch(
        self,
        target: &mut dae::DaeConstruction<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<(dae::FunctionBody<'dae>, dae::FunctionBranchFinish<Self>), dae::DaeConstructionError>;

    fn finish_branch(
        target: &mut dae::DaeConstruction<'dae>,
        finish: dae::FunctionBranchFinish<Self>,
        child: dae::FunctionBody<'dae>,
    ) -> Result<(Self, dae::FunctionBranchOwner<'dae>), dae::DaeConstructionError>;

    fn attach(
        &mut self,
        target: &mut dae::DaeConstruction<'dae>,
        branches: Vec<(dae::ExprId<'dae>, dae::FunctionBranchOwner<'dae>)>,
        fallback: dae::FunctionBranchOwner<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError>;

    fn require_assigned(
        &mut self,
        target: &mut dae::DaeConstruction<'dae>,
        value: dae::FunctionValueId<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError>;
}

impl<'dae> RebuildRegion<'dae> for dae::FunctionBody<'dae> {
    fn body(&self) -> &dae::FunctionBody<'dae> {
        self
    }

    fn begin_branch(
        self,
        target: &mut dae::DaeConstruction<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<(dae::FunctionBody<'dae>, dae::FunctionBranchFinish<Self>), dae::DaeConstructionError>
    {
        target.functions(|functions| functions.begin_branch(self, provenance))
    }

    fn finish_branch(
        target: &mut dae::DaeConstruction<'dae>,
        finish: dae::FunctionBranchFinish<Self>,
        child: dae::FunctionBody<'dae>,
    ) -> Result<(Self, dae::FunctionBranchOwner<'dae>), dae::DaeConstructionError> {
        target.functions(|functions| functions.finish_branch(finish, child))
    }

    fn attach(
        &mut self,
        target: &mut dae::DaeConstruction<'dae>,
        branches: Vec<(dae::ExprId<'dae>, dae::FunctionBranchOwner<'dae>)>,
        fallback: dae::FunctionBranchOwner<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        target.functions(|functions| {
            functions.finish_conditional(self, branches, fallback, provenance)
        })?;
        Ok(())
    }

    fn require_assigned(
        &mut self,
        target: &mut dae::DaeConstruction<'dae>,
        value: dae::FunctionValueId<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        target.functions(|functions| functions.require_assigned(self, value, provenance))?;
        Ok(())
    }
}

impl<'dae> RebuildRegion<'dae> for dae::FunctionLoop<'dae> {
    fn body(&self) -> &dae::FunctionBody<'dae> {
        self.body()
    }

    fn begin_branch(
        self,
        target: &mut dae::DaeConstruction<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<(dae::FunctionBody<'dae>, dae::FunctionBranchFinish<Self>), dae::DaeConstructionError>
    {
        target.functions(|functions| functions.begin_branch_loop(self, provenance))
    }

    fn finish_branch(
        target: &mut dae::DaeConstruction<'dae>,
        finish: dae::FunctionBranchFinish<Self>,
        child: dae::FunctionBody<'dae>,
    ) -> Result<(Self, dae::FunctionBranchOwner<'dae>), dae::DaeConstructionError> {
        target.functions(|functions| functions.finish_branch_loop(finish, child))
    }

    fn attach(
        &mut self,
        target: &mut dae::DaeConstruction<'dae>,
        branches: Vec<(dae::ExprId<'dae>, dae::FunctionBranchOwner<'dae>)>,
        fallback: dae::FunctionBranchOwner<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        target.functions(|functions| {
            functions.finish_conditional_loop(self, branches, fallback, provenance)
        })?;
        Ok(())
    }

    fn require_assigned(
        &mut self,
        target: &mut dae::DaeConstruction<'dae>,
        value: dae::FunctionValueId<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        target.functions(|functions| functions.require_assigned_loop(self, value, provenance))?;
        Ok(())
    }
}

impl<'source, 'target> FunctionRebuilder<'source, '_, 'target> {
    pub(super) fn rebuild_scoped_conditional<Region: RebuildRegion<'target>>(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        function: &RebuiltFunction<'target>,
        mut parent: Region,
        conditional: dae::FunctionConditionalBodyView<'source>,
    ) -> Result<Region, dae::DaeConstructionError> {
        let mut branches = Vec::with_capacity(conditional.conditions().len());
        for (condition, branch) in conditional.conditions().zip(conditional.branches()) {
            let condition = self.rebuild_expression(target, parent.body(), condition)?;
            let (restored, owner) = self.rebuild_branch(target, function, parent, branch)?;
            parent = restored;
            branches.push((condition, owner));
        }
        let (mut parent, fallback) =
            self.rebuild_branch(target, function, parent, conditional.fallback())?;
        parent.attach(target, branches, fallback, conditional.declaration())?;
        for definition in conditional.outputs() {
            self.seed_issued_definition(target, function, parent.body(), definition)?;
        }
        Ok(parent)
    }

    fn rebuild_branch<Region: RebuildRegion<'target>>(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        function: &RebuiltFunction<'target>,
        parent: Region,
        branch: dae::FunctionBranchView<'source>,
    ) -> Result<(Region, dae::FunctionBranchOwner<'target>), dae::DaeConstructionError> {
        let (child, finish) = parent.begin_branch(target, branch.declaration())?;
        let child = self.rebuild_statements(target, function, child, branch.statements())?;
        Region::finish_branch(target, finish, child)
    }

    pub(super) fn rebuild_assigned_read<Region: RebuildRegion<'target>>(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        function: &RebuiltFunction<'target>,
        region: &mut Region,
        value: dae::FunctionValueId<'source>,
        definition: dae::FunctionDefinitionView<'source>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        region.require_assigned(
            target,
            function.values[value.ordinal() as usize],
            provenance,
        )?;
        self.seed_issued_definition(target, function, region.body(), definition)
    }

    fn seed_issued_definition(
        &mut self,
        target: &mut dae::DaeConstruction<'target>,
        function: &RebuiltFunction<'target>,
        body: &dae::FunctionBody<'target>,
        definition: dae::FunctionDefinitionView<'source>,
    ) -> Result<(), dae::DaeConstructionError> {
        let value = function.values[definition.target().ordinal() as usize];
        let provenance = definition.provenance();
        let (id, rhs) = target.functions(|functions| {
            Ok((
                functions.current_definition_id(body, value, provenance)?,
                functions.current_definition(body, value, provenance)?,
            ))
        })?;
        self.seed(definition.rhs(), rhs, provenance)?;
        self.materialize_definition_uses(target, body, definition, definition.target(), value, id)
    }
}
