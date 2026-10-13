use super::*;

pub(crate) fn conditional_output_facts<'dae>(
    storage: &Storage,
    owner: FunctionConditionalId<'dae>,
    value: FunctionValueId<'dae>,
    definition: FunctionDefinitionId<'dae>,
    provenance: DaeProvenance,
) -> Result<(u32, ExpressionVariability, Option<u32>), DaeConstructionError> {
    check_function_value_owner(owner.function(), value, provenance)?;
    let expected = next_function_definition_id(storage, owner.function(), provenance)?;
    if definition != expected {
        return Err(incomplete(
            "function conditional definition",
            definition.ordinal() as usize,
            provenance,
        ));
    }
    let target = function_value_entry(storage, value, provenance)?;
    let entry = &storage.functions[owner.function().index() as usize];
    let conditional = entry
        .conditionals
        .get(owner.ordinal() as usize)
        .ok_or_else(|| unknown("function conditional", owner.ordinal(), provenance))?;
    let mut variability = ExpressionVariability::Constant;
    let mut result = Ok(());
    visit_conditional_inputs(
        &storage.functions[owner.function().index() as usize],
        owner,
        value,
        |input| {
            if result.is_ok() {
                match storage.expr_variability(input, provenance) {
                    Ok(found) => variability = variability.max(found),
                    Err(error) => result = Err(error),
                }
            }
        },
    );
    result?;

    Ok((target.value_type, variability, conditional.domain))
}

impl<'dae> Functions<'_, 'dae> {
    pub fn finish_conditional(
        &mut self,
        parent: &mut FunctionBody<'dae>,
        branches: Vec<(ExprId<'dae>, FunctionBranchOwner<'dae>)>,
        fallback: FunctionBranchOwner<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionConditionalId<'dae>, DaeConstructionError> {
        self.finish_conditional_region(parent, branches, fallback, provenance)
    }

    pub fn finish_conditional_loop(
        &mut self,
        parent: &mut FunctionLoop<'dae>,
        branches: Vec<(ExprId<'dae>, FunctionBranchOwner<'dae>)>,
        fallback: FunctionBranchOwner<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionConditionalId<'dae>, DaeConstructionError> {
        self.finish_conditional_region(parent, branches, fallback, provenance)
    }

    /// Attach source-ordered branch owners against their exact incoming state.
    fn finish_conditional_region<Parent: FunctionRegion<'dae>>(
        &mut self,
        parent: &mut Parent,
        branches: Vec<(ExprId<'dae>, FunctionBranchOwner<'dae>)>,
        fallback: FunctionBranchOwner<'dae>,
        provenance: DaeProvenance,
    ) -> Result<FunctionConditionalId<'dae>, DaeConstructionError> {
        let body = parent.function_body();
        check_provenance(self.source_map, provenance)?;
        for (condition, branch) in &branches {
            self.check_scoped_condition(body, *condition, provenance)?;
            self.check_branch_attachment(body, branch, provenance)?;
        }
        self.check_branch_attachment(body, &fallback, provenance)?;
        let entry = &self.storage.functions[body.function.index() as usize];
        let ordinal = checked_u32(entry.conditionals.len(), "function conditional", provenance)?;
        let owner = FunctionConditionalId::from_raw(body.function.index(), ordinal);
        let branch_ordinals = branches
            .iter()
            .map(|(_, branch)| branch.ordinal)
            .collect::<Vec<_>>();
        let outgoing = scoped_outgoing(entry, &branch_ordinals, fallback.ordinal, ordinal);
        let output_start = entry.definitions.len();
        let conditional = ScopedFunctionConditionalEntry {
            outputs: output_start..output_start,
            conditions: branches
                .iter()
                .map(|(condition, _)| condition.index())
                .collect(),
            branches: branch_ordinals,
            fallback: fallback.ordinal,
            outgoing,
            domain: body.domain.map(DomainId::index),
            declaration: provenance,
        };
        self.storage.functions[body.function.index() as usize]
            .conditionals
            .push(conditional);
        self.issue_conditional_outputs(body, owner, provenance)?;
        let entry = &mut self.storage.functions[body.function.index() as usize];
        entry.conditionals[ordinal as usize].outputs.end = entry.definitions.len();
        entry.unfilled_branches -= branches.len() + 1;
        let outgoing = entry.conditionals[ordinal as usize].outgoing.clone();
        let build = function_build_state_mut(self.storage, body);
        build.current_values = outgoing;
        build
            .statements
            .push(FunctionStatementWire::ScopedConditional { owner: ordinal });
        Ok(owner)
    }

    fn check_scoped_condition(
        &self,
        body: &FunctionBody<'dae>,
        condition: ExprId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        expect_function_body_expression(self.storage, body, condition, provenance)?;
        validate_function_value_reads(self.storage, body, condition, provenance)?;
        let ty = self.storage.expr_type(condition, provenance)?;
        if !ty.is_scalar() || ty.scalar_type() != ScalarType::Boolean {
            return Err(DaeConstructionError::TypeMismatch {
                expected: ScalarType::Boolean,
                found: ty.scalar_type(),
                span: provenance.span(),
            });
        }
        Ok(())
    }

    fn check_branch_attachment(
        &self,
        body: &FunctionBody<'dae>,
        owner: &FunctionBranchOwner<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        if owner.function != body.function {
            return Err(DaeConstructionError::InvalidFunctionScope {
                expected_function: Some(body.function.index()),
                found_function: owner.function.index(),
                span: provenance.span(),
            });
        }
        let branch = self.branch_entry(owner, provenance)?;
        let entry = &self.storage.functions[body.function.index() as usize];
        let current = function_build_state(self.storage, body);
        if entry.scopes[branch.scope as usize].parent != body.scope
            || branch.incoming != current.current_values
        {
            return Err(incomplete(
                "function branch incoming state",
                owner.ordinal as usize,
                provenance,
            ));
        }
        Ok(())
    }

    fn issue_conditional_outputs(
        &mut self,
        body: &FunctionBody<'dae>,
        owner: FunctionConditionalId<'dae>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        let entry = &self.storage.functions[body.function.index() as usize];
        let conditional = &entry.conditionals[owner.ordinal() as usize];
        let targets = projection_targets(
            entry,
            &function_build_state(self.storage, body).current_values,
            &conditional.branches,
            conditional.fallback,
        )
        .collect::<Vec<_>>();
        for value in targets {
            let target = FunctionValueId::from_raw(
                body.function.index(),
                checked_u32(value, "function value", provenance)?,
            );
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
            self.storage.functions[body.function.index() as usize].conditionals
                [owner.ordinal() as usize]
                .outgoing[value] = FunctionValueState::Assigned(definition.ordinal());
        }
        Ok(())
    }
}

pub(super) fn projection_targets<'entry>(
    entry: &'entry FunctionEntry,
    incoming: &'entry [FunctionValueState],
    branches: &'entry [u32],
    fallback: u32,
) -> impl Iterator<Item = usize> + 'entry {
    incoming
        .iter()
        .copied()
        .enumerate()
        .filter_map(move |(value, previous)| {
            let states = branches
                .iter()
                .copied()
                .chain(std::iter::once(fallback))
                .map(|branch| entry.branches[branch as usize].outgoing[value]);
            let complete = states.clone().all(|state| state.definition().is_some());
            let changed = states.clone().any(|state| state != previous);
            (complete && changed).then_some(value)
        })
}

fn scoped_outgoing(
    entry: &FunctionEntry,
    branches: &[u32],
    fallback: u32,
    owner: u32,
) -> Vec<FunctionValueState> {
    let fallback_values = &entry.branches[fallback as usize].outgoing;
    fallback_values
        .iter()
        .copied()
        .enumerate()
        .map(|(value, fallback)| {
            let states = branches
                .iter()
                .map(|branch| entry.branches[*branch as usize].outgoing[value]);
            let all_equal = states.clone().all(|state| state == fallback);
            if all_equal {
                return fallback;
            }
            if fallback.definition().is_some()
                && states.clone().all(|state| state.definition().is_some())
            {
                return fallback;
            }
            FunctionValueState::Conditional {
                owner,
                value: value as u32,
            }
        })
        .collect()
}

/// Traverse retained assigned sources and selection conditions; no operand is
/// created for an unassigned path. Constructors own the exact state graph.
pub(super) fn visit_conditional_inputs<'dae>(
    function: &FunctionEntry,
    owner: FunctionConditionalId<'dae>,
    value: FunctionValueId<'dae>,
    visitor: impl FnMut(ExprId<'dae>),
) {
    visit_conditional_inputs_pruned(function, owner, value, |_, _| false, visitor);
}

pub(super) fn visit_conditional_inputs_pruned<'dae>(
    function: &FunctionEntry,
    source: FunctionConditionalId<'dae>,
    value: FunctionValueId<'dae>,
    mut prune: impl FnMut(FunctionConditionalId<'dae>, FunctionValueId<'dae>) -> bool,
    mut visitor: impl FnMut(ExprId<'dae>),
) {
    let mut pending = vec![FunctionValueState::Conditional {
        owner: source.ordinal(),
        value: value.ordinal(),
    }];
    while let Some(state) = pending.pop() {
        match state {
            FunctionValueState::Unassigned => {}
            FunctionValueState::Assigned(definition) => visitor(ExprId::from_raw(
                function.definitions[definition as usize].rhs,
            )),
            FunctionValueState::Conditional { owner, value } => {
                if prune(
                    FunctionConditionalId::from_raw(source.function().index(), owner),
                    FunctionValueId::from_raw(source.function().index(), value),
                ) {
                    continue;
                }
                let conditional = &function.conditionals[owner as usize];
                for condition in &conditional.conditions {
                    visitor(ExprId::from_raw(*condition));
                }
                for branch in conditional
                    .branches
                    .iter()
                    .copied()
                    .chain(std::iter::once(conditional.fallback))
                {
                    pending.push(function.branches[branch as usize].outgoing[value as usize]);
                }
            }
        }
    }
}
