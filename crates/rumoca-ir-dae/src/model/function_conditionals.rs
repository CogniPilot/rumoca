use std::marker::PhantomData;

use crate::expression::{ExprNode, Expressions};

use super::*;

impl<'dae> Functions<'_, 'dae> {
    /// Atomically join all values written by one ordered function conditional.
    ///
    /// The correlation is constructed with the resulting definitions, so a
    /// downstream projection can preserve shared branch evaluation without
    /// matching independently derived conditional expression trees.
    pub fn assign_conditional_all(
        &mut self,
        body: &mut FunctionBody<'dae>,
        targets: &[FunctionValueId<'dae>],
        conditions: &[ExprId<'dae>],
        branches: &[Vec<ExprId<'dae>>],
        fallback: &[ExprId<'dae>],
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        validate_conditional_arity(targets.len(), conditions, branches, fallback, provenance)?;
        validate_unique_targets(targets.iter().copied(), provenance)?;
        self.validate_conditional_values(
            body, targets, conditions, branches, fallback, provenance,
        )?;

        let assignments =
            self.join_conditional_values(targets, conditions, branches, fallback, provenance)?;
        let definitions = assignments
            .into_iter()
            .map(|(target, value)| {
                let definition =
                    insert_function_definition(self.storage, target, value, provenance)?;
                function_build_state_mut(self.storage, body).current_values
                    [target.ordinal() as usize] = Some(definition.ordinal());
                Ok(definition.ordinal())
            })
            .collect::<Result<Vec<_>, DaeConstructionError>>()?;
        function_build_state_mut(self.storage, body)
            .statements
            .push(FunctionStatementWire::AssignmentGroup {
                definitions,
                conditional: Some(conditional_wire(conditions, branches, fallback)),
            });
        Ok(())
    }

    /// Replay a serialized conditional correlation whose joined expressions
    /// have already been reconstructed in the expression arena.
    pub(crate) fn replay_conditional_all(
        &mut self,
        body: &mut FunctionBody<'dae>,
        assignments: &[(FunctionValueId<'dae>, ExprId<'dae>)],
        conditions: &[ExprId<'dae>],
        branches: &[Vec<ExprId<'dae>>],
        fallback: &[ExprId<'dae>],
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        validate_conditional_arity(
            assignments.len(),
            conditions,
            branches,
            fallback,
            provenance,
        )?;
        validate_unique_targets(assignments.iter().map(|(target, _)| *target), provenance)?;
        self.validate_joined_expressions(assignments, conditions, branches, fallback, provenance)?;
        self.assign_all(body, assignments, provenance)?;
        let FunctionStatementWire::AssignmentGroup { conditional, .. } =
            function_build_state_mut(self.storage, body)
                .statements
                .last_mut()
                .expect("a nonempty assign_all appends one assignment group")
        else {
            unreachable!("assign_all appends an assignment group")
        };
        *conditional = Some(conditional_wire(conditions, branches, fallback));
        Ok(())
    }

    /// Atomically commit one correlated conditional transition to loop-carried values.
    pub fn assign_conditional_all_loop(
        &mut self,
        loop_body: &mut FunctionLoop<'dae>,
        targets: &[FunctionValueId<'dae>],
        conditions: &[ExprId<'dae>],
        branches: &[Vec<ExprId<'dae>>],
        fallback: &[ExprId<'dae>],
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        self.validate_loop_targets(loop_body, targets.iter().copied(), provenance)?;
        self.assign_conditional_all(
            &mut loop_body.body,
            targets,
            conditions,
            branches,
            fallback,
            provenance,
        )
    }

    pub(crate) fn replay_conditional_all_loop(
        &mut self,
        loop_body: &mut FunctionLoop<'dae>,
        assignments: &[(FunctionValueId<'dae>, ExprId<'dae>)],
        conditions: &[ExprId<'dae>],
        branches: &[Vec<ExprId<'dae>>],
        fallback: &[ExprId<'dae>],
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        self.validate_loop_targets(
            loop_body,
            assignments.iter().map(|(target, _)| *target),
            provenance,
        )?;
        self.replay_conditional_all(
            &mut loop_body.body,
            assignments,
            conditions,
            branches,
            fallback,
            provenance,
        )
    }

    fn validate_conditional_values(
        &self,
        body: &FunctionBody<'dae>,
        targets: &[FunctionValueId<'dae>],
        conditions: &[ExprId<'dae>],
        branches: &[Vec<ExprId<'dae>>],
        fallback: &[ExprId<'dae>],
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        for condition in conditions {
            expect_function_body_expression(self.storage, body, *condition, provenance)?;
            validate_function_value_reads(self.storage, body, *condition, provenance)?;
            let ty = self.storage.expr_type(*condition, provenance)?;
            if !ty.is_scalar() || ty.scalar_type() != ScalarType::Boolean {
                return Err(DaeConstructionError::TypeMismatch {
                    expected: ScalarType::Boolean,
                    found: ty.scalar_type(),
                    span: provenance.span(),
                });
            }
        }
        for (ordinal, target) in targets.iter().enumerate() {
            check_function_value_owner(body.function, *target, provenance)?;
            let entry = function_value_entry(self.storage, *target, provenance)?;
            for value in branches
                .iter()
                .map(|branch| branch[ordinal])
                .chain(std::iter::once(fallback[ordinal]))
            {
                expect_function_body_expression(self.storage, body, value, provenance)?;
                validate_function_value_reads(self.storage, body, value, provenance)?;
                let found = self
                    .storage
                    .expressions
                    .value_types
                    .get(value.index() as usize)
                    .copied()
                    .ok_or_else(|| unknown("expression", value.index(), provenance))?;
                self.storage
                    .expect_value_type_compatible(entry.value_type, found, provenance)?;
            }
        }
        Ok(())
    }

    fn join_conditional_values(
        &mut self,
        targets: &[FunctionValueId<'dae>],
        conditions: &[ExprId<'dae>],
        branches: &[Vec<ExprId<'dae>>],
        fallback: &[ExprId<'dae>],
        provenance: DaeProvenance,
    ) -> Result<Vec<(FunctionValueId<'dae>, ExprId<'dae>)>, DaeConstructionError> {
        targets
            .iter()
            .enumerate()
            .map(|(ordinal, target)| {
                let branch_values = conditions
                    .iter()
                    .copied()
                    .zip(branches.iter().map(|branch| branch[ordinal]));
                let value = Expressions {
                    source_map: self.source_map,
                    storage: self.storage,
                    marker: PhantomData,
                }
                .at(provenance)
                .conditional(branch_values, fallback[ordinal])?;
                Ok((*target, value))
            })
            .collect()
    }

    fn validate_joined_expressions(
        &self,
        assignments: &[(FunctionValueId<'dae>, ExprId<'dae>)],
        conditions: &[ExprId<'dae>],
        branches: &[Vec<ExprId<'dae>>],
        fallback: &[ExprId<'dae>],
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        for (ordinal, (_, joined)) in assignments.iter().enumerate() {
            let expected = conditions
                .iter()
                .copied()
                .zip(branches.iter().map(|branch| branch[ordinal]))
                .flat_map(|(condition, value)| [condition.index(), value.index()])
                .chain(std::iter::once(fallback[ordinal].index()))
                .collect::<Vec<_>>();
            let Some(ExprNode::Conditional { operands }) =
                self.storage.expressions.nodes.get(joined.index() as usize)
            else {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: provenance.span(),
                });
            };
            if self.storage.expressions.operands[operands.indices()] != expected {
                return Err(DaeConstructionError::ShapeMismatch {
                    span: provenance.span(),
                });
            }
        }
        Ok(())
    }

    fn validate_loop_targets(
        &self,
        loop_body: &FunctionLoop<'dae>,
        targets: impl IntoIterator<Item = FunctionValueId<'dae>>,
        provenance: DaeProvenance,
    ) -> Result<(), DaeConstructionError> {
        check_provenance(self.source_map, provenance)?;
        let carried = &function_build_state(self.storage, &loop_body.body).carried_targets;
        for target in targets {
            check_function_value_owner(loop_body.body.function, target, provenance)?;
            if !carried.contains(&target.ordinal()) {
                return Err(DaeConstructionError::IncompleteDefinition {
                    kind: "function loop target",
                    index: target.ordinal(),
                    span: provenance.span(),
                });
            }
        }
        Ok(())
    }
}

fn validate_conditional_arity(
    target_count: usize,
    conditions: &[ExprId<'_>],
    branches: &[Vec<ExprId<'_>>],
    fallback: &[ExprId<'_>],
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    if conditions.is_empty() || conditions.len() != branches.len() {
        return Err(invalid_arity(conditions.len(), branches.len(), provenance));
    }
    if target_count == 0 || fallback.len() != target_count {
        return Err(invalid_arity(target_count, fallback.len(), provenance));
    }
    if let Some(branch) = branches.iter().find(|branch| branch.len() != target_count) {
        return Err(invalid_arity(target_count, branch.len(), provenance));
    }
    Ok(())
}

fn validate_unique_targets<'dae>(
    targets: impl IntoIterator<Item = FunctionValueId<'dae>>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let mut unique_targets = rustc_hash::FxHashSet::default();
    for target in targets {
        if !unique_targets.insert(target.ordinal()) {
            return Err(DaeConstructionError::DuplicateDefinition {
                kind: "function conditional target",
                index: target.ordinal(),
                span: provenance.span(),
            });
        }
    }
    Ok(())
}

fn conditional_wire(
    conditions: &[ExprId<'_>],
    branches: &[Vec<ExprId<'_>>],
    fallback: &[ExprId<'_>],
) -> FunctionConditionalWire {
    FunctionConditionalWire {
        conditions: conditions.iter().map(|value| value.index()).collect(),
        branches: branches
            .iter()
            .map(|branch| branch.iter().map(|value| value.index()).collect())
            .collect(),
        fallback: fallback.iter().map(|value| value.index()).collect(),
    }
}
