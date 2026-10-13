//! Exact semantic capture selection for checked typed regions.

use std::collections::HashSet;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{EnvironmentLayout, ExpressionLowerer, LoweredValue, ModelCoordinateKey};

#[derive(Default)]
struct EnvironmentRequirements<'dae> {
    model_coordinates: HashSet<ModelCoordinateKey<'dae>>,
    parameters: HashSet<dae::FunctionParameterId<'dae>>,
    values: HashSet<dae::FunctionDefinitionId<'dae>>,
    expressions: HashSet<dae::ExprId<'dae>>,
    fold_parameters: HashSet<(dae::FunctionFoldId<'dae>, u32)>,
    binders: HashSet<(u32, u32)>,
}

impl<'dae> EnvironmentRequirements<'dae> {
    fn record_model_coordinate(&mut self, coordinate: dae::CoordinateView<'dae>) {
        if let Some(key) = ModelCoordinateKey::from_view(coordinate) {
            self.model_coordinates.insert(key);
        }
    }
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    fn has_pending_assigned_source(&self, operation: dae::ExpressionOperation<'dae>) -> bool {
        matches!(
            operation,
            dae::ExpressionOperation::FunctionConditionalOutput { conditional, value, definition }
                if self.assigned_values.contains_key(&(conditional, value))
                    && !self.function_values.contains_key(&definition.id())
        )
    }

    /// Value of one construction-issued SSA definition in this scope.
    ///
    /// A definition that no statement has lowered yet is resolved from its own
    /// right-hand side, which makes demand order topological: a read always
    /// names the exact reaching definition, so the definition it names is what
    /// gets computed. The result is memoized under the issued definition
    /// identity so a later capture pass sees it as available here.
    ///
    /// This is the only resolver: the in-order statement arm, a read of a
    /// definition this scope has not lowered yet, and a fold's entry value all
    /// arrive here, so a definition is computed once and every demand that
    /// follows names the value that was already issued for it. Resolution goes
    /// through the one definition-value rule, so the memoized value carries the
    /// type its target declares. Storing the right-hand side's own type instead
    /// would publish an Integer register under a Real definition, and every
    /// later consumer reads that definition through the DAE node's Real type.
    pub(super) fn function_definition_value(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
    ) -> Result<LoweredValue<'program, 'dae>, solve::SolveProgramConstructionError> {
        if let Some(value) = self.function_values.get(&definition.id()).cloned() {
            return Ok(value);
        }
        let value = self.definition_value(definition)?;
        self.function_values.insert(definition.id(), value.clone());
        Ok(value)
    }

    /// Capture selection for a region evaluated in this same scope.
    ///
    /// A conditional or map body reads exactly the values this scope owns, so
    /// every definition it names may be resolved here. Passing no scope states
    /// that fact: the region opens no new definition scope, so there is no
    /// boundary for a definition to fall on the far side of.
    pub(super) fn capture_environment_for(
        &mut self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>> + Clone,
    ) -> Result<
        (
            Vec<solve::ProgramRegister<'program>>,
            EnvironmentLayout<'dae>,
        ),
        solve::SolveProgramConstructionError,
    > {
        let requirements = self.environment_requirements(expressions, None, true)?;
        Ok(self.capture_environment(&requirements))
    }

    /// A message-only call belongs to the failed-only region. Already-issued
    /// values remain captures, while discovery cannot issue a new eager call.
    pub(super) fn capture_environment_for_message(
        &mut self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>> + Clone,
    ) -> Result<
        (
            Vec<solve::ProgramRegister<'program>>,
            EnvironmentLayout<'dae>,
        ),
        solve::SolveProgramConstructionError,
    > {
        let requirements = self.environment_requirements(expressions, None, false)?;
        Ok(self.capture_environment(&requirements))
    }

    /// Capture selection for a fold body.
    ///
    /// `scope` is the ownership relation the DAE issued for this loop's body:
    /// it is what separates a definition the region computes once per iteration
    /// from one an enclosing scope already completed. Nothing here recovers
    /// that boundary from definition insertion order.
    pub(super) fn capture_environment_for_fold(
        &mut self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>> + Clone,
        fold: dae::FunctionFoldId<'dae>,
        scope: dae::FunctionScopeView<'dae>,
    ) -> Result<
        (
            Vec<solve::ProgramRegister<'program>>,
            EnvironmentLayout<'dae>,
        ),
        solve::SolveProgramConstructionError,
    > {
        self.issue_constant_aggregates(expressions.clone(), Some(fold.function()))?;
        let mut requirements = self.environment_requirements(expressions, Some(scope), false)?;
        requirements
            .fold_parameters
            .retain(|(candidate, _)| *candidate != fold);
        Ok(self.capture_environment(&requirements))
    }

    pub(super) fn capture_environment_for_branches(
        &mut self,
        branches: &[(Vec<dae::ExprId<'dae>>, dae::FunctionScopeView<'dae>)],
    ) -> Result<
        (
            Vec<solve::ProgramRegister<'program>>,
            EnvironmentLayout<'dae>,
        ),
        solve::SolveProgramConstructionError,
    > {
        let mut requirements = EnvironmentRequirements::default();
        for (expressions, scope) in branches {
            let branch =
                self.environment_requirements(expressions.iter().copied(), Some(*scope), false)?;
            requirements
                .model_coordinates
                .extend(branch.model_coordinates);
            requirements.parameters.extend(branch.parameters);
            requirements.values.extend(branch.values);
            requirements.expressions.extend(branch.expressions);
            requirements.fold_parameters.extend(branch.fold_parameters);
            requirements.binders.extend(branch.binders);
        }
        Ok(self.capture_environment(&requirements))
    }

    fn capture_environment(
        &self,
        requirements: &EnvironmentRequirements<'dae>,
    ) -> (
        Vec<solve::ProgramRegister<'program>>,
        EnvironmentLayout<'dae>,
    ) {
        let mut captures = Vec::new();
        let mut model_coordinates = self.model_coordinates.iter().collect::<Vec<_>>();
        model_coordinates.sort_by_key(|(key, _)| key.stable_key());
        let model_coordinates = model_coordinates
            .into_iter()
            .filter(|(key, _)| requirements.model_coordinates.contains(key))
            .map(|(key, value)| {
                let start = captures.len();
                captures.extend(value.leaves.iter().copied());
                (*key, value.value_type, start..captures.len())
            })
            .collect();
        let mut parameters = self.parameters.iter().collect::<Vec<_>>();
        parameters.sort_by_key(|(id, _)| **id);
        let parameters = parameters
            .into_iter()
            .filter(|(id, _)| requirements.parameters.contains(id))
            .map(|(id, value)| {
                let start = captures.len();
                captures.extend(value.leaves.iter().copied());
                (*id, value.value_type, start..captures.len())
            })
            .collect();
        let mut values = self.function_values.iter().collect::<Vec<_>>();
        values.sort_by_key(|(id, _)| **id);
        let values = values
            .into_iter()
            .filter(|(id, _)| requirements.values.contains(id))
            .map(|(id, value)| {
                let start = captures.len();
                captures.extend(value.leaves.iter().copied());
                (*id, value.value_type, start..captures.len())
            })
            .collect();
        let mut expressions = self.cache.iter().collect::<Vec<_>>();
        expressions.sort_by_key(|(id, _)| **id);
        let expressions = expressions
            .into_iter()
            .filter(|(id, _)| requirements.expressions.contains(id))
            .map(|(id, value)| {
                let start = captures.len();
                captures.extend(value.leaves.iter().copied());
                (*id, value.value_type, start..captures.len())
            })
            .collect();
        let mut fold_parameters = self.fold_parameters.iter().collect::<Vec<_>>();
        fold_parameters.sort_by_key(|((fold, carried), _)| {
            (fold.function().index(), fold.ordinal(), *carried)
        });
        let fold_parameters = fold_parameters
            .into_iter()
            .filter(|((fold, carried), _)| {
                requirements.fold_parameters.contains(&(*fold, *carried))
            })
            .map(|((fold, carried), value)| {
                let start = captures.len();
                captures.extend(value.leaves.iter().copied());
                (*fold, *carried, value.value_type, start..captures.len())
            })
            .collect();
        let mut binders = self.binders.iter().collect::<Vec<_>>();
        binders.sort_by_key(|(id, _)| **id);
        let binders = binders
            .into_iter()
            .filter(|(id, _)| requirements.binders.contains(id))
            .map(|(id, register)| {
                let start = captures.len();
                captures.push(*register);
                (*id, start..captures.len())
            })
            .collect();
        (
            captures,
            EnvironmentLayout {
                model_coordinates,
                parameters,
                values,
                expressions,
                fold_parameters,
                binders,
            },
        )
    }

    /// Whether this scope issued a value for `expression` that a region reads
    /// as one capture, so nothing beneath it is demanded of the region.
    fn has_issued_value(
        &self,
        expression: dae::ExprId<'dae>,
        private: &std::collections::BTreeSet<dae::ExprId<'dae>>,
    ) -> bool {
        !private.contains(&expression)
            && self
                .cache
                .get(&expression)
                .is_some_and(|value| !value.leaves.is_empty())
    }

    /// The literals a loop of the scope's function takes over as the entry
    /// value of a carried array: a region does not read them as captures,
    /// because the loop owns and updates that value in place.
    fn private_literals(
        &self,
        scope: Option<dae::FunctionScopeView<'dae>>,
    ) -> std::collections::BTreeSet<dae::ExprId<'dae>> {
        scope.map_or_else(Default::default, |scope| {
            self.carried_entry_literals(scope.function())
        })
    }

    /// Requirement set of `expressions`, after resolving every definition they
    /// read that this scope owns but has not yet computed.
    ///
    /// Resolving first is what keeps a region's environment exact and its body
    /// compact. A read of a definition produced by an earlier sibling region is
    /// computed here, in the scope that owns the sibling, and then captured as
    /// one value - instead of being recomputed inside the region on every
    /// iteration.
    fn environment_requirements(
        &mut self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>> + Clone,
        scope: Option<dae::FunctionScopeView<'dae>>,
        issue_eager_calls: bool,
    ) -> Result<EnvironmentRequirements<'dae>, solve::SolveProgramConstructionError> {
        // One workspace serves both passes of this capture: the visited set is
        // the whole expression arena, so sizing it per root would cost
        // `roots * arena` before either walk touched an operand.
        let mut traversal = dae::ExpressionTraversal::new();
        if issue_eager_calls {
            self.issue_demanded_calls(&mut traversal, expressions.clone())?;
        }
        for definition in self.pending_definitions(&mut traversal, expressions.clone(), scope)? {
            self.function_definition_value(definition)?;
        }
        let mut requirements = EnvironmentRequirements::default();
        let function_values = &self.function_values;
        let private = self.private_literals(scope);
        let issued = |id| self.has_issued_value(id, &private);
        let fold_parameters = &self.fold_parameters;
        self.visit_capture_expressions(&mut traversal, expressions, |id, node| {
            match node.operation() {
                operation if self.has_pending_assigned_source(operation) => false,
                dae::ExpressionOperation::Literal(_) => false,
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                    parameter,
                )) => {
                    requirements.parameters.insert(parameter);
                    false
                }
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
                    requirements
                        .binders
                        .insert((binder.domain().index(), binder.ordinal()));
                    false
                }
                dae::ExpressionOperation::Coordinate(coordinate) => {
                    requirements.record_model_coordinate(coordinate);
                    false
                }
                dae::ExpressionOperation::FunctionValue { definition, .. }
                | dae::ExpressionOperation::FunctionConditionalOutput { definition, .. }
                | dae::ExpressionOperation::FunctionFoldOutput { definition, .. }
                    if function_values.contains_key(&definition.id()) =>
                {
                    requirements.values.insert(definition.id());
                    false
                }
                dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. }
                    if fold_parameters.contains_key(&(fold, carried)) =>
                {
                    requirements.fold_parameters.insert((fold, carried));
                    false
                }
                dae::ExpressionOperation::FunctionFoldParameter { .. } => false,
                // A value this scope already issued is read, not lowered again
                // inside the region: one identity per scope.
                _ if issued(id) => {
                    requirements.expressions.insert(id);
                    false
                }
                _ => true,
            }
        });
        Ok(requirements)
    }

    pub(super) fn visit_capture_expressions(
        &self,
        traversal: &mut dae::ExpressionTraversal<'dae>,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
        visit: impl FnMut(dae::ExprId<'dae>, dae::ExpressionView<'dae>) -> bool,
    ) {
        traversal.visit_pruned_with_conditional_states(
            self.view,
            expressions,
            |conditional, value| self.assigned_values.contains_key(&(conditional, value)),
            visit,
        );
    }

    /// Definitions read by `expressions` that this scope owns and has not
    /// computed yet.
    ///
    /// A definition the DAE issued inside the region being built is stepped
    /// through rather than resolved: the region computes it itself, and this
    /// walk only needs the operands it reads. A definition an enclosing scope
    /// issued is resolved here, once, and captured. A definition that belongs
    /// to neither is a wire the DAE could not have issued - a sibling region's
    /// per-iteration value is not in scope here - so it is rejected instead of
    /// being placed on a guessed side of the boundary.
    fn pending_definitions(
        &self,
        traversal: &mut dae::ExpressionTraversal<'dae>,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
        scope: Option<dae::FunctionScopeView<'dae>>,
    ) -> Result<Vec<dae::FunctionDefinitionView<'dae>>, solve::SolveProgramConstructionError> {
        let mut pending = Vec::new();
        let mut seen = HashSet::new();
        let mut rejected = None;
        let private = self.private_literals(scope);
        self.visit_capture_expressions(traversal, expressions, |expression, node| {
            if self.has_pending_assigned_source(node.operation()) {
                return false;
            }
            let (dae::ExpressionOperation::FunctionValue { definition, .. }
            | dae::ExpressionOperation::FunctionConditionalOutput { definition, .. }
            | dae::ExpressionOperation::FunctionFoldOutput { definition, .. }) = node.operation()
            else {
                // The region reads a value this scope issued; the definitions
                // beneath it were resolved when it was.
                return !self.has_issued_value(expression, &private);
            };
            let id = definition.id();
            if self.function_values.contains_key(&id) {
                return false;
            }
            match scope.map(|scope| scope.relation(id)) {
                Some(Some(dae::FunctionScopeRelation::Region)) => return true,
                Some(Some(dae::FunctionScopeRelation::Enclosing)) | None => {}
                Some(Some(dae::FunctionScopeRelation::Disjoint)) | Some(None) => {
                    rejected.get_or_insert(definition.provenance().span());
                    return false;
                }
            }
            if seen.insert(id) {
                pending.push(definition);
            }
            false
        });
        if let Some(provenance) = rejected {
            return Err(solve::SolveProgramConstructionError::InvalidRegion { provenance });
        }
        Ok(pending)
    }
}
