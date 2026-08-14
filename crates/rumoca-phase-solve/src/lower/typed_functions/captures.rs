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
    fold_parameters: HashSet<(dae::FunctionFoldId<'dae>, u32)>,
    binders: HashSet<(u32, u32)>,
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
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
        let requirements = self.environment_requirements(expressions, None)?;
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
        let mut requirements = self.environment_requirements(expressions, Some(scope))?;
        requirements
            .fold_parameters
            .retain(|(candidate, _)| *candidate != fold);
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
                fold_parameters,
                binders,
            },
        )
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
    ) -> Result<EnvironmentRequirements<'dae>, solve::SolveProgramConstructionError> {
        // One workspace serves both passes of this capture: the visited set is
        // the whole expression arena, so sizing it per root would cost
        // `roots * arena` before either walk touched an operand.
        let mut traversal = dae::ExpressionTraversal::new();
        for definition in self.pending_definitions(&mut traversal, expressions.clone(), scope)? {
            self.function_definition_value(definition)?;
        }
        let mut requirements = EnvironmentRequirements::default();
        let function_values = &self.function_values;
        let fold_parameters = &self.fold_parameters;
        traversal.visit_pruned(self.view, expressions, |_, node| match node.operation() {
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
                if let Some(key) = ModelCoordinateKey::from_view(coordinate) {
                    requirements.model_coordinates.insert(key);
                }
                false
            }
            dae::ExpressionOperation::FunctionValue { definition, .. }
                if function_values.contains_key(&definition.id()) =>
            {
                requirements.values.insert(definition.id());
                false
            }
            dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. } => {
                if fold_parameters.contains_key(&(fold, carried)) {
                    requirements.fold_parameters.insert((fold, carried));
                }
                false
            }
            _ => true,
        });
        Ok(requirements)
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
        traversal.visit_pruned(self.view, expressions, |_, node| {
            let dae::ExpressionOperation::FunctionValue { definition, .. } = node.operation()
            else {
                return true;
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
