//! Exact semantic capture selection for checked typed regions.

use std::collections::HashSet;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{EnvironmentLayout, ExpressionLowerer, ModelCoordinateKey};

#[derive(Default)]
struct EnvironmentRequirements<'dae> {
    model_coordinates: HashSet<ModelCoordinateKey<'dae>>,
    parameters: HashSet<dae::FunctionParameterId<'dae>>,
    values: HashSet<dae::FunctionValueId<'dae>>,
    fold_parameters: HashSet<(dae::FunctionFoldId<'dae>, u32)>,
    binders: HashSet<(u32, u32)>,
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    pub(super) fn capture_environment_for(
        &self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
    ) -> (
        Vec<solve::ProgramRegister<'program>>,
        EnvironmentLayout<'dae>,
    ) {
        let requirements = self.environment_requirements(expressions);
        self.capture_environment(&requirements)
    }

    pub(super) fn capture_environment_for_fold(
        &self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
        fold: dae::FunctionFoldId<'dae>,
        carried_targets: &[dae::FunctionValueId<'dae>],
    ) -> (
        Vec<solve::ProgramRegister<'program>>,
        EnvironmentLayout<'dae>,
    ) {
        let mut requirements = self.environment_requirements(expressions);
        for target in carried_targets {
            requirements.values.remove(target);
        }
        requirements
            .fold_parameters
            .retain(|(candidate, _)| *candidate != fold);
        self.capture_environment(&requirements)
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

    fn environment_requirements(
        &self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
    ) -> EnvironmentRequirements<'dae> {
        let mut requirements = EnvironmentRequirements::default();
        for root in expressions {
            dae::for_each_expression_pruned(self.view, root, |_, node| match node.operation() {
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
                dae::ExpressionOperation::FunctionValue { value, .. }
                    if self.function_values.contains_key(&value) =>
                {
                    requirements.values.insert(value);
                    false
                }
                dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. } => {
                    if self.fold_parameters.contains_key(&(fold, carried)) {
                        requirements.fold_parameters.insert((fold, carried));
                    }
                    false
                }
                _ => true,
            });
        }
        requirements
    }
}
