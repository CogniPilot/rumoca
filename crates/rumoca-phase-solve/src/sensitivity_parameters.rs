//! Which model parameters a derivative may be taken with respect to.
//!
//! A parameter is a differentiation variable only when changing its runtime
//! slot reproduces what recompiling with the new value would produce: it must
//! be a tunable, source-declared `Real` parameter, no other parameter's
//! expression may read it (a dependent binding is evaluated once at lowering,
//! so it would not follow the changed slot), some Solve program must read it,
//! and the initialization must not define it (SOLVE-C74). A state whose
//! `start` reads it must be one the initialization solves or updates, or the
//! lowered model fixed that start as a constant (SOLVE-C75).
//!
//! The decision is made here once, where the DAE and its Solve lowering are
//! both visible, and is carried as a `solve::ParameterClassification` into
//! `solve::SensitivityProblem::construct`, which refuses a request against it.
//! The excluded-parameter report is the same classification.

use std::collections::{BTreeMap, BTreeSet};

use indexmap::IndexSet;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use solve::{ExcludedParameter, ExclusionReason, ParameterClassification};

/// The storage the initialization system defines, from the Solve problem.
struct InitializationOwned {
    /// Parameter slots a projection unknown defines (`fixed = false`).
    parameters: BTreeSet<usize>,
    /// Solver-vector indices the projection solves or the updates write.
    solver: BTreeSet<usize>,
}

impl InitializationOwned {
    fn of(problem: &solve::SolveProblem) -> Self {
        let initialization = &problem.initialization;
        let parameters = initialization
            .projection_unknowns()
            .iter()
            .filter_map(|slot| match slot {
                solve::ScalarSlot::P { index, .. } => Some(*index),
                _ => None,
            })
            .collect();
        let solver = initialization
            .projection_unknowns()
            .iter()
            .chain(initialization.update_targets())
            .filter_map(|slot| match slot {
                solve::ScalarSlot::Y { index, .. } => Some(*index),
                _ => None,
            })
            .collect();
        Self { parameters, solver }
    }
}

/// Scalar names of every independent tunable `Real` parameter of `model`, in
/// declaration order.
#[must_use]
pub fn independent_tunable_parameters(model: &dae::Dae) -> Vec<String> {
    classify(model).selected().to_vec()
}

/// Classify every parameter of `model` against its Solve `problem`.
#[must_use]
pub fn select_sensitivity_parameters(
    model: &dae::Dae,
    problem: &solve::SolveProblem,
) -> ParameterClassification {
    let read = solve::read_parameter_slots(problem);
    let owned = InitializationOwned::of(problem);
    let readers = start_readers(model);
    let maps = &problem.solve_layout.solver_maps;
    let dae_selection = classify(model);
    let mut selected = Vec::new();
    let mut excluded = dae_selection.excluded().to_vec();
    let mut baked_starts = BTreeMap::new();
    for name in dae_selection.selected() {
        let reason = match problem.layout.binding(name) {
            Some(solve::ScalarSlot::P { index, .. }) if !read.contains(&index) => {
                Some(ExclusionReason::FoldedAtTranslation)
            }
            Some(solve::ScalarSlot::P { index, .. }) if owned.parameters.contains(&index) => {
                Some(ExclusionReason::InitializationDefined)
            }
            Some(solve::ScalarSlot::P { .. }) => None,
            _ => Some(ExclusionReason::FoldedAtTranslation),
        };
        if let Some(reason) = reason {
            excluded.push(ExcludedParameter {
                name: name.clone(),
                reason,
            });
            continue;
        }
        let baked: Vec<String> = readers
            .get(name)
            .into_iter()
            .flatten()
            .filter(|state| {
                maps.name_to_idx
                    .get(state.as_str())
                    .is_none_or(|index| !owned.solver.contains(index))
            })
            .cloned()
            .collect();
        if !baked.is_empty() {
            baked_starts.insert(name.clone(), baked);
        }
        selected.push(name.clone());
    }
    ParameterClassification::new(selected, excluded, baked_starts)
}

/// The DAE-only part of the classification: which parameters are tunable,
/// independent, real, and source declared.
fn classify(model: &dae::Dae) -> ParameterClassification {
    model.inspect(|view| {
        let participants = parameter_dependency_participants(view);
        let mut selected = Vec::new();
        let mut excluded = Vec::new();
        for (id, variable) in view
            .variables()
            .filter(|(_, variable)| variable.role() == dae::VariableRole::Parameter)
        {
            let reason = exclusion(id, variable, &participants);
            for name in (0..variable.scalar_count())
                .filter_map(|scalar| variable.scalar_name(scalar))
                .filter(|name| !name.starts_with("__"))
            {
                match reason {
                    Some(reason) => excluded.push(ExcludedParameter { name, reason }),
                    None => selected.push(name),
                }
            }
        }
        ParameterClassification::new(selected, excluded, BTreeMap::new())
    })
}

fn exclusion<'dae>(
    id: dae::VariableId<'dae>,
    variable: dae::VariableView<'dae>,
    participants: &Participants,
) -> Option<ExclusionReason> {
    if variable.value_type().scalar_type() != dae::ScalarType::Real {
        return Some(ExclusionReason::NotReal);
    }
    if !variable.is_tunable() || variable.causality() != dae::VariableCausality::Parameter {
        return Some(ExclusionReason::NotTunable);
    }
    if variable.origin() != dae::VariableOrigin::Source {
        return Some(ExclusionReason::Generated);
    }
    if participants.dependents.contains(&id.index()) {
        return Some(ExclusionReason::DependsOnParameters);
    }
    if participants.read.contains(&id.index()) {
        return Some(ExclusionReason::ReadByParameters);
    }
    None
}

/// Parameters whose binding reads others (`dependents`) and the parameters so
/// read (`read`).
#[derive(Default)]
struct Participants {
    dependents: IndexSet<u32>,
    read: IndexSet<u32>,
}

fn parameter_dependency_participants(view: dae::DaeView<'_>) -> Participants {
    let mut participants = Participants::default();
    for (id, variable) in view
        .variables()
        .filter(|(_, variable)| variable.role() == dae::VariableRole::Parameter)
    {
        let refs = parameter_dependency_refs(view, variable);
        if refs.is_empty() {
            continue;
        }
        participants.dependents.insert(id.index());
        participants.read.extend(refs);
    }
    participants
}

/// The parameters a variable's `start` expression reads.
fn parameter_start_refs<'dae>(
    view: dae::DaeView<'dae>,
    variable: dae::VariableView<'dae>,
) -> IndexSet<u32> {
    variable.start().map_or_else(IndexSet::new, |start| {
        expression_parameter_refs(view, start)
    })
}

/// The parameters a parameter's own binding or `start` expression reads. The
/// declared start may already be the evaluated constant, so the binding is the
/// authority for a dependent parameter.
fn parameter_dependency_refs<'dae>(
    view: dae::DaeView<'dae>,
    variable: dae::VariableView<'dae>,
) -> IndexSet<u32> {
    let mut refs = parameter_start_refs(view, variable);
    if let Some(binding) = variable.binding() {
        refs.extend(expression_parameter_refs(view, binding));
    }
    refs
}

fn expression_parameter_refs<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> IndexSet<u32> {
    let mut references = IndexSet::new();
    dae::for_each_expression(view, expression, |_, node| {
        if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(id)) =
            node.operation()
        {
            references.insert(id.index());
        }
    });
    references
}

/// For each parameter scalar name, the scalar names of the states whose
/// `start` expression reads it.
///
/// A state whose initial value is a constant of the lowered model cannot
/// follow such a parameter, so a derivative with respect to it would silently
/// carry a zero initial sensitivity unless the initialization owns the state.
fn start_readers(model: &dae::Dae) -> BTreeMap<String, Vec<String>> {
    model.inspect(|view| {
        let names_by_id: BTreeMap<u32, Vec<String>> = view
            .variables()
            .filter(|(_, variable)| variable.role() == dae::VariableRole::Parameter)
            .map(|(id, variable)| {
                let names = (0..variable.scalar_count())
                    .filter_map(|scalar| variable.scalar_name(scalar))
                    .collect();
                (id.index(), names)
            })
            .collect();
        let mut readers: BTreeMap<String, Vec<String>> = BTreeMap::new();
        for (_, variable) in view
            .variables()
            .filter(|(_, variable)| variable.role() == dae::VariableRole::State)
        {
            let states: Vec<String> = (0..variable.scalar_count())
                .filter_map(|scalar| variable.scalar_name(scalar))
                .collect();
            let names = parameter_start_refs(view, variable)
                .into_iter()
                .filter_map(|id| names_by_id.get(&id))
                .flatten();
            for parameter in names {
                readers
                    .entry(parameter.clone())
                    .or_default()
                    .extend(states.iter().cloned());
            }
        }
        readers
    })
}
