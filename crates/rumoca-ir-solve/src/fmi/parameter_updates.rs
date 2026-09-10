//! Parameter binding ownership shared by FMI metadata and C admission.

use super::{FmiCausality, FmiVariable};
use crate::{ScalarSlot, SolveProblem, SolvePureCallTable, SolveVariableStorageRole};

pub(super) fn classify_metadata(problem: &SolveProblem, variables: &mut [FmiVariable]) {
    let owned: std::collections::BTreeSet<_> = problem
        .initialization
        .update_targets
        .iter()
        .chain(&problem.initialization.projection_unknowns)
        .filter_map(|slot| match slot {
            ScalarSlot::P { index, .. } => Some(*index),
            _ => None,
        })
        .collect();
    for variable in variables {
        if variable.role() != Some(SolveVariableStorageRole::Parameter) {
            continue;
        }
        let Some(storage) = variable.backing.storage() else {
            continue;
        };
        if (storage.base()..storage.base() + storage.scalar_count()).any(|i| owned.contains(&i)) {
            variable.causality = FmiCausality::CalculatedParameter;
            let binding_owned = problem.initialization.update_targets.iter().any(|slot| {
                matches!(slot, ScalarSlot::P { index, .. }
                    if *index >= storage.base() && *index - storage.base() < storage.scalar_count())
            });
            if binding_owned {
                variable.initial = Some(super::FmiInitial::Calculated);
                variable.start = None;
            } else {
                variable.initial = Some(super::FmiInitial::Approx);
            }
        }
    }
}

pub(super) fn validate(
    problem: &SolveProblem,
    calls: &SolvePureCallTable,
) -> Result<(), &'static str> {
    let init = &problem.initialization;
    if !init.residual.is_empty()
        || !init.projection_unknowns.is_empty()
        || !init.projection_plan.is_empty()
    {
        return Err(
            "C initialization requires parameter assignments without residuals or projection",
        );
    }
    let mut p = stable_parameters(problem);
    let y = vec![false; problem.layout.y_scalars()];
    let mut targets = Vec::with_capacity(init.update_targets.len());
    for slot in &init.update_targets {
        let ScalarSlot::P { index, .. } = slot else {
            return Err("C initialization can only assign parameter storage");
        };
        if !problem.solve_layout.variable_storage_runs.iter().any(|run| {
            run.role == SolveVariableStorageRole::Parameter && matches!(run.base,
                ScalarSlot::P { index: base, .. } if *index >= base && *index - base < run.scalar_count)
        }) {
            return Err("C initialization can only assign parameters");
        }
        targets.push(*index);
    }
    for target in &targets {
        p[*target] = false;
    }
    let mut cursor = 0;
    for program in init.update_rhs.programs() {
        let values =
            super::static_assertions::scalar_dependencies::program_outputs(calls, program, &y, &p)
                .ok_or("unsupported parameter binding dependency operation")?;
        for value in values {
            let output = init
                .update_rhs
                .output_indices()
                .get(cursor)
                .ok_or("missing parameter binding output index")?;
            let target = targets
                .get(*output)
                .ok_or("missing parameter update target")?;
            if !value {
                return Err("parameter binding depends on unsettled or non-parameter values");
            }
            p[*target] = true;
            cursor += 1;
        }
    }
    if cursor != targets.len() {
        return Err("missing parameter binding output");
    }
    Ok(())
}

pub(super) fn stable_parameters(problem: &SolveProblem) -> Vec<bool> {
    let mut p = vec![false; problem.layout.p_scalars()];
    for run in &problem.solve_layout.variable_storage_runs {
        if !matches!(
            run.role,
            SolveVariableStorageRole::Parameter | SolveVariableStorageRole::Constant
        ) {
            continue;
        }
        if let ScalarSlot::P { index, .. } = run.base {
            p[index..index + run.scalar_count].fill(true);
        }
    }
    p
}
