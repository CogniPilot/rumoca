//! Parameter binding ownership shared by FMI metadata and C admission.

use super::{FmiCausality, FmiVariable};
use crate::{ScalarSlot, SolveProblem, SolvePureCallTable, SolveVariableStorageRole};

pub(super) fn classify_metadata(problem: &SolveProblem, variables: &mut [FmiVariable]) {
    let owned: std::collections::BTreeSet<_> = problem
        .initialization
        .update_targets()
        .iter()
        .chain(problem.initialization.projection_unknowns())
        .filter_map(|slot| match slot {
            ScalarSlot::P { index, .. } => Some(*index),
            _ => None,
        })
        .collect();
    classify_initialized_coordinates(problem, variables);
    for variable in variables {
        if variable.role() != Some(SolveVariableStorageRole::Parameter) {
            continue;
        }
        let Some(storage) = variable.backing.storage() else {
            continue;
        };
        if (storage.base()..storage.base() + storage.scalar_count()).any(|i| owned.contains(&i)) {
            variable.causality = FmiCausality::CalculatedParameter;
            let binding_owned = problem.initialization.update_targets().iter().any(|slot| {
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

/// FMI 2.0.5 §2.2.7 and FMI 3.0.2 §2.4.7: a non-parameter coordinate whose
/// start value the initialization may change is `approx` (a state or other
/// coordinate the initialization projection solves for keeps its start as the
/// guess), and one a discrete equation defines is `calculated` (its start is
/// not its value).
fn classify_initialized_coordinates(problem: &SolveProblem, variables: &mut [FmiVariable]) {
    let projected: std::collections::BTreeSet<_> = problem
        .initialization
        .projection_unknowns()
        .iter()
        .filter_map(|slot| match slot {
            ScalarSlot::Y { index, .. } => Some(*index),
            _ => None,
        })
        .collect();
    let discrete = &problem.discrete;
    let defined: std::collections::BTreeSet<_> = discrete
        .update_targets
        .iter()
        .zip(&discrete.row_roles)
        .filter(|(_, role)| **role == crate::DiscreteRowRole::Equation)
        .filter_map(|(slot, _)| match slot {
            ScalarSlot::P { index, .. } => Some(*index),
            _ => None,
        })
        .collect();
    for variable in variables.iter_mut() {
        let Some(storage) = variable.backing.storage() else {
            continue;
        };
        if matches!(
            variable.role(),
            Some(SolveVariableStorageRole::Parameter | SolveVariableStorageRole::Constant) | None
        ) || variable.causality == FmiCausality::Input
        {
            continue;
        }
        let range = storage.base()..storage.base() + storage.scalar_count();
        let column = storage.column();
        if column == super::FmiStorageColumn::P && range.clone().any(|i| defined.contains(&i)) {
            variable.initial = Some(super::FmiInitial::Calculated);
            variable.start = None;
        } else if column == super::FmiStorageColumn::Y
            && variable.initial == Some(super::FmiInitial::Exact)
            && range.clone().any(|i| projected.contains(&i))
        {
            variable.initial = Some(super::FmiInitial::Approx);
        }
    }
}

/// Admit the parameter bindings and return the order the generated component
/// evaluates their programs in, with the number of dependency levels (the
/// repeated sweeps the runtime needs to settle them).
///
/// The runtime applies the bindings as a Jacobi sweep repeated until no value
/// changes (`eval_and_apply_update_rows`), whatever their issued order. Each
/// program is a function of settled parameters and constants alone, so the
/// fixed point assigns every target the value its program computes from the
/// final values of its reads; evaluating the programs once in dependency
/// order computes exactly those values, bit for bit. A binding that reads
/// time, a continuous coordinate, an input, or anything in a cycle is refused.
pub(super) fn validate(
    problem: &SolveProblem,
    calls: &SolvePureCallTable,
) -> Result<(Vec<usize>, usize), &'static str> {
    let init = &problem.initialization;
    validate_projection(problem)?;
    let mut p = stable_parameters(problem);
    let y = vec![false; problem.layout.y_scalars()];
    let mut targets = Vec::with_capacity(init.update_targets().len());
    for slot in init.update_targets() {
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
    let programs = init.update_rhs().programs();
    let mut outputs = Vec::with_capacity(programs.len());
    let mut cursor = 0;
    for program in programs {
        let count = crate::ScalarProgramBlock::program_output_count(program);
        let written = init
            .update_rhs()
            .output_indices()
            .get(cursor..cursor + count)
            .ok_or("missing parameter binding output index")?
            .iter()
            .map(|output| targets.get(*output).copied())
            .collect::<Option<Vec<_>>>()
            .ok_or("missing parameter update target")?;
        outputs.push(written);
        cursor += count;
    }
    if cursor != targets.len() {
        return Err("missing parameter binding output");
    }
    let candidates = (0..programs.len()).collect::<Vec<_>>();
    use super::static_assertions::scalar_dependencies::{OrderError, dependency_order};
    dependency_order(calls, programs, &candidates, &outputs, &y, &mut p).map_err(
        |error| match error {
            OrderError::Unsupported => "unsupported parameter binding dependency operation",
            OrderError::Unsettled => {
                "parameter binding depends on unsettled or non-parameter values"
            }
        },
    )
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

/// The initialization projection the C kernel runs: the runtime's
/// `settle_initialization_system` without a homotopy sweep, delay history,
/// or retained manifold rows, for residual rows evaluated on the settled
/// coordinates (the bindings and the algebraic refresh applied first, as the
/// runtime does whenever a row reads a reconstructed algebraic). Rows over
/// declaration seeds need single-row isolation and the forward-mode initial
/// Jacobian, which the C kernel does not emit.
fn validate_projection(problem: &SolveProblem) -> Result<(), &'static str> {
    use crate::{InitializationCoordinateKind, InitializationRowRole};
    let init = &problem.initialization;
    let rows = init
        .residual()
        .len()
        .map_err(|_| "the initialization residual has no checked row count")?;
    if rows == 0 {
        return if init.projection_plan().is_empty() {
            Ok(())
        } else {
            Err("C initialization projects blocks without residual rows")
        };
    }
    if init.manifold_row_count() > 0 {
        return Err("C initialization cannot certify retained state-manifold rows");
    }
    if problem
        .solve_layout
        .initial_homotopy_parameter_index
        .is_some()
    {
        return Err("C initialization cannot run the homotopy continuation");
    }
    let delays = &problem.events.delays;
    if !delays.source_rhs.is_empty() || !delays.value_parameter_indices.is_empty() {
        return Err("C initialization cannot synchronize delay histories");
    }
    let settled = init.row_roles().iter().any(|role| {
        matches!(
            role,
            InitializationRowRole::UnownedCoordinate(InitializationCoordinateKind::Algebraic)
                | InitializationRowRole::SolvedThroughAlgebraicRefresh
                | InitializationRowRole::SurplusAlgebraicCheck
        )
    });
    if !settled {
        return Err(
            "C initialization evaluates residual rows only on the settled algebraic refresh; rows over declaration seeds need row isolation and a forward initial Jacobian",
        );
    }
    if problem.solve_layout.solver_scalar_count() != problem.layout.y_scalars() {
        return Err("C initialization needs the solver coordinates to fill the Y storage");
    }
    Ok(())
}
