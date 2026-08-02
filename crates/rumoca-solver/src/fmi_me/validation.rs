//! Construction-time validation for the linked FMI ME component.
//!
//! A component must reject a continuous system it cannot evaluate from the
//! standard state/parameter surface. Keeping this proof obligation here means
//! every numerical solver sees the same admissible model set.

use std::collections::{BTreeMap, BTreeSet};

use rumoca_ir_solve as solve;

use super::MeError;

pub(super) fn validate_explicit_solve_model(model: &solve::SolveModel) -> Result<(), MeError> {
    let state_count = model.state_scalar_count();
    if state_count == 0 {
        return Err(MeError::NoContinuousStates);
    }
    if model.initial_y.len() != model.solver_scalar_count() {
        return Err(evaluation(format!(
            "initial vector length {} does not match solver layout {}",
            model.initial_y.len(),
            model.solver_scalar_count()
        )));
    }
    let derivative_count = model
        .problem
        .continuous
        .derivative_rhs
        .len()
        .map_err(|error| evaluation(error.to_string()))?;
    if derivative_count != state_count {
        return Err(unsupported(format!(
            "the model declares {state_count} continuous state(s) but lowers to \
             {derivative_count} state-derivative row(s), so no reduced state-only ODE exists"
        )));
    }
    let derivative_rows =
        rumoca_eval_solve::to_scalar_program_block(&model.problem.continuous.derivative_rhs)
            .map_err(|error| evaluation(error.to_string()))?;
    let direct_dependencies = derivative_non_state_loads(model, &derivative_rows);
    if direct_dependencies.is_empty() {
        return Ok(());
    }
    require_projection_producers(model, direct_dependencies)
}

fn require_projection_producers(
    model: &solve::SolveModel,
    direct_dependencies: BTreeMap<usize, usize>,
) -> Result<(), MeError> {
    let state_count = model.state_scalar_count();
    let solver_count = model.solver_scalar_count();
    let implicit_rows =
        rumoca_eval_solve::to_scalar_program_block(&model.problem.continuous.implicit_rhs)
            .map_err(|error| evaluation(error.to_string()))?;
    let producer_rows = projection_producer_rows(model, implicit_rows.output_count())?;
    let mut visited = BTreeSet::new();
    let mut pending = direct_dependencies.into_iter().collect::<Vec<_>>();
    while let Some((index, state_index)) = pending.pop() {
        if index < state_count || !visited.insert(index) {
            continue;
        }
        let Some(output_index) = producer_rows.get(&index).copied() else {
            return Err(unsupported(format!(
                "der({}) reads solver coordinate '{}' (y[{index}]), which no algebraic \
                 projection block produces; the general/implicit DAE fallback was retired \
                 (SPEC 0038)",
                coordinate_name(model, state_index),
                coordinate_name(model, index),
            )));
        };
        let row = implicit_rows
            .program_index_for_output(output_index)
            .and_then(|program_index| implicit_rows.program(program_index))
            .ok_or_else(|| {
                unsupported(format!(
                    "solver coordinate '{}' (y[{index}]) is projected from implicit residual \
                     output {output_index}, which carries no evaluable scalar program",
                    coordinate_name(model, index),
                ))
            })?;
        pending.extend(
            non_state_y_loads(row, state_count, solver_count)
                .into_iter()
                .map(|dependency| (dependency, state_index)),
        );
    }
    Ok(())
}

fn derivative_non_state_loads(
    model: &solve::SolveModel,
    derivative_rows: &solve::ScalarProgramBlock,
) -> BTreeMap<usize, usize> {
    let state_count = model.state_scalar_count();
    let solver_count = model.solver_scalar_count();
    let mut dependencies = BTreeMap::new();
    for (state_index, row) in derivative_rows
        .programs()
        .iter()
        .take(state_count)
        .enumerate()
    {
        for dependency in non_state_y_loads(row, state_count, solver_count) {
            dependencies.entry(dependency).or_insert(state_index);
        }
    }
    dependencies
}

fn projection_producer_rows(
    model: &solve::SolveModel,
    implicit_output_count: usize,
) -> Result<BTreeMap<usize, usize>, MeError> {
    let mut producer_rows = BTreeMap::new();
    for block in &model.problem.continuous.algebraic_projection_plan.blocks {
        for (row_index, target_index) in block
            .rows
            .iter()
            .copied()
            .zip(block.y_indices.iter().copied())
        {
            if row_index >= implicit_output_count {
                return Err(unsupported(format!(
                    "the algebraic projection plan references implicit residual row \
                     {row_index}, but the implicit residual emits only \
                     {implicit_output_count} output(s)"
                )));
            }
            if let Some(previous_row) = producer_rows.insert(target_index, row_index)
                && previous_row != row_index
            {
                return Err(unsupported(format!(
                    "solver coordinate '{}' (y[{target_index}]) is claimed by two algebraic \
                     projection rows ({previous_row} and {row_index})",
                    coordinate_name(model, target_index),
                )));
            }
        }
    }
    Ok(producer_rows)
}

fn non_state_y_loads(
    row: &[solve::LinearOp],
    state_count: usize,
    solver_count: usize,
) -> Vec<usize> {
    let mut loads = row
        .iter()
        .filter_map(|operation| match *operation {
            solve::LinearOp::LoadY { index, .. }
                if index >= state_count && index < solver_count =>
            {
                Some(index)
            }
            _ => None,
        })
        .collect::<Vec<_>>();
    loads.sort_unstable();
    loads.dedup();
    loads
}

fn coordinate_name(model: &solve::SolveModel, index: usize) -> String {
    model
        .problem
        .solve_layout
        .solver_maps
        .names
        .get(index)
        .cloned()
        .unwrap_or_else(|| format!("y[{index}]"))
}

fn unsupported(reason: String) -> MeError {
    MeError::UnsupportedModel { reason }
}

fn evaluation(message: String) -> MeError {
    MeError::Evaluation { message }
}
