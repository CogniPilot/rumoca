use std::collections::{BTreeMap, BTreeSet};

use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;

pub(crate) fn can_use_state_only_bdf(model: &solve::SolveModel) -> bool {
    let state_count = model.state_scalar_count();
    if model.problem.continuous.derivative_rhs.len() != state_count {
        return false;
    }
    let derivative_rows =
        solve_eval::to_scalar_program_block(&model.problem.continuous.derivative_rhs);
    // The state-only path reduces the system to an explicit ODE: it projects the
    // algebraics from the states each step and integrates only the states. This
    // is far more robust than handing the full mass-matrix DAE to BDF, so it is
    // used whenever the derivatives' non-state dependencies can be refreshed from
    // the states by the algebraic projection plan. Its exact state Jacobian comes
    // from the projection-aware AD JVP (`SolveRuntime::eval_state_jacobian_v_ad_into`),
    // which propagates the seed through the same projection (`d(alg)/d(state)`)
    // before applying the derivative JVP. Models whose derivatives read algebraics
    // the projection plan cannot produce fall back to the general/implicit path.
    let direct_deps = derivative_non_state_loads(model, &derivative_rows);
    direct_deps.is_empty() || projection_plan_covers_non_state_loads(model, direct_deps)
}

fn derivative_non_state_loads(
    model: &solve::SolveModel,
    derivative_rows: &solve::ScalarProgramBlock,
) -> BTreeSet<usize> {
    let state_count = model.state_scalar_count();
    let solver_count = model.solver_scalar_count();
    derivative_rows
        .programs
        .iter()
        .take(state_count)
        .flat_map(|row| non_state_y_loads(row, state_count, solver_count))
        .collect()
}

fn projection_plan_covers_non_state_loads(
    model: &solve::SolveModel,
    direct_deps: BTreeSet<usize>,
) -> bool {
    let state_count = model.state_scalar_count();
    let solver_count = model.solver_scalar_count();
    let implicit_rows = solve_eval::to_scalar_program_block(&model.problem.continuous.implicit_rhs);
    let Some(producer_rows) = projection_producer_rows(model, implicit_rows.len()) else {
        return false;
    };
    let mut needed = BTreeSet::new();
    let mut stack = direct_deps.into_iter().collect::<Vec<_>>();
    while let Some(index) = stack.pop() {
        if index < state_count || !needed.insert(index) {
            continue;
        }
        let Some(row_idx) = producer_rows.get(&index).copied() else {
            return false;
        };
        let Some(row) = implicit_rows.programs.get(row_idx) else {
            return false;
        };
        stack.extend(non_state_y_loads(row, state_count, solver_count));
    }
    true
}

fn projection_producer_rows(
    model: &solve::SolveModel,
    implicit_row_count: usize,
) -> Option<BTreeMap<usize, usize>> {
    let mut producer_rows = BTreeMap::new();
    for block in &model.problem.continuous.algebraic_projection_plan.blocks {
        for (row_idx, target_index) in block
            .rows
            .iter()
            .copied()
            .zip(block.y_indices.iter().copied())
            .chain(
                block
                    .causal_steps
                    .iter()
                    .map(|step| (step.row, step.y_index)),
            )
        {
            if row_idx >= implicit_row_count {
                return None;
            }
            if let Some(previous_row) = producer_rows.insert(target_index, row_idx)
                && previous_row != row_idx
            {
                return None;
            }
        }
    }
    Some(producer_rows)
}

fn non_state_y_loads(
    row: &[solve::LinearOp],
    state_count: usize,
    solver_count: usize,
) -> Vec<usize> {
    let mut loads = row
        .iter()
        .filter_map(|op| match *op {
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
