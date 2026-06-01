use std::{collections::VecDeque, sync::Arc};

use indexmap::{IndexMap, IndexSet};
use rumoca_ir_solve as solve;

use crate::PreparedScalarProgramBlock;

pub(crate) fn trace_refresh_plan(model: &solve::SolveModel, name: &str, plan: &RefreshPlan) {
    if !trace_algebraic_refresh() {
        return;
    }
    let preview = plan
        .rows
        .iter()
        .take(64)
        .map(|row| {
            let target = model
                .problem
                .solve_layout
                .solver_maps
                .names
                .get(row.target_index)
                .map_or("<unnamed>", String::as_str);
            format!("{target}@row{}", row.row_idx)
        })
        .collect::<Vec<_>>()
        .join(", ");
    let duplicate_targets = plan.rows.len()
        - plan
            .rows
            .iter()
            .map(|row| row.target_index)
            .collect::<IndexSet<_>>()
            .len();
    let missing = plan
        .missing_dependencies
        .iter()
        .take(32)
        .map(|index| {
            model
                .problem
                .solve_layout
                .solver_maps
                .names
                .get(*index)
                .map_or_else(|| format!("y[{index}]"), Clone::clone)
        })
        .collect::<Vec<_>>()
        .join(", ");
    tracing::debug!(
        target: "rumoca_eval_solve::refresh",
        "{name} refresh plan: rows={} duplicate_targets={} missing={} iterative={} [{}] missing=[{}]",
        plan.rows.len(),
        duplicate_targets,
        plan.missing_dependencies.len(),
        plan.iterative,
        preview,
        missing
    );
}

fn trace_algebraic_refresh() -> bool {
    tracing::enabled!(target: "rumoca_eval_solve::refresh", tracing::Level::DEBUG)
}

#[derive(Clone)]
pub(crate) struct AlgebraicRefreshRow {
    pub(crate) row_idx: usize,
    pub(crate) target_index: usize,
}

#[derive(Clone, Default)]
pub(crate) struct RefreshPlan {
    pub(crate) source_block: Arc<solve::ScalarProgramBlock>,
    pub(crate) rows: Vec<AlgebraicRefreshRow>,
    pub(crate) missing_dependencies: Vec<usize>,
    pub(crate) iterative: bool,
}

impl RefreshPlan {
    pub(crate) fn source_block(&self) -> &solve::ScalarProgramBlock {
        &self.source_block
    }
}

pub(crate) fn build_algebraic_refresh_plan(
    model: &solve::SolveModel,
    block: &PreparedScalarProgramBlock,
) -> RefreshPlan {
    let state_count = model.state_scalar_count();
    let mut rows_by_target = algebraic_refresh_rows_from_row_targets(model, block, state_count)
        .into_iter()
        .map(|row| (row.target_index, row))
        .collect::<IndexMap<_, _>>();
    for row in algebraic_refresh_rows_from_projection_plan(model, state_count) {
        rows_by_target.insert(row.target_index, row);
    }
    let rows = rows_by_target.into_values().collect();
    order_refresh_rows(rows, Arc::new(block.block().clone()), state_count)
}

fn algebraic_refresh_rows_from_projection_plan(
    model: &solve::SolveModel,
    state_count: usize,
) -> Vec<AlgebraicRefreshRow> {
    model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .iter()
        .flat_map(|block| {
            block
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
        })
        .filter_map(|(row_idx, target_index)| {
            (target_index >= state_count).then_some(AlgebraicRefreshRow {
                row_idx,
                target_index,
            })
        })
        .collect()
}

fn algebraic_refresh_rows_from_row_targets(
    model: &solve::SolveModel,
    block: &PreparedScalarProgramBlock,
    state_count: usize,
) -> Vec<AlgebraicRefreshRow> {
    let solver_count = model.solver_scalar_count();
    let mut rows = Vec::new();
    let mut claimed_targets = IndexSet::new();
    for (row_idx, target) in model
        .problem
        .continuous
        .implicit_row_targets
        .iter()
        .enumerate()
    {
        let Some(solve::ScalarSlot::Y { index, .. }) = target else {
            continue;
        };
        let target_index = *index;
        if target_index < state_count
            || target_index >= solver_count
            || !block.can_evaluate_target_assignment(row_idx, target_index)
            || !claimed_targets.insert(target_index)
        {
            continue;
        }
        rows.push(AlgebraicRefreshRow {
            row_idx,
            target_index,
        });
    }
    rows
}

pub(crate) fn build_derivative_refresh_plan(
    model: &solve::SolveModel,
    derivative_block: &solve::ScalarProgramBlock,
    full_plan: &RefreshPlan,
) -> RefreshPlan {
    let state_count = model.state_scalar_count();
    let initial_deps = derivative_row_dependencies(derivative_block, state_count);
    build_dependency_refresh_plan(model, full_plan, initial_deps)
}

pub(crate) fn build_root_refresh_plan(
    model: &solve::SolveModel,
    full_plan: &RefreshPlan,
) -> RefreshPlan {
    let state_count = model.state_scalar_count();
    let initial_deps =
        root_condition_dependencies(&model.problem.events.root_conditions, state_count);
    build_dependency_refresh_plan(model, full_plan, initial_deps)
}

fn build_dependency_refresh_plan(
    model: &solve::SolveModel,
    full_plan: &RefreshPlan,
    initial_deps: IndexSet<usize>,
) -> RefreshPlan {
    let implicit_block = full_plan.source_block();
    let state_count = model.state_scalar_count();
    let target_to_row = full_plan
        .rows
        .iter()
        .map(|row| (row.target_index, row.row_idx))
        .collect::<IndexMap<_, _>>();
    let mut needed = IndexSet::new();
    let mut missing = IndexSet::new();
    let mut stack = initial_deps.into_iter().collect::<Vec<_>>();
    while let Some(index) = stack.pop() {
        if index < state_count || !needed.insert(index) {
            continue;
        }
        let Some(row_idx) = target_to_row.get(&index).copied() else {
            missing.insert(index);
            continue;
        };
        for dep in row_all_y_dependencies(implicit_block, row_idx) {
            if dep >= state_count {
                stack.push(dep);
            }
        }
    }
    let rows = full_plan
        .rows
        .iter()
        .filter(|row| needed.contains(&row.target_index))
        .cloned()
        .collect();
    let mut plan = order_refresh_rows(rows, full_plan.source_block.clone(), state_count);
    plan.missing_dependencies = missing.into_iter().collect();
    plan
}

fn order_refresh_rows(
    rows: Vec<AlgebraicRefreshRow>,
    block: Arc<solve::ScalarProgramBlock>,
    state_count: usize,
) -> RefreshPlan {
    let producer_by_target = rows
        .iter()
        .enumerate()
        .map(|(pos, row)| (row.target_index, pos))
        .collect::<IndexMap<_, _>>();
    let mut edges = vec![Vec::new(); rows.len()];
    let mut indegree = vec![0usize; rows.len()];
    for (row_pos, row) in rows.iter().enumerate() {
        let Some(ops) = block.programs.get(row.row_idx) else {
            continue;
        };
        for dep_index in row_y_dependencies(ops, row.target_index, state_count) {
            let Some(&dep_pos) = producer_by_target.get(&dep_index) else {
                continue;
            };
            if dep_pos == row_pos || edges[dep_pos].contains(&row_pos) {
                continue;
            }
            edges[dep_pos].push(row_pos);
            indegree[row_pos] += 1;
        }
    }
    let mut ready = indegree
        .iter()
        .enumerate()
        .filter_map(|(idx, degree)| (*degree == 0).then_some(idx))
        .collect::<VecDeque<_>>();
    let mut ordered = Vec::with_capacity(rows.len());
    while let Some(row_pos) = ready.pop_front() {
        ordered.push(rows[row_pos].clone());
        for &next in &edges[row_pos] {
            indegree[next] -= 1;
            if indegree[next] == 0 {
                ready.push_back(next);
            }
        }
    }
    let requires_iteration = ordered.len() != rows.len();
    if requires_iteration {
        let mut emitted = vec![false; rows.len()];
        for row in &ordered {
            if let Some(pos) = rows.iter().position(|candidate| {
                candidate.row_idx == row.row_idx && candidate.target_index == row.target_index
            }) {
                emitted[pos] = true;
            }
        }
        ordered.extend(
            rows.into_iter()
                .enumerate()
                .filter_map(|(idx, row)| (!emitted[idx]).then_some(row)),
        );
    }
    RefreshPlan {
        source_block: block,
        rows: ordered,
        missing_dependencies: Vec::new(),
        iterative: requires_iteration,
    }
}

fn derivative_row_dependencies(
    block: &solve::ScalarProgramBlock,
    state_count: usize,
) -> IndexSet<usize> {
    let mut deps = IndexSet::new();
    for row_idx in 0..state_count.min(block.programs.len()) {
        deps.extend(
            row_all_y_dependencies(block, row_idx)
                .into_iter()
                .filter(|index| *index >= state_count),
        );
    }
    deps
}

fn root_condition_dependencies(
    block: &solve::ScalarProgramBlock,
    state_count: usize,
) -> IndexSet<usize> {
    scalar_program_block_dependencies(block, state_count)
}

fn scalar_program_block_dependencies(
    block: &solve::ScalarProgramBlock,
    state_count: usize,
) -> IndexSet<usize> {
    let mut deps = IndexSet::new();
    for row_idx in 0..block.programs.len() {
        deps.extend(
            row_all_y_dependencies(block, row_idx)
                .into_iter()
                .filter(|index| *index >= state_count),
        );
    }
    deps
}

fn row_y_dependencies(
    row: &[solve::LinearOp],
    target_index: usize,
    state_count: usize,
) -> impl Iterator<Item = usize> + '_ {
    row.iter().filter_map(move |op| match *op {
        solve::LinearOp::LoadY { index, .. } if index >= state_count && index != target_index => {
            Some(index)
        }
        _ => None,
    })
}

fn row_all_y_dependencies(block: &solve::ScalarProgramBlock, row_idx: usize) -> Vec<usize> {
    let Some(row) = block.programs.get(row_idx) else {
        return Vec::new();
    };
    row.iter()
        .filter_map(|op| match *op {
            solve::LinearOp::LoadY { index, .. } => Some(index),
            _ => None,
        })
        .collect()
}
