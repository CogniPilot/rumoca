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
    /// Solver-Y slot this plan entry updates.
    pub(crate) target_index: usize,
    /// The row's own implicit assignment target, when the lowering placed it
    /// as `target = expr`. When this differs from `target_index` the runtime
    /// must linear-solve the row's residual for the paired variable instead
    /// of evaluating the assignment value.
    pub(crate) assignment_target: Option<usize>,
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
    for row in algebraic_refresh_rows_from_projection_plan(model, block, state_count) {
        rows_by_target.insert(row.target_index, row);
    }
    let rows = rows_by_target.into_values().collect();
    order_refresh_rows(rows, Arc::new(block.block().clone()), state_count)
}

fn algebraic_refresh_rows_from_projection_plan(
    model: &solve::SolveModel,
    block: &PreparedScalarProgramBlock,
    state_count: usize,
) -> Vec<AlgebraicRefreshRow> {
    let implicit_targets = &model.problem.continuous.implicit_row_targets;
    let assignment_target = |row_idx: usize| -> Option<usize> {
        match implicit_targets.get(row_idx).copied().flatten() {
            Some(solve::ScalarSlot::Y { index, .. }) => Some(index),
            _ => None,
        }
    };
    let mut rows = Vec::new();
    for plan_block in &model.problem.continuous.algebraic_projection_plan.blocks {
        // A coupled block's `rows` and `y_indices` are independent sets (the
        // unknowns are even sorted), so each row must be paired with an
        // unknown it actually determines — positional pairing produces a
        // convergent but wrong system (a gear-torque row "assigned" to an
        // unrelated flange torque). Pair by maximum bipartite matching over
        // real incidence, preferring each row's own implicit target.
        let eligible_ys: Vec<usize> = plan_block
            .y_indices
            .iter()
            .copied()
            .filter(|&y| y >= state_count)
            .collect();
        let pairs = match_block_rows_to_targets(
            &plan_block.rows,
            &eligible_ys,
            &assignment_target,
            |row_idx, y| block.row_reads_y(row_idx, y),
        );
        for (row_idx, target_index) in pairs {
            rows.push(AlgebraicRefreshRow {
                row_idx,
                target_index,
                assignment_target: assignment_target(row_idx),
            });
        }
        for step in &plan_block.causal_steps {
            if step.y_index >= state_count {
                rows.push(AlgebraicRefreshRow {
                    row_idx: step.row,
                    target_index: step.y_index,
                    assignment_target: assignment_target(step.row),
                });
            }
        }
    }
    rows
}

/// Pair each block row with a distinct block unknown it can determine:
/// its own implicit assignment target when that is part of the block,
/// otherwise an unknown the row's program reads. Kuhn's augmenting-path
/// matching; unmatched rows/unknowns are left out (the runtime reports the
/// unknowns as missing producers instead of silently mis-solving).
fn match_block_rows_to_targets(
    rows: &[usize],
    ys: &[usize],
    assignment_target: &dyn Fn(usize) -> Option<usize>,
    reads: impl Fn(usize, usize) -> bool,
) -> Vec<(usize, usize)> {
    let y_pos: IndexMap<usize, usize> = ys
        .iter()
        .copied()
        .enumerate()
        .map(|(p, y)| (y, p))
        .collect();
    let adjacency: Vec<Vec<usize>> = rows
        .iter()
        .map(|&row_idx| {
            let own = assignment_target(row_idx).and_then(|y| y_pos.get(&y).copied());
            let mut candidates: Vec<usize> = own.into_iter().collect();
            for (&y, &pos) in &y_pos {
                if Some(pos) != own && reads(row_idx, y) {
                    candidates.push(pos);
                }
            }
            candidates
        })
        .collect();
    let mut matched_row_for_y: Vec<Option<usize>> = vec![None; ys.len()];
    fn try_assign(
        row_pos: usize,
        adjacency: &[Vec<usize>],
        matched_row_for_y: &mut [Option<usize>],
        visited: &mut [bool],
    ) -> bool {
        for &y_pos in &adjacency[row_pos] {
            if visited[y_pos] {
                continue;
            }
            visited[y_pos] = true;
            if matched_row_for_y[y_pos].is_none()
                || try_assign(
                    matched_row_for_y[y_pos].expect("checked above"),
                    adjacency,
                    matched_row_for_y,
                    visited,
                )
            {
                matched_row_for_y[y_pos] = Some(row_pos);
                return true;
            }
        }
        false
    }
    for row_pos in 0..rows.len() {
        let mut visited = vec![false; ys.len()];
        try_assign(row_pos, &adjacency, &mut matched_row_for_y, &mut visited);
    }
    matched_row_for_y
        .iter()
        .enumerate()
        .filter_map(|(y_pos, row_pos)| row_pos.map(|rp| (rows[rp], ys[y_pos])))
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
            assignment_target: Some(target_index),
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
