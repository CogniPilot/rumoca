use std::{collections::VecDeque, sync::Arc};

use indexmap::{IndexMap, IndexSet};
use rumoca_ir_solve as solve;

use crate::{EvalSolveError, PreparedScalarProgramBlock};

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
    /// ScalarProgramBlock program index that produces this refresh row.
    pub(crate) row_idx: usize,
    /// Output offset inside `row_idx`. Shared Solve programs may store multiple
    /// row outputs while still evaluating one source program.
    pub(crate) output_offset: usize,
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
) -> Result<RefreshPlan, EvalSolveError> {
    let state_count = model.state_scalar_count();
    let row_target_rows = algebraic_refresh_rows_from_row_targets(model, block, state_count)?;
    let mut rows_by_target = IndexMap::new();
    reserve_refresh_index_map_capacity(
        &mut rows_by_target,
        row_target_rows.len(),
        "row-target map",
        first_block_span(block.block()),
    )?;
    for row in row_target_rows {
        rows_by_target.insert(row.target_index, row);
    }
    let projection_rows = algebraic_refresh_rows_from_projection_plan(model, block, state_count)?;
    reserve_refresh_index_map_capacity(
        &mut rows_by_target,
        projection_rows.len(),
        "projection target map",
        first_block_span(block.block()),
    )?;
    for row in projection_rows {
        rows_by_target.insert(row.target_index, row);
    }
    let mut rows = Vec::new();
    reserve_refresh_vec_capacity(
        &mut rows,
        rows_by_target.len(),
        "ordered target rows",
        first_block_span(block.block()),
    )?;
    rows.extend(rows_by_target.into_values());
    order_refresh_rows(rows, Arc::new(block.block().clone()), state_count)
}

fn algebraic_refresh_rows_from_projection_plan(
    model: &solve::SolveModel,
    block: &PreparedScalarProgramBlock,
    state_count: usize,
) -> Result<Vec<AlgebraicRefreshRow>, EvalSolveError> {
    let implicit_targets = &model.problem.continuous.implicit_row_targets;
    let output_row_positions = output_row_positions(block.block())?;
    let assignment_target = |row_idx: usize| -> Option<usize> {
        match implicit_targets.get(row_idx).copied().flatten() {
            Some(solve::ScalarSlot::Y { index, .. }) => Some(index),
            _ => None,
        }
    };
    let mut rows = Vec::new();
    reserve_refresh_vec_capacity(
        &mut rows,
        model
            .problem
            .continuous
            .algebraic_projection_plan
            .blocks
            .len(),
        "projection rows",
        first_block_span(block.block()),
    )?;
    for plan_block in &model.problem.continuous.algebraic_projection_plan.blocks {
        // A coupled block's `rows` and `y_indices` are independent sets (the
        // unknowns are even sorted), so each row must be paired with an
        // unknown it actually determines — positional pairing produces a
        // convergent but wrong system (a gear-torque row "assigned" to an
        // unrelated flange torque). Pair by maximum bipartite matching over
        // real incidence, preferring each row's own implicit target.
        let span = plan_block_span(block.block(), &plan_block.rows, &output_row_positions);
        let mut eligible_ys = Vec::new();
        reserve_refresh_vec_capacity(
            &mut eligible_ys,
            plan_block.y_indices.len(),
            "projection eligible-y list",
            span,
        )?;
        eligible_ys.extend(
            plan_block
                .y_indices
                .iter()
                .copied()
                .filter(|&y| y >= state_count),
        );
        let pairs = match_block_rows_to_targets(
            &plan_block.rows,
            &eligible_ys,
            &assignment_target,
            |row_idx, y| {
                output_row_positions
                    .get(&row_idx)
                    .is_some_and(|position| block.row_reads_y(position.program_index, y))
            },
            span,
        )?;
        let row_capacity = pairs
            .len()
            .checked_add(plan_block.causal_steps.len())
            .ok_or_else(|| refresh_plan_capacity_error("projection refresh rows", span))?;
        reserve_refresh_vec_capacity(&mut rows, row_capacity, "projection refresh rows", span)?;
        for (row_idx, target_index) in pairs {
            let position = required_program_position(&output_row_positions, row_idx, span)?;
            rows.push(AlgebraicRefreshRow {
                row_idx: position.program_index,
                output_offset: position.output_offset,
                target_index,
                assignment_target: assignment_target(row_idx),
            });
        }
        for step in &plan_block.causal_steps {
            if step.y_index >= state_count {
                let position = required_program_position(&output_row_positions, step.row, span)?;
                rows.push(AlgebraicRefreshRow {
                    row_idx: position.program_index,
                    output_offset: position.output_offset,
                    target_index: step.y_index,
                    assignment_target: assignment_target(step.row),
                });
            }
        }
    }
    Ok(rows)
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
    span: Option<rumoca_core::Span>,
) -> Result<Vec<(usize, usize)>, EvalSolveError> {
    let mut y_pos = IndexMap::new();
    reserve_refresh_index_map_capacity(&mut y_pos, ys.len(), "matching y-position map", span)?;
    for (pos, y) in ys.iter().copied().enumerate() {
        y_pos.insert(y, pos);
    }
    let mut adjacency = Vec::new();
    reserve_refresh_vec_capacity(&mut adjacency, rows.len(), "matching adjacency", span)?;
    for &row_idx in rows {
        let own = assignment_target(row_idx).and_then(|y| y_pos.get(&y).copied());
        let mut candidates = Vec::new();
        let candidate_capacity = y_pos
            .len()
            .checked_add(usize::from(own.is_some()))
            .ok_or_else(|| refresh_plan_capacity_error("matching candidates", span))?;
        reserve_refresh_vec_capacity(
            &mut candidates,
            candidate_capacity,
            "matching candidates",
            span,
        )?;
        candidates.extend(own);
        for (&y, &pos) in &y_pos {
            if Some(pos) != own && reads(row_idx, y) {
                candidates.push(pos);
            }
        }
        adjacency.push(candidates);
    }
    let mut matched_row_for_y = Vec::new();
    reserve_refresh_vec_capacity(
        &mut matched_row_for_y,
        ys.len(),
        "matching result slots",
        span,
    )?;
    matched_row_for_y.resize(ys.len(), None);
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
            let can_assign = match matched_row_for_y[y_pos] {
                Some(matched_row) => try_assign(matched_row, adjacency, matched_row_for_y, visited),
                None => true,
            };
            if can_assign {
                matched_row_for_y[y_pos] = Some(row_pos);
                return true;
            }
        }
        false
    }
    for row_pos in 0..rows.len() {
        let mut visited = Vec::new();
        reserve_refresh_vec_capacity(&mut visited, ys.len(), "matching visited flags", span)?;
        visited.resize(ys.len(), false);
        try_assign(row_pos, &adjacency, &mut matched_row_for_y, &mut visited);
    }
    let mut pairs = Vec::new();
    reserve_refresh_vec_capacity(&mut pairs, ys.len(), "matching output pairs", span)?;
    pairs.extend(
        matched_row_for_y
            .iter()
            .enumerate()
            .filter_map(|(y_pos, row_pos)| row_pos.map(|rp| (rows[rp], ys[y_pos]))),
    );
    Ok(pairs)
}

fn algebraic_refresh_rows_from_row_targets(
    model: &solve::SolveModel,
    block: &PreparedScalarProgramBlock,
    state_count: usize,
) -> Result<Vec<AlgebraicRefreshRow>, EvalSolveError> {
    let solver_count = model.solver_scalar_count();
    let span = first_block_span(block.block());
    let output_row_positions = output_row_positions(block.block())?;
    let mut rows = Vec::new();
    reserve_refresh_vec_capacity(
        &mut rows,
        model.problem.continuous.implicit_row_targets.len(),
        "row-target refresh rows",
        span,
    )?;
    let mut claimed_targets = IndexSet::new();
    reserve_refresh_index_set_capacity(
        &mut claimed_targets,
        model.problem.continuous.implicit_row_targets.len(),
        "claimed row targets",
        span,
    )?;
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
        if target_index < state_count || target_index >= solver_count {
            continue;
        }
        let Some(position) = output_row_positions.get(&row_idx).copied() else {
            continue;
        };
        if position.output_offset != 0
            || !block.can_evaluate_target_assignment(position.program_index, target_index)
        {
            continue;
        }
        reserve_refresh_index_set_capacity(&mut claimed_targets, 1, "claimed row targets", span)?;
        if !claimed_targets.insert(target_index) {
            continue;
        }
        rows.push(AlgebraicRefreshRow {
            row_idx: position.program_index,
            output_offset: position.output_offset,
            target_index,
            assignment_target: Some(target_index),
        });
    }
    Ok(rows)
}

pub(crate) fn build_derivative_refresh_plan(
    model: &solve::SolveModel,
    derivative_block: &solve::ScalarProgramBlock,
    full_plan: &RefreshPlan,
) -> Result<RefreshPlan, EvalSolveError> {
    let state_count = model.state_scalar_count();
    let initial_deps = derivative_row_dependencies(derivative_block, state_count)?;
    build_dependency_refresh_plan(model, full_plan, initial_deps)
}

pub(crate) fn build_root_refresh_plan(
    model: &solve::SolveModel,
    full_plan: &RefreshPlan,
) -> Result<RefreshPlan, EvalSolveError> {
    let state_count = model.state_scalar_count();
    let initial_deps =
        root_condition_dependencies(&model.problem.events.root_conditions, state_count)?;
    build_dependency_refresh_plan(model, full_plan, initial_deps)
}

fn build_dependency_refresh_plan(
    model: &solve::SolveModel,
    full_plan: &RefreshPlan,
    initial_deps: IndexSet<usize>,
) -> Result<RefreshPlan, EvalSolveError> {
    let implicit_block = full_plan.source_block();
    let state_count = model.state_scalar_count();
    let span = first_block_span(implicit_block);
    let mut target_to_row = IndexMap::new();
    reserve_refresh_index_map_capacity(
        &mut target_to_row,
        full_plan.rows.len(),
        "dependency target-to-row map",
        span,
    )?;
    for row in &full_plan.rows {
        target_to_row.insert(row.target_index, row.row_idx);
    }
    let mut needed = IndexSet::new();
    reserve_refresh_index_set_capacity(
        &mut needed,
        initial_deps.len(),
        "dependency needed set",
        span,
    )?;
    let mut missing = IndexSet::new();
    reserve_refresh_index_set_capacity(
        &mut missing,
        initial_deps.len(),
        "dependency missing set",
        span,
    )?;
    let mut stack = Vec::new();
    reserve_refresh_vec_capacity(&mut stack, initial_deps.len(), "dependency stack", span)?;
    stack.extend(initial_deps);
    while let Some(index) = stack.pop() {
        if index < state_count {
            continue;
        }
        reserve_refresh_index_set_capacity(&mut needed, 1, "dependency needed set", span)?;
        if !needed.insert(index) {
            continue;
        }
        let Some(row_idx) = target_to_row.get(&index).copied() else {
            reserve_refresh_index_set_capacity(&mut missing, 1, "dependency missing set", span)?;
            missing.insert(index);
            continue;
        };
        for dep in row_all_y_dependencies(implicit_block, row_idx) {
            if dep >= state_count {
                reserve_refresh_vec_capacity(&mut stack, 1, "dependency stack", span)?;
                stack.push(dep);
            }
        }
    }
    let mut rows = Vec::new();
    reserve_refresh_vec_capacity(
        &mut rows,
        full_plan.rows.len(),
        "dependency refresh rows",
        span,
    )?;
    rows.extend(
        full_plan
            .rows
            .iter()
            .filter(|row| needed.contains(&row.target_index))
            .cloned(),
    );
    let mut plan = order_refresh_rows(rows, full_plan.source_block.clone(), state_count)?;
    reserve_refresh_vec_capacity(
        &mut plan.missing_dependencies,
        missing.len(),
        "dependency missing list",
        span,
    )?;
    plan.missing_dependencies.extend(missing);
    Ok(plan)
}

fn order_refresh_rows(
    rows: Vec<AlgebraicRefreshRow>,
    block: Arc<solve::ScalarProgramBlock>,
    state_count: usize,
) -> Result<RefreshPlan, EvalSolveError> {
    let span = first_block_span(&block);
    let mut producer_by_target = IndexMap::new();
    reserve_refresh_index_map_capacity(
        &mut producer_by_target,
        rows.len(),
        "refresh order producer map",
        span,
    )?;
    for (pos, row) in rows.iter().enumerate() {
        producer_by_target.insert(row.target_index, pos);
    }
    let mut edges = Vec::new();
    reserve_refresh_vec_capacity(&mut edges, rows.len(), "refresh order edges", span)?;
    edges.resize_with(rows.len(), Vec::new);
    let mut indegree = Vec::new();
    reserve_refresh_vec_capacity(&mut indegree, rows.len(), "refresh order indegree", span)?;
    indegree.resize(rows.len(), 0usize);
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
            reserve_refresh_vec_capacity(
                &mut edges[dep_pos],
                1,
                "refresh order edge list",
                row_span(&block, row.row_idx),
            )?;
            edges[dep_pos].push(row_pos);
            indegree[row_pos] += 1;
        }
    }
    let mut ready = VecDeque::new();
    reserve_refresh_deque_capacity(&mut ready, rows.len(), "refresh order queue", span)?;
    ready.extend(
        indegree
            .iter()
            .enumerate()
            .filter_map(|(idx, degree)| (*degree == 0).then_some(idx)),
    );
    let mut ordered = Vec::new();
    reserve_refresh_vec_capacity(&mut ordered, rows.len(), "ordered refresh rows", span)?;
    while let Some(row_pos) = ready.pop_front() {
        ordered.push(rows[row_pos].clone());
        for &next in &edges[row_pos] {
            indegree[next] -= 1;
            if indegree[next] == 0 {
                ready.push_back(next);
            }
        }
    }
    // A row also needs iteration when it is *nonlinear in its own target*, even
    // with no dependency cycle: the single secant step that the non-iterative
    // single pass performs is exact only for residuals that are affine in the
    // target (slope constant). A self-nonlinear acyclic row (e.g. `z*z = b*x`)
    // would otherwise be left one secant step short of convergence.
    let mut self_nonlinear = false;
    for row in &ordered {
        if let Some(ops) = block.programs.get(row.row_idx)
            && row_is_nonlinear_in_target(ops, row.target_index)?
        {
            self_nonlinear = true;
            break;
        }
    }
    let requires_iteration = ordered.len() != rows.len() || self_nonlinear;
    if ordered.len() != rows.len() {
        let mut emitted = Vec::new();
        reserve_refresh_vec_capacity(&mut emitted, rows.len(), "refresh emitted flags", span)?;
        emitted.resize(rows.len(), false);
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
    Ok(RefreshPlan {
        source_block: block,
        rows: ordered,
        missing_dependencies: Vec::new(),
        iterative: requires_iteration,
    })
}

/// How a register's value depends on the refresh row's own target solver-Y slot,
/// holding every other unknown fixed — which is exactly how a single refresh row
/// is solved for its target.
#[derive(Clone, Copy, PartialEq, Eq)]
enum TargetDep {
    /// Independent of the target.
    Independent,
    /// Affine in the target: `c0 + c1 * target`.
    Affine,
    /// Depends on the target nonlinearly.
    Nonlinear,
}

impl TargetDep {
    /// Combine two operands under an addition-like op (Add/Sub, or a Select with
    /// a target-independent condition): the result is as nonlinear as its most
    /// nonlinear operand.
    fn join_additive(self, other: Self) -> Self {
        match (self, other) {
            (Self::Nonlinear, _) | (_, Self::Nonlinear) => Self::Nonlinear,
            (Self::Affine, _) | (_, Self::Affine) => Self::Affine,
            _ => Self::Independent,
        }
    }

    fn depends_on_target(self) -> bool {
        !matches!(self, Self::Independent)
    }
}

/// `Nonlinear` if `condition` holds (a target-dependent input to a nonlinear or
/// non-smooth op), else `Independent`.
fn nonlinear_if(condition: bool) -> TargetDep {
    if condition {
        TargetDep::Nonlinear
    } else {
        TargetDep::Independent
    }
}

/// Class of register `reg`. `classes` is sized to `max_dst + 1`; every operand is
/// a prior `dst` in the (SSA) op stream, so the index is in bounds by
/// construction (a malformed forward-reference panics loudly rather than silently
/// reading an under-classified `Independent` default).
fn target_dep(classes: &[TargetDep], reg: solve::Reg) -> TargetDep {
    classes[reg as usize]
}

/// Most-nonlinear class among the registers in `[base, base + count)`.
fn range_target_dep(classes: &[TargetDep], base: solve::Reg, count: usize) -> TargetDep {
    (0..count).fold(TargetDep::Independent, |acc, offset| {
        acc.join_additive(target_dep(classes, base + offset as solve::Reg))
    })
}

/// Class of a binary op's result given its operand classes.
fn classify_binary(op: solve::BinaryOp, l: TargetDep, r: TargetDep) -> TargetDep {
    use TargetDep::{Affine, Nonlinear};
    match op {
        solve::BinaryOp::Add | solve::BinaryOp::Sub => l.join_additive(r),
        // A product is affine only when at most one factor depends on the target;
        // two target-dependent factors give a quadratic term.
        solve::BinaryOp::Mul => match (l, r) {
            (Nonlinear, _) | (_, Nonlinear) | (Affine, Affine) => Nonlinear,
            (Affine, _) | (_, Affine) => Affine,
            _ => TargetDep::Independent,
        },
        // Dividing by a target-dependent denominator is nonlinear; dividing by a
        // constant preserves the numerator's class.
        solve::BinaryOp::Div => {
            if r.depends_on_target() {
                Nonlinear
            } else {
                l
            }
        }
        // Pow, Atan2, Min, Max, And, Or are nonlinear once any operand depends.
        _ => nonlinear_if(l.depends_on_target() || r.depends_on_target()),
    }
}

/// Class of a single op's destination register given the classes computed so far
/// (the `StoreOutput` sink has no destination and is handled by the caller).
fn classify_op_target_dep(
    op: &solve::LinearOp,
    classes: &[TargetDep],
    target_y_index: usize,
) -> TargetDep {
    use solve::LinearOp as Op;
    match *op {
        // Other unknowns are held fixed while this row solves for its own target,
        // so only the target itself is (affinely) variable.
        Op::LoadY { index, .. } if index == target_y_index => TargetDep::Affine,
        Op::LoadY { .. }
        | Op::Const { .. }
        | Op::LoadTime { .. }
        | Op::LoadP { .. }
        | Op::LoadSeed { .. }
        | Op::StoreOutput { .. } => TargetDep::Independent,
        // A target-dependent subscript selects discontinuously among slots.
        Op::LoadIndexedP { index, .. } | Op::LoadIndexedSeed { index, .. } => {
            nonlinear_if(target_dep(classes, index).depends_on_target())
        }
        Op::Move { src, .. } => target_dep(classes, src),
        // Negation preserves affinity; every other unary is nonlinear once its
        // argument depends on the target.
        Op::Unary {
            op: solve::UnaryOp::Neg,
            arg,
            ..
        } => target_dep(classes, arg),
        Op::Unary { arg, .. } => nonlinear_if(target_dep(classes, arg).depends_on_target()),
        Op::Binary { op, lhs, rhs, .. } => {
            classify_binary(op, target_dep(classes, lhs), target_dep(classes, rhs))
        }
        Op::Compare { lhs, rhs, .. } => nonlinear_if(
            target_dep(classes, lhs).depends_on_target()
                || target_dep(classes, rhs).depends_on_target(),
        ),
        Op::Select { cond, .. } if target_dep(classes, cond).depends_on_target() => {
            TargetDep::Nonlinear
        }
        Op::Select {
            if_true, if_false, ..
        } => target_dep(classes, if_true).join_additive(target_dep(classes, if_false)),
        // A target-dependent system matrix makes the solution nonlinear; with a
        // constant matrix the solution is only as nonlinear as the RHS.
        Op::LinearSolveComponent {
            matrix_start, n, ..
        } if range_target_dep(classes, matrix_start, n.saturating_mul(n)).depends_on_target() => {
            TargetDep::Nonlinear
        }
        Op::LinearSolveComponent { rhs_start, n, .. } => range_target_dep(classes, rhs_start, n),
        Op::TableBounds { table_id, .. } => {
            nonlinear_if(target_dep(classes, table_id).depends_on_target())
        }
        Op::TableLookup {
            table_id,
            column,
            input,
            ..
        }
        | Op::TableLookupSlope {
            table_id,
            column,
            input,
            ..
        } => nonlinear_if(
            target_dep(classes, table_id).depends_on_target()
                || target_dep(classes, column).depends_on_target()
                || target_dep(classes, input).depends_on_target(),
        ),
        Op::TableNextEvent { table_id, time, .. } => nonlinear_if(
            target_dep(classes, table_id).depends_on_target()
                || target_dep(classes, time).depends_on_target(),
        ),
        // Random generators are nonlinear (and non-smooth) in any target-dependent
        // input; conservatively flag them.
        Op::RandomInitialState {
            local_seed,
            global_seed,
            ..
        } => nonlinear_if(
            target_dep(classes, local_seed).depends_on_target()
                || target_dep(classes, global_seed).depends_on_target(),
        ),
        Op::RandomResult {
            state_start,
            state_len,
            ..
        }
        | Op::RandomState {
            state_start,
            state_len,
            ..
        } => nonlinear_if(range_target_dep(classes, state_start, state_len).depends_on_target()),
        Op::ImpureRandomInit { seed, .. } => {
            nonlinear_if(target_dep(classes, seed).depends_on_target())
        }
        Op::ImpureRandom { id, .. } => nonlinear_if(target_dep(classes, id).depends_on_target()),
        Op::ImpureRandomInteger { id, imin, imax, .. } => nonlinear_if(
            target_dep(classes, id).depends_on_target()
                || target_dep(classes, imin).depends_on_target()
                || target_dep(classes, imax).depends_on_target(),
        ),
    }
}

/// True when the row's residual program is nonlinear in its own target slot, so
/// a single secant step does not solve it exactly and the refresh must iterate.
/// Dependency cycles are handled separately; this catches the *acyclic* case of
/// a self-nonlinear implicit row (e.g. `z*z = b*x`), which would otherwise be
/// classified non-iterative and left one secant step short of convergence.
///
/// The analysis is a conservative forward dataflow over the row's register
/// stream (see [`classify_op_target_dep`]): any op that could make the target
/// enter nonlinearly yields `Nonlinear`. Affine and independent rows keep the
/// exact single-step fast path.
fn row_is_nonlinear_in_target(
    ops: &[solve::LinearOp],
    target_y_index: usize,
) -> Result<bool, EvalSolveError> {
    use solve::LinearOp as Op;
    let Some(max_reg) = ops.iter().filter_map(solve::LinearOp::dst_register).max() else {
        return Ok(false);
    };
    let len = (max_reg as usize)
        .checked_add(1)
        .ok_or_else(|| refresh_plan_capacity_error("target-dependence register map", None))?;
    let mut classes = Vec::new();
    reserve_refresh_vec_capacity(&mut classes, len, "target-dependence register map", None)?;
    classes.resize(len, TargetDep::Independent);
    let mut output = TargetDep::Independent;
    for op in ops {
        if let Op::StoreOutput { src } = *op {
            output = output.join_additive(target_dep(&classes, src));
            continue;
        }
        let class = classify_op_target_dep(op, &classes, target_y_index);
        if let Some(dst) = op.dst_register()
            && let Some(slot) = classes.get_mut(dst as usize)
        {
            *slot = class;
        }
    }
    Ok(matches!(output, TargetDep::Nonlinear))
}

fn derivative_row_dependencies(
    block: &solve::ScalarProgramBlock,
    state_count: usize,
) -> Result<IndexSet<usize>, EvalSolveError> {
    let mut deps = IndexSet::new();
    reserve_refresh_index_set_capacity(
        &mut deps,
        state_count.min(block.programs.len()),
        "derivative dependency set",
        first_block_span(block),
    )?;
    for row_idx in 0..state_count.min(block.programs.len()) {
        for index in row_all_y_dependencies(block, row_idx).filter(|index| *index >= state_count) {
            reserve_refresh_index_set_capacity(
                &mut deps,
                1,
                "derivative dependency set",
                first_block_span(block),
            )?;
            deps.insert(index);
        }
    }
    Ok(deps)
}

fn root_condition_dependencies(
    block: &solve::ScalarProgramBlock,
    state_count: usize,
) -> Result<IndexSet<usize>, EvalSolveError> {
    scalar_program_block_dependencies(block, state_count)
}

fn scalar_program_block_dependencies(
    block: &solve::ScalarProgramBlock,
    state_count: usize,
) -> Result<IndexSet<usize>, EvalSolveError> {
    let mut deps = IndexSet::new();
    reserve_refresh_index_set_capacity(
        &mut deps,
        block.programs.len(),
        "scalar block dependency set",
        first_block_span(block),
    )?;
    for row_idx in 0..block.programs.len() {
        for index in row_all_y_dependencies(block, row_idx).filter(|index| *index >= state_count) {
            reserve_refresh_index_set_capacity(
                &mut deps,
                1,
                "scalar block dependency set",
                first_block_span(block),
            )?;
            deps.insert(index);
        }
    }
    Ok(deps)
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

fn row_all_y_dependencies(
    block: &solve::ScalarProgramBlock,
    row_idx: usize,
) -> impl Iterator<Item = usize> + '_ {
    block
        .programs
        .get(row_idx)
        .into_iter()
        .flat_map(|row| row.iter())
        .filter_map(|op| match *op {
            solve::LinearOp::LoadY { index, .. } => Some(index),
            _ => None,
        })
}

fn first_block_span(block: &solve::ScalarProgramBlock) -> Option<rumoca_core::Span> {
    block.first_source_span()
}

fn row_span(block: &solve::ScalarProgramBlock, row: usize) -> Option<rumoca_core::Span> {
    block.program_span(row).or_else(|| first_block_span(block))
}

fn plan_block_span(
    block: &solve::ScalarProgramBlock,
    rows: &[usize],
    output_row_positions: &IndexMap<usize, OutputRowPosition>,
) -> Option<rumoca_core::Span> {
    rows.iter()
        .filter_map(|row| output_row_positions.get(row).copied())
        .find_map(|position| block.program_span(position.program_index))
        .or_else(|| first_block_span(block))
}

#[derive(Clone, Copy)]
struct OutputRowPosition {
    program_index: usize,
    output_offset: usize,
}

fn output_row_positions(
    block: &solve::ScalarProgramBlock,
) -> Result<IndexMap<usize, OutputRowPosition>, EvalSolveError> {
    let span = first_block_span(block);
    let mut positions = IndexMap::new();
    reserve_refresh_index_map_capacity(
        &mut positions,
        block.output_indices.len(),
        "output-row position map",
        span,
    )?;
    let mut output_ordinal = 0usize;
    for (program_index, program) in block.programs.iter().enumerate() {
        let output_count = solve::ScalarProgramBlock::program_output_count(program);
        for output_offset in 0..output_count {
            let Some(output_index) = block.output_indices.get(output_ordinal).copied() else {
                return Err(EvalSolveError::InvalidRow {
                    message: format!(
                        "program output ordinal {output_ordinal} is missing scalar output metadata"
                    ),
                    span: block.program_span(program_index),
                });
            };
            output_ordinal =
                output_ordinal
                    .checked_add(1)
                    .ok_or_else(|| EvalSolveError::InvalidRow {
                        message: "program output ordinal overflows host index limits".to_string(),
                        span,
                    })?;
            if positions
                .insert(
                    output_index,
                    OutputRowPosition {
                        program_index,
                        output_offset,
                    },
                )
                .is_some()
            {
                return Err(EvalSolveError::InvalidRow {
                    message: format!("duplicate scalar program output row {output_index}"),
                    span: block.program_span(program_index),
                });
            }
        }
    }
    if output_ordinal != block.output_indices.len() {
        return Err(EvalSolveError::InvalidRow {
            message: format!(
                "scalar program block has {} output indices but {output_ordinal} StoreOutput ops",
                block.output_indices.len()
            ),
            span,
        });
    }
    Ok(positions)
}

fn required_program_position(
    output_row_positions: &IndexMap<usize, OutputRowPosition>,
    output_row: usize,
    span: Option<rumoca_core::Span>,
) -> Result<OutputRowPosition, EvalSolveError> {
    output_row_positions
        .get(&output_row)
        .copied()
        .ok_or_else(|| EvalSolveError::InvalidRow {
            message: format!(
                "algebraic projection references missing implicit output row {output_row}"
            ),
            span,
        })
}

fn reserve_refresh_vec_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    values
        .try_reserve_exact(capacity)
        .map_err(|_| refresh_plan_capacity_error(context, span))
}

fn reserve_refresh_deque_capacity<T>(
    values: &mut VecDeque<T>,
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    values
        .try_reserve_exact(capacity)
        .map_err(|_| refresh_plan_capacity_error(context, span))
}

fn reserve_refresh_index_map_capacity<K, V>(
    values: &mut IndexMap<K, V>,
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError>
where
    K: std::hash::Hash + Eq,
{
    values
        .try_reserve(capacity)
        .map_err(|_| refresh_plan_capacity_error(context, span))
}

fn reserve_refresh_index_set_capacity<T>(
    values: &mut IndexSet<T>,
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError>
where
    T: std::hash::Hash + Eq,
{
    values
        .try_reserve(capacity)
        .map_err(|_| refresh_plan_capacity_error(context, span))
}

fn refresh_plan_capacity_error(
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: format!("refresh plan {context} capacity overflows"),
        span,
    }
}
