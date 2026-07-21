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
    tracing::debug!(
        target: "rumoca_eval_solve::refresh",
        "{name} refresh plan: rows={} duplicate_targets={} [{}]",
        plan.rows.len(),
        duplicate_targets,
        preview
    );
}

fn trace_algebraic_refresh() -> bool {
    tracing::enabled!(target: "rumoca_eval_solve::refresh", tracing::Level::DEBUG)
}

#[derive(Clone)]
pub(crate) struct AlgebraicRefreshRow {
    /// Logical equation/output index in the canonical implicit system.
    pub(crate) equation_index: usize,
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
    /// Complete compiler-owned BLT used to certify and solve the canonical
    /// implicit residual system after the causal seed schedule.
    pub(crate) simultaneous_plan: solve::AlgebraicProjectionPlan,
    pub(crate) rows: Vec<AlgebraicRefreshRow>,
    pub(crate) causal_solution_certified: bool,
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
    validate_implicit_output_inventory(model, block.block())?;
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
    let mut rows = Vec::new();
    reserve_refresh_vec_capacity(
        &mut rows,
        rows_by_target.len(),
        "ordered target rows",
        first_block_span(block.block()),
    )?;
    rows.extend(rows_by_target.into_values());
    let causal_solution_certified = complete_causal_projection_is_certified(model, block, &rows);
    let mut plan = order_refresh_rows(
        rows,
        Arc::new(block.block().clone()),
        state_count,
        causal_solution_certified,
    )?;
    plan.simultaneous_plan = model.problem.continuous.algebraic_projection_plan.clone();
    Ok(plan)
}

fn validate_implicit_output_inventory(
    model: &solve::SolveModel,
    block: &solve::ScalarProgramBlock,
) -> Result<(), EvalSolveError> {
    let positions = output_row_positions(block)?;
    let solver_count = model.solver_scalar_count();
    for output in 0..solver_count {
        if !positions.contains_key(&output) {
            return Err(EvalSolveError::InvalidRow {
                message: format!("implicit algebraic system is missing output row {output}"),
                span: first_block_span(block),
            });
        }
    }
    if let Some(output) = positions
        .keys()
        .copied()
        .find(|output| *output >= solver_count)
    {
        return Err(EvalSolveError::InvalidRow {
            message: format!(
                "implicit algebraic system has output row {output}, but solver layout has {solver_count} rows"
            ),
            span: first_block_span(block),
        });
    }
    Ok(())
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
        if !block.can_evaluate_target_assignment(position.program_index, target_index) {
            continue;
        }
        reserve_refresh_index_set_capacity(&mut claimed_targets, 1, "claimed row targets", span)?;
        if !claimed_targets.insert(target_index) {
            continue;
        }
        rows.push(AlgebraicRefreshRow {
            equation_index: row_idx,
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
    let causal_solution_certified =
        full_plan.causal_solution_certified && rows.len() == needed.len();
    let mut plan = order_refresh_rows(
        rows,
        full_plan.source_block.clone(),
        state_count,
        causal_solution_certified,
    )?;
    plan.simultaneous_plan = full_plan.simultaneous_plan.clone();
    Ok(plan)
}

fn order_refresh_rows(
    rows: Vec<AlgebraicRefreshRow>,
    block: Arc<solve::ScalarProgramBlock>,
    state_count: usize,
    causal_solution_certified: bool,
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
    let causal_solution_certified = causal_solution_certified && ordered.len() == rows.len();
    if !causal_solution_certified {
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
        simultaneous_plan: solve::AlgebraicProjectionPlan::default(),
        rows: ordered,
        causal_solution_certified,
    })
}

fn complete_causal_projection_is_certified(
    model: &solve::SolveModel,
    block: &PreparedScalarProgramBlock,
    rows: &[AlgebraicRefreshRow],
) -> bool {
    let state_count = model.state_scalar_count();
    let solver_count = model.solver_scalar_count();
    // The projection tail contains both algebraic variables and computed
    // outputs; all are solver-Y unknowns in the compiler-owned BLT plan.
    let Some(projection_count) = solver_count.checked_sub(state_count) else {
        return false;
    };
    if rows.len() != projection_count {
        return false;
    }
    let rows_by_equation = rows
        .iter()
        .map(|row| (row.equation_index, row))
        .collect::<IndexMap<_, _>>();
    if rows_by_equation.len() != projection_count
        || rows.iter().any(|row| {
            row.equation_index < state_count
                || row.equation_index >= solver_count
                || row.output_offset != 0
                || block.row_output_count(row.row_idx) != Some(1)
                || row_all_y_dependencies(block.block(), row.row_idx)
                    .any(|index| index >= solver_count)
                || !block.certifies_direct_target_assignment(row.row_idx, row.target_index)
        })
    {
        return false;
    }
    let mut matched_rows = IndexSet::new();
    let mut matched_targets = IndexSet::new();
    for projection_block in &model.problem.continuous.algebraic_projection_plan.blocks {
        let ([equation_index], [target_index]) = (
            projection_block.rows.as_slice(),
            projection_block.y_indices.as_slice(),
        ) else {
            return false;
        };
        if *equation_index < state_count
            || *equation_index >= solver_count
            || *target_index < state_count
            || *target_index >= solver_count
            || rows_by_equation
                .get(equation_index)
                .is_none_or(|row| row.target_index != *target_index)
            || !matched_rows.insert(*equation_index)
            || !matched_targets.insert(*target_index)
        {
            return false;
        }
    }
    matched_rows.len() == projection_count
        && matched_targets.len() == projection_count
        && (state_count..solver_count)
            .all(|index| matched_rows.contains(&index) && matched_targets.contains(&index))
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
