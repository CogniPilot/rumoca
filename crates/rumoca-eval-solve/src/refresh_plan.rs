//! Algebraic/derivative/root refresh-plan construction.
//!
//! The plans built here are consumed by `rumoca_solver::runtime::solve_runtime`
//! (the runtime state machine) and by this crate's prepared-block batching. The
//! module is `pub` only for that cross-crate consumer; nothing outside
//! `rumoca-solver`'s runtime should construct or mutate a [`RefreshPlan`].

mod capacity;
mod dependency_domain;
mod event_dependencies;
mod schedule;
mod source_catalog;
mod static_domain;
#[cfg(test)]
mod tests;

use std::collections::{BTreeMap, BTreeSet, VecDeque};
#[cfg(test)]
use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};
use rumoca_ir_solve as solve;

use crate::prepared::{assignment_shape_reads_y_index, row_y_input_ranges};
use crate::{EvalSolveError, PreparedScalarProgramBlock};

use capacity::{
    reserve_refresh_deque_capacity, reserve_refresh_index_map_capacity,
    reserve_refresh_index_set_capacity, reserve_refresh_vec_capacity,
};
use dependency_domain::{CompactYDependencyError, CompactYDependencySet};
use event_dependencies::event_consumer_dependencies;

use rumoca_ir_solve::{
    AlgebraicRefreshRow, RefreshPlan, RefreshRowOwnerId, RefreshRowSelection, RefreshRows,
    RefreshStage,
};
pub use schedule::build_refresh_stages;
use source_catalog::CanonicalScalarProgramCatalog;
use static_domain::ContinuousStaticParameters;

pub fn trace_refresh_plan(model: &solve::SolveModel, name: &str, plan: &RefreshPlan) {
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
                .get(row.target_index())
                .map_or("<unnamed>", String::as_str);
            format!(
                "{target}@source{}:{}",
                row.source().node(),
                row.source().program()
            )
        })
        .collect::<Vec<_>>()
        .join(", ");
    let dynamic_preview = plan
        .dynamic_causal_rows()
        .iter()
        .map(|row| {
            let target = model
                .problem
                .solve_layout
                .solver_maps
                .names
                .get(row.target_index())
                .map_or("<unnamed>", String::as_str);
            format!(
                "{target}@source{}:{}",
                row.source().node(),
                row.source().program()
            )
        })
        .collect::<Vec<_>>()
        .join(", ");
    let duplicate_targets = plan.rows.len()
        - plan
            .rows
            .iter()
            .map(|row| row.target_index())
            .collect::<IndexSet<_>>()
            .len();
    let projection_unknowns = plan
        .simultaneous_plan
        .blocks
        .iter()
        .map(|block| block.y_indices.len())
        .sum::<usize>();
    let coupled_blocks = plan
        .simultaneous_plan
        .blocks
        .iter()
        .filter(|block| block.y_indices.len() > 1)
        .count();
    let max_block = plan
        .simultaneous_plan
        .blocks
        .iter()
        .map(|block| block.y_indices.len())
        .max()
        .unwrap_or(0);
    let direct_rows = plan
        .rows
        .iter()
        .filter(|row| row.direct_assignment_certified())
        .count();
    let exact_rows = plan
        .rows
        .iter()
        .filter(|row| row.exact_assignment_certified())
        .count();
    tracing::debug!(
        target: "rumoca_eval_solve::refresh",
        "{name} refresh plan: rows={} seed_rows={} static_seed_rows={} direct_rows={} exact_rows={} duplicate_targets={} projection_blocks={} value_projection_blocks={} projection_unknowns={} coupled_blocks={} max_block={} causal_certified={} rows=[{}] dynamic=[{}]",
        plan.rows.len(),
        plan.causal_seed_rows.len(),
        plan.static_causal_seed_rows.len(),
        direct_rows,
        exact_rows,
        duplicate_targets,
        plan.simultaneous_plan.blocks.len(),
        plan.value_projection_plan.blocks.len(),
        projection_unknowns,
        coupled_blocks,
        max_block,
        plan.causal_solution_certified,
        preview,
        dynamic_preview
    );
}

fn trace_algebraic_refresh() -> bool {
    tracing::enabled!(target: "rumoca_eval_solve::refresh", tracing::Level::DEBUG)
}

fn construct_refresh_row(
    draft: solve::AlgebraicRefreshRowDraft,
    span: Option<rumoca_core::Span>,
) -> Result<AlgebraicRefreshRow, EvalSolveError> {
    AlgebraicRefreshRow::checked(draft).map_err(|error| EvalSolveError::InvalidRow {
        message: error.to_string(),
        span,
    })
}

fn construct_refresh_selection(
    row_count: usize,
    indices: impl IntoIterator<Item = usize>,
    span: Option<rumoca_core::Span>,
) -> Result<RefreshRowSelection, EvalSolveError> {
    RefreshRowSelection::checked(row_count, indices).map_err(|error| EvalSolveError::InvalidRow {
        message: error.to_string(),
        span,
    })
}

fn build_canonical_algebraic_refresh_plan(
    problem: &solve::SolveProblem,
    catalog: &CanonicalScalarProgramCatalog<'_>,
) -> Result<RefreshPlan, EvalSolveError> {
    validate_canonical_implicit_output_inventory(problem, catalog)?;
    let state_count = problem.solve_layout.state_scalar_count();
    let solver_count = problem.solve_layout.solver_scalar_count();
    let span = catalog.first_span();
    let mut rows = Vec::new();
    reserve_refresh_vec_capacity(
        &mut rows,
        catalog.len(),
        "canonical refresh assignment owners",
        span,
    )?;
    let mut claimed_targets = IndexSet::new();
    reserve_refresh_index_set_capacity(
        &mut claimed_targets,
        catalog.len(),
        "canonical refresh target owners",
        span,
    )?;
    for (equation_index, target) in problem.continuous.implicit_row_targets.iter().enumerate() {
        let Some(solve::ScalarSlot::Y {
            index: target_index,
            ..
        }) = target
        else {
            continue;
        };
        if *target_index < state_count || *target_index >= solver_count {
            continue;
        }
        let Some(position) = catalog.positions().get(&equation_index).copied() else {
            // Tensor nodes remain owned by the compact ComputeBlock and its
            // BLT projection stage. They never become scalar assignments here.
            continue;
        };
        let Some(program) = catalog.program(position.program_index) else {
            continue;
        };
        if !crate::prepared::program_can_evaluate_declared_target(
            program.operations,
            position.output_offset,
            *target_index,
        )? || !claimed_targets.insert(*target_index)
        {
            continue;
        }
        let assignment_shape = crate::prepared::assignment_shape_for_program_output(
            program.operations,
            position.output_offset,
            *target_index,
        )?;
        rows.push(construct_refresh_row(
            solve::AlgebraicRefreshRowDraft {
                owner_id: RefreshRowOwnerId::checked(*target_index).ok_or_else(|| {
                    EvalSolveError::InvalidRow {
                        message: "continuous refresh row owner index exceeds u32".to_string(),
                        span: Some(program.span),
                    }
                })?,
                source: program.source,
                equation_index,
                output_offset: position.output_offset,
                target_index: *target_index,
                assignment_target: Some(*target_index),
                assignment_shape,
                direct_assignment_certified: crate::prepared::program_certifies_direct_target(
                    program.operations,
                    position.output_offset,
                    *target_index,
                )?,
                exact_assignment_certified: crate::prepared::program_certifies_exact_target(
                    program.operations,
                    position.output_offset,
                    *target_index,
                )?,
            },
            Some(program.span),
        )?);
    }
    let causal_solution_certified =
        complete_causal_projection_is_certified(problem, catalog, &rows);
    let mut plan = order_refresh_rows(rows, catalog, state_count, causal_solution_certified)?;
    plan.simultaneous_plan = problem.continuous.algebraic_projection_plan.clone();
    plan.simultaneous_block_indices = (0..plan.simultaneous_plan.blocks.len()).collect();
    configure_causal_seed_rows(
        &mut plan,
        catalog,
        state_count,
        ContinuousStaticParameters::from_layout(&problem.solve_layout),
    )?;
    Ok(plan)
}

fn validate_canonical_implicit_output_inventory(
    problem: &solve::SolveProblem,
    catalog: &CanonicalScalarProgramCatalog<'_>,
) -> Result<(), EvalSolveError> {
    let state_count = problem.solve_layout.state_scalar_count();
    let solver_count = problem.solve_layout.solver_scalar_count();
    let mut produced = vec![false; solver_count];
    for (equation, target) in problem.continuous.implicit_row_targets.iter().enumerate() {
        let Some(solve::ScalarSlot::Y { index, .. }) = target else {
            continue;
        };
        if *index >= solver_count {
            return Err(EvalSolveError::InvalidRow {
                message: format!(
                    "implicit residual output row {equation} targets Y index {index}, but the solver layout has {solver_count} rows"
                ),
                span: catalog.first_span(),
            });
        }
        if *index < state_count {
            continue;
        }
        if !catalog.produces_output(equation) {
            return Err(EvalSolveError::InvalidRow {
                message: format!(
                    "implicit algebraic system is missing residual output row {equation}"
                ),
                span: catalog.first_span(),
            });
        }
        produced[*index] = true;
    }
    for projection in &problem.continuous.algebraic_projection_plan.blocks {
        if projection
            .rows
            .iter()
            .all(|row| catalog.produces_output(*row))
        {
            mark_projection_outputs(&mut produced, solver_count, projection);
        }
    }
    if let Some(index) = (state_count..solver_count).find(|index| !produced[*index]) {
        return Err(EvalSolveError::InvalidRow {
            message: format!("implicit algebraic system is missing a producer for Y index {index}"),
            span: catalog.first_span(),
        });
    }
    Ok(())
}

pub fn build_algebraic_refresh_plan(
    problem: &solve::SolveProblem,
    block: &PreparedScalarProgramBlock,
) -> Result<RefreshPlan, EvalSolveError> {
    let state_count = problem.solve_layout.state_scalar_count();
    validate_implicit_output_inventory(problem, block.block())?;
    let row_target_rows = algebraic_refresh_rows_from_row_targets(problem, block, state_count)?;
    let mut rows_by_target = IndexMap::new();
    reserve_refresh_index_map_capacity(
        &mut rows_by_target,
        row_target_rows.len(),
        "row-target map",
        first_block_span(block.block()),
    )?;
    for row in row_target_rows {
        rows_by_target.insert(row.target_index(), row);
    }
    let mut rows = Vec::new();
    reserve_refresh_vec_capacity(
        &mut rows,
        rows_by_target.len(),
        "ordered target rows",
        first_block_span(block.block()),
    )?;
    rows.extend(rows_by_target.into_values());
    if trace_algebraic_refresh() {
        let owned = rows
            .iter()
            .map(|row| row.target_index())
            .collect::<BTreeSet<_>>();
        let missing = problem
            .continuous
            .algebraic_projection_plan
            .blocks
            .iter()
            .flat_map(|block| block.y_indices.iter().copied())
            .filter(|target| !owned.contains(target))
            .filter_map(|target| {
                problem
                    .solve_layout
                    .solver_maps
                    .names
                    .get(target)
                    .map(|name| format!("{name}@{target}"))
            })
            .collect::<Vec<_>>();
        tracing::debug!(target: "rumoca_eval_solve::refresh", ?missing, "algebraic targets without exact refresh owners");
    }
    let causal_solution_certified =
        complete_causal_projection_is_certified(problem, block.block(), &rows);
    tracing::debug!(
        target: "rumoca_eval_solve::refresh",
        candidate = causal_solution_certified,
        rows = rows.len(),
        "complete algebraic causal-certificate candidate"
    );
    let mut plan = order_refresh_rows(rows, block.block(), state_count, causal_solution_certified)?;
    plan.simultaneous_plan = problem.continuous.algebraic_projection_plan.clone();
    plan.simultaneous_block_indices = (0..plan.simultaneous_plan.blocks.len()).collect();
    configure_causal_seed_rows(
        &mut plan,
        block.block(),
        state_count,
        ContinuousStaticParameters::from_layout(&problem.solve_layout),
    )?;
    Ok(plan)
}

fn validate_implicit_output_inventory(
    problem: &solve::SolveProblem,
    block: &solve::ScalarProgramBlock,
) -> Result<(), EvalSolveError> {
    let positions = output_row_positions(block)?;
    let state_count = problem.solve_layout.state_scalar_count();
    let solver_count = problem.solve_layout.solver_scalar_count();
    let targets = &problem.continuous.implicit_row_targets;
    let mut produced = vec![false; solver_count];
    for (row, target) in targets.iter().enumerate() {
        let Some(solve::ScalarSlot::Y { index, .. }) = target else {
            continue;
        };
        if *index >= solver_count {
            return Err(EvalSolveError::InvalidRow {
                message: format!(
                    "implicit residual output row {row} targets Y index {index}, but the solver layout has {solver_count} rows"
                ),
                span: first_block_span(block),
            });
        }
        // State-owned implicit rows are part of the residual inventory, but
        // algebraic refresh neither evaluates nor owns them.  In particular,
        // an explicit derivative row is allowed to have no residual program
        // in this block.  Validate only the algebraic suffix that this plan
        // is responsible for constructing.
        if *index < state_count {
            continue;
        }
        if !positions.contains_key(&row) {
            return Err(EvalSolveError::InvalidRow {
                message: format!("implicit algebraic system is missing residual output row {row}"),
                span: first_block_span(block),
            });
        }
        produced[*index] = true;
    }
    // A coupled or otherwise non-isolatable residual legitimately has no
    // direct row target. Its compiler-owned projection block is the producer.
    for projection in &problem.continuous.algebraic_projection_plan.blocks {
        if projection
            .rows
            .iter()
            .all(|row| positions.contains_key(row))
        {
            mark_projection_outputs(&mut produced, solver_count, projection);
        }
    }
    if let Some(index) = (state_count..solver_count).find(|index| !produced[*index]) {
        return Err(EvalSolveError::InvalidRow {
            message: format!("implicit algebraic system is missing a producer for Y index {index}"),
            span: first_block_span(block),
        });
    }
    Ok(())
}

fn mark_projection_outputs(
    produced: &mut [bool],
    solver_count: usize,
    projection: &solve::AlgebraicProjectionBlock,
) {
    for &index in &projection.y_indices {
        if index < solver_count {
            produced[index] = true;
        }
    }
}

fn algebraic_refresh_rows_from_row_targets(
    problem: &solve::SolveProblem,
    block: &PreparedScalarProgramBlock,
    state_count: usize,
) -> Result<Vec<AlgebraicRefreshRow>, EvalSolveError> {
    let solver_count = problem.solve_layout.solver_scalar_count();
    let span = first_block_span(block.block());
    let output_row_positions = output_row_positions(block.block())?;
    let mut rows = Vec::new();
    reserve_refresh_vec_capacity(
        &mut rows,
        problem.continuous.implicit_row_targets.len(),
        "row-target refresh rows",
        span,
    )?;
    let mut claimed_targets = IndexSet::new();
    reserve_refresh_index_set_capacity(
        &mut claimed_targets,
        problem.continuous.implicit_row_targets.len(),
        "claimed row targets",
        span,
    )?;
    for (row_idx, target) in problem.continuous.implicit_row_targets.iter().enumerate() {
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
        if !block.can_evaluate_declared_target_assignment(
            position.program_index,
            position.output_offset,
            target_index,
        ) {
            continue;
        }
        reserve_refresh_index_set_capacity(&mut claimed_targets, 1, "claimed row targets", span)?;
        if !claimed_targets.insert(target_index) {
            continue;
        }
        rows.push(construct_refresh_row(
            solve::AlgebraicRefreshRowDraft {
                owner_id: RefreshRowOwnerId::checked(target_index).ok_or_else(|| {
                    EvalSolveError::InvalidRow {
                        message: "continuous refresh row owner index exceeds u32".to_string(),
                        span,
                    }
                })?,
                source: solve::RefreshScalarProgramSource::checked(0, position.program_index)
                    .ok_or_else(|| EvalSolveError::InvalidRow {
                        message: "continuous refresh source identity exceeds u32".to_string(),
                        span,
                    })?,
                equation_index: row_idx,
                output_offset: position.output_offset,
                target_index,
                assignment_target: Some(target_index),
                assignment_shape: block.assignment_shape_for_output(
                    position.program_index,
                    position.output_offset,
                    target_index,
                ),
                direct_assignment_certified: block.certifies_direct_target_assignment(
                    position.program_index,
                    position.output_offset,
                    target_index,
                ),
                exact_assignment_certified: block.certifies_exact_target_assignment_output(
                    position.program_index,
                    position.output_offset,
                    target_index,
                ),
            },
            span,
        )?);
    }
    Ok(rows)
}

pub fn build_derivative_refresh_plan(
    problem: &solve::SolveProblem,
    derivative_block: &solve::ScalarProgramBlock,
    implicit_block: &PreparedScalarProgramBlock,
    full_plan: &RefreshPlan,
) -> Result<RefreshPlan, EvalSolveError> {
    let state_count = problem.solve_layout.state_scalar_count();
    let initial_deps = derivative_row_dependencies(derivative_block, state_count)?;
    build_dependency_refresh_plan(problem, implicit_block, full_plan, initial_deps)
}

/// Construct the complete checked refresh inventory while Solve lowering owns
/// the canonical continuous and event projections. Runtime adapters consume
/// this aggregate and never rerun dependency or assignment discovery.
pub fn build_continuous_refresh_owners(
    problem: &mut solve::SolveProblem,
) -> Result<solve::ContinuousRefreshOwners, EvalSolveError> {
    // Make the canonical block tearing exact-only before any refresh plan clones
    // it, so every runtime projection (the refresh owners' `simultaneous_plan`
    // and `value_projection_plan`, the value-stage plans that must replay those
    // blocks, and any direct reader of `algebraic_projection_plan`) sees the
    // same exact-only tearing.
    normalize_algebraic_projection_tearing(problem)?;
    let catalog = CanonicalScalarProgramCatalog::construct(&problem.continuous.implicit_rhs)?;
    let algebraic = build_canonical_algebraic_refresh_plan(problem, &catalog)?;
    let state_count = problem.solve_layout.state_scalar_count();
    let derivative_dependencies =
        compute_block_dependencies(&problem.continuous.derivative_rhs, state_count)?;
    let derivative = build_dependency_refresh_plan_from_access(
        problem,
        &catalog,
        &algebraic,
        derivative_dependencies,
        catalog.positions(),
    )?;
    let mut root_dependencies =
        scalar_program_block_dependencies(&problem.events.root_conditions, state_count)?;
    root_dependencies.extend(scalar_program_block_dependencies(
        &problem.events.delays.delay_time_rhs,
        state_count,
    )?);
    let root = build_dependency_refresh_plan_from_access(
        problem,
        &catalog,
        &algebraic,
        CompactYDependencySet::from_explicit(root_dependencies),
        catalog.positions(),
    )?;
    let event_dependencies = event_consumer_dependencies(problem, state_count, None)?;
    let event = build_dependency_refresh_plan_from_access(
        problem,
        &catalog,
        &algebraic,
        event_dependencies,
        catalog.positions(),
    )?;
    let clock_events = (0..problem.clocks.periodic_event_schedules.len())
        .map(|clock_index| {
            let clock = problem
                .clocks
                .periodic_clock_id(clock_index)
                .ok_or_else(|| EvalSolveError::InvalidRow {
                    message: "periodic refresh clock identity is invalid".to_string(),
                    span: catalog.first_span(),
                })?;
            let dependencies = event_consumer_dependencies(problem, state_count, Some(clock))?;
            build_dependency_refresh_plan_from_access(
                problem,
                &catalog,
                &algebraic,
                dependencies,
                catalog.positions(),
            )
        })
        .collect::<Result<Vec<_>, _>>()?;
    solve::ContinuousRefreshOwners::checked_for_source(
        &problem.continuous.implicit_rhs,
        algebraic,
        derivative,
        root,
        event,
        clock_events,
    )
    .map_err(|error| EvalSolveError::InvalidRow {
        message: error.to_string(),
        span: catalog.first_span(),
    })
}

/// Rewrite every algebraic block's tearing so back-substitution is exact-only.
///
/// Structural tearing is incidence-based and cannot know which causal rows are
/// exact explicit assignments, so a greedy causal step may be a genuine
/// implicit constraint. Runtime back-substitution is exact-only by
/// construction, so any causal step that is not a compiler-certified exact
/// target assignment is promoted into the reduced Newton here: its unknown
/// becomes a tear variable and its row a reduced residual. The reduced Newton
/// (with a finite-difference Jacobian and line search over the tear variables)
/// then carries that nonlinearity, and the remaining causal steps stay exact.
fn normalize_algebraic_projection_tearing(
    problem: &mut solve::SolveProblem,
) -> Result<(), EvalSolveError> {
    // Build the exactness predicate from the same scalar projection the runtime
    // prepares for `implicit_scalar_rhs`, so the promotion decision matches the
    // runtime's `implicit_target_assignment_is_exact` row for row.
    let implicit_scalar_rhs = PreparedScalarProgramBlock::new(
        crate::to_scalar_program_projection(&problem.continuous.implicit_rhs)?.into_block(),
    )?;
    for block in &mut problem.continuous.algebraic_projection_plan.blocks {
        if let Some(tearing) = block.tearing.as_mut() {
            promote_inexact_causal_steps(tearing, &implicit_scalar_rhs);
        }
    }
    Ok(())
}

/// Promote each causal step that is not an exact explicit assignment into the
/// reduced Newton, preserving `tear_y_indices.len() == residual_rows.len()`.
///
/// A promotion appends the step's unknown to `tear_y_indices` and its row to
/// `residual_rows` (one of each). Retained causal steps keep their original
/// relative order and stay valid, because a promoted unknown is fixed as a tear
/// variable before back-substitution runs. When every step is promoted the
/// block degenerates to a dense reduced Newton over all its unknowns, which is
/// exactly what an empty `causal_steps` drives.
fn promote_inexact_causal_steps(
    tearing: &mut solve::BlockTearing,
    implicit_scalar_rhs: &PreparedScalarProgramBlock,
) {
    let mut retained = Vec::with_capacity(tearing.causal_steps.len());
    for step in std::mem::take(&mut tearing.causal_steps) {
        if causal_step_certifies_exact_assignment(implicit_scalar_rhs, step.row, step.y_index) {
            retained.push(step);
        } else {
            tearing.tear_y_indices.push(step.y_index);
            tearing.residual_rows.push(step.row);
        }
    }
    tearing.causal_steps = retained;
}

/// Whether evaluating `row`'s target isolator and writing its value satisfies
/// the scalar residual exactly for solver-Y unknown `y_index`. This mirrors the
/// runtime `implicit_target_assignment_is_exact` predicate exactly.
fn causal_step_certifies_exact_assignment(
    implicit_scalar_rhs: &PreparedScalarProgramBlock,
    row: usize,
    y_index: usize,
) -> bool {
    implicit_scalar_rhs
        .row_output_position(row)
        .is_some_and(|(program_idx, output_offset)| {
            implicit_scalar_rhs.certifies_exact_target_assignment_output(
                program_idx,
                output_offset,
                y_index,
            )
        })
}

fn extend_scalar_block_dependencies(
    dependencies: &mut CompactYDependencySet,
    block: &solve::ScalarProgramBlock,
    state_count: usize,
) -> Result<(), EvalSolveError> {
    dependencies
        .extend_explicit(scalar_program_block_dependencies(block, state_count)?)
        .map_err(|error| compact_dependency_error(error, first_block_span(block)))
}

fn compute_block_dependencies(
    block: &solve::ComputeBlock,
    state_count: usize,
) -> Result<CompactYDependencySet, EvalSolveError> {
    block.validate_shape_contract("continuous refresh dependency certificate")?;
    let mut dependencies = CompactYDependencySet::default();
    for node in &block.nodes {
        extend_compute_node_dependencies(&mut dependencies, node, state_count)?;
    }
    Ok(dependencies)
}

fn extend_compute_node_dependencies(
    dependencies: &mut CompactYDependencySet,
    node: &solve::ComputeNode,
    state_count: usize,
) -> Result<(), EvalSolveError> {
    match node {
        solve::ComputeNode::ScalarPrograms(block) => {
            extend_scalar_block_dependencies(dependencies, block, state_count)?;
        }
        solve::ComputeNode::MatMul {
            lhs_ops,
            rhs_ops,
            span,
            ..
        } => {
            extend_program_dependencies(dependencies, lhs_ops, state_count, Some(*span))?;
            extend_program_dependencies(dependencies, rhs_ops, state_count, Some(*span))?;
        }
        solve::ComputeNode::LinSolve {
            setup_ops, span, ..
        } => extend_program_dependencies(dependencies, setup_ops, state_count, Some(*span))?,
        solve::ComputeNode::Map {
            domain,
            base_ops,
            load_strides,
            span,
            ..
        }
        | solve::ComputeNode::AffineStencil {
            domain,
            base_ops,
            load_strides,
            span,
            ..
        } => extend_affine_program_dependencies(
            dependencies,
            domain,
            base_ops,
            load_strides,
            state_count,
            *span,
        )?,
    }
    Ok(())
}

fn extend_affine_program_dependencies(
    dependencies: &mut CompactYDependencySet,
    domain: &rumoca_core::StructuredIndexDomain,
    base_ops: &[solve::LinearOp],
    load_strides: &[solve::AffineStencilLoadStride],
    state_count: usize,
    span: rumoca_core::Span,
) -> Result<(), EvalSolveError> {
    let point_count = domain
        .scalar_count()
        .map_err(|error| EvalSolveError::InvalidRow {
            message: format!("continuous refresh dependency domain is invalid: {error:?}"),
            span: Some(span),
        })?;
    if point_count == 0 {
        return Ok(());
    }
    extend_program_dependencies(dependencies, base_ops, state_count, Some(span))?;
    for (op_position, operation) in base_ops.iter().enumerate() {
        let solve::LinearOp::LoadY { index, .. } = operation else {
            continue;
        };
        let terms = load_strides
            .iter()
            .filter(|stride| stride.op_position == op_position)
            .flat_map(|stride| stride.terms.iter());
        dependencies
            .insert_affine(*index, domain, terms)
            .map_err(|error| compact_dependency_error(error, Some(span)))?;
    }
    Ok(())
}

fn extend_program_dependencies(
    dependencies: &mut CompactYDependencySet,
    program: &[solve::LinearOp],
    state_count: usize,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    for mut range in row_y_input_ranges(program) {
        range.start = range.start.max(state_count);
        dependencies
            .insert_range(range)
            .map_err(|error| compact_dependency_error(error, span))?;
    }
    Ok(())
}

fn compact_dependency_error(
    error: CompactYDependencyError,
    span: Option<rumoca_core::Span>,
) -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: error.to_string(),
        span,
    }
}

fn build_dependency_refresh_plan(
    problem: &solve::SolveProblem,
    prepared_implicit_block: &PreparedScalarProgramBlock,
    full_plan: &RefreshPlan,
    initial_deps: IndexSet<usize>,
) -> Result<RefreshPlan, EvalSolveError> {
    let implicit_block = prepared_implicit_block.block();
    let output_positions = output_row_positions(implicit_block)?;
    build_dependency_refresh_plan_from_access(
        problem,
        implicit_block,
        full_plan,
        CompactYDependencySet::from_explicit(initial_deps),
        &output_positions,
    )
}

fn build_dependency_refresh_plan_from_access<A: RefreshProgramAccess + ?Sized>(
    problem: &solve::SolveProblem,
    implicit_block: &A,
    full_plan: &RefreshPlan,
    initial_deps: CompactYDependencySet,
    output_positions: &IndexMap<usize, OutputRowPosition>,
) -> Result<RefreshPlan, EvalSolveError> {
    let state_count = problem.solve_layout.state_scalar_count();
    let span = implicit_block.first_span();
    let target_to_row = dependency_target_rows(full_plan, span)?;
    let block_by_target = dependency_blocks_by_target(problem, full_plan, state_count, span)?;
    let (needed, needed_blocks) = collect_dependency_closure(
        full_plan,
        implicit_block,
        initial_deps,
        &target_to_row,
        &block_by_target,
        output_positions,
        state_count,
    )?;
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
            .filter(|row| needed.contains(&row.target_index()))
            .cloned(),
    );
    append_exact_projection_owners(
        implicit_block,
        full_plan,
        &needed,
        &needed_blocks,
        output_positions,
        &mut rows,
    )?;
    let selected_blocks = full_plan
        .simultaneous_plan
        .blocks
        .iter()
        .enumerate()
        .filter(|(block_idx, _)| needed_blocks.contains(block_idx))
        .collect::<Vec<_>>();
    let simultaneous_plan = solve::AlgebraicProjectionPlan {
        blocks: selected_blocks
            .iter()
            .map(|(_, block)| (*block).clone())
            .collect(),
    };
    let simultaneous_block_indices = selected_blocks
        .iter()
        .map(|(local_index, _)| full_plan.simultaneous_block_indices[*local_index])
        .collect();
    let causal_solution_certified = dependency_causal_projection_is_certified(
        &rows,
        &needed,
        &simultaneous_plan,
        state_count,
        problem.solve_layout.solver_scalar_count(),
    );
    let mut plan =
        order_refresh_rows(rows, implicit_block, state_count, causal_solution_certified)?;
    plan.simultaneous_plan = simultaneous_plan;
    plan.simultaneous_block_indices = simultaneous_block_indices;
    configure_causal_seed_rows(
        &mut plan,
        implicit_block,
        state_count,
        ContinuousStaticParameters::from_layout(&problem.solve_layout),
    )?;
    Ok(plan)
}

fn append_exact_projection_owners<A: RefreshProgramAccess + ?Sized>(
    block: &A,
    full_plan: &RefreshPlan,
    needed: &IndexSet<usize>,
    needed_blocks: &IndexSet<usize>,
    output_positions: &IndexMap<usize, OutputRowPosition>,
    rows: &mut Vec<AlgebraicRefreshRow>,
) -> Result<(), EvalSolveError> {
    let span = block.first_span();
    let mut claimed_targets = rows
        .iter()
        .map(|row| row.target_index())
        .collect::<IndexSet<_>>();
    reserve_refresh_index_set_capacity(
        &mut claimed_targets,
        needed.len(),
        "dependency exact-owner targets",
        span,
    )?;
    for block_index in needed_blocks {
        let Some(projection_block) = full_plan.simultaneous_plan.blocks.get(*block_index) else {
            continue;
        };
        let ([equation_index], [target_index]) = (
            projection_block.rows.as_slice(),
            projection_block.y_indices.as_slice(),
        ) else {
            continue;
        };
        if !needed.contains(target_index) || claimed_targets.contains(target_index) {
            continue;
        }
        let Some(position) = output_positions.get(equation_index).copied() else {
            continue;
        };
        let Some(program) = block.program(position.program_index) else {
            continue;
        };
        if !crate::prepared::program_certifies_exact_target(
            program,
            position.output_offset,
            *target_index,
        )? {
            continue;
        }
        reserve_refresh_vec_capacity(rows, 1, "dependency exact-owner rows", span)?;
        rows.push(construct_refresh_row(
            solve::AlgebraicRefreshRowDraft {
                owner_id: RefreshRowOwnerId::checked(*target_index).ok_or_else(|| {
                    EvalSolveError::InvalidRow {
                        message: "continuous refresh row owner index exceeds u32".to_string(),
                        span,
                    }
                })?,
                source: block.source(position.program_index).ok_or_else(|| {
                    EvalSolveError::InvalidRow {
                        message: "continuous refresh source identity exceeds u32".to_string(),
                        span,
                    }
                })?,
                equation_index: *equation_index,
                output_offset: position.output_offset,
                target_index: *target_index,
                assignment_target: Some(*target_index),
                assignment_shape: crate::prepared::assignment_shape_for_program_output(
                    program,
                    position.output_offset,
                    *target_index,
                )?,
                direct_assignment_certified: crate::prepared::program_certifies_direct_target(
                    program,
                    position.output_offset,
                    *target_index,
                )?,
                exact_assignment_certified: true,
            },
            span,
        )?);
        claimed_targets.insert(*target_index);
    }
    Ok(())
}

fn dependency_target_rows(
    plan: &RefreshPlan,
    span: Option<rumoca_core::Span>,
) -> Result<IndexMap<usize, solve::RefreshScalarProgramSource>, EvalSolveError> {
    let mut target_to_row = IndexMap::new();
    reserve_refresh_index_map_capacity(
        &mut target_to_row,
        plan.rows.len(),
        "dependency target-to-row map",
        span,
    )?;
    target_to_row.extend(
        plan.rows
            .iter()
            .map(|row| (row.target_index(), row.source())),
    );
    Ok(target_to_row)
}

fn dependency_blocks_by_target(
    problem: &solve::SolveProblem,
    plan: &RefreshPlan,
    state_count: usize,
    span: Option<rumoca_core::Span>,
) -> Result<IndexMap<usize, usize>, EvalSolveError> {
    let mut block_by_target = IndexMap::new();
    reserve_refresh_index_map_capacity(
        &mut block_by_target,
        problem
            .solve_layout
            .solver_scalar_count()
            .saturating_sub(state_count),
        "dependency projection-block map",
        span,
    )?;
    for (block_index, block) in plan.simultaneous_plan.blocks.iter().enumerate() {
        block_by_target.extend(
            block
                .y_indices
                .iter()
                .map(|target_index| (*target_index, block_index)),
        );
    }
    Ok(block_by_target)
}

fn collect_dependency_closure<A: RefreshProgramAccess + ?Sized>(
    plan: &RefreshPlan,
    implicit_block: &A,
    initial_deps: CompactYDependencySet,
    target_to_row: &IndexMap<usize, solve::RefreshScalarProgramSource>,
    block_by_target: &IndexMap<usize, usize>,
    output_positions: &IndexMap<usize, OutputRowPosition>,
    state_count: usize,
) -> Result<(IndexSet<usize>, IndexSet<usize>), EvalSolveError> {
    let span = implicit_block.first_span();
    let mut stack = initial_deps
        .into_seed_stack(target_to_row.keys().chain(block_by_target.keys()).copied())
        .map_err(|error| compact_dependency_error(error, span))?;
    let mut needed = IndexSet::new();
    reserve_refresh_index_set_capacity(&mut needed, stack.len(), "dependency needed set", span)?;
    let mut needed_blocks = IndexSet::new();
    reserve_refresh_index_set_capacity(
        &mut needed_blocks,
        plan.simultaneous_plan.blocks.len(),
        "dependency projection blocks",
        span,
    )?;
    while let Some(index) = stack.pop() {
        if index < state_count || !insert_dependency(&mut needed, index, span)? {
            continue;
        }
        if let Some(block_index) = block_by_target.get(&index).copied() {
            if insert_projection_block(&mut needed_blocks, block_index, span)? {
                enqueue_projection_block_dependencies(
                    plan,
                    implicit_block,
                    output_positions,
                    block_index,
                    state_count,
                    &mut stack,
                )?;
            }
        } else if let Some(source) = target_to_row.get(&index).copied() {
            enqueue_source_dependencies(implicit_block, source, state_count, &mut stack, span)?;
        }
    }
    Ok((needed, needed_blocks))
}

fn insert_dependency(
    needed: &mut IndexSet<usize>,
    index: usize,
    span: Option<rumoca_core::Span>,
) -> Result<bool, EvalSolveError> {
    reserve_refresh_index_set_capacity(needed, 1, "dependency needed set", span)?;
    Ok(needed.insert(index))
}

fn insert_projection_block(
    needed: &mut IndexSet<usize>,
    index: usize,
    span: Option<rumoca_core::Span>,
) -> Result<bool, EvalSolveError> {
    reserve_refresh_index_set_capacity(needed, 1, "dependency projection blocks", span)?;
    Ok(needed.insert(index))
}

fn enqueue_projection_block_dependencies<A: RefreshProgramAccess + ?Sized>(
    plan: &RefreshPlan,
    implicit_block: &A,
    output_positions: &IndexMap<usize, OutputRowPosition>,
    block_index: usize,
    state_count: usize,
    stack: &mut Vec<usize>,
) -> Result<(), EvalSolveError> {
    let span = implicit_block.first_span();
    let block = &plan.simultaneous_plan.blocks[block_index];
    reserve_refresh_vec_capacity(stack, block.y_indices.len(), "dependency stack", span)?;
    stack.extend(block.y_indices.iter().copied());
    for equation_index in &block.rows {
        if let Some(position) = output_positions.get(equation_index) {
            enqueue_row_dependencies(
                implicit_block,
                position.program_index,
                state_count,
                stack,
                span,
            )?;
        }
    }
    Ok(())
}

fn enqueue_row_dependencies<A: RefreshProgramAccess + ?Sized>(
    block: &A,
    row_index: usize,
    state_count: usize,
    stack: &mut Vec<usize>,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    let Some(program) = block.program(row_index) else {
        return Ok(());
    };
    for dependency in row_y_input_ranges(program).into_iter().flatten() {
        if dependency >= state_count {
            reserve_refresh_vec_capacity(stack, 1, "dependency stack", span)?;
            stack.push(dependency);
        }
    }
    Ok(())
}

fn enqueue_source_dependencies<A: RefreshProgramAccess + ?Sized>(
    block: &A,
    source: solve::RefreshScalarProgramSource,
    state_count: usize,
    stack: &mut Vec<usize>,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    let Some(program) = block.source_program(source) else {
        return Ok(());
    };
    for dependency in row_y_input_ranges(program).into_iter().flatten() {
        if dependency >= state_count {
            reserve_refresh_vec_capacity(stack, 1, "dependency stack", span)?;
            stack.push(dependency);
        }
    }
    Ok(())
}

fn configure_causal_seed_rows<A: RefreshProgramAccess + ?Sized>(
    plan: &mut RefreshPlan,
    block: &A,
    state_count: usize,
    continuous_static_parameters: ContinuousStaticParameters,
) -> Result<(), EvalSolveError> {
    let span = block.first_span();
    plan.causal_seed_rows = construct_refresh_selection(plan.rows.len(), 0..plan.rows.len(), span)?;
    let static_targets =
        parameter_static_refresh_targets(plan, block, state_count, continuous_static_parameters);
    plan.static_causal_seed_rows = construct_refresh_selection(
        plan.rows.len(),
        plan.rows
            .iter()
            .enumerate()
            .filter(|(_, row)| static_targets.contains(&row.target_index()))
            .map(|(index, _)| index),
        span,
    )?;
    plan.dynamic_causal_seed_rows = construct_refresh_selection(
        plan.rows.len(),
        plan.rows
            .iter()
            .enumerate()
            .filter(|(_, row)| !static_targets.contains(&row.target_index()))
            .map(|(index, _)| index),
        span,
    )?;
    plan.value_projection_plan = if plan.causal_solution_certified {
        solve::AlgebraicProjectionPlan {
            blocks: plan
                .simultaneous_plan
                .blocks
                .iter()
                .filter(|block| !block_is_exactly_seeded(block, plan.causal_rows()))
                .cloned()
                .collect(),
        }
    } else {
        // An uncertified seed order may read an algebraic dependency whose
        // producer is only present in the simultaneous plan. Such a seed is a
        // useful warm start, but it cannot replace its residual block: the
        // producer can change after the seed ran. Retain every block until the
        // complete seed schedule is dependency-certified.
        plan.simultaneous_plan.clone()
    };
    plan.value_stages = build_refresh_stages(
        &plan.simultaneous_plan,
        &plan.simultaneous_block_indices,
        &plan.rows,
        &plan.static_causal_seed_rows,
        plan.causal_solution_certified,
    )
    .map_err(|error| EvalSolveError::InvalidRow {
        message: error.to_string(),
        span,
    })?;
    Ok(())
}

fn parameter_static_refresh_targets<A: RefreshProgramAccess + ?Sized>(
    plan: &RefreshPlan,
    block: &A,
    state_count: usize,
    continuous_static_parameters: ContinuousStaticParameters,
) -> BTreeSet<usize> {
    // Begin with the complete algebraic candidate set and remove every target
    // whose dependency closure reaches time, a state, or a non-candidate.
    // This greatest-fixed-point direction is load-bearing for parameter-only
    // algebraic loops: a closed simultaneous block can be static even though
    // none of its members is independently orderable from parameters first.
    let candidates = plan
        .causal_rows()
        .iter()
        .map(|refresh_row| {
            let target = refresh_row.target_index();
            let dependencies = block
                .source_program(refresh_row.source())
                .and_then(ParameterStaticDependencies::derive);
            (target, dependencies)
        })
        .collect::<Vec<_>>();
    let mut static_targets = candidates
        .iter()
        .map(|(target, _)| *target)
        .collect::<BTreeSet<_>>();
    loop {
        let rejected = candidates
            .iter()
            .filter(|(target, _)| static_targets.contains(target))
            .filter(|(target, dependencies)| {
                dependencies.as_ref().is_none_or(|dependencies| {
                    !dependencies.is_parameter_static(
                        *target,
                        state_count,
                        &static_targets,
                        continuous_static_parameters,
                    )
                })
            })
            .map(|(target, _)| *target)
            .collect::<Vec<_>>();
        if rejected.is_empty() {
            return static_targets;
        }
        for target in rejected {
            static_targets.remove(&target);
        }
    }
}

struct ParameterStaticDependencies {
    y: Vec<BTreeSet<usize>>,
    parameters: Vec<BTreeSet<usize>>,
    time: Vec<bool>,
    seed: Vec<bool>,
    effect: Vec<bool>,
}

impl ParameterStaticDependencies {
    fn derive(program: &[solve::LinearOp]) -> Option<Self> {
        Some(Self {
            y: solve::StructuralPattern::derive_output_y_dependencies(program, None).ok()?,
            parameters: solve::StructuralPattern::derive_output_p_dependencies(program, None)
                .ok()?,
            time: solve::StructuralPattern::derive_output_time_dependencies(program, None).ok()?,
            seed: solve::StructuralPattern::derive_output_seed_dependencies(program, None).ok()?,
            effect: solve::StructuralPattern::derive_output_effect_dependencies(program, None)
                .ok()?,
        })
    }

    fn is_parameter_static(
        &self,
        target_index: usize,
        state_count: usize,
        static_targets: &BTreeSet<usize>,
        continuous_static_parameters: ContinuousStaticParameters,
    ) -> bool {
        let parameters_are_static = self
            .parameters
            .iter()
            .flatten()
            .all(|index| continuous_static_parameters.contains(*index));
        let solver_values_are_static = self.y.iter().flatten().all(|index| {
            parameter_static_y_index(*index, target_index, state_count, static_targets)
        });
        parameters_are_static
            && solver_values_are_static
            && self.time.iter().all(|dependency| !dependency)
            && self.seed.iter().all(|dependency| !dependency)
            && self.effect.iter().all(|dependency| !dependency)
    }
}

#[cfg(test)]
fn parameter_static_refresh_program(
    program: &[solve::LinearOp],
    target_index: usize,
    state_count: usize,
    static_targets: &BTreeSet<usize>,
    continuous_static_parameters: ContinuousStaticParameters,
) -> bool {
    ParameterStaticDependencies::derive(program).is_some_and(|dependencies| {
        dependencies.is_parameter_static(
            target_index,
            state_count,
            static_targets,
            continuous_static_parameters,
        )
    })
}

fn parameter_static_y_index(
    index: usize,
    target_index: usize,
    state_count: usize,
    static_targets: &BTreeSet<usize>,
) -> bool {
    index == target_index || (index >= state_count && static_targets.contains(&index))
}

fn block_is_exactly_seeded(
    block: &solve::AlgebraicProjectionBlock,
    seed_rows: RefreshRows<'_>,
) -> bool {
    block.rows.len() == 1
        && block.y_indices.len() == 1
        && block
            .rows
            .iter()
            .zip(&block.y_indices)
            .all(|(&row, &target)| {
                seed_rows.iter().any(|seed| {
                    seed.equation_index() == row
                        && seed.target_index() == target
                        && seed.assignment_target() == Some(target)
                        && seed.exact_assignment_certified()
                })
            })
}

fn dependency_causal_projection_is_certified(
    rows: &[AlgebraicRefreshRow],
    needed: &IndexSet<usize>,
    plan: &solve::AlgebraicProjectionPlan,
    state_count: usize,
    solver_count: usize,
) -> bool {
    if rows.len() != needed.len() {
        return false;
    }
    let rows_by_target = rows
        .iter()
        .map(|row| (row.target_index(), row))
        .collect::<IndexMap<_, _>>();
    if rows_by_target.len() != needed.len()
        || rows.iter().any(|row| {
            row.target_index() < state_count
                || row.target_index() >= solver_count
                || row.assignment_target() != Some(row.target_index())
                || !row.exact_assignment_certified()
        })
    {
        return false;
    }
    let mut matched = IndexSet::new();
    for projection_block in &plan.blocks {
        let ([equation_index], [target_index]) = (
            projection_block.rows.as_slice(),
            projection_block.y_indices.as_slice(),
        ) else {
            return false;
        };
        let Some(row) = rows_by_target.get(target_index) else {
            return false;
        };
        if row.equation_index() != *equation_index
            || !needed.contains(target_index)
            || !matched.insert(*target_index)
        {
            return false;
        }
    }
    matched.len() == needed.len()
}

trait RefreshProgramAccess {
    fn program(&self, index: usize) -> Option<&[solve::LinearOp]>;
    fn source(&self, index: usize) -> Option<solve::RefreshScalarProgramSource>;
    fn source_index(&self, source: solve::RefreshScalarProgramSource) -> Option<usize>;
    fn program_span(&self, index: usize) -> Option<rumoca_core::Span>;
    fn first_span(&self) -> Option<rumoca_core::Span>;

    fn source_program(
        &self,
        source: solve::RefreshScalarProgramSource,
    ) -> Option<&[solve::LinearOp]> {
        self.source_index(source)
            .and_then(|index| self.program(index))
    }

    fn source_span(&self, source: solve::RefreshScalarProgramSource) -> Option<rumoca_core::Span> {
        self.source_index(source)
            .and_then(|index| self.program_span(index))
    }
}

impl RefreshProgramAccess for solve::ScalarProgramBlock {
    fn program(&self, index: usize) -> Option<&[solve::LinearOp]> {
        self.programs().get(index).map(Vec::as_slice)
    }

    fn source(&self, index: usize) -> Option<solve::RefreshScalarProgramSource> {
        solve::RefreshScalarProgramSource::checked(0, index)
    }

    fn source_index(&self, source: solve::RefreshScalarProgramSource) -> Option<usize> {
        (source.node() == 0)
            .then(|| usize::try_from(source.program()).ok())
            .flatten()
            .filter(|index| *index < self.programs().len())
    }

    fn program_span(&self, index: usize) -> Option<rumoca_core::Span> {
        self.program_span(index)
    }

    fn first_span(&self) -> Option<rumoca_core::Span> {
        first_block_span(self)
    }
}

impl RefreshProgramAccess for CanonicalScalarProgramCatalog<'_> {
    fn program(&self, index: usize) -> Option<&[solve::LinearOp]> {
        self.program(index).map(|program| program.operations)
    }

    fn source(&self, index: usize) -> Option<solve::RefreshScalarProgramSource> {
        self.program(index).map(|program| program.source)
    }

    fn source_index(&self, source: solve::RefreshScalarProgramSource) -> Option<usize> {
        CanonicalScalarProgramCatalog::source_index(self, source)
    }

    fn program_span(&self, index: usize) -> Option<rumoca_core::Span> {
        self.program(index).map(|program| program.span)
    }

    fn first_span(&self) -> Option<rumoca_core::Span> {
        self.first_span()
    }
}

fn order_refresh_rows<A: RefreshProgramAccess + ?Sized>(
    rows: Vec<AlgebraicRefreshRow>,
    block: &A,
    state_count: usize,
    causal_solution_certified: bool,
) -> Result<RefreshPlan, EvalSolveError> {
    let span = block.first_span();
    let mut producer_by_target = BTreeMap::new();
    for (pos, row) in rows.iter().enumerate() {
        producer_by_target.insert(row.target_index(), pos);
    }
    let mut edges = Vec::new();
    reserve_refresh_vec_capacity(&mut edges, rows.len(), "refresh order edges", span)?;
    edges.resize_with(rows.len(), Vec::new);
    let mut indegree = Vec::new();
    reserve_refresh_vec_capacity(&mut indegree, rows.len(), "refresh order indegree", span)?;
    indegree.resize(rows.len(), 0usize);
    for (row_pos, row) in rows.iter().enumerate() {
        let Some(ops) = block.source_program(row.source()) else {
            continue;
        };
        for dep_pos in refresh_row_dependency_positions(row, ops, state_count, &producer_by_target)
        {
            if dep_pos == row_pos || edges[dep_pos].contains(&row_pos) {
                continue;
            }
            reserve_refresh_vec_capacity(
                &mut edges[dep_pos],
                1,
                "refresh order edge list",
                block.source_span(row.source()).or(span),
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
    tracing::debug!(
        target: "rumoca_eval_solve::refresh",
        candidate = causal_solution_certified,
        ordered = ordered.len(),
        rows = rows.len(),
        "refresh causal ordering result"
    );
    let causal_solution_certified = causal_solution_certified && ordered.len() == rows.len();
    if !causal_solution_certified {
        let mut emitted = Vec::new();
        reserve_refresh_vec_capacity(&mut emitted, rows.len(), "refresh emitted flags", span)?;
        emitted.resize(rows.len(), false);
        for row in &ordered {
            if let Some(pos) = rows
                .iter()
                .position(|candidate| candidate.owner_id() == row.owner_id())
            {
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
        static_causal_sequence: Default::default(),
        dynamic_causal_sequence: Default::default(),
        simultaneous_plan: solve::AlgebraicProjectionPlan::default(),
        simultaneous_block_indices: Vec::new(),
        value_projection_plan: solve::AlgebraicProjectionPlan::default(),
        causal_seed_rows: RefreshRowSelection::all(ordered.len()).map_err(|error| {
            EvalSolveError::InvalidRow {
                message: error.to_string(),
                span,
            }
        })?,
        static_causal_seed_rows: RefreshRowSelection::default(),
        dynamic_causal_seed_rows: RefreshRowSelection::default(),
        value_stages: Vec::new(),
        rows: ordered,
        causal_solution_certified,
    })
}

fn refresh_row_dependency_positions(
    row: &AlgebraicRefreshRow,
    ops: &[solve::LinearOp],
    state_count: usize,
    producer_by_target: &BTreeMap<usize, usize>,
) -> Vec<usize> {
    let mut positions = BTreeSet::new();
    for mut range in row_y_input_ranges(ops) {
        range.start = range.start.max(state_count);
        if range.is_empty() {
            continue;
        }
        for (&index, &position) in producer_by_target.range(range) {
            if index == row.target_index()
                || row
                    .assignment_shape()
                    .is_some_and(|shape| !assignment_shape_reads_y_index(ops, shape, index))
            {
                continue;
            }
            positions.insert(position);
        }
    }
    positions.into_iter().collect()
}

fn complete_causal_projection_is_certified<A: RefreshProgramAccess + ?Sized>(
    problem: &solve::SolveProblem,
    block: &A,
    rows: &[AlgebraicRefreshRow],
) -> bool {
    let state_count = problem.solve_layout.state_scalar_count();
    let solver_count = problem.solve_layout.solver_scalar_count();
    // The projection tail contains both algebraic variables and computed
    // outputs; all are solver-Y unknowns in the compiler-owned BLT plan.
    let Some(projection_count) = solver_count.checked_sub(state_count) else {
        return false;
    };
    if rows.len() != projection_count {
        tracing::debug!(target: "rumoca_eval_solve::refresh", reason = "row count", rows = rows.len(), projection_count, "causal certificate rejected");
        return false;
    }
    let rows_by_equation = rows
        .iter()
        .map(|row| (row.equation_index(), row))
        .collect::<IndexMap<_, _>>();
    if rows_by_equation.len() != projection_count {
        tracing::debug!(target: "rumoca_eval_solve::refresh", reason = "duplicate equation", equations = rows_by_equation.len(), projection_count, "causal certificate rejected");
        return false;
    }
    let implicit_row_targets = &problem.continuous.implicit_row_targets;
    if let Some(row) = rows.iter().find(|row| {
        implicit_row_targets
            .get(row.equation_index())
            .is_none_or(|target| {
                !matches!(target, Some(solve::ScalarSlot::Y { index, .. }) if *index == row.target_index())
            })
            || row.target_index() < state_count
            || row.target_index() >= solver_count
            || block.source_program(row.source()).is_none_or(|program| {
                row_y_input_ranges(program)
                    .into_iter()
                    .flatten()
                    .any(|index| index >= solver_count)
                    || !crate::prepared::program_certifies_exact_target(
                        program,
                        row.output_offset(),
                        row.target_index(),
                    )
                    .unwrap_or(false)
            })
    }) {
        tracing::debug!(target: "rumoca_eval_solve::refresh", reason = "invalid row", equation = row.equation_index(), source_node = row.source().node(), source_program = row.source().program(), target = row.target_index(), "causal certificate rejected");
        return false;
    }
    let mut matched_rows = IndexSet::new();
    let mut matched_targets = IndexSet::new();
    for projection_block in &problem.continuous.algebraic_projection_plan.blocks {
        let ([equation_index], [target_index]) = (
            projection_block.rows.as_slice(),
            projection_block.y_indices.as_slice(),
        ) else {
            tracing::debug!(target: "rumoca_eval_solve::refresh", reason = "non-singleton projection", "causal certificate rejected");
            return false;
        };
        if *target_index < state_count
            || *target_index >= solver_count
            || implicit_row_targets
                .get(*equation_index)
                .is_none_or(|target| {
                    !matches!(target, Some(solve::ScalarSlot::Y { index, .. }) if index == target_index)
                })
            || rows_by_equation
                .get(equation_index)
                .is_none_or(|row| row.target_index() != *target_index)
            || !matched_rows.insert(*equation_index)
            || !matched_targets.insert(*target_index)
        {
            tracing::debug!(target: "rumoca_eval_solve::refresh", reason = "projection mismatch", equation = *equation_index, target = *target_index, state_count, solver_count, mapped = rows_by_equation.get(equation_index).map(|row| row.target_index()), "causal certificate rejected");
            return false;
        }
    }
    let certified = matched_rows.len() == projection_count
        && matched_targets.len() == projection_count
        && rows_by_equation
            .keys()
            .all(|equation_index| matched_rows.contains(equation_index))
        && (state_count..solver_count).all(|index| matched_targets.contains(&index));
    if !certified {
        tracing::debug!(target: "rumoca_eval_solve::refresh", reason = "incomplete coverage", matched_rows = matched_rows.len(), matched_targets = matched_targets.len(), projection_count, "causal certificate rejected");
    }
    certified
}

fn derivative_row_dependencies(
    block: &solve::ScalarProgramBlock,
    state_count: usize,
) -> Result<IndexSet<usize>, EvalSolveError> {
    let mut deps = IndexSet::new();
    reserve_refresh_index_set_capacity(
        &mut deps,
        state_count.min(block.programs().len()),
        "derivative dependency set",
        first_block_span(block),
    )?;
    for row_idx in 0..state_count.min(block.programs().len()) {
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

fn scalar_program_block_dependencies(
    block: &solve::ScalarProgramBlock,
    state_count: usize,
) -> Result<IndexSet<usize>, EvalSolveError> {
    let mut deps = IndexSet::new();
    reserve_refresh_index_set_capacity(
        &mut deps,
        block.programs().len(),
        "scalar block dependency set",
        first_block_span(block),
    )?;
    for row_idx in 0..block.programs().len() {
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

fn row_all_y_dependencies(
    block: &solve::ScalarProgramBlock,
    row_idx: usize,
) -> impl Iterator<Item = usize> + '_ {
    block
        .programs()
        .get(row_idx)
        .into_iter()
        .flat_map(|row| row_y_input_ranges(row).into_iter())
        .flatten()
}

fn first_block_span(block: &solve::ScalarProgramBlock) -> Option<rumoca_core::Span> {
    block.first_source_span()
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
        block.output_indices().len(),
        "output-row position map",
        span,
    )?;
    let mut output_ordinal = 0usize;
    for (program_index, program) in block.programs().iter().enumerate() {
        let output_count = solve::ScalarProgramBlock::program_output_count(program);
        for output_offset in 0..output_count {
            let Some(output_index) = block.output_indices().get(output_ordinal).copied() else {
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
            if let Some(previous) = positions.insert(
                output_index,
                OutputRowPosition {
                    program_index,
                    output_offset,
                },
            ) {
                return Err(EvalSolveError::InvalidRow {
                    message: format!(
                        "duplicate scalar program output row {output_index}: first at program {} output {}, repeated at program {program_index} output {output_offset}",
                        previous.program_index, previous.output_offset
                    ),
                    span: block.program_span(program_index),
                });
            }
        }
    }
    if output_ordinal != block.output_indices().len() {
        return Err(EvalSolveError::InvalidRow {
            message: format!(
                "scalar program block has {} output indices but {output_ordinal} StoreOutput ops",
                block.output_indices().len()
            ),
            span,
        });
    }
    Ok(positions)
}
