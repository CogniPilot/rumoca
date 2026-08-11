//! Algebraic/derivative/root refresh-plan construction.
//!
//! The plans built here are consumed by `rumoca_solver::runtime::solve_runtime`
//! (the runtime state machine) and by this crate's prepared-block batching. The
//! module is `pub` only for that cross-crate consumer; nothing outside
//! `rumoca-solver`'s runtime should construct or mutate a [`RefreshPlan`].

mod schedule;

use std::{
    collections::{BTreeMap, BTreeSet, VecDeque},
    sync::Arc,
};

use indexmap::{IndexMap, IndexSet};
use rumoca_ir_solve as solve;

use crate::prepared::{assignment_shape_reads_y_index, row_y_input_ranges};
use crate::sparsity::program_output_y_dependencies;
use crate::{EvalSolveError, PreparedScalarProgramBlock, TargetAssignmentShape};

pub use schedule::{RefreshStage, build_refresh_stages};

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
                .get(row.target_index)
                .map_or("<unnamed>", String::as_str);
            format!("{target}@row{}", row.row_idx)
        })
        .collect::<Vec<_>>()
        .join(", ");
    let dynamic_preview = plan
        .dynamic_causal_seed_rows
        .iter()
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
        .filter(|row| row.direct_assignment_certified)
        .count();
    let exact_rows = plan
        .rows
        .iter()
        .filter(|row| row.exact_assignment_certified)
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

#[derive(Clone)]
pub struct AlgebraicRefreshRow {
    /// Logical equation/output index in the canonical implicit system.
    pub equation_index: usize,
    /// ScalarProgramBlock program index that produces this refresh row.
    pub row_idx: usize,
    /// Output offset inside `row_idx`. Shared Solve programs may store multiple
    /// row outputs while still evaluating one source program.
    pub output_offset: usize,
    /// Solver-Y slot this plan entry updates.
    pub target_index: usize,
    /// The row's own implicit assignment target, when the lowering placed it
    /// as `target = expr`. When this differs from `target_index` the runtime
    /// must linear-solve the row's residual for the paired variable instead
    /// of evaluating the assignment value.
    pub assignment_target: Option<usize>,
    /// Prepared isolator selected for this exact target during construction.
    /// Runtime execution consumes this certificate directly instead of
    /// searching the row's other algebraically valid isolators.
    pub assignment_shape: Option<TargetAssignmentShape>,
    /// The prepared row proves a direct target assignment, allowing an
    /// acyclic dependency-complete schedule to skip simultaneous projection.
    pub direct_assignment_certified: bool,
    /// The row has an exact direct or affine target assignment. This can omit
    /// a seeded singleton from value rechecking. A complete acyclic inventory
    /// of exact assignments also certifies the whole value solution; sensitivity
    /// projection continues to retain the compiler-owned simultaneous plan.
    pub exact_assignment_certified: bool,
}

#[derive(Clone, Default)]
pub struct RefreshPlan {
    pub source_block: Arc<solve::ScalarProgramBlock>,
    /// Complete compiler-owned BLT used to certify and solve the canonical
    /// implicit residual system after the causal seed schedule.
    pub simultaneous_plan: solve::AlgebraicProjectionPlan,
    /// Stable indices into the complete compiler-owned BLT. Subplans retain
    /// artifact identity instead of renumbering blocks from zero.
    pub simultaneous_block_indices: Vec<usize>,
    /// Residual blocks that still require value settling after exact causal
    /// seed rows run. Sensitivity projection retains `simultaneous_plan`.
    pub value_projection_plan: solve::AlgebraicProjectionPlan,
    pub rows: Vec<AlgebraicRefreshRow>,
    /// Causal seed rows that can initialize algebraic values before the
    /// remaining simultaneous projection blocks are settled.
    pub causal_seed_rows: Vec<AlgebraicRefreshRow>,
    /// Causal rows proven to depend only on the parameter vector and other
    /// rows in this set. Runtime may reuse them while that complete snapshot
    /// is unchanged.
    pub static_causal_seed_rows: Vec<AlgebraicRefreshRow>,
    /// Causal rows that can depend on time, state, runtime operations, or
    /// dynamic algebraics and therefore execute on every refresh.
    pub dynamic_causal_seed_rows: Vec<AlgebraicRefreshRow>,
    /// Proof-directed value order. The complete causal warm-start precedes
    /// the numerical projection blocks whose residuals still require settling.
    pub value_stages: Vec<RefreshStage>,
    pub causal_solution_certified: bool,
}

impl RefreshPlan {
    pub fn source_block(&self) -> &solve::ScalarProgramBlock {
        &self.source_block
    }

    /// Derive the value stages that remain after `settled` has completed at
    /// the identical coordinate. Shared exact assignments and atomic
    /// projection blocks are construction-identical; uncovered stages retain
    /// this plan's checked BLT order.
    pub fn certified_value_remainder_after(&self, settled: &Self) -> Option<Self> {
        if !Arc::ptr_eq(&self.source_block, &settled.source_block) {
            return None;
        }
        let settled = refresh_stage_identities(&settled.value_stages);
        let value_stages = self
            .value_stages
            .iter()
            .filter_map(|stage| uncovered_refresh_stage(stage, &settled))
            .collect();
        let mut remainder = self.clone();
        remainder.value_stages = value_stages;
        Some(remainder)
    }
}

#[derive(PartialEq)]
enum RefreshStageIdentity {
    ExactAssignment {
        equation_index: usize,
        row_idx: usize,
        output_offset: usize,
        target_index: usize,
        assignment_target: Option<usize>,
        assignment_shape: Option<TargetAssignmentShape>,
    },
    ProjectionBlock {
        block_index: usize,
        blocks: Vec<(Vec<usize>, Vec<usize>)>,
    },
}

fn refresh_stage_identities(stages: &[RefreshStage]) -> Vec<RefreshStageIdentity> {
    let mut identities = Vec::new();
    for stage in stages {
        match stage {
            RefreshStage::CausalSeedSweep {
                static_rows,
                dynamic_rows,
            }
            | RefreshStage::ExactAssignments {
                static_rows,
                dynamic_rows,
            } => identities.extend(
                static_rows
                    .iter()
                    .chain(dynamic_rows.iter())
                    .map(refresh_row_identity),
            ),
            RefreshStage::ProjectionBlock {
                block_index, plan, ..
            } => {
                identities.push(projection_stage_identity(*block_index, plan));
            }
        }
    }
    identities
}

fn uncovered_refresh_rows(
    rows: &[AlgebraicRefreshRow],
    settled: &[RefreshStageIdentity],
) -> Vec<AlgebraicRefreshRow> {
    rows.iter()
        .filter(|row| !settled.contains(&refresh_row_identity(row)))
        .cloned()
        .collect()
}

fn uncovered_refresh_stage(
    stage: &RefreshStage,
    settled: &[RefreshStageIdentity],
) -> Option<RefreshStage> {
    match stage {
        RefreshStage::CausalSeedSweep {
            static_rows,
            dynamic_rows,
        } => uncovered_causal_seed_stage(static_rows, dynamic_rows, settled),
        RefreshStage::ExactAssignments {
            static_rows,
            dynamic_rows,
        } => uncovered_exact_assignment_stage(static_rows, dynamic_rows, settled),
        RefreshStage::ProjectionBlock {
            block_index, plan, ..
        } => {
            let identity = projection_stage_identity(*block_index, plan);
            (!settled.contains(&identity)).then(|| stage.clone())
        }
    }
}

fn uncovered_exact_assignment_stage(
    static_rows: &[AlgebraicRefreshRow],
    dynamic_rows: &[AlgebraicRefreshRow],
    settled: &[RefreshStageIdentity],
) -> Option<RefreshStage> {
    let static_rows = uncovered_refresh_rows(static_rows, settled);
    let dynamic_rows = uncovered_refresh_rows(dynamic_rows, settled);
    (!static_rows.is_empty() || !dynamic_rows.is_empty()).then(|| RefreshStage::ExactAssignments {
        static_rows: static_rows.into_boxed_slice(),
        dynamic_rows: dynamic_rows.into_boxed_slice(),
    })
}

fn uncovered_causal_seed_stage(
    static_rows: &[AlgebraicRefreshRow],
    dynamic_rows: &[AlgebraicRefreshRow],
    settled: &[RefreshStageIdentity],
) -> Option<RefreshStage> {
    let static_rows = uncovered_refresh_rows(static_rows, settled);
    let dynamic_rows = uncovered_refresh_rows(dynamic_rows, settled);
    (!static_rows.is_empty() || !dynamic_rows.is_empty()).then(|| RefreshStage::CausalSeedSweep {
        static_rows: static_rows.into_boxed_slice(),
        dynamic_rows: dynamic_rows.into_boxed_slice(),
    })
}

fn projection_stage_identity(
    block_index: usize,
    plan: &solve::AlgebraicProjectionPlan,
) -> RefreshStageIdentity {
    RefreshStageIdentity::ProjectionBlock {
        block_index,
        blocks: plan
            .blocks
            .iter()
            .map(|block| (block.rows.clone(), block.y_indices.clone()))
            .collect(),
    }
}

fn refresh_row_identity(row: &AlgebraicRefreshRow) -> RefreshStageIdentity {
    RefreshStageIdentity::ExactAssignment {
        equation_index: row.equation_index,
        row_idx: row.row_idx,
        output_offset: row.output_offset,
        target_index: row.target_index,
        assignment_target: row.assignment_target,
        assignment_shape: row.assignment_shape,
    }
}

pub fn build_algebraic_refresh_plan(
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
    if trace_algebraic_refresh() {
        let owned = rows
            .iter()
            .map(|row| row.target_index)
            .collect::<BTreeSet<_>>();
        let missing = model
            .problem
            .continuous
            .algebraic_projection_plan
            .blocks
            .iter()
            .flat_map(|block| block.y_indices.iter().copied())
            .filter(|target| !owned.contains(target))
            .filter_map(|target| {
                model
                    .problem
                    .solve_layout
                    .solver_maps
                    .names
                    .get(target)
                    .map(|name| format!("{name}@{target}"))
            })
            .collect::<Vec<_>>();
        tracing::debug!(target: "rumoca_eval_solve::refresh", ?missing, "algebraic targets without exact refresh owners");
    }
    let causal_solution_certified = complete_causal_projection_is_certified(model, block, &rows);
    tracing::debug!(
        target: "rumoca_eval_solve::refresh",
        candidate = causal_solution_certified,
        rows = rows.len(),
        "complete algebraic causal-certificate candidate"
    );
    let mut plan = order_refresh_rows(
        rows,
        Arc::new(block.block().clone()),
        state_count,
        causal_solution_certified,
    )?;
    plan.simultaneous_plan = model.problem.continuous.algebraic_projection_plan.clone();
    plan.simultaneous_block_indices = (0..plan.simultaneous_plan.blocks.len()).collect();
    configure_causal_seed_rows(&mut plan, state_count)?;
    Ok(plan)
}

fn validate_implicit_output_inventory(
    model: &solve::SolveModel,
    block: &solve::ScalarProgramBlock,
) -> Result<(), EvalSolveError> {
    let positions = output_row_positions(block)?;
    let state_count = model.state_scalar_count();
    let solver_count = model.solver_scalar_count();
    let targets = &model.problem.continuous.implicit_row_targets;
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
    for projection in &model.problem.continuous.algebraic_projection_plan.blocks {
        if projection
            .rows
            .iter()
            .all(|row| positions.contains_key(row))
        {
            for &index in &projection.y_indices {
                if index < solver_count {
                    produced[index] = true;
                }
            }
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
        rows.push(AlgebraicRefreshRow {
            equation_index: row_idx,
            row_idx: position.program_index,
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
        });
    }
    Ok(rows)
}

pub fn build_derivative_refresh_plan(
    model: &solve::SolveModel,
    derivative_block: &solve::ScalarProgramBlock,
    implicit_block: &PreparedScalarProgramBlock,
    full_plan: &RefreshPlan,
) -> Result<RefreshPlan, EvalSolveError> {
    let state_count = model.state_scalar_count();
    let initial_deps = derivative_row_dependencies(derivative_block, state_count)?;
    build_dependency_refresh_plan(model, implicit_block, full_plan, initial_deps)
}

pub fn build_root_refresh_plan(
    model: &solve::SolveModel,
    implicit_block: &PreparedScalarProgramBlock,
    full_plan: &RefreshPlan,
) -> Result<RefreshPlan, EvalSolveError> {
    let state_count = model.state_scalar_count();
    let initial_deps =
        root_condition_dependencies(&model.problem.events.root_conditions, state_count)?;
    build_dependency_refresh_plan(model, implicit_block, full_plan, initial_deps)
}

/// Build the algebraic refresh closure required by a collection of scalar
/// consumer blocks.
///
/// This keeps dependency selection on the compact scalar-program graph.  It
/// does not execute or reconstruct unrelated algebraic/output lanes merely
/// because the consumers run at the same runtime boundary.
pub fn build_scalar_dependency_refresh_plan(
    model: &solve::SolveModel,
    implicit_block: &PreparedScalarProgramBlock,
    full_plan: &RefreshPlan,
    consumer_blocks: &[&solve::ScalarProgramBlock],
) -> Result<RefreshPlan, EvalSolveError> {
    build_scalar_dependency_refresh_plan_with_outputs(
        model,
        implicit_block,
        full_plan,
        consumer_blocks,
        &[],
    )
}

/// Build a dependency refresh plan from whole consumer blocks plus selected
/// logical outputs of grouped programs.
pub fn build_scalar_dependency_refresh_plan_with_outputs(
    model: &solve::SolveModel,
    implicit_block: &PreparedScalarProgramBlock,
    full_plan: &RefreshPlan,
    consumer_blocks: &[&solve::ScalarProgramBlock],
    consumer_outputs: &[(&solve::ScalarProgramBlock, &[usize])],
) -> Result<RefreshPlan, EvalSolveError> {
    build_scalar_dependency_refresh_plan_with_outputs_and_programs(
        model,
        implicit_block,
        full_plan,
        consumer_blocks,
        consumer_outputs,
        &[],
    )
}

/// Build a dependency refresh plan while retaining compact multi-output
/// consumer programs that have no scalar output catalog.
pub fn build_scalar_dependency_refresh_plan_with_outputs_and_programs(
    model: &solve::SolveModel,
    implicit_block: &PreparedScalarProgramBlock,
    full_plan: &RefreshPlan,
    consumer_blocks: &[&solve::ScalarProgramBlock],
    consumer_outputs: &[(&solve::ScalarProgramBlock, &[usize])],
    compact_consumers: &[(&[solve::LinearOp], rumoca_core::Span)],
) -> Result<RefreshPlan, EvalSolveError> {
    let state_count = model.state_scalar_count();
    let mut initial_deps = IndexSet::new();
    for block in consumer_blocks {
        let block_deps = scalar_program_block_dependencies(block, state_count)?;
        reserve_refresh_index_set_capacity(
            &mut initial_deps,
            block_deps.len(),
            "consumer dependency set",
            first_block_span(block),
        )?;
        initial_deps.extend(block_deps);
    }
    for &(block, outputs) in consumer_outputs {
        let positions = output_row_positions(block)?;
        let mut dependencies_by_program = BTreeMap::new();
        for &output in outputs {
            let position =
                positions
                    .get(&output)
                    .copied()
                    .ok_or_else(|| EvalSolveError::InvalidRow {
                        message: format!(
                            "selected consumer output {output} has no scalar program producer"
                        ),
                        span: first_block_span(block),
                    })?;
            let row = &block.programs()[position.program_index];
            let output_dependencies = match dependencies_by_program.entry(position.program_index) {
                std::collections::btree_map::Entry::Occupied(entry) => entry.into_mut(),
                std::collections::btree_map::Entry::Vacant(entry) => entry.insert(
                    program_output_y_dependencies(row, block.program_span(position.program_index))?,
                ),
            };
            let dependencies =
                output_dependencies
                    .get(position.output_offset)
                    .ok_or_else(|| EvalSolveError::InvalidRow {
                        message: format!(
                            "selected consumer output {output} has no dependency result"
                        ),
                        span: block.program_span(position.program_index),
                    })?;
            reserve_refresh_index_set_capacity(
                &mut initial_deps,
                dependencies.len(),
                "selected consumer dependency set",
                block.program_span(position.program_index),
            )?;
            initial_deps.extend(
                dependencies
                    .iter()
                    .copied()
                    .filter(|index| *index >= state_count && *index < model.solver_scalar_count()),
            );
        }
    }
    for &(program, span) in compact_consumers {
        for index in row_y_input_ranges(program)
            .into_iter()
            .flatten()
            .filter(|index| *index >= state_count && *index < model.solver_scalar_count())
        {
            reserve_refresh_index_set_capacity(
                &mut initial_deps,
                1,
                "compact consumer dependency set",
                Some(span),
            )?;
            initial_deps.insert(index);
        }
    }
    build_dependency_refresh_plan(model, implicit_block, full_plan, initial_deps)
}

/// Union already dependency-closed refresh plans and rebuild one canonical
/// causal schedule. This is used for coincident typed clocks so shared
/// algebraic prerequisites execute once.
pub fn merge_dependency_refresh_plans(
    model: &solve::SolveModel,
    implicit_block: &PreparedScalarProgramBlock,
    full_plan: &RefreshPlan,
    plans: &[&RefreshPlan],
) -> Result<RefreshPlan, EvalSolveError> {
    let mut initial_deps = IndexSet::new();
    for plan in plans {
        reserve_refresh_index_set_capacity(
            &mut initial_deps,
            plan.rows.len(),
            "merged dependency set",
            first_block_span(full_plan.source_block()),
        )?;
        initial_deps.extend(plan.rows.iter().map(|row| row.target_index));
        for block in &plan.simultaneous_plan.blocks {
            initial_deps.extend(block.y_indices.iter().copied());
        }
    }
    build_dependency_refresh_plan(model, implicit_block, full_plan, initial_deps)
}

fn build_dependency_refresh_plan(
    model: &solve::SolveModel,
    prepared_implicit_block: &PreparedScalarProgramBlock,
    full_plan: &RefreshPlan,
    initial_deps: IndexSet<usize>,
) -> Result<RefreshPlan, EvalSolveError> {
    let implicit_block = full_plan.source_block();
    let state_count = model.state_scalar_count();
    let span = first_block_span(implicit_block);
    let output_positions = output_row_positions(implicit_block)?;
    let target_to_row = dependency_target_rows(full_plan, span)?;
    let block_by_target = dependency_blocks_by_target(model, full_plan, state_count, span)?;
    let (needed, needed_blocks) = collect_dependency_closure(
        full_plan,
        implicit_block,
        initial_deps,
        &target_to_row,
        &block_by_target,
        &output_positions,
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
            .filter(|row| needed.contains(&row.target_index))
            .cloned(),
    );
    append_exact_projection_owners(
        prepared_implicit_block,
        full_plan,
        &needed,
        &needed_blocks,
        &output_positions,
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
        implicit_block,
        &rows,
        &needed,
        &simultaneous_plan,
        state_count,
        model.solver_scalar_count(),
    );
    let mut plan = order_refresh_rows(
        rows,
        full_plan.source_block.clone(),
        state_count,
        causal_solution_certified,
    )?;
    plan.simultaneous_plan = simultaneous_plan;
    plan.simultaneous_block_indices = simultaneous_block_indices;
    configure_causal_seed_rows(&mut plan, state_count)?;
    Ok(plan)
}

fn append_exact_projection_owners(
    block: &PreparedScalarProgramBlock,
    full_plan: &RefreshPlan,
    needed: &IndexSet<usize>,
    needed_blocks: &IndexSet<usize>,
    output_positions: &IndexMap<usize, OutputRowPosition>,
    rows: &mut Vec<AlgebraicRefreshRow>,
) -> Result<(), EvalSolveError> {
    let span = first_block_span(block.block());
    let mut claimed_targets = rows
        .iter()
        .map(|row| row.target_index)
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
        if !block.can_evaluate_target_assignment_output(
            position.program_index,
            position.output_offset,
            *target_index,
        ) || !block.certifies_exact_target_assignment_output(
            position.program_index,
            position.output_offset,
            *target_index,
        ) {
            continue;
        }
        reserve_refresh_vec_capacity(rows, 1, "dependency exact-owner rows", span)?;
        rows.push(AlgebraicRefreshRow {
            equation_index: *equation_index,
            row_idx: position.program_index,
            output_offset: position.output_offset,
            target_index: *target_index,
            assignment_target: Some(*target_index),
            assignment_shape: block.assignment_shape_for_output(
                position.program_index,
                position.output_offset,
                *target_index,
            ),
            direct_assignment_certified: block.certifies_direct_target_assignment(
                position.program_index,
                position.output_offset,
                *target_index,
            ),
            exact_assignment_certified: true,
        });
        claimed_targets.insert(*target_index);
    }
    Ok(())
}

fn dependency_target_rows(
    plan: &RefreshPlan,
    span: Option<rumoca_core::Span>,
) -> Result<IndexMap<usize, usize>, EvalSolveError> {
    let mut target_to_row = IndexMap::new();
    reserve_refresh_index_map_capacity(
        &mut target_to_row,
        plan.rows.len(),
        "dependency target-to-row map",
        span,
    )?;
    target_to_row.extend(plan.rows.iter().map(|row| (row.target_index, row.row_idx)));
    Ok(target_to_row)
}

fn dependency_blocks_by_target(
    model: &solve::SolveModel,
    plan: &RefreshPlan,
    state_count: usize,
    span: Option<rumoca_core::Span>,
) -> Result<IndexMap<usize, usize>, EvalSolveError> {
    let mut block_by_target = IndexMap::new();
    reserve_refresh_index_map_capacity(
        &mut block_by_target,
        model.solver_scalar_count().saturating_sub(state_count),
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

fn collect_dependency_closure(
    plan: &RefreshPlan,
    implicit_block: &solve::ScalarProgramBlock,
    initial_deps: IndexSet<usize>,
    target_to_row: &IndexMap<usize, usize>,
    block_by_target: &IndexMap<usize, usize>,
    output_positions: &IndexMap<usize, OutputRowPosition>,
    state_count: usize,
) -> Result<(IndexSet<usize>, IndexSet<usize>), EvalSolveError> {
    let span = first_block_span(implicit_block);
    let mut needed = IndexSet::new();
    reserve_refresh_index_set_capacity(
        &mut needed,
        initial_deps.len(),
        "dependency needed set",
        span,
    )?;
    let mut stack = initial_deps.into_iter().collect::<Vec<_>>();
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
        } else if let Some(row_index) = target_to_row.get(&index).copied() {
            enqueue_row_dependencies(implicit_block, row_index, state_count, &mut stack, span)?;
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

fn enqueue_projection_block_dependencies(
    plan: &RefreshPlan,
    implicit_block: &solve::ScalarProgramBlock,
    output_positions: &IndexMap<usize, OutputRowPosition>,
    block_index: usize,
    state_count: usize,
    stack: &mut Vec<usize>,
) -> Result<(), EvalSolveError> {
    let span = first_block_span(implicit_block);
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

fn enqueue_row_dependencies(
    block: &solve::ScalarProgramBlock,
    row_index: usize,
    state_count: usize,
    stack: &mut Vec<usize>,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    for dependency in row_all_y_dependencies(block, row_index) {
        if dependency >= state_count {
            reserve_refresh_vec_capacity(stack, 1, "dependency stack", span)?;
            stack.push(dependency);
        }
    }
    Ok(())
}

fn configure_causal_seed_rows(
    plan: &mut RefreshPlan,
    state_count: usize,
) -> Result<(), EvalSolveError> {
    let span = first_block_span(plan.source_block());
    let mut rows = Vec::new();
    reserve_refresh_vec_capacity(&mut rows, plan.rows.len(), "causal seed rows", span)?;
    rows.extend(plan.rows.iter().cloned());
    plan.causal_seed_rows = rows;
    let static_targets = parameter_static_refresh_targets(plan, state_count);
    plan.static_causal_seed_rows = plan
        .causal_seed_rows
        .iter()
        .filter(|row| static_targets.contains(&row.target_index))
        .cloned()
        .collect();
    plan.dynamic_causal_seed_rows = plan
        .causal_seed_rows
        .iter()
        .filter(|row| !static_targets.contains(&row.target_index))
        .cloned()
        .collect();
    plan.value_projection_plan = if plan.causal_solution_certified {
        solve::AlgebraicProjectionPlan {
            blocks: plan
                .simultaneous_plan
                .blocks
                .iter()
                .filter(|block| !block_is_exactly_seeded(block, &plan.causal_seed_rows))
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
        &plan.causal_seed_rows,
        &plan.static_causal_seed_rows,
    );
    Ok(())
}

fn parameter_static_refresh_targets(plan: &RefreshPlan, state_count: usize) -> BTreeSet<usize> {
    let mut static_targets = BTreeSet::new();
    loop {
        let mut changed = false;
        for refresh_row in &plan.causal_seed_rows {
            if static_targets.contains(&refresh_row.target_index) {
                continue;
            }
            let Some(row) = plan.source_block.programs().get(refresh_row.row_idx) else {
                continue;
            };
            if parameter_static_refresh_row(
                row,
                refresh_row.target_index,
                state_count,
                &static_targets,
            ) {
                static_targets.insert(refresh_row.target_index);
                changed = true;
            }
        }
        if !changed {
            return static_targets;
        }
    }
}

fn parameter_static_refresh_row(
    row: &[solve::LinearOp],
    target_index: usize,
    state_count: usize,
    static_targets: &BTreeSet<usize>,
) -> bool {
    parameter_static_refresh_program(row, target_index, state_count, static_targets)
}

// SPEC_0021: Exception - exhaustive classification of every Solve LinearOp
// keeps additions fail-closed at compile time instead of silently extending
// the parameter-static cache certificate.
#[allow(clippy::too_many_lines)]
fn parameter_static_refresh_program(
    program: &[solve::LinearOp],
    target_index: usize,
    state_count: usize,
    static_targets: &BTreeSet<usize>,
) -> bool {
    program.iter().all(|op| match op {
        solve::LinearOp::LoadY { index, .. } => {
            parameter_static_y_index(*index, target_index, state_count, static_targets)
        }
        solve::LinearOp::TensorLoad {
            input: solve::TensorInputKind::Y,
            input_start,
            count,
            ..
        } => input_start.checked_add(*count).is_some_and(|end| {
            (*input_start..end).all(|index| {
                parameter_static_y_index(index, target_index, state_count, static_targets)
            })
        }),
        solve::LinearOp::FunctionFold { program, .. }
        | solve::LinearOp::GuardedFunctionFold { program, .. } => parameter_static_refresh_program(
            &program.update,
            target_index,
            state_count,
            static_targets,
        ),
        solve::LinearOp::FunctionConditional { program, .. } => {
            program.arms.iter().all(|arm| {
                parameter_static_refresh_program(
                    &arm.condition,
                    target_index,
                    state_count,
                    static_targets,
                ) && parameter_static_refresh_program(
                    &arm.result,
                    target_index,
                    state_count,
                    static_targets,
                )
            }) && parameter_static_refresh_program(
                &program.fallback,
                target_index,
                state_count,
                static_targets,
            )
        }
        solve::LinearOp::StoreOutputFunctionFold { program, .. } => {
            parameter_static_refresh_program(
                &program.update,
                target_index,
                state_count,
                static_targets,
            )
        }
        solve::LinearOp::LoadTime { .. }
        | solve::LinearOp::LoadSeed { .. }
        | solve::LinearOp::LoadIndexedSeed { .. }
        | solve::LinearOp::TableBounds { .. }
        | solve::LinearOp::TableLookup { .. }
        | solve::LinearOp::TableLookupSlope { .. }
        | solve::LinearOp::TableNextEvent { .. }
        | solve::LinearOp::ImpureRandomInit { .. }
        | solve::LinearOp::ImpureRandom { .. }
        | solve::LinearOp::ImpureRandomInteger { .. } => false,
        solve::LinearOp::Const { .. }
        | solve::LinearOp::LoadP { .. }
        | solve::LinearOp::LoadIndexedP { .. }
        | solve::LinearOp::LoadIndexedRegister { .. }
        | solve::LinearOp::LoadIndexedFoldCarried { .. }
        | solve::LinearOp::LoadIndexedFoldCapture { .. }
        | solve::LinearOp::LoadFoldCarried { .. }
        | solve::LinearOp::LoadFoldIndex { .. }
        | solve::LinearOp::LoadFoldCapture { .. }
        | solve::LinearOp::LoadFunctionConditionalCapture { .. }
        | solve::LinearOp::LoadFunctionConditionalCaptureRange { .. }
        | solve::LinearOp::Move { .. }
        | solve::LinearOp::LinearSolveComponent { .. }
        | solve::LinearOp::DotProduct { .. }
        | solve::LinearOp::MatrixMultiply { .. }
        | solve::LinearOp::TensorBinary { .. }
        | solve::LinearOp::TensorCross { .. }
        | solve::LinearOp::TensorTranspose { .. }
        | solve::LinearOp::TensorConcatenate { .. }
        | solve::LinearOp::TensorUpdate { .. }
        | solve::LinearOp::TensorFill { .. }
        | solve::LinearOp::TensorIdentity { .. }
        | solve::LinearOp::TensorLoad {
            input: solve::TensorInputKind::P,
            seed_start: None,
            ..
        }
        | solve::LinearOp::RandomInitialState { .. }
        | solve::LinearOp::RandomResult { .. }
        | solve::LinearOp::RandomState { .. }
        | solve::LinearOp::Unary { .. }
        | solve::LinearOp::Binary { .. }
        | solve::LinearOp::Compare { .. }
        | solve::LinearOp::Select { .. }
        | solve::LinearOp::PureCall { .. }
        | solve::LinearOp::StoreOutputFoldTensorUpdate { .. }
        | solve::LinearOp::StoreOutputRange { .. }
        | solve::LinearOp::StoreOutput { .. } => true,
        solve::LinearOp::TensorLoad {
            input: solve::TensorInputKind::P,
            seed_start: Some(_),
            ..
        } => false,
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
    seed_rows: &[AlgebraicRefreshRow],
) -> bool {
    block.rows.len() == 1
        && block.y_indices.len() == 1
        && block
            .rows
            .iter()
            .zip(&block.y_indices)
            .all(|(&row, &target)| {
                seed_rows.iter().any(|seed| {
                    seed.equation_index == row
                        && seed.target_index == target
                        && seed.assignment_target == Some(target)
                        && seed.exact_assignment_certified
                })
            })
}

fn dependency_causal_projection_is_certified(
    _block: &solve::ScalarProgramBlock,
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
        .map(|row| (row.target_index, row))
        .collect::<IndexMap<_, _>>();
    if rows_by_target.len() != needed.len()
        || rows.iter().any(|row| {
            row.target_index < state_count
                || row.target_index >= solver_count
                || row.assignment_target != Some(row.target_index)
                || !row.exact_assignment_certified
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
        if row.equation_index != *equation_index
            || !needed.contains(target_index)
            || !matched.insert(*target_index)
        {
            return false;
        }
    }
    matched.len() == needed.len()
}

fn order_refresh_rows(
    rows: Vec<AlgebraicRefreshRow>,
    block: Arc<solve::ScalarProgramBlock>,
    state_count: usize,
    causal_solution_certified: bool,
) -> Result<RefreshPlan, EvalSolveError> {
    let span = first_block_span(&block);
    let mut producer_by_target = BTreeMap::new();
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
        let Some(ops) = block.programs().get(row.row_idx) else {
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
        simultaneous_block_indices: Vec::new(),
        value_projection_plan: solve::AlgebraicProjectionPlan::default(),
        causal_seed_rows: ordered.clone(),
        static_causal_seed_rows: Vec::new(),
        dynamic_causal_seed_rows: Vec::new(),
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
            if index == row.target_index
                || row
                    .assignment_shape
                    .is_some_and(|shape| !assignment_shape_reads_y_index(ops, shape, index))
            {
                continue;
            }
            positions.insert(position);
        }
    }
    positions.into_iter().collect()
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
        tracing::debug!(target: "rumoca_eval_solve::refresh", reason = "row count", rows = rows.len(), projection_count, "causal certificate rejected");
        return false;
    }
    let rows_by_equation = rows
        .iter()
        .map(|row| (row.equation_index, row))
        .collect::<IndexMap<_, _>>();
    if rows_by_equation.len() != projection_count {
        tracing::debug!(target: "rumoca_eval_solve::refresh", reason = "duplicate equation", equations = rows_by_equation.len(), projection_count, "causal certificate rejected");
        return false;
    }
    let implicit_row_targets = &model.problem.continuous.implicit_row_targets;
    if let Some(row) = rows.iter().find(|row| {
        implicit_row_targets
            .get(row.equation_index)
            .is_none_or(|target| {
                !matches!(target, Some(solve::ScalarSlot::Y { index, .. }) if *index == row.target_index)
            })
            || row.target_index < state_count
            || row.target_index >= solver_count
            || row_all_y_dependencies(block.block(), row.row_idx).any(|index| index >= solver_count)
            || !block.certifies_exact_target_assignment_output(
                row.row_idx,
                row.output_offset,
                row.target_index,
            )
    }) {
        tracing::debug!(target: "rumoca_eval_solve::refresh", reason = "invalid row", equation = row.equation_index, program = row.row_idx, target = row.target_index, exact = block.certifies_exact_target_assignment_output(row.row_idx, row.output_offset, row.target_index), "causal certificate rejected");
        return false;
    }
    let mut matched_rows = IndexSet::new();
    let mut matched_targets = IndexSet::new();
    for projection_block in &model.problem.continuous.algebraic_projection_plan.blocks {
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
                .is_none_or(|row| row.target_index != *target_index)
            || !matched_rows.insert(*equation_index)
            || !matched_targets.insert(*target_index)
        {
            tracing::debug!(target: "rumoca_eval_solve::refresh", reason = "projection mismatch", equation = *equation_index, target = *target_index, state_count, solver_count, mapped = rows_by_equation.get(equation_index).map(|row| row.target_index), "causal certificate rejected");
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

#[cfg(test)]
mod parameter_static_tests {
    use super::*;
    use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};

    fn checked(program: Vec<solve::LinearOp>) -> Vec<solve::LinearOp> {
        solve::ScalarProgramRegisterFlow::derive(&program)
            .expect("parameter-static fixture must be a checked register program");
        program
    }

    fn certifies(program: &[solve::LinearOp]) -> bool {
        parameter_static_refresh_program(program, 10, 2, &BTreeSet::from([11, 12]))
    }

    #[test]
    fn compact_tensor_inputs_preserve_the_parameter_static_certificate() {
        let parameter_tensor = checked(vec![
            solve::LinearOp::TensorLoad {
                dst_start: 0,
                input: solve::TensorInputKind::P,
                input_start: 4,
                count: 3,
                seed_start: None,
                lanes: 1,
            },
            solve::LinearOp::TensorFill {
                dst_start: 3,
                value_start: 0,
                count: 3,
                lanes: 1,
            },
            solve::LinearOp::StoreOutputRange {
                start: 3,
                count: 3,
                stride: 1,
            },
        ]);
        assert!(certifies(&parameter_tensor));

        let certified_y_tensor = checked(vec![
            solve::LinearOp::TensorLoad {
                dst_start: 0,
                input: solve::TensorInputKind::Y,
                input_start: 10,
                count: 3,
                seed_start: None,
                lanes: 1,
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        assert!(certifies(&certified_y_tensor));

        let dynamic_y_tensor = checked(vec![
            solve::LinearOp::TensorLoad {
                dst_start: 0,
                input: solve::TensorInputKind::Y,
                input_start: 10,
                count: 4,
                seed_start: None,
                lanes: 1,
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        assert!(!certifies(&dynamic_y_tensor));
    }

    #[test]
    fn seed_dependent_tensor_load_is_never_parameter_static() {
        let seeded_parameter_tensor = checked(vec![
            solve::LinearOp::TensorLoad {
                dst_start: 0,
                input: solve::TensorInputKind::P,
                input_start: 4,
                count: 1,
                seed_start: Some(0),
                lanes: 2,
            },
            solve::LinearOp::StoreOutput { src: 1 },
        ]);
        assert!(!certifies(&seeded_parameter_tensor));
    }

    #[test]
    fn compact_fold_owner_is_certified_without_domain_expansion() {
        let fold = solve::FunctionFoldProgram::checked(
            StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    id: 0,
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 3,
                    step: 1,
                }],
            },
            1,
            1,
            vec![
                solve::LinearOp::LoadFoldCarried { dst: 0, index: 0 },
                solve::LinearOp::LoadFoldCapture { dst: 1, index: 0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ],
        )
        .expect("compact fold fixture has a checked carried/capture ABI");
        let program = checked(vec![
            solve::LinearOp::LoadP { dst: 0, index: 4 },
            solve::LinearOp::Const { dst: 1, value: 0.0 },
            solve::LinearOp::FunctionFold {
                dst_start: 2,
                initial_start: 1,
                capture_start: 0,
                program: Arc::new(fold),
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]);
        assert!(certifies(&program));
    }

    #[test]
    fn lazy_conditional_regions_are_recursively_fail_closed() {
        let conditional = solve::FunctionConditionalProgram::checked(
            0,
            [1],
            [(
                vec![
                    solve::LinearOp::LoadTime { dst: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    solve::LinearOp::Const { dst: 0, value: 1.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            )],
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        )
        .expect("lazy conditional fixture has checked correlated regions");
        let program = checked(vec![
            solve::LinearOp::FunctionConditional {
                dst_start: 0,
                capture_start: 0,
                program: Arc::new(conditional),
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        assert!(!certifies(&program));
    }
}
