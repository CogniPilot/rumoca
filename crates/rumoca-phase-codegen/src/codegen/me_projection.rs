//! Checked C execution view of the Model-Exchange algebraic refresh.
//!
//! Generated FMI 2/3 components settle their algebraic coordinates with the
//! same refresh plans and the same block projection algorithms as the linked
//! ME kernel (`rumoca-solver` `runtime::projection`). This module reads each
//! construction-issued refresh plan once, through
//! [`solve::ContinuousRefreshOwners::staged_refresh_steps`], and exposes every
//! step with the executable programs the shared C kernel
//! (`templates/fmi3/me_projection.jinja`) consumes, together with the one
//! numerical policy (`rumoca_eval_solve::projection_policy`) both executors
//! read. It derives execution views only; it never records canonical IR.

mod assign;
mod block;
mod initial;
#[cfg(test)]
mod lane_render_tests;
mod table;
#[cfg(test)]
mod tests;

use std::collections::BTreeMap;

use minijinja::Value;
use rumoca_eval_solve::{
    PreparedScalarProgramBlock, projection_policy as policy, to_scalar_program_projection,
};
use rumoca_ir_solve as solve;

use crate::errors::CodegenError;
use assign::AssignmentCatalog;
use block::{BlockRecord, BlockSources, block_record};
use table::ProgramTable;

fn refusal(label: &str, refusal: solve::StagedRefreshRefusal) -> String {
    let reason = match refusal {
        solve::StagedRefreshRefusal::NonExactStage => {
            "selects rows that no exact-assignment schedule replays"
        }
        solve::StagedRefreshRefusal::IncompleteCoverage => "leaves a coordinate unsettled",
    };
    format!("the {label} refresh {reason}")
}

/// The plans a generated component executes: the derivative refresh ahead of
/// every derivative evaluation and the complete algebraic refresh ahead of
/// every observation, exactly as the linked component uses them.
fn component_plans(
    owners: &solve::ContinuousRefreshOwners,
) -> [(&'static str, &solve::RefreshPlan); 2] {
    [
        ("derivative", owners.derivative()),
        ("algebraic", owners.algebraic()),
    ]
}

/// Whether every algebraic coordinate is settled by the algebraic refresh.
fn algebraic_refresh_covers(
    problem: &solve::SolveProblem,
    plan: &solve::RefreshPlan,
    steps: &[solve::StagedRefreshStep<'_>],
) -> bool {
    let owners = &problem.continuous.refresh_owners;
    let mut settled = std::collections::BTreeSet::new();
    for step in steps {
        match step {
            solve::StagedRefreshStep::ProjectComplete => settled.extend(
                plan.simultaneous_plan
                    .blocks
                    .iter()
                    .flat_map(|block| block.y_indices.iter().copied()),
            ),
            solve::StagedRefreshStep::Assignments(schedule) => {
                let Some(programs) = schedule
                    .program_ids()
                    .iter()
                    .map(|id| owners.exact_assignment_program(*id))
                    .collect::<Option<Vec<_>>>()
                else {
                    return false;
                };
                settled.extend(
                    programs
                        .iter()
                        .flat_map(|program| program.target_indices().iter().copied()),
                );
            }
            solve::StagedRefreshStep::Project { block_index, .. } => {
                let Some(block) = problem
                    .continuous
                    .algebraic_projection_plan
                    .blocks
                    .get(*block_index)
                else {
                    return false;
                };
                settled.extend(block.y_indices.iter().copied());
            }
        }
    }
    let state_count = problem.solve_layout.state_scalar_count();
    let algebraic_count = problem.solve_layout.algebraic_scalar_count();
    (state_count..state_count + algebraic_count).all(|index| settled.contains(&index))
}

/// The runtime projection shapes the shared kernel does not execute: a block
/// with alternate reduced charts (the linked kernel switches charts when the
/// active one folds) and a retained state-manifold projection (the linked
/// kernel projects accepted states onto lower-order constraints). A model
/// carrying either is refused rather than integrated on one chart without the
/// manifold correction.
fn unsupported_runtime_shape(problem: &solve::SolveProblem) -> Option<&'static str> {
    let continuous = &problem.continuous;
    if continuous
        .algebraic_projection_plan
        .blocks
        .iter()
        .any(|block| !block.alternate_charts.is_empty())
    {
        return Some("a projection block carries alternate reduced charts");
    }
    if !continuous.manifold_projection_plan.is_empty() {
        return Some("the model retains a state-manifold projection");
    }
    None
}

/// Solve-only admissibility of the staged ME refresh for a target that
/// executes algebraic projection stages.
#[must_use]
pub fn me_refresh_admissible(problem: &solve::SolveProblem) -> bool {
    if problem.validate().is_err() {
        return false;
    }
    if unsupported_runtime_shape(problem).is_some() {
        return false;
    }
    let owners = &problem.continuous.refresh_owners;
    component_plans(owners).into_iter().all(|(label, plan)| {
        plan.possible_stage_schedules().iter().all(|&schedule| {
            owners
                .staged_refresh_steps(plan, schedule)
                .is_ok_and(|steps| {
                    label != "algebraic" || algebraic_refresh_covers(problem, plan, &steps)
                })
        })
    })
}

/// Canonical blocks referenced by any step or complete-plan fallback, each
/// described once over the shared program table.
struct BlockCatalog<'a> {
    sources: BlockSources<'a>,
    table: ProgramTable,
    ids: BTreeMap<usize, usize>,
    records: Vec<BlockRecord>,
}

impl BlockCatalog<'_> {
    fn intern(&mut self, canonical: usize) -> Result<usize, CodegenError> {
        if let Some(&id) = self.ids.get(&canonical) {
            return Ok(id);
        }
        let record = block_record(&self.sources, &mut self.table, canonical)?;
        let id = self.records.len();
        self.records.push(record);
        self.ids.insert(canonical, id);
        Ok(id)
    }

    /// The pool range of coordinates a failed seed restores before its
    /// stage projects its own block.
    fn rescue_targets(
        &mut self,
        plan: &solve::RefreshPlan,
        seed_rows: &solve::RefreshRowSelection,
        block_index: usize,
    ) -> Result<(usize, usize), CodegenError> {
        let block = self
            .sources
            .problem
            .continuous
            .algebraic_projection_plan
            .blocks
            .get(block_index)
            .ok_or_else(|| {
                block::refuse(block_index, "is outside the canonical projection plan")
            })?;
        let targets = solve::projection_seed_rescue_targets(plan.selected_rows(seed_rows), block);
        let count = targets.len();
        Ok((self.table.push(targets), count))
    }

    /// The complete simultaneous plan a failed seed or exact-assignment stage
    /// falls back to, as pool pairs of (block id, invalidates-earlier flag).
    fn complete_plan(&mut self, plan: &solve::RefreshPlan) -> Result<(usize, usize), CodegenError> {
        let structural = &self.sources.artifacts.continuous.structural;
        let mut entries = Vec::with_capacity(2 * plan.simultaneous_block_indices.len());
        for &canonical in &plan.simultaneous_block_indices {
            let invalidates = structural
                .algebraic_invalidates_earlier(canonical)
                .unwrap_or(true);
            entries.extend([self.intern(canonical)?, usize::from(invalidates)]);
        }
        Ok((
            self.table.push(entries),
            plan.simultaneous_block_indices.len(),
        ))
    }
}

/// Step kinds of the C step table.
const STEP_ASSIGN: usize = 0;
const STEP_COMPLETE: usize = 1;
const STEP_PROJECT: usize = 2;

/// The executable steps of one plan as rows of the C step table (kind,
/// assignment range, block, seed range, rescue range) plus, for a staged plan,
/// the complete simultaneous projection the linked kernel falls back to when a
/// seed or an exact-assignment stage yields a non-finite coordinate.
fn plan_value(
    problem: &solve::SolveProblem,
    plan: &solve::RefreshPlan,
    steps: &[solve::StagedRefreshStep<'_>],
    catalog: &mut BlockCatalog<'_>,
    assignments: &mut AssignmentCatalog,
) -> Result<Value, CodegenError> {
    let mut rows = Vec::with_capacity(steps.len());
    let mut fallback = false;
    for step in steps {
        rows.push(match step {
            solve::StagedRefreshStep::ProjectComplete => [STEP_COMPLETE, 0, 0, 0, 0, 0, 0, 0],
            solve::StagedRefreshStep::Assignments(schedule) => {
                fallback = true;
                let (first, count) = assignments.schedule(problem, schedule)?;
                [STEP_ASSIGN, first, count, 0, 0, 0, 0, 0]
            }
            solve::StagedRefreshStep::Project {
                block_index,
                seeds,
                seed_rows,
            } => {
                let block = catalog.intern(*block_index)?;
                let (rescue, nrescue) = catalog.rescue_targets(plan, seed_rows, *block_index)?;
                let (seed_first, seed_count) = match seeds {
                    Some(schedule) => {
                        fallback = true;
                        assignments.schedule(problem, schedule)?
                    }
                    None => (0, 0),
                };
                [
                    STEP_PROJECT,
                    0,
                    0,
                    block,
                    seed_first,
                    seed_count,
                    rescue,
                    nrescue,
                ]
            }
        });
    }
    let staged = !steps.is_empty() && !plan.causal_solution_certified;
    let complete = if staged {
        Some(catalog.complete_plan(plan)?)
    } else {
        None
    };
    Ok(minijinja::context! {
        steps => rows,
        complete => complete,
        fallback => fallback && complete.is_some(),
    })
}

fn float_literal(value: f64) -> String {
    format!("{value:?}")
}

/// The runtime settles the bindings with at most this many simultaneous sweeps
/// and fails beyond it (`eval_and_apply_update_rows`); a component that would
/// succeed where the linked kernel fails is refused instead.
fn require_settleable_bindings(
    component: &solve::fmi::FmiCCodegenView,
) -> Result<(), CodegenError> {
    let levels = component.parameter_binding_levels();
    if levels < policy::ALGEBRAIC_REFRESH_MAX_ITERS {
        return Ok(());
    }
    Err(CodegenError::dae_preparation_failed(
        format!(
            "unsupported-feature:initialization: parameter bindings form a dependency chain \
             {levels} levels deep; the runtime settles at most {} levels",
            policy::ALGEBRAIC_REFRESH_MAX_ITERS - 1
        ),
        None,
    ))
}

/// The complete ME refresh view of one checked FMI C component.
pub(super) fn me_refresh_value(
    component: &solve::fmi::FmiCCodegenView,
) -> Result<Value, CodegenError> {
    let problem = component.problem();
    let artifacts = component.artifacts();
    require_settleable_bindings(component)?;
    let owners = &problem.continuous.refresh_owners;
    if let Some(reason) = unsupported_runtime_shape(problem) {
        return Err(CodegenError::dae_preparation_failed(
            format!("unsupported-feature:algebraic_projection: {reason}"),
            None,
        ));
    }
    let structural = &artifacts.continuous.structural;
    let mut plans = Vec::with_capacity(2);
    for (label, plan) in component_plans(owners) {
        let steps = owners
            .staged_refresh_steps(plan, plan.stage_schedule(structural))
            .map_err(|error| {
                CodegenError::dae_preparation_failed(
                    format!(
                        "unsupported-feature:algebraic_projection: {}",
                        refusal(label, error)
                    ),
                    None,
                )
            })?;
        plans.push((plan, steps));
    }
    if !algebraic_refresh_covers(problem, plans[1].0, &plans[1].1) {
        return Err(CodegenError::dae_preparation_failed(
            "unsupported-feature:algebraic_projection: the algebraic refresh leaves an \
             algebraic coordinate unsettled",
            None,
        ));
    }
    let implicit = PreparedScalarProgramBlock::new(
        to_scalar_program_projection(&problem.continuous.implicit_rhs)?.into_block(),
    )
    .map_err(|error| CodegenError::template(error.to_string()))?;
    let y_len = problem.solve_layout.solver_scalar_count();
    let seed_len = y_len + problem.layout.p_scalars();
    let mut catalog = BlockCatalog {
        sources: BlockSources {
            problem,
            artifacts,
            implicit: &implicit,
            seed_len,
        },
        table: ProgramTable::default(),
        ids: BTreeMap::new(),
        records: Vec::new(),
    };
    let mut assignments = AssignmentCatalog::default();
    let derivative = plan_value(
        problem,
        plans[0].0,
        &plans[0].1,
        &mut catalog,
        &mut assignments,
    )?;
    let algebraic = plan_value(
        problem,
        plans[1].0,
        &plans[1].1,
        &mut catalog,
        &mut assignments,
    )?;
    let variable_scales = (0..y_len)
        .map(|index| float_literal(component.solver_variable_scale(index)))
        .collect::<Vec<_>>();
    let BlockCatalog { table, records, .. } = catalog;
    let lane_max = records.iter().map(BlockRecord::lane_max).max().unwrap_or(0);
    let (block_doubles, block_sizes) = records
        .iter()
        .map(|record| record.workspace(seed_len))
        .fold((1, 1), |(doubles, sizes), (d, s)| {
            (doubles.max(d), sizes.max(s))
        });
    let (init, init_doubles) = initial::initialization_value(problem, artifacts)?;
    let init_sizes = init.get_attr("sizes")?.as_usize().unwrap_or(0);
    let kernel = !records.is_empty() || init_doubles > 0;
    let table = table.into_value(&implicit)?;
    // The initialization frames hold the algebraic refresh, whose deepest
    // block frame and isolator outputs nest inside them.
    let doubles =
        block_doubles + table.get_attr("iso_max_outputs")?.as_usize().unwrap_or(1) + init_doubles;
    let sizes = block_sizes + init_sizes;
    Ok(minijinja::context! {
        derivative => derivative,
        algebraic => algebraic,
        blocks => Value::from_serialize(&records),
        table => table,
        kernel => kernel,
        assign => assignments.into_value()?,
        init => init,
        work => minijinja::context! { doubles => doubles, sizes => sizes },
        seed_len => seed_len,
        lane_max => lane_max,
        variable_scales => variable_scales,
        policy => policy_value(),
    })
}

/// The one numerical policy both executors read.
fn policy_value() -> Value {
    minijinja::context! {
        tolerance => float_literal(policy::ALGEBRAIC_REFRESH_TOLERANCE),
        refresh_iters => policy::ALGEBRAIC_REFRESH_MAX_ITERS
            * policy::ALGEBRAIC_PROJECTION_ITER_FACTOR,
        refine_iters => policy::ALGEBRAIC_PROJECTION_MAX_ITERS,
        update_iters => policy::ALGEBRAIC_REFRESH_MAX_ITERS,
        trust_fraction => float_literal(policy::ALGEBRAIC_PROJECTION_TRUST_FRACTION),
        torn_iters => policy::TORN_OUTER_MAX_ITERS,
        torn_backtracks => policy::TORN_BACKTRACK_STEPS,
        fd_step => float_literal(policy::FINITE_DIFFERENCE_RELATIVE_STEP),
    }
}
