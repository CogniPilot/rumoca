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
mod chart;
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
/// with alternate reduced charts (the first-integral mirrors the linked kernel
/// selects per block) and a retained state-manifold projection (the linked
/// kernel projects accepted states onto lower-order constraints). A model
/// carrying either is refused rather than integrated on one chart or without
/// the manifold correction. A reduced chart set with executable alternates is
/// executed: the generated kernel switches among its charts exactly as the
/// linked kernel does (SPEC_0040 STRUCT-T07 constraint-fold chart rows).
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

/// Whether every dispatch of one chart's refresh plans is executable.
fn refresh_plans_admissible(problem: &solve::SolveProblem) -> bool {
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

/// Solve-only admissibility of the staged ME refresh for a target that
/// executes algebraic projection stages: the primary's plans and those of
/// every executable alternate chart.
#[must_use]
pub fn me_refresh_admissible(problem: &solve::SolveProblem) -> bool {
    if problem.validate().is_err() {
        return false;
    }
    if unsupported_runtime_shape(problem).is_some() {
        return false;
    }
    refresh_plans_admissible(problem)
        && problem
            .continuous
            .reduced_chart_set
            .charts
            .iter()
            .filter_map(|chart| chart.plan.as_ref())
            .all(|plan| refresh_plans_admissible(&chart::alternate_problem(problem, plan)))
}

/// Canonical blocks of one chart referenced by any step or complete-plan
/// fallback, each described once over the shared program table.
struct BlockCatalog<'a> {
    sources: BlockSources<'a>,
    table: &'a mut ProgramTable,
    ids: BTreeMap<usize, usize>,
    records: &'a mut Vec<BlockRecord>,
}

impl BlockCatalog<'_> {
    fn intern(&mut self, canonical: usize) -> Result<usize, CodegenError> {
        if let Some(&id) = self.ids.get(&canonical) {
            return Ok(id);
        }
        let record = block_record(&self.sources, self.table, canonical)?;
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
                let (first, count) = assignments
                    .schedule((catalog.sources.chart, catalog.sources.problem), schedule)?;
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
                        assignments
                            .schedule((catalog.sources.chart, catalog.sources.problem), schedule)?
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

/// One chart's staged refresh plans (derivative, then algebraic), with the
/// algebraic plan's coverage of every algebraic coordinate proved.
fn chart_plans<'a>(
    problem: &'a solve::SolveProblem,
    artifacts: &solve::SolveArtifacts,
) -> Result<Vec<(&'a solve::RefreshPlan, Vec<solve::StagedRefreshStep<'a>>)>, CodegenError> {
    let owners = &problem.continuous.refresh_owners;
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
    Ok(plans)
}

/// The implicit-residual block one chart's projection blocks read.
fn prepared_implicit(
    problem: &solve::SolveProblem,
) -> Result<PreparedScalarProgramBlock, CodegenError> {
    PreparedScalarProgramBlock::new(
        to_scalar_program_projection(&problem.continuous.implicit_rhs)?.into_block(),
    )
    .map_err(|error| CodegenError::template(error.to_string()))
}

/// The derivative kernel of an alternate chart, which the component evaluates
/// in place of the primary's while that chart is active.
fn alternate_derivative(problem: &solve::SolveProblem) -> Result<Value, CodegenError> {
    let scalar = rumoca_eval_solve::to_scalar_program_block(&problem.continuous.derivative_rhs)?;
    if problem.uses_linear_solve_component()
        || super::scalar_program_block_uses_linear_solve_component(&scalar)
    {
        return Err(CodegenError::dae_preparation_failed(
            "built-in FMI templates do not implement tensor linear-solve components",
            None,
        ));
    }
    Ok(Value::from_object(
        super::scalar_program_plan::ScalarProgramPlan::new(std::sync::Arc::new(scalar))?,
    ))
}

/// Shared tables every chart's view is interned into.
struct ComponentTables {
    table: ProgramTable,
    assignments: AssignmentCatalog,
    records: Vec<BlockRecord>,
}

/// The step tables, blocks, and (for an alternate) derivative kernel of one
/// chart. With `seed_blocks`, the blocks of the chart's complete algebraic
/// plan, the ones the settled initialization tangent linearizes, are interned
/// and recorded there as (block id, canonical block) in plan order.
fn chart_value(
    system: &chart::ChartSystem<'_>,
    (chart, implicit, seed_len): (usize, &PreparedScalarProgramBlock, usize),
    tables: &mut ComponentTables,
    seed_blocks: Option<&mut Vec<(usize, usize)>>,
) -> Result<Value, CodegenError> {
    let plans = chart_plans(&system.problem, &system.artifacts)?;
    let tangent_jvp = rumoca_eval_solve::to_scalar_program_block(
        &system.artifacts.continuous.implicit_jacobian_v,
    )?;
    let mut catalog = BlockCatalog {
        sources: BlockSources {
            problem: &system.problem,
            artifacts: &system.artifacts,
            implicit,
            seed_len,
            chart,
            tangent_jvp: &tangent_jvp,
            linearize_all: chart == 0 && seed_blocks.is_some(),
        },
        table: &mut tables.table,
        ids: BTreeMap::new(),
        records: &mut tables.records,
    };
    let assignments = &mut tables.assignments;
    let derivative = plan_value(plans[0].0, &plans[0].1, &mut catalog, assignments)?;
    let algebraic = plan_value(plans[1].0, &plans[1].1, &mut catalog, assignments)?;
    if let Some(seed_blocks) = seed_blocks {
        for &canonical in &plans[1].0.simultaneous_block_indices {
            seed_blocks.push((catalog.intern(canonical)?, canonical));
        }
    }
    let rhs = if chart == 0 {
        None
    } else {
        Some(alternate_derivative(&system.problem)?)
    };
    Ok(minijinja::context! {
        derivative => derivative,
        algebraic => algebraic,
        rhs => rhs,
    })
}

/// The switching descriptors of every chart, in chart order.
fn chart_records(
    component: &solve::fmi::FmiCCodegenView,
    systems: &[chart::ChartSystem<'_>],
    (implicits, seed_len): (&[PreparedScalarProgramBlock], usize),
    table: &mut ProgramTable,
) -> Result<Vec<chart::ChartRecord>, CodegenError> {
    let problem = component.problem();
    let (y, p) = component.instantiation_point();
    let point = chart::BindingPoint {
        y,
        p,
        pure_calls: component.pure_calls(),
        state_count: problem.solve_layout.state_scalar_count(),
        seed_len,
    };
    let mut records = systems
        .iter()
        .zip(implicits)
        .enumerate()
        .map(|(chart, (system, implicit))| {
            let sources = chart::ChartSources {
                chart,
                system,
                implicit,
                set: &problem.continuous.reduced_chart_set,
            };
            chart::chart_record(&sources, &point, table)
        })
        .collect::<Result<Vec<_>, _>>()?;
    chart::assign_nominals(&mut records, |index| component.solver_variable_scale(index));
    Ok(records)
}

/// The complete ME refresh view of one checked FMI C component.
pub(super) fn me_refresh_value(
    component: &solve::fmi::FmiCCodegenView,
) -> Result<Value, CodegenError> {
    let problem = component.problem();
    let artifacts = component.artifacts();
    require_settleable_bindings(component)?;
    if let Some(reason) = unsupported_runtime_shape(problem) {
        return Err(CodegenError::dae_preparation_failed(
            format!("unsupported-feature:algebraic_projection: {reason}"),
            None,
        ));
    }
    let systems = chart::chart_systems(problem, artifacts);
    let switching = systems.len() > 1;
    let implicits = systems
        .iter()
        .map(|system| prepared_implicit(&system.problem))
        .collect::<Result<Vec<_>, _>>()?;
    let y_len = problem.solve_layout.solver_scalar_count();
    let seed_len = y_len + problem.layout.p_scalars();
    let mut tables = ComponentTables {
        table: ProgramTable::default(),
        assignments: AssignmentCatalog::default(),
        records: Vec::new(),
    };
    if switching {
        tables.table.share_contents();
        tables.assignments.share_contents();
    }
    // A settled initialization linearizes the primary chart's complete
    // algebraic plan.
    let settled_init = problem
        .initialization
        .residual()
        .len()
        .is_ok_and(|rows| rows > 0);
    let mut seed_blocks = Vec::new();
    let charts = systems
        .iter()
        .zip(&implicits)
        .enumerate()
        .map(|(chart, (system, implicit))| {
            let seeds = (chart == 0 && settled_init).then_some(&mut seed_blocks);
            chart_value(system, (chart, implicit, seed_len), &mut tables, seeds)
        })
        .collect::<Result<Vec<_>, _>>()?;
    let switch_records = if switching {
        chart_records(
            component,
            &systems,
            (&implicits, seed_len),
            &mut tables.table,
        )?
    } else {
        Vec::new()
    };
    let variable_scales = (0..y_len)
        .map(|index| float_literal(component.solver_variable_scale(index)))
        .collect::<Vec<_>>();
    let ComponentTables {
        mut table,
        assignments,
        records,
    } = tables;
    let lane_max = records.iter().map(BlockRecord::lane_max).max().unwrap_or(0);
    let (block_doubles, block_sizes) = records
        .iter()
        .map(|record| record.workspace(seed_len))
        .fold((1, 1), |(doubles, sizes), (d, s)| {
            (doubles.max(d), sizes.max(s))
        });
    let chart_doubles = switch_records
        .iter()
        .map(chart::ChartRecord::workspace)
        .max()
        .unwrap_or(0);
    let (init, init_doubles) =
        initial::initialization_value(problem, artifacts, (&mut table, &seed_blocks, seed_len))?;
    let init_sizes = init.get_attr("sizes")?.as_usize().unwrap_or(0);
    let kernel = !records.is_empty() || init_doubles > 0;
    let table = table.into_value(&implicits)?;
    // The initialization frames hold the algebraic refresh, whose deepest
    // block frame and isolator outputs nest inside them; a chart
    // conditioning or transfer frame holds no other.
    let doubles =
        (block_doubles + table.get_attr("iso_max_outputs")?.as_usize().unwrap_or(1) + init_doubles)
            .max(chart_doubles);
    let sizes = block_sizes + init_sizes;
    Ok(minijinja::context! {
        derivative => charts[0].get_attr("derivative")?,
        algebraic => charts[0].get_attr("algebraic")?,
        charts => charts,
        switching => switching.then(|| Value::from_serialize(&switch_records)),
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
        chart_regular_multiple => float_literal(policy::CHART_REGULAR_MULTIPLE),
        chart_switch_keep => float_literal(policy::CHART_SWITCH_KEEP),
        chart_switch_improvement => float_literal(policy::CHART_SWITCH_IMPROVEMENT),
    }
}
