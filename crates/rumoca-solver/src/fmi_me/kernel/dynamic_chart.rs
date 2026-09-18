//! Runtime state selection over a folding definitional first-integral group
//! (SPEC_0053 section 2a, carrier A).
//!
//! A conserved holonomic norm `g = x*x - 1` has no globally injective reduced
//! chart: a fixed reduced basis reconstructs a dependent coordinate whose
//! gradient `g_d = 2*x_i` vanishes as `x_i` passes through zero. The compiler
//! issues a bounded set of admissible reconstruction charts on the continuous
//! solve system; each alternate carries its complete executable image in the
//! same solver-Y space as the primary basis. This module builds one runtime per
//! chart and precomputes the geometry the completed-step detector and the
//! Event-Mode basis transition need.
//!
//! Every model without such a group carries an empty chart set, so
//! [`build_reduced_charts`] returns `None` and the component behaves exactly as
//! it did before dynamic state selection existed.

use std::rc::Rc;

use rumoca_ir_solve::{AlgebraicProjectionPlan, ComputeBlock, ContinuousSolveSystem, SolveModel};

use crate::runtime::solve_ops::RuntimeSolveError;
use crate::runtime::solve_runtime::SolveRuntime;

/// The compile-time geometry of one admissible reduced chart, resolved into the
/// solver-Y index space shared by every chart of the group.
pub(super) struct KernelChart {
    /// Positions within [`ReducedChartRuntimes::group_cols`] of the solver-Y
    /// columns this chart reconstructs (its dependent coordinates).
    pub(super) dependent_positions: Vec<usize>,
    /// Implicit residual rows of the folding first integral, taken from the
    /// scalar reconstruction block of this chart's dependent coordinate.
    pub(super) folding_rows: Vec<usize>,
    /// Per generated state coordinate, the implicit residual row whose value is
    /// the identity `state - source`. `state[k] - residual[binding_rows[k]]`
    /// recovers this chart's integrated source value for state coordinate `k`.
    pub(super) binding_rows: Vec<usize>,
}

/// Every runtime-executable reduced chart of one continuous system, index zero
/// being the primary basis executed by the enclosing continuous system.
pub(super) struct ReducedChartRuntimes {
    /// Chart index zero is `Rc::clone` of the enclosing runtime; higher indices
    /// are the alternate bases spliced from their carried plans.
    pub(super) runtimes: Vec<Rc<SolveRuntime>>,
    pub(super) charts: Vec<KernelChart>,
    /// The solver-Y columns of the constrained coordinate group, identical for
    /// every chart. The folding stage Jacobian is evaluated over these columns
    /// and each chart selects its dependent subset.
    pub(super) group_cols: Vec<usize>,
}

/// A completed step requested a basis change: the target chart and the last
/// consistent full physical coordinate captured while the outgoing basis was
/// still regular, used to re-seed and branch-limit the transferred solve.
#[derive(Clone)]
pub(super) struct PendingBasisChange {
    pub(super) target: usize,
    pub(super) physical_solver_y: Vec<f64>,
}

/// Reciprocal-conditioning floor below which the active chart is treated as
/// approaching its fold. A first-integral chart's `rcond` falls smoothly from
/// one toward zero as its reconstructed coordinate passes through zero; firing
/// here keeps the transfer strictly inside the regular regime (`rcond` is still
/// many orders above the singular threshold), honoring the SPEC_0053 section 2a
/// requirement to request the change while the current basis is still regular.
const CONDITIONING_MARGIN: f64 = 0.5;

/// A candidate alternate chart must improve the reciprocal conditioning by at
/// least this factor to be adopted, so the transfer is only taken toward a
/// meaningfully better-conditioned basis and does not chatter at the crossover.
const CONDITIONING_IMPROVEMENT: f64 = 1.5;

/// Estimate the conditioning of the active chart and its alternates at a settled
/// coordinate and, when the active chart is approaching its fold while a
/// strictly better-conditioned regular alternate exists, return that alternate.
pub(super) fn detect_basis_change(
    active_runtime: &SolveRuntime,
    charts: &[KernelChart],
    group_cols: &[usize],
    active_chart: usize,
    t: f64,
    solver_y: &[f64],
    params: &[f64],
) -> Result<Option<usize>, RuntimeSolveError> {
    let active = &charts[active_chart];
    let dependent_positions: Vec<Vec<usize>> = charts
        .iter()
        .map(|chart| chart.dependent_positions.clone())
        .collect();
    let conditioning = active_runtime.reduced_chart_dependent_conditioning(
        t,
        solver_y,
        params,
        &active.folding_rows,
        group_cols,
        &dependent_positions,
    )?;
    let active_cond = conditioning[active_chart];
    if active_cond.rcond >= CONDITIONING_MARGIN {
        return Ok(None);
    }
    let mut best: Option<(usize, f64)> = None;
    for (index, cond) in conditioning.iter().enumerate() {
        if index == active_chart {
            continue;
        }
        let regular = cond.rcond > cond.singular_threshold;
        let improves = cond.rcond > active_cond.rcond * CONDITIONING_IMPROVEMENT;
        if regular && improves && best.is_none_or(|(_, rcond)| cond.rcond > rcond) {
            best = Some((index, cond.rcond));
        }
    }
    Ok(best.map(|(index, _)| index))
}

/// Splice one alternate reduced chart's carried plan and artifacts into the
/// shared problem skeleton, producing the solver model of that basis. It runs
/// as the continuous system in the same solver-Y space as the primary, backed
/// by the alternate's own reconstruction, derivative kernel, refresh owners,
/// and continuous artifacts.
fn alternate_chart_model(model: &SolveModel, chart_index: usize) -> Option<SolveModel> {
    let plan = model.problem.continuous.reduced_chart_set.charts[chart_index]
        .plan
        .as_ref()?;
    let mut alternate = model.clone();
    alternate.problem.continuous = ContinuousSolveSystem {
        implicit_rhs: plan.implicit_rhs.clone(),
        implicit_row_targets: plan.implicit_row_targets.clone(),
        algebraic_projection_plan: plan.algebraic_projection_plan.clone(),
        residual: plan.residual.clone(),
        manifold_residual: ComputeBlock::default(),
        manifold_projection_plan: AlgebraicProjectionPlan::default(),
        derivative_rhs: plan.derivative_rhs.clone(),
        refresh_owners: plan.refresh_owners.clone(),
        reduced_chart_set: Default::default(),
    };
    alternate.artifacts.continuous = plan.artifacts.clone();
    Some(alternate)
}

/// The single-row scalar reconstruction block rows of `dependent` in one chart's
/// algebraic projection plan: the folding first-integral rows for that chart.
fn folding_rows_for(plan: &AlgebraicProjectionPlan, dependent: &[usize]) -> Option<Vec<usize>> {
    plan.blocks
        .iter()
        .find(|block| block.y_indices.len() == 1 && block.y_indices == dependent)
        .map(|block| block.rows.clone())
}

/// Build the runtime-executable image of every reduced chart of `runtime`'s
/// continuous system, or `None` when the system carries no folding
/// first-integral group.
pub(super) fn build_reduced_charts(
    runtime: &Rc<SolveRuntime>,
    state_count: usize,
) -> Result<Option<ReducedChartRuntimes>, RuntimeSolveError> {
    let model = &runtime.model;
    let chart_set = &model.problem.continuous.reduced_chart_set;
    if chart_set.is_empty() {
        return Ok(None);
    }

    // The constrained group is the union of any chart's independent and
    // dependent columns; it is identical across charts, so chart zero defines
    // it and every other chart is required to match.
    let mut group_cols: Vec<usize> = chart_set.charts[0]
        .independent_y_indices
        .iter()
        .chain(&chart_set.charts[0].dependent_y_indices)
        .copied()
        .collect();
    group_cols.sort_unstable();
    group_cols.dedup();

    let mut runtimes = Vec::with_capacity(chart_set.charts.len());
    let mut charts = Vec::with_capacity(chart_set.charts.len());
    for (index, chart) in chart_set.charts.iter().enumerate() {
        let chart_runtime = if index == 0 {
            Rc::clone(runtime)
        } else {
            let alternate = alternate_chart_model(model, index).ok_or_else(|| {
                RuntimeSolveError::solve_ir("alternate reduced chart carries no executable plan")
            })?;
            Rc::new(SolveRuntime::new(&alternate).map_err(|error| {
                RuntimeSolveError::solve_ir(format!(
                    "alternate reduced chart is not runtime-executable: {error:?}"
                ))
            })?)
        };

        let mut chart_group: Vec<usize> = chart
            .independent_y_indices
            .iter()
            .chain(&chart.dependent_y_indices)
            .copied()
            .collect();
        chart_group.sort_unstable();
        chart_group.dedup();
        if chart_group != group_cols {
            return Err(RuntimeSolveError::solve_ir(
                "reduced charts span different coordinate groups",
            ));
        }

        let dependent_positions = chart
            .dependent_y_indices
            .iter()
            .map(|dependent| {
                group_cols
                    .iter()
                    .position(|column| column == dependent)
                    .ok_or_else(|| {
                        RuntimeSolveError::solve_ir(
                            "reduced-chart dependent column is not in group",
                        )
                    })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let folding_rows = folding_rows_for(
            &chart_runtime
                .model
                .problem
                .continuous
                .algebraic_projection_plan,
            &chart.dependent_y_indices,
        )
        .ok_or_else(|| {
            RuntimeSolveError::solve_ir(
                "reduced chart has no scalar reconstruction block for its dependent coordinate",
            )
        })?;

        let binding_rows = chart_runtime.implicit_state_binding_rows(
            0.0,
            &chart_runtime.model.initial_y,
            &chart_runtime.model.parameters,
            state_count,
        )?;

        runtimes.push(chart_runtime);
        charts.push(KernelChart {
            dependent_positions,
            folding_rows,
            binding_rows,
        });
    }

    Ok(Some(ReducedChartRuntimes {
        runtimes,
        charts,
        group_cols,
    }))
}
