//! Runtime state selection over the issued reduced charts of a folding group
//! (SPEC_0053 section 2a; SPEC_0040 STRUCT-T07 constraint-fold chart rows).
//!
//! A fixed reduced basis folds where the slope of a reconstructed coordinate
//! vanishes on the manifold: the gradient `2*x_i` of a conserved norm, or the
//! Cardan factor of a loop closure. The compiler issues a bounded set of
//! alternate charts on the continuous solve system, each carrying its complete
//! executable image in the same solver-Y space as the primary basis: the
//! mirrors of a first-integral group, or the single exchanges of a reduced
//! constraint group. This module builds one runtime per chart, precomputes each
//! chart's slope geometry, and decides at an accepted step whether to switch.
//!
//! Every model without such a group carries an empty chart set, so
//! [`build_reduced_charts`] returns `None` and the component behaves exactly as
//! it did before dynamic state selection existed.

use std::rc::Rc;

use rumoca_eval_solve::dense_basis::DependentConditioning;
use rumoca_eval_solve::projection_policy::{
    CHART_REGULAR_MULTIPLE, CHART_SWITCH_IMPROVEMENT, CHART_SWITCH_KEEP,
};
use rumoca_ir_solve::{AlgebraicProjectionPlan, ComputeBlock, ContinuousSolveSystem, SolveModel};

use crate::runtime::solve_ops::RuntimeSolveError;
use crate::runtime::solve_runtime::SolveRuntime;

/// The compile-time geometry of one admissible reduced chart, resolved into the
/// solver-Y index space shared by every chart of the group.
///
/// A chart's slope program is the reconstruction of its folding coordinates in
/// its own plan: every projection block that solves one of them. `sigma` is the
/// reciprocal conditioning of that block Jacobian over its own unknowns,
/// measured against the largest pivot over those unknowns plus the group
/// columns.
pub(super) struct KernelChart {
    /// Implicit residual rows of this chart's slope blocks, in its own plan.
    pub(super) slope_rows: Vec<usize>,
    /// Solver-Y columns of the conditioning matrix: the slope blocks' unknowns
    /// plus the group columns, sorted.
    pub(super) slope_cols: Vec<usize>,
    /// Positions within `slope_cols` of the slope blocks' unknowns.
    pub(super) slope_dependent_positions: Vec<usize>,
    /// Per generated state coordinate, the implicit residual row whose value is
    /// the identity `state - source`. `state[k] - residual[binding_rows[k]]`
    /// recovers this chart's integrated source value for state coordinate `k`.
    pub(super) binding_rows: Vec<usize>,
    /// The chart's conditioning at the construction trial point.
    pub(super) trial_rcond: f64,
}

/// Every runtime-executable reduced chart of one continuous system, index zero
/// being the primary basis executed by the enclosing continuous system.
pub(super) struct ReducedChartRuntimes {
    /// Chart index zero is `Rc::clone` of the enclosing runtime; higher indices
    /// are the alternate bases spliced from their carried plans.
    pub(super) runtimes: Vec<Rc<SolveRuntime>>,
    pub(super) charts: Vec<KernelChart>,
}

/// A completed step requested a basis change: the target chart and the last
/// consistent full physical coordinate captured while the outgoing basis was
/// still regular, used to re-seed and branch-limit the transferred solve.
#[derive(Clone)]
pub(super) struct PendingBasisChange {
    pub(super) target: usize,
    pub(super) physical_solver_y: Vec<f64>,
}

/// What an accepted step decides about the active chart.
#[derive(Clone, Copy, Debug, PartialEq)]
pub(super) enum ChartDecision {
    /// Keep integrating on the active chart.
    Keep,
    /// Request an Event-Mode change to this chart.
    Switch(usize),
    /// The active chart settled below its regular region: the fold was crossed
    /// before a change could be requested.
    Folded { sigma: f64, regular: f64 },
}

/// The lower bound of a chart's regular region.
fn regular_bound(conditioning: &DependentConditioning) -> f64 {
    conditioning.singular_threshold * CHART_REGULAR_MULTIPLE
}

/// Decide from every chart's conditioning at one accepted point.
///
/// The active chart is kept while its `sigma` is at least `CHART_SWITCH_KEEP`.
/// Below that, the chart with the largest `sigma` that exceeds the active one by
/// `CHART_SWITCH_IMPROVEMENT` and lies in its own regular region is requested;
/// ties go to the lowest chart index. Because adopting `b` over `a` requires
/// `sigma(b) > CHART_SWITCH_IMPROVEMENT * sigma(a)`, switching back needs the
/// ratio to swing by the square of that factor: the hysteresis band. An active
/// chart below its regular region has already folded.
pub(super) fn decide(conditioning: &[DependentConditioning], active: usize) -> ChartDecision {
    let current = conditioning[active];
    if current.rcond < regular_bound(&current) {
        return ChartDecision::Folded {
            sigma: current.rcond,
            regular: regular_bound(&current),
        };
    }
    if current.rcond >= CHART_SWITCH_KEEP {
        return ChartDecision::Keep;
    }
    let mut best: Option<(usize, f64)> = None;
    for (index, candidate) in conditioning.iter().enumerate() {
        let qualifies = index != active
            && candidate.rcond > CHART_SWITCH_IMPROVEMENT * current.rcond
            && candidate.rcond >= regular_bound(candidate);
        if qualifies && best.is_none_or(|(_, sigma)| candidate.rcond > sigma) {
            best = Some((index, candidate.rcond));
        }
    }
    best.map_or(ChartDecision::Keep, |(index, _)| {
        ChartDecision::Switch(index)
    })
}

/// Every chart's `sigma` at one settled physical coordinate, each evaluated
/// through its own runtime's slope blocks. The coordinate is shared: every chart
/// runs in the same solver-Y space and its slope rows read physical values.
pub(super) fn chart_conditioning(
    charts: &ReducedChartRuntimes,
    t: f64,
    solver_y: &[f64],
    params: &[f64],
) -> Result<Vec<DependentConditioning>, RuntimeSolveError> {
    charts
        .runtimes
        .iter()
        .zip(&charts.charts)
        .map(|(runtime, chart)| {
            let mut conditioning = runtime.reduced_chart_dependent_conditioning(
                t,
                solver_y,
                params,
                &chart.slope_rows,
                &chart.slope_cols,
                std::slice::from_ref(&chart.slope_dependent_positions),
            )?;
            conditioning.pop().ok_or_else(|| {
                RuntimeSolveError::solve_ir("reduced-chart conditioning returned no chart")
            })
        })
        .collect()
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

/// The slope geometry of one chart: the rows and unknowns of every projection
/// block of its plan that reconstructs one of its folding coordinates, and the
/// conditioning columns those unknowns span together with the group columns.
fn slope_geometry(
    plan: &AlgebraicProjectionPlan,
    dependent: &[usize],
    group_cols: &[usize],
) -> Option<(Vec<usize>, Vec<usize>, Vec<usize>)> {
    let blocks = plan
        .blocks
        .iter()
        .filter(|block| block.y_indices.iter().any(|y| dependent.contains(y)))
        .collect::<Vec<_>>();
    if blocks.is_empty() {
        return None;
    }
    let rows = blocks
        .iter()
        .flat_map(|block| block.rows.iter().copied())
        .collect::<Vec<_>>();
    let unknowns = blocks
        .iter()
        .flat_map(|block| block.y_indices.iter().copied())
        .collect::<Vec<_>>();
    let mut cols = unknowns
        .iter()
        .chain(group_cols)
        .copied()
        .collect::<Vec<_>>();
    cols.sort_unstable();
    cols.dedup();
    let positions = unknowns
        .iter()
        .map(|unknown| cols.iter().position(|col| col == unknown))
        .collect::<Option<Vec<_>>>()?;
    Some((rows, cols, positions))
}

/// Build the runtime-executable image of every reduced chart of `runtime`'s
/// continuous system that carries a lowered executable plan. Returns `None`
/// when the system carries no chart set, and equally when no alternate basis
/// lowered to an executable plan, so in both cases the component runs on its
/// primary basis alone.
pub(super) fn build_reduced_charts(
    runtime: &Rc<SolveRuntime>,
    state_count: usize,
) -> Result<Option<ReducedChartRuntimes>, RuntimeSolveError> {
    let model = &runtime.model;
    let chart_set = &model.problem.continuous.reduced_chart_set;
    if chart_set.charts.iter().all(|chart| chart.plan.is_none()) {
        return Ok(None);
    }

    // The group is the union of any chart's independent and dependent columns;
    // it is identical across charts, so chart zero defines it and every other
    // chart is required to match.
    let group_cols = chart_columns(&chart_set.charts[0]);

    let mut runtimes = Vec::with_capacity(chart_set.charts.len());
    let mut charts = Vec::with_capacity(chart_set.charts.len());
    for (index, chart) in chart_set.charts.iter().enumerate() {
        let chart_runtime = if index == 0 {
            Rc::clone(runtime)
        } else {
            // An alternate may be admissible geometry without a lowered plan
            // (a partition-only chart); such a chart cannot run and is not built,
            // so the detector can only ever select a chart built here.
            let Some(alternate) = alternate_chart_model(model, index) else {
                continue;
            };
            Rc::new(SolveRuntime::new(&alternate).map_err(|error| {
                RuntimeSolveError::solve_ir(format!(
                    "alternate reduced chart is not runtime-executable: {error:?}"
                ))
            })?)
        };
        if chart_columns(chart) != group_cols {
            return Err(RuntimeSolveError::solve_ir(
                "reduced charts span different coordinate groups",
            ));
        }
        let (slope_rows, slope_cols, slope_dependent_positions) = slope_geometry(
            &chart_runtime
                .model
                .problem
                .continuous
                .algebraic_projection_plan,
            &chart.dependent_y_indices,
            &group_cols,
        )
        .ok_or_else(|| {
            RuntimeSolveError::solve_ir(
                "reduced chart has no reconstruction block for its dependent coordinate",
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
            slope_rows,
            slope_cols,
            slope_dependent_positions,
            binding_rows,
            trial_rcond: chart.trial_rcond,
        });
    }

    // With no executable alternate the component holds only its primary basis
    // and performs no runtime state selection.
    if charts.len() <= 1 {
        return Ok(None);
    }
    Ok(Some(ReducedChartRuntimes { runtimes, charts }))
}

/// The sorted union of a chart's independent and dependent columns.
fn chart_columns(chart: &rumoca_ir_solve::ReducedChart) -> Vec<usize> {
    let mut columns = chart
        .independent_y_indices
        .iter()
        .chain(&chart.dependent_y_indices)
        .copied()
        .collect::<Vec<_>>();
    columns.sort_unstable();
    columns.dedup();
    columns
}

#[cfg(test)]
mod tests {
    use super::*;

    const THRESHOLD: f64 = 1.0e-15;

    fn sigma(rcond: f64) -> DependentConditioning {
        DependentConditioning {
            rcond,
            singular_threshold: THRESHOLD,
        }
    }

    #[test]
    fn a_well_conditioned_active_chart_is_kept() {
        assert_eq!(decide(&[sigma(0.6), sigma(1.0)], 0), ChartDecision::Keep);
    }

    #[test]
    fn a_chart_approaching_its_fold_switches_to_a_clearly_better_one() {
        assert_eq!(
            decide(&[sigma(0.3), sigma(0.46)], 0),
            ChartDecision::Switch(1)
        );
        // Not better by the improvement factor: keep.
        assert_eq!(decide(&[sigma(0.3), sigma(0.44)], 0), ChartDecision::Keep);
    }

    #[test]
    fn the_best_qualifying_chart_wins_and_ties_go_to_the_lowest_index() {
        assert_eq!(
            decide(&[sigma(0.1), sigma(0.4), sigma(0.8), sigma(0.8)], 0),
            ChartDecision::Switch(2)
        );
    }

    #[test]
    fn switching_back_needs_the_squared_hysteresis_band() {
        // After a switch from chart 0 at (0.3, 0.46), chart 1 is active. The same
        // point, and every point until chart 0 exceeds chart 1 by the factor
        // again, keeps chart 1.
        assert_eq!(decide(&[sigma(0.3), sigma(0.46)], 1), ChartDecision::Keep);
        assert_eq!(decide(&[sigma(0.6), sigma(0.45)], 1), ChartDecision::Keep);
        assert_eq!(
            decide(&[sigma(0.7), sigma(0.45)], 1),
            ChartDecision::Switch(0)
        );
        // A well-conditioned active chart is kept however good the other is.
        assert_eq!(decide(&[sigma(0.9), sigma(0.55)], 1), ChartDecision::Keep);
    }

    #[test]
    fn an_irregular_alternate_is_never_adopted() {
        let irregular = THRESHOLD * CHART_REGULAR_MULTIPLE * 0.5;
        assert_eq!(
            decide(&[sigma(irregular * 3.0), sigma(irregular)], 0),
            ChartDecision::Keep
        );
    }

    #[test]
    fn an_active_chart_below_its_regular_region_has_folded() {
        let regular = THRESHOLD * CHART_REGULAR_MULTIPLE;
        assert_eq!(
            decide(&[sigma(regular * 0.5), sigma(1.0)], 0),
            ChartDecision::Folded {
                sigma: regular * 0.5,
                regular
            }
        );
    }

    #[test]
    fn slope_geometry_spans_every_block_reconstructing_a_folding_coordinate() {
        use rumoca_ir_solve::AlgebraicProjectionBlock;
        let block = |rows: Vec<usize>, y_indices: Vec<usize>| AlgebraicProjectionBlock {
            rows,
            y_indices,
            tearing: None,
            alternate_charts: Vec::new(),
        };
        let plan = AlgebraicProjectionPlan {
            blocks: vec![
                block(vec![0], vec![5]),
                block(vec![1, 2], vec![7, 8]),
                block(vec![3], vec![9]),
            ],
        };
        // Folding coordinate 7 lives in the coupled block; the group adds 5 and 6.
        let (rows, cols, positions) = slope_geometry(&plan, &[7], &[5, 6, 7]).unwrap();
        assert_eq!(rows, vec![1, 2]);
        assert_eq!(cols, vec![5, 6, 7, 8]);
        assert_eq!(positions, vec![2, 3]);
        assert!(slope_geometry(&plan, &[4], &[4]).is_none());
    }
}
