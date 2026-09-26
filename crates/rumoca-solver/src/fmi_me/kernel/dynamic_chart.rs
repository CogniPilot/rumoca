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

use std::cell::OnceCell;
use std::rc::Rc;

use rumoca_eval_solve::dense_basis::DependentConditioning;
use rumoca_eval_solve::projection_policy::{
    CHART_REGULAR_MULTIPLE, CHART_SWITCH_IMPROVEMENT, CHART_SWITCH_KEEP,
};
use rumoca_ir_solve::{AlgebraicProjectionPlan, ComputeBlock, ContinuousSolveSystem, SolveModel};

use crate::runtime::solve_ops::RuntimeSolveError;
use crate::runtime::solve_runtime::{SolveRuntime, SolveRuntimeSnapshot};

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
    /// The chart's conditioning at the construction trial point.
    pub(super) trial_rcond: f64,
    /// Index of this chart in the model's reduced chart set.
    set_index: usize,
    /// The chart's runtime and state-binding rows, built on first use: an
    /// alternate is needed only near a fold. Its runtime shares the primary's
    /// prepared and compiled programs and prepares only the replaced ones.
    built: OnceCell<BuiltChart>,
}

/// A chart's executable runtime and, per generated state coordinate, the
/// implicit residual row whose value is the identity `state - source`:
/// `state[k] - residual[binding_rows[k]]` recovers this chart's integrated
/// source value for state coordinate `k`.
pub(super) struct BuiltChart {
    pub(super) runtime: Rc<SolveRuntime>,
    pub(super) binding_rows: Vec<usize>,
    /// The nominal of each generated state coordinate under this chart: the
    /// scale of the source coordinate it integrates. A state whose source the
    /// chart shares with the primary basis keeps the primary's nominal.
    pub(super) state_nominals: Vec<f64>,
}

/// Every runtime-executable reduced chart of one continuous system, index zero
/// being the primary basis executed by the enclosing continuous system.
pub(super) struct ReducedChartRuntimes {
    /// The primary runtime, whose model carries every chart's plan.
    primary: Rc<SolveRuntime>,
    state_count: usize,
    pub(super) charts: Vec<KernelChart>,
}

impl ReducedChartRuntimes {
    /// Chart `index`'s runtime and binding rows, built on first request.
    pub(super) fn built(&self, index: usize) -> Result<&BuiltChart, RuntimeSolveError> {
        let chart = self
            .charts
            .get(index)
            .ok_or_else(|| RuntimeSolveError::solve_ir("reduced chart index is out of range"))?;
        if let Some(built) = chart.built.get() {
            return Ok(built);
        }
        let runtime = if index == 0 {
            Rc::clone(&self.primary)
        } else {
            let alternate = alternate_chart_model(&self.primary.model, chart.set_index)
                .ok_or_else(|| RuntimeSolveError::solve_ir("reduced chart carries no plan"))?;
            Rc::new(self.primary.new_alternate(&alternate).map_err(|error| {
                RuntimeSolveError::solve_ir(format!(
                    "alternate reduced chart is not runtime-executable: {error:?}"
                ))
            })?)
        };
        let binding_rows = runtime.implicit_state_binding_rows(
            0.0,
            &runtime.model.initial_y,
            &runtime.model.parameters,
            self.state_count,
        )?;
        let state_nominals = if index == 0 {
            (0..self.state_count)
                .map(|state| runtime.model.solver_variable_scale(state))
                .collect()
        } else {
            alternate_state_nominals(self.built(0)?, &runtime, &binding_rows)
        };
        Ok(chart.built.get_or_init(|| BuiltChart {
            runtime,
            binding_rows,
            state_nominals,
        }))
    }

    /// The state nominals of chart `index` if it has been built.
    pub(super) fn built_state_nominals(&self, index: usize) -> Option<&[f64]> {
        self.charts
            .get(index)
            .and_then(|chart| chart.built.get())
            .map(|built| built.state_nominals.as_slice())
    }

    /// The runtime of chart `index` if it has been built.
    pub(super) fn built_runtime(&self, index: usize) -> Option<&Rc<SolveRuntime>> {
        self.charts
            .get(index)
            .and_then(|chart| chart.built.get())
            .map(|built| &built.runtime)
    }

    pub(super) fn len(&self) -> usize {
        self.charts.len()
    }

    /// Restore every built runtime that has a saved snapshot. A runtime built
    /// after the save carries no saved state; the next transfer onto it
    /// re-establishes its reconstruction.
    pub(super) fn restore_built(&self, snapshots: &[Option<SolveRuntimeSnapshot>]) {
        for (index, snapshot) in snapshots.iter().enumerate() {
            let runtime = self.built_runtime(index);
            if let (Some(runtime), Some(snapshot)) = (runtime, snapshot) {
                runtime.restore(snapshot);
            }
        }
    }

    /// Whether every built runtime matches its snapshot and no unbuilt one has
    /// a snapshot.
    #[cfg(test)]
    pub(super) fn match_snapshots(&self, snapshots: &[Option<SolveRuntimeSnapshot>]) -> bool {
        self.len() == snapshots.len()
            && snapshots.iter().enumerate().all(|(index, snapshot)| {
                match (self.built_runtime(index), snapshot) {
                    (Some(runtime), Some(snapshot)) => runtime.as_ref().matches_snapshot(snapshot),
                    (None, None) => true,
                    _ => false,
                }
            })
    }
}

/// A completed step requested a basis change: the target chart and the last
/// consistent full physical coordinate captured while the outgoing basis was
/// still regular, used to re-seed and branch-limit the transferred solve.
#[derive(Clone)]
pub(super) struct PendingBasisChange {
    pub(super) target: usize,
    pub(super) physical_solver_y: Vec<f64>,
    /// The target's conditioning at the request: its keep reference once active.
    pub(super) target_reference: f64,
    /// The active chart's conditioning at the request.
    pub(super) active_conditioning: f64,
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

/// The conditioning below which the active chart is re-tested: a fraction
/// `CHART_SWITCH_KEEP` of its reference, the conditioning it had when it became
/// active (its construction value for the primary basis). A multi-row chart can
/// be well conditioned far below an absolute threshold, so the keep test is
/// relative to where the chart started.
pub(super) fn keep_threshold(reference: f64) -> f64 {
    CHART_SWITCH_KEEP * reference
}

/// Decide from every chart's conditioning at one accepted point.
///
/// The active chart is kept while its `sigma` is at least `keep` (see
/// [`keep_threshold`]). Below that, the chart with the largest `sigma` that exceeds the active one by
/// `CHART_SWITCH_IMPROVEMENT` and lies in its own regular region is requested;
/// ties go to the lowest chart index. Because adopting `b` over `a` requires
/// `sigma(b) > CHART_SWITCH_IMPROVEMENT * sigma(a)`, switching back needs the
/// ratio to swing by the square of that factor: the hysteresis band. An active
/// chart below its regular region has already folded.
pub(super) fn decide(
    conditioning: &[DependentConditioning],
    active: usize,
    keep: f64,
) -> ChartDecision {
    let current = conditioning[active];
    if current.rcond < regular_bound(&current) {
        return ChartDecision::Folded {
            sigma: current.rcond,
            regular: regular_bound(&current),
        };
    }
    if current.rcond >= keep {
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

/// The decision at one accepted point. Every chart's `sigma` is evaluated
/// through its own runtime's slope blocks at the shared solver-Y point, but
/// lazily: see [`decide_lazily`].
pub(super) fn decide_at(
    charts: &ReducedChartRuntimes,
    active: usize,
    reference: f64,
    t: f64,
    solver_y: &[f64],
    params: &[f64],
) -> Result<(ChartDecision, Vec<DependentConditioning>), RuntimeSolveError> {
    decide_lazily(charts.len(), active, reference, &mut |index| {
        one_chart_conditioning(charts, index, t, solver_y, params)
    })
}

/// Evaluate the active chart first: a chart kept by its own conditioning needs
/// no alternate evaluated, which is the common case away from a fold. Below
/// its keep threshold or its regular region, every other chart is evaluated
/// once and [`decide`] chooses.
fn decide_lazily(
    chart_count: usize,
    active: usize,
    reference: f64,
    evaluate: &mut dyn FnMut(usize) -> Result<DependentConditioning, RuntimeSolveError>,
) -> Result<(ChartDecision, Vec<DependentConditioning>), RuntimeSolveError> {
    let keep = keep_threshold(reference);
    let current = evaluate(active)?;
    if current.rcond >= keep && current.rcond >= regular_bound(&current) {
        return Ok((ChartDecision::Keep, Vec::new()));
    }
    let conditioning = (0..chart_count)
        .map(|index| {
            if index == active {
                Ok(current)
            } else {
                evaluate(index)
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    Ok((decide(&conditioning, active, keep), conditioning))
}

fn one_chart_conditioning(
    charts: &ReducedChartRuntimes,
    index: usize,
    t: f64,
    solver_y: &[f64],
    params: &[f64],
) -> Result<DependentConditioning, RuntimeSolveError> {
    let (runtime, chart) = (&charts.built(index)?.runtime, &charts.charts[index]);
    let mut conditioning = runtime.reduced_chart_dependent_conditioning(
        t,
        solver_y,
        params,
        &chart.slope_rows,
        &chart.slope_cols,
        std::slice::from_ref(&chart.slope_dependent_positions),
    )?;
    conditioning
        .pop()
        .ok_or_else(|| RuntimeSolveError::solve_ir("reduced-chart conditioning returned no chart"))
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

/// The state nominals of an alternate chart: a state whose binding row
/// integrates another source than the primary's takes that source's scale;
/// one that integrates the same source, or whose source is not a single
/// coordinate, keeps the primary's nominal.
fn alternate_state_nominals(
    primary: &BuiltChart,
    runtime: &SolveRuntime,
    binding_rows: &[usize],
) -> Vec<f64> {
    binding_rows
        .iter()
        .enumerate()
        .map(|(state, &row)| {
            let source = runtime.binding_row_source(row, state);
            let primary_source = primary
                .binding_rows
                .get(state)
                .and_then(|&row| primary.runtime.binding_row_source(row, state));
            match source {
                Some(source) if Some(source) != primary_source => {
                    runtime.model.solver_variable_scale(source)
                }
                _ => primary.state_nominals[state],
            }
        })
        .collect()
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
    let mut charts = Vec::with_capacity(chart_set.charts.len());
    for (index, chart) in chart_set.charts.iter().enumerate() {
        // An alternate may be admissible geometry without a lowered plan (a
        // partition-only chart); it cannot run, so the detector never sees it.
        let plan = match &chart.plan {
            Some(plan) => &plan.algebraic_projection_plan,
            None if index == 0 => &model.problem.continuous.algebraic_projection_plan,
            None => continue,
        };
        if chart_columns(chart) != group_cols {
            return Err(RuntimeSolveError::solve_ir(
                "reduced charts span different coordinate groups",
            ));
        }
        let (slope_rows, slope_cols, slope_dependent_positions) =
            slope_geometry(plan, &chart.dependent_y_indices, &group_cols).ok_or_else(|| {
                RuntimeSolveError::solve_ir(
                    "reduced chart has no reconstruction block for its dependent coordinate",
                )
            })?;
        charts.push(KernelChart {
            slope_rows,
            slope_cols,
            slope_dependent_positions,
            trial_rcond: chart.trial_rcond,
            set_index: index,
            built: OnceCell::new(),
        });
    }

    // With no executable alternate the component holds only its primary basis
    // and performs no runtime state selection.
    if charts.len() <= 1 {
        return Ok(None);
    }
    let charts = ReducedChartRuntimes {
        primary: Rc::clone(runtime),
        state_count,
        charts,
    };
    // The primary is built now: its binding rows are checked at instantiation.
    charts.built(0)?;
    Ok(Some(charts))
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

    fn decide_abs(conditioning: &[DependentConditioning], active: usize) -> ChartDecision {
        decide(conditioning, active, keep_threshold(1.0))
    }

    fn sigma(rcond: f64) -> DependentConditioning {
        DependentConditioning {
            rcond,
            singular_threshold: THRESHOLD,
        }
    }

    /// The decision and the charts a lazy decision evaluates, in order.
    fn lazily(sigmas: &[f64], active: usize, reference: f64) -> (ChartDecision, Vec<usize>) {
        let mut evaluated = Vec::new();
        let (decision, _) = decide_lazily(sigmas.len(), active, reference, &mut |index| {
            evaluated.push(index);
            Ok(sigma(sigmas[index]))
        })
        .unwrap();
        (decision, evaluated)
    }

    #[test]
    fn a_chart_within_its_keep_evaluates_no_alternate() {
        // A multi-row chart constructed at 0.3 and now at 0.2 is within half of
        // its reference, so no alternate slope is evaluated even though 0.2 is
        // far below an absolute 0.5.
        assert_eq!(
            lazily(&[0.2, 0.9, 0.9], 0, 0.3),
            (ChartDecision::Keep, vec![0])
        );
        // Degraded below half its reference, every chart is evaluated once.
        assert_eq!(
            lazily(&[0.1, 0.9, 0.2], 0, 0.3),
            (ChartDecision::Switch(1), vec![0, 1, 2])
        );
        // An alternate's reference is its conditioning when it became active.
        assert_eq!(lazily(&[0.9, 0.3], 1, 0.4), (ChartDecision::Keep, vec![1]));
    }

    #[test]
    fn a_well_conditioned_active_chart_is_kept() {
        assert_eq!(
            decide_abs(&[sigma(0.6), sigma(1.0)], 0),
            ChartDecision::Keep
        );
    }

    #[test]
    fn a_chart_approaching_its_fold_switches_to_a_clearly_better_one() {
        assert_eq!(
            decide_abs(&[sigma(0.3), sigma(0.46)], 0),
            ChartDecision::Switch(1)
        );
        // Not better by the improvement factor: keep.
        assert_eq!(
            decide_abs(&[sigma(0.3), sigma(0.44)], 0),
            ChartDecision::Keep
        );
    }

    #[test]
    fn the_best_qualifying_chart_wins_and_ties_go_to_the_lowest_index() {
        assert_eq!(
            decide_abs(&[sigma(0.1), sigma(0.4), sigma(0.8), sigma(0.8)], 0),
            ChartDecision::Switch(2)
        );
    }

    #[test]
    fn switching_back_needs_the_squared_hysteresis_band() {
        // After a switch from chart 0 at (0.3, 0.46), chart 1 is active. The same
        // point, and every point until chart 0 exceeds chart 1 by the factor
        // again, keeps chart 1.
        assert_eq!(
            decide_abs(&[sigma(0.3), sigma(0.46)], 1),
            ChartDecision::Keep
        );
        assert_eq!(
            decide_abs(&[sigma(0.6), sigma(0.45)], 1),
            ChartDecision::Keep
        );
        assert_eq!(
            decide_abs(&[sigma(0.7), sigma(0.45)], 1),
            ChartDecision::Switch(0)
        );
        // A well-conditioned active chart is kept however good the other is.
        assert_eq!(
            decide_abs(&[sigma(0.9), sigma(0.55)], 1),
            ChartDecision::Keep
        );
    }

    #[test]
    fn an_irregular_alternate_is_never_adopted() {
        let irregular = THRESHOLD * CHART_REGULAR_MULTIPLE * 0.5;
        assert_eq!(
            decide_abs(&[sigma(irregular * 3.0), sigma(irregular)], 0),
            ChartDecision::Keep
        );
    }

    #[test]
    fn an_active_chart_below_its_regular_region_has_folded() {
        let regular = THRESHOLD * CHART_REGULAR_MULTIPLE;
        assert_eq!(
            decide_abs(&[sigma(regular * 0.5), sigma(1.0)], 0),
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
