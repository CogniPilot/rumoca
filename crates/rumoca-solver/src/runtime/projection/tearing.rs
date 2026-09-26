//! Torn solve for one coupled algebraic projection block.
//!
//! When the constructor tore a coupled block into a small iteration (tear) set
//! plus an ordered back-substitution, the projection iterates Newton only over
//! the tear variables and recovers every other unknown by evaluating its causal
//! row's exact explicit assignment. This is the causalized solve OpenModelica
//! performs: it keeps the nonlinear system at the tear dimension and never
//! forms the dense, often ill-conditioned Jacobian over the whole loop, so the
//! block converges from starts where the dense block Newton diverges.
//!
//! Back-substitution is exact-only by construction. Every causal step is an
//! exact explicit assignment for its unknown: prepare-time tearing
//! normalization promotes any step that is not exact into the reduced Newton
//! (its unknown becomes a tear variable and its row a reduced residual), so all
//! nonlinearity and multi-root robustness is carried by the reduced Newton's
//! finite-difference Jacobian and line search rather than a fragile isolated 1D
//! solve. A causal step therefore never runs an inner iteration; it evaluates
//! the certified target isolator once. The runtime still fails closed: if the
//! exactness invariant is ever violated it declines the step rather than
//! accepting an under-solved value.
//!
//! Every evaluation of the block goes through one sweep primitive
//! ([`ImplicitProjectionModel::torn_block_sweep`]): back-substitution followed
//! by the reduced residual rows. Models that can batch the sweep into a single
//! call override it; the per-row default here is the reference semantics.
//!
//! The torn solve is a strict refinement: it either converges the block and
//! reports it settled, or it declines (restoring the incoming values) and the
//! caller falls back to the dense block Newton, so no block that solved before
//! can regress.

use nalgebra::DMatrix;
use rumoca_ir_solve as solve;

use super::scaling::{
    ScaledNewtonSystem, jacobian_row_scales, model_variable_scale, scaled_correction_converged,
    scaled_newton_delta, scaled_residual_converged, scaled_residual_norm,
};
use super::{
    ImplicitProjectionModel, KernelAnswer, KernelRequest, ProjectionBlockUpdate, RuntimeSolveError,
};

use rumoca_eval_solve::projection_policy::{
    TORN_BACKTRACK_STEPS, TORN_OUTER_MAX_ITERS, finite_difference_perturbation,
};

/// Attempt the torn solve of one coupled block.
///
/// Returns `Ok(Some(update))` when the block converged (with `update.settled`
/// set) and `Ok(None)` when the torn solve could not proceed, in which case
/// `y` is restored to the values it held on entry and the caller solves the
/// block densely.
pub(super) fn project_torn_algebraic_block<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    tearing: &solve::BlockTearing,
    tol: f64,
    certify_coordinates: bool,
) -> Result<Option<ProjectionBlockUpdate>, RuntimeSolveError> {
    if tearing.tear_y_indices.len() != tearing.residual_rows.len() {
        return Ok(None);
    }
    // Decline any plan whose tear or causal unknowns fall outside solver storage
    // rather than index out of bounds; the caller then solves the block densely.
    let out_of_range = tearing
        .tear_y_indices
        .iter()
        .chain(tearing.causal_steps.iter().map(|step| &step.y_index))
        .any(|&index| index >= y.len());
    if out_of_range {
        return Ok(None);
    }
    let snapshot = TornValues::save(tearing, y);

    let mut residual = Vec::with_capacity(tearing.residual_rows.len());
    if !model.torn_block_sweep(tearing, y, p, t, &mut residual)? {
        snapshot.restore(tearing, y);
        return Ok(None);
    }

    // The sweep writes only causal unknowns and the scales read only tear
    // slots, so computing them after the residual rows leaves the values the
    // pre-sweep ordering produced.
    let variable_scales = tearing
        .tear_y_indices
        .iter()
        .map(|&index| model_variable_scale(model, index, y[index]))
        .collect::<Vec<_>>();

    if !all_finite(&residual) {
        snapshot.restore(tearing, y);
        return Ok(None);
    }

    for _ in 0..TORN_OUTER_MAX_ITERS {
        match advance_torn_newton(
            model,
            y,
            p,
            t,
            tearing,
            &residual,
            &variable_scales,
            tol,
            certify_coordinates,
        )? {
            TornStep::Settled => {
                let changed = snapshot.changed(tearing, y);
                return Ok(Some(ProjectionBlockUpdate {
                    changed,
                    settled: true,
                }));
            }
            TornStep::Advanced(next) => residual = next,
            TornStep::Decline => {
                snapshot.restore(tearing, y);
                return Ok(None);
            }
        }
    }

    // The reduced Newton exhausted its iterations without meeting tolerance.
    // Restore the incoming values so the dense fallback starts exactly where it
    // would have without the torn attempt.
    snapshot.restore(tearing, y);
    Ok(None)
}

/// Outcome of one reduced Newton iteration over the tear variables.
enum TornStep {
    /// The reduced residual meets tolerance; the block is solved.
    Settled,
    /// A step was accepted; carries the reduced residual at the new point.
    Advanced(Vec<f64>),
    /// The torn solve cannot proceed; the caller falls back to the dense solve.
    Decline,
}

/// Advance the reduced Newton by one iteration from the current point in `y`.
///
/// On `Advanced` `y` holds the accepted point; on `Settled` it holds the
/// converged point; on `Decline` it is restored to the point held on entry.
// SPEC_0021: Exception - one reduced Newton iteration threads model, storage, tearing, residual, scales, and tolerance together.
#[allow(clippy::too_many_arguments)]
fn advance_torn_newton<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    tearing: &solve::BlockTearing,
    residual: &[f64],
    variable_scales: &[f64],
    tol: f64,
    certify_coordinates: bool,
) -> Result<TornStep, RuntimeSolveError> {
    // A residual that is exactly zero rowwise satisfies every positive scaled
    // tolerance (`scaled_tolerance` never falls below `f64::MIN_POSITIVE`),
    // so the fresh finite-difference Jacobian's row scales could only confirm
    // what is already proven; settle without paying the Jacobian sweeps. This
    // mirrors the dense block's exact-zero shortcut and, like it, settles
    // without probing the Jacobian at the solved point. It stays out of
    // coordinate certification, which also requires the Newton correction to
    // be within tolerance and therefore needs the Jacobian. A nonzero
    // residual keeps the full path: its convergence test reads the fresh row
    // scales, which can shrink between iterates, so passing under stale
    // scales proves nothing.
    if !certify_coordinates && residual.iter().all(|value| *value == 0.0) {
        return Ok(TornStep::Settled);
    }
    let base = TornValues::save(tearing, y);
    let Some(jacobian) = reduced_jacobian(
        model,
        y,
        p,
        t,
        tearing,
        residual,
        variable_scales,
        &base,
        certify_coordinates,
    )?
    else {
        return Ok(TornStep::Decline);
    };
    let row_scales =
        jacobian_row_scales(&jacobian.residual, variable_scales, variable_scales, None);
    let converged = scaled_residual_converged(residual, &row_scales, tol);
    let delta = scaled_newton_delta(ScaledNewtonSystem {
        jacobian: &jacobian.residual,
        residual,
        row_scales: &row_scales,
        variable_scales,
        structure: None,
        tolerance: tol,
    });
    let Some(delta) = delta.filter(|delta| delta.iter().all(|value| value.is_finite())) else {
        return Ok(TornStep::Decline);
    };
    let step = LineSearchStep {
        base: &base,
        delta: delta.as_slice(),
        row_scales: &row_scales,
        before: scaled_residual_norm(residual, &row_scales),
        tol,
        alpha: 1.0,
    };
    // Small tear corrections can still produce large recovered-coordinate
    // changes. Certification checks the undamped step through the complete
    // causal sweep as well as the tear coordinates.
    if converged
        && (!certify_coordinates
            || (scaled_correction_converged(delta.as_slice(), variable_scales, tol)
                && recovered_correction_converged(
                    model,
                    &base,
                    tearing,
                    &jacobian.recovered,
                    delta.as_slice(),
                    tol,
                )
                && causal_correction_converged(model, y, (p, t), tearing, &step)?))
    {
        return Ok(TornStep::Settled);
    }
    match line_search(model, y, p, t, tearing, step)? {
        Some(next) => Ok(TornStep::Advanced(next)),
        None => Ok(TornStep::Decline),
    }
}

/// Per-row torn sweep: back-substitute every causal step in order, then
/// evaluate the reduced residual rows into `residual_out` (NaN for a row that
/// yields no finite scalar value). Returns `false` when any causal step
/// cannot be evaluated to a finite value, in which case the caller declines
/// the torn solve, restores `y`, and falls back to the dense block Newton.
///
/// This is both the default [`ImplicitProjectionModel::torn_block_sweep`] and
/// the reference semantics a batching override must reproduce bit for bit.
pub(crate) fn per_row_torn_block_sweep<M: ImplicitProjectionModel + ?Sized>(
    model: &M,
    tearing: &solve::BlockTearing,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    residual_out: &mut Vec<f64>,
) -> Result<bool, RuntimeSolveError> {
    for step in &tearing.causal_steps {
        if !solve_causal_step(model, y, p, t, step.row, step.y_index)? {
            return Ok(false);
        }
    }
    residual_out.clear();
    for &row in &tearing.residual_rows {
        residual_out.push(residual_row(model, y, p, t, row)?.unwrap_or(f64::NAN));
    }
    Ok(true)
}

/// Recover one causal unknown by evaluating its exact explicit assignment.
///
/// Prepare-time tearing normalization guarantees every causal step is an exact
/// explicit assignment for its unknown, so this only ever evaluates the
/// certified target isolator once; there is no inner iteration to under-solve.
/// The step still fails closed: if the exactness invariant is ever violated, or
/// the isolator yields no finite value, it declines (`Ok(false)`) so the caller
/// falls back to the dense block Newton rather than accepting a wrong value.
fn solve_causal_step<M: ImplicitProjectionModel + ?Sized>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    row: usize,
    y_index: usize,
) -> Result<bool, RuntimeSolveError> {
    if y_index >= y.len() {
        return Ok(false);
    }
    debug_assert!(
        model.implicit_target_assignment_is_exact(row, y_index),
        "causal back-substitution requires an exact explicit assignment; \
         a non-exact step must be promoted to a tear variable at prepare time"
    );
    if !model.implicit_target_assignment_is_exact(row, y_index) {
        return Ok(false);
    }
    match model.isolation_value(row, y_index, y, p, t)? {
        Some(value) => {
            y[y_index] = value;
            Ok(true)
        }
        None => Ok(false),
    }
}

fn all_finite(values: &[f64]) -> bool {
    values.iter().all(|value| value.is_finite())
}

struct ReducedJacobian {
    residual: DMatrix<f64>,
    recovered: DMatrix<f64>,
}

/// Total finite-difference Jacobian of the reduced residual with respect to the
/// tear variables, back-substituting through each perturbation so the causal
/// unknowns track their tear dependence. Certification retains their derivatives
/// from the same sweeps, before a Newton correction can round away in `base`.
// SPEC_0021: Exception - a total FD Jacobian needs model, storage, tearing, the base residual, scales, and the base point together.
#[allow(clippy::too_many_arguments)]
fn reduced_jacobian<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    tearing: &solve::BlockTearing,
    residual: &[f64],
    variable_scales: &[f64],
    base: &TornValues,
    certify_coordinates: bool,
) -> Result<Option<ReducedJacobian>, RuntimeSolveError> {
    let rows = tearing.residual_rows.len();
    let columns = tearing.tear_y_indices.len();
    if let KernelAnswer::TornJacobian(exact) = model.linked_kernel(KernelRequest::TornJacobian {
        tearing,
        point: (&*y, p, t),
    })? && let Some(jacobian) =
        tangent_reduced_jacobian(&exact, (rows, columns), tearing, certify_coordinates)
    {
        return Ok(Some(jacobian));
    }
    let mut jacobian = DMatrix::zeros(rows, columns);
    let recovered_rows = if certify_coordinates {
        tearing.causal_steps.len()
    } else {
        0
    };
    let mut recovered = DMatrix::zeros(recovered_rows, columns);
    let mut perturbed = Vec::with_capacity(rows);
    for (column, &tear_index) in tearing.tear_y_indices.iter().enumerate() {
        base.restore(tearing, y);
        let h = finite_difference_perturbation(base.tear(column), variable_scales[column]);
        y[tear_index] = base.tear(column) + h;
        if !model.torn_block_sweep(tearing, y, p, t, &mut perturbed)? || !all_finite(&perturbed) {
            base.restore(tearing, y);
            return Ok(None);
        }
        for row in 0..rows {
            jacobian[(row, column)] = (perturbed[row] - residual[row]) / h;
        }
        for row in 0..recovered_rows {
            let index = tearing.causal_steps[row].y_index;
            recovered[(row, column)] = (y[index] - base.causal(row)) / h;
        }
    }
    base.restore(tearing, y);
    if jacobian
        .iter()
        .chain(recovered.iter())
        .all(|value| value.is_finite())
    {
        Ok(Some(ReducedJacobian {
            residual: jacobian,
            recovered,
        }))
    } else {
        Ok(None)
    }
}

/// The reduced Jacobian of a tangent plan, when every entry is finite and no
/// row or column vanishes. A vanished row or column (a slope that is exactly
/// zero at a symmetric start, such as a squared norm at the origin) leaves the
/// Newton system singular; the finite-difference Jacobian's perturbation
/// reaches the nearby nonzero slope instead.
fn tangent_reduced_jacobian(
    exact: &rumoca_eval_solve::TornTangentJacobian,
    (rows, columns): (usize, usize),
    tearing: &solve::BlockTearing,
    certify_coordinates: bool,
) -> Option<ReducedJacobian> {
    let recovered_rows = if certify_coordinates {
        tearing.causal_steps.len()
    } else {
        0
    };
    let recovered = exact.recovered.get(..recovered_rows * columns)?;
    let finite = exact
        .residual
        .iter()
        .chain(recovered)
        .all(|value| value.is_finite());
    if !finite || exact.residual.len() != rows * columns {
        return None;
    }
    let residual = DMatrix::from_row_slice(rows, columns, &exact.residual);
    let vanished = residual
        .row_iter()
        .any(|row| row.iter().all(|value| *value == 0.0))
        || residual
            .column_iter()
            .any(|column| column.iter().all(|value| *value == 0.0));
    (!vanished).then(|| ReducedJacobian {
        residual,
        recovered: DMatrix::from_row_slice(recovered_rows, columns, recovered),
    })
}

/// Backtracking line search along the reduced Newton direction. Accepts the
/// first step that reaches tolerance or strictly reduces the scaled residual
/// norm, returning the residual at the accepted point.
fn line_search<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    tearing: &solve::BlockTearing,
    mut step: LineSearchStep<'_>,
) -> Result<Option<Vec<f64>>, RuntimeSolveError> {
    for _ in 0..TORN_BACKTRACK_STEPS {
        // Halving cannot recover progress once every tear update rounds away.
        if step
            .delta
            .iter()
            .enumerate()
            .all(|(tear, &delta)| step.base.tear(tear) + step.alpha * delta == step.base.tear(tear))
        {
            break;
        }
        if let Some(residual) = line_search_step(model, y, p, t, tearing, &step)? {
            return Ok(Some(residual));
        }
        step.alpha *= 0.5;
    }
    step.base.restore(tearing, y);
    Ok(None)
}

/// One backtracking candidate: the base point, Newton direction, acceptance
/// scales, and the step fraction under trial.
struct LineSearchStep<'a> {
    base: &'a TornValues,
    delta: &'a [f64],
    row_scales: &'a [f64],
    before: f64,
    tol: f64,
    alpha: f64,
}

fn recovered_correction_converged<M: ImplicitProjectionModel>(
    model: &M,
    base: &TornValues,
    tearing: &solve::BlockTearing,
    jacobian: &DMatrix<f64>,
    delta: &[f64],
    tol: f64,
) -> bool {
    tearing
        .causal_steps
        .iter()
        .enumerate()
        .all(|(row, causal)| {
            let correction = jacobian
                .row(row)
                .iter()
                .zip(delta)
                .map(|(a, b)| a * b)
                .sum();
            let scale = model_variable_scale(model, causal.y_index, base.causal(row));
            scaled_correction_converged(&[correction], &[scale], tol)
        })
}

/// Whether the undamped step moves every causal target within tolerance. The
/// trial runs in `y`, which holds the step's base on entry and on return.
fn causal_correction_converged<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    (p, t): (&[f64], f64),
    tearing: &solve::BlockTearing,
    step: &LineSearchStep<'_>,
) -> Result<bool, RuntimeSolveError> {
    if tearing.causal_steps.is_empty() {
        return Ok(true);
    }
    let accepted = line_search_step(model, y, p, t, tearing, step)?.is_some();
    let converged = accepted
        && tearing
            .causal_steps
            .iter()
            .enumerate()
            .all(|(row, causal)| {
                let base = step.base.causal(row);
                let scale = model_variable_scale(model, causal.y_index, base);
                scaled_correction_converged(&[y[causal.y_index] - base], &[scale], step.tol)
            });
    step.base.restore(tearing, y);
    Ok(converged)
}

/// Evaluate one backtracking candidate, returning its residual when accepted.
/// `y` holds the candidate point on acceptance and stays at the (partially
/// written) trial point otherwise; the caller resets it from `base` before
/// the next trial or on exhaustion.
fn line_search_step<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    tearing: &solve::BlockTearing,
    step: &LineSearchStep<'_>,
) -> Result<Option<Vec<f64>>, RuntimeSolveError> {
    step.base.restore(tearing, y);
    for (tear, (&tear_index, &direction)) in tearing
        .tear_y_indices
        .iter()
        .zip(step.delta.iter())
        .enumerate()
    {
        let candidate = step.base.tear(tear) + step.alpha * direction;
        if !candidate.is_finite() {
            return Ok(None);
        }
        y[tear_index] = candidate;
    }
    let mut residual = Vec::with_capacity(tearing.residual_rows.len());
    if !model.torn_block_sweep(tearing, y, p, t, &mut residual)? || !all_finite(&residual) {
        return Ok(None);
    }
    let norm = scaled_residual_norm(&residual, step.row_scales);
    let accepted = norm.is_finite()
        && (scaled_residual_converged(&residual, step.row_scales, step.tol) || norm < step.before);
    Ok(accepted.then_some(residual))
}

fn residual_row<M: ImplicitProjectionModel + ?Sized>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    row: usize,
) -> Result<Option<f64>, RuntimeSolveError> {
    Ok(model
        .eval_implicit_residual_row(row, y, p, t)?
        .filter(|value| value.is_finite()))
}

/// The coordinates a torn solve writes, as they were when saved: its tear
/// coordinates, then its causal targets. The sweep writes only causal targets
/// and the reduced Newton only tears, so restoring these restores `y`.
struct TornValues {
    values: Vec<f64>,
    tears: usize,
}

impl TornValues {
    fn save(tearing: &solve::BlockTearing, y: &[f64]) -> Self {
        let mut values =
            Vec::with_capacity(tearing.tear_y_indices.len() + tearing.causal_steps.len());
        values.extend(tearing.tear_y_indices.iter().map(|&index| y[index]));
        values.extend(tearing.causal_steps.iter().map(|step| y[step.y_index]));
        Self {
            values,
            tears: tearing.tear_y_indices.len(),
        }
    }

    fn tear(&self, tear: usize) -> f64 {
        self.values[tear]
    }

    fn causal(&self, step: usize) -> f64 {
        self.values[self.tears + step]
    }

    fn restore(&self, tearing: &solve::BlockTearing, y: &mut [f64]) {
        for (&index, &value) in tearing.tear_y_indices.iter().zip(&self.values) {
            y[index] = value;
        }
        for (step, &value) in tearing.causal_steps.iter().zip(&self.values[self.tears..]) {
            y[step.y_index] = value;
        }
    }

    /// Whether the solve moved a coordinate it writes.
    fn changed(&self, tearing: &solve::BlockTearing, y: &[f64]) -> bool {
        let tears = tearing.tear_y_indices.iter().copied();
        let causal = tearing.causal_steps.iter().map(|step| step.y_index);
        tears
            .chain(causal)
            .zip(&self.values)
            .any(|(index, &value)| y[index] != value)
    }
}
