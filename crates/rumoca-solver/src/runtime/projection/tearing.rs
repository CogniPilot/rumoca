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
use super::{ImplicitProjectionModel, ProjectionBlockUpdate, RuntimeSolveError};

/// Maximum reduced Newton iterations over the tear variables.
const TORN_OUTER_MAX_ITERS: usize = 64;
/// Maximum step halvings in the reduced Newton line search.
const TORN_BACKTRACK_STEPS: usize = 24;

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
    let snapshot = y.to_vec();

    let mut residual = Vec::with_capacity(tearing.residual_rows.len());
    if !model.torn_block_sweep(tearing, y, p, t, &mut residual)? {
        y.copy_from_slice(&snapshot);
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
        y.copy_from_slice(&snapshot);
        return Ok(None);
    }

    let block = TornBlock {
        model,
        p,
        t,
        tearing,
    };
    for _ in 0..TORN_OUTER_MAX_ITERS {
        match advance_torn_newton(
            &block,
            y,
            &residual,
            &variable_scales,
            tol,
            certify_coordinates,
        )? {
            TornStep::Settled => {
                let changed = slices_differ(y, &snapshot);
                return Ok(Some(ProjectionBlockUpdate {
                    changed,
                    settled: true,
                }));
            }
            TornStep::Advanced(next) => residual = next,
            TornStep::Decline => {
                y.copy_from_slice(&snapshot);
                return Ok(None);
            }
        }
    }

    // The reduced Newton exhausted its iterations without meeting tolerance.
    // Restore the incoming values so the dense fallback starts exactly where it
    // would have without the torn attempt.
    y.copy_from_slice(&snapshot);
    Ok(None)
}

/// The block under solve: the model, the parameter vector and time it is
/// evaluated at, and the tearing plan that says which rows are residuals and
/// which unknowns are back-substituted.
///
/// Every reduced-Newton primitive evaluates this same block, so they carry it
/// as one borrowed value instead of threading the four parts separately. The
/// working point `y` stays a separate argument because it is written.
struct TornBlock<'a, M: ImplicitProjectionModel + ?Sized> {
    model: &'a M,
    p: &'a [f64],
    t: f64,
    tearing: &'a solve::BlockTearing,
}

impl<M: ImplicitProjectionModel + ?Sized> TornBlock<'_, M> {
    /// One sweep of the block at `y`: back-substitution followed by the reduced
    /// residual rows, written into `residual_out`.
    fn sweep(&self, y: &mut [f64], residual_out: &mut Vec<f64>) -> Result<bool, RuntimeSolveError> {
        self.model
            .torn_block_sweep(self.tearing, y, self.p, self.t, residual_out)
    }
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
fn advance_torn_newton<M: ImplicitProjectionModel>(
    block: &TornBlock<'_, M>,
    y: &mut [f64],
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
    let base = y.to_vec();
    let Some(jacobian) = reduced_jacobian(block, y, residual, variable_scales, &base)? else {
        return Ok(TornStep::Decline);
    };
    let row_scales = jacobian_row_scales(&jacobian, variable_scales, variable_scales, None);
    let converged = scaled_residual_converged(residual, &row_scales, tol);
    let delta = scaled_newton_delta(ScaledNewtonSystem {
        jacobian: &jacobian,
        residual,
        row_scales: &row_scales,
        variable_scales,
        structure: None,
        tolerance: tol,
    });
    let Some(delta) = delta.filter(|delta| delta.iter().all(|value| value.is_finite())) else {
        return Ok(TornStep::Decline);
    };
    // Coordinate certification also requires the correction to be within
    // tolerance; the ordinary boundary projection settles on the residual alone.
    if converged
        && (!certify_coordinates
            || scaled_correction_converged(delta.as_slice(), variable_scales, tol))
    {
        return Ok(TornStep::Settled);
    }
    let before = scaled_residual_norm(residual, &row_scales);
    let search = ReducedLineSearch {
        base: &base,
        delta: delta.as_slice(),
        row_scales: &row_scales,
        before,
        tol,
    };
    match line_search(block, y, &search)? {
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

/// Total finite-difference Jacobian of the reduced residual with respect to the
/// tear variables, back-substituting through each perturbation so the causal
/// unknowns track their tear dependence. `base` holds `y` at the current point.
fn reduced_jacobian<M: ImplicitProjectionModel>(
    block: &TornBlock<'_, M>,
    y: &mut [f64],
    residual: &[f64],
    variable_scales: &[f64],
    base: &[f64],
) -> Result<Option<DMatrix<f64>>, RuntimeSolveError> {
    let rows = block.tearing.residual_rows.len();
    let columns = block.tearing.tear_y_indices.len();
    let mut jacobian = DMatrix::zeros(rows, columns);
    let mut perturbed = Vec::with_capacity(rows);
    for (column, &tear_index) in block.tearing.tear_y_indices.iter().enumerate() {
        y.copy_from_slice(base);
        let h = perturbation(base[tear_index], variable_scales[column]);
        y[tear_index] = base[tear_index] + h;
        if !block.sweep(y, &mut perturbed)? || !all_finite(&perturbed) {
            y.copy_from_slice(base);
            return Ok(None);
        }
        for row in 0..rows {
            jacobian[(row, column)] = (perturbed[row] - residual[row]) / h;
        }
    }
    y.copy_from_slice(base);
    if jacobian.iter().all(|value| value.is_finite()) {
        Ok(Some(jacobian))
    } else {
        Ok(None)
    }
}

/// Backtracking line search along the reduced Newton direction. Accepts the
/// first step that reaches tolerance or strictly reduces the scaled residual
/// norm, returning the residual at the accepted point.
fn line_search<M: ImplicitProjectionModel>(
    block: &TornBlock<'_, M>,
    y: &mut [f64],
    search: &ReducedLineSearch<'_>,
) -> Result<Option<Vec<f64>>, RuntimeSolveError> {
    let mut alpha = 1.0;
    for _ in 0..TORN_BACKTRACK_STEPS {
        if let Some(residual) = line_search_step(block, y, search, alpha)? {
            return Ok(Some(residual));
        }
        alpha *= 0.5;
    }
    y.copy_from_slice(search.base);
    Ok(None)
}

/// The fixed data of one backtracking search: the base point, the Newton
/// direction, and the scales and norms the acceptance test reads. The step
/// fraction under trial varies per candidate and is passed separately.
struct ReducedLineSearch<'a> {
    base: &'a [f64],
    delta: &'a [f64],
    row_scales: &'a [f64],
    before: f64,
    tol: f64,
}

/// Evaluate one backtracking candidate, returning its residual when accepted.
/// `y` holds the candidate point on acceptance and stays at the (partially
/// written) trial point otherwise; the caller resets it from `base` before
/// the next trial or on exhaustion.
fn line_search_step<M: ImplicitProjectionModel>(
    block: &TornBlock<'_, M>,
    y: &mut [f64],
    search: &ReducedLineSearch<'_>,
    alpha: f64,
) -> Result<Option<Vec<f64>>, RuntimeSolveError> {
    y.copy_from_slice(search.base);
    for (&tear_index, &direction) in block.tearing.tear_y_indices.iter().zip(search.delta.iter()) {
        let candidate = search.base[tear_index] + alpha * direction;
        if !candidate.is_finite() {
            return Ok(None);
        }
        y[tear_index] = candidate;
    }
    let mut residual = Vec::with_capacity(block.tearing.residual_rows.len());
    if !block.sweep(y, &mut residual)? || !all_finite(&residual) {
        return Ok(None);
    }
    let norm = scaled_residual_norm(&residual, search.row_scales);
    let accepted = norm.is_finite()
        && (scaled_residual_converged(&residual, search.row_scales, search.tol)
            || norm < search.before);
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

fn slices_differ(a: &[f64], b: &[f64]) -> bool {
    a != b
}

/// A finite, sign-stable perturbation for finite differencing, scaled to the
/// variable's magnitude.
fn perturbation(value: f64, scale: f64) -> f64 {
    let magnitude = value.abs().max(scale.abs()).max(1.0);
    let step = magnitude * 1.0e-7;
    if value < 0.0 { -step } else { step }
}
