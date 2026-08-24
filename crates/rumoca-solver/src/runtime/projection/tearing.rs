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

    if !back_substitute(model, y, p, t, tearing)? {
        y.copy_from_slice(&snapshot);
        return Ok(None);
    }

    let variable_scales = tearing
        .tear_y_indices
        .iter()
        .map(|&index| model_variable_scale(model, index, y[index]))
        .collect::<Vec<_>>();

    let Some(mut residual) = reduced_residual(model, y, p, t, tearing)?.take_if_finite() else {
        y.copy_from_slice(&snapshot);
        return Ok(None);
    };

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
    let base = y.to_vec();
    let Some(jacobian) =
        reduced_jacobian(model, y, p, t, tearing, residual, variable_scales, &base)?
    else {
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
    match line_search(
        model,
        y,
        p,
        t,
        tearing,
        &base,
        delta.as_slice(),
        &row_scales,
        before,
        tol,
    )? {
        Some(next) => Ok(TornStep::Advanced(next)),
        None => Ok(TornStep::Decline),
    }
}

/// Evaluate every causal step in order, writing each recovered unknown into
/// `y`. Returns `false` when any step cannot be evaluated to a finite value, in
/// which case the caller declines the torn solve and falls back to the dense
/// block Newton.
fn back_substitute<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    tearing: &solve::BlockTearing,
) -> Result<bool, RuntimeSolveError> {
    for step in &tearing.causal_steps {
        if !solve_causal_step(model, y, p, t, step.row, step.y_index)? {
            return Ok(false);
        }
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
fn solve_causal_step<M: ImplicitProjectionModel>(
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

/// Reduced Newton residual over the tear system's residual rows.
struct ReducedResidual(Vec<f64>);

impl ReducedResidual {
    fn take_if_finite(self) -> Option<Vec<f64>> {
        self.0
            .iter()
            .all(|value| value.is_finite())
            .then_some(self.0)
    }
}

fn reduced_residual<M: ImplicitProjectionModel>(
    model: &M,
    y: &[f64],
    p: &[f64],
    t: f64,
    tearing: &solve::BlockTearing,
) -> Result<ReducedResidual, RuntimeSolveError> {
    let mut values = Vec::with_capacity(tearing.residual_rows.len());
    for &row in &tearing.residual_rows {
        match residual_row(model, y, p, t, row)? {
            Some(value) => values.push(value),
            None => {
                values.push(f64::NAN);
            }
        }
    }
    Ok(ReducedResidual(values))
}

/// Total finite-difference Jacobian of the reduced residual with respect to the
/// tear variables, back-substituting through each perturbation so the causal
/// unknowns track their tear dependence. `base` holds `y` at the current point.
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
    base: &[f64],
) -> Result<Option<DMatrix<f64>>, RuntimeSolveError> {
    let rows = tearing.residual_rows.len();
    let columns = tearing.tear_y_indices.len();
    let mut jacobian = DMatrix::zeros(rows, columns);
    for (column, &tear_index) in tearing.tear_y_indices.iter().enumerate() {
        y.copy_from_slice(base);
        let h = perturbation(base[tear_index], variable_scales[column]);
        y[tear_index] = base[tear_index] + h;
        if !back_substitute(model, y, p, t, tearing)? {
            y.copy_from_slice(base);
            return Ok(None);
        }
        let Some(perturbed) = reduced_residual(model, y, p, t, tearing)?.take_if_finite() else {
            y.copy_from_slice(base);
            return Ok(None);
        };
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
// SPEC_0021: Exception - a backtracking search threads model, storage, tearing, the base point, step, row scales, and tolerance together.
#[allow(clippy::too_many_arguments)]
fn line_search<M: ImplicitProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    tearing: &solve::BlockTearing,
    base: &[f64],
    delta: &[f64],
    row_scales: &[f64],
    before: f64,
    tol: f64,
) -> Result<Option<Vec<f64>>, RuntimeSolveError> {
    let mut alpha = 1.0;
    for _ in 0..TORN_BACKTRACK_STEPS {
        y.copy_from_slice(base);
        let mut finite = true;
        for (&tear_index, &step) in tearing.tear_y_indices.iter().zip(delta.iter()) {
            let candidate = base[tear_index] + alpha * step;
            if !candidate.is_finite() {
                finite = false;
                break;
            }
            y[tear_index] = candidate;
        }
        if finite
            && back_substitute(model, y, p, t, tearing)?
            && let Some(residual) = reduced_residual(model, y, p, t, tearing)?.take_if_finite()
        {
            let norm = scaled_residual_norm(&residual, row_scales);
            if norm.is_finite()
                && (scaled_residual_converged(&residual, row_scales, tol) || norm < before)
            {
                return Ok(Some(residual));
            }
        }
        alpha *= 0.5;
    }
    y.copy_from_slice(base);
    Ok(None)
}

fn residual_row<M: ImplicitProjectionModel>(
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
