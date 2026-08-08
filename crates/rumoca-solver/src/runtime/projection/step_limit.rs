//! Per-iteration bound on a Newton step taken by the algebraic projection.

/// Per-iteration bound on how far one unknown may move, as a fraction of its own
/// magnitude, in the branch-preserving algebraic projection.
///
/// A block whose rows contain a steeply curved constitutive law (the soft-magnetic
/// `mu_r(B)` of `Modelica.Magnetic.FluxTubes` is the reference case: relative
/// permeability falls by an order of magnitude across the saturation knee) has a
/// Newton direction whose linear extrapolation lands far outside the branch it was
/// taken on. The residual *decreases* there — the row is scaled by its own steep
/// derivative, so a large error in the row's target reads small — and the damped
/// Newton then walks a non-physical branch (`mu_r < 0`, hence a negative reluctance)
/// until it stalls. Capping every per-iteration movement makes the projection
/// follow the continuation branch instead of jumping to another converged root.
///
/// Provenance: the stall is *latent and pre-existing*, not a regression of any one
/// commit. The same block, the same guess and the same stall reproduce at the parent
/// of the commit whose certification run first reported it, and on every build once
/// the output grid is fine enough.
///
/// What changed was only whether the stall is reached, and that is a semantic
/// correction, not nondeterminism. `time_event_instant`
/// (`rumoca-phase-dae/src/construction/analysis/expression_events.rs`) now refuses an
/// `instant <= 0.0`, leaving a start-instant time relation to its zero crossing per
/// MLS §8.5 — an event is the instant an event generating expression *changes* value,
/// and initialization has already fixed that one. `Modelica.Blocks.Sources.Sine`
/// reaches this through `if time < startTime` with `startTime = 0`, so the example
/// that surfaced this used to get a scheduled stop at `t = 0` and now does not. That
/// stop was restarting the integrator, and the restart incidentally kept the warm
/// starts close enough for the unlimited Newton to survive. Removing it is correct;
/// the projection's inability to recover from a distant warm start is the defect, and
/// it is what this bound fixes.
///
/// The fraction is a globalization constant, and the root the projection converges to does
/// not depend on it: sweeping 0.01 to 0.9 reaches the same solution and changes only
/// the evaluation count.
pub(super) const ALGEBRAIC_PROJECTION_TRUST_FRACTION: f64 = 0.25;

/// Iteration-budget multiplier for the step-limited projection: a pass that advances
/// each unknown by at most [`ALGEBRAIC_PROJECTION_TRUST_FRACTION`] of its own
/// magnitude needs proportionally more steps to cover the same distance.
pub(super) const ALGEBRAIC_PROJECTION_ITER_FACTOR: usize = 4;

/// Per-iteration bound on a block's Newton step.
#[derive(Clone, Copy, PartialEq)]
pub(super) enum StepLimit {
    /// Complete Newton step for a construction-certified affine block.
    None,
    /// No unknown may move more than this fraction of its own magnitude (or of
    /// its declared scale, whichever is larger) in one accepted step.
    Fraction(f64),
}

impl StepLimit {
    /// Largest step fraction admissible for `delta` under this limit.
    pub(super) fn initial_alpha(
        self,
        y: &[f64],
        y_indices: &[usize],
        delta: &[f64],
        scales: &[f64],
    ) -> f64 {
        let fraction = match self {
            Self::Fraction(fraction) => fraction,
            Self::None => return 1.0,
        };
        let mut alpha = 1.0_f64;
        for ((&y_index, &step), &scale) in y_indices.iter().zip(delta).zip(scales) {
            let Some(&value) = y.get(y_index) else {
                continue;
            };
            let bound = fraction * value.abs().max(scale.abs()).max(f64::MIN_POSITIVE);
            if step.abs() > bound {
                alpha = alpha.min(bound / step.abs());
            }
        }
        if alpha.is_finite() && alpha > 0.0 {
            alpha.min(1.0)
        } else {
            1.0
        }
    }
}
