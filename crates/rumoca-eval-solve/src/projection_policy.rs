//! Numerical policy of the Model-Exchange algebraic projection.
//!
//! The in-process ME kernel (`rumoca-solver`) and every generated C component
//! (`rumoca-phase-codegen`) solve the same construction-issued projection
//! stages. They share this one owner for every iteration budget, step bound,
//! finite-difference rule, and the torn affine elimination's admission and
//! tear-promotion capacity, so a generated component cannot converge under
//! a different policy than the linked kernel it is compared against.

/// Scaled residual and correction tolerance of every continuous algebraic
/// refresh issued by the ME component.
pub const ALGEBRAIC_REFRESH_TOLERANCE: f64 = 1.0e-10;

/// Outer sweep budget of one algebraic refresh before the branch-continuity
/// multiplier is applied.
pub const ALGEBRAIC_REFRESH_MAX_ITERS: usize = 32;

/// Iteration budget of the affine refinement and of the unlimited projection.
pub const ALGEBRAIC_PROJECTION_MAX_ITERS: usize = 32;

/// Largest fraction of its own magnitude (or declared scale) one unknown may
/// move in one accepted dense block Newton step.
pub const ALGEBRAIC_PROJECTION_TRUST_FRACTION: f64 = 0.25;

/// Budget multiplier compensating the trust fraction's bounded progress.
pub const ALGEBRAIC_PROJECTION_ITER_FACTOR: usize = 4;

/// Maximum reduced Newton iterations over the tear variables of one block.
pub const TORN_OUTER_MAX_ITERS: usize = 64;

/// Maximum step halvings in the reduced Newton line search.
pub const TORN_BACKTRACK_STEPS: usize = 24;

/// Relative step of the reduced finite-difference Jacobian.
pub const FINITE_DIFFERENCE_RELATIVE_STEP: f64 = 1.0e-7;

/// A finite, sign-stable finite-difference perturbation scaled to the larger
/// of the variable's magnitude, its declared scale, and one.
#[must_use]
pub fn finite_difference_perturbation(value: f64, scale: f64) -> f64 {
    let magnitude = value.abs().max(scale.abs()).max(1.0);
    let step = magnitude * FINITE_DIFFERENCE_RELATIVE_STEP;
    if value < 0.0 { -step } else { step }
}

/// Growth factor of the torn affine elimination's tear capacity over its
/// construction-issued tear count.
pub const TORN_PROMOTION_FACTOR: usize = 2;

/// Promoted tears always available beyond the issued tear count.
pub const TORN_PROMOTION_MIN_EXTRA: usize = 4;

/// Largest reduced system a torn affine elimination may promote to.
pub const TORN_PROMOTION_LIMIT: usize = 32;

/// Tear capacity of a torn affine elimination issued with `base` tears: the
/// issued tears plus the causal steps it may promote in place when a guard is
/// nonzero or a pivot is weak, before the reduction declines to the full
/// solve. `None` when no reduction of that size is admissible.
#[must_use]
pub fn torn_promotion_capacity(base: usize) -> Option<usize> {
    if base == 0 || base > TORN_PROMOTION_LIMIT {
        return None;
    }
    Some(
        base.saturating_mul(TORN_PROMOTION_FACTOR)
            .max(base.saturating_add(TORN_PROMOTION_MIN_EXTRA))
            .min(TORN_PROMOTION_LIMIT),
    )
}

/// Admission of the torn affine elimination, shared by the linked kernel and
/// every generated C component: the block is a sparse candidate, its issued
/// reduced system is small and dense, and a promotion capacity exists. The
/// result is that capacity.
#[must_use]
pub fn affine_elimination_capacity(
    layout: &rumoca_ir_solve::AffineEliminationLayout,
) -> Option<usize> {
    use crate::tensor_policy::{LinearSolveKernel, select_linear_solve_kernel};
    let n = layout.pattern().rows() as usize;
    let admitted = matches!(
        select_linear_solve_kernel(n, layout.pattern()),
        Ok(LinearSolveKernel::SparseCandidate)
    ) && matches!(
        select_linear_solve_kernel(layout.tears().len(), layout.reduced_pattern()),
        Ok(LinearSolveKernel::SmallDense)
    );
    admitted
        .then(|| torn_promotion_capacity(layout.tears().len()))
        .flatten()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn perturbation_follows_the_sign_and_the_largest_magnitude() {
        assert_eq!(finite_difference_perturbation(0.0, 0.0), 1.0e-7);
        assert_eq!(finite_difference_perturbation(-2.0, 0.5), -2.0e-7);
        assert_eq!(finite_difference_perturbation(0.5, 4.0), 4.0e-7);
    }

    #[test]
    fn promotion_capacity_doubles_small_tear_sets_up_to_the_limit() {
        assert_eq!(torn_promotion_capacity(0), None);
        assert_eq!(torn_promotion_capacity(1), Some(5));
        assert_eq!(torn_promotion_capacity(4), Some(8));
        assert_eq!(torn_promotion_capacity(14), Some(28));
        assert_eq!(torn_promotion_capacity(16), Some(32));
        assert_eq!(torn_promotion_capacity(20), Some(32));
        assert_eq!(torn_promotion_capacity(32), Some(32));
        assert_eq!(torn_promotion_capacity(33), None);
    }
}
