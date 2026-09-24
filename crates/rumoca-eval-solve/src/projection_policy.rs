//! Numerical policy of the Model-Exchange algebraic projection.
//!
//! The in-process ME kernel (`rumoca-solver`) and every generated C component
//! (`rumoca-phase-codegen`) solve the same construction-issued projection
//! stages. They share this one owner for every iteration budget, step bound,
//! and finite-difference rule, so a generated component cannot converge under
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn perturbation_follows_the_sign_and_the_largest_magnitude() {
        assert_eq!(finite_difference_perturbation(0.0, 0.0), 1.0e-7);
        assert_eq!(finite_difference_perturbation(-2.0, 0.5), -2.0e-7);
        assert_eq!(finite_difference_perturbation(0.5, 4.0), 4.0e-7);
    }
}
