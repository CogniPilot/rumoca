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

/// Whether the reduced Newton takes its tear Jacobian from the block's issued
/// tangent plan (`rumoca_ir_solve::TornTangentPlan`): each causal step's
/// tangent follows from the implicit function theorem on its row in sweep
/// order, evaluated from multi-lane tangent programs. A block without a plan,
/// or a point where the plan declines, differences the causal sweep.
///
/// The exact tear Jacobian is an accuracy option: each tangent lane costs about
/// a primal pass, so it is costlier than the partial finite-difference columns
/// of the causal sweep, and it changes Newton iterates and so trajectories at
/// the refresh tolerance level. It is off, so the linked kernel and the
/// generated C share one Jacobian source.
pub const TORN_TANGENT_JACOBIAN: bool = false;

/// Whether a colored projection Jacobian evaluates each application program
/// once with one tangent lane per color that calls it
/// (`rumoca_ir_solve::ColoredTangentPlan`) instead of once per color. Each
/// lane equals the one-direction call it replaces, so the Jacobian is the
/// same; the primal of every program runs once.
pub const COLORED_TANGENT_LANES: bool = true;

/// The projection Jacobian sources an evaluator is built with: the policy
/// constants unless a caller evaluates another choice with
/// [`with_jacobian_sources`].
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct JacobianSources {
    /// [`TORN_TANGENT_JACOBIAN`]
    pub torn_tangent: bool,
    /// [`COLORED_TANGENT_LANES`]
    pub colored_lanes: bool,
}

impl JacobianSources {
    /// The sources this policy selects.
    pub const POLICY: Self = Self {
        torn_tangent: TORN_TANGENT_JACOBIAN,
        colored_lanes: COLORED_TANGENT_LANES,
    };
}

thread_local! {
    static JACOBIAN_SOURCES: std::cell::Cell<JacobianSources> =
        const { std::cell::Cell::new(JacobianSources::POLICY) };
}

/// The Jacobian sources of evaluators built on this thread: the policy's,
/// or those of an enclosing [`with_jacobian_sources`].
#[must_use]
pub fn jacobian_sources() -> JacobianSources {
    JACOBIAN_SOURCES.with(std::cell::Cell::get)
}

/// Run `body` with evaluators built on this thread taking `sources`, and
/// restore the previous sources afterwards, on unwinding included. An
/// evaluator keeps the sources it was built with, so this compares the
/// accuracy options of one model without changing the policy.
pub fn with_jacobian_sources<R>(sources: JacobianSources, body: impl FnOnce() -> R) -> R {
    struct Restore(JacobianSources);
    impl Drop for Restore {
        fn drop(&mut self) {
            JACOBIAN_SOURCES.with(|current| current.set(self.0));
        }
    }
    let _restore = Restore(JACOBIAN_SOURCES.with(|current| current.replace(sources)));
    body()
}

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

/// Whether a block projection call evaluates each residual program's
/// invariant part once and its dependent part per pass (SPEC_0043 §6a block
/// residual split). Every pass computes the same bits either way.
pub const BLOCK_RESIDUAL_SPLIT: bool = true;

thread_local! {
    static BLOCK_RESIDUAL_SPLIT_ENABLED: std::cell::Cell<bool> =
        const { std::cell::Cell::new(BLOCK_RESIDUAL_SPLIT) };
}

/// Whether block projection calls on this thread use the residual split: the
/// policy's choice, or that of an enclosing [`with_block_residual_split`].
#[must_use]
pub fn block_residual_split() -> bool {
    BLOCK_RESIDUAL_SPLIT_ENABLED.with(std::cell::Cell::get)
}

/// Run `body` with block projection calls on this thread using the residual
/// split or not, and restore the previous choice afterwards, on unwinding
/// included.
pub fn with_block_residual_split<R>(enabled: bool, body: impl FnOnce() -> R) -> R {
    struct Restore(bool);
    impl Drop for Restore {
        fn drop(&mut self) {
            BLOCK_RESIDUAL_SPLIT_ENABLED.with(|current| current.set(self.0));
        }
    }
    let _restore = Restore(BLOCK_RESIDUAL_SPLIT_ENABLED.with(|current| current.replace(enabled)));
    body()
}
