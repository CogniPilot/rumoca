//! Numerical policy of the Model-Exchange algebraic projection.
//!
//! The in-process ME kernel (`rumoca-solver`) and every generated C component
//! (`rumoca-phase-codegen`) solve the same construction-issued projection
//! stages. They share this one owner for every iteration budget, step bound,
//! Jacobian source, and the torn affine elimination's admission and
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

/// Whether a colored projection Jacobian evaluates each application program
/// once with one tangent lane per color that calls it
/// (`rumoca_ir_solve::ColoredTangentPlan`) instead of once per color. Each
/// lane equals the one-direction call it replaces, so the Jacobian is the
/// same; the primal of every program runs once.
pub const COLORED_TANGENT_LANES: bool = true;

/// The projection Jacobian sources an evaluator is built with: the policy
/// constants unless a caller evaluates another choice with
/// [`with_jacobian_sources`]. A torn block's tear Jacobian always comes from
/// its tangent plan; it is not a choice.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct JacobianSources {
    /// [`COLORED_TANGENT_LANES`]
    pub colored_lanes: bool,
}

impl JacobianSources {
    /// The sources this policy selects.
    pub const POLICY: Self = Self {
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

/// Most alternate charts one reduced constraint group issues. Ranked single
/// exchanges past this cap are recorded as withheld (SPEC_0040 STRUCT-T07
/// constraint-fold chart rows).
pub const MAX_ALTERNATE_CHARTS_PER_GROUP: usize = 4;

/// Fraction of its reference conditioning (its conditioning when it became
/// active) at or above which the active chart is kept without testing an
/// alternate. A chart's `sigma` falls smoothly toward zero as it approaches
/// its fold, so requesting a change below this keeps the transfer strictly
/// inside the regular regime.
pub const CHART_SWITCH_KEEP: f64 = 0.5;

/// Factor by which an alternate's conditioning must exceed the active chart's
/// to be adopted. Switching back needs the same factor the other way, so the
/// ratio must swing by its square: the hysteresis band.
pub const CHART_SWITCH_IMPROVEMENT: f64 = 1.5;

/// Multiple of a chart's singular threshold that bounds its regular region.
/// An alternate below it is never adopted, and an active chart that settles
/// below it at an accepted step has crossed its fold.
pub const CHART_REGULAR_MULTIPLE: f64 = 1.0e6;

/// Admission of the torn affine elimination, shared by the linked kernel and
/// every generated C component: the block is a sparse candidate or a small
/// dense system its issued tearing reduces (fewer tears than unknowns), its
/// issued reduced system is small and dense, and a promotion capacity exists.
/// The result is that capacity. A small block the tearing reduces is
/// eliminated like a large one, so the construction's causal order is what
/// every call executes.
#[must_use]
pub fn affine_elimination_capacity(
    layout: &rumoca_ir_solve::AffineEliminationLayout,
) -> Option<usize> {
    use crate::tensor_policy::{LinearSolveKernel, select_linear_solve_kernel};
    let n = layout.pattern().rows() as usize;
    let block_admitted = match select_linear_solve_kernel(n, layout.pattern()) {
        Ok(LinearSolveKernel::SparseCandidate) => true,
        Ok(LinearSolveKernel::SmallDense) => layout.tears().len() < n,
        _ => false,
    };
    let admitted = block_admitted
        && matches!(
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
