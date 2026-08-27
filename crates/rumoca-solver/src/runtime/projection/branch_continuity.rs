//! Step-limited algebraic projection that preserves nonlinear branch continuity.

use rumoca_ir_solve as solve;

use super::step_limit::{
    ALGEBRAIC_PROJECTION_ITER_FACTOR, ALGEBRAIC_PROJECTION_TRUST_FRACTION, StepLimit,
};
use super::{
    AlgebraicProjectionArgs, ImplicitProjectionModel, RuntimeSolveError,
    project_algebraics_with_plan_inner, projection_unknown_values,
    restore_projection_unknown_values,
};

/// Project `y` onto the algebraic manifold while limiting each Newton correction
/// relative to the incoming coordinate. The incoming algebraics are the continuation
/// seed from the preceding solver coordinate; bounding every step is what prevents a
/// converged solve from silently selecting a distant root of a multi-root constitutive
/// system. Residual convergence alone cannot prove branch continuity.
///
/// A failed projection restores every unknown exactly and remains a loud error. The
/// larger iteration budget compensates for the deliberately bounded progress.
pub(super) fn project_with_branch_continuity<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    y: &mut [f64],
    args: AlgebraicProjectionArgs<'_>,
    max_iters: usize,
    certify_coordinates: bool,
) -> Result<(), RuntimeSolveError> {
    let snapshot = projection_unknown_values(plan, y);
    let limited_iters = max_iters.saturating_mul(ALGEBRAIC_PROJECTION_ITER_FACTOR);
    let result = project_algebraics_with_plan_inner(
        model,
        plan,
        y,
        args,
        limited_iters,
        StepLimit::Fraction(ALGEBRAIC_PROJECTION_TRUST_FRACTION),
        certify_coordinates,
    );
    if result.is_err() {
        restore_projection_unknown_values(plan, y, &snapshot);
    }
    result
}
