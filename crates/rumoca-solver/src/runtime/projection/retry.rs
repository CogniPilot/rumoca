//! Second, step-limited attempt at an algebraic projection the unlimited damped
//! Newton could not settle.

use rumoca_ir_solve as solve;

use super::step_limit::{
    ALGEBRAIC_PROJECTION_RETRY_ITER_FACTOR, ALGEBRAIC_PROJECTION_TRUST_FRACTION, StepLimit,
};
use super::{
    AlgebraicProjectionArgs, ImplicitProjectionModel, RuntimeSolveError,
    project_algebraics_with_plan_inner, projection_unknown_values,
    restore_projection_unknown_values,
};

/// Project `y` onto the algebraic manifold, retrying a stalled solve with a bounded
/// step before giving up.
///
/// The retry solves the *same* rows to the *same* tolerance from the *same* guess;
/// only the per-iteration step length differs. It is a second attempt, never a
/// fallback: a retry that does not converge re-raises the first pass's failure and
/// restores every unknown, so an unsolved projection stays loud.
///
/// Any `Err` from the first pass is retried, including structural ones (an out-of-range
/// row index, say) that cannot possibly succeed the second time. That costs one wasted
/// pass on a run that is failing anyway. Discriminating retryable numeric failures from
/// structural ones wants the treatment `seed_error_allows_projection`
/// (`solve_runtime/refresh_projection.rs`) already gives the seeding path; until that
/// classification is shared, retrying everything is the conservative choice.
pub(super) fn project_with_step_limited_retry<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    y: &mut [f64],
    args: AlgebraicProjectionArgs<'_>,
    max_iters: usize,
) -> Result<(), RuntimeSolveError> {
    let snapshot = projection_unknown_values(plan, y);
    let Err(unlimited) =
        project_algebraics_with_plan_inner(model, plan, y, args, max_iters, StepLimit::None)
    else {
        return Ok(());
    };
    restore_projection_unknown_values(plan, y, &snapshot);
    let retry_iters = max_iters.saturating_mul(ALGEBRAIC_PROJECTION_RETRY_ITER_FACTOR);
    match project_algebraics_with_plan_inner(
        model,
        plan,
        y,
        args,
        retry_iters,
        StepLimit::Fraction(ALGEBRAIC_PROJECTION_TRUST_FRACTION),
    ) {
        Ok(()) => Ok(()),
        Err(limited) => {
            restore_projection_unknown_values(plan, y, &snapshot);
            tracing::debug!(
                target: "rumoca_solver::projection",
                time = args.time,
                unlimited = %unlimited,
                step_limited = %limited,
                "algebraic projection failed on both the unlimited and the step-limited pass"
            );
            Err(unlimited)
        }
    }
}
