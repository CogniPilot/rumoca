//! Tangent evaluators the runtime prepares once per projection plan.

use super::*;

/// The tangent evaluator of each projection block's tearing over the
/// solver-Y JVP rows `jvp`, aligned with `plan.blocks`.
pub(super) fn torn_tangent_evaluators(
    plan: &solve::AlgebraicProjectionPlan,
    jvp: &solve::ScalarProgramBlock,
    compiled: bool,
) -> Rc<[Option<rumoca_eval_solve::TornTangentEvaluator>]> {
    // One-direction plans all read the same JVP rows; prepare them once.
    let mut shared = None;
    plan.blocks
        .iter()
        .map(|block| {
            let tearing = block.tearing.as_ref()?;
            // A backend-compiled JVP answers each direction natively, faster
            // than the interpreted lane widening and with the same values.
            let plan = if compiled {
                solve::TornTangentPlan::derive_directional(tearing, jvp)
            } else {
                solve::TornTangentPlan::derive(tearing, jvp)
            }
            .ok()?;
            rumoca_eval_solve::TornTangentEvaluator::sharing_directions(plan, jvp, &mut shared).ok()
        })
        .collect()
}

/// The colored tangent evaluator of each projection block's issued Jacobian
/// application, aligned with `plan.blocks` and their structures.
pub(super) fn colored_tangent_evaluators(
    plan: &solve::AlgebraicProjectionPlan,
    structures: &solve::ContinuousStructuralArtifacts,
) -> Rc<[Option<rumoca_eval_solve::ColoredTangentEvaluator>]> {
    plan.blocks
        .iter()
        .zip(structures.algebraic_projection())
        .map(|(block, structure)| {
            if !rumoca_eval_solve::projection_policy::jacobian_sources().colored_lanes {
                return None;
            }
            let application = structure.jacobian_application().filter(|application| {
                application.rows() == block.rows && application.y_indices() == block.y_indices
            })?;
            let plan = solve::ColoredTangentPlan::derive(application).ok()?;
            Some(rumoca_eval_solve::ColoredTangentEvaluator::new(plan))
        })
        .collect()
}
