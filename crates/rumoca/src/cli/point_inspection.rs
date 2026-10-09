//! The `--inspect` kinds that need no simulation window: structure, point
//! evaluation, the Jacobians, the steady objective gradient, and the
//! linearization. `compile` and `sim` share them.

use std::path::Path;

use anyhow::Result;

use super::{
    GradMode, InspectFormat, InspectKind, SimCommandArgs, SimulateSolverMode, inspect_at_spec,
    sim_window, simulate_solver_or_auto,
};
use crate::{DaeCompilationResult, sim_inspect, sim_trajectory};

/// One inspection of the lowered model at a point (or of its structure).
pub(super) struct PointInspection<'a> {
    pub kind: InspectKind,
    pub dae: &'a rumoca_compile::compile::Dae,
    pub model: &'a str,
    pub at: &'a str,
    pub solver: SimulateSolverMode,
    pub objective: Option<&'a str>,
    pub adjoint: bool,
    pub json: bool,
}

/// Run one point inspection. A trajectory inspection integrates a run, so it is
/// refused here and handled by `sim` before it reaches this point.
pub(super) fn run_point_inspection(inspection: PointInspection<'_>) -> Result<()> {
    let PointInspection {
        kind,
        dae,
        model,
        at,
        solver,
        objective,
        adjoint,
        json,
    } = inspection;
    if json
        && !matches!(
            kind,
            InspectKind::Jacobian | InspectKind::ObjectiveGradient | InspectKind::Linearize
        )
    {
        anyhow::bail!(
            "`--format json` is only supported with \
             `--inspect jacobian|objective-gradient|linearize`"
        );
    }
    match kind {
        InspectKind::Structure => sim_inspect::run_structure_dump(dae, model, solver.into()),
        InspectKind::Eval => sim_inspect::run_eval_at(dae, model, at, solver.into()),
        InspectKind::Jacobian => sim_inspect::run_jacobian(dae, model, at, solver.into(), json),
        InspectKind::ObjectiveGradient => {
            sim_inspect::run_objective_gradient(dae, model, at, objective, adjoint, json)
        }
        InspectKind::Linearize => {
            let (overrides, t) = sim_inspect::parse_eval_at_spec(at)?;
            sim_trajectory::run_linearize(dae, model, &overrides, t, json)
        }
        InspectKind::TrajectorySensitivity => anyhow::bail!(
            "`--inspect trajectory-sensitivity` integrates a run; use \
             `rumoca sim --inspect trajectory-sensitivity`"
        ),
    }
}

/// Run the `sim --inspect` kinds: the trajectory inspections that integrate the
/// run's window, and the point inspections.
pub(super) fn run_sim_inspection(
    args: &SimCommandArgs,
    kind: InspectKind,
    result: &DaeCompilationResult,
    model: &str,
    workspace_root: Option<&Path>,
) -> Result<()> {
    let solver = simulate_solver_or_auto(args.solver, result.experiment_solver.as_deref())?;
    let objective_request = sim_trajectory::ObjectiveRequest {
        integral: &args.integral,
        terminal: &args.terminal,
        fit_data: args.fit_data.as_deref(),
    };
    let json = matches!(args.format, InspectFormat::Json);
    let adjoint = matches!(args.grad_mode, GradMode::Adjoint);
    let trajectory_kind = matches!(kind, InspectKind::TrajectorySensitivity)
        || (matches!(kind, InspectKind::ObjectiveGradient) && objective_request.is_requested());
    if trajectory_kind {
        require_trajectory_plugin(args.solver)?;
        let (t_start, t_end) = sim_window(args, result);
        let trajectory = sim_trajectory::TrajectoryRun {
            dae: result.dae.as_ref(),
            model,
            window: (t_start, t_end),
            dt: args.dt,
            atol: args.atol,
            rtol: args.rtol,
            wrt: &args.wrt,
            checkpoint_budget: args.checkpoint_budget,
            workspace_root,
            source_map: result.source_map.as_ref(),
        };
        return if matches!(kind, InspectKind::TrajectorySensitivity) {
            sim_trajectory::run_trajectory_sensitivity(
                &trajectory,
                args.output.as_deref().map(Path::new),
                json,
            )
        } else {
            sim_trajectory::run_trajectory_objective_gradient(
                &trajectory,
                &objective_request,
                adjoint,
                json,
            )
        };
    }
    run_point_inspection(PointInspection {
        kind,
        dae: result.dae.as_ref(),
        model,
        at: inspect_at_spec(args.at.as_deref()),
        solver,
        objective: args.objective.as_deref(),
        adjoint,
        json,
    })
}

/// The sensitivity systems are advanced by the explicit Dormand-Prince plugin
/// (`rk-like`); an implicit solver would need the second-order linearization of
/// the augmented system, which is not constructed, so it is refused rather
/// than silently replaced.
fn require_trajectory_plugin(solver: Option<SimulateSolverMode>) -> Result<()> {
    match solver {
        None | Some(SimulateSolverMode::Auto | SimulateSolverMode::RkLike) => Ok(()),
        Some(SimulateSolverMode::Bdf) => anyhow::bail!(
            "trajectory sensitivities are advanced by the explicit `rk-like` (Dormand-Prince) \
             plugin; `--solver bdf` is not supported for them"
        ),
    }
}
