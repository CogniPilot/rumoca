use rumoca_ir_solve as solve;

/// Unsealed continuous pieces used only to exercise the production checked
/// refresh-owner and continuous-system constructors in MLIR integration tests.
pub(crate) struct ContinuousFixtureParts {
    pub(crate) implicit_rhs: solve::ComputeBlock,
    pub(crate) implicit_row_targets: Vec<Option<solve::ScalarSlot>>,
    pub(crate) algebraic_projection_plan: solve::AlgebraicProjectionPlan,
    pub(crate) residual: solve::ComputeBlock,
    pub(crate) manifold_residual: solve::ComputeBlock,
    pub(crate) manifold_projection_plan: solve::AlgebraicProjectionPlan,
    pub(crate) derivative_rhs: solve::ComputeBlock,
}

/// Seal a continuous fixture through the same refresh-owner construction path
/// used by Solve lowering. No incomplete executable system escapes this helper.
pub(crate) fn checked_continuous_system(
    solve_layout: &solve::SolveLayout,
    mut parts: ContinuousFixtureParts,
    discrete: &solve::DiscreteSolveSystem,
    events: &solve::SolveEventPartition,
    clocks: &solve::SolveClockPartition,
) -> solve::ContinuousSolveSystem {
    let refresh_plans = rumoca_eval_solve::refresh_plan::build_continuous_refresh_plans(
        solve_layout,
        (
            &parts.implicit_rhs,
            &parts.implicit_row_targets,
            &mut parts.algebraic_projection_plan,
            &parts.derivative_rhs,
        ),
        discrete,
        events,
        clocks,
    )
    .expect("MLIR fixture issues checked continuous refresh owners");
    solve::ContinuousSolveSystem::construct(
        solve_layout,
        solve::ContinuousSolveSystemInputs::new(
            parts.implicit_rhs,
            parts.implicit_row_targets,
            parts.algebraic_projection_plan,
            parts.residual,
            (parts.manifold_residual, parts.manifold_projection_plan),
            parts.derivative_rhs,
            refresh_plans,
        ),
    )
    .expect("MLIR fixture seals one complete continuous system")
}
