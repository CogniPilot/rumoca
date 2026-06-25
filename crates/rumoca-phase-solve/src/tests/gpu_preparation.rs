use super::*;

#[test]
fn gpu_preparation_inlines_input_driven_algebraic_in_derivative_rhs() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("mask"), scalar_var("mask"));
    dae_model
        .variables
        .inputs
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), var("mask")),
        span,
        "state derivative reads derived field",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(
            rumoca_core::OpBinary::Sub,
            var("mask"),
            binary(rumoca_core::OpBinary::Add, var("u"), int_expr(1)),
        ),
        span,
        "input-derived explicit field",
    ));

    let runtime = lower_solve_problem(&dae_model).expect("runtime lowering should succeed");
    let runtime_mask_y = match runtime.layout.binding("mask") {
        Some(solve::ScalarSlot::Y { index, .. }) => index,
        other => panic!("mask should be a retained runtime algebraic Y slot: {other:?}"),
    };
    let runtime_rhs = scalar_program_block_fixture(&runtime.continuous.derivative_rhs);
    assert!(
        runtime_rhs.programs[0].iter().any(
            |op| matches!(op, solve::LinearOp::LoadY { index, .. } if *index == runtime_mask_y)
        ),
        "{:?}",
        runtime_rhs.programs[0]
    );

    let gpu = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        1,
        Some(span),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect("GPU-preparation lowering should succeed");
    let gpu_u_p = match gpu.layout.binding("u") {
        Some(solve::ScalarSlot::P { index, .. }) => index,
        other => panic!("input u should be a P slot: {other:?}"),
    };
    assert_eq!(gpu.layout.y_scalars(), 1);
    assert_eq!(gpu.layout.binding("mask"), None);
    assert!(gpu.continuous.residual.is_empty());
    assert!(gpu.continuous.algebraic_projection_plan.blocks.is_empty());
    let gpu_rhs = scalar_program_block_fixture(&gpu.continuous.derivative_rhs);
    assert!(
        gpu_rhs.programs[0]
            .iter()
            .any(|op| matches!(op, solve::LinearOp::LoadP { index, .. } if *index == gpu_u_p)),
        "{:?}",
        gpu_rhs.programs[0]
    );
    assert!(
        !gpu_rhs.programs[0]
            .iter()
            .any(|op| matches!(op, solve::LinearOp::LoadY { .. })),
        "{:?}",
        gpu_rhs.programs[0]
    );
}
