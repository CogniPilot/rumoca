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

#[test]
fn gpu_preparation_rejects_nonstructured_initial_assignment_shape() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "der(x) = 0",
    ));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            binary(rumoca_core::OpBinary::Sub, var("x"), int_expr(7)),
            span,
            "x = 7",
        ));

    let gpu = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        1,
        Some(span),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect_err("GPU preparation must fail closed instead of scalarizing initialization rows");
    assert!(matches!(gpu, crate::lower::LowerError::Unsupported { .. }));
}

#[test]
fn gpu_preparation_ignores_automatic_fixed_start_rows() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        binary(rumoca_core::OpBinary::Sub, der(var("x")), int_expr(0)),
        span,
        "der(x) = 0",
    ));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            binary(rumoca_core::OpBinary::Sub, var("x"), int_expr(7)),
            span,
            "fixed start initialization for x",
        ));
    dae_model
        .initialization
        .equation_provenance
        .push(dae::InitializationEquationProvenance::FixedStart);

    let gpu = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        1,
        Some(span),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect("GPU preparation should retain declared fixed starts without scalar initialization");
    assert!(gpu.initialization.residual.is_empty());
    assert!(gpu.initialization.direct_families.is_empty());
    assert!(gpu.initialization.row_targets.is_empty());
}

#[test]
fn gpu_preparation_rejects_partial_fixed_start_target_coverage() {
    let span = solve_test_span();
    let mut dae_model = dae::Dae::default();
    for name in ["x", "y"] {
        dae_model
            .variables
            .states
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
        dae_model.continuous.equations.push(dae::Equation::residual(
            binary(rumoca_core::OpBinary::Sub, der(var(name)), int_expr(0)),
            span,
            "derivative",
        ));
    }
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            binary(rumoca_core::OpBinary::Sub, var("x"), int_expr(7)),
            span,
            "fixed start initialization for x",
        ));
    dae_model
        .initialization
        .equation_provenance
        .push(dae::InitializationEquationProvenance::FixedStart);

    let error = lower_solve_problem_with_solver_len_and_model_span_and_profile(
        &dae_model,
        2,
        Some(span),
        SolveProblemLoweringProfile::GpuPreparation,
    )
    .expect_err("partial GPU initialization coverage must fail closed");
    assert!(error.to_string().contains("cover every solver Y slot"));
}

#[test]
fn gpu_initial_projection_rejects_degenerate_structured_binder() {
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 1,
            step: 1,
        }],
    };

    let error = gpu_corner_cell_index(&domain, 0, solve_test_span())
        .expect_err("direct GPU initial projection must fail closed for one-cell binders");
    assert!(matches!(
        error,
        crate::lower::LowerError::Unsupported { .. }
    ));
    assert!(
        error
            .to_string()
            .contains("non-degenerate structured binder")
    );
}
