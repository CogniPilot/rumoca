//! Refresh-projection regression cases: affine coordinates, Newton
//! backtracking across expression-domain boundaries, rank-deficient and
//! singular starts, staged dependencies, and missing-producer rejection.
//!
//! Split out of the parent `tests` module to keep each file within the
//! SPEC_0021 line budget; every case still shares the parent fixtures.

use super::*;

#[test]
fn projection_affinity_treats_earlier_block_values_as_coefficients() {
    let rows = vec![
        shifted_variable_residual_row(0, 2.0),
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::LoadY { dst: 1, index: 0 },
            solve::LinearOp::LoadY { dst: 2, index: 2 },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Mul,
                lhs: 1,
                rhs: 2,
            },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ],
        scale_and_offset_assignment_residual_row(2, 1, 0.25, 1.0),
    ];
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: ["coefficient", "a", "b"].map(str::to_string).to_vec(),
                    ..Default::default()
                },
                algebraic_scalar_count: 3,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    rows,
                    "block_affinity.mo",
                )),
                implicit_row_targets: (0..3)
                    .map(|index| Some(solve::scalar_slot_y(index)))
                    .collect(),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![
                        solve::AlgebraicProjectionBlock {
                            rows: vec![0],
                            y_indices: vec![0],
                            tearing: None,
                            alternate_charts: Vec::new(),
                        },
                        solve::AlgebraicProjectionBlock {
                            rows: vec![1, 2],
                            y_indices: vec![1, 2],
                            tearing: None,
                            alternate_charts: Vec::new(),
                        },
                    ],
                },
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![2.0, 4.0, 2.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new_fixture(&model).unwrap();
    let projection = RefreshProjectionModel {
        seed_linearizations: None,
        runtime: &runtime,
        plan: &runtime.algebraic_refresh.simultaneous_plan,
        block_indices: &runtime.algebraic_refresh.simultaneous_block_indices,
        plan_validated: true,
        jacobian_v: ProjectionJacobian::SolverY {
            block: &runtime.implicit_projection_jacobian_v,
            scalar: &runtime.implicit_projection_scalar_jacobian_v,
        },
    };
    assert!(projection.algebraic_projection_block_is_affine(0));
    assert!(
        projection.algebraic_projection_block_is_affine(1),
        "a - coefficient*b and b - 0.25*a - 1 are affine in the block unknowns a and b"
    );
}

#[test]
fn refresh_solves_affine_coordinates_even_when_the_seed_residual_is_small() {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string(), "y".to_string()],
                    ..Default::default()
                },
                algebraic_scalar_count: 2,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        scale_and_offset_assignment_residual_row(0, 1, 2.0, 0.0),
                        scale_and_offset_assignment_residual_row(1, 0, 0.75, 0.0),
                    ],
                    "accepted_refresh_iterate.mo",
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                ],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![2.0e-7, 1.0e-7],
        ..Default::default()
    };
    set_complete_test_projection_plan(&mut model);
    let runtime = SolveRuntime::new_fixture(&model).expect("valid runtime should prepare");
    assert!(!runtime.algebraic_refresh.causal_solution_certified);
    let mut solver_y = model.initial_y.clone();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-6, 4)
        .expect("the preserved residual system should be projected");

    // x = 2*y and y = 0.75*x have the unique solution x = y = 0.
    assert_eq!(solver_y, vec![0.0, 0.0]);
}

#[test]
fn refresh_newton_backtracks_across_expression_domain_boundary() {
    // The first finite sweep yields z=0, x=1. An undamped Newton step for
    // z=sqrt(x), x=1-10*z crosses to x<0; backtracking must keep the iterate
    // inside sqrt's domain while reducing the simultaneous residual.
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["z".to_string(), "x".to_string()],
                    ..Default::default()
                },
                algebraic_scalar_count: 2,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        sqrt_assignment_residual_row(0, 1),
                        scale_and_offset_assignment_residual_row(1, 0, -10.0, 1.0),
                    ],
                    "damped_newton_domain.mo",
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                ],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0, 0.0],
        ..Default::default()
    };
    set_test_implicit_jvp(
        &mut model,
        vec![
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::LoadSeed { dst: 1, index: 1 },
                solve::LinearOp::Const { dst: 2, value: 2.0 },
                solve::LinearOp::LoadY { dst: 3, index: 1 },
                solve::LinearOp::Unary {
                    dst: 4,
                    op: solve::UnaryOp::Sqrt,
                    arg: 3,
                },
                solve::LinearOp::Binary {
                    dst: 5,
                    op: solve::BinaryOp::Mul,
                    lhs: 2,
                    rhs: 4,
                },
                solve::LinearOp::Binary {
                    dst: 6,
                    op: solve::BinaryOp::Div,
                    lhs: 1,
                    rhs: 5,
                },
                solve::LinearOp::Binary {
                    dst: 7,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 6,
                },
                solve::LinearOp::StoreOutput { src: 7 },
            ],
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 1 },
                solve::LinearOp::Const {
                    dst: 1,
                    value: 10.0,
                },
                solve::LinearOp::LoadSeed { dst: 2, index: 0 },
                solve::LinearOp::Binary {
                    dst: 3,
                    op: solve::BinaryOp::Mul,
                    lhs: 1,
                    rhs: 2,
                },
                solve::LinearOp::Binary {
                    dst: 4,
                    op: solve::BinaryOp::Add,
                    lhs: 0,
                    rhs: 3,
                },
                solve::LinearOp::StoreOutput { src: 4 },
            ],
        ],
        "damped_newton_domain_jvp.mo",
    );
    set_complete_test_projection_plan(&mut model);
    let runtime = SolveRuntime::new_fixture(&model).expect("valid runtime should prepare");
    let mut solver_y = model.initial_y.clone();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-10, 8)
        .expect("damped Newton should remain in the square-root domain");

    let expected_z = (104.0_f64.sqrt() - 10.0) / 2.0;
    assert!((solver_y[0] - expected_z).abs() <= 1.0e-8);
    assert!((solver_y[1] - expected_z * expected_z).abs() <= 1.0e-8);
}

#[test]
fn refresh_projects_rank_deficient_bilinear_start() {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string(), "a".to_string(), "b".to_string()],
                    ..Default::default()
                },
                algebraic_scalar_count: 3,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        shifted_variable_residual_row(1, 2.0),
                        shifted_variable_residual_row(2, 3.0),
                        bilinear_residual_row(),
                    ],
                    "bilinear_start.mo",
                )),
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0; 3],
        ..Default::default()
    };
    set_test_implicit_jvp(
        &mut model,
        vec![
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 2 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::LoadY { dst: 1, index: 1 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::LoadY { dst: 3, index: 0 },
                solve::LinearOp::LoadSeed { dst: 4, index: 1 },
                solve::LinearOp::Binary {
                    dst: 5,
                    op: solve::BinaryOp::Mul,
                    lhs: 3,
                    rhs: 4,
                },
                solve::LinearOp::Binary {
                    dst: 6,
                    op: solve::BinaryOp::Add,
                    lhs: 2,
                    rhs: 5,
                },
                solve::LinearOp::LoadSeed { dst: 7, index: 2 },
                solve::LinearOp::Binary {
                    dst: 8,
                    op: solve::BinaryOp::Sub,
                    lhs: 6,
                    rhs: 7,
                },
                solve::LinearOp::StoreOutput { src: 8 },
            ],
        ],
        "bilinear_start_jvp.mo",
    );
    set_complete_test_projection_plan(&mut model);
    let runtime = SolveRuntime::new_fixture(&model).expect("valid runtime should prepare");
    let mut solver_y = model.initial_y.clone();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-10, 8)
        .expect("shared projection should leave the rank-deficient start");

    assert!((solver_y[0] - 1.5).abs() <= 1.0e-8);
    assert!((solver_y[1] - 2.0).abs() <= 1.0e-8);
    assert!((solver_y[2] - 3.0).abs() <= 1.0e-8);
}

#[test]
fn refresh_iteration_propagates_semantic_errors_and_restores_snapshot() {
    let span = test_span("refresh_semantic_error.mo");
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string()],
                    ..Default::default()
                },
                algebraic_scalar_count: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(
                    solve::ScalarProgramBlock::with_source_span(
                        vec![vec![
                            solve::LinearOp::LoadY { dst: 0, index: 0 },
                            solve::LinearOp::LoadP { dst: 1, index: 0 },
                            solve::LinearOp::Binary {
                                dst: 2,
                                op: solve::BinaryOp::Sub,
                                lhs: 0,
                                rhs: 1,
                            },
                            solve::LinearOp::StoreOutput { src: 2 },
                        ]],
                        span.require_provenance("refresh semantic-error fixture")
                            .expect("fixture span is source-backed"),
                    )
                    .expect("fixture program is computable"),
                ),
                implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![7.0],
        ..Default::default()
    };
    set_complete_test_projection_plan(&mut model);
    let runtime = SolveRuntime::new_fixture(&model).expect("valid runtime should prepare");
    let mut solver_y = vec![19.0];

    let error = runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-10, 1)
        .expect_err("a missing runtime input is not a Newton-recoverable error");

    assert_eq!(solver_y, vec![19.0]);
    assert!(error.to_string().contains("missing p[0]"));
    assert!(!seed_error_allows_projection(&error));
}

#[test]
fn singular_affine_seed_falls_back_to_preserved_projection() {
    let span = test_span("singular_affine_seed.mo");
    let error: RuntimeSolveError = EvalSolveError::SingularTargetAssignment {
        row: 7,
        target_y_index: 0,
        coefficient: -0.0,
        span: Some(span),
    }
    .into();

    assert!(matches!(
        &error,
        RuntimeSolveError::RefreshTargetSingular {
            row: 7,
            target_y_index: 0,
            coefficient,
            span: Some(error_span),
        } if *coefficient == 0.0 && *error_span == span
    ));
    assert!(seed_error_allows_projection(&error));
}

#[test]
fn staged_projection_still_rejects_a_genuine_backward_dependency() {
    let mut model = mode_dependent_repivot_model();
    let layout = &mut model.problem.solve_layout;
    layout.solver_maps.names.push("late_input".to_owned());
    layout.algebraic_scalar_count += 1;
    model.initial_y.push(0.0);
    let mut residual = mode_dependent_repivot_residual_rows();
    residual[0][5] = solve::LinearOp::LoadY { dst: 5, index: 2 };
    residual.push(shifted_variable_residual_row(2, 1.0));
    let continuous = &mut model.problem.continuous;
    continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(spanned_block(
        residual,
        "late_coupled_input.mo",
    ));
    continuous
        .implicit_row_targets
        .push(Some(solve::scalar_slot_y(2)));
    continuous
        .algebraic_projection_plan
        .blocks
        .push(solve::AlgebraicProjectionBlock {
            rows: vec![2],
            y_indices: vec![2],
            tearing: None,
            alternate_charts: Vec::new(),
        });
    let mut jvp = mode_dependent_repivot_jvp_rows();
    jvp[0].pop();
    jvp[0].extend([
        solve::LinearOp::LoadSeed { dst: 5, index: 2 },
        solve::LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::Sub,
            lhs: 4,
            rhs: 5,
        },
        solve::LinearOp::StoreOutput { src: 6 },
    ]);
    jvp.push(vec![
        solve::LinearOp::LoadSeed { dst: 0, index: 2 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]);
    set_test_implicit_jvp(&mut model, jvp, "late_coupled_input_jvp.mo");
    derive_test_structural_artifacts(&mut model);
    let runtime = SolveRuntime::new_fixture(&model).unwrap();
    assert_eq!(
        runtime
            .continuous_structural
            .algebraic_invalidates_earlier(1),
        Some(true)
    );
    assert!(!runtime.value_stage_schedule_is_certified(&runtime.algebraic_refresh));
}

#[test]
fn refresh_projects_complete_system_with_empty_causal_schedule() {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string()],
                    ..Default::default()
                },
                algebraic_scalar_count: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![shifted_variable_residual_row(0, 3.0)],
                    "empty_causal_projection.mo",
                )),
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0],
        ..Default::default()
    };
    set_test_implicit_jvp(
        &mut model,
        vec![vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "empty_causal_projection_jvp.mo",
    );
    set_complete_test_projection_plan(&mut model);
    derive_test_structural_artifacts(&mut model);
    let runtime = SolveRuntime::new_fixture(&model).expect("valid runtime should prepare");
    assert!(runtime.algebraic_refresh.rows.is_empty());
    assert!(runtime.value_stage_schedule_is_certified(&runtime.algebraic_refresh));
    let mut solver_y = model.initial_y.clone();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-12, 4)
        .expect("the simultaneous residual must run without a causal schedule");

    assert!((solver_y[0] - 3.0).abs() <= 1.0e-12);
}

#[test]
fn runtime_rejects_missing_algebraic_implicit_row() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string(), "alias".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 1,
                algebraic_scalar_count: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                derivative_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![derivative_placeholder_row(1)],
                    "missing_producer_derivative.mo",
                )),
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0, 0.0],
        ..Default::default()
    };
    let err = match SolveRuntime::new_fixture(&model) {
        Ok(_) => panic!("incomplete implicit algebraic system must be rejected"),
        Err(err) => err,
    };

    assert!(
        err.to_string()
            .contains("implicit algebraic system is missing a producer for Y index 1"),
        "error should identify the missing implicit row: {err}"
    );
}
