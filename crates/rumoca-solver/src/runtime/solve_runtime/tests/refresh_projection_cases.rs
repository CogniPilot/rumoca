//! Refresh-projection regression cases: authoritative residual tolerance, Newton
//! backtracking across expression-domain boundaries, rank-deficient and
//! singular starts, staged-seed coverage, and missing-producer rejection.
//!
//! Split out of the parent `tests` module to keep each file within the
//! SPEC_0021 line budget; every case still shares the parent fixtures.

use super::*;

#[test]
fn refresh_preserves_incoming_coordinate_when_residual_is_within_tolerance() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "y".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 2,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![
                scale_and_offset_assignment_residual_row(0, 1, 2.0, 0.0),
                scale_and_offset_assignment_residual_row(1, 0, 0.75, 0.0),
            ],
            "accepted_refresh_iterate.mo",
        )),
        implicit_row_targets: vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![0, 1],
                tearing: None,
            }],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let initialization = solve::InitializationSolveSystem::empty();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 2, 0),
            solve_layout,
            continuous,
            initialization,
            discrete,
            events,
            clocks,
        )
        .expect("accepted-iterate fixture satisfies the checked root contract"),
        initial_y: vec![2.0e-7, 1.0e-7],
        solver_nominals: vec![1.0; 2],
        ..empty_binary64_first_product_model()
    };
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    assert!(!runtime.algebraic_refresh.causal_solution_certified());
    let mut solver_y = model.initial_y().to_vec();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-6, 4)
        .expect("the preserved residual system should be projected");

    assert!((solver_y[0] - 2.0e-7).abs() <= f64::EPSILON);
    assert!((solver_y[1] - 1.0e-7).abs() <= f64::EPSILON);
}

fn damped_newton_jvp_rows() -> Vec<Vec<solve::LinearOp>> {
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
    ]
}

#[test]
fn refresh_newton_backtracks_across_expression_domain_boundary() {
    // The first finite sweep yields z=0, x=1. An undamped Newton step for
    // z=sqrt(x), x=1-10*z crosses to x<0; backtracking must keep the iterate
    // inside sqrt's domain while reducing the simultaneous residual.
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["z".to_string(), "x".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 2,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![
                sqrt_assignment_residual_row(0, 1),
                scale_and_offset_assignment_residual_row(1, 0, -10.0, 1.0),
            ],
            "damped_newton_domain.mo",
        )),
        implicit_row_targets: vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![0, 1],
                tearing: None,
            }],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let initialization = solve::InitializationSolveSystem::empty();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 2, 0),
            solve_layout,
            continuous,
            initialization,
            discrete,
            events,
            clocks,
        )
        .expect("damped-Newton fixture satisfies the checked root contract"),
        initial_y: vec![0.0, 0.0],
        solver_nominals: vec![1.0; 2],
        ..empty_binary64_first_product_model()
    };
    let model = set_test_implicit_jvp(
        model,
        damped_newton_jvp_rows(),
        "damped_newton_domain_jvp.mo",
    );
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    let mut solver_y = model.initial_y().to_vec();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-10, 8)
        .expect("damped Newton should remain in the square-root domain");

    let expected_z = (104.0_f64.sqrt() - 10.0) / 2.0;
    assert!((solver_y[0] - expected_z).abs() <= 1.0e-8);
    assert!((solver_y[1] - expected_z * expected_z).abs() <= 1.0e-8);
}

fn bilinear_start_jvp_rows() -> Vec<Vec<solve::LinearOp>> {
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
    ]
}

#[test]
fn refresh_projects_rank_deficient_bilinear_start() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "a".to_string(), "b".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 3,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![
                shifted_variable_residual_row(1, 2.0),
                shifted_variable_residual_row(2, 3.0),
                bilinear_residual_row(),
            ],
            "bilinear_start.mo",
        )),
        implicit_row_targets: vec![
            Some(solve::scalar_slot_y(0)),
            Some(solve::scalar_slot_y(1)),
            Some(solve::scalar_slot_y(2)),
        ],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1, 2],
                y_indices: vec![0, 1, 2],
                tearing: None,
            }],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let initialization = solve::InitializationSolveSystem::empty();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 3, 0),
            solve_layout,
            continuous,
            initialization,
            discrete,
            events,
            clocks,
        )
        .expect("bilinear fixture satisfies the checked root contract"),
        initial_y: vec![0.0; 3],
        solver_nominals: vec![1.0; 3],
        ..empty_binary64_first_product_model()
    };
    let model = set_test_implicit_jvp(model, bilinear_start_jvp_rows(), "bilinear_start_jvp.mo");
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    let mut solver_y = model.initial_y().to_vec();

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
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 1,
        compiled_parameter_len: 1,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
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
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
                tearing: None,
            }],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let initialization = solve::InitializationSolveSystem::empty();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 1, 1),
            solve_layout,
            continuous,
            initialization,
            discrete,
            events,
            clocks,
        )
        .expect("semantic-error fixture satisfies the checked root contract"),
        initial_y: vec![7.0],
        solver_nominals: vec![1.0],
        parameters: vec![0.0],
        ..empty_binary64_first_product_model()
    };
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    let mut solver_y = vec![19.0];

    let error = runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-10, 1)
        .expect_err("a missing runtime input is not a Newton-recoverable error");

    assert_eq!(solver_y, vec![19.0]);
    assert!(error.to_string().contains("missing p[0]"));
}

#[test]
fn refresh_projects_complete_system_with_empty_causal_schedule() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 1,
        ..Default::default()
    };
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Binary {
                    dst: 1,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 0,
                },
                solve::LinearOp::Const { dst: 2, value: 9.0 },
                solve::LinearOp::Binary {
                    dst: 3,
                    op: solve::BinaryOp::Sub,
                    lhs: 1,
                    rhs: 2,
                },
                solve::LinearOp::StoreOutput { src: 3 },
            ]],
            "empty_causal_projection.mo",
        )),
        implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
                tearing: None,
            }],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let initialization = solve::InitializationSolveSystem::empty();
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 1, 0),
            solve_layout,
            continuous,
            initialization,
            discrete,
            events,
            clocks,
        )
        .expect("empty-causal fixture satisfies the checked root contract"),
        initial_y: vec![2.0],
        solver_nominals: vec![1.0],
        ..empty_binary64_first_product_model()
    };
    let model = set_test_implicit_jvp(
        model,
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 2.0 },
            solve::LinearOp::LoadY { dst: 1, index: 0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::LoadSeed { dst: 3, index: 0 },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Mul,
                lhs: 2,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ]],
        "empty_causal_projection_jvp.mo",
    );
    let model = std::sync::Arc::new(model);
    let runtime =
        SolveRuntime::new(std::sync::Arc::clone(&model)).expect("valid runtime should prepare");
    assert!(runtime.algebraic_refresh.rows().is_empty());
    let mut solver_y = model.initial_y().to_vec();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-12, 8)
        .expect("the simultaneous residual must run without a causal schedule");

    assert!((solver_y[0] - 3.0).abs() <= 1.0e-12);
}

#[test]
fn construction_rejects_missing_algebraic_implicit_row() {
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "alias".to_string()],
            ..Default::default()
        },
        state_scalar_count: 1,
        algebraic_scalar_count: 1,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let mut continuous = crate::test_support::ContinuousSystemFixture {
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![derivative_placeholder_row(1)],
            "missing_producer_derivative.mo",
        )),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let error = continuous
        .try_refresh_plans(&solve_layout, &discrete, &events, &clocks)
        .expect_err("incomplete implicit algebraic ownership must not receive an owner proof");

    assert!(
        error
            .to_string()
            .contains("missing a producer for Y index 1"),
        "error should identify the uncovered algebraic inventory: {error}"
    );
}
