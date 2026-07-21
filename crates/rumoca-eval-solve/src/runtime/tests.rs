use super::*;

fn valid_algebraic_refresh_plan(
    model: &solve::SolveModel,
    block: &PreparedScalarProgramBlock,
) -> RefreshPlan {
    match build_algebraic_refresh_plan(model, block) {
        Ok(plan) => plan,
        Err(error) => panic!("valid algebraic refresh plan should build: {error}"),
    }
}

fn test_span(name: &'static str) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(name), 1, 2)
}

fn spanned_block(rows: Vec<Vec<solve::LinearOp>>, name: &'static str) -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_source_span(rows, test_span(name))
}

fn mirror_scalar_implicit_jvp(model: &mut solve::SolveModel) {
    model.artifacts.continuous.implicit_jacobian_v = solve::ComputeBlock::from_scalar_program_block(
        model
            .artifacts
            .continuous
            .implicit_jacobian_v_scalar
            .clone(),
    );
}

fn set_test_implicit_jvp(
    model: &mut solve::SolveModel,
    rows: Vec<Vec<solve::LinearOp>>,
    name: &'static str,
) {
    model.artifacts.continuous.implicit_jacobian_v_scalar = spanned_block(rows, name);
    mirror_scalar_implicit_jvp(model);
}

fn set_complete_test_projection_plan(model: &mut solve::SolveModel) {
    let state_count = model.state_scalar_count();
    let solver_count = model.solver_scalar_count();
    let rows = (state_count..solver_count).collect::<Vec<_>>();
    model.problem.continuous.algebraic_projection_plan = if rows.is_empty() {
        solve::AlgebraicProjectionPlan::default()
    } else {
        solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                y_indices: rows.clone(),
                rows,
            }],
        }
    };
}

fn set_causal_test_projection_plan(model: &mut solve::SolveModel) {
    let state_count = model.state_scalar_count();
    let solver_count = model.solver_scalar_count();
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: (state_count..solver_count)
            .map(|index| solve::AlgebraicProjectionBlock {
                rows: vec![index],
                y_indices: vec![index],
            })
            .collect(),
    };
}

fn warm_start_test_model() -> solve::SolveModel {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string(), "z".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 1,
                algebraic_scalar_count: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        derivative_placeholder_row(0),
                        shifted_variable_residual_row(1, 0.0),
                    ],
                    "solver_y_warm_start.mo",
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                ],
                derivative_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![derivative_placeholder_row(0)],
                    "solver_y_warm_start_derivative.mo",
                )),
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![1.0, 2.0],
        ..Default::default()
    };
    set_test_implicit_jvp(
        &mut model,
        vec![
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        "solver_y_warm_start_jvp.mo",
    );
    set_complete_test_projection_plan(&mut model);
    model
}

#[test]
fn solver_y_warm_start_preserves_algebraic_guess() {
    let model = warm_start_test_model();
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let mut solver_y = vec![10.0, 42.0];

    runtime
        .update_solver_y_guess_from_state(&mut solver_y, &[3.0])
        .expect("state update should preserve a compatible algebraic guess");

    assert_eq!(solver_y, vec![3.0, 42.0]);
}

#[test]
fn solver_y_warm_start_rejects_layout_mismatch() {
    let model = warm_start_test_model();
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let mut solver_y = vec![9.0];

    let error = runtime
        .update_solver_y_guess_from_state(&mut solver_y, &[3.0])
        .expect_err("an established warm start must match the Solve-IR layout");

    assert!(error.to_string().contains("expected 2, got 1"));
    assert_eq!(solver_y, vec![9.0]);
}

#[test]
fn runtime_new_reports_invalid_native_stride_metadata() {
    let span =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("bad.mo"), 3, 8);
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 1,
            step: 1,
        }],
    };
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::Map {
            domain: domain.clone(),
            output_map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                .expect("valid dense output map"),
            base_ops: vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: vec![solve::AffineStencilLoadStride {
                op_position: 99,
                terms: Vec::new(),
            }],
            const_strides: Vec::new(),
            metadata: solve::TensorNodeMetadata::default(),
            span,
        }],
    };
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: block,
                ..Default::default()
            },
            ..Default::default()
        },
        ..Default::default()
    };

    let error = match SolveRuntime::new(&model) {
        Ok(_) => panic!("invalid native stride metadata should fail runtime preparation"),
        Err(error) => error,
    };
    assert!(
        error
            .to_string()
            .contains("native map family stride op position 99 out of bounds for 2 ops"),
        "error should explain invalid native metadata: {error}"
    );
    assert_eq!(error.source_span(), Some(span));
}

#[test]
fn refresh_plan_does_not_let_residual_target_shadow_assignment_row() {
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
                        non_assignment_targeted_residual_row(),
                        assignment_residual_row(),
                    ],
                    "refresh_shadow.mo",
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(1)),
                    Some(solve::scalar_slot_y(1)),
                ],
                ..Default::default()
            },
            ..Default::default()
        },
        ..Default::default()
    };
    set_complete_test_projection_plan(&mut model);
    let block =
        PreparedScalarProgramBlock::from_compute_block(&model.problem.continuous.implicit_rhs)
            .expect("valid implicit RHS should prepare");

    let plan = valid_algebraic_refresh_plan(&model, &block);

    assert_eq!(plan.rows.len(), 1);
    assert_eq!(plan.rows[0].row_idx, 1);
    assert_eq!(plan.rows[0].target_index, 1);
    assert!(!plan.causal_solution_certified);
    assert_eq!(plan.simultaneous_plan.blocks.len(), 1);
    assert_eq!(plan.simultaneous_plan.blocks[0].rows, vec![0, 1]);
    assert_eq!(plan.simultaneous_plan.blocks[0].y_indices, vec![0, 1]);
}

#[test]
fn refresh_plan_accepts_scaled_affine_residual_target() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string(), "a".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 1,
                algebraic_scalar_count: 1,
                parameter_count: 2,
                compiled_parameter_len: 2,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        derivative_placeholder_row(0),
                        scaled_assignment_residual_row(),
                    ],
                    "scaled_affine_residual.mo",
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                ],
                ..Default::default()
            },
            ..Default::default()
        },
        ..Default::default()
    };
    let block =
        PreparedScalarProgramBlock::from_compute_block(&model.problem.continuous.implicit_rhs)
            .expect("valid implicit RHS should prepare");

    let plan = valid_algebraic_refresh_plan(&model, &block);
    let value = block
        .eval_target_assignment_row_with_context(
            1,
            1,
            &[0.0, 0.0],
            &[6.0, 2.0],
            0.0,
            RowEvalContext::default(),
        )
        .expect("scaled residual should evaluate");

    assert_eq!(plan.rows.len(), 1);
    assert_eq!(plan.rows[0].row_idx, 1);
    assert_eq!(plan.rows[0].target_index, 1);
    assert_eq!(value, Some(3.0));
}

#[test]
fn batched_assignment_refresh_preserves_row_order_dependencies() {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string(), "a".to_string(), "b".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 1,
                algebraic_scalar_count: 1,
                output_scalar_count: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        derivative_placeholder_row(0),
                        add_assignment_residual_row(1, 0, 2.0),
                        scale_assignment_residual_row(2, 1, 3.0),
                    ],
                    "batched_assignment_refresh.mo",
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                    Some(solve::scalar_slot_y(2)),
                ],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![1.0, 0.0, 0.0],
        ..Default::default()
    };
    set_causal_test_projection_plan(&mut model);
    let runtime = SolveRuntime::new(&model).expect("runtime should prepare");
    assert!(runtime.algebraic_refresh.causal_solution_certified);
    let mut solver_y = model.initial_y.clone();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-12, 4)
        .expect("batched assignment refresh should evaluate");

    assert_eq!(solver_y, vec![1.0, 3.0, 9.0]);
}

#[test]
fn certified_assignment_refresh_rejects_nonfinite_value_and_restores_input() {
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
                    vec![nonfinite_assignment_residual_row(0)],
                    "nonfinite_certified_assignment.mo",
                )),
                implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![7.0],
        ..Default::default()
    };
    set_causal_test_projection_plan(&mut model);
    let runtime = SolveRuntime::new(&model).expect("runtime should prepare");
    assert!(runtime.algebraic_refresh.causal_solution_certified);
    let mut solver_y = model.initial_y.clone();

    let error = runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-12, 4)
        .expect_err("nonfinite direct assignment must not take the certified fast exit");

    assert!(
        error.to_string().contains("inf"),
        "unexpected error: {error}"
    );
    assert_eq!(solver_y, vec![7.0]);
}

#[test]
fn causal_certificate_rejects_swapped_blt_equation_target_pairs() {
    let model = solve::SolveModel {
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
                        shifted_variable_residual_row(0, 1.0),
                        shifted_variable_residual_row(1, 2.0),
                    ],
                    "swapped_blt_pairs.mo",
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                ],
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![
                        solve::AlgebraicProjectionBlock {
                            rows: vec![0],
                            y_indices: vec![1],
                        },
                        solve::AlgebraicProjectionBlock {
                            rows: vec![1],
                            y_indices: vec![0],
                        },
                    ],
                },
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0; 2],
        ..Default::default()
    };

    let runtime = SolveRuntime::new(&model).expect("runtime should prepare");
    assert!(!runtime.algebraic_refresh.causal_solution_certified);
}

#[test]
fn refresh_plan_accepts_direct_affine_residual_target() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["T1".to_string(), "T2".to_string(), "q".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 2,
                algebraic_scalar_count: 1,
                parameter_count: 1,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        derivative_placeholder_row(2),
                        derivative_placeholder_row(3),
                        direct_assignment_residual_row(),
                    ],
                    "direct_affine_residual.mo",
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                    Some(solve::scalar_slot_y(2)),
                ],
                ..Default::default()
            },
            ..Default::default()
        },
        ..Default::default()
    };
    let block =
        PreparedScalarProgramBlock::from_compute_block(&model.problem.continuous.implicit_rhs)
            .expect("valid implicit RHS should prepare");

    let plan = valid_algebraic_refresh_plan(&model, &block);
    let value = block
        .eval_target_assignment_row_with_context(
            2,
            2,
            &[373.15, 273.15, 0.0],
            &[10.0],
            0.0,
            RowEvalContext::default(),
        )
        .expect("direct affine residual should evaluate");

    assert_eq!(plan.rows.len(), 1);
    assert_eq!(plan.rows[0].row_idx, 2);
    assert_eq!(plan.rows[0].target_index, 2);
    assert_eq!(value, Some(-1000.0));
}

#[test]
fn refresh_residual_fallback_solves_positive_unit_coefficient() {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string()],
                    ..Default::default()
                },
                algebraic_scalar_count: 1,
                parameter_count: 1,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![vec![
                        solve::LinearOp::LoadY { dst: 0, index: 0 },
                        solve::LinearOp::LoadP { dst: 1, index: 0 },
                        solve::LinearOp::Binary {
                            dst: 2,
                            op: solve::BinaryOp::Add,
                            lhs: 0,
                            rhs: 1,
                        },
                        solve::LinearOp::StoreOutput { src: 2 },
                    ]],
                    "positive_residual.mo",
                )),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![solve::AlgebraicProjectionBlock {
                        rows: vec![0],
                        y_indices: vec![0],
                    }],
                },
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![10.0],
        ..Default::default()
    };
    set_test_implicit_jvp(
        &mut model,
        vec![vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "positive_residual_jvp.mo",
    );
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let mut solver_y = model.initial_y.clone();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[4.0], 1.0e-12, 1)
        .expect("positive-coefficient residual should refresh");

    assert_eq!(solver_y[0], -4.0);
}

fn mode_dependent_repivot_residual_rows() -> Vec<Vec<solve::LinearOp>> {
    vec![
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::LoadY { dst: 1, index: 0 },
            solve::LinearOp::LoadP { dst: 2, index: 0 },
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
            solve::LinearOp::Const { dst: 5, value: 1.0 },
            solve::LinearOp::Binary {
                dst: 6,
                op: solve::BinaryOp::Sub,
                lhs: 4,
                rhs: 5,
            },
            solve::LinearOp::StoreOutput { src: 6 },
        ],
        vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::LoadY { dst: 1, index: 1 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Const { dst: 3, value: 3.0 },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 2,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ],
    ]
}

fn mode_dependent_repivot_jvp_rows() -> Vec<Vec<solve::LinearOp>> {
    vec![
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 1 },
            solve::LinearOp::LoadSeed { dst: 1, index: 0 },
            solve::LinearOp::LoadP { dst: 2, index: 0 },
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
        vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::LoadSeed { dst: 1, index: 1 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ],
    ]
}

fn mode_dependent_repivot_model() -> solve::SolveModel {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string(), "y".to_string()],
                    ..Default::default()
                },
                algebraic_scalar_count: 2,
                parameter_count: 1,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    mode_dependent_repivot_residual_rows(),
                    "mode_dependent_repivot.mo",
                )),
                implicit_row_targets: vec![None, Some(solve::scalar_slot_y(1))],
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![solve::AlgebraicProjectionBlock {
                        rows: vec![0, 1],
                        y_indices: vec![0, 1],
                    }],
                },
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0, 0.0],
        ..Default::default()
    };
    set_test_implicit_jvp(
        &mut model,
        mode_dependent_repivot_jvp_rows(),
        "mode_dependent_repivot_jvp.mo",
    );
    model
}

#[test]
fn refresh_newton_repivots_mode_dependent_coupled_residuals() {
    // At k=0, row 0 is structurally incident on x but numerically independent
    // of it. The complete Jacobian remains nonsingular.
    let model = mode_dependent_repivot_model();
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    assert!(runtime.algebraic_refresh.rows.is_empty());
    assert_eq!(runtime.algebraic_refresh.simultaneous_plan.blocks.len(), 1);

    let mut solver_y = model.initial_y.clone();
    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[0.0], 1.0e-10, 4)
        .expect("coupled Newton solve should dynamically repivot the residuals");

    assert!((solver_y[0] - 2.0).abs() <= 1.0e-9);
    assert!((solver_y[1] - 1.0).abs() <= 1.0e-9);
}

#[test]
fn refresh_newton_keeps_finite_causal_values_from_first_sweep() {
    // The coupled x/y sweep diverges and must fall back to Newton. The x row
    // also divides by the independently assigned epsilon, so restarting from
    // the original all-zero vector would make the simultaneous residual
    // non-finite even though the first ordered sweep established epsilon = 1.
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["eps".to_string(), "x".to_string(), "y".to_string()],
                    ..Default::default()
                },
                algebraic_scalar_count: 3,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        assignment_residual_for_constant(0, 1.0),
                        assignment_residual_with_reciprocal(1, 2, 0),
                        scale_and_offset_assignment_residual_row(2, 1, 2.0, 1.0),
                    ],
                    "causal_newton_seed.mo",
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                    Some(solve::scalar_slot_y(2)),
                ],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0, 0.0, 0.0],
        ..Default::default()
    };
    set_test_implicit_jvp(
        &mut model,
        vec![
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 1 },
                solve::LinearOp::LoadSeed { dst: 1, index: 2 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::LoadSeed { dst: 3, index: 0 },
                solve::LinearOp::LoadY { dst: 4, index: 0 },
                solve::LinearOp::Binary {
                    dst: 5,
                    op: solve::BinaryOp::Mul,
                    lhs: 4,
                    rhs: 4,
                },
                solve::LinearOp::Binary {
                    dst: 6,
                    op: solve::BinaryOp::Div,
                    lhs: 3,
                    rhs: 5,
                },
                solve::LinearOp::Binary {
                    dst: 7,
                    op: solve::BinaryOp::Add,
                    lhs: 2,
                    rhs: 6,
                },
                solve::LinearOp::StoreOutput { src: 7 },
            ],
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 2 },
                solve::LinearOp::Const { dst: 1, value: 2.0 },
                solve::LinearOp::LoadSeed { dst: 2, index: 1 },
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
        ],
        "causal_newton_seed_jvp.mo",
    );
    set_complete_test_projection_plan(&mut model);
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    assert!(!runtime.algebraic_refresh.causal_solution_certified);
    let mut solver_y = model.initial_y.clone();
    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-10, 8)
        .expect("Newton should retain the finite causal epsilon seed");

    assert!((solver_y[0] - 1.0).abs() <= 1.0e-9);
    assert!((solver_y[1] + 2.0).abs() <= 1.0e-9);
    assert!((solver_y[2] + 3.0).abs() <= 1.0e-9);
}

#[test]
fn refresh_accepts_assignment_seed_only_when_residual_is_within_tolerance() {
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
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    assert!(!runtime.algebraic_refresh.causal_solution_certified);
    let mut solver_y = model.initial_y.clone();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-6, 4)
        .expect("the preserved residual system should be projected");

    assert!((solver_y[0] - 2.0e-7).abs() <= f64::EPSILON);
    assert!((solver_y[1] - 1.5e-7).abs() <= f64::EPSILON);
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
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
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
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
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
                        span,
                    ),
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
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let mut solver_y = vec![19.0];

    let error = runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-10, 1)
        .expect_err("a missing runtime input is not a Newton-recoverable error");

    assert_eq!(solver_y, vec![19.0]);
    assert!(error.to_string().contains("missing p[0]"));
    assert!(!seed_error_allows_projection(&error));
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
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    assert!(runtime.algebraic_refresh.rows.is_empty());
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
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![derivative_placeholder_row(0)],
                    "missing_producer_implicit.mo",
                )),
                implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
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
    let err = match SolveRuntime::new(&model) {
        Ok(_) => panic!("incomplete implicit algebraic system must be rejected"),
        Err(err) => err,
    };

    assert!(
        err.to_string()
            .contains("implicit algebraic system is missing output row 1"),
        "error should identify the missing implicit row: {err}"
    );
}

#[test]
fn visible_values_for_names_preserves_requested_order() {
    let model = solve::SolveModel {
        visible_names: vec!["b".to_string(), "a".to_string()],
        visible_value_rows: spanned_block(
            vec![const_visible_value_row(2.0), const_visible_value_row(1.0)],
            "visible_values.mo",
        ),
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let names = vec!["a".to_string(), "missing".to_string(), "b".to_string()];

    let values = runtime
        .visible_values_for_names(&[], &[], 0.0, &names)
        .expect("visible values should evaluate");

    assert_eq!(
        values.keys().cloned().collect::<Vec<_>>(),
        vec!["a".to_string(), "b".to_string()]
    );
    assert_eq!(values.get("a"), Some(&1.0));
    assert_eq!(values.get("b"), Some(&2.0));
}

#[test]
fn visible_values_fast_path_reads_direct_sources() {
    let model = solve::SolveModel {
        visible_names: vec!["y2".to_string(), "p1".to_string(), "time".to_string()],
        visible_value_rows: spanned_block(
            vec![
                direct_y_visible_value_row(1),
                direct_param_visible_value_row(0),
                direct_time_visible_value_row(),
            ],
            "visible_fast_path.mo",
        ),
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");

    let values = runtime
        .visible_values(&[10.0, 20.0], &[3.5], 4.25)
        .expect("direct visible values should evaluate");

    assert_eq!(values, vec![20.0, 3.5, 4.25]);
}

#[test]
fn visible_values_mixed_plan_keeps_expression_rows() {
    let model = solve::SolveModel {
        visible_names: vec!["y2".to_string(), "computed".to_string()],
        visible_value_rows: spanned_block(
            vec![direct_y_visible_value_row(1), positive_sum_residual_row()],
            "visible_mixed_plan.mo",
        ),
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");

    let values = runtime
        .visible_values(&[10.0, 20.0], &[], 0.0)
        .expect("mixed visible values should evaluate");

    assert_eq!(values, vec![20.0, 30.0]);
}

#[test]
fn visible_value_plan_deduplicates_equal_expression_rows() {
    let model = solve::SolveModel {
        visible_names: vec![
            "y2".to_string(),
            "computed_a".to_string(),
            "computed_b".to_string(),
        ],
        visible_value_rows: spanned_block(
            vec![
                direct_y_visible_value_row(1),
                positive_sum_residual_row(),
                positive_sum_residual_row(),
            ],
            "visible_duplicate_expressions.mo",
        ),
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let plan = runtime
        .visible_value_plan
        .as_ref()
        .expect("visible value plan should build");

    assert_eq!(plan.expression_rows, vec![1]);
    assert_eq!(plan.expression_groups.len(), 1);
    assert_eq!(plan.expression_groups[0].row_index, 1);
    assert_eq!(plan.expression_groups[0].output_indices, vec![1, 2]);

    let values = runtime
        .visible_values(&[10.0, 20.0], &[], 0.0)
        .expect("deduplicated visible values should evaluate");

    assert_eq!(values, vec![20.0, 30.0, 30.0]);
}

#[test]
fn root_condition_plan_keeps_full_values_but_neutralizes_search_roots() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![
                        constant_expression_root_row(),
                        param_minus_time_root_row(0),
                        direct_param_visible_value_row(1),
                        indexed_param_root_row(),
                        time_plus_one_root_row(),
                    ],
                    "root_plan.mo",
                ),
                root_relation_memory_targets: vec![None; 5],
                root_zero_domains: vec![solve::RootZeroDomain::Previous; 5],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![2.5, 9.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let plan = runtime
        .root_condition_plan
        .as_ref()
        .expect("root condition plan should build");

    assert_eq!(plan.evaluated_rows, vec![2, 3, 4]);
    assert_eq!(plan.search_rows, vec![4]);

    let full = runtime
        .eval_root_conditions_from_solver_y(1.0, &[], &model.parameters)
        .expect("full root values should evaluate");
    assert_eq!(full, vec![5.0, 1.5, 9.0, 9.0, 2.0]);

    let mut search = vec![0.0; 5];
    runtime
        .eval_root_search_conditions_into(1.0, &[], &model.parameters, 1.0e-12, 1, &mut search)
        .expect("search root values should evaluate");
    assert_eq!(search, vec![1.0, 1.0, 1.0, 1.0, 2.0]);
}

#[test]
fn root_condition_plan_neutralizes_parameter_static_algebraic_outputs() {
    let model = algebraic_output_root_model(assignment_residual_row());
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let plan = runtime
        .root_condition_plan
        .as_ref()
        .expect("root condition plan should build");

    assert_eq!(plan.evaluated_rows, vec![0]);
    assert!(plan.search_rows.is_empty());

    let full = runtime
        .eval_root_conditions_from_solver_y(0.0, &[0.0, 2.0], &[])
        .expect("full root value should evaluate");
    assert_eq!(full, vec![2.0]);

    let mut search = vec![0.0];
    runtime
        .eval_root_search_conditions_into(0.0, &[0.0], &[], 1.0e-12, 1, &mut search)
        .expect("search root value should evaluate");
    assert_eq!(search, vec![1.0]);
}

#[test]
fn root_condition_plan_keeps_state_dependent_algebraic_outputs_dynamic() {
    let model = algebraic_output_root_model(add_assignment_residual_row(1, 0, 1.0));
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let plan = runtime
        .root_condition_plan
        .as_ref()
        .expect("root condition plan should build");

    assert_eq!(plan.evaluated_rows, vec![0]);
    assert_eq!(plan.search_rows, vec![0]);

    let mut search = vec![0.0];
    runtime
        .eval_root_search_conditions_into(0.0, &[3.0], &[], 1.0e-12, 1, &mut search)
        .expect("search root value should evaluate");
    assert_eq!(search, vec![4.0]);
}

fn algebraic_output_root_model(implicit_row: Vec<solve::LinearOp>) -> solve::SolveModel {
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["state".to_string(), "output".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 1,
                algebraic_scalar_count: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![derivative_placeholder_row(0), implicit_row],
                    "algebraic_output_root.mo",
                )),
                implicit_row_targets: vec![None, Some(solve::scalar_slot_y(1))],
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![direct_y_visible_value_row(1)],
                    "algebraic_output_root.mo",
                ),
                root_relation_memory_targets: vec![None],
                root_zero_domains: vec![solve::RootZeroDomain::Previous],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0, 2.0],
        ..Default::default()
    };
    set_complete_test_projection_plan(&mut model);
    model
}

#[test]
fn root_condition_plan_reports_next_direct_time_root() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![param_minus_time_root_row(0)],
                    "direct_time_root.mo",
                ),
                root_relation_memory_targets: vec![None],
                root_zero_domains: vec![solve::RootZeroDomain::Previous],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![2.5],
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");

    assert_eq!(
        runtime
            .next_planned_time_root(&model.parameters, 1.0, 3.0, 1.0e-12)
            .expect("direct time root should be found"),
        Some(2.5)
    );
    assert_eq!(
        runtime
            .next_planned_time_root(&model.parameters, 2.5, 3.0, 1.0e-12)
            .expect("current root should not be rescheduled"),
        None
    );
    assert_eq!(
        runtime
            .next_planned_time_root(&model.parameters, 1.0, 2.0, 1.0e-12)
            .expect("future root beyond target should be ignored"),
        None
    );
}

#[test]
fn visible_value_runtime_errors_keep_row_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("visible.mo"),
        4,
        9,
    );
    let model = solve::SolveModel {
        visible_names: vec!["x".to_string()],
        visible_value_rows: solve::ScalarProgramBlock::with_source_span(
            vec![derivative_placeholder_row(0)],
            span,
        ),
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");

    let names = vec!["x".to_string()];
    let err = runtime
        .visible_values_for_names(&[], &[], 0.0, &names)
        .expect_err("missing input should fail visible row evaluation");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string().contains("missing y[0]"),
        "error should explain the missing visible input: {err}"
    );
}

fn non_assignment_targeted_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Binary {
            dst: 1,
            op: solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        solve::LinearOp::Const { dst: 2, value: 1.0 },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Add,
            lhs: 1,
            rhs: 2,
        },
        solve::LinearOp::StoreOutput { src: 3 },
    ]
}

fn assignment_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::Const { dst: 1, value: 2.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn assignment_residual_for_constant(target: usize, value: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::Const { dst: 1, value },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn assignment_residual_with_reciprocal(
    target: usize,
    source: usize,
    denominator: usize,
) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::LoadY {
            dst: 1,
            index: source,
        },
        solve::LinearOp::Const { dst: 2, value: 1.0 },
        solve::LinearOp::LoadY {
            dst: 3,
            index: denominator,
        },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Div,
            lhs: 2,
            rhs: 3,
        },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Add,
            lhs: 1,
            rhs: 4,
        },
        solve::LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 5,
        },
        solve::LinearOp::StoreOutput { src: 6 },
    ]
}

fn sqrt_assignment_residual_row(target: usize, source: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::LoadY {
            dst: 1,
            index: source,
        },
        solve::LinearOp::Unary {
            dst: 2,
            op: solve::UnaryOp::Sqrt,
            arg: 1,
        },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 2,
        },
        solve::LinearOp::StoreOutput { src: 3 },
    ]
}

fn scale_and_offset_assignment_residual_row(
    target: usize,
    source: usize,
    scale: f64,
    offset: f64,
) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::LoadY {
            dst: 1,
            index: source,
        },
        solve::LinearOp::Const {
            dst: 2,
            value: scale,
        },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        solve::LinearOp::Const {
            dst: 4,
            value: offset,
        },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Add,
            lhs: 3,
            rhs: 4,
        },
        solve::LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 5,
        },
        solve::LinearOp::StoreOutput { src: 6 },
    ]
}

fn add_assignment_residual_row(target: usize, source: usize, offset: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::LoadY {
            dst: 1,
            index: source,
        },
        solve::LinearOp::Const {
            dst: 2,
            value: offset,
        },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Add,
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
    ]
}

fn scale_assignment_residual_row(target: usize, source: usize, scale: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::LoadY {
            dst: 1,
            index: source,
        },
        solve::LinearOp::Const {
            dst: 2,
            value: scale,
        },
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
    ]
}

fn nonfinite_assignment_residual_row(target: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        solve::LinearOp::Const { dst: 1, value: 1.0 },
        solve::LinearOp::Const { dst: 2, value: 0.0 },
        solve::LinearOp::Binary {
            dst: 3,
            op: solve::BinaryOp::Div,
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
    ]
}

fn shifted_variable_residual_row(index: usize, value: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index },
        solve::LinearOp::Const { dst: 1, value },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn bilinear_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::LoadY { dst: 1, index: 1 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::LoadY { dst: 3, index: 2 },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Sub,
            lhs: 2,
            rhs: 3,
        },
        solve::LinearOp::StoreOutput { src: 4 },
    ]
}

fn scaled_assignment_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index: 0 },
        solve::LinearOp::Unary {
            dst: 1,
            op: solve::UnaryOp::Neg,
            arg: 0,
        },
        solve::LinearOp::LoadP { dst: 2, index: 1 },
        solve::LinearOp::LoadY { dst: 3, index: 1 },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Mul,
            lhs: 2,
            rhs: 3,
        },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Add,
            lhs: 1,
            rhs: 4,
        },
        solve::LinearOp::StoreOutput { src: 5 },
    ]
}

fn const_visible_value_row(value: f64) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn direct_y_visible_value_row(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn direct_param_visible_value_row(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn indexed_param_root_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 1.0 },
        solve::LinearOp::LoadIndexedP {
            dst: 1,
            base: 0,
            count: 2,
            index: 0,
        },
        solve::LinearOp::StoreOutput { src: 1 },
    ]
}

fn direct_time_visible_value_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadTime { dst: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn param_minus_time_root_row(index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index },
        solve::LinearOp::LoadTime { dst: 1 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn constant_expression_root_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 2.0 },
        solve::LinearOp::Const { dst: 1, value: 3.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn time_plus_one_root_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadTime { dst: 0 },
        solve::LinearOp::Const { dst: 1, value: 1.0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn positive_sum_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 0 },
        solve::LinearOp::LoadY { dst: 1, index: 1 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn derivative_placeholder_row(y_index: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY {
            dst: 0,
            index: y_index,
        },
        solve::LinearOp::StoreOutput { src: 0 },
    ]
}

fn direct_assignment_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 2 },
        solve::LinearOp::LoadP { dst: 1, index: 0 },
        solve::LinearOp::LoadY { dst: 2, index: 0 },
        solve::LinearOp::LoadY { dst: 3, index: 1 },
        solve::LinearOp::Binary {
            dst: 4,
            op: solve::BinaryOp::Sub,
            lhs: 2,
            rhs: 3,
        },
        solve::LinearOp::Binary {
            dst: 5,
            op: solve::BinaryOp::Mul,
            lhs: 1,
            rhs: 4,
        },
        solve::LinearOp::Binary {
            dst: 6,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 5,
        },
        solve::LinearOp::StoreOutput { src: 6 },
    ]
}

// Build a state-only-eligible model: state x (slot 0), algebraic a (slot 1),
// with der(x) = a and the projection a = k*x. The exact reduced state
// Jacobian is d(der)/dx = d(a)/dx = k, which a states-only seed would miss
// (it would yield 0) — so this pins the projection forward-sensitivity.

mod projection_output_mapping;
mod projection_sensitivity;
