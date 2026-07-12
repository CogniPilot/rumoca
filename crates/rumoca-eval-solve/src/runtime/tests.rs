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
    let block =
        PreparedScalarProgramBlock::from_compute_block(&model.problem.continuous.implicit_rhs)
            .expect("valid implicit RHS should prepare");

    let plan = valid_algebraic_refresh_plan(&model, &block);

    assert!(!plan.iterative);
    assert_eq!(plan.rows.len(), 1);
    assert_eq!(plan.rows[0].row_idx, 1);
    assert_eq!(plan.rows[0].target_index, 1);
}

#[test]
fn projection_refresh_rows_do_not_shadow_direct_assignment_rows() {
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
                        assignment_residual_row(),
                        non_assignment_targeted_residual_row(),
                    ],
                    "projection_shadow.mo",
                )),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(1)),
                    Some(solve::scalar_slot_y(1)),
                ],
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![solve::AlgebraicProjectionBlock {
                        rows: vec![1],
                        y_indices: vec![1],
                        causal_steps: vec![solve::AlgebraicProjectionStep { row: 1, y_index: 1 }],
                    }],
                },
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

    assert!(!plan.iterative);
    assert_eq!(plan.rows.len(), 1);
    assert_eq!(plan.rows[0].row_idx, 0);
    assert_eq!(plan.rows[0].target_index, 1);
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
                algebraic_scalar_count: 2,
                parameter_count: 2,
                compiled_parameter_len: 2,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![scaled_assignment_residual_row()],
                    "scaled_affine_residual.mo",
                )),
                implicit_row_targets: vec![Some(solve::scalar_slot_y(1))],
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
            0,
            1,
            &[0.0, 0.0],
            &[6.0, 2.0],
            0.0,
            RowEvalContext::default(),
        )
        .expect("scaled residual should evaluate");

    assert!(!plan.iterative);
    assert_eq!(plan.rows.len(), 1);
    assert_eq!(plan.rows[0].row_idx, 0);
    assert_eq!(plan.rows[0].target_index, 1);
    assert_eq!(value, Some(3.0));
}

#[test]
fn refresh_once_batches_shapeless_multi_output_assignment_program() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.algebraic_scalar_count = 2;
    model.problem.solve_layout.solver_maps.names =
        vec!["x".to_string(), "a".to_string(), "b".to_string()];
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(multi_output_assignment_block());
    model.problem.continuous.implicit_row_targets = vec![
        Some(solve::scalar_slot_y(0)),
        Some(solve::scalar_slot_y(1)),
        Some(solve::scalar_slot_y(2)),
    ];

    let runtime = SolveRuntime::new(&model).expect("runtime should prepare");

    assert_eq!(runtime.algebraic_refresh.rows.len(), 2);
    assert_eq!(runtime.algebraic_refresh.rows[0].row_idx, 1);
    assert_eq!(runtime.algebraic_refresh.rows[0].output_offset, 0);
    assert_eq!(runtime.algebraic_refresh.rows[1].row_idx, 1);
    assert_eq!(runtime.algebraic_refresh.rows[1].output_offset, 1);

    let mut solver_y = vec![5.0, 0.0, 0.0];
    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-9, 4)
        .expect("multi-output refresh should update both targets");

    assert_eq!(solver_y, vec![5.0, 10.0, 20.0]);
}

#[test]
fn refresh_once_batches_contiguous_matmul_projection_outputs() {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.algebraic_scalar_count = 2;
    model.problem.solve_layout.solver_maps.names =
        vec!["x".to_string(), "a".to_string(), "b".to_string()];
    model.problem.continuous.implicit_rhs = solve::ComputeBlock {
        nodes: vec![
            solve::ComputeNode::ScalarPrograms(spanned_block(
                vec![derivative_placeholder_row(0)],
                "matmul_refresh.mo",
            )),
            solve::ComputeNode::MatMul {
                lhs_ops: vec![
                    solve::LinearOp::Const { dst: 0, value: 2.0 },
                    solve::LinearOp::Const { dst: 1, value: 3.0 },
                    solve::LinearOp::Const { dst: 2, value: 5.0 },
                    solve::LinearOp::Const { dst: 3, value: 7.0 },
                ],
                lhs_start: 0,
                rhs_ops: vec![
                    solve::LinearOp::LoadY { dst: 4, index: 0 },
                    solve::LinearOp::Const { dst: 5, value: 1.0 },
                ],
                rhs_start: 4,
                m: 2,
                k: 2,
                n: 1,
                lhs_sparsity: solve::SparsityPattern::Dense,
                rhs_sparsity: solve::SparsityPattern::Dense,
                metadata: solve::TensorNodeMetadata::default(),
                span: test_span("matmul_refresh.mo"),
            },
        ],
    };
    model.problem.continuous.implicit_row_targets = vec![
        Some(solve::scalar_slot_y(0)),
        Some(solve::scalar_slot_y(1)),
        Some(solve::scalar_slot_y(2)),
    ];

    let runtime = SolveRuntime::new(&model).expect("runtime should prepare");

    assert_eq!(runtime.algebraic_refresh.rows.len(), 2);
    assert_eq!(runtime.algebraic_refresh.rows[0].row_idx, 1);
    assert_eq!(runtime.algebraic_refresh.rows[0].output_offset, 0);
    assert_eq!(runtime.algebraic_refresh.rows[1].row_idx, 1);
    assert_eq!(runtime.algebraic_refresh.rows[1].output_offset, 1);

    let mut solver_y = vec![11.0, 0.0, 0.0];
    let rows = runtime.algebraic_refresh.rows.clone();
    let next = runtime
        .try_refresh_tensor_output_segment(&rows, 0, 0.0, &mut solver_y, &[])
        .expect("tensor refresh segment should evaluate");
    assert_eq!(next, Some(2));
    assert_eq!(solver_y, vec![11.0, 25.0, 62.0]);

    solver_y = vec![11.0, 0.0, 0.0];
    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-12, 4)
        .expect("MatMul projection refresh should update both targets");

    assert_eq!(solver_y, vec![11.0, 25.0, 62.0]);
}

#[test]
fn batched_assignment_refresh_preserves_row_order_dependencies() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string(), "a".to_string(), "b".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 1,
                algebraic_scalar_count: 2,
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
    let runtime = SolveRuntime::new(&model).expect("runtime should prepare");
    let mut solver_y = model.initial_y.clone();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-12, 4)
        .expect("batched assignment refresh should evaluate");

    assert_eq!(solver_y, vec![1.0, 3.0, 9.0]);
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

    assert!(!plan.iterative);
    assert_eq!(plan.rows.len(), 1);
    assert_eq!(plan.rows[0].row_idx, 2);
    assert_eq!(plan.rows[0].target_index, 2);
    assert_eq!(value, Some(-1000.0));
}

#[test]
fn refresh_plan_evaluates_projected_multi_output_program_row() {
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
                    vec![vec![
                        solve::LinearOp::Const { dst: 0, value: 1.0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                        solve::LinearOp::Const { dst: 0, value: 2.0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ]],
                    "projected_multi_output.mo",
                )),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![solve::AlgebraicProjectionBlock {
                        rows: vec![1],
                        y_indices: vec![1],
                        causal_steps: vec![solve::AlgebraicProjectionStep { row: 1, y_index: 1 }],
                    }],
                },
                implicit_row_targets: vec![None, Some(solve::scalar_slot_y(1))],
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0, 0.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let plan = &runtime.algebraic_refresh;

    assert!(!plan.iterative);
    assert_eq!(plan.rows.len(), 1);
    assert_eq!(plan.rows[0].row_idx, 0);
    assert_eq!(plan.rows[0].output_offset, 1);
    assert_eq!(plan.rows[0].target_index, 1);

    let mut solver_y = model.initial_y.clone();
    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-10, 4)
        .expect("projected multi-output refresh row should evaluate");

    assert_eq!(solver_y, vec![0.0, 2.0]);
}

#[test]
fn refresh_residual_fallback_solves_positive_unit_coefficient() {
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
                    vec![positive_sum_residual_row(), derivative_placeholder_row(1)],
                    "positive_residual.mo",
                )),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![solve::AlgebraicProjectionBlock {
                        rows: vec![0],
                        y_indices: vec![0],
                        causal_steps: Vec::new(),
                    }],
                },
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![10.0, 4.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let mut solver_y = model.initial_y.clone();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &[], 1.0e-12, 1)
        .expect("positive-coefficient residual should refresh");

    assert_eq!(solver_y[0], -4.0);
    assert_eq!(solver_y[1], 4.0);
}

#[test]
fn refresh_uses_projection_alternative_when_parameter_select_deactivates_primary_row() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["target".to_string(), "other".to_string()],
                    ..Default::default()
                },
                algebraic_scalar_count: 2,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(spanned_block(
                    vec![
                        parameter_select_primary_row(),
                        target_minus_constant_residual_row(0, 2.0),
                    ],
                    "parameter_select_projection.mo",
                )),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![solve::AlgebraicProjectionBlock {
                        rows: vec![0, 1],
                        y_indices: vec![0],
                        causal_steps: vec![solve::AlgebraicProjectionStep { row: 0, y_index: 0 }],
                    }],
                },
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![10.0, 5.0],
        parameters: vec![0.0],
        ..Default::default()
    };
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let mut solver_y = model.initial_y.clone();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &model.parameters, 1.0e-12, 1)
        .expect("active projection alternative should solve the target");

    assert_eq!(solver_y[0], 2.0);
    assert_eq!(solver_y[1], 5.0);
}

#[test]
fn derivative_refresh_errors_on_missing_algebraic_producer() {
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
    let runtime = SolveRuntime::new(&model).expect("valid runtime should prepare");
    let err = runtime
        .eval_state_derivatives(0.0, &[1.0], &[], 1.0e-10, 1)
        .expect_err("missing algebraic producer should not fall back to initial_y");

    assert!(
        err.to_string().contains("without producer rows: alias"),
        "error should name the missing producer dependency: {err}"
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
                        time_plus_one_root_row(),
                    ],
                    "root_plan.mo",
                ),
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

    assert_eq!(plan.evaluated_rows, vec![2, 3]);
    assert_eq!(plan.search_rows, vec![3]);

    let full = runtime
        .eval_root_conditions_from_solver_y(1.0, &[], &model.parameters)
        .expect("full root values should evaluate");
    assert_eq!(full, vec![5.0, 1.5, 9.0, 2.0]);

    let mut search = vec![0.0; 4];
    runtime
        .eval_root_search_conditions_into(1.0, &[], &model.parameters, 1.0e-12, 1, &mut search)
        .expect("search root values should evaluate");
    assert_eq!(search, vec![1.0, 1.0, 1.0, 2.0]);
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

fn multi_output_assignment_block() -> solve::ScalarProgramBlock {
    solve::ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::Const {
                    dst: 0,
                    value: 10.0,
                },
                solve::LinearOp::StoreOutput { src: 0 },
                solve::LinearOp::Const {
                    dst: 1,
                    value: 20.0,
                },
                solve::LinearOp::StoreOutput { src: 1 },
            ],
        ],
        vec![
            test_span("multi_output_assignment.mo"),
            test_span("multi_output_assignment.mo"),
        ],
        vec![0, 1, 2],
    )
    .expect("multi-output assignment fixture should be valid")
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

fn parameter_select_primary_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP { dst: 0, index: 0 },
        solve::LinearOp::Const { dst: 1, value: 0.0 },
        solve::LinearOp::Compare {
            dst: 2,
            op: solve::CompareOp::Le,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::LoadY { dst: 3, index: 1 },
        solve::LinearOp::LoadY { dst: 4, index: 0 },
        solve::LinearOp::Select {
            dst: 5,
            cond: 2,
            if_true: 3,
            if_false: 4,
        },
        solve::LinearOp::StoreOutput { src: 5 },
    ]
}

fn target_minus_constant_residual_row(target: usize, value: f64) -> Vec<solve::LinearOp> {
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
fn projection_coupled_state_model(k: f64) -> solve::SolveModel {
    use solve::LinearOp::{Binary, Const, LoadSeed, LoadY, StoreOutput};
    use solve::{BinaryOp, ComputeBlock};
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "a".to_string()];
    // der(x) = a  (reads the algebraic slot 1)
    model.problem.continuous.derivative_rhs =
        ComputeBlock::from_scalar_program_block(spanned_block(
            vec![vec![LoadY { dst: 0, index: 1 }, StoreOutput { src: 0 }]],
            "projection_derivative.mo",
        ));
    // implicit_rhs: row 0 state residual placeholder, row 1 algebraic residual a - k*x
    model.problem.continuous.implicit_rhs = ComputeBlock::from_scalar_program_block(spanned_block(
        vec![
            vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            vec![
                LoadY { dst: 0, index: 1 },
                Const { dst: 1, value: k },
                LoadY { dst: 2, index: 0 },
                Binary {
                    dst: 3,
                    op: BinaryOp::Mul,
                    lhs: 1,
                    rhs: 2,
                },
                Binary {
                    dst: 4,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 3,
                },
                StoreOutput { src: 4 },
            ],
        ],
        "projection_implicit.mo",
    ));
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![1],
            y_indices: vec![1],
            causal_steps: Vec::new(),
        }],
    };
    // full_jacobian_v: JVP of der(x)=a → d(der) = seed[a]
    model.artifacts.continuous.full_jacobian_v = spanned_block(
        vec![vec![LoadSeed { dst: 0, index: 1 }, StoreOutput { src: 0 }]],
        "projection_full_jvp.mo",
    );
    // implicit_jacobian_v_scalar: per-row JVP of implicit_rhs (row-aligned).
    model.artifacts.continuous.implicit_jacobian_v_scalar = spanned_block(
        vec![
            vec![LoadSeed { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            vec![
                LoadSeed { dst: 0, index: 1 },
                Const { dst: 1, value: k },
                LoadSeed { dst: 2, index: 0 },
                Binary {
                    dst: 3,
                    op: BinaryOp::Mul,
                    lhs: 1,
                    rhs: 2,
                },
                Binary {
                    dst: 4,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 3,
                },
                StoreOutput { src: 4 },
            ],
        ],
        "projection_implicit_jvp.mo",
    );
    model.initial_y = vec![0.0, 0.0];
    model
}

#[test]
fn state_jacobian_includes_projection_forward_sensitivity() {
    let k = 2.0;
    let runtime = SolveRuntime::new(&projection_coupled_state_model(k))
        .expect("valid runtime should prepare");
    let mut out = [0.0_f64];
    runtime
        .eval_state_jacobian_v_ad_into(
            AlgebraicLinearization {
                t: 0.0,
                params: &[],
                settle: AlgebraicSettle {
                    tol: 1.0e-12,
                    max_iters: 32,
                },
            },
            &[3.0],
            &[1.0],
            &mut out,
        )
        .expect("projection-coupled state Jacobian should evaluate");
    // d(der)/dx = d(a)/dx = k. A states-only seed would give 0.
    assert!(
        (out[0] - k).abs() <= 1.0e-9,
        "expected total state Jacobian {k}, got {} (projection sensitivity missing?)",
        out[0]
    );
}

/// Mirror a real failing model: a state coupled to a **linear algebraic loop**
/// (two mutually-dependent algebraics solved iteratively, as the electrical
/// and signal MSL models have). Solving
///   4a +  b =  x        a = 2x/15
///    a + 4b = 2x   ⇒    b = 7x/15
/// with der(x) = a + b gives the reduced ODE der = 3x/5, so the exact state
/// Jacobian is `d(der)/dx = 3/5`. This exercises the iterative seed-refresh
/// and the row-aligned implicit JVP — the case that regressed to the
/// states-only Jacobian (which would give 0) before the projection-aware fix.
fn linear_algebraic_loop_state_model() -> solve::SolveModel {
    use solve::ComputeBlock;
    use solve::LinearOp::{LoadSeed, LoadY, StoreOutput};
    let load_y: fn(u32, usize) -> solve::LinearOp = |dst, index| LoadY { dst, index };
    let load_seed: fn(u32, usize) -> solve::LinearOp = |dst, index| LoadSeed { dst, index };
    // Slots: x = 0 (state), a = 1, b = 2 (algebraics).
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.algebraic_scalar_count = 2;
    model.problem.solve_layout.solver_maps.names =
        vec!["x".to_string(), "a".to_string(), "b".to_string()];
    // der(x) = a + b
    model.problem.continuous.derivative_rhs =
        ComputeBlock::from_scalar_program_block(spanned_block(
            vec![sum_row(
                LoadY { dst: 0, index: 1 },
                LoadY { dst: 1, index: 2 },
            )],
            "loop_derivative.mo",
        ));
    // 2x2 algebraic loop: r_a = 4a + b - x, r_b = a + 4b - 2x (diagonally
    // dominant so both the value and seed Gauss-Seidel refreshes converge).
    model.problem.continuous.implicit_rhs = ComputeBlock::from_scalar_program_block(spanned_block(
        vec![
            vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            affine3(load_y, 4.0, 1, 1.0, 2, 1.0, 0),
            affine3(load_y, 1.0, 1, 4.0, 2, 2.0, 0),
        ],
        "loop_implicit.mo",
    ));
    model.problem.continuous.implicit_row_targets = vec![
        Some(solve::scalar_slot_y(0)),
        Some(solve::scalar_slot_y(1)),
        Some(solve::scalar_slot_y(2)),
    ];
    // Mutual dependence (row 1 -> a reads b, row 2 -> b reads a) => iterative refresh.
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![1, 2],
            y_indices: vec![1, 2],
            causal_steps: Vec::new(),
        }],
    };
    // full_jacobian_v: JVP of der = a + b.
    model.artifacts.continuous.full_jacobian_v = spanned_block(
        vec![sum_row(
            LoadSeed { dst: 0, index: 1 },
            LoadSeed { dst: 1, index: 2 },
        )],
        "loop_full_jvp.mo",
    );
    // Row-aligned per-row JVP of implicit_rhs (same structure, seeds for values).
    model.artifacts.continuous.implicit_jacobian_v_scalar = spanned_block(
        vec![
            vec![LoadSeed { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            affine3(load_seed, 4.0, 1, 1.0, 2, 1.0, 0),
            affine3(load_seed, 1.0, 1, 4.0, 2, 2.0, 0),
        ],
        "loop_implicit_jvp.mo",
    );
    model.initial_y = vec![0.0, 0.0, 0.0];
    model
}

fn shapeless_linear_algebraic_loop_state_model() -> solve::SolveModel {
    use solve::ComputeBlock;
    use solve::LinearOp::{LoadSeed, LoadY, StoreOutput};
    let load_y: fn(u32, usize) -> solve::LinearOp = |dst, index| LoadY { dst, index };
    let load_seed: fn(u32, usize) -> solve::LinearOp = |dst, index| LoadSeed { dst, index };
    let mut model = linear_algebraic_loop_state_model();
    // Same solution as the residual-row fixture, but each algebraic row is a
    // shapeless target-value row:
    //   a = (x - b)/4, b = (2x - a)/4.
    // Value refresh residualizes these as `raw - target`; seed refresh must
    // differentiate that same residual, including the `-seed[target]` term.
    model.problem.continuous.implicit_rhs = ComputeBlock::from_scalar_program_block(spanned_block(
        vec![
            vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            affine2_sub(load_y, 0.25, 0, 0.25, 2),
            affine2_sub(load_y, 0.5, 0, 0.25, 1),
        ],
        "shapeless_loop_implicit.mo",
    ));
    model.artifacts.continuous.implicit_jacobian_v_scalar = spanned_block(
        vec![
            vec![LoadSeed { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            affine2_sub(load_seed, 0.25, 0, 0.25, 2),
            affine2_sub(load_seed, 0.5, 0, 0.25, 1),
        ],
        "shapeless_loop_implicit_jvp.mo",
    );
    model
}

fn parameterized_shapeless_algebraic_loop_state_model() -> solve::SolveModel {
    use solve::ComputeBlock;
    use solve::LinearOp::{LoadSeed, LoadY, StoreOutput};
    let mut model = shapeless_linear_algebraic_loop_state_model();
    model.problem.layout = solve::VarLayout::from_parts(
        indexmap::IndexMap::from([
            ("x".to_string(), solve::scalar_slot_y(0)),
            ("a".to_string(), solve::scalar_slot_y(1)),
            ("b".to_string(), solve::scalar_slot_y(2)),
            ("p".to_string(), solve::scalar_slot_p(0)),
        ]),
        3,
        1,
    );
    model.parameters = vec![10.0];
    // Parameterized target-value loop:
    //   a = (x + p - b)/4, b = (2x + 2p - a)/4,
    // so der = a + b = 3/5 * (x + p) and d(der)/dp = 3/5.
    model.problem.continuous.implicit_rhs = ComputeBlock::from_scalar_program_block(spanned_block(
        vec![
            vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            shapeless_param_row(0.25, 0.25, 2),
            shapeless_param_row(0.5, 0.5, 1),
        ],
        "shapeless_param_loop_implicit.mo",
    ));
    model.artifacts.continuous.implicit_jacobian_v_scalar = spanned_block(
        vec![
            vec![LoadSeed { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            shapeless_param_jvp_row(0.25, 0.25, 2),
            shapeless_param_jvp_row(0.5, 0.5, 1),
        ],
        "shapeless_param_loop_implicit_jvp.mo",
    );
    model
}

fn singular_algebraic_loop_state_model() -> solve::SolveModel {
    use solve::ComputeBlock;
    use solve::LinearOp::{LoadSeed, LoadY, StoreOutput};
    let load_y: fn(u32, usize) -> solve::LinearOp = |dst, index| LoadY { dst, index };
    let load_seed: fn(u32, usize) -> solve::LinearOp = |dst, index| LoadSeed { dst, index };
    let mut model = linear_algebraic_loop_state_model();
    // Singular 2x2 algebraic block: r_a = a + b - x,
    // r_b = 2a + 2b - 2x. The value point can be consistent, but the algebraic
    // seed matrix has dependent rows and must not be accepted as a valid gradient.
    model.problem.continuous.implicit_rhs = ComputeBlock::from_scalar_program_block(spanned_block(
        vec![
            vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            affine3(load_y, 1.0, 1, 1.0, 2, 1.0, 0),
            affine3(load_y, 2.0, 1, 2.0, 2, 2.0, 0),
        ],
        "singular_loop_implicit.mo",
    ));
    model.artifacts.continuous.implicit_jacobian_v_scalar = spanned_block(
        vec![
            vec![LoadSeed { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            affine3(load_seed, 1.0, 1, 1.0, 2, 1.0, 0),
            affine3(load_seed, 2.0, 1, 2.0, 2, 2.0, 0),
        ],
        "singular_loop_implicit_jvp.mo",
    );
    model
}

/// Build an affine residual/JVP row `k1*L(i1) + k2*L(i2) - k3*L(i3)`, where `L`
/// is the load constructor (`LoadY` for the value rows, `LoadSeed` for the JVP).
fn affine3(
    load: fn(u32, usize) -> solve::LinearOp,
    k1: f64,
    i1: usize,
    k2: f64,
    i2: usize,
    k3: f64,
    i3: usize,
) -> Vec<solve::LinearOp> {
    use solve::BinaryOp::{Add, Mul, Sub};
    use solve::LinearOp::{Binary, Const, StoreOutput};
    vec![
        Const { dst: 0, value: k1 },
        load(1, i1),
        Binary {
            dst: 2,
            op: Mul,
            lhs: 0,
            rhs: 1,
        },
        Const { dst: 3, value: k2 },
        load(4, i2),
        Binary {
            dst: 5,
            op: Mul,
            lhs: 3,
            rhs: 4,
        },
        Binary {
            dst: 6,
            op: Add,
            lhs: 2,
            rhs: 5,
        },
        Const { dst: 7, value: k3 },
        load(8, i3),
        Binary {
            dst: 9,
            op: Mul,
            lhs: 7,
            rhs: 8,
        },
        Binary {
            dst: 10,
            op: Sub,
            lhs: 6,
            rhs: 9,
        },
        StoreOutput { src: 10 },
    ]
}

fn affine2_sub(
    load: fn(u32, usize) -> solve::LinearOp,
    k1: f64,
    i1: usize,
    k2: f64,
    i2: usize,
) -> Vec<solve::LinearOp> {
    use solve::BinaryOp::{Mul, Sub};
    use solve::LinearOp::{Binary, Const, StoreOutput};
    vec![
        Const { dst: 0, value: k1 },
        load(1, i1),
        Binary {
            dst: 2,
            op: Mul,
            lhs: 0,
            rhs: 1,
        },
        Const { dst: 3, value: 0.0 },
        Binary {
            dst: 4,
            op: solve::BinaryOp::Add,
            lhs: 2,
            rhs: 3,
        },
        Const { dst: 5, value: k2 },
        load(6, i2),
        Binary {
            dst: 7,
            op: Mul,
            lhs: 5,
            rhs: 6,
        },
        Const { dst: 8, value: 0.0 },
        Binary {
            dst: 9,
            op: solve::BinaryOp::Add,
            lhs: 7,
            rhs: 8,
        },
        Binary {
            dst: 10,
            op: Sub,
            lhs: 4,
            rhs: 9,
        },
        StoreOutput { src: 10 },
    ]
}

fn shapeless_param_row(x_scale: f64, p_scale: f64, feedback_index: usize) -> Vec<solve::LinearOp> {
    use solve::BinaryOp::{Add, Mul, Sub};
    use solve::LinearOp::{Binary, Const, LoadP, LoadY, StoreOutput};
    vec![
        Const {
            dst: 0,
            value: x_scale,
        },
        LoadY { dst: 1, index: 0 },
        Binary {
            dst: 2,
            op: Mul,
            lhs: 0,
            rhs: 1,
        },
        Const {
            dst: 3,
            value: p_scale,
        },
        LoadP { dst: 4, index: 0 },
        Binary {
            dst: 5,
            op: Mul,
            lhs: 3,
            rhs: 4,
        },
        Binary {
            dst: 6,
            op: Add,
            lhs: 2,
            rhs: 5,
        },
        Const {
            dst: 7,
            value: 0.25,
        },
        LoadY {
            dst: 8,
            index: feedback_index,
        },
        Binary {
            dst: 9,
            op: Mul,
            lhs: 7,
            rhs: 8,
        },
        Const {
            dst: 10,
            value: 0.0,
        },
        Binary {
            dst: 11,
            op: Add,
            lhs: 9,
            rhs: 10,
        },
        Binary {
            dst: 12,
            op: Sub,
            lhs: 6,
            rhs: 11,
        },
        StoreOutput { src: 12 },
    ]
}

fn shapeless_param_jvp_row(
    x_scale: f64,
    p_scale: f64,
    feedback_index: usize,
) -> Vec<solve::LinearOp> {
    use solve::BinaryOp::{Add, Mul, Sub};
    use solve::LinearOp::{Binary, Const, LoadSeed, StoreOutput};
    vec![
        Const {
            dst: 0,
            value: x_scale,
        },
        LoadSeed { dst: 1, index: 0 },
        Binary {
            dst: 2,
            op: Mul,
            lhs: 0,
            rhs: 1,
        },
        Const {
            dst: 3,
            value: p_scale,
        },
        LoadSeed { dst: 4, index: 3 },
        Binary {
            dst: 5,
            op: Mul,
            lhs: 3,
            rhs: 4,
        },
        Binary {
            dst: 6,
            op: Add,
            lhs: 2,
            rhs: 5,
        },
        Const {
            dst: 7,
            value: 0.25,
        },
        LoadSeed {
            dst: 8,
            index: feedback_index,
        },
        Binary {
            dst: 9,
            op: Mul,
            lhs: 7,
            rhs: 8,
        },
        Const {
            dst: 10,
            value: 0.0,
        },
        Binary {
            dst: 11,
            op: Add,
            lhs: 9,
            rhs: 10,
        },
        Binary {
            dst: 12,
            op: Sub,
            lhs: 6,
            rhs: 11,
        },
        StoreOutput { src: 12 },
    ]
}

#[test]
fn state_jacobian_resolves_linear_algebraic_loop_sensitivity() {
    let runtime = SolveRuntime::new(&linear_algebraic_loop_state_model())
        .expect("valid runtime should prepare");
    assert!(
        runtime.derivative_refresh.iterative,
        "the mutually-dependent algebraic loop should refresh iteratively"
    );
    let mut out = [0.0_f64];
    runtime
        .eval_state_jacobian_v_ad_into(
            AlgebraicLinearization {
                t: 0.0,
                params: &[],
                settle: AlgebraicSettle {
                    tol: 1.0e-12,
                    max_iters: 1,
                },
            },
            &[1.0],
            &[1.0],
            &mut out,
        )
        .expect("looped-projection state Jacobian should evaluate");
    // der = a + b = 3x/5, so d(der)/dx = 3/5. States-only would give 0.
    assert!(
        (out[0] - 0.6).abs() <= 1.0e-9,
        "expected total state Jacobian 0.6 through the algebraic loop, got {}",
        out[0]
    );
}

#[test]
fn state_jacobian_resolves_shapeless_algebraic_loop_sensitivity() {
    let runtime = SolveRuntime::new(&shapeless_linear_algebraic_loop_state_model())
        .expect("valid runtime should prepare");
    assert!(
        runtime.derivative_refresh.iterative,
        "the mutually-dependent shapeless algebraic loop should refresh iteratively"
    );
    let mut out = [0.0_f64];
    runtime
        .eval_state_jacobian_v_ad_into(
            AlgebraicLinearization {
                t: 0.0,
                params: &[],
                settle: AlgebraicSettle {
                    tol: 1.0e-12,
                    max_iters: 1,
                },
            },
            &[1.0],
            &[1.0],
            &mut out,
        )
        .expect("shapeless looped-projection state Jacobian should evaluate");

    assert!(
        (out[0] - 0.6).abs() <= 1.0e-9,
        "expected total state Jacobian 0.6 through the shapeless loop, got {}",
        out[0]
    );
}

#[test]
fn seed_refresh_directly_solves_coupled_algebraic_loop() {
    let runtime = SolveRuntime::new(&linear_algebraic_loop_state_model())
        .expect("valid runtime should prepare");
    let solver_y = [1.0, 2.0 / 15.0, 7.0 / 15.0];
    let mut seed = [1.0, 0.0, 0.0];
    let mut unit_seed = [0.0, 0.0, 0.0];

    runtime
        .seed_refresh_with_plan(
            &runtime.derivative_refresh,
            AlgebraicLinearization {
                t: 0.0,
                params: &[],
                settle: AlgebraicSettle {
                    tol: 1.0e-12,
                    max_iters: 1,
                },
            },
            &solver_y,
            &mut seed,
            &mut unit_seed,
        )
        .expect("direct seed refresh should not depend on fixed-point iterations");

    assert!(
        (seed[1] - 2.0 / 15.0).abs() <= 1.0e-12,
        "unexpected seed for a: {}",
        seed[1]
    );
    assert!(
        (seed[2] - 7.0 / 15.0).abs() <= 1.0e-12,
        "unexpected seed for b: {}",
        seed[2]
    );
}

#[test]
fn seed_refresh_directly_solves_shapeless_coupled_algebraic_loop() {
    let runtime = SolveRuntime::new(&shapeless_linear_algebraic_loop_state_model())
        .expect("valid runtime should prepare");
    let solver_y = [1.0, 2.0 / 15.0, 7.0 / 15.0];
    let mut seed = [1.0, 0.0, 0.0];
    let mut unit_seed = [0.0, 0.0, 0.0];

    runtime
        .seed_refresh_with_plan(
            &runtime.derivative_refresh,
            AlgebraicLinearization {
                t: 0.0,
                params: &[],
                settle: AlgebraicSettle {
                    tol: 1.0e-12,
                    max_iters: 1,
                },
            },
            &solver_y,
            &mut seed,
            &mut unit_seed,
        )
        .expect("direct seed refresh should residualize shapeless target-value rows");

    assert!(
        (seed[1] - 2.0 / 15.0).abs() <= 1.0e-12,
        "unexpected seed for a: {}",
        seed[1]
    );
    assert!(
        (seed[2] - 7.0 / 15.0).abs() <= 1.0e-12,
        "unexpected seed for b: {}",
        seed[2]
    );
}

#[test]
fn full_jacobian_resolves_shapeless_loop_parameter_seed() {
    let runtime = SolveRuntime::new(&parameterized_shapeless_algebraic_loop_state_model())
        .expect("valid runtime should prepare");
    let mut out = [0.0_f64];
    let seed = [0.0, 0.0, 0.0, 1.0];
    runtime
        .eval_full_jacobian_v_ad_into(
            AlgebraicLinearization {
                t: 0.0,
                params: &[10.0],
                settle: AlgebraicSettle {
                    tol: 1.0e-12,
                    max_iters: 1,
                },
            },
            &[2.0],
            &seed,
            &mut out,
        )
        .expect("parameter-seeded shapeless loop Jacobian should evaluate");

    assert!(
        (out[0] - 0.6).abs() <= 1.0e-9,
        "expected d(der)/dp = 0.6 through the shapeless loop, got {}",
        out[0]
    );
}

#[test]
fn seed_refresh_reports_singular_coupled_algebraic_loop() {
    let runtime = SolveRuntime::new(&singular_algebraic_loop_state_model())
        .expect("valid runtime should prepare");
    let solver_y = [1.0, 0.5, 0.5];
    let mut seed = [1.0, 9.0, -4.0];
    let original_seed = seed;
    let mut unit_seed = [0.0, 0.0, 0.0];

    let error = runtime
        .seed_refresh_with_plan(
            &runtime.derivative_refresh,
            AlgebraicLinearization {
                t: 0.0,
                params: &[],
                settle: AlgebraicSettle {
                    tol: 1.0e-12,
                    max_iters: 1,
                },
            },
            &solver_y,
            &mut seed,
            &mut unit_seed,
        )
        .expect_err("singular seed system should report an invalid gradient");

    assert!(
        error
            .to_string()
            .contains("algebraic forward-sensitivity linear solve is singular"),
        "{error}"
    );
    assert_eq!(
        seed, original_seed,
        "failed seed refresh should restore the caller's seed buffer"
    );
}

fn sum_row(load_lhs: solve::LinearOp, load_rhs: solve::LinearOp) -> Vec<solve::LinearOp> {
    // Both loads must target distinct destination registers; callers pass
    // `{ dst: 0, .. }` and `{ dst: 1, .. }`.
    vec![
        load_lhs,
        load_rhs,
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

/// The implicit-residual reverse VJP `(∂g/∂y)ᵀ μ` must be the exact transpose of
/// the forward implicit JVP `∂g/∂y · v`: the dot-product identity
/// `μᵀ(J_g v) = (J_gᵀ μ)ᵀ v` (Track B foundation — the constraint-Jacobian
/// transpose used by the algebraic-projection adjoint). Checked on the 2x2
/// algebraic-loop model at an arbitrary point (transpose holds anywhere).
#[test]
fn reverse_implicit_residual_vjp_transposes_forward_jvp() {
    let model = linear_algebraic_loop_state_model();
    let runtime = SolveRuntime::new(&model).expect("runtime builds");
    let n = runtime.solver_count;
    let p_scalars = runtime.model.problem.layout.p_scalars();
    let solver_y = [2.0_f64, 0.5, -0.3]; // arbitrary linearization point
    let params: [f64; 0] = [];

    // Forward J_g v (seed over solver-y; the implicit JVP is SolverYOnly).
    let v = [0.7_f64, -0.2, 0.4];
    let mut jg_v = vec![0.0_f64; runtime.implicit_jacobian_v.len()];
    runtime
        .implicit_jacobian_v
        .eval_with_context(
            &solver_y,
            &params,
            0.0,
            RowEvalContext {
                seed: Some(&v),
                external_tables: None,
                runtime_state: None,
            },
            &mut jg_v,
        )
        .expect("forward implicit JVP");

    // Reverse J_gᵀ μ.
    let mu = [0.5_f64, 1.1, -0.6];
    let mut jgt_mu = vec![0.0_f64; n + p_scalars];
    runtime
        .reverse_implicit_residual_vjp(0.0, &solver_y, &params, &mu, &mut jgt_mu)
        .expect("reverse implicit residual VJP");

    let lhs: f64 = mu.iter().zip(&jg_v).map(|(m, j)| m * j).sum();
    let rhs: f64 = jgt_mu[..n].iter().zip(&v).map(|(g, vi)| g * vi).sum();
    assert!(
        (lhs - rhs).abs() < 1.0e-9,
        "implicit VJP transpose identity failed: μᵀ(J_g v)={lhs}, (J_gᵀ μ)ᵀv={rhs}"
    );
    assert!(
        lhs.abs() > 1.0e-6,
        "test point should produce a nonzero pairing"
    );
}
