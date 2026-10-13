use super::*;

#[test]
fn projection_honors_nonidentity_scalar_output_mapping() {
    let span = test_span("projection_output_mapping.mo");
    let residual = solve::ScalarProgramBlock::with_output_indices(
        vec![
            parameter_assignment_residual_row(),
            chained_assignment_residual_row(),
        ],
        vec![span; 2],
        vec![1, 0],
    )
    .expect("valid nonidentity residual output mapping");
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            layout: crate::fixture_layout::named_y_layout(
                rumoca_ir_solve::VarLayout::from_parts(Default::default(), 0, 1),
                vec!["output".to_string(), "sample".to_string()],
            ),
            solve_layout: solve::SolveLayout {
                algebraic_scalar_count: 2,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(residual),
                implicit_row_targets: (vec![None, Some(solve::scalar_slot_y(1))]).into(),
                algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                    blocks: (vec![
                        solve::AlgebraicProjectionBlock {
                            rows: vec![1],
                            y_indices: vec![1],
                            tearing: None,
                            alternate_charts: Vec::new(),
                        },
                        solve::AlgebraicProjectionBlock {
                            rows: vec![0],
                            y_indices: vec![0],
                            tearing: None,
                            alternate_charts: Vec::new(),
                        },
                    ])
                    .into(),
                },
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![0.0, 0.0].into(),
        parameters: vec![1.0].into(),
        ..Default::default()
    };
    // Both primal and tangent retain program outputs [1, 0]; projection rows
    // are logical output coordinates, not source program positions.
    let jvp = solve::ScalarProgramBlock::with_output_indices(
        vec![
            affine_seed_row(&[(1, 1.0)]),
            affine_seed_row(&[(1, 1.0), (0, -1.0)]),
        ],
        vec![span; 2],
        vec![1, 0],
    )
    .expect("checked tangent preserves nonidentity output mapping");
    model.artifacts.continuous.implicit_jacobian_v_scalar = jvp;
    mirror_scalar_implicit_jvp(&mut model);
    mapped_tangent_matches_directional_difference(&model);
    derive_test_structural_artifacts(&mut model);
    let runtime = SolveRuntime::new_fixture(&model).expect("runtime should prepare");
    let mut solver_y = model.initial_y.to_vec();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, &model.parameters, 1.0e-12, 4)
        .expect("nonidentity residual rows should project in logical BLT order");

    assert_eq!(solver_y, vec![1.0, 1.0]);
}

fn parameter_assignment_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::LoadP { dst: 1, index: 0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn chained_assignment_residual_row() -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadY { dst: 0, index: 1 },
        solve::LinearOp::LoadY { dst: 1, index: 0 },
        solve::LinearOp::Binary {
            dst: 2,
            op: solve::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: 2 },
    ]
}

fn mapped_tangent_matches_directional_difference(model: &solve::SolveModel) {
    let primal = to_scalar_program_block(&model.problem.continuous.implicit_rhs).unwrap();
    let y = [2.0, 4.0];
    let direction = [0.3, -0.7];
    let mut tangent = [0.0; 2];
    solve_eval::eval_scalar_program_block(
        &model.artifacts.continuous.implicit_jacobian_v_scalar,
        &y,
        &[1.0],
        0.0,
        Some(&direction),
        &mut tangent,
    )
    .unwrap();
    assert_eq!(
        tangent,
        [-1.0, -0.7],
        "logical outputs are [d(sample-output), d(sample)]"
    );
    let step = 1e-6;
    let mut plus = [0.0; 2];
    let mut minus = [0.0; 2];
    let y_plus = std::array::from_fn::<_, 2, _>(|index| y[index] + step * direction[index]);
    let y_minus = std::array::from_fn::<_, 2, _>(|index| y[index] - step * direction[index]);
    solve_eval::eval_scalar_program_block(&primal, &y_plus, &[1.0], 0.0, None, &mut plus).unwrap();
    solve_eval::eval_scalar_program_block(&primal, &y_minus, &[1.0], 0.0, None, &mut minus)
        .unwrap();
    for index in 0..2 {
        assert!((tangent[index] - (plus[index] - minus[index]) / (2.0 * step)).abs() < 1e-8);
    }
}
