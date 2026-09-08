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
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["output".to_string(), "sample".to_string()],
            ..Default::default()
        },
        algebraic_scalar_count: 2,
        parameter_count: 1,
        static_parameter_names: vec!["p".to_string()],
        compiled_parameter_len: 1,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(residual),
        implicit_row_targets: vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))],
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![
                solve::AlgebraicProjectionBlock {
                    rows: vec![1],
                    y_indices: vec![1],
                    tearing: None,
                },
                solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                    tearing: None,
                },
            ],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 2, 1),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("projection fixture satisfies the checked root contract"),
        initial_y: vec![0.0, 0.0],
        solver_nominals: vec![1.0; 2],
        parameters: vec![1.0],
        ..empty_binary64_first_product_model()
    };
    let model = std::sync::Arc::new(model);
    let runtime = SolveRuntime::new(std::sync::Arc::clone(&model)).expect("runtime should prepare");
    let mut solver_y = model.initial_y().to_vec();

    runtime
        .refresh_algebraic_and_output_slots(0.0, &mut solver_y, model.parameters(), 1.0e-12, 4)
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
