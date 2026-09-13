use super::*;

fn constrained_state_with_nonlinear_output(target: f64) -> solve::SolveModel {
    use solve::LinearOp::{Binary, Const, LoadSeed, LoadY, StoreOutput};
    let mut model = nonlinear_right_limit_seed_model();
    model.problem.continuous.derivative_rhs = solve::ComputeBlock::from_scalar_program_block(
        zero_derivative("fmi_me_constrained_stationary_state.mo"),
    );
    model.artifacts.continuous.full_jacobian_v =
        zero_derivative("fmi_me_constrained_stationary_jacobian.mo");
    model.problem.continuous.manifold_residual =
        solve::ComputeBlock::from_scalar_program_block(block(
            vec![vec![
                LoadY { dst: 0, index: 0 },
                Const {
                    dst: 1,
                    value: target,
                },
                Binary {
                    dst: 2,
                    op: solve::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                StoreOutput { src: 2 },
            ]],
            "fmi_me_constrained_state.mo",
        ));
    model.artifacts.continuous.manifold_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(block(
            vec![vec![LoadSeed { dst: 0, index: 0 }, StoreOutput { src: 0 }]],
            "fmi_me_constrained_state_jacobian.mo",
        ));
    model.problem.continuous.manifold_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![0],
            y_indices: vec![0],
            tearing: None,
        }],
    };
    model
}

#[test]
fn state_constraint_is_projected_before_evaluating_its_algebraic_output() {
    // x = 4, der(x) = 0, a*a = x. An off-manifold x = -1 has no real a,
    // although the projected state and its output are well-defined.
    let mut kernel = instantiate(&constrained_state_with_nonlinear_output(4.0));
    let mut state = [-1.0];
    assert!(
        kernel
            .project_continuous_states(&mut state)
            .expect("state-only projection")
    );
    assert!((state[0] - 4.0).abs() < 1e-10);
    kernel
        .set_continuous_states(&state)
        .expect("projected state");
    let observation = kernel.observe().expect("output at the corrected state");
    assert!((observation.solver_y[1] - 2.0).abs() < 1e-10);
}

#[test]
fn state_projection_does_not_hide_an_invalid_algebraic_output() {
    let mut kernel = instantiate(&constrained_state_with_nonlinear_output(-1.0));
    let mut state = [4.0];
    assert!(
        kernel
            .project_continuous_states(&mut state)
            .expect("state-only projection")
    );
    assert!((state[0] + 1.0).abs() < 1e-10);
    kernel
        .set_continuous_states(&state)
        .expect("projected state");
    assert!(
        kernel.observe().is_err(),
        "a*a = -1 remains an invalid observation"
    );
}

#[test]
fn a_manifold_program_cannot_consume_an_algebraic_warm_start() {
    let mut model = constrained_state_with_nonlinear_output(4.0);
    model.problem.continuous.manifold_residual =
        solve::ComputeBlock::from_scalar_program_block(block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "fmi_me_invalid_algebraic_manifold.mo",
        ));
    let mut kernel = instantiate(&model);
    let mut state = [4.0];
    let error = kernel
        .project_continuous_states(&mut state)
        .expect_err("non-state dependency");
    assert_eq!(error.stage(), Some(MeStage::ManifoldProjection));
    let expected = rumoca_eval_solve::EvalSolveError::MissingInput {
        vector: "y",
        index: 1,
        len: 1,
        span: None,
    }
    .to_string();
    assert!(matches!(error.into_kind(), MeError::Evaluation { message } if message == expected));
    assert_eq!(state, [4.0], "failed projection preserves the input state");
}
