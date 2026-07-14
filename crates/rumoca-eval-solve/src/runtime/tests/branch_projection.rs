use super::*;

#[test]
fn explicit_branch_seeds_refresh_only_their_dependency_plan() {
    use solve::LinearOp::{Const, LoadY, StoreOutput};

    let mut model = projection_coupled_state_model(2.0);
    // The derivative and root depend only on the state. The full observation
    // projection still owns algebraic `a = 2*x`.
    model.problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![vec![Const { dst: 0, value: 0.0 }, StoreOutput { src: 0 }]],
            "branch_seed_derivative.mo",
        ));
    model.problem.events.root_conditions = spanned_block(
        vec![vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }]],
        "branch_seed_root.mo",
    );
    model.problem.events.root_relation_memory_targets = vec![None];
    let runtime = SolveRuntime::new(&model).expect("valid branch-seed runtime should prepare");

    let mut derivative_seed = vec![99.0, 42.0];
    let mut derivative = [f64::NAN];
    runtime
        .eval_state_derivatives_with_guess_into(
            0.0,
            &[3.0],
            &[],
            &mut derivative_seed,
            1.0e-12,
            32,
            &mut derivative,
        )
        .expect("derivative branch should settle");

    let mut root_seed = vec![99.0, 42.0];
    let mut root = [f64::NAN];
    runtime
        .eval_root_search_conditions_with_guess_into(RootSearchInput {
            t: 0.0,
            state: &[3.0],
            params: &[],
            guess: &mut root_seed,
            tol: 1.0e-12,
            max_iters: 32,
            out: &mut root,
        })
        .expect("root branch should settle");

    let mut observation_seed = vec![99.0, 42.0];
    runtime
        .full_solver_y_with_guess(0.0, &[3.0], &[], &mut observation_seed, 1.0e-12, 32)
        .expect("full observation projection should settle");

    assert_eq!(derivative_seed, vec![3.0, 42.0]);
    assert_eq!(root_seed, vec![3.0, 42.0]);
    assert_eq!(observation_seed, vec![3.0, 6.0]);
    assert_eq!(derivative, [0.0]);
    assert_eq!(root, [3.0]);
}

#[test]
fn coupled_projection_preserves_the_accepted_local_branch() {
    use solve::LinearOp::{Binary, Const, LoadY, StoreOutput};
    use solve::{BinaryOp, ComputeBlock};

    let mut model = linear_algebraic_loop_state_model();
    // a = b; b = F(a), with nearby root 1 and remote root 100.
    model.problem.continuous.implicit_rhs = ComputeBlock::from_scalar_program_block(spanned_block(
        vec![
            vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            vec![
                LoadY { dst: 0, index: 1 },
                LoadY { dst: 1, index: 2 },
                Binary {
                    dst: 2,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                StoreOutput { src: 2 },
            ],
            vec![
                LoadY { dst: 0, index: 1 },
                Const { dst: 1, value: 1.0 },
                Binary {
                    dst: 2,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                Const {
                    dst: 3,
                    value: 100.0,
                },
                Binary {
                    dst: 4,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 3,
                },
                Binary {
                    dst: 5,
                    op: BinaryOp::Mul,
                    lhs: 2,
                    rhs: 4,
                },
                Const {
                    dst: 6,
                    value: 1.0 / 99.0,
                },
                Binary {
                    dst: 7,
                    op: BinaryOp::Mul,
                    lhs: 5,
                    rhs: 6,
                },
                Binary {
                    dst: 8,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 7,
                },
                LoadY { dst: 9, index: 2 },
                Binary {
                    dst: 10,
                    op: BinaryOp::Sub,
                    lhs: 9,
                    rhs: 8,
                },
                StoreOutput { src: 10 },
            ],
        ],
        "accepted_branch_projection.mo",
    ));
    model.initial_y = vec![0.0, 1.1, 1.1];
    let runtime = SolveRuntime::new(&model).expect("valid multi-root projection runtime");
    assert!(runtime.algebraic_refresh.iterative);

    let tol = 1.0e-8;
    let mut accepted_seed = vec![0.0, 1.1, 1.1];
    let first = runtime
        .eval_state_derivatives_with_guess(0.0, &[0.0], &[], &mut accepted_seed, tol, 256)
        .expect("accepted branch projection should settle");
    let map = accepted_seed[1] - (accepted_seed[1] - 1.0) * (accepted_seed[1] - 100.0) / 99.0;
    let residual = (accepted_seed[1] - accepted_seed[2])
        .abs()
        .max((accepted_seed[2] - map).abs());
    assert!(
        (accepted_seed[1] - 1.0).abs() <= tol,
        "projection jumped from the accepted local branch to {}",
        accepted_seed[1]
    );
    assert!((accepted_seed[2] - 1.0).abs() <= tol);
    assert!(
        residual <= tol,
        "projection residual {residual} exceeds {tol}"
    );

    let mut projected_seed = accepted_seed.clone();
    let second = runtime
        .eval_state_derivatives_with_guess(0.0, &[0.0], &[], &mut projected_seed, tol, 256)
        .expect("reprojecting the accepted branch should settle");
    assert!((first[0] - second[0]).abs() <= tol);
}

#[test]
fn tolerance_converged_projection_does_not_polish_to_remote_root() {
    use solve::LinearOp::{Binary, Const, LoadY, StoreOutput};
    use solve::{BinaryOp, ComputeBlock};

    let mut model = linear_algebraic_loop_state_model();
    let epsilon = 1.0e-6;
    model.problem.continuous.implicit_rhs = ComputeBlock::from_scalar_program_block(spanned_block(
        vec![
            vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            vec![
                LoadY { dst: 0, index: 1 },
                LoadY { dst: 1, index: 2 },
                Binary {
                    dst: 2,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                StoreOutput { src: 2 },
            ],
            vec![
                LoadY { dst: 0, index: 1 },
                Const {
                    dst: 1,
                    value: 100.0,
                },
                Binary {
                    dst: 2,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                Const {
                    dst: 3,
                    value: epsilon,
                },
                Binary {
                    dst: 4,
                    op: BinaryOp::Mul,
                    lhs: 2,
                    rhs: 3,
                },
                Binary {
                    dst: 5,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 4,
                },
                LoadY { dst: 6, index: 2 },
                Binary {
                    dst: 7,
                    op: BinaryOp::Sub,
                    lhs: 6,
                    rhs: 5,
                },
                StoreOutput { src: 7 },
            ],
        ],
        "ill_scaled_branch_projection.mo",
    ));
    model.initial_y = vec![0.0, 0.0, 0.0];
    let runtime = SolveRuntime::new(&model).expect("ill-scaled projection should prepare");
    assert!(runtime.algebraic_refresh.iterative);

    let mut accepted_seed = model.initial_y.clone();
    runtime
        .full_solver_y_with_guess(0.0, &[0.0], &[], &mut accepted_seed, 1.0e-4, 32)
        .expect("already-tolerant accepted branch should settle");

    assert!(
        accepted_seed[1].abs() <= 1.0e-6 && accepted_seed[2].abs() <= 1.0e-6,
        "tolerance polish jumped to the remote root: {accepted_seed:?}"
    );
}

#[test]
fn confirmed_root_override_wins_after_other_relation_memories_update() {
    use solve::LinearOp::{LoadY, StoreOutput};

    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.algebraic_scalar_count = 2;
    model.problem.solve_layout.relation_memory_parameter_indices = vec![0, 1];
    model.initial_y = vec![-1.0, -1.0];
    model.problem.events.root_conditions = spanned_block(
        vec![
            vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            vec![LoadY { dst: 0, index: 1 }, StoreOutput { src: 0 }],
        ],
        "coincident_relation_roots.mo",
    );
    model.problem.events.root_relation_memory_targets = vec![
        Some(solve::ScalarSlot::P {
            index: 0,
            byte_offset: 0,
        }),
        Some(solve::ScalarSlot::P {
            index: 1,
            byte_offset: 0,
        }),
    ];
    let runtime = SolveRuntime::new(&model).expect("coincident relation roots should prepare");
    let mut y = model.initial_y.clone();
    let mut p = vec![0.0, 0.0];

    runtime
        .settle_projected_runtime_and_relation_memory_with_overrides(
            ProjectedRuntimeSettleInput {
                y: &mut y,
                p: &mut p,
                t: 0.0,
                tol: 1.0e-12,
                max_iters: 8,
                root_relation_overrides: &[(0, 0.0)],
            },
            |_, _| Ok(false),
        )
        .expect("relation memory update should converge atomically");

    assert_eq!(p, vec![0.0, 1.0]);
}

#[test]
fn converged_coupled_projection_polishes_reconstructed_zero_flow() {
    use solve::LinearOp::{Binary, Const, LoadY, StoreOutput};
    use solve::{BinaryOp, ComputeBlock};

    // Artifact-derived analogue of the severe `ground.p.i` channels in
    // IdealTriacCircuit and HBridge_TrianglePWM_RL. Structural elimination
    // reconstructs the grounded flow as the difference of two branch currents;
    // those currents belong to a coupled projection block. A merely
    // tolerance-converged solve leaks its residual into that observation even
    // though the physical connection flow is exactly zero.
    let mut model = linear_algebraic_loop_state_model();
    let coupled_row = |target: usize, other: usize| {
        vec![
            LoadY {
                dst: 0,
                index: target,
            },
            Const { dst: 1, value: 2.0 },
            Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LoadY {
                dst: 3,
                index: other,
            },
            Binary {
                dst: 4,
                op: BinaryOp::Sub,
                lhs: 2,
                rhs: 3,
            },
            Const { dst: 5, value: 1.0 },
            Binary {
                dst: 6,
                op: BinaryOp::Sub,
                lhs: 4,
                rhs: 5,
            },
            StoreOutput { src: 6 },
        ]
    };
    model.problem.continuous.implicit_rhs = ComputeBlock::from_scalar_program_block(spanned_block(
        vec![
            vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            coupled_row(1, 2),
            coupled_row(2, 1),
        ],
        "ground_flow_projection.mo",
    ));
    model.initial_y = vec![0.0, 1.0 + 5.0e-11, 1.0 - 5.0e-11];
    let runtime = SolveRuntime::new(&model).expect("coupled flow fixture should prepare");
    assert!(runtime.algebraic_refresh.iterative);

    let mut observation = model.initial_y.clone();
    runtime
        .full_solver_y_with_guess(0.0, &[0.0], &[], &mut observation, 1.0e-10, 32)
        .expect("coupled observation projection should settle");

    assert!(
        (observation[1] - observation[2]).abs() <= 4.0 * f64::EPSILON,
        "reconstructed grounded flow retained projection residual: {:?}",
        observation
    );
}
