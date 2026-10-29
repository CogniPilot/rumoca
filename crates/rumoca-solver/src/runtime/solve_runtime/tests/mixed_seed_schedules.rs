//! Exact assignments around a nonlinear projection retain the full tangent.
use super::*;
use solve::{BinaryOp as B, LinearOp as L};

fn residual_rows() -> Vec<Vec<L>> {
    // a=2*x; b*b=a; c=b+p; x'=c.
    vec![
        derivative_placeholder_row(0),
        vec![
            L::LoadY { dst: 0, index: 1 },
            L::LoadY { dst: 1, index: 0 },
            L::Const { dst: 2, value: 2.0 },
            L::Binary {
                dst: 3,
                op: B::Mul,
                lhs: 1,
                rhs: 2,
            },
            L::Binary {
                dst: 4,
                op: B::Sub,
                lhs: 0,
                rhs: 3,
            },
            L::StoreOutput { src: 4 },
        ],
        vec![
            L::LoadY { dst: 0, index: 2 },
            L::LoadY { dst: 1, index: 1 },
            L::Binary {
                dst: 2,
                op: B::Mul,
                lhs: 0,
                rhs: 0,
            },
            L::Binary {
                dst: 3,
                op: B::Sub,
                lhs: 2,
                rhs: 1,
            },
            L::StoreOutput { src: 3 },
        ],
        vec![
            L::LoadY { dst: 0, index: 3 },
            L::LoadY { dst: 1, index: 2 },
            L::LoadP { dst: 2, index: 0 },
            L::Binary {
                dst: 3,
                op: B::Sub,
                lhs: 0,
                rhs: 1,
            },
            L::Binary {
                dst: 4,
                op: B::Sub,
                lhs: 3,
                rhs: 2,
            },
            L::StoreOutput { src: 4 },
        ],
    ]
}

fn model() -> solve::SolveModel {
    let mut model = warm_start_test_model();
    model.problem.layout = solve::VarLayout::from_parts(Default::default(), 4, 1);
    model.problem.solve_layout.algebraic_scalar_count = 3;
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.parameters = vec![3.0].into();
    model.initial_y = vec![2.0, 4.0, 2.0, 5.0].into();
    let primal = spanned_block(residual_rows(), "mixed_seed.mo");
    let jvp = solve_eval::ad::lower_scalar_program_block_full_jvp(&primal, 4).unwrap();
    let state_jvp = solve_eval::ad::lower_scalar_program_block_ad(primal.programs()).unwrap();
    model.problem.continuous.implicit_rhs = solve::ComputeBlock::from_scalar_program_block(primal);
    model.artifacts.continuous.implicit_jacobian_v_scalar = jvp;
    model.artifacts.continuous.implicit_jacobian_v = solve::ComputeBlock::from_scalar_program_block(
        spanned_block(state_jvp, "mixed_state_jvp.mo"),
    );
    model.problem.continuous.implicit_row_targets = (0..4)
        .map(|i| Some(solve::scalar_slot_y(i)))
        .collect::<Vec<_>>()
        .into();
    model.problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![vec![
                L::LoadY { dst: 0, index: 3 },
                L::StoreOutput { src: 0 },
            ]],
            "mixed_derivative.mo",
        ));
    model.artifacts.continuous.full_jacobian_v = spanned_block(
        vec![vec![
            L::LoadSeed { dst: 0, index: 3 },
            L::StoreOutput { src: 0 },
        ]],
        "mixed_derivative_jvp.mo",
    );
    model.problem.continuous.algebraic_projection_plan.blocks = (1..4)
        .map(|i| solve::AlgebraicProjectionBlock {
            rows: vec![i],
            y_indices: vec![i],
            tearing: None,
            alternate_charts: Vec::new(),
        })
        .collect::<Vec<_>>()
        .into();
    derive_test_structural_artifacts(&mut model);
    model
}

#[test]
fn mixed_seed_sweep_matches_implicit_projection_and_analytic_parameter_tangent() {
    let model = model();
    let runtime = SolveRuntime::new_fixture(&model).unwrap();
    let view = runtime.scalar_execution().unwrap();
    assert!(!view.scalar.derivative_refresh.causal_solution_certified);
    assert!(view.scalar.causal_seed_schedules.has_mixed_steps());
    for (x, p, sx, sp) in [
        (2.0, 3.0, 1.0, 0.0),
        (2.0, 3.0, 0.0, 1.0),
        (8.0, -1.0, 2.0, 3.0),
    ] {
        let b: f64 = (2.0_f64 * x).sqrt();
        let y = [x, 2.0 * x, b, b + p];
        let params = [p];
        let lin = AlgebraicLinearization {
            t: 0.0,
            params: &params,
            settle: AlgebraicSettle {
                tol: 1e-10,
                max_iters: 8,
            },
        };
        let mut expected = [sx, 0.0, 0.0, 0.0, sp];
        view.seed_refresh_with_plan(&view.scalar.derivative_refresh, lin, &y, &mut expected)
            .unwrap();
        assert!((expected[3] - (sx / b + sp)).abs() < 1e-9);
        let mut output = [0.0];
        view.eval_full_jacobian_v_ad_into(lin, &[x], &[sx, 0.0, 0.0, 0.0, sp], &mut output)
            .unwrap();
        assert!((output[0] - expected[3]).abs() < 1e-9);
    }
}

#[test]
fn mixed_seed_shared_tensor_forwarding_is_single_assignment_and_preserves_full_jvp() {
    for forwarded in [vec![0], vec![1], vec![0, 2, 4], vec![0, 1, 2, 3, 4]] {
        let mut first = vec![
            L::LoadY { dst: 0, index: 0 },
            L::LoadP { dst: 1, index: 0 },
            L::Binary {
                dst: 2,
                op: B::Add,
                lhs: 0,
                rhs: 1,
            },
        ];
        first.extend(forwarded.iter().map(|_| L::StoreOutput { src: 2 }));
        let first_targets = forwarded.iter().map(|&index| index + 1).collect::<Vec<_>>();
        let mut second = vec![L::TensorLoad {
            dst_start: 0,
            input: solve::TensorInputKind::Y,
            input_start: 1,
            count: 5,
            seed_start: None,
            lanes: 1,
        }];
        second.extend((0..5).map(|src| L::StoreOutput { src }));
        let second_targets = [6, 7, 8, 9, 10];
        let inputs = [
            solve::AssignmentProgram {
                ops: &first,
                targets: &first_targets,
            },
            solve::AssignmentProgram {
                ops: &second,
                targets: &second_targets,
            },
        ];
        let shared = solve::SharedValueSegments::derive(&inputs);
        shared.check(&inputs).unwrap();
        assert_eq!(shared.segments().len(), 1, "sharing must remain active");
        let ops = shared.segments()[0].ops();
        assert_single_assignment(ops);
        let primal = spanned_block(vec![ops.to_vec()], "partial_tensor_forwarding.mo");
        let tangent = solve_eval::ad::lower_scalar_program_block_full_jvp(&primal, 6).unwrap();
        let program =
            DirectionalProgram::prepare("tensor_forwarding", tangent, None, None).unwrap();
        let seed = [2.0, 20.0, 3.0, 40.0, 5.0, 60.0, 7.0];
        let mut output = Vec::new();
        program
            .eval_program_outputs(
                0,
                solve_eval::JacobianEvalInputs {
                    y: &[2.0, 10.0, 11.0, 12.0, 13.0, 14.0],
                    p: &[3.0],
                    t: 0.0,
                    seed: &seed,
                },
                RowEvalContext::default(),
                &mut output,
            )
            .unwrap();
        let mut expected = vec![9.0; forwarded.len()];
        expected.extend((0..5).map(|index| {
            if forwarded.contains(&index) {
                9.0
            } else {
                seed[index + 1]
            }
        }));
        assert_eq!(output, expected);
    }
}

fn assert_single_assignment(ops: &[L]) {
    let mut written = BTreeSet::new();
    for op in ops {
        if let Some(start) = op.dst_register() {
            for offset in 0..op.dst_register_count() {
                assert!(
                    written.insert(start + offset as u32),
                    "rewritten destination: {op:?}"
                );
            }
        }
    }
}
