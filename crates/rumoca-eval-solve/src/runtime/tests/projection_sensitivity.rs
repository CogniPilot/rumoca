use super::*;

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
            tearing: None,
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
    mirror_scalar_implicit_jvp(&mut model);
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

fn parameter_projection_residual() -> solve::ComputeBlock {
    use solve::LinearOp::{Binary, LoadP, LoadY, StoreOutput};
    use solve::{BinaryOp, ComputeBlock};

    ComputeBlock::from_scalar_program_block(spanned_block(
        vec![
            vec![LoadY { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            vec![
                LoadY { dst: 0, index: 1 },
                LoadP { dst: 1, index: 0 },
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
        "projection_parameter_path.mo",
    ))
}

fn parameter_projection_jvp(include_parameter_seed: bool) -> solve::ScalarProgramBlock {
    use solve::BinaryOp;
    use solve::LinearOp::{Binary, LoadP, LoadSeed, LoadY, StoreOutput};

    let mut algebraic_row = vec![
        LoadSeed { dst: 0, index: 1 },
        LoadP { dst: 1, index: 0 },
        LoadSeed { dst: 2, index: 0 },
        Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
    ];
    let (output, name) = if include_parameter_seed {
        algebraic_row.extend([
            LoadY { dst: 4, index: 0 },
            LoadSeed { dst: 5, index: 2 },
            Binary {
                dst: 6,
                op: BinaryOp::Mul,
                lhs: 4,
                rhs: 5,
            },
            Binary {
                dst: 7,
                op: BinaryOp::Add,
                lhs: 3,
                rhs: 6,
            },
            Binary {
                dst: 8,
                op: BinaryOp::Sub,
                lhs: 0,
                rhs: 7,
            },
        ]);
        (8, "projection_parameter_path_full_jvp.mo")
    } else {
        algebraic_row.push(Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        });
        (4, "projection_parameter_path_y_only_jvp.mo")
    };
    algebraic_row.push(StoreOutput { src: output });
    spanned_block(
        vec![
            vec![LoadSeed { dst: 0, index: 0 }, StoreOutput { src: 0 }],
            algebraic_row,
        ],
        name,
    )
}

fn parameter_projection_model() -> solve::SolveModel {
    let mut model = solve::SolveModel::default();
    model.problem.layout = solve::VarLayout::from_parts(Default::default(), 2, 1);
    model.problem.solve_layout.state_scalar_count = 1;
    model.problem.solve_layout.algebraic_scalar_count = 1;
    model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "z".to_string()];
    model.problem.continuous.implicit_rhs = parameter_projection_residual();
    model.problem.continuous.implicit_row_targets =
        vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
    model.problem.continuous.algebraic_projection_plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![1],
            y_indices: vec![1],
            tearing: None,
        }],
    };
    model.artifacts.continuous.implicit_jacobian_v_scalar = parameter_projection_jvp(true);
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(parameter_projection_jvp(false));
    model.initial_y = vec![0.0, 0.0];
    model
}

#[test]
fn algebraic_projection_seed_includes_direct_parameter_path() {
    let model = parameter_projection_model();
    let runtime = SolveRuntime::new(&model).expect("parameter projection fixture should prepare");
    let mut out = [0.0; 2];
    runtime
        .project_state_sensitivity_to_solver_y(
            AlgebraicLinearization {
                t: 0.0,
                params: &[4.0],
                settle: AlgebraicSettle {
                    tol: 1.0e-12,
                    max_iters: 8,
                },
            },
            &[3.0],
            &[2.0],
            0,
            &mut out,
        )
        .expect("projection sensitivity should include state and parameter paths");

    assert_eq!(out[0], 2.0);
    assert_eq!(out[1], 11.0, "dz/dp = p*dx/dp + x");
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
            tearing: None,
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
    mirror_scalar_implicit_jvp(&mut model);
    model.initial_y = vec![0.0, 0.0, 0.0];
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
    mirror_scalar_implicit_jvp(&mut model);
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

#[test]
fn state_jacobian_resolves_linear_algebraic_loop_sensitivity() {
    let runtime = SolveRuntime::new(&linear_algebraic_loop_state_model())
        .expect("valid runtime should prepare");
    assert_eq!(runtime.derivative_refresh.simultaneous_plan.blocks.len(), 1);
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
            .contains("algebraic projection sensitivity matrix is singular"),
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
