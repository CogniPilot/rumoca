use super::*;

pub(super) fn jacobian_ad(
    dae: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    n_x: usize,
) -> Vec<Vec<f64>> {
    let n = y.len();
    let mut jac = vec![vec![0.0; n]; n];
    for col in 0..n {
        let mut v = vec![0.0; n];
        v[col] = 1.0;
        let mut jv = vec![0.0; n];
        eval_jacobian_vector_ad(dae, y, p, t, &v, &mut jv, n_x);
        for row in 0..n {
            jac[row][col] = jv[row];
        }
    }
    jac
}

/// Helper: compute the full n×n Jacobian matrix using central finite
/// differences: J[i][j] ≈ (f(y+h*e_j) - f(y-h*e_j)) / (2h).
pub(super) fn jacobian_fd(
    dae: &dae::Dae,
    y: &[f64],
    p: &[f64],
    t: f64,
    n_x: usize,
) -> Vec<Vec<f64>> {
    let n = y.len();
    let h = 1e-7;
    let mut jac = vec![vec![0.0; n]; n];
    for col in 0..n {
        let mut y_plus = y.to_vec();
        let mut y_minus = y.to_vec();
        y_plus[col] += h;
        y_minus[col] -= h;
        let mut f_plus = vec![0.0; n];
        let mut f_minus = vec![0.0; n];
        eval_rhs_equations(dae, &y_plus, p, t, &mut f_plus, n_x);
        eval_rhs_equations(dae, &y_minus, p, t, &mut f_minus, n_x);
        for row in 0..n {
            jac[row][col] = (f_plus[row] - f_minus[row]) / (2.0 * h);
        }
    }
    jac
}

#[test]
fn test_solve_initial_algebraic_accepts_consistent_singular_initial_point() {
    // Structurally singular IC Jacobian: two unknowns (a, b) but both equations
    // constrain only `a`. The default starts y=0 are already consistent.
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("a"),
        dae::Variable::new(dae::VarName::new("a")),
    );
    dae.algebraics.insert(
        dae::VarName::new("b"),
        dae::Variable::new(dae::VarName::new("b")),
    );
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("a"),
        lit(0.0),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("a"),
        lit(0.0),
    )));

    let timeout = rumoca_sim::TimeoutBudget::new(None);
    let ok = solve_initial_algebraic(&mut dae, 0, 1e-9, &timeout)
        .expect("IC solve should not error on consistent singular point");
    assert!(
        ok,
        "IC solve should accept an already-consistent initial point without Newton singular failure"
    );
}

#[test]
fn test_solve_initial_algebraic_writes_seeded_solution_on_singular_jacobian() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("aux"),
        dae::Variable::new(dae::VarName::new("aux")),
    );

    let mut p_var = dae::Variable::new(dae::VarName::new("p"));
    p_var.start = Some(lit(2.5));
    dae.parameters.insert(dae::VarName::new("p"), p_var);

    // Direct assignment gives a meaningful IC seed for aux.
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("aux"),
        var("p"),
    )));
    // Constant residual row keeps Newton singular and non-convergent.
    dae.f_x.push(eq_from(lit(1.0)));

    let timeout = rumoca_sim::TimeoutBudget::new(None);
    let ok = solve_initial_algebraic(&mut dae, 0, 1e-9, &timeout)
        .expect("IC solve should gracefully handle singular Jacobian");
    assert!(!ok, "singular system should report non-converged IC solve");

    let aux_start = dae
        .algebraics
        .get(&dae::VarName::new("aux"))
        .and_then(|v| v.start.as_ref())
        .expect("aux start should be written from seeded IC estimate");
    let aux_val = eval_expr::<f64>(aux_start, &VarEnv::new());
    assert!(
        (aux_val - 2.5).abs() < 1e-12,
        "seeded aux start should be retained when Newton fails singular"
    );
}

#[test]
fn test_solve_initial_algebraic_uses_homotopy_continuation_for_singular_actual_root() {
    // MLS §3.7.4.3 permits initialization-time continuation from simplified to
    // actual. The actual system y*y - 1 = 0 is singular at the default start
    // y=0, but the simplified branch y - 1 = 0 provides a valid homotopy path
    // to the positive root.
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );

    let actual = binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        binop(
            rumoca_ir_core::OpBinary::Mul(Default::default()),
            var("y"),
            var("y"),
        ),
        lit(1.0),
    );
    let simplified = binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("y"),
        lit(1.0),
    );
    dae.f_x.push(eq_from(dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Homotopy,
        args: vec![actual, simplified],
    }));

    let timeout = rumoca_sim::TimeoutBudget::new(None);
    let ok = solve_initial_algebraic(&mut dae, 0, 1e-9, &timeout)
        .expect("homotopy continuation should solve singular initial root");
    assert!(ok, "IC solve should converge through the homotopy path");

    let y_start = dae
        .algebraics
        .get(&dae::VarName::new("y"))
        .and_then(|v| v.start.as_ref())
        .expect("homotopy solve should write solved y start");
    let y_val = eval_expr::<f64>(y_start, &VarEnv::new());
    assert!(
        (y_val - 1.0).abs() < 1e-6,
        "homotopy continuation should land on the positive root, got {y_val}"
    );
}

#[test]
fn test_solve_initial_algebraic_errors_when_residual_stays_non_finite_after_perturbation() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );
    let denom = binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("z"),
        var("z"),
    );
    let inv = binop(
        rumoca_ir_core::OpBinary::Div(Default::default()),
        lit(1.0),
        denom,
    );
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("z"),
        inv,
    )));

    let timeout = rumoca_sim::TimeoutBudget::new(None);
    let err = solve_initial_algebraic(&mut dae, 0, 1e-9, &timeout)
        .expect_err("non-finite IC residual should fail fast");
    match err {
        crate::SimError::SolverError(msg) => {
            assert!(
                msg.contains("initial-condition residual is non-finite"),
                "unexpected error message: {msg}"
            );
        }
        other => panic!("unexpected error: {other}"),
    }
}

#[test]
fn test_project_runtime_keeps_direct_assigned_state_free() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );

    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        var("z"),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("x"),
        var("time"),
    )));

    let timeout = rumoca_sim::TimeoutBudget::new(None);
    let projected =
        project_algebraics_with_fixed_states_at_time(&dae, &[0.0, 0.0], 1, 2.0, 1e-9, &timeout)
            .expect("runtime projection should not error")
            .expect("runtime projection should converge");

    assert!((projected[0] - 2.0).abs() < 1e-9);
    assert!(projected[1].abs() < 1e-9);
}

#[test]
fn test_runtime_projection_masks_track_rows_independently_from_solver_columns() {
    let mut dae = dae::Dae::new();
    dae.inputs.insert(
        dae::VarName::new("src"),
        dae::Variable::new(dae::VarName::new("src")),
    );
    dae.algebraics.insert(
        dae::VarName::new("a"),
        dae::Variable::new(dae::VarName::new("a")),
    );
    dae.algebraics.insert(
        dae::VarName::new("b"),
        dae::Variable::new(dae::VarName::new("b")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );

    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("z"),
        var("src"),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("a"),
        lit(1.0),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("b"),
        binop(
            rumoca_ir_core::OpBinary::Add(Default::default()),
            var("a"),
            lit(1.0),
        ),
    )));

    let masks = build_runtime_projection_masks(&dae, 0, dae.f_x.len());
    assert_eq!(masks.fixed_cols, vec![true, false, true]);
    assert_eq!(masks.ignored_rows, vec![true, true, false]);
}

#[test]
fn test_runtime_projection_masks_fix_solver_clock_aliases_of_discrete_targets() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("sample2.clock"),
        dae::Variable::new(dae::VarName::new("sample2.clock")),
    );
    dae.discrete_reals.insert(
        dae::VarName::new("periodicClock.c"),
        dae::Variable::new(dae::VarName::new("periodicClock.c")),
    );
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("periodicClock.c"),
        var("sample2.clock"),
    )));

    let masks = build_runtime_projection_masks(&dae, 0, dae.f_x.len());
    assert_eq!(masks.fixed_cols, vec![true]);
    assert_eq!(masks.ignored_rows, vec![true]);
}

#[test]
fn test_runtime_direct_seed_reused_env_refreshes_time_dependent_bindings() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("z"),
        var("time"),
    )));

    let ctx = build_runtime_direct_seed_context(&dae, 1, 0);
    let mut y = vec![0.0];
    let mut reusable_env = None;

    let updates_t1 = seed_runtime_direct_assignment_values_with_context_and_env(
        &ctx,
        &dae,
        &mut y,
        &[],
        1.0,
        Some(&mut reusable_env),
    );
    assert_eq!(updates_t1, 1);
    assert!((y[0] - 1.0).abs() <= 1.0e-12);

    let updates_t2 = seed_runtime_direct_assignment_values_with_context_and_env(
        &ctx,
        &dae,
        &mut y,
        &[],
        2.0,
        Some(&mut reusable_env),
    );
    assert_eq!(updates_t2, 1);
    assert!((y[0] - 2.0).abs() <= 1.0e-12);
}

#[test]
fn test_runtime_direct_seed_reused_env_solver_chain_tracks_y_without_solver_env_bindings() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("a"),
        dae::Variable::new(dae::VarName::new("a")),
    );
    dae.algebraics.insert(
        dae::VarName::new("b"),
        dae::Variable::new(dae::VarName::new("b")),
    );
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("a"),
        var("time"),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("b"),
        binop(
            rumoca_ir_core::OpBinary::Add(Default::default()),
            var("a"),
            lit(1.0),
        ),
    )));

    let ctx = build_runtime_direct_seed_context(&dae, 2, 0);
    let mut y = vec![0.0, 0.0];
    let mut reusable_env = None;

    let updates_t1 = seed_runtime_direct_assignment_values_with_context_and_env(
        &ctx,
        &dae,
        &mut y,
        &[],
        1.0,
        Some(&mut reusable_env),
    );
    assert!(updates_t1 > 0);
    assert!((y[0] - 1.0).abs() <= 1.0e-12);
    assert!((y[1] - 2.0).abs() <= 1.0e-12);

    let updates_t2 = seed_runtime_direct_assignment_values_with_context_and_env(
        &ctx,
        &dae,
        &mut y,
        &[],
        2.0,
        Some(&mut reusable_env),
    );
    assert!(updates_t2 > 0);
    assert!((y[0] - 2.0).abs() <= 1.0e-12);
    assert!((y[1] - 3.0).abs() <= 1.0e-12);
}

#[test]
fn test_runtime_direct_seed_orients_raw_indexed_solver_connection_to_runtime_target() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("src"),
        dae::Variable::new(dae::VarName::new("src")),
    );
    dae.discrete_reals.insert(
        dae::VarName::new("u"),
        dae::Variable::new(dae::VarName::new("u")),
    );
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("src[1]"),
        var("u"),
    )));

    let ctx = build_runtime_direct_seed_context(&dae, 1, 0);
    let mut y = vec![3.5];
    let mut reusable_env = None;

    let _updates = seed_runtime_direct_assignment_values_with_context_and_env(
        &ctx,
        &dae,
        &mut y,
        &[],
        0.0,
        Some(&mut reusable_env),
    );

    let env = reusable_env.expect("runtime direct seed reusable env should be populated");
    assert!((env.get("u") - 3.5).abs() <= 1.0e-12);
}

#[test]
fn test_runtime_direct_seed_skips_plain_alias_candidate_when_target_has_unique_defining_rhs() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("w"),
        dae::Variable::new(dae::VarName::new("w")),
    );
    dae.algebraics.insert(
        dae::VarName::new("y"),
        dae::Variable::new(dae::VarName::new("y")),
    );
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("y"),
        var("time"),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("y"),
        var("w"),
    )));

    let ctx = build_runtime_direct_seed_context(&dae, 2, 0);
    let mut y = vec![0.0, 0.0];
    let updates = seed_runtime_direct_assignment_values_with_context(&ctx, &dae, &mut y, &[], 1.0);

    assert!(updates > 0);
    assert!((y[0] - 0.0).abs() <= 1.0e-12);
    assert!(
        (y[1] - 1.0).abs() <= 1.0e-12,
        "direct seed should keep the unique non-alias defining equation"
    );
}

#[test]
fn test_runtime_direct_seed_skips_projection_fixed_solver_targets() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("node"),
        dae::Variable::new(dae::VarName::new("node")),
    );
    dae.f_x.push(eq_from(lit(0.0)));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("node"),
        var("x"),
    )));

    let ctx = build_runtime_direct_seed_context(&dae, 2, 1);
    let mut y = vec![3.0, 0.0];
    let mut reusable_env = None;
    let blocked_solver_cols = vec![true, true];
    let updates =
        seed_runtime_direct_assignment_values_with_context_and_env_and_blocked_solver_cols(
            &ctx,
            &dae,
            &mut y,
            &[],
            0.0,
            Some(&mut reusable_env),
            &blocked_solver_cols,
        );

    assert_eq!(updates, 0);
    assert!((y[1] - 0.0).abs() <= 1.0e-12);
}

#[test]
fn test_runtime_projection_in_place_reuses_scratch_for_time_dependent_residual() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("z"),
        var("time"),
    )));

    let compiled_runtime =
        build_compiled_runtime_newton_context(&dae, 1).expect("compile runtime Newton");
    let timeout = rumoca_sim::TimeoutBudget::new(None);
    let fixed_cols = vec![false];
    let ignored_rows = vec![false];
    let mut y = vec![0.0];
    let mut scratch = RuntimeProjectionScratch::default();

    let converged_t1 =
        project_algebraics_with_fixed_states_at_time_with_context_and_cache_in_place(
            &dae,
            &mut y,
            RuntimeProjectionContext {
                p: &[],
                compiled_runtime: Some(&compiled_runtime),
                fixed_cols: &fixed_cols,
                ignored_rows: &ignored_rows,
                branch_local_analog_cols: &[],
                direct_seed_ctx: None,
                direct_seed_env_cache: None,
            },
            RuntimeProjectionStep {
                y_seed: &[],
                n_x: 0,
                t_eval: 1.0,
                tol: 1.0e-9,
                timeout: &timeout,
            },
            None,
            &mut scratch,
        )
        .expect("runtime projection should not error");
    assert!(converged_t1);
    assert!((y[0] - 1.0).abs() <= 1.0e-12);

    let converged_t2 =
        project_algebraics_with_fixed_states_at_time_with_context_and_cache_in_place(
            &dae,
            &mut y,
            RuntimeProjectionContext {
                p: &[],
                compiled_runtime: Some(&compiled_runtime),
                fixed_cols: &fixed_cols,
                ignored_rows: &ignored_rows,
                branch_local_analog_cols: &[],
                direct_seed_ctx: None,
                direct_seed_env_cache: None,
            },
            RuntimeProjectionStep {
                y_seed: &[],
                n_x: 0,
                t_eval: 2.0,
                tol: 1.0e-9,
                timeout: &timeout,
            },
            None,
            &mut scratch,
        )
        .expect("runtime projection should not error");
    assert!(converged_t2);
    assert!((y[0] - 2.0).abs() <= 1.0e-12);
}

#[test]
fn test_runtime_projection_cached_step_reuses_prefilled_rhs() {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("z"),
        var("time"),
    )));

    let compiled_runtime =
        build_compiled_runtime_newton_context(&dae, 1).expect("compile runtime Newton");
    let timeout = rumoca_sim::TimeoutBudget::new(None);
    let fixed_cols = vec![false];
    let ignored_rows = vec![false];
    let mut y = vec![0.0];
    let mut scratch = RuntimeProjectionScratch::default();
    let cached_jacobian = build_init_jacobian_dense(
        &InitJacobianEvalContext {
            dae: &dae,
            y: &y,
            p: &[],
            t_eval: 2.0,
            n_x: 0,
            use_initial: false,
            compiled_initial: None,
            compiled_runtime: Some(&compiled_runtime),
        },
        &fixed_cols,
        &timeout,
    )
    .expect("build runtime Jacobian");
    let converged_t2 = project_algebraics_with_cached_runtime_jacobian_step_in_place(
        &dae,
        &mut y,
        RuntimeProjectionContext {
            p: &[],
            compiled_runtime: Some(&compiled_runtime),
            fixed_cols: &fixed_cols,
            ignored_rows: &ignored_rows,
            branch_local_analog_cols: &[],
            direct_seed_ctx: None,
            direct_seed_env_cache: None,
        },
        RuntimeProjectionStep {
            y_seed: &[],
            n_x: 0,
            t_eval: 2.0,
            tol: 1.0e-9,
            timeout: &timeout,
        },
        &cached_jacobian,
        &mut scratch,
    )
    .expect("cached runtime projection step should not error");

    assert!(converged_t2);
    assert!((y[0] - 2.0).abs() <= 1.0e-12);
}

#[test]
fn test_project_runtime_converges_on_rank_deficient_consistent_system() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("a"),
        dae::Variable::new(dae::VarName::new("a")),
    );
    dae.algebraics.insert(
        dae::VarName::new("b"),
        dae::Variable::new(dae::VarName::new("b")),
    );

    // State row (fixed during runtime projection).
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        lit(0.0),
    )));
    // Rank-deficient algebraic rows: both constrain only `a`.
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("a"),
        lit(1.0),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("a"),
        lit(1.0),
    )));

    let timeout = rumoca_sim::TimeoutBudget::new(None);
    let projected = project_algebraics_with_fixed_states_at_time(
        &dae,
        &[0.0, 0.0, 0.0],
        1,
        0.0,
        1e-9,
        &timeout,
    )
    .expect("runtime projection should not error")
    .expect("rank-deficient but consistent runtime projection should converge");

    assert!((projected[1] - 1.0).abs() < 1e-9);
    assert!(projected[2].is_finite());
}

fn assert_matrix_close(lhs: &nalgebra::DMatrix<f64>, rhs: &nalgebra::DMatrix<f64>, tol: f64) {
    assert_eq!(lhs.nrows(), rhs.nrows());
    assert_eq!(lhs.ncols(), rhs.ncols());
    for i in 0..lhs.nrows() {
        for j in 0..lhs.ncols() {
            let a = lhs[(i, j)];
            let b = rhs[(i, j)];
            assert!(
                (a - b).abs() <= tol,
                "matrix mismatch at ({i}, {j}): {a} vs {b}"
            );
        }
    }
}

fn build_coloring_test_dae() -> dae::Dae {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x1"),
        dae::Variable::new(dae::VarName::new("x1")),
    );
    dae.states.insert(
        dae::VarName::new("x2"),
        dae::Variable::new(dae::VarName::new("x2")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z1"),
        dae::Variable::new(dae::VarName::new("z1")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z2"),
        dae::Variable::new(dae::VarName::new("z2")),
    );

    // ODE rows first
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x1")],
        },
        var("z1"),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x2")],
        },
        var("z2"),
    )));

    // Algebraic rows
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("z1"),
        binop(
            rumoca_ir_core::OpBinary::Mul(Default::default()),
            var("x1"),
            var("x1"),
        ),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("z2"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sin,
            args: vec![var("x2")],
        },
    )));

    dae
}

struct TestNewtonContexts<'a> {
    compiled_initial: Option<&'a CompiledInitialNewtonContext>,
    compiled_runtime: Option<&'a CompiledRuntimeNewtonContext>,
}

fn init_jac_ctx<'a>(
    dae: &'a dae::Dae,
    y: &'a [f64],
    p: &'a [f64],
    t_eval: f64,
    n_x: usize,
    use_initial: bool,
    contexts: TestNewtonContexts<'a>,
) -> InitJacobianEvalContext<'a> {
    InitJacobianEvalContext {
        dae,
        y,
        p,
        t_eval,
        n_x,
        use_initial,
        compiled_initial: contexts.compiled_initial,
        compiled_runtime: contexts.compiled_runtime,
    }
}

fn build_initial_mode_newton_test_dae() -> dae::Dae {
    let mut dae = dae::Dae::new();
    dae.algebraics.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.f_x.push(eq_from(dae::Expression::If {
        branches: vec![(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Initial,
                args: vec![],
            },
            binop(
                rumoca_ir_core::OpBinary::Sub(Default::default()),
                binop(
                    rumoca_ir_core::OpBinary::Mul(Default::default()),
                    var("x"),
                    var("x"),
                ),
                lit(4.0),
            ),
        )],
        else_branch: Box::new(binop(
            rumoca_ir_core::OpBinary::Sub(Default::default()),
            binop(
                rumoca_ir_core::OpBinary::Mul(Default::default()),
                lit(3.0),
                var("x"),
            ),
            lit(4.0),
        )),
    }));
    dae
}

#[test]
fn test_compiled_runtime_newton_context_residual_matches_runtime_reference() {
    let dae = build_coloring_test_dae();
    let y = vec![0.25, -0.4, 0.6, -0.7];
    let p = default_params(&dae);
    let compiled =
        build_compiled_runtime_newton_context(&dae, y.len()).expect("compile runtime Newton");
    let mut got = vec![0.0; y.len()];
    let mut expected = vec![0.0; y.len()];

    eval_compiled_runtime_residual(&compiled, &y, &p, 0.0, &mut got);
    eval_rhs_equations(&dae, &y, &p, 0.0, &mut expected, 2);

    assert_eq!(got, expected);
}

#[test]
fn test_compiled_runtime_newton_context_residual_uses_runtime_tail_start_chain() {
    let mut dae = dae::Dae::new();

    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );

    let mut p = dae::Variable::new(dae::VarName::new("p"));
    p.start = Some(lit(2.0));
    dae.parameters.insert(dae::VarName::new("p"), p);

    let mut u = dae::Variable::new(dae::VarName::new("u"));
    u.start = Some(binop(
        rumoca_ir_core::OpBinary::Add(Default::default()),
        var("p"),
        lit(1.0),
    ));
    dae.inputs.insert(dae::VarName::new("u"), u);

    let mut d = dae::Variable::new(dae::VarName::new("d"));
    d.start = Some(binop(
        rumoca_ir_core::OpBinary::Mul(Default::default()),
        var("u"),
        lit(3.0),
    ));
    dae.discrete_reals.insert(dae::VarName::new("d"), d);

    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("x"),
        var("d"),
    )));

    let y = vec![9.0];
    let p = default_params(&dae);
    let compiled =
        build_compiled_runtime_newton_context(&dae, y.len()).expect("compile runtime Newton");
    let mut got = vec![0.0; y.len()];
    let mut expected = vec![0.0; y.len()];

    eval_compiled_runtime_residual(&compiled, &y, &p, 0.0, &mut got);
    eval_rhs_equations(&dae, &y, &p, 0.0, &mut expected, 1);

    assert_eq!(got, expected);
    assert_eq!(got, vec![0.0]);
}

#[test]
fn test_compiled_runtime_newton_context_jacobian_matches_runtime_ad() {
    let dae = build_coloring_test_dae();
    let y = vec![0.25, -0.4, 0.6, -0.7];
    let p = default_params(&dae);
    let v = vec![1.0, -0.5, 0.25, 0.75];
    let compiled =
        build_compiled_runtime_newton_context(&dae, y.len()).expect("compile runtime Newton");
    let mut got = vec![0.0; y.len()];
    let mut expected = vec![0.0; y.len()];

    eval_compiled_runtime_jacobian(&compiled, &y, &p, 0.0, &v, &mut got);
    eval_jacobian_vector_ad(&dae, &y, &p, 0.0, &v, &mut expected, 2);

    assert_eq!(got, expected);
}

#[test]
fn test_build_init_jacobian_colored_matches_dense() {
    let dae = build_coloring_test_dae();
    let y = vec![0.25, -0.4, 0.6, -0.7];
    let p = default_params(&dae);
    let fixed = vec![false, false];
    let timeout = rumoca_sim::TimeoutBudget::new(None);
    let compiled =
        build_compiled_runtime_newton_context(&dae, y.len()).expect("compile runtime Newton");
    let ctx = init_jac_ctx(
        &dae,
        &y,
        &p,
        0.0,
        2,
        false,
        TestNewtonContexts {
            compiled_initial: None,
            compiled_runtime: Some(&compiled),
        },
    );

    let dense =
        build_init_jacobian_dense(&ctx, &fixed, &timeout).expect("dense Jacobian should build");
    let colored = build_init_jacobian_colored(&ctx, &fixed, &timeout)
        .expect("colored Jacobian build should not error")
        .expect("colored Jacobian should not fallback for this test case");

    assert_matrix_close(&dense, &colored, 1e-12);
}

#[test]
fn test_build_init_jacobian_colored_skips_fixed_state_columns() {
    let dae = build_coloring_test_dae();
    let y = vec![0.25, -0.4, 0.6, -0.7];
    let p = default_params(&dae);
    let fixed = vec![true, false];
    let timeout = rumoca_sim::TimeoutBudget::new(None);
    let compiled =
        build_compiled_runtime_newton_context(&dae, y.len()).expect("compile runtime Newton");
    let ctx = init_jac_ctx(
        &dae,
        &y,
        &p,
        0.0,
        2,
        false,
        TestNewtonContexts {
            compiled_initial: None,
            compiled_runtime: Some(&compiled),
        },
    );

    let dense =
        build_init_jacobian_dense(&ctx, &fixed, &timeout).expect("dense Jacobian should build");
    let colored = build_init_jacobian_colored(&ctx, &fixed, &timeout)
        .expect("colored Jacobian build should not error")
        .expect("colored Jacobian should not fallback for this test case");

    assert_matrix_close(&dense, &colored, 1e-12);
    for row in 0..colored.nrows() {
        assert_eq!(colored[(row, 0)], 0.0);
    }
}

fn build_time_switch_jacobian_dae() -> dae::Dae {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    dae.algebraics.insert(
        dae::VarName::new("z"),
        dae::Variable::new(dae::VarName::new("z")),
    );

    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        var("z"),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        var("z"),
        dae::Expression::If {
            branches: vec![(
                binop(
                    rumoca_ir_core::OpBinary::Lt(Default::default()),
                    var("time"),
                    lit(1.0),
                ),
                var("x"),
            )],
            else_branch: Box::new(binop(
                rumoca_ir_core::OpBinary::Mul(Default::default()),
                lit(2.0),
                var("x"),
            )),
        },
    )));

    dae
}

#[test]
fn test_build_init_jacobian_respects_time_dependent_if_branches() {
    let dae = build_time_switch_jacobian_dae();
    let y = vec![0.25, 0.5];
    let p = default_params(&dae);
    let fixed = vec![false];
    let timeout = rumoca_sim::TimeoutBudget::new(None);
    let compiled =
        build_compiled_runtime_newton_context(&dae, y.len()).expect("compile runtime Newton");
    let ctx_before = init_jac_ctx(
        &dae,
        &y,
        &p,
        0.5,
        1,
        false,
        TestNewtonContexts {
            compiled_initial: None,
            compiled_runtime: Some(&compiled),
        },
    );
    let ctx_after = init_jac_ctx(
        &dae,
        &y,
        &p,
        2.0,
        1,
        false,
        TestNewtonContexts {
            compiled_initial: None,
            compiled_runtime: Some(&compiled),
        },
    );

    let jac_before = build_init_jacobian_dense(&ctx_before, &fixed, &timeout)
        .expect("dense Jacobian before event should build");
    let jac_after = build_init_jacobian_dense(&ctx_after, &fixed, &timeout)
        .expect("dense Jacobian after event should build");
    assert!((jac_before[(1, 0)] + 1.0).abs() < 1e-12);
    assert!((jac_after[(1, 0)] + 2.0).abs() < 1e-12);

    let colored_before = build_init_jacobian_colored(&ctx_before, &fixed, &timeout)
        .expect("colored Jacobian before event should not error")
        .expect("colored Jacobian before event should build");
    let colored_after = build_init_jacobian_colored(&ctx_after, &fixed, &timeout)
        .expect("colored Jacobian after event should not error")
        .expect("colored Jacobian after event should build");
    assert_matrix_close(&jac_before, &colored_before, 1e-12);
    assert_matrix_close(&jac_after, &colored_after, 1e-12);
}

#[test]
fn test_eval_jacobian_vector_seeds_size1_array_aliases() {
    let mut dae = dae::Dae::new();
    dae.states.insert(
        dae::VarName::new("x"),
        dae::Variable::new(dae::VarName::new("x")),
    );
    let mut y_arr = dae::Variable::new(dae::VarName::new("y"));
    y_arr.dims = vec![1];
    dae.outputs.insert(dae::VarName::new("y"), y_arr);

    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("x")],
        },
        lit(0.0),
    )));
    dae.f_x.push(eq_from(binop(
        rumoca_ir_core::OpBinary::Sub(Default::default()),
        dae::Expression::VarRef {
            name: dae::VarName::new("y[1]"),
            subscripts: vec![],
        },
        var("x"),
    )));

    let y = vec![0.0, 0.0];
    let p = default_params(&dae);
    let mut out = vec![0.0; 2];
    let v = vec![0.0, 1.0];
    eval_jacobian_vector_ad(&dae, &y, &p, 0.0, &v, &mut out, 1);

    assert!(
        (out[1] - 1.0).abs() < 1e-12,
        "expected dy[1] derivative to propagate through y[1] alias"
    );
}

#[test]
fn test_compiled_initial_newton_context_residual_matches_initial_reference() {
    let dae = build_initial_mode_newton_test_dae();
    let y = vec![2.0];
    let p = default_params(&dae);
    let compiled =
        build_compiled_initial_newton_context(&dae, y.len()).expect("compile initial Newton");
    let mut got = vec![0.0; y.len()];
    let mut expected = vec![0.0; y.len()];

    eval_compiled_initial_residual(&compiled, &y, &p, 0.0, &mut got);
    eval_rhs_equations_initial(&dae, &y, &p, 0.0, &mut expected, 0);

    assert_eq!(got, expected);
}

#[test]
fn test_compiled_initial_newton_context_jacobian_matches_initial_ad() {
    let dae = build_initial_mode_newton_test_dae();
    let y = vec![2.0];
    let p = default_params(&dae);
    let v = vec![1.0];
    let compiled =
        build_compiled_initial_newton_context(&dae, y.len()).expect("compile initial Newton");
    let mut got = vec![0.0; y.len()];
    let mut expected = vec![0.0; y.len()];

    eval_compiled_initial_jacobian(&compiled, &y, &p, 0.0, &v, &mut got);
    eval_jacobian_vector_ad_initial(&dae, &y, &p, 0.0, &v, &mut expected, 0);

    assert_eq!(got, expected);
}

#[test]
fn test_build_init_jacobian_initial_mode_uses_compiled_initial_context() {
    let dae = build_initial_mode_newton_test_dae();
    let y = vec![2.0];
    let p = default_params(&dae);
    let fixed = vec![false];
    let timeout = rumoca_sim::TimeoutBudget::new(None);
    let compiled =
        build_compiled_initial_newton_context(&dae, y.len()).expect("compile initial Newton");
    let ctx = init_jac_ctx(
        &dae,
        &y,
        &p,
        0.0,
        0,
        true,
        TestNewtonContexts {
            compiled_initial: Some(&compiled),
            compiled_runtime: None,
        },
    );

    let jac = build_init_jacobian_dense(&ctx, &fixed, &timeout)
        .expect("initial-mode dense Jacobian should build");
    assert!((jac[(0, 0)] - 4.0).abs() <= 1.0e-12);
}

// =========================================================================
// BLT elimination numerical equivalence tests
//
// Verify that the reduced DAE (after eliminate_trivial) is numerically
// equivalent to the original DAE at concrete test points.
//
// Approach: evaluate the original residual with eliminated variables
// computed from substitutions, then evaluate the reduced residual.
// The remaining equations' residuals must match.
// =========================================================================
