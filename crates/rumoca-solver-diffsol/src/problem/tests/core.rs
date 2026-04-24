use super::jacobian_and_newton::{jacobian_ad, jacobian_fd};
use super::*;

/// Build a system with a chain of algebraic dependencies.
///
/// State: x
/// Algebraics: a, b, c
///
/// ODE: der(x) = c^2            (eq0)
/// ALG: 0 = a - sin(x)          (eq1: a = sin(x))
/// ALG: 0 = b - (a + 1)         (eq2: b = a + 1 = sin(x) + 1)
/// ALG: 0 = c - (a * b)         (eq3: c = a*b = sin(x)*(sin(x)+1))
///
/// BLT should eliminate a→b→c in topological order.
fn build_blt_test_dae_chain() -> Dae {
    let mut dae = Dae::new();

    let mut vx = Variable::new(VarName::new("x"));
    vx.start = Some(lit(1.0));
    dae.states.insert(VarName::new("x"), vx);

    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));
    dae.algebraics
        .insert(VarName::new("c"), Variable::new(VarName::new("c")));

    let der_x = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var("x")],
    };

    // eq0: 0 = der(x) - c^2
    let c2 = binop(OpBinary::Exp(Default::default()), var("c"), lit(2.0));
    dae.f_x
        .push(eq_from(binop(OpBinary::Sub(Default::default()), der_x, c2)));

    // eq1: 0 = a - sin(x)
    let sin_x = Expression::BuiltinCall {
        function: BuiltinFunction::Sin,
        args: vec![var("x")],
    };
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("a"),
        sin_x,
    )));

    // eq2: 0 = b - (a + 1)
    let a_plus_1 = binop(OpBinary::Add(Default::default()), var("a"), lit(1.0));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("b"),
        a_plus_1,
    )));

    // eq3: 0 = c - a*b
    let ab = binop(OpBinary::Mul(Default::default()), var("a"), var("b"));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("c"),
        ab,
    )));

    dae
}

/// Test BLT with a chain of dependent algebraic variables.
#[test]
fn test_blt_elimination_numerical_equivalence_chain() {
    let original = build_blt_test_dae_chain();
    let mut reduced = original.clone();
    let elim = crate::eliminate::eliminate_trivial(&mut reduced);

    assert_eq!(elim.n_eliminated, 3, "should eliminate a, b, c");
    assert!(reduced.algebraics.is_empty());
    assert_eq!(reduced.f_x.len(), 1, "only ODE equation should remain");

    for &xv in &[0.0, 0.5, 1.0, -0.7, 2.0] {
        let av = xv.sin();
        let bv = av + 1.0;
        let cv = av * bv;

        // Original system: y = [x, a, b, c]
        let y_orig = vec![xv, av, bv, cv];
        let p_orig = default_params(&original);
        let n_x_orig = count_states(&original);
        let mut f_orig = vec![0.0; original.f_x.len()];
        eval_rhs_equations(&original, &y_orig, &p_orig, 0.0, &mut f_orig, n_x_orig);

        // Reduced system: y = [x]
        let y_red = vec![xv];
        let p_red = default_params(&reduced);
        let n_x_red = count_states(&reduced);
        let mut f_red = vec![0.0; reduced.f_x.len()];
        eval_rhs_equations(&reduced, &y_red, &p_red, 0.0, &mut f_red, n_x_red);

        // The ODE residual should match
        assert!(
            (f_orig[0] - f_red[0]).abs() < 1e-10,
            "x={xv}: ODE residual mismatch: orig={:.8e}, reduced={:.8e}",
            f_orig[0],
            f_red[0]
        );

        // Verify substitution chain gives correct values
        let env = rumoca_eval_dae::runtime::build_env(&reduced, &y_red, &p_red, 0.0);
        let mut env_subs = env.clone();
        apply_substitutions_to_env(&elim.substitutions, &mut env_subs);

        let a_sub = env_subs.vars.get("a").copied().unwrap_or(f64::NAN);
        let b_sub = env_subs.vars.get("b").copied().unwrap_or(f64::NAN);
        let c_sub = env_subs.vars.get("c").copied().unwrap_or(f64::NAN);

        assert!(
            (a_sub - av).abs() < 1e-10,
            "x={xv}: a substitution: got {a_sub}, expected {av}"
        );
        assert!(
            (b_sub - bv).abs() < 1e-10,
            "x={xv}: b substitution: got {b_sub}, expected {bv}"
        );
        assert!(
            (c_sub - cv).abs() < 1e-10,
            "x={xv}: c substitution: got {c_sub}, expected {cv}"
        );
    }
}

/// Build a system where BLT should NOT eliminate (multi-unknown block).
///
/// State: x
/// Algebraics: a, b
///
/// ODE: der(x) = a + b          (eq0)
/// ALG: 0 = a*b - x             (eq1: coupled, can't isolate a or b)
/// ALG: 0 = a^2 + b^2 - 2*x    (eq2: coupled)
///
/// The algebraic equations form a 2x2 block that can't be solved
/// symbolically, so a and b should NOT be eliminated.
fn build_blt_test_dae_unsolvable_block() -> Dae {
    let mut dae = Dae::new();

    let mut vx = Variable::new(VarName::new("x"));
    vx.start = Some(lit(1.0));
    dae.states.insert(VarName::new("x"), vx);

    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));

    let der_x = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var("x")],
    };

    // eq0: 0 = der(x) - (a + b)
    let ab_sum = binop(OpBinary::Add(Default::default()), var("a"), var("b"));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        der_x,
        ab_sum,
    )));

    // eq1: 0 = a*b - x
    let ab_prod = binop(OpBinary::Mul(Default::default()), var("a"), var("b"));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        ab_prod,
        var("x"),
    )));

    // eq2: 0 = a^2 + b^2 - 2*x
    let a2 = binop(OpBinary::Exp(Default::default()), var("a"), lit(2.0));
    let b2 = binop(OpBinary::Exp(Default::default()), var("b"), lit(2.0));
    let sum_sq = binop(OpBinary::Add(Default::default()), a2, b2);
    let two_x = binop(OpBinary::Mul(Default::default()), lit(2.0), var("x"));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        sum_sq,
        two_x,
    )));

    dae
}

/// Test that BLT doesn't eliminate coupled algebraic blocks.
#[test]
fn test_blt_preserves_coupled_blocks() {
    let mut dae = build_blt_test_dae_unsolvable_block();
    let elim = crate::eliminate::eliminate_trivial(&mut dae);

    // The coupled a*b - x and a^2+b^2 - 2x can't be solved for a or b
    assert_eq!(
        elim.n_eliminated, 0,
        "coupled block should not be eliminated"
    );
    assert_eq!(dae.f_x.len(), 3, "all equations should remain");
    assert_eq!(dae.algebraics.len(), 2, "both algebraics should remain");
}

/// Build a mixed system: some variables eliminable, some not.
///
/// State: x
/// Algebraics: a, b, c
///
/// ODE: der(x) = a + b*c        (eq0)
/// ALG: 0 = a - 3*x             (eq1: a = 3*x, trivially solvable)
/// ALG: 0 = b*c - x             (eq2: coupled, b and c can't be isolated)
/// ALG: 0 = b^2 - c             (eq3: coupled with eq2)
fn build_blt_test_dae_partial_elimination() -> Dae {
    let mut dae = Dae::new();

    let mut vx = Variable::new(VarName::new("x"));
    vx.start = Some(lit(1.0));
    dae.states.insert(VarName::new("x"), vx);

    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));
    dae.algebraics
        .insert(VarName::new("c"), Variable::new(VarName::new("c")));

    let der_x = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var("x")],
    };

    // eq0: 0 = der(x) - (a + b*c)
    let bc = binop(OpBinary::Mul(Default::default()), var("b"), var("c"));
    let a_plus_bc = binop(OpBinary::Add(Default::default()), var("a"), bc);
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        der_x,
        a_plus_bc,
    )));

    // eq1: 0 = a - 3*x
    let three_x = binop(OpBinary::Mul(Default::default()), lit(3.0), var("x"));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("a"),
        three_x,
    )));

    // eq2: 0 = b*c - x
    let bc2 = binop(OpBinary::Mul(Default::default()), var("b"), var("c"));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        bc2,
        var("x"),
    )));

    // eq3: 0 = b^2 - c
    let b2 = binop(OpBinary::Exp(Default::default()), var("b"), lit(2.0));
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        b2,
        var("c"),
    )));

    dae
}

/// Test partial elimination: direct assignments are eliminated while at least
/// one nonlinear algebraic unknown remains.
#[test]
fn test_blt_partial_elimination() {
    let original = build_blt_test_dae_partial_elimination();
    let mut reduced = original.clone();
    let elim = crate::eliminate::eliminate_trivial(&mut reduced);

    // Direct assignments should be eliminated (`a = 3*x`, `c = b^2`), but one
    // nonlinear unknown remains (`b` in b^3 - x = 0).
    assert_eq!(
        elim.n_eliminated, 2,
        "expected direct assignments eliminated"
    );
    assert!(
        !reduced.algebraics.contains_key(&VarName::new("a")),
        "`a` should be removed"
    );
    assert!(
        !reduced.algebraics.contains_key(&VarName::new("c")),
        "`c` should be removed"
    );
    assert!(
        reduced.algebraics.contains_key(&VarName::new("b")),
        "`b` should remain as nonlinear unknown"
    );
    assert_eq!(reduced.f_x.len(), 2, "2 equations should remain");

    // Numerical check: at x=1, a=3, b and c satisfy b*c=1, b^2=c
    // b^2=c, b*c=1 → b*b^2=1 → b^3=1 → b=1, c=1
    let xv = 1.0;
    let av = 3.0;
    let bv = 1.0;
    let cv = 1.0;

    // Original: y = [x, a, b, c]
    let y_orig = vec![xv, av, bv, cv];
    let p_orig = default_params(&original);
    let n_x_orig = count_states(&original);
    let mut f_orig = vec![0.0; original.f_x.len()];
    eval_rhs_equations(&original, &y_orig, &p_orig, 0.0, &mut f_orig, n_x_orig);

    // Reduced: y = [x, b]
    let y_red = vec![xv, bv];
    let p_red = default_params(&reduced);
    let n_x_red = count_states(&reduced);
    let mut f_red = vec![0.0; reduced.f_x.len()];
    eval_rhs_equations(&reduced, &y_red, &p_red, 0.0, &mut f_red, n_x_red);

    // ODE residual (eq0) should match between original and reduced
    assert!(
        (f_orig[0] - f_red[0]).abs() < 1e-10,
        "ODE residual mismatch: orig={:.8e}, reduced={:.8e}",
        f_orig[0],
        f_red[0]
    );

    // Algebraic residuals should be ~0 at the consistent point.
    // (ODE residuals are non-zero because der(x)=0 at evaluation time.)
    let n_x_orig_cnt = count_states(&original);
    for (i, f) in f_orig.iter().enumerate().skip(n_x_orig_cnt) {
        assert!(
            f.abs() < 1e-10,
            "original algebraic residual[{i}] should be ~0, got {f:.8e}",
        );
    }
    let n_x_red_cnt = count_states(&reduced);
    for (i, f) in f_red.iter().enumerate().skip(n_x_red_cnt) {
        assert!(
            f.abs() < 1e-10,
            "reduced algebraic residual[{i}] should be ~0, got {f:.8e}",
        );
    }
}

/// Test BLT + Jacobian: verify the Jacobian of the reduced system is correct.
///
/// Uses the chain DAE (x → a=sin(x), b=a+1, c=a*b) and checks that
/// after elimination, the ODE Jacobian df/dx matches finite differences.
#[test]
fn test_blt_reduced_jacobian_matches_fd() {
    let mut dae = build_blt_test_dae_chain();
    let _elim = crate::eliminate::eliminate_trivial(&mut dae);

    // Reduced system: only 1 ODE equation, der(x) = c^2
    // where c = sin(x)*(sin(x)+1) after substitution.
    // f[0] = -(sin(x)*(sin(x)+1))^2  (negated ODE row)
    // df/dx by chain rule is nontrivial — that's why we use FD to check.
    let n_x = count_states(&dae);
    let p = default_params(&dae);

    for &xv in &[0.5, 1.0, -0.3, 2.0] {
        let y = vec![xv];
        let jac_ad = jacobian_ad(&dae, &y, &p, 0.0, n_x);
        let jac_fd = jacobian_fd(&dae, &y, &p, 0.0, n_x);

        assert_jacobians_close(&jac_ad, &jac_fd, 1e-5, &format!("blt_reduced_chain_x={xv}"));
    }
}
