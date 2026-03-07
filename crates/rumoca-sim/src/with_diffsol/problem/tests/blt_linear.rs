use super::*;

use rumoca_eval_dae::runtime::eval_expr;

/// Evaluate the substitutions and set eliminated variable values in an env.
pub(super) fn apply_substitutions_to_env(
    subs: &[rumoca_phase_solve::eliminate::Substitution],
    env: &mut VarEnv<f64>,
) {
    for sub in subs {
        let val = eval_expr::<f64>(&sub.expr, env);
        for key in &sub.env_keys {
            env.set(key, val);
        }
    }
}

/// Build a 2-state + 2-algebraic system for BLT testing.
///
/// States: x, y
/// Algebraics: a, b
///
/// ODE: der(x) = a          (eq0)
/// ODE: der(y) = b          (eq1)
/// ALG: 0 = a - 2*x         (eq2: a = 2*x)
/// ALG: 0 = b - (x + y)     (eq3: b = x + y)
///
/// After BLT elimination, a and b should be eliminated:
///   der(x) = 2*x
///   der(y) = x + y
fn build_blt_test_dae_linear() -> Dae {
    let mut dae = Dae::new();

    let mut vx = Variable::new(VarName::new("x"));
    vx.start = Some(lit(1.0));
    dae.states.insert(VarName::new("x"), vx);

    let mut vy = Variable::new(VarName::new("y"));
    vy.start = Some(lit(0.0));
    dae.states.insert(VarName::new("y"), vy);

    dae.algebraics
        .insert(VarName::new("a"), Variable::new(VarName::new("a")));
    dae.algebraics
        .insert(VarName::new("b"), Variable::new(VarName::new("b")));

    let der_x = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var("x")],
    };
    let der_y = Expression::BuiltinCall {
        function: BuiltinFunction::Der,
        args: vec![var("y")],
    };

    // eq0: 0 = der(x) - a
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        der_x,
        var("a"),
    )));

    // eq1: 0 = der(y) - b
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        der_y,
        var("b"),
    )));

    // eq2: 0 = a - 2*x
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("a"),
        binop(OpBinary::Mul(Default::default()), lit(2.0), var("x")),
    )));

    // eq3: 0 = b - (x + y)
    dae.f_x.push(eq_from(binop(
        OpBinary::Sub(Default::default()),
        var("b"),
        binop(OpBinary::Add(Default::default()), var("x"), var("y")),
    )));

    dae
}

/// Test BLT elimination on a linear system and verify numerical equivalence.
#[test]
fn test_blt_elimination_numerical_equivalence_linear() {
    let original = build_blt_test_dae_linear();
    let mut reduced = original.clone();
    let elim = crate::with_diffsol::eliminate::eliminate_trivial(&mut reduced);

    // Should have eliminated a and b
    assert_eq!(elim.n_eliminated, 2, "should eliminate 2 variables");
    assert!(reduced.algebraics.is_empty(), "no algebraics should remain");
    assert_eq!(reduced.f_x.len(), 2, "2 ODE equations should remain");

    // Test at several points
    for &(xv, yv) in &[(1.0, 0.0), (2.0, 3.0), (-1.0, 0.5)] {
        // Compute eliminated variables from substitutions
        let av = 2.0 * xv; // a = 2*x
        let bv = xv + yv; // b = x + y

        // Build state vector and evaluate original system
        // Original: y = [x, y, a, b] (4 vars)
        let y_orig = vec![xv, yv, av, bv];
        let p_orig = default_params(&original);
        let n_x = count_states(&original);
        let mut f_orig = vec![0.0; original.f_x.len()];
        eval_rhs_equations(&original, &y_orig, &p_orig, 0.0, &mut f_orig, n_x);

        // Build state vector for reduced system and evaluate
        // Reduced: y = [x, y] (2 vars, but substitutions applied)
        let y_red = vec![xv, yv];
        let p_red = default_params(&reduced);
        let n_x_red = count_states(&reduced);
        let mut f_red = vec![0.0; reduced.f_x.len()];
        eval_rhs_equations(&reduced, &y_red, &p_red, 0.0, &mut f_red, n_x_red);

        // The ODE residuals should match (remaining equations)
        // Original eq0 and eq1 (ODE rows) should match reduced eq0 and eq1
        for i in 0..reduced.f_x.len() {
            assert!(
                (f_orig[i] - f_red[i]).abs() < 1e-10,
                "x={xv}, y={yv}: residual[{i}] mismatch: orig={:.8e}, reduced={:.8e}",
                f_orig[i],
                f_red[i]
            );
        }

        // Verify substitution values are correct by evaluating them
        let env = rumoca_eval_dae::runtime::build_env(&reduced, &y_red, &p_red, 0.0);
        let mut env_with_subs = env.clone();
        apply_substitutions_to_env(&elim.substitutions, &mut env_with_subs);

        // Check a = 2*x
        let a_val = env_with_subs.vars.get("a").copied().unwrap_or(f64::NAN);
        assert!(
            (a_val - av).abs() < 1e-10,
            "substitution a: got {a_val}, expected {av}"
        );

        // Check b = x + y
        let b_val = env_with_subs.vars.get("b").copied().unwrap_or(f64::NAN);
        assert!(
            (b_val - bv).abs() < 1e-10,
            "substitution b: got {b_val}, expected {bv}"
        );
    }
}
