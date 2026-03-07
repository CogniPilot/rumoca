use super::*;

/// Helper: compare two Jacobian matrices entry-by-entry.
pub(super) fn assert_jacobians_close(
    jac_ad: &[Vec<f64>],
    jac_fd: &[Vec<f64>],
    tol: f64,
    label: &str,
) {
    let n = jac_ad.len();
    for row in 0..n {
        for col in 0..n {
            let ad = jac_ad[row][col];
            let fd = jac_fd[row][col];
            let diff = (ad - fd).abs();
            let scale = fd.abs().max(ad.abs()).max(1.0);
            assert!(
                diff / scale < tol,
                "{label}: J[{row}][{col}] mismatch: AD={ad:.8e}, FD={fd:.8e}, diff={diff:.2e}"
            );
        }
    }
}

/// Test 1: Linear system — der(x) = a*x + b*z, 0 = x - z
///
/// State: x, Algebraic: z, Params: a=2, b=3
/// Residual (after sign convention):
///   f[0] = -(a*x + b*z)   (ODE row, negated)
///   f[1] = x - z           (algebraic row)
///
/// Analytical Jacobian:
///   df/dy = [[-a, -b],
///            [ 1, -1]]
/// At a=2, b=3: [[-2, -3], [1, -1]]
#[test]
fn test_jacobian_ad_vs_fd_linear() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    let mut p_a = Variable::new(VarName::new("a"));
    p_a.start = Some(lit(2.0));
    let mut p_b = Variable::new(VarName::new("b"));
    p_b.start = Some(lit(3.0));
    dae.parameters.insert(VarName::new("a"), p_a);
    dae.parameters.insert(VarName::new("b"), p_b);

    // eq0: 0 = a*x + b*z  (ODE row, for der(x))
    let ax = binop(OpBinary::Mul(Default::default()), var("a"), var("x"));
    let bz = binop(OpBinary::Mul(Default::default()), var("b"), var("z"));
    let rhs0 = binop(OpBinary::Add(Default::default()), ax, bz);
    dae.f_x.push(eq_from(rhs0));

    // eq1: 0 = x - z  (algebraic row)
    let rhs1 = binop(OpBinary::Sub(Default::default()), var("x"), var("z"));
    dae.f_x.push(eq_from(rhs1));

    let n_x = count_states(&dae);
    let y = vec![1.0, 1.0]; // x=1, z=1
    let p = default_params(&dae);

    let jac_ad = jacobian_ad(&dae, &y, &p, 0.0, n_x);
    let jac_fd = jacobian_fd(&dae, &y, &p, 0.0, n_x);

    // Check analytical: [[-2, -3], [1, -1]]
    assert!(
        (jac_ad[0][0] - (-2.0)).abs() < 1e-10,
        "J[0][0] should be -2"
    );
    assert!(
        (jac_ad[0][1] - (-3.0)).abs() < 1e-10,
        "J[0][1] should be -3"
    );
    assert!((jac_ad[1][0] - 1.0).abs() < 1e-10, "J[1][0] should be 1");
    assert!(
        (jac_ad[1][1] - (-1.0)).abs() < 1e-10,
        "J[1][1] should be -1"
    );

    assert_jacobians_close(&jac_ad, &jac_fd, 1e-5, "linear");
}

/// Test 2: Nonlinear system — der(x) = x^2, 0 = x*z - 1
///
/// State: x, Algebraic: z
/// Residual:
///   f[0] = -(x^2)       (ODE row, negated)
///   f[1] = x*z - 1      (algebraic row)
///
/// Analytical Jacobian:
///   df/dy = [[-2x,  0],
///            [  z,  x]]
/// At x=2, z=0.5: [[-4, 0], [0.5, 2]]
#[test]
fn test_jacobian_ad_vs_fd_nonlinear() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // eq0: 0 = x^2  (ODE row)
    let rhs0 = binop(OpBinary::Exp(Default::default()), var("x"), lit(2.0));
    dae.f_x.push(eq_from(rhs0));

    // eq1: 0 = x*z - 1  (algebraic row)
    let xz = binop(OpBinary::Mul(Default::default()), var("x"), var("z"));
    let rhs1 = binop(OpBinary::Sub(Default::default()), xz, lit(1.0));
    dae.f_x.push(eq_from(rhs1));

    let n_x = count_states(&dae);
    let y = vec![2.0, 0.5]; // x=2, z=0.5
    let p = default_params(&dae);

    let jac_ad = jacobian_ad(&dae, &y, &p, 0.0, n_x);
    let jac_fd = jacobian_fd(&dae, &y, &p, 0.0, n_x);

    // Analytical: [[-4, 0], [0.5, 2]]
    assert!(
        (jac_ad[0][0] - (-4.0)).abs() < 1e-10,
        "J[0][0] should be -4"
    );
    assert!((jac_ad[0][1] - 0.0).abs() < 1e-10, "J[0][1] should be 0");
    assert!((jac_ad[1][0] - 0.5).abs() < 1e-10, "J[1][0] should be 0.5");
    assert!((jac_ad[1][1] - 2.0).abs() < 1e-10, "J[1][1] should be 2");

    assert_jacobians_close(&jac_ad, &jac_fd, 1e-5, "nonlinear");
}

/// Test 3: Transcendental system — der(x) = sin(x)*exp(z), 0 = cos(z) - x
///
/// Residual:
///   f[0] = -(sin(x)*exp(z))      (ODE row, negated)
///   f[1] = cos(z) - x            (algebraic row)
///
/// Analytical Jacobian:
///   df/dy = [[-cos(x)*exp(z), -sin(x)*exp(z)],
///            [          -1,         -sin(z)  ]]
#[test]
fn test_jacobian_ad_vs_fd_transcendental() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // eq0: 0 = sin(x) * exp(z)
    let sin_x = Expression::BuiltinCall {
        function: BuiltinFunction::Sin,
        args: vec![var("x")],
    };
    let exp_z = Expression::BuiltinCall {
        function: BuiltinFunction::Exp,
        args: vec![var("z")],
    };
    let rhs0 = binop(OpBinary::Mul(Default::default()), sin_x, exp_z);
    dae.f_x.push(eq_from(rhs0));

    // eq1: 0 = cos(z) - x
    let cos_z = Expression::BuiltinCall {
        function: BuiltinFunction::Cos,
        args: vec![var("z")],
    };
    let rhs1 = binop(OpBinary::Sub(Default::default()), cos_z, var("x"));
    dae.f_x.push(eq_from(rhs1));

    let n_x = count_states(&dae);
    let y = vec![0.5, 0.3]; // x=0.5, z=0.3
    let p = default_params(&dae);

    let jac_ad = jacobian_ad(&dae, &y, &p, 0.0, n_x);
    let jac_fd = jacobian_fd(&dae, &y, &p, 0.0, n_x);

    // Analytical at x=0.5, z=0.3:
    let x = 0.5_f64;
    let z = 0.3_f64;
    let expected = [
        [-(x.cos() * z.exp()), -(x.sin() * z.exp())],
        [-1.0, -z.sin()],
    ];

    for row in 0..2 {
        for col in 0..2 {
            assert!(
                (jac_ad[row][col] - expected[row][col]).abs() < 1e-10,
                "J[{row}][{col}] should be {:.8e}, got {:.8e}",
                expected[row][col],
                jac_ad[row][col]
            );
        }
    }

    assert_jacobians_close(&jac_ad, &jac_fd, 1e-5, "transcendental");
}

/// Regression: constant `asin(1.0)` must not inject NaN into AD Jacobian rows.
///
/// Before the fix, Dual::asin produced `0/0 -> NaN` for constant arguments at
/// the singular boundary and the Jacobian row was clamped to zero.
#[test]
fn test_jacobian_constant_asin_boundary_keeps_variable_derivative() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));

    let asin_one = Expression::BuiltinCall {
        function: BuiltinFunction::Asin,
        args: vec![lit(1.0)],
    };
    let rhs = binop(OpBinary::Sub(Default::default()), asin_one, var("x"));
    dae.f_x.push(eq_from(rhs));

    let n_x = count_states(&dae);
    let y = vec![0.0];
    let p = default_params(&dae);

    let jac_ad = jacobian_ad(&dae, &y, &p, 0.0, n_x);
    let jac_fd = jacobian_fd(&dae, &y, &p, 0.0, n_x);

    assert!(jac_ad[0][0].is_finite(), "AD Jacobian must remain finite");
    assert!((jac_ad[0][0] + 1.0).abs() < 1e-10, "expected d/dx = -1");
    assert_jacobians_close(&jac_ad, &jac_fd, 1e-6, "asin_boundary_constant");
}

/// Test 4: Larger system (3 states) with mixed terms.
///
/// States: x, y; Algebraic: z
/// der(x) = y*z
/// der(y) = -x + z^2
/// 0 = x + y - 2*z
///
/// Analytical Jacobian:
///   [[-0   -z   -y  ]    (ODE row, negated: -(y*z))
///    [ 1   -0   -2z ]    (ODE row, negated: -(-x + z^2))
///    [ 1    1   -2  ]]   (algebraic: x + y - 2*z)
#[test]
fn test_jacobian_ad_vs_fd_three_vars() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.states
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // eq0: 0 = y*z  (ODE row for x)
    let rhs0 = binop(OpBinary::Mul(Default::default()), var("y"), var("z"));
    dae.f_x.push(eq_from(rhs0));

    // eq1: 0 = -x + z^2  (ODE row for y)
    let neg_x = Expression::Unary {
        op: rumoca_ir_core::OpUnary::Minus(Default::default()),
        rhs: Box::new(var("x")),
    };
    let z2 = binop(OpBinary::Exp(Default::default()), var("z"), lit(2.0));
    let rhs1 = binop(OpBinary::Add(Default::default()), neg_x, z2);
    dae.f_x.push(eq_from(rhs1));

    // eq2: 0 = x + y - 2*z  (algebraic row)
    let xy = binop(OpBinary::Add(Default::default()), var("x"), var("y"));
    let two_z = binop(OpBinary::Mul(Default::default()), lit(2.0), var("z"));
    let rhs2 = binop(OpBinary::Sub(Default::default()), xy, two_z);
    dae.f_x.push(eq_from(rhs2));

    let n_x = count_states(&dae);
    assert_eq!(n_x, 2);
    let yv = vec![1.0, 2.0, 1.5]; // x=1, y=2, z=1.5
    let p = default_params(&dae);

    let jac_ad = jacobian_ad(&dae, &yv, &p, 0.0, n_x);
    let jac_fd = jacobian_fd(&dae, &yv, &p, 0.0, n_x);

    // Analytical at x=1, y=2, z=1.5:
    let (xv, yval, zv) = (1.0_f64, 2.0_f64, 1.5_f64);
    let expected = [
        [0.0, -zv, -yval],       // -(y*z) → -[0, z, y]
        [1.0, 0.0, -(2.0 * zv)], // -(-x + z^2) → -[-1, 0, 2z] = [1, 0, -2z]
        [1.0, 1.0, -2.0],        // x + y - 2*z → [1, 1, -2]
    ];
    let _ = xv; // used conceptually above

    for row in 0..3 {
        for col in 0..3 {
            assert!(
                (jac_ad[row][col] - expected[row][col]).abs() < 1e-10,
                "J[{row}][{col}] should be {:.8e}, got {:.8e}",
                expected[row][col],
                jac_ad[row][col]
            );
        }
    }

    assert_jacobians_close(&jac_ad, &jac_fd, 1e-5, "three_vars");
}

/// Test 5: If-else expression — discontinuous branch.
///
/// State: x, Algebraic: z
/// der(x) = if x > 0 then -x else x  (equivalent to -abs(x))
/// 0 = z - x^2
///
/// At x=2 (positive branch): f[0] = -(-x) = x, df/dx = 1
/// At x=-3 (negative branch): f[0] = -(x) = -x, df/dx = -1
#[test]
fn test_jacobian_ad_vs_fd_conditional() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // eq0: 0 = if x > 0 then -x else x
    let neg_x = Expression::Unary {
        op: rumoca_ir_core::OpUnary::Minus(Default::default()),
        rhs: Box::new(var("x")),
    };
    let cond = Expression::Binary {
        op: OpBinary::Gt(Default::default()),
        lhs: Box::new(var("x")),
        rhs: Box::new(lit(0.0)),
    };
    let rhs0 = Expression::If {
        branches: vec![(cond, neg_x)],
        else_branch: Box::new(var("x")),
    };
    dae.f_x.push(eq_from(rhs0));

    // eq1: 0 = z - x^2
    let x2 = binop(OpBinary::Exp(Default::default()), var("x"), lit(2.0));
    let rhs1 = binop(OpBinary::Sub(Default::default()), var("z"), x2);
    dae.f_x.push(eq_from(rhs1));

    let n_x = count_states(&dae);
    let p = default_params(&dae);

    // Test positive branch (x=2)
    let y_pos = vec![2.0, 4.0];
    let jac_ad = jacobian_ad(&dae, &y_pos, &p, 0.0, n_x);
    let jac_fd = jacobian_fd(&dae, &y_pos, &p, 0.0, n_x);
    // f[0] = -(-x) = x → df/dx = 1, df/dz = 0 (negated for ODE)
    assert!(
        (jac_ad[0][0] - 1.0).abs() < 1e-10,
        "positive: J[0][0]={}",
        jac_ad[0][0]
    );
    assert_jacobians_close(&jac_ad, &jac_fd, 1e-5, "conditional_positive");

    // Test negative branch (x=-3)
    let y_neg = vec![-3.0, 9.0];
    let jac_ad = jacobian_ad(&dae, &y_neg, &p, 0.0, n_x);
    let jac_fd = jacobian_fd(&dae, &y_neg, &p, 0.0, n_x);
    // f[0] = -(x) = -x → df/dx = -1 (negated for ODE)
    assert!(
        (jac_ad[0][0] - (-1.0)).abs() < 1e-10,
        "negative: J[0][0]={}",
        jac_ad[0][0]
    );
    assert_jacobians_close(&jac_ad, &jac_fd, 1e-5, "conditional_negative");
}

/// Test 6: Division chain — tests quotient rule propagation.
///
/// State: x, Algebraic: z
/// der(x) = x / (1 + x^2)
/// 0 = z - 1/(1+x)
///
/// d(x/(1+x^2))/dx = (1+x^2 - x*2x) / (1+x^2)^2 = (1-x^2)/(1+x^2)^2
#[test]
fn test_jacobian_ad_vs_fd_division() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // eq0: 0 = x / (1 + x^2)
    let x2 = binop(OpBinary::Exp(Default::default()), var("x"), lit(2.0));
    let denom = binop(OpBinary::Add(Default::default()), lit(1.0), x2);
    let rhs0 = binop(OpBinary::Div(Default::default()), var("x"), denom);
    dae.f_x.push(eq_from(rhs0));

    // eq1: 0 = z - 1/(1+x)
    let one_plus_x = binop(OpBinary::Add(Default::default()), lit(1.0), var("x"));
    let inv = binop(OpBinary::Div(Default::default()), lit(1.0), one_plus_x);
    let rhs1 = binop(OpBinary::Sub(Default::default()), var("z"), inv);
    dae.f_x.push(eq_from(rhs1));

    let n_x = count_states(&dae);
    let p = default_params(&dae);

    for &xval in &[0.5, 1.0, 2.0, 3.0] {
        let zval = 1.0 / (1.0 + xval);
        let y = vec![xval, zval];
        let jac_ad = jacobian_ad(&dae, &y, &p, 0.0, n_x);
        let jac_fd = jacobian_fd(&dae, &y, &p, 0.0, n_x);

        // Analytical: df0/dx = -((1-x^2)/(1+x^2)^2) [negated for ODE row]
        let d0 = 1.0 + xval * xval;
        let expected_00 = -((1.0 - xval * xval) / (d0 * d0));
        assert!(
            (jac_ad[0][0] - expected_00).abs() < 1e-8,
            "x={xval}: J[0][0]={}, expected={expected_00}",
            jac_ad[0][0]
        );

        assert_jacobians_close(&jac_ad, &jac_fd, 1e-5, &format!("division_x={xval}"));
    }
}

/// Test 7: Nested builtin calls — sqrt(sin(x)^2 + cos(z)^2).
///
/// Tests chain rule through nested function compositions.
#[test]
fn test_jacobian_ad_vs_fd_nested_builtins() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // eq0: 0 = sqrt(sin(x)^2 + cos(z)^2) - 1
    let sin_x = Expression::BuiltinCall {
        function: BuiltinFunction::Sin,
        args: vec![var("x")],
    };
    let cos_z = Expression::BuiltinCall {
        function: BuiltinFunction::Cos,
        args: vec![var("z")],
    };
    let sin2 = binop(OpBinary::Exp(Default::default()), sin_x, lit(2.0));
    let cos2 = binop(OpBinary::Exp(Default::default()), cos_z, lit(2.0));
    let sum = binop(OpBinary::Add(Default::default()), sin2, cos2);
    let sqr = Expression::BuiltinCall {
        function: BuiltinFunction::Sqrt,
        args: vec![sum],
    };
    let rhs0 = binop(OpBinary::Sub(Default::default()), sqr, lit(1.0));
    dae.f_x.push(eq_from(rhs0));

    // eq1: 0 = tanh(x*z) - 0.5
    let xz = binop(OpBinary::Mul(Default::default()), var("x"), var("z"));
    let tanh_xz = Expression::BuiltinCall {
        function: BuiltinFunction::Tanh,
        args: vec![xz],
    };
    let rhs1 = binop(OpBinary::Sub(Default::default()), tanh_xz, lit(0.5));
    dae.f_x.push(eq_from(rhs1));

    let n_x = count_states(&dae);
    let p = default_params(&dae);

    // Test at several points
    for &(xv, zv) in &[(0.5, 0.3), (1.0, 0.7), (0.1, 2.0)] {
        let y = vec![xv, zv];
        let jac_ad = jacobian_ad(&dae, &y, &p, 0.0, n_x);
        let jac_fd = jacobian_fd(&dae, &y, &p, 0.0, n_x);
        assert_jacobians_close(
            &jac_ad,
            &jac_fd,
            1e-5,
            &format!("nested_builtins_x={xv}_z={zv}"),
        );
    }
}
