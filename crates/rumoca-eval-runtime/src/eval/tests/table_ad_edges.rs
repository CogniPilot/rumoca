use super::*;

#[test]
fn test_table1d_periodic_extrapolation_preserves_ad_slope() {
    let y = eval_table1d_dual(Dual::new(3.0, 1.0), 3); // wraps to 1.0 with span=2.0
    assert!((y.re - 12.0).abs() < 1e-12);
    assert!((y.du - 2.0).abs() < 1e-12);
}

#[test]
fn test_table1d_linear_ad_at_table_ends() {
    let y_min = eval_table1d_dual(Dual::new(0.0, 1.0), 1);
    assert!((y_min.re - 10.0).abs() < 1e-12);
    assert!((y_min.du - 2.0).abs() < 1e-12);

    let y_max = eval_table1d_dual(Dual::new(2.0, 1.0), 1);
    assert!((y_max.re - 14.0).abs() < 1e-12);
    assert!((y_max.du - 2.0).abs() < 1e-12);
}

#[test]
fn test_table1d_last_two_points_extrapolation_at_ends() {
    let y_hi = eval_table1d_dual(Dual::new(2.5, 1.0), 2);
    assert!((y_hi.re - 15.0).abs() < 1e-12);
    assert!((y_hi.du - 2.0).abs() < 1e-12);

    let y_lo = eval_table1d_dual(Dual::new(-0.5, 1.0), 2);
    assert!((y_lo.re - 9.0).abs() < 1e-12);
    assert!((y_lo.du - 2.0).abs() < 1e-12);
}

#[test]
fn test_timetable_linear_ad_at_zero_and_end() {
    let y0 = eval_timetable_dual(Dual::new(0.0, 1.0), 1);
    assert!((y0.re - 10.0).abs() < 1e-12);
    assert!((y0.du - 2.0).abs() < 1e-12);

    let y_end = eval_timetable_dual(Dual::new(2.0, 1.0), 1);
    assert!((y_end.re - 14.0).abs() < 1e-12);
    assert!((y_end.du - 2.0).abs() < 1e-12);
}

#[test]
fn test_builtin_ad_zero_edge_guards() {
    let mut env = VarEnv::<Dual>::new();
    env.set("x", Dual::new(0.0, 1.0));

    let log_x = eval_expr::<Dual>(
        &flat::Expression::BuiltinCall {
            function: flat::BuiltinFunction::Log,
            args: vec![var("x")],
        },
        &env,
    );
    assert!(log_x.re.is_infinite() && log_x.re.is_sign_negative());
    assert!(log_x.du.is_finite());
    assert!(log_x.du.abs() < 1e-12);

    let sqrt_x = eval_expr::<Dual>(
        &flat::Expression::BuiltinCall {
            function: flat::BuiltinFunction::Sqrt,
            args: vec![var("x")],
        },
        &env,
    );
    assert!(sqrt_x.re.abs() < 1e-12);
    assert!(sqrt_x.du.abs() < 1e-12);
}

#[test]
fn test_pow_and_division_ad_zero_edges() {
    let mut env = VarEnv::<Dual>::new();
    env.set("x", Dual::new(0.0, 1.0));

    let x_pow_1 = eval_expr::<Dual>(
        &binop(flat::OpBinary::Exp(Default::default()), var("x"), lit(1.0)),
        &env,
    );
    assert!(x_pow_1.re.abs() < 1e-12);
    assert!((x_pow_1.du - 1.0).abs() < 1e-12);

    let x_pow_2 = eval_expr::<Dual>(
        &binop(flat::OpBinary::Exp(Default::default()), var("x"), lit(2.0)),
        &env,
    );
    assert!(x_pow_2.re.abs() < 1e-12);
    assert!(x_pow_2.du.abs() < 1e-12);

    let zero_over_zero = eval_expr::<Dual>(
        &binop(flat::OpBinary::Div(Default::default()), var("x"), var("x")),
        &env,
    );
    assert!(zero_over_zero.re.abs() < 1e-12);
    assert!(zero_over_zero.du.abs() < 1e-12);
}
