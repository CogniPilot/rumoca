use super::*;

#[test]
fn test_eval_binary_sub() {
    let expr = binop(OpBinary::Sub, lit(5.0), lit(3.0));
    assert_eq!(eval_expr_value::<f64>(&expr, &VarEnv::new()), 2.0);
}

#[test]
fn test_eval_binary_mul() {
    let expr = binop(OpBinary::Mul, lit(4.0), lit(3.0));
    assert_eq!(eval_expr_value::<f64>(&expr, &VarEnv::new()), 12.0);
}

#[test]
fn test_eval_binary_mul_vector_dot_product() {
    let expr = binop(
        OpBinary::Mul,
        arr(vec![lit(1.0), lit(2.0), lit(3.0)], false),
        arr(vec![lit(4.0), lit(5.0), lit(6.0)], false),
    );
    assert_eq!(eval_expr_value::<f64>(&expr, &VarEnv::new()), 32.0);
}

#[test]
fn test_checked_eval_binary_mul_vector_dot_product() {
    let expr = binop(
        OpBinary::Mul,
        arr(vec![lit(-1.0), lit(0.3), lit(0.1)], false),
        arr(vec![lit(-1.0), lit(0.3), lit(0.1)], false),
    );
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()).unwrap(), 1.1);
}

#[test]
fn vector_dot_product_remains_scalar_inside_array_literal() {
    let mut env = VarEnv::new();
    set_array_entries(&mut env, "normal", &[2], &[-0.6, 0.8]);
    set_array_entries(&mut env, "left", &[2], &[1.0, 2.0]);
    set_array_entries(&mut env, "right", &[2], &[3.0, 4.0]);
    let dims = std::sync::Arc::make_mut(&mut env.dims);
    dims.insert("normal".to_string(), vec![2]);
    dims.insert("left".to_string(), vec![2]);
    dims.insert("right".to_string(), vec![2]);

    let average = binop(
        OpBinary::Mul,
        lit(0.5),
        binop(OpBinary::Add, var("left"), var("right")),
    );
    let dot = binop(OpBinary::Mul, var("normal"), average);
    let vector = arr(vec![dot.clone(), dot.clone(), dot], false);

    let values = eval_array_values::<f64>(&vector, &env).unwrap();
    assert_eq!(values.len(), 3);
    assert!(values.iter().all(|value| (value - 1.2).abs() < 1.0e-14));
}

#[test]
fn test_eval_binary_div() {
    let expr = binop(OpBinary::Div, lit(10.0), lit(4.0));
    assert_eq!(eval_expr_value::<f64>(&expr, &VarEnv::new()), 2.5);
}

#[test]
fn test_eval_binary_exp() {
    let expr = binop(OpBinary::Exp, lit(2.0), lit(3.0));
    assert_eq!(eval_expr_value::<f64>(&expr, &VarEnv::new()), 8.0);
}

#[test]
fn elementwise_vector_power_broadcasts_scalar_exponent() {
    let mut env = VarEnv::new();
    set_array_entries(&mut env, "x", &[3], &[2.0, 3.0, 4.0]);
    std::sync::Arc::make_mut(&mut env.dims).insert("x".to_string(), vec![3]);
    let expr = binop(OpBinary::ExpElem, var("x"), lit(2.0));

    assert_eq!(
        eval_array_values::<f64>(&expr, &env).unwrap(),
        vec![4.0, 9.0, 16.0]
    );
}

#[test]
fn row_slice_quadratic_form_evaluates_to_scalar() {
    // coeff[2, :] * (P * coeff[2, :]) with P = identity(4) is the squared
    // norm of row 2 — a scalar, evaluated through the checked scalar path.
    let mut env = VarEnv::new();
    set_array_entries(
        &mut env,
        "coeff",
        &[3, 4],
        &[
            1.0, 2.0, 3.0, 4.0, //
            5.0, 6.0, 7.0, 8.0, //
            9.0, 10.0, 11.0, 12.0,
        ],
    );
    #[rustfmt::skip]
    set_array_entries(
        &mut env,
        "P",
        &[4, 4],
        &[
            1.0, 0.0, 0.0, 0.0,
            0.0, 1.0, 0.0, 0.0,
            0.0, 0.0, 1.0, 0.0,
            0.0, 0.0, 0.0, 1.0,
        ],
    );
    let dims = std::sync::Arc::make_mut(&mut env.dims);
    dims.insert("coeff".to_string(), vec![3, 4]);
    dims.insert("P".to_string(), vec![4, 4]);

    let row_slice = || rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new("coeff"),
        subscripts: vec![
            Subscript::generated_index(2, rumoca_core::Span::DUMMY),
            Subscript::Colon {
                span: rumoca_core::Span::DUMMY,
            },
        ],
        span: rumoca_core::Span::DUMMY,
    };
    let quadratic = binop(
        OpBinary::Mul,
        row_slice(),
        binop(OpBinary::Mul, var("P"), row_slice()),
    );

    let value = eval_expr::<f64>(&quadratic, &env).unwrap();
    assert!((value - 174.0).abs() < 1.0e-12);
}

#[test]
fn matrix_binary_difference_keeps_matrix_shape_and_validates_as_array_argument() {
    let mut env = VarEnv::new();
    set_array_entries(&mut env, "contraction", &[2, 2], &[1.0, 2.0, 3.0, 4.0]);
    std::sync::Arc::make_mut(&mut env.dims).insert("contraction".to_string(), vec![2, 2]);

    let matrix_literal = arr(
        vec![
            arr(vec![lit(-2.0), lit(0.0)], false),
            arr(vec![lit(0.0), lit(-4.0)], false),
        ],
        true,
    );
    let difference = binop(OpBinary::Sub, var("contraction"), matrix_literal);

    assert!(validate_array_argument(&difference, &env).is_ok());
    assert_eq!(
        try_infer_runtime_expr_dims(&difference, &env).unwrap(),
        vec![2, 2]
    );
    assert_eq!(
        eval_array_values::<f64>(&difference, &env).unwrap(),
        vec![3.0, 2.0, 3.0, 8.0]
    );
}

#[test]
fn identity_builtin_infers_square_runtime_dims() {
    let identity_call = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Identity,
        args: vec![int_lit(2)],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(
        try_infer_runtime_expr_dims::<f64>(&identity_call, &VarEnv::new()).unwrap(),
        vec![2, 2]
    );
}
