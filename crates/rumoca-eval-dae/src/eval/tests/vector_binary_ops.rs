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
fn test_eval_binary_div() {
    let expr = binop(OpBinary::Div, lit(10.0), lit(4.0));
    assert_eq!(eval_expr_value::<f64>(&expr, &VarEnv::new()), 2.5);
}

#[test]
fn test_eval_binary_exp() {
    let expr = binop(OpBinary::Exp, lit(2.0), lit(3.0));
    assert_eq!(eval_expr_value::<f64>(&expr, &VarEnv::new()), 8.0);
}
