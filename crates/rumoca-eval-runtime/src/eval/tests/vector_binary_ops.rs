use super::*;

#[test]
fn test_eval_binary_sub() {
    let expr = binop(OpBinary::Sub(Default::default()), lit(5.0), lit(3.0));
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 2.0);
}

#[test]
fn test_eval_binary_mul() {
    let expr = binop(OpBinary::Mul(Default::default()), lit(4.0), lit(3.0));
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 12.0);
}

#[test]
fn test_eval_binary_mul_vector_dot_product() {
    let expr = binop(
        OpBinary::Mul(Default::default()),
        arr(vec![lit(1.0), lit(2.0), lit(3.0)], false),
        arr(vec![lit(4.0), lit(5.0), lit(6.0)], false),
    );
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 32.0);
}

#[test]
fn test_eval_binary_div() {
    let expr = binop(OpBinary::Div(Default::default()), lit(10.0), lit(4.0));
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 2.5);
}

#[test]
fn test_eval_binary_exp() {
    let expr = binop(OpBinary::Exp(Default::default()), lit(2.0), lit(3.0));
    assert_eq!(eval_expr::<f64>(&expr, &VarEnv::new()), 8.0);
}
