use super::*;

#[test]
fn test_full_path_name_runtime_special_is_numeric_placeholder() {
    let env = VarEnv::<f64>::new();
    let value = eval_expr::<f64>(
        &fn_call(
            "Modelica.Utilities.Files.fullPathName",
            vec![dae::Expression::Literal(dae::Literal::String(
                "a.txt".to_string(),
            ))],
        ),
        &env,
    );
    assert_eq!(value, 0.0);
}
