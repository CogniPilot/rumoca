use super::*;

#[test]
fn test_eval_pre_uses_seeded_previous_value_for_varref() {
    clear_pre_values();
    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("x", 1.5);
    seed_pre_values_from_env(&seed_env);

    let mut eval_env = VarEnv::<f64>::new();
    eval_env.set("x", 9.0);
    let pre_x = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Pre,
        args: vec![var("x")],
    };
    let value = eval_expr::<f64>(&pre_x, &eval_env);
    assert!((value - 1.5).abs() < 1e-12);

    clear_pre_values();
}

#[test]
fn test_eval_pre_uses_seeded_previous_value_for_indexed_varref_and_dual() {
    clear_pre_values();
    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("x[1]", 2.25);
    seed_env.set("x", 2.25);
    seed_pre_values_from_env(&seed_env);

    let indexed = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Pre,
        args: vec![dae::Expression::VarRef {
            name: dae::VarName::new("x"),
            subscripts: vec![dae::Subscript::Index(1)],
        }],
    };

    let mut eval_env = VarEnv::<Dual>::new();
    eval_env.set("x[1]", Dual::new(8.0, 3.0));
    eval_env.set("x", Dual::new(8.0, 3.0));
    let value = eval_expr::<Dual>(&indexed, &eval_env);
    assert!((value.re - 2.25).abs() < 1e-12);
    assert!(value.du.abs() < 1e-12);

    clear_pre_values();
}

#[test]
fn test_eval_builtin_edge_and_change_use_pre_seeded_values() {
    clear_pre_values();

    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("b", 0.0);
    seed_pre_values_from_env(&seed_env);

    let mut env = VarEnv::<f64>::new();
    env.set("b", 1.0);

    let edge_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Edge,
        args: vec![var("b")],
    };
    let change_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Change,
        args: vec![var("b")],
    };
    assert_eq!(eval_expr::<f64>(&edge_expr, &env), 1.0);
    assert_eq!(eval_expr::<f64>(&change_expr, &env), 1.0);

    clear_pre_values();
    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("b", 1.0);
    seed_pre_values_from_env(&seed_env);

    let mut env = VarEnv::<f64>::new();
    env.set("b", 1.0);
    assert_eq!(eval_expr::<f64>(&edge_expr, &env), 0.0);
    assert_eq!(eval_expr::<f64>(&change_expr, &env), 0.0);

    clear_pre_values();
}
