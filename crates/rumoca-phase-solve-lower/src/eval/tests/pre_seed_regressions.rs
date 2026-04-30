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

#[test]
fn test_eval_builtin_edge_on_relational_expr_uses_pre_seeded_values() {
    clear_pre_values();

    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("trig", 2.0);
    seed_pre_values_from_env(&seed_env);

    let mut env = VarEnv::<f64>::new();
    env.set("trig", 4.0);
    env.enum_literal_ordinals = std::sync::Arc::new(indexmap::IndexMap::from([(
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
        4,
    )]));

    let relation = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Eq(Default::default()),
        lhs: Box::new(var("trig")),
        rhs: Box::new(var("Modelica.Electrical.Digital.Interfaces.Logic.'1'")),
    };
    let edge_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Edge,
        args: vec![relation],
    };

    assert_eq!(eval_expr::<f64>(&edge_expr, &env), 1.0);

    clear_pre_values();
}

#[test]
fn test_eval_builtin_edge_on_sample_conjunction_uses_left_limit_sample_false() {
    clear_pre_values();

    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("sampling", 1.0);
    seed_pre_values_from_env(&seed_env);

    let mut env = VarEnv::<f64>::new();
    env.set("sampling", 1.0);
    env.set("time", 1.0);

    let condition = dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::And(Default::default()),
        lhs: Box::new(var("sampling")),
        rhs: Box::new(dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![
                dae::Expression::Literal(dae::Literal::Real(0.0)),
                dae::Expression::Literal(dae::Literal::Real(1.0)),
            ],
        }),
    };
    let edge_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Edge,
        args: vec![condition],
    };

    // MLS §16.5.1 / Appendix B: sample(start, interval) is false on the
    // event left-limit, so edge(sampling and sample(...)) must fire at the tick.
    assert_eq!(eval_expr::<f64>(&edge_expr, &env), 1.0);

    clear_pre_values();
}

#[test]
fn test_eval_builtin_edge_on_initial_is_true_during_initial_event() {
    clear_pre_values();

    let mut env = VarEnv::<f64>::new();
    env.is_initial = true;

    let edge_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Edge,
        args: vec![dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Initial,
            args: vec![],
        }],
    };

    // MLS §8.6: initial() is false on the event left-limit, so edge(initial())
    // must fire once at startup for lowered when {initial(), ...} guards.
    assert_eq!(eval_expr::<f64>(&edge_expr, &env), 1.0);

    clear_pre_values();
}

#[test]
fn test_eval_builtin_edge_on_time_ge_next_event_uses_left_limit_time() {
    clear_pre_values();

    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("nextEvent", 1.0);
    seed_pre_values_from_env(&seed_env);

    let mut env = VarEnv::<f64>::new();
    env.set("time", 1.0);
    env.set("nextEvent", 1.0);

    let edge_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Edge,
        args: vec![dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Ge(Default::default()),
            lhs: Box::new(var("time")),
            rhs: Box::new(var("nextEvent")),
        }],
    };

    // MLS Appendix B / SPEC_0022 SIM-001: time-event guards such as
    // edge(time >= t_next) must see a false left-limit and fire at t=t_next.
    assert_eq!(eval_expr::<f64>(&edge_expr, &env), 1.0);

    clear_pre_values();
}

#[test]
fn test_eval_builtin_change_on_logic_ordinal_detects_non_boolean_transition() {
    clear_pre_values();

    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("logic", 1.0);
    seed_pre_values_from_env(&seed_env);

    let mut env = VarEnv::<f64>::new();
    env.set("logic", 3.0);

    let change_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Change,
        args: vec![var("logic")],
    };

    // MLS §3.7.2: change(v) detects discrete value changes, not just boolean
    // truth-value flips. Digital logic ordinals such as 1 -> 3 must fire.
    assert_eq!(eval_expr::<f64>(&change_expr, &env), 1.0);

    clear_pre_values();
}

#[test]
fn test_eval_lowered_pre_parameter_reads_pre_store_before_stale_env_binding() {
    clear_pre_values();
    set_pre_value("reset", 3.0);

    let mut env = VarEnv::<f64>::new();
    env.set("__pre__.reset", 0.0);

    assert_eq!(eval_expr::<f64>(&var("__pre__.reset"), &env), 3.0);

    clear_pre_values();
}

#[test]
fn test_seed_pre_values_from_env_updates_existing_layout_in_place() {
    clear_pre_values();

    let mut first = VarEnv::<f64>::new();
    first.set("x", 1.0);
    first.set("y", 2.0);
    seed_pre_values_from_env(&first);

    let mut second = VarEnv::<f64>::new();
    second.set("x", 3.0);
    second.set("y", 4.0);
    seed_pre_values_from_env(&second);

    let snapshot = snapshot_pre_values();
    assert_eq!(snapshot.get("x").copied(), Some(3.0));
    assert_eq!(snapshot.get("y").copied(), Some(4.0));

    clear_pre_values();
}
