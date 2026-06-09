use super::*;

#[test]
fn test_eval_pre_uses_seeded_previous_value_for_varref() {
    clear_pre_values();
    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("x", 1.5);
    seed_pre_values_from_env(&seed_env);

    let mut eval_env = VarEnv::<f64>::new();
    eval_env.runtime = seed_env.runtime.clone();
    eval_env.set("x", 9.0);
    let pre_x = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Pre,
        args: vec![var("x")],
        span: rumoca_core::Span::DUMMY,
    };
    let value = eval_expr_value::<f64>(&pre_x, &eval_env);
    assert!((value - 1.5).abs() < 1e-12);

    clear_pre_values();
}

#[test]
fn test_eval_pre_uses_seeded_previous_value_for_indexed_varref_and_dual() {
    clear_pre_values();
    let mut seed_env = VarEnv::<f64>::new();
    set_array_entries(&mut seed_env, "x", &[1], &[2.25]);
    seed_env.dims = std::sync::Arc::new(IndexMap::from([("x".to_string(), vec![1])]));
    seed_pre_values_from_env(&seed_env);

    let indexed = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Pre,
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::Reference::new("x"),
            subscripts: vec![rumoca_core::Subscript::generated_index(
                1,
                rumoca_core::Span::DUMMY,
            )],
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    };

    let mut eval_env = VarEnv::<Dual>::new();
    eval_env.runtime = seed_env.runtime.clone();
    set_array_entries(&mut eval_env, "x", &[1], &[Dual::new(8.0, 3.0)]);
    eval_env.dims = std::sync::Arc::new(IndexMap::from([("x".to_string(), vec![1])]));
    let value = eval_expr_value::<Dual>(&indexed, &eval_env);
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
    env.runtime = seed_env.runtime.clone();
    env.set("b", 1.0);

    let edge_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Edge,
        args: vec![var("b")],
        span: rumoca_core::Span::DUMMY,
    };
    let change_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Change,
        args: vec![var("b")],
        span: rumoca_core::Span::DUMMY,
    };
    assert_eq!(eval_expr_value::<f64>(&edge_expr, &env), 1.0);
    assert_eq!(eval_expr_value::<f64>(&change_expr, &env), 1.0);

    clear_pre_values();
    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("b", 1.0);
    seed_pre_values_from_env(&seed_env);

    let mut env = VarEnv::<f64>::new();
    env.runtime = seed_env.runtime.clone();
    env.set("b", 1.0);
    assert_eq!(eval_expr_value::<f64>(&edge_expr, &env), 0.0);
    assert_eq!(eval_expr_value::<f64>(&change_expr, &env), 0.0);

    clear_pre_values();
}

#[test]
fn test_eval_builtin_edge_on_relational_expr_uses_pre_seeded_values() {
    clear_pre_values();

    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("trig", 2.0);
    seed_pre_values_from_env(&seed_env);

    let mut env = VarEnv::<f64>::new();
    env.runtime = seed_env.runtime.clone();
    env.set("trig", 4.0);
    env.enum_literal_ordinals = std::sync::Arc::new(indexmap::IndexMap::from([(
        "Modelica.Electrical.Digital.Interfaces.Logic.'1'".to_string(),
        4,
    )]));

    let relation = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Eq,
        lhs: Box::new(var("trig")),
        rhs: Box::new(var("Modelica.Electrical.Digital.Interfaces.Logic.'1'")),
        span: rumoca_core::Span::DUMMY,
    };
    let edge_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Edge,
        args: vec![relation],
        span: rumoca_core::Span::DUMMY,
    };

    assert_eq!(eval_expr_value::<f64>(&edge_expr, &env), 1.0);

    clear_pre_values();
}

#[test]
fn test_eval_builtin_edge_on_sample_conjunction_uses_left_limit_sample_false() {
    clear_pre_values();

    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("sampling", 1.0);
    seed_pre_values_from_env(&seed_env);

    let mut env = VarEnv::<f64>::new();
    env.runtime = seed_env.runtime.clone();
    env.set("sampling", 1.0);
    env.set("time", 1.0);

    let condition = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::And,
        lhs: Box::new(var("sampling")),
        rhs: Box::new(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sample,
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.0),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(1.0),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            span: rumoca_core::Span::DUMMY,
        }),
        span: rumoca_core::Span::DUMMY,
    };
    let edge_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Edge,
        args: vec![condition],
        span: rumoca_core::Span::DUMMY,
    };

    // MLS §16.5.1 / Appendix B: sample(start, interval) is false on the
    // event left-limit, so edge(sampling and sample(...)) must fire at the tick.
    assert_eq!(eval_expr_value::<f64>(&edge_expr, &env), 1.0);

    clear_pre_values();
}

#[test]
fn test_eval_builtin_edge_on_initial_is_true_during_initial_event() {
    clear_pre_values();

    let mut env = VarEnv::<f64>::new();
    env.is_initial = true;

    let edge_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Edge,
        args: vec![rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Initial,
            args: vec![],
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    };

    // MLS §8.6: initial() is false on the event left-limit, so edge(initial())
    // must fire once at startup for lowered when {initial(), ...} guards.
    assert_eq!(eval_expr_value::<f64>(&edge_expr, &env), 1.0);

    clear_pre_values();
}

#[test]
fn test_eval_builtin_edge_on_time_ge_next_event_uses_left_limit_time() {
    clear_pre_values();

    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("nextEvent", 1.0);
    seed_pre_values_from_env(&seed_env);

    let mut env = VarEnv::<f64>::new();
    env.runtime = seed_env.runtime.clone();
    env.set("time", 1.0);
    env.set("nextEvent", 1.0);

    let edge_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Edge,
        args: vec![rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Ge,
            lhs: Box::new(var("time")),
            rhs: Box::new(var("nextEvent")),
            span: rumoca_core::Span::DUMMY,
        }],
        span: rumoca_core::Span::DUMMY,
    };

    // MLS Appendix B / SPEC_0022 SIM-001: time-event guards such as
    // edge(time >= t_next) must see a false left-limit and fire at t=t_next.
    assert_eq!(eval_expr_value::<f64>(&edge_expr, &env), 1.0);

    clear_pre_values();
}

#[test]
fn test_eval_builtin_change_on_logic_ordinal_detects_non_boolean_transition() {
    clear_pre_values();

    let mut seed_env = VarEnv::<f64>::new();
    seed_env.set("logic", 1.0);
    seed_pre_values_from_env(&seed_env);

    let mut env = VarEnv::<f64>::new();
    env.runtime = seed_env.runtime.clone();
    env.set("logic", 3.0);

    let change_expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Change,
        args: vec![var("logic")],
        span: rumoca_core::Span::DUMMY,
    };

    // MLS §3.7.2: change(v) detects discrete value changes, not just boolean
    // truth-value flips. Digital logic ordinals such as 1 -> 3 must fire.
    assert_eq!(eval_expr_value::<f64>(&change_expr, &env), 1.0);

    clear_pre_values();
}

#[test]
fn test_eval_lowered_pre_parameter_reads_pre_store_before_stale_env_binding() {
    clear_pre_values();
    let mut env = VarEnv::<f64>::new();
    set_pre_value_in_env(&env, "reset", 3.0);
    env.set("__pre__.reset", 0.0);

    assert_eq!(eval_expr_value::<f64>(&var("__pre__.reset"), &env), 3.0);

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
    second.runtime = first.runtime.clone();
    second.set("x", 3.0);
    second.set("y", 4.0);
    seed_pre_values_from_env(&second);

    let snapshot = snapshot_pre_values_from_env(&second);
    assert_eq!(snapshot.get("x").copied(), Some(3.0));
    assert_eq!(snapshot.get("y").copied(), Some(4.0));

    clear_pre_values();
}
