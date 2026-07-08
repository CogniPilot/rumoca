use super::*;

#[test]
fn test_eval_size_of_single_value_range_with_fill_bound() {
    let env = VarEnv::<f64>::new();
    let fill = builtin(
        rumoca_core::BuiltinFunction::Fill,
        vec![lit(0.0), int_lit(0), int_lit(2)],
    );
    let range = rumoca_core::Expression::Range {
        start: Box::new(int_lit(2)),
        step: None,
        end: Box::new(builtin(
            rumoca_core::BuiltinFunction::Size,
            vec![fill, int_lit(2)],
        )),
        span: rumoca_core::Span::DUMMY,
    };
    let size = builtin(rumoca_core::BuiltinFunction::Size, vec![range, int_lit(1)]);

    assert_eq!(eval_expr::<f64>(&size, &env), Ok(1.0));
}

#[test]
fn test_build_env_defaults_empty_external_table_bound_start() {
    let mut dae = rumoca_ir_dae::Dae::default();
    let mut x = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("table_u_min"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    let no_name = rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::String("NoName".to_string()),
        span: rumoca_core::Span::DUMMY,
    };
    let constructor = fn_call(
        "ExternalCombiTimeTable",
        vec![
            no_name.clone(),
            no_name,
            rumoca_core::Expression::Empty {
                span: rumoca_core::Span::DUMMY,
            },
            lit(0.0),
            arr(vec![int_lit(2)], false),
            int_lit(3),
            int_lit(1),
        ],
    );
    x.start = Some(fn_call("getTimeTableTmin", vec![constructor]));
    dae.variables.parameters.insert("table_u_min".into(), x);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0).expect("test env should build");

    assert_eq!(env.vars.get("table_u_min").copied(), Some(0.0));
}

#[test]
fn test_build_env_seeds_sum_size_start_for_string_array_literal() {
    let substance_names = arr(
        vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("N2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("O2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("H2O".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("CO2".to_string()),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        false,
    );
    let size = builtin(rumoca_core::BuiltinFunction::Size, vec![substance_names]);
    let mut dae = rumoca_ir_dae::Dae::default();
    let mut n = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("n"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    n.start = Some(builtin(rumoca_core::BuiltinFunction::Sum, vec![size]));
    dae.variables.constants.insert("n".into(), n);

    let env = build_runtime_parameter_tail_env(&dae, &[], 0.0).expect("test env should build");

    assert_eq!(env_value(&env, "n"), 4.0);
}

#[test]
fn test_build_env_discrete_start_forward_ref_re_evaluates_and_preserves_pre_seed() {
    clear_pre_values();

    let mut dae = rumoca_ir_dae::Dae::default();

    // Insert dependent start first to exercise forward-reference handling.
    let mut a = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("a"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    a.start = Some(dae_var("b"));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("a"), a);

    let mut b = rumoca_ir_dae::Variable::new(
        rumoca_core::VarName::new("b"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    b.start = Some(dae_bool_lit(true));
    dae.variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("b"), b);

    let env = build_env(&dae, &[], &[], 0.0).expect("test env should build");
    assert_eq!(env_value(&env, "b"), 1.0);
    assert_eq!(env_value(&env, "a"), 1.0);

    // Pre-seeded values must take precedence over start expressions.
    let mut pre_env = VarEnv::<f64>::new();
    pre_env.set("a", 0.0);
    pre_env.set("b", 0.0);
    seed_pre_values_from_env(&pre_env);

    let env_from_pre = build_env_with_runtime(&dae, &[], &[], 1.0, pre_env.runtime.clone())
        .expect("test env should build");
    assert_eq!(env_value(&env_from_pre, "a"), 0.0);
    assert_eq!(env_value(&env_from_pre, "b"), 0.0);

    clear_pre_values();
}
