use super::*;

#[test]
fn test_eval_expr_homotopy_blends_during_initial_phase() {
    let mut env = VarEnv::<f64>::new();
    env.is_initial = true;
    env.set(INIT_HOMOTOPY_LAMBDA_KEY, 0.25);

    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Homotopy,
        args: vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(10.0),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        span: rumoca_core::Span::DUMMY,
    };

    assert!((eval_expr_value::<f64>(&expr, &env) - 4.0).abs() < 1e-12);
}

#[test]
fn test_eval_expr_homotopy_returns_actual_after_initial_phase() {
    let mut env = VarEnv::<f64>::new();
    env.is_initial = false;
    env.set(INIT_HOMOTOPY_LAMBDA_KEY, 0.0);

    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Homotopy,
        args: vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(10.0),
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: rumoca_core::Span::DUMMY,
            },
        ],
        span: rumoca_core::Span::DUMMY,
    };

    assert!((eval_expr_value::<f64>(&expr, &env) - 10.0).abs() < 1e-12);
}

#[test]
fn test_string_is_empty_runtime_special() {
    let env = VarEnv::<f64>::new();
    let empty = eval_expr_value::<f64>(
        &fn_call(
            "Modelica.Utilities.Strings.isEmpty",
            vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("   ".to_string()),
                span: rumoca_core::Span::DUMMY,
            }],
        ),
        &env,
    );
    assert_eq!(empty, 1.0);

    let non_empty = eval_expr_value::<f64>(
        &fn_call(
            "Modelica.Utilities.Strings.isEmpty",
            vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String("abc".to_string()),
                span: rumoca_core::Span::DUMMY,
            }],
        ),
        &env,
    );
    assert_eq!(non_empty, 0.0);
}

#[test]
fn test_random_runtime_special_seed_and_stream() {
    let env = VarEnv::<f64>::new();
    let seed = eval_expr_value::<f64>(&fn_call("automaticGlobalSeed", vec![]), &env);
    assert!(seed.is_finite());
    assert!(seed >= 1.0);

    let id = eval_expr_value::<f64>(&fn_call("initializeImpureRandom", vec![lit(seed)]), &env);
    assert!(id.is_finite());
    assert!(id >= 1.0);

    let r1 = eval_expr_value::<f64>(&fn_call("impureRandom", vec![lit(id)]), &env);
    let r2 = eval_expr_value::<f64>(&fn_call("impureRandom", vec![lit(id)]), &env);
    assert!(r1 > 0.0 && r1 <= 1.0);
    assert!(r2 > 0.0 && r2 <= 1.0);
    assert!(
        (r1 - r2).abs() > 1e-15,
        "impureRandom should advance stream state"
    );
}

#[test]
fn test_random_runtime_special_automatic_local_seed_is_stable_for_literal_path() {
    let env = VarEnv::<f64>::new();
    let a = eval_expr_value::<f64>(
        &fn_call(
            "automaticLocalSeed",
            vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String(
                    "Modelica.Blocks.Examples.Noise.UniformNoise".to_string(),
                ),
                span: rumoca_core::Span::DUMMY,
            }],
        ),
        &env,
    );
    let b = eval_expr_value::<f64>(
        &fn_call(
            "automaticLocalSeed",
            vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::String(
                    "Modelica.Blocks.Examples.Noise.UniformNoise".to_string(),
                ),
                span: rumoca_core::Span::DUMMY,
            }],
        ),
        &env,
    );
    assert_eq!(a, b);
    assert!(a.is_finite());
}

#[test]
fn test_modelica_strings_hash_string_matches_msl_examples() {
    assert_eq!(
        crate::modelica_strings_hash_string("this is a test"),
        1_827_717_433
    );
    assert_eq!(
        crate::modelica_strings_hash_string("Controller.noise1"),
        -1_025_762_750
    );
}

#[test]
fn random_runtime_special_missing_required_integer_arg_returns_error() {
    let env = VarEnv::<f64>::new();

    assert_eq!(
        eval_expr::<f64>(&fn_call("initializeImpureRandom", vec![]), &env),
        Err(EvalError::UnsupportedExpression {
            kind: "integer argument"
        })
    );
}

#[test]
fn test_random_runtime_special_qualified_xorshift_calls_are_supported() {
    let mut env = VarEnv::<f64>::new();
    env.set("state", 12345.0);

    let init = eval_expr_value::<f64>(
        &fn_call(
            "Modelica.Math.Random.Generators.Xorshift128plus.initialState",
            vec![int_lit(10), int_lit(20)],
        ),
        &env,
    );
    assert!(init.is_finite() && init >= 1.0);

    let r = eval_expr_value::<f64>(
        &fn_call(
            "Modelica.Math.Random.Generators.Xorshift64star.random",
            vec![var("state")],
        ),
        &env,
    );
    assert!(r > 0.0 && r <= 1.0);
}

#[test]
fn test_random_runtime_special_uses_structured_indexed_output_selection() {
    let mut env = VarEnv::<f64>::new();
    env.set("state[1]", 12345.0);
    env.set("state[2]", 67890.0);
    std::sync::Arc::make_mut(&mut env.dims).insert("state".to_string(), vec![2]);

    let selected = eval_expr::<f64>(
        &index_expr(
            fn_call(
                "Modelica.Math.Random.Generators.Xorshift64star.random.stateOut",
                vec![var("state")],
            ),
            1,
        ),
        &env,
    );

    assert_eq!(selected, Ok(2094191441.0));
    assert!(
        eval_expr::<f64>(
            &fn_call(
                "Modelica.Math.Random.Generators.Xorshift64star.random.stateOut[1]",
                vec![var("state")],
            ),
            &env,
        )
        .is_err()
    );
}

#[test]
fn test_msl_distribution_runtime_specials_are_supported() {
    let env = VarEnv::<f64>::new();

    let uniform = eval_expr_value::<f64>(
        &fn_call(
            "Modelica.Math.Distributions.Uniform.quantile",
            vec![lit(0.25), lit(-2.0), lit(2.0)],
        ),
        &env,
    );
    assert!((uniform + 1.0).abs() <= 1.0e-12);

    let normal_density = eval_expr_value::<f64>(
        &fn_call(
            "Modelica.Math.Distributions.Normal.density",
            vec![lit(0.0), lit(0.0), lit(1.0)],
        ),
        &env,
    );
    assert!((normal_density - 0.398_942_280_401_432_7).abs() <= 1.0e-12);

    // MLS Appendix B initialization solves equations containing pure function
    // calls. MSL's random-noise blocks use these distribution quantiles in
    // discrete initialization and event equations.
    let normal_quantile = eval_expr_value::<f64>(
        &fn_call(
            "Modelica.Math.Distributions.Normal.quantile",
            vec![lit(0.975), lit(0.0), lit(1.0)],
        ),
        &env,
    );
    assert!((normal_quantile - 1.959_963_986_120_195).abs() <= 1.0e-9);

    let erf_inv = eval_expr_value::<f64>(
        &fn_call("Modelica.Math.Special.erfInv", vec![lit(0.95)]),
        &env,
    );
    assert!((erf_inv - (normal_quantile / std::f64::consts::SQRT_2)).abs() <= 1.0e-12);

    let truncated = eval_expr_value::<f64>(
        &fn_call(
            "Modelica.Math.Distributions.TruncatedNormal.quantile",
            vec![lit(0.5), lit(-1.0), lit(1.0), lit(0.0), lit(1.0)],
        ),
        &env,
    );
    assert!(truncated.abs() <= 1.0e-9);
}

#[test]
fn test_table1d_default_columns_skip_abscissa() {
    let mut env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            rumoca_core::Expression::Empty {
                span: rumoca_core::Span::DUMMY,
            }, // no explicit columns mapping
            int_lit(1), // LinearSegments
            int_lit(1), // HoldLastPoint
        ],
    );
    let table_id = eval_expr_value::<f64>(&constructor, &env);
    assert!(table_id > 0.0);
    env.set("table_id", table_id);
    env.set("u", 1.0);

    let y = eval_expr_value::<f64>(
        &fn_call(
            "getTable1DValueNoDer",
            vec![var("table_id"), int_lit(1), var("u")],
        ),
        &env,
    );
    assert!((y - 12.0).abs() < 1e-12);
}
