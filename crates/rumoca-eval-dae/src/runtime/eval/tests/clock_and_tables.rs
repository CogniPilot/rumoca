use super::*;

#[test]
fn test_eval_clock_special_functions_are_finite() {
    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.0);

    let clock_expr = fn_call("Clock", vec![lit(0.02)]);
    let sub_expr = fn_call("subSample", vec![clock_expr.clone(), lit(2.0)]);
    let super_expr = fn_call("superSample", vec![clock_expr.clone(), lit(2.0)]);
    let shift_expr = fn_call(
        "shiftSample",
        vec![clock_expr.clone(), lit(1.0), lit(100.0)],
    );
    let back_expr = fn_call("backSample", vec![clock_expr.clone(), lit(1.0), lit(100.0)]);
    let hold_expr = fn_call("hold", vec![lit(7.0)]);
    let previous_expr = fn_call("previous", vec![lit(3.0)]);
    let interval_expr = fn_call("interval", vec![clock_expr.clone()]);
    let first_tick_expr = fn_call("firstTick", vec![]);

    for expr in [
        clock_expr,
        sub_expr,
        super_expr,
        shift_expr,
        back_expr,
        hold_expr,
        previous_expr,
        interval_expr,
        first_tick_expr,
    ] {
        let v = eval_expr::<f64>(&expr, &env);
        assert!(v.is_finite(), "clock special function returned non-finite");
    }
}

#[test]
fn test_eval_previous_uses_start_or_default_without_pre_store() {
    let mut env = VarEnv::<f64>::new();
    env.set("x", 5.0);
    env.start_exprs = std::sync::Arc::new(indexmap::IndexMap::from([("x".to_string(), lit(3.0))]));

    let previous = fn_call("previous", vec![var("x")]);
    assert_eq!(eval_expr::<f64>(&previous, &env), 3.0);

    env.start_exprs = std::sync::Arc::new(indexmap::IndexMap::new());
    assert_eq!(eval_expr::<f64>(&previous, &env), 0.0);
}

#[test]
fn test_eval_interval_for_clocked_var_uses_env_clock_interval_metadata() {
    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.0);
    env.clock_intervals = std::sync::Arc::new(IndexMap::from([("pulse.simTime".to_string(), 0.1)]));

    let interval_expr = fn_call("interval", vec![var("pulse.simTime")]);
    let value = eval_expr::<f64>(&interval_expr, &env);
    assert!(
        (value - 0.1).abs() <= 1e-12,
        "expected interval(pulse.simTime)=0.1, got {value}"
    );
}

#[test]
fn test_eval_builtin_sample_with_clock_alias_varref_uses_clock_metadata() {
    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.1);
    env.set("u", 42.0);
    env.set("sample2.clock", 0.0);
    env.clock_intervals = std::sync::Arc::new(IndexMap::from([("sample2.clock".to_string(), 0.1)]));

    let expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![var("u"), var("sample2.clock")],
    };
    let value = eval_expr::<f64>(&expr, &env);
    assert!(
        (value - 42.0).abs() <= 1e-12,
        "expected sample(value, clockAlias) to return sampled value, got {value}"
    );
}

#[test]
fn test_eval_subsample_counter_clock_uses_factor_resolution_ratio() {
    let mut env = VarEnv::<f64>::new();
    env.set("factor", 20.0);
    env.set("resolutionFactor", 1000.0);

    let sub_expr = fn_call(
        "subSample",
        vec![
            fn_call("Clock", vec![var("factor")]),
            var("resolutionFactor"),
        ],
    );

    for (time, expected_tick) in [
        (0.0, true),
        (0.01, false),
        (0.02, true),
        (0.03, false),
        (0.04, true),
    ] {
        env.set("time", time);
        let value = eval_expr::<f64>(&sub_expr, &env);
        assert_eq!(
            value > 0.5,
            expected_tick,
            "subSample(Clock(factor), resolutionFactor) tick mismatch at t={time}: value={value}"
        );
    }
}

#[test]
fn test_eval_shift_sample_clock_uses_fraction_of_base_interval() {
    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.06);

    let shift_expr = fn_call(
        "shiftSample",
        vec![fn_call("Clock", vec![lit(0.02)]), lit(2.0), lit(1.0)],
    );

    let value = eval_expr::<f64>(&shift_expr, &env);
    assert!(
        value > 0.5,
        "expected shiftSample(Clock(0.02), 2, 1) to tick at t=0.06, got {value}"
    );
}

#[test]
fn test_eval_builtin_sample_with_clock_returns_sampled_value_not_event_tick() {
    clear_pre_values();

    let mut env = VarEnv::<f64>::new();
    env.set("x", 10.0);
    env.set("time", 0.0);

    let sample_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![var("x"), fn_call("Clock", vec![lit(0.5)])],
    };

    assert_eq!(eval_expr::<f64>(&sample_expr, &env), 10.0);

    env.set("x", 20.0);
    env.set("time", 0.25);
    assert_eq!(eval_expr::<f64>(&sample_expr, &env), 20.0);

    env.set("time", 0.5);
    assert_eq!(eval_expr::<f64>(&sample_expr, &env), 20.0);
}

#[test]
fn test_eval_builtin_sample_start_interval_keeps_event_boolean_semantics() {
    clear_pre_values();

    let mut env = VarEnv::<f64>::new();
    let sample_event_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![lit(0.0), lit(0.5)],
    };

    env.set("time", 0.0);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 1.0);

    env.set("time", 0.25);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 0.0);

    env.set("time", 0.5);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 1.0);
}

#[test]
fn test_eval_builtin_sample_internal_three_arg_form_uses_start_and_interval() {
    clear_pre_values();

    let mut env = VarEnv::<f64>::new();
    let sample_event_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![lit(3.0), lit(0.5), lit(0.1)],
    };

    env.set("time", 0.0);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 0.0);

    env.set("time", 0.5);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 1.0);

    env.set("time", 0.6);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 1.0);

    env.set("time", 0.65);
    assert_eq!(eval_expr::<f64>(&sample_event_expr, &env), 0.0);
}

#[test]
fn test_eval_stream_special_functions_passthrough() {
    let mut env = VarEnv::<f64>::new();
    env.set("port.h_outflow", 123.0);

    let actual_stream = fn_call("actualStream", vec![var("port.h_outflow")]);
    let in_stream = fn_call("inStream", vec![var("port.h_outflow")]);

    assert_eq!(eval_expr::<f64>(&actual_stream, &env), 123.0);
    assert_eq!(eval_expr::<f64>(&in_stream, &env), 123.0);
}

#[test]
fn test_eval_stream_write_real_matrix_returns_success() {
    let env = VarEnv::<f64>::new();
    let write_real_matrix = fn_call(
        "Modelica.Utilities.Streams.writeRealMatrix",
        vec![
            dae::Expression::Literal(dae::Literal::String("test.mat".to_string())),
            dae::Expression::Literal(dae::Literal::String("A".to_string())),
            arr(
                vec![
                    dae::Expression::Literal(dae::Literal::Real(1.0)),
                    dae::Expression::Literal(dae::Literal::Real(2.0)),
                ],
                false,
            ),
        ],
    );

    assert_eq!(eval_expr::<f64>(&write_real_matrix, &env), 1.0);
}

#[test]
fn test_eval_state_accessor_special_functions() {
    let mut env = VarEnv::<f64>::new();
    env.set("state.T", 312.5);
    env.set("state.p", 101325.0);
    env.set("state.d", 998.2);
    env.set("state.h", 2.6e5);
    env.set("state.u", 2.4e5);
    env.set("state.s", 900.0);
    env.set("state_a.T", 315.0);
    env.set("states[1].T", 320.0);
    env.set("medium.state.T", 333.0);
    env.set("media[2].state.T", 346.0);

    assert_eq!(
        eval_expr::<f64>(&fn_call("Medium.temperature", vec![var("state")]), &env),
        312.5
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Medium.pressure", vec![var("state")]), &env),
        101325.0
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Medium.density", vec![var("state")]), &env),
        998.2
    );
    assert_eq!(
        eval_expr::<f64>(
            &fn_call("Medium.specificEnthalpy", vec![var("state")]),
            &env
        ),
        2.6e5
    );
    assert_eq!(
        eval_expr::<f64>(
            &fn_call("Medium.specificInternalEnergy", vec![var("state")]),
            &env
        ),
        2.4e5
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Medium.specificEntropy", vec![var("state")]), &env),
        900.0
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Medium.temperature", vec![var("state_a")]), &env),
        315.0
    );
    assert_eq!(
        eval_expr::<f64>(&fn_call("Medium.temperature", vec![var("states")]), &env),
        320.0
    );
    assert_eq!(
        eval_expr::<f64>(
            &fn_call(
                "Medium.temperature",
                vec![dae::Expression::FieldAccess {
                    base: Box::new(var("medium")),
                    field: "state".to_string(),
                }],
            ),
            &env
        ),
        333.0
    );
    assert_eq!(
        eval_expr::<f64>(
            &fn_call(
                "Medium.temperature",
                vec![dae::Expression::FieldAccess {
                    base: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("media"),
                        subscripts: vec![dae::Subscript::Index(2)],
                    }),
                    field: "state".to_string(),
                }],
            ),
            &env
        ),
        346.0
    );

    let h_from_phx = eval_expr::<f64>(
        &fn_call(
            "Medium.specificEnthalpy",
            vec![fn_call(
                "Medium.setState_phX",
                vec![lit(2.0e5), lit(4.2e5), arr(vec![lit(1.0)], false)],
            )],
        ),
        &env,
    );
    assert!((h_from_phx - 4.2e5).abs() < 1e-9);

    let t_from_ptx = eval_expr::<f64>(
        &fn_call(
            "Medium.temperature",
            vec![fn_call(
                "Medium.setState_pTX",
                vec![lit(1.5e5), lit(333.0), arr(vec![lit(1.0)], false)],
            )],
        ),
        &env,
    );
    assert!((t_from_ptx - 333.0).abs() < 1e-9);
}

#[test]
fn test_eval_state_accessor_temperature_setstate_phx_uses_user_helper() {
    let mut env = VarEnv::<f64>::new();
    let mut funcs = IndexMap::new();
    funcs.insert(
        "Medium.temperature_phX".to_string(),
        user_function_with_default_output("Medium.temperature_phX", 347.5),
    );
    env.functions = std::sync::Arc::new(funcs);

    let expr = fn_call(
        "Medium.temperature",
        vec![fn_call(
            "Medium.setState_phX",
            vec![lit(1.2e5), lit(4.0e5), arr(vec![lit(1.0)], false)],
        )],
    );
    assert!((eval_expr::<f64>(&expr, &env) - 347.5).abs() < 1e-9);
}

#[test]
fn test_eval_builtin_sample_behaviour() {
    let mut env = VarEnv::<f64>::new();
    env.set("u", 3.5);
    env.set("time", 0.0);

    let sampled_value = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![var("u")],
    };
    assert!((eval_expr::<f64>(&sampled_value, &env) - 3.5).abs() < 1e-12);

    let sample_tick = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![lit(0.0), lit(0.5)],
    };
    assert_eq!(eval_expr::<f64>(&sample_tick, &env), 1.0);
    env.set("time", 0.25);
    assert_eq!(eval_expr::<f64>(&sample_tick, &env), 0.0);
}

#[test]
fn test_eval_function_call_builtin_case_insensitive_alias() {
    let env = VarEnv::<f64>::new();
    let expr = fn_call("Integer", vec![lit(1.8)]);
    assert_eq!(eval_expr::<f64>(&expr, &env), 1.0);
}

#[test]
fn test_eval_special_distribution_density_overloads() {
    let mut env = VarEnv::<f64>::new();

    env.set("u", 0.0);
    env.set("u_min", -2.0);
    env.set("u_max", 2.0);
    let uniform = eval_expr::<f64>(
        &fn_call("distribution", vec![var("u"), var("u_min"), var("u_max")]),
        &env,
    );
    assert!((uniform - 0.25).abs() < 1e-12);

    env.set("mu", 0.0);
    env.set("sigma", 2.0);
    let normal = eval_expr::<f64>(
        &fn_call("distribution", vec![var("u"), var("mu"), var("sigma")]),
        &env,
    );
    assert!((normal - 0.199_471_140_200_716_35).abs() < 1e-12);

    env.set("lambda", 3.0);
    env.set("k", 1.5);
    env.set("u", 1.0);
    let weibull = eval_expr::<f64>(
        &fn_call("distribution", vec![var("u"), var("lambda"), var("k")]),
        &env,
    );
    assert!(weibull.is_finite() && weibull > 0.0);
}

#[test]
fn test_eval_builtin_cat_array_values() {
    let env = VarEnv::<f64>::new();
    let cat_expr = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Cat,
        args: vec![
            int_lit(1),
            arr(vec![lit(1.0), lit(2.0)], false),
            arr(vec![lit(3.0)], false),
        ],
    };

    let scalar = eval_expr::<f64>(&cat_expr, &env);
    assert!((scalar - 1.0).abs() < 1e-12);

    let values = eval_array_like_f64_values(&cat_expr, &env);
    assert_eq!(values, vec![1.0, 2.0, 3.0]);
}

#[test]
fn test_table1d_lookup_dual_linear_ad_slope() {
    let mut env = VarEnv::<Dual>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr::<Dual>(&constructor, &env).real();
    assert!(table_id > 0.0);
    env.set("table_id", Dual::from_f64(table_id));
    env.set("u", Dual::new(1.0, 1.0));

    let lookup = fn_call(
        "getTable1DValueNoDer",
        vec![var("table_id"), int_lit(1), var("u")],
    );
    let y = eval_expr::<Dual>(&lookup, &env);
    assert!((y.re - 12.0).abs() < 1e-12);
    assert!((y.du - 2.0).abs() < 1e-12);
}

#[test]
fn test_table1d_hold_extrapolation_clamps_ad_slope_to_zero() {
    let mut env = VarEnv::<Dual>::new();
    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr::<Dual>(&constructor, &env).real();
    assert!(table_id > 0.0);
    env.set("table_id", Dual::from_f64(table_id));
    env.set("u", Dual::new(5.0, 1.0));

    let lookup = fn_call(
        "getTable1DValueNoDer",
        vec![var("table_id"), int_lit(1), var("u")],
    );
    let y = eval_expr::<Dual>(&lookup, &env);
    assert!((y.re - 14.0).abs() < 1e-12);
    assert!(y.du.abs() < 1e-12);
}

#[test]
fn test_table1d_constructor_uses_start_expr_fallback_for_dynamic_dims() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([("tbl".to_string(), vec![0, 2])]));
    env.start_exprs = Arc::new(IndexMap::from([("tbl".to_string(), simple_table_expr())]));

    let constructor = fn_call(
        "ExternalCombiTable1D",
        vec![
            lit(0.0),
            lit(0.0),
            var("tbl"),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr::<f64>(&constructor, &env);
    assert!(table_id > 0.0);

    env.set("table_id", table_id);
    env.set("u", 1.0);
    let lookup = fn_call(
        "getTable1DValueNoDer",
        vec![var("table_id"), int_lit(1), var("u")],
    );
    let y = eval_expr::<f64>(&lookup, &env);
    assert!((y - 12.0).abs() < 1e-12);
}

#[test]
fn test_time_table_bounds_and_lookup() {
    let mut env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTimeTable",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            lit(0.0),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr::<f64>(&constructor, &env);
    assert!(table_id > 0.0);
    env.set("table_id", table_id);

    let t_min = eval_expr::<f64>(&fn_call("getTimeTableTmin", vec![var("table_id")]), &env);
    let t_max = eval_expr::<f64>(&fn_call("getTimeTableTmax", vec![var("table_id")]), &env);
    assert!((t_min - 0.0).abs() < 1e-12);
    assert!((t_max - 2.0).abs() < 1e-12);

    let y = eval_expr::<f64>(
        &fn_call(
            "getTimeTableValueNoDer",
            vec![var("table_id"), int_lit(1), lit(1.0)],
        ),
        &env,
    );
    assert!((y - 12.0).abs() < 1e-12);
}

#[test]
fn test_time_table_constructor_uses_if_matrix_start_fallback() {
    let mut env = VarEnv::<f64>::new();
    env.dims = Arc::new(IndexMap::from([("table_dyn".to_string(), vec![0, 2])]));
    env.start_exprs = Arc::new(IndexMap::from([(
        "table_dyn".to_string(),
        simple_table_if_expr(),
    )]));

    let constructor = fn_call(
        "ExternalCombiTimeTable",
        vec![
            lit(0.0),
            lit(0.0),
            var("table_dyn"),
            lit(0.0),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr::<f64>(&constructor, &env);
    assert!(table_id > 0.0);

    env.set("table_id", table_id);
    let y = eval_expr::<f64>(
        &fn_call(
            "getTimeTableValueNoDer",
            vec![var("table_id"), int_lit(1), lit(1.0)],
        ),
        &env,
    );
    assert!((y - 12.0).abs() < 1e-12);
}

#[test]
fn test_eval_array_values_handles_array_comprehension() {
    let env = VarEnv::<f64>::new();
    let expr = dae::Expression::ArrayComprehension {
        expr: Box::new(var("i")),
        indices: vec![dae::ComprehensionIndex {
            name: "i".to_string(),
            range: dae::Expression::Range {
                start: Box::new(int_lit(1)),
                step: None,
                end: Box::new(int_lit(4)),
            },
        }],
        filter: None,
    };
    let values = eval_array_values::<f64>(&expr, &env);
    assert_eq!(values, vec![1.0, 2.0, 3.0, 4.0]);
}

#[test]
fn test_time_table_next_event_edges() {
    let mut env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTimeTable",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            lit(0.0),
            columns_expr(),
            int_lit(1),
            int_lit(1),
        ],
    );
    let table_id = eval_expr::<f64>(&constructor, &env);
    assert!(table_id > 0.0);
    env.set("table_id", table_id);

    let before_start = eval_expr::<f64>(
        &fn_call("getNextTimeEvent", vec![var("table_id"), lit(-0.25)]),
        &env,
    );
    assert!((before_start - 0.0).abs() < 1e-12);

    let at_start = eval_expr::<f64>(
        &fn_call("getNextTimeEvent", vec![var("table_id"), lit(0.0)]),
        &env,
    );
    assert!((at_start - 2.0).abs() < 1e-12);

    let at_end = eval_expr::<f64>(
        &fn_call("getNextTimeEvent", vec![var("table_id"), lit(2.0)]),
        &env,
    );
    assert!(at_end.is_infinite() && at_end.is_sign_positive());
}

#[test]
fn test_time_table_next_event_periodic_wrap() {
    let mut env = VarEnv::<f64>::new();
    let constructor = fn_call(
        "ExternalCombiTimeTable",
        vec![
            lit(0.0),
            lit(0.0),
            simple_table_expr(),
            lit(0.0),
            columns_expr(),
            int_lit(1),
            int_lit(3),
        ],
    );
    let table_id = eval_expr::<f64>(&constructor, &env);
    assert!(table_id > 0.0);
    env.set("table_id", table_id);

    let next_after_end = eval_expr::<f64>(
        &fn_call("getNextTimeEvent", vec![var("table_id"), lit(2.25)]),
        &env,
    );
    assert!((next_after_end - 4.0).abs() < 1e-12);
}

#[test]
fn test_time_table_interpolation_coefficients_match_interaction1_edges() {
    let mut env = VarEnv::<f64>::new();
    let mut functions = IndexMap::new();
    functions.insert(
        "Modelica.Blocks.Sources.TimeTable.getInterpolationCoefficients".to_string(),
        interaction_time_table_coeff_function(),
    );
    env.functions = Arc::new(functions);
    env.start_exprs = Arc::new(IndexMap::from([(
        "srcTable".to_string(),
        interaction_time_table_expr(),
    )]));
    env.dims = Arc::new(IndexMap::from([("srcTable".to_string(), vec![6, 2])]));
    let table_values = eval_array_values::<f64>(&interaction_time_table_expr(), &VarEnv::new());
    set_array_entries(&mut env, "srcTable", &[6, 2], &table_values);

    let args = vec![
        var("srcTable"),
        lit(0.0),
        lit(0.0),
        lit(0.0),
        int_lit(1),
        lit(100.0 * f64::EPSILON),
        lit(0.0),
    ];

    let a = eval_expr::<f64>(
        &fn_call(
            "Modelica.Blocks.Sources.TimeTable.getInterpolationCoefficients.a",
            args.clone(),
        ),
        &env,
    );
    let b = eval_expr::<f64>(
        &fn_call(
            "Modelica.Blocks.Sources.TimeTable.getInterpolationCoefficients.b",
            args.clone(),
        ),
        &env,
    );
    let next_event = eval_expr::<f64>(
        &fn_call(
            "Modelica.Blocks.Sources.TimeTable.getInterpolationCoefficients.nextEventScaled",
            args.clone(),
        ),
        &env,
    );
    let next = eval_expr::<f64>(
        &fn_call(
            "Modelica.Blocks.Sources.TimeTable.getInterpolationCoefficients.next",
            args,
        ),
        &env,
    );

    assert!((a - 2.1).abs() < 1e-12);
    assert!(b.abs() < 1e-12);
    assert!((next_event - 1.0).abs() < 1e-12);
    assert!((next - 2.0).abs() < 1e-12);
}

#[test]
fn test_eval_expr_if_handles_when_condition_vectors() {
    let mut env = VarEnv::<f64>::new();
    env.is_initial = true;

    let expr = dae::Expression::If {
        branches: vec![(
            dae::Expression::Array {
                elements: vec![
                    dae::Expression::Literal(dae::Literal::Boolean(false)),
                    dae::Expression::BuiltinCall {
                        function: dae::BuiltinFunction::Initial,
                        args: vec![],
                    },
                ],
                is_matrix: false,
            },
            dae::Expression::Literal(dae::Literal::Real(5.0)),
        )],
        else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
    };

    assert_eq!(eval_expr::<f64>(&expr, &env), 5.0);
}
