use super::*;

#[test]
fn implicit_sample_of_derivative_reads_recovered_state_derivative_at_tick() {
    let mut dae_model = dae::Dae::default();
    for name in ["load.phi", "load.w"] {
        dae_model.states.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }
    for name in ["periodicClock.y", "sample1.y"] {
        dae_model.discrete_reals.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }

    dae_model.f_x.push(dae::Equation::explicit(
        dae::VarName::new("load.w"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("load.phi")],
        },
        rumoca_core::Span::DUMMY,
        "load.w = der(load.phi)",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.y"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![real(0.1)],
            is_constructor: false,
        },
        rumoca_core::Span::DUMMY,
        "periodicClock.y = Clock(0.1)",
    ));
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("sample1.y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Der,
                args: vec![var("load.phi")],
            }],
        },
        rumoca_core::Span::DUMMY,
        "sample1.y = sample(der(load.phi))",
    ));

    let mut pre_env = VarEnv::<f64>::new();
    pre_env.set("time", 0.09);
    pre_env.set("load.phi", 0.0);
    pre_env.set("load.w", 0.05);
    pre_env.set("periodicClock.y", 0.0);
    pre_env.set("sample1.y", 0.05);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&pre_env);

    let mut env = pre_env.clone();
    env.set("time", 0.1);
    env.set("load.w", 0.11);

    let recovered_env = build_sample_source_runtime_env(
        &dae_model,
        &dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("load.phi")],
        },
        &env,
    )
    .expect("closure");
    assert!((recovered_env.get("der(load.phi)") - 0.11).abs() <= 1.0e-12);

    let sampled_derivative = sampled_tick_value(
        &dae_model,
        &dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Der,
            args: vec![var("load.phi")],
        },
        None,
        &env,
    );
    assert!(
        (sampled_derivative - 0.11).abs() <= 1.0e-12,
        "sampled_derivative={sampled_derivative}"
    );

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);
    assert!(changed);
    assert!(
        (env.get("sample1.y") - 0.11).abs() <= 1.0e-12,
        "periodicClock.y={} der(load.phi)={} load.w={} sample1.y={}",
        env.get("periodicClock.y"),
        env.get("der(load.phi)"),
        env.get("load.w"),
        env.get("sample1.y"),
    );

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn explicit_signal_clock_expr_resolves_solver_backed_shift_hold_chain() {
    let dae_model = build_solver_backed_shift_hold_chain_dae();
    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.04);
    env.set("periodicClock.c", 1.0);
    env.set("sample1.clock", 1.0);

    let shift_clock = explicit_signal_clock_expr(&dae_model, &var("shiftSample1.y"), &env, 8)
        .expect("solver-backed sampled chain should keep explicit derived clock");
    let hold_clock = explicit_signal_clock_expr(&dae_model, &var("hold1.y"), &env, 8)
        .expect("hold input should inherit solver-backed derived clock");

    assert!(matches!(
        shift_clock,
        dae::Expression::FunctionCall { ref name, .. } if name.as_str() == "shiftSample"
    ));
    assert!(eval_sample_clock_active(&dae_model, &shift_clock, &env));
    let dae::Expression::FunctionCall { name, args, .. } = hold_clock else {
        panic!("hold wrapper should preserve a derived shiftSample clock");
    };
    assert_eq!(name.as_str(), "shiftSample");
    assert!(
        crate::runtime::clock::sample_clock_arg_is_explicit_clock(&dae_model, &args[0], &env),
        "hold wrapper should preserve an explicit source clock through solver-backed aliases"
    );
}

#[test]
fn discrete_array_guard_condition_activates_when_any_edge_fires() {
    let dae_model = dae::Dae::default();
    let mut pre_env = VarEnv::<f64>::new();
    pre_env.set("trigger", 0.0);
    pre_env.set("reset", 0.0);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&pre_env);

    let mut env = pre_env.clone();
    env.set("trigger", 1.0);
    let condition = dae::Expression::Array {
        elements: vec![
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Edge,
                args: vec![var("trigger")],
            },
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Edge,
                args: vec![var("reset")],
            },
        ],
        is_matrix: false,
    };

    assert_eq!(
        eval_discrete_condition_bool(&dae_model, &condition, &env),
        Some(true)
    );

    env.set("trigger", 0.0);
    assert_eq!(
        eval_discrete_condition_bool(&dae_model, &condition, &env),
        Some(false)
    );
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn discrete_assignment_value_handles_noevent_wrapped_scalar() {
    let dae_model = dae::Dae::default();
    let mut env = VarEnv::<f64>::new();
    env.set("x", 2.5);
    let solution = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::NoEvent,
        args: vec![var("x")],
    };

    let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, false);
    assert!((value - 2.5).abs() <= 1.0e-12);
}

#[test]
fn eval_discrete_condition_bool_treats_edge_of_implicit_clock_as_active_tick() {
    let dae_model = dae::Dae::default();
    let mut env = VarEnv::<f64>::new();
    env.set(rumoca_eval_dae::runtime::IMPLICIT_CLOCK_ACTIVE_ENV_KEY, 1.0);
    let condition = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Edge,
        args: vec![dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![],
            is_constructor: false,
        }],
    };

    assert_eq!(
        eval_discrete_condition_bool(&dae_model, &condition, &env),
        Some(true)
    );
}

#[test]
fn discrete_assignment_value_handles_singleton_tuple_scalar() {
    let dae_model = dae::Dae::default();
    let env = VarEnv::<f64>::new();
    let solution = dae::Expression::Tuple {
        elements: vec![dae::Expression::Literal(dae::Literal::Real(4.5))],
    };

    let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, false);
    assert!((value - 4.5).abs() <= 1.0e-12);
}

#[test]
fn discrete_partition_array_target_fast_paths_unary_builtin_array_source() {
    let dae_model = dae::Dae::default();
    let mut env = VarEnv::<f64>::new();
    env.dims = std::sync::Arc::new(indexmap::IndexMap::from([("y".to_string(), vec![3])]));
    rumoca_eval_dae::runtime::set_array_entries(&mut env, "y", &[3], &[0.0, 0.0, 0.0]);
    let solution = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Exp,
        args: vec![dae::Expression::Range {
            start: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
            step: None,
            end: Box::new(dae::Expression::Literal(dae::Literal::Real(2.0))),
        }],
    };
    let eq = dae::Equation::explicit(
        dae::VarName::new("y"),
        solution.clone(),
        rumoca_core::Span::DUMMY,
        "y = exp(0:2)",
    );

    let changed = apply_scalar_discrete_partition_equation(
        ScalarDiscreteEquationInput {
            dae: &dae_model,
            eq: &eq,
            target: "y",
            solution: &solution,
            env: &mut env,
            rhs_env: None,
            implicit_clock_active: true,
        },
        |env, target, new_value| {
            let old = env.vars.get(target).copied().unwrap_or(0.0);
            env.set(target, new_value);
            (old - new_value).abs() > 1.0e-12
        },
        |_target, _old_value, _new_value| {},
    );

    assert!(changed);
    assert!((env.get("y[1]") - 1.0).abs() <= 1.0e-12);
    assert!((env.get("y[2]") - 1.0f64.exp()).abs() <= 1.0e-12);
    assert!((env.get("y[3]") - 2.0f64.exp()).abs() <= 1.0e-12);
}

#[test]
fn shift_sample_value_form_uses_current_value_on_ticks_and_holds_between_ticks() {
    let dae_model = dae::Dae::default();
    let solution = dae::Expression::FunctionCall {
        name: dae::VarName::new("shiftSample"),
        args: vec![
            var("sampled"),
            dae::Expression::Literal(dae::Literal::Real(0.0)),
            dae::Expression::Literal(dae::Literal::Real(1.0)),
        ],
        is_constructor: false,
    };
    let mut env = VarEnv::<f64>::new();
    env.set("sampled", 9.0);
    env.set("shifted", 2.0);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("shifted", 2.0);

    let held = eval_discrete_assignment_value(&dae_model, "shifted", &solution, &env, false);
    let tick = eval_discrete_assignment_value(&dae_model, "shifted", &solution, &env, true);

    assert!((held - 2.0).abs() <= 1.0e-12);
    assert!((tick - 9.0).abs() <= 1.0e-12);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn shift_sample_value_form_updates_on_its_explicit_derived_clock() {
    let dae_model = dae::Dae::default();
    let solution = dae::Expression::FunctionCall {
        name: dae::VarName::new("shiftSample"),
        args: vec![
            var("sampled"),
            dae::Expression::Literal(dae::Literal::Real(2.0)),
            dae::Expression::Literal(dae::Literal::Real(1.0)),
        ],
        is_constructor: false,
    };
    let mut env = VarEnv::<f64>::new();
    env.clock_intervals =
        std::sync::Arc::new(indexmap::IndexMap::from([("sampled".to_string(), 0.02)]));
    env.set("time", 0.06);
    env.set("sampled", 1.0);
    env.set("shifted", 0.0);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("shifted", 0.0);

    let value = eval_discrete_assignment_value(&dae_model, "shifted", &solution, &env, false);
    assert!((value - 1.0).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn hold_assignment_updates_on_explicit_input_clock_when_implicit_clock_is_idle() {
    let mut dae_model = dae::Dae::default();
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("clocked_u"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("shiftSample"),
            args: vec![
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Sample,
                    args: vec![
                        dae::Expression::Literal(dae::Literal::Real(1.0)),
                        dae::Expression::FunctionCall {
                            name: dae::VarName::new("Clock"),
                            args: vec![dae::Expression::Literal(dae::Literal::Real(0.02))],
                            is_constructor: false,
                        },
                    ],
                },
                dae::Expression::Literal(dae::Literal::Real(2.0)),
                dae::Expression::Literal(dae::Literal::Real(1.0)),
            ],
            is_constructor: false,
        },
        Span::DUMMY,
        "clocked_u = shiftSample(sample(1.0, Clock(0.02)), 2, 1)",
    ));
    let solution = dae::Expression::FunctionCall {
        name: dae::VarName::new("hold"),
        args: vec![var("clocked_u")],
        is_constructor: false,
    };
    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.06);
    env.set("clocked_u", 1.0);
    env.set("y", 0.0);
    rumoca_eval_dae::runtime::clear_pre_values();
    rumoca_eval_dae::runtime::set_pre_value("y", 0.0);

    let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, false);
    assert!((value - 1.0).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn discrete_partition_tracks_sample_shift_hold_chain_from_flattened_clock_aliases() {
    let dae_model = build_flattened_clock_alias_shift_hold_dae();
    let mut env = VarEnv::<f64>::new();
    env.set("sample1.u", 0.0);
    env.set("sample1.y", 0.0);
    env.set("shiftSample1.u", 0.0);
    env.set("shiftSample1.y", 0.0);
    env.set("hold1.u", 0.0);
    env.set("hold1.y", 0.0);
    rumoca_eval_dae::runtime::clear_pre_values();

    for (time, sample_u) in [
        (0.0, 0.0),
        (0.02, 0.0),
        (0.04, 0.0),
        (0.05, 1.0),
        (0.06, 1.0),
    ] {
        env.set("time", time);
        env.set("sample1.u", sample_u);
        apply_discrete_partition_updates(&dae_model, &mut env);
        rumoca_eval_dae::runtime::seed_pre_values_from_env(&env);
    }

    assert!((env.get("sample1.y") - 1.0).abs() <= 1.0e-12);
    assert!((env.get("shiftSample1.y") - 0.0).abs() <= 1.0e-12);
    assert!((env.get("hold1.y") - 0.0).abs() <= 1.0e-12);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn discrete_partition_updates_sampled_output_on_initial_subsample_tick() {
    let mut dae_model = dae::Dae::default();
    for name in [
        "periodicClock.c",
        "periodicClock.y",
        "subSample1.u",
        "subSample1.y",
        "sample1.clock",
        "sample1.u",
        "sample1.y",
    ] {
        dae_model.discrete_valued.insert(
            dae::VarName::new(name),
            dae::Variable::new(dae::VarName::new(name)),
        );
    }
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.c"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![
                dae::Expression::Literal(dae::Literal::Integer(20)),
                dae::Expression::Literal(dae::Literal::Integer(1000)),
            ],
            is_constructor: false,
        },
        Span::DUMMY,
        "periodicClock.c = Clock(20, 1000)",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.y"),
        var("subSample1.u"),
        Span::DUMMY,
        "periodicClock.y = subSample1.u",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("periodicClock.y"),
        var("periodicClock.c"),
        Span::DUMMY,
        "periodicClock.y = periodicClock.c",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("subSample1.y"),
        var("sample1.clock"),
        Span::DUMMY,
        "subSample1.y = sample1.clock",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("subSample1.y"),
        dae::Expression::FunctionCall {
            name: dae::VarName::new("subSample"),
            args: vec![
                var("subSample1.u"),
                dae::Expression::Literal(dae::Literal::Integer(3)),
            ],
            is_constructor: false,
        },
        Span::DUMMY,
        "subSample1.y = subSample(subSample1.u, 3)",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("sample1.y"),
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![var("sample1.u"), var("sample1.clock")],
        },
        Span::DUMMY,
        "sample1.y = sample(sample1.u, sample1.clock)",
    ));

    let mut env = VarEnv::<f64>::new();
    env.set("time", 0.0);
    env.set("sample1.u", 0.1);
    env.set("sample1.y", 0.0);
    rumoca_eval_dae::runtime::clear_pre_values();

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);

    assert!(changed);
    assert!(
        (env.get("sample1.y") - 0.1).abs() <= 1.0e-12,
        "MLS §16.5.1 / §16.5.2: a sampled value on an active initial subSample tick must read the left limit of its unclocked input"
    );

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn discrete_partition_updates_sampled_output_on_initial_periodic_exact_subsample_tick() {
    let dae_model = build_periodic_exact_subsample_chain_dae();
    let mut env = seed_periodic_exact_subsample_env();
    rumoca_eval_dae::runtime::clear_pre_values();

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);

    assert!(changed);
    assert!(
        (env.get("periodicClock.c") - 1.0).abs() <= 1.0e-12,
        "PeriodicExactClock base clock must tick at initialization when lowered through subSample(Clock(factor), resolutionFactor)"
    );
    assert!(
        (env.get("periodicClock.y") - 1.0).abs() <= 1.0e-12,
        "clock alias propagation must forward the active base clock tick"
    );
    assert!(
        (env.get("subSample1.y") - 1.0).abs() <= 1.0e-12,
        "derived subSample clock must be active on the initial tick"
    );
    assert!(
        (env.get("sample1.clock") - 1.0).abs() <= 1.0e-12,
        "clock connector alias must forward the active derived clock tick"
    );
    assert!(
        crate::runtime::clock::sample_clock_arg_is_explicit_clock(
            &dae_model,
            &var("sample1.clock"),
            &env
        ),
        "sample1.clock must be recognized as an explicit alias-backed clock expression"
    );
    assert!(
        eval_sample_clock_active(&dae_model, &var("sample1.clock"), &env),
        "sample1.clock must be active on the initial PeriodicExactClock tick"
    );
    assert!(
        (env.get("sample1.y") - 0.1).abs() <= 1.0e-12,
        "MLS §16.5.1 / §16.5.2: PeriodicExactClock lowered through subSample(Clock(factor), resolutionFactor) must still trigger the initial sampled output tick"
    );

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn discrete_post_event_refresh_preserves_parameterized_shift_hold_chain_timings() {
    let dae_model = build_parameterized_shift_hold_chain_dae();
    let mut env = seed_parameterized_shift_hold_env();
    rumoca_eval_dae::runtime::clear_pre_values();

    let (shift_series, hold_series) = run_parameterized_shift_hold_series(
        &dae_model,
        &mut env,
        &[
            (0.0, 0.0),
            (0.02, 0.0),
            (0.04, 0.0),
            (0.05, 1.0),
            (0.06, 1.0),
            (0.16, 0.0),
            (0.2, 0.0),
        ],
    );
    assert_eq!(shift_series, vec![0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0]);
    assert_eq!(hold_series, vec![0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0]);

    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn hold_assignment_active_clock_handles_fill_builtin() {
    let dae_model = dae::Dae::default();
    let env = VarEnv::<f64>::new();
    let solution = dae::Expression::FunctionCall {
        name: dae::VarName::new("hold"),
        args: vec![dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Fill,
            args: vec![dae::Expression::Literal(dae::Literal::Real(3.5))],
        }],
        is_constructor: false,
    };

    let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, true);
    assert!((value - 3.5).abs() <= 1.0e-12);
}

#[test]
fn implicit_sample_inactive_uses_wrapped_value_without_prehistory() {
    let dae_model = dae::Dae::default();
    let mut env = VarEnv::<f64>::new();
    env.set("x", 6.0);
    rumoca_eval_dae::runtime::clear_pre_values();

    let solution = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::NoEvent,
            args: vec![var("x")],
        }],
    };
    let value = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, false);
    assert!((value - 6.0).abs() <= 1.0e-12);
}

#[test]
fn sample_start_interval_with_varrefs_is_not_treated_as_clocked_value_sample() {
    let dae_model = dae::Dae::default();
    let mut env = VarEnv::<f64>::new();
    env.set("start", 0.3);
    env.set("period", 0.3);

    let solution = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![var("start"), var("period")],
    };

    env.set("time", 0.45);
    let off_tick = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, true);
    assert!(
        (off_tick - 0.0).abs() <= 1.0e-12,
        "sample(start, period) must be event boolean between ticks, got {off_tick}"
    );

    env.set("time", 0.6);
    let on_tick = eval_discrete_assignment_value(&dae_model, "y", &solution, &env, true);
    assert!(
        (on_tick - 1.0).abs() <= 1.0e-12,
        "sample(start, period) must tick at t=start+n*period, got {on_tick}"
    );
}

#[test]
fn edge_of_sample_conjunction_updates_discrete_target_on_later_tick() {
    let mut dae_model = dae::Dae::default();
    dae_model.discrete_valued.insert(
        dae::VarName::new("sampling"),
        dae::Variable::new(dae::VarName::new("sampling")),
    );
    dae_model.discrete_reals.insert(
        dae::VarName::new("pulseStart"),
        dae::Variable::new(dae::VarName::new("pulseStart")),
    );
    dae_model.f_z.push(dae::Equation::explicit(
        dae::VarName::new("pulseStart"),
        dae::Expression::If {
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Edge,
                    args: vec![dae::Expression::Binary {
                        op: rumoca_ir_core::OpBinary::And(Default::default()),
                        lhs: Box::new(var("sampling")),
                        rhs: Box::new(dae::Expression::BuiltinCall {
                            function: dae::BuiltinFunction::Sample,
                            args: vec![real(0.0), real(1.0)],
                        }),
                    }],
                },
                var("time"),
            )],
            else_branch: Box::new(dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Pre,
                args: vec![var("pulseStart")],
            }),
        },
        Span::DUMMY,
        "pulseStart := if edge(sampling and sample(0, 1)) then time else pre(pulseStart)",
    ));

    rumoca_eval_dae::runtime::clear_pre_values();
    let mut env = VarEnv::<f64>::new();
    env.set("sampling", 1.0);
    env.set("pulseStart", 0.0);
    env.set("time", 0.5);
    rumoca_eval_dae::runtime::seed_pre_values_from_env(&env);

    env.set("time", 1.0);
    let changed = apply_discrete_partition_updates(&dae_model, &mut env);

    assert!(changed);
    assert!((env.get("pulseStart") - 1.0).abs() <= 1.0e-12);
    rumoca_eval_dae::runtime::clear_pre_values();
}

#[test]
fn discrete_partition_evaluates_parameter_to_discrete_assignment() {
    let mut dae_model = dae::Dae::default();
    dae_model.parameters.insert(
        dae::VarName::new("integerConstant.k"),
        dae::Variable::new(dae::VarName::new("integerConstant.k")),
    );
    dae_model.discrete_valued.insert(
        dae::VarName::new("integerConstant.y"),
        dae::Variable::new(dae::VarName::new("integerConstant.y")),
    );
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("integerConstant.y"),
        var("integerConstant.k"),
        rumoca_core::Span::DUMMY,
        "integerConstant.y = integerConstant.k",
    ));

    let mut env = VarEnv::<f64>::new();
    env.set("integerConstant.k", 1.0);
    env.set("integerConstant.y", 0.0);

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);
    assert!(
        changed,
        "parameter sourced assignment must update discrete target"
    );
    assert!((env.get("integerConstant.y") - 1.0).abs() <= 1.0e-12);
}

#[test]
fn discrete_connection_alias_preserves_runtime_source_value() {
    let mut dae_model = dae::Dae::default();
    dae_model.outputs.insert(
        dae::VarName::new("src"),
        dae::Variable::new(dae::VarName::new("src")),
    );
    dae_model.inputs.insert(
        dae::VarName::new("dst"),
        dae::Variable::new(dae::VarName::new("dst")),
    );
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("src"),
        real(1.0),
        rumoca_core::Span::DUMMY,
        "src = 1",
    ));
    dae_model.f_m.push(dae::Equation::explicit(
        dae::VarName::new("src"),
        var("dst"),
        rumoca_core::Span::DUMMY,
        "explicit connection equation: src = dst",
    ));

    let mut env = VarEnv::<f64>::new();
    env.set("src", 0.0);
    env.set("dst", 0.0);

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);
    assert!(changed);
    assert_eq!(env.get("src"), 1.0);
    assert_eq!(env.get("dst"), 1.0);
}

#[test]
fn discrete_partition_keeps_unique_non_alias_target_and_updates_alias_peer() {
    let mut dae_model = dae::Dae::default();
    dae_model.discrete_reals.insert(
        dae::VarName::new("sample1.y"),
        dae::Variable::new(dae::VarName::new("sample1.y")),
    );
    dae_model.discrete_reals.insert(
        dae::VarName::new("feedback.u2"),
        dae::Variable::new(dae::VarName::new("feedback.u2")),
    );
    dae_model.f_z.push(dae::Equation {
        lhs: None,
        rhs: dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(var("sample1.y")),
            rhs: Box::new(real(5.0)),
        },
        span: rumoca_core::Span::DUMMY,
        origin: "sample1.y = 5".to_string(),
        scalar_count: 1,
    });
    dae_model.f_z.push(dae::Equation {
        lhs: None,
        rhs: dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(Default::default()),
            lhs: Box::new(var("sample1.y")),
            rhs: Box::new(var("feedback.u2")),
        },
        span: rumoca_core::Span::DUMMY,
        origin: "sample1.y = feedback.u2".to_string(),
        scalar_count: 1,
    });

    let mut env = VarEnv::<f64>::new();
    env.set("sample1.y", 0.0);
    env.set("feedback.u2", 1.0);

    let changed = apply_discrete_partition_updates(&dae_model, &mut env);
    assert!(changed);
    assert!(
        (env.get("sample1.y") - 5.0).abs() <= 1.0e-12,
        "unique non-alias discrete definition must not be overwritten by alias peer"
    );
    assert!(
        (env.get("feedback.u2") - 5.0).abs() <= 1.0e-12,
        "alias peer should follow the settled discrete target"
    );
}

#[test]
fn discrete_clock_sources_filter_plain_non_clock_assignments() {
    let sample_rhs = dae::Expression::BuiltinCall {
        function: dae::BuiltinFunction::Sample,
        args: vec![
            var("u"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("Clock"),
                args: vec![real(0.1)],
                is_constructor: false,
            },
        ],
    };
    let dae_model = dae::Dae {
        f_z: vec![
            dae::Equation::explicit(
                dae::VarName::new("clocked"),
                sample_rhs.clone(),
                rumoca_core::Span::DUMMY,
                "clocked = sample(u, Clock(0.1))",
            ),
            dae::Equation::explicit(
                dae::VarName::new("forwarded"),
                var("clocked"),
                rumoca_core::Span::DUMMY,
                "forwarded = clocked",
            ),
            dae::Equation::explicit(
                dae::VarName::new("plain"),
                var("x"),
                rumoca_core::Span::DUMMY,
                "plain = x",
            ),
        ],
        ..Default::default()
    };
    let env = VarEnv::<f64>::new();

    let (_sources, active_solutions) =
        build_discrete_source_map_and_active_solutions(&dae_model, &env);

    assert_eq!(active_solutions.len(), 2);
    assert!(active_solutions.contains(&&sample_rhs));
    assert!(active_solutions.contains(&&var("clocked")));
    assert!(!active_solutions.contains(&&var("x")));
}
