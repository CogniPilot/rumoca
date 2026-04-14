use super::*;

fn dynamic_next_event_guard(include_initial: bool) -> Expression {
    let time_reached = Expression::Binary {
        op: OpBinary::Ge(Default::default()),
        lhs: Box::new(var_ref("time")),
        rhs: Box::new(Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: vec![var_ref("nextEvent")],
        }),
    };
    if include_initial {
        Expression::Array {
            elements: vec![
                time_reached,
                Expression::BuiltinCall {
                    function: BuiltinFunction::Initial,
                    args: vec![],
                },
            ],
            is_matrix: false,
        }
    } else {
        time_reached
    }
}

fn next_event_rhs(include_initial: bool, step: f64) -> Expression {
    let post_event = Expression::Binary {
        op: OpBinary::Add(Default::default()),
        lhs: Box::new(Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: vec![var_ref("nextEvent")],
        }),
        rhs: Box::new(real(step)),
    };
    let updated = if include_initial {
        Expression::If {
            branches: vec![(
                Expression::BuiltinCall {
                    function: BuiltinFunction::Initial,
                    args: vec![],
                },
                real(step),
            )],
            else_branch: Box::new(post_event),
        }
    } else {
        post_event
    };
    Expression::If {
        branches: vec![(dynamic_next_event_guard(include_initial), updated)],
        else_branch: Box::new(Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: vec![var_ref("nextEvent")],
        }),
    }
}

fn counter_rhs(include_initial: bool) -> Expression {
    let updated = if include_initial {
        Expression::If {
            branches: vec![(
                Expression::BuiltinCall {
                    function: BuiltinFunction::Initial,
                    args: vec![],
                },
                real(1.0),
            )],
            else_branch: Box::new(Expression::Binary {
                op: OpBinary::Add(Default::default()),
                lhs: Box::new(Expression::BuiltinCall {
                    function: BuiltinFunction::Pre,
                    args: vec![var_ref("y")],
                }),
                rhs: Box::new(real(1.0)),
            }),
        }
    } else {
        Expression::Binary {
            op: OpBinary::Add(Default::default()),
            lhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                args: vec![var_ref("y")],
            }),
            rhs: Box::new(real(1.0)),
        }
    };
    Expression::If {
        branches: vec![(dynamic_next_event_guard(include_initial), updated)],
        else_branch: Box::new(Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args: vec![var_ref("y")],
        }),
    }
}

fn build_dynamic_next_event_counter_dae(initial_seed: bool) -> Dae {
    let mut dae = Dae::new();
    dae.outputs
        .insert(VarName::new("y_out"), Variable::new(VarName::new("y_out")));
    let mut y_var = Variable::new(VarName::new("y"));
    y_var.start = Some(real(0.0));
    dae.discrete_reals.insert(VarName::new("y"), y_var);
    let mut next_event = Variable::new(VarName::new("nextEvent"));
    next_event.start = Some(real(if initial_seed { 0.0 } else { 0.1 }));
    dae.discrete_reals
        .insert(VarName::new("nextEvent"), next_event);
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y_out"), var_ref("y")),
        span: Span::DUMMY,
        origin: "y_alias".to_string(),
        scalar_count: 1,
    });
    dae.f_z.push(dae::Equation {
        lhs: Some(VarName::new("nextEvent")),
        rhs: next_event_rhs(initial_seed, if initial_seed { 0.2 } else { 0.1 }),
        span: Span::DUMMY,
        origin: if initial_seed {
            "next_event_initial_seed".to_string()
        } else {
            "next_event_update".to_string()
        },
        scalar_count: 1,
    });
    dae.f_z.push(dae::Equation {
        lhs: Some(VarName::new("y")),
        rhs: counter_rhs(initial_seed),
        span: Span::DUMMY,
        origin: if initial_seed {
            "y_initial_seed".to_string()
        } else {
            "y_counter".to_string()
        },
        scalar_count: 1,
    });
    dae
}

#[test]
fn test_simulate_empty_dae() {
    let dae = Dae::new();
    let result = simulate(&dae, &SimOptions::default());
    assert!(matches!(result, Err(SimError::EmptySystem)));
}

#[test]
fn test_simulate_no_state_time_dependent_output_evolves_over_time() {
    let mut dae = Dae::new();
    dae.outputs
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));

    // 0 = y - (if time < 0.5 then 1 else 2)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            var_ref("y"),
            Expression::If {
                branches: vec![(
                    Expression::Binary {
                        op: OpBinary::Lt(Default::default()),
                        lhs: Box::new(var_ref("time")),
                        rhs: Box::new(real(0.5)),
                    },
                    real(1.0),
                )],
                else_branch: Box::new(real(2.0)),
            },
        ),
        span: Span::DUMMY,
        origin: "alg_time_switch".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 1.0,
            dt: Some(0.1),
            max_wall_seconds: Some(5.0),
            ..SimOptions::default()
        },
    )
    .expect("no-state time-dependent simulation should succeed");

    assert_eq!(
        result.n_states, 0,
        "dummy state must be hidden from outputs"
    );
    assert!(
        !result
            .names
            .iter()
            .any(|name| name == "_rumoca_dummy_state"),
        "dummy state should not leak into result names"
    );
    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("result should include y output");
    let y = &result.data[y_idx];
    assert!(
        !y.is_empty(),
        "expected output samples for y over the requested time horizon"
    );
    let first = *y.first().expect("first sample");
    let last = *y.last().expect("last sample");
    assert!(
        (first - 1.0).abs() < 1.0e-6,
        "y(0) should match the first branch value, got {first}"
    );
    assert!(
        (last - 2.0).abs() < 1.0e-6,
        "y(t_end) should switch to the second branch value, got {last}"
    );
}

#[test]
fn test_simulate_no_state_respects_intermediate_scheduled_events() {
    let mut dae = Dae::new();
    dae.outputs
        .insert(VarName::new("y_out"), Variable::new(VarName::new("y_out")));
    dae.discrete_reals
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y_out"), var_ref("y")),
        span: Span::DUMMY,
        origin: "y_alias".to_string(),
        scalar_count: 1,
    });
    dae.f_z.push(dae::Equation {
        lhs: Some(VarName::new("y")),
        rhs: Expression::If {
            branches: vec![(
                Expression::BuiltinCall {
                    function: BuiltinFunction::Sample,
                    args: vec![real(0.0), real(0.01)],
                },
                Expression::Binary {
                    op: OpBinary::Add(Default::default()),
                    lhs: Box::new(Expression::BuiltinCall {
                        function: BuiltinFunction::Pre,
                        args: vec![var_ref("y")],
                    }),
                    rhs: Box::new(real(1.0)),
                },
            )],
            else_branch: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                args: vec![var_ref("y")],
            }),
        },
        span: Span::DUMMY,
        origin: "y_counter".to_string(),
        scalar_count: 1,
    });
    // MLS §8.6 / Appendix B and §16.5.1: implicit sample(start, interval)
    // events must be processed at every tick even when the output sampling
    // grid is coarser.

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.05,
            dt: Some(0.05),
            max_wall_seconds: Some(2.0),
            ..SimOptions::default()
        },
    )
    .expect("no-state simulation should respect scheduled event times");

    let y_out_idx = result
        .names
        .iter()
        .position(|name| name == "y_out")
        .unwrap_or_else(|| panic!("missing y_out channel, got {:?}", result.names));
    let y_out = &result.data[y_out_idx];
    assert!(
        result.times.len() >= 2,
        "expected output schedule to include coarse endpoints, got {:?}",
        result.times
    );
    assert!(
        result
            .times
            .first()
            .is_some_and(|t| (*t - 0.0).abs() < 1.0e-12)
            && result
                .times
                .last()
                .is_some_and(|t| (*t - 0.05).abs() < 1.0e-12),
        "expected output schedule to keep coarse endpoints, got {:?}",
        result.times
    );
    assert!(
        result
            .times
            .iter()
            .any(|t| (*t - 0.01).abs() < 1.0e-12 && *t > 0.0)
            && result.times.iter().any(|t| *t > 0.01 && *t < 0.05),
        "expected no-state output schedule to include scheduled-event observations, got {:?}",
        result.times
    );
    let y0 = y_out.first().copied().unwrap_or_default();
    let y_end = y_out.last().copied().unwrap_or_default();
    assert!(
        y_end >= y0 + 4.0,
        "expected no-state y_out to advance across scheduled events, got start={y0} end={y_end}; series={y_out:?}, times={:?}",
        result.times
    );
}

#[test]
fn test_simulate_no_state_respects_dynamic_next_event_updates() {
    let dae = build_dynamic_next_event_counter_dae(false);
    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.31,
            dt: Some(0.31),
            max_wall_seconds: Some(2.0),
            ..SimOptions::default()
        },
    )
    .expect("no-state simulation should honor dynamically scheduled event times");

    let y_out_idx = result
        .names
        .iter()
        .position(|name| name == "y_out")
        .unwrap_or_else(|| panic!("missing y_out channel, got {:?}", result.names));
    let y_out = &result.data[y_out_idx];
    let y0 = y_out.first().copied().unwrap_or_default();
    let y_end = y_out.last().copied().unwrap_or_default();
    assert!(
        (y_end - (y0 + 3.0)).abs() < 1.0e-9,
        "expected y_out to increment at dynamic nextEvent times, got start={y0} end={y_end}; series={y_out:?}, times={:?}",
        result.times
    );
}

#[test]
fn test_collect_no_state_schedule_events_recognizes_periodic_pre_counter_guards() {
    let mut dae = Dae::new();
    let mut count = Variable::new(VarName::new("count"));
    count.start = Some(real(0.0));
    dae.discrete_valued.insert(VarName::new("count"), count);
    dae.f_m.push(dae::Equation {
        lhs: Some(VarName::new("count")),
        rhs: Expression::If {
            branches: vec![(
                Expression::Binary {
                    op: OpBinary::Ge(Default::default()),
                    lhs: Box::new(var_ref("time")),
                    rhs: Box::new(Expression::Binary {
                        op: OpBinary::Add(Default::default()),
                        lhs: Box::new(Expression::Binary {
                            op: OpBinary::Mul(Default::default()),
                            lhs: Box::new(Expression::Binary {
                                op: OpBinary::Add(Default::default()),
                                lhs: Box::new(Expression::BuiltinCall {
                                    function: BuiltinFunction::Pre,
                                    args: vec![var_ref("count")],
                                }),
                                rhs: Box::new(Expression::Literal(Literal::Integer(1))),
                            }),
                            rhs: Box::new(real(0.1)),
                        }),
                        rhs: Box::new(real(-0.035)),
                    }),
                },
                Expression::Binary {
                    op: OpBinary::Add(Default::default()),
                    lhs: Box::new(Expression::BuiltinCall {
                        function: BuiltinFunction::Pre,
                        args: vec![var_ref("count")],
                    }),
                    rhs: Box::new(Expression::Literal(Literal::Integer(1))),
                },
            )],
            else_branch: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Pre,
                args: vec![var_ref("count")],
            }),
        },
        span: Span::DUMMY,
        origin: "periodic_counter".to_string(),
        scalar_count: 1,
    });

    let events = super::collect_no_state_schedule_events(&dae, &[], 0.0, 0.3);
    assert!(
        events.iter().any(|t| (*t - 0.065).abs() < 1.0e-9),
        "expected first periodic guard event near 0.065, got {events:?}"
    );
    assert!(
        events.iter().any(|t| (*t - 0.165).abs() < 1.0e-9),
        "expected second periodic guard event near 0.165, got {events:?}"
    );
    assert!(
        events.iter().any(|t| (*t - 0.265).abs() < 1.0e-9),
        "expected third periodic guard event near 0.265, got {events:?}"
    );
}

#[test]
fn test_simulate_no_state_honors_initial_guarded_discrete_seed() {
    let dae = build_dynamic_next_event_counter_dae(true);
    // MLS §8.6 / Appendix B: `initial()` participates in the initial event
    // iteration for no-state runtime sampling, including lowered
    // `when {cond, initial()} then` guards.

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.5,
            dt: Some(0.5),
            max_wall_seconds: Some(2.0),
            ..SimOptions::default()
        },
    )
    .expect("no-state simulation should honor initial()-guarded discrete seeding");

    let y_out_idx = result
        .names
        .iter()
        .position(|name| name == "y_out")
        .unwrap_or_else(|| panic!("missing y_out channel, got {:?}", result.names));
    let y_out = &result.data[y_out_idx];
    let y0 = y_out.first().copied().unwrap_or_default();
    let y_end = y_out.last().copied().unwrap_or_default();
    assert!(
        (y0 - 1.0).abs() < 1.0e-9,
        "expected initial guard to seed y_out at t_start, got {y0}; series={y_out:?}, times={:?}",
        result.times
    );
    assert!(
        (y_end - 3.0).abs() < 1.0e-9,
        "expected follow-up dynamic events to increment y_out after initial seed, got {y_end}; series={y_out:?}, times={:?}",
        result.times
    );
}

#[test]
fn test_simulate_no_state_skips_ic_newton_for_singular_algebraic_seed() {
    let mut dae = Dae::new();
    dae.outputs
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));

    // Add algebraics that are structurally singular (`0 = a - a`). These are
    // harmless for sampled algebraic projection but can stall IC Newton if run
    // up-front on no-state systems.
    for idx in 0..40 {
        let name = VarName::new(format!("a{idx}"));
        let mut var = Variable::new(name.clone());
        var.start = Some(real(1.0));
        dae.algebraics.insert(name.clone(), var);
        dae.f_x.push(dae::Equation {
            lhs: None,
            rhs: sub(var_ref(name.as_str()), var_ref(name.as_str())),
            span: Span::DUMMY,
            origin: "singular_alg_identity".to_string(),
            scalar_count: 1,
        });
    }

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(var_ref("y"), real(1.0)),
        span: Span::DUMMY,
        origin: "output_assignment".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_start: 0.0,
            t_end: 0.1,
            dt: Some(0.1),
            max_wall_seconds: Some(0.2),
            ..SimOptions::default()
        },
    )
    .expect("no-state simulation should bypass IC Newton and complete");

    let y_idx = result
        .names
        .iter()
        .position(|name| name == "y")
        .expect("result should include y");
    assert!(
        result.data[y_idx].iter().all(|v| (v - 1.0).abs() < 1.0e-6),
        "y should stay at assigned value"
    );
}

#[test]
fn test_simulate_timeout_enforced_during_initialization() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.algebraics
        .insert(VarName::new("z"), Variable::new(VarName::new("z")));

    // 0 = z - der(x)
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(Expression::VarRef {
                name: VarName::new("z"),
                subscripts: vec![],
            }),
            rhs: Box::new(Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![Expression::VarRef {
                    name: VarName::new("x"),
                    subscripts: vec![],
                }],
            }),
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    });
    // 0 = z - 1
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(Expression::VarRef {
                name: VarName::new("z"),
                subscripts: vec![],
            }),
            rhs: Box::new(Expression::Literal(Literal::Real(1.0))),
        },
        span: Span::DUMMY,
        origin: "test".to_string(),
        scalar_count: 1,
    });

    let opts = SimOptions {
        max_wall_seconds: Some(f64::MIN_POSITIVE),
        ..SimOptions::default()
    };
    let result = simulate(&dae, &opts);
    assert!(
        matches!(result, Err(SimError::Timeout { .. })),
        "expected timeout, got {result:?}"
    );
}

#[test]
fn test_simulate_fails_fast_for_unsupported_external_function_call() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            Expression::FunctionCall {
                name: VarName::new("f"),
                args: vec![var_ref("x")],
                is_constructor: false,
            },
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let mut external_stub = rumoca_ir_dae::Function::new("f", Span::DUMMY);
    external_stub.external = Some(ExternalFunction {
        language: "C".to_string(),
        function_name: Some("f".to_string()),
        output_name: None,
        arg_names: vec!["x".to_string()],
    });
    dae.functions.insert(VarName::new("f"), external_stub);

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    );
    assert!(
        matches!(result, Err(SimError::UnsupportedFunction { .. })),
        "expected unsupported function error, got {result:?}"
    );
}

#[test]
fn test_simulate_rejects_member_style_function_call_without_exact_definition() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            Expression::FunctionCall {
                name: VarName::new("world.gravityAcceleration"),
                args: vec![var_ref("x")],
                is_constructor: false,
            },
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let mut fn_def = rumoca_ir_dae::Function::new(
        "Modelica.Mechanics.MultiBody.World.gravityAcceleration",
        Span::DUMMY,
    );
    fn_def.body.push(rumoca_ir_dae::Statement::Return);
    dae.functions.insert(fn_def.name.clone(), fn_def);

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    );
    assert!(
        matches!(
            result,
            Err(SimError::UnsupportedFunction { ref name, .. })
                if name == "world.gravityAcceleration"
        ),
        "expected unresolved member-style function to fail fast, got {result:?}"
    );
}

#[test]
fn test_simulate_rejects_constructor_field_projection_without_definition() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));

    let constructor_field = Expression::FieldAccess {
        base: Box::new(Expression::FunctionCall {
            name: VarName::new("My.Record"),
            args: vec![real(2.0), real(3.0)],
            is_constructor: true,
        }),
        field: "C".to_string(),
    };

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            constructor_field,
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    );
    assert!(
        matches!(
            result,
            Err(SimError::UnsupportedFunction { ref name, .. }) if name == "My.Record.C"
        ),
        "expected unresolved constructor field projection to fail fast, got {result:?}"
    );
}

#[test]
fn test_simulate_allows_constructor_field_projection_with_signature() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));

    let constructor_field = Expression::FieldAccess {
        base: Box::new(Expression::FunctionCall {
            name: VarName::new("My.Record"),
            args: vec![real(2.0), real(3.0)],
            is_constructor: true,
        }),
        field: "C".to_string(),
    };

    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            constructor_field,
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let mut record_ctor = rumoca_ir_dae::Function::new("My.Record", Span::DUMMY);
    record_ctor.inputs.push(rumoca_ir_dae::FunctionParam {
        name: "R".to_string(),
        type_name: "Real".to_string(),
        dims: Vec::new(),
        default: None,
        description: None,
    });
    record_ctor.inputs.push(rumoca_ir_dae::FunctionParam {
        name: "C".to_string(),
        type_name: "Real".to_string(),
        dims: Vec::new(),
        default: None,
        description: None,
    });
    dae.functions.insert(record_ctor.name.clone(), record_ctor);

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    );
    assert!(
        result.is_ok(),
        "constructor field projection with constructor signature should be simulatable, got {result:?}"
    );
}

#[test]
fn test_validate_simulation_support_allows_assert_statement_message_helpers() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::FunctionCall {
                name: VarName::new("Modelica.Utilities.Strings.length"),
                args: vec![Expression::Literal(Literal::String("hello".to_string()))],
                is_constructor: false,
            }),
        },
        span: Span::DUMMY,
        origin: "string-helper".to_string(),
        scalar_count: 1,
    });

    let result = validate_simulation_function_support(&dae);
    assert!(
        result.is_ok(),
        "assert message helpers should not block simulation preflight, got {result:?}"
    );
}

#[test]
fn test_validate_simulation_support_allows_assert_function_call_message_helpers() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::FunctionCall {
                name: VarName::new("Modelica.Utilities.Strings.length"),
                args: vec![Expression::Literal(Literal::String("hello".to_string()))],
                is_constructor: false,
            }),
        },
        span: Span::DUMMY,
        origin: "string-helper-2".to_string(),
        scalar_count: 1,
    });

    let result = validate_simulation_function_support(&dae);
    assert!(
        result.is_ok(),
        "assert(...) call should be accepted in simulation preflight, got {result:?}"
    );
}

#[test]
fn test_validate_simulation_support_allows_full_path_name_helper() {
    let mut dae = Dae::new();
    dae.algebraics
        .insert(VarName::new("y"), Variable::new(VarName::new("y")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::Binary {
            op: OpBinary::Sub(Default::default()),
            lhs: Box::new(var_ref("y")),
            rhs: Box::new(Expression::FunctionCall {
                name: VarName::new("Modelica.Utilities.Files.fullPathName"),
                args: vec![Expression::Literal(Literal::String("hello".to_string()))],
                is_constructor: false,
            }),
        },
        span: Span::DUMMY,
        origin: "full-path-helper".to_string(),
        scalar_count: 1,
    });

    let result = validate_simulation_function_support(&dae);
    assert!(
        result.is_ok(),
        "fullPathName helper should not block simulation preflight, got {result:?}"
    );
}

#[test]
fn test_validate_simulation_support_allows_time_table_next_event_function() {
    let mut dae = Dae::new();
    dae.parameters.insert(
        VarName::new("table_id"),
        Variable::new(VarName::new("table_id")),
    );
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: Expression::FunctionCall {
            name: VarName::new("Modelica.Blocks.Tables.Internal.getNextTimeEvent"),
            args: vec![var_ref("table_id"), real(0.0)],
            is_constructor: false,
        },
        span: Span::DUMMY,
        origin: "next-time-event".to_string(),
        scalar_count: 1,
    });

    let result = validate_simulation_function_support(&dae);
    assert!(
        result.is_ok(),
        "table next-time-event helper should be accepted in simulation preflight, got {result:?}"
    );
}

#[test]
fn test_validate_simulation_support_allows_function_parameter_call_aliases() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            Expression::FunctionCall {
                name: VarName::new("wrapper"),
                args: vec![
                    Expression::FunctionCall {
                        name: VarName::new("fun_impl"),
                        args: vec![real(2.0)],
                        is_constructor: false,
                    },
                    var_ref("x"),
                ],
                is_constructor: false,
            },
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let mut fun_impl = rumoca_ir_dae::Function::new("fun_impl", Span::DUMMY);
    fun_impl
        .inputs
        .push(rumoca_ir_dae::FunctionParam::new("u", "Real"));
    fun_impl
        .inputs
        .push(rumoca_ir_dae::FunctionParam::new("a", "Real"));
    fun_impl
        .outputs
        .push(
            rumoca_ir_dae::FunctionParam::new("y", "Real").with_default(Expression::Binary {
                op: OpBinary::Add(Default::default()),
                lhs: Box::new(var_ref("u")),
                rhs: Box::new(var_ref("a")),
            }),
        );
    fun_impl.body.push(Statement::Empty);
    dae.functions.insert(fun_impl.name.clone(), fun_impl);

    let mut wrapper = rumoca_ir_dae::Function::new("wrapper", Span::DUMMY);
    wrapper.inputs.push(rumoca_ir_dae::FunctionParam::new(
        "f",
        "Pkg.Interfaces.PartialFunction",
    ));
    wrapper
        .inputs
        .push(rumoca_ir_dae::FunctionParam::new("x", "Real"));
    wrapper
        .outputs
        .push(rumoca_ir_dae::FunctionParam::new("y", "Real").with_default(
            Expression::FunctionCall {
                name: VarName::new("wrapper.f"),
                args: vec![var_ref("x")],
                is_constructor: false,
            },
        ));
    wrapper.body.push(Statement::Empty);
    dae.functions.insert(wrapper.name.clone(), wrapper);

    let result = validate_simulation_function_support(&dae);
    assert!(
        result.is_ok(),
        "function-typed input aliases (wrapper.f) should be accepted in simulation preflight, got {result:?}"
    );
}

#[test]
fn test_simulate_rejects_reachable_nested_unsupported_external_function() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            Expression::FunctionCall {
                name: VarName::new("wrapper"),
                args: vec![],
                is_constructor: false,
            },
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let mut bad = rumoca_ir_dae::Function::new("bad_external", Span::DUMMY);
    bad.external = Some(ExternalFunction {
        language: "C".to_string(),
        function_name: Some("bad_external".to_string()),
        output_name: None,
        arg_names: vec![],
    });
    dae.functions.insert(VarName::new("bad_external"), bad);

    let mut wrapper = rumoca_ir_dae::Function::new("wrapper", Span::DUMMY);
    wrapper.body.push(rumoca_ir_dae::Statement::FunctionCall {
        comp: comp_ref("bad_external"),
        args: vec![],
        outputs: vec![],
    });
    dae.functions.insert(VarName::new("wrapper"), wrapper);

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    );
    assert!(
        matches!(
            result,
            Err(SimError::UnsupportedFunction { ref name, .. }) if name == "bad_external"
        ),
        "expected reachable nested external function to fail preflight, got {result:?}"
    );
}

#[test]
fn test_validate_simulation_support_ignores_unreachable_nested_unsupported_function() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            real(0.0),
        ),
        span: Span::DUMMY,
        origin: "ode".to_string(),
        scalar_count: 1,
    });

    let mut bad = rumoca_ir_dae::Function::new("bad_external", Span::DUMMY);
    bad.external = Some(ExternalFunction {
        language: "C".to_string(),
        function_name: Some("bad_external".to_string()),
        output_name: None,
        arg_names: vec![],
    });
    dae.functions.insert(VarName::new("bad_external"), bad);

    let mut wrapper = rumoca_ir_dae::Function::new("wrapper", Span::DUMMY);
    wrapper.body.push(rumoca_ir_dae::Statement::FunctionCall {
        comp: comp_ref("bad_external"),
        args: vec![],
        outputs: vec![],
    });
    dae.functions.insert(VarName::new("wrapper"), wrapper);

    let result = validate_simulation_function_support(&dae);
    assert!(
        result.is_ok(),
        "unreachable nested unsupported functions should not fail simulation preflight, got {result:?}"
    );
}

#[test]
fn test_simulate_reports_division_by_zero_at_initialization() {
    let mut dae = Dae::new();
    dae.states
        .insert(VarName::new("x"), Variable::new(VarName::new("x")));
    dae.parameters
        .insert(VarName::new("p"), Variable::new(VarName::new("p")));

    // 0 = der(x) - (1 / if initial() then p else 1), with p defaulting to 0.0.
    dae.f_x.push(dae::Equation {
        lhs: None,
        rhs: sub(
            Expression::BuiltinCall {
                function: BuiltinFunction::Der,
                args: vec![var_ref("x")],
            },
            Expression::Binary {
                op: OpBinary::Div(Default::default()),
                lhs: Box::new(real(1.0)),
                rhs: Box::new(Expression::If {
                    branches: vec![(
                        Expression::BuiltinCall {
                            function: BuiltinFunction::Initial,
                            args: vec![],
                        },
                        var_ref("p"),
                    )],
                    else_branch: Box::new(real(1.0)),
                }),
            },
        ),
        span: Span::DUMMY,
        origin: "ode_with_div_zero".to_string(),
        scalar_count: 1,
    });

    let result = simulate(
        &dae,
        &SimOptions {
            t_end: 0.1,
            max_wall_seconds: Some(1.0),
            ..SimOptions::default()
        },
    );
    match result {
        Err(SimError::SolverError(msg)) => {
            assert!(
                msg.contains("division by zero at initialization"),
                "expected division-by-zero diagnostic, got: {msg}"
            );
            assert!(
                msg.contains("divisor expression is: If")
                    && msg.contains("BuiltinCall { function: Initial"),
                "expected divisor expression in diagnostic, got: {msg}"
            );
            assert!(
                msg.contains("origin='ode_with_div_zero'"),
                "expected equation origin in diagnostic, got: {msg}"
            );
        }
        other => panic!("expected solver error, got: {other:?}"),
    }
}
