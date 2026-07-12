use super::*;

#[test]
fn test_runtime_precompute_extracts_affine_time_event() {
    let cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Le,
        lhs: Box::new(rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs: Box::new(var("time")),
            rhs: Box::new(var("delay")),
            span: test_span(1, 2),
        }),
        rhs: Box::new(var("switch_at")),
        span: test_span(1, 2),
    };
    let mut dae_model = dae_with_if_condition(cond);
    let mut delay = dae::Variable::new(
        rumoca_core::VarName::new("delay"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    delay.start = Some(lit(0.25));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("delay"), delay);
    let mut switch_at = dae::Variable::new(
        rumoca_core::VarName::new("switch_at"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    switch_at.start = Some(lit(1.5));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("switch_at"), switch_at);

    populate_conditions(&mut dae_model);
    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert_eq!(dae_model.events.scheduled_time_events.len(), 1);
    assert!((dae_model.events.scheduled_time_events[0] - 1.25).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_extracts_discrete_partition_events() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("c"),
        dae::Variable::new(
            rumoca_core::VarName::new("c"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            sub(
                var("c"),
                rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Gt,
                    lhs: Box::new(var("time")),
                    rhs: Box::new(lit(0.5)),
                    span: test_span(1, 2),
                },
            ),
            test_span(1, 2),
            "test_discrete_partition",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert_eq!(dae_model.events.scheduled_time_events, vec![0.5]);
}

#[test]
fn test_runtime_precompute_collects_clock_constructor_exprs() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("s"),
        dae::Variable::new(
            rumoca_core::VarName::new("s"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("s").into(),
                    subscripts: vec![],
                    span: test_span(1, 2),
                }),
                rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sample,
                    args: vec![
                        rumoca_core::Expression::VarRef {
                            name: rumoca_core::VarName::new("u").into(),
                            subscripts: vec![],
                            span: test_span(100, 101),
                        },
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("Clock").into(),
                            args: vec![rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Real(0.1),
                                span: test_span(108, 111),
                            }],
                            is_constructor: false,
                            span: test_span(102, 112),
                        },
                    ],
                    span: test_span(95, 113),
                }),
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "test_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(dae_model.clocks.constructor_exprs.len(), 1);
    assert_eq!(dae_model.clocks.schedules.len(), 1);
    assert!((dae_model.clocks.schedules[0].period_seconds - 0.1).abs() <= 1e-12);
    assert!(dae_model.clocks.schedules[0].phase_seconds.abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["s"] - 0.1).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_rejects_static_clock_constructor_without_source_provenance() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("s"),
        dae::Variable::new(
            rumoca_core::VarName::new("s"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            sub(
                var("s"),
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Clock").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(0.1),
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                },
            ),
            Span::DUMMY,
            "unspanned_static_clock_constructor",
        ));

    let err = populate_runtime_precompute(&mut dae_model)
        .expect_err("source-free static clock constructors must fail fast");
    assert!(matches!(
        err,
        crate::ToDaeError::RuntimeMetadataViolation { detail }
            if detail.contains("source provenance")
    ));
}

#[test]
fn test_runtime_precompute_collects_sample_start_interval_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("s"),
        dae::Variable::new(
            rumoca_core::VarName::new("s"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            sub(
                var("s"),
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sample,
                    args: vec![
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(0.2),
                            span: test_span(120, 123),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(0.1),
                            span: test_span(125, 128),
                        },
                    ],
                    span: test_span(113, 129),
                },
            ),
            test_span(1, 2),
            // MLS §16.5.1: sample(start, interval) defines a periodic event.
            "periodic_sample_start_interval",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(dae_model.clocks.schedules.len(), 1);
    assert!((dae_model.clocks.schedules[0].period_seconds - 0.1).abs() <= 1e-12);
    assert!((dae_model.clocks.schedules[0].phase_seconds - 0.2).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_marks_schedule_backed_sample_root_condition() {
    let mut dae_model = dae::Dae::default();
    let relation = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME).into(),
        args: vec![lit(42.0), lit(0.05), lit(0.1)],
        is_constructor: false,
        span: test_span(120, 149),
    };
    dae_model.conditions.relations.push(relation.clone());
    dae_model.conditions.equations.push(dae::Equation::explicit(
        condition_lhs("c", 1),
        relation,
        test_span(1, 2),
        "scheduled sample condition memory",
    ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(dae_model.events.scheduled_root_conditions.len(), 1);
    let root = &dae_model.events.scheduled_root_conditions[0];
    assert_eq!(root.root_index, 0);
    assert!((root.period_seconds - 0.1).abs() <= 1e-12);
    assert!((root.phase_seconds - 0.05).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_collects_sample_schedule_from_initial_time_parameter() {
    let mut dae_model = dae::Dae::default();
    let mut frequency = dae::Variable::new(
        rumoca_core::VarName::new("mean.f"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    frequency.start = Some(lit(150.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("mean.f"), frequency);
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("mean.t0"),
        dae::Variable::new(
            rumoca_core::VarName::new("mean.t0"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("s"),
        dae::Variable::new(
            rumoca_core::VarName::new("s"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            sub(var("mean.t0"), var("time")),
            test_span(1, 2),
            "initial t0 = time",
        ));

    let interval = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Div,
        lhs: Box::new(lit(1.0)),
        rhs: Box::new(var("mean.f")),
        span: test_span(140, 148),
    };
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            sub(
                var("s"),
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME)
                        .into(),
                    args: vec![
                        rumoca_core::Expression::Binary {
                            op: rumoca_core::OpBinary::Add,
                            lhs: Box::new(var("mean.t0")),
                            rhs: Box::new(interval.clone()),
                            span: test_span(130, 148),
                        },
                        interval,
                    ],
                    is_constructor: false,
                    span: test_span(120, 149),
                },
            ),
            test_span(1, 2),
            "periodic sample with initial-time origin",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert_eq!(dae_model.clocks.schedules.len(), 1);
    assert!((dae_model.clocks.schedules[0].period_seconds - 1.0 / 150.0).abs() <= 1e-12);
    assert!((dae_model.clocks.schedules[0].phase_seconds - 1.0 / 150.0).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_assigns_implicit_sample_interval_from_unique_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("simTime"),
        dae::Variable::new(
            rumoca_core::VarName::new("simTime"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("clockY"),
        dae::Variable::new(
            rumoca_core::VarName::new("clockY"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    // simTime = sample(time) (implicit clock sample form)
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            sub(
                var("simTime"),
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sample,
                    args: vec![var("time")],
                    span: test_span(1, 2),
                },
            ),
            test_span(1, 2),
            "implicit_clocked_sample",
        ));

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            sub(
                var("clockY"),
                rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Clock").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(0.1),
                        span: test_span(140, 143),
                    }],
                    is_constructor: false,
                    span: test_span(134, 144),
                },
            ),
            test_span(1, 2),
            "periodic_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert_eq!(dae_model.clocks.schedules.len(), 1);
    assert!((dae_model.clocks.intervals["simTime"] - 0.1).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_propagates_no_argument_clock_guard_timing() {
    let mut dae_model = dae::Dae::default();
    for name in ["u", "dummy", "b"] {
        dae_model.variables.discrete_valued.insert(
            rumoca_core::VarName::new(name),
            dae::Variable::new(
                rumoca_core::VarName::new(name),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            ),
        );
    }
    let clock_span = test_span(1_000, 1_005);
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("u").into()),
        rhs: clock_call(0.02),
        span: test_span(1, 2),
        origin: "u = Clock(0.02)".to_string(),
        scalar_count: 1,
    });
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("dummy").into()),
        rhs: if_then_else(
            no_argument_clock_call(clock_span),
            var("u"),
            var("__pre__.dummy"),
        ),
        span: test_span(1, 2),
        origin: "when Clock() then dummy = u".to_string(),
        scalar_count: 1,
    });
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("b").into()),
        rhs: if_then_else(
            no_argument_clock_call(clock_span),
            rumoca_core::Expression::Unary {
                op: rumoca_core::OpUnary::Not,
                rhs: Box::new(var("__pre__.__pre__.b")),
                span: test_span(1, 2),
            },
            var("__pre__.b"),
        ),
        span: test_span(1, 2),
        origin: "when Clock() then b = not previous(b)".to_string(),
        scalar_count: 1,
    });

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert!((dae_model.clocks.intervals["dummy"] - 0.02).abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["b"] - 0.02).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_assigns_clock_interval_to_algebraic_alias_chain() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.inputs.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable::new(
            rumoca_core::VarName::new("u"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("feedback.y"),
        dae::Variable::new(
            rumoca_core::VarName::new("feedback.y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("PI.u"),
        dae::Variable::new(
            rumoca_core::VarName::new("PI.u"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("sample2.y"),
        dae::Variable::new(
            rumoca_core::VarName::new("sample2.y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("sample2.clock"),
        dae::Variable::new(
            rumoca_core::VarName::new("sample2.clock"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sample2.clock"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Clock").into(),
                args: vec![rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.1),
                    span: test_span(150, 153),
                }],
                is_constructor: false,
                span: test_span(144, 154),
            },
            test_span(1, 2),
            "explicit_clock_alias",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sample2.y"),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![var("u"), var("sample2.clock")],
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "explicit_sample_value",
        ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("sample2.y")),
            rhs: Box::new(var("feedback.y")),
            span: test_span(1, 2),
        },
        test_span(1, 2),
        "sample_alias",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("feedback.y")),
            rhs: Box::new(var("PI.u")),
            span: test_span(1, 2),
        },
        test_span(1, 2),
        "controller_input_alias",
    ));
    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert!((dae_model.clocks.intervals["sample2.clock"] - 0.1).abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["sample2.y"] - 0.1).abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["feedback.y"] - 0.1).abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["PI.u"] - 0.1).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_propagates_clock_across_indexed_vector_and_previous() {
    let mut dae_model = dae::Dae::default();
    let mut sampled = dae::Variable::new(rumoca_core::VarName::new("sampled"), test_span(1, 2));
    sampled.dims = vec![2];
    dae_model
        .variables
        .discrete_reals
        .insert(sampled.name.clone(), sampled);
    for name in ["delay1.u", "delay1.y", "delay2.u", "delay2.y"] {
        dae_model.variables.discrete_reals.insert(
            rumoca_core::VarName::new(name),
            dae::Variable::new(rumoca_core::VarName::new(name), test_span(1, 2)),
        );
    }
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("clock"),
        dae::Variable::new(rumoca_core::VarName::new("clock"), test_span(1, 2)),
    );

    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("clock"),
            clock_call(0.02),
            test_span(1, 2),
            "clock_source",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sampled"),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![var("u"), var("clock")],
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "sampled_vector",
        ));
    for (delay, index) in [("delay1", 1), ("delay2", 2)] {
        dae_model
            .discrete
            .real_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new(format!("{delay}.u")),
                condition_memory_ref("sampled", index),
                test_span(1, 2),
                "indexed_vector_alias",
            ));
        dae_model
            .discrete
            .real_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new(format!("{delay}.y")),
                var(&format!("__pre__.{delay}.u")),
                test_span(1, 2),
                "clocked_previous_value",
            ));
    }

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    for name in ["sampled", "delay1.u", "delay1.y", "delay2.u", "delay2.y"] {
        assert!((dae_model.clocks.intervals[name] - 0.02).abs() <= 1e-12);
    }
}

#[test]
fn test_runtime_precompute_propagates_uniform_clock_through_vector_alias_projection() {
    let mut dae_model = dae::Dae::default();
    for name in ["sampled", "alias"] {
        let mut variable = dae::Variable::new(rumoca_core::VarName::new(name), test_span(1, 2));
        variable.dims = vec![2];
        dae_model
            .variables
            .discrete_reals
            .insert(variable.name.clone(), variable);
    }
    for name in ["delay.u", "delay.y"] {
        dae_model.variables.discrete_reals.insert(
            rumoca_core::VarName::new(name),
            dae::Variable::new(rumoca_core::VarName::new(name), test_span(1, 2)),
        );
    }

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sampled"),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![var("u"), clock_call(0.02)],
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "direct_clocked_vector",
        ));
    dae_model.continuous.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("alias"),
        var("sampled"),
        test_span(1, 2),
        "untimed_vector_alias",
    ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("delay.u"),
            condition_memory_ref("alias", 1),
            test_span(1, 2),
            "indexed_alias_consumer",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("delay.y"),
            var("__pre__.delay.u"),
            test_span(1, 2),
            "previous_consumer",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    for name in ["sampled", "alias", "delay.u", "delay.y"] {
        assert!((dae_model.clocks.intervals[name] - 0.02).abs() <= 1e-12);
    }
}

#[test]
fn test_runtime_precompute_keeps_distinct_clocks_for_array_elements() {
    let mut dae_model = dae::Dae::default();
    let mut sampled = dae::Variable::new(rumoca_core::VarName::new("sampled"), test_span(1, 2));
    sampled.dims = vec![2];
    dae_model
        .variables
        .discrete_reals
        .insert(sampled.name.clone(), sampled);
    for name in ["clock1", "clock2"] {
        dae_model.variables.discrete_valued.insert(
            rumoca_core::VarName::new(name),
            dae::Variable::new(rumoca_core::VarName::new(name), test_span(1, 2)),
        );
    }
    for (name, period) in [("clock1", 0.1), ("clock2", 0.2)] {
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new(name),
                clock_call(period),
                test_span(1, 2),
                "independent_clock_source",
            ));
    }
    for (index, clock) in [(1, "clock1"), (2, "clock2")] {
        dae_model.discrete.real_updates.push(dae::Equation {
            lhs: Some(condition_lhs("sampled", index)),
            rhs: rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![var("u"), var(clock)],
                span: test_span(1, 2),
            },
            span: test_span(1, 2),
            origin: "independently_clocked_array_element".to_string(),
            scalar_count: 1,
        });
    }

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    assert!(!dae_model.clocks.intervals.contains_key("sampled"));
    assert!((dae_model.clocks.intervals["sampled[1]"] - 0.1).abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["sampled[2]"] - 0.2).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_promotes_equal_element_clocks_to_uniform_array() {
    let mut dae_model = dae::Dae::default();
    for name in ["sampled", "alias"] {
        let mut variable = dae::Variable::new(rumoca_core::VarName::new(name), test_span(1, 2));
        variable.dims = vec![2];
        dae_model
            .variables
            .discrete_reals
            .insert(variable.name.clone(), variable);
    }
    for name in ["clock1", "clock2"] {
        dae_model.variables.discrete_valued.insert(
            rumoca_core::VarName::new(name),
            dae::Variable::new(rumoca_core::VarName::new(name), test_span(1, 2)),
        );
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new(name),
                clock_call(0.1),
                test_span(1, 2),
                "uniform_clock_source",
            ));
    }
    for (index, clock) in [(1, "clock1"), (2, "clock2")] {
        dae_model.discrete.real_updates.push(dae::Equation {
            lhs: Some(condition_lhs("sampled", index)),
            rhs: rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![var("u"), var(clock)],
                span: test_span(1, 2),
            },
            span: test_span(1, 2),
            origin: "uniformly_clocked_array_element".to_string(),
            scalar_count: 1,
        });
    }
    dae_model.continuous.equations.push(dae::Equation::explicit(
        rumoca_core::VarName::new("alias"),
        var("sampled"),
        test_span(1, 2),
        "whole_array_alias",
    ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    for name in ["sampled", "alias"] {
        let interval = dae_model.clocks.intervals.get(name).unwrap_or_else(|| {
            panic!(
                "missing {name}; intervals={:?}",
                dae_model.clocks.intervals.keys().collect::<Vec<_>>()
            )
        });
        assert!((*interval - 0.1).abs() <= 1e-12);
    }
}

#[test]
fn test_runtime_precompute_does_not_assign_fallback_interval_for_non_sample_clock_ops() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("b"),
        dae::Variable::new(
            rumoca_core::VarName::new("b"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("clockY"),
        dae::Variable::new(
            rumoca_core::VarName::new("clockY"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );

    // b = pre(b) is discrete/event logic, not an implicit sample(..) form.
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("b")),
                rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Pre,
                    args: vec![var("b")],
                    span: test_span(1, 2),
                }),
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "pre_based_discrete_update",
        ));

    // Add one static periodic schedule in the model.
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("clockY")),
                rhs: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Clock").into(),
                    args: vec![rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(0.1),
                        span: test_span(160, 163),
                    }],
                    is_constructor: false,
                    span: test_span(154, 164),
                }),
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "periodic_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert_eq!(dae_model.clocks.schedules.len(), 1);
    assert!(
        !dae_model.clocks.intervals.contains_key("b"),
        "fallback interval must only apply to implicit sample(..) sources",
    );
}

#[test]
fn test_runtime_precompute_extracts_shifted_clock_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("b").into(),
                    subscripts: vec![],
                    span: test_span(1, 2),
                }),
                rhs: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("shiftSample").into(),
                    args: vec![
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("Clock").into(),
                            args: vec![rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Real(0.2),
                                span: test_span(170, 173),
                            }],
                            is_constructor: false,
                            span: test_span(164, 174),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: test_span(176, 179),
                        },
                    ],
                    is_constructor: false,
                    span: test_span(152, 180),
                }),
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "test_shifted_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert_eq!(dae_model.clocks.constructor_exprs.len(), 2);
    assert_eq!(dae_model.clocks.schedules.len(), 2);

    let has_base = dae_model.clocks.schedules.iter().any(|sched| {
        (sched.period_seconds - 0.2).abs() <= 1e-12 && sched.phase_seconds.abs() <= 1e-12
    });
    let has_shifted = dae_model.clocks.schedules.iter().any(|sched| {
        (sched.period_seconds - 0.2).abs() <= 1e-12 && (sched.phase_seconds - 0.2).abs() <= 1e-12
    });
    assert!(has_base);
    assert!(has_shifted);
}

#[test]
fn test_runtime_precompute_extracts_fractional_shift_sample_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("b")),
                rhs: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("shiftSample").into(),
                    args: vec![
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("Clock").into(),
                            args: vec![rumoca_core::Expression::Literal {
                                value: rumoca_core::Literal::Real(0.2),
                                span: test_span(190, 193),
                            }],
                            is_constructor: false,
                            span: test_span(184, 194),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: test_span(196, 199),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(5.0),
                            span: test_span(201, 204),
                        },
                    ],
                    is_constructor: false,
                    span: test_span(180, 205),
                }),
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "test_fractional_shifted_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert!(
        dae_model.clocks.schedules.iter().any(|sched| {
            (sched.period_seconds - 0.2).abs() <= 1e-12
                && (sched.phase_seconds - 0.04).abs() <= 1e-12
        }),
        "expected shiftSample(Clock(0.2), 1, 5) to shift by 1/5 of the base period"
    );
}

#[test]
fn test_runtime_precompute_extracts_fractional_back_sample_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("b")),
                rhs: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("backSample").into(),
                    args: vec![
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("shiftSample").into(),
                            args: vec![
                                rumoca_core::Expression::FunctionCall {
                                    name: rumoca_core::VarName::new("Clock").into(),
                                    args: vec![rumoca_core::Expression::Literal {
                                        value: rumoca_core::Literal::Real(0.2),
                                        span: test_span(210, 213),
                                    }],
                                    is_constructor: false,
                                    span: test_span(204, 214),
                                },
                                rumoca_core::Expression::Literal {
                                    value: rumoca_core::Literal::Real(2.0),
                                    span: test_span(216, 219),
                                },
                                rumoca_core::Expression::Literal {
                                    value: rumoca_core::Literal::Real(5.0),
                                    span: test_span(221, 224),
                                },
                            ],
                            is_constructor: false,
                            span: test_span(192, 225),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(1.0),
                            span: test_span(227, 230),
                        },
                        rumoca_core::Expression::Literal {
                            value: rumoca_core::Literal::Real(5.0),
                            span: test_span(232, 235),
                        },
                    ],
                    is_constructor: false,
                    span: test_span(180, 236),
                }),
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "test_fractional_back_sample_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert!(
        dae_model.clocks.schedules.iter().any(|sched| {
            (sched.period_seconds - 0.2).abs() <= 1e-12
                && (sched.phase_seconds - 0.04).abs() <= 1e-12
        }),
        "expected backSample(shiftSample(Clock(0.2), 2, 5), 1, 5) to land at phase 0.04"
    );
}

#[test]
fn test_runtime_precompute_records_per_variable_clock_phase() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("u"), {
            let mut source = dae::Variable::new(
                rumoca_core::VarName::new("u"),
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name(file!()),
                    1,
                    2,
                ),
            );
            source.start = Some(var("u_start"));
            source
        });
    let mut start = dae::Variable::new(
        rumoca_core::VarName::new("u_start"),
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
    );
    start.start = Some(lit(1.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("u_start"), start);
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable::new(
            rumoca_core::VarName::new("y"),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        ),
    );
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("u").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("shiftSample").into(),
            args: vec![clock_call(0.02), lit(4.0), lit(3.0)],
            is_constructor: false,
            span: test_span(1, 2),
        },
        span: test_span(1, 2),
        origin: "u = shiftSample(Clock(0.02), 4, 3)".to_string(),
        scalar_count: 1,
    });
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("backSample").into(),
            args: vec![var("u"), lit(4.0), lit(3.0)],
            is_constructor: false,
            span: test_span(1, 2),
        },
        span: test_span(1, 2),
        origin: "y = backSample(u, 4, 3)".to_string(),
        scalar_count: 1,
    });

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

    let u = dae_model
        .clocks
        .timings
        .get("u")
        .expect("shifted source timing should be recorded");
    assert!((u.period_seconds - 0.02).abs() <= 1e-12);
    assert!((u.phase_seconds - ((4.0 / 3.0) * 0.02)).abs() <= 1e-12);

    let y = dae_model
        .clocks
        .timings
        .get("y")
        .expect("back-sampled target timing should be recorded");
    assert!((y.period_seconds - 0.02).abs() <= 1e-12);
    assert!(y.phase_seconds.abs() <= 1e-12);
    assert!((dae_model.clocks.intervals["y"] - 0.02).abs() <= 1e-12);
}
