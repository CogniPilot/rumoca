use super::*;

#[cfg(test)]
mod clocked_initial_tests;

fn test_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("lower_intrinsics_test.mo"),
        0,
        1,
    )
}

#[test]
fn lower_expression_supports_interval_intrinsic() {
    let layout = VarLayout::default();
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("interval").into(),
        args: vec![rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Clock").into(),
            args: vec![rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.2),
                span: test_span(),
            }],
            is_constructor: false,

            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };
    let lowered =
        lower_expression(&expr, &layout, &IndexMap::new()).expect("interval should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 0.2).abs() < 1e-12);
}

#[test]
fn malformed_synchronous_intrinsics_report_call_span() {
    let layout = VarLayout::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("missing_sync_arg.mo"),
        5,
        19,
    );
    for name in ["previous", "hold", "noClock", "superSample"] {
        let expr = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new(name).into(),
            args: Vec::new(),
            is_constructor: false,
            span,
        };
        let result = lower_expression(&expr, &layout, &IndexMap::new());
        let Err(err) = result else {
            panic!("{name} without required arguments should fail");
        };
        assert_eq!(err.source_span(), Some(span), "{name} should use call span");
        assert!(
            err.reason().contains("requires argument 1"),
            "{name} error should describe missing argument: {}",
            err.reason()
        );
    }
}

#[test]
fn malformed_synchronous_builtins_report_call_span() {
    let layout = VarLayout::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("missing_sync_builtin_arg.mo"),
        11,
        25,
    );
    for function in [
        rumoca_core::BuiltinFunction::Sample,
        rumoca_core::BuiltinFunction::Edge,
        rumoca_core::BuiltinFunction::Change,
    ] {
        let expr = rumoca_core::Expression::BuiltinCall {
            function,
            args: Vec::new(),
            span,
        };
        let result = lower_expression(&expr, &layout, &IndexMap::new());
        let Err(err) = result else {
            panic!("{} without required arguments should fail", function.name());
        };
        assert_eq!(
            err.source_span(),
            Some(span),
            "{} should use call span",
            function.name()
        );
        assert!(
            err.reason().contains("requires argument 1"),
            "{} error should describe missing argument: {}",
            function.name(),
            err.reason()
        );
    }
}

#[test]
fn malformed_complex_projection_intrinsics_report_call_span() {
    let layout = VarLayout::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("missing_complex_arg.mo"),
        7,
        29,
    );
    for name in ["Modelica.ComplexMath.sum", "Complex.'+'"] {
        let expr = rumoca_core::Expression::FieldAccess {
            base: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::Reference::from_component_reference(
                    test_component_ref_from_name(name),
                ),
                args: Vec::new(),
                is_constructor: false,
                span,
            }),
            field: "re".to_string(),
            span,
        };
        let result = lower_expression(&expr, &layout, &IndexMap::new());
        let Err(err) = result else {
            panic!("{name}.re without required arguments should fail");
        };
        assert_eq!(err.source_span(), Some(span), "{name} should use call span");
        assert!(
            err.reason().contains("requires"),
            "{name} error should describe missing argument: {}",
            err.reason()
        );
    }
}

#[test]
fn lower_expression_supports_sample_start_interval_tick() {
    let layout = VarLayout::default();
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sample,
        args: vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.2),
                span: test_span(),
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.1),
                span: test_span(),
            },
        ],
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("MLS §16.5.1 sample(start, interval) should lower");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.3);
    assert_eq!(read_reg(&regs, lowered.result), 1.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.35);
    assert_eq!(read_reg(&regs, lowered.result), 0.0);
}

#[test]
fn lower_expression_samples_time_from_runtime_clock_not_pre_parameter() {
    let mut bindings = IndexMap::new();
    bindings.insert("time".to_string(), ScalarSlot::Time);
    bindings.insert(
        "__pre__.time".to_string(),
        rumoca_ir_solve::scalar_slot_p(0),
    );
    let layout = VarLayout::from_parts(bindings, 0, 1);
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sample,
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("time").into(),
            subscripts: vec![],
            span: test_span(),
        }],
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("sample(time) should lower to the runtime time scalar");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[99.0], 0.2);

    assert_eq!(read_reg(&regs, lowered.result), 0.2);
}

#[test]
fn lower_discrete_rhs_holds_clocked_sample_between_clock_ticks() {
    let mut dae_model = dae::Dae::default();
    for name in ["u", "y"] {
        dae_model
            .variables
            .discrete_reals
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.u"),
        scalar_var("__pre__.u"),
    );
    dae_model.clocks.intervals.insert("clock".to_string(), 0.02);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sample,
            args: vec![
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("u").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("clock").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
            ],
            span: test_span(),
        },
        span: test_span(),
        origin: "y = sample(u, clock)".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("clocked sample value should lower with target hold");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "y", 9.0);
    set_p_value(&layout, &mut p, "__pre__.u", 3.0);

    let (_, held) = eval_linear_ops(&rows[0], &[], &p, 0.01);
    assert_eq!(
        held.expect("row output"),
        9.0,
        "clocked sample must hold target value between source clock ticks"
    );

    let (_, sampled) = eval_linear_ops(&rows[0], &[], &p, 0.02);
    assert_eq!(
        sampled.expect("row output"),
        3.0,
        "clocked sample must sample the source left limit at its clock tick"
    );
}

#[test]
fn lower_discrete_rhs_holds_vector_clocked_sample_elements_between_ticks() {
    let mut dae_model = dae::Dae::default();
    for name in ["u", "y"] {
        dae_model.variables.discrete_reals.insert(
            rumoca_core::VarName::new(name),
            dae::Variable {
                dims: vec![2],
                ..scalar_var(name)
            },
        );
    }
    insert_pre_parameter(&mut dae_model, "u", &[2]);
    dae_model.clocks.intervals.insert("clock".to_string(), 0.02);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sample,
            args: vec![
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("u").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("clock").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
            ],
            span: test_span(),
        },
        span: test_span(),
        // MLS §10.6 and §16.5.1: array-valued sample equations denote
        // element-wise clocked sample equations; each scalar target holds
        // between ticks and samples the corresponding source left limit.
        origin: "y = sample(u, clock)".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("vector clocked sample should lower as scalar sample rows");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "y[1]", 9.0);
    set_p_value(&layout, &mut p, "y[2]", 8.0);
    set_p_value(&layout, &mut p, "__pre__.u[1]", 3.0);
    set_p_value(&layout, &mut p, "__pre__.u[2]", 4.0);

    let (_, held_first) = eval_linear_ops(&rows[0], &[], &p, 0.01);
    let (_, held_second) = eval_linear_ops(&rows[1], &[], &p, 0.01);
    let (_, sampled_first) = eval_linear_ops(&rows[0], &[], &p, 0.02);
    let (_, sampled_second) = eval_linear_ops(&rows[1], &[], &p, 0.02);

    assert_eq!(held_first.expect("first held output"), 9.0);
    assert_eq!(held_second.expect("second held output"), 8.0);
    assert_eq!(sampled_first.expect("first sampled output"), 3.0);
    assert_eq!(sampled_second.expect("second sampled output"), 4.0);
}

#[test]
fn lower_discrete_rhs_samples_internal_vector_current_elements_at_initial_clock_tick() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("u")
        },
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("y")
        },
    );
    insert_pre_parameter(&mut dae_model, "u", &[2]);
    dae_model.clocks.intervals.insert("clock".to_string(), 0.02);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new(rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME).into(),
            args: vec![
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("u").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("clock").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
            ],
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
        origin: "y = __rumoca_sample(u, clock)".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("internal vector clocked sample should lower element-wise");
    let mut y = vec![0.0; layout.y_scalars()];
    let mut p = vec![0.0; layout.p_scalars()];
    set_y_value(&layout, &mut y, "u[1]", 10.0);
    set_y_value(&layout, &mut y, "u[2]", 20.0);
    set_p_value(&layout, &mut p, "__pre__.u[1]", 3.0);
    set_p_value(&layout, &mut p, "__pre__.u[2]", 4.0);
    let Some(ScalarSlot::P {
        index: initial_index,
        ..
    }) = layout.binding(crate::layout::INITIAL_EVENT_PARAMETER_NAME)
    else {
        panic!("initial event flag should be represented in the solve layout");
    };

    p[initial_index] = 1.0;
    let (_, initial_first) = eval_linear_ops(&rows[0], &y, &p, 0.0);
    let (_, initial_second) = eval_linear_ops(&rows[1], &y, &p, 0.0);
    p[initial_index] = 0.0;
    let (_, ordinary_first) = eval_linear_ops(&rows[0], &y, &p, 0.02);
    let (_, ordinary_second) = eval_linear_ops(&rows[1], &y, &p, 0.02);

    assert_eq!(initial_first.expect("first initial output"), 10.0);
    assert_eq!(
        initial_second.expect("second initial output"),
        20.0,
        "internal vector sample must read the current source element at the initial tick"
    );
    assert_eq!(ordinary_first.expect("first ordinary output"), 3.0);
    assert_eq!(
        ordinary_second.expect("second ordinary output"),
        4.0,
        "internal vector sample must read the matching pre source element after initialization"
    );
}

#[test]
fn lower_discrete_rhs_reads_previous_array_from_pre_slots() {
    let mut dae_model = dae::Dae::default();
    for name in ["u", "y"] {
        dae_model.variables.discrete_reals.insert(
            rumoca_core::VarName::new(name),
            dae::Variable {
                dims: vec![2],
                ..scalar_var(name)
            },
        );
    }
    insert_pre_parameter(&mut dae_model, "u", &[2]);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("previous").into(),
            args: vec![rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("u").into(),
                subscripts: vec![],
                span: test_span(),
            }],
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
        origin: "y = previous(u)".to_string(),
        scalar_count: 2,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("array-valued previous() should lower element-wise");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u[1]", 30.0);
    set_p_value(&layout, &mut p, "u[2]", 40.0);
    set_p_value(&layout, &mut p, "__pre__.u[1]", 3.0);
    set_p_value(&layout, &mut p, "__pre__.u[2]", 4.0);

    let (_, first) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, second) = eval_linear_ops(&rows[1], &[], &p, 0.0);

    assert_eq!(
        first.expect("first previous output"),
        3.0,
        "array-valued previous() must read __pre__.u[1], not current u[1]"
    );
    assert_eq!(
        second.expect("second previous output"),
        4.0,
        "array-valued previous() must read __pre__.u[2], not current u[2]"
    );
}

#[test]
fn lower_discrete_rhs_samples_current_value_at_initial_clock_tick() {
    let mut dae_model = dae::Dae::default();
    for name in ["u", "y"] {
        dae_model
            .variables
            .discrete_reals
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.u"),
        scalar_var("__pre__.u"),
    );
    dae_model.clocks.intervals.insert("clock".to_string(), 0.02);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sample,
            args: vec![
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("u").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("clock").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
            ],
            span: test_span(),
        },
        span: test_span(),
        origin: "y = sample(u, clock)".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("clocked sample value should lower with target hold");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u", 4.0);
    set_p_value(&layout, &mut p, "__pre__.u", 3.0);
    let Some(ScalarSlot::P {
        index: initial_index,
        ..
    }) = layout.binding(crate::layout::INITIAL_EVENT_PARAMETER_NAME)
    else {
        panic!("initial event flag should be represented in the solve layout");
    };

    p[initial_index] = 1.0;
    let (_, initial_tick) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    p[initial_index] = 0.0;
    let (_, ordinary_tick) = eval_linear_ops(&rows[0], &[], &p, 0.02);

    assert_eq!(
        initial_tick.expect("initial tick output"),
        4.0,
        "sample(u, clock) must read initialized source value at the initial clock tick"
    );
    assert_eq!(
        ordinary_tick.expect("ordinary tick output"),
        3.0,
        "sample(u, clock) must read the source left limit after initialization"
    );
}

#[test]
fn lower_discrete_rhs_uses_dynamic_clock_var_for_sample_clock() {
    let mut dae_model = dae::Dae::default();
    for name in ["u", "y"] {
        dae_model
            .variables
            .discrete_reals
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("clock"), scalar_var("clock"));
    insert_pre_parameter(&mut dae_model, "u", &[]);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sample,
            args: vec![
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("u").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("clock").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
            ],
            span: test_span(),
        },
        span: test_span(),
        origin: "y = sample(u, clock)".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("sample(value, dynamic clock variable) should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "y", 9.0);
    set_p_value(&layout, &mut p, "__pre__.u", 3.0);

    set_p_value(&layout, &mut p, "clock", 0.0);
    let (_, held) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    set_p_value(&layout, &mut p, "clock", 1.0);
    let (_, sampled) = eval_linear_ops(&rows[0], &[], &p, 0.0);

    assert_eq!(
        held.expect("held output"),
        9.0,
        "sample(u, clock_var) must hold the target when the dynamic clock is false"
    );
    assert_eq!(
        sampled.expect("sampled output"),
        3.0,
        "sample(u, clock_var) must sample the source left limit when the dynamic clock is true"
    );
}

#[test]
fn lower_discrete_rhs_treats_sample_parameter_interval_as_periodic_tick() {
    let mut dae_model = dae::Dae::default();
    for name in ["startTime", "period"] {
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sample,
            args: vec![
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("startTime").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("period").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
            ],
            span: test_span(),
        },
        span: test_span(),
        origin: "y = sample(startTime, period)".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("MLS §16.5.1 sample(start, interval) should lower with parameter interval");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "startTime", 0.3);
    set_p_value(&layout, &mut p, "period", 0.5);

    let (_, before_start) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, at_start) = eval_linear_ops(&rows[0], &[], &p, 0.3);

    assert_eq!(
        before_start.expect("periodic sample output before start"),
        0.0,
        "sample(startTime, period) must not be interpreted as sample(value, clock_var)"
    );
    assert_eq!(
        at_start.expect("periodic sample output at start"),
        1.0,
        "sample(startTime, period) must tick at startTime"
    );
}

#[test]
fn lower_expression_supports_terminal_builtin_as_runtime_false() {
    let layout = VarLayout::default();
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Terminal,
        args: vec![],
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("MLS §8.6 terminal() should lower for ordinary simulation rows");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);

    assert_eq!(read_reg(&regs, lowered.result), 0.0);
}

#[test]
fn lower_expression_maps_capitalized_integer_intrinsic_function_call() {
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Integer").into(),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(3.7),
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("Integer should lower as intrinsic");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() < 1e-12);
}

#[test]
fn lower_expression_maps_strings_length_runtime_special() {
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Modelica.Utilities.Strings.length").into(),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("hello".to_string()),
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("Modelica.Utilities.Strings.length should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 5.0).abs() < 1e-12);
}

#[test]
fn lower_expression_does_not_fold_vector_length_as_string_length() {
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Modelica.Math.Vectors.length").into(),
        args: vec![rumoca_core::Expression::Array {
            elements: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(3.0),
                    span: test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(4.0),
                    span: test_span(),
                },
            ],
            is_matrix: false,
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };

    assert!(
        lower_expression(&expr, &VarLayout::default(), &IndexMap::new()).is_err(),
        "numeric vector length must be lowered through the Modelica function body, not string-special folded to 0.0"
    );
}

#[test]
fn lower_expression_maps_strings_hash_string_runtime_special() {
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Modelica.Utilities.Strings.hashString").into(),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("Controller.noise1".to_string()),
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("Modelica.Utilities.Strings.hashString should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert_eq!(read_reg(&regs, lowered.result), -1_025_762_750.0);
}

#[test]
fn lower_expression_maps_random_automatic_seed_runtime_specials() {
    let global = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Modelica.Math.Random.Utilities.automaticGlobalSeed")
            .into(),
        args: vec![],
        is_constructor: false,
        span: test_span(),
    };
    let lowered = lower_expression(&global, &VarLayout::default(), &IndexMap::new())
        .expect("automaticGlobalSeed should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!(read_reg(&regs, lowered.result).is_finite());

    let local = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Modelica.Math.Random.Utilities.automaticLocalSeed").into(),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("Controller.noise1".to_string()),
            span: rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name(
                    "phase_solve_lower_tests_intrinsics_source_92.mo",
                ),
                10,
                29,
            ),
        }],
        is_constructor: false,
        span: test_span(),
    };
    let lowered = lower_expression(&local, &VarLayout::default(), &IndexMap::new())
        .expect("automaticLocalSeed should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert_eq!(read_reg(&regs, lowered.result), -1_025_762_750.0);
}

#[test]
fn lower_expression_rejects_unlowered_full_path_name_runtime_special() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_intrinsics_source_91.mo"),
        10,
        40,
    );
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Modelica.Utilities.Files.fullPathName").into(),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("a.txt".to_string()),
            span: test_span(),
        }],
        is_constructor: false,
        span,
    };
    let err = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect_err("unlowered fullPathName should be rejected");
    assert_eq!(err.source_span(), Some(span));
}

#[test]
fn lower_expression_rejects_unlowered_streams_read_line_runtime_special() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("phase_solve_lower_tests_intrinsics_source_91.mo"),
        50,
        90,
    );
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Modelica.Utilities.Streams.readLine").into(),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::String("memory.txt".to_string()),
            span: test_span(),
        }],
        is_constructor: false,
        span,
    };
    let err = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect_err("unlowered readLine should be rejected");
    assert_eq!(err.source_span(), Some(span));
}

#[test]
fn lower_expression_maps_stream_connector_intrinsics_to_source_value() {
    let mut bindings = IndexMap::new();
    bindings.insert("port.h_outflow".to_string(), ScalarSlot::Constant(123.0));
    let layout = VarLayout::from_parts(bindings, 0, 0);
    for name in ["inStream", "actualStream"] {
        let expr = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new(name).into(),
            args: vec![rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("port.h_outflow").into(),
                subscripts: vec![],
                span: test_span(),
            }],
            is_constructor: false,
            span: test_span(),
        };
        let lowered = lower_expression(&expr, &layout, &IndexMap::new())
            .expect("stream connector intrinsic should lower");
        let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
        assert_eq!(read_reg(&regs, lowered.result), 123.0);
    }
}

#[test]
fn lower_expression_rejects_unlowered_advanced_string_scanner_runtime_specials() {
    for name in [
        "Modelica.Utilities.Strings.Advanced.scanInteger",
        "Modelica.Utilities.Strings.Advanced.skipWhiteSpace",
    ] {
        let expr = rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new(name).into(),
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::String("1".to_string()),
                    span: test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: test_span(),
                },
            ],
            is_constructor: false,
            span: test_span(),
        };
        let err = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
            .expect_err("unlowered scanner intrinsic should be rejected");
        assert!(err.reason().contains("must be lowered"), "{name}: {err}");
    }
}

#[test]
fn lower_expression_prefers_resolved_string_function_body_over_intrinsic_name() {
    let mut functions = IndexMap::new();
    let mut find_last = test_function("Modelica.Utilities.Strings.findLast", test_span());
    find_last.inputs.push(function_param("u"));
    find_last.outputs.push(function_param("index"));
    find_last.body.push(rumoca_core::Statement::Assignment {
        comp: component_ref("index"),
        value: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(var("u")),
            rhs: Box::new(var("u")),
            span: test_span(),
        },
        span: test_span(),
    });
    functions.insert(
        rumoca_core::VarName::new("Modelica.Utilities.Strings.findLast"),
        find_last,
    );

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Modelica.Utilities.Strings.findLast").into(),
        args: vec![real_lit(3.0)],
        is_constructor: false,
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &functions)
        .expect("resolved function body should take precedence over its intrinsic-like name");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert_eq!(read_reg(&regs, lowered.result), 9.0);
}

#[test]
fn lower_expression_supports_edge_of_sample_start_interval_tick() {
    let layout = VarLayout::default();
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Edge,
        args: vec![rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Sample,
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.2),
                    span: test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(0.1),
                    span: test_span(),
                },
            ],

            span: test_span(),
        }],
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("MLS §8.6 and §16.5.1 edge(sample(...)) should lower");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.3);
    assert_eq!(read_reg(&regs, lowered.result), 1.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.35);
    assert_eq!(read_reg(&regs, lowered.result), 0.0);
}

#[test]
fn lower_expression_supports_clock_constructor_tick() {
    let layout = VarLayout::default();
    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Clock").into(),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.25),
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new())
        .expect("MLS §16.5.1 Clock(period) should lower");

    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.5);
    assert_eq!(read_reg(&regs, lowered.result), 1.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.6);
    assert_eq!(read_reg(&regs, lowered.result), 0.0);
}

#[test]
fn lower_expression_supports_synchronous_value_intrinsics() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");

    let previous = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("previous").into(),
        args: vec![rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("u").into(),
            subscripts: vec![],
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };
    let lowered = lower_expression(&previous, &layout, &IndexMap::new())
        .expect("MLS §16.4 previous(v) should lower through solve-IR pre values");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[7.0], 0.0);
    assert_eq!(read_reg(&regs, lowered.result), 7.0);

    let hold = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("hold").into(),
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(3.0),
            span: test_span(),
        }],
        is_constructor: false,
        span: test_span(),
    };
    let lowered = lower_expression(&hold, &layout, &IndexMap::new())
        .expect("MLS §16.5.1 hold(v) should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert_eq!(read_reg(&regs, lowered.result), 3.0);

    let first_tick = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("firstTick").into(),
        args: vec![],
        is_constructor: false,
        span: test_span(),
    };
    let lowered = lower_expression(&first_tick, &layout, &IndexMap::new())
        .expect("MLS §16.5.1 firstTick() should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert_eq!(read_reg(&regs, lowered.result), 1.0);
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.1);
    assert_eq!(read_reg(&regs, lowered.result), 0.0);
}

#[test]
fn lower_discrete_rhs_supports_interval_intrinsic_for_clocked_varref_metadata() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("PI.u"), scalar_var("PI.u"));
    dae_model.clocks.intervals.insert("PI.u".to_string(), 0.1);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("PI.Ts").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("interval").into(),
            args: vec![rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("PI.u").into(),
                subscripts: vec![],
                span: test_span(),
            }],
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
        // MLS §16.5.1: interval(v) uses the associated clock interval of v.
        origin: "test interval metadata".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("interval(varref) should lower");
    let (_, output) = eval_linear_ops(&rows[0], &[], &[], 0.0);

    assert!((output.expect("row output") - 0.1).abs() < 1e-12);
}

#[test]
fn lower_discrete_rhs_infers_interval_as_scalar_inside_arithmetic() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("pulse.u"), scalar_var("pulse.u"));
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("pulse.tol"),
        scalar_var("pulse.tol"),
    );
    dae_model
        .clocks
        .intervals
        .insert("pulse.u".to_string(), 0.1);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("pulse.tol").into()),
        rhs: rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(0.25),
                span: test_span(),
            }),
            rhs: Box::new(rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("interval").into(),
                args: vec![rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("pulse.u").into(),
                    subscripts: vec![],
                    span: test_span(),
                }],
                is_constructor: false,
                span: test_span(),
            }),
            span: test_span(),
        },
        span: test_span(),
        origin: "test scalar interval arithmetic".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("interval arithmetic should lower");
    let (_, output) = eval_linear_ops(&rows[0], &[], &[], 0.0);
    assert!((output.expect("row output") - 0.025).abs() < 1e-12);
}

#[test]
fn lower_discrete_rhs_preserves_super_sample_value_form_for_clocked_varref() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.clocks.intervals.insert("u".to_string(), 0.02);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("superSample").into(),
            args: vec![
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("u").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(2),
                    span: test_span(),
                },
            ],
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
        origin: "y = superSample(u, 2)".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("value-form superSample(var, factor) should lower as value");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u", 4.0);
    let (_, output) = eval_linear_ops(&rows[0], &[], &p, 0.0);

    assert_eq!(
        output.expect("row output"),
        4.0,
        "value-form superSample must preserve u, not emit a clock-tick boolean"
    );
}

#[test]
fn lower_discrete_rhs_uses_target_timing_for_inferred_clock_constructor() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.clocks.timings.insert(
        "y".to_string(),
        dae::ClockSchedule {
            period_seconds: 0.02 / 3.0,
            phase_seconds: 0.0,
            source_span: test_span(),
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::If {
            branches: vec![(
                rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Edge,
                    args: vec![rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::VarName::new("Clock").into(),
                        args: vec![],
                        is_constructor: false,
                        span: test_span(),
                    }],
                    span: test_span(),
                },
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("u").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
            )],
            else_branch: Box::new(rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("y").into(),
                subscripts: vec![],
                span: test_span(),
            }),
            span: test_span(),
        },
        span: test_span(),
        origin: "when Clock() then y = u".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("inferred Clock() should use target timing metadata");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u", 4.0);
    set_p_value(&layout, &mut p, "y", 9.0);

    let (_, target_tick_output) = eval_linear_ops(&rows[0], &[], &p, 0.02 / 3.0);
    let (_, unrelated_event_output) = eval_linear_ops(&rows[0], &[], &p, 0.01);

    assert_eq!(
        target_tick_output.expect("target-tick row output"),
        4.0,
        "no-argument Clock() should tick on the row target's inferred clock"
    );
    assert_eq!(
        unrelated_event_output.expect("unrelated-event row output"),
        9.0,
        "no-argument Clock() should hold the target away from its inferred clock tick"
    );
}

#[test]
fn lower_discrete_rhs_uses_triggered_condition_for_event_clock_constructor() {
    let mut dae_model = dae::Dae::default();
    for name in ["u", "tick"] {
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new(name), scalar_var(name));
    }
    let condition = rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new("u").into(),
        subscripts: vec![],
        span: test_span(),
    };
    dae_model
        .clocks
        .triggered_conditions
        .push(condition.clone());
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("tick").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Clock").into(),
            args: vec![condition],
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
        origin: "tick = Clock(u)".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("event Clock() should lower");
    let mut p = vec![0.0; layout.p_scalars()];

    set_p_value(&layout, &mut p, "u", 0.0);
    let (_, false_tick) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    set_p_value(&layout, &mut p, "u", 1.0);
    let (_, true_tick) = eval_linear_ops(&rows[0], &[], &p, 0.0);

    assert_eq!(
        false_tick.expect("false condition output"),
        0.0,
        "Clock(condition) must not be treated as a periodic tick at t=0"
    );
    assert_eq!(true_tick.expect("true condition output"), 1.0);
}
