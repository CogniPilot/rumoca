use super::*;
use crate::LowerError;

fn spanned_real_lit(value: f64) -> rumoca_core::Expression {
    rumoca_core::Expression::Literal {
        value: rumoca_core::Literal::Real(value),
        span: test_span(),
    }
}

#[test]
fn lower_discrete_rhs_holds_shift_sample_value_until_target_tick() {
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
            name: rumoca_core::VarName::new("shiftSample").into(),
            args: vec![
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("u").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(3),
                    span: test_span(),
                },
            ],
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
        origin: "y = shiftSample(u, 1, 3)".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("value-form shiftSample(var, factor, resolution) should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u", 4.0);
    set_p_value(&layout, &mut p, "y", 9.0);

    let (_, source_tick_output) = eval_linear_ops(&rows[0], &[], &p, 0.02);
    let (_, shifted_tick_output) = eval_linear_ops(&rows[0], &[], &p, 0.02 + (0.02 / 3.0));

    assert_eq!(
        source_tick_output.expect("source tick row output"),
        9.0,
        "value-form shiftSample must hold y at the source clock tick"
    );
    assert_eq!(
        shifted_tick_output.expect("shifted tick row output"),
        4.0,
        "value-form shiftSample must update y at the shifted target clock tick"
    );
}

#[test]
fn lower_discrete_rhs_uses_base_clock_for_indexed_shift_sample_value_form() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable {
            dims: vec![2],
            ..scalar_var("u")
        },
    );
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model.clocks.intervals.insert("u".to_string(), 0.02);
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("shiftSample").into(),
            args: vec![
                rumoca_core::Expression::Index {
                    base: Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("u").into(),
                        subscripts: vec![],
                        span: test_span(),
                    }),
                    subscripts: vec![rumoca_core::Subscript::Index {
                        value: 2,
                        span: test_span(),
                    }],
                    span: test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(3),
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
        origin: "y = shiftSample(u[2], 3, 2)".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("indexed value-form shiftSample should lower from base clock metadata");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u[2]", 4.0);
    set_p_value(&layout, &mut p, "y", 9.0);

    let (_, source_tick_output) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, shifted_tick_output) = eval_linear_ops(&rows[0], &[], &p, 0.03);

    assert_eq!(
        source_tick_output.expect("source tick row output"),
        9.0,
        "indexed value-form shiftSample must hold y before the shifted target tick"
    );
    assert_eq!(
        shifted_tick_output.expect("shifted tick row output"),
        4.0,
        "indexed value-form shiftSample must use the indexed source at the shifted target tick"
    );
}

#[test]
fn lower_discrete_rhs_uses_source_phase_for_back_sample_value_form() {
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
    dae_model.clocks.timings.insert(
        "u".to_string(),
        dae::ClockSchedule {
            period_seconds: 0.02,
            phase_seconds: (4.0 / 3.0) * 0.02,
            source_span: test_span(),
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("backSample").into(),
            args: vec![
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("u").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(4),
                    span: test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(3),
                    span: test_span(),
                },
            ],
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
        origin: "y = backSample(u, 4, 3)".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("value-form backSample(var, factor, resolution) should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u", 4.0);
    set_p_value(&layout, &mut p, "y", 9.0);

    let (_, target_tick_output) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, source_tick_output) = eval_linear_ops(&rows[0], &[], &p, (4.0 / 3.0) * 0.02);

    assert_eq!(
        target_tick_output.expect("target tick row output"),
        4.0,
        "backSample must update on the phase-shifted target tick, not only phase-zero sources"
    );
    assert_eq!(
        source_tick_output.expect("source tick row output"),
        9.0,
        "backSample must hold y at the input source tick"
    );
}

#[test]
fn lower_discrete_rhs_uses_back_sample_source_start_before_first_source_tick() {
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
    dae_model.clocks.timings.insert(
        "u".to_string(),
        dae::ClockSchedule {
            period_seconds: 0.02,
            phase_seconds: (4.0 / 3.0) * 0.02,
            source_span: test_span(),
        },
    );
    dae_model.metadata.variable_starts.insert(
        "u".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: test_span(),
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("backSample").into(),
            args: vec![
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("u").into(),
                    subscripts: vec![],
                    span: test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(4),
                    span: test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(3),
                    span: test_span(),
                },
            ],
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
        origin: "y = backSample(u, 4, 3)".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("value-form backSample(var, factor, resolution) should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u", 4.0);
    set_p_value(&layout, &mut p, "y", 9.0);

    let (_, first_target_tick_output) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, later_target_tick_output) = eval_linear_ops(&rows[0], &[], &p, 0.08);

    assert_eq!(
        first_target_tick_output.expect("first target tick row output"),
        1.0,
        "backSample must use the input start value before the first input clock tick"
    );
    assert_eq!(
        later_target_tick_output.expect("later target tick row output"),
        4.0,
        "backSample must use the current held source value after the first input clock tick"
    );
}

#[test]
fn lower_discrete_rhs_uses_hold_source_start_before_first_source_tick() {
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
        "u".to_string(),
        dae::ClockSchedule {
            period_seconds: 0.02,
            phase_seconds: 0.04,
            source_span: test_span(),
        },
    );
    dae_model.metadata.variable_starts.insert(
        "u".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(-1.0),
            span: test_span(),
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("hold").into(),
            args: vec![rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("u").into(),
                subscripts: vec![],
                span: test_span(),
            }],
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
        origin: "y = hold(u)".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("hold(var) should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u", 2.0);

    let (_, startup_output) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, first_source_tick_output) = eval_linear_ops(&rows[0], &[], &p, 0.04);

    assert_eq!(
        startup_output.expect("startup row output"),
        -1.0,
        "hold must use the source start value before the first source clock tick"
    );
    assert_eq!(
        first_source_tick_output.expect("first source tick row output"),
        2.0,
        "hold must expose the held source value once the source clock has ticked"
    );
}

#[test]
fn lower_discrete_rhs_uses_hold_start_during_initial_event_at_first_tick() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .variables
        .discrete_reals
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model
        .metadata
        .variable_starts
        .insert("u".to_string(), real_lit(-1.0));
    dae_model.clocks.timings.insert(
        "u".to_string(),
        dae::ClockSchedule {
            period_seconds: 0.02,
            phase_seconds: 0.0,
            source_span: test_span(),
        },
    );
    dae_model.discrete.real_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("y").into()),
        rhs: rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("hold").into(),
            args: vec![rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("u").into(),
                subscripts: vec![],
                span: test_span(),
            }],
            is_constructor: false,
            span: test_span(),
        },
        span: test_span(),
        origin: "y = hold(u)".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout).expect("hold(var) should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u", 2.0);
    set_p_value(&layout, &mut p, crate::INITIAL_EVENT_PARAMETER_NAME, 1.0);

    let (_, initial_tick_output) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    set_p_value(&layout, &mut p, crate::INITIAL_EVENT_PARAMETER_NAME, 0.0);
    let (_, post_initial_tick_output) = eval_linear_ops(&rows[0], &[], &p, 0.0);

    assert_eq!(
        initial_tick_output.expect("initial tick row output"),
        -1.0,
        "hold must expose the held start value while initial() is true"
    );
    assert_eq!(
        post_initial_tick_output.expect("post-initial tick row output"),
        2.0,
        "hold must expose the current held source value after initialization"
    );
}

#[test]
fn lower_runtime_assignment_keeps_clocked_target_start_before_first_tick() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("source"), scalar_var("source"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("target"), scalar_var("target"));
    dae_model.clocks.timings.insert(
        "target".to_string(),
        dae::ClockSchedule {
            period_seconds: 0.02,
            phase_seconds: 0.04,
            source_span: test_span(),
        },
    );
    dae_model.metadata.variable_starts.insert(
        "target".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(-1),
            span: test_span(),
        },
    );
    let equation = dae::Equation {
        lhs: Some(rumoca_core::VarName::new("target").into()),
        rhs: rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("source").into(),
            subscripts: vec![],
            span: test_span(),
        },
        span: test_span(),
        origin: "target = source".to_string(),
        scalar_count: 1,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_runtime_assignment_rhs(&dae_model, &layout, &[equation])
        .expect("clocked runtime alias should lower");
    let y = [2.0];

    let (_, before_first_tick) = eval_linear_ops(&rows[0], &y, &[], 0.0);
    let (_, at_first_tick) = eval_linear_ops(&rows[0], &y, &[], 0.04);

    assert_eq!(
        before_first_tick.expect("runtime assignment output before first tick"),
        -1.0,
        "clocked runtime-tail aliases must expose the target start before the first target tick"
    );
    assert_eq!(
        at_first_tick.expect("runtime assignment output at first tick"),
        2.0,
        "clocked runtime-tail aliases must follow the connected source at the first target tick"
    );
}

#[test]
fn lower_runtime_assignment_inherits_rhs_clock_for_target_start_guard() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("source"), scalar_var("source"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("target"), scalar_var("target"));
    dae_model.clocks.timings.insert(
        "source".to_string(),
        dae::ClockSchedule {
            period_seconds: 0.02,
            phase_seconds: 0.04,
            source_span: test_span(),
        },
    );
    dae_model.metadata.variable_starts.insert(
        "target".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(-1),
            span: test_span(),
        },
    );
    let equation = dae::Equation {
        lhs: Some(rumoca_core::VarName::new("target").into()),
        rhs: rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("source").into(),
            subscripts: vec![],
            span: test_span(),
        },
        span: test_span(),
        origin: "target = source".to_string(),
        scalar_count: 1,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_runtime_assignment_rhs(&dae_model, &layout, &[equation])
        .expect("clocked runtime alias should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "source", 2.0);

    let (_, before_first_source_tick) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, at_first_source_tick) = eval_linear_ops(&rows[0], &[], &p, 0.04);

    assert_eq!(
        before_first_source_tick.expect("runtime assignment output before first source tick"),
        -1.0,
        "clocked runtime-tail aliases must use the target start before the inherited source clock ticks"
    );
    assert_eq!(
        at_first_source_tick.expect("runtime assignment output at first source tick"),
        2.0,
        "clocked runtime-tail aliases must follow the source at the inherited source clock tick"
    );
}

#[test]
fn lower_runtime_assignment_uses_equation_span_for_unspanned_start_guard_rhs() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("source"), scalar_var("source"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("target"), scalar_var("target"));
    dae_model.clocks.timings.insert(
        "target".to_string(),
        dae::ClockSchedule {
            period_seconds: 0.02,
            phase_seconds: 0.04,
            source_span: test_span(),
        },
    );
    dae_model.metadata.variable_starts.insert(
        "target".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(-1),
            span: test_span(),
        },
    );
    let equation_span = test_span();
    let equation = dae::Equation {
        lhs: Some(rumoca_core::VarName::new("target").into()),
        rhs: rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("source").into(),
            subscripts: vec![],
            span: unspanned_test_span(),
        },
        span: equation_span,
        origin: "target = source".to_string(),
        scalar_count: 1,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_runtime_assignment_rhs(&dae_model, &layout, &[equation])
        .expect("equation span should provide current-update guard provenance");
    let (_, before_first_tick) = eval_linear_ops(&rows[0], &[2.0], &[], 0.0);

    assert_eq!(
        before_first_tick.expect("runtime assignment output before first tick"),
        -1.0,
        "equation provenance must be enough for generated start guard rows"
    );
}

#[test]
fn lower_runtime_assignment_rejects_unspanned_start_guard_context() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .outputs
        .insert(rumoca_core::VarName::new("source"), scalar_var("source"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("target"), scalar_var("target"));
    dae_model.clocks.timings.insert(
        "target".to_string(),
        dae::ClockSchedule {
            period_seconds: 0.02,
            phase_seconds: 0.04,
            source_span: test_span(),
        },
    );
    dae_model.metadata.variable_starts.insert(
        "target".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(-1),
            span: test_span(),
        },
    );
    let equation = dae::Equation {
        lhs: Some(rumoca_core::VarName::new("target").into()),
        rhs: rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("source").into(),
            subscripts: vec![],
            span: unspanned_test_span(),
        },
        span: unspanned_test_span(),
        origin: "target = source".to_string(),
        scalar_count: 1,
    };

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let err = lower_runtime_assignment_rhs(&dae_model, &layout, &[equation])
        .expect_err("unspanned current-update guard context must fail");

    assert!(
        matches!(err, LowerError::UnspannedContractViolation { .. }),
        "unspanned guarded row should not fabricate a dummy span: {err:?}"
    );
    assert!(
        err.reason()
            .contains("current update start guard requires source span"),
        "error should identify the missing current-update provenance: {err}"
    );
}

#[test]
fn lower_discrete_rhs_keeps_clocked_alias_target_start_before_first_target_tick() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("source"), scalar_var("source"));
    dae_model
        .variables
        .discrete_valued
        .insert(rumoca_core::VarName::new("target"), scalar_var("target"));
    dae_model.clocks.timings.insert(
        "target".to_string(),
        dae::ClockSchedule {
            period_seconds: 0.02,
            phase_seconds: 0.04,
            source_span: test_span(),
        },
    );
    dae_model.metadata.variable_starts.insert(
        "target".to_string(),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(-1),
            span: test_span(),
        },
    );
    dae_model.discrete.valued_updates.push(dae::Equation {
        lhs: Some(rumoca_core::VarName::new("source").into()),
        rhs: rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new("target").into(),
            subscripts: vec![],
            span: test_span(),
        },
        span: test_span(),
        origin: "connection equation: source = target".to_string(),
        scalar_count: 1,
    });

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let rows = lower_discrete_rhs(&dae_model, &layout)
        .expect("clocked discrete alias assignment should lower");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "source", 2.0);
    set_p_value(&layout, &mut p, "target", -1.0);

    let (_, before_first_tick) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    let (_, at_first_tick) = eval_linear_ops(&rows[0], &[], &p, 0.04);

    assert_eq!(
        before_first_tick.expect("alias output before first target tick"),
        -1.0,
        "clocked alias targets must keep their start value before their first tick"
    );
    assert_eq!(
        at_first_tick.expect("alias output at first target tick"),
        2.0,
        "clocked alias targets must follow the source at their first tick"
    );
}

#[test]
fn lower_expression_supports_size_builtin_for_known_array_dims() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("A"),
        source_array_var("A", &[2, 3]),
    );
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Size,
        args: vec![
            rumoca_core::Expression::VarRef {
                name: rumoca_core::VarName::new("A").into(),
                subscripts: vec![],
                span: test_span(),
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(2),
                span: test_span(),
            },
        ],
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new()).expect("size should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[0.0; 6], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 3.0).abs() < 1e-12);
}

#[test]
fn lower_function_local_structural_symmetric_orientation() {
    let mut dae_model = dae::Dae::default();
    let mut function = test_function("Space.phases", test_span());
    function.inputs.push(function_param_with_dims("x", &[3]));
    function.outputs.push(function_param_with_dims("y", &[3]));
    function.locals.push(rumoca_core::FunctionParam {
        def_id: None,
        type_def_id: None,
        name: "m".to_string(),
        span: test_span(),
        type_name: "Integer".to_string(),
        type_class: None,
        dims: Vec::new(),
        shape_expr: Vec::new(),
        default: Some(rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Size,
            args: vec![
                source_var("x"),
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Integer(1),
                    span: test_span(),
                },
            ],
            span: test_span(),
        }),
        min: None,
        max: None,
        description: None,
    });
    function.locals.push(rumoca_core::FunctionParam {
        def_id: None,
        type_def_id: None,
        name: "phi".to_string(),
        span: test_span(),
        type_name: "Real".to_string(),
        type_class: None,
        dims: vec![0],
        shape_expr: vec![rumoca_core::Subscript::Expr {
            expr: Box::new(source_var("m")),
            span: test_span(),
        }],
        default: Some(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new(
                "Modelica.Electrical.Polyphase.Functions.symmetricOrientation",
            )
            .into(),
            args: vec![source_var("m")],
            is_constructor: false,
            span: test_span(),
        }),
        min: None,
        max: None,
        description: None,
    });
    function.body = vec![
        rumoca_core::Statement::Assignment {
            comp: component_ref_index("y", 1),
            value: var_index("phi", 1),
            span: test_span(),
        },
        rumoca_core::Statement::Assignment {
            comp: component_ref_index("y", 2),
            value: var_index("phi", 2),
            span: test_span(),
        },
        rumoca_core::Statement::Assignment {
            comp: component_ref_index("y", 3),
            value: var_index("phi", 3),
            span: test_span(),
        },
    ];
    dae_model
        .symbols
        .functions
        .insert(function.name.clone(), function);

    let expr = rumoca_core::Expression::Index {
        base: Box::new(rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new("Space.phases").into(),
            args: vec![rumoca_core::Expression::Array {
                elements: vec![
                    spanned_real_lit(0.0),
                    spanned_real_lit(0.0),
                    spanned_real_lit(0.0),
                ],
                is_matrix: false,
                span: test_span(),
            }],
            is_constructor: false,
            span: test_span(),
        }),
        subscripts: vec![rumoca_core::Subscript::generated_index(2, test_span())],
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &dae_model.symbols.functions)
        .expect("function-local symmetricOrientation should lower structurally");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    let expected = 2.0 * std::f64::consts::PI / 3.0;
    assert!((read_reg(&regs, lowered.result) - expected).abs() < 1e-12);
}

#[test]
fn lower_expression_supports_sum_builtin_for_range() {
    let layout = VarLayout::default();
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Sum,
        args: vec![rumoca_core::Expression::Range {
            start: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: test_span(),
            }),
            step: None,
            end: Box::new(rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(4),
                span: test_span(),
            }),
            span: test_span(),
        }],
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &IndexMap::new()).expect("sum(range) lowers");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 10.0).abs() < 1e-12);
}

#[test]
fn lower_expression_supports_scalar_array_builtins() {
    let layout = VarLayout::default();

    let zeros = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Zeros,
        args: vec![rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(3),
            span: test_span(),
        }],
        span: test_span(),
    };
    let lowered = lower_expression(&zeros, &layout, &IndexMap::new()).expect("zeros lowers");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!(read_reg(&regs, lowered.result).abs() < 1e-12);

    let fill = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Fill,
        args: vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.5),
                span: test_span(),
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(4),
                span: test_span(),
            },
        ],
        span: test_span(),
    };
    let lowered = lower_expression(&fill, &layout, &IndexMap::new()).expect("fill lowers");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 2.5).abs() < 1e-12);

    let fill_index = rumoca_core::Expression::Index {
        base: Box::new(fill),
        subscripts: vec![rumoca_core::Subscript::generated_index(3, test_span())],
        span: test_span(),
    };
    let lowered =
        lower_expression(&fill_index, &layout, &IndexMap::new()).expect("fill index lowers");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 2.5).abs() < 1e-12);

    let cat = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Cat,
        args: vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: test_span(),
            },
            rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(7.0),
                        span: test_span(),
                    },
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(8.0),
                        span: test_span(),
                    },
                ],
                is_matrix: false,
                span: test_span(),
            },
        ],
        span: test_span(),
    };
    let lowered = lower_expression(&cat, &layout, &IndexMap::new()).expect("cat lowers");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert!((read_reg(&regs, lowered.result) - 7.0).abs() < 1e-12);
}

#[test]
fn lower_expression_supports_mod_and_rem_builtins() {
    let layout = VarLayout::default();
    for (function, expected) in [
        (rumoca_core::BuiltinFunction::Mod, 0.5),
        (rumoca_core::BuiltinFunction::Rem, -1.5),
    ] {
        let expr = rumoca_core::Expression::BuiltinCall {
            function,
            args: vec![
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(-5.5),
                    span: test_span(),
                },
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(2.0),
                    span: test_span(),
                },
            ],
            span: test_span(),
        };
        let lowered =
            lower_expression(&expr, &layout, &IndexMap::new()).expect("mod/rem should lower");
        let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
        let compiled = read_reg(&regs, lowered.result);
        assert!(
            (compiled - expected).abs() < 1e-12,
            "builtin {} mismatch: compiled={compiled}, expected={expected}",
            function.name()
        );
    }
}

#[test]
fn lower_expression_reports_builtin_arity_errors_with_call_span() {
    let layout = VarLayout::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_tests_intrinsics_clocked_initial_tests_source_93.mo",
        ),
        12,
        28,
    );
    for function in [
        rumoca_core::BuiltinFunction::Div,
        rumoca_core::BuiltinFunction::Mod,
        rumoca_core::BuiltinFunction::Rem,
        rumoca_core::BuiltinFunction::Ndims,
    ] {
        let expr = rumoca_core::Expression::BuiltinCall {
            function,
            args: vec![],
            span,
        };
        let err = lower_expression(&expr, &layout, &IndexMap::new())
            .expect_err("malformed builtin call should report a contract error");
        assert_eq!(
            err.source_span(),
            Some(span),
            "{} arity error should carry the call span",
            function.name()
        );
        assert!(
            err.reason().contains("requires"),
            "{} arity error should describe the missing argument contract: {}",
            function.name(),
            err.reason()
        );
    }

    let ndims_with_extra_arg = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Ndims,
        args: vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(1.0),
                span,
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span,
            },
        ],
        span,
    };
    let err = lower_expression(&ndims_with_extra_arg, &layout, &IndexMap::new())
        .expect_err("ndims with extra arguments should report a contract error");
    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason().contains("got 2"),
        "ndims extra-argument error should report actual arity: {}",
        err.reason()
    );
}

#[test]
fn lower_expression_reports_invalid_range_dimension_with_span() {
    let layout = VarLayout::default();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name(
            "phase_solve_lower_tests_intrinsics_clocked_initial_tests_source_94.mo",
        ),
        32,
        41,
    );
    let range = rumoca_core::Expression::Range {
        start: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span,
        }),
        step: Some(Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span,
        })),
        end: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(3),
            span,
        }),
        span,
    };
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Ndims,
        args: vec![range],
        span,
    };

    let err = lower_expression(&expr, &layout, &IndexMap::new())
        .expect_err("invalid range dimension should report a lower error");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.reason().contains("invalid static range expression"),
        "invalid range dimension should describe the rejected range: {}",
        err.reason()
    );
}

#[test]
fn lower_initial_residual_treats_initial_builtin_as_true() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Initial,
            args: vec![],
            span: test_span(),
        },
        test_span(),
        "initial() row",
    ));
    let rows =
        lower_initial_residual(&dae_model, &VarLayout::default()).expect("initial residual lowers");
    assert_eq!(rows.len(), 1);
    let (regs, out) = eval_linear_ops(&rows[0], &[0.0], &[], 0.0);
    assert_eq!(out.expect("output"), 1.0);
    assert_eq!(read_reg(&regs, 0), 1.0);
}

#[test]
fn lower_initial_residual_uses_simplified_homotopy_expression() {
    let expr = rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Homotopy,
        args: vec![
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(10.0),
                span: test_span(),
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.0),
                span: test_span(),
            },
        ],
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &VarLayout::default(), &IndexMap::new())
        .expect("ordinary homotopy expression should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &[], 0.0);
    assert_eq!(read_reg(&regs, lowered.result), 10.0);

    let mut dae_model = dae::Dae::default();
    dae_model.continuous.equations.push(dae::Equation::residual(
        expr,
        test_span(),
        "homotopy initialization row",
    ));
    let rows = lower_initial_residual(&dae_model, &VarLayout::default())
        .expect("initial homotopy residual should lower");
    let (_, out) = eval_linear_ops(&rows[0], &[], &[], 0.0);

    assert_eq!(out.expect("initial residual"), 2.0);
}

#[test]
fn lower_initial_residual_includes_dae_initialization_equations() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::explicit(
            source_ref("x"),
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(2.5),
                span: test_span(),
            },
            test_span(),
            "fixed start initialization for x",
        ));
    let layout = crate::build_var_layout(&dae_model).expect("test DAE layout should build");

    let rows = lower_initial_residual(&dae_model, &layout)
        .expect("initial residual should include DAE initialization equations");

    assert_eq!(rows.len(), 1);
    let (regs, out) = eval_linear_ops(&rows[0], &[3.0], &[], 0.0);
    assert_eq!(out.expect("output"), 0.5);
    assert_eq!(read_reg(&regs, 0), 3.0);
}

#[test]
fn lower_initial_residual_does_not_treat_initial_rows_as_derivative_rows() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.states.insert(
        rumoca_core::VarName::new("x"),
        dae::Variable {
            dims: vec![2],
            ..source_scalar_var("x")
        },
    );
    dae_model
        .continuous
        .equations
        .push(dae::Equation::residual_array(
            rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(0.0),
                        span: test_span(),
                    },
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(0.0),
                        span: test_span(),
                    },
                ],
                is_matrix: false,
                span: test_span(),
            },
            test_span(),
            "two scalar state residuals",
            2,
        ));
    dae_model
        .initialization
        .equations
        .push(dae::Equation::explicit_with_scalar_count(
            rumoca_core::Reference::from_component_reference(source_component_ref_from_name("x")),
            rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(1.0),
                        span: test_span(),
                    },
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(2.0),
                        span: test_span(),
                    },
                ],
                is_matrix: false,
                span: test_span(),
            },
            test_span(),
            "fixed start initialization for x",
            2,
        ));
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");

    let rows = lower_initial_residual(&dae_model, &layout)
        .expect("array state initialization rows should lower");

    assert_eq!(rows.len(), 4);
    let (_, first_init) = eval_linear_ops(&rows[2], &[1.25, 2.5], &[], 0.0);
    let (_, second_init) = eval_linear_ops(&rows[3], &[1.25, 2.5], &[], 0.0);
    assert!((first_init.expect("x[1] residual") - 0.25).abs() < 1e-12);
    assert!((second_init.expect("x[2] residual") - 0.5).abs() < 1e-12);
}

#[test]
fn lower_initial_residual_keeps_derivative_rows_with_algebraic_targets() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .states
        .insert(rumoca_core::VarName::new("x"), scalar_var("x"));
    dae_model
        .variables
        .algebraics
        .insert(rumoca_core::VarName::new("y"), scalar_var("y"));
    dae_model
        .continuous
        .equations
        .push(residual(sub(var("y"), der(var("x")))));
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");

    let rows = lower_initial_residual(&dae_model, &layout)
        .expect("algebraic derivative row should stay in initial residual");

    assert_eq!(rows.len(), 1);
}

#[test]
fn lower_initial_residual_includes_parameter_only_initial_equations() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("p"),
        dae::Variable {
            fixed: Some(false),
            ..source_scalar_var("p")
        },
    );
    dae_model
        .initialization
        .equations
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Mul,
                    lhs: Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("p").into(),
                        subscripts: Vec::new(),
                        span: test_span(),
                    }),
                    rhs: Box::new(rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("p").into(),
                        subscripts: Vec::new(),
                        span: test_span(),
                    }),
                    span: test_span(),
                }),
                rhs: Box::new(rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Real(4.0),
                    span: test_span(),
                }),
                span: test_span(),
            },
            test_span(),
            "parameter initial equation",
        ));
    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "p", 3.0);

    let rows = lower_initial_residual(&dae_model, &layout)
        .expect("parameter-only initial equation should lower");

    assert_eq!(rows.len(), 1);
    let (_, out) = eval_linear_ops(&rows[0], &[], &p, 0.0);
    assert!((out.expect("p*p - 4 residual") - 5.0).abs() < 1e-12);
}

#[test]
fn lower_initial_expression_rows_treat_initial_builtin_as_true() {
    let expression = rumoca_core::Expression::If {
        branches: vec![(
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Initial,
                args: vec![],
                span: test_span(),
            },
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Real(3.0),
                span: test_span(),
            },
        )],
        else_branch: Box::new(rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(-1.0),
            span: test_span(),
        }),
        span: test_span(),
    };
    let rows = lower_initial_expression_rows_from_expressions(
        &[expression],
        &VarLayout::default(),
        &IndexMap::new(),
    )
    .expect("initial-mode expression rows lower");
    assert_eq!(rows.len(), 1);
    let (_regs, out) = eval_linear_ops(&rows[0], &[], &[], 0.0);
    assert_eq!(out.expect("output"), 3.0);
}

#[test]
fn lower_expression_prefers_qualified_normal_quantile_intrinsic_over_function_body() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("u"), scalar_var("u"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("mu"), scalar_var("mu"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("sigma"), scalar_var("sigma"));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("c"), source_array_var("c", &[1]));

    let mut quantile = test_function("Modelica.Math.Distributions.Normal.quantile", test_span());
    quantile.inputs = vec![
        rumoca_core::FunctionParam::new("p", "Real", test_span()),
        rumoca_core::FunctionParam::new("mu", "Real", test_span()),
        rumoca_core::FunctionParam::new("sigma", "Real", test_span()),
    ];
    quantile.outputs = vec![rumoca_core::FunctionParam::new("y", "Real", test_span())];
    quantile.body = vec![rumoca_core::Statement::Assignment {
        comp: component_ref("y"),
        value: binary(
            rumoca_core::OpBinary::Div,
            indexed_var("c", 1),
            indexed_var("c", 1),
        ),

        span: test_span(),
    }];
    dae_model
        .symbols
        .functions
        .insert(quantile.name.clone(), quantile);

    let layout = build_var_layout(&dae_model).expect("test DAE layout should build");
    let mut p = vec![0.0; layout.p_scalars()];
    set_p_value(&layout, &mut p, "u", 0.5);
    set_p_value(&layout, &mut p, "mu", 0.0);
    set_p_value(&layout, &mut p, "sigma", 1.0);
    set_p_value(&layout, &mut p, "c[1]", 0.0);
    let Some(ScalarSlot::P { index: c_index, .. }) = layout.binding("c[1]") else {
        panic!("c[1] should be a parameter slot");
    };

    let expr = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Modelica.Math.Distributions.Normal.quantile").into(),
        args: vec![var("u"), var("mu"), var("sigma")],
        is_constructor: false,
        span: test_span(),
    };
    let lowered = lower_expression(&expr, &layout, &dae_model.symbols.functions)
        .expect("MLS §12.4 qualified standard-library scalar function should lower");
    let (regs, _) = eval_linear_ops(&lowered.ops, &[], &p, 0.0);

    assert!(
        !lowered
            .ops
            .iter()
            .any(|op| matches!(op, LinearOp::LoadP { index, .. } if *index == c_index))
    );
    assert!(read_reg(&regs, lowered.result).abs() <= 1.0e-10);
}
