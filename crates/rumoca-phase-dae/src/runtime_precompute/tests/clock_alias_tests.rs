use super::*;

fn seed_conditional_residual_alias_chain_params(dae_model: &mut dae::Dae) {
    for (name, value) in [
        ("periodicClock.factor", 20.0),
        ("periodicClock.resolution", 2.0),
        ("periodicClock.resolutionFactor", 1000.0),
        ("shiftSample1.shiftCounter", 4.0),
        ("shiftSample1.resolution", 2.0),
        ("threshold", 3.0),
    ] {
        let mut p = dae::Variable::new(rumoca_core::VarName::new(name));
        p.start = Some(lit(value));
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), p);
    }
}

fn add_conditional_residual_clock_equation(dae_model: &mut dae::Dae) {
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::If {
                branches: vec![(
                    rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Lt,
                        lhs: Box::new(var("periodicClock.resolution")),
                        rhs: Box::new(var("threshold")),
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Expression::Binary {
                        op: rumoca_core::OpBinary::Sub,
                        lhs: Box::new(var("periodicClock.c")),
                        rhs: Box::new(rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("subSample").into(),
                            args: vec![
                                rumoca_core::Expression::FunctionCall {
                                    name: rumoca_core::VarName::new("Clock").into(),
                                    args: vec![var("periodicClock.factor")],
                                    is_constructor: false,
                                    span: rumoca_core::Span::DUMMY,
                                },
                                var("periodicClock.resolutionFactor"),
                            ],
                            is_constructor: false,
                            span: rumoca_core::Span::DUMMY,
                        }),
                        span: rumoca_core::Span::DUMMY,
                    },
                )],
                else_branch: Box::new(rumoca_core::Expression::Binary {
                    op: rumoca_core::OpBinary::Sub,
                    lhs: Box::new(var("periodicClock.c")),
                    rhs: Box::new(rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::VarName::new("Clock").into(),
                        args: vec![
                            var("periodicClock.factor"),
                            var("periodicClock.resolutionFactor"),
                        ],
                        is_constructor: false,
                        span: rumoca_core::Span::DUMMY,
                    }),
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "periodic_clock_conditional_residual",
        ));
}

fn add_shift_alias_chain_equations(dae_model: &mut dae::Dae) {
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("periodicClock.y"),
            var("periodicClock.c"),
            Span::DUMMY,
            "periodic_clock_output_alias",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sample1.clock"),
            var("periodicClock.y"),
            Span::DUMMY,
            "sample_clock_alias",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sample1.y"),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![var("u"), var("sample1.clock")],
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "sample_output",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("shiftSample1.u"),
            var("sample1.y"),
            Span::DUMMY,
            "shift_input_alias",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("shiftSample1.y"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("shiftSample").into(),
                args: vec![
                    var("shiftSample1.u"),
                    var("shiftSample1.shiftCounter"),
                    var("shiftSample1.resolution"),
                ],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "shift_output",
        ));
}

#[test]
fn test_runtime_precompute_resolves_clock_alias_chain_from_conditional_residual_branch() {
    let mut dae_model = dae::Dae::default();
    seed_conditional_residual_alias_chain_params(&mut dae_model);
    add_conditional_residual_clock_equation(&mut dae_model);
    add_shift_alias_chain_equations(&mut dae_model);
    populate_runtime_precompute(&mut dae_model)
        .expect("conditional residual clock aliases should resolve to a periodic schedule");
    assert!(
        dae_model
            .clocks
            .schedules
            .iter()
            .any(|sched| (sched.period_seconds - 0.02).abs() <= 1e-12),
        "expected 20/1000 second periodic schedule through conditional-residual alias chain"
    );
}

#[test]
fn test_runtime_precompute_resolves_clock_resolution_factor_from_table_lookup() {
    let mut dae_model = dae::Dae::default();

    let mut factor = dae::Variable::new(rumoca_core::VarName::new("periodicClock.factor"));
    factor.start = Some(lit(20.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("periodicClock.factor"), factor);

    let mut resolution = dae::Variable::new(rumoca_core::VarName::new("periodicClock.resolution"));
    resolution.start = Some(lit(2.0));
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("periodicClock.resolution"),
        resolution,
    );

    let mut conversion_table =
        dae::Variable::new(rumoca_core::VarName::new("periodicClock.conversionTable"));
    conversion_table.start = Some(rumoca_core::Expression::Array {
        elements: vec![lit(1.0), lit(1000.0)],
        is_matrix: false,
        span: rumoca_core::Span::DUMMY,
    });
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("periodicClock.conversionTable"),
        conversion_table,
    );

    let mut resolution_factor =
        dae::Variable::new(rumoca_core::VarName::new("periodicClock.resolutionFactor"));
    resolution_factor.start = Some(rumoca_core::Expression::VarRef {
        name: rumoca_core::VarName::new("periodicClock.conversionTable").into(),
        subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Integer").into(),
                args: vec![var("periodicClock.resolution")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
        ))],
        span: rumoca_core::Span::DUMMY,
    });
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("periodicClock.resolutionFactor"),
        resolution_factor,
    );

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("periodicClock.c"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Clock").into(),
                args: vec![
                    var("periodicClock.factor"),
                    var("periodicClock.resolutionFactor"),
                ],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "periodic_clock_from_conversion_table",
        ));

    populate_runtime_precompute(&mut dae_model)
        .expect("clock schedule should resolve through indexed conversion table lookup");
    assert!(
        dae_model
            .clocks
            .schedules
            .iter()
            .any(|sched| (sched.period_seconds - 0.02).abs() <= 1e-12),
        "expected period 20/1000 = 0.02"
    );
}

#[test]
fn test_runtime_precompute_uses_else_clock_timing_when_condition_is_false() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable::new(rumoca_core::VarName::new("u")),
    );
    dae_model.variables.discrete_reals.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable::new(rumoca_core::VarName::new("y")),
    );

    let mut infer_factor = dae::Variable::new(rumoca_core::VarName::new("inferFactor"));
    infer_factor.start = Some(lit(0.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("inferFactor"), infer_factor);

    let mut factor = dae::Variable::new(rumoca_core::VarName::new("factor"));
    factor.start = Some(lit(3.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("factor"), factor);

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("u"),
            clock_call(0.02),
            Span::DUMMY,
            "source_clock",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("y"),
            rumoca_core::Expression::If {
                branches: vec![(
                    var("inferFactor"),
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::VarName::new("superSample").into(),
                        args: vec![var("u")],
                        is_constructor: false,
                        span: rumoca_core::Span::DUMMY,
                    },
                )],
                else_branch: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("superSample").into(),
                    args: vec![var("u"), var("factor")],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "const_false_clock_branch",
        ));

    populate_runtime_precompute(&mut dae_model)
        .expect("const-false clock branch should infer timing from else branch");
    let timing = dae_model
        .clocks
        .timings
        .get("y")
        .expect("y should receive the super-sampled else-branch timing");
    assert!((timing.period_seconds - (0.02 / 3.0)).abs() <= 1e-12);
}

#[test]
fn test_runtime_precompute_infers_supersample_timing_from_same_clock_equations() {
    let mut dae_model = clock_equivalence_test_model();

    populate_runtime_precompute(&mut dae_model)
        .expect("equation clock timing propagation should succeed");
    for name in ["y", "u_super", "b_super", "add.u1"] {
        let timing = dae_model
            .clocks
            .timings
            .get(name)
            .unwrap_or_else(|| panic!("{name} should have inferred timing"));
        assert!(
            (timing.period_seconds - (0.02 / 3.0)).abs() <= 1e-12,
            "{name} should inherit the known fast clock"
        );
    }
}

fn clock_equivalence_test_model() -> dae::Dae {
    let mut dae_model = dae::Dae::default();
    add_discrete_real_vars(
        &mut dae_model,
        &[
            "u", "u_fast", "y_fast", "add.u1", "add.u2", "add.y", "b_super", "u_super", "y",
        ],
    );
    push_real_update(&mut dae_model, "u", clock_call(0.02), "source_clock");
    push_real_update(
        &mut dae_model,
        "u_fast",
        super_sample_expr("u", Some(3.0)),
        "explicit_fast_clock",
    );
    push_real_update(&mut dae_model, "y_fast", var("u_fast"), "fast_output_alias");
    push_real_update(&mut dae_model, "add.u2", var("y_fast"), "fast_add_input");
    push_same_clock_add_equation(&mut dae_model);
    push_real_update(&mut dae_model, "add.u1", var("y"), "inferred_add_input");
    push_real_update(
        &mut dae_model,
        "u_super",
        super_sample_expr("u", None),
        "inferred_value_supersample",
    );
    push_real_update(
        &mut dae_model,
        "y",
        rumoca_core::Expression::If {
            branches: vec![(var("b_super"), var("u_super"))],
            else_branch: Box::new(lit(0.0)),
            span: rumoca_core::Span::DUMMY,
        },
        "same_target_clock_condition",
    );
    dae_model
}

fn add_discrete_real_vars(dae_model: &mut dae::Dae, names: &[&str]) {
    for name in names {
        dae_model.variables.discrete_reals.insert(
            rumoca_core::VarName::new(*name),
            dae::Variable::new(rumoca_core::VarName::new(*name)),
        );
    }
}

fn push_real_update(
    dae_model: &mut dae::Dae,
    lhs: &str,
    rhs: rumoca_core::Expression,
    origin: &str,
) {
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new(lhs),
            rhs,
            Span::DUMMY,
            origin,
        ));
}

fn super_sample_expr(source: &str, factor: Option<f64>) -> rumoca_core::Expression {
    let mut args = vec![var(source)];
    if let Some(factor) = factor {
        args.push(lit(factor));
    }
    rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("superSample").into(),
        args,
        is_constructor: false,
        span: rumoca_core::Span::DUMMY,
    }
}

fn push_same_clock_add_equation(dae_model: &mut dae::Dae) {
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("add.y")),
            rhs: Box::new(rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Add,
                lhs: Box::new(var("add.u1")),
                rhs: Box::new(var("add.u2")),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
        },
        Span::DUMMY,
        "same_clock_add",
    ));
}

#[test]
fn test_runtime_precompute_skips_inferred_clock_forms_without_static_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("b"),
            rumoca_core::Expression::If {
                branches: vec![(
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::VarName::new("Clock").into(),
                        args: vec![],
                        is_constructor: false,
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Expression::Unary {
                        op: rumoca_core::OpUnary::Not,
                        rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                            function: rumoca_core::BuiltinFunction::Pre,
                            args: vec![var("b")],
                            span: rumoca_core::Span::DUMMY,
                        }),
                        span: rumoca_core::Span::DUMMY,
                    },
                )],
                else_branch: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Pre,
                    args: vec![var("b")],
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "inferred_clock_toggle",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("y"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("superSample").into(),
                args: vec![var("b")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "inferred_super_sample",
        ));

    populate_runtime_precompute(&mut dae_model)
        .expect("inferred clock forms should remain as dynamic runtime clock expressions");
    assert!(
        dae_model.clocks.schedules.is_empty(),
        "inferred clock forms should not synthesize static periodic schedules"
    );
    assert_eq!(
        dae_model.clocks.constructor_exprs.len(),
        2,
        "all clock constructor expressions should remain visible for diagnostics"
    );
}

#[test]
fn test_runtime_precompute_uses_unique_static_clock_for_no_argument_clock_guard() {
    let mut dae_model = dae::Dae::default();
    add_discrete_real_vars(&mut dae_model, &["u"]);
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("y"),
        dae::Variable::new(rumoca_core::VarName::new("y")),
    );
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("y2"),
        dae::Variable::new(rumoca_core::VarName::new("y2")),
    );
    dae_model.variables.parameters.insert(
        rumoca_core::VarName::new("__pre__.y2"),
        dae::Variable::new(rumoca_core::VarName::new("__pre__.y2")),
    );
    push_real_update(&mut dae_model, "u", clock_call(0.02), "source_clock");
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("y2"),
            rumoca_core::Expression::If {
                branches: vec![(
                    rumoca_core::Expression::BuiltinCall {
                        function: rumoca_core::BuiltinFunction::Edge,
                        args: vec![rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("Clock").into(),
                            args: vec![],
                            is_constructor: false,
                            span: rumoca_core::Span::DUMMY,
                        }],
                        span: rumoca_core::Span::DUMMY,
                    },
                    rumoca_core::Expression::Unary {
                        op: rumoca_core::OpUnary::Not,
                        rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                            function: rumoca_core::BuiltinFunction::Pre,
                            args: vec![var("__pre__.y2")],
                            span: rumoca_core::Span::DUMMY,
                        }),
                        span: rumoca_core::Span::DUMMY,
                    },
                )],
                else_branch: Box::new(var("__pre__.y2")),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "guarded when equation assignment to y2",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("y"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("hold").into(),
                args: vec![var("y2")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "y = hold(y2)",
        ));

    populate_runtime_precompute(&mut dae_model)
        .expect("unique static clock should provide inferred Clock() timing");
    for name in ["y2", "y"] {
        let timing = dae_model
            .clocks
            .timings
            .get(name)
            .unwrap_or_else(|| panic!("{name} should inherit the unique static clock"));
        assert!(
            (timing.period_seconds - 0.02).abs() <= 1e-12,
            "{name} should use the unique static clock period"
        );
    }
}

#[test]
fn test_runtime_precompute_skips_event_clock_constructor_without_static_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new("tick"),
        dae::Variable::new(rumoca_core::VarName::new("tick")),
    );
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("eventClock"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Clock").into(),
                args: vec![var("tick")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "event_clock_constructor",
        ));

    populate_runtime_precompute(&mut dae_model)
        .expect("Clock(condition) should be treated as non-static event clock");
    assert!(
        dae_model.clocks.schedules.is_empty(),
        "Clock(condition) should not synthesize static periodic schedules"
    );
    assert_eq!(
        dae_model.clocks.constructor_exprs.len(),
        1,
        "event clock constructor remains visible in diagnostics metadata"
    );
}
