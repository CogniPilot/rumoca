use super::*;

#[test]
// SPEC_0021: Exception - single regression fixture for shiftSample alias-chain lowering.
#[allow(clippy::too_many_lines)]
fn test_runtime_precompute_resolves_shift_sample_via_sample_clock_alias_chain() {
    let mut dae_model = dae::Dae::default();

    for name in ["factor", "resolutionFactor"] {
        let mut p = dae::Variable::new(
            rumoca_core::VarName::new(name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        p.start = Some(if name == "factor" {
            lit(20.0)
        } else {
            lit(1000.0)
        });
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), p);
    }

    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("periodicClock.y")),
            rhs: Box::new(var("periodicClock.c")),
            span: test_span(1, 2),
        },
        test_span(1, 2),
        "periodicClock_alias",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("sample1.clock")),
            rhs: Box::new(var("periodicClock.y")),
            span: test_span(1, 2),
        },
        test_span(1, 2),
        "clock_alias",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("sample1.y")),
            rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![var("sample1.u"), var("sample1.clock")],
                span: test_span(1, 2),
            }),
            span: test_span(1, 2),
        },
        test_span(1, 2),
        "sample_rhs",
    ));
    dae_model.continuous.equations.push(dae::Equation::residual(
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(var("shiftSample1.u")),
            rhs: Box::new(var("sample1.y")),
            span: test_span(1, 2),
        },
        test_span(1, 2),
        "shift_source_alias",
    ));

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("periodicClock.c"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("subSample").into(),
                args: vec![
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::VarName::new("Clock").into(),
                        args: vec![var("factor")],
                        is_constructor: false,
                        span: test_span(1, 2),
                    },
                    var("resolutionFactor"),
                ],
                is_constructor: false,
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "periodic_clock",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("shifted"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("shiftSample").into(),
                args: vec![var("shiftSample1.u"), lit(1.0), lit(1.0)],
                is_constructor: false,
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "shifted_clock",
        ));

    populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
    assert!(
        dae_model.clocks.schedules.iter().any(|sched| {
            (sched.period_seconds - 0.02).abs() <= 1e-12
                && (sched.phase_seconds - 0.02).abs() <= 1e-12
        }),
        "expected shifted periodic schedule resolved through sample clock aliases"
    );
}

#[test]
fn test_runtime_precompute_resolves_shift_sample_with_reversed_clock_alias_equation() {
    let mut dae_model = dae::Dae::default();

    for (name, value) in [("factor", 20.0), ("resolutionFactor", 1000.0)] {
        let mut p = dae::Variable::new(
            rumoca_core::VarName::new(name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        p.start = Some(lit(value));
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), p);
    }

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("periodicClock.c"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("subSample").into(),
                args: vec![
                    rumoca_core::Expression::FunctionCall {
                        name: rumoca_core::VarName::new("Clock").into(),
                        args: vec![var("factor")],
                        is_constructor: false,
                        span: test_span(1, 2),
                    },
                    var("resolutionFactor"),
                ],
                is_constructor: false,
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "periodic_clock",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("periodicClock.y"),
            var("periodicClock.c"),
            test_span(1, 2),
            "periodic_clock_output",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("periodicClock.y"),
            var("sample1.clock"),
            test_span(1, 2),
            "reversed_connection_alias",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sample1.y"),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![var("sample1.u"), var("sample1.clock")],
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "sample_rhs",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("shiftSample1.u"),
            var("sample1.y"),
            test_span(1, 2),
            "shift_source_alias",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("shifted"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("shiftSample").into(),
                args: vec![var("shiftSample1.u"), lit(1.0), lit(1.0)],
                is_constructor: false,
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "shifted_clock",
        ));

    populate_runtime_precompute(&mut dae_model).expect(
        "clock schedule should resolve even when connection alias equation orientation is reversed",
    );
    assert!(
        dae_model.clocks.schedules.iter().any(|sched| {
            (sched.period_seconds - 0.02).abs() <= 1e-12
                && (sched.phase_seconds - 0.02).abs() <= 1e-12
        }),
        "expected shifted periodic schedule resolved through reversed connection alias equation"
    );
}

#[test]
fn test_runtime_precompute_prunes_dead_clock_constructor_branch_from_const_relation() {
    let mut dae_model = dae::Dae::default();

    for (name, value) in [
        ("resolution", 1.0),
        ("threshold", 2.0),
        ("factor", 20.0),
        ("resolutionFactor", 1000.0),
    ] {
        let mut p = dae::Variable::new(
            rumoca_core::VarName::new(name),
            rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 1, 2),
        );
        p.start = Some(lit(value));
        dae_model
            .variables
            .parameters
            .insert(rumoca_core::VarName::new(name), p);
    }

    let cond = rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Lt,
        lhs: Box::new(var("resolution")),
        rhs: Box::new(var("threshold")),
        span: test_span(1, 2),
    };
    let live_branch = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("subSample").into(),
        args: vec![
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("Clock").into(),
                args: vec![var("factor")],
                is_constructor: false,
                span: test_span(1, 2),
            },
            var("resolutionFactor"),
        ],
        is_constructor: false,
        span: test_span(1, 2),
    };
    let dead_branch = rumoca_core::Expression::FunctionCall {
        name: rumoca_core::VarName::new("Clock").into(),
        args: vec![
            var("periodicClock.factor"),
            var("periodicClock.resolutionFactor"),
        ],
        is_constructor: false,
        span: test_span(1, 2),
    };

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("periodicClock.c"),
            rumoca_core::Expression::If {
                branches: vec![(cond, live_branch)],
                else_branch: Box::new(dead_branch),
                span: test_span(1, 2),
            },
            test_span(1, 2),
            "periodic_clock_conditional_constructor",
        ));

    populate_runtime_precompute(&mut dae_model)
        .expect("const-true branch should prune dead unresolved clock constructor");
    assert!(
        dae_model
            .clocks
            .schedules
            .iter()
            .any(|sched| (sched.period_seconds - 0.02).abs() <= 1e-12),
        "expected resolved subSample schedule from live branch only"
    );
}
