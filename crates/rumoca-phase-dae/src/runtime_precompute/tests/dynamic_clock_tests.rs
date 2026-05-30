use super::*;

#[test]
fn test_runtime_precompute_skips_shift_sample_with_non_static_source_clock() {
    let mut dae_model = dae::Dae::default();
    let mut shift_counter = dae::Variable::new(rumoca_core::VarName::new("shiftCounter"));
    shift_counter.start = Some(lit(2.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("shiftCounter"), shift_counter);
    let mut resolution = dae::Variable::new(rumoca_core::VarName::new("resolution"));
    resolution.start = Some(lit(10.0));
    dae_model
        .variables
        .parameters
        .insert(rumoca_core::VarName::new("resolution"), resolution);
    dae_model.variables.algebraics.insert(
        rumoca_core::VarName::new("u"),
        dae::Variable::new(rumoca_core::VarName::new("u")),
    );

    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("y"),
            rumoca_core::Expression::FunctionCall {
                name: rumoca_core::VarName::new("shiftSample").into(),
                args: vec![var("u"), var("shiftCounter"), var("resolution")],
                is_constructor: false,
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "shift_sample_inferred_source_clock",
        ));

    populate_runtime_precompute(&mut dae_model)
        .expect("shiftSample with non-static source clock should remain dynamic");
    assert!(
        dae_model.clocks.schedules.is_empty(),
        "dynamic shiftSample form should not synthesize a static schedule"
    );
    assert_eq!(
        dae_model.clocks.constructor_exprs.len(),
        1,
        "dynamic shiftSample should remain visible in constructor metadata"
    );
}

#[test]
fn test_runtime_precompute_rejects_dynamic_clock_constructor_without_schedule() {
    let mut dae_model = dae::Dae::default();
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("s")),
                rhs: Box::new(rumoca_core::Expression::BuiltinCall {
                    function: rumoca_core::BuiltinFunction::Sample,
                    args: vec![
                        var("u"),
                        rumoca_core::Expression::FunctionCall {
                            name: rumoca_core::VarName::new("Clock").into(),
                            args: vec![var("u")],
                            is_constructor: false,
                            span: rumoca_core::Span::DUMMY,
                        },
                    ],
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
            "test_dynamic_clock_constructor",
        ));

    let err = populate_runtime_precompute(&mut dae_model)
        .expect_err("dynamic clock constructors must fail during ToDae runtime precompute");
    assert!(
        matches!(err, ToDaeError::UnresolvedClockSchedule { .. }),
        "expected unresolved clock schedule error, got {err:?}"
    );
}

#[test]
fn test_runtime_precompute_resolves_event_clock_condition_through_spanned_alias() {
    let mut dae_model = dae::Dae::default();
    let source_span = Span::from_offsets(rumoca_core::SourceId(7), 11, 23);
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("tick")),
                rhs: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("Clock").into(),
                    args: vec![var("clock_u")],
                    is_constructor: false,
                    span: source_span,
                }),
                span: source_span,
            },
            source_span,
            "tick = Clock(clock_u)",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("less_y"),
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Lt,
                lhs: Box::new(var("angle")),
                rhs: Box::new(var("limit")),
                span: source_span,
            },
            source_span,
            "less_y = angle < limit",
        ));
    dae_model
        .discrete
        .valued_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("less_y")),
                rhs: Box::new(var("clock_u")),
                span: source_span,
            },
            source_span,
            "less_y = clock_u",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::residual(
            rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(var("tick")),
                rhs: Box::new(var("sample_clock")),
                span: source_span,
            },
            source_span,
            "tick = sample_clock",
        ));
    dae_model
        .discrete
        .real_updates
        .push(dae::Equation::explicit(
            rumoca_core::VarName::new("sample_y"),
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![var("sample_u"), var("sample_clock")],
                span: source_span,
            },
            source_span,
            "sample_y = sample(sample_u, sample_clock)",
        ));

    populate_runtime_precompute(&mut dae_model)
        .expect("spanned event-clock alias chain should resolve as a dynamic event clock");
    assert!(
        dae_model.clocks.schedules.is_empty(),
        "event Clock(condition) must not synthesize a periodic schedule"
    );
    assert_eq!(
        dae_model.clocks.triggered_conditions,
        vec![var("clock_u")],
        "event Clock(condition) should be preserved for Solve IR lowering"
    );
    assert!(
        !dae_model.clocks.intervals.contains_key("sample_clock"),
        "event-clock aliases must not be inferred as periodic clock intervals"
    );
}
