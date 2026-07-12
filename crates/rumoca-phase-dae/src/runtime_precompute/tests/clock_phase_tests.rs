use super::*;

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
