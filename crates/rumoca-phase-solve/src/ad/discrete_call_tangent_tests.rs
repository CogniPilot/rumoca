use super::*;
use rumoca_ir_solve as solve;

#[test]
fn nonzero_real_input_still_requires_its_directional_relation() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("real_call_tangent.mo"),
        0,
        1,
    );
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![real.clone()],
            vec![
                solve::SolvePureCallOutput::result(real),
                solve::SolvePureCallOutput::assertion_predicate(),
                solve::SolvePureCallOutput::assertion_message_value(
                    solve::SolveValueType::scalar(solve::SolveScalarType::integer(arithmetic)),
                    1,
                ),
            ],
            span,
            |builder, inputs, outputs| {
                let condition = builder.constant(solve::SolveValue::boolean(true), span)?;
                let assertion = builder.assertion_output(1, span)?;
                let message =
                    builder.check_assertion(assertion, condition, &[], span, |b, _, out| {
                        let value =
                            b.constant(solve::SolveValue::integer(arithmetic, 17).unwrap(), span)?;
                        b.store(out[0], value, span)
                    })?;
                let value = builder.load(inputs[0], span)?;
                builder.store(outputs[0], value, span)?;
                builder.store(outputs[1], condition, span)?;
                builder.store(outputs[2], message[0], span)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let mut builder = AdBuilder::new_with_span(SeedMode::SolverYAndP { p_seed_offset: 0 }, span);
    builder
        .lower_op(LinearOp::LoadP { dst: 0, index: 0 })
        .unwrap();
    let error = builder
        .lower_op(LinearOp::PureCall {
            dst_start: 1,
            input_starts: Box::new([0]),
            site: table.owners()[0].call_site(),
        })
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("directional owner has not been constructed")
    );
}

#[test]
fn seeded_discrete_arguments_preserve_primals_without_a_directional_owner() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("discrete_call_tangent.mo"),
        0,
        1,
    );
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![
                solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
                solve::SolveValueType::scalar(solve::SolveScalarType::integer(arithmetic)),
            ],
            vec![
                solve::SolvePureCallOutput::result(real),
                solve::SolvePureCallOutput::assertion_predicate(),
                solve::SolvePureCallOutput::assertion_message_value(
                    solve::SolveValueType::scalar(solve::SolveScalarType::integer(arithmetic)),
                    1,
                ),
            ],
            span,
            |builder, inputs, outputs| {
                let valid = builder.constant(solve::SolveValue::boolean(true), span)?;
                let assertion = builder.assertion_output(1, span)?;
                let message =
                    builder.check_assertion(assertion, valid, &[], span, |b, _, out| {
                        let value =
                            b.constant(solve::SolveValue::integer(arithmetic, 17).unwrap(), span)?;
                        b.store(out[0], value, span)
                    })?;
                let condition = builder.load(inputs[0], span)?;
                let integer = builder.load(inputs[1], span)?;
                let converted = builder.convert(
                    solve::SolveConversionOperator::IntegerToReal,
                    integer,
                    span,
                )?;
                let zero = builder.constant(solve::SolveValue::real(arithmetic, 0.0), span)?;
                let result = builder.select(condition, converted, zero, span)?;
                builder.store(outputs[0], result, span)?;
                builder.store(outputs[1], valid, span)?;
                builder.store(outputs[2], message[0], span)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    assert!(site.directional().is_none());
    let mut builder = AdBuilder::new_with_span(SeedMode::SolverYAndP { p_seed_offset: 0 }, span);
    builder.store_output_mode = StoreOutputMode::Dual;
    for operation in [
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::LoadP { dst: 1, index: 1 },
        LinearOp::PureCall {
            dst_start: 2,
            input_starts: Box::new([0, 1]),
            site,
        },
        LinearOp::StoreOutputRange {
            start: 2,
            count: 2,
            stride: 1,
        },
    ] {
        builder.lower_op(operation).unwrap();
    }
    let block = ScalarProgramBlock::with_source_span(
        vec![builder.ops],
        span.require_provenance("discrete input tangent control")
            .unwrap(),
    )
    .unwrap();
    for (condition, expected) in [(0.0, 0.0), (1.0, 17.0)] {
        let mut output = [91.0, 92.0, 93.0, 94.0];
        rumoca_eval_solve::eval_scalar_program_block_with_context(
            &block,
            &[],
            &[condition, 17.0],
            0.0,
            rumoca_eval_solve::RowEvalContext {
                seed: Some(&[3.0, 5.0]),
                pure_calls: Some(&table),
                ..Default::default()
            },
            &mut output,
        )
        .unwrap();
        assert_eq!(output, [expected, 0.0, 1.0, 0.0]);
    }
}
