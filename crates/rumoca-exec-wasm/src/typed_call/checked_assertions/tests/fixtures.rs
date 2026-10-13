use super::*;

fn profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    )
}

fn identity(value: u64) -> solve::SolvePureCallIdentity {
    solve::SolvePureCallIdentity::issued(NonZeroU64::new(value).unwrap())
}

pub(super) fn invocation_program(site: solve::SolvePureCallSite) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 0.0 },
        solve::LinearOp::PureCall {
            dst_start: 1,
            input_starts: (0..site.inputs().len())
                .map(|_| 0)
                .collect::<Vec<_>>()
                .into(),
            site,
        },
        solve::LinearOp::StoreOutput { src: 1 },
    ]
}

pub(super) fn action(
    site: solve::SolvePureCallSite,
    level: solve::SolveAssertionLevel,
) -> solve::SolveEventAction {
    solve::SolveEventAction {
        kind: match level {
            solve::SolveAssertionLevel::Error => solve::SolveEventActionKind::Assert,
            solve::SolveAssertionLevel::Warning => solve::SolveEventActionKind::Warning,
        },
        message: solve::SolveEventMessage {
            parts: vec![solve::SolveEventMessagePart::Text(
                "authored assertion".into(),
            )],
        },
        span: span(4),
        origin: "authored assertion".into(),
        clock_owner: None,
        assertion_projection: solve::SolveAssertionActionProjection::new(site, 1),
    }
}

fn outputs(level: solve::SolveAssertionLevel) -> Vec<solve::SolvePureCallOutput> {
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(profile()));
    vec![
        solve::SolvePureCallOutput::result(integer.clone()),
        solve::SolvePureCallOutput::assertion_predicate_at_level(level),
        solve::SolvePureCallOutput::assertion_message_value(integer, 1),
    ]
}

pub(super) fn zero_result_flow_table(
    fail_true: bool,
    fail_false: bool,
) -> solve::SolvePureCallTable {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    table
        .add_owner(
            identity(91),
            vec![solve::SolveValueType::scalar(
                solve::SolveScalarType::Boolean,
            )],
            vec![
                solve::SolvePureCallOutput::result(integer),
                solve::SolvePureCallOutput::assertion_predicate(),
                solve::SolvePureCallOutput::assertion_predicate(),
            ],
            span(1),
            |b, inputs, outputs| {
                let condition = b.load(inputs[0], span(2))?;
                let values = b.conditional(
                    condition,
                    &[],
                    Vec::new(),
                    span(3),
                    |region, _, outputs| {
                        assert!(outputs.is_empty());
                        zero_result_flow_arm(region, 1, fail_true, 4)
                    },
                    |region, _, outputs| {
                        assert!(outputs.is_empty());
                        zero_result_flow_arm(region, 2, fail_false, 14)
                    },
                )?;
                assert!(values.is_empty());
                let result = b.constant(solve::SolveValue::integer(p, 42).unwrap(), span(25))?;
                let inactive = b.constant(solve::SolveValue::boolean(true), span(26))?;
                b.store(outputs[0], result, span(27))?;
                b.store(outputs[1], inactive, span(28))?;
                b.store(outputs[2], inactive, span(29))
            },
        )
        .unwrap();
    table.finish()
}

fn zero_result_flow_arm(
    region: &mut solve::TypedProgramBuilder<'_>,
    predicate: usize,
    fails: bool,
    at: usize,
) -> Result<(), solve::SolveProgramConstructionError> {
    let condition = region.constant(solve::SolveValue::boolean(!fails), span(at + 1))?;
    let assertion = region.assertion_output(predicate, span(at + 2))?;
    region.check_assertion(assertion, condition, &[], span(at), |_, _, _| Ok(()))?;
    if fails {
        let one = region.constant(
            solve::SolveValue::integer(profile(), 1).unwrap(),
            span(at + 3),
        )?;
        let zero = region.constant(
            solve::SolveValue::integer(profile(), 0).unwrap(),
            span(at + 4),
        )?;
        region.binary(
            solve::SolveBinaryOperator::IntegerQuotient,
            one,
            zero,
            span(at + 5),
        )?;
    }
    Ok(())
}

pub(super) fn simple_table(
    level: solve::SolveAssertionLevel,
    faulty_message: bool,
    later_fault: bool,
) -> solve::SolvePureCallTable {
    let p = profile();
    let mut table = solve::SolvePureCallTable::builder(p);
    table
        .add_owner(
            identity(1),
            vec![solve::SolveValueType::scalar(
                solve::SolveScalarType::Boolean,
            )],
            outputs(level),
            span(1),
            |b, input, out| {
                let condition = b.load(input[0], span(2))?;
                let check = b.assertion_output(1, span(3))?;
                let message =
                    b.check_assertion(check, condition, &[], span(4), |m, _, output| {
                        let value = m.constant(
                            solve::SolveValue::integer(p, 9_007_199_254_740_993).unwrap(),
                            span(5),
                        )?;
                        let value = if faulty_message {
                            let zero =
                                m.constant(solve::SolveValue::integer(p, 0).unwrap(), span(5))?;
                            m.binary(
                                solve::SolveBinaryOperator::IntegerQuotient,
                                value,
                                zero,
                                span(6),
                            )?
                        } else {
                            value
                        };
                        m.store(output[0], value, span(7))
                    })?;
                let value = b.constant(solve::SolveValue::integer(p, 42).unwrap(), span(8))?;
                let value = if later_fault {
                    let zero = b.constant(solve::SolveValue::integer(p, 0).unwrap(), span(9))?;
                    b.binary(
                        solve::SolveBinaryOperator::IntegerQuotient,
                        value,
                        zero,
                        span(10),
                    )?
                } else {
                    value
                };
                b.store(out[0], value, span(11))?;
                b.store(out[1], condition, span(12))?;
                b.store(out[2], message[0], span(13))
            },
        )
        .unwrap();
    table.finish()
}

pub(super) fn loop_table(level: solve::SolveAssertionLevel) -> solve::SolvePureCallTable {
    loop_table_with_upper(level, 3)
}

pub(super) fn loop_table_with_upper(
    level: solve::SolveAssertionLevel,
    upper: i64,
) -> solve::SolvePureCallTable {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let child = table
        .add_owner(
            identity(2),
            vec![integer],
            outputs(level),
            span(20),
            |b, input, out| {
                let value = b.load(input[0], span(21))?;
                let condition = b.constant(solve::SolveValue::boolean(false), span(22))?;
                let assertion = b.assertion_output(1, span(23))?;
                let message = b.check_assertion(
                    assertion,
                    condition,
                    &[value],
                    span(24),
                    |m, captures, outputs| {
                        let value = m.load(captures[0], span(25))?;
                        m.store(outputs[0], value, span(26))
                    },
                )?;
                b.store(out[0], value, span(27))?;
                b.store(out[1], condition, span(28))?;
                b.store(out[2], message[0], span(29))
            },
        )
        .unwrap();
    table
        .add_owner(
            identity(3),
            vec![],
            outputs(level),
            span(30),
            |b, _, out| {
                let zero = b.constant(solve::SolveValue::integer(p, 0).unwrap(), span(31))?;
                let result = b.fold(
                    rumoca_core::StructuredIndexDomain {
                        binders: vec![rumoca_core::StructuredIndexBinder {
                            provenance: rumoca_core::IndexBinderProvenance::Generated,
                            id: 0,
                            display_name: "i".into(),
                            lower: 1,
                            upper,
                            step: 1,
                        }],
                    },
                    &[zero],
                    &[],
                    span(32),
                    |r, _, _, binders, outputs| {
                        let i = r.load(binders[0], span(33))?;
                        let call = r.emit_call(child, &[i], span(34))?;
                        let predicate = r.assertion_output(1, span(35))?;
                        r.forward_assertion(&call, 1, predicate, span(36))?;
                        r.store(outputs[0], call.registers()[0], span(37))
                    },
                )?;
                let yes = b.constant(solve::SolveValue::boolean(true), span(38))?;
                b.store(out[0], result[0], span(39))?;
                b.store(out[1], yes, span(40))?;
                b.store(out[2], zero, span(41))
            },
        )
        .unwrap();
    table.finish()
}

pub(super) fn math_message_table() -> solve::SolvePureCallTable {
    let p = profile();
    let mut output_types = outputs(solve::SolveAssertionLevel::Error);
    output_types[2] = solve::SolvePureCallOutput::assertion_message_value(
        solve::SolveValueType::scalar(solve::SolveScalarType::real(p)),
        1,
    );
    let mut table = solve::SolvePureCallTable::builder(p);
    table
        .add_owner(
            identity(4),
            vec![solve::SolveValueType::scalar(
                solve::SolveScalarType::Boolean,
            )],
            output_types,
            span(50),
            |b, inputs, outputs| {
                let condition = b.load(inputs[0], span(51))?;
                let assertion = b.assertion_output(1, span(52))?;
                let message =
                    b.check_assertion(assertion, condition, &[], span(53), |m, _, output| {
                        let value = m.constant(solve::SolveValue::real(p, 1.0), span(54))?;
                        let value = m.unary(solve::SolveUnaryOperator::Sin, value, span(55))?;
                        m.store(output[0], value, span(56))
                    })?;
                let value = b.constant(solve::SolveValue::integer(p, 42).unwrap(), span(57))?;
                b.store(outputs[0], value, span(58))?;
                b.store(outputs[1], condition, span(59))?;
                b.store(outputs[2], message[0], span(60))
            },
        )
        .unwrap();
    table.finish()
}
