use super::*;

fn profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    )
}

pub(super) fn absent_table() -> solve::SolvePureCallTable {
    let p = profile();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    table
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(301).unwrap()),
            vec![],
            vec![
                solve::SolvePureCallOutput::result(real.clone()),
                solve::SolvePureCallOutput::assertion_predicate(),
            ],
            span(301),
            |body, _, output| {
                let absent = body.absent_assigned_state(real.clone(), span(302))?;
                let assertion = body.assertion_output(1, span(303))?;
                let value = body.require_assigned_state(assertion, absent, span(304))?;
                let inactive = body.constant(solve::SolveValue::boolean(true), span(305))?;
                body.store(output[0], value, span(306))?;
                body.store(output[1], inactive, span(307))
            },
        )
        .unwrap();
    table.finish()
}

pub(super) fn real_table(level: solve::SolveAssertionLevel) -> solve::SolvePureCallTable {
    let p = profile();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let inputs = vec![
        real.clone(),
        solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
    ];
    let outputs = vec![
        solve::SolvePureCallOutput::result(real.clone()),
        solve::SolvePureCallOutput::assertion_predicate_at_level(level),
        solve::SolvePureCallOutput::assertion_message_value(real, 1),
    ];
    let mut table = solve::SolvePureCallTable::builder(p);
    let child = table
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(311).unwrap()),
            inputs.clone(),
            outputs.clone(),
            span(311),
            |body, input, output| {
                let value = body.load(input[0], span(312))?;
                let condition = body.load(input[1], span(313))?;
                let assertion = body.assertion_output(1, span(314))?;
                let message = body.check_assertion(
                    assertion,
                    condition,
                    &[value],
                    span(315),
                    |region, captures, outputs| {
                        let value = region.load(captures[0], span(316))?;
                        let value = region.binary(
                            solve::SolveBinaryOperator::Multiply,
                            value,
                            value,
                            span(317),
                        )?;
                        region.store(outputs[0], value, span(318))
                    },
                )?;
                let result = body.binary(
                    solve::SolveBinaryOperator::Multiply,
                    value,
                    value,
                    span(319),
                )?;
                body.store(output[0], result, span(320))?;
                body.store(output[1], condition, span(321))?;
                body.store(output[2], message[0], span(322))
            },
        )
        .unwrap();
    table
        .add_owner(
            solve::SolvePureCallIdentity::issued(NonZeroU64::new(312).unwrap()),
            inputs,
            outputs,
            span(323),
            |body, inputs, outputs| {
                let value = body.load(inputs[0], span(324))?;
                let condition = body.load(inputs[1], span(325))?;
                let call = body.emit_call(child, &[value, condition], span(326))?;
                let assertion = body.assertion_output(1, span(327))?;
                body.forward_assertion(&call, 1, assertion, span(328))?;
                for (output, value) in outputs.iter().zip(call.registers()) {
                    body.store(*output, *value, span(329))?;
                }
                Ok(())
            },
        )
        .unwrap();
    table.finish()
}

pub(super) fn directional_block(site: solve::SolvePureCallSite) -> solve::ScalarProgramBlock {
    let operations = vec![
        LinearOp::Const {
            dst: 0,
            value: -0.0,
        },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::LoadY { dst: 1, index: 0 },
        LinearOp::LoadY { dst: 2, index: 1 },
        LinearOp::LoadSeed { dst: 3, index: 0 },
        LinearOp::PureCall {
            dst_start: 4,
            input_starts: vec![1, 2].into(),
            site: site.clone(),
        },
        LinearOp::StoreOutputRange {
            start: 4,
            count: 3,
            stride: 1,
        },
        LinearOp::PureCallDirectional {
            dst_start: 7,
            input_starts: vec![1, 3, 2].into(),
            site: site.directional().unwrap().clone(),
        },
        LinearOp::StoreOutputRange {
            start: 7,
            count: 5,
            stride: 1,
        },
    ];
    solve::ScalarProgramBlock::with_program_spans(vec![operations], vec![span(330)]).unwrap()
}
