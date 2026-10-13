use super::*;

#[derive(Clone, Copy)]
enum CheckKind {
    AssignedRead,
    AuthoredBoolean,
    WarningPresence,
    CapturedPresence,
}

fn checked_owner(kind: CheckKind) -> SolvePureCallTable {
    SolvePureCallTable::construct(profile(), |table| {
        add_checked_owner(table, kind, 910)?;
        Ok(())
    })
    .unwrap()
}

fn add_checked_owner(
    table: &mut SolvePureCallTableBuilder,
    kind: CheckKind,
    id: u64,
) -> Result<SolvePureCallOwnerId, crate::SolveProgramConstructionError> {
    let p = profile();
    let real = SolveValueType::scalar(SolveScalarType::real(p));
    let level = if matches!(kind, CheckKind::WarningPresence) {
        SolveAssertionLevel::Warning
    } else {
        SolveAssertionLevel::Error
    };
    let mut interface = vec![
        SolvePureCallOutput::result(real),
        SolvePureCallOutput::assertion_predicate_at_level(level),
    ];
    if matches!(kind, CheckKind::CapturedPresence) {
        interface.push(SolvePureCallOutput::assertion_message_value(
            SolveValueType::scalar(SolveScalarType::integer(p)),
            1,
        ));
    }
    table.add_owner(
        identity(id),
        vec![],
        interface,
        span(910),
        |builder, _, outputs| {
            let value = builder.constant(SolveValue::real(p, 7.0), span(911))?;
            let state = builder.present_assigned_state(value, span(912))?;
            let assertion = builder.assertion_output(1, span(913))?;
            let (condition, result) = match kind {
                CheckKind::AssignedRead => {
                    let (condition, projected) =
                        builder.require_assigned_states(assertion, &[state], span(914))?;
                    (condition, projected[0])
                }
                CheckKind::AuthoredBoolean
                | CheckKind::WarningPresence
                | CheckKind::CapturedPresence => {
                    let condition = if !matches!(kind, CheckKind::AuthoredBoolean) {
                        builder.assigned_state_is_present(state, span(914))?
                    } else {
                        builder.constant(SolveValue::boolean(true), span(914))?
                    };
                    let captures = builder.check_assertion(
                        assertion,
                        condition,
                        &[],
                        span(915),
                        |message, _, outputs| assigned_read_message(p, message, outputs),
                    )?;
                    if let Some(capture) = captures.first() {
                        builder.store(outputs[2], *capture, span(916))?;
                    }
                    (condition, value)
                }
            };
            builder.store(outputs[0], result, span(916))?;
            builder.store(outputs[1], condition, span(917))
        },
    )
}

#[test]
fn assigned_read_subtype_replays_from_exact_presence_and_error_check() {
    let table = checked_owner(CheckKind::AssignedRead);
    assert_eq!(
        table.owners()[0]
            .assertion_flow()
            .unwrap()
            .assigned_read_predicates(),
        [1]
    );
    let wire = serde_json::to_value(&table).unwrap();
    assert!(
        wire["owners"][0]["body"]["assigned_states"]
            .get("assigned_read_checks")
            .is_none()
    );
    let replay: SolvePureCallTable = serde_json::from_value(wire.clone()).unwrap();
    assert_eq!(replay, table);
    assert_eq!(
        replay.owners()[0]
            .assertion_flow()
            .unwrap()
            .assigned_read_predicates(),
        [1]
    );

    let mut forged = wire;
    forged["owners"][0]["body"]["assigned_states"]["assigned_read_checks"] =
        serde_json::json!([[0, 1]]);
    assert!(serde_json::from_value::<SolvePureCallTable>(forged).is_err());
}

#[test]
fn predicate_severity_and_presence_alone_do_not_classify_authored_checks() {
    for kind in [
        CheckKind::AuthoredBoolean,
        CheckKind::WarningPresence,
        CheckKind::CapturedPresence,
    ] {
        let table = checked_owner(kind);
        assert!(
            table.owners()[0]
                .assertion_flow()
                .unwrap()
                .assigned_read_predicates()
                .is_empty()
        );
        let replay: SolvePureCallTable =
            serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
        assert!(
            replay.owners()[0]
                .assertion_flow()
                .unwrap()
                .assigned_read_predicates()
                .is_empty()
        );
    }
}

#[test]
fn assigned_read_subtype_follows_original_child_forwarding() {
    let table = SolvePureCallTable::construct(profile(), |table| {
        let child = add_checked_owner(table, CheckKind::AssignedRead, 920)?;
        let mut previous = child;
        for ordinal in 921..923 {
            let outputs = table.call_site(previous).unwrap().outputs().to_vec();
            previous = table.add_owner(
                identity(ordinal),
                vec![],
                outputs,
                span(927),
                |builder, _, outputs| forward_assigned_read(builder, outputs, previous),
            )?;
        }
        Ok(())
    })
    .unwrap();
    let encoded = serde_json::to_value(&table).unwrap();
    let replay: SolvePureCallTable = serde_json::from_value(encoded).unwrap();
    for candidate in [&table, &replay] {
        for owner in candidate.owners() {
            assert_eq!(
                owner.assertion_flow().unwrap().assigned_read_predicates(),
                [1]
            );
        }
    }
}

#[test]
fn assigned_read_subtype_retains_selected_region_source() {
    let p = profile();
    let real = SolveValueType::scalar(SolveScalarType::real(p));
    let table = SolvePureCallTable::construct(p, |table| {
        table.add_owner(
            identity(940),
            vec![SolveValueType::scalar(SolveScalarType::Boolean)],
            vec![
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::assertion_predicate(),
            ],
            span(940),
            |builder, inputs, outputs| {
                let selected = builder.load(inputs[0], span(941))?;
                let values = builder.conditional(
                    selected,
                    &[],
                    vec![real.clone()],
                    span(942),
                    |branch, _, outputs| {
                        let state = branch.absent_assigned_state(real.clone(), span(943))?;
                        let assertion = branch.assertion_output(1, span(944))?;
                        let value = branch.require_assigned_state(assertion, state, span(945))?;
                        branch.store(outputs[0], value, span(946))
                    },
                    |branch, _, outputs| {
                        let value = branch.constant(SolveValue::real(p, 7.0), span(947))?;
                        branch.store(outputs[0], value, span(948))
                    },
                )?;
                builder.store(outputs[0], values[0], span(949))?;
                let inactive = builder.constant(SolveValue::boolean(true), span(950))?;
                builder.store(outputs[1], inactive, span(951))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let replay: SolvePureCallTable =
        serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
    for candidate in [&table, &replay] {
        let flow = candidate.owners()[0].assertion_flow().unwrap();
        assert_eq!(flow.assigned_read_predicates(), [1]);
        assert_eq!(flow.sources().len(), 1);
        assert_eq!(flow.sources()[0].regions().len(), 1);
        assert_eq!(
            flow.sources()[0].regions()[0].kind(),
            AssertionRegionKind::ConditionalThen
        );
    }
}

fn assigned_read_message<'program>(
    p: SolveArithmeticProfile,
    message: &mut TypedProgramBuilder<'program>,
    outputs: &[ProgramSlot<'program>],
) -> Result<(), crate::SolveProgramConstructionError> {
    if let Some(output) = outputs.first() {
        let value = message.constant(SolveValue::integer(p, 11).unwrap(), span(915))?;
        message.store(*output, value, span(915))?;
    }
    Ok(())
}

fn forward_assigned_read<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    outputs: &[ProgramSlot<'program>],
    previous: SolvePureCallOwnerId,
) -> Result<(), crate::SolveProgramConstructionError> {
    let call = builder.emit_call(previous, &[], span(928))?;
    let assertion = builder.assertion_output(1, span(929))?;
    builder.forward_assertion(&call, 1, assertion, span(930))?;
    for (output, value) in outputs.iter().zip(call.registers()) {
        builder.store(*output, *value, span(931))?;
    }
    Ok(())
}
