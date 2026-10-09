use super::*;

fn assertion_child(
    table: &mut SolvePureCallTableBuilder,
    sequential: bool,
) -> SolvePureCallOwnerId {
    let p = profile();
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    table
        .add_owner(
            identity(851),
            vec![SolveValueType::scalar(SolveScalarType::Boolean)],
            vec![
                SolvePureCallOutput::result(integer.clone()),
                SolvePureCallOutput::assertion_predicate(),
                SolvePureCallOutput::assertion_message_value(integer, 1),
            ],
            span(851),
            |builder, inputs, outputs| {
                let condition = builder.load(inputs[0], span(852))?;
                let capture = if sequential {
                    let capability = builder.assertion_output(1, span(853))?;
                    builder.check_assertion(
                        capability,
                        condition,
                        &[],
                        span(854),
                        |message, _, outputs| {
                            let capture =
                                message.constant(SolveValue::integer(p, 23).unwrap(), span(855))?;
                            message.store(outputs[0], capture, span(856))
                        },
                    )?[0]
                } else {
                    builder.constant(SolveValue::integer(p, 23).unwrap(), span(855))?
                };
                let value = builder.constant(SolveValue::integer(p, 17).unwrap(), span(857))?;
                builder.store(outputs[0], value, span(858))?;
                builder.store(outputs[1], condition, span(859))?;
                builder.store(outputs[2], capture, span(860))
            },
        )
        .unwrap()
}

fn forwarder(
    table: &mut SolvePureCallTableBuilder,
    child: SolvePureCallOwnerId,
    id: u64,
    duplicate: bool,
) -> SolvePureCallOwnerId {
    let interface = table.call_site(child).unwrap();
    table
        .add_owner(
            identity(id),
            interface.inputs().to_vec(),
            interface.outputs().to_vec(),
            span(861),
            |builder, inputs, outputs| {
                let condition = builder.load(inputs[0], span(862))?;
                let call = builder.emit_call(child, &[condition], span(863))?;
                let parent = builder.assertion_output(1, span(864))?;
                builder.forward_assertion(&call, 1, parent, span(865))?;
                if duplicate {
                    assert!(
                        builder
                            .forward_assertion(&call, 1, parent, span(865))
                            .is_err()
                    );
                }
                for (slot, value) in outputs.iter().zip(call.registers()) {
                    builder.store(*slot, *value, span(866))?;
                }
                Ok(())
            },
        )
        .unwrap()
}

#[test]
fn nested_forwarding_issues_one_flow_and_replays_exact_call_relations() {
    let mut builder = SolvePureCallTable::builder(profile());
    let child = assertion_child(&mut builder, true);
    let parent = forwarder(&mut builder, child, 852, true);
    forwarder(&mut builder, parent, 853, false);
    let table = builder.finish();
    assert!(
        table
            .owners()
            .iter()
            .all(|owner| owner.assertion_flow().is_some())
    );
    assert_eq!(
        table.owners()[1]
            .assertion_flow()
            .unwrap()
            .sources()
            .iter()
            .map(|source| source.operation())
            .collect::<Vec<_>>(),
        [1]
    );
    let encoded = serde_json::to_value(&table).unwrap();
    let replayed: SolvePureCallTable = serde_json::from_value(encoded).unwrap();
    assert_eq!(table, replayed);
    assert!(
        table
            .owners()
            .iter()
            .all(|owner| owner.directional().is_none())
    );
}

#[test]
fn structural_forwarding_does_not_certify_a_legacy_child() {
    let mut builder = SolvePureCallTable::builder(profile());
    let child = assertion_child(&mut builder, false);
    forwarder(&mut builder, child, 852, false);
    let table = builder.finish();
    assert!(
        table
            .owners()
            .iter()
            .all(|owner| owner.assertion_flow().is_none())
    );
}

#[test]
fn current_wire_rejects_forged_duplicate_and_missing_forwarding() {
    let mut builder = SolvePureCallTable::builder(profile());
    let child = assertion_child(&mut builder, true);
    forwarder(&mut builder, child, 852, false);
    let encoded = serde_json::to_value(builder.finish()).unwrap();
    for field in ["child_predicate", "parent_predicate"] {
        let mut result_predicate = encoded.clone();
        result_predicate["owners"][1]["body"]["operations"][1]["operation"]["assertion_forwarding"]
            [0][field] = 0.into();
        assert!(serde_json::from_value::<SolvePureCallTable>(result_predicate).is_err());
    }
    let mut duplicate = encoded.clone();
    let forwarding =
        duplicate["owners"][1]["body"]["operations"][1]["operation"]["assertion_forwarding"]
            .as_array_mut()
            .unwrap();
    forwarding.push(forwarding[0].clone());
    assert!(serde_json::from_value::<SolvePureCallTable>(duplicate).is_err());
    let mut missing = encoded.clone();
    missing["owners"][1]["body"]["operations"][1]["operation"]
        .as_object_mut()
        .unwrap()
        .remove("assertion_forwarding");
    assert!(serde_json::from_value::<SolvePureCallTable>(missing).is_err());
    let mut unbound = encoded;
    unbound["owners"][1]["body"]["operations"][1]["operation"]["assertion_forwarding"] =
        serde_json::json!([]);
    let unbound: SolvePureCallTable = serde_json::from_value(unbound).unwrap();
    assert!(unbound.owners()[0].assertion_flow().is_some());
    assert!(unbound.owners()[1].assertion_flow().is_none());
}

#[test]
fn forwarding_rejects_severity_capture_count_and_capture_type_mismatch() {
    let p = profile();
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    let real = SolveValueType::scalar(SolveScalarType::real(p));
    let cases = [
        vec![
            SolvePureCallOutput::assertion_predicate_at_level(SolveAssertionLevel::Warning),
            SolvePureCallOutput::assertion_message_value(integer.clone(), 1),
        ],
        vec![SolvePureCallOutput::assertion_predicate()],
        vec![
            SolvePureCallOutput::assertion_predicate(),
            SolvePureCallOutput::assertion_message_value(real, 1),
        ],
        vec![
            SolvePureCallOutput::assertion_predicate(),
            SolvePureCallOutput::assertion_message_value(integer.clone(), 1),
            SolvePureCallOutput::assertion_message_value(integer, 2),
        ],
    ];
    for outputs in cases {
        let mut table = SolvePureCallTable::builder(p);
        let child = assertion_child(&mut table, true);
        let result = table.add_owner(
            identity(854),
            vec![SolveValueType::scalar(SolveScalarType::Boolean)],
            outputs,
            span(870),
            |builder, inputs, _| {
                let condition = builder.load(inputs[0], span(871))?;
                let call = builder.emit_call(child, &[condition], span(872))?;
                let parent = builder.assertion_output(0, span(873))?;
                builder.forward_assertion(&call, 1, parent, span(874))
            },
        );
        assert!(matches!(
            result,
            Err(SolveProgramConstructionError::InvalidCallOutput { .. })
        ));
    }
}

#[test]
fn empty_transitive_flow_does_not_hide_unforwarded_assertions() {
    let p = profile();
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let mut table = SolvePureCallTable::builder(p);
    let ordinary = table
        .add_owner(
            identity(880),
            vec![boolean.clone()],
            vec![SolvePureCallOutput::result(boolean.clone())],
            span(880),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span(881))?;
                b.store(outputs[0], value, span(882))
            },
        )
        .unwrap();
    let child = assertion_child(&mut table, true);
    let mut wrappers = Vec::new();
    for (index, callee) in [ordinary, child].into_iter().enumerate() {
        wrappers.push(
            table
                .add_owner(
                    identity(881 + index as u64),
                    vec![boolean.clone()],
                    vec![SolvePureCallOutput::result(boolean.clone())],
                    span(883),
                    |b, inputs, outputs| {
                        let value = b.load(inputs[0], span(884))?;
                        let values = b.call(callee, &[value], span(885))?;
                        let value = if callee == child {
                            values[1]
                        } else {
                            values[0]
                        };
                        b.store(outputs[0], value, span(886))
                    },
                )
                .unwrap(),
        );
    }
    for (index, wrapper) in wrappers.into_iter().enumerate() {
        let site = table.call_site(wrapper).unwrap();
        table
            .add_owner(
                identity(883 + index as u64),
                site.inputs().to_vec(),
                site.outputs().to_vec(),
                span(887),
                |b, inputs, outputs| {
                    let value = b.load(inputs[0], span(888))?;
                    let values = b.call(wrapper, &[value], span(889))?;
                    b.store(outputs[0], values[0], span(890))
                },
            )
            .unwrap();
    }
    let table = table.finish();
    assert!(
        table.owners()[0]
            .assertion_flow()
            .unwrap()
            .sources()
            .iter()
            .map(|source| source.operation())
            .collect::<Vec<_>>()
            .is_empty()
    );
    for index in [2, 4] {
        assert!(
            table.owners()[index]
                .assertion_flow()
                .unwrap()
                .sources()
                .iter()
                .map(|source| source.operation())
                .collect::<Vec<_>>()
                .is_empty()
        );
    }
    for index in [3, 5] {
        assert!(table.owners()[index].assertion_flow().is_none());
    }
    let replayed: SolvePureCallTable =
        serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
    assert_eq!(replayed, table);
}

#[test]
fn every_child_predicate_requires_exactly_one_forwarded_parent_role() {
    let p = profile();
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let outputs = vec![
        SolvePureCallOutput::assertion_predicate(),
        SolvePureCallOutput::assertion_predicate(),
    ];
    let mut table = SolvePureCallTable::builder(p);
    let child = table
        .add_owner(
            identity(891),
            vec![boolean.clone()],
            outputs.clone(),
            span(891),
            |builder, inputs, outputs| {
                let condition = builder.load(inputs[0], span(892))?;
                for (index, output) in outputs.iter().enumerate() {
                    let assertion = builder.assertion_output(index, span(893))?;
                    builder
                        .check_assertion(assertion, condition, &[], span(894), |_, _, _| Ok(()))?;
                    builder.store(*output, condition, span(895))?;
                }
                Ok(())
            },
        )
        .unwrap();
    for (index, complete) in [false, true].into_iter().enumerate() {
        table
            .add_owner(
                identity(892 + index as u64),
                vec![boolean.clone()],
                outputs.clone(),
                span(896),
                |builder, inputs, outputs| {
                    let argument = builder.load(inputs[0], span(897))?;
                    let call = builder.emit_call(child, &[argument], span(898))?;
                    let first = builder.assertion_output(0, span(899))?;
                    let second = builder.assertion_output(1, span(899))?;
                    builder.forward_assertion(&call, 0, first, span(900))?;
                    assert!(
                        builder
                            .forward_assertion(&call, 1, first, span(900))
                            .is_err()
                    );
                    if complete {
                        builder.forward_assertion(&call, 1, second, span(900))?;
                    }
                    for (output, value) in outputs.iter().zip(call.registers()) {
                        builder.store(*output, *value, span(901))?;
                    }
                    Ok(())
                },
            )
            .unwrap();
    }
    let table = table.finish();
    assert!(table.owners()[0].assertion_flow().is_some());
    assert!(table.owners()[1].assertion_flow().is_none());
    assert!(table.owners()[2].assertion_flow().is_some());
}
