use super::*;
use crate::SolveAssertionMessage;

fn message_owner() -> SolvePureCallTable {
    let p = profile();
    let real = SolveValueType::scalar(SolveScalarType::real(p));
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    let mut table = SolvePureCallTable::builder(p);
    table
        .add_owner(
            identity(701),
            vec![],
            vec![
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::assertion_predicate_at_level(SolveAssertionLevel::Warning),
                SolvePureCallOutput::assertion_message_value(real, 1),
                SolvePureCallOutput::assertion_predicate(),
                SolvePureCallOutput::assertion_message_value(integer, 1),
            ],
            span(701),
            |b, _, outputs| {
                let real = b.constant(SolveValue::real(p, 3.0), span(702))?;
                let predicate = b.constant(SolveValue::boolean(true), span(703))?;
                let integer = b.constant(SolveValue::integer(p, 4).unwrap(), span(704))?;
                for (output, value) in outputs
                    .iter()
                    .zip([real, predicate, real, predicate, integer])
                {
                    b.store(*output, value, span(705))?;
                }
                Ok(())
            },
        )
        .unwrap();
    table.finish()
}

#[test]
fn assertion_roles_survive_wire_and_directional_pairing() {
    let table = message_owner();
    let encoded = serde_json::to_value(&table).unwrap();
    let decoded: SolvePureCallTable = serde_json::from_value(encoded).unwrap();
    assert_eq!(decoded, table);
    let owner = &decoded.owners()[0];
    assert_eq!(
        owner.outputs()[1].assertion_level(),
        Some(SolveAssertionLevel::Warning)
    );
    assert_eq!(
        owner.outputs()[3].assertion_level(),
        Some(SolveAssertionLevel::Error)
    );
    let directional = owner.directional().unwrap();
    let outputs = directional.outputs();
    assert_eq!(outputs.len(), 7);
    assert_eq!(
        outputs[2].assertion_level(),
        Some(SolveAssertionLevel::Warning)
    );
    assert_eq!(outputs[3].message_predicate_output(3), Some(2));
    assert_eq!(outputs[4].message_predicate_output(4), Some(2));
    assert_eq!(
        outputs[5].assertion_level(),
        Some(SolveAssertionLevel::Error)
    );
    assert_eq!(outputs[6].message_predicate_output(6), Some(5));
}

#[test]
fn message_role_cannot_name_a_result_or_an_unissued_predicate() {
    let table = message_owner();
    for distance in [0, 2, 3, usize::MAX] {
        let mut encoded = serde_json::to_value(&table).unwrap();
        encoded["owners"][0]["outputs"][2]["role"]["predicate_distance"] = distance.into();
        assert!(
            serde_json::from_value::<SolvePureCallTable>(encoded).is_err(),
            "distance={distance}"
        );
    }
}

#[test]
fn previous_unqualified_output_kind_is_not_a_current_wire_role() {
    let mut encoded = serde_json::to_value(message_owner()).unwrap();
    let output = encoded["owners"][0]["outputs"][1].as_object_mut().unwrap();
    output.remove("role");
    output.insert("kind".into(), "assertion_predicate".into());
    assert!(serde_json::from_value::<SolvePureCallTable>(encoded).is_err());
}

#[test]
fn sequential_check_replay_requires_its_exact_owner_output_role() {
    let p = profile();
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    let mut table = SolvePureCallTable::builder(p);
    table
        .add_owner(
            identity(711),
            vec![],
            vec![
                SolvePureCallOutput::result(integer),
                SolvePureCallOutput::assertion_predicate(),
            ],
            span(711),
            |b, _, outputs| {
                let predicate = b.constant(SolveValue::boolean(false), span(712))?;
                let assertion = b.assertion_output(1, span(713))?;
                b.check_assertion(assertion, predicate, &[], span(714), |_, _, _| Ok(()))?;
                let value = b.constant(SolveValue::integer(p, 1).unwrap(), span(715))?;
                b.store(outputs[0], value, span(716))?;
                b.store(outputs[1], predicate, span(717))
            },
        )
        .unwrap();
    let table = table.finish();
    let encoded = serde_json::to_value(&table).unwrap();
    let decoded: SolvePureCallTable = serde_json::from_value(encoded.clone()).unwrap();
    assert_eq!(decoded, table);
    let directional = decoded.owners()[0].directional().unwrap();
    assert!(matches!(
        directional.body().operations()[1].operation(),
        SolveOperation::CheckAssertion {
            predicate_output: 1,
            message: SolveAssertionMessage::NoCaptures,
            ..
        }
    ));
    let mut tampered = encoded;
    let mut bad_association = tampered.clone();
    bad_association["owners"][0]["body"]["operations"][1]["operation"]["message_outputs"] =
        serde_json::json!([1]);
    assert!(serde_json::from_value::<SolvePureCallTable>(bad_association).is_err());
    tampered["owners"][0]["body"]["operations"][1]["operation"]["predicate_output"] = 0.into();
    assert!(serde_json::from_value::<SolvePureCallTable>(tampered).is_err());
    let standalone = serde_json::to_value(table.owners()[0].body()).unwrap();
    assert!(serde_json::from_value::<TypedProgram>(standalone).is_err());
}

#[test]
fn standalone_program_cannot_issue_an_assertion_capability() {
    assert!(
        TypedProgram::construct(profile(), |builder| {
            builder.assertion_output(0, span(721)).map(|_| ())
        })
        .is_err()
    );
}

#[test]
fn captureless_assertion_rejects_callback_work_and_unassociated_inputs() {
    for with_capture in [false, true] {
        assert_captureless_work_rejected(with_capture);
    }
}

fn assert_captureless_work_rejected(with_capture: bool) {
    let p = profile();
    let mut table = SolvePureCallTable::builder(p);
    let result = table.add_owner(
        identity(731),
        vec![],
        vec![SolvePureCallOutput::assertion_predicate()],
        span(731),
        |builder, _, outputs| {
            let condition = builder.constant(SolveValue::boolean(false), span(732))?;
            let assertion = builder.assertion_output(0, span(733))?;
            let captures = if with_capture {
                vec![condition]
            } else {
                vec![]
            };
            builder.check_assertion(
                assertion,
                condition,
                &captures,
                span(734),
                |message, _, _| {
                    if !with_capture {
                        message.constant(SolveValue::boolean(true), span(735))?;
                    }
                    Ok(())
                },
            )?;
            builder.store(outputs[0], condition, span(736))
        },
    );
    assert!(result.is_err());
}
