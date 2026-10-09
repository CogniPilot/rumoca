use super::*;

#[derive(Clone, Copy)]
enum Mutation {
    None,
    TruePredicate,
    FalsePredicate,
    WrongMessage,
    MessageRead,
    Warning,
    Control,
    Call,
}

fn publication_fixture(mutation: Mutation) -> SolvePureCallTable {
    let p = profile();
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    let level = if matches!(mutation, Mutation::Warning) {
        SolveAssertionLevel::Warning
    } else {
        SolveAssertionLevel::Error
    };
    let mut table = SolvePureCallTable::builder(p);
    let child = if matches!(mutation, Mutation::Call) {
        Some(
            table
                .add_owner(
                    identity(800),
                    vec![],
                    vec![SolvePureCallOutput::result(boolean.clone())],
                    span(800),
                    |builder, _, outputs| {
                        let value = builder.constant(SolveValue::boolean(true), span(800))?;
                        builder.store(outputs[0], value, span(800))
                    },
                )
                .unwrap(),
        )
    } else {
        None
    };
    table
        .add_owner(
            identity(801),
            vec![boolean],
            vec![
                SolvePureCallOutput::assertion_predicate_at_level(level),
                SolvePureCallOutput::assertion_message_value(integer.clone(), 1),
                SolvePureCallOutput::result(integer),
            ],
            span(801),
            |b, inputs, outputs| {
                let condition = b.load(inputs[0], span(802))?;
                if let Some(child) = child {
                    b.call(child, &[], span(803))?;
                }
                if matches!(mutation, Mutation::Control) {
                    b.conditional(
                        condition,
                        &[],
                        vec![SolveValueType::scalar(SolveScalarType::Boolean)],
                        span(803),
                        store_true,
                        store_true,
                    )?;
                }
                let capability = b.assertion_output(0, span(803))?;
                let message =
                    b.check_assertion(capability, condition, &[], span(804), |m, _, outputs| {
                        let value = m.constant(SolveValue::integer(p, 7).unwrap(), span(805))?;
                        m.store(outputs[0], value, span(806))
                    })?;
                let value = b.constant(SolveValue::integer(p, 11).unwrap(), span(807))?;
                let predicate = match mutation {
                    Mutation::TruePredicate => b.constant(SolveValue::boolean(true), span(808))?,
                    Mutation::FalsePredicate => {
                        b.constant(SolveValue::boolean(false), span(808))?
                    }
                    _ => condition,
                };
                let capture = if matches!(mutation, Mutation::WrongMessage) {
                    value
                } else {
                    message[0]
                };
                let result = if matches!(mutation, Mutation::MessageRead) {
                    b.binary(
                        crate::SolveBinaryOperator::Add,
                        value,
                        message[0],
                        span(809),
                    )?
                } else {
                    value
                };
                b.store(outputs[0], predicate, span(810))?;
                b.store(outputs[1], capture, span(811))?;
                b.store(outputs[2], result, span(812))
            },
        )
        .unwrap();
    table.finish()
}

#[test]
fn direct_fatal_publication_is_derived_and_replayed_from_the_same_body() {
    for mutation in [
        Mutation::None,
        Mutation::TruePredicate,
        Mutation::Call,
        Mutation::Control,
        Mutation::Warning,
    ] {
        let table = publication_fixture(mutation);
        let proof = table.owners().last().unwrap().assertion_flow().unwrap();
        let operation = if matches!(mutation, Mutation::Call | Mutation::Control) {
            2
        } else {
            1
        };
        assert_eq!(
            proof
                .sources()
                .iter()
                .map(|source| source.operation())
                .collect::<Vec<_>>(),
            [operation]
        );
        let encoded = serde_json::to_value(&table).unwrap();
        assert!(encoded["owners"][0].get("assertion_flow").is_none());
        let replayed: SolvePureCallTable = serde_json::from_value(encoded).unwrap();
        assert_eq!(replayed, table);
        assert_eq!(
            replayed
                .owners()
                .last()
                .unwrap()
                .assertion_flow()
                .unwrap()
                .sources()
                .iter()
                .map(|source| source.operation())
                .collect::<Vec<_>>(),
            [operation]
        );
    }
}

#[test]
fn role_membership_cannot_certify_unrelated_publication() {
    for mutation in [
        Mutation::FalsePredicate,
        Mutation::WrongMessage,
        Mutation::MessageRead,
    ] {
        let table = publication_fixture(mutation);
        assert!(table.owners().last().unwrap().assertion_flow().is_none());
        let replayed: SolvePureCallTable =
            serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
        assert!(replayed.owners().last().unwrap().assertion_flow().is_none());
    }
}

fn store_true<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    _: &[super::super::ProgramSlot<'program>],
    outputs: &[super::super::ProgramSlot<'program>],
) -> Result<(), SolveProgramConstructionError> {
    let value = builder.constant(SolveValue::boolean(true), span(813))?;
    builder.store(outputs[0], value, span(814))
}

#[test]
fn replay_rejects_register_redefinition_and_output_slot_aliases() {
    let table = publication_fixture(Mutation::TruePredicate);
    let encoded = serde_json::to_value(table).unwrap();
    let mut duplicate_register = encoded.clone();
    duplicate_register["owners"][0]["body"]["operations"][3]["operation"]["destination"] = 0.into();
    assert!(serde_json::from_value::<SolvePureCallTable>(duplicate_register).is_err());
    let mut duplicate_store = encoded.clone();
    let operations = duplicate_store["owners"][0]["body"]["operations"]
        .as_array_mut()
        .unwrap();
    operations.push(operations[4].clone());
    assert!(serde_json::from_value::<SolvePureCallTable>(duplicate_store).is_err());
    let mut load_output = encoded;
    let body = &mut load_output["owners"][0]["body"];
    let mut load = body["operations"][0].clone();
    let register_types = body["register_types"].as_array_mut().unwrap();
    load["operation"]["destination"] = register_types.len().into();
    load["operation"]["slot"] = 1.into();
    register_types.push(register_types[0].clone());
    body["operations"].as_array_mut().unwrap().push(load);
    assert!(serde_json::from_value::<SolvePureCallTable>(load_output).is_err());
}

fn nested_region<'p>(
    r: &mut TypedProgramBuilder<'p>,
    result: &[ProgramSlot<'p>],
    escape: bool,
    p: SolveArithmeticProfile,
) -> Result<(), SolveProgramConstructionError> {
    let condition = r.constant(SolveValue::boolean(true), span(853))?;
    let cap = r.assertion_output(0, span(853))?;
    let captures = r.check_assertion(cap, condition, &[], span(854), |m, _, out| {
        let value = m.constant(SolveValue::integer(p, 23).unwrap(), span(855))?;
        m.store(out[0], value, span(855))
    })?;
    let value = if escape {
        captures[0]
    } else {
        r.constant(SolveValue::integer(p, 7).unwrap(), span(856))?
    };
    r.store(result[0], value, span(856))
}

fn nested_fixture(
    fold: bool,
    duplicate: bool,
    escape: bool,
    wrong_inactive: bool,
) -> SolvePureCallTable {
    use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
    let p = profile();
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    let mut table = SolvePureCallTable::builder(p);
    table
        .add_owner(
            identity(850),
            vec![],
            vec![
                SolvePureCallOutput::assertion_predicate(),
                SolvePureCallOutput::assertion_message_value(integer.clone(), 1),
                SolvePureCallOutput::result(integer.clone()),
            ],
            span(850),
            |b, _, outputs| {
                let yes = b.constant(SolveValue::boolean(true), span(851))?;
                let zero = b.constant(SolveValue::integer(p, 0).unwrap(), span(852))?;
                let values = if fold {
                    b.fold(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "i".into(),
                                lower: 1,
                                upper: 3,
                                step: 1,
                            }],
                        },
                        &[zero],
                        &[],
                        span(857),
                        |r, _, _, _, out| nested_region(r, out, escape, p),
                    )?
                } else {
                    b.conditional(
                        yes,
                        &[],
                        vec![integer.clone()],
                        span(857),
                        |r, _, out| nested_region(r, out, escape, p),
                        |r, _, out| nested_else(r, out, duplicate, escape, p),
                    )?
                };
                let inactive = b.constant(
                    SolveValue::inactive_assertion_message(integer.element_type()),
                    span(859),
                )?;
                let wrong = b.constant(SolveValue::integer(p, 1).unwrap(), span(859))?;
                b.store(outputs[0], yes, span(860))?;
                b.store(
                    outputs[1],
                    if wrong_inactive { wrong } else { inactive },
                    span(860),
                )?;
                b.store(outputs[2], values[0], span(860))
            },
        )
        .unwrap();
    table.finish()
}

fn nested_else<'p>(
    r: &mut TypedProgramBuilder<'p>,
    out: &[ProgramSlot<'p>],
    duplicate: bool,
    escape: bool,
    p: SolveArithmeticProfile,
) -> Result<(), SolveProgramConstructionError> {
    if duplicate {
        return nested_region(r, out, escape, p);
    }
    let value = r.constant(SolveValue::integer(p, 9).unwrap(), span(858))?;
    r.store(out[0], value, span(858))
}

#[test]
fn nested_fatal_flow_has_exact_region_coordinates_and_replays() {
    for (fold, kind) in [
        (false, AssertionRegionKind::ConditionalThen),
        (true, AssertionRegionKind::FoldTransition),
    ] {
        let table = nested_fixture(fold, false, false, false);
        let flow = table.owners()[0].assertion_flow().unwrap();
        assert_eq!(flow.sources().len(), 1);
        assert_eq!(flow.sources()[0].regions().len(), 1);
        assert_eq!(flow.sources()[0].regions()[0].kind(), kind);
        assert_eq!(flow.sources()[0].operation(), 1);
        let replay: SolvePureCallTable =
            serde_json::from_str(&serde_json::to_string(&table).unwrap()).unwrap();
        assert_eq!(replay, table);
        assert_eq!(replay.owners()[0].assertion_flow(), Some(flow));
    }
}

#[test]
fn nested_duplicate_sources_and_escaping_captures_have_no_flow() {
    for (fold, duplicate, escape) in [
        (false, true, false),
        (false, false, true),
        (true, false, true),
    ] {
        let table = nested_fixture(fold, duplicate, escape, false);
        assert!(table.owners()[0].assertion_flow().is_none());
        let replay: SolvePureCallTable =
            serde_json::from_str(&serde_json::to_string(&table).unwrap()).unwrap();
        assert!(replay.owners()[0].assertion_flow().is_none());
    }
    for fold in [false, true] {
        let table = nested_fixture(fold, false, false, true);
        assert!(table.owners()[0].assertion_flow().is_none());
        let replay: SolvePureCallTable =
            serde_json::from_str(&serde_json::to_string(&table).unwrap()).unwrap();
        assert!(replay.owners()[0].assertion_flow().is_none());
    }
}
