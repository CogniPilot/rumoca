use super::*;
use rumoca_ir_solve::{SolveAssertionLevel, SolveOperation};

fn checked_square(format: SolveRealFormat, level: SolveAssertionLevel) -> SolvePureCallTable {
    let p = profile(format);
    let real = SolveValueType::scalar(SolveScalarType::real(p));
    SolvePureCallTable::construct(p, |table| {
        table.add_owner(
            identity(930),
            vec![
                real.clone(),
                SolveValueType::scalar(SolveScalarType::Boolean),
                SolveValueType::scalar(SolveScalarType::integer(p)),
            ],
            vec![
                SolvePureCallOutput::result(real),
                SolvePureCallOutput::assertion_predicate_at_level(level),
            ],
            span(1),
            |b, inputs, outputs| {
                let valid = b.load(inputs[1], span(2))?;
                let assertion = b.assertion_output(1, span(3))?;
                b.check_assertion(assertion, valid, &[], span(4), |_, _, _| Ok(()))?;
                let x = b.load(inputs[0], span(5))?;
                let array = b.construct_aggregate(&[x, x], vec![2], span(6))?;
                let index = b.load(inputs[2], span(7))?;
                let value = b.project_element_dynamic(array, &[index], span(8))?;
                let squared = b.binary(SolveBinaryOperator::Multiply, value, x, span(9))?;
                b.store(outputs[0], squared, span(10))?;
                b.store(outputs[1], valid, span(11))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn directional_arguments(table: &SolvePureCallTable, valid: bool, index: i64) -> Vec<TypedValue> {
    let p = table.arithmetic();
    vec![
        TypedValue::scalar(&SolveValue::real(p, 3.0)),
        TypedValue::scalar(&SolveValue::real(p, 2.0)),
        TypedValue::scalar(&SolveValue::boolean(valid)),
        TypedValue::scalar(&SolveValue::integer(p, index).unwrap()),
    ]
}

#[test]
fn uncaptured_checks_preserve_nonzero_tangents_and_source_roles() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        for level in [SolveAssertionLevel::Error, SolveAssertionLevel::Warning] {
            let table = checked_square(format, level);
            let replayed: SolvePureCallTable =
                serde_json::from_value(serde_json::to_value(&table).unwrap()).unwrap();
            assert_eq!(table, replayed);
            let owner = &replayed.owners()[0];
            let directional = owner.directional().unwrap();
            assert_eq!(directional.outputs()[2].assertion_level(), Some(level));
            assert_eq!(owner.directional_primal_output_index(2), Some(1));
            assert_eq!(owner.directional_primal_output_index(1), None);
            assert!(matches!(
                directional.body().operations()[1].operation(),
                SolveOperation::CheckAssertion {
                    predicate_output: 2,
                    ..
                }
            ));
            let values = eval_pure_call_directional(
                &replayed,
                owner.id(),
                &directional_arguments(&replayed, true, 1),
            )
            .unwrap();
            assert_eq!(values.len(), 3);
            assert_eq!(values[0].elements(), [real_kind(format, 9.0)]);
            assert_eq!(values[1].elements(), [real_kind(format, 12.0)]);
            assert_eq!(values[2].elements(), [SolveValueKind::Boolean(true)]);
        }
    }
}

#[test]
fn directional_error_stops_before_later_bounds_with_original_identity() {
    let table = checked_square(SolveRealFormat::Binary64, SolveAssertionLevel::Error);
    let owner = &table.owners()[0];
    let Err(TypedProgramEvalError::AssertionFailed { failure }) =
        eval_pure_call_directional(&table, owner.id(), &directional_arguments(&table, false, 3))
    else {
        panic!("the authored assertion must win over later bounds");
    };
    assert_eq!(failure.owner(), owner.id());
    assert_eq!(failure.predicate_output(), 1);
    assert_eq!(failure.source_span(), span(4));
    assert!(failure.message_captures().is_empty());
    assert!(
        eval_pure_call_directional(&table, owner.id(), &directional_arguments(&table, true, 3),)
            .is_err(),
        "a successful check must retain the later bounds fault"
    );
}

#[test]
fn directional_warning_continues_and_later_fault_publishes_nothing() {
    let table = checked_square(SolveRealFormat::Binary64, SolveAssertionLevel::Warning);
    let owner = &table.owners()[0];
    let values =
        eval_pure_call_directional(&table, owner.id(), &directional_arguments(&table, false, 1))
            .unwrap();
    assert_eq!(
        values[0].elements(),
        [real_kind(SolveRealFormat::Binary64, 9.0)]
    );
    assert_eq!(
        values[1].elements(),
        [real_kind(SolveRealFormat::Binary64, 12.0)]
    );
    assert_eq!(values[2].elements(), [SolveValueKind::Boolean(false)]);
    let error =
        eval_pure_call_directional(&table, owner.id(), &directional_arguments(&table, false, 3))
            .expect_err("a warning must retain the later bounds fault and return no tuple");
    assert!(!matches!(
        error,
        TypedProgramEvalError::AssertionFailed { .. }
    ));
}

fn region_checks(upper: Option<i64>) -> SolvePureCallTable {
    let p = profile(SolveRealFormat::Binary64);
    let real = SolveValueType::scalar(SolveScalarType::real(p));
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    SolvePureCallTable::construct(p, |table| {
        table.add_owner(
            identity(940),
            vec![real.clone(), boolean.clone(), boolean.clone()],
            vec![
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::assertion_predicate(),
            ],
            span(15),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(16))?;
                let valid = b.load(inputs[1], span(17))?;
                let selected = b.load(inputs[2], span(18))?;
                let yes = b.constant(SolveValue::boolean(true), span(19))?;
                let values = if let Some(upper) = upper {
                    checked_fold(b, x, yes, upper, p)?
                } else {
                    checked_conditional(b, &[x, valid, selected], &[real.clone(), boolean.clone()])?
                };
                for (slot, value) in outputs.iter().zip(values) {
                    b.store(*slot, value, span(44))?;
                }
                Ok(())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn checked_fold<'p>(
    b: &mut TypedProgramBuilder<'p>,
    x: ProgramRegister<'p>,
    yes: ProgramRegister<'p>,
    upper: i64,
    p: SolveArithmeticProfile,
) -> Result<Vec<ProgramRegister<'p>>, SolveProgramConstructionError> {
    b.fold(
        StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: 0,
                display_name: "i".into(),
                lower: 1,
                upper,
                step: 1,
            }],
        },
        &[x, yes],
        &[x],
        span(20),
        |r, _, captures, binders, out| {
            let i = r.load(binders[0], span(21))?;
            let two = r.constant(SolveValue::integer(p, 2).unwrap(), span(22))?;
            let valid = r.compare(SolveCompareOperator::Less, i, two, span(23))?;
            let assertion = r.assertion_output(1, span(24))?;
            r.check_assertion(assertion, valid, &[], span(25), |_, _, _| Ok(()))?;
            let x = r.load(captures[0], span(26))?;
            let array = r.construct_aggregate(&[x], vec![1], span(27))?;
            let x = r.project_element_dynamic(array, &[i], span(28))?;
            let value = r.binary(SolveBinaryOperator::Multiply, x, x, span(29))?;
            r.store(out[0], value, span(30))?;
            r.store(out[1], valid, span(31))
        },
    )
}

fn checked_conditional<'p>(
    b: &mut TypedProgramBuilder<'p>,
    arguments: &[ProgramRegister<'p>],
    types: &[SolveValueType],
) -> Result<Vec<ProgramRegister<'p>>, SolveProgramConstructionError> {
    b.conditional(
        arguments[2],
        &arguments[..2],
        types.to_vec(),
        span(32),
        |r, captures, out| {
            let x = r.load(captures[0], span(33))?;
            let valid = r.load(captures[1], span(34))?;
            let assertion = r.assertion_output(1, span(35))?;
            r.check_assertion(assertion, valid, &[], span(36), |_, _, _| Ok(()))?;
            let value = r.binary(SolveBinaryOperator::Multiply, x, x, span(37))?;
            r.store(out[0], value, span(38))?;
            r.store(out[1], valid, span(39))
        },
        |r, captures, out| {
            let x = r.load(captures[0], span(40))?;
            let yes = r.constant(SolveValue::boolean(true), span(41))?;
            r.store(out[0], x, span(42))?;
            r.store(out[1], yes, span(43))
        },
    )
}

#[test]
fn directional_checks_retain_selected_region_and_fold_order() {
    for (upper, valid, selected, expected, tangent) in [
        (None, false, false, 3.0, 2.0),
        (None, true, true, 9.0, 12.0),
        (Some(0), false, false, 3.0, 2.0),
        (Some(1), false, false, 9.0, 12.0),
    ] {
        let table = region_checks(upper);
        let owner = &table.owners()[0];
        let p = table.arithmetic();
        let arguments = [
            TypedValue::scalar(&SolveValue::real(p, 3.0)),
            TypedValue::scalar(&SolveValue::real(p, 2.0)),
            TypedValue::scalar(&SolveValue::boolean(valid)),
            TypedValue::scalar(&SolveValue::boolean(selected)),
        ];
        let values = eval_pure_call_directional(&table, owner.id(), &arguments).unwrap();
        assert_eq!(
            values[0].elements(),
            [real_kind(SolveRealFormat::Binary64, expected)]
        );
        assert_eq!(
            values[1].elements(),
            [real_kind(SolveRealFormat::Binary64, tangent)]
        );
        assert_eq!(values[2].elements(), [SolveValueKind::Boolean(true)]);
    }
    for (upper, expected_span) in [(None, span(36)), (Some(3), span(25))] {
        let table = region_checks(upper);
        let owner = &table.owners()[0];
        let p = table.arithmetic();
        let arguments = [
            TypedValue::scalar(&SolveValue::real(p, 3.0)),
            TypedValue::scalar(&SolveValue::real(p, 2.0)),
            TypedValue::scalar(&SolveValue::boolean(false)),
            TypedValue::scalar(&SolveValue::boolean(true)),
        ];
        let Err(TypedProgramEvalError::AssertionFailed { failure }) =
            eval_pure_call_directional(&table, owner.id(), &arguments)
        else {
            panic!("selected check or second loop iteration must stop first");
        };
        assert_eq!(failure.predicate_output(), 1);
        assert_eq!(failure.source_span(), expected_span);
    }
}

#[test]
fn forwarding_and_effectful_maps_retain_directional_refusals() {
    let p = profile(SolveRealFormat::Binary64);
    let real = SolveValueType::scalar(SolveScalarType::real(p));
    let mut table = SolvePureCallTable::builder(p);
    let child = table
        .add_owner(
            identity(950),
            vec![],
            vec![
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::assertion_predicate(),
            ],
            span(50),
            |b, _, outputs| {
                let no = b.constant(SolveValue::boolean(false), span(51))?;
                let assertion = b.assertion_output(1, span(52))?;
                b.check_assertion(assertion, no, &[], span(53), |_, _, _| Ok(()))?;
                let value = b.constant(SolveValue::real(p, 17.0), span(54))?;
                b.store(outputs[0], value, span(55))?;
                b.store(outputs[1], no, span(56))
            },
        )
        .unwrap();
    let outputs = table.call_site(child).unwrap().outputs().to_vec();
    let parent = table
        .add_owner(identity(951), vec![], outputs, span(57), |b, _, outputs| {
            let call = b.emit_call(child, &[], span(58))?;
            let assertion = b.assertion_output(1, span(59))?;
            b.forward_assertion(&call, 1, assertion, span(60))?;
            for (slot, value) in outputs.iter().zip(call.registers()) {
                b.store(*slot, *value, span(61))?;
            }
            Ok(())
        })
        .unwrap();
    let mapped = table
        .add_owner(
            identity(952),
            vec![],
            vec![SolvePureCallOutput::result(
                SolveValueType::tensor(real.element_type(), vec![2]).unwrap(),
            )],
            span(62),
            |b, _, outputs| {
                let value = b.map(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            id: 0,
                            display_name: "i".into(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    &[],
                    real.clone(),
                    span(63),
                    |r, _, _, output| {
                        let values = r.call(child, &[], span(64))?;
                        r.store(output, values[0], span(65))
                    },
                )?;
                b.store(outputs[0], value, span(66))
            },
        )
        .unwrap();
    let table = table.finish();
    assert!(table.owner(child).unwrap().directional().is_some());
    assert!(table.owner(parent).unwrap().directional().is_none());
    assert!(table.owner(mapped).unwrap().directional().is_none());
    let Err(TypedProgramEvalError::AssertionFailed { failure }) =
        eval_pure_call(&table, mapped, &[])
    else {
        panic!("the mapped child check must remain observable in primal execution");
    };
    assert_eq!(failure.owner(), child);
    assert_eq!(failure.source_span(), span(53));
}
