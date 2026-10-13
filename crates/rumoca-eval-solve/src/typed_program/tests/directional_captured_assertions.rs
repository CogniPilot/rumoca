use super::stopping_directional_call as eval_pure_call_directional;
use super::*;
use rumoca_ir_solve::SolveAssertionLevel;

#[test]
fn successful_directional_check_skips_faulty_message_at_original_source_point() {
    let table = super::assertions::assertion_then_fault_builder(SolveAssertionLevel::Error, true)
        .0
        .finish();
    let arguments = [TypedValue::scalar(&SolveValue::boolean(true))];
    let result = eval_pure_call_directional(&table, table.owners()[0].id(), &arguments);
    assert!(
        matches!(result, Err(TypedProgramEvalError::IntegerArithmetic { provenance, .. })
        if provenance == span(11))
    );
}

#[test]
fn failed_directional_check_keeps_message_fault_before_later_body_fault() {
    let table = super::assertions::assertion_then_fault_builder(SolveAssertionLevel::Error, true)
        .0
        .finish();
    let arguments = [TypedValue::scalar(&SolveValue::boolean(false))];
    let result = eval_pure_call_directional(&table, table.owners()[0].id(), &arguments);
    assert!(
        matches!(result, Err(TypedProgramEvalError::IntegerArithmetic { provenance, .. })
        if provenance == span(7))
    );
}

fn captured_table(format: SolveRealFormat, level: SolveAssertionLevel) -> SolvePureCallTable {
    let p = profile(format);
    let real = SolveValueType::scalar(SolveScalarType::real(p));
    SolvePureCallTable::construct(p, |table| {
        let child = table.add_owner(
            identity(1930),
            vec![
                real.clone(),
                SolveValueType::scalar(SolveScalarType::Boolean),
            ],
            vec![
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::assertion_predicate_at_level(level),
                SolvePureCallOutput::assertion_message_value(real.clone(), 1),
            ],
            span(1),
            |b, inputs, outputs| {
                let x = b.load(inputs[0], span(2))?;
                let valid = b.load(inputs[1], span(3))?;
                let assertion = b.assertion_output(1, span(4))?;
                let message =
                    b.check_assertion(assertion, valid, &[x], span(5), |m, captures, out| {
                        let x = m.load(captures[0], span(6))?;
                        let squared = m.binary(SolveBinaryOperator::Multiply, x, x, span(7))?;
                        m.store(out[0], squared, span(8))
                    })?;
                let squared = b.binary(SolveBinaryOperator::Multiply, x, x, span(9))?;
                b.store(outputs[0], squared, span(10))?;
                b.store(outputs[1], valid, span(11))?;
                b.store(outputs[2], message[0], span(12))
            },
        )?;
        let site = table.call_site(child).unwrap();
        table.add_owner(
            identity(1931),
            site.inputs().to_vec(),
            site.outputs().to_vec(),
            span(13),
            |b, inputs, outputs| {
                let arguments = inputs
                    .iter()
                    .map(|input| b.load(*input, span(14)))
                    .collect::<Result<Vec<_>, _>>()?;
                let call = b.emit_call(child, &arguments, span(15))?;
                let assertion = b.assertion_output(1, span(16))?;
                b.forward_assertion(&call, 1, assertion, span(17))?;
                for (slot, value) in outputs.iter().zip(call.registers()) {
                    b.store(*slot, *value, span(18))?;
                }
                Ok(())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn arguments(table: &SolvePureCallTable, valid: bool) -> Vec<TypedValue> {
    let p = table.arithmetic();
    vec![
        TypedValue::scalar(&SolveValue::real(p, 3.0)),
        TypedValue::scalar(&SolveValue::real(p, 2.0)),
        TypedValue::scalar(&SolveValue::boolean(valid)),
    ]
}

#[test]
fn captured_message_directional_error_retains_only_original_message_and_identity() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        let table = captured_table(format, SolveAssertionLevel::Error);
        let table: SolvePureCallTable =
            serde_json::from_value(serde_json::to_value(table).unwrap()).unwrap();
        assert!(
            table
                .owners()
                .iter()
                .all(|owner| owner.directional().is_some())
        );
        let parent = table.owners().last().unwrap();
        let Err(TypedProgramEvalError::AssertionFailed { failure }) =
            eval_pure_call_directional(&table, parent.id(), &arguments(&table, false))
        else {
            panic!("the original captured assertion must stop the directional call");
        };
        assert_eq!(failure.owner(), table.owners()[0].id());
        assert_eq!(failure.predicate_output(), 1);
        assert_eq!(failure.source_span(), span(5));
        assert_eq!(failure.message_captures().len(), 1);
        assert_eq!(failure.message_captures()[0].0, 2);
        assert_eq!(
            failure.message_captures()[0].1.elements(),
            [real_kind(format, 9.0)]
        );
        let values =
            eval_pure_call_directional(&table, parent.id(), &arguments(&table, true)).unwrap();
        assert_eq!(values[0].elements(), [real_kind(format, 9.0)]);
        assert_eq!(values[1].elements(), [real_kind(format, 12.0)]);
        assert_eq!(values[2].elements(), [SolveValueKind::Boolean(true)]);
        assert_eq!(values.len(), 5);
    }
}

#[test]
fn captured_message_directional_warning_preserves_message_tangent_and_single_observation() {
    use super::super::{InvocationCompletion, RecursionChain, eval_directional_owner};
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        let table = captured_table(format, SolveAssertionLevel::Warning);
        let parent = table.owners().last().unwrap();
        let InvocationCompletion::Complete {
            values,
            observations,
        } = eval_directional_owner(
            &table,
            parent,
            &arguments(&table, false),
            RecursionChain::ROOT,
        )
        .unwrap()
        else {
            panic!("a captured warning must preserve the completed call");
        };
        assert_eq!(values[0].elements(), [real_kind(format, 9.0)]);
        assert_eq!(values[1].elements(), [real_kind(format, 12.0)]);
        assert_eq!(values[3].elements(), [real_kind(format, 9.0)]);
        assert_eq!(values[4].elements(), [real_kind(format, 12.0)]);
        assert_eq!(observations.len(), 1);
        let observation = &observations[0].observation;
        assert_eq!(observation.owner, table.owners()[0].id());
        assert_eq!(observation.predicate_output, 1);
        assert_eq!(observation.provenance, span(5));
        assert_eq!(observation.captures.len(), 1);
        assert_eq!(observation.captures[0].0, 2);
        assert_eq!(
            observation.captures[0].1.elements(),
            [real_kind(format, 9.0)]
        );
    }
}
