use super::*;
use rumoca_ir_solve::SolveAssertionLevel;

fn assertion_then_fault(level: SolveAssertionLevel, faulty_message: bool) -> SolvePureCallTable {
    assertion_then_fault_builder(level, faulty_message)
        .0
        .finish()
}

pub(super) fn assertion_then_fault_builder(
    level: SolveAssertionLevel,
    faulty_message: bool,
) -> (
    rumoca_ir_solve::SolvePureCallTableBuilder,
    SolvePureCallOwnerId,
) {
    let p = profile(SolveRealFormat::Binary64);
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    let mut table = SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(901),
            vec![boolean],
            vec![
                SolvePureCallOutput::result(integer.clone()),
                SolvePureCallOutput::assertion_predicate_at_level(level),
                SolvePureCallOutput::assertion_message_value(integer, 1),
            ],
            span(1),
            |b, inputs, outputs| {
                let predicate = b.load(inputs[0], span(2))?;
                let assertion = b.assertion_output(1, span(3))?;
                let message =
                    b.check_assertion(assertion, predicate, &[], span(4), |region, _, outputs| {
                        let value =
                            region.constant(SolveValue::integer(p, 17).unwrap(), span(5))?;
                        let value = if faulty_message {
                            let zero =
                                region.constant(SolveValue::integer(p, 0).unwrap(), span(6))?;
                            region.binary(
                                SolveBinaryOperator::IntegerQuotient,
                                value,
                                zero,
                                span(7),
                            )?
                        } else {
                            value
                        };
                        region.store(outputs[0], value, span(8))
                    })?;
                let one = b.constant(SolveValue::integer(p, 1).unwrap(), span(9))?;
                let zero = b.constant(SolveValue::integer(p, 0).unwrap(), span(10))?;
                let value = b.binary(SolveBinaryOperator::IntegerQuotient, one, zero, span(11))?;
                b.store(outputs[0], value, span(12))?;
                b.store(outputs[1], predicate, span(13))?;
                b.store(outputs[2], message[0], span(14))
            },
        )
        .unwrap();
    (table, owner)
}

fn evaluate(
    table: &SolvePureCallTable,
    condition: bool,
) -> Result<Vec<TypedValue>, TypedProgramEvalError> {
    eval_pure_call(
        table,
        table.owners()[0].id(),
        &[TypedValue::scalar(&SolveValue::boolean(condition))],
    )
}

#[test]
fn fatal_assertion_stops_before_later_fault_and_uninitialized_result_reads() {
    let table = assertion_then_fault(SolveAssertionLevel::Error, false);
    let Err(TypedProgramEvalError::AssertionFailed { failure }) = evaluate(&table, false) else {
        panic!("fatal call returned a value or later fault");
    };
    assert_eq!(failure.source_span(), span(4));
    assert_eq!(failure.owner(), table.owners()[0].id());
    assert_eq!(failure.predicate_output(), 1);
    assert_eq!(
        failure.message_captures()[0].1.elements(),
        [SolveValueKind::Integer(17)]
    );
}

#[test]
fn warning_continues_to_the_later_numerical_fault() {
    let table = assertion_then_fault(SolveAssertionLevel::Warning, false);
    assert!(
        matches!(evaluate(&table, false), Err(TypedProgramEvalError::IntegerArithmetic { provenance, .. }) if provenance == span(11))
    );
}

#[test]
fn successful_assertion_does_not_evaluate_its_faulty_message() {
    let table = assertion_then_fault(SolveAssertionLevel::Error, true);
    assert!(
        matches!(evaluate(&table, true), Err(TypedProgramEvalError::IntegerArithmetic { provenance, .. }) if provenance == span(11))
    );
}

#[test]
fn failed_message_fault_occurs_before_the_later_body_fault() {
    let table = assertion_then_fault(SolveAssertionLevel::Error, true);
    assert!(
        matches!(evaluate(&table, false), Err(TypedProgramEvalError::IntegerArithmetic { provenance, .. }) if provenance == span(7))
    );
}

#[test]
fn stop_retains_only_its_initialized_message_capture() {
    use crate::typed_program::{RecursionChain, assertions::InvocationCompletion, eval_owner};
    let table = assertion_then_fault(SolveAssertionLevel::Error, false);
    let owner = &table.owners()[0];
    let outcome = eval_owner(
        &table,
        owner,
        &[TypedValue::scalar(&SolveValue::boolean(false))],
        RecursionChain::ROOT,
    )
    .unwrap();
    let InvocationCompletion::Stopped(stop) = outcome else {
        panic!("fatal call completed");
    };
    let [observed] = stop.observations.as_slice() else {
        panic!("one reached assertion");
    };
    let observation = &observed.observation;
    assert_eq!(observation.owner, owner.id());
    assert_eq!(observation.predicate_output, 1);
    assert_eq!(observation.captures.len(), 1);
    assert_eq!(observation.captures[0].0, 2);
    assert_eq!(
        observation.captures[0].1.elements(),
        [SolveValueKind::Integer(17)]
    );
}

fn nested_assertion_table() -> SolvePureCallTable {
    let p = profile(SolveRealFormat::Binary64);
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let (mut table, child) = assertion_then_fault_builder(SolveAssertionLevel::Error, false);
    table
        .add_owner(
            identity(902),
            vec![boolean],
            vec![SolvePureCallOutput::result(integer.clone())],
            span(30),
            |b, inputs, outputs| {
                let active = b.load(inputs[0], span(31))?;
                let value = b.conditional(
                    active,
                    &[],
                    vec![integer],
                    span(32),
                    |region, _, outputs| {
                        let invalid = region.constant(SolveValue::boolean(false), span(33))?;
                        let values = region.call(child, &[invalid], span(34))?;
                        region.store(outputs[0], values[0], span(35))
                    },
                    |region, _, outputs| {
                        let value =
                            region.constant(SolveValue::integer(p, 23).unwrap(), span(36))?;
                        region.store(outputs[0], value, span(37))
                    },
                )?;
                b.store(outputs[0], value[0], span(38))
            },
        )
        .unwrap();
    table.finish()
}

#[test]
fn nested_selected_call_preserves_first_assertion_origin_and_path() {
    use crate::typed_program::{RecursionChain, assertions::InvocationCompletion, eval_owner};
    let table = nested_assertion_table();
    let parent = &table.owners()[1];
    let outcome = eval_owner(
        &table,
        parent,
        &[TypedValue::scalar(&SolveValue::boolean(true))],
        RecursionChain::ROOT,
    )
    .unwrap();
    let InvocationCompletion::Stopped(stop) = outcome else {
        panic!("nested fatal call completed");
    };
    assert_eq!(
        stop.observations.last().unwrap().observation.owner,
        table.owners()[0].id()
    );
    assert_eq!(
        stop.observations.last().unwrap().observation.provenance,
        span(4)
    );
    assert_eq!(
        stop.observations
            .last()
            .unwrap()
            .invocation_path
            .first()
            .unwrap()
            .owner,
        table.owners()[0].id()
    );
    assert!(
        stop.observations
            .last()
            .unwrap()
            .invocation_path
            .iter()
            .skip(1)
            .all(|entry| entry.owner == parent.id())
    );
    let path = &stop.observations.last().unwrap().invocation_path;
    let child_invocation = path[0].invocation.as_ref().unwrap();
    assert!(std::ptr::eq(child_invocation.owner, &table.owners()[0]));
    assert_eq!(
        child_invocation.arguments[0].elements(),
        [SolveValueKind::Boolean(false)]
    );
    let parent_invocation = path.last().unwrap().invocation.as_ref().unwrap();
    assert!(std::ptr::eq(parent_invocation.owner, parent));
    assert_eq!(
        parent_invocation.arguments[0].elements(),
        [SolveValueKind::Boolean(true)]
    );
    assert!(std::ptr::eq(path[0].program, table.owners()[0].body()));
    assert!(std::ptr::eq(path.last().unwrap().program, parent.body()));
    assert!(!std::ptr::eq(path[1].program, parent.body()));
    assert!(
        path.iter()
            .all(|frame| frame.operation < frame.program.operations().len())
    );
    assert!(path.iter().all(|frame| frame.domain_point.is_none()));
    assert!(
        stop.observations.last().unwrap().invocation_path.len() >= 3,
        "child, selected region and parent retain their boundaries"
    );
}

#[test]
fn inactive_nested_call_does_not_evaluate_assertion_or_later_fault() {
    let table = nested_assertion_table();
    let values = eval_pure_call(
        &table,
        table.owners()[1].id(),
        &[TypedValue::scalar(&SolveValue::boolean(false))],
    )
    .unwrap();
    assert_eq!(values[0].elements(), [SolveValueKind::Integer(23)]);
}

fn two_assertions(preceding_fault: bool, first: SolveAssertionLevel) -> SolvePureCallTable {
    let p = profile(SolveRealFormat::Binary64);
    let mut table = SolvePureCallTable::builder(p);
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    table
        .add_owner(
            identity(903),
            vec![],
            vec![
                SolvePureCallOutput::result(integer),
                SolvePureCallOutput::assertion_predicate_at_level(first),
                SolvePureCallOutput::assertion_predicate(),
            ],
            span(40),
            |b, _, outputs| {
                let one = b.constant(SolveValue::integer(p, 1).unwrap(), span(41))?;
                let zero = b.constant(SolveValue::integer(p, 0).unwrap(), span(42))?;
                if preceding_fault {
                    b.binary(SolveBinaryOperator::IntegerQuotient, one, zero, span(43))?;
                }
                let predicate = b.constant(SolveValue::boolean(false), span(44))?;
                for output in [1, 2] {
                    let assertion = b.assertion_output(output, span(45))?;
                    b.check_assertion(assertion, predicate, &[], span(45 + output), |_, _, _| {
                        Ok(())
                    })?;
                }
                b.store(outputs[0], one, span(48))?;
                b.store(outputs[1], predicate, span(49))?;
                b.store(outputs[2], predicate, span(50))
            },
        )
        .unwrap();
    table.finish()
}

#[test]
fn multiple_false_assertions_report_the_first_only() {
    let table = two_assertions(false, SolveAssertionLevel::Error);
    let Err(TypedProgramEvalError::AssertionFailed { failure }) =
        eval_pure_call(&table, table.owners()[0].id(), &[])
    else {
        panic!("expected first assertion");
    };
    assert_eq!(failure.predicate_output(), 1);
    assert_eq!(failure.source_span(), span(46));
    assert!(failure.message_captures().is_empty());
}

#[test]
fn preceding_numerical_fault_keeps_its_origin_before_assertions() {
    let table = two_assertions(true, SolveAssertionLevel::Error);
    assert!(
        matches!(eval_pure_call(&table, table.owners()[0].id(), &[]), Err(TypedProgramEvalError::IntegerArithmetic { provenance, .. }) if provenance == span(43))
    );
}

#[test]
fn nested_assertion_in_failed_message_stops_before_outer_message_publication() {
    let p = profile(SolveRealFormat::Binary64);
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    let mut table = SolvePureCallTable::builder(p);
    table
        .add_owner(
            identity(904),
            vec![],
            vec![
                SolvePureCallOutput::result(integer.clone()),
                SolvePureCallOutput::assertion_predicate(),
                SolvePureCallOutput::assertion_predicate(),
                SolvePureCallOutput::assertion_message_value(integer, 2),
            ],
            span(51),
            |b, _, outputs| {
                let predicate = b.constant(SolveValue::boolean(false), span(52))?;
                let assertion = b.assertion_output(1, span(53))?;
                let messages = b.check_assertion(
                    assertion,
                    predicate,
                    &[],
                    span(54),
                    |region, _, outputs| {
                        let predicate = region.constant(SolveValue::boolean(false), span(55))?;
                        let assertion = region.assertion_output(2, span(56))?;
                        region.check_assertion(
                            assertion,
                            predicate,
                            &[],
                            span(57),
                            |_, _, _| Ok(()),
                        )?;
                        let value =
                            region.constant(SolveValue::integer(p, 17).unwrap(), span(58))?;
                        region.store(outputs[0], value, span(59))
                    },
                )?;
                let value = b.constant(SolveValue::integer(p, 1).unwrap(), span(60))?;
                for (output, value) in
                    outputs
                        .iter()
                        .zip([value, predicate, predicate, messages[0]])
                {
                    b.store(*output, value, span(61))?;
                }
                Ok(())
            },
        )
        .unwrap();
    let table = table.finish();
    let Err(TypedProgramEvalError::AssertionFailed { failure }) =
        eval_pure_call(&table, table.owners()[0].id(), &[])
    else {
        panic!("nested message assertion did not stop");
    };
    assert_eq!(failure.predicate_output(), 2);
    assert_eq!(failure.source_span(), span(57));
    assert!(failure.message_captures().is_empty());
}

#[test]
fn fold_stops_at_first_failed_iteration_without_publishing_carried_results() {
    let p = profile(SolveRealFormat::Binary64);
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    };
    let mut table = SolvePureCallTable::builder(p);
    table
        .add_owner(
            identity(905),
            vec![],
            vec![
                SolvePureCallOutput::result(integer.clone()),
                SolvePureCallOutput::assertion_predicate(),
                SolvePureCallOutput::assertion_message_value(integer.clone(), 1),
            ],
            span(70),
            |b, _, outputs| {
                let zero = b.constant(SolveValue::integer(p, 0).unwrap(), span(71))?;
                let folded = b.fold(
                    domain,
                    &[zero],
                    &[],
                    span(72),
                    |region, _, _, binders, outputs| {
                        let i = region.load(binders[0], span(73))?;
                        let two = region.constant(SolveValue::integer(p, 2).unwrap(), span(74))?;
                        let valid = region.compare(SolveCompareOperator::Less, i, two, span(75))?;
                        let assertion = region.assertion_output(1, span(76))?;
                        region.check_assertion(
                            assertion,
                            valid,
                            &[i],
                            span(77),
                            |message, inputs, outputs| {
                                let i = message.load(inputs[0], span(78))?;
                                message.store(outputs[0], i, span(79))
                            },
                        )?;
                        let three =
                            region.constant(SolveValue::integer(p, 3).unwrap(), span(80))?;
                        let third =
                            region.compare(SolveCompareOperator::Equal, i, three, span(81))?;
                        let value = region.conditional(
                            third,
                            &[i],
                            vec![integer],
                            span(82),
                            |arm, inputs, outputs| {
                                let i = arm.load(inputs[0], span(83))?;
                                let zero =
                                    arm.constant(SolveValue::integer(p, 0).unwrap(), span(84))?;
                                let bad = arm.binary(
                                    SolveBinaryOperator::IntegerQuotient,
                                    i,
                                    zero,
                                    span(85),
                                )?;
                                arm.store(outputs[0], bad, span(86))
                            },
                            |arm, inputs, outputs| {
                                let i = arm.load(inputs[0], span(87))?;
                                arm.store(outputs[0], i, span(88))
                            },
                        )?;
                        region.store(outputs[0], value[0], span(89))
                    },
                )?;
                b.store(outputs[0], folded[0], span(90))?;
                let valid = b.constant(SolveValue::boolean(true), span(91))?;
                b.store(outputs[1], valid, span(92))?;
                b.store(outputs[2], zero, span(93))
            },
        )
        .unwrap();
    let table = table.finish();
    let Err(TypedProgramEvalError::AssertionFailed { failure }) =
        eval_pure_call(&table, table.owners()[0].id(), &[])
    else {
        panic!("fold published a value or later iteration fault");
    };
    assert_fold_stop_path(&table);
    assert_eq!(failure.source_span(), span(77));
    assert_eq!(
        failure.message_captures()[0].1.elements(),
        [SolveValueKind::Integer(2)]
    );
}

#[test]
fn warning_observations_remain_private_and_ordered_before_the_fatal_stop() {
    use crate::typed_program::{RecursionChain, assertions::InvocationCompletion, eval_owner};
    let table = two_assertions(false, SolveAssertionLevel::Warning);
    let owner = &table.owners()[0];
    let outcome = eval_owner(&table, owner, &[], RecursionChain::ROOT).unwrap();
    let InvocationCompletion::Stopped(stop) = outcome else {
        panic!("second assertion did not stop");
    };
    let outputs = stop
        .observations
        .iter()
        .map(|observation| observation.observation.predicate_output)
        .collect::<Vec<_>>();
    assert_eq!(outputs, [1, 2]);
    assert_eq!(stop.observations[0].observation.provenance, span(46));
    assert_eq!(stop.observations[1].observation.provenance, span(47));
}

fn assert_fold_stop_path(table: &SolvePureCallTable) {
    use crate::typed_program::{InvocationCompletion, RecursionChain, eval_owner};
    let owner = &table.owners()[0];
    let InvocationCompletion::Stopped(stop) =
        eval_owner(table, owner, &[], RecursionChain::ROOT).unwrap()
    else {
        panic!("fold completed during private observation");
    };
    let path = &stop.observations.last().unwrap().invocation_path;
    assert_eq!(
        path.last().unwrap().domain_point.as_deref(),
        Some([2].as_slice())
    );
    assert!(std::ptr::eq(path.last().unwrap().program, owner.body()));
    assert!(!std::ptr::eq(path[0].program, owner.body()));
}
