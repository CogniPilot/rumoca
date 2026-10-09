use super::*;
use crate::typed_program::{InvocationCompletion, RecursionChain, eval_owner};

fn region_child(table: &mut rumoca_ir_solve::SolvePureCallTableBuilder) -> SolvePureCallOwnerId {
    let p = profile(SolveRealFormat::Binary64);
    let integer = SolveValueType::scalar(SolveScalarType::integer(p));
    table
        .add_owner(
            identity(960),
            vec![integer.clone()],
            vec![
                SolvePureCallOutput::result(integer.clone()),
                SolvePureCallOutput::assertion_predicate(),
                SolvePureCallOutput::assertion_message_value(integer, 1),
            ],
            span(960),
            |b, inputs, outputs| {
                let i = b.load(inputs[0], span(961))?;
                let two = b.constant(SolveValue::integer(p, 2).unwrap(), span(962))?;
                later_iteration_fault(b, i, p)?;
                let condition = b.compare(SolveCompareOperator::Less, i, two, span(963))?;
                let cap = b.assertion_output(1, span(964))?;
                let message =
                    b.check_assertion(cap, condition, &[i], span(965), |m, captures, out| {
                        let value = m.load(captures[0], span(966))?;
                        m.store(out[0], value, span(966))
                    })?;
                b.store(outputs[0], i, span(967))?;
                b.store(outputs[1], condition, span(967))?;
                b.store(outputs[2], message[0], span(967))
            },
        )
        .unwrap()
}

fn later_iteration_fault<'p>(
    b: &mut TypedProgramBuilder<'p>,
    i: ProgramRegister<'p>,
    p: SolveArithmeticProfile,
) -> Result<(), SolveProgramConstructionError> {
    let three = b.constant(SolveValue::integer(p, 3).unwrap(), span(968))?;
    let is_three = b.compare(SolveCompareOperator::Equal, i, three, span(968))?;
    b.conditional(
        is_three,
        &[i],
        vec![SolveValueType::scalar(SolveScalarType::integer(p))],
        span(968),
        |r, captures, out| {
            let value = r.load(captures[0], span(969))?;
            let zero = r.constant(SolveValue::integer(p, 0).unwrap(), span(969))?;
            let fault = r.binary(SolveBinaryOperator::IntegerQuotient, value, zero, span(969))?;
            r.store(out[0], fault, span(969))
        },
        |r, captures, out| {
            let value = r.load(captures[0], span(968))?;
            r.store(out[0], value, span(968))
        },
    )?;
    Ok(())
}

fn forward_region<'p>(
    b: &mut TypedProgramBuilder<'p>,
    argument: ProgramRegister<'p>,
    result: ProgramSlot<'p>,
    child: SolvePureCallOwnerId,
) -> Result<(), SolveProgramConstructionError> {
    let call = b.emit_call(child, &[argument], span(970))?;
    let parent = b.assertion_output(1, span(971))?;
    b.forward_assertion(&call, 1, parent, span(972))?;
    b.store(result, call.registers()[0], span(973))
}

fn region_table(fold_upper: Option<i64>, later_fault: bool) -> SolvePureCallTable {
    let p = profile(SolveRealFormat::Binary64);
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let mut table = SolvePureCallTable::builder(p);
    let child = region_child(&mut table);
    let interface = table.call_site(child).unwrap();
    table
        .add_owner(
            identity(974),
            vec![boolean],
            interface.outputs().to_vec(),
            span(974),
            |b, input, outputs| {
                let yes = b.constant(SolveValue::boolean(true), span(975))?;
                let zero = b.constant(
                    SolveValue::inactive_assertion_message(SolveScalarType::integer(p)),
                    span(976),
                )?;
                let result = if let Some(upper) = fold_upper {
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
                        &[zero],
                        &[],
                        span(977),
                        |r, _, _, binders, out| {
                            let i = r.load(binders[0], span(978))?;
                            forward_region(r, i, out[0], child)
                        },
                    )?
                } else {
                    let selected = b.load(input[0], span(979))?;
                    b.conditional(
                        selected,
                        &[],
                        vec![SolveValueType::scalar(SolveScalarType::integer(p))],
                        span(980),
                        |r, _, out| {
                            let two = r.constant(SolveValue::integer(p, 2).unwrap(), span(981))?;
                            forward_region(r, two, out[0], child)
                        },
                        |r, _, out| {
                            let nine = r.constant(SolveValue::integer(p, 9).unwrap(), span(982))?;
                            r.store(out[0], nine, span(982))
                        },
                    )?
                };
                if later_fault {
                    b.binary(
                        SolveBinaryOperator::IntegerQuotient,
                        result[0],
                        zero,
                        span(983),
                    )?;
                }
                b.store(outputs[0], result[0], span(984))?;
                b.store(outputs[1], yes, span(984))?;
                b.store(outputs[2], zero, span(984))
            },
        )
        .unwrap();
    table.finish()
}

#[test]
fn checked_selected_forwarding_is_lazy_and_keeps_exact_region_membership() {
    let table = region_table(None, false);
    let parent = &table.owners()[1];
    assert!(parent.assertion_flow().is_some());
    let inactive = eval_pure_call(
        &table,
        parent.id(),
        &[TypedValue::scalar(&SolveValue::boolean(false))],
    )
    .unwrap();
    assert_eq!(inactive[0].elements(), [SolveValueKind::Integer(9)]);
    assert_eq!(inactive[1].elements(), [SolveValueKind::Boolean(true)]);
    assert_eq!(inactive[2].elements(), [SolveValueKind::Integer(0)]);
    let InvocationCompletion::Stopped(stop) = eval_owner(
        &table,
        parent,
        &[TypedValue::scalar(&SolveValue::boolean(true))],
        RecursionChain::ROOT,
    )
    .unwrap() else {
        panic!("selected failure completed");
    };
    let observed = &stop.observations[0];
    assert_eq!(observed.observation.provenance, span(965));
    assert_eq!(
        observed.observation.captures[0].1.elements(),
        [SolveValueKind::Integer(2)]
    );
    assert_eq!(observed.invocation_path.len(), 3);
    assert!(std::ptr::eq(
        observed.invocation_path[0].program,
        table.owners()[0].body()
    ));
    assert!(!std::ptr::eq(
        observed.invocation_path[1].program,
        parent.body()
    ));
    assert!(std::ptr::eq(
        observed.invocation_path[2].program,
        parent.body()
    ));
}

#[test]
fn checked_fold_forwarding_stops_at_actual_second_invocation_before_caller_fault() {
    let table = region_table(Some(3), true);
    let parent = &table.owners()[1];
    assert!(parent.assertion_flow().is_some());
    let InvocationCompletion::Stopped(stop) = eval_owner(
        &table,
        parent,
        &[TypedValue::scalar(&SolveValue::boolean(false))],
        RecursionChain::ROOT,
    )
    .unwrap() else {
        panic!("fold failure completed");
    };
    assert_eq!(stop.observations.len(), 1);
    let observed = &stop.observations[0];
    assert_eq!(
        observed.observation.captures[0].1.elements(),
        [SolveValueKind::Integer(2)]
    );
    assert_eq!(
        observed
            .invocation_path
            .last()
            .unwrap()
            .domain_point
            .as_deref(),
        Some([2].as_slice())
    );
    let invocation = observed.invocation_path[0].invocation.as_ref().unwrap();
    assert_eq!(
        invocation.arguments[0].elements(),
        [SolveValueKind::Integer(2)]
    );
    let later = eval_pure_call(
        &table,
        table.owners()[0].id(),
        &[TypedValue::scalar(
            &SolveValue::integer(table.arithmetic(), 3).unwrap(),
        )],
    );
    assert!(
        matches!(later, Err(TypedProgramEvalError::IntegerArithmetic { provenance, .. }) if provenance == span(969))
    );
}

#[test]
fn checked_zero_iteration_and_successful_fold_publish_only_inactive_cells() {
    for (upper, expected) in [(0, 0), (1, 1)] {
        let table = region_table(Some(upper), false);
        let parent = &table.owners()[1];
        assert!(parent.assertion_flow().is_some());
        let values = eval_pure_call(
            &table,
            parent.id(),
            &[TypedValue::scalar(&SolveValue::boolean(false))],
        )
        .unwrap();
        assert_eq!(values[0].elements(), [SolveValueKind::Integer(expected)]);
        assert_eq!(values[1].elements(), [SolveValueKind::Boolean(true)]);
        assert_eq!(values[2].elements(), [SolveValueKind::Integer(0)]);
    }
}

fn continuation_table(enter: bool) -> SolvePureCallTable {
    let p = profile(SolveRealFormat::Binary64);
    let mut table = SolvePureCallTable::builder(p);
    let child = region_child(&mut table);
    let interface = table.call_site(child).unwrap();
    table
        .add_owner(
            identity(990),
            vec![],
            interface.outputs().to_vec(),
            span(990),
            |b, _, outputs| {
                let one = b.constant(SolveValue::integer(p, 1).unwrap(), span(991))?;
                let values = b.fold_while(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            id: 0,
                            display_name: "i".into(),
                            lower: 1,
                            upper: 3,
                            step: 1,
                        }],
                    },
                    &[one],
                    &[],
                    span(992),
                    |r, carried, _, out| continuation_region(r, carried[0], out[0], child, enter),
                    |r, carried, _, _, out| {
                        let previous = r.load(carried[0], span(993))?;
                        let one = r.constant(SolveValue::integer(p, 1).unwrap(), span(993))?;
                        let next = r.binary(SolveBinaryOperator::Add, previous, one, span(993))?;
                        r.store(out[0], next, span(993))
                    },
                )?;
                let yes = b.constant(SolveValue::boolean(true), span(994))?;
                let zero = b.constant(
                    SolveValue::inactive_assertion_message(SolveScalarType::integer(p)),
                    span(994),
                )?;
                b.store(outputs[0], values[0], span(994))?;
                b.store(outputs[1], yes, span(994))?;
                b.store(outputs[2], zero, span(994))
            },
        )
        .unwrap();
    table.finish()
}

fn continuation_region<'p>(
    r: &mut TypedProgramBuilder<'p>,
    carried: ProgramSlot<'p>,
    out: ProgramSlot<'p>,
    child: SolvePureCallOwnerId,
    enter: bool,
) -> Result<(), SolveProgramConstructionError> {
    let selected = r.constant(SolveValue::boolean(enter), span(995))?;
    let argument = r.load(carried, span(995))?;
    let values = r.conditional(
        selected,
        &[argument],
        vec![SolveValueType::scalar(SolveScalarType::Boolean)],
        span(995),
        |r, captures, out| {
            let argument = r.load(captures[0], span(996))?;
            let call = r.emit_call(child, &[argument], span(996))?;
            let cap = r.assertion_output(1, span(996))?;
            r.forward_assertion(&call, 1, cap, span(996))?;
            let yes = r.constant(SolveValue::boolean(true), span(996))?;
            r.store(out[0], yes, span(996))
        },
        |r, _, out| {
            let no = r.constant(SolveValue::boolean(false), span(997))?;
            r.store(out[0], no, span(997))
        },
    )?;
    r.store(out, values[0], span(998))
}

#[test]
fn checked_continuation_selected_forwarding_retains_path_and_stops_before_transition() {
    let inactive = continuation_table(false);
    let values = eval_pure_call(&inactive, inactive.owners()[1].id(), &[]).unwrap();
    assert_eq!(values[0].elements(), [SolveValueKind::Integer(1)]);
    let table = continuation_table(true);
    let owner = &table.owners()[1];
    let flow = owner.assertion_flow().unwrap();
    assert_eq!(flow.sources()[0].regions().len(), 2);
    assert_eq!(
        flow.sources()[0].regions()[0].kind(),
        rumoca_ir_solve::AssertionRegionKind::FoldContinuation
    );
    assert_eq!(
        flow.sources()[0].regions()[1].kind(),
        rumoca_ir_solve::AssertionRegionKind::ConditionalThen
    );
    let InvocationCompletion::Stopped(stop) =
        eval_owner(&table, owner, &[], RecursionChain::ROOT).unwrap()
    else {
        panic!("continuation failure completed");
    };
    let observation = &stop.observations[0];
    assert_eq!(observation.invocation_path.len(), 4);
    assert_eq!(
        observation
            .invocation_path
            .last()
            .unwrap()
            .domain_point
            .as_deref(),
        Some([2].as_slice())
    );
    assert_eq!(
        observation.observation.captures[0].1.elements(),
        [SolveValueKind::Integer(2)]
    );
}
