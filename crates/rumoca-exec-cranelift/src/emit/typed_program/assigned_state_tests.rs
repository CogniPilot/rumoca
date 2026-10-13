//! Assigned-state operations execute natively and agree with the typed interpreter.

use super::*;
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_eval_solve::{PureCallInvocation, TypedValue};

fn at(offset: usize) -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_assigned_state.mo"),
        offset,
        offset + 1,
    )
}

fn profile() -> solve::SolveArithmeticProfile {
    solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    )
}

fn identity(value: u64) -> solve::SolvePureCallIdentity {
    solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(value).unwrap())
}

fn real() -> solve::SolveValueType {
    solve::SolveValueType::scalar(solve::SolveScalarType::real(profile()))
}

fn boolean() -> solve::SolveValueType {
    solve::SolveValueType::scalar(solve::SolveScalarType::Boolean)
}

fn domain(upper: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper,
            step: 1,
        }],
    }
}

fn native(table: &solve::SolvePureCallTable, owner: usize, input: &[f64]) -> Vec<f64> {
    let compiled = crate::compile_pure_call_table(table).unwrap();
    let site = table.owners()[owner].call_site();
    assert!(compiled.executes(PureCallInvocation::Primal(&site)));
    let mut output = vec![f64::NAN; site.output_scalar_count().unwrap()];
    compiled
        .call_scalar_payload(
            PureCallInvocation::Primal(&site),
            input,
            &mut output,
            &mut Vec::new(),
            &mut Vec::new(),
        )
        .unwrap();
    output
}

/// The typed interpreter's value row of `owner`, as the f64 scalars the native
/// payload carries.
fn interpreted(table: &solve::SolvePureCallTable, owner: usize, input: &[f64]) -> Vec<f64> {
    let site = table.owners()[owner].call_site();
    let mut offset = 0;
    let arguments = site
        .inputs()
        .iter()
        .map(|value_type| {
            let end = offset + value_type.scalar_count() as usize;
            let elements = input[offset..end]
                .iter()
                .map(|value| rumoca_eval_solve::typed_kind_from_scalar(*value, value_type).unwrap())
                .collect();
            offset = end;
            TypedValue::construct(value_type.clone(), elements).unwrap()
        })
        .collect::<Vec<_>>();
    rumoca_eval_solve::eval_pure_call(table, table.owners()[owner].id(), &arguments)
        .expect("a value row never stops on an assertion")
        .iter()
        .flat_map(|value| value.elements().iter().copied())
        .map(|kind| match kind {
            solve::SolveValueKind::Real32(bits) => f64::from(f32::from_bits(bits)),
            solve::SolveValueKind::Real64(bits) => f64::from_bits(bits),
            solve::SolveValueKind::Integer(value) => value as f64,
            solve::SolveValueKind::Boolean(value) => f64::from(value),
        })
        .collect()
}

/// One pass: add the binder to the carried sum, plus one when `read` selects
/// the presence of the captured state.
fn state_fold_pass<'program>(
    r: &mut solve::TypedProgramBuilder<'program>,
    (states, carried, binders, outputs): (
        &[solve::ProgramAssignedState<'program>],
        &[solve::ProgramSlot<'program>],
        &[solve::ProgramSlot<'program>],
        &[solve::ProgramSlot<'program>],
    ),
    read: bool,
) -> Result<(), solve::SolveProgramConstructionError> {
    let p = profile();
    let old = r.load(carried[0], at(1105))?;
    let index = r.load(binders[0], at(1106))?;
    let index = r.convert(
        solve::SolveConversionOperator::IntegerToReal,
        index,
        at(1107),
    )?;
    let mut sum = r.binary(solve::SolveBinaryOperator::Add, old, index, at(1108))?;
    if read {
        let present = r.assigned_state_is_present(states[0], at(1109))?;
        let one = r.constant(solve::SolveValue::real(p, 1.0), at(1110))?;
        let none = r.constant(solve::SolveValue::real(p, 0.0), at(1111))?;
        let bonus = r.select(present, one, none, at(1112))?;
        sum = r.binary(solve::SolveBinaryOperator::Add, sum, bonus, at(1113))?;
    }
    r.store(outputs[0], sum, at(1114))
}

/// The sum of `real(i)` for `i in 1:last`, plus one per pass when the body
/// reads the presence of the captured present state.
fn state_fold(read: bool, last: i64) -> solve::SolvePureCallTable {
    let p = profile();
    solve::SolvePureCallTable::construct(p, |table| {
        table.add_owner(
            identity(95),
            vec![],
            vec![solve::SolvePureCallOutput::result(real())],
            at(1100),
            |b, _, outputs| {
                let seven = b.constant(solve::SolveValue::real(p, 7.0), at(1101))?;
                let state = b.present_assigned_state(seven, at(1102))?;
                let zero = b.constant(solve::SolveValue::real(p, 0.0), at(1103))?;
                let result = b.fold_with_assigned(
                    domain(last),
                    &[zero],
                    &[],
                    &[state],
                    at(1104),
                    |r, inputs, carried, _, binders, outputs| {
                        state_fold_pass(r, (inputs, carried, binders, outputs), read)
                    },
                )?;
                b.store(outputs[0], result[0], at(1115))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn captured_present_state_is_copied_into_the_fold_frame_only_for_reads() {
    for (read, expected) in [(false, 15.0), (true, 20.0)] {
        let table = state_fold(read, 5);
        assert_eq!(native(&table, 0, &[]), [expected]);
        assert_eq!(interpreted(&table, 0, &[]), [expected]);
    }
}

fn selected_read_transition<'program>(
    region: &mut solve::TypedProgramBuilder<'program>,
    states: &[solve::ProgramAssignedState<'program>],
    carried: &[solve::ProgramSlot<'program>],
    outputs: &[solve::ProgramSlot<'program>],
    read: bool,
) -> Result<(), solve::SolveProgramConstructionError> {
    let value = region.load(carried[0], at(1204))?;
    let selected = region.constant(solve::SolveValue::boolean(read), at(1205))?;
    let (normal, _) = region.conditional_assigned(
        selected,
        (&[value], states),
        (vec![real()], vec![]),
        at(1206),
        |branch, inputs, states, outputs| {
            let assertion = branch.assertion_output(1, at(1207))?;
            let _ = branch.require_assigned_state(assertion, states[0], at(1208))?;
            let value = branch.load(inputs[0], at(1209))?;
            branch.store(outputs[0], value, at(1210))
        },
        |branch, inputs, _, outputs| {
            let value = branch.load(inputs[0], at(1211))?;
            branch.store(outputs[0], value, at(1212))
        },
    )?;
    region.store(outputs[0], normal[0], at(1213))
}

/// A fold over an absent captured state whose body reads the state under a
/// checked assertion when `read` selects it, optionally under a continuation
/// predicate that proceeds as `continuation` says.
fn absent_fold(upper: i64, continuation: Option<bool>, read: bool) -> solve::SolvePureCallTable {
    let p = profile();
    solve::SolvePureCallTable::construct(p, |table| {
        table.add_owner(
            identity(1201),
            vec![],
            vec![
                solve::SolvePureCallOutput::result(real()),
                solve::SolvePureCallOutput::assertion_predicate_at_level(
                    solve::SolveAssertionLevel::Error,
                ),
            ],
            at(1201),
            |builder, _, outputs| {
                let absent = builder.absent_assigned_state(real(), at(1202))?;
                let initial = builder.constant(solve::SolveValue::real(p, 7.0), at(1203))?;
                let values = match continuation {
                    Some(proceed) => builder.fold_while_assigned(
                        domain(upper),
                        &[initial],
                        (&[], &[absent]),
                        at(1214),
                        |predicate, _, _, _, _, outputs| {
                            let value = predicate
                                .constant(solve::SolveValue::boolean(proceed), at(1215))?;
                            predicate.store(outputs[0], value, at(1216))
                        },
                        |region, states, carried, _, _, outputs| {
                            selected_read_transition(region, states, carried, outputs, read)
                        },
                    )?,
                    None => builder.fold_with_assigned(
                        domain(upper),
                        &[initial],
                        &[],
                        &[absent],
                        at(1214),
                        |region, states, carried, _, _, outputs| {
                            selected_read_transition(region, states, carried, outputs, read)
                        },
                    )?,
                };
                builder.store(outputs[0], values[0], at(1217))?;
                let inactive = builder.constant(solve::SolveValue::boolean(true), at(1218))?;
                builder.store(outputs[1], inactive, at(1219))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn absent_capture_folds_agree_with_the_interpreter_with_and_without_a_continuation() {
    for (upper, continuation, read) in [
        (0, None, true),
        (2, Some(false), true),
        (2, Some(true), true),
        (2, None, false),
        (2, None, true),
        (3, Some(true), false),
    ] {
        let table = absent_fold(upper, continuation, read);
        assert_eq!(
            native(&table, 0, &[]),
            interpreted(&table, 0, &[]),
            "upper {upper} continuation {continuation:?} read {read}"
        );
        assert_eq!(native(&table, 0, &[]), [7.0, 1.0]);
    }
}

/// Owner 0 assigns its state in the selected branch of a conditional and
/// requires it under a checked assertion; owner 1 calls owner 0 in one arm.
fn assigned_pair() -> solve::SolvePureCallTable {
    let p = profile();
    solve::SolvePureCallTable::construct(p, |table| {
        let child = table.add_owner(
            identity(230),
            vec![boolean()],
            vec![
                solve::SolvePureCallOutput::result(real()),
                solve::SolvePureCallOutput::assertion_predicate(),
            ],
            at(1000),
            |builder, inputs, outputs| {
                let assigned = builder.load(inputs[0], at(1001))?;
                let (_, states) = builder.conditional_assigned(
                    assigned,
                    (&[], &[]),
                    (vec![], vec![real()]),
                    at(1002),
                    |branch, _, _, _| {
                        let value = branch.constant(solve::SolveValue::real(p, 7.0), at(1003))?;
                        let state = branch.present_assigned_state(value, at(1004))?;
                        branch.store_assigned_state(0, state, at(1005))
                    },
                    |branch, _, _, _| {
                        let state = branch.absent_assigned_state(real(), at(1006))?;
                        branch.store_assigned_state(0, state, at(1007))
                    },
                )?;
                let assertion = builder.assertion_output(1, at(1008))?;
                let (condition, values) =
                    builder.require_assigned_states(assertion, &states, at(1009))?;
                builder.store(outputs[0], values[0], at(1013))?;
                builder.store(outputs[1], condition, at(1014))
            },
        )?;
        let outputs = table.call_site(child).unwrap().outputs().to_vec();
        table.add_owner(
            identity(231),
            vec![boolean(), boolean()],
            outputs,
            at(1020),
            |builder, inputs, outputs| {
                let selected = builder.load(inputs[0], at(1021))?;
                let assigned = builder.load(inputs[1], at(1022))?;
                let values = builder.conditional(
                    selected,
                    &[assigned],
                    vec![real()],
                    at(1023),
                    |branch, captures, outputs| {
                        let assigned = branch.load(captures[0], at(1024))?;
                        let call = branch.emit_call(child, &[assigned], at(1025))?;
                        let assertion = branch.assertion_output(1, at(1026))?;
                        branch.forward_assertion(&call, 1, assertion, at(1027))?;
                        branch.store(outputs[0], call.registers()[0], at(1028))
                    },
                    |branch, _, outputs| {
                        let value = branch.constant(solve::SolveValue::real(p, 0.0), at(1029))?;
                        branch.store(outputs[0], value, at(1030))
                    },
                )?;
                builder.store(outputs[0], values[0], at(1031))?;
                let inactive = builder.constant(solve::SolveValue::boolean(true), at(1032))?;
                builder.store(outputs[1], inactive, at(1033))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn conditional_assigned_state_publishes_presence_and_payload_of_the_selected_arm() {
    let table = assigned_pair();
    assert_eq!(native(&table, 0, &[1.0]), [7.0, 1.0]);
    assert_eq!(native(&table, 0, &[1.0]), interpreted(&table, 0, &[1.0]));
    assert_eq!(native(&table, 0, &[0.0])[1], 0.0);
    for (selected, assigned) in [(1.0, 1.0), (1.0, 0.0), (0.0, 1.0), (0.0, 0.0)] {
        let input = [selected, assigned];
        let actual = native(&table, 1, &input);
        assert_eq!(actual[1], 1.0, "the parent predicate stays inactive");
        if selected == 0.0 || assigned == 1.0 {
            assert_eq!(actual, interpreted(&table, 1, &input));
        }
    }
}
