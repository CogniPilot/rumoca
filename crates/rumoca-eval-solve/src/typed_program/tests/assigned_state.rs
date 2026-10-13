use super::stopping_call as eval_pure_call;
use super::*;
use rumoca_ir_solve::SolveAssertionLevel;

fn selected_read_transition<'program>(
    region: &mut TypedProgramBuilder<'program>,
    states: &[rumoca_ir_solve::ProgramAssignedState<'program>],
    carried: &[ProgramSlot<'program>],
    outputs: &[ProgramSlot<'program>],
    (read, real): (bool, &SolveValueType),
) -> Result<(), SolveProgramConstructionError> {
    let value = region.load(carried[0], span(1204))?;
    let selected = region.constant(SolveValue::boolean(read), span(1205))?;
    let (normal, _) = region.conditional_assigned(
        selected,
        (&[value], states),
        (vec![real.clone()], vec![]),
        span(1206),
        |branch, inputs, states, outputs| {
            let assertion = branch.assertion_output(1, span(1207))?;
            let _ = branch.require_assigned_state(assertion, states[0], span(1208))?;
            let value = branch.load(inputs[0], span(1209))?;
            branch.store(outputs[0], value, span(1210))
        },
        |branch, inputs, _, outputs| {
            let value = branch.load(inputs[0], span(1211))?;
            branch.store(outputs[0], value, span(1212))
        },
    )?;
    region.store(outputs[0], normal[0], span(1213))
}

fn absent_fold(upper: i64, continuation: Option<bool>, read: bool) -> SolvePureCallTable {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper,
            step: 1,
        }],
    };
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(1201),
            vec![],
            vec![
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::assertion_predicate_at_level(SolveAssertionLevel::Error),
            ],
            span(1201),
            |builder, _, outputs| {
                let absent = builder.absent_assigned_state(real.clone(), span(1202))?;
                let initial = builder.constant(SolveValue::real(arithmetic, 7.0), span(1203))?;
                let values = match continuation {
                    Some(proceed) => builder.fold_while_assigned(
                        domain.clone(),
                        &[initial],
                        (&[], &[absent]),
                        span(1214),
                        |predicate, _, _, _, _, outputs| {
                            let value =
                                predicate.constant(SolveValue::boolean(proceed), span(1215))?;
                            predicate.store(outputs[0], value, span(1216))
                        },
                        |region, states, carried, _, _, outputs| {
                            selected_read_transition(
                                region,
                                states,
                                carried,
                                outputs,
                                (read, &real),
                            )
                        },
                    )?,
                    None => builder.fold_with_assigned(
                        domain.clone(),
                        &[initial],
                        &[],
                        &[absent],
                        span(1214),
                        |region, states, carried, _, _, outputs| {
                            selected_read_transition(
                                region,
                                states,
                                carried,
                                outputs,
                                (read, &real),
                            )
                        },
                    )?,
                };
                builder.store(outputs[0], values[0], span(1217))?;
                let inactive = builder.constant(SolveValue::boolean(true), span(1218))?;
                builder.store(outputs[1], inactive, span(1219))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn absent_fold_capture_is_inert_for_zero_iterations_false_continuation_and_inactive_read() {
    for (upper, continuation, read) in [(0, None, true), (2, Some(false), true), (2, None, false)] {
        let table = absent_fold(upper, continuation, read);
        let output = eval_pure_call(&table, table.owners()[0].id(), &[]).unwrap();
        assert_eq!(
            output[0].elements(),
            [SolveValueKind::Real64(7.0_f64.to_bits())]
        );
        assert_eq!(output[1].elements(), [SolveValueKind::Boolean(true)]);
        let wire = serde_json::to_vec(&table).unwrap();
        let replay: SolvePureCallTable = serde_json::from_slice(&wire).unwrap();
        let repeated = eval_pure_call(&replay, replay.owners()[0].id(), &[]).unwrap();
        assert_eq!(repeated, output);
    }
}

#[test]
fn demanded_absent_fold_capture_faults_at_selected_source_read() {
    let table = absent_fold(2, None, true);
    let Err(TypedProgramEvalError::AssertionFailed { failure }) =
        eval_pure_call(&table, table.owners()[0].id(), &[])
    else {
        panic!("the demanded source-owned read must fail");
    };
    assert_eq!(failure.source_span(), span(1208));
}

#[test]
fn a_value_row_continues_past_a_demanded_absent_capture() {
    let table = absent_fold(2, None, true);
    let output = super::eval_pure_call(&table, table.owners()[0].id(), &[])
        .expect("a value row leaves the failure to the scheduled assertion owner");
    assert_eq!(
        output[0].elements(),
        [SolveValueKind::Real64(7.0_f64.to_bits())]
    );
}
