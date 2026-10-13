use super::*;

#[test]
fn compact_optional_tensor_has_no_missing_numeric_register() {
    let tensor = SolveValueType::tensor(SolveScalarType::real(profile()), vec![131_072]).unwrap();
    let program = TypedProgram::construct(profile(), |builder| {
        builder.absent_assigned_state(tensor.clone(), span(1))?;
        Ok(())
    })
    .unwrap();
    assert!(program.register_types().is_empty());
    assert_eq!(program.assigned_state_types(), &[tensor]);
    assert_eq!(program.operations().len(), 1);
}

#[test]
fn incomplete_optional_publication_is_refused_without_weakening_normal_tuple() {
    let scalar = SolveValueType::scalar(SolveScalarType::real(profile()));
    let result = TypedProgram::construct(profile(), |builder| {
        let condition = builder.constant(SolveValue::boolean(true), span(1))?;
        builder.conditional_assigned(
            condition,
            (&[], &[]),
            (vec![], vec![scalar.clone()]),
            span(2),
            |region, _, _, _| {
                let value = region.absent_assigned_state(scalar.clone(), span(3))?;
                region.store_assigned_state(0, value, span(4))
            },
            |_, _, _, _| Ok(()),
        )?;
        Ok(())
    });
    assert_eq!(
        result.unwrap_err(),
        SolveProgramConstructionError::InvalidRegion {
            provenance: span(2),
        }
    );
}

#[test]
fn optional_payload_projection_requires_exact_prior_checked_assertion() {
    let result = TypedProgram::construct(profile(), |builder| {
        let state = builder.absent_assigned_state(
            SolveValueType::scalar(SolveScalarType::real(profile())),
            span(1),
        )?;
        builder.project_checked_assigned_state(state, 0, span(2))?;
        Ok(())
    });
    assert!(matches!(
        result,
        Err(SolveProgramConstructionError::InvalidProjection { .. })
    ));
}

#[test]
fn nested_optional_state_is_transferred_without_numeric_default() {
    let scalar = SolveValueType::scalar(SolveScalarType::real(profile()));
    let program = TypedProgram::construct(profile(), |builder| {
        let condition = builder.constant(SolveValue::boolean(false), span(1))?;
        let absent = builder.absent_assigned_state(scalar.clone(), span(2))?;
        builder.conditional_assigned(
            condition,
            (&[], &[absent]),
            (vec![], vec![scalar.clone()]),
            span(3),
            |region, _, states, _| region.store_assigned_state(0, states[0], span(4)),
            |region, _, states, _| {
                let condition = region.constant(SolveValue::boolean(true), span(5))?;
                let (_, nested) = region.conditional_assigned(
                    condition,
                    (&[], states),
                    (vec![], vec![scalar.clone()]),
                    span(6),
                    |nested, _, states, _| nested.store_assigned_state(0, states[0], span(7)),
                    |nested, _, states, _| nested.store_assigned_state(0, states[0], span(8)),
                )?;
                region.store_assigned_state(0, nested[0], span(9))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_eq!(program.register_types().len(), 1);
    assert_eq!(program.assigned_state_types().len(), 2);
    let wire = serde_json::to_vec(&program).unwrap();
    let replay: TypedProgram = serde_json::from_slice(&wire).unwrap();
    assert_eq!(replay, program);
}

#[test]
fn directional_optional_real_retains_compact_tagged_primal_and_tangent() {
    use crate::{SolvePureCallIdentity, SolvePureCallOutput, SolvePureCallTable};
    let scalar = SolveValueType::scalar(SolveScalarType::real(profile()));
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![scalar.clone(), boolean.clone()],
            vec![SolvePureCallOutput::result(boolean.clone())],
            span(1),
            |builder, inputs, outputs| {
                let value = builder.load(inputs[0], span(2))?;
                let condition = builder.load(inputs[1], span(3))?;
                let (_, states) = builder.conditional_assigned(
                    condition,
                    (&[value], &[]),
                    (vec![], vec![scalar.clone()]),
                    span(4),
                    |branch, captures, _, _| {
                        let value = branch.load(captures[0], span(5))?;
                        let state = branch.present_assigned_state(value, span(6))?;
                        branch.store_assigned_state(0, state, span(7))
                    },
                    |branch, _, _, _| {
                        let state = branch.absent_assigned_state(scalar.clone(), span(8))?;
                        branch.store_assigned_state(0, state, span(9))
                    },
                )?;
                let present = builder.assigned_state_is_present(states[0], span(10))?;
                builder.store(outputs[0], present, span(11))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[0];
    let directional = owner
        .directional()
        .expect("compact optional state is differentiable");
    assert!(directional.body().assigned_state_inputs().is_empty());
    assert!(directional.body().assigned_state_outputs().is_empty());
    assert_eq!(
        directional.body().assigned_state_types(),
        &[scalar.clone(), scalar]
    );
    let wire = serde_json::to_vec(&table).unwrap();
    let replay: SolvePureCallTable = serde_json::from_slice(&wire).unwrap();
    assert_eq!(
        replay.owners()[0].directional().unwrap().body(),
        directional.body()
    );
}

#[test]
fn assigned_presence_output_visitors_preserve_complete_register_issuance_on_replay() {
    let program = TypedProgram::construct(profile(), |builder| {
        let scalar = SolveValueType::scalar(SolveScalarType::real(profile()));
        let first = builder.absent_assigned_state(scalar.clone(), span(1))?;
        let second = builder.absent_assigned_state(scalar, span(2))?;
        builder.assigned_state_is_present(first, span(3))?;
        builder.assigned_states_all_present(&[first, second], span(4))?;
        builder.constant(SolveValue::boolean(true), span(5))?;
        Ok(())
    })
    .unwrap();
    assert_eq!(program.register_types().len(), 3);
    let replay: TypedProgram =
        serde_json::from_slice(&serde_json::to_vec(&program).unwrap()).unwrap();
    for checked in [&program, &replay] {
        let mut destinations = Vec::new();
        for operation in checked.operations() {
            operation
                .operation()
                .visit_output_registers(|register| destinations.push(register.index()));
        }
        assert_eq!(destinations, vec![0, 1, 2]);
    }
}

#[test]
fn source_free_unwritten_assigned_interface_has_no_fabricated_span() {
    let scalar = SolveValueType::scalar(SolveScalarType::real(profile()));
    let result = TypedProgram::construct_with_assigned_interface(
        profile(),
        SolvePureCallTableView::default(),
        &[],
        Vec::new(),
        vec![scalar],
        None,
        |_| Ok(()),
    );
    assert_eq!(
        result.unwrap_err(),
        SolveProgramConstructionError::MissingProvenance,
    );
    let empty = TypedProgram::construct(profile(), |_| Ok(())).unwrap();
    let replay: TypedProgram =
        serde_json::from_slice(&serde_json::to_vec(&empty).unwrap()).unwrap();
    assert_eq!(empty, replay);
}
