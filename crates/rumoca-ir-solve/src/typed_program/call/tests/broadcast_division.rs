use super::*;

fn broadcast_table(
    operator: crate::SolveBinaryOperator,
    arithmetic: SolveArithmeticProfile,
    aggregate: &SolveValueType,
    scalar: &SolveValueType,
    scalar_on_lhs: bool,
) -> SolvePureCallTable {
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(1),
            vec![aggregate.clone(), scalar.clone()],
            vec![SolvePureCallOutput::result(aggregate.clone())],
            span(0),
            |builder, inputs, outputs| {
                let aggregate = builder.load(inputs[0], span(1))?;
                let scalar = builder.load(inputs[1], span(2))?;
                let quotient = builder.broadcast_binary(
                    operator,
                    aggregate,
                    scalar,
                    scalar_on_lhs,
                    span(3),
                )?;
                builder.store(outputs[0], quotient, span(4))
            },
        )?;
        Ok(())
    })
    .expect("unsupported derivative domains do not reject the admitted primal owner")
}

#[test]
fn broadcast_division_directional_preserves_narrow_integer_profile_primal_owner() {
    for (minimum, maximum, extent, supports_directional) in [
        (-128, 127, 4, true),
        (-128, 127, 128, false),
        (2, 127, 4, false),
    ] {
        let arithmetic = SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            SolveIntegerDomain::construct(minimum, maximum).unwrap(),
        );
        let aggregate =
            SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![extent]).unwrap();
        let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
        for scalar_on_lhs in [false, true] {
            let table = broadcast_table(
                crate::SolveBinaryOperator::Divide,
                arithmetic,
                &aggregate,
                &scalar,
                scalar_on_lhs,
            );
            assert_eq!(table.owners().len(), 1);
            let owner = &table.owners()[0];
            assert_eq!(owner.inputs(), [aggregate.clone(), scalar.clone()]);
            assert_eq!(owner.outputs()[0].value_type(), &aggregate);
            assert_eq!(
                owner.directional().is_some(),
                supports_directional,
                "Integer [{minimum},{maximum}], extent {extent}, scalar_on_lhs={scalar_on_lhs}"
            );
        }
    }
    assert_eq!(
        SolveValueType::tensor(SolveScalarType::real(profile()), vec![0]),
        Err(crate::SolveTypeConstructionError::ZeroTensorExtent),
        "the canonical tensor constructor refuses zero extents before owner issuance"
    );
}

#[test]
fn broadcast_power_directional_keeps_compact_shape_and_checked_domain() {
    for (minimum, maximum, extent, supported) in [
        (-128, 127, 4, true),
        (-128, 127, 128, false),
        (2, 127, 4, false),
        (i64::MIN, i64::MAX, 1_000_000, true),
    ] {
        let arithmetic = SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            SolveIntegerDomain::construct(minimum, maximum).unwrap(),
        );
        let aggregate =
            SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![extent]).unwrap();
        let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
        for scalar_on_lhs in [false, true] {
            let table = broadcast_table(
                crate::SolveBinaryOperator::Power,
                arithmetic,
                &aggregate,
                &scalar,
                scalar_on_lhs,
            );
            assert_power_body(&table, supported);
            let json = serde_json::to_string(&table).unwrap();
            let decoded: SolvePureCallTable = serde_json::from_str(&json).unwrap();
            assert_eq!(serde_json::to_string(&decoded).unwrap(), json);
        }
    }
}

fn assert_power_body(table: &SolvePureCallTable, supported: bool) {
    let owner = &table.owners()[0];
    assert_eq!(owner.directional().is_some(), supported);
    let Some(directional) = owner.directional() else {
        return;
    };
    assert!(directional.body().operations().len() < 25);
    assert_eq!(
        directional
            .body()
            .operations()
            .iter()
            .filter(|op| matches!(
                op.operation(),
                crate::SolveOperation::BroadcastBinary {
                    operator: crate::SolveBinaryOperator::Power,
                    ..
                }
            ))
            .count(),
        1
    );
    assert_eq!(
        directional
            .body()
            .operations()
            .iter()
            .filter(|op| matches!(op.operation(), crate::SolveOperation::Map { .. }))
            .count(),
        1
    );
}
