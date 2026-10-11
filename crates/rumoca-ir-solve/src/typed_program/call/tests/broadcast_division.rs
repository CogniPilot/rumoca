use super::*;

fn division_table(
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
                    crate::SolveBinaryOperator::Divide,
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
            let table = division_table(arithmetic, &aggregate, &scalar, scalar_on_lhs);
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
