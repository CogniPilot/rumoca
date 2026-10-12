use super::*;

fn logarithm_table(
    arithmetic: SolveArithmeticProfile,
    tensor: &SolveValueType,
    operator: crate::SolveUnaryOperator,
) -> SolvePureCallTable {
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(731),
            vec![tensor.clone()],
            vec![SolvePureCallOutput::result(tensor.clone())],
            span(1200),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1201))?;
                let output = builder.unary(operator, input, span(1202))?;
                builder.store(outputs[0], output, span(1203))
            },
        )?;
        Ok(())
    })
    .expect("derivative profile refusal must retain the admitted primal owner")
}

#[test]
fn aggregate_logarithm_directional_keeps_checked_index_profile_and_compact_map() {
    for (minimum, maximum, extent, admitted) in [
        (-128, 127, 4, true),
        (-128, 127, 128, false),
        (2, 127, 4, false),
    ] {
        let arithmetic = SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            SolveIntegerDomain::construct(minimum, maximum).unwrap(),
        );
        let tensor =
            SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![extent]).unwrap();
        for operator in [
            crate::SolveUnaryOperator::Log,
            crate::SolveUnaryOperator::Log10,
        ] {
            let table = logarithm_table(arithmetic, &tensor, operator);
            let owner = &table.owners()[0];
            assert_eq!(owner.inputs(), std::slice::from_ref(&tensor));
            assert_eq!(owner.outputs()[0].value_type(), &tensor);
            assert_eq!(owner.directional().is_some(), admitted);
            if let Some(directional) = owner.directional() {
                assert_eq!(
                    directional
                        .body()
                        .operations()
                        .iter()
                        .filter(|op| matches!(op.operation(), crate::SolveOperation::Map { .. }))
                        .count(),
                    1,
                    "one compact tangent owner regardless of aggregate extent"
                );
            }
        }
    }
}
