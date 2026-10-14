use super::*;

/// SPEC_0049 / MLS §10.3.4: false precedes true in both extrema forms.
#[test]
fn boolean_extrema_match_scalar_truth_table_and_array_edges() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let array = SolveValueType::tensor(SolveScalarType::Boolean, vec![2]).unwrap();
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(4910),
            vec![boolean.clone(), boolean.clone(), array.clone()],
            vec![SolvePureCallOutput::result(boolean.clone()); 4],
            span(0),
            |builder, inputs, outputs| {
                let lhs = builder.load(inputs[0], span(1))?;
                let rhs = builder.load(inputs[1], span(2))?;
                let values = builder.load(inputs[2], span(3))?;
                let results = [
                    builder.binary(SolveBinaryOperator::Min, lhs, rhs, span(4))?,
                    builder.binary(SolveBinaryOperator::Max, lhs, rhs, span(5))?,
                    builder.reduce(SolveReductionOperator::Minimum, values, span(6))?,
                    builder.reduce(SolveReductionOperator::Maximum, values, span(7))?,
                ];
                for (output, result) in outputs.iter().zip(results) {
                    builder.store(*output, result, span(8))?;
                }
                Ok(())
            },
        )?;
        Ok(())
    })
    .expect("catalog Boolean interfaces construct");
    for (lhs, rhs, expected) in [
        (false, false, [false, false]),
        (false, true, [false, true]),
        (true, false, [false, true]),
        (true, true, [true, true]),
    ] {
        let scalar = |value| {
            TypedValue::construct(boolean.clone(), vec![SolveValueKind::Boolean(value)]).unwrap()
        };
        let values = TypedValue::construct(
            array.clone(),
            vec![SolveValueKind::Boolean(lhs), SolveValueKind::Boolean(rhs)],
        )
        .unwrap();
        let result = eval_pure_call(
            &table,
            table.owners()[0].id(),
            &[scalar(lhs), scalar(rhs), values],
        )
        .expect("Boolean extrema execute without numeric conversion");
        assert_eq!(result.len(), 4);
        for (value, expected) in result.iter().zip(expected.into_iter().cycle()) {
            assert_eq!(value.elements(), [SolveValueKind::Boolean(expected)]);
        }
    }
}
