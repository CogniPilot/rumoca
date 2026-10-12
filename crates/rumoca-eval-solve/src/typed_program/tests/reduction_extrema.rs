use super::*;

fn table(
    format: SolveRealFormat,
    dimensions: Vec<u32>,
    operator: SolveReductionOperator,
) -> SolvePureCallTable {
    let arithmetic = profile(format);
    let aggregate = SolveValueType::tensor(SolveScalarType::real(arithmetic), dimensions).unwrap();
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(1912),
            vec![aggregate],
            vec![SolvePureCallOutput::result(SolveValueType::scalar(
                SolveScalarType::real(arithmetic),
            ))],
            span(0),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1))?;
                let result = builder.reduce(operator, input, span(2))?;
                builder.store(outputs[0], result, span(3))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn tensor(table: &SolvePureCallTable, values: &[f64]) -> TypedValue {
    let value_type = table.owners()[0].inputs()[0].clone();
    let format = match value_type.element_type() {
        SolveScalarType::Real { format } => format,
        _ => panic!("Real interface"),
    };
    TypedValue::construct(
        value_type,
        values.iter().map(|&v| real_kind(format, v)).collect(),
    )
    .unwrap()
}

fn number(value: &TypedValue) -> f64 {
    match value.elements()[0] {
        SolveValueKind::Real32(bits) => f32::from_bits(bits) as f64,
        SolveValueKind::Real64(bits) => f64::from_bits(bits),
        other => panic!("Real result, got {other:?}"),
    }
}

fn check_directional(
    table: &SolvePureCallTable,
    values: &[f64],
    seeds: &[f64],
    expected: [f64; 2],
) {
    let input = tensor(table, values);
    let owner = &table.owners()[0];
    let primal = eval_pure_call(table, owner.id(), std::slice::from_ref(&input)).unwrap();
    let value = number(&primal[0]);
    if expected[0].is_nan() {
        assert!(value.is_nan());
    } else {
        assert_eq!(value.to_bits(), expected[0].to_bits());
    }
    let outputs = eval_pure_call_directional(table, owner.id(), &[input, tensor(table, seeds)])
        .expect("same checked owner supplies the directional relation");
    assert_eq!(
        outputs[0].elements(),
        primal[0].elements(),
        "primal cells stay bit-identical, including NaN/zero bits"
    );
    assert_eq!(number(&outputs[1]), expected[1]);
}

#[test]
fn reduction_extrema_directional_matches_ranked_unique_extrema_and_finite_differences() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        for dimensions in [vec![4], vec![2, 2]] {
            for (operator, expected) in [
                (SolveReductionOperator::Minimum, [1.0, 13.0]),
                (SolveReductionOperator::Maximum, [5.0, 17.0]),
            ] {
                check_unique(format, dimensions.clone(), operator, expected);
            }
        }
    }
}

fn check_unique(
    format: SolveRealFormat,
    dimensions: Vec<u32>,
    operator: SolveReductionOperator,
    expected: [f64; 2],
) {
    let table = table(format, dimensions, operator);
    let values = [3.0, 1.0, 5.0, 2.0];
    let seeds = [11.0, 13.0, 17.0, 19.0];
    check_directional(&table, &values, &seeds, expected);
    let h = if format == SolveRealFormat::Binary32 {
        1e-3
    } else {
        1e-6
    };
    let sample = |sign: f64| {
        let shifted = values
            .iter()
            .zip(seeds)
            .map(|(&v, s)| v + sign * h * s)
            .collect::<Vec<_>>();
        number(
            &eval_pure_call(&table, table.owners()[0].id(), &[tensor(&table, &shifted)]).unwrap()
                [0],
        )
    };
    let difference = (sample(1.0) - sample(-1.0)) / (2.0 * h);
    let tolerance = if format == SolveRealFormat::Binary32 {
        0.01
    } else {
        1e-8
    };
    assert!(
        (difference - expected[1]).abs() < tolerance,
        "unique-extremum JVP {difference}, expected {}",
        expected[1]
    );
}

#[test]
fn reduction_extrema_directional_preserves_ordered_primal_bits_and_scalar_kink_policy() {
    for operator in [
        SolveReductionOperator::Minimum,
        SolveReductionOperator::Maximum,
    ] {
        let table = table(SolveRealFormat::Binary64, vec![2, 2], operator);
        check_directional(&table, &[4.0; 4], &[11.0, 13.0, 17.0, 19.0], [4.0, 11.0]);
        check_directional(
            &table,
            &[0.0, -0.0, 0.0, -0.0],
            &[11.0, 13.0, 17.0, 19.0],
            [-0.0, 11.0],
        );
        let (primal, tangent) = if operator == SolveReductionOperator::Minimum {
            (1.0, 13.0)
        } else {
            (4.0, 19.0)
        };
        // The existing scalar kink rule selects the right tangent on unordered comparison.
        check_directional(
            &table,
            &[1.0, f64::NAN, 3.0, 4.0],
            &[11.0, 13.0, 17.0, 19.0],
            [primal, tangent],
        );
        check_directional(
            &table,
            &[f64::NAN; 4],
            &[11.0, 13.0, 17.0, 19.0],
            [f64::NAN, 19.0],
        );
        let expected = if operator == SolveReductionOperator::Minimum {
            [2.0, 17.0]
        } else {
            [f64::INFINITY, 11.0]
        };
        check_directional(
            &table,
            &[f64::INFINITY, 4.0, 2.0, f64::INFINITY],
            &[11.0, 13.0, 17.0, 19.0],
            expected,
        );
        let singleton = table_singleton(operator);
        check_directional(&singleton, &[2.0], &[7.0], [2.0, 7.0]);
    }
}

fn table_singleton(operator: SolveReductionOperator) -> SolvePureCallTable {
    table(SolveRealFormat::Binary64, vec![1], operator)
}
