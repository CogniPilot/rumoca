use super::*;

fn table(format: SolveRealFormat, scalar_on_lhs: bool) -> SolvePureCallTable {
    let arithmetic = profile(format);
    let tensor = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 2]).unwrap();
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(1920),
            vec![tensor.clone(), scalar],
            vec![SolvePureCallOutput::result(tensor)],
            span(0),
            |builder, inputs, outputs| {
                let aggregate = builder.load(inputs[0], span(1))?;
                let scalar = builder.load(inputs[1], span(2))?;
                let value = builder.broadcast_binary(
                    SolveBinaryOperator::Power,
                    aggregate,
                    scalar,
                    scalar_on_lhs,
                    span(3),
                )?;
                builder.store(outputs[0], value, span(4))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn typed(table: &SolvePureCallTable, index: usize, values: &[f64]) -> TypedValue {
    let value_type = table.owners()[0].inputs()[index].clone();
    let SolveScalarType::Real { format } = value_type.element_type() else {
        panic!("Real input")
    };
    TypedValue::construct(
        value_type,
        values
            .iter()
            .map(|&value| real_kind(format, value))
            .collect(),
    )
    .unwrap()
}

fn numbers(value: &TypedValue) -> Vec<f64> {
    value
        .elements()
        .iter()
        .map(|value| match value {
            SolveValueKind::Real32(bits) => f64::from(f32::from_bits(*bits)),
            SolveValueKind::Real64(bits) => f64::from_bits(*bits),
            other => panic!("Real result, got {other:?}"),
        })
        .collect()
}

fn directional(table: &SolvePureCallTable, values: &[f64], scalar: f64) -> Vec<TypedValue> {
    let owner = &table.owners()[0];
    let primal = eval_pure_call(
        table,
        owner.id(),
        &[typed(table, 0, values), typed(table, 1, &[scalar])],
    )
    .unwrap();
    let result = eval_pure_call_directional(
        table,
        owner.id(),
        &[
            typed(table, 0, values),
            typed(table, 0, &[1.0, -0.5, 2.0, 0.25]),
            typed(table, 1, &[scalar]),
            typed(table, 1, &[0.5]),
        ],
    )
    .expect("broadcast power supplies the checked directional relation");
    assert_eq!(
        result[0].elements(),
        primal[0].elements(),
        "retain exact original primal cells"
    );
    result
}

fn analytic(base: f64, exponent: f64, base_seed: f64, exponent_seed: f64) -> f64 {
    base_seed * exponent * base.powf(exponent - 1.0)
        + exponent_seed * base.powf(exponent) * base.ln()
}

fn check_unique(format: SolveRealFormat, scalar_on_lhs: bool) {
    let table = table(format, scalar_on_lhs);
    let values = [1.5, 2.0, 2.5, 3.0];
    let seeds = [1.0, -0.5, 2.0, 0.25];
    let result = directional(&table, &values, 2.0);
    let h = if format == SolveRealFormat::Binary32 {
        1e-3
    } else {
        1e-6
    };
    let tolerance = if format == SolveRealFormat::Binary32 {
        0.005
    } else {
        1e-7
    };
    let sample = |sign: f64| {
        let shifted = values
            .iter()
            .zip(seeds)
            .map(|(&value, seed)| value + sign * h * seed)
            .collect::<Vec<_>>();
        numbers(
            &eval_pure_call(
                &table,
                table.owners()[0].id(),
                &[
                    typed(&table, 0, &shifted),
                    typed(&table, 1, &[2.0 + sign * h * 0.5]),
                ],
            )
            .unwrap()[0],
        )
    };
    let plus = sample(1.0);
    let minus = sample(-1.0);
    for (index, tangent) in numbers(&result[1]).into_iter().enumerate() {
        let (base, exponent, base_seed, exponent_seed) = if scalar_on_lhs {
            (2.0, values[index], 0.5, seeds[index])
        } else {
            (values[index], 2.0, seeds[index], 0.5)
        };
        let expected = analytic(base, exponent, base_seed, exponent_seed);
        assert!(
            (tangent - expected).abs() < tolerance,
            "JVP {tangent} expected {expected}"
        );
        assert!((tangent - (plus[index] - minus[index]) / (2.0 * h)).abs() < tolerance);
    }
}

#[test]
fn broadcast_power_directional_matches_ranked_analytic_and_finite_difference_values() {
    for format in [SolveRealFormat::Binary32, SolveRealFormat::Binary64] {
        for scalar_on_lhs in [false, true] {
            check_unique(format, scalar_on_lhs);
        }
    }
}

#[test]
fn broadcast_power_directional_preserves_nonfinite_primal_and_scalar_partial_policy() {
    let table = table(SolveRealFormat::Binary64, false);
    let values = [0.0, -2.0, f64::INFINITY, f64::NAN];
    let result = directional(&table, &values, 2.0);
    assert_eq!(numbers(&result[1]), [0.0, 2.0, 0.0, 0.0]);
    let primal = numbers(&result[0]);
    assert_eq!(primal[..3], [0.0, 4.0, f64::INFINITY]);
    assert!(primal[3].is_nan());
}
