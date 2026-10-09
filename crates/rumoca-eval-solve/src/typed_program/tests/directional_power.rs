use super::*;
use rumoca_ir_solve::SolveOperation;

fn power_table(
    arithmetic: SolveArithmeticProfile,
    dimensions: Vec<u32>,
    scalar_on_lhs: bool,
    constant: Option<f64>,
) -> SolvePureCallTable {
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let tensor = SolveValueType::tensor(real.element_type(), dimensions).unwrap();
    let mut inputs = vec![tensor.clone()];
    if constant.is_none() {
        inputs.push(real);
    }
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(600),
            inputs,
            vec![SolvePureCallOutput::result(tensor)],
            span(1400),
            |builder, inputs, outputs| {
                let aggregate = builder.load(inputs[0], span(1401))?;
                let scalar = match constant {
                    Some(value) => {
                        builder.constant(SolveValue::real(arithmetic, value), span(1402))?
                    }
                    None => builder.load(inputs[1], span(1402))?,
                };
                let result = builder.broadcast_binary(
                    SolveBinaryOperator::Power,
                    aggregate,
                    scalar,
                    scalar_on_lhs,
                    span(1403),
                )?;
                builder.store(outputs[0], result, span(1404))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn real_value(value_type: &SolveValueType, values: &[f64]) -> TypedValue {
    TypedValue::construct(
        value_type.clone(),
        values
            .iter()
            .map(|value| real_kind(SolveRealFormat::Binary64, *value))
            .collect(),
    )
    .unwrap()
}

fn numbers(value: &TypedValue) -> Vec<f64> {
    value
        .elements()
        .iter()
        .map(|value| match value {
            SolveValueKind::Real64(bits) => f64::from_bits(*bits),
            _ => panic!("expected Binary64 fixture"),
        })
        .collect()
}

fn close(actual: f64, expected: f64, tolerance: f64) {
    assert!(
        (actual - expected).abs() <= tolerance * (1.0 + expected.abs()),
        "{actual} != {expected}"
    );
}

#[test]
fn broadcast_power_directional_matches_analytic_and_finite_difference_in_both_orders() {
    for dimensions in [vec![4], vec![2, 2]] {
        for scalar_on_lhs in [false, true] {
            check_direction(&dimensions, scalar_on_lhs);
        }
    }
}

fn check_direction(dimensions: &[u32], scalar_on_lhs: bool) {
    let table = power_table(
        profile(SolveRealFormat::Binary64),
        dimensions.to_vec(),
        scalar_on_lhs,
        None,
    );
    let owner = &table.owners()[0];
    let aggregate = [1.5_f64, 2.0, 3.0, 4.0];
    let direction = [0.3, -0.2, 0.4, 0.7];
    let scalar = 2.5_f64;
    let scalar_direction = 0.6;
    let inputs = owner.inputs();
    let primal = eval_pure_call(
        &table,
        owner.id(),
        &[
            real_value(&inputs[0], &aggregate),
            real_value(&inputs[1], &[scalar]),
        ],
    )
    .unwrap();
    let outputs = eval_pure_call_directional(
        &table,
        owner.id(),
        &[
            real_value(&inputs[0], &aggregate),
            real_value(&inputs[0], &direction),
            real_value(&inputs[1], &[scalar]),
            real_value(&inputs[1], &[scalar_direction]),
        ],
    )
    .unwrap();
    assert_eq!(outputs[0], primal[0]);
    let tangent = numbers(&outputs[1]);
    let epsilon = 1e-6;
    let perturbed = |sign: f64| {
        eval_pure_call(
            &table,
            owner.id(),
            &[
                real_value(
                    &inputs[0],
                    &std::array::from_fn::<_, 4, _>(|i| {
                        aggregate[i] + sign * epsilon * direction[i]
                    }),
                ),
                real_value(&inputs[1], &[scalar + sign * epsilon * scalar_direction]),
            ],
        )
        .unwrap()
    };
    let plus = numbers(&perturbed(1.0)[0]);
    let minus = numbers(&perturbed(-1.0)[0]);
    for i in 0..aggregate.len() {
        let expected = if scalar_on_lhs {
            scalar.powf(aggregate[i])
                * (direction[i] * scalar.ln() + aggregate[i] * scalar_direction / scalar)
        } else {
            scalar * aggregate[i].powf(scalar - 1.0) * direction[i]
                + aggregate[i].powf(scalar) * aggregate[i].ln() * scalar_direction
        };
        close(tangent[i], expected, 1e-12);
        close(tangent[i], (plus[i] - minus[i]) / (2.0 * epsilon), 1e-8);
    }
    let body = owner.directional().unwrap().body();
    let maps = body
        .operations()
        .iter()
        .filter_map(|operation| match operation.operation() {
            SolveOperation::Map { domain, .. } => Some(domain),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(maps.len(), 1);
    assert_eq!(
        maps[0].extents().unwrap(),
        dimensions.iter().map(|n| *n as usize).collect::<Vec<_>>()
    );
}

#[test]
fn broadcast_power_constant_exponent_preserves_zero_tangent_fact_in_map_capture() {
    for exponent in [0.0_f64, 2.0, 3.0] {
        let table = power_table(
            profile(SolveRealFormat::Binary64),
            vec![3],
            false,
            Some(exponent),
        );
        let owner = &table.owners()[0];
        let inputs = owner.inputs();
        let bases = [-3.0_f64, 0.0, 2.0];
        let seeds = [1.0, -2.0, 0.5];
        let outputs = eval_pure_call_directional(
            &table,
            owner.id(),
            &[
                real_value(&inputs[0], &bases),
                real_value(&inputs[0], &seeds),
            ],
        )
        .unwrap();
        for ((actual, primal), (base, seed)) in numbers(&outputs[1])
            .into_iter()
            .zip(numbers(&outputs[0]))
            .zip(bases.into_iter().zip(seeds))
        {
            close(primal, base.powf(exponent), 0.0);
            let partial = exponent * base.powf(exponent - 1.0);
            let expected = if partial.is_finite() {
                seed * partial
            } else {
                0.0
            };
            close(actual, expected, 0.0);
        }
        let body = owner.directional().unwrap().body();
        let map = body
            .operations()
            .iter()
            .find_map(|operation| match operation.operation() {
                SolveOperation::Map { body, captures, .. } => Some((body, captures)),
                _ => None,
            })
            .unwrap();
        assert_eq!(map.1.len(), 4, "constant exponent has no tangent capture");
        assert!(!map.0.body().operations().iter().any(|operation| matches!(
            operation.operation(),
            SolveOperation::Unary {
                operator: SolveUnaryOperator::Log,
                ..
            }
        )));
    }
}

#[test]
fn broadcast_power_empty_tensor_retains_the_typed_extent_refusal() {
    let element = SolveScalarType::real(profile(SolveRealFormat::Binary64));
    let error = SolveValueType::tensor(element, vec![2, 0, 3]).unwrap_err();
    assert_eq!(
        error,
        rumoca_ir_solve::SolveTypeConstructionError::ZeroTensorExtent
    );
}

#[test]
fn broadcast_power_call_boundary_expands_constant_real_argument_as_zero_seed() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let vector = SolveValueType::tensor(real.element_type(), vec![3]).unwrap();
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        let child = table.add_owner(
            identity(601),
            vec![vector.clone(), real],
            vec![SolvePureCallOutput::result(vector.clone())],
            span(1500),
            |builder, inputs, outputs| {
                let aggregate = builder.load(inputs[0], span(1501))?;
                let scalar = builder.load(inputs[1], span(1502))?;
                let result = builder.broadcast_binary(
                    SolveBinaryOperator::Power,
                    aggregate,
                    scalar,
                    false,
                    span(1503),
                )?;
                builder.store(outputs[0], result, span(1504))
            },
        )?;
        table.add_owner(
            identity(602),
            vec![vector.clone()],
            vec![SolvePureCallOutput::result(vector.clone())],
            span(1510),
            |builder, inputs, outputs| {
                let aggregate = builder.load(inputs[0], span(1511))?;
                let two = builder.constant(SolveValue::real(arithmetic, 2.0), span(1512))?;
                let results = builder.call(child, &[aggregate, two], span(1513))?;
                builder.store(outputs[0], results[0], span(1514))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let owner = &table.owners()[1];
    let result = eval_pure_call_directional(
        &table,
        owner.id(),
        &[
            real_value(&vector, &[-3.0, 0.0, 2.0]),
            real_value(&vector, &[1.0, -2.0, 0.5]),
        ],
    )
    .unwrap();
    assert_eq!(numbers(&result[0]), [9.0, 0.0, 4.0]);
    assert_eq!(numbers(&result[1]), [-6.0, 0.0, 2.0]);
}

#[test]
fn broadcast_power_map_refuses_unrepresentable_domain_and_overflowing_tensor() {
    let arithmetic = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::construct(-2, 2).unwrap(),
    );
    let element = SolveScalarType::real(arithmetic);
    assert!(SolveValueType::tensor(element, vec![u32::MAX, u32::MAX, 2]).is_err());
    let real = SolveValueType::scalar(element);
    let vector = SolveValueType::tensor(element, vec![3]).unwrap();
    let result = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(603),
            vec![vector.clone(), real],
            vec![SolvePureCallOutput::result(vector)],
            span(1520),
            |builder, inputs, outputs| {
                let aggregate = builder.load(inputs[0], span(1521))?;
                let scalar = builder.load(inputs[1], span(1522))?;
                let result = builder.broadcast_binary(
                    SolveBinaryOperator::Power,
                    aggregate,
                    scalar,
                    false,
                    span(1523),
                )?;
                builder.store(outputs[0], result, span(1524))
            },
        )?;
        Ok(())
    });
    assert!(
        matches!(result, Err(SolveProgramConstructionError::InvalidMap { provenance }) if provenance == span(1523))
    );
}

#[test]
fn broadcast_power_wire_replay_keeps_one_map_independent_of_tensor_extent() {
    for scalar_on_lhs in [false, true] {
        let small = power_table(
            profile(SolveRealFormat::Binary64),
            vec![2, 2],
            scalar_on_lhs,
            None,
        );
        let large = power_table(
            profile(SolveRealFormat::Binary64),
            vec![64, 64],
            scalar_on_lhs,
            None,
        );
        let replayed: SolvePureCallTable =
            serde_json::from_str(&serde_json::to_string(&large).unwrap()).unwrap();
        assert_eq!(replayed, large);
        let shape = |table: &SolvePureCallTable| {
            let body = table.owners()[0].directional().unwrap().body();
            let maps = body
                .operations()
                .iter()
                .filter_map(|operation| match operation.operation() {
                    SolveOperation::Map { body, .. } => {
                        assert!(
                            body.body()
                                .operations()
                                .iter()
                                .all(|operation| operation.provenance() == span(1403))
                        );
                        Some(body.body().operations().len())
                    }
                    _ => None,
                })
                .collect::<Vec<_>>();
            assert_eq!(maps.len(), 1);
            (
                body.operations().len(),
                body.register_types().len(),
                maps[0],
            )
        };
        assert_eq!(shape(&small), shape(&large));
    }
}

#[test]
fn broadcast_power_map_uses_the_checked_binary32_profile() {
    let table = power_table(
        profile(SolveRealFormat::Binary32),
        vec![3],
        false,
        Some(2.0),
    );
    let owner = &table.owners()[0];
    let value_type = &owner.inputs()[0];
    let value = |numbers: &[f64]| {
        TypedValue::construct(
            value_type.clone(),
            numbers
                .iter()
                .map(|number| real_kind(SolveRealFormat::Binary32, *number))
                .collect(),
        )
        .unwrap()
    };
    let outputs = eval_pure_call_directional(
        &table,
        owner.id(),
        &[value(&[-3.0, 0.0, 2.0]), value(&[1.0, -2.0, 0.5])],
    )
    .unwrap();
    assert_eq!(
        outputs[0].elements(),
        [9.0, 0.0, 4.0].map(|v| real_kind(SolveRealFormat::Binary32, v))
    );
    assert_eq!(
        outputs[1].elements(),
        [-6.0, -0.0, 2.0].map(|v| real_kind(SolveRealFormat::Binary32, v))
    );
}
