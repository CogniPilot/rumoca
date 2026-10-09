use super::*;
use rumoca_ir_solve::SolveOperation;

fn abs_table(dimensions: Vec<u32>, constant: bool) -> SolvePureCallTable {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let tensor =
        SolveValueType::tensor(SolveScalarType::real(arithmetic), dimensions.clone()).unwrap();
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(610),
            if constant {
                vec![]
            } else {
                vec![tensor.clone()]
            },
            vec![SolvePureCallOutput::result(tensor)],
            span(1600),
            |builder, inputs, outputs| {
                let input = if constant {
                    let scalar =
                        builder.constant(SolveValue::real(arithmetic, -2.0), span(1601))?;
                    builder.fill(scalar, dimensions, span(1601))?
                } else {
                    builder.load(inputs[0], span(1601))?
                };
                let result = builder.unary(SolveUnaryOperator::Abs, input, span(1602))?;
                builder.store(outputs[0], result, span(1603))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn typed(value_type: &SolveValueType, numbers: &[f64]) -> TypedValue {
    TypedValue::construct(
        value_type.clone(),
        numbers
            .iter()
            .map(|v| real_kind(SolveRealFormat::Binary64, *v))
            .collect(),
    )
    .unwrap()
}

fn numbers(value: &TypedValue) -> Vec<f64> {
    value
        .elements()
        .iter()
        .map(|v| match v {
            SolveValueKind::Real64(bits) => f64::from_bits(*bits),
            _ => panic!("Binary64 fixture"),
        })
        .collect()
}

#[test]
fn aggregate_abs_directional_matches_analytic_and_finite_difference() {
    for dimensions in [vec![4], vec![2, 2]] {
        let table = abs_table(dimensions, false);
        let owner = &table.owners()[0];
        let input_type = &owner.inputs()[0];
        let x = [-3.0_f64, -1.0, 2.0, 4.0];
        let du = [0.3, -0.2, 0.4, 0.7];
        let outputs = eval_pure_call_directional(
            &table,
            owner.id(),
            &[typed(input_type, &x), typed(input_type, &du)],
        )
        .unwrap();
        let epsilon = 1e-6;
        let shifted = |sign: f64| {
            let input = std::array::from_fn::<_, 4, _>(|i| x[i] + sign * epsilon * du[i]);
            numbers(&eval_pure_call(&table, owner.id(), &[typed(input_type, &input)]).unwrap()[0])
        };
        let plus = shifted(1.0);
        let minus = shifted(-1.0);
        for (i, tangent) in numbers(&outputs[1]).into_iter().enumerate() {
            assert_eq!(tangent, if x[i] >= 0.0 { du[i] } else { -du[i] });
            assert!((tangent - (plus[i] - minus[i]) / (2.0 * epsilon)).abs() < 1e-9);
        }
        assert_eq!(numbers(&outputs[0]), x.map(f64::abs));
    }
}

#[test]
fn aggregate_abs_uses_canonical_zero_and_nonfinite_kink_rules() {
    let x = [-f64::INFINITY, -0.0_f64, 0.0, f64::INFINITY, f64::NAN];
    let du = [0.5, -0.0_f64, 2.0, -3.0, 4.0];
    let table = abs_table(vec![5], false);
    let owner = &table.owners()[0];
    let ty = &owner.inputs()[0];
    let outputs =
        eval_pure_call_directional(&table, owner.id(), &[typed(ty, &x), typed(ty, &du)]).unwrap();
    for (i, actual) in numbers(&outputs[0]).into_iter().enumerate() {
        if x[i].is_nan() {
            assert!(actual.is_nan());
        } else {
            assert_eq!(actual.to_bits(), x[i].abs().to_bits());
        }
    }
    for (i, actual) in numbers(&outputs[1]).into_iter().enumerate() {
        let expected = if x[i] >= 0.0 { du[i] } else { -du[i] };
        assert_eq!(actual.to_bits(), expected.to_bits());
    }
}

#[test]
fn aggregate_abs_constant_input_preserves_known_zero_tangent() {
    let table = abs_table(vec![2, 2], true);
    let owner = &table.owners()[0];
    let outputs = eval_pure_call_directional(&table, owner.id(), &[]).unwrap();
    assert_eq!(numbers(&outputs[0]), [2.0; 4]);
    assert_eq!(numbers(&outputs[1]), [0.0; 4]);
    assert!(
        !owner
            .directional()
            .unwrap()
            .body()
            .operations()
            .iter()
            .any(|operation| matches!(operation.operation(), SolveOperation::Map { .. }))
    );
}

#[test]
fn aggregate_abs_map_replays_and_keeps_size_and_source_independent_of_extent() {
    let small = abs_table(vec![2, 2], false);
    let large = abs_table(vec![64, 64], false);
    let replayed: SolvePureCallTable =
        serde_json::from_str(&serde_json::to_string(&large).unwrap()).unwrap();
    assert_eq!(large, replayed);
    let counts = |table: &SolvePureCallTable| {
        let body = table.owners()[0].directional().unwrap().body();
        let maps = body
            .operations()
            .iter()
            .filter_map(|op| match op.operation() {
                SolveOperation::Map { body, .. } => Some(body),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(maps.len(), 1);
        assert!(
            maps[0]
                .body()
                .operations()
                .iter()
                .all(|op| op.provenance() == span(1602))
        );
        (
            body.operations().len(),
            body.register_types().len(),
            maps[0].body().operations().len(),
        )
    };
    assert_eq!(counts(&small), counts(&large));
}

#[test]
fn aggregate_abs_retains_integer_aggregate_and_profile_domain_refusals() {
    for integer in [true, false] {
        let arithmetic = SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            SolveIntegerDomain::construct(-1, 1).unwrap(),
        );
        let element = if integer {
            SolveScalarType::integer(arithmetic)
        } else {
            SolveScalarType::real(arithmetic)
        };
        let tensor = SolveValueType::tensor(element, vec![2]).unwrap();
        let result = SolvePureCallTable::construct(arithmetic, |table| {
            table.add_owner(
                identity(611),
                vec![tensor.clone()],
                vec![SolvePureCallOutput::result(tensor)],
                span(1610),
                |builder, inputs, outputs| {
                    let input = builder.load(inputs[0], span(1611))?;
                    let value = builder.unary(SolveUnaryOperator::Abs, input, span(1612))?;
                    builder.store(outputs[0], value, span(1613))
                },
            )?;
            Ok(())
        });
        if integer {
            assert!(result.unwrap().owners()[0].directional().is_none());
        } else {
            assert!(
                matches!(result,Err(SolveProgramConstructionError::InvalidMap { provenance }) if provenance == span(1612))
            );
        }
    }
}
