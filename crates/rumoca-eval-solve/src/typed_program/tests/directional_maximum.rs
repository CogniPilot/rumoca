use super::*;
use rumoca_ir_solve::{SolveOperation, SolveTypeConstructionError, TypedProgram};

fn maximum_table(
    arithmetic: SolveArithmeticProfile,
    dimensions: Vec<u32>,
    constant: Option<f64>,
    parameter: bool,
) -> Result<SolvePureCallTable, SolveProgramConstructionError> {
    let scalar = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let tensor = SolveValueType::tensor(scalar.element_type(), dimensions.clone()).unwrap();
    let mut inputs = if constant.is_some() {
        vec![]
    } else {
        vec![tensor]
    };
    if parameter {
        inputs.push(scalar.clone());
    }
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(620),
            inputs,
            vec![SolvePureCallOutput::result(scalar)],
            span(1700),
            |builder, inputs, outputs| {
                let mut value = match constant {
                    Some(value) => {
                        let scalar =
                            builder.constant(SolveValue::real(arithmetic, value), span(1701))?;
                        builder.fill(scalar, dimensions, span(1701))?
                    }
                    None => builder.load(inputs[0], span(1701))?,
                };
                if parameter {
                    let offset = builder.load(inputs[1], span(1702))?;
                    value = builder.broadcast_binary(
                        SolveBinaryOperator::Add,
                        value,
                        offset,
                        false,
                        span(1702),
                    )?;
                }
                let result = builder.reduce(SolveReductionOperator::Maximum, value, span(1703))?;
                builder.store(outputs[0], result, span(1704))
            },
        )?;
        Ok(())
    })
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

fn number(value: &TypedValue) -> f64 {
    match value.elements()[0] {
        SolveValueKind::Real64(bits) => f64::from_bits(bits),
        _ => panic!("Binary64 fixture"),
    }
}

fn evaluate(table: &SolvePureCallTable, x: &[f64], seed: &[f64]) -> Vec<TypedValue> {
    let owner = &table.owners()[0];
    let input_type = &owner.inputs()[0];
    eval_pure_call_directional(
        table,
        owner.id(),
        &[typed(input_type, x), typed(input_type, seed)],
    )
    .unwrap()
}

#[test]
fn maximum_directional_matches_analytic_and_finite_difference_for_singleton_vector_and_tensor() {
    for dimensions in [vec![1], vec![4], vec![2, 2]] {
        let table =
            maximum_table(profile(SolveRealFormat::Binary64), dimensions, None, false).unwrap();
        let ty = &table.owners()[0].inputs()[0];
        let count = ty.scalar_count() as usize;
        let x = &[-3.0_f64, 2.0, 7.0, 1.0][..count];
        let seed = &[0.3, -0.2, 0.4, 0.7][..count];
        let expected = if count == 1 { 0 } else { 2 };
        let outputs = evaluate(&table, x, seed);
        assert_eq!(number(&outputs[0]), x[expected]);
        assert_eq!(number(&outputs[1]), seed[expected]);
        let epsilon = 1e-6;
        let shifted = |sign: f64| {
            let input = x
                .iter()
                .zip(seed)
                .map(|(x, d)| x + sign * epsilon * d)
                .collect::<Vec<_>>();
            number(
                &eval_pure_call(&table, table.owners()[0].id(), &[typed(ty, &input)]).unwrap()[0],
            )
        };
        assert!(
            (number(&outputs[1]) - (shifted(1.0) - shifted(-1.0)) / (2.0 * epsilon)).abs() < 1e-9
        );
    }
}

#[test]
fn maximum_directional_preserves_ordered_nan_ties_infinities_and_signed_zero() {
    let table = maximum_table(profile(SolveRealFormat::Binary64), vec![3], None, false).unwrap();
    let seed = [11.0, 22.0, 33.0];
    for (x, expected) in [
        ([3.0, 3.0, 1.0], 11.0),
        ([f64::INFINITY, f64::INFINITY, f64::NEG_INFINITY], 11.0),
        ([f64::NEG_INFINITY; 3], 11.0),
        ([-0.0, 0.0, -0.0], 11.0),
        ([2.0, f64::NAN, 1.0], 22.0),
        ([f64::NAN, 2.0, 1.0], 22.0),
        ([2.0, 1.0, f64::NAN], 33.0),
    ] {
        let outputs = evaluate(&table, &x, &seed);
        let primal = eval_pure_call(
            &table,
            table.owners()[0].id(),
            &[typed(&table.owners()[0].inputs()[0], &x)],
        )
        .unwrap();
        assert_eq!(number(&outputs[0]).to_bits(), number(&primal[0]).to_bits());
        assert_eq!(number(&outputs[1]), expected);
    }
    let singleton =
        maximum_table(profile(SolveRealFormat::Binary64), vec![1], None, false).unwrap();
    for value in [-0.0, f64::from_bits(0x7ff8_0000_0000_1234)] {
        let outputs = evaluate(&singleton, &[value], &[-0.0]);
        assert_eq!(number(&outputs[0]).to_bits(), value.to_bits());
        assert_eq!(number(&outputs[1]).to_bits(), (-0.0_f64).to_bits());
    }
}

#[test]
fn maximum_directional_keeps_parameter_seed_and_constant_zero_fact() {
    let table = maximum_table(profile(SolveRealFormat::Binary64), vec![2, 2], None, true).unwrap();
    let owner = &table.owners()[0];
    let outputs = eval_pure_call_directional(
        &table,
        owner.id(),
        &[
            typed(&owner.inputs()[0], &[-3.0, 2.0, 7.0, 1.0]),
            typed(&owner.inputs()[0], &[0.3, -0.2, 0.4, 0.7]),
            typed(&owner.inputs()[1], &[5.0]),
            typed(&owner.inputs()[1], &[0.6]),
        ],
    )
    .unwrap();
    assert_eq!(number(&outputs[0]), 12.0);
    assert_eq!(number(&outputs[1]), 1.0);
    let constant = maximum_table(
        profile(SolveRealFormat::Binary64),
        vec![2, 2],
        Some(-2.0),
        false,
    )
    .unwrap();
    let outputs = eval_pure_call_directional(&constant, constant.owners()[0].id(), &[]).unwrap();
    assert_eq!(number(&outputs[0]), -2.0);
    assert_eq!(number(&outputs[1]), 0.0);
    assert!(
        !constant.owners()[0]
            .directional()
            .unwrap()
            .body()
            .operations()
            .iter()
            .any(|op| matches!(op.operation(), SolveOperation::Fold { .. }))
    );
}

fn region_counts(program: &TypedProgram, counts: &mut Vec<(usize, usize)>) {
    counts.push((program.operations().len(), program.register_types().len()));
    for operation in program.operations() {
        assert_eq!(operation.provenance(), span(1703));
        if let SolveOperation::Conditional {
            if_true, if_false, ..
        } = operation.operation()
        {
            region_counts(if_true.body(), counts);
            region_counts(if_false.body(), counts);
        }
    }
}

#[test]
fn maximum_fold_replays_and_preserves_compact_shape_and_source() {
    let small = maximum_table(profile(SolveRealFormat::Binary64), vec![2, 2], None, false).unwrap();
    let large = maximum_table(
        profile(SolveRealFormat::Binary64),
        vec![64, 64],
        None,
        false,
    )
    .unwrap();
    let replayed: SolvePureCallTable =
        serde_json::from_str(&serde_json::to_string(&large).unwrap()).unwrap();
    assert_eq!(large, replayed);
    let counts = |table: &SolvePureCallTable| {
        let body = table.owners()[0].directional().unwrap().body();
        let mut counts = vec![(body.operations().len(), body.register_types().len())];
        let folds = body
            .operations()
            .iter()
            .filter_map(|op| match op.operation() {
                SolveOperation::Fold {
                    domain, transition, ..
                } => {
                    assert_eq!(domain.binders.len(), 2);
                    Some(transition.body())
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(folds.len(), 1);
        assert_eq!(
            body.operations()
                .iter()
                .filter(|op| matches!(op.operation(), SolveOperation::Reduce { .. }))
                .count(),
            1
        );
        region_counts(folds[0], &mut counts);
        counts
    };
    assert_eq!(counts(&small), counts(&large));
}

#[test]
fn maximum_keeps_checked_extent_and_integer_domain_refusals() {
    let limited = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::construct(-1, 1).unwrap(),
    );
    let result = maximum_table(limited, vec![2], None, false);
    assert!(
        matches!(result,Err(SolveProgramConstructionError::InvalidFold {provenance}) if provenance == span(1703))
    );
    let scalar = SolveScalarType::real(profile(SolveRealFormat::Binary64));
    assert_eq!(
        SolveValueType::tensor(scalar, vec![0]),
        Err(SolveTypeConstructionError::ZeroTensorExtent)
    );
    assert_eq!(
        SolveValueType::tensor(scalar, vec![u32::MAX, 2]),
        Err(SolveTypeConstructionError::TensorScalarCountOverflow)
    );
}

#[test]
fn maximum_admission_retains_other_reduction_and_discrete_refusals() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    for (operator, element) in [
        (
            SolveReductionOperator::Maximum,
            SolveScalarType::integer(arithmetic),
        ),
        (SolveReductionOperator::Maximum, SolveScalarType::Boolean),
        (
            SolveReductionOperator::Minimum,
            SolveScalarType::real(arithmetic),
        ),
        (
            SolveReductionOperator::Product,
            SolveScalarType::real(arithmetic),
        ),
    ] {
        let tensor = SolveValueType::tensor(element, vec![2]).unwrap();
        let scalar = SolveValueType::scalar(element);
        let table = SolvePureCallTable::construct(arithmetic, |table| {
            table.add_owner(
                identity(621),
                vec![tensor],
                vec![SolvePureCallOutput::result(scalar)],
                span(1710),
                |builder, inputs, outputs| {
                    let input = builder.load(inputs[0], span(1711))?;
                    let result = builder.reduce(operator, input, span(1712))?;
                    builder.store(outputs[0], result, span(1713))
                },
            )?;
            Ok(())
        })
        .unwrap();
        assert!(table.owners()[0].directional().is_none());
    }
}

#[test]
fn maximum_directional_preserves_binary32_nan_order_and_signed_zero() {
    let arithmetic = profile(SolveRealFormat::Binary32);
    let typed32 = |ty: &SolveValueType, values: &[f32]| {
        TypedValue::construct(
            ty.clone(),
            values
                .iter()
                .map(|v| SolveValueKind::Real32(v.to_bits()))
                .collect(),
        )
        .unwrap()
    };
    let bits = |value: &TypedValue| match value.elements()[0] {
        SolveValueKind::Real32(bits) => bits,
        _ => panic!("Binary32 fixture"),
    };
    let table = maximum_table(arithmetic, vec![3], None, false).unwrap();
    let owner = &table.owners()[0];
    for (x, seed, primal, tangent) in [
        (
            [2.0_f32, f32::NAN, 1.0],
            [11.0, 22.0, 33.0],
            2.0_f32,
            22.0_f32,
        ),
        ([-0.0, 0.0, -0.0], [-0.0, 22.0, 33.0], 0.0, -0.0),
        (
            [f32::INFINITY, f32::INFINITY, f32::NEG_INFINITY],
            [11.0, 22.0, 33.0],
            f32::INFINITY,
            11.0,
        ),
    ] {
        let outputs = eval_pure_call_directional(
            &table,
            owner.id(),
            &[
                typed32(&owner.inputs()[0], &x),
                typed32(&owner.inputs()[0], &seed),
            ],
        )
        .unwrap();
        assert_eq!(f32::from_bits(bits(&outputs[0])), primal);
        assert_eq!(bits(&outputs[1]), tangent.to_bits());
    }
    let singleton = maximum_table(arithmetic, vec![1], None, false).unwrap();
    let owner = &singleton.owners()[0];
    for x in [-0.0, f32::from_bits(0x7fc0_1234)] {
        let outputs = eval_pure_call_directional(
            &singleton,
            owner.id(),
            &[
                typed32(&owner.inputs()[0], &[x]),
                typed32(&owner.inputs()[0], &[-0.0]),
            ],
        )
        .unwrap();
        assert_eq!(bits(&outputs[0]), x.to_bits());
        assert_eq!(bits(&outputs[1]), (-0.0_f32).to_bits());
    }
}
