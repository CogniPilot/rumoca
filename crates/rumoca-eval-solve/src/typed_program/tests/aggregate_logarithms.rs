//! Ranked logarithms use the same checked directional owner as scalar lanes.
use super::*;

fn logarithm_table(operator: SolveUnaryOperator, dimensions: Vec<u32>) -> SolvePureCallTable {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let tensor = SolveValueType::tensor(SolveScalarType::real(arithmetic), dimensions).unwrap();
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(730),
            vec![tensor.clone()],
            vec![SolvePureCallOutput::result(tensor)],
            span(1200),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1201))?;
                let result = builder.unary(operator, input, span(1202))?;
                builder.store(outputs[0], result, span(1203))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn tensor_value(table: &SolvePureCallTable, values: &[f64]) -> TypedValue {
    TypedValue::construct(
        table.owners()[0].inputs()[0].clone(),
        values
            .iter()
            .map(|&v| real_kind(SolveRealFormat::Binary64, v))
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
            other => panic!("expected Binary64 logarithm cell, got {other:?}"),
        })
        .collect()
}

fn reference_logarithm(operator: SolveUnaryOperator, x: f64) -> f64 {
    if operator == SolveUnaryOperator::Log {
        x.ln()
    } else {
        x.log10()
    }
}

fn logarithm_derivative_scale(operator: SolveUnaryOperator) -> f64 {
    if operator == SolveUnaryOperator::Log {
        1.0
    } else {
        std::f64::consts::LN_10
    }
}

#[test]
fn aggregate_logarithm_directional_matches_ranked_finite_differences() {
    let values = [1.5, 2.5, 4.5, 8.5];
    let seeds = [0.7, -1.1, 2.3, -3.2];
    let h = 1e-6;
    for dimensions in [vec![4], vec![2, 2]] {
        for operator in [SolveUnaryOperator::Log, SolveUnaryOperator::Log10] {
            let table = logarithm_table(operator, dimensions.clone());
            let owner = &table.owners()[0];
            assert!(
                owner.directional().is_some(),
                "ranked {operator:?} owner is required"
            );
            let result = eval_pure_call_directional(
                &table,
                owner.id(),
                &[tensor_value(&table, &values), tensor_value(&table, &seeds)],
            )
            .unwrap();
            assert_eq!(result.len(), 2);
            for output in &result {
                assert_eq!(output.value_type().dimensions(), dimensions);
                assert_eq!(output.elements().len(), 4);
            }
            let primal = numbers(&result[0]);
            let derivative = numbers(&result[1]);
            let logarithm = |x: f64| reference_logarithm(operator, x);
            for index in 0..4 {
                assert_eq!(primal[index], logarithm(values[index]));
                let expected = seeds[index] / values[index] / logarithm_derivative_scale(operator);
                let finite_difference = (logarithm(values[index] + h * seeds[index])
                    - logarithm(values[index] - h * seeds[index]))
                    / (2.0 * h);
                assert!(derivative[index].is_finite());
                assert!((derivative[index] - expected).abs() < 1e-14);
                assert!((derivative[index] - finite_difference).abs() < 2e-9);
            }
        }
    }
}

#[test]
fn aggregate_logarithm_directional_retains_scalar_nonfinite_partial_policy() {
    let values = [0.0, -0.0, f64::INFINITY, f64::NAN, -2.0];
    let seeds = [1.0, 2.0, 3.0, 4.0, 5.0];
    for operator in [SolveUnaryOperator::Log, SolveUnaryOperator::Log10] {
        let table = logarithm_table(operator, vec![5]);
        let owner = &table.owners()[0];
        assert!(owner.directional().is_some());
        let outputs = eval_pure_call_directional(
            &table,
            owner.id(),
            &[tensor_value(&table, &values), tensor_value(&table, &seeds)],
        )
        .unwrap();
        assert_eq!(outputs.len(), 2);
        assert_eq!(outputs[0].elements().len(), 5);
        assert_eq!(outputs[1].elements().len(), 5);
        let primal = numbers(&outputs[0]);
        let tangents = numbers(&outputs[1]);
        assert_eq!(primal[0], f64::NEG_INFINITY);
        assert_eq!(primal[1], f64::NEG_INFINITY);
        assert_eq!(primal[2], f64::INFINITY);
        assert!(primal[3].is_nan() && primal[4].is_nan());
        assert_eq!(&tangents[..4], &[0.0; 4]);
        let expected = -2.5 / logarithm_derivative_scale(operator);
        assert!((tangents[4] - expected).abs() < 1e-14);
    }
}
