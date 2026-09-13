use super::*;

fn solve_table() -> SolvePureCallTable {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let matrix = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 2]).unwrap();
    let vector = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2]).unwrap();
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(900),
            vec![matrix, vector.clone()],
            vec![SolvePureCallOutput::result(vector)],
            span(900),
            |builder, inputs, outputs| {
                let matrix = builder.load(inputs[0], span(901))?;
                let rhs = builder.load(inputs[1], span(902))?;
                let solution = builder.linear_solve(matrix, rhs, span(903))?;
                builder.store(outputs[0], solution, span(904))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn real_tensor(dimensions: Vec<u32>, values: &[f64]) -> TypedValue {
    let arithmetic = profile(SolveRealFormat::Binary64);
    TypedValue::construct(
        SolveValueType::tensor(SolveScalarType::real(arithmetic), dimensions).unwrap(),
        values
            .iter()
            .map(|&value| SolveValueKind::Real64(value.to_bits()))
            .collect(),
    )
    .unwrap()
}

fn real_elements(value: &TypedValue) -> Vec<f64> {
    value
        .elements()
        .iter()
        .map(|value| match *value {
            SolveValueKind::Real64(bits) => f64::from_bits(bits),
            _ => panic!("expected Real64 tensor"),
        })
        .collect()
}

#[test]
fn tensor_linear_solve_pivots_and_preserves_uniform_scaling() {
    let table = solve_table();
    for scale in [1.0, 1e-20, 1e20] {
        let matrix = [0.0, 2.0, 3.0, 4.0].map(|value| value * scale);
        let rhs = [-4.0, -5.0].map(|value| value * scale);
        let result = eval_pure_call(
            &table,
            table.owners()[0].id(),
            &[real_tensor(vec![2, 2], &matrix), real_tensor(vec![2], &rhs)],
        )
        .unwrap_or_else(|error| panic!("scale {scale}: {error}"));
        for (actual, expected) in real_elements(&result[0]).iter().zip([1.0, -2.0]) {
            assert!((actual - expected).abs() < 1e-12, "scale {scale}: {actual}");
        }
    }
}

#[test]
fn tensor_linear_solve_directional_relation_matches_differentiated_equation() {
    let table = solve_table();
    let table: SolvePureCallTable =
        serde_json::from_str(&serde_json::to_string(&table).unwrap()).unwrap();
    let result = eval_pure_call_directional(
        &table,
        table.owners()[0].id(),
        &[
            real_tensor(vec![2, 2], &[0.0, 2.0, 3.0, 4.0]),
            real_tensor(vec![2, 2], &[1.0, 0.0, 0.0, -1.0]),
            real_tensor(vec![2], &[-4.0, -5.0]),
            real_tensor(vec![2], &[2.0, 3.0]),
        ],
    )
    .unwrap();
    let value = real_elements(&result[0]);
    let derivative = real_elements(&result[1]);
    assert!((2.0 * derivative[1] + value[0] - 2.0).abs() < 1e-12);
    assert!((3.0 * derivative[0] + 4.0 * derivative[1] - value[1] - 3.0).abs() < 1e-12);
}

#[test]
fn tensor_linear_solve_reports_singular_and_nonfinite_inputs() {
    let table = solve_table();
    for matrix in [[1.0, 2.0, 2.0, 4.0], [f64::NAN, 0.0, 0.0, 1.0]] {
        let error = eval_pure_call(
            &table,
            table.owners()[0].id(),
            &[
                real_tensor(vec![2, 2], &matrix),
                real_tensor(vec![2], &[1.0, 1.0]),
            ],
        )
        .unwrap_err();
        assert!(matches!(error, TypedProgramEvalError::LinearSolve { .. }));
        assert_eq!(error.source_span(), Some(span(903)));
    }
}
