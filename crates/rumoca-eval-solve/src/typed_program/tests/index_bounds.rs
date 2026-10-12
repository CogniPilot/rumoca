use super::*;

#[test]
fn dynamic_tensor_index_preserves_source_bounds_and_provenance() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let matrix = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 3]).unwrap();
    let integer = SolveValueType::scalar(SolveScalarType::integer(arithmetic));
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(3),
            vec![matrix.clone(), integer.clone(), integer.clone()],
            vec![SolvePureCallOutput::result(real)],
            span(20),
            |builder, inputs, outputs| {
                let matrix = builder.load(inputs[0], span(21))?;
                let row = builder.load(inputs[1], span(22))?;
                let column = builder.load(inputs[2], span(23))?;
                let selected = builder.project_element_dynamic(matrix, &[row, column], span(25))?;
                builder.store(outputs[0], selected, span(26))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let matrix = TypedValue::construct(
        matrix,
        (1..=6)
            .map(|value| real_kind(SolveRealFormat::Binary64, f64::from(value)))
            .collect(),
    )
    .unwrap();
    let integer_value = |value| {
        TypedValue::construct(integer.clone(), vec![SolveValueKind::Integer(value)]).unwrap()
    };
    let owner = table.owners()[0].id();
    let selected = eval_pure_call(
        &table,
        owner,
        &[matrix.clone(), integer_value(2), integer_value(3)],
    )
    .unwrap();
    assert_eq!(
        selected[0].elements(),
        [real_kind(SolveRealFormat::Binary64, 6.0)]
    );
    for (row, column, axis, index, extent) in [
        (0, 1, 1, 0, 2),
        (-1, 1, 1, -1, 2),
        (3, 1, 1, 3, 2),
        (i64::MIN, 1, 1, i64::MIN, 2),
        (i64::MAX, 1, 1, i64::MAX, 2),
        (1, 4, 2, 4, 3),
        (1, i64::MAX, 2, i64::MAX, 3),
    ] {
        let error = eval_pure_call(
            &table,
            owner,
            &[matrix.clone(), integer_value(row), integer_value(column)],
        )
        .unwrap_err();
        assert_eq!(
            error,
            TypedProgramEvalError::TensorIndexOutOfBounds {
                axis,
                index,
                extent,
                provenance: span(25),
            }
        );
        assert!(matches!(crate::EvalSolveError::from(error),
            crate::EvalSolveError::TensorIndexOutOfBounds {
                axis: actual_axis, index: actual_index, extent: actual_extent, span: Some(actual_span)
            } if (actual_axis, actual_index, actual_extent, actual_span) == (axis, index, extent, span(25))
        ));
    }
}
