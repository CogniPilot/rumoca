//! Supported static coordinates execute through the shared scalar projection.

use super::*;

#[test]
fn scalar_selection_preserves_runtime_boolean_and_checked_constant_unary() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(200),
            vec![boolean, real.clone()],
            vec![SolvePureCallOutput::result(real)],
            span(1280),
            |builder, inputs, outputs| {
                let condition = builder.load(inputs[0], span(1281))?;
                let input = builder.load(inputs[1], span(1282))?;
                let seven =
                    builder.constant(SolveValue::integer(arithmetic, 7).unwrap(), span(1283))?;
                let negative = builder.unary(SolveUnaryOperator::Negate, seven, span(1284))?;
                let negative = builder.convert(
                    SolveConversionOperator::IntegerToReal,
                    negative,
                    span(1285),
                )?;
                let selected = builder.select(condition, negative, input, span(1286))?;
                builder.store(outputs[0], selected, span(1287))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_scalar_call_projection(&table, 0, &[1.0, 3.5], &[-7.0]);
    assert_scalar_call_projection(&table, 0, &[0.0, 3.5], &[3.5]);
}

#[test]
fn static_tensor_views_preserve_column_order_and_update_coordinates() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let matrix = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3, 2]).unwrap();
    let vector = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3]).unwrap();
    for (column, expected) in [
        (1, [1.0, 3.0, 5.0, 9.0, 2.0, 8.0, 4.0, 7.0, 6.0]),
        (2, [2.0, 4.0, 6.0, 1.0, 9.0, 3.0, 8.0, 5.0, 7.0]),
    ] {
        let table = SolvePureCallTable::construct(arithmetic, |table| {
            table.add_owner(
                identity(201),
                vec![matrix.clone(), vector.clone()],
                vec![
                    SolvePureCallOutput::result(vector.clone()),
                    SolvePureCallOutput::result(matrix.clone()),
                ],
                span(1300),
                |builder, inputs, outputs| {
                    let matrix = builder.load(inputs[0], span(1301))?;
                    let vector = builder.load(inputs[1], span(1302))?;
                    let column = builder
                        .constant(SolveValue::integer(arithmetic, column).unwrap(), span(1303))?;
                    let axes = [
                        ProgramTensorViewAxis::Span {
                            origin: 0,
                            extent: 3,
                        },
                        ProgramTensorViewAxis::Index(column),
                    ];
                    let projected = builder.project_view(matrix, &axes, span(1304))?;
                    let updated = builder.update_view(matrix, vector, &axes, span(1305))?;
                    builder.store(outputs[0], projected, span(1306))?;
                    builder.store(outputs[1], updated, span(1307))
                },
            )?;
            Ok(())
        })
        .unwrap();
        assert_scalar_call_projection(
            &table,
            0,
            &[1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 9.0, 8.0, 7.0],
            &expected,
        );
    }
}

#[test]
fn static_tensor_updates_keep_original_reads_and_selection_fallback() {
    let arithmetic = profile(SolveRealFormat::Binary64);
    let matrix = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3, 3]).unwrap();
    let block = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![2, 2]).unwrap();
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let table = SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(202),
            vec![matrix.clone(), block, real.clone()],
            vec![
                SolvePureCallOutput::result(matrix.clone()),
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::result(real),
            ],
            span(1310),
            |builder, inputs, outputs| {
                let matrix = builder.load(inputs[0], span(1311))?;
                let block = builder.load(inputs[1], span(1312))?;
                let replacement = builder.load(inputs[2], span(1313))?;
                let one =
                    builder.constant(SolveValue::integer(arithmetic, 1).unwrap(), span(1314))?;
                let three =
                    builder.constant(SolveValue::integer(arithmetic, 3).unwrap(), span(1315))?;
                let zero =
                    builder.constant(SolveValue::integer(arithmetic, 0).unwrap(), span(1316))?;
                let original = builder.project_element_dynamic(matrix, &[one, one], span(1317))?;
                let with_block = builder.update_slice(matrix, block, vec![1, 1], span(1318))?;
                let updated =
                    builder.update_element(with_block, replacement, &[one, one], span(1319))?;
                let fallback = builder.constant(SolveValue::real(arithmetic, -99.0), span(1320))?;
                let selected =
                    builder.select_element(updated, &[three, three], fallback, span(1321))?;
                let absent = builder.select_element(updated, &[zero, one], fallback, span(1322))?;
                builder.store(outputs[0], updated, span(1323))?;
                builder.store(outputs[1], original, span(1324))?;
                builder.store(outputs[2], selected, span(1325))?;
                builder.store(outputs[3], absent, span(1326))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_scalar_call_projection(
        &table,
        0,
        &[
            0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 10.0, 11.0, 12.0, 13.0, 99.0,
        ],
        &[
            99.0, 1.0, 2.0, 3.0, 10.0, 11.0, 6.0, 12.0, 13.0, 0.0, 13.0, -99.0,
        ],
    );
}
