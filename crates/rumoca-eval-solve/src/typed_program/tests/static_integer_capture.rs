//! Literal Integer call inputs keep their checked value in scalar projections.
use super::*;

fn comparison_table() -> SolvePureCallTable {
    let arithmetic = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::construct(-3, 3).unwrap(),
    );
    let integer = SolveValueType::scalar(SolveScalarType::Integer(arithmetic.integer_domain()));
    let boolean = SolveValueType::scalar(SolveScalarType::Boolean);
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            identity(203),
            vec![integer],
            vec![SolvePureCallOutput::result(boolean)],
            span(1400),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1401))?;
                let zero =
                    builder.constant(SolveValue::integer(arithmetic, 0).unwrap(), span(1402))?;
                let positive =
                    builder.compare(SolveCompareOperator::Greater, input, zero, span(1403))?;
                builder.store(outputs[0], positive, span(1404))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

fn comparison_row(
    table: &SolvePureCallTable,
    mut prefix: Vec<LinearOp>,
    input: u32,
) -> ScalarProgramBlock {
    prefix.push(LinearOp::PureCall {
        dst_start: 2,
        input_starts: vec![input].into_boxed_slice(),
        site: table.owners()[0].call_site(),
    });
    prefix.push(LinearOp::StoreOutput { src: 2 });
    ScalarProgramBlock::with_output_indices(vec![prefix], vec![span(1405)], vec![0]).unwrap()
}

#[test]
fn literal_integer_capture_preserves_value_and_move_time() {
    let table = comparison_table();
    for (value, expected) in [(-2.0, 0.0), (0.0, 0.0), (1.0, 1.0)] {
        let original = comparison_row(
            &table,
            vec![
                LinearOp::Const { dst: 0, value },
                LinearOp::Move { dst: 1, src: 0 },
                LinearOp::Const {
                    dst: 0,
                    value: -value,
                },
            ],
            1,
        );
        let lowered = crate::lower_scalar_ops(&original.programs()[0], Some(&table))
            .expect("literal checked Integer capture");
        let projected =
            ScalarProgramBlock::with_output_indices(vec![lowered], vec![span(1405)], vec![0])
                .unwrap();
        for block in [original, projected] {
            let mut output = [f64::NAN];
            crate::eval_scalar_program_block_with_context(
                &block,
                &[],
                &[],
                0.0,
                crate::RowEvalContext {
                    pure_calls: Some(&table),
                    ..Default::default()
                },
                &mut output,
            )
            .unwrap();
            assert_eq!(
                output,
                [expected],
                "comparison uses the value copied before overwrite"
            );
        }
    }
}

#[test]
fn overwritten_literal_integer_capture_remains_runtime_refusal() {
    let table = comparison_table();
    let original = comparison_row(
        &table,
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::LoadY { dst: 0, index: 0 },
        ],
        0,
    );
    assert!(crate::lower_scalar_ops(&original.programs()[0], Some(&table)).is_none());
    for (input, expected) in [(-2.0, 0.0), (1.0, 1.0)] {
        let mut output = [f64::NAN];
        crate::eval_scalar_program_block_with_context(
            &original,
            &[input],
            &[],
            0.0,
            crate::RowEvalContext {
                pure_calls: Some(&table),
                ..Default::default()
            },
            &mut output,
        )
        .unwrap();
        assert_eq!(output, [expected]);
    }
}

#[test]
fn invalid_literal_integer_capture_preserves_checked_domain_refusal() {
    let table = comparison_table();
    for value in [0.5, f64::NAN, f64::INFINITY, -4.0, 4.0] {
        let original = comparison_row(&table, vec![LinearOp::Const { dst: 0, value }], 0);
        assert!(crate::lower_scalar_ops(&original.programs()[0], Some(&table)).is_none());
        let mut output = [f64::NAN];
        assert!(
            crate::eval_scalar_program_block_with_context(
                &original,
                &[],
                &[],
                0.0,
                crate::RowEvalContext {
                    pure_calls: Some(&table),
                    ..Default::default()
                },
                &mut output
            )
            .is_err()
        );
    }
}
