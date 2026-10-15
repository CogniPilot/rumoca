use super::*;
use rumoca_solver::CompiledSolveJacobianExpression;

fn same_as_source(operations: Vec<LinearOp>, y: &[f64], p: &[f64], seed: &[f64], outputs: usize) {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("original_dual.mo"),
        0,
        37,
    );
    let source = ScalarProgramBlock::with_program_spans(vec![operations], vec![span]).unwrap();
    let layout = VarLayout::from_parts(Default::default(), y.len(), p.len());
    let calls = rumoca_ir_solve::SolvePureCallTable::default();
    let code = WasmJacobian::compile(&source, &layout, &calls).unwrap();
    let prepared = rumoca_eval_solve::PreparedScalarProgramBlock::new(source).unwrap();
    let mut expected = vec![0.0; outputs];
    prepared
        .eval_with_context(
            y,
            p,
            0.0,
            rumoca_eval_solve::RowEvalContext {
                seed: Some(seed),
                pure_calls: Some(&calls),
                ..Default::default()
            },
            &mut expected,
        )
        .unwrap();
    let mut actual = vec![19.0; outputs];
    code.call(y, p, 0.0, seed, &[], &mut actual).unwrap();
    for (actual, expected) in actual.iter().zip(&expected) {
        assert!(
            actual.to_bits() == expected.to_bits() || actual.is_nan() && expected.is_nan(),
            "{actual:?} != {expected:?}"
        );
    }
    let mut tuple = Vec::new();
    assert!(
        code.call_program_outputs(
            0,
            rumoca_eval_solve::JacobianEvalInputs { y, p, t: 0.0, seed },
            &[],
            &mut tuple
        )
        .unwrap()
    );
    assert_eq!(tuple.len(), outputs);
    for (offset, expected) in expected.iter().enumerate() {
        let selected = code
            .call_program_output((0, offset), y, p, 0.0, seed, &[])
            .unwrap()
            .unwrap();
        assert!(selected.to_bits() == expected.to_bits() || selected.is_nan() && expected.is_nan());
    }
    assert!(
        code.call_program_output((0, outputs), y, p, 0.0, seed, &[])
            .is_err()
    );
    assert!(
        code.call_program_output((1, 0), y, p, 0.0, seed, &[])
            .is_err()
    );
    for (actual, expected) in tuple.iter().zip(expected) {
        assert!(actual.to_bits() == expected.to_bits() || actual.is_nan() && expected.is_nan());
    }
}

fn load(
    dst_start: u32,
    input: rumoca_ir_solve::TensorInputKind,
    count: usize,
    seed_start: usize,
) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start,
        input,
        input_start: 0,
        count,
        seed_start: Some(seed_start),
        lanes: 2,
    }
}

#[test]
fn emitted_two_lane_arithmetic_matches_original_program_and_division_kinks() {
    for op in [BinaryOp::Add, BinaryOp::Sub, BinaryOp::Mul, BinaryOp::Div] {
        same_as_source(
            vec![
                load(0, rumoca_ir_solve::TensorInputKind::Y, 2, 0),
                load(4, rumoca_ir_solve::TensorInputKind::P, 2, 2),
                LinearOp::TensorBinary {
                    dst_start: 8,
                    op,
                    lhs_start: 0,
                    rhs_start: 4,
                    count: 2,
                    lhs_stride: 1,
                    rhs_stride: 1,
                    lanes: 2,
                },
                LinearOp::StoreOutputRange {
                    start: 8,
                    count: 4,
                    stride: 1,
                },
            ],
            &[0.0, 1.0],
            &[0.0, 0.0],
            &[2.0, 3.0, 4.0, 5.0],
            4,
        );
    }
}

#[test]
fn emitted_two_lane_matrix_and_cross_keep_original_accumulation_order() {
    same_as_source(
        vec![
            load(0, rumoca_ir_solve::TensorInputKind::Y, 4, 0),
            load(8, rumoca_ir_solve::TensorInputKind::P, 4, 4),
            LinearOp::MatrixMultiply {
                dst_start: 16,
                lhs_start: 0,
                rhs_start: 8,
                rows: 2,
                inner: 2,
                columns: 2,
                lanes: 2,
            },
            LinearOp::StoreOutputRange {
                start: 16,
                count: 8,
                stride: 1,
            },
        ],
        &[1.0, -2.0, 3.0, -4.0],
        &[2.0, 4.0, 6.0, 8.0],
        &[1.0, 2.0, 3.0, 4.0, -1.0, -2.0, -3.0, -4.0],
        8,
    );
    same_as_source(
        vec![
            load(0, rumoca_ir_solve::TensorInputKind::Y, 3, 0),
            load(6, rumoca_ir_solve::TensorInputKind::P, 3, 3),
            LinearOp::TensorCross {
                dst_start: 12,
                lhs_start: 0,
                rhs_start: 6,
                lanes: 2,
            },
            LinearOp::StoreOutputRange {
                start: 12,
                count: 6,
                stride: 1,
            },
        ],
        &[1.0, -2.0, 3.0],
        &[2.0, 4.0, 6.0],
        &[1.0, 2.0, 3.0, -1.0, -2.0, -3.0],
        6,
    );
}
