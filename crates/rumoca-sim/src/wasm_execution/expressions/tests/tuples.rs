use super::*;
use rumoca_solver::CompiledSolveExpression;

#[test]
fn portable_complete_program_returns_its_entire_tuple_with_original_placement() {
    let span = Span::from_offsets(SourceId::from_source_name("portable_tuple.mo"), 7, 29);
    let source = ScalarProgramBlock::with_output_indices(
        vec![vec![
            LinearOp::Const {
                dst: 0,
                value: -0.0,
            },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::LoadY { dst: 1, index: 0 },
            LinearOp::StoreOutput { src: 1 },
        ]],
        vec![span],
        vec![4, 1],
    )
    .unwrap();
    let layout = VarLayout::from_parts(Default::default(), 1, 0);
    let code = WasmExpression::compile(
        &source,
        &layout,
        &rumoca_ir_solve::SolvePureCallTable::default(),
    )
    .unwrap();
    let mut tuple = Vec::new();
    assert!(
        code.call_program_outputs(0, &[6.0], &[], 0.0, &[], &mut tuple)
            .unwrap()
    );
    assert_eq!(tuple[0].to_bits(), (-0.0_f64).to_bits());
    assert_eq!(tuple[1], 6.0);
    assert_eq!(
        code.call_program_output((0, 1), &[9.0], &[], 0.0, &[])
            .unwrap(),
        Some(9.0)
    );
    let mut output = [17.0; 5];
    code.call(&[6.0], &[], 0.0, &[], &mut output).unwrap();
    assert_eq!(output[4].to_bits(), (-0.0_f64).to_bits());
    assert_eq!(&output[..4], &[0.0, 6.0, 0.0, 0.0]);
}

#[test]
fn portable_block_commits_no_partial_outputs_after_a_later_source_fault() {
    let span = Span::from_offsets(SourceId::from_source_name("portable_fault.mo"), 5, 41);
    let source = ScalarProgramBlock::with_program_spans(
        vec![
            vec![
                LinearOp::Const {
                    dst: 0,
                    value: 23.0,
                },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::LoadY { dst: 1, index: 0 },
                LinearOp::LoadIndexedRegister {
                    dst: 2,
                    base: 0,
                    stride: 1,
                    dimensions: vec![1].into_boxed_slice(),
                    indices: vec![rumoca_ir_solve::TensorIndex::Runtime(1)].into_boxed_slice(),
                },
                LinearOp::StoreOutput { src: 2 },
            ],
        ],
        vec![span; 2],
    )
    .unwrap();
    let layout = VarLayout::from_parts(Default::default(), 1, 0);
    let code = WasmExpression::compile(
        &source,
        &layout,
        &rumoca_ir_solve::SolvePureCallTable::default(),
    )
    .unwrap();
    let mut output = [17.0, -0.0];
    let before = output.map(f64::to_bits);
    let error = code.call(&[2.0], &[], 0.0, &[], &mut output).unwrap_err();
    assert!(matches!(
        error,
        rumoca_solver::RuntimeSolveError::SourceFault { .. }
    ));
    assert_eq!(error.source_span(), Some(span));
    assert_eq!(output.map(f64::to_bits), before);
}

#[test]
fn portable_selected_output_keeps_unselected_nonfinite_values_local() {
    let source = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const {
                dst: 0,
                value: f64::NAN,
            },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::Const {
                dst: 1,
                value: -0.0,
            },
            LinearOp::StoreOutput { src: 1 },
        ]],
        vec![Span::from_offsets(
            SourceId::from_source_name("selected_nonfinite.mo"),
            0,
            19,
        )],
    )
    .unwrap();
    let layout = VarLayout::default();
    let code = WasmExpression::compile(
        &source,
        &layout,
        &rumoca_ir_solve::SolvePureCallTable::default(),
    )
    .unwrap();
    let selected = code
        .call_program_output((0, 1), &[], &[], 0.0, &[])
        .unwrap()
        .unwrap();
    assert_eq!(selected.to_bits(), (-0.0_f64).to_bits());
    let mut tuple = Vec::new();
    code.call_program_outputs(0, &[], &[], 0.0, &[], &mut tuple)
        .unwrap();
    assert!(tuple[0].is_nan());
    assert_eq!(tuple[1].to_bits(), (-0.0_f64).to_bits());
}

#[test]
fn real_program_admission_checks_even_unused_loads_against_issued_layout() {
    let span = Span::from_offsets(SourceId::from_source_name("unused_invalid_load.mo"), 0, 31);
    for load in [
        LinearOp::LoadY { dst: 0, index: 2 },
        LinearOp::LoadP { dst: 0, index: 1 },
    ] {
        let source = ScalarProgramBlock::with_program_spans(
            vec![vec![
                load,
                LinearOp::Const { dst: 1, value: 3.0 },
                LinearOp::StoreOutput { src: 1 },
            ]],
            vec![span],
        )
        .unwrap();
        let layout = VarLayout::from_parts(Default::default(), 2, 1);
        assert!(
            WasmExpression::compile(
                &source,
                &layout,
                &rumoca_ir_solve::SolvePureCallTable::default()
            )
            .is_err()
        );
    }
}

#[test]
fn emitted_extrema_preserve_canonical_nan_and_signed_zero_ties() {
    for op in [
        rumoca_ir_solve::BinaryOp::Min,
        rumoca_ir_solve::BinaryOp::Max,
    ] {
        for (lhs, rhs) in [
            (f64::NAN, 2.0),
            (2.0, f64::NAN),
            (f64::NAN, f64::NAN),
            (-0.0, 0.0),
            (0.0, -0.0),
            (f64::INFINITY, f64::NEG_INFINITY),
        ] {
            let source = ScalarProgramBlock::with_program_spans(
                vec![vec![
                    LinearOp::Const { dst: 0, value: lhs },
                    LinearOp::Const { dst: 1, value: rhs },
                    LinearOp::Binary {
                        dst: 2,
                        op,
                        lhs: 0,
                        rhs: 1,
                    },
                    LinearOp::StoreOutput { src: 2 },
                ]],
                vec![Span::from_offsets(
                    SourceId::from_source_name("emitted_extrema.mo"),
                    0,
                    29,
                )],
            )
            .unwrap();
            let code = WasmExpression::compile(
                &source,
                &VarLayout::default(),
                &rumoca_ir_solve::SolvePureCallTable::default(),
            )
            .unwrap();
            let mut actual = [0.0];
            code.call(&[], &[], 0.0, &[], &mut actual).unwrap();
            let mut expected = [0.0];
            rumoca_eval_solve::eval_scalar_program_block(
                &source,
                &[],
                &[],
                0.0,
                None,
                &mut expected,
            )
            .unwrap();
            assert!(
                actual[0].to_bits() == expected[0].to_bits()
                    || actual[0].is_nan() && expected[0].is_nan()
            );
        }
    }
}
