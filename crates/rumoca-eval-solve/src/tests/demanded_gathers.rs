//! Exact demanded addresses, source provenance, and lazy replay reachability.
use super::*;
use rumoca_ir_solve::{FunctionConditionalProgram, TensorIndex, TensorUpdateSubscript};

#[derive(Clone, Copy)]
enum Mode {
    Checked,
    Fast,
    Lazy,
}

fn row() -> Vec<LinearOp> {
    let mut row = vec![
        LinearOp::LoadP { dst: 0, index: 1 },
        LinearOp::Const { dst: 1, value: 42. },
        // r2 is deliberately uninitialized padding between strided source cells.
        LinearOp::Const { dst: 3, value: 64. },
        LinearOp::LoadIndexedRegister {
            dst: 4,
            base: 1,
            stride: 2,
            dimensions: vec![1, 2].into_boxed_slice(),
            indices: vec![TensorIndex::Constant(0), TensorIndex::Runtime(0)].into_boxed_slice(),
        },
        LinearOp::Const { dst: 5, value: 7. },
        LinearOp::LoadP { dst: 6, index: 0 },
        LinearOp::Select {
            dst: 7,
            cond: 6,
            if_true: 5,
            if_false: 4,
        },
    ];
    row.extend((8..=70).map(|dst| LinearOp::Const {
        dst,
        value: dst as f64,
    }));
    row.push(LinearOp::StoreOutput { src: 7 });
    row
}

fn evaluate(
    mode: Mode,
    row: &[LinearOp],
    parameters: &[f64],
    plan: Option<&PreparedLazyRowPlan>,
    scratch: &mut RowEvalScratch,
) -> Result<f64, EvalSolveError> {
    let input = PreparedRowEval::new(
        row,
        required_registers(row)?,
        &[],
        parameters,
        0.,
        RowEvalContext::default(),
    )
    .with_source_span(Some(fixture_span()))
    .with_lazy_plan(if matches!(mode, Mode::Lazy) {
        plan
    } else {
        None
    });
    eval_program_single(input, !matches!(mode, Mode::Checked), scratch)
}

#[test]
fn demanded_gathers_fault_precisely_with_original_span_in_checked_fast_and_lazy_owners() {
    let row = row();
    let plan = PreparedLazyRowPlan::new(&row, required_registers(&row).unwrap()).unwrap();
    for mode in [Mode::Checked, Mode::Fast, Mode::Lazy] {
        let mut scratch = RowEvalScratch::default();
        for (index, expected) in [(1., 42.), (2., 64.)] {
            assert_eq!(
                evaluate(mode, &row, &[0., index], Some(&plan), &mut scratch).unwrap(),
                expected
            );
        }
        for index in [
            0.,
            -1.,
            3.,
            -9223372036854775808.,
            1.5,
            f64::NAN,
            f64::INFINITY,
            f64::NEG_INFINITY,
            9223372036854775808.,
        ] {
            let error = evaluate(mode, &row, &[0., index], Some(&plan), &mut scratch).unwrap_err();
            assert_eq!(error.source_span(), Some(fixture_span()));
            if !index.is_finite() || index.fract() != 0. || index >= 9223372036854775808. {
                assert!(
                    matches!(error, EvalSolveError::InvalidTensorIndex { axis: 2, .. }),
                    "{error:?}"
                );
            } else {
                assert!(
                    matches!(
                        error,
                        EvalSolveError::TensorIndexOutOfBounds {
                            axis: 2,
                            extent: 2,
                            ..
                        }
                    ),
                    "{error:?}"
                );
            }
        }
        assert_eq!(
            evaluate(mode, &row, &[0., 1.], Some(&plan), &mut scratch).unwrap(),
            42.
        );
    }
}

#[test]
fn lazy_gather_replay_checks_activation_before_prior_branch_reads_and_retains_unused_omission() {
    let row = row();
    let plan = PreparedLazyRowPlan::new(&row, required_registers(&row).unwrap()).unwrap();
    let mut scratch = RowEvalScratch::default();
    // First publish an active branch trace. The next invalid gather is inactive.
    assert_eq!(
        evaluate(Mode::Lazy, &row, &[0., 1.], Some(&plan), &mut scratch).unwrap(),
        42.
    );
    for invalid in [3., f64::NAN, f64::INFINITY] {
        assert_eq!(
            evaluate(Mode::Lazy, &row, &[1., invalid], Some(&plan), &mut scratch).unwrap(),
            7.
        );
        assert!(evaluate(Mode::Lazy, &row, &[0., invalid], Some(&plan), &mut scratch).is_err());
        assert_eq!(
            evaluate(Mode::Lazy, &row, &[1., invalid], Some(&plan), &mut scratch).unwrap(),
            7.
        );
        assert_eq!(
            evaluate(Mode::Lazy, &row, &[0., 2.], Some(&plan), &mut scratch).unwrap(),
            64.
        );
    }
    assert!(
        plan.specialization(&row).is_none(),
        "a runtime gather cannot execute before a trailing native specialization guard"
    );
    let mut constant = row.clone();
    let LinearOp::LoadIndexedRegister { indices, .. } = &mut constant[3] else {
        panic!("gather")
    };
    indices[1] = TensorIndex::Constant(0);
    let literal =
        PreparedLazyRowPlan::new(&constant, required_registers(&constant).unwrap()).unwrap();
    assert_eq!(
        evaluate(
            Mode::Lazy,
            &constant,
            &[0., 3.],
            Some(&literal),
            &mut scratch
        )
        .unwrap(),
        42.
    );
    assert!(
        literal.specialization(&constant).is_some(),
        "canonical literal indices retain totality"
    );
}

fn conditional_row() -> Vec<LinearOp> {
    let program = FunctionConditionalProgram::checked(
        1,
        [1],
        [(
            vec![
                LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 7. },
                LinearOp::StoreOutput { src: 0 },
            ],
        )],
        vec![
            LinearOp::LoadP { dst: 0, index: 1 },
            LinearOp::Const { dst: 1, value: 42. },
            LinearOp::LoadIndexedRegister {
                dst: 2,
                base: 1,
                stride: 1,
                dimensions: vec![1].into_boxed_slice(),
                indices: vec![TensorIndex::Runtime(0)].into_boxed_slice(),
            },
            LinearOp::StoreOutput { src: 2 },
        ],
    )
    .unwrap();
    vec![
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::FunctionConditional {
            dst_start: 1,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        LinearOp::StoreOutput { src: 1 },
    ]
}

#[test]
fn structured_conditional_demanded_gathers_stay_lazy_in_checked_and_fast_execution() {
    let row = conditional_row();
    for mode in [Mode::Checked, Mode::Fast] {
        let mut scratch = RowEvalScratch::default();
        for invalid in [2., 1.5, f64::NAN] {
            assert_eq!(
                evaluate(mode, &row, &[1., invalid], None, &mut scratch).unwrap(),
                7.
            );
            let error = evaluate(mode, &row, &[0., invalid], None, &mut scratch).unwrap_err();
            assert_eq!(error.source_span(), Some(fixture_span()));
        }
        assert_eq!(
            evaluate(mode, &row, &[0., 1.], None, &mut scratch).unwrap(),
            42.
        );
    }
}

fn prepare_mutable_region(mut operations: Vec<LinearOp>) -> PreparedScalarProgramBlock {
    operations.insert(
        0,
        LinearOp::TensorLoad {
            dst_start: 0,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 0,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
    );
    let Some(LinearOp::StoreOutput { src }) = operations.pop() else {
        panic!("one region result")
    };
    operations.extend([
        LinearOp::Const {
            dst: 90,
            value: 1.0,
        },
        LinearOp::Select {
            dst: 91,
            cond: 90,
            if_true: src,
            if_false: src,
        },
    ]);
    operations.extend((100..165).map(|dst| LinearOp::Const {
        dst,
        value: dst as f64,
    }));
    operations.push(LinearOp::StoreOutput { src: 91 });
    let block =
        ScalarProgramBlock::with_program_spans(vec![operations], vec![fixture_span()]).unwrap();
    let prepared = PreparedScalarProgramBlock::new(block).unwrap();
    assert!(
        prepared.has_lazy_row_plan(0),
        "the mixed compact profile remains admitted"
    );
    prepared
}

#[test]
fn mutable_lazy_conditional_keeps_inactive_body_and_original_fault() {
    let prepared = prepare_mutable_region(conditional_row());
    for index in [2.0, 1.5, f64::NAN] {
        let evaluate = |selected| {
            prepared.eval_row_with_context(
                0,
                &[91.0, 92.0],
                &[selected, index],
                0.0,
                RowEvalContext::default(),
            )
        };
        assert_eq!(evaluate(1.0).unwrap(), 7.0);
        assert_eq!(
            evaluate(0.0).unwrap_err().source_span(),
            Some(fixture_span())
        );
        assert_eq!(evaluate(1.0).unwrap(), 7.0);
    }
    assert!(prepared.specialized_row_program(0).is_none());
}

fn guarded_gather_fold() -> Vec<LinearOp> {
    use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
    let update = vec![
        LinearOp::LoadP { dst: 0, index: 1 },
        LinearOp::Const {
            dst: 1,
            value: 42.0,
        },
        LinearOp::LoadIndexedRegister {
            dst: 2,
            base: 1,
            stride: 1,
            dimensions: vec![1].into_boxed_slice(),
            indices: vec![TensorIndex::Runtime(0)].into_boxed_slice(),
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 1,
            step: 1,
        }],
    };
    let program = rumoca_ir_solve::FunctionFoldProgram::checked(domain, 1, 0, update).unwrap();
    vec![
        LinearOp::Const { dst: 0, value: 7.0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::GuardedFunctionFold {
            dst_start: 2,
            initial_start: 0,
            capture_start: 0,
            activation: 1,
            program: std::sync::Arc::new(program),
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

#[test]
fn mutable_lazy_guarded_fold_keeps_inactive_body_and_original_fault() {
    let prepared = prepare_mutable_region(guarded_gather_fold());
    let evaluate = |active, index| {
        prepared.eval_row_with_context(
            0,
            &[91.0, 92.0],
            &[active, index],
            0.0,
            RowEvalContext::default(),
        )
    };
    assert_eq!(evaluate(1.0, 1.0).unwrap(), 42.0);
    for index in [2.0, 1.5, f64::NAN] {
        assert_eq!(evaluate(0.0, index).unwrap(), 7.0);
        assert_eq!(
            evaluate(1.0, index).unwrap_err().source_span(),
            Some(fixture_span())
        );
        assert_eq!(evaluate(0.0, index).unwrap(), 7.0);
    }
    assert!(prepared.specialized_row_program(0).is_none());
}

#[test]
fn fold_carried_and_capture_reads_use_strict_addresses_while_update_no_match_remains_distinct() {
    for carried in [true, false] {
        let read = if carried {
            LinearOp::LoadIndexedFoldCarried {
                dst: 1,
                base: 0,
                stride: 1,
                dimensions: vec![1].into_boxed_slice(),
                indices: vec![TensorIndex::Runtime(0)].into_boxed_slice(),
            }
        } else {
            LinearOp::LoadIndexedFoldCapture {
                dst: 1,
                base: 0,
                stride: 1,
                dimensions: vec![1].into_boxed_slice(),
                indices: vec![TensorIndex::Runtime(0)].into_boxed_slice(),
            }
        };
        let row = [
            LinearOp::LoadP { dst: 0, index: 0 },
            read,
            LinearOp::StoreOutput { src: 1 },
        ];
        for register_safe in [false, true] {
            let mut scratch = RowEvalScratch::default();
            for index in [1., 2., f64::NAN] {
                let parameters = [index];
                let input =
                    PreparedRowEval::new(&row, 2, &[], &parameters, 0., RowEvalContext::default())
                        .with_source_span(Some(fixture_span()))
                        .with_fold_context(&[42.], &[], &[64.]);
                let value = eval_program_single(input, register_safe, &mut scratch);
                assert_fold_read(value, index, carried);
            }
        }
    }
    let subscripts = [TensorUpdateSubscript::Index(TensorIndex::Runtime(0))];
    assert_eq!(
        tensor_update_register_value_offset(&[3], &subscripts, 0, |_| Ok::<_, EvalSolveError>(2.))
            .unwrap(),
        None
    );
    assert_eq!(
        tensor_update_register_value_offset(&[3], &subscripts, 1, |_| Ok::<_, EvalSolveError>(2.))
            .unwrap(),
        Some(0)
    );
}

fn assert_fold_read(value: Result<f64, EvalSolveError>, index: f64, carried: bool) {
    if index == 1. {
        assert_eq!(value.unwrap(), if carried { 42. } else { 64. });
    } else {
        assert_eq!(value.unwrap_err().source_span(), Some(fixture_span()));
    }
}

#[test]
fn prepared_model_program_gather_faults_keep_reachability_after_success_and_failure() {
    let block = ScalarProgramBlock::with_program_spans(vec![row()], vec![fixture_span()]).unwrap();
    let prepared = PreparedScalarProgramBlock::new(block).unwrap();
    assert!(prepared.has_lazy_row_plan(0));
    let evaluate = |first, index| {
        prepared.eval_row_with_context(0, &[], &[first, index], 0., RowEvalContext::default())
    };
    assert_eq!(evaluate(0., 1.).unwrap(), 42.);
    assert_eq!(evaluate(1., 3.).unwrap(), 7.);
    let error = evaluate(0., 3.).unwrap_err();
    assert!(matches!(
        error,
        EvalSolveError::TensorIndexOutOfBounds {
            axis: 2,
            index: 3,
            extent: 2,
            ..
        }
    ));
    assert_eq!(error.source_span(), Some(fixture_span()));
    assert_eq!(evaluate(1., f64::NAN).unwrap(), 7.);
    assert_eq!(evaluate(0., 2.).unwrap(), 64.);
    assert!(prepared.specialized_row_program(0).is_none());
}

#[test]
fn mutable_tensor_registers_preserve_inactive_gather_and_original_fault() {
    let mut operations = row();
    operations.insert(
        0,
        LinearOp::TensorLoad {
            dst_start: 1,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 0,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
    );
    let block =
        ScalarProgramBlock::with_program_spans(vec![operations], vec![fixture_span()]).unwrap();
    let prepared = PreparedScalarProgramBlock::new(block).unwrap();
    let evaluate = |selected, index| {
        prepared.eval_row_with_context(
            0,
            &[91.0, 92.0, 93.0],
            &[selected, index],
            0.0,
            RowEvalContext::default(),
        )
    };
    assert_eq!(evaluate(0.0, 1.0).unwrap(), 42.0);
    for index in [3.0, f64::NAN, f64::INFINITY] {
        assert_eq!(evaluate(1.0, index).unwrap(), 7.0);
        let error = evaluate(0.0, index).unwrap_err();
        assert_eq!(error.source_span(), Some(fixture_span()));
        assert_eq!(evaluate(1.0, index).unwrap(), 7.0);
    }
    assert_eq!(evaluate(0.0, 2.0).unwrap(), 64.0);
}

#[test]
fn mutable_strided_operand_does_not_demand_invalid_gap_version() {
    let mut operations = vec![
        LinearOp::TensorLoad {
            dst_start: 1,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 0,
            count: 3,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::Const {
            dst: 20,
            value: 91.0,
        },
        LinearOp::Const {
            dst: 21,
            value: 92.0,
        },
        LinearOp::LoadIndexedRegister {
            dst: 2,
            base: 20,
            stride: 1,
            dimensions: vec![2].into_boxed_slice(),
            indices: vec![TensorIndex::Runtime(0)].into_boxed_slice(),
        },
        LinearOp::Const {
            dst: 1,
            value: 42.0,
        },
        LinearOp::Const {
            dst: 3,
            value: 64.0,
        },
        LinearOp::DotProduct {
            dst: 4,
            lhs_start: 1,
            rhs_start: 1,
            count: 2,
            lhs_stride: 2,
            rhs_stride: 2,
        },
        LinearOp::Const { dst: 5, value: 7.0 },
        LinearOp::LoadP { dst: 6, index: 1 },
        LinearOp::Select {
            dst: 7,
            cond: 6,
            if_true: 5,
            if_false: 4,
        },
    ];
    operations.extend((22..80).map(|dst| LinearOp::Const {
        dst,
        value: dst as f64,
    }));
    operations.push(LinearOp::StoreOutput { src: 7 });
    let block =
        ScalarProgramBlock::with_program_spans(vec![operations], vec![fixture_span()]).unwrap();
    let prepared = PreparedScalarProgramBlock::new(block).unwrap();
    for index in [3.0, f64::NAN, f64::INFINITY] {
        for (selected, expected) in [(0.0, 42.0 * 42.0 + 64.0 * 64.0), (1.0, 7.0)] {
            assert_eq!(
                prepared
                    .eval_row_with_context(
                        0,
                        &[1.0, 2.0, 3.0],
                        &[index, selected],
                        0.0,
                        RowEvalContext::default()
                    )
                    .unwrap(),
                expected
            );
        }
    }
}

#[test]
fn mutable_dot_product_keeps_interleaved_first_fault() {
    let mut operations = vec![
        LinearOp::TensorLoad {
            dst_start: 1,
            input: rumoca_ir_solve::TensorInputKind::Y,
            input_start: 0,
            count: 4,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::Const {
            dst: 10,
            value: 91.0,
        },
        LinearOp::Const {
            dst: 11,
            value: 92.0,
        },
        LinearOp::Const { dst: 5, value: 4.0 },
        LinearOp::Const { dst: 6, value: 3.0 },
        LinearOp::Const {
            dst: 1,
            value: 42.0,
        },
        LinearOp::LoadIndexedRegister {
            dst: 2,
            base: 10,
            stride: 1,
            dimensions: vec![2].into_boxed_slice(),
            indices: vec![TensorIndex::Runtime(5)].into_boxed_slice(),
        },
        LinearOp::LoadIndexedRegister {
            dst: 3,
            base: 10,
            stride: 1,
            dimensions: vec![2].into_boxed_slice(),
            indices: vec![TensorIndex::Runtime(6)].into_boxed_slice(),
        },
        LinearOp::Const {
            dst: 4,
            value: 64.0,
        },
        LinearOp::DotProduct {
            dst: 7,
            lhs_start: 1,
            rhs_start: 3,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 1,
        },
        LinearOp::Const { dst: 8, value: 1.0 },
        LinearOp::Select {
            dst: 9,
            cond: 8,
            if_true: 7,
            if_false: 1,
        },
    ];
    operations.extend((20..85).map(|dst| LinearOp::Const {
        dst,
        value: dst as f64,
    }));
    operations.push(LinearOp::StoreOutput { src: 9 });
    let block =
        ScalarProgramBlock::with_program_spans(vec![operations], vec![fixture_span()]).unwrap();
    let prepared = PreparedScalarProgramBlock::new(block).unwrap();
    let error = prepared
        .eval_row_with_context(0, &[0.0; 4], &[], 0.0, RowEvalContext::default())
        .unwrap_err();
    assert!(
        matches!(
            error,
            EvalSolveError::TensorIndexOutOfBounds {
                axis: 1,
                index: 3,
                extent: 2,
                ..
            }
        ),
        "{error:?}"
    );
    assert_eq!(error.source_span(), Some(fixture_span()));
}

#[test]
fn a_fold_patch_slice_selects_the_value_at_its_window_position() {
    // The window `a[2:3]` of a four-element tensor is the packed index range
    // {2, 3}; elements 1 and 2 (zero-based) take update values 0 and 1.
    let registers = [2., 3.];
    let subscripts = [TensorUpdateSubscript::Slice {
        start: 0,
        dimensions: vec![2].into_boxed_slice(),
    }];
    let offsets = (0..4)
        .map(|element| {
            tensor_update_register_value_offset(&[4], &subscripts, element, |register| {
                Ok::<_, EvalSolveError>(registers[register as usize])
            })
            .unwrap()
        })
        .collect::<Vec<_>>();
    assert_eq!(offsets, [None, Some(0), Some(1), None]);

    // A matrix row window keeps its row index and offsets by the column window.
    let subscripts = [
        TensorUpdateSubscript::Index(TensorIndex::Constant(1)),
        TensorUpdateSubscript::Slice {
            start: 0,
            dimensions: vec![2].into_boxed_slice(),
        },
    ];
    let offsets = (0..6)
        .map(|element| {
            tensor_update_register_value_offset(&[2, 3], &subscripts, element, |register| {
                Ok::<_, EvalSolveError>(registers[register as usize])
            })
            .unwrap()
        })
        .collect::<Vec<_>>();
    assert_eq!(offsets, [None, None, None, None, Some(0), Some(1)]);
}

#[test]
fn short_prepared_rows_skip_inactive_gathers_and_check_active_addresses() {
    let mut row = row();
    row.truncate(7);
    row.push(LinearOp::StoreOutput { src: 7 });
    let block = ScalarProgramBlock::with_program_spans(vec![row], vec![fixture_span()]).unwrap();
    let prepared = PreparedScalarProgramBlock::new(block).unwrap();
    for (condition, index, expected) in [(1., 0., 7.), (0., 1., 42.), (1., 0., 7.), (0., 2., 64.)] {
        assert_eq!(
            prepared
                .eval_row_with_context(0, &[], &[condition, index], 0., RowEvalContext::default())
                .unwrap(),
            expected
        );
    }
    let error = prepared
        .eval_row_with_context(0, &[], &[0., 0.], 0., RowEvalContext::default())
        .unwrap_err();
    assert!(matches!(
        error,
        EvalSolveError::TensorIndexOutOfBounds { axis: 2, .. }
    ));
    assert_eq!(error.source_span(), Some(fixture_span()));
    assert_eq!(
        prepared
            .eval_row_with_context(0, &[], &[1., 0.], 0., RowEvalContext::default())
            .unwrap(),
        7.
    );
}

#[test]
fn short_conditional_regions_skip_inactive_captured_gathers() {
    let mut region = row();
    region.truncate(7);
    region[0] = LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 1 };
    region[5] = LinearOp::LoadFunctionConditionalCapture { dst: 6, index: 0 };
    region.push(LinearOp::StoreOutput { src: 7 });
    let program = FunctionConditionalProgram::checked(
        2,
        [1],
        [(
            vec![
                LinearOp::Const { dst: 0, value: 1. },
                LinearOp::StoreOutput { src: 0 },
            ],
            region,
        )],
        vec![
            LinearOp::Const { dst: 0, value: -1. },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .unwrap();
    let block = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::LoadP { dst: 1, index: 1 },
            LinearOp::FunctionConditional {
                dst_start: 2,
                capture_start: 0,
                program: std::sync::Arc::new(program),
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        vec![fixture_span()],
    )
    .unwrap();
    let prepared = PreparedScalarProgramBlock::new(block).unwrap();
    for (condition, index, expected) in [(1., 0., 7.), (0., 1., 42.), (1., 0., 7.), (0., 2., 64.)] {
        assert_eq!(
            prepared
                .eval_row_with_context(0, &[], &[condition, index], 0., RowEvalContext::default())
                .unwrap(),
            expected
        );
    }
    let error = prepared
        .eval_row_with_context(0, &[], &[0., 0.], 0., RowEvalContext::default())
        .unwrap_err();
    assert!(matches!(
        error,
        EvalSolveError::TensorIndexOutOfBounds { axis: 2, .. }
    ));
    assert_eq!(error.source_span(), Some(fixture_span()));
}
