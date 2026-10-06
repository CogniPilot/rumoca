//! Exact demanded addresses, source provenance, and lazy replay reachability.
use super::*;
use rumoca_ir_solve::{FunctionConditionalProgram, TensorIndex, TensorSubscript};

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
    let subscripts = [TensorSubscript::Index(TensorIndex::Runtime(0))];
    assert_eq!(
        tensor_update_value_offset(&[3], &subscripts, 0, |_| Ok::<_, EvalSolveError>(2.)).unwrap(),
        None
    );
    assert_eq!(
        tensor_update_value_offset(&[3], &subscripts, 1, |_| Ok::<_, EvalSolveError>(2.)).unwrap(),
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
