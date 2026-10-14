use super::*;

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("direct_conditional_region.mo"),
        0,
        1,
    )
}

fn input<'a>(parameters: &'a [f64], state: &'a [f64]) -> PreparedRowEval<'a, 'a> {
    PreparedRowEval::new(&[], 0, state, parameters, 0.25, RowEvalContext::default())
        .with_source_span(Some(span()))
}

#[test]
fn scalar_regions_read_current_inputs_and_preserve_exact_value_bits_without_frames() {
    let mut frames = Vec::new();
    for value in [0.0_f64, -0.0, 7.25, f64::from_bits(0x7ff8_0000_0000_1234)] {
        for operation in [
            LinearOp::Const { dst: 5, value },
            LinearOp::LoadP { dst: 5, index: 0 },
            LinearOp::LoadY { dst: 5, index: 0 },
        ] {
            let region = [operation, LinearOp::StoreOutput { src: 5 }];
            let mut out = [0.0];
            eval_function_conditional_region(
                input(&[value], &[value]),
                &region,
                6,
                &[],
                &mut frames,
                &mut out,
            )
            .unwrap();
            assert_eq!(out[0].to_bits(), value.to_bits());
            assert!(
                frames.is_empty(),
                "one scalar read requires no nested evaluation frame"
            );
        }
    }
    let region = [
        LinearOp::LoadTime { dst: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let mut out = [0.0];
    eval_function_conditional_region(input(&[], &[]), &region, 1, &[], &mut frames, &mut out)
        .unwrap();
    assert_eq!(out[0], 0.25);
    assert!(frames.is_empty());
}

#[test]
fn direct_region_read_errors_keep_the_original_source_and_exact_missing_input() {
    let region = [
        LinearOp::LoadP { dst: 0, index: 4 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let mut frames = Vec::new();
    let error = eval_function_conditional_region(
        input(&[1.0], &[]),
        &region,
        1,
        &[],
        &mut frames,
        &mut [0.0],
    )
    .unwrap_err();
    assert_eq!(
        error,
        EvalSolveError::MissingInput {
            vector: "p",
            index: 4,
            len: 1,
            span: Some(span())
        }
    );
}

#[test]
fn extra_or_uncorrelated_operations_cannot_skip_an_executed_failure() {
    let extra = [
        LinearOp::LoadP { dst: 0, index: 0 },
        LinearOp::LoadSeed { dst: 1, index: 4 },
        LinearOp::StoreOutput { src: 0 },
    ];
    assert!(direct_value(input(&[0.0], &[]), &extra, 2).is_none());
    let error = eval_function_conditional_region(
        input(&[0.0], &[]),
        &extra,
        2,
        &[],
        &mut Vec::new(),
        &mut [0.0],
    )
    .unwrap_err();
    assert_eq!(
        error,
        EvalSolveError::MissingInput {
            vector: "seed",
            index: 4,
            len: 0,
            span: Some(span())
        }
    );
    let wrong_output = [
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    assert!(direct_value(input(&[0.0], &[]), &wrong_output, 2).is_none());
}

#[test]
fn an_inconsistent_region_register_count_cannot_bypass_the_ordinary_driver() {
    let region = [
        LinearOp::LoadP { dst: 5, index: 0 },
        LinearOp::StoreOutput { src: 5 },
    ];
    for count in [0, 1, 5] {
        assert!(direct_value(input(&[1.0], &[]), &region, count).is_none());
    }
    assert_eq!(
        direct_value(input(&[1.0], &[]), &region, 6)
            .unwrap()
            .unwrap(),
        1.0
    );
}
