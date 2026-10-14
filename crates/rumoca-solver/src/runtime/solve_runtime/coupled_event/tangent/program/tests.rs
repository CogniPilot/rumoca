use super::*;
use crate::runtime::solve_runtime::test_kernels::ReferenceBackend;

fn source() -> solve::ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("event_tangent.mo"),
        1,
        2,
    );
    solve::ScalarProgramBlock::with_program_spans(
        vec![
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::LoadSeed { dst: 1, index: 1 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
                solve::LinearOp::StoreOutput { src: 1 },
            ],
        ],
        vec![span, span],
    )
    .unwrap()
}

#[test]
fn prepared_event_tangents_preserve_selected_tuple_and_direction_contracts() {
    let source = source();
    for backend in [
        None,
        Some(Rc::new(ReferenceBackend) as Rc<dyn SolveExecutionBackend>),
    ] {
        let program = TangentProgram::prepare("event_tangent", &source, backend.as_ref()).unwrap();
        let context = RowEvalContext {
            seed: Some(&[2.0, 5.0]),
            ..Default::default()
        };
        let mut whole = [0.0; 3];
        program
            .eval_with_context(&[3.0], &[], 0.0, context, &mut whole)
            .unwrap();
        assert_eq!(whole, [2.0, 15.0, 5.0]);
        assert_eq!(program.row_output_position(2), Some((1, 1)));
        assert_eq!(
            program
                .eval_row_output_unchecked_with_context(1, 1, &[3.0], &[], 0.0, context)
                .unwrap(),
            5.0
        );
        let mut tuple = Vec::new();
        program
            .eval_row_outputs_unchecked_with_context(1, &[3.0], &[], 0.0, context, &mut tuple)
            .unwrap();
        assert_eq!(tuple, [15.0, 5.0]);
        assert_eq!(
            program
                .eval_row_unchecked_with_context(0, &[3.0], &[], 0.0, context)
                .unwrap(),
            2.0
        );
        assert!(
            program
                .eval_row_unchecked_with_context(1, &[3.0], &[], 0.0, context)
                .is_err(),
            "a tuple cannot masquerade as a scalar tangent"
        );
        let selected_context = RowEvalContext {
            seed: Some(&[2.0]),
            ..Default::default()
        };
        assert_eq!(
            program
                .eval_row_unchecked_with_context(0, &[3.0], &[], 0.0, selected_context)
                .unwrap(),
            2.0,
            "an unrelated program's seed footprint must not execute"
        );
        assert!(
            program
                .eval_with_context(&[3.0], &[], 0.0, selected_context, &mut whole)
                .is_err()
        );
    }
}

#[test]
fn compiled_event_tangent_rejects_missing_direction_before_outputs() {
    let backend: Rc<dyn SolveExecutionBackend> = Rc::new(ReferenceBackend);
    let program = TangentProgram::prepare("event_tangent", &source(), Some(&backend)).unwrap();
    let mut out = [73.0; 3];
    let error = program
        .eval_with_context(&[3.0], &[], 0.0, RowEvalContext::default(), &mut out)
        .unwrap_err();
    assert!(error.to_string().contains("no direction"), "{error}");
    assert_eq!(out, [73.0; 3]);
}
