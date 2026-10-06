use super::*;
use rumoca_core::{SourceId, Span};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("portable_me.mo"), 3, 17)
}

fn block(programs: Vec<Vec<LinearOp>>, outputs: Vec<usize>) -> ScalarProgramBlock {
    let spans = vec![span(); programs.len()];
    ScalarProgramBlock::with_output_indices(programs, spans, outputs).unwrap()
}

fn scalar(value: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value },
        LinearOp::StoreOutput { src: 0 },
    ]
}

fn layout() -> VarLayout {
    VarLayout::from_parts(Default::default(), 2, 1)
}

#[test]
fn portable_me_declines_unsupported_model_context_before_whole_call_admission() {
    use rumoca_solver::SimExecutionPolicy::{Auto, Interpreter};
    // A scalar block alone is eligible even in a model whose unrelated tables
    // cannot be admitted by the whole-call interface. Decline at composition.
    let source = block(vec![scalar(2.0)], vec![0]);
    assert!(single_program(&source, 0, &layout()).is_ok());
    assert!(model_context_admitted(Auto, 1, 0));
    assert!(!model_context_admitted(Auto, 1, 1));
    assert!(!model_context_admitted(Auto, 0, 0));
    assert!(!model_context_admitted(Interpreter, 1, 0));
}

#[test]
fn portable_me_retains_original_program_span_and_signed_zero() {
    let source = block(vec![scalar(-0.0)], vec![7]);
    let admitted = single_program(&source, 0, &layout()).unwrap();
    assert_eq!(admitted.program_span(0), source.program_span(0));
    assert_eq!(admitted.output_indices(), &[0]);
    let LinearOp::Const { value, .. } = admitted.programs()[0][0] else {
        panic!("constant changed")
    };
    assert_eq!(value.to_bits(), (-0.0_f64).to_bits());
    assert_eq!(source.output_indices(), &[7]);
}

#[test]
fn portable_me_selection_keeps_original_indices_around_refused_program() {
    let indexed = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::LoadIndexedP {
            dst: 1,
            base: 0,
            count: 1,
            index: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let source = block(vec![scalar(4.0), indexed, scalar(9.0)], vec![5, 1, 8]);
    assert!(single_program(&source, 0, &layout()).is_ok());
    assert!(single_program(&source, 1, &layout()).is_err());
    assert!(single_program(&source, 2, &layout()).is_ok());
    assert_eq!(source.output_indices(), &[5, 1, 8]);
    assert!(single_program(&source, 3, &layout()).is_err());
}

#[test]
fn portable_me_declines_nan_sensitive_extrema_in_complete_prefix() {
    for op in [BinaryOp::Min, BinaryOp::Max] {
        let source = block(
            vec![vec![
                LinearOp::Const {
                    dst: 0,
                    value: f64::NAN,
                },
                LinearOp::Const { dst: 1, value: 2.0 },
                LinearOp::Binary {
                    dst: 2,
                    op,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 1 },
            ]],
            vec![0],
        );
        assert!(single_program(&source, 0, &layout()).is_err());
        let mut used_ops = source.programs()[0].clone();
        *used_ops.last_mut().unwrap() = LinearOp::StoreOutput { src: 2 };
        let used = block(vec![used_ops], vec![0]);
        let mut result = [0.0];
        rumoca_eval_solve::eval_scalar_program_block(
            &used,
            &[0.0; 2],
            &[0.0],
            0.0,
            None,
            &mut result,
        )
        .unwrap();
        assert_eq!(result, [2.0]);
        assert!(single_program(&used, 0, &layout()).is_err());
    }
}

#[test]
fn portable_me_declines_multi_output_and_dense_linear_solve() {
    let source = block(
        vec![vec![
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![0, 1],
    );
    assert!(single_program(&source, 0, &layout()).is_err());
    let source = block(
        vec![vec![
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::Const { dst: 1, value: 8.0 },
            LinearOp::LinearSolveComponent {
                dst: 2,
                matrix_start: 0,
                rhs_start: 1,
                n: 1,
                component: 0,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        vec![0],
    );
    assert!(single_program(&source, 0, &layout()).is_err());
}

#[test]
fn portable_me_checks_every_load_against_the_issued_layout() {
    for op in [
        LinearOp::LoadY { dst: 0, index: 2 },
        LinearOp::LoadP { dst: 0, index: 1 },
    ] {
        let source = block(
            vec![vec![
                op,
                LinearOp::Const { dst: 1, value: 3.0 },
                LinearOp::StoreOutput { src: 1 },
            ]],
            vec![0],
        );
        assert!(single_program(&source, 0, &layout()).is_err());
    }
    let source = block(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]],
        vec![0],
    );
    assert!(single_program(&source, 0, &layout()).is_ok());
}

#[test]
fn portable_me_never_zero_pads_short_inputs_or_ignores_external_tables() {
    assert!(validate_inputs(&layout(), 2, 1, 0).is_ok());
    for (y, p, tables) in [(1, 1, 0), (3, 1, 0), (2, 0, 0), (2, 2, 0), (2, 1, 1)] {
        assert!(validate_inputs(&layout(), y, p, tables).is_err());
    }
}
