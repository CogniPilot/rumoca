use rumoca_ir_solve::{BinaryOp, BlockResidualSplit, LinearOp, ScalarProgramBlock, UnaryOp};

use super::PreparedBlockResidualSplit;
use crate::{EvalSolveError, PreparedScalarProgramBlock, RowEvalContext};

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("block_residual_split_tests.mo"),
        0,
        1,
    )
}

fn whole(program: &[LinearOp], y: &[f64], p: &[f64]) -> Result<Vec<f64>, EvalSolveError> {
    let block = PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_program_spans(vec![program.to_vec()], vec![span()])
            .expect("a checked program"),
    )
    .expect("a prepared program");
    let mut out = Vec::new();
    block.eval_row_outputs_unchecked_with_context(
        0,
        y,
        p,
        0.0,
        RowEvalContext::default(),
        &mut out,
    )?;
    Ok(out)
}

fn split_of(program: &[LinearOp], unknowns: &[usize]) -> PreparedBlockResidualSplit {
    let split = BlockResidualSplit::derive(program, unknowns).expect("a split");
    split.check(program, unknowns).expect("the split checks");
    let outputs = ScalarProgramBlock::program_output_count(program);
    PreparedBlockResidualSplit::new(split, outputs, Some(span()))
}

/// Two residuals over unknowns y0, y1 and fixed y2, y3: trigonometric and
/// exponential work on the fixed coordinates hoists.
fn two_residuals() -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: 2 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Sin,
            arg: 0,
        },
        LinearOp::LoadY { dst: 2, index: 3 },
        LinearOp::Unary {
            dst: 3,
            op: UnaryOp::Exp,
            arg: 2,
        },
        LinearOp::LoadP { dst: 4, index: 0 },
        LinearOp::Binary {
            dst: 5,
            op: BinaryOp::Mul,
            lhs: 3,
            rhs: 4,
        },
        LinearOp::LoadY { dst: 6, index: 0 },
        LinearOp::LoadY { dst: 7, index: 1 },
        LinearOp::Binary {
            dst: 8,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 6,
        },
        LinearOp::Binary {
            dst: 9,
            op: BinaryOp::Sub,
            lhs: 8,
            rhs: 7,
        },
        LinearOp::Binary {
            dst: 10,
            op: BinaryOp::Add,
            lhs: 6,
            rhs: 5,
        },
        LinearOp::Binary {
            dst: 11,
            op: BinaryOp::Div,
            lhs: 10,
            rhs: 7,
        },
        LinearOp::StoreOutput { src: 9 },
        LinearOp::StoreOutput { src: 11 },
    ]
}

#[test]
fn the_dependent_part_over_invariant_values_equals_the_whole_program_bit_for_bit() {
    let program = two_residuals();
    let split = split_of(&program, &[0, 1]);
    assert_eq!(split.split().live_out(), &[1, 5]);
    let p = [1.7];
    let mut values = Vec::new();
    split
        .eval_invariant(
            (&[9.0, 9.0, 0.3, -0.8], &p, 0.0),
            RowEvalContext::default(),
            &mut values,
        )
        .expect("the invariant part evaluates");
    // Every pass of one call moves only the unknowns.
    for (y0, y1) in [(0.0, 0.0), (1.25, -3.5), (-0.0, 1e-300), (f64::MAX, 2.0)] {
        let y = [y0, y1, 0.3, -0.8];
        let mut out = Vec::new();
        split
            .eval_dependent(&values, (&y, &p, 0.0), RowEvalContext::default(), &mut out)
            .expect("the dependent part evaluates");
        let reference = whole(&program, &y, &p).expect("the program evaluates");
        assert_eq!(
            out.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            reference.iter().map(|v| v.to_bits()).collect::<Vec<_>>(),
            "y = {y:?}"
        );
    }
}

/// A singular 1x1 dense solve on a fixed coordinate (`(y2 - 0.5) z = 1`)
/// hoists; at y2 = 0.5 the invariant part fails with the error the whole
/// program raises, and a caller then evaluates the whole program.
#[test]
fn a_failing_invariant_operation_fails_as_the_whole_program_does() {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 1 },
        LinearOp::Const { dst: 1, value: 0.5 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Const { dst: 3, value: 1.0 },
        LinearOp::LinearSolveComponent {
            dst: 4,
            matrix_start: 2,
            rhs_start: 3,
            n: 1,
            component: 0,
        },
        LinearOp::LoadY { dst: 5, index: 0 },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Sub,
            lhs: 5,
            rhs: 4,
        },
        LinearOp::StoreOutput { src: 6 },
    ];
    let split = split_of(&program, &[0]);
    assert!(
        split.split().invariant().contains(&program[4]),
        "the solve hoists"
    );
    let y = [2.0, 0.5];
    let mut values = Vec::new();
    let invariant = split.eval_invariant((&y, &[], 0.0), RowEvalContext::default(), &mut values);
    let reference = whole(&program, &y, &[]);
    assert!(
        matches!(invariant, Err(EvalSolveError::LinearSolve { .. })),
        "{invariant:?}"
    );
    assert_eq!(
        invariant.map(|_| ()),
        reference.map(|_| ()),
        "the same error, span included"
    );
}
