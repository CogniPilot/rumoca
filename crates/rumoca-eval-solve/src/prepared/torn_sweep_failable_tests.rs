//! A causal step whose prefix holds an operation that can fail on an earlier
//! step's target starts a new run, so the grouped sweep fails exactly where
//! the per-step isolators fail.

use rumoca_ir_solve::{BinaryOp, LinearOp, ScalarProgramBlock};

use super::{PreparedScalarProgramBlock, TargetAssignmentOutputRequest, TornSweepStatus};
use crate::{EvalSolveError, RowEvalContext};

/// `y0 - 2*y2` and `y1 - (y2 + 1)`; the second prefix also solves the 1x1
/// system `(y[source] - 0.9) * z = 1`, dead to the second isolated value.
/// With `source = 0` the solve is singular exactly at the first step's new
/// value `y0 = 2 * 0.45 = 0.9`; with `source = 2` it never reads a target.
fn two_outputs_with_dead_solve(source: usize) -> PreparedScalarProgramBlock {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 2 },
        LinearOp::Const { dst: 2, value: 2.0 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::LoadY { dst: 5, index: 1 },
        LinearOp::Const { dst: 6, value: 1.0 },
        LinearOp::LoadY {
            dst: 7,
            index: source,
        },
        LinearOp::Const { dst: 8, value: 0.9 },
        LinearOp::Binary {
            dst: 9,
            op: BinaryOp::Sub,
            lhs: 7,
            rhs: 8,
        },
        LinearOp::LinearSolveComponent {
            dst: 10,
            matrix_start: 9,
            rhs_start: 6,
            n: 1,
            component: 0,
        },
        LinearOp::Binary {
            dst: 11,
            op: BinaryOp::Add,
            lhs: 1,
            rhs: 6,
        },
        LinearOp::Binary {
            dst: 12,
            op: BinaryOp::Sub,
            lhs: 5,
            rhs: 11,
        },
        LinearOp::StoreOutput { src: 12 },
    ];
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("torn_sweep_failable_tests.mo"),
        0,
        1,
    );
    PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_program_spans(vec![program], vec![span]).expect("a checked block"),
    )
    .expect("a prepared block")
}

const STEPS: [(usize, usize); 2] = [(0, 0), (1, 1)];
const START: [f64; 3] = [0.3, -1.7, 0.45];

fn grouped_sweep(
    block: &PreparedScalarProgramBlock,
) -> Result<(TornSweepStatus, [f64; 3]), EvalSolveError> {
    let sweep = block
        .prepare_torn_sweep(&STEPS, &[])
        .expect("the sweep prepares");
    let mut y = START;
    let mut residual = Vec::new();
    let status = block.eval_torn_sweep_unchecked_with_context(
        &sweep,
        &mut y,
        &[],
        0.0,
        RowEvalContext::default(),
        &mut residual,
    )?;
    Ok((status, y))
}

/// Each step's isolator on the targets written before it.
fn per_step_isolators(block: &PreparedScalarProgramBlock) -> Result<[f64; 3], EvalSolveError> {
    let mut y = START;
    for (output, target) in STEPS {
        let value = block.eval_target_assignment_output_unchecked_with_context(
            TargetAssignmentOutputRequest {
                row_idx: 0,
                output_offset: output,
                target_y_index: target,
                y: &y,
                p: &[],
                t: 0.0,
                context: RowEvalContext::default(),
            },
        )?;
        y[target] = value.expect("the isolator answers");
    }
    Ok(y)
}

#[test]
fn a_failable_operation_reading_an_earlier_target_splits_the_run() {
    let block = two_outputs_with_dead_solve(0);
    let runs = block.torn_sweep_runs(&STEPS).expect("both steps isolate");
    assert_eq!(runs.len(), 2, "the dead solve reads the first target");
    let reference = per_step_isolators(&block);
    assert!(
        matches!(reference, Err(EvalSolveError::LinearSolve { .. })),
        "the per-step reference fails at the second step: {reference:?}"
    );
    let grouped = grouped_sweep(&block);
    assert!(
        matches!(grouped, Err(EvalSolveError::LinearSolve { .. })),
        "the grouped sweep fails at the same step: {grouped:?}"
    );
}

#[test]
fn a_failable_operation_reading_no_earlier_target_keeps_the_run() {
    let block = two_outputs_with_dead_solve(2);
    let runs = block.torn_sweep_runs(&STEPS).expect("both steps isolate");
    assert_eq!(runs.len(), 1, "the dead solve reads no target");
    let reference = per_step_isolators(&block).expect("the isolators evaluate");
    let (status, grouped) = grouped_sweep(&block).expect("the sweep evaluates");
    assert_eq!(status, TornSweepStatus::Completed);
    assert_eq!(grouped.map(f64::to_bits), reference.map(f64::to_bits));
}
