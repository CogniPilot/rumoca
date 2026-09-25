//! Grouping of consecutive causal steps that share one residual program.

use rumoca_ir_solve::{BinaryOp, LinearOp, ScalarProgramBlock};

use super::{PreparedScalarProgramBlock, TargetAssignmentOutputRequest, TornSweepStatus};
use crate::RowEvalContext;

/// Two residual outputs of one program: `y0 - 2*y2` and `y1 - (s + 1)`, where
/// the second isolated value reads `s = y2`, or `s = y0` when `reads_earlier`.
/// The prefix of the second output always reads `y0`, the first target, to
/// form the first residual.
fn two_outputs(reads_earlier: bool) -> PreparedScalarProgramBlock {
    let source = if reads_earlier { 0 } else { 1 };
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
        LinearOp::Binary {
            dst: 7,
            op: BinaryOp::Add,
            lhs: source,
            rhs: 6,
        },
        LinearOp::Binary {
            dst: 8,
            op: BinaryOp::Sub,
            lhs: 5,
            rhs: 7,
        },
        LinearOp::StoreOutput { src: 8 },
    ];
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("torn_sweep_run_tests.mo"),
        0,
        1,
    );
    PreparedScalarProgramBlock::new(
        ScalarProgramBlock::with_program_spans(vec![program], vec![span]).expect("a checked block"),
    )
    .expect("a prepared block")
}

const STEPS: [(usize, usize); 2] = [(0, 0), (1, 1)];

#[test]
fn steps_whose_values_read_no_earlier_target_share_one_run() {
    let block = two_outputs(false);
    let runs = block.torn_sweep_runs(&STEPS).expect("both steps isolate");
    assert_eq!(
        runs.len(),
        1,
        "the residual prefix reading y0 does not split the run"
    );
    assert_eq!(runs[0].pairs, [(0, 0), (1, 1)]);
}

#[test]
fn a_step_whose_value_reads_an_earlier_target_starts_a_new_run() {
    let block = two_outputs(true);
    let runs = block.torn_sweep_runs(&STEPS).expect("both steps isolate");
    assert_eq!(runs.len(), 2, "the second value reads the first target");
}

/// The grouped sweep writes exactly the values the per-step isolators give
/// when each step sees the targets written before it.
#[test]
fn a_grouped_sweep_matches_the_per_step_isolators_bit_for_bit() {
    for reads_earlier in [false, true] {
        let block = two_outputs(reads_earlier);
        let sweep = block
            .prepare_torn_sweep(&STEPS, &[])
            .expect("the sweep prepares");
        let start = [0.3, -1.7, 0.45];
        let mut grouped = start;
        let mut residual = Vec::new();
        let status = block
            .eval_torn_sweep_unchecked_with_context(
                &sweep,
                &mut grouped,
                &[],
                0.0,
                RowEvalContext::default(),
                &mut residual,
            )
            .expect("the sweep evaluates");
        assert_eq!(status, TornSweepStatus::Completed);
        let mut sequential = start;
        for (output, target) in STEPS {
            let value = block
                .eval_target_assignment_output_unchecked_with_context(
                    TargetAssignmentOutputRequest {
                        row_idx: 0,
                        output_offset: output,
                        target_y_index: target,
                        y: &sequential,
                        p: &[],
                        t: 0.0,
                        context: RowEvalContext::default(),
                    },
                )
                .expect("the isolator evaluates")
                .expect("the isolator answers");
            sequential[target] = value;
        }
        assert_eq!(
            grouped.map(f64::to_bits),
            sequential.map(f64::to_bits),
            "reads_earlier = {reads_earlier}"
        );
    }
}
