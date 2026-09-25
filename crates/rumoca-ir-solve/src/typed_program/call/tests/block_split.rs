//! A pure call runs whole: in a block residual split it stays dependent when
//! any input reads a block unknown, even when its output summaries leave some
//! outputs independent of that input.

use super::dependencies::separated_outputs;
use super::*;
use crate::{BlockResidualSplit, LinearOp, TensorInputKind};

fn call_program(site: SolvePureCallSite) -> Vec<LinearOp> {
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 6,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::PureCall {
            dst_start: 6,
            input_starts: vec![0, 3].into_boxed_slice(),
            site,
        },
        LinearOp::StoreOutputRange {
            start: 6,
            count: 7,
            stride: 1,
        },
    ]
}

#[test]
fn a_pure_call_with_an_input_reading_an_unknown_does_not_hoist() {
    let table = separated_outputs(3);
    let program = call_program(table.owners()[1].call_site());
    // y0 is an unknown: the load reading it and the call are dependent,
    // although outputs 0..3 depend only on the second input (y3..y5).
    assert!(BlockResidualSplit::derive(&program, &[0]).is_none());
}

#[test]
fn a_pure_call_reading_no_unknown_hoists_whole() {
    let table = separated_outputs(3);
    let program = call_program(table.owners()[1].call_site());
    let split = BlockResidualSplit::derive(&program, &[9]).expect("a split");
    assert_eq!(split.invariant(), &program[..2]);
    assert_eq!(split.live_out(), &[6, 7, 8, 9, 10, 11, 12]);
    split.check(&program, &[9]).expect("the split checks");
}
