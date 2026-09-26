//! A pure call in shared-value segments (SPEC_0043 §6a): its body may reach
//! an external function, so it runs at every occurrence, read or not.

use super::*;
use crate::{AssignmentProgram, LinearOp, SharedValueSegments, TensorInputKind};

/// One call of two 3-vectors returning both, values only.
fn value_call() -> SolvePureCallTable {
    let vector = SolveValueType::tensor(SolveScalarType::real(profile()), vec![3]).unwrap();
    let inputs = vec![vector.clone(), vector.clone()];
    let outputs = vec![
        SolvePureCallOutput::result(vector.clone()),
        SolvePureCallOutput::result(vector),
    ];
    SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1),
            inputs,
            outputs,
            span(0),
            |builder, inputs, outputs| {
                let first = builder.load(inputs[0], span(1))?;
                let second = builder.load(inputs[1], span(2))?;
                builder.store(outputs[0], second, span(3))?;
                builder.store(outputs[1], first, span(4))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

/// `y[target] = y[0]`, after a call on `y[0..6]` whose outputs are `read`
/// or left unread.
fn call_then_copy(site: SolvePureCallSite, read: bool) -> Vec<LinearOp> {
    let mut program = vec![
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
    ];
    program.push(LinearOp::StoreOutput {
        src: if read { 6 } else { 0 },
    });
    program
}

fn calls(ops: &[LinearOp]) -> usize {
    ops.iter()
        .filter(|op| matches!(op, LinearOp::PureCall { .. }))
        .count()
}

#[test]
fn a_call_runs_at_every_occurrence_and_is_kept_when_unread() {
    let table = value_call();
    let site = || table.owners()[0].call_site();
    let rows = [
        (call_then_copy(site(), true), vec![20]),
        (call_then_copy(site(), true), vec![21]),
        (call_then_copy(site(), false), vec![22]),
    ];
    let programs = rows
        .iter()
        .map(|(ops, targets)| AssignmentProgram { ops, targets })
        .collect::<Vec<_>>();
    let shared = SharedValueSegments::derive(&programs);
    shared.check(&programs).unwrap();
    let [segment] = shared.segments() else {
        panic!("the programs fuse into one segment");
    };
    assert_eq!(calls(segment.ops()), 3, "no call is shared or dropped");
}
