//! A pure call in shared-value segments (SPEC_0043 §6a): a value-only call
//! of a closed-input owner on identical operand terms is computed once, a
//! call is kept when unread, and a program reading a call the open segment
//! holds joins it past the register cap.

use super::*;
use crate::{
    AssignmentProgram, LinearOp, Reg, SHARED_VALUE_REGISTER_CAP, ScalarProgramRegisterFlow,
    SharedValueSegments, TensorInputKind,
};

/// One call of two `width`-vectors returning both swapped, values only.
fn value_call(width: u32) -> SolvePureCallTable {
    let vector = SolveValueType::tensor(SolveScalarType::real(profile()), vec![width]).unwrap();
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

/// `y[target] = y[source]` or call output `read`, after a call on the
/// `2 * width` slots starting at `first`.
fn call_then_copy(site: SolvePureCallSite, first: usize, read: Option<Reg>) -> Vec<LinearOp> {
    let width = site.inputs()[0].scalar_count();
    let mut program = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: first,
            count: 2 * width as usize,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::PureCall {
            dst_start: 2 * width,
            input_starts: vec![0, width].into_boxed_slice(),
            site,
        },
    ];
    program.push(LinearOp::StoreOutput {
        src: read.map_or(0, |output| 2 * width + output),
    });
    program
}

fn calls(ops: &[LinearOp]) -> usize {
    ops.iter()
        .filter(|op| matches!(op, LinearOp::PureCall { .. }))
        .count()
}

fn derive(rows: &[(Vec<LinearOp>, Vec<usize>)]) -> SharedValueSegments {
    let programs = rows
        .iter()
        .map(|(ops, targets)| AssignmentProgram { ops, targets })
        .collect::<Vec<_>>();
    let shared = SharedValueSegments::derive(&programs);
    shared.check(&programs).unwrap();
    shared
}

#[test]
fn a_native_identity_barrier_retains_the_complete_checked_call_interface() {
    let table = value_call(3);
    let mut ops = call_then_copy(table.owners()[0].call_site(), 0, None);
    ops.pop();
    ops.push(LinearOp::StoreOutputRange {
        start: 6,
        count: 6,
        stride: 1,
    });
    let targets = (20..26).collect::<Vec<_>>();
    let sources = [AssignmentProgram {
        ops: &ops,
        targets: &targets,
    }];
    let shared = SharedValueSegments::derive(&sources);
    shared.check(&sources).unwrap();
    assert_eq!(shared.segments()[0].ops(), ops);
    let mut changed = ops.clone();
    if let LinearOp::PureCall { site, .. } = &mut changed[1] {
        let mut wire = serde_json::to_value(&*site).unwrap();
        wire["owner"] = serde_json::json!(site.owner().index() + 1);
        *site = serde_json::from_value(wire).unwrap();
    }
    assert!(
        shared
            .check(&[AssignmentProgram {
                ops: &changed,
                targets: &targets
            }])
            .is_err()
    );
}

#[test]
fn a_call_on_identical_inputs_is_computed_once_and_kept_when_unread() {
    let table = value_call(3);
    let site = || table.owners()[0].call_site();
    let shared = derive(&[
        (call_then_copy(site(), 0, Some(0)), vec![20]),
        (call_then_copy(site(), 0, Some(4)), vec![21]),
        (call_then_copy(site(), 0, None), vec![22]),
    ]);
    let [segment] = shared.segments() else {
        panic!("the programs fuse into one segment");
    };
    assert_eq!(calls(segment.ops()), 1, "one call serves every occurrence");
    let unread = derive(&[(call_then_copy(site(), 0, None), vec![22])]);
    assert_eq!(
        calls(unread.segments()[0].ops()),
        1,
        "an unread call is kept"
    );
}

#[test]
fn a_call_on_other_inputs_or_a_stored_input_runs_again() {
    let table = value_call(3);
    let site = || table.owners()[0].call_site();
    let shared = derive(&[
        (call_then_copy(site(), 0, Some(0)), vec![20]),
        (call_then_copy(site(), 6, Some(0)), vec![21]),
        // Stores slot 1, an input of the first call.
        (call_then_copy(site(), 0, Some(1)), vec![1]),
        (call_then_copy(site(), 0, Some(2)), vec![23]),
    ]);
    let [segment] = shared.segments() else {
        panic!("the programs fuse into one segment");
    };
    assert_eq!(
        calls(segment.ops()),
        3,
        "other slots and a rewritten input are other coordinates"
    );
}

#[test]
fn a_program_past_the_cap_joins_the_segment_holding_its_call() {
    let width = 1500;
    let table = value_call(width);
    let site = || table.owners()[0].call_site();
    let rows = (0..4)
        .map(|output| {
            (
                call_then_copy(site(), 0, Some(output)),
                vec![10_000 + output as usize],
            )
        })
        .collect::<Vec<_>>();
    assert!(4 * 4 * width as usize > SHARED_VALUE_REGISTER_CAP);
    let shared = derive(&rows);
    let [segment] = shared.segments() else {
        panic!("every program joins the segment holding its call");
    };
    assert_eq!(calls(segment.ops()), 1);
    let registers = ScalarProgramRegisterFlow::derive(segment.ops())
        .expect("the segment is a valid program")
        .register_count();
    assert_eq!(
        registers,
        4 * width as usize,
        "the register file holds the loaded inputs and the call outputs once"
    );
}

#[test]
fn compaction_keeps_the_offsets_between_call_operands() {
    let table = value_call(3);
    let site = || table.owners()[0].call_site();
    // The call's two inputs lie at 0..3 and 4..7; register 3 holds an unread
    // load, which dead-operation removal drops.
    let gapped = |output: Reg| {
        let load = |dst_start: Reg, input_start: usize| LinearOp::TensorLoad {
            dst_start,
            input: TensorInputKind::Y,
            input_start,
            count: 3,
            seed_start: None,
            lanes: 1,
        };
        vec![
            load(0, 0),
            LinearOp::LoadY { dst: 3, index: 9 },
            load(4, 3),
            LinearOp::PureCall {
                dst_start: 7,
                input_starts: vec![0, 4].into_boxed_slice(),
                site: site(),
            },
            LinearOp::StoreOutput { src: 7 + output },
        ]
    };
    let shared = derive(&[(gapped(0), vec![20]), (gapped(5), vec![21])]);
    let [segment] = shared.segments() else {
        panic!("the programs fuse into one segment");
    };
    assert_eq!(calls(segment.ops()), 1);
}
