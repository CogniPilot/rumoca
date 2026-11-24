use super::super::super::random_programs::{Stream, random_defined_program, random_program};
use super::*;

/// The number of outputs `program` stores.
fn stored(program: &[LinearOp]) -> usize {
    program
        .iter()
        .map(|op| match op {
            LinearOp::StoreOutput { .. } => 1,
            LinearOp::StoreOutputRange { count, .. } => *count,
            _ => 0,
        })
        .sum()
}

/// What a caller makes of the per-output sweeps: their slices, or, when
/// `bounded`, no slice at all once their work, a declined output weighing
/// the whole program, exceeds the program's.
fn reference(program: &[LinearOp], outputs: &[usize], bounded: bool) -> Vec<Option<Vec<LinearOp>>> {
    let slices: Vec<_> = outputs
        .iter()
        .map(|&output| slice_to_output(program, output))
        .collect();
    let total: usize = slices
        .iter()
        .map(|slice| {
            slice
                .as_ref()
                .map_or(program_work(program), |slice| program_work(slice))
        })
        .sum();
    if bounded && total > program_work(program) {
        return vec![None; outputs.len()];
    }
    slices
}

/// The same reading of the one-pass answer, a rejected request slicing no
/// output.
fn one_pass(program: &[LinearOp], outputs: &[usize], bounded: bool) -> Vec<Option<Vec<LinearOp>>> {
    slice_to_outputs(program, outputs, bounded).unwrap_or_else(|| vec![None; outputs.len()])
}

/// Every output in order, every output in reverse with a repeat, and one
/// past the last.
fn requests(program: &[LinearOp]) -> Vec<Vec<usize>> {
    let count = stored(program);
    let mut reversed: Vec<usize> = (0..count).rev().collect();
    reversed.extend(reversed.first().copied());
    vec![(0..count).collect(), reversed, vec![count]]
}

fn assert_same(program: &[LinearOp]) -> usize {
    let mut sliced = 0;
    for outputs in requests(program) {
        let unbounded = one_pass(program, &outputs, false);
        assert_eq!(
            unbounded,
            reference(program, &outputs, false),
            "{outputs:?} of {program:?}"
        );
        sliced += unbounded.iter().filter(|slice| slice.is_some()).count();
        assert_eq!(
            one_pass(program, &outputs, true),
            reference(program, &outputs, true),
            "{outputs:?} of {program:?} within its work"
        );
    }
    sliced
}

#[test]
fn every_random_program_slices_in_one_pass_as_output_by_output() {
    let mut stream = Stream(0x5851_f42d_4c95_7f2d);
    let mut sliced = 0;
    for _ in 0..2000 {
        sliced += assert_same(&random_program(&mut stream));
        sliced += assert_same(&random_defined_program(&mut stream));
    }
    assert!(sliced > 10_000, "only {sliced} slices were compared");
}

#[test]
fn an_unmodelled_operation_declines_only_the_outputs_stored_after_it() {
    // r0 = y0; store r0; r1..3 = identity; r4 = y1; store r4.
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::TensorIdentity {
            dst_start: 1,
            size: 1,
            lanes: 1,
        },
        LinearOp::LoadY { dst: 4, index: 1 },
        LinearOp::StoreOutput { src: 4 },
    ];
    let slices = slice_to_outputs(&program, &[0, 1], false).expect("no budget");
    assert!(slices[0].is_some());
    assert!(slices[1].is_none());
    assert_eq!(slices, reference(&program, &[0, 1], false));
}

/// `program` with a table lookup, an operation that can fail, at a random
/// position.
fn with_fallible(stream: &mut Stream, mut program: Vec<LinearOp>) -> Vec<LinearOp> {
    let at = stream.next(program.len() + 1);
    let register = |stream: &mut Stream| stream.next(24) as Reg;
    let lookup = LinearOp::TableLookup {
        dst: register(stream),
        table_id: register(stream),
        column: register(stream),
        input: register(stream),
    };
    program.insert(at, lookup);
    program
}

/// No slice drops an operation that can fail, in either form.
#[test]
fn no_slice_drops_an_operation_that_can_fail() {
    let mut stream = Stream(0x2c1b_3c6d_4a5f_0e17);
    for _ in 0..2000 {
        let program = random_program(&mut stream);
        let program = with_fallible(&mut stream, program);
        assert_same(&program);
        for output in 0..stored(&program) {
            let Some(slice) = slice_to_output(&program, output) else {
                continue;
            };
            for op in program.iter().filter(|op| can_fail(op)) {
                assert!(slice.contains(op), "{op:?} dropped from {program:?}");
            }
        }
    }
    let scenario = super::super::tests::a_store_before_a_fallible_operation();
    assert_eq!(
        slice_to_outputs(&scenario, &[0, 1], false),
        Some(vec![None, None])
    );
}

/// Rows that each read every element of a tensor operation keep it whole.
/// Counted as operations their slices fit in the program, which also
/// computes values no row reads, but every slice computes all four
/// elements, so together they compute more than the program does once and
/// the program keeps its full form.
#[test]
fn rows_sharing_a_whole_tensor_operation_keep_the_full_program() {
    // r0..4 = y0..4; six values no row reads; per row k:
    // r = r0 + r1 + r2 + r3 + p_k; store r.
    let mut program = vec![LinearOp::TensorLoad {
        dst_start: 0,
        input: TensorInputKind::Y,
        input_start: 0,
        count: 4,
        seed_start: None,
        lanes: 1,
    }];
    program.extend((0..6).map(|index| LinearOp::LoadY {
        dst: 100 + index as Reg,
        index,
    }));
    for row in 0..3u32 {
        let base = 10 + 10 * row;
        program.extend([
            LinearOp::Binary {
                dst: base,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::Binary {
                dst: base + 1,
                op: BinaryOp::Add,
                lhs: base,
                rhs: 2,
            },
            LinearOp::Binary {
                dst: base + 2,
                op: BinaryOp::Add,
                lhs: base + 1,
                rhs: 3,
            },
            LinearOp::LoadP {
                dst: base + 3,
                index: row as usize,
            },
            LinearOp::Binary {
                dst: base + 4,
                op: BinaryOp::Add,
                lhs: base + 2,
                rhs: base + 3,
            },
            LinearOp::StoreOutput { src: base + 4 },
        ]);
    }
    let outputs = [0, 1, 2];
    let slices: Vec<_> = outputs
        .iter()
        .map(|&output| slice_to_output(&program, output).expect("each row slices"))
        .collect();
    let operations: usize = slices.iter().map(Vec::len).sum();
    assert!(operations <= program.len(), "{operations} operations");
    assert_eq!(slice_to_outputs(&program, &outputs, true), None);
    assert_eq!(
        one_pass(&program, &outputs, true),
        reference(&program, &outputs, true)
    );
}
