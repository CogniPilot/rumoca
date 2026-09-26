use super::*;

/// `y[target] = y[a] * y[b] + k`, the product computed at the program's own
/// registers.
fn product_plus(a: usize, b: usize, k: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: a },
        LinearOp::LoadY { dst: 1, index: b },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Const { dst: 3, value: k },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ]
}

fn programs<'a>(rows: &'a [(Vec<LinearOp>, Vec<usize>)]) -> Vec<AssignmentProgram<'a>> {
    rows.iter()
        .map(|(ops, targets)| AssignmentProgram { ops, targets })
        .collect()
}

fn count(ops: &[LinearOp], kind: &str) -> usize {
    ops.iter().filter(|op| op.kind_name() == kind).count()
}

#[test]
fn a_repeated_value_is_computed_once_and_the_segment_checks() {
    let rows = [
        (product_plus(0, 1, 1.0), vec![10]),
        (product_plus(0, 1, 2.0), vec![11]),
    ];
    let programs = programs(&rows);
    let shared = SharedValueSegments::derive(&programs);
    shared.check(&programs).unwrap();
    let [segment] = shared.segments() else {
        panic!("both programs fuse into one segment");
    };
    assert_eq!(segment.targets(), [10, 11]);
    assert_eq!(count(segment.ops(), "LoadY"), 2, "each slot is loaded once");
    assert_eq!(count(segment.ops(), "Binary"), 3, "one product, two sums");
    assert!(shared.shared_operations() >= 3);
    ScalarProgramRegisterFlow::derive(segment.ops()).expect("the segment is a valid program");
}

#[test]
fn a_slot_an_earlier_program_stored_is_read_from_its_register() {
    let rows = [
        (product_plus(0, 1, 1.0), vec![5]),
        (product_plus(5, 2, 3.0), vec![6]),
    ];
    let programs = programs(&rows);
    let shared = SharedValueSegments::derive(&programs);
    shared.check(&programs).unwrap();
    let [segment] = shared.segments() else {
        panic!("one segment");
    };
    assert!(
        !segment
            .ops()
            .iter()
            .any(|op| matches!(op, LinearOp::LoadY { index: 5, .. })),
        "the stored slot is forwarded, never reloaded"
    );
}

#[test]
fn a_program_with_an_effect_or_a_rewritten_register_is_its_own_segment() {
    let random = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::ImpureRandomInit { dst: 1, seed: 0 },
        LinearOp::StoreOutput { src: 1 },
    ];
    let rewritten = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 0,
            op: UnaryOp::Neg,
            arg: 0,
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    let rows = [
        (product_plus(0, 1, 1.0), vec![5]),
        (random.clone(), vec![6]),
        (product_plus(0, 1, 2.0), vec![7]),
        (rewritten.clone(), vec![8]),
    ];
    let programs = programs(&rows);
    let shared = SharedValueSegments::derive(&programs);
    shared.check(&programs).unwrap();
    let segments = shared.segments();
    assert_eq!(segments.len(), 4);
    assert_eq!(
        segments[1].ops(),
        random.as_slice(),
        "an effect runs unchanged"
    );
    assert_eq!(segments[3].ops(), rewritten.as_slice());
}

#[test]
fn a_tensor_value_keeps_its_layout_and_is_shared_whole() {
    let product = |target_offset: Reg| {
        vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: 4,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::MatrixMultiply {
                dst_start: 4,
                lhs_start: 0,
                rhs_start: 0,
                rows: 2,
                inner: 2,
                columns: 2,
                lanes: 1,
            },
            LinearOp::StoreOutput {
                src: 4 + target_offset,
            },
        ]
    };
    let rows = [
        (product(0), vec![10]),
        (product(1), vec![11]),
        (product(3), vec![12]),
    ];
    let programs = programs(&rows);
    let shared = SharedValueSegments::derive(&programs);
    shared.check(&programs).unwrap();
    let [segment] = shared.segments() else {
        panic!("one segment");
    };
    assert_eq!(count(segment.ops(), "MatrixMultiply"), 1);
    assert_eq!(count(segment.ops(), "TensorLoad"), 1);
}

#[test]
fn a_segment_closes_at_the_register_cap_and_records_what_is_recomputed() {
    let size = 91;
    assert!(size * size > SHARED_VALUE_REGISTER_CAP / 2);
    let identity = |slot: usize| {
        (
            vec![
                LinearOp::TensorIdentity {
                    dst_start: 0,
                    size,
                    lanes: 1,
                },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![slot],
        )
    };
    let rows = [identity(0), identity(1)];
    let programs = programs(&rows);
    let shared = SharedValueSegments::derive(&programs);
    shared.check(&programs).unwrap();
    assert_eq!(shared.segments().len(), 2);
    assert_eq!(
        shared.capped(),
        [CappedValue {
            segment: 1,
            operation: "TensorIdentity"
        }]
    );
}

#[test]
fn the_checker_rejects_a_segment_that_computes_another_value() {
    let rows = [
        (product_plus(0, 1, 1.0), vec![10]),
        (product_plus(0, 1, 2.0), vec![11]),
    ];
    let programs = programs(&rows);
    let mut shared = SharedValueSegments::derive(&programs);
    for op in &mut shared.segments[0].ops {
        if let LinearOp::Const { value, .. } = op
            && *value == 2.0
        {
            *value = 2.5;
        }
    }
    assert_eq!(
        shared.check(&programs),
        Err(SharedValueError::Output { index: 1 })
    );
    shared.segments[0].targets[1] = 12;
    assert!(
        shared.check(&programs).is_err(),
        "a moved store changes a final slot"
    );
}

/// `y[target] = fill(0, 1) + y[a]` with the fill value copied into place.
fn copied_fill(a: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::Move { dst: 1, src: 0 },
        LinearOp::TensorFill {
            dst_start: 2,
            value_start: 1,
            count: 1,
            lanes: 1,
        },
        LinearOp::LoadY { dst: 3, index: a },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ]
}

#[test]
fn a_copy_names_the_value_it_copies_and_a_gathered_range_checks() {
    let rows = [(copied_fill(0), vec![10]), (copied_fill(1), vec![11])];
    let programs = programs(&rows);
    let shared = SharedValueSegments::derive(&programs);
    shared.check(&programs).unwrap();
    let [segment] = shared.segments() else {
        panic!("one segment");
    };
    assert_eq!(count(segment.ops(), "TensorFill"), 1, "the fill is shared");
    assert_eq!(
        count(segment.ops(), "Const"),
        1,
        "the copied constant is shared"
    );
    ScalarProgramRegisterFlow::derive(segment.ops()).expect("the segment is a valid program");
}
