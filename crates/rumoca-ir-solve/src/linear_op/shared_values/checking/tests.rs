use super::*;
use crate::{BinaryOp, TensorInputKind};

fn mixed_sources() -> Vec<(Vec<LinearOp>, Vec<usize>)> {
    let scalar = |target| {
        (
            vec![
                LinearOp::LoadY { dst: 0, index: 100 },
                LinearOp::Const { dst: 1, value: 3.0 },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Add,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
            vec![target],
        )
    };
    vec![
        (
            vec![
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![0],
        ),
        (
            vec![
                LinearOp::TensorLoad {
                    dst_start: 0,
                    input: TensorInputKind::Y,
                    input_start: 0,
                    count: 10_000,
                    seed_start: None,
                    lanes: 1,
                },
                LinearOp::StoreOutputRange {
                    start: 0,
                    count: 10_000,
                    stride: 1,
                },
            ],
            (100..10_100).collect(),
        ),
        scalar(20_000),
        scalar(20_001),
    ]
}

fn programs(rows: &[(Vec<LinearOp>, Vec<usize>)]) -> Vec<AssignmentProgram<'_>> {
    rows.iter()
        .map(|(ops, targets)| AssignmentProgram { ops, targets })
        .collect()
}

#[test]
fn native_identity_barrier_composes_with_changed_scalar_intervals_and_slot_effects() {
    let rows = mixed_sources();
    let sources = programs(&rows);
    let shared = SharedValueSegments::derive(&sources);
    shared.check(&sources).unwrap();
    assert_eq!(shared.segments.len(), 3);
    assert_eq!(shared.segments[1].ops, rows[1].0);
    assert_eq!(shared.segments[1].targets, rows[1].1);
    assert!(shared.shared_operations() > 0);
    assert_eq!(shared.segments[2].targets, [20_000, 20_001]);
}

#[test]
fn native_partition_refuses_reordered_omitted_duplicated_and_grouped_owners() {
    let rows = mixed_sources();
    let sources = programs(&rows);
    let shared = SharedValueSegments::derive(&sources);
    let mut reordered = shared.clone();
    reordered.segments.swap(0, 1);
    assert!(reordered.check(&sources).is_err());
    let mut omitted = shared.clone();
    omitted.segments.remove(1);
    assert!(omitted.check(&sources).is_err());
    let mut duplicated = shared.clone();
    duplicated
        .segments
        .insert(1, duplicated.segments[1].clone());
    assert!(duplicated.check(&sources).is_err());
    let mut grouped = shared;
    grouped.segments[2].first_program = 1;
    assert!(grouped.check(&sources).is_err());
}

#[test]
fn native_identity_refuses_changed_slot_input_target_cardinality_and_store_order() {
    let rows = mixed_sources();
    let sources = programs(&rows);
    let shared = SharedValueSegments::derive(&sources);
    let mut changed = shared.clone();
    if let LinearOp::TensorLoad { input_start, .. } = &mut changed.segments[1].ops[0] {
        *input_start = 1;
    }
    assert!(changed.check(&sources).is_err());
    let mut changed = shared.clone();
    changed.segments[1].targets.swap(0, 1);
    assert!(changed.check(&sources).is_err());
    let mut changed = shared.clone();
    changed.segments[1].targets.pop();
    assert!(changed.check(&sources).is_err());
    let mut changed = shared;
    changed.segments[1].ops.pop();
    changed.segments[1].ops.extend([
        LinearOp::StoreOutput { src: 1 },
        LinearOp::StoreOutput { src: 0 },
    ]);
    assert!(
        changed.check(&sources).is_err(),
        "changed store order changes the first fault"
    );
}

#[test]
fn native_identity_compares_ieee_bits_and_refuses_invalid_register_flow() {
    for bits in [0u64, (-0.0f64).to_bits(), 0x7ff8_0000_0000_0042] {
        let rows = vec![(
            vec![
                LinearOp::Const {
                    dst: 0,
                    value: f64::from_bits(bits),
                },
                LinearOp::TensorFill {
                    dst_start: 1,
                    value_start: 0,
                    count: 2,
                    lanes: 1,
                },
                LinearOp::StoreOutputRange {
                    start: 1,
                    count: 2,
                    stride: 1,
                },
            ],
            vec![0, 1],
        )];
        let sources = programs(&rows);
        let shared = SharedValueSegments::derive(&sources);
        shared.check(&sources).unwrap();
        for changed_bits in [bits ^ (1u64 << 63), bits ^ 1] {
            let mut changed = shared.clone();
            if let LinearOp::Const { value, .. } = &mut changed.segments[0].ops[0] {
                *value = f64::from_bits(changed_bits);
            }
            assert!(changed.check(&sources).is_err());
        }
    }
    let malformed = [(
        vec![LinearOp::StoreOutputRange {
            start: 99,
            count: 2,
            stride: 1,
        }],
        vec![0, 1],
    )];
    let sources = programs(&malformed);
    let shared = SharedValueSegments::derive(&sources);
    assert!(shared.check(&sources).is_err());
}
