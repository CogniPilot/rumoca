use super::super::{RootRegisterLayout, RootRegisterTarget};
use crate::{BinaryOp, LinearOp, TensorInputKind, UnaryOp};

fn load(start: u32, count: usize) -> LinearOp {
    LinearOp::TensorLoad {
        dst_start: start,
        input: TensorInputKind::P,
        input_start: 0,
        count,
        seed_start: None,
        lanes: 1,
    }
}

#[test]
fn diagnostic_scalar_operands_and_destination_are_live_together() {
    let layout = RootRegisterLayout::for_target(
        &[
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::Const { dst: 1, value: 3.0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ],
        RootRegisterTarget::scalar_locals(50_000),
    )
    .unwrap();
    assert_eq!(layout.scalar_local_count(), 3);
    assert_ne!(layout.scalar_local_slot(0), layout.scalar_local_slot(2));
    assert_ne!(layout.scalar_local_slot(1), layout.scalar_local_slot(2));
}

#[test]
fn diagnostic_scalar_slots_reuse_only_after_the_last_read() {
    let layout = RootRegisterLayout::for_target(
        &[
            LinearOp::Const {
                dst: 1_000_000_000,
                value: -0.0,
            },
            LinearOp::StoreOutput { src: 1_000_000_000 },
            LinearOp::Const { dst: 3, value: 1.0 },
            LinearOp::StoreOutput { src: 3 },
        ],
        RootRegisterTarget::scalar_locals(50_000),
    )
    .unwrap();
    assert_eq!(layout.scalar_local_count(), 1);
    assert_eq!(
        layout.scalar_local_slot(1_000_000_000),
        layout.scalar_local_slot(3)
    );
}

#[test]
fn diagnostic_scalar_partial_overwrite_keeps_the_entire_tensor_in_memory() {
    let layout = RootRegisterLayout::for_target(
        &[
            load(0, 3),
            LinearOp::Const { dst: 1, value: 4.0 },
            LinearOp::Unary {
                dst: 3,
                op: UnaryOp::Neg,
                arg: 0,
            },
            LinearOp::StoreOutput { src: 3 },
        ],
        RootRegisterTarget::scalar_locals(50_000),
    )
    .unwrap();
    assert_eq!(layout.scalar_local_count(), 1);
    for register in 0..3 {
        assert_eq!(layout.scalar_local_slot(register), None);
    }
    assert!(layout.scalar_local_slot(3).is_some());
}

#[test]
fn diagnostic_scalar_tensor_broadcast_is_not_a_scalar_reader() {
    let layout = RootRegisterLayout::for_target(
        &[
            LinearOp::Const { dst: 0, value: 2.0 },
            load(1, 3),
            LinearOp::TensorBinary {
                dst_start: 4,
                op: BinaryOp::Mul,
                lhs_start: 1,
                rhs_start: 0,
                count: 3,
                lhs_stride: 1,
                rhs_stride: 0,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 4,
                count: 3,
                stride: 1,
            },
        ],
        RootRegisterTarget::scalar_locals(50_000),
    )
    .unwrap();
    assert_eq!(layout.scalar_local_count(), 0);
}

#[test]
fn diagnostic_scalar_addressed_output_keeps_even_a_single_cell_in_memory() {
    let layout = RootRegisterLayout::for_target(
        &[
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 1,
                stride: 1,
            },
        ],
        RootRegisterTarget::scalar_locals(50_000),
    )
    .unwrap();
    assert_eq!(layout.scalar_local_slot(0), None);
}

#[test]
fn diagnostic_scalar_undefined_read_has_no_placement() {
    assert!(
        RootRegisterLayout::for_target(
            &[LinearOp::StoreOutput { src: 5 }],
            RootRegisterTarget::scalar_locals(50_000)
        )
        .is_err()
    );
}

#[test]
fn target_capacity_and_memory_only_are_explicit_construction_inputs() {
    let program = [
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let memory = RootRegisterLayout::derive(&program).unwrap();
    assert_eq!(memory.scalar_local_count(), 0);
    assert!(
        RootRegisterLayout::for_target(&program, RootRegisterTarget::scalar_locals(2)).is_err()
    );
    let locals =
        RootRegisterLayout::for_target(&program, RootRegisterTarget::scalar_locals(3)).unwrap();
    assert_eq!(locals.scalar_local_count(), 3);
    assert_eq!(memory.extent(), locals.extent());
    for register in 0..3 {
        assert_eq!(memory.physical(register), locals.physical(register));
    }
}

fn scale(dst_start: u32, lhs_start: u32, rhs_start: u32, lanes: usize) -> LinearOp {
    LinearOp::TensorBinary {
        dst_start,
        op: BinaryOp::Mul,
        lhs_start,
        rhs_start,
        count: 3,
        lhs_stride: 1,
        rhs_stride: 0,
        lanes,
    }
}

fn store(start: u32, count: usize) -> LinearOp {
    LinearOp::StoreOutputRange {
        start,
        count,
        stride: 1,
    }
}

#[test]
fn value_target_issues_one_contiguous_run_per_static_tensor_component() {
    let layout = RootRegisterLayout::for_target(
        &[
            load(0, 3),
            LinearOp::Const { dst: 3, value: 2.0 },
            scale(4, 0, 3, 1),
            store(4, 3),
        ],
        RootRegisterTarget::value_locals(50_000, 36),
    )
    .unwrap();
    assert_eq!(layout.scalar_local_count(), 7);
    for start in [0, 4] {
        let base = layout.scalar_local_slot(start).unwrap();
        for offset in 1..3 {
            assert_eq!(
                layout.scalar_local_slot(start + offset),
                Some(base + offset)
            );
        }
    }
    let scalar = layout.scalar_local_slot(3).unwrap();
    for start in [0, 4] {
        let base = layout.scalar_local_slot(start).unwrap();
        assert!(!(base..base + 3).contains(&scalar));
    }
}

#[test]
fn value_target_reuses_a_run_only_after_the_whole_component_is_dead() {
    let layout = RootRegisterLayout::for_target(
        &[load(0, 3), store(0, 3), load(10, 3), store(10, 3)],
        RootRegisterTarget::value_locals(50_000, 36),
    )
    .unwrap();
    assert_eq!(layout.scalar_local_count(), 3);
    for offset in 0..3 {
        assert_eq!(
            layout.scalar_local_slot(offset),
            layout.scalar_local_slot(10 + offset)
        );
    }
}

#[test]
fn value_target_extent_bounds_an_operation_and_everything_it_names() {
    let layout = RootRegisterLayout::for_target(
        &[
            load(0, 3),
            LinearOp::Const { dst: 3, value: 2.0 },
            scale(4, 0, 3, 1),
            store(4, 3),
        ],
        RootRegisterTarget::value_locals(50_000, 2),
    )
    .unwrap();
    // The three-cell operation exceeds the extent, so even its one-cell
    // broadcast operand keeps a memory home and the counted loop stays whole.
    assert_eq!(layout.scalar_local_count(), 0);
    for register in 0..7 {
        assert_eq!(layout.scalar_local_slot(register), None);
    }
}

#[test]
fn value_target_keeps_addressed_and_dual_lane_tensors_in_memory() {
    let addressed = RootRegisterLayout::for_target(
        &[
            load(0, 3),
            load(3, 3),
            LinearOp::DotProduct {
                dst: 6,
                lhs_start: 0,
                rhs_start: 3,
                count: 3,
                lhs_stride: 1,
                rhs_stride: 1,
            },
            LinearOp::StoreOutput { src: 6 },
        ],
        RootRegisterTarget::value_locals(50_000, 36),
    )
    .unwrap();
    for register in 0..6 {
        assert_eq!(addressed.scalar_local_slot(register), None);
    }
    let dual = RootRegisterLayout::for_target(
        &[
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::P,
                input_start: 0,
                count: 3,
                seed_start: None,
                lanes: 2,
            },
            LinearOp::Const { dst: 6, value: 2.0 },
            LinearOp::Const { dst: 7, value: 0.0 },
            scale(8, 0, 6, 2),
            store(8, 6),
        ],
        RootRegisterTarget::value_locals(50_000, 36),
    )
    .unwrap();
    for register in (0..6).chain(8..14) {
        assert_eq!(dual.scalar_local_slot(register), None);
    }
}

#[test]
fn value_target_capacity_counts_every_live_cell() {
    let program = [load(0, 3), store(0, 3)];
    assert!(
        RootRegisterLayout::for_target(&program, RootRegisterTarget::value_locals(2, 36)).is_err()
    );
    let layout =
        RootRegisterLayout::for_target(&program, RootRegisterTarget::value_locals(3, 36)).unwrap();
    assert_eq!(layout.scalar_local_count(), 3);
    let memory = RootRegisterLayout::derive(&program).unwrap();
    assert_eq!(memory.extent(), layout.extent());
}

#[test]
fn value_target_extent_bounds_every_run_an_operation_names() {
    let target = RootRegisterTarget::value_locals(50_000, 36);
    // A local scalar broadcast over a larger tensor would print the whole run.
    let broadcast = RootRegisterLayout::for_target(
        &[
            LinearOp::Const { dst: 0, value: 2.0 },
            load(1, 100),
            LinearOp::TensorBinary {
                dst_start: 200,
                op: BinaryOp::Mul,
                lhs_start: 1,
                rhs_start: 0,
                count: 100,
                lhs_stride: 1,
                rhs_stride: 0,
                lanes: 1,
            },
            store(200, 100),
        ],
        target,
    )
    .unwrap();
    assert_eq!(broadcast.scalar_local_count(), 0);
    // A 1xN by Nx1 product has a one-cell result and N terms.
    let dot = RootRegisterLayout::for_target(
        &[
            load(0, 100),
            load(100, 100),
            LinearOp::MatrixMultiply {
                dst_start: 200,
                lhs_start: 0,
                rhs_start: 100,
                rows: 1,
                inner: 100,
                columns: 1,
                lanes: 1,
            },
            LinearOp::StoreOutput { src: 200 },
        ],
        target,
    )
    .unwrap();
    assert_eq!(dot.scalar_local_count(), 0);
    // One cell repeated by a stride-zero output range has no canonical
    // register versions, so no layout of either kind is issued for it.
    let repeated = [
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 100,
            stride: 0,
        },
    ];
    assert!(RootRegisterLayout::for_target(&repeated, target).is_err());
    assert!(RootRegisterLayout::derive(&repeated).is_err());
    // A wide output range read from a one-cell-per-element run stays in memory.
    let wide = RootRegisterLayout::for_target(&[load(0, 100), store(0, 100)], target).unwrap();
    assert_eq!(wide.scalar_local_count(), 0);
    // The dense 6x6 product the extent was chosen for is admitted whole.
    let dense = RootRegisterLayout::for_target(
        &[
            load(0, 36),
            load(36, 36),
            LinearOp::MatrixMultiply {
                dst_start: 72,
                lhs_start: 0,
                rhs_start: 36,
                rows: 6,
                inner: 6,
                columns: 6,
                lanes: 1,
            },
            store(72, 36),
        ],
        target,
    )
    .unwrap();
    assert_eq!(dense.scalar_local_count(), 108);
}
