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
