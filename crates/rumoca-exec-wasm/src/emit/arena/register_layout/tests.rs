use super::*;
use rumoca_ir_solve::{BinaryOp, TensorIndex, TensorInputKind};

fn load(start: Reg, count: usize) -> LinearOp {
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
fn dead_ranges_reuse_storage_without_same_operation_aliasing() {
    let ops = vec![
        load(0, 12),
        LinearOp::Const {
            dst: 12,
            value: 2.0,
        },
        LinearOp::TensorBinary {
            dst_start: 13,
            op: BinaryOp::Mul,
            lhs_start: 0,
            rhs_start: 12,
            count: 12,
            lhs_stride: 1,
            rhs_stride: 0,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 13,
            count: 12,
            stride: 1,
        },
        load(25, 12),
        LinearOp::StoreOutputRange {
            start: 25,
            count: 12,
            stride: 1,
        },
    ];
    let layout = RegisterLayout::derive(&ops).unwrap();
    assert_eq!(layout.extent(), 25);
    assert_eq!(layout.physical(25).unwrap(), 0);
    assert_ne!(layout.physical(0).unwrap(), layout.physical(13).unwrap());
    for offset in 0..12 {
        assert_eq!(layout.physical(25 + offset).unwrap(), offset);
    }
}

#[test]
fn indexed_selection_remains_affine_across_overwritten_versions() {
    let ops = vec![
        load(10_000, 12),
        LinearOp::Const {
            dst: 10_005,
            value: 7.0,
        },
        LinearOp::LoadIndexedRegister {
            dst: 20_000,
            base: 10_000,
            stride: 1,
            dimensions: vec![3, 4].into_boxed_slice(),
            indices: vec![TensorIndex::Constant(1), TensorIndex::Constant(1)].into_boxed_slice(),
        },
        LinearOp::StoreOutput { src: 20_000 },
    ];
    let layout = RegisterLayout::derive(&ops).unwrap();
    assert_eq!(layout.extent(), 13);
    let base = layout.physical(10_000).unwrap();
    for offset in 0..12 {
        assert_eq!(layout.physical(10_000 + offset).unwrap(), base + offset);
    }
    assert!(layout.physical(9_999).is_err());
}

#[test]
fn undefined_register_sources_still_have_no_checked_placement() {
    assert!(RegisterLayout::derive(&[LinearOp::StoreOutput { src: 7 }]).is_err());
}

#[test]
fn capacity_and_address_consumers_share_the_complete_issued_program_with_ieee_bits() {
    let program = vec![
        LinearOp::Const {
            dst: 10_000,
            value: f64::from_bits(0x7ff8000000000042),
        },
        LinearOp::Const {
            dst: 20_000,
            value: -0.0,
        },
        LinearOp::StoreOutput { src: 10_000 },
        LinearOp::StoreOutput { src: 20_000 },
    ];
    let arena = super::super::ArenaPlan::required(std::slice::from_ref(&program), 0).unwrap();
    let first = arena.root_program(0).unwrap();
    let clone = arena.clone();
    let second = clone.root_program(0).unwrap();
    assert!(std::rc::Rc::ptr_eq(&first.layout, &second.layout));
    assert!(std::rc::Rc::ptr_eq(&first.program, &second.program));
    assert_eq!(arena.root_registers, first.layout.extent());
    for (operation, bits) in first
        .program
        .iter()
        .zip([0x7ff8000000000042, (-0.0f64).to_bits()])
    {
        let LinearOp::Const { value, .. } = operation else {
            panic!("original constant required")
        };
        assert_eq!(value.to_bits(), bits);
    }
    assert!(arena.root_program(1).is_err());
}

#[test]
fn offline_placement_keeps_a_long_lived_output_from_fragmenting_later_tensors() {
    let ops = vec![
        load(0, 3),
        load(3, 2),
        LinearOp::TensorBinary {
            dst_start: 5,
            op: BinaryOp::Add,
            lhs_start: 0,
            rhs_start: 3,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 0,
            lanes: 1,
        },
        load(8, 3),
        LinearOp::TensorBinary {
            dst_start: 11,
            op: BinaryOp::Add,
            lhs_start: 8,
            rhs_start: 8,
            count: 3,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 5,
            count: 3,
            stride: 1,
        },
    ];
    let layout = RegisterLayout::derive(&ops).unwrap();
    // Chronological first-fit needs eleven cells for this exact source program.
    assert_eq!(layout.extent(), 9);
    assert_eq!(layout.physical(5).unwrap(), 0);
    assert_ne!(layout.physical(0).unwrap(), layout.physical(5).unwrap());
    assert_ne!(layout.physical(8).unwrap(), layout.physical(11).unwrap());
    assert_eq!(layout.physical(3).unwrap(), layout.physical(11).unwrap());
    for start in [0, 5, 8, 11] {
        for offset in 0..3 {
            assert_eq!(
                layout.physical(start + offset).unwrap(),
                layout.physical(start).unwrap() + offset,
            );
        }
    }
}

#[test]
fn explicit_private_bound_controls_the_same_checked_contiguous_layout() {
    let ops = vec![
        load(0, 10_000),
        LinearOp::StoreOutputRange {
            start: 0,
            count: 10_000,
            stride: 1,
        },
    ];
    let policy = |pages: u32| {
        crate::CheckedNativeWasmCapacityPolicy::new(crate::NativeWasmCapacityRequest {
            private_arena_bytes: pages * 65_536,
            public_scratch_bytes: 65_536,
        })
        .unwrap()
    };
    let refused =
        super::super::ArenaPlan::required_with_policy(std::slice::from_ref(&ops), 0, policy(1))
            .err()
            .unwrap();
    assert!(refused.contains("required_bytes=80000 capacity_bytes=65536"));
    let admitted = super::super::ArenaPlan::required_with_policy(&[ops], 0, policy(2)).unwrap();
    assert_eq!(admitted.root_registers, 10_000);
    assert_eq!(admitted.root_program(0).unwrap().layout.extent(), 10_000);
}

#[test]
fn default_private_limit_stays_strict_while_explicit_pages_admit_the_same_range() {
    let ops = vec![
        load(0, 9_000_000),
        LinearOp::StoreOutputRange {
            start: 0,
            count: 9_000_000,
            stride: 1,
        },
    ];
    let default = super::super::ArenaPlan::required(std::slice::from_ref(&ops), 0)
        .err()
        .unwrap();
    assert!(default.contains("exceeds 64 MiB: required_bytes=72000000"));
    let policy = crate::CheckedNativeWasmCapacityPolicy::new(crate::NativeWasmCapacityRequest {
        private_arena_bytes: 2048 * 65_536,
        public_scratch_bytes: 65_536,
    })
    .unwrap();
    let arena = super::super::ArenaPlan::required_with_policy(&[ops], 0, policy).unwrap();
    assert_eq!(arena.root_registers, 9_000_000);
}
