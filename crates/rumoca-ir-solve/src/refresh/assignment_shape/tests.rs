use std::cell::Cell;

use super::*;
use crate::TensorInputKind;

thread_local! {
    static AFFINE_DERIVATIONS: Cell<usize> = const { Cell::new(0) };
}

pub(super) fn record_affine_derivation() {
    AFFINE_DERIVATIONS.with(|count| count.set(count.get() + 1));
}

fn tensor_difference() -> Vec<LinearOp> {
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 23,
            count: 9,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 9,
            input: TensorInputKind::P,
            input_start: 0,
            count: 9,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 18,
            op: BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: 9,
            count: 9,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 18,
            count: 9,
            stride: 1,
        },
    ]
}

#[test]
fn tensor_outputs_share_affine_analysis_across_targets() {
    let program = tensor_difference();
    AFFINE_DERIVATIONS.set(0);
    let shapes = derive_target_assignment_shapes(&program);
    assert_eq!(shapes.len(), 9);
    assert_eq!(AFFINE_DERIVATIONS.get(), 9);
    for (output, shape) in shapes {
        assert_eq!(shape.target_y_index(), 23 + output);
        assert!(matches!(shape, TargetAssignmentShape::Direct { .. }));
        assert_eq!(
            derive_target_assignment_shape_for_output(&program, output, 23 + output),
            Some(shape)
        );
    }
}

#[test]
fn empty_affine_result_is_shared_after_nonlinear_refusal() {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
    ];
    AFFINE_DERIVATIONS.set(0);
    assert!(derive_target_assignment_shapes(&program).is_empty());
    assert_eq!(AFFINE_DERIVATIONS.get(), 1);
}

#[test]
fn direct_single_target_does_not_derive_unused_affine_shapes() {
    let program = tensor_difference();
    AFFINE_DERIVATIONS.set(0);
    assert!(derive_target_assignment_shape_for_output(&program, 0, 23).is_some());
    assert_eq!(AFFINE_DERIVATIONS.get(), 0);
}

#[test]
fn affine_sharing_preserves_output_prefix_and_coefficient_identity() {
    let program = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::LoadP { dst: 2, index: 0 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 3,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::LoadP { dst: 5, index: 1 },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 5,
        },
        LinearOp::Binary {
            dst: 7,
            op: BinaryOp::Sub,
            lhs: 6,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 7 },
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 7 },
    ];
    let shapes = derive_target_assignment_shapes(&program);
    assert_eq!(shapes.len(), 4);
    for (output, coefficient) in [(0, 2), (1, 5)] {
        let expected = derive_target_assignment_shape_for_output(&program, output, 0).unwrap();
        assert!(matches!(expected, TargetAssignmentShape::Affine {
            coefficient_reg: Some(register), ..
        } if register == coefficient));
        assert!(shapes.contains(&(output, expected)));
    }
    assert!(derive_target_assignment_shape_for_output(&program, 2, 0).is_none());
}
