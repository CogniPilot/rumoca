//! Rebinding derived-discrete reads inside compact families and tensor
//! nodes: a family load whose address range meets a rebound P slot, and any
//! tensor-node read of one, is refused rather than partially redirected.

use std::collections::BTreeMap;

use super::*;
use crate::refresh::native_assignment::rebinding::rebind_block;

const P_SCALARS: usize = 8;

fn compact_read() -> NativeScheduleRefusal {
    NativeEvaluationRefusal::DerivedOutputCompactRead.into()
}

fn family_reading_p(index: usize) -> ComputeBlock {
    ComputeBlock {
        nodes: vec![family(3, 0, 5, LinearOp::LoadP { dst: 1, index })],
    }
}

fn matmul_reading_p(index: usize) -> ComputeBlock {
    ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops: vec![LinearOp::LoadP { dst: 0, index }],
            lhs_start: 0,
            rhs_ops: vec![LinearOp::Const { dst: 1, value: 2.0 }],
            rhs_start: 1,
            m: 1,
            k: 1,
            n: 1,
            lhs_pattern: crate::fixture_pattern(1, 1, false),
            rhs_pattern: crate::fixture_pattern(1, 1, false),
            metadata: TensorNodeMetadata::default(),
            span: Span::DUMMY,
        }],
    }
}

#[test]
fn a_family_load_whose_address_range_meets_a_rebound_slot_is_refused() {
    // The strided load at family position 1 covers P slots 2, 3 and 4.
    let rebinding = BTreeMap::from([(4usize, 6usize)]);
    assert_eq!(
        rebind_block(&family_reading_p(2), &rebinding, P_SCALARS).err(),
        Some(compact_read())
    );
}

#[test]
fn a_family_load_beside_every_rebound_slot_is_kept_as_written() {
    let block = family_reading_p(2);
    let rebinding = BTreeMap::from([(5usize, 6usize), (1usize, 7usize)]);
    let rebound = rebind_block(&block, &rebinding, P_SCALARS).expect("no family read is rebound");
    assert_eq!(format!("{:?}", rebound.nodes), format!("{:?}", block.nodes));
}

#[test]
fn a_tensor_node_read_of_a_rebound_slot_is_refused_and_an_unrelated_one_kept() {
    let rebinding = BTreeMap::from([(3usize, 6usize)]);
    assert_eq!(
        rebind_block(&matmul_reading_p(3), &rebinding, P_SCALARS).err(),
        Some(compact_read())
    );
    let block = matmul_reading_p(2);
    let rebound = rebind_block(&block, &rebinding, P_SCALARS).expect("the read is not rebound");
    assert_eq!(format!("{:?}", rebound.nodes), format!("{:?}", block.nodes));
}
