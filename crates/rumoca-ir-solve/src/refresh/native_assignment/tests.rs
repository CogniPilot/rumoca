mod blocks;
mod calls;
mod conditionals;
mod gathers;
mod mixed;
mod packed_tuples;
mod permutations;
mod ranges;
mod scalar_stores;
mod strided;
mod varying_constants;

use super::*;
use crate::{
    AffineStencilIndexStrideTerm, AffineStencilLoadStride, BinaryOp, ScalarSlot,
    TensorNodeMetadata, scalar_slot_y,
};
use rumoca_core::{SourceId, Span, StructuredIndexBinder, StructuredIndexDomain};

fn family(count: usize, output: usize, target: usize, input: LinearOp) -> ComputeNode {
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: count as i64,
            step: 1,
        }],
    };
    let strides = vec![AffineStencilIndexStrideTerm {
        dimension: 0,
        stride: 1,
    }];
    ComputeNode::Map {
        output_map: TensorOutputMap::dense_contiguous(output, &domain).unwrap(),
        domain,
        base_ops: vec![
            LinearOp::LoadY {
                dst: 0,
                index: target,
            },
            input,
            LinearOp::Const { dst: 2, value: 2.0 },
            LinearOp::Binary {
                dst: 3,
                op: BinaryOp::Mul,
                lhs: 1,
                rhs: 2,
            },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Sub,
                lhs: 0,
                rhs: 3,
            },
            LinearOp::StoreOutput { src: 4 },
        ],
        load_strides: vec![
            AffineStencilLoadStride {
                op_position: 0,
                terms: strides.clone(),
            },
            AffineStencilLoadStride {
                op_position: 1,
                terms: strides,
            },
        ],
        const_strides: vec![],
        metadata: TensorNodeMetadata::default(),
        span: Span::from_offsets(SourceId::from_source_name("NativeRefresh.mo"), 0, 1),
    }
}

fn fixture(count: usize) -> (ComputeBlock, Vec<Option<ScalarSlot>>, VarLayout) {
    // The consumer is deliberately stored first: only the owner may issue the
    // execution order, which is unrelated to canonical node storage order.
    let source = ComputeBlock {
        nodes: vec![
            family(count, count, count, LinearOp::LoadY { dst: 1, index: 0 }),
            family(count, 0, 0, LinearOp::LoadP { dst: 1, index: 0 }),
        ],
    };
    let targets = (0..count * 2)
        .map(|index| Some(scalar_slot_y(index)))
        .collect();
    let layout = VarLayout::from_parts(Default::default(), count * 2, count);
    (source, targets, layout)
}

#[test]
fn image_sized_native_stages_retain_compact_issued_order_and_prefix() {
    for count in [16, 160 * 90, 320 * 180] {
        let (source, targets, layout) = fixture(count);
        let mut owner = crate::ContinuousRefreshOwners::default();
        owner
            .issue_native_assignment_schedule(&source, &targets, &layout)
            .unwrap();
        let schedule = owner.native_assignment_schedule().unwrap();
        assert_eq!(
            schedule
                .stages()
                .iter()
                .map(|stage| stage.source_node())
                .collect::<Vec<_>>(),
            [1, 0]
        );
        assert_eq!(schedule.stages()[0].target_range().unwrap(), 0..count);
        assert_eq!(
            schedule.stages()[1].target_range().unwrap(),
            count..count * 2
        );
        for stage in schedule.stages() {
            let ComputeNode::Map { base_ops, .. } = &stage.value_kernel().nodes[0] else {
                panic!("native Map retained")
            };
            assert_eq!(base_ops.len(), 6);
            assert_eq!(base_ops.last(), Some(&LinearOp::StoreOutput { src: 3 }));
        }
        owner
            .validate_native_assignment_schedule(&source, &targets, &layout)
            .unwrap();
        let wire = serde_json::to_value(&owner).unwrap();
        assert!(wire.get("native_assignment_schedule").is_none());
    }
}

#[test]
fn native_owner_rejects_cycles_coupled_targets_bad_bounds_and_overlaps() {
    let (mut source, targets, layout) = fixture(16);
    if let ComputeNode::Map { base_ops, .. } = &mut source.nodes[1] {
        base_ops[1] = LinearOp::LoadY { dst: 1, index: 16 };
    }
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native assignment dependency cycle"
    );
    let (mut source, targets, layout) = fixture(16);
    if let ComputeNode::Map {
        base_ops,
        load_strides,
        ..
    } = &mut source.nodes[0]
    {
        base_ops[1] = LinearOp::LoadY { dst: 1, index: 17 };
        // This read remains in bounds and is independent of the base target
        // (16), but aliases a later point's target. Base-point isolation alone
        // must therefore be insufficient to certify the entire family.
        load_strides[1].terms.clear();
    }
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native family reads coupled targets in its own assignment range"
    );
    let (mut source, targets, layout) = fixture(16);
    if let ComputeNode::Map { load_strides, .. } = &mut source.nodes[1] {
        load_strides[1].terms[0].stride = 2;
    }
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native affine address exceeds its owned variable layout"
    );
    let (mut source, mut targets, layout) = fixture(16);
    // Both individual families have valid direct isolators; only their
    // overlapping global target inventories make the schedule inadmissible.
    source.nodes[0] = family(16, 16, 0, LinearOp::LoadP { dst: 1, index: 0 });
    targets[16..].copy_from_slice(
        &(0..16)
            .map(|index| Some(scalar_slot_y(index)))
            .collect::<Vec<_>>(),
    );
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native stateless targets are overlapping or do not cover the complete Y layout"
    );
}

#[test]
fn replacing_native_source_revokes_issued_certificate() {
    let (mut source, targets, layout) = fixture(16);
    let mut owner = crate::ContinuousRefreshOwners::default();
    owner
        .issue_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
    if let ComputeNode::Map { base_ops, .. } = &mut source.nodes[1] {
        base_ops[2] = LinearOp::Const { dst: 2, value: 3.0 };
    }
    assert!(
        owner
            .validate_native_assignment_schedule(&source, &targets, &layout)
            .is_err()
    );
    owner
        .issue_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
    owner
        .validate_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
}

#[test]
fn equivalent_value_projection_does_not_hide_canonical_source_replacement() {
    let (mut source, targets, layout) = fixture(16);
    if let ComputeNode::Map { base_ops, .. } = &mut source.nodes[1] {
        base_ops.insert(
            base_ops.len() - 1,
            LinearOp::Binary {
                dst: 5,
                op: BinaryOp::Sub,
                lhs: 3,
                rhs: 0,
            },
        );
    }
    let mut owner = crate::ContinuousRefreshOwners::default();
    owner
        .issue_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
    let prior = owner.native_assignment_schedule().unwrap().clone();
    if let ComputeNode::Map { base_ops, .. } = &mut source.nodes[1] {
        *base_ops.last_mut().unwrap() = LinearOp::StoreOutput { src: 5 };
    }
    // The equation changes from target-value=0 to value-target=0. Its final
    // assignment value and prefix are identical, but its original source is
    // different and must revoke the construction certificate.
    let replacement = derive(&source, &targets, &layout).unwrap();
    assert!(node_matches(
        &prior.stages()[0].value_kernel().nodes[0],
        &replacement.stages()[0].value_kernel().nodes[0]
    ));
    assert!(
        owner
            .validate_native_assignment_schedule(&source, &targets, &layout)
            .is_err()
    );
    owner
        .issue_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
    owner
        .validate_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();

    let (mut source, mut targets, layout) = fixture(16);
    owner
        .issue_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
    let prior = owner.native_assignment_schedule().unwrap().clone();
    for node in &mut source.nodes {
        if let ComputeNode::Map { output_map, .. } = node {
            output_map.start = 16 - output_map.start;
        }
    }
    targets.rotate_left(16);
    // Reordering logical source outputs together with their targets also
    // leaves every final value stage unchanged; it still changes source facts.
    let replacement = derive(&source, &targets, &layout).unwrap();
    for (prior, replacement) in prior.stages().iter().zip(replacement.stages()) {
        assert!(node_matches(
            &prior.value_kernel().nodes[0],
            &replacement.value_kernel().nodes[0]
        ));
    }
    assert!(
        owner
            .validate_native_assignment_schedule(&source, &targets, &layout)
            .is_err()
    );
    owner
        .issue_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
}

#[test]
fn source_signed_zero_replacement_revokes_native_certificate() {
    let (mut source, targets, layout) = fixture(16);
    if let ComputeNode::Map { base_ops, .. } = &mut source.nodes[1] {
        base_ops[2] = LinearOp::Const { dst: 2, value: 0.0 };
    }
    let mut owner = crate::ContinuousRefreshOwners::default();
    owner
        .issue_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
    if let ComputeNode::Map { base_ops, .. } = &mut source.nodes[1] {
        base_ops[2] = LinearOp::Const {
            dst: 2,
            value: -0.0,
        };
    }
    assert!(
        owner
            .validate_native_assignment_schedule(&source, &targets, &layout)
            .is_err()
    );
    owner
        .issue_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
    owner
        .validate_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
}

#[test]
fn duplicate_stride_records_cannot_forge_parallel_target_addressing() {
    let (mut source, targets, layout) = fixture(16);
    if let ComputeNode::Map { load_strides, .. } = &mut source.nodes[1] {
        load_strides.push(load_strides[0].clone());
    }
    assert!(derive(&source, &targets, &layout).is_err());
}

#[test]
fn affine_stencil_reads_only_previous_family_and_rejects_neighbor_targets() {
    let (mut source, mut targets, _) = fixture(16);
    targets.truncate(30);
    let layout = VarLayout::from_parts(Default::default(), 30, 16);
    source.nodes[0] = family(14, 16, 16, LinearOp::LoadY { dst: 1, index: 0 });
    // The stencil subtracts two shifted addresses of the already assigned
    // family, once per interior point.
    if let ComputeNode::Map {
        base_ops,
        load_strides,
        ..
    } = &mut source.nodes[0]
    {
        base_ops[2] = LinearOp::LoadY { dst: 2, index: 2 };
        base_ops[3] = LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Sub,
            lhs: 2,
            rhs: 1,
        };
        load_strides.push(AffineStencilLoadStride {
            op_position: 2,
            terms: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        });
    }
    let ComputeNode::Map {
        domain,
        output_map,
        base_ops,
        load_strides,
        const_strides,
        metadata,
        span,
    } = source.nodes[0].clone()
    else {
        unreachable!()
    };
    source.nodes[0] = ComputeNode::AffineStencil {
        domain,
        output_map,
        base_ops,
        load_strides,
        const_strides,
        metadata,
        span,
    };
    assert!(derive(&source, &targets, &layout).is_ok());
    if let ComputeNode::AffineStencil { base_ops, .. } = &mut source.nodes[0] {
        base_ops[1] = LinearOp::LoadY { dst: 1, index: 16 };
    }
    assert!(derive(&source, &targets, &layout).is_err());
}
