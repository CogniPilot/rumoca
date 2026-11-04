use super::*;
use crate::{
    AffineStencilIndexStrideTerm, ComputeBlock, LinearOp, ScalarProgramBlock, TensorNodeMetadata,
    TensorOutputMap,
};
use rumoca_core::{IndexBinderProvenance, SourceId, StructuredIndexBinder, StructuredIndexDomain};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("dispatch.mo"), 3, 7)
}

fn map(count: i64, start: usize, stride: isize) -> ComputeNode {
    ComputeNode::Map {
        domain: StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                provenance: IndexBinderProvenance::Generated,
                id: 0,
                display_name: "i".into(),
                lower: 1,
                upper: count,
                step: 1,
            }],
        },
        output_map: TensorOutputMap {
            start,
            strides: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride,
            }],
        },
        base_ops: vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        load_strides: vec![],
        const_strides: vec![],
        metadata: TensorNodeMetadata::default(),
        span: span(),
    }
}

fn scalar() -> ComputeNode {
    ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_output_indices(
            vec![vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            vec![span()],
            vec![0],
        )
        .unwrap(),
    )
}

#[test]
fn dispatch_geometry_target_bounds_refuse_invalid_and_accept_exact_limit() {
    for (size, limit, groups) in [(0, 64, 1), (65, 64, 1), (1, 0, 1), (1, 1, 0)] {
        assert!(matches!(
            DispatchBounds::construct(size, limit, groups),
            Err(NativeDispatchError::InvalidBounds)
        ));
    }
    assert!(DispatchBounds::construct(64, 64, 1).is_ok());
}

#[test]
fn dispatch_geometry_zero_singleton_and_large_domains_stay_compact() {
    let bounds = DispatchBounds::construct(64, 64, 1_000_000).unwrap();
    for (count, expected) in [
        (0, 0),
        (1, 1),
        (63, 1),
        (64, 1),
        (65, 2),
        (2_000_000, 31_250),
    ] {
        let block = ComputeBlock {
            nodes: vec![map(count, 0, 1)],
        };
        let source = block.checked_source_view("geometry").unwrap();
        let geometry = source.native_dispatch_geometry(&bounds).unwrap();
        assert_eq!(
            geometry
                .checked_entries(block.checked_source_view("read").unwrap(), &bounds)
                .unwrap()
                .len(),
            1
        );
        let entry = &geometry
            .checked_entries(block.checked_source_view("read").unwrap(), &bounds)
            .unwrap()[0];
        assert_eq!(entry.invocations(), count as usize);
        assert_eq!(entry.groups(), expected);
        assert_eq!(entry.group_size(), 64);
        assert_eq!(entry.span(), span());
        assert_eq!(entry.node_index(), 0);
        assert_eq!(entry.family(), NativeDispatchFamily::Map);
    }
}

#[test]
fn dispatch_geometry_uses_domain_points_not_sparse_output_extent() {
    let bounds = DispatchBounds::construct(2, 2, 10).unwrap();
    let block = ComputeBlock {
        nodes: vec![map(3, 4, -2)],
    };
    let source = block.checked_source_view("sparse").unwrap();
    assert_eq!(source.output_count(), 5);
    let geometry = source.native_dispatch_geometry(&bounds).unwrap();
    assert_eq!(
        geometry
            .checked_entries(block.checked_source_view("read").unwrap(), &bounds)
            .unwrap()[0]
            .invocations(),
        3
    );
    assert_eq!(
        geometry
            .checked_entries(block.checked_source_view("read").unwrap(), &bounds)
            .unwrap()[0]
            .groups(),
        2
    );
}

#[test]
fn dispatch_geometry_group_limit_refuses_without_a_partial_product() {
    let bounds = DispatchBounds::construct(64, 64, 1).unwrap();
    let block = ComputeBlock {
        nodes: vec![map(64, 0, 1), map(65, 64, 1)],
    };
    assert!(
        matches!(block.checked_source_view("limit").unwrap().native_dispatch_geometry(&bounds),
        Err(NativeDispatchError::GroupLimit { node_index:1,groups:2,limit:1,span:at }) if at==span())
    );
}

#[test]
fn dispatch_geometry_scalar_nodes_do_not_invent_a_representation() {
    let bounds = DispatchBounds::construct(64, 64, 10).unwrap();
    let block = ComputeBlock {
        nodes: vec![map(1, 0, 1), scalar()],
    };
    assert!(
        matches!(block.checked_source_view("unsupported").unwrap().native_dispatch_geometry(&bounds),
        Err(NativeDispatchError::UnsupportedNode { node_index:1,span:Some(at) }) if at==span())
    );
}

#[test]
fn dispatch_geometry_source_order_kind_and_wire_are_preserved() {
    let ComputeNode::Map {
        domain,
        output_map,
        base_ops,
        load_strides,
        const_strides,
        metadata,
        span: at,
    } = map(2, 2, 1)
    else {
        unreachable!()
    };
    let stencil = ComputeNode::AffineStencil {
        domain,
        output_map,
        base_ops,
        load_strides,
        const_strides,
        metadata,
        span: at,
    };
    let block = ComputeBlock {
        nodes: vec![map(2, 0, 1), stencil],
    };
    let before = serde_json::to_value(&block).unwrap();
    let bounds = DispatchBounds::construct(2, 2, 10).unwrap();
    let geometry = block
        .checked_source_view("order")
        .unwrap()
        .native_dispatch_geometry(&bounds)
        .unwrap();
    assert_eq!(
        geometry
            .checked_entries(block.checked_source_view("read").unwrap(), &bounds)
            .unwrap()
            .iter()
            .map(NativeDispatchEntry::node_index)
            .collect::<Vec<_>>(),
        [0, 1]
    );
    assert_eq!(
        geometry
            .checked_entries(block.checked_source_view("read").unwrap(), &bounds)
            .unwrap()[1]
            .family(),
        NativeDispatchFamily::AffineStencil
    );
    assert_eq!(serde_json::to_value(&block).unwrap(), before);
}

#[test]
fn dispatch_geometry_ceil_does_not_overflow_at_large_valid_sizes() {
    let bounds = DispatchBounds::construct(usize::MAX, usize::MAX, 1).unwrap();
    let block = ComputeBlock {
        nodes: vec![map(i64::from(i32::MAX), 0, 1)],
    };
    let geometry = block
        .checked_source_view("large")
        .unwrap()
        .native_dispatch_geometry(&bounds)
        .unwrap();
    assert_eq!(
        geometry
            .checked_entries(block.checked_source_view("read").unwrap(), &bounds)
            .unwrap()[0]
            .groups(),
        1
    );
}

#[test]
fn dispatch_geometry_empty_source_has_no_invented_kernel() {
    let block = ComputeBlock::default();
    let bounds = DispatchBounds::construct(1, 1, 1).unwrap();
    assert!(
        block
            .checked_source_view("empty")
            .unwrap()
            .native_dispatch_geometry(&bounds)
            .unwrap()
            .checked_entries(block.checked_source_view("read").unwrap(), &bounds)
            .unwrap()
            .is_empty()
    );
    let error = reserve_entries(usize::MAX).unwrap_err();
    assert!(matches!(error, NativeDispatchError::Allocation(_)));
    assert!(!error.to_string().is_empty());
}

#[test]
fn dispatch_geometry_refuses_foreign_equal_source_and_equal_target_bounds() {
    let block = ComputeBlock {
        nodes: vec![map(4, 0, 1)],
    };
    let bounds = DispatchBounds::construct(2, 2, 10).unwrap();
    let source = block.checked_source_view("source").unwrap();
    let geometry = source.native_dispatch_geometry(&bounds).unwrap();
    assert_eq!(geometry.checked_entries(source, &bounds).unwrap().len(), 1);
    let foreign_source = block.clone();
    assert!(
        matches!(geometry.checked_entries(foreign_source.checked_source_view("foreign").unwrap(), &bounds),
        Err(NativeDispatchError::ForeignSource { issued_at:Some(at) }) if at == span())
    );
    let foreign_bounds = DispatchBounds::construct(2, 2, 10).unwrap();
    assert!(matches!(
        geometry.checked_entries(source, &foreign_bounds),
        Err(NativeDispatchError::ForeignBounds)
    ));
}
