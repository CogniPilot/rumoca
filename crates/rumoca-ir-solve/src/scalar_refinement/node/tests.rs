use super::*;
use crate::{
    AffineStencilIndexStrideTerm, ComputeBlock, ComputeNode, LinearOp, ScalarProgramBlock,
    TensorNodeMetadata, TensorOutputMap,
};
use rumoca_core::{
    IndexBinderProvenance, SourceId, Span, StructuredIndexBinder, StructuredIndexDomain,
};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("node.mo"), 7, 13)
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

fn scalar(outputs: Option<Vec<usize>>, value: f64) -> ComputeNode {
    let programs = vec![vec![
        LinearOp::Const { dst: 0, value },
        LinearOp::StoreOutput { src: 0 },
    ]];
    let block = match outputs {
        Some(outputs) => {
            ScalarProgramBlock::with_output_indices(programs, vec![span()], outputs).unwrap()
        }
        None => ScalarProgramBlock::with_program_spans(programs, vec![span()]).unwrap(),
    };
    ComputeNode::ScalarPrograms(block)
}

#[test]
fn selected_node_does_not_expand_large_unselected_domain_and_keeps_global_identity() {
    for count in [2, 2_000_000] {
        let block = ComputeBlock {
            nodes: vec![map(count, 0, 1), scalar(None, -0.0)],
        };
        let source = block.checked_source_view("large prefix").unwrap();
        let issued = source.scalar_node_refinement(1).unwrap();
        let projection = issued.checked_projection(source, 1).unwrap();
        assert_eq!(projection.block().programs().len(), 1);
        assert_eq!(projection.block().output_indices(), [count as usize]);
        assert_eq!(
            projection.sources(),
            [crate::RefreshScalarProgramSource::checked(1, 0)]
        );
        assert_eq!(projection.block().program_spans(), [span()]);
        let LinearOp::Const { value, .. } = projection.block().programs()[0][0] else {
            panic!("constant missing")
        };
        assert_eq!(value.to_bits(), (-0.0f64).to_bits());
    }
}

#[test]
fn selected_node_keeps_sparse_cursor_order_and_matches_whole_projection() {
    let block = ComputeBlock {
        nodes: vec![map(2, 3, -2), scalar(Some(vec![2]), 2.0), scalar(None, 3.0)],
    };
    let before = serde_json::to_value(&block).unwrap();
    let source = block.checked_source_view("mixed").unwrap();
    let whole = source.scalar_refinement().unwrap();
    let whole = whole.checked_projection(source).unwrap();
    for (index, range, outputs) in [
        (0, 0..2, vec![3, 1]),
        (1, 2..3, vec![2]),
        (2, 3..4, vec![4]),
    ] {
        let issued = source.scalar_node_refinement(index).unwrap();
        let projection = issued.checked_projection(source, index).unwrap();
        assert_eq!(
            projection.block().programs(),
            &whole.block().programs()[range.clone()]
        );
        assert_eq!(projection.block().output_indices(), outputs);
        assert_eq!(projection.sources(), &whole.sources()[range]);
    }
    assert_eq!(serde_json::to_value(&block).unwrap(), before);
}

#[test]
fn selected_node_rejects_foreign_equal_source_different_node_and_absent_indices() {
    let block = ComputeBlock {
        nodes: vec![scalar(None, 1.0), scalar(None, 1.0)],
    };
    let source = block.checked_source_view("source").unwrap();
    let issued = source.scalar_node_refinement(0).unwrap();
    assert!(matches!(
        issued.checked_projection(source, 1),
        Err(SourceNodeRefinementError::ForeignNode {
            issued: 0,
            expected: 1
        })
    ));
    let foreign = block.clone();
    assert!(matches!(
        issued.checked_projection(foreign.checked_source_view("foreign").unwrap(), 0),
        Err(SourceNodeRefinementError::ForeignSource(_))
    ));
    for node_index in [2, usize::MAX] {
        assert!(
            matches!(source.scalar_node_refinement(node_index),Err(SourceNodeRefinementError::UnknownNode {node_index:at}) if at==node_index)
        );
    }
}

#[test]
fn selected_empty_domain_and_refinement_error_do_not_issue_partial_products() {
    let block = ComputeBlock {
        nodes: vec![map(0, 0, 1)],
    };
    let source = block.checked_source_view("zero").unwrap();
    let issued = source.scalar_node_refinement(0).unwrap();
    assert!(
        issued
            .checked_projection(source, 0)
            .unwrap()
            .block()
            .programs()
            .is_empty()
    );
    let empty = ComputeBlock::default();
    assert!(matches!(
        empty
            .checked_source_view("empty")
            .unwrap()
            .scalar_node_refinement(0),
        Err(SourceNodeRefinementError::UnknownNode { node_index: 0 })
    ));
}

#[test]
fn selected_refinement_overflow_keeps_its_original_span_and_issues_nothing() {
    let block = ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops: vec![LinearOp::Const { dst: 0, value: 1.0 }],
            lhs_start: 0,
            rhs_ops: vec![LinearOp::Const { dst: 1, value: 2.0 }],
            rhs_start: 1,
            m: usize::MAX,
            k: 2,
            n: 1,
            lhs_pattern: crate::fixture_pattern(1, 1, false),
            rhs_pattern: crate::fixture_pattern(1, 1, false),
            metadata: TensorNodeMetadata::default(),
            span: span(),
        }],
    };
    let source = block.checked_source_view("overflow").unwrap();
    let error = source.scalar_node_refinement(0).err().unwrap();
    assert!(error.to_string().contains("overflow"));
    let SourceNodeRefinementError::Scalar(error) = error else {
        panic!("scalar refusal lost")
    };
    assert_eq!(error.source_span(), Some(span()));
}
