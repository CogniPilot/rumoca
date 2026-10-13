use super::*;
use rumoca_core::{SourceId, StructuredIndexBinder};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("compute_source_view.mo"), 0, 1)
}

fn scalar(indices: Vec<usize>) -> ComputeNode {
    let mut ops = vec![LinearOp::Const { dst: 0, value: 1.0 }];
    ops.extend(indices.iter().map(|_| LinearOp::StoreOutput { src: 0 }));
    ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_output_indices(vec![ops], vec![span()], indices).unwrap(),
    )
}

fn map(count: i64, start: usize, stride: isize) -> ComputeNode {
    ComputeNode::Map {
        domain: StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                provenance: rumoca_core::IndexBinderProvenance::Generated,
                id: 0,
                display_name: "i".to_string(),
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
        load_strides: Vec::new(),
        const_strides: Vec::new(),
        metadata: TensorNodeMetadata::default(),
        span: span(),
    }
}

fn matmul(m: usize, n: usize) -> ComputeNode {
    ComputeNode::MatMul {
        lhs_ops: vec![LinearOp::Const { dst: 0, value: 1.0 }],
        lhs_start: 0,
        rhs_ops: vec![LinearOp::Const { dst: 0, value: 1.0 }],
        rhs_start: 0,
        m,
        k: 1,
        n,
        lhs_pattern: crate::fixture_pattern(m, 1, true),
        rhs_pattern: crate::fixture_pattern(1, n, true),
        metadata: TensorNodeMetadata::default(),
        span: span(),
    }
}

fn linsolve() -> ComputeNode {
    ComputeNode::LinSolve {
        setup_ops: vec![LinearOp::Const { dst: 0, value: 1.0 }],
        matrix_start: 0,
        rhs_start: 0,
        n: 2,
        next_reg: 1,
        matrix_pattern: crate::fixture_pattern(2, 2, true),
        metadata: TensorNodeMetadata::default(),
        span: span(),
    }
}

#[test]
fn borrowed_source_preserves_order_native_ranges_and_sparse_affine_holes() {
    let source = ComputeBlock {
        nodes: vec![
            scalar(vec![2, 9]),
            scalar(vec![0, 1, 2]),
            matmul(2, 2),
            linsolve(),
            map(3, 8, -2),
            map(0, 100, 1),
        ],
    };
    let wire = serde_json::to_string(&source).unwrap();
    let replay: ComputeBlock = serde_json::from_str(&wire).unwrap();
    for source in [&source, &replay] {
        let view = source.checked_source_view("placement fixture").unwrap();
        assert!(std::ptr::eq(view.source(), source));
        assert_eq!(view.output_count(), 19);
        let mut visited = 0;
        let flow = view
            .visit_output_placements(|placement| {
                assert_eq!(placement.node_index(), visited);
                assert!(std::ptr::eq(placement.node(), &source.nodes[visited]));
                match (visited, placement.image()) {
                    (0, ComputeOutputImage::Explicit(indices)) => assert_eq!(*indices, &[2, 9]),
                    (1, ComputeOutputImage::Contiguous(range)) => assert_eq!(*range, 10..13),
                    (2, ComputeOutputImage::Contiguous(range)) => assert_eq!(*range, 13..17),
                    (3, ComputeOutputImage::Contiguous(range)) => assert_eq!(*range, 17..19),
                    (
                        4,
                        ComputeOutputImage::Affine {
                            domain, output_map, ..
                        },
                    ) => {
                        assert_eq!(output_map.output_indices(domain).unwrap(), [8, 6, 4]);
                    }
                    (
                        5,
                        ComputeOutputImage::Affine {
                            domain, output_map, ..
                        },
                    ) => {
                        assert!(output_map.output_indices(domain).unwrap().is_empty());
                    }
                    _ => panic!("source placement was changed"),
                }
                visited += 1;
                ControlFlow::<()>::Continue(())
            })
            .unwrap();
        assert_eq!(flow, ControlFlow::Continue(()));
        assert_eq!(visited, 6);
        let mut expected = vec![2, 4, 6, 8, 9];
        expected.extend(10..19);
        assert_eq!(
            source.produced_output_indices("placement fixture").unwrap(),
            expected
        );
    }
}

#[test]
fn borrowed_source_keeps_large_affine_domain_as_one_placement() {
    let source = ComputeBlock {
        nodes: vec![map(131_072, 3, 2)],
    };
    let view = source.checked_source_view("compact placement").unwrap();
    assert_eq!(view.output_count(), 262_146);
    let mut visits = 0;
    let flow = view
        .visit_output_placements(|placement| {
            let ComputeOutputImage::Affine {
                domain, output_map, ..
            } = placement.image()
            else {
                panic!("affine source was materialized");
            };
            let ComputeNode::Map {
                domain: original_domain,
                output_map: original_map,
                ..
            } = &source.nodes[0]
            else {
                unreachable!();
            };
            assert!(std::ptr::eq(*domain, original_domain));
            assert!(std::ptr::eq(*output_map, original_map));
            visits += 1;
            ControlFlow::<()>::Continue(())
        })
        .unwrap();
    assert_eq!(flow, ControlFlow::Continue(()));
    assert_eq!(visits, 1);
}

#[test]
fn borrowed_source_preflight_checks_later_nodes_before_early_stop() {
    let mut source = ComputeBlock {
        nodes: vec![scalar(vec![0]), matmul(0, 1)],
    };
    assert!(matches!(
        source.checked_source_view("preflight"),
        Err(SolveProblemShapeContractError::ZeroTensorDimension { node_index: 1, .. })
    ));
    source.nodes[0] = map(1, usize::MAX - 1, 1);
    source.nodes[1] = scalar(vec![0]);
    assert!(matches!(
        source.checked_source_view("preflight"),
        Err(SolveProblemShapeContractError::OutputIndexOverflow { node_index: 1, .. })
    ));
    source.nodes[0] = scalar(vec![0]);
    source.nodes[1] = map(3, 0, -1);
    assert!(matches!(
        source.checked_source_view("preflight"),
        Err(SolveProblemShapeContractError::TensorOutputMapNegativeIndex { node_index: 1, .. })
    ));
    source.nodes[1] = matmul(1, 1);
    let ComputeNode::MatMul { m, n, .. } = &mut source.nodes[1] else {
        unreachable!();
    };
    *m = usize::MAX;
    *n = 2;
    assert!(matches!(
        source.checked_source_view("preflight"),
        Err(SolveProblemShapeContractError::OutputIndexOverflow { node_index: 1, .. })
    ));
    source.nodes[1] = scalar(vec![5]);
    let flow = source
        .checked_source_view("preflight")
        .unwrap()
        .visit_output_placements(|placement| ControlFlow::Break(placement.node_index()))
        .unwrap();
    assert_eq!(flow, ControlFlow::Break(0));
}
