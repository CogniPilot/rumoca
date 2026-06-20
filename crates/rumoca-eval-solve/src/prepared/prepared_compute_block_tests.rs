use super::*;
use rumoca_core::{SourceId, Span, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{
    AffineStencilLoadStride, ComputeBlock, ComputeNode, LinearOp, ScalarProgramBlock,
    SparsityPattern, TensorNodeMetadata,
};

fn test_domain() -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    }
}

fn matmul_node(lhs: f64, rhs: &[f64]) -> ComputeNode {
    let mut rhs_ops = Vec::with_capacity(rhs.len());
    for (offset, value) in rhs.iter().copied().enumerate() {
        rhs_ops.push(LinearOp::Const {
            dst: (offset + 1) as u32,
            value,
        });
    }
    ComputeNode::MatMul {
        lhs_ops: vec![LinearOp::Const { dst: 0, value: lhs }],
        lhs_start: 0,
        rhs_ops,
        rhs_start: 1,
        m: 1,
        k: 1,
        n: rhs.len(),
        lhs_sparsity: SparsityPattern::Dense,
        rhs_sparsity: SparsityPattern::Dense,
        metadata: TensorNodeMetadata::default(),
        span: test_span("prepared_matmul.mo"),
    }
}

fn test_span(name: &'static str) -> Span {
    Span::from_offsets(SourceId::from_source_name(name), 1, 2)
}

fn const_store_row(value: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value },
        LinearOp::StoreOutput { src: 0 },
    ]
}

#[test]
fn prepared_vec_with_capacity_rejects_impossible_capacity_with_span() {
    let span = Span::from_offsets(SourceId::from_source_name("prepared.mo"), 3, 9);

    let err =
        prepared_vec_with_capacity::<u8>(usize::MAX, "prepared test vector count", Some(span))
            .expect_err("impossible prepared capacity should fail");

    assert!(matches!(err, EvalSolveError::InvalidRow { .. }));
    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string()
            .contains("prepared test vector count exceeds host memory limits")
    );
}

#[test]
fn prepared_compute_block_evaluates_map_through_scalar_view() {
    let domain = test_domain();
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: domain.clone(),
            output_map: rumoca_ir_solve::TensorOutputMap::dense_contiguous(0, &domain)
                .expect("valid dense output map"),
            base_ops: vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: vec![AffineStencilLoadStride {
                op_position: 0,
                terms: vec![rumoca_ir_solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            }],
            const_strides: Vec::new(),
            metadata: TensorNodeMetadata::default(),
            span: test_span("prepared_map.mo"),
        }],
    };

    let prepared = PreparedComputeBlock::new(&block).expect("valid map block should prepare");
    let mut out = vec![0.0; prepared.len()];
    prepared
        .eval_with_context(
            &[10.0, 20.0, 30.0],
            &[],
            0.0,
            RowEvalContext::default(),
            &mut out,
        )
        .expect("prepared Map evaluation should succeed");

    assert_eq!(out, vec![10.0, 20.0, 30.0]);
}

#[test]
fn prepared_compute_block_writes_sparse_map_output_slots() {
    let domain = test_domain();
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain,
            output_map: rumoca_ir_solve::TensorOutputMap {
                start: 2,
                strides: vec![rumoca_ir_solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 2,
                }],
            },
            base_ops: vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: vec![AffineStencilLoadStride {
                op_position: 0,
                terms: vec![rumoca_ir_solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                }],
            }],
            const_strides: Vec::new(),
            metadata: TensorNodeMetadata::default(),
            span: test_span("prepared_sparse_map.mo"),
        }],
    };

    let prepared =
        PreparedComputeBlock::new(&block).expect("valid sparse map block should prepare");
    let mut out = vec![0.0; prepared.len()];
    prepared
        .eval_with_context(
            &[10.0, 20.0, 30.0],
            &[],
            0.0,
            RowEvalContext::default(),
            &mut out,
        )
        .expect("prepared Map evaluation should succeed");

    assert_eq!(out, vec![0.0, 0.0, 10.0, 0.0, 20.0, 0.0, 30.0]);
}

#[test]
fn prepared_compute_block_rejects_negative_tensor_output_map_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_prepared_output.mo"),
        12,
        24,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: test_domain(),
            output_map: rumoca_ir_solve::TensorOutputMap {
                start: 0,
                strides: vec![rumoca_ir_solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: -1,
                }],
            },
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: Vec::new(),
            metadata: TensorNodeMetadata::default(),
            span,
        }],
    };

    let err = match PreparedComputeBlock::new(&block) {
        Ok(_) => panic!("negative tensor output map should fail preparation"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string()
            .contains("output map produced negative output index -1"),
        "error should explain the invalid output map: {err}"
    );
}

#[test]
fn prepared_compute_block_rejects_scalar_output_count_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_scalar_output.mo"),
        3,
        9,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(
            ScalarProgramBlock::with_output_indices(
                vec![const_store_row(1.0)],
                vec![span],
                vec![usize::MAX],
            )
            .expect("overflow fixture metadata should match row count"),
        )],
    };

    let err = match PreparedComputeBlock::new(&block) {
        Ok(_) => panic!("overflowing scalar output index should fail preparation"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string()
            .contains("node 0 output index arithmetic overflowed"),
        "error should explain output-count overflow: {err}"
    );
}

#[test]
fn prepared_compute_block_rejects_matmul_output_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_matmul_output.mo"),
        20,
        35,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops: Vec::new(),
            lhs_start: 0,
            rhs_ops: Vec::new(),
            rhs_start: 0,
            m: usize::MAX,
            k: 1,
            n: 2,
            lhs_sparsity: SparsityPattern::Dense,
            rhs_sparsity: SparsityPattern::Dense,
            metadata: TensorNodeMetadata::default(),
            span,
        }],
    };

    let err = match PreparedComputeBlock::new(&block) {
        Ok(_) => panic!("overflowing MatMul output shape should fail preparation"),
        Err(err) => err,
    };

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string()
            .contains("node 0 output index arithmetic overflowed"),
        "error should explain MatMul output overflow: {err}"
    );
}

#[test]
fn prepared_compute_block_places_local_scalar_node_after_tensor_output() {
    let block = ComputeBlock {
        nodes: vec![
            matmul_node(2.0, &[3.0, 4.0]),
            ComputeNode::ScalarPrograms(ScalarProgramBlock::with_source_span(
                vec![const_store_row(9.0)],
                test_span("prepared_scalar.mo"),
            )),
            matmul_node(5.0, &[6.0]),
        ],
    };

    let prepared =
        PreparedComputeBlock::new(&block).expect("valid mixed tensor block should prepare");
    let mut out = vec![0.0; prepared.len()];
    prepared
        .eval_with_context(&[], &[], 0.0, RowEvalContext::default(), &mut out)
        .expect("mixed tensor/scalar block should evaluate");

    assert_eq!(out, vec![6.0, 8.0, 9.0, 30.0]);
}

#[test]
fn prepared_compute_block_keeps_tensor_cursor_after_absolute_scalar_node() {
    let block = ComputeBlock {
        nodes: vec![
            matmul_node(2.0, &[3.0, 4.0]),
            ComputeNode::ScalarPrograms(
                ScalarProgramBlock::with_output_indices(
                    vec![const_store_row(9.0)],
                    vec![test_span("prepared_scalar_absolute.mo")],
                    vec![2],
                )
                .expect("absolute scalar fixture metadata should match row count"),
            ),
            matmul_node(5.0, &[6.0]),
        ],
    };

    let prepared =
        PreparedComputeBlock::new(&block).expect("valid mixed tensor block should prepare");
    let mut out = vec![0.0; prepared.len()];
    prepared
        .eval_with_context(&[], &[], 0.0, RowEvalContext::default(), &mut out)
        .expect("mixed tensor/scalar block should evaluate");

    assert_eq!(out, vec![6.0, 8.0, 9.0, 30.0]);
}
