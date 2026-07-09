use rumoca_core::{SourceId, Span, StructuredIndexBinder, StructuredIndexDomain};

use crate::{
    AffineStencilConstStride, AffineStencilConstStrideTerm, AffineStencilIndexStrideTerm,
    AffineStencilLoadStride, ComputeBlock, ComputeNode, LinearOp, ScalarProgramBlock, SolveProblem,
    SolveProblemShapeContractError, SparsityPattern, TensorNodeMetadata, TensorOutputMap,
    TensorOutputMapError,
};

fn test_domain(count: usize) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: count as i64,
            step: 1,
        }],
    }
}

fn fixture_span() -> Span {
    Span::from_offsets(SourceId::from_source_name(file!()), 0, 1)
}

fn tensor_node(output_width: usize) -> ComputeNode {
    ComputeNode::MatMul {
        lhs_ops: vec![LinearOp::Const { dst: 0, value: 1.0 }],
        lhs_start: 0,
        rhs_ops: vec![LinearOp::Const { dst: 1, value: 2.0 }],
        rhs_start: 1,
        m: 1,
        k: 1,
        n: output_width,
        lhs_sparsity: SparsityPattern::Dense,
        rhs_sparsity: SparsityPattern::Dense,
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn wide_2d_domain() -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![
            StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            },
            StructuredIndexBinder {
                id: 1,
                display_name: "j".to_string(),
                lower: 1,
                upper: i64::MAX,
                step: 1,
            },
            StructuredIndexBinder {
                id: 2,
                display_name: "k".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            },
        ],
    }
}

fn linsolve_node() -> ComputeNode {
    ComputeNode::LinSolve {
        setup_ops: vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const { dst: 1, value: 2.0 },
        ],
        matrix_start: 0,
        rhs_start: 1,
        n: 1,
        next_reg: 2,
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn map_node() -> ComputeNode {
    let domain = test_domain(3);
    ComputeNode::Map {
        output_map: TensorOutputMap::dense_contiguous(0, &domain).expect("valid dense output map"),
        domain,
        base_ops: vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        load_strides: Vec::new(),
        const_strides: vec![AffineStencilConstStride {
            op_position: 0,
            terms: vec![AffineStencilConstStrideTerm {
                dimension: 0,
                stride: 1.0,
            }],
        }],
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn stencil_node() -> ComputeNode {
    let domain = test_domain(8);
    ComputeNode::AffineStencil {
        output_map: TensorOutputMap::dense_contiguous(0, &domain).expect("valid dense output map"),
        domain,
        base_ops: vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ],
        load_strides: vec![AffineStencilLoadStride {
            op_position: 0,
            terms: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        }],
        const_strides: Vec::new(),
        metadata: TensorNodeMetadata::default(),
        span: Span::DUMMY,
    }
}

fn scalar_node(output_indices: Vec<usize>) -> ComputeNode {
    ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_output_indices(
            vec![vec![
                LinearOp::Const { dst: 0, value: 3.0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            vec![Span::DUMMY],
            output_indices,
        )
        .expect("scalar fixture metadata should match row count"),
    )
}

#[test]
fn compute_block_len_places_mixed_scalar_and_tensor_outputs() {
    let tensor = tensor_node(2);
    let scalar = ComputeNode::ScalarPrograms(ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::Const { dst: 0, value: 3.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span(),
    ));
    let block = ComputeBlock {
        nodes: vec![tensor.clone(), scalar, tensor],
    };

    assert_eq!(block.len(), Ok(5));
}

#[test]
fn compute_block_len_advances_after_absolute_sparse_scalar_outputs() {
    let tensor = tensor_node(1);
    let block = ComputeBlock {
        nodes: vec![tensor.clone(), scalar_node(vec![3]), tensor],
    };

    assert_eq!(block.len(), Ok(5));
}

#[test]
fn compute_block_len_reports_negative_tensor_output_map_with_span() {
    let span = Span::from_offsets(SourceId::from_source_name("bad_tensor_output.mo"), 4, 9);
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: test_domain(2),
            output_map: TensorOutputMap {
                start: 0,
                strides: vec![AffineStencilIndexStrideTerm {
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

    assert_eq!(
        block.len(),
        Err(
            SolveProblemShapeContractError::TensorOutputMapNegativeIndex {
                context: "ComputeBlock::len".to_string(),
                node_index: 0,
                dimension: "Map",
                value: -1,
                span,
            }
        )
    );
}

#[test]
fn compute_block_len_rejects_matmul_output_count_overflow_with_span() {
    let span = Span::from_offsets(SourceId::from_source_name("matmul_overflow.mo"), 6, 10);
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

    assert_eq!(
        block.len(),
        Err(SolveProblemShapeContractError::OutputIndexOverflow {
            context: "ComputeBlock::len".to_string(),
            node_index: 0,
            span: Some(span),
        })
    );
}

#[test]
fn compute_block_len_rejects_scalar_cursor_overflow_with_span() {
    let span = Span::from_offsets(SourceId::from_source_name("scalar_overflow.mo"), 3, 8);
    let block = ComputeBlock {
        nodes: vec![
            ComputeNode::MatMul {
                lhs_ops: Vec::new(),
                lhs_start: 0,
                rhs_ops: Vec::new(),
                rhs_start: 0,
                m: usize::MAX,
                k: 1,
                n: 1,
                lhs_sparsity: SparsityPattern::Dense,
                rhs_sparsity: SparsityPattern::Dense,
                metadata: TensorNodeMetadata::default(),
                span: Span::DUMMY,
            },
            ComputeNode::ScalarPrograms(ScalarProgramBlock::with_source_span(
                vec![vec![LinearOp::StoreOutput { src: 0 }]],
                span,
            )),
        ],
    };

    assert_eq!(
        block.len(),
        Err(SolveProblemShapeContractError::OutputIndexOverflow {
            context: "ComputeBlock::len".to_string(),
            node_index: 1,
            span: Some(span),
        })
    );
}

#[test]
fn dense_output_map_rejects_stride_overflow() {
    let err = TensorOutputMap::dense_contiguous(0, &wide_2d_domain())
        .expect_err("dense output strides should reject host index overflow");

    assert_eq!(err, TensorOutputMapError::OutputIndexOverflow);
}

#[test]
fn compute_block_len_rejects_output_map_start_overflow_with_span() {
    let span = Span::from_offsets(SourceId::from_source_name("map_overflow.mo"), 10, 14);
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: StructuredIndexDomain {
                binders: Vec::new(),
            },
            output_map: TensorOutputMap {
                start: usize::MAX,
                strides: Vec::new(),
            },
            base_ops: vec![LinearOp::StoreOutput { src: 0 }],
            load_strides: Vec::new(),
            const_strides: Vec::new(),
            metadata: TensorNodeMetadata::default(),
            span,
        }],
    };

    assert_eq!(
        block.len(),
        Err(SolveProblemShapeContractError::OutputIndexOverflow {
            context: "ComputeBlock::len".to_string(),
            node_index: 0,
            span: Some(span),
        })
    );
}

#[test]
fn compute_block_is_not_empty_for_rank_zero_tensor_domain() {
    let domain = StructuredIndexDomain {
        binders: Vec::new(),
    };
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            output_map: TensorOutputMap::dense_contiguous(0, &domain)
                .expect("valid dense output map"),
            domain,
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: Vec::new(),
            metadata: TensorNodeMetadata::default(),
            span: Span::DUMMY,
        }],
    };

    assert!(!block.is_empty());
    assert_eq!(block.len(), Ok(1));
}

#[test]
fn compute_node_counts_cover_blocks_and_problem() {
    let scalar = ComputeNode::ScalarPrograms(ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        fixture_span(),
    ));
    let matmul = tensor_node(1);
    let linsolve = linsolve_node();
    let map = map_node();
    let stencil = stencil_node();
    let block = ComputeBlock {
        nodes: vec![
            scalar,
            matmul.clone(),
            linsolve.clone(),
            map.clone(),
            stencil.clone(),
        ],
    };

    let counts = block.compute_node_counts();
    assert_eq!(counts.scalar_programs, 1);
    assert_eq!(counts.matmul, 1);
    assert_eq!(counts.linsolve, 1);
    assert_eq!(counts.map, 1);
    assert_eq!(counts.affine_stencil, 1);
    assert_eq!(block.tensor_node_count(), 4);

    let mut problem = SolveProblem::default();
    problem.continuous.implicit_rhs = block;
    problem.continuous.derivative_rhs = ComputeBlock {
        nodes: vec![matmul, linsolve, map, stencil],
    };
    let problem_counts = problem.compute_node_counts();
    assert_eq!(problem_counts.scalar_programs, 1);
    assert_eq!(problem_counts.matmul, 2);
    assert_eq!(problem_counts.linsolve, 2);
    assert_eq!(problem_counts.map, 2);
    assert_eq!(problem_counts.affine_stencil, 2);
    assert_eq!(problem_counts.tensor_nodes(), 8);
}

#[test]
fn linear_solve_component_query_covers_scalar_programs_and_nodes() {
    let scalar_linsolve = ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::LinearSolveComponent {
                dst: 1,
                matrix_start: 0,
                rhs_start: 1,
                n: 1,
                component: 0,
            },
            LinearOp::StoreOutput { src: 1 },
        ]],
        fixture_span(),
    );
    assert!(scalar_linsolve.uses_linear_solve_component());

    let scalar_block = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(scalar_linsolve)],
    };
    assert!(scalar_block.uses_linear_solve_component());

    let tensor_block = ComputeBlock {
        nodes: vec![linsolve_node()],
    };
    assert!(tensor_block.uses_linear_solve_component());

    let mut problem = SolveProblem::default();
    problem.continuous.derivative_rhs = scalar_block;
    assert!(problem.uses_linear_solve_component());
}
