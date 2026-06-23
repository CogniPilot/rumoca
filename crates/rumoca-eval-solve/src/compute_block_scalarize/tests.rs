use super::*;
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{
    AffineStencilConstStride, AffineStencilLoadStride, SparsityPattern, TensorNodeMetadata, UnaryOp,
};

fn test_tensor_domain(count: usize) -> StructuredIndexDomain {
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

fn one_by_one_matmul_node(lhs: f64, rhs: f64) -> ComputeNode {
    ComputeNode::MatMul {
        lhs_ops: vec![LinearOp::Const { dst: 0, value: lhs }],
        lhs_start: 0,
        rhs_ops: vec![LinearOp::Const { dst: 1, value: rhs }],
        rhs_start: 1,
        m: 1,
        k: 1,
        n: 1,
        lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
        rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
        metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
        span: rumoca_core::Span::DUMMY,
    }
}

fn const_store_row(value: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value },
        LinearOp::StoreOutput { src: 0 },
    ]
}

fn load_p_ops(start: Reg, count: usize) -> Vec<LinearOp> {
    (0..count)
        .map(|i| LinearOp::LoadP {
            dst: start + i as Reg,
            index: i,
        })
        .collect()
}

fn store_output_count(program: &[LinearOp]) -> usize {
    program
        .iter()
        .filter(|op| matches!(op, LinearOp::StoreOutput { .. }))
        .count()
}

/// A matmul must scalarize to a SINGLE program whose operand ops appear once,
/// not once-per-output. This is the fix for the multiplicative memory blowup.
#[test]
fn matmul_scalarizes_to_one_program_without_operand_duplication() {
    let (m, k, n) = (2usize, 2usize, 2usize);
    let lhs_ops = load_p_ops(0, m * k); // regs 0..4
    let rhs_ops = load_p_ops(4, k * n); // regs 4..8
    let block = ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops,
            lhs_start: 0,
            rhs_ops,
            rhs_start: 4,
            m,
            k,
            n,
            lhs_sparsity: SparsityPattern::Dense,
            rhs_sparsity: SparsityPattern::Dense,
            metadata: TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let scalar = to_scalar_program_block(&block).expect("matmul should scalarize");

    // One self-contained program, m*n outputs.
    assert_eq!(scalar.programs.len(), 1);
    assert_eq!(scalar.output_count(), m * n);
    assert_eq!(scalar.output_count(), block.len().expect("block length"));
    assert_eq!(store_output_count(&scalar.programs[0]), m * n);

    // Operand loads appear exactly ONCE (8 = m*k + k*n), not m*n times.
    let load_p_count = scalar.programs[0]
        .iter()
        .filter(|op| matches!(op, LinearOp::LoadP { .. }))
        .count();
    assert_eq!(
        load_p_count,
        m * k + k * n,
        "operands must not be duplicated"
    );
}

/// Every computed destination register in the single program must be unique
/// and monotonic across outputs, so the AD lowering's per-program `bind`
/// never sees a duplicate destination.
#[test]
fn matmul_program_has_unique_monotonic_destination_registers() {
    let (m, k, n) = (3usize, 3usize, 3usize);
    let block = ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops: load_p_ops(0, m * k),
            lhs_start: 0,
            rhs_ops: load_p_ops((m * k) as Reg, k * n),
            rhs_start: (m * k) as Reg,
            m,
            k,
            n,
            lhs_sparsity: SparsityPattern::Dense,
            rhs_sparsity: SparsityPattern::Dense,
            metadata: TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let scalar = to_scalar_program_block(&block).expect("matmul should scalarize");
    let mut seen = std::collections::HashSet::new();
    for op in &scalar.programs[0] {
        if let LinearOp::Binary { dst, .. } = op {
            assert!(seen.insert(*dst), "duplicate destination register {dst}");
        }
    }
}

/// A LinSolve scalarizes to one program: setup ops once, then one solve
/// component + StoreOutput per row, with unique destination registers.
#[test]
fn linsolve_scalarizes_to_one_program_with_unique_components() {
    let n = 3usize;
    // setup writes the n*n matrix then the n rhs entries: regs 0..(n*n+n)
    let setup_ops = load_p_ops(0, n * n + n);
    let next_reg = (n * n + n) as Reg;
    let block = ComputeBlock {
        nodes: vec![ComputeNode::LinSolve {
            setup_ops,
            matrix_start: 0,
            rhs_start: (n * n) as Reg,
            n,
            next_reg,
            metadata: TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let scalar = to_scalar_program_block(&block).expect("linsolve should scalarize");
    assert_eq!(scalar.programs.len(), 1);
    assert_eq!(scalar.output_count(), n);
    assert_eq!(scalar.output_count(), block.len().expect("block length"));

    let mut seen = std::collections::HashSet::new();
    let mut components = 0;
    for op in &scalar.programs[0] {
        if let LinearOp::LinearSolveComponent { dst, component, .. } = op {
            assert!(
                seen.insert(*dst),
                "duplicate solve-component register {dst}"
            );
            assert_eq!(*component, components);
            components += 1;
        }
    }
    assert_eq!(components, n);
}

#[test]
fn affine_stencil_expands_to_exact_scalar_rows() {
    let block = ComputeBlock {
        nodes: vec![ComputeNode::AffineStencil {
            domain: test_tensor_domain(3),
            output_map: rumoca_ir_solve::TensorOutputMap::dense_contiguous(
                0,
                &test_tensor_domain(3),
            )
            .expect("valid dense output map"),
            base_ops: vec![
                LinearOp::LoadP { dst: 0, index: 2 },
                LinearOp::LoadY { dst: 1, index: 10 },
                LinearOp::Unary {
                    dst: 2,
                    op: UnaryOp::Neg,
                    arg: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
            load_strides: vec![AffineStencilLoadStride {
                op_position: 1,
                terms: vec![rumoca_ir_solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 2,
                }],
            }],
            const_strides: Vec::new(),
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let rows = to_scalar_program_block(&block).expect("valid stencil should scalarize");
    assert_eq!(rows.programs.len(), 3);
    assert_eq!(rows.output_indices, vec![0, 1, 2]);
    assert!(matches!(
        rows.programs[0][1],
        LinearOp::LoadY { dst: 1, index: 10 }
    ));
    assert!(matches!(
        rows.programs[1][1],
        LinearOp::LoadY { dst: 1, index: 12 }
    ));
    assert!(matches!(
        rows.programs[2][1],
        LinearOp::LoadY { dst: 1, index: 14 }
    ));
    assert!(matches!(
        rows.programs[2][0],
        LinearOp::LoadP { dst: 0, index: 2 }
    ));
}

#[test]
fn map_expands_through_same_scalar_view_ordering() {
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: test_tensor_domain(3),
            output_map: rumoca_ir_solve::TensorOutputMap::dense_contiguous(
                0,
                &test_tensor_domain(3),
            )
            .expect("valid dense output map"),
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: vec![AffineStencilConstStride {
                op_position: 0,
                terms: vec![rumoca_ir_solve::AffineStencilConstStrideTerm {
                    dimension: 0,
                    stride: 2.0,
                }],
            }],
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let rows = to_scalar_program_block(&block).expect("valid map should scalarize");
    assert_eq!(rows.programs.len(), 3);
    assert_eq!(rows.output_indices, vec![0, 1, 2]);
    assert!(matches!(
        rows.programs[0][0],
        LinearOp::Const { dst: 0, value: 1.0 }
    ));
    assert!(matches!(
        rows.programs[1][0],
        LinearOp::Const { dst: 0, value: 3.0 }
    ));
    assert!(matches!(
        rows.programs[2][0],
        LinearOp::Const { dst: 0, value: 5.0 }
    ));
}

#[test]
fn map_scalar_view_preserves_sparse_output_indices() {
    let domain = test_tensor_domain(3);
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: domain.clone(),
            output_map: rumoca_ir_solve::TensorOutputMap {
                start: 2,
                strides: vec![rumoca_ir_solve::AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 2,
                }],
            },
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: vec![AffineStencilConstStride {
                op_position: 0,
                terms: vec![rumoca_ir_solve::AffineStencilConstStrideTerm {
                    dimension: 0,
                    stride: 1.0,
                }],
            }],
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let rows = to_scalar_program_block(&block).expect("valid sparse map should scalarize");
    assert_eq!(rows.output_indices, vec![2, 4, 6]);
    assert_eq!(rows.output_count(), 7);
}

#[test]
fn scalar_program_block_output_overflow_reports_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_scalarized_output.mo"),
        11,
        17,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(
            ScalarProgramBlock::with_output_indices(
                vec![vec![
                    LinearOp::Const { dst: 0, value: 1.0 },
                    LinearOp::StoreOutput { src: 0 },
                ]],
                vec![span],
                vec![usize::MAX],
            )
            .expect("overflow fixture metadata should match row count"),
        )],
    };

    let err = to_scalar_program_block(&block)
        .expect_err("overflowing scalar output index should fail scalarization");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string()
            .contains(&format!("output index {} overflows", usize::MAX)),
        "error should explain scalar output overflow: {err}"
    );
}

#[test]
fn scalar_program_output_indices_use_first_source_span_after_dummy_row() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_scalarized_cursor.mo"),
        19,
        31,
    );
    let block = ScalarProgramBlock::with_program_spans(
        vec![
            vec![LinearOp::StoreOutput { src: 0 }],
            vec![LinearOp::StoreOutput { src: 1 }],
        ],
        vec![rumoca_core::Span::DUMMY, span],
    )
    .expect("scalar cursor fixture metadata should match row count");

    let err = scalar_program_output_indices(&block, usize::MAX, "scalar cursor")
        .expect_err("overflowing scalar cursor should fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string().contains("scalar cursor"),
        "error should explain scalar cursor overflow: {err}"
    );
}

#[test]
fn scalarize_vec_with_capacity_reports_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_scalarized_capacity.mo"),
        5,
        9,
    );
    let err = scalarize_vec_with_capacity::<u8>(usize::MAX, "scalarize test", span)
        .expect_err("impossible scalarize capacity should fail");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string()
            .contains("scalarized scalarize test capacity"),
        "error should explain scalarize allocation overflow: {err}"
    );
}

#[test]
fn scalarize_rejects_scalar_program_span_mismatch_before_visiting_rows() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_scalarized_spans.mo"),
        3,
        8,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(ScalarProgramBlock {
            programs: vec![vec![LinearOp::StoreOutput { src: 0 }]],
            program_spans: vec![span, span],
            output_indices: vec![0],
        })],
    };

    let err = to_scalar_program_block(&block)
        .expect_err("scalarization should reject malformed scalar row spans");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string().contains("1 scalar programs but 2 spans"),
        "error should explain malformed scalar span metadata: {err}"
    );
}

#[test]
fn scalarize_rejects_scalar_program_output_index_mismatch_before_visiting_rows() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_scalarized_outputs.mo"),
        5,
        13,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(ScalarProgramBlock {
            programs: vec![vec![LinearOp::StoreOutput { src: 0 }]],
            program_spans: vec![span],
            output_indices: vec![0, 1],
        })],
    };

    let err = to_scalar_program_block(&block)
        .expect_err("scalarization should reject malformed scalar output indices");

    assert_eq!(err.source_span(), Some(span));
    assert!(
        err.to_string()
            .contains("1 scalar programs but 2 output indices"),
        "error should explain malformed scalar output metadata: {err}"
    );
}

#[test]
fn scalarize_reports_negative_tensor_output_map_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_output.mo"),
        10,
        20,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: test_tensor_domain(2),
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
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span,
        }],
    };

    let error = to_scalar_program_block(&block)
        .expect_err("negative tensor output map should fail scalarization");

    assert_eq!(error.source_span(), Some(span));
    assert!(
        error
            .to_string()
            .contains("output map produced negative output index -1"),
        "error should explain the invalid output map: {error}"
    );
}

#[test]
fn scalarize_reports_invalid_tensor_output_map_dimension_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_output_dimension.mo"),
        5,
        12,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: test_tensor_domain(1),
            output_map: rumoca_ir_solve::TensorOutputMap {
                start: 0,
                strides: vec![rumoca_ir_solve::AffineStencilIndexStrideTerm {
                    dimension: 1,
                    stride: 1,
                }],
            },
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: Vec::new(),
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span,
        }],
    };

    let error = to_scalar_program_block(&block)
        .expect_err("invalid tensor output map dimension should fail scalarization");

    assert_eq!(error.source_span(), Some(span));
    assert!(
        error
            .to_string()
            .to_string()
            .contains("Map output map references dimension 1, but domain rank is 1"),
        "error should explain the invalid output map dimension at the contract boundary: {error}"
    );
}

#[test]
fn scalarize_reports_invalid_native_stride_position_with_span() {
    let span =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name("bad.mo"), 4, 9);
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: test_tensor_domain(1),
            output_map: rumoca_ir_solve::TensorOutputMap::dense_contiguous(
                0,
                &test_tensor_domain(1),
            )
            .expect("valid dense output map"),
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: vec![AffineStencilLoadStride {
                op_position: 99,
                terms: Vec::new(),
            }],
            const_strides: Vec::new(),
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span,
        }],
    };

    let error = to_scalar_program_block(&block)
        .expect_err("invalid native stride metadata should fail scalarization");
    assert_eq!(error.source_span(), Some(span));
    assert!(
        error
            .to_string()
            .contains("native map family stride op position 99 out of bounds for 2 ops"),
        "error should explain the invalid stride position: {error}"
    );
}

#[test]
fn scalarize_reports_native_stride_wrong_op_kind() {
    let block = ComputeBlock {
        nodes: vec![ComputeNode::AffineStencil {
            domain: test_tensor_domain(1),
            output_map: rumoca_ir_solve::TensorOutputMap::dense_contiguous(
                0,
                &test_tensor_domain(1),
            )
            .expect("valid dense output map"),
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: vec![AffineStencilLoadStride {
                op_position: 0,
                terms: Vec::new(),
            }],
            const_strides: Vec::new(),
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let error = to_scalar_program_block(&block)
        .expect_err("load stride pointing at Const should fail scalarization");
    assert!(
        error.to_string().contains(
            "native affine stencil family stride op position 0 points at Const, expected LoadY or LoadP"
        ),
        "error should explain the wrong stride target: {error}"
    );
}

#[test]
fn scalarize_reports_native_stride_invalid_dimension() {
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: test_tensor_domain(1),
            output_map: rumoca_ir_solve::TensorOutputMap::dense_contiguous(
                0,
                &test_tensor_domain(1),
            )
            .expect("valid dense output map"),
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: Vec::new(),
            const_strides: vec![AffineStencilConstStride {
                op_position: 0,
                terms: vec![rumoca_ir_solve::AffineStencilConstStrideTerm {
                    dimension: 1,
                    stride: 1.0,
                }],
            }],
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };

    let error = to_scalar_program_block(&block)
        .expect_err("stride dimension outside domain should fail scalarization");
    assert!(
        error
            .to_string()
            .contains("native map family stride dimension 1 out of bounds for 1 dimensions"),
        "error should explain the invalid stride dimension: {error}"
    );
}

#[test]
fn scalarize_reports_invalid_stride_metadata_for_empty_domain() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_empty_domain.mo"),
        2,
        6,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            domain: test_tensor_domain(0),
            output_map: rumoca_ir_solve::TensorOutputMap::dense_contiguous(
                0,
                &test_tensor_domain(0),
            )
            .expect("valid dense output map"),
            base_ops: vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            load_strides: vec![AffineStencilLoadStride {
                op_position: 99,
                terms: Vec::new(),
            }],
            const_strides: Vec::new(),
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span,
        }],
    };

    let error = to_scalar_program_block(&block)
        .expect_err("empty native domain should fail shape-contract validation");
    assert_eq!(error.source_span(), Some(span));
    assert!(
        error
            .to_string()
            .contains("scalarize compute block node 0 has zero Map tensor dimension"),
        "error should explain invalid empty domains before zero-row expansion: {error}"
    );
}

#[test]
fn scalarize_matmul_rejects_temp_register_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_matmul_temp.mo"),
        13,
        21,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops: vec![LinearOp::Const {
                dst: u32::MAX,
                value: 1.0,
            }],
            lhs_start: u32::MAX,
            rhs_ops: Vec::new(),
            rhs_start: 0,
            m: 1,
            k: 1,
            n: 1,
            lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span,
        }],
    };

    let error = to_scalar_program_block(&block)
        .expect_err("overflowing MatMul temp register should fail scalarization");

    assert_eq!(error.source_span(), Some(span));
    assert!(
        error
            .to_string()
            .contains("register range starting at 4294967295 with offset 1 overflows"),
        "error should explain temp register overflow: {error}"
    );
}

#[test]
fn scalarize_matmul_rejects_source_register_range_overflow_with_span() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("bad_matmul_source.mo"),
        3,
        8,
    );
    let block = ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops: Vec::new(),
            lhs_start: u32::MAX,
            rhs_ops: Vec::new(),
            rhs_start: 0,
            m: 1,
            k: 2,
            n: 1,
            lhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            rhs_sparsity: rumoca_ir_solve::SparsityPattern::Dense,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span,
        }],
    };

    let error = to_scalar_program_block(&block)
        .expect_err("overflowing MatMul source register range should fail scalarization");

    assert_eq!(error.source_span(), Some(span));
    assert!(
        error
            .to_string()
            .contains("register range starting at 4294967295 with offset 1 overflows"),
        "error should explain source register overflow: {error}"
    );
}

#[test]
fn local_scalar_node_is_placed_after_prior_tensor_output() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("mixed_scalar_tensor.mo"),
        3,
        9,
    );
    let block = ComputeBlock {
        nodes: vec![
            one_by_one_matmul_node(2.0, 3.0),
            ComputeNode::ScalarPrograms(ScalarProgramBlock::with_source_span(
                vec![const_store_row(7.0)],
                span,
            )),
            one_by_one_matmul_node(5.0, 11.0),
        ],
    };

    let rows = to_scalar_program_block(&block).expect("valid mixed block should scalarize");
    assert_eq!(rows.output_indices, vec![0, 1, 2]);
    assert_eq!(rows.output_count(), 3);
}

#[test]
fn explicit_scalar_node_advances_following_tensor_output_cursor() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("explicit_scalar_tensor.mo"),
        7,
        12,
    );
    let block = ComputeBlock {
        nodes: vec![
            one_by_one_matmul_node(2.0, 3.0),
            ComputeNode::ScalarPrograms(
                ScalarProgramBlock::with_output_indices(
                    vec![const_store_row(7.0)],
                    vec![span],
                    vec![3],
                )
                .expect("absolute scalar fixture metadata should match row count"),
            ),
            one_by_one_matmul_node(5.0, 11.0),
        ],
    };

    let rows = to_scalar_program_block(&block).expect("valid mixed block should scalarize");
    assert_eq!(rows.output_indices, vec![0, 3, 4]);
    assert_eq!(rows.output_count(), 5);
}
