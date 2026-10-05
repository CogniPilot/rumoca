use super::*;
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{
    AffineStencilConstStride, AffineStencilConstStrideTerm, AffineStencilIndexStrideTerm,
    AffineStencilLoadStride, BinaryOp, LinearOp, TensorNodeMetadata, TensorOutputMap, UnaryOp,
};

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 0, 1)
}

fn binder(id: usize, lower: i64, upper: i64, step: i64) -> StructuredIndexBinder {
    StructuredIndexBinder {
        id,
        display_name: format!("b{id}"),
        lower,
        upper,
        step,
    }
}

/// The interior `i in 2:4, j in 2:6` of a 6 x 8 grid stored row-major: a
/// strided output map, two strided state loads, a parameter, and a strided
/// constant (the cell coordinate) feeding a transcendental.
fn grid_stencil() -> ComputeNode {
    let domain = StructuredIndexDomain {
        binders: vec![binder(0, 2, 4, 1), binder(1, 2, 6, 1)],
    };
    let index_strides = |row: isize| {
        vec![
            AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: row,
            },
            AffineStencilIndexStrideTerm {
                dimension: 1,
                stride: 1,
            },
        ]
    };
    ComputeNode::AffineStencil {
        output_map: TensorOutputMap {
            start: 2 + 9,
            strides: index_strides(8),
        },
        domain,
        base_ops: vec![
            LinearOp::LoadY { dst: 0, index: 9 },
            LinearOp::LoadY { dst: 1, index: 17 },
            LinearOp::LoadP { dst: 2, index: 1 },
            LinearOp::Const { dst: 3, value: 0.1 },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Sub,
                lhs: 1,
                rhs: 0,
            },
            LinearOp::Binary {
                dst: 5,
                op: BinaryOp::Mul,
                lhs: 4,
                rhs: 2,
            },
            LinearOp::Unary {
                dst: 6,
                op: UnaryOp::Sin,
                arg: 3,
            },
            LinearOp::Binary {
                dst: 7,
                op: BinaryOp::Add,
                lhs: 5,
                rhs: 6,
            },
            LinearOp::StoreOutput { src: 7 },
        ],
        load_strides: vec![
            AffineStencilLoadStride {
                op_position: 0,
                terms: index_strides(8),
            },
            AffineStencilLoadStride {
                op_position: 1,
                terms: index_strides(8),
            },
        ],
        const_strides: vec![AffineStencilConstStride {
            op_position: 3,
            terms: vec![
                AffineStencilConstStrideTerm {
                    dimension: 0,
                    stride: 0.3,
                },
                AffineStencilConstStrideTerm {
                    dimension: 1,
                    stride: -0.07,
                },
            ],
        }],
        metadata: TensorNodeMetadata::default(),
        span: span(),
    }
}

fn scalar_node() -> ComputeNode {
    ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_program_spans(
            vec![
                vec![
                    LinearOp::LoadY { dst: 0, index: 3 },
                    LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    LinearOp::LoadTime { dst: 0 },
                    LinearOp::StoreOutput { src: 0 },
                ],
            ],
            vec![span(), span()],
        )
        .expect("scalar rows"),
    )
}

fn inputs() -> (Vec<f64>, Vec<f64>, f64) {
    let y = (0..48).map(|k| (k as f64 * 0.37).cos() * 1.5).collect();
    (y, vec![0.0, 1.0 / 3.0], 0.25)
}

fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}

#[test]
fn compact_kernel_matches_the_compiled_scalar_view_bit_for_bit() {
    let block = ComputeBlock {
        nodes: vec![scalar_node(), grid_stencil()],
    };
    let view = rumoca_eval_solve::to_scalar_program_block(&block).expect("scalar view");
    let rows = compile_expression_scalar_program_block(&view).expect("row compile");
    let compact = compile_expression_compute_block(&block, None)
        .expect("compact compile")
        .expect("the stencil owns a loop kernel");
    assert_eq!(compact.kernel_count(), 1);
    assert_eq!(compact.compiled_row_count(), 2);

    let (y, p, t) = inputs();
    let width = 48;
    let mut expected = vec![f64::NAN; width];
    let mut actual = vec![f64::NAN; width];
    rows.call(&y, &p, t, &mut expected).expect("row call");
    compact
        .call_with_external_tables(&y, &p, t, &[], &mut actual)
        .expect("compact call");
    assert_eq!(bits(&actual), bits(&expected));
}

#[test]
fn compact_kernel_rejects_short_inputs_before_execution() {
    let block = ComputeBlock {
        nodes: vec![grid_stencil()],
    };
    let compact = compile_expression_compute_block(&block, None)
        .expect("compact compile")
        .expect("the stencil owns a loop kernel");
    let (y, p, t) = inputs();
    // The largest strided load reads y[17 + 2 * 8 + 4] = y[37].
    let mut out = vec![0.0; 48];
    assert!(
        compact
            .call_with_external_tables(&y[..37], &p, t, &[], &mut out)
            .is_err()
    );
    compact
        .call_with_external_tables(&y[..38], &p, t, &[], &mut out)
        .expect("the proven bound admits exactly the read inputs");
}

#[test]
fn block_without_a_kernel_owned_node_keeps_the_scalar_path() {
    let block = ComputeBlock {
        nodes: vec![scalar_node()],
    };
    assert!(
        compile_expression_compute_block(&block, None)
            .expect("compile")
            .is_none()
    );
}

/// A directional Map over `i in 1:6`: `d/dv (y[i] * p[0] + sin(c_i))` with a
/// strided state load, its strided seed load, and a strided constant.
fn directional_map() -> ComputeNode {
    let domain = StructuredIndexDomain {
        binders: vec![binder(0, 1, 6, 1)],
    };
    let unit = || {
        vec![AffineStencilIndexStrideTerm {
            dimension: 0,
            stride: 1,
        }]
    };
    ComputeNode::Map {
        output_map: TensorOutputMap {
            start: 3,
            strides: unit(),
        },
        domain,
        base_ops: vec![
            LinearOp::LoadY { dst: 0, index: 2 },
            LinearOp::LoadSeed { dst: 1, index: 2 },
            LinearOp::LoadP { dst: 2, index: 1 },
            LinearOp::Const { dst: 3, value: 0.5 },
            LinearOp::Unary {
                dst: 4,
                op: UnaryOp::Cos,
                arg: 3,
            },
            LinearOp::Binary {
                dst: 5,
                op: BinaryOp::Mul,
                lhs: 1,
                rhs: 2,
            },
            LinearOp::Binary {
                dst: 6,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 4,
            },
            LinearOp::Binary {
                dst: 7,
                op: BinaryOp::Add,
                lhs: 5,
                rhs: 6,
            },
            LinearOp::StoreOutput { src: 7 },
        ],
        load_strides: vec![
            AffineStencilLoadStride {
                op_position: 0,
                terms: unit(),
            },
            AffineStencilLoadStride {
                op_position: 1,
                terms: unit(),
            },
        ],
        const_strides: vec![AffineStencilConstStride {
            op_position: 3,
            terms: vec![AffineStencilConstStrideTerm {
                dimension: 0,
                stride: 0.25,
            }],
        }],
        metadata: TensorNodeMetadata::default(),
        span: span(),
    }
}

#[test]
fn directional_kernel_matches_the_compiled_scalar_view_bit_for_bit() {
    let block = ComputeBlock {
        nodes: vec![directional_map(), scalar_node()],
    };
    let view = rumoca_eval_solve::to_scalar_program_block(&block).expect("scalar view");
    let rows = compile_jacobian_scalar_program_block(&view).expect("row compile");
    let compact = compile_jacobian_compute_block(&block, None)
        .expect("compact compile")
        .expect("the map owns a directional loop kernel");
    assert_eq!(compact.kernel_count(), 1);
    assert_eq!(compact.compiled_row_count(), 2);

    let (y, p, t) = inputs();
    let seed = (0..48).map(|k| (k as f64 * 0.11).sin()).collect::<Vec<_>>();
    let mut expected = vec![f64::NAN; 12];
    let mut actual = vec![f64::NAN; 12];
    rows.call(&y, &p, t, &seed, &mut expected)
        .expect("row call");
    compact
        .call_with_external_tables(&y, &p, t, &seed, &[], &mut actual)
        .expect("compact call");
    assert_eq!(bits(&actual), bits(&expected));
}

#[test]
fn a_seed_load_keeps_a_residual_block_on_the_row_path() {
    let block = ComputeBlock {
        nodes: vec![directional_map()],
    };
    assert!(
        compile_expression_compute_block(&block, None)
            .expect("compile")
            .is_none()
    );
}

#[test]
fn a_node_whose_plan_reads_below_zero_is_rejected_at_compile_time() {
    // `i in 0:2` loads y[9 - 5 i]: the plan proves the load index over the whole
    // domain at construction and rejects the corner i = 2.
    let ComputeNode::AffineStencil {
        output_map,
        base_ops,
        metadata,
        span,
        ..
    } = grid_stencil()
    else {
        unreachable!("grid_stencil is an affine stencil");
    };
    let node = ComputeNode::Map {
        domain: StructuredIndexDomain {
            binders: vec![binder(0, 0, 2, 1)],
        },
        output_map: TensorOutputMap {
            start: output_map.start,
            strides: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 1,
            }],
        },
        base_ops: base_ops.clone(),
        load_strides: vec![AffineStencilLoadStride {
            op_position: 0,
            terms: vec![AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: -5,
            }],
        }],
        const_strides: Vec::new(),
        metadata,
        span,
    };
    let block = ComputeBlock { nodes: vec![node] };
    assert!(matches!(
        compile_expression_compute_block(&block, None),
        Err(CompileError::Input(_))
    ));
}
