use super::*;
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{
    AffineStencilConstStride, AffineStencilConstStrideTerm, AffineStencilIndexStrideTerm,
    AffineStencilLoadStride, BinaryOp, TensorNodeMetadata, TensorOutputMap, UnaryOp,
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
