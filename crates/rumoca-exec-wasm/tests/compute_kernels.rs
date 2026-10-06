//! Execute emitted binaries and compare to the shared scalar projection.
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
use rumoca_exec_wasm::compile_expression_compute_block_wasm;
use rumoca_ir_solve::*;
mod support;
use support::{assert_bits, execute, scalar_reference};

fn domain(width: usize, height: usize) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![
            StructuredIndexBinder {
                id: 0,
                display_name: "row".into(),
                lower: 9,
                upper: 9 - 2 * (height as i64 - 1),
                step: -2,
            },
            StructuredIndexBinder {
                id: 1,
                display_name: "column".into(),
                lower: -3,
                upper: -3 + 3 * (width as i64 - 1),
                step: 3,
            },
        ],
    }
}

fn terms(width: usize) -> Vec<AffineStencilIndexStrideTerm> {
    vec![
        AffineStencilIndexStrideTerm {
            dimension: 0,
            stride: width as isize,
        },
        AffineStencilIndexStrideTerm {
            dimension: 1,
            stride: 1,
        },
    ]
}

fn image_operations(pitch: usize) -> Vec<LinearOp> {
    let center = pitch + 1;
    let offsets = [center + 1, center - 1, center + pitch, center - pitch];
    let mut ops: Vec<_> = offsets
        .into_iter()
        .enumerate()
        .map(|(dst, index)| LinearOp::LoadY {
            dst: dst as Reg,
            index,
        })
        .collect();
    ops.extend([
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 5,
            op: BinaryOp::Sub,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Mul,
            lhs: 4,
            rhs: 4,
        },
        LinearOp::Binary {
            dst: 7,
            op: BinaryOp::Mul,
            lhs: 5,
            rhs: 5,
        },
        LinearOp::Binary {
            dst: 8,
            op: BinaryOp::Add,
            lhs: 6,
            rhs: 7,
        },
        LinearOp::Const {
            dst: 9,
            value: -0.004,
        },
        LinearOp::LoadP { dst: 10, index: 0 },
        LinearOp::Binary {
            dst: 11,
            op: BinaryOp::Mul,
            lhs: 9,
            rhs: 10,
        },
        LinearOp::Binary {
            dst: 12,
            op: BinaryOp::Add,
            lhs: 8,
            rhs: 11,
        },
        LinearOp::StoreOutput { src: 12 },
    ]);
    ops
}

fn image_block(
    width: usize,
    height: usize,
    stencil: bool,
) -> (ComputeBlock, VarLayout, Vec<f64>, Vec<f64>) {
    let pitch = width + 2;
    let count = pitch * (height + 2);
    let ops = image_operations(pitch);
    let loads = (0..4)
        .map(|op_position| AffineStencilLoadStride {
            op_position,
            terms: terms(pitch),
        })
        .collect();
    let constants = vec![AffineStencilConstStride {
        op_position: 9,
        terms: vec![
            AffineStencilConstStrideTerm {
                dimension: 0,
                stride: 0.0007,
            },
            AffineStencilConstStrideTerm {
                dimension: 0,
                stride: -0.0002,
            },
            AffineStencilConstStrideTerm {
                dimension: 1,
                stride: 0.0003,
            },
        ],
    }];
    let domain = domain(width, height);
    let output_map = TensorOutputMap::dense_contiguous(0, &domain).unwrap();
    let span = source_span_from_offsets(1, 0, 1);
    let node = if stencil {
        ComputeNode::AffineStencil {
            domain,
            output_map,
            base_ops: ops,
            load_strides: loads,
            const_strides: constants,
            metadata: TensorNodeMetadata::default(),
            span,
        }
    } else {
        ComputeNode::Map {
            domain,
            output_map,
            base_ops: ops,
            load_strides: loads,
            const_strides: constants,
            metadata: TensorNodeMetadata::default(),
            span,
        }
    };
    let y = (0..count)
        .map(|i| ((i * 37 % 253) as f64 - 126.) / 11.)
        .collect();
    (
        ComputeBlock { nodes: vec![node] },
        VarLayout::from_parts(Default::default(), count, 1),
        y,
        vec![1.25],
    )
}

#[test]
fn image_map_and_stencil_execute_with_scalar_bit_parity_and_extent_independent_code_size() {
    for stencil in [false, true] {
        let mut sizes = vec![];
        for (width, height) in [(160, 90), (320, 180)] {
            let (block, layout, y, p) = image_block(width, height, stencil);
            let compiled = compile_expression_compute_block_wasm(&block, &layout).unwrap();
            wasmparser::Validator::new()
                .validate_all(compiled.module_bytes())
                .unwrap();
            let expected = scalar_reference(&block, &y, &p);
            assert_bits(
                &execute(compiled.module_bytes(), &y, &p, compiled.rows()),
                &expected,
            );
            sizes.push(compiled.module_bytes().len());
            eprintln!(
                "compact {width}x{height} stencil={stencil}: {} bytes, {} scalar-identical outputs",
                compiled.module_bytes().len(),
                compiled.rows()
            );
            assert!(
                sizes.last().unwrap() < &1500,
                "kernel unexpectedly expanded image coordinates"
            );
        }
        assert!(
            sizes[1].abs_diff(sizes[0]) < 32,
            "4x pixels must not produce4x code"
        );
    }
}

#[test]
fn sparse_reordered_outputs_and_negative_affine_load_strides_preserve_slots() {
    let (mut block, layout, y, p) = image_block(4, 3, true);
    if let ComputeNode::AffineStencil {
        output_map,
        load_strides,
        base_ops,
        ..
    } = &mut block.nodes[0]
    {
        output_map.start = 3;
        output_map.strides = vec![
            AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 16,
            },
            AffineStencilIndexStrideTerm {
                dimension: 1,
                stride: 2,
            },
        ];
        for stride in load_strides {
            stride.terms[1].stride = -1;
        }
        for op in &mut base_ops[..4] {
            if let LinearOp::LoadY { index, .. } = op {
                *index += 3;
            }
        }
        *base_ops.last_mut().unwrap() = LinearOp::StoreOutputRange {
            start: 12,
            count: 1,
            stride: 1,
        };
    }
    let scalar = ScalarProgramBlock::with_output_indices(
        vec![vec![
            LinearOp::Const { dst: 0, value: 4.5 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        vec![source_span_from_offsets(1, 0, 1)],
        vec![1],
    )
    .unwrap();
    block.nodes.push(ComputeNode::ScalarPrograms(scalar));
    let compiled = compile_expression_compute_block_wasm(&block, &layout).unwrap();
    assert_bits(
        &execute(compiled.module_bytes(), &y, &p, compiled.rows()),
        &scalar_reference(&block, &y, &p),
    );
}

#[test]
fn empty_domains_execute_no_body_and_invalid_bounds_or_unsupported_kernels_fail() {
    let (mut block, layout, y, p) = image_block(4, 0, false);
    let compiled = compile_expression_compute_block_wasm(&block, &layout).unwrap();
    assert_eq!(compiled.rows(), 0);
    assert!(execute(compiled.module_bytes(), &y, &p, 0).is_empty());
    let (valid, _, _, _) = image_block(4, 3, true);
    let too_short = VarLayout::from_parts(Default::default(), 4, 1);
    assert!(compile_expression_compute_block_wasm(&valid, &too_short).is_err());
    if let ComputeNode::Map { load_strides, .. } = &mut block.nodes[0] {
        load_strides[0].terms[0].dimension = 8;
    }
    assert!(compile_expression_compute_block_wasm(&block, &layout).is_err());
    let node = ComputeNode::LinSolve {
        setup_ops: vec![],
        matrix_start: 0,
        rhs_start: 0,
        n: 1,
        next_reg: 0,
        matrix_pattern: StructuralPattern::full(
            1,
            1,
            PatternProvenance::derived(
                PatternDerivation::ConservativeFull,
                source_span_from_offsets(1, 0, 1),
            )
            .unwrap(),
        )
        .unwrap(),
        metadata: TensorNodeMetadata::default(),
        span: source_span_from_offsets(1, 0, 1),
    };
    assert!(
        compile_expression_compute_block_wasm(&ComputeBlock { nodes: vec![node] }, &layout)
            .is_err()
    );
}
