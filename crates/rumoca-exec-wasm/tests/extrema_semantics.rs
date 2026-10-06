//! Execute scalar and full-frame Map extrema against the canonical evaluator.
use rumoca_exec_wasm::compile_expression_compute_block_wasm_bytes;
use rumoca_ir_solve as solve;
mod support;
use support::{assert_bits, execute, scalar_reference};

fn inputs() -> Vec<f64> {
    vec![
        f64::NEG_INFINITY,
        -f64::MAX,
        -1.0,
        -f64::MIN_POSITIVE,
        -f64::from_bits(1),
        -0.0,
        0.0,
        f64::from_bits(1),
        f64::MIN_POSITIVE,
        1.0,
        f64::MAX,
        f64::INFINITY,
        f64::NAN,
        f64::from_bits(0xfff8_0000_0000_0042),
        f64::from_bits(0x7ff0_0000_0000_0001),
        f64::from_bits(0xfff0_0000_0000_0042),
    ]
}

fn operations(
    op: solve::BinaryOp,
    lhs_index: usize,
    rhs_index: usize,
    dst: u32,
) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::LoadP {
            dst: 0,
            index: lhs_index,
        },
        solve::LinearOp::LoadP {
            dst: 1,
            index: rhs_index,
        },
        solve::LinearOp::Binary {
            dst,
            op,
            lhs: 0,
            rhs: 1,
        },
        solve::LinearOp::StoreOutput { src: dst },
    ]
}

fn assert_extremum(actual: f64, expected: f64, lhs: f64, rhs: f64) {
    if expected.is_nan() {
        assert!(actual.is_nan(), "both NaNs must produce NaN");
        assert_ne!(
            actual.to_bits() & (1 << 51),
            0,
            "result must quiet signaling NaN"
        );
    } else if lhs == 0.0 && rhs == 0.0 {
        // Canonical Rust min/max permits either equal operand, including ±0.
        assert_eq!(actual, 0.0);
    } else {
        assert_bits(&[actual], &[expected]);
    }
}

#[test]
fn scalar_extrema_match_canonical_ieee_edges_and_aliasing_destinations() {
    let p = inputs();
    let mut programs = Vec::new();
    let mut pairs = Vec::new();
    for op in [solve::BinaryOp::Min, solve::BinaryOp::Max] {
        for lhs in 0..p.len() {
            for rhs in 0..p.len() {
                programs.push(operations(op, lhs, rhs, ((lhs + rhs) % 2) as u32));
                pairs.push((p[lhs], p[rhs]));
            }
        }
    }
    let block = solve::ComputeBlock {
        nodes: vec![solve::ComputeNode::ScalarPrograms(
            solve::ScalarProgramBlock::with_source_span(
                programs,
                solve::source_span_from_offsets(1, 0, 1)
                    .require_provenance("extrema test")
                    .unwrap(),
            )
            .unwrap(),
        )],
    };
    let layout = solve::VarLayout::from_parts(Default::default(), 0, p.len());
    let bytes = compile_expression_compute_block_wasm_bytes(&block, &layout).unwrap();
    let expected = scalar_reference(&block, &[], &p);
    let actual = execute(&bytes, &[], &p, pairs.len());
    for ((actual, expected), (lhs, rhs)) in actual.into_iter().zip(expected).zip(pairs) {
        assert_extremum(actual, expected, lhs, rhs);
    }
}

#[test]
fn full_frame_map_extrema_match_canonical_ieee_edges() {
    let count = 160 * 90;
    let values = inputs();
    let p: Vec<_> = (0..count)
        .flat_map(|index| {
            [
                values[index % values.len()],
                values[(index / values.len()) % values.len()],
            ]
        })
        .collect();
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: 0,
            display_name: "pixel".into(),
            lower: 1,
            upper: count as i64,
            step: 1,
        }],
    };
    let nodes = [solve::BinaryOp::Min, solve::BinaryOp::Max]
        .into_iter()
        .enumerate()
        .map(|(index, op)| solve::ComputeNode::Map {
            output_map: solve::TensorOutputMap::dense_contiguous(index * count, &domain).unwrap(),
            domain: domain.clone(),
            base_ops: operations(op, 0, 1, 0),
            load_strides: (0..2)
                .map(|op_position| solve::AffineStencilLoadStride {
                    op_position,
                    terms: vec![solve::AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: 2,
                    }],
                })
                .collect(),
            const_strides: vec![],
            metadata: solve::TensorNodeMetadata::default(),
            span: solve::source_span_from_offsets(1, 0, 1),
        })
        .collect();
    let block = solve::ComputeBlock { nodes };
    let layout = solve::VarLayout::from_parts(Default::default(), 0, p.len());
    let bytes = compile_expression_compute_block_wasm_bytes(&block, &layout).unwrap();
    let expected = scalar_reference(&block, &[], &p);
    let actual = execute(&bytes, &[], &p, count * 2);
    for (index, (actual, expected)) in actual.into_iter().zip(expected).enumerate() {
        let offset = (index % count) * 2;
        assert_extremum(actual, expected, p[offset], p[offset + 1]);
    }
}
