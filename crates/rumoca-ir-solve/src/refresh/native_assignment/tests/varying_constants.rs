use super::*;
use crate::{AffineStencilConstStride, AffineStencilConstStrideTerm};

fn varying(count: usize, stride: f64) -> (ComputeBlock, Vec<Option<ScalarSlot>>, VarLayout) {
    let mut node = family(count, 0, 0, LinearOp::LoadP { dst: 1, index: 0 });
    let ComputeNode::Map { const_strides, .. } = &mut node else {
        unreachable!()
    };
    const_strides.push(AffineStencilConstStride {
        op_position: 2,
        terms: vec![AffineStencilConstStrideTerm {
            dimension: 0,
            stride,
        }],
    });
    (
        ComputeBlock { nodes: vec![node] },
        (0..count).map(|i| Some(scalar_slot_y(i))).collect(),
        VarLayout::from_parts(Default::default(), count, count),
    )
}

// Independent dense reference: apply each source address/constant shift and
// execute the original scalar prefix in order for every domain coordinate.
fn dense(node: &ComputeNode, y: &[f64], p: &[f64]) -> Vec<f64> {
    let ComputeNode::Map {
        base_ops,
        load_strides,
        const_strides,
        ..
    } = node
    else {
        panic!("test Map")
    };
    (0..y.len())
        .map(|ordinal| {
            let mut registers = [0.0; 8];
            let mut output = None;
            for (position, op) in base_ops.iter().enumerate() {
                let address = |base: usize| {
                    let offset: isize = load_strides
                        .iter()
                        .filter(|s| s.op_position == position)
                        .flat_map(|s| &s.terms)
                        .map(|t| t.stride * ordinal as isize)
                        .sum();
                    usize::try_from(base as isize + offset).unwrap()
                };
                match *op {
                    LinearOp::LoadY { dst, index } => registers[dst as usize] = y[address(index)],
                    LinearOp::LoadP { dst, index } => registers[dst as usize] = p[address(index)],
                    LinearOp::Const { dst, value } => {
                        registers[dst as usize] = const_strides
                            .iter()
                            .filter(|s| s.op_position == position)
                            .flat_map(|s| &s.terms)
                            .fold(value, |v, t| v + t.stride * ordinal as f64);
                    }
                    LinearOp::Binary { dst, op, lhs, rhs } => {
                        let (a, b) = (registers[lhs as usize], registers[rhs as usize]);
                        registers[dst as usize] = match op {
                            BinaryOp::Mul => a * b,
                            BinaryOp::Sub => a - b,
                            _ => panic!("test binary"),
                        };
                    }
                    LinearOp::StoreOutput { src } => output = Some(registers[src as usize]),
                    _ => panic!("test scalar profile"),
                }
            }
            output.unwrap()
        })
        .collect()
}

#[test]
fn varying_value_constants_retain_full_source_and_exact_dense_values() {
    for count in [3, 350, 14_400] {
        for stride in [1.0, -2.0] {
            let (source, targets, layout) = varying(count, stride);
            let schedule = derive(&source, &targets, &layout).unwrap();
            assert_eq!(schedule.stages.len(), 1);
            let original = &source.nodes[0];
            let value = &schedule.stages[0].value_kernel.nodes[0];
            let (
                ComputeNode::Map {
                    base_ops,
                    const_strides,
                    ..
                },
                ComputeNode::Map {
                    base_ops: projected,
                    const_strides: retained,
                    ..
                },
            ) = (original, value)
            else {
                panic!("Map identity")
            };
            assert!(operations_match(&base_ops[..5], &projected[..5]));
            assert_eq!(const_strides, retained);
            let p: Vec<_> = (0..count)
                .map(|i| if i % 3 == 0 { -0.0 } else { i as f64 / 8.0 })
                .collect();
            let y = vec![7.0; count];
            let residual = dense(original, &y, &p);
            let result = dense(value, &y, &p);
            for i in 0..count {
                let expected = p[i] * (2.0 + stride * i as f64);
                assert_eq!(result[i].to_bits(), expected.to_bits());
                assert_eq!(residual[i].to_bits(), (y[i] - expected).to_bits());
            }
        }
    }
}

#[test]
fn varying_target_coefficients_and_wrapped_targets_still_refuse() {
    let (mut source, targets, layout) = varying(3, 1.0);
    let ComputeNode::Map { base_ops, .. } = &mut source.nodes[0] else {
        panic!("Map")
    };
    base_ops[3] = LinearOp::Binary {
        dst: 3,
        op: BinaryOp::Mul,
        lhs: 0,
        rhs: 2,
    };
    base_ops[4] = LinearOp::Binary {
        dst: 4,
        op: BinaryOp::Sub,
        lhs: 3,
        rhs: 1,
    };
    assert!(derive(&source, &targets, &layout).is_err());
    let (mut source, targets, layout) = varying(3, 1.0);
    let ComputeNode::Map { base_ops, .. } = &mut source.nodes[0] else {
        panic!("Map")
    };
    base_ops.insert(
        5,
        LinearOp::Unary {
            dst: 5,
            op: crate::UnaryOp::Neg,
            arg: 4,
        },
    );
    base_ops[6] = LinearOp::StoreOutput { src: 5 };
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native assignment varying constants require an unsupported coefficient proof"
    );
}

#[test]
fn varying_values_do_not_allow_any_cross_target_affine_read() {
    let (mut source, targets, layout) = varying(3, 1.0);
    let ComputeNode::Map {
        base_ops,
        load_strides,
        ..
    } = &mut source.nodes[0]
    else {
        panic!("Map")
    };
    base_ops[1] = LinearOp::LoadY { dst: 1, index: 2 };
    load_strides[1].terms[0].stride = -1;
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native family reads coupled targets in its own assignment range"
    );
}

#[test]
fn varying_unused_prefix_keeps_bare_zero_and_reversed_subtraction_profiles() {
    for zero in [false, true] {
        let (mut source, targets, layout) = varying(350, 1.0);
        let ComputeNode::Map { base_ops, .. } = &mut source.nodes[0] else {
            panic!("Map")
        };
        if zero {
            base_ops[5] = LinearOp::StoreOutput { src: 0 };
        } else {
            base_ops[4] = LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Sub,
                lhs: 3,
                rhs: 0,
            };
        }
        let schedule = derive(&source, &targets, &layout).unwrap();
        let result = dense(
            &schedule.stages[0].value_kernel.nodes[0],
            &vec![7.0; 350],
            &vec![0.25; 350],
        );
        for (i, result) in result.into_iter().enumerate() {
            let expected: f64 = if zero { 0.0 } else { 0.25 * (2.0 + i as f64) };
            assert_eq!(result.to_bits(), expected.to_bits());
        }
    }
}
