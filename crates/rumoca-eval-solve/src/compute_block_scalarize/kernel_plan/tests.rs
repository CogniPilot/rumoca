use super::*;
use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
use rumoca_ir_solve::{AffineStencilConstStrideTerm, AffineStencilIndexStrideTerm};

fn span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 0, 1)
}

fn domain(binders: &[(i64, i64, i64)]) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: binders
            .iter()
            .enumerate()
            .map(|(id, &(lower, upper, step))| StructuredIndexBinder {
                id,
                display_name: format!("b{id}"),
                lower,
                upper,
                step,
            })
            .collect(),
    }
}

fn load(op_position: usize, terms: &[(usize, isize)]) -> AffineStencilLoadStride {
    AffineStencilLoadStride {
        op_position,
        terms: terms
            .iter()
            .map(|&(dimension, stride)| AffineStencilIndexStrideTerm { dimension, stride })
            .collect(),
    }
}

fn plan(
    domain: &StructuredIndexDomain,
    base_ops: &[LinearOp],
    loads: &[AffineStencilLoadStride],
    consts: &[AffineStencilConstStride],
) -> Result<AffineKernelPlan, ScalarizeError> {
    AffineKernelPlan::new(AffineKernelNode {
        domain,
        output_map: None,
        base_ops,
        load_strides: loads,
        const_strides: consts,
        kind: "map",
        span: span(),
    })
}

#[test]
fn split_strides_of_one_load_combine_per_binder() {
    let domain = domain(&[(1, 3, 1), (5, 1, -2)]);
    let base_ops = [
        LinearOp::LoadY { dst: 0, index: 4 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let plan = plan(
        &domain,
        &base_ops,
        &[load(0, &[(0, 3)]), load(0, &[(1, 1), (0, 2)])],
        &[],
    )
    .expect("valid plan");
    assert_eq!(plan.extents(), [3, 3]);
    assert_eq!(plan.loads()[0].strides(), [5, 1]);
    assert_eq!(plan.loads()[0].max_index(), 4 + 2 * 5 + 2);
    let mut indices = Vec::new();
    plan.for_each_point(|ordinals| indices.push(plan.loads()[0].index_at(ordinals)));
    assert_eq!(indices, [4, 5, 6, 9, 10, 11, 14, 15, 16]);
}

#[test]
fn a_load_below_zero_anywhere_in_the_domain_is_rejected_at_construction() {
    let domain = domain(&[(1, 4, 1)]);
    let base_ops = [
        LinearOp::LoadY { dst: 0, index: 2 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let error = plan(&domain, &base_ops, &[load(0, &[(0, -1)])], &[])
        .expect_err("the last point reads y[-1]");
    assert!(matches!(
        error,
        ScalarizeError::NegativeLoadIndex { value: -1, .. }
    ));
}

#[test]
fn strided_constants_follow_the_scalar_view_operation_order() {
    let domain = domain(&[(1, 2, 1), (1, 3, 1)]);
    let base_ops = [
        LinearOp::Const {
            dst: 0,
            value: -0.0,
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    let consts = [AffineStencilConstStride {
        op_position: 0,
        terms: vec![
            AffineStencilConstStrideTerm {
                dimension: 1,
                stride: 0.1,
            },
            AffineStencilConstStrideTerm {
                dimension: 0,
                stride: 0.0,
            },
            AffineStencilConstStrideTerm {
                dimension: 1,
                stride: 0.2,
            },
        ],
    }];
    let plan = plan(&domain, &base_ops, &[], &consts).expect("valid plan");
    let mut values = Vec::new();
    plan.for_each_point(|ordinals| values.push(plan.consts()[0].value_at(ordinals)));
    let stride = 0.1f64 + 0.2;
    let expected = [0usize, 0, 0, 1, 1, 1]
        .iter()
        .zip([0usize, 1, 2, 0, 1, 2])
        .map(|(&row, column)| -0.0 + row as f64 * 0.0 + column as f64 * stride)
        .collect::<Vec<_>>();
    assert_eq!(
        values
            .iter()
            .map(|value| value.to_bits())
            .collect::<Vec<_>>(),
        expected
            .iter()
            .map(|value| value.to_bits())
            .collect::<Vec<_>>()
    );
    // `-0.0 + 0.0` is `+0.0`: the plan keeps every binder's term.
    assert_eq!(values[0].to_bits(), 0.0f64.to_bits());
}

#[test]
fn a_constant_that_overflows_at_a_corner_is_rejected_at_construction() {
    let domain = domain(&[(1, 3, 1)]);
    let base_ops = [
        LinearOp::Const {
            dst: 0,
            value: f64::MAX,
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    let consts = [AffineStencilConstStride {
        op_position: 0,
        terms: vec![AffineStencilConstStrideTerm {
            dimension: 0,
            stride: f64::MAX,
        }],
    }];
    assert!(plan(&domain, &base_ops, &[], &consts).is_err());
}

#[test]
fn output_indices_match_the_tensor_output_map() {
    let domain = domain(&[(2, 4, 1), (2, 6, 2)]);
    let map = TensorOutputMap {
        start: 11,
        strides: vec![
            AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: 8,
            },
            AffineStencilIndexStrideTerm {
                dimension: 1,
                stride: 2,
            },
        ],
    };
    let base_ops = [
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let plan = AffineKernelPlan::new(AffineKernelNode {
        domain: &domain,
        output_map: Some(&map),
        base_ops: &base_ops,
        load_strides: &[],
        const_strides: &[],
        kind: "map",
        span: span(),
    })
    .expect("valid plan");
    let mut indices = Vec::new();
    plan.for_each_point(|ordinals| indices.push(plan.output_index_at(ordinals)));
    assert_eq!(indices, map.output_indices(&domain).expect("map indices"));
    assert_eq!(plan.output_count(), 11 + 16 + 4 + 1);
}
