use super::*;
use rumoca_core::{SourceId, StructuredIndexBinder};
use rumoca_ir_solve::BinaryOp;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name(file!()), 0, 1)
}

fn binder(id: usize, lower: i64, upper: i64, step: i64) -> StructuredIndexBinder {
    StructuredIndexBinder {
        provenance: rumoca_core::IndexBinderProvenance::Generated,
        id,
        display_name: format!("i{id}"),
        lower,
        upper,
        step,
    }
}

fn domain(binders: Vec<StructuredIndexBinder>) -> StructuredIndexDomain {
    StructuredIndexDomain { binders }
}

fn stride(op_position: usize, dimension: usize, stride: isize) -> AffineStencilLoadStride {
    AffineStencilLoadStride {
        op_position,
        terms: vec![AffineStencilIndexStrideTerm { dimension, stride }],
    }
}

fn output(start: usize, stride: isize) -> TensorOutputMap {
    TensorOutputMap {
        start,
        strides: vec![AffineStencilIndexStrideTerm {
            dimension: 0,
            stride,
        }],
    }
}

/// `out[k] = y[a + sa·k] * y[b + sb·k]`, one binder.
fn product(a: usize, b: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: a },
        LinearOp::LoadY { dst: 1, index: b },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

fn reason(result: Result<CompactNode<'_>, ModelStateJacobianError>) -> &'static str {
    match result {
        Err(ModelStateJacobianError::Unsupported { reason, .. }) => reason,
        other => panic!("expected a named refusal, got {other:?}"),
    }
}

#[test]
fn neighbouring_loads_admit_with_one_seed_each() {
    let ops = product(0, 1);
    let node = admit(
        &domain(vec![binder(0, 1, 4, 1)]),
        &output(0, 1),
        &ops,
        &[stride(0, 0, 1), stride(1, 0, 1)],
        5,
        span(),
    )
    .expect("y[k]·y[k+1] never meet");
    assert_eq!(node.extents, vec![4]);
    assert_eq!(node.seeds.len(), 2);
}

#[test]
fn equal_affine_loads_share_one_seed() {
    let ops = product(2, 2);
    let node = admit(
        &domain(vec![binder(0, 1, 3, 1)]),
        &output(0, 1),
        &ops,
        &[stride(0, 0, 1), stride(1, 0, 1)],
        5,
        span(),
    )
    .expect("y[k+2]·y[k+2] is one variable");
    assert_eq!(node.seeds.len(), 1);
}

#[test]
fn loads_that_meet_at_a_point_refuse() {
    // y[k] and y[2k] both read y[0] at k = 0.
    let ops = product(0, 0);
    let refused = admit(
        &domain(vec![binder(0, 1, 3, 1)]),
        &output(0, 1),
        &ops,
        &[stride(0, 0, 1), stride(1, 0, 2)],
        8,
        span(),
    );
    assert_eq!(reason(refused), "affine loads alias a state column");
    // y[k] and y[4 − k] meet at k = 2.
    let ops = product(0, 4);
    let refused = admit(
        &domain(vec![binder(0, 1, 5, 1)]),
        &output(0, 1),
        &ops,
        &[stride(0, 0, 1), stride(1, 0, -1)],
        8,
        span(),
    );
    assert_eq!(reason(refused), "affine loads alias a state column");
}

#[test]
fn load_leaving_the_state_vector_refuses() {
    let ops = product(0, 1);
    let refused = admit(
        &domain(vec![binder(0, 1, 5, 1)]),
        &output(0, 1),
        &ops,
        &[stride(0, 0, 1), stride(1, 0, 1)],
        5,
        span(),
    );
    assert_eq!(
        reason(refused),
        "compact load index leaves the state vector"
    );
}

#[test]
fn extreme_source_bounds_are_counted_in_ordinals() {
    // Three points over signed-64 extremes: the proof sees only ordinals 0..3.
    let ops = product(0, 1);
    let node = admit(
        &domain(vec![binder(0, i64::MIN, i64::MAX, i64::MAX)]),
        &output(0, 1),
        &ops,
        &[stride(0, 0, 2), stride(1, 0, 2)],
        6,
        span(),
    )
    .expect("three ordinal points fit the state vector");
    assert_eq!(node.extents, vec![3]);
}

#[test]
fn huge_stride_refuses_without_overflow() {
    let ops = product(0, 1);
    let refused = admit(
        &domain(vec![binder(0, 1, 3, 1)]),
        &output(0, 1),
        &ops,
        &[stride(0, 0, isize::MAX), stride(1, 0, 1)],
        8,
        span(),
    );
    assert_eq!(reason(refused), "compact load index exceeds i64");
}

#[test]
fn output_placement_outside_the_state_domain_refuses() {
    let ops = product(0, 1);
    let refused = admit(
        &domain(vec![binder(0, 1, 3, 1)]),
        &output(3, 1),
        &ops,
        &[stride(0, 0, 1), stride(1, 0, 1)],
        5,
        span(),
    );
    assert_eq!(
        reason(refused),
        "compact output placement leaves the state domain"
    );
}

#[test]
fn empty_domain_refuses() {
    let ops = product(0, 1);
    let refused = admit(
        &domain(vec![binder(0, 3, 1, 1)]),
        &output(0, 1),
        &ops,
        &[],
        5,
        span(),
    );
    assert_eq!(reason(refused), "compact domain is empty");
}

#[test]
fn two_dimensional_alias_check_is_conservative_but_sound() {
    // Strides (1, 3) vs (3, 1) over 3×3: base 0 vs base 0 meet at (0, 0).
    let ops = product(0, 0);
    let strides = [
        AffineStencilLoadStride {
            op_position: 0,
            terms: vec![
                AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1,
                },
                AffineStencilIndexStrideTerm {
                    dimension: 1,
                    stride: 3,
                },
            ],
        },
        AffineStencilLoadStride {
            op_position: 1,
            terms: vec![
                AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 3,
                },
                AffineStencilIndexStrideTerm {
                    dimension: 1,
                    stride: 1,
                },
            ],
        },
    ];
    let refused = admit(
        &domain(vec![binder(0, 1, 3, 1), binder(1, 1, 3, 1)]),
        &TensorOutputMap {
            start: 0,
            strides: vec![
                AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 3,
                },
                AffineStencilIndexStrideTerm {
                    dimension: 1,
                    stride: 1,
                },
            ],
        },
        &ops,
        &strides,
        9,
        span(),
    );
    assert_eq!(reason(refused), "affine loads alias a state column");
}
