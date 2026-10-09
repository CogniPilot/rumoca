use super::*;
use crate::{BinaryOp, Reg, TensorInputKind};

fn ranged(count: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::P,
            input_start: 0,
            count,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: count as Reg,
            input: TensorInputKind::Y,
            input_start: 0,
            count,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: (count * 2) as Reg,
            op: BinaryOp::Sub,
            lhs_start: count as Reg,
            rhs_start: 0,
            count,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: (count * 2) as Reg,
            count,
            stride: 1,
        },
    ]
}

fn compare(source: &[LinearOp], requests: &[(usize, usize)]) {
    let mut cache = CanonicalAssignmentQueries::new(source);
    for &(output, target) in requests {
        assert_eq!(
            cache.derive(output, target),
            super::super::canonical_assignment_shape_for_output(source, output, target),
            "output{output}/target{target}",
        );
    }
}

#[test]
fn lazy_queries_preserve_additive_reciprocal_and_singular_refusal() {
    for reciprocal in [false, true] {
        for numerator in [0.0, 2.0] {
            let source = vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::Const {
                    dst: 1,
                    value: numerator,
                },
                LinearOp::LoadY { dst: 2, index: 5 },
                LinearOp::Binary {
                    dst: 3,
                    op: if reciprocal {
                        BinaryOp::Div
                    } else {
                        BinaryOp::Add
                    },
                    lhs: if reciprocal { 1 } else { 2 },
                    rhs: 2,
                },
                LinearOp::Binary {
                    dst: 4,
                    op: BinaryOp::Sub,
                    lhs: 0,
                    rhs: 3,
                },
                LinearOp::StoreOutput { src: 4 },
            ];
            let expected = crate::derive_target_assignment_shapes(&source);
            let mut query = CanonicalAssignmentQueries::new(&source);
            assert_eq!(query.has_any(0), !expected.is_empty());
            let shape = query.derive(0, 5);
            match (reciprocal, numerator) {
                (false, _) => assert!(matches!(
                    shape,
                    Some(TargetAssignmentShape::Additive { .. })
                )),
                (true, 0.0) => assert_eq!(shape, None),
                (true, _) => assert!(matches!(
                    shape,
                    Some(TargetAssignmentShape::Reciprocal { .. })
                )),
            }
            assert_eq!(query.derive(0, 9), None);
            assert_eq!(query.has_any(0), !expected.is_empty());
            compare(&source, &[(0, 5), (0, 9), (0, 5), (1, 5)]);
        }
    }
}

#[test]
fn ranged_full14400_certificate_queries_share_one_exact_prefix() {
    let source = ranged(14400);
    let mut cache = CanonicalAssignmentQueries::new(&source);
    assert_eq!(
        cache.stores.as_ref().unwrap().store_count(),
        1,
        "range metadata remains compact"
    );
    for target in 0..14400 {
        let shape = cache
            .derive(target, target)
            .expect("source-owned direct target");
        assert_eq!(shape.target_y_index(), target);
        assert!(matches!(shape, TargetAssignmentShape::Direct { .. }));
        assert!(cache.has_any(target));
        assert_eq!(cache.any_shape, Some((target, true)));
    }
    assert_eq!(cache.prefix_builds, 1);
    assert!(!cache.has_any(14400));
    assert_eq!(cache.derive(14400, 0), None);
    assert_eq!(cache.prefix_builds, 1);
    compare(
        &source,
        &[(0, 0), (1, 1), (14399, 14399), (2, 14399), (14400, 0)],
    );
}

#[test]
fn prefix_changes_overwrites_and_distinct_sources_keep_original_refusals() {
    let mut source = ranged(3);
    source.push(LinearOp::Const { dst: 9, value: 4.0 });
    source.push(LinearOp::StoreOutput { src: 9 });
    source.push(LinearOp::Const { dst: 3, value: 8.0 });
    source.push(LinearOp::StoreOutput { src: 6 });
    compare(&source, &[(0, 0), (1, 1), (3, 0), (4, 0), (0, 0), (2, 2)]);
    let other = ranged(5);
    compare(&other, &[(0, 0), (4, 4), (3, 1), (0, 0)]);
    let expected = crate::derive_target_assignment_shapes(&source);
    let mut query = CanonicalAssignmentQueries::new(&source);
    for output in [0, 3, 4, 0, 2, 99] {
        assert_eq!(
            query.has_any(output),
            expected.iter().any(|(offset, _)| *offset == output)
        );
    }
    let mut cache = CanonicalAssignmentQueries::new(&source);
    cache.derive(0, 0);
    cache.derive(3, 0);
    cache.derive(4, 0);
    assert_eq!(cache.prefix_builds, 3);
    assert_eq!(
        cache.prefix.as_ref().map(|(position, _)| *position),
        Some(source.len() - 1)
    );
}

#[test]
fn compact_output_projection_preserves_zero_stride_empty_and_overflow_skips() {
    let source = vec![
        LinearOp::Const { dst: 1, value: 2.0 },
        LinearOp::StoreOutputRange {
            start: 1,
            count: 0,
            stride: 1,
        },
        LinearOp::StoreOutputRange {
            start: 1,
            count: 3,
            stride: 0,
        },
        LinearOp::StoreOutputRange {
            start: Reg::MAX,
            count: 3,
            stride: 1,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let stores = ScalarProgramOutputStores::new(&source).unwrap();
    let expected = super::super::store_output_registers(&source).collect::<Vec<_>>();
    let actual = (0..expected.len())
        .map(|offset| stores.output(offset).unwrap())
        .collect::<Vec<_>>();
    assert_eq!(actual, expected);
    compare(&source, &[(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0)]);
}

#[test]
fn changing_target_does_not_reuse_a_shape_and_uninitialized_dependencies_fail_closed() {
    let mut source = ranged(3);
    source.insert(0, LinearOp::Move { dst: 20, src: 21 });
    compare(&source, &[(0, 0), (0, 1), (2, 2), (2, 0)]);
    let source = ranged(3);
    compare(&source, &[(0, 0), (0, 1), (0, 0), (2, 2), (1, 2)]);
}

#[test]
fn overflow_in_optional_store_index_keeps_original_early_prefix_answer() {
    let source = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutputRange {
            start: 0,
            count: usize::MAX,
            stride: 0,
        },
    ];
    assert!(ScalarProgramOutputStores::new(&source).is_none());
    compare(&source, &[(0, 0), (1, 0)]);
    let source = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 2,
            stride: usize::MAX,
        },
        LinearOp::StoreOutput { src: 0 },
    ];
    compare(&source, &[(0, 0), (1, 0), (2, 0)]);
}

fn refresh_row(node: usize, target: usize) -> crate::AlgebraicRefreshRow {
    crate::AlgebraicRefreshRow {
        owner_id: crate::RefreshRowOwnerId::checked(0).unwrap(),
        source: crate::RefreshScalarProgramSource::checked(node, 0).unwrap(),
        equation_index: 0,
        output_offset: 0,
        target_index: target,
        assignment_target: Some(target),
        assignment_shape: None,
        direct_assignment_certified: false,
        exact_assignment_certified: false,
    }
}

#[test]
fn source_program_switches_and_invalid_output_preserve_owner_and_first_diagnostic() {
    use crate::refresh::{source_outputs::SourceOutputs, validate_refresh_sources};
    let a = ranged(3);
    let mut b = ranged(1);
    let LinearOp::TensorLoad { input_start, .. } = &mut b[1] else {
        panic!("Y load");
    };
    *input_start = 3;
    b.insert(0, LinearOp::LoadSeed { dst: 20, index: 0 });
    let span =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(file!()), 0, 1);
    let source = crate::ComputeBlock {
        nodes: vec![a.clone(), b.clone()]
            .into_iter()
            .map(|program| {
                crate::ComputeNode::ScalarPrograms(
                    crate::ScalarProgramBlock::with_program_spans(vec![program], vec![span])
                        .unwrap(),
                )
            })
            .collect(),
    };
    let mut cache = SourceCertificateQueries::new(&source);
    for (node, target, program) in [(0, 0, &a), (1, 3, &b), (0, 1, &a)] {
        let row = refresh_row(node, target);
        let (shape, causal) = cache.derive(&row, "query").unwrap();
        assert_eq!(
            shape,
            super::super::canonical_assignment_shape_for_output(program, 0, target)
        );
        assert_eq!(causal, node == 0);
    }
    let mut wrong = refresh_row(0, 0);
    wrong.output_offset = 3;
    assert_eq!(cache.derive(&wrong, "query").unwrap().0, None);
    let missing = refresh_row(9, 0);
    assert_eq!(
        cache.derive(&missing, "query").unwrap_err().reason,
        "query refresh row refers to a missing canonical source program"
    );
    let outputs = SourceOutputs::new(&source).unwrap();
    let plan = crate::RefreshPlan {
        rows: vec![wrong],
        ..crate::RefreshPlan::default()
    };
    assert_eq!(
        validate_refresh_sources("query", &plan, &outputs, &mut cache)
            .unwrap_err()
            .reason,
        "query refresh row refers to a missing canonical scalar-program output"
    );
}
