//! Construction obligations and exact demand-materialized column views.
use super::*;

fn full(rows: usize, columns: usize) -> StructuralPattern {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("lazy_column_cache.mo"),
        1,
        2,
    );
    StructuralPattern::full(
        rows,
        columns,
        PatternProvenance::derived(PatternDerivation::ConservativeFull, span).unwrap(),
    )
    .unwrap()
}

fn affine_node(points: usize, start: usize) -> ComputeNode {
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            provenance: rumoca_core::IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: points as i64,
            step: 1,
        }],
    };
    ComputeNode::Map {
        output_map: TensorOutputMap::dense_contiguous(start, &domain).unwrap(),
        domain,
        base_ops: vec![
            LinearOp::LoadSeed { dst: 0, index: 0 },
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
        span: rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("lazy_affine_coloring.mo"),
            1,
            2,
        ),
    }
}

fn compact_patterns(points: usize, columns: usize) -> [StructuralPattern; 2] {
    let first = affine_node(points, 0);
    let span = match &first {
        ComputeNode::Map { span, .. } => *span,
        _ => unreachable!("fixture creates a map"),
    };
    let affine = StructuralPattern::derive_from_compute_jvp(
        &ComputeBlock {
            nodes: vec![first.clone()],
        },
        points,
        columns,
        span,
    )
    .unwrap();
    let stacked = StructuralPattern::derive_from_compute_jvp(
        &ComputeBlock {
            nodes: vec![first, affine_node(points, points)],
        },
        2 * points,
        columns,
        span,
    )
    .unwrap();
    [affine, stacked]
}

#[test]
fn huge_compact_patterns_keep_global_structures_uncolored() {
    let [affine, stacked] = compact_patterns(8, 1 << 28);
    assert!(matches!(
        affine.view(),
        StructuralPatternView::Affine { .. }
    ));
    assert!(matches!(
        stacked.view(),
        StructuralPatternView::Stacked { .. }
    ));
    for pattern in [affine, stacked] {
        let structure = JacobianStructure::derived(pattern.clone());
        assert_eq!(structure.pattern().columns(), 1 << 28);
        assert!(structure.column_rows.get().is_none());
        assert!(structure.clone().column_rows.get().is_none());
        let continuous = ContinuousStructuralArtifacts::derived(
            Some(pattern.clone()),
            vec![],
            vec![],
            Some(pattern.clone()),
            vec![],
            Some(pattern.clone()),
        );
        let initialization =
            InitializationStructuralArtifacts::derived(Some(pattern.clone()), vec![]);
        assert_eq!(continuous.implicit().unwrap().pattern(), &pattern);
        assert_eq!(initialization.residual().unwrap().pattern(), &pattern);
        assert_eq!(pattern.coloring_derivation_count(), 0);
    }
}

#[test]
fn projection_construction_issues_coloring_once_for_upstream_clones() {
    let [affine, stacked] = compact_patterns(3, 7);
    for pattern in [affine, stacked, full(3, 5), full(0, 4)] {
        let cold = pattern.clone();
        assert_eq!(pattern.coloring_derivation_count(), 0);
        let first = ColoredJacobianStructure::derived(pattern.clone());
        assert_eq!(pattern.coloring_derivation_count(), 1);
        let second = ColoredJacobianStructure::derived(cold);
        let warm = ColoredJacobianStructure::derived(pattern.clone());
        let cloned = first.clone();
        for structure in [&first, &second, &warm, &cloned] {
            assert!(std::ptr::eq(first.coloring(), structure.coloring()));
            assert_eq!(structure.pattern(), &pattern);
            assert!(structure.structure.column_rows.get().is_none());
        }
        assert_eq!(pattern.coloring_derivation_count(), 1);
    }
}

#[test]
fn actual_artifact_roles_issue_only_required_coloring() {
    let global = full(3, 5);
    let projection = full(1, 1);
    let continuous = ContinuousStructuralArtifacts::derived(
        Some(global.clone()),
        vec![projection.clone(); 2],
        vec![false; 2],
        Some(global.clone()),
        vec![projection.clone()],
        Some(global.clone()),
    );
    let initialization =
        InitializationStructuralArtifacts::derived(Some(global.clone()), vec![projection.clone()]);
    let first = &continuous.algebraic_projection()[0];
    assert_eq!(first.coloring().groups(), &[Box::from([0])]);
    assert!(std::ptr::eq(
        first.coloring(),
        continuous.algebraic_projection()[1].coloring()
    ));
    assert!(std::ptr::eq(
        first.coloring(),
        continuous.manifold_projection()[0].coloring()
    ));
    assert!(std::ptr::eq(
        first.coloring(),
        initialization.projection()[0].coloring()
    ));
    assert_eq!(projection.coloring_derivation_count(), 1);
    assert_eq!(global.coloring_derivation_count(), 0);
}

#[test]
fn independent_construction_and_checked_replay_own_distinct_colorings() {
    let original = full(3, 5);
    let cold_wire = serde_json::to_value(&original).unwrap();
    let first = ColoredJacobianStructure::derived(original.clone());
    let independent = full(3, 5);
    assert_eq!(original, independent);
    assert_eq!(independent.coloring_derivation_count(), 0);
    let second = ColoredJacobianStructure::derived(independent.clone());
    let replayed: StructuralPattern = serde_json::from_value(cold_wire.clone()).unwrap();
    assert_eq!(replayed, original);
    assert_eq!(replayed.coloring_derivation_count(), 0);
    let replay = ColoredJacobianStructure::derived(replayed.clone());
    assert_eq!(first.coloring(), second.coloring());
    assert_eq!(first.coloring(), replay.coloring());
    assert!(!std::ptr::eq(first.coloring(), second.coloring()));
    assert!(!std::ptr::eq(first.coloring(), replay.coloring()));
    assert_eq!(serde_json::to_value(&original).unwrap(), cold_wire);
    for pattern in [&original, &independent, &replayed] {
        assert_eq!(pattern.coloring_derivation_count(), 1);
    }
    let mut forged = cold_wire;
    forged["coloring"] = serde_json::json!([[0, 1, 2, 3, 4]]);
    assert!(serde_json::from_value::<StructuralPattern>(forged).is_err());
}

#[test]
fn checked_projection_and_union_issue_fresh_coloring_owners() {
    let original = full(3, 5);
    let first = ColoredJacobianStructure::derived(original.clone());
    let projected = original.project(&[0, 1, 2], &[0, 1, 2, 3, 4]).unwrap();
    let union = original.union(&original).unwrap();
    for pattern in [projected, union] {
        assert_eq!(pattern, original);
        assert_eq!(pattern.coloring_derivation_count(), 0);
        let issued = ColoredJacobianStructure::derived(pattern.clone());
        let repeated = ColoredJacobianStructure::derived(pattern.clone());
        assert_eq!(issued.coloring(), first.coloring());
        assert!(!std::ptr::eq(issued.coloring(), first.coloring()));
        assert!(std::ptr::eq(issued.coloring(), repeated.coloring()));
        assert_eq!(pattern.coloring_derivation_count(), 1);
    }
    assert_eq!(original.coloring_derivation_count(), 1);
}

#[test]
fn cold_pattern_clones_construct_one_coloring_concurrently() {
    let pattern = compact_patterns(3, 7).into_iter().nth(1).unwrap();
    let start = std::sync::Arc::new(std::sync::Barrier::new(8));
    let mut tasks = Vec::new();
    for _ in 0..8 {
        let cold = pattern.clone();
        let start = std::sync::Arc::clone(&start);
        tasks.push(std::thread::spawn(move || {
            start.wait();
            ColoredJacobianStructure::derived(cold)
        }));
    }
    let structures: Vec<_> = tasks.into_iter().map(|task| task.join().unwrap()).collect();
    for structure in &structures {
        assert!(std::ptr::eq(structures[0].coloring(), structure.coloring()));
        assert_eq!(
            structure.coloring().groups(),
            &[Box::from([0, 1, 2, 3, 4, 5, 6])]
        );
    }
    assert_eq!(pattern.coloring_derivation_count(), 1);
}

#[test]
fn unused_full_structure_has_no_column_coordinate_cache() {
    let pattern = full(128, 192);
    let structure = JacobianStructure::derived(pattern.clone());
    assert!(structure.column_rows.get().is_none());
    assert_eq!(structure.pattern().rows(), 128);
    assert_eq!(structure.pattern().columns(), 192);
    assert_eq!(pattern.coloring_derivation_count(), 0);
}

#[test]
fn full_cache_keeps_exact_rectangular_rows_and_stable_borrow() {
    let structure = JacobianStructure::derived(full(3, 5));
    let rows = structure.column_rows();
    assert_eq!(rows, vec![vec![0, 1, 2]; 5]);
    let pointer = rows.as_ptr();
    assert_eq!(pointer, structure.column_rows().as_ptr());
    assert_eq!(structure.column_rows.get().unwrap().len(), 5);
    assert_eq!(structure.pattern().coloring_derivation_count(), 0);
}

#[test]
fn empty_full_columns_retain_the_source_dimension() {
    let structure = JacobianStructure::derived(full(0, 4));
    assert!(structure.column_rows.get().is_none());
    assert_eq!(structure.column_rows(), vec![Vec::<usize>::new(); 4]);
    let no_columns = JacobianStructure::derived(full(3, 0));
    assert!(no_columns.column_rows().is_empty());
}

#[test]
fn clone_preserves_column_cache_lifetimes_and_shares_issued_coloring() {
    let source = ColoredJacobianStructure::derived(full(3, 5));
    let cold = source.clone();
    assert!(source.structure.column_rows.get().is_none());
    assert!(cold.structure.column_rows.get().is_none());
    let source_rows = source.column_rows();
    let source_coloring = source.coloring();
    assert!(cold.structure.column_rows.get().is_none());
    let warm = source.clone();
    assert!(warm.structure.column_rows.get().is_some());
    assert!(std::ptr::eq(source_coloring, warm.coloring()));
    assert!(std::ptr::eq(source_coloring, cold.coloring()));
    assert_eq!(source_rows, warm.column_rows());
    assert_ne!(source_rows.as_ptr(), warm.column_rows().as_ptr());
    assert_ne!(source_rows[0].as_ptr(), warm.column_rows()[0].as_ptr());
    assert_eq!(source_rows, cold.column_rows());
    assert_ne!(source_rows.as_ptr(), cold.column_rows().as_ptr());
    assert_eq!(source.pattern().coloring_derivation_count(), 1);
}

#[test]
fn simultaneous_consumers_only_borrow_the_issued_coloring() {
    let structure = std::sync::Arc::new(ColoredJacobianStructure::derived(full(3, 5)));
    let mut tasks = Vec::new();
    for _ in 0..8 {
        let owner = std::sync::Arc::clone(&structure);
        tasks.push(std::thread::spawn(move || {
            let rows = owner.column_rows();
            assert_eq!(rows, vec![vec![0, 1, 2]; 5]);
            let coloring = owner.coloring();
            assert_eq!(coloring.column_count(), 5);
            assert_eq!(
                coloring.groups(),
                &[
                    Box::from([0]),
                    Box::from([1]),
                    Box::from([2]),
                    Box::from([3]),
                    Box::from([4])
                ]
            );
            (
                rows.as_ptr() as usize,
                std::ptr::from_ref(coloring) as usize,
            )
        }));
    }
    let addresses: Vec<_> = tasks.into_iter().map(|task| task.join().unwrap()).collect();
    assert!(addresses.iter().all(|address| *address == addresses[0]));
    assert!(structure.structure.column_rows.get().is_some());
    assert_eq!(structure.pattern().coloring_derivation_count(), 1);
}
