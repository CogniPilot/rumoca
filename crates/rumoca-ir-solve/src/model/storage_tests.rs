use super::*;

#[test]
fn jacobian_structure_layout_measurement() {
    assert!(
        std::mem::size_of::<Option<Box<ProjectionJacobianApplication>>>()
            < std::mem::size_of::<Option<ProjectionJacobianApplication>>()
    );
    assert!(
        std::mem::size_of::<Option<Box<AffineEliminationLayout>>>()
            < std::mem::size_of::<Option<AffineEliminationLayout>>()
    );
    for (name, size) in [
        (
            "JacobianStructure",
            std::mem::size_of::<JacobianStructure>(),
        ),
        (
            "StructuralPattern",
            std::mem::size_of::<StructuralPattern>(),
        ),
        ("ColumnColoring", std::mem::size_of::<ColumnColoring>()),
        (
            "ProjectionOutputSelection",
            std::mem::size_of::<ProjectionOutputSelection>(),
        ),
        (
            "ProjectionJacobianApplication",
            std::mem::size_of::<ProjectionJacobianApplication>(),
        ),
        (
            "AffineEliminationLayout",
            std::mem::size_of::<AffineEliminationLayout>(),
        ),
    ] {
        println!("{name}: {size}");
    }
}

#[test]
#[cfg(target_pointer_width = "64")]
fn absent_checked_payloads_do_not_inflate_projection_metadata() {
    const BLOCKS: usize = 2_574_602;
    let bytes = BLOCKS * std::mem::size_of::<JacobianStructure>();
    assert!(
        bytes <= BLOCKS * 32 * std::mem::size_of::<usize>(),
        "{BLOCKS} structures require {bytes} bytes"
    );
}

fn pattern(rows: usize, columns: usize) -> StructuralPattern {
    StructuralPattern::full(
        rows,
        columns,
        PatternProvenance::derived(
            PatternDerivation::ConservativeFull,
            rumoca_core::Span::from_offsets(
                rumoca_core::SourceId::from_source_name("storage.mo"),
                1,
                2,
            ),
        )
        .unwrap(),
    )
    .unwrap()
}

#[test]
fn large_cold_projection_cohort_retains_exact_patterns_and_colorings() {
    const BLOCKS: usize = 10_000;
    let source = pattern(1, 1);
    let artifacts = ContinuousStructuralArtifacts::derived(
        None,
        vec![source.clone(); BLOCKS],
        vec![false; BLOCKS],
        None,
        vec![],
        None,
    );
    assert_eq!(artifacts.algebraic_projection().len(), BLOCKS);
    for structure in artifacts.algebraic_projection() {
        assert_eq!(structure.pattern(), &source);
        assert_eq!(structure.coloring().groups(), &[Box::from([0])]);
        assert!(structure.column_rows.get().is_none());
        assert!(structure.compact.get().is_none());
        assert!(structure.jacobian_application().is_none());
        assert!(structure.affine_elimination().is_none());
    }
    let cloned = artifacts.clone();
    assert_ne!(
        artifacts.algebraic_projection().as_ptr(),
        cloned.algebraic_projection().as_ptr()
    );
    let initialized =
        InitializationStructuralArtifacts::derived(Some(source.clone()), vec![source]);
    assert_eq!(
        initialized.residual().unwrap().pattern(),
        cloned.algebraic_projection()[0].pattern()
    );
    assert_eq!(
        initialized.projection()[0].coloring(),
        cloned.algebraic_projection()[0].coloring()
    );
}

fn bound_application() -> ContinuousStructuralArtifacts {
    let span = pattern(1, 1).provenance().span();
    let source = ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::LoadSeed { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        span.require_provenance("storage test").unwrap(),
    )
    .unwrap();
    let plan = AlgebraicProjectionPlan {
        blocks: vec![AlgebraicProjectionBlock {
            rows: vec![0],
            y_indices: vec![0],
            tearing: None,
            alternate_charts: vec![],
        }],
    };
    ContinuousStructuralArtifacts::derived(
        None,
        vec![pattern(1, 1)],
        vec![false],
        None,
        vec![],
        None,
    )
    .with_algebraic_output_evaluations(&plan, &source, &source, &source)
}

#[test]
fn boxed_application_clone_keeps_owned_payload_and_exact_source_binding() {
    let mut artifacts = bound_application();
    let cloned = artifacts.clone();
    let before = artifacts.algebraic_projection()[0]
        .jacobian_application()
        .unwrap();
    let after = cloned.algebraic_projection()[0]
        .jacobian_application()
        .unwrap();
    assert!(!std::ptr::eq(before, after));
    assert_ne!(before.rows().as_ptr(), after.rows().as_ptr());
    assert_eq!(before.rows(), after.rows());
    assert_eq!(before.y_indices(), after.y_indices());
    assert!(
        before
            .canonical_source()
            .shares_program_owner(after.canonical_source())
    );
    assert_eq!(
        before.colors()[0].outputs().programs()[0].placements(),
        &[(0, 0)]
    );
    let application = after.clone();
    artifacts
        .bind_algebraic_jacobian_application(application)
        .unwrap();
    let foreign = bound_application().algebraic_projection()[0]
        .jacobian_application()
        .unwrap()
        .clone();
    assert_eq!(
        artifacts.bind_algebraic_jacobian_application(foreign),
        Err("projection specialization belongs to a different source or block")
    );
    assert!(
        artifacts.algebraic_projection()[0]
            .jacobian_application()
            .unwrap()
            .canonical_source()
            .shares_program_owner(after.canonical_source())
    );
}

#[test]
fn boxed_elimination_clone_keeps_owned_layout_and_refusal() {
    let mut block = AlgebraicProjectionBlock {
        rows: vec![0, 1],
        y_indices: vec![0, 1],
        tearing: Some(BlockTearing {
            tear_y_indices: vec![1],
            residual_rows: vec![1],
            causal_steps: vec![CausalStep {
                row: 0,
                y_index: 0,
                ..Default::default()
            }],
        }),
        alternate_charts: vec![],
    };
    let mut structure = JacobianStructure::derived(pattern(2, 2));
    structure.affine_elimination =
        AffineEliminationLayout::derive(&block, structure.pattern()).map(Box::new);
    let cloned = structure.clone();
    let source = structure.affine_elimination().unwrap();
    let copy = cloned.affine_elimination().unwrap();
    assert_eq!(source, copy);
    assert!(!std::ptr::eq(source, copy));
    assert_ne!(source.row_columns(0).as_ptr(), copy.row_columns(0).as_ptr());
    assert_eq!(source.causal(), &[(0, 0)]);
    assert_eq!(source.tears(), &[1]);
    assert_eq!(source.residuals(), &[1]);
    block.tearing.as_mut().unwrap().tear_y_indices = vec![0];
    assert!(AffineEliminationLayout::derive(&block, structure.pattern()).is_none());
}

#[test]
fn boxed_application_specialization_retains_canonical_owner() {
    let mut artifacts = bound_application();
    let original = artifacts.algebraic_projection()[0]
        .jacobian_application()
        .unwrap()
        .clone();
    let primal = ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        original
            .source()
            .first_source_span()
            .unwrap()
            .require_provenance("primal")
            .unwrap(),
    )
    .unwrap();
    let specialized = ProjectionJacobianSeedDomain::derive(&original, &primal)
        .unwrap()
        .with_lowered_derivative(original.source().clone())
        .unwrap();
    artifacts
        .bind_algebraic_jacobian_application(specialized)
        .unwrap();
    let bound = artifacts.algebraic_projection()[0]
        .jacobian_application()
        .unwrap();
    assert!(
        bound
            .canonical_source()
            .shares_program_owner(original.canonical_source())
    );
    assert!(bound.primal_source().unwrap().shares_program_owner(&primal));
    assert_eq!(bound.colors()[0].seed_indices(), &[0]);
    assert_eq!(
        bound.colors()[0].outputs().programs()[0].placements(),
        &[(0, 0)]
    );
}
