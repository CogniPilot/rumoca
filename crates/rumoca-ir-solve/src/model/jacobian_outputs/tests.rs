use super::*;

#[test]
fn projection_blocks_derive_shared_source_invariance_once() {
    const BLOCKS: usize = 32;
    let programs = (0..BLOCKS)
        .map(|index| {
            vec![
                LinearOp::LoadP { dst: 0, index: 0 },
                LinearOp::LoadSeed { dst: 1, index },
                LinearOp::Binary {
                    dst: 2,
                    op: BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ]
        })
        .collect();
    let source = ScalarProgramBlock::with_output_indices(
        programs,
        vec![span(); BLOCKS],
        (0..BLOCKS).collect(),
    )
    .unwrap();
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, span()).unwrap();
    let pattern = StructuralPattern::from_row_dependencies(1, 1, &[vec![0]], provenance).unwrap();
    let plan = AlgebraicProjectionPlan {
        blocks: (0..BLOCKS)
            .map(|index| AlgebraicProjectionBlock {
                rows: vec![index],
                y_indices: vec![index],
                tearing: None,
                alternate_charts: Vec::new(),
            })
            .collect(),
    };
    projection_application::INVARIANCE_PROOF_OPERATIONS.with(|count| count.set(0));
    let artifacts = ContinuousStructuralArtifacts::derived(
        None,
        vec![pattern.clone(); BLOCKS],
        vec![false; BLOCKS],
        None,
        vec![],
        None,
    )
    .with_algebraic_output_evaluations(&plan, &source, &source, &source);
    for (index, structure) in artifacts.algebraic_projection().iter().enumerate() {
        let application = structure.jacobian_application().unwrap();
        assert_eq!(application.rows(), &[index]);
        assert_eq!(
            application.invariant_operations(index),
            &[true, false, false, false]
        );
    }
    projection_application::INVARIANCE_PROOF_OPERATIONS.with(|count| {
        assert_eq!(
            count.get(),
            4 * BLOCKS,
            "proof work must scale with source size, not source size times block count"
        );
    });
    let mut programs = source.programs().to_vec();
    for program in &mut programs {
        program[1] = LinearOp::LoadP { dst: 1, index: 0 };
    }
    let replacement = ScalarProgramBlock::with_output_indices(
        programs,
        vec![span(); BLOCKS],
        (0..BLOCKS).collect(),
    )
    .unwrap();
    let changed = ContinuousStructuralArtifacts::derived(
        None,
        vec![pattern; BLOCKS],
        vec![false; BLOCKS],
        None,
        vec![],
        None,
    )
    .with_algebraic_output_evaluations(&plan, &replacement, &replacement, &replacement);
    let before = artifacts.algebraic_projection()[0]
        .jacobian_application()
        .unwrap();
    let after = changed.algebraic_projection()[0]
        .jacobian_application()
        .unwrap();
    assert_eq!(before.invariant_operations(0), &[true, false, false, false]);
    assert_eq!(after.invariant_operations(0), &[true, true, true, false]);
    assert!(after.canonical_source().shares_program_owner(&replacement));
    assert!(!after.canonical_source().shares_program_owner(&source));
    projection_application::INVARIANCE_PROOF_OPERATIONS.with(|count| {
        assert_eq!(
            count.get(),
            2 * 4 * BLOCKS,
            "a changed source must derive fresh facts once"
        );
    });
}

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("projection_outputs.mo"), 1, 2)
}

fn source(indices: Vec<usize>, aggregate_first: bool) -> ScalarProgramBlock {
    let aggregate = vec![
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::LoadSeed { dst: 1, index: 1 },
        LinearOp::StoreOutputRange {
            start: 0,
            count: 2,
            stride: 1,
        },
    ];
    let singleton = vec![
        LinearOp::LoadSeed { dst: 0, index: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let programs = if aggregate_first {
        vec![aggregate, singleton]
    } else {
        vec![singleton, aggregate]
    };
    ScalarProgramBlock::with_output_indices(programs, vec![span(); 2], indices).unwrap()
}

fn outputs(y: &ScalarProgramBlock, full: &ScalarProgramBlock) -> ContinuousStructuralArtifacts {
    outputs_with_primal(y, y, full)
}

fn outputs_with_primal(
    primal: &ScalarProgramBlock,
    y: &ScalarProgramBlock,
    full: &ScalarProgramBlock,
) -> ContinuousStructuralArtifacts {
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, span()).unwrap();
    let pattern =
        StructuralPattern::from_row_dependencies(2, 2, &[vec![0, 1], vec![0, 1]], provenance)
            .unwrap();
    let plan = AlgebraicProjectionPlan {
        blocks: vec![AlgebraicProjectionBlock {
            rows: vec![3, 7],
            y_indices: vec![1, 0],
            tearing: None,
            alternate_charts: Vec::new(),
        }],
    };
    ContinuousStructuralArtifacts::derived(None, vec![pattern], vec![false], None, vec![], None)
        .with_algebraic_output_evaluations(&plan, primal, y, full)
}

fn manifold_outputs(source: &ScalarProgramBlock) -> ContinuousStructuralArtifacts {
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, span()).unwrap();
    let pattern =
        StructuralPattern::from_row_dependencies(2, 2, &[vec![0, 1], vec![0, 1]], provenance)
            .unwrap();
    let plan = AlgebraicProjectionPlan {
        blocks: vec![AlgebraicProjectionBlock {
            rows: vec![3, 7],
            y_indices: vec![1, 0],
            tearing: None,
            alternate_charts: Vec::new(),
        }],
    };
    ContinuousStructuralArtifacts::derived(None, vec![], vec![], None, vec![pattern], None)
        .with_manifold_output_evaluations(&plan, source)
}

#[test]
fn manifold_colors_preserve_shared_output_identity_and_reject_unproved_ownership() {
    let valid = source(vec![7, 3, 11], true);
    let artifacts = manifold_outputs(&valid);
    for color in 0..2 {
        let selection = artifacts.manifold_projection()[0]
            .output_evaluation(color)
            .unwrap()
            .solver_y()
            .unwrap();
        assert_eq!(selection.programs().len(), 1);
        assert_eq!(selection.programs()[0].program(), 0);
        assert_eq!(selection.programs()[0].placements(), [(1, 0), (0, 1)]);
    }
    let mut operations = valid.programs().to_vec();
    operations[0].extend([
        LinearOp::Const {
            dst: 8,
            value: 42.0,
        },
        LinearOp::ImpureRandomInit { dst: 9, seed: 8 },
    ]);
    let impure = ScalarProgramBlock::with_output_indices(
        operations,
        valid.program_spans().to_vec(),
        valid.output_indices().to_vec(),
    )
    .unwrap();
    for unproved in [
        source(vec![3, 3, 11], true),
        source(vec![3, 8, 11], true),
        impure,
    ] {
        let artifacts = manifold_outputs(&unproved);
        assert!(
            artifacts.manifold_projection()[0]
                .output_evaluation(0)
                .unwrap()
                .solver_y()
                .is_none()
        );
    }
}

#[test]
fn retained_linearizations_require_repeatable_primal_and_both_directional_owners() {
    let pure = source(vec![7, 3, 11], true);
    let full = source(vec![11, 3, 7], false);
    assert!(
        outputs_with_primal(&pure, &pure, &full).algebraic_projection()[0]
            .linearization_is_repeatable()
    );
    let mut programs = pure.programs().to_vec();
    programs[0].extend([
        LinearOp::Const {
            dst: 8,
            value: 42.0,
        },
        LinearOp::ImpureRandomInit { dst: 9, seed: 8 },
    ]);
    let impure = ScalarProgramBlock::with_output_indices(
        programs,
        pure.program_spans().to_vec(),
        pure.output_indices().to_vec(),
    )
    .unwrap();
    for (primal, y, full) in [
        (&impure, &pure, &full),
        (&pure, &impure, &full),
        (&pure, &pure, &impure),
    ] {
        assert!(
            !outputs_with_primal(primal, y, full).algebraic_projection()[0]
                .linearization_is_repeatable()
        );
    }
    for missing in [source(vec![3, 3, 11], true), source(vec![3, 8, 11], true)] {
        assert!(
            !outputs_with_primal(&missing, &pure, &full).algebraic_projection()[0]
                .linearization_is_repeatable()
        );
    }
}

#[test]
fn projections_preserve_distinct_program_and_output_maps_in_both_seed_spaces() {
    let artifacts = outputs(
        &source(vec![7, 3, 11], true),
        &source(vec![11, 3, 7], false),
    );
    let residual = artifacts.algebraic_projection()[0]
        .residual_output_evaluation()
        .unwrap();
    assert_eq!(residual.programs().len(), 1);
    assert_eq!(residual.programs()[0].program(), 0);
    assert_eq!(residual.programs()[0].placements(), [(1, 0), (0, 1)]);
    let structure = &artifacts.algebraic_projection()[0];
    assert_eq!(structure.coloring().groups().len(), 2);
    for color in 0..2 {
        let evaluation = structure.output_evaluation(color).unwrap();
        let y = evaluation.solver_y().unwrap();
        assert_eq!(y.output_len(), 2);
        assert_eq!(y.programs().len(), 1);
        assert_eq!(y.programs()[0].program(), 0);
        assert_eq!(y.programs()[0].output_count(), 2);
        assert_eq!(y.programs()[0].placements(), [(1, 0), (0, 1)]);
        let full = evaluation.solver_y_and_parameters().unwrap();
        assert_eq!(full.programs().len(), 1);
        assert_eq!(full.programs()[0].program(), 1);
        assert_eq!(full.programs()[0].placements(), [(0, 0), (1, 1)]);
    }
}

#[test]
fn ambiguous_or_missing_output_ownership_cannot_issue_a_batch() {
    for indices in [vec![3, 3, 11], vec![3, 8, 11]] {
        let artifacts = outputs(&source(indices, true), &ScalarProgramBlock::default());
        assert!(
            artifacts.algebraic_projection()[0]
                .residual_output_evaluation()
                .is_none()
        );
        let evaluation = artifacts.algebraic_projection()[0]
            .output_evaluation(0)
            .unwrap();
        assert!(evaluation.solver_y().is_none());
        assert!(evaluation.solver_y_and_parameters().is_none());
    }
}

#[test]
fn unused_impure_operations_cannot_be_reused_through_output_grouping() {
    let base = source(vec![3, 7, 11], true);
    let mut programs = base.programs().to_vec();
    programs[0].extend([
        LinearOp::Const {
            dst: 8,
            value: 42.0,
        },
        LinearOp::ImpureRandomInit { dst: 9, seed: 8 },
    ]);
    let impure = ScalarProgramBlock::with_output_indices(
        programs,
        base.program_spans().to_vec(),
        base.output_indices().to_vec(),
    )
    .unwrap();
    let artifacts = outputs(&impure, &ScalarProgramBlock::default());
    assert!(
        artifacts.algebraic_projection()[0]
            .residual_output_evaluation()
            .is_none()
    );
    assert!(
        artifacts.algebraic_projection()[0]
            .output_evaluation(0)
            .unwrap()
            .solver_y()
            .is_none()
    );
}

#[test]
fn repeatability_checks_discarded_effects_inside_conditional_regions() {
    let scalar = || {
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]
    };
    let mut impure = scalar();
    impure.push(LinearOp::ImpureRandomInit { dst: 1, seed: 0 });
    let conditional = |body| LinearOp::FunctionConditional {
        dst_start: 0,
        capture_start: 0,
        program: std::sync::Arc::new(
            FunctionConditionalProgram::checked(0, vec![1], [(scalar(), body)], scalar()).unwrap(),
        ),
    };
    assert!(super::program_effects::program_is_repeatable(&[
        conditional(scalar())
    ]));
    assert!(!super::program_effects::program_is_repeatable(&[
        conditional(impure)
    ]));
}
