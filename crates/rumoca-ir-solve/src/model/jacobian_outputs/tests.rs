use super::*;

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
        }],
    };
    ContinuousStructuralArtifacts::derived(None, vec![pattern], vec![false], None, vec![], None)
        .with_algebraic_output_evaluations(&plan, y, full)
}

#[test]
fn projections_preserve_distinct_program_and_output_maps_in_both_seed_spaces() {
    let artifacts = outputs(
        &source(vec![7, 3, 11], true),
        &source(vec![11, 3, 7], false),
    );
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
