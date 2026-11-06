use super::*;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("fused_residual.mo"), 1, 2)
}

/// Two residual outputs `y[a] - y[b]` and `y[a] + y[b]`: every operation
/// reads a block unknown, so no split divides the program.
fn unsplit_pair(a: usize, b: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: a },
        LinearOp::LoadY { dst: 1, index: b },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutputRange {
            start: 2,
            count: 2,
            stride: 1,
        },
    ]
}

/// One residual output `y[a] - p[0]`, whose parameter load is invariant.
fn split_single(a: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: a },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

/// Programs 0 and 2 unsplit (rows 0-1 and 3-4), program 1 split (row 2).
fn primal() -> ScalarProgramBlock {
    ScalarProgramBlock::with_output_indices(
        vec![unsplit_pair(0, 1), split_single(2), unsplit_pair(3, 4)],
        vec![span(); 3],
        (0..5).collect(),
    )
    .unwrap()
}

fn entry(program: usize, output_count: usize, rows: &[usize]) -> ProjectionProgramOutputs {
    ProjectionProgramOutputs {
        program,
        output_count,
        placements: rows.iter().copied().enumerate().collect(),
    }
}

fn residual(entries: Vec<ProjectionProgramOutputs>) -> ProjectionOutputSelection {
    ProjectionOutputSelection {
        output_len: 5,
        programs: entries,
    }
}

const UNKNOWNS: [usize; 5] = [0, 1, 2, 3, 4];

#[test]
fn the_fused_selection_keeps_the_unsplit_programs_in_order_with_their_bases() {
    let primal = primal();
    assert!(BlockResidualSplit::derive(primal.program(0).unwrap(), &UNKNOWNS).is_none());
    assert!(BlockResidualSplit::derive(primal.program(1).unwrap(), &UNKNOWNS).is_some());
    let residual = residual(vec![
        entry(0, 2, &[0, 1]),
        entry(1, 1, &[2]),
        entry(2, 2, &[3, 4]),
    ]);
    let fused = FusedResidualSelection::derive(&residual, &primal, &UNKNOWNS).unwrap();
    let programs: Vec<_> = fused
        .selection()
        .programs()
        .iter()
        .map(|p| p.program())
        .collect();
    assert_eq!(
        programs,
        [0, 2],
        "the split program keeps its own evaluation"
    );
    assert_eq!(fused.bases(), [0, 2]);
    assert_eq!(fused.output_count(), 4);
    assert_eq!(fused.selection().output_len(), 5);
    assert_eq!(
        fused.selection().programs()[1].placements(),
        [(0, 3), (1, 4)]
    );
    assert!(fused.primal_source().shares_program_owner(&primal));
}

#[test]
fn the_fused_selection_refuses_one_program_a_repeat_and_a_count_mismatch() {
    let primal = primal();
    let single = residual(vec![entry(0, 2, &[0, 1]), entry(1, 1, &[2])]);
    assert!(
        FusedResidualSelection::derive(&single, &primal, &UNKNOWNS).is_none(),
        "one unsplit program gains nothing from a fused entry"
    );
    let repeated = residual(vec![
        entry(0, 2, &[0, 1]),
        entry(2, 2, &[3, 4]),
        entry(0, 2, &[0, 1]),
    ]);
    assert!(FusedResidualSelection::derive(&repeated, &primal, &UNKNOWNS).is_none());
    let mismatch = residual(vec![entry(0, 3, &[0, 1]), entry(2, 2, &[3, 4])]);
    assert!(FusedResidualSelection::derive(&mismatch, &primal, &UNKNOWNS).is_none());
}

#[test]
fn algebraic_output_evaluations_issue_the_fused_residual_selection() {
    let primal = primal();
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, span()).unwrap();
    let rows: Vec<Vec<usize>> = (0..5).map(|row| vec![row]).collect();
    let pattern = StructuralPattern::from_row_dependencies(5, 5, &rows, provenance).unwrap();
    let plan = AlgebraicProjectionPlan {
        blocks: vec![AlgebraicProjectionBlock {
            rows: (0..5).collect(),
            y_indices: UNKNOWNS.to_vec(),
            tearing: None,
            alternate_charts: Vec::new(),
        }]
        .into(),
    };
    let artifacts = ContinuousStructuralArtifacts::derived(
        None,
        vec![pattern],
        vec![false],
        None,
        vec![],
        None,
    )
    .with_algebraic_output_evaluations(&plan, &primal, &primal, &primal);
    let structure = &artifacts.algebraic_projection()[0];
    assert_eq!(
        structure
            .residual_output_evaluation()
            .unwrap()
            .programs()
            .len(),
        3
    );
    let fused = structure.fused_residual_evaluation().unwrap();
    let programs: Vec<_> = fused
        .selection()
        .programs()
        .iter()
        .map(|p| p.program())
        .collect();
    assert_eq!(programs, [0, 2]);
    assert_eq!(fused.bases(), [0, 2]);
}
