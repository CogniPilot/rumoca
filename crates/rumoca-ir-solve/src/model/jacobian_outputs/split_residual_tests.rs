use super::*;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("split_residual.mo"), 1, 2)
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

/// Two residual outputs `y[a] - p[0]` and `y[a] + p[0]`, whose parameter
/// load is invariant, so a split divides the program.
fn split_pair(a: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: a },
        LinearOp::LoadP { dst: 1, index: 0 },
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

/// Programs 0 and 2 split (rows 0-1 and 4-5), programs 1 and 3 unsplit
/// (rows 2-3 and 6-7).
fn primal() -> ScalarProgramBlock {
    ScalarProgramBlock::with_output_indices(
        vec![
            split_pair(0),
            unsplit_pair(2, 3),
            split_pair(4),
            unsplit_pair(6, 7),
        ],
        vec![span(); 4],
        (0..8).collect(),
    )
    .unwrap()
}

/// Programs 0 and 1 both split (rows 0-1 and 2-3).
fn fully_split_primal() -> ScalarProgramBlock {
    ScalarProgramBlock::with_output_indices(
        vec![split_pair(0), split_pair(2)],
        vec![span(); 2],
        (0..4).collect(),
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

fn residual(
    output_len: usize,
    entries: Vec<ProjectionProgramOutputs>,
) -> ProjectionOutputSelection {
    ProjectionOutputSelection {
        output_len,
        programs: entries,
    }
}

fn mixed_residual() -> ProjectionOutputSelection {
    residual(
        8,
        vec![
            entry(0, 2, &[0, 1]),
            entry(1, 2, &[2, 3]),
            entry(2, 2, &[4, 5]),
            entry(3, 2, &[6, 7]),
        ],
    )
}

const UNKNOWNS: [usize; 8] = [0, 1, 2, 3, 4, 5, 6, 7];

fn programs(selection: &SplitResidualSelection) -> Vec<usize> {
    selection
        .selection()
        .programs()
        .iter()
        .map(ProjectionProgramOutputs::program)
        .collect()
}

#[test]
fn the_split_selection_keeps_the_split_programs_in_order_beside_the_fused_ones() {
    let primal = primal();
    let residual = mixed_residual();
    let fused = FusedResidualSelection::derive(&residual, &primal, &UNKNOWNS).unwrap();
    let split =
        SplitResidualSelection::derive(&residual, Some(&fused), &primal, &UNKNOWNS).unwrap();
    assert_eq!(programs(&split), [0, 2], "entry i is split ordinal i");
    assert_eq!(split.selection().output_len(), 8);
    assert_eq!(
        split.selection().programs()[1].placements(),
        [(0, 4), (1, 5)]
    );
    assert!(split.primal_source().shares_program_owner(&primal));
}

#[test]
fn a_fully_split_selection_needs_no_fused_one() {
    let primal = fully_split_primal();
    let residual = residual(4, vec![entry(0, 2, &[0, 1]), entry(1, 2, &[2, 3])]);
    assert!(FusedResidualSelection::derive(&residual, &primal, &UNKNOWNS[..4]).is_none());
    let split = SplitResidualSelection::derive(&residual, None, &primal, &UNKNOWNS[..4]).unwrap();
    assert_eq!(programs(&split), [0, 1]);
}

#[test]
fn the_split_selection_refuses_an_inexact_partition_one_program_a_repeat_and_a_count_mismatch() {
    let primal = primal();
    let residual = mixed_residual();
    assert!(
        SplitResidualSelection::derive(&residual, None, &primal, &UNKNOWNS).is_none(),
        "unsplit programs without a fused selection leave the partition inexact"
    );
    let one_unsplit = self::residual(
        6,
        vec![
            entry(0, 2, &[0, 1]),
            entry(1, 2, &[2, 3]),
            entry(2, 2, &[4, 5]),
        ],
    );
    assert!(FusedResidualSelection::derive(&one_unsplit, &primal, &UNKNOWNS).is_none());
    assert!(
        SplitResidualSelection::derive(&one_unsplit, None, &primal, &UNKNOWNS).is_none(),
        "a single unsplit program outside any fused selection is not swept"
    );
    let other = self::residual(4, vec![entry(1, 2, &[0, 1]), entry(3, 2, &[2, 3])]);
    let foreign_fused = FusedResidualSelection::derive(&other, &primal, &UNKNOWNS).unwrap();
    let shifted = self::residual(
        8,
        vec![
            entry(0, 2, &[0, 1]),
            entry(3, 2, &[2, 3]),
            entry(2, 2, &[4, 5]),
            entry(1, 2, &[6, 7]),
        ],
    );
    assert!(
        SplitResidualSelection::derive(&shifted, Some(&foreign_fused), &primal, &UNKNOWNS)
            .is_none(),
        "unsplit programs in another order than the fused selection's"
    );
    let single = self::residual(2, vec![entry(0, 2, &[0, 1])]);
    assert!(
        SplitResidualSelection::derive(&single, None, &primal, &UNKNOWNS).is_none(),
        "one split program gains nothing from a sweep"
    );
    let repeated = self::residual(
        4,
        vec![
            entry(0, 2, &[0, 1]),
            entry(2, 2, &[2, 3]),
            entry(0, 2, &[0, 1]),
        ],
    );
    assert!(SplitResidualSelection::derive(&repeated, None, &primal, &UNKNOWNS).is_none());
    let mismatch = self::residual(4, vec![entry(0, 3, &[0, 1]), entry(2, 2, &[2, 3])]);
    assert!(SplitResidualSelection::derive(&mismatch, None, &primal, &UNKNOWNS).is_none());
}

#[test]
fn algebraic_output_evaluations_issue_the_split_residual_selection() {
    let primal = primal();
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, span()).unwrap();
    let rows: Vec<Vec<usize>> = (0..8).map(|row| vec![row]).collect();
    let pattern = StructuralPattern::from_row_dependencies(8, 8, &rows, provenance).unwrap();
    let plan = AlgebraicProjectionPlan {
        blocks: vec![AlgebraicProjectionBlock {
            rows: (0..8).collect(),
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
        programs(structure.split_residual_evaluation().unwrap()),
        [0, 2]
    );
    assert_eq!(
        programs(structure.structure.split_residual_evaluation().unwrap()),
        [0, 2]
    );
}
