use rumoca_core::{SourceId, Span};

use super::*;
use crate::LinearOp;

/// A program writing `outputs` dense outputs.
fn program(outputs: usize) -> ScalarProgramBlock {
    let programs: Vec<_> = (0..outputs)
        .map(|row| {
            vec![
                LinearOp::Const {
                    dst: 0,
                    value: row as f64,
                },
                LinearOp::StoreOutput { src: 0 },
            ]
        })
        .collect();
    let spans = vec![
        Span::from_offsets(SourceId::from_source_name("tangent_workspace.mo"), 0, 1);
        programs.len()
    ];
    ScalarProgramBlock::with_program_spans(programs, spans).expect("valid test program")
}

/// The problem's own seed and state columns.
fn columns(problem: &SolveProblem) -> usize {
    SolveTangentSchedule::seed_len(problem).unwrap() + problem.solve_layout.state_scalar_count()
}

#[test]
fn the_seed_domain_is_the_problems_y_p_and_time() {
    let problem = SolveProblem::default();
    assert_eq!(
        SolveTangentSchedule::seed_len(&problem),
        Ok(problem.layout.y_scalars() + problem.layout.p_scalars() + 1)
    );
}

#[test]
fn sequential_phases_share_one_scratch_region_of_the_largest_demand() {
    let problem = SolveProblem::default();
    let (rhs, a, b, c) = (program(3), program(2), program(7), program(1));
    let workspace = SolveTangentSchedule::new(&problem, &rhs)
        .and_then(|s| s.assignment(&a, 2))
        .and_then(|s| s.assignment(&b, 7))
        .and_then(|s| s.assignment(&c, 1))
        .and_then(SolveTangentSchedule::issue)
        .unwrap();
    // Segments of 2, 7 and 1 never overlap: scratch is 7, not 10.
    assert_eq!(workspace.scratch_len(), 7);
    assert_eq!(workspace.private_len(), 0);
    assert_eq!(workspace.work_len(), columns(&problem) + 7);
    assert_eq!(
        (
            workspace.implicit_matrix_len(),
            workspace.implicit_pivot_len()
        ),
        (0, 0)
    );
}

#[test]
fn the_implicit_solve_and_consistency_reuse_one_private_region() {
    let problem = SolveProblem::default();
    let seed = SolveTangentSchedule::seed_len(&problem).unwrap();
    let (rhs, segment, implicit, checks) = (program(3), program(2), program(5), program(9));
    let both = SolveTangentSchedule::new(&problem, &rhs)
        .and_then(|s| s.assignment(&segment, 2))
        .and_then(|s| s.implicit(&implicit, 3))
        .and_then(|s| s.consistency(&checks, 20))
        .and_then(SolveTangentSchedule::issue)
        .unwrap();
    assert_eq!(both.implicit_matrix_len(), 9);
    assert_eq!(both.implicit_pivot_len(), 6);
    // Implicit needs seed + 9 + 3, consistency seed + 20: they run in turn,
    // so the larger region serves both, not their sum.
    assert_eq!(both.private_len(), seed + 20);
    assert_eq!(both.scratch_len(), 9);
    assert_eq!(both.work_len(), columns(&problem) + 9 + seed + 20);

    let implicit_only = SolveTangentSchedule::new(&problem, &rhs)
        .and_then(|s| s.implicit(&implicit, 3))
        .and_then(SolveTangentSchedule::issue)
        .unwrap();
    assert_eq!(implicit_only.private_len(), seed + 9 + 3);
    assert_eq!(implicit_only.scratch_len(), 5);
}

#[test]
fn a_segment_must_write_exactly_its_targets() {
    let problem = SolveProblem::default();
    let (rhs, segment) = (program(1), program(2));
    assert_eq!(
        SolveTangentSchedule::new(&problem, &rhs)
            .and_then(|s| s.assignment(&segment, 3))
            .err(),
        Some(SolveTangentWorkspaceError::SegmentCoverage)
    );
}

/// The refusal, if any, of issuing `rhs` followed by `phases`.
fn refusal<'p>(
    problem: &'p SolveProblem,
    rhs: &ScalarProgramBlock,
    phases: impl FnOnce(
        SolveTangentSchedule<'p>,
    ) -> Result<SolveTangentSchedule<'p>, SolveTangentWorkspaceError>,
) -> Option<SolveTangentWorkspaceError> {
    SolveTangentSchedule::new(problem, rhs)
        .and_then(phases)
        .and_then(SolveTangentSchedule::issue)
        .err()
}

#[test]
fn phases_are_admitted_only_in_their_sequential_order() {
    let problem = SolveProblem::default();
    let (rhs, p) = (program(1), program(1));
    let order = Some(SolveTangentWorkspaceError::PhaseOrder);
    assert_eq!(
        refusal(&problem, &rhs, |s| s
            .implicit(&p, 1)
            .and_then(|s| s.assignment(&p, 1))),
        order
    );
    assert_eq!(
        refusal(&problem, &rhs, |s| s
            .consistency(&p, 1)
            .and_then(|s| s.implicit(&p, 1))),
        order
    );
    assert_eq!(
        refusal(&problem, &rhs, |s| s
            .implicit(&p, 1)
            .and_then(|s| s.implicit(&p, 1))),
        order
    );
    assert_eq!(
        refusal(&problem, &rhs, |s| s
            .consistency(&p, 1)
            .and_then(|s| s.consistency(&p, 1))),
        order
    );
    assert_eq!(
        refusal(&problem, &rhs, |s| s
            .assignment(&p, 1)
            .and_then(|s| s.assignment(&p, 1))),
        None
    );
}

#[test]
fn a_workspace_serves_only_the_schedule_it_was_issued_for() {
    let problem = SolveProblem::default();
    let (rhs, segment) = (program(2), program(2));
    let schedule = |problem| {
        SolveTangentSchedule::new(problem, &rhs)
            .and_then(|s| s.assignment(&segment, 2))
            .unwrap()
    };
    let workspace = schedule(&problem).issue().unwrap();
    assert!(workspace.for_schedule(&schedule(&problem)).is_ok());

    let foreign = Some(SolveTangentWorkspaceError::ForeignSchedule);
    // An equal problem that is not the issued one is foreign.
    let other_problem = problem.clone();
    assert_eq!(
        workspace.for_schedule(&schedule(&other_problem)).err(),
        foreign
    );
    // So is a program with equal counts but another owner.
    let other = program(2);
    let other_program = SolveTangentSchedule::new(&problem, &rhs)
        .and_then(|s| s.assignment(&other, 2))
        .unwrap();
    assert_eq!(workspace.for_schedule(&other_program).err(), foreign);
    // And a schedule missing a phase.
    let missing_phase = SolveTangentSchedule::new(&problem, &rhs).unwrap();
    assert_eq!(workspace.for_schedule(&missing_phase).err(), foreign);
}

#[test]
fn every_reachable_overflowing_extent_is_refused() {
    let problem = SolveProblem::default();
    let seed = SolveTangentSchedule::seed_len(&problem).unwrap();
    let (rhs, p) = (program(1), program(1));
    assert_eq!(
        refusal(&problem, &rhs, |s| s.implicit(&p, usize::MAX / 2 + 1)),
        Some(SolveTangentWorkspaceError::ImplicitPivot)
    );
    assert_eq!(
        refusal(&problem, &rhs, |s| s
            .implicit(&p, 1usize << (usize::BITS / 2))),
        Some(SolveTangentWorkspaceError::ImplicitMatrix)
    );
    assert_eq!(
        refusal(&problem, &rhs, |s| s.consistency(&p, usize::MAX)),
        Some(SolveTangentWorkspaceError::ConsistencyPrivate)
    );
    // The private region alone fits; the whole workspace does not.
    assert_eq!(
        refusal(&problem, &rhs, |s| s.consistency(&p, usize::MAX - seed)),
        Some(SolveTangentWorkspaceError::Workspace)
    );
}
