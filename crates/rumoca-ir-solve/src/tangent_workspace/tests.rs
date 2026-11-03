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

/// The refusal, if any, of issuing `occurrences` over `problem`.
fn refusal<'p>(
    problem: &'p SolveProblem,
    occurrences: impl FnOnce(
        SolveTangentSchedule<'p>,
    ) -> Result<SolveTangentSchedule<'p>, SolveTangentWorkspaceError>,
) -> Option<SolveTangentWorkspaceError> {
    SolveTangentSchedule::new(problem)
        .and_then(occurrences)
        .and_then(SolveTangentSchedule::issue)
        .err()
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
fn the_issued_occurrences_are_the_consumer_execution_order() {
    let problem = SolveProblem::default();
    let (residuals, a, b, checks, rhs) =
        (program(5), program(2), program(1), program(9), program(3));
    let workspace = SolveTangentSchedule::new(&problem)
        .and_then(|s| s.implicit_solve(&residuals, 3))
        .and_then(|s| s.assignment(&a, 2))
        .and_then(|s| s.assignment(&b, 1))
        .and_then(|s| s.implicit_verification(&residuals))
        .and_then(|s| s.consistency(&checks, 20))
        .and_then(|s| s.rhs(&rhs))
        .and_then(SolveTangentSchedule::issue)
        .unwrap();
    assert_eq!(
        workspace.occurrences().collect::<Vec<_>>(),
        [
            SolveTangentOccurrence::ImplicitSolve { unknowns: 3 },
            SolveTangentOccurrence::Assignment {
                segment: 0,
                targets: 2
            },
            SolveTangentOccurrence::Assignment {
                segment: 1,
                targets: 1
            },
            SolveTangentOccurrence::ImplicitVerification { unknowns: 3 },
            SolveTangentOccurrence::Consistency { rows: 20 },
            SolveTangentOccurrence::Rhs,
        ]
    );
    let wire = serde_json::to_value(workspace.occurrences().collect::<Vec<_>>()).unwrap();
    assert_eq!(wire[1]["kind"], "assignment");
    assert_eq!(wire[3]["kind"], "implicit_verification");
    assert!(workspace.uses_scratch() && workspace.uses_private() && workspace.uses_pivots());
}

#[test]
fn sequential_occurrences_share_one_scratch_region_of_the_largest_demand() {
    let problem = SolveProblem::default();
    let (a, b, c, rhs) = (program(2), program(7), program(1), program(3));
    let workspace = SolveTangentSchedule::new(&problem)
        .and_then(|s| s.assignment(&a, 2))
        .and_then(|s| s.assignment(&b, 7))
        .and_then(|s| s.assignment(&c, 1))
        .and_then(|s| s.rhs(&rhs))
        .and_then(SolveTangentSchedule::issue)
        .unwrap();
    // Segments of 2, 7 and 1 never overlap: scratch is 7, not 10.
    assert_eq!(workspace.scratch_len(), 7);
    assert_eq!(workspace.private_len(), 0);
    assert_eq!(workspace.work_len(), columns(&problem) + 7);
    assert!(workspace.uses_scratch() && !workspace.uses_private() && !workspace.uses_pivots());

    let rhs_only = SolveTangentSchedule::new(&problem)
        .and_then(|s| s.rhs(&rhs))
        .and_then(SolveTangentSchedule::issue)
        .unwrap();
    assert!(!rhs_only.uses_scratch());
}

#[test]
fn the_implicit_solve_verification_and_consistency_reuse_one_private_region() {
    let problem = SolveProblem::default();
    let seed = SolveTangentSchedule::seed_len(&problem).unwrap();
    let (residuals, checks, rhs) = (program(5), program(9), program(3));
    let all = SolveTangentSchedule::new(&problem)
        .and_then(|s| s.implicit_solve(&residuals, 3))
        .and_then(|s| s.implicit_verification(&residuals))
        .and_then(|s| s.consistency(&checks, 20))
        .and_then(|s| s.rhs(&rhs))
        .and_then(SolveTangentSchedule::issue)
        .unwrap();
    assert_eq!(all.implicit_matrix_len(), 9);
    assert_eq!(all.implicit_pivot_len(), 6);
    // Solve seed + 9 + 3, verification seed + 3, consistency seed + 20: they
    // run in turn, so the largest region serves all three, not their sum.
    assert_eq!(all.private_len(), seed + 20);
    assert_eq!(all.scratch_len(), 9);
    assert_eq!(all.work_len(), columns(&problem) + 9 + seed + 20);

    let implicit_only = SolveTangentSchedule::new(&problem)
        .and_then(|s| s.implicit_solve(&residuals, 3))
        .and_then(|s| s.implicit_verification(&residuals))
        .and_then(|s| s.rhs(&rhs))
        .and_then(SolveTangentSchedule::issue)
        .unwrap();
    assert_eq!(implicit_only.private_len(), seed + 9 + 3);
    assert_eq!(implicit_only.scratch_len(), 5);
}

#[test]
fn a_segment_must_write_exactly_its_targets() {
    let problem = SolveProblem::default();
    let segment = program(2);
    assert_eq!(
        refusal(&problem, |s| s.assignment(&segment, 3)),
        Some(SolveTangentWorkspaceError::SegmentCoverage)
    );
}

#[test]
fn occurrences_are_admitted_only_in_execution_order_and_complete() {
    let problem = SolveProblem::default();
    let (p, other, rhs) = (program(1), program(1), program(1));
    let order = Some(SolveTangentWorkspaceError::OccurrenceOrder);
    // Out of order.
    assert_eq!(
        refusal(&problem, |s| s
            .assignment(&p, 1)
            .and_then(|s| s.implicit_solve(&p, 1))),
        order
    );
    assert_eq!(
        refusal(&problem, |s| s.rhs(&rhs).and_then(|s| s.assignment(&p, 1))),
        order
    );
    assert_eq!(
        refusal(&problem, |s| s
            .consistency(&p, 1)
            .and_then(|s| s.consistency(&p, 1))),
        order
    );
    // A verification needs its own solve, over the same residual program.
    assert_eq!(refusal(&problem, |s| s.implicit_verification(&p)), order);
    assert_eq!(
        refusal(&problem, |s| s
            .implicit_solve(&p, 1)
            .and_then(|s| s.implicit_verification(&other))),
        order
    );
    // Incomplete: no right-hand side, or a solve without its verification.
    assert_eq!(refusal(&problem, |s| s.assignment(&p, 1)), order);
    assert_eq!(
        refusal(&problem, |s| s
            .implicit_solve(&p, 1)
            .and_then(|s| s.rhs(&rhs))),
        order
    );
    // Repeated segments are the one admitted repetition.
    assert_eq!(
        refusal(&problem, |s| s
            .assignment(&p, 1)
            .and_then(|s| s.assignment(&p, 1))
            .and_then(|s| s.rhs(&rhs))),
        None
    );
}

#[test]
fn a_workspace_serves_only_the_schedule_it_was_issued_for() {
    let problem = SolveProblem::default();
    let (rhs, segment) = (program(2), program(2));
    let schedule = |problem| {
        SolveTangentSchedule::new(problem)
            .and_then(|s| s.assignment(&segment, 2))
            .and_then(|s| s.rhs(&rhs))
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
    let other_program = SolveTangentSchedule::new(&problem)
        .and_then(|s| s.assignment(&other, 2))
        .and_then(|s| s.rhs(&rhs))
        .unwrap();
    assert_eq!(workspace.for_schedule(&other_program).err(), foreign);
    // And a schedule missing an occurrence.
    let missing = SolveTangentSchedule::new(&problem)
        .and_then(|s| s.rhs(&rhs))
        .unwrap();
    assert_eq!(workspace.for_schedule(&missing).err(), foreign);
}

/// The refusal of a complete implicit schedule of `unknowns` targets.
fn implicit_refusal(problem: &SolveProblem, unknowns: usize) -> Option<SolveTangentWorkspaceError> {
    let (residuals, rhs) = (program(1), program(1));
    refusal(problem, |s| {
        s.implicit_solve(&residuals, unknowns)
            .and_then(|s| s.implicit_verification(&residuals))
            .and_then(|s| s.rhs(&rhs))
    })
}

#[test]
fn every_reachable_overflowing_extent_is_refused() {
    let problem = SolveProblem::default();
    let seed = SolveTangentSchedule::seed_len(&problem).unwrap();
    let (p, rhs) = (program(1), program(1));
    assert_eq!(
        implicit_refusal(&problem, usize::MAX / 2 + 1),
        Some(SolveTangentWorkspaceError::ImplicitPivot)
    );
    assert_eq!(
        implicit_refusal(&problem, 1usize << (usize::BITS / 2)),
        Some(SolveTangentWorkspaceError::ImplicitMatrix)
    );
    assert_eq!(
        refusal(&problem, |s| s
            .consistency(&p, usize::MAX)
            .and_then(|s| s.rhs(&rhs))),
        Some(SolveTangentWorkspaceError::ConsistencyPrivate)
    );
    // The private region alone fits; the whole workspace does not.
    assert_eq!(
        refusal(&problem, |s| s
            .consistency(&p, usize::MAX - seed)
            .and_then(|s| s.rhs(&rhs))),
        Some(SolveTangentWorkspaceError::Workspace)
    );
}
