use super::*;
use crate::{PreParamBinding, source_span_from_offsets};

fn block(programs: Vec<Vec<LinearOp>>) -> ScalarProgramBlock {
    let count = programs.len();
    ScalarProgramBlock::with_output_indices(
        programs,
        vec![source_span_from_offsets(1, 0, 1); count],
        (0..count).collect(),
    )
    .expect("one output per program is a scalar-program owner")
}

/// `y[y_index] + p[p_index]`.
fn sum(y_index: usize, p_index: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY {
            dst: 0,
            index: y_index,
        },
        LinearOp::LoadP {
            dst: 1,
            index: p_index,
        },
        LinearOp::Binary {
            dst: 2,
            op: crate::BinaryOp::Add,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ]
}

/// Two root programs and two discrete-row programs over distinct lanes.
fn problem() -> SolveProblem {
    let mut problem = SolveProblem::default();
    problem.events.root_conditions = block(vec![sum(0, 1), sum(2, 3)]);
    problem.discrete.rhs = block(vec![sum(4, 5), sum(6, 7)]);
    problem
}

fn reads_of(reach: &EventOwnerReach, wanted: EventOwner) -> &EventOwnerReads {
    reach
        .owners()
        .find_map(|(owner, reads)| (owner == wanted).then_some(reads))
        .expect("owner is recorded")
}

#[test]
fn each_owner_program_records_exactly_the_lanes_it_reads() {
    let mut problem = problem();
    issue_event_owner_reach(&mut problem);
    let reach = problem
        .events
        .owner_reach
        .checked_for(&problem)
        .expect("the issued reach is the problem's own");
    let lanes = |reads: &EventOwnerReads| {
        (
            reads.y().iter().collect::<Vec<_>>(),
            reads.p().iter().collect::<Vec<_>>(),
        )
    };
    assert_eq!(
        lanes(reads_of(reach, EventOwner::RootCondition(1))),
        (vec![2], vec![3])
    );
    assert_eq!(
        lanes(reads_of(reach, EventOwner::DiscreteRow(0))),
        (vec![4], vec![5])
    );
    assert!(!reads_of(reach, EventOwner::DiscreteRow(1)).reads_time());
    let instant = reach.instant_reads().expect("every owner is analysable");
    assert_eq!(instant.y.iter().collect::<Vec<_>>(), [0, 2, 4, 6]);
    assert_eq!(instant.p.iter().collect::<Vec<_>>(), [1, 3, 5, 7]);
}

#[test]
fn a_reach_is_refused_for_any_problem_but_its_own() {
    let mut problem = problem();
    assert_eq!(
        problem.events.owner_reach.checked_for(&problem).err(),
        Some(EventReachRefusal::NotIssued)
    );
    issue_event_owner_reach(&mut problem);
    // The owners change after the reach was issued.
    let mut changed = problem.clone();
    changed.discrete.rhs = block(vec![sum(4, 5), sum(6, 8)]);
    assert_eq!(
        changed.events.owner_reach.checked_for(&changed).err(),
        Some(EventReachRefusal::Mismatch)
    );
    // A reach copied from another problem.
    let mut other = SolveProblem::default();
    other.events.owner_reach = problem.events.owner_reach.clone();
    assert_eq!(
        other.events.owner_reach.checked_for(&other).err(),
        Some(EventReachRefusal::Mismatch)
    );
}

#[test]
fn owners_the_walk_cannot_see_through_refuse_the_restricted_update() {
    let restricted = |problem: &mut SolveProblem| {
        issue_event_owner_reach(problem);
        problem
            .events
            .owner_reach
            .checked_for(problem)
            .expect("issued")
            .restricted_event_update()
    };
    // The default refresh plans are not certified causal schedules.
    assert_eq!(
        restricted(&mut problem()),
        Err(EventReachRefusal::UncertifiedRefresh)
    );
    let mut delayed = problem();
    delayed.events.delays.value_parameter_indices.push(9);
    assert_eq!(
        restricted(&mut delayed),
        Err(EventReachRefusal::DelayChannels)
    );
}

#[test]
fn a_pre_copy_of_a_solver_coordinate_refuses_the_input_reach_only() {
    let mut problem = problem();
    problem
        .solve_layout
        .pre_param_bindings
        .push(PreParamBinding {
            dest_p_index: 8,
            source: PreParamSource::Y { index: 0 },
            clock_schedule: None,
        });
    issue_event_owner_reach(&mut problem);
    let reach = problem
        .events
        .owner_reach
        .checked_for(&problem)
        .expect("issued");
    assert_eq!(
        reach.instant_reads().err(),
        Some(EventReachRefusal::SolverPreCopy)
    );
}
