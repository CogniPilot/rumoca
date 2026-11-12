//! Private identity serialization controls; these issue no executable schedule.
use super::*;
use crate::refresh::RefreshSequenceId;

fn bytes(schedule: &ExactRefreshAssignmentSchedule) -> Vec<u8> {
    bincode::serialize(&schedule.key_identity()).unwrap()
}

fn schedule() -> ExactRefreshAssignmentSchedule {
    ExactRefreshAssignmentSchedule {
        sequence_id: RefreshSequenceId(1),
        program_ids: vec![
            ExactRefreshAssignmentProgramId(0),
            ExactRefreshAssignmentProgramId(1),
        ]
        .into_boxed_slice(),
        chunk_ends: vec![2].into_boxed_slice(),
    }
}

#[test]
fn private_identity_keeps_program_order_chunks_and_sequence() {
    let original = schedule();
    let expected = bytes(&original);
    assert_eq!(expected, bytes(&original.clone()));
    let mut changed = original.clone();
    changed.sequence_id = RefreshSequenceId(2);
    assert_ne!(expected, bytes(&changed));
    let mut changed = original.clone();
    changed.program_ids.reverse();
    assert_ne!(expected, bytes(&changed));
    let mut changed = original.clone();
    changed.program_ids[1] = ExactRefreshAssignmentProgramId(2);
    assert_ne!(expected, bytes(&changed));
    let mut changed = original.clone();
    changed.chunk_ends = vec![1, 2].into_boxed_slice();
    assert_ne!(expected, bytes(&changed));
}

#[test]
fn schedule_key_is_its_own_domain_and_names_the_selected_schedule() {
    let context = crate::PreparedKernelSource::new(
        &crate::VarLayout::default(),
        &crate::SolvePureCallTable::default(),
    )
    .unwrap();
    let source = crate::ComputeBlock::default();
    let owners = crate::ContinuousRefreshOwners::default();
    let key = |schedule: &ExactRefreshAssignmentSchedule| {
        context
            .exact_assignment_schedule(&source, &owners, schedule)
            .unwrap()
    };
    let original = schedule();
    assert_eq!(key(&original), key(&original.clone()));
    assert_ne!(
        key(&original),
        context.exact_assignments(&source).unwrap(),
        "a schedule key is not the bare source fingerprint"
    );
    let mut changed = original.clone();
    changed.chunk_ends = vec![1, 2].into_boxed_slice();
    assert_ne!(key(&original), key(&changed));
}
