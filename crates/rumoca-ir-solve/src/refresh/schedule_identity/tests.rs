//! Private identity serialization controls; these issue no executable schedule.
use super::*;
use crate::refresh::RefreshSequenceId;

fn bytes(schedule: &ExactRefreshAssignmentSchedule) -> Vec<u8> {
    bincode::serialize(&schedule.key_identity()).unwrap()
}

#[test]
fn private_identity_keeps_program_order_chunks_and_sequence() {
    let original = ExactRefreshAssignmentSchedule {
        sequence_id: RefreshSequenceId(1),
        program_ids: vec![
            ExactRefreshAssignmentProgramId(0),
            ExactRefreshAssignmentProgramId(1),
        ]
        .into_boxed_slice(),
        chunk_ends: vec![2].into_boxed_slice(),
    };
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
