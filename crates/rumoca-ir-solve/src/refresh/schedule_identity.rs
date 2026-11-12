//! Borrowed identity of a constructor-issued exact schedule, not a wire plan.
#[cfg(test)]
mod tests;
use super::{ExactRefreshAssignmentProgramId, ExactRefreshAssignmentSchedule};
use serde::{Serialize, Serializer};

#[derive(Serialize)]
struct ScheduleIdentity<'a> {
    sequence: u64,
    #[serde(serialize_with = "program_ids")]
    programs: &'a [ExactRefreshAssignmentProgramId],
    chunks: &'a [usize],
}

fn program_ids<S: Serializer>(
    ids: &[ExactRefreshAssignmentProgramId],
    serializer: S,
) -> Result<S::Ok, S::Error> {
    serializer.collect_seq(ids.iter().map(|id| id.0))
}

impl ExactRefreshAssignmentSchedule {
    pub(crate) fn key_identity(&self) -> impl Serialize + '_ {
        ScheduleIdentity {
            sequence: self.sequence_id.0,
            programs: &self.program_ids,
            chunks: &self.chunk_ends,
        }
    }
}
