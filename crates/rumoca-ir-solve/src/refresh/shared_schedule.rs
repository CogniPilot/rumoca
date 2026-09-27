//! The shared-value segments of one exact refresh assignment schedule
//! (SPEC_0043 §6a): the one construction every executor of the schedule
//! runs.

use rumoca_core::Span;

use super::{
    ContinuousRefreshConstructionError, ContinuousRefreshOwners, ExactRefreshAssignmentSchedule,
};
use crate::{AssignmentProgram, ComputeBlock, SharedValueSegments};

/// The shared-value segments of one schedule, with the provenance of the
/// first program each segment holds.
#[derive(Clone, Debug)]
pub struct SharedAssignmentSchedule {
    segments: SharedValueSegments,
    spans: Vec<Span>,
}

impl SharedAssignmentSchedule {
    #[must_use]
    pub fn segments(&self) -> &SharedValueSegments {
        &self.segments
    }

    /// Provenance of each segment, in segment order.
    #[must_use]
    pub fn spans(&self) -> &[Span] {
        &self.spans
    }
}

impl ExactRefreshAssignmentSchedule {
    /// The shared-value segments of this schedule's final programs, in
    /// schedule order. The checker proves them equivalent to the programs at
    /// construction; executors run the segments in order.
    pub fn shared_segments(
        &self,
        source: &ComputeBlock,
        owners: &ContinuousRefreshOwners,
    ) -> Result<SharedAssignmentSchedule, ContinuousRefreshConstructionError> {
        let mut blocks = Vec::with_capacity(self.program_ids().len());
        let mut targets = Vec::with_capacity(self.program_ids().len());
        for id in self.program_ids() {
            let Some(program) = owners.exact_assignment_program(*id) else {
                return Err(ContinuousRefreshConstructionError {
                    reason: "exact assignment schedule refers to a missing program".to_string(),
                });
            };
            let block = program.final_scalar_program(source)?;
            if block.programs().len() != 1 {
                return Err(ContinuousRefreshConstructionError {
                    reason: "exact assignment owner is not one correlated program".to_string(),
                });
            }
            blocks.push(block);
            targets.push(program.target_indices());
        }
        let programs = blocks
            .iter()
            .zip(&targets)
            .map(|(block, targets)| AssignmentProgram {
                ops: &block.programs()[0],
                targets,
            })
            .collect::<Vec<_>>();
        let segments = SharedValueSegments::derive(&programs);
        // The checker is the construction proof that the segments compute the
        // programs' values; it runs once here, never while executing them.
        if let Err(error) = segments.check(&programs) {
            return Err(ContinuousRefreshConstructionError {
                reason: format!("shared-value segments fail their proof: {error:?}"),
            });
        }
        let spans = segments
            .segments()
            .iter()
            .map(|segment| blocks[segment.first_program()].program_span(0))
            .collect::<Option<Vec<_>>>()
            .ok_or_else(|| ContinuousRefreshConstructionError {
                reason: "exact assignment owner has no provenance".to_string(),
            })?;
        Ok(SharedAssignmentSchedule { segments, spans })
    }
}
