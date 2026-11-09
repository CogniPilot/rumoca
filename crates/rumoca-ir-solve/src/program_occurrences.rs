//! The component occurrence of each scalar program (SPEC_0040 FLAT-C08).
//!
//! Lowering records which component class equation and instance a program
//! came from. It is identity only: no evaluation reads it, and a block built
//! any other way has none.
use super::*;
use rumoca_core::ComponentOccurrence;
use serde::ser::SerializeStruct;

/// The serialized block. Absent occurrences are dropped only from
/// human-readable formats, keeping JSON byte-identical for blocks without
/// them, while positional formats (bincode) keep every field and round-trip.
pub(super) struct ScalarProgramBlockWireRef<'a> {
    pub(super) programs: &'a [Vec<LinearOp>],
    pub(super) program_spans: &'a [Span],
    pub(super) output_placements: ScalarOutputPlacementView<'a>,
    pub(super) program_occurrences: &'a [Option<ComponentOccurrence>],
}

impl Serialize for ScalarProgramBlockWireRef<'_> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let omit = serializer.is_human_readable() && self.program_occurrences.is_empty();
        let mut state =
            serializer.serialize_struct("ScalarProgramBlockWire", if omit { 3 } else { 4 })?;
        state.serialize_field("programs", self.programs)?;
        state.serialize_field("program_spans", self.program_spans)?;
        state.serialize_field("output_placements", &self.output_placements)?;
        if !omit {
            state.serialize_field("program_occurrences", self.program_occurrences)?;
        }
        state.end()
    }
}

impl ScalarProgramBlock {
    /// Attach one occurrence per program to a block lowering just built.
    ///
    /// An empty list, or one with no occurrence at all, leaves the block
    /// without occurrences. Any other length is refused.
    pub fn with_program_occurrences(
        mut self,
        occurrences: Vec<Option<ComponentOccurrence>>,
    ) -> Result<Self, SolveProblemShapeContractError> {
        if occurrences.iter().all(Option::is_none) {
            return Ok(self);
        }
        let programs = self.data.programs.len();
        if occurrences.len() != programs {
            return Err(SolveProblemShapeContractError::ScalarProgramSpanMismatch {
                context: "ScalarProgramBlock occurrences".into(),
                node_index: 0,
                programs,
                spans: occurrences.len(),
                span: first_span(&self.data.program_spans),
            });
        }
        let data = Arc::get_mut(&mut self.data).ok_or(
            SolveProblemShapeContractError::InitializationOwnership {
                detail: "program occurrences attach only to a block lowering still owns",
            },
        )?;
        data.program_occurrences = occurrences.into_boxed_slice();
        Ok(self)
    }

    /// The component occurrence program `row` was lowered from, if recorded.
    pub fn program_occurrence(&self, row: usize) -> Option<ComponentOccurrence> {
        self.data.program_occurrences.get(row).copied().flatten()
    }

    /// One occurrence per program, or empty when none was recorded.
    pub(crate) fn program_occurrences(&self) -> &[Option<ComponentOccurrence>] {
        &self.data.program_occurrences
    }
}
