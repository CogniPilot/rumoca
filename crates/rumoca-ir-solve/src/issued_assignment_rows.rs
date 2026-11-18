//! Ordered assignment rows issued outside a refresh schedule: a torn sweep's
//! causal chain, or a torn block's tangent sweep over its extended slots.
//! Each row's complete output tuple commits to its slots, in order, before
//! the next row runs; a backend prints the rows, their targets and their
//! function chunks as issued. Rows that are forward-mode programs read
//! their AD seed from a declared slot range, so a commit to a seed slot is
//! the seed the next row reads.

use crate::{LinearOp, ScalarProgramBlock, assignment_program_chunk_ends};
use rumoca_core::Span;
use std::ops::Range;

/// Why ordered assignment rows do not issue.
#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum IssuedAssignmentRowsError {
    #[error("issued assignment rows have no row")]
    Empty,
    #[error("issued assignment rows are not a checked block: {0}")]
    Block(String),
    #[error("issued assignment rows store {outputs} outputs for {targets} targets")]
    Targets { outputs: usize, targets: usize },
    #[error("issued assignment target {target} is outside its {slots} slots")]
    Slot { target: usize, slots: usize },
    #[error("issued seed slots {seed:?} are outside the {slots} slots")]
    Seed { seed: Range<usize>, slots: usize },
}

/// Ordered rows, their slot targets in output order, and the function chunks
/// a backend emits them in.
#[derive(Clone, Debug, serde::Serialize)]
pub struct IssuedAssignmentRows {
    rows: ScalarProgramBlock,
    targets: Vec<usize>,
    chunk_ends: Box<[usize]>,
    slots: usize,
    seed: Option<Range<usize>>,
}

impl IssuedAssignmentRows {
    /// Rows with their source spans, writing `targets` (one per stored
    /// output, in row and output order) of a slot vector of `slots` values.
    /// The chunks are cut by the exact-assignment operation budget.
    pub fn new(
        rows: Vec<Vec<LinearOp>>,
        spans: Vec<Span>,
        targets: Vec<usize>,
        slots: usize,
    ) -> Result<Self, IssuedAssignmentRowsError> {
        if rows.is_empty() {
            return Err(IssuedAssignmentRowsError::Empty);
        }
        let chunk_ends = assignment_program_chunk_ends(&rows);
        let rows = ScalarProgramBlock::with_program_spans(rows, spans)
            .map_err(|error| IssuedAssignmentRowsError::Block(format!("{error:?}")))?;
        if rows.stored_output_count() != targets.len() {
            return Err(IssuedAssignmentRowsError::Targets {
                outputs: rows.stored_output_count(),
                targets: targets.len(),
            });
        }
        if let Some(&target) = targets.iter().find(|&&target| target >= slots) {
            return Err(IssuedAssignmentRowsError::Slot { target, slots });
        }
        Ok(Self {
            rows,
            targets,
            chunk_ends,
            slots,
            seed: None,
        })
    }

    /// The rows reading their AD seed from slots `seed`.
    pub fn with_seed(mut self, seed: Range<usize>) -> Result<Self, IssuedAssignmentRowsError> {
        if seed.start > seed.end || seed.end > self.slots {
            return Err(IssuedAssignmentRowsError::Seed {
                seed,
                slots: self.slots,
            });
        }
        self.seed = Some(seed);
        Ok(self)
    }

    #[must_use]
    pub const fn rows(&self) -> &ScalarProgramBlock {
        &self.rows
    }

    #[must_use]
    pub fn targets(&self) -> &[usize] {
        &self.targets
    }

    /// Length of the slot vector the rows read and write.
    #[must_use]
    pub const fn slots(&self) -> usize {
        self.slots
    }

    /// The slots the rows read as their AD seed; `None` for primal rows.
    #[must_use]
    pub fn seed(&self) -> Option<Range<usize>> {
        self.seed.clone()
    }

    /// Consecutive ranges of rows a backend emits as one function each.
    pub fn chunks(&self) -> impl Iterator<Item = Range<usize>> + '_ {
        self.chunk_ends
            .iter()
            .scan(0, |start, &end| Some(std::mem::replace(start, end)..end))
    }
}
