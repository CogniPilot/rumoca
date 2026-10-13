//! Checked affine projections of canonical ranged output stores.

#[cfg(test)]
mod tests;

use crate::scalar_output_placements::ScalarOutputPlacements;
use crate::{LinearOp, ScalarProgramBlock};

/// One ranged store's exact logical outputs, in canonical store order.
///
/// Only a checked scalar-program owner issues this projection. A descending
/// span subtracts its stride; every endpoint and intermediate output is
/// checked against the owner's complete output mapping before it is issued.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ScalarProgramOutputSpan {
    start: usize,
    stride: usize,
    descending: bool,
    count: usize,
}

impl ScalarProgramOutputSpan {
    pub(crate) fn contiguous(start: usize, count: usize) -> Option<Self> {
        let next = if count > 1 {
            start.checked_add(1)?
        } else {
            start
        };
        Self::from_start_next(start, next, count)
    }

    pub(crate) fn checked(indices: &[usize]) -> Option<Self> {
        let &start = indices.first()?;
        let next = indices.get(1).copied().unwrap_or(start);
        let span = Self::from_start_next(start, next, indices.len())?;
        indices
            .iter()
            .enumerate()
            .all(|(ordinal, &index)| span.index(ordinal) == Some(index))
            .then_some(span)
    }

    pub(crate) fn from_start_next(start: usize, next: usize, count: usize) -> Option<Self> {
        let span = Self {
            start,
            stride: start.abs_diff(next),
            descending: next < start,
            count,
        };
        span.index(count.checked_sub(1)?)?;
        Some(span)
    }

    pub(crate) fn matches_contiguous(self, range: std::ops::Range<usize>, offset: usize) -> bool {
        self.index(offset) == Some(range.start)
            && (range.len() <= 1 || (!self.descending && self.stride == 1))
    }

    pub(crate) fn matches_explicit(self, indices: &[usize], offset: usize) -> bool {
        indices.iter().enumerate().all(|(ordinal, &index)| {
            offset
                .checked_add(ordinal)
                .and_then(|ordinal| self.index(ordinal))
                == Some(index)
        })
    }

    #[must_use]
    pub const fn start(self) -> usize {
        self.start
    }

    #[must_use]
    pub const fn stride(self) -> usize {
        self.stride
    }

    #[must_use]
    pub const fn descending(self) -> bool {
        self.descending
    }

    #[must_use]
    pub const fn count(self) -> usize {
        self.count
    }

    #[must_use]
    pub fn index(self, ordinal: usize) -> Option<usize> {
        if ordinal >= self.count {
            return None;
        }
        let offset = ordinal.checked_mul(self.stride)?;
        if self.descending {
            self.start.checked_sub(offset)
        } else {
            self.start.checked_add(offset)
        }
    }
}

pub(crate) fn output_spans(
    programs: &[Vec<LinearOp>],
    placements: &ScalarOutputPlacements,
    program_ends: &[usize],
) -> Vec<Vec<Option<ScalarProgramOutputSpan>>> {
    let mut ordinal = 0;
    programs
        .iter()
        .map(|program| {
            program
                .iter()
                .map(|op| {
                    let count = match op {
                        LinearOp::StoreOutput { .. } => 1,
                        LinearOp::StoreOutputRange { count, .. } => *count,
                        _ => return None,
                    };
                    let end = ordinal + count;
                    let span = matches!(op, LinearOp::StoreOutputRange { .. })
                        .then(|| placements.span_for_ordinals(ordinal, count, program_ends))
                        .flatten();
                    ordinal = end;
                    span
                })
                .collect()
        })
        .collect()
}

impl ScalarProgramBlock {
    /// The affine output pairing of this exact canonical ranged store.
    /// Scalar stores and irregular mappings expose no ranged projection.
    #[must_use]
    pub fn program_output_span(
        &self,
        program: usize,
        operation: usize,
    ) -> Option<ScalarProgramOutputSpan> {
        self.data
            .output_spans
            .get(program)?
            .get(operation)
            .copied()
            .flatten()
    }
}
