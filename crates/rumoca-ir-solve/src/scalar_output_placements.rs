mod range_wire;
#[cfg(test)]
mod tests;

use crate::{SolveProblemShapeContractError, validate_scalar_program_metadata_lengths};
use rumoca_core::Span;
use serde::{Deserialize, Serialize};
use std::ops::Range;

/// Untrusted placement input for one complete program's stored outputs.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ScalarProgramOutputPlacement {
    Range(#[serde(with = "range_wire")] Range<usize>),
    Explicit(Vec<usize>),
    Segments(Vec<ScalarOutputSegment>),
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum ScalarOutputSegment {
    Range(#[serde(with = "range_wire")] Range<usize>),
    Explicit(Vec<usize>),
}

/// Borrowed canonical placement; scalar slices are explicit materialized views.
#[derive(Clone, Copy, Debug, Serialize)]
pub enum ScalarOutputPlacementView<'source> {
    Dense { count: usize },
    ProgramRanges(&'source [ScalarProgramOutputPlacement]),
    Explicit(&'source [usize]),
}

#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) enum ScalarOutputPlacementsWire {
    Dense { count: usize },
    ProgramRanges(Vec<ScalarProgramOutputPlacement>),
    Explicit(Vec<usize>),
}

#[derive(Debug, Default)]
pub(crate) struct ScalarOutputPlacements {
    kind: PlacementKind,
    count: usize,
    maximum: Option<usize>,
}

#[derive(Debug, Default)]
enum PlacementKind {
    #[default]
    Dense,
    ProgramRanges(Box<[ScalarProgramOutputPlacement]>),
    Explicit {
        indices: Vec<usize>,
        ordinals: Box<[usize]>,
    },
}

impl ScalarProgramOutputPlacement {
    fn segments(&self) -> impl Iterator<Item = ScalarOutputSegmentRef<'_>> {
        let (single, multiple): (Option<ScalarOutputSegmentRef<'_>>, &[ScalarOutputSegment]) =
            match self {
                Self::Range(range) => (Some(ScalarOutputSegmentRef::Range(range)), &[]),
                Self::Explicit(indices) => (Some(ScalarOutputSegmentRef::Explicit(indices)), &[]),
                Self::Segments(segments) => (None, segments),
            };
        single
            .into_iter()
            .chain(multiple.iter().map(ScalarOutputSegmentRef::from))
    }

    pub fn concatenate(self, other: Self) -> Self {
        let mut segments = self.into_segments();
        for segment in other.into_segments() {
            if let (Some(ScalarOutputSegment::Range(last)), ScalarOutputSegment::Range(next)) =
                (segments.last_mut(), &segment)
                && last.end == next.start
            {
                last.end = next.end;
                continue;
            }
            segments.push(segment);
        }
        Self::Segments(segments)
    }

    fn into_segments(self) -> Vec<ScalarOutputSegment> {
        match self {
            Self::Range(range) => vec![ScalarOutputSegment::Range(range)],
            Self::Explicit(indices) => vec![ScalarOutputSegment::Explicit(indices)],
            Self::Segments(segments) => segments,
        }
    }

    pub(crate) fn extend_materialized_indices(
        &self,
        outputs: &mut std::collections::BTreeSet<usize>,
    ) {
        for segment in self.segments() {
            match segment {
                ScalarOutputSegmentRef::Range(range) => outputs.extend(range.clone()),
                ScalarOutputSegmentRef::Explicit(indices) => {
                    outputs.extend(indices.iter().copied())
                }
            }
        }
    }

    pub(crate) fn checked_count(&self) -> Option<usize> {
        self.segments().try_fold(0usize, |count, segment| {
            count.checked_add(segment.checked_count()?)
        })
    }
    pub(crate) fn index_at(&self, mut ordinal: usize) -> Option<usize> {
        for segment in self.segments() {
            let count = segment.checked_count()?;
            if ordinal < count {
                return segment.index_at(ordinal);
            }
            ordinal -= count;
        }
        None
    }
    fn position(&self, output: usize) -> Option<usize> {
        let mut ordinal = 0usize;
        for segment in self.segments() {
            if let Some(position) = segment.position(output) {
                return ordinal.checked_add(position);
            }
            ordinal = ordinal.checked_add(segment.checked_count()?)?;
        }
        None
    }
    fn maximum(&self) -> Option<usize> {
        self.segments()
            .filter_map(|segment| segment.maximum())
            .max()
    }
    fn is_dense_at(&self, mut ordinal: usize) -> bool {
        for segment in self.segments() {
            if !segment.is_dense_at(ordinal) {
                return false;
            }
            let Some(next) = segment
                .checked_count()
                .and_then(|count| ordinal.checked_add(count))
            else {
                return false;
            };
            ordinal = next;
        }
        true
    }
    fn span(&self, mut ordinal: usize, count: usize) -> Option<crate::ScalarProgramOutputSpan> {
        let first = self.index_at(ordinal)?;
        let next = if count > 1 {
            self.index_at(ordinal.checked_add(1)?)?
        } else {
            first
        };
        let span = crate::ScalarProgramOutputSpan::from_start_next(first, next, count)?;
        let mut remaining = count;
        for segment in self.segments() {
            let size = segment.checked_count()?;
            if ordinal >= size {
                ordinal -= size;
                continue;
            }
            let selected = remaining.min(size - ordinal);
            if !segment.matches_span(ordinal, selected, span, count - remaining) {
                return None;
            }
            remaining -= selected;
            if remaining == 0 {
                return Some(span);
            }
            ordinal = 0;
        }
        None
    }
}

#[derive(Clone, Copy)]
enum ScalarOutputSegmentRef<'a> {
    Range(&'a Range<usize>),
    Explicit(&'a [usize]),
}
impl<'a> From<&'a ScalarOutputSegment> for ScalarOutputSegmentRef<'a> {
    fn from(segment: &'a ScalarOutputSegment) -> Self {
        match segment {
            ScalarOutputSegment::Range(range) => Self::Range(range),
            ScalarOutputSegment::Explicit(indices) => Self::Explicit(indices),
        }
    }
}
impl ScalarOutputSegmentRef<'_> {
    fn checked_count(self) -> Option<usize> {
        match self {
            Self::Range(range) => range.end.checked_sub(range.start),
            Self::Explicit(indices) => Some(indices.len()),
        }
    }
    fn index_at(self, ordinal: usize) -> Option<usize> {
        match self {
            Self::Range(range) => (ordinal < self.checked_count()?).then(|| range.start + ordinal),
            Self::Explicit(indices) => indices.get(ordinal).copied(),
        }
    }
    fn position(self, output: usize) -> Option<usize> {
        match self {
            Self::Range(range) => range.contains(&output).then(|| output - range.start),
            Self::Explicit(indices) => indices.iter().position(|&index| index == output),
        }
    }
    fn maximum(self) -> Option<usize> {
        match self {
            Self::Range(range) => (!range.is_empty()).then(|| range.end - 1),
            Self::Explicit(indices) => indices.iter().copied().max(),
        }
    }
    fn is_dense_at(self, ordinal: usize) -> bool {
        match self {
            Self::Range(range) => range.is_empty() || range.start == ordinal,
            Self::Explicit(indices) => indices
                .iter()
                .enumerate()
                .all(|(offset, &index)| ordinal.checked_add(offset) == Some(index)),
        }
    }
    fn matches_span(
        self,
        ordinal: usize,
        count: usize,
        span: crate::ScalarProgramOutputSpan,
        offset: usize,
    ) -> bool {
        let Some(end) = ordinal.checked_add(count) else {
            return false;
        };
        match self {
            Self::Range(range) => {
                let Some(start) = range.start.checked_add(ordinal) else {
                    return false;
                };
                let Some(end) = range.start.checked_add(end) else {
                    return false;
                };
                span.matches_contiguous(start..end, offset)
            }
            Self::Explicit(indices) => indices
                .get(ordinal..end)
                .is_some_and(|indices| span.matches_explicit(indices, offset)),
        }
    }
}

impl ScalarOutputPlacements {
    pub(crate) fn indices(&self) -> impl Iterator<Item = usize> + '_ {
        let (dense, programs, explicit): (Range<usize>, &[ScalarProgramOutputPlacement], &[usize]) =
            match &self.kind {
                PlacementKind::Dense => (0..self.count, &[][..], &[][..]),
                PlacementKind::ProgramRanges(entries) => (0..0, entries.as_ref(), &[][..]),
                PlacementKind::Explicit { indices, .. } => (0..0, &[][..], indices.as_slice()),
            };
        dense
            .chain(programs.iter().flat_map(|entry| {
                entry.segments().flat_map(|segment| {
                    let (range, indices): (_, &[_]) = match segment {
                        ScalarOutputSegmentRef::Range(range) => (range.clone(), &[][..]),
                        ScalarOutputSegmentRef::Explicit(indices) => (0..0, indices),
                    };
                    range.chain(indices.iter().copied())
                })
            }))
            .chain(explicit.iter().copied())
    }

    pub(crate) fn checked(
        wire: ScalarOutputPlacementsWire,
        program_ends: &[usize],
        spans: &[Span],
    ) -> Result<Self, SolveProblemShapeContractError> {
        let count = program_ends.last().copied().unwrap_or(0);
        let span = spans.first().copied();
        match wire {
            ScalarOutputPlacementsWire::Dense { count: actual } => {
                check_count(count, actual, 0, span)?;
                Ok(Self {
                    kind: PlacementKind::Dense,
                    count,
                    maximum: count.checked_sub(1),
                })
            }
            ScalarOutputPlacementsWire::Explicit(indices) => {
                check_count(count, indices.len(), 0, span)?;
                if let Some(ordinal) = indices.iter().position(|&index| index == usize::MAX) {
                    let program = program_ends.partition_point(|&end| end <= ordinal);
                    return Err(crate::output_index_overflow(
                        "ScalarProgramBlock",
                        program,
                        spans.get(program).copied(),
                    ));
                }
                Ok(Self::from_explicit(indices, count))
            }
            ScalarOutputPlacementsWire::ProgramRanges(entries) => {
                if entries.len() != program_ends.len() {
                    return Err(SolveProblemShapeContractError::ScalarProgramCountMismatch {
                        context: "ScalarProgramBlock.output_placements",
                        expected: program_ends.len(),
                        actual: entries.len(),
                        span,
                    });
                }
                Self::from_program_ranges(entries, program_ends, spans, count)
            }
        }
    }

    fn from_explicit(indices: Vec<usize>, count: usize) -> Self {
        let maximum = indices.iter().copied().max();
        let kind = if indices
            .iter()
            .enumerate()
            .all(|(ordinal, &index)| ordinal == index)
        {
            PlacementKind::Dense
        } else {
            let mut ordinals = (0..indices.len()).collect::<Vec<_>>();
            ordinals.sort_unstable_by_key(|&ordinal| (indices[ordinal], ordinal));
            PlacementKind::Explicit {
                indices,
                ordinals: ordinals.into_boxed_slice(),
            }
        };
        Self {
            kind,
            count,
            maximum,
        }
    }

    fn from_program_ranges(
        entries: Vec<ScalarProgramOutputPlacement>,
        program_ends: &[usize],
        spans: &[Span],
        count: usize,
    ) -> Result<Self, SolveProblemShapeContractError> {
        let mut start = 0;
        let mut dense = true;
        let mut maximum = None;
        for (program, entry) in entries.iter().enumerate() {
            let actual = entry.checked_count().ok_or_else(|| {
                crate::output_index_overflow(
                    "ScalarProgramBlock",
                    program,
                    spans.get(program).copied(),
                )
            })?;
            check_count(
                program_ends[program] - start,
                actual,
                program,
                spans.get(program).copied(),
            )?;
            if entry.maximum() == Some(usize::MAX) {
                return Err(crate::output_index_overflow(
                    "ScalarProgramBlock",
                    program,
                    spans.get(program).copied(),
                ));
            }
            dense &= entry.is_dense_at(start);
            maximum = maximum.max(entry.maximum());
            start = program_ends[program];
        }
        let kind = if dense {
            PlacementKind::Dense
        } else {
            PlacementKind::ProgramRanges(entries.into_boxed_slice())
        };
        Ok(Self {
            kind,
            count,
            maximum,
        })
    }

    pub(crate) fn view(&self) -> ScalarOutputPlacementView<'_> {
        match &self.kind {
            PlacementKind::Dense => ScalarOutputPlacementView::Dense { count: self.count },
            PlacementKind::ProgramRanges(entries) => {
                ScalarOutputPlacementView::ProgramRanges(entries)
            }
            PlacementKind::Explicit { indices, .. } => ScalarOutputPlacementView::Explicit(indices),
        }
    }

    pub(crate) fn count(&self) -> usize {
        self.count
    }
    pub(crate) fn maximum(&self) -> Option<usize> {
        self.maximum
    }
    pub(crate) fn is_dense(&self) -> bool {
        matches!(self.kind, PlacementKind::Dense)
    }

    pub(crate) fn index_at(&self, ordinal: usize, program_ends: &[usize]) -> Option<usize> {
        if ordinal >= self.count {
            return None;
        }
        match &self.kind {
            PlacementKind::Dense => Some(ordinal),
            PlacementKind::Explicit { indices, .. } => indices.get(ordinal).copied(),
            PlacementKind::ProgramRanges(entries) => {
                let program = program_ends.partition_point(|&end| end <= ordinal);
                let start = program.checked_sub(1).map_or(0, |p| program_ends[p]);
                entries.get(program)?.index_at(ordinal - start)
            }
        }
    }

    pub(crate) fn span_for_ordinals(
        &self,
        ordinal: usize,
        count: usize,
        program_ends: &[usize],
    ) -> Option<crate::ScalarProgramOutputSpan> {
        let end = ordinal.checked_add(count)?;
        if count == 0 || end > self.count {
            return None;
        }
        match &self.kind {
            PlacementKind::Dense => crate::ScalarProgramOutputSpan::contiguous(ordinal, count),
            PlacementKind::Explicit { indices, .. } => {
                crate::ScalarProgramOutputSpan::checked(indices.get(ordinal..end)?)
            }
            PlacementKind::ProgramRanges(entries) => {
                let program = program_ends.partition_point(|&end| end <= ordinal);
                if end > *program_ends.get(program)? {
                    return None;
                }
                let start = program.checked_sub(1).map_or(0, |p| program_ends[p]);
                let offset = ordinal - start;
                entries.get(program)?.span(offset, count)
            }
        }
    }

    pub(crate) fn ordinal_for_output(
        &self,
        output: usize,
        program_ends: &[usize],
    ) -> Option<usize> {
        match &self.kind {
            PlacementKind::Dense => (output < self.count).then_some(output),
            PlacementKind::Explicit { indices, ordinals } => {
                let position = ordinals.partition_point(|&ordinal| indices[ordinal] < output);
                let ordinal = *ordinals.get(position)?;
                (indices[ordinal] == output).then_some(ordinal)
            }
            PlacementKind::ProgramRanges(entries) => {
                entries.iter().enumerate().find_map(|(program, entry)| {
                    let position = entry.position(output)?;
                    let start = program.checked_sub(1).map_or(0, |p| program_ends[p]);
                    Some(start + position)
                })
            }
        }
    }
}

fn check_count(
    expected: usize,
    actual: usize,
    program: usize,
    span: Option<Span>,
) -> Result<(), SolveProblemShapeContractError> {
    validate_scalar_program_metadata_lengths(
        "ScalarProgramBlock",
        program,
        1,
        1,
        expected,
        actual,
        span,
    )
}
