//! Composition around exact native-program identity barriers.

#[cfg(test)]
mod tests;

use super::{AssignmentProgram, LinearOp, SharedValueError, SharedValueSegments};
use crate::{ScalarProgramBlock, ScalarProgramRegisterFlow};

pub(super) fn native_range(ops: &[LinearOp]) -> bool {
    for op in ops {
        if matches!(op, LinearOp::StoreOutputRange { count, .. } if *count > 1) {
            return true;
        }
    }
    false
}

/// A complete ordered partition whose native segments are exact identities.
struct CheckedPartition {
    barriers: Vec<(usize, usize)>,
    output_ends: Vec<usize>,
}

impl CheckedPartition {
    fn construct(
        shared: &SharedValueSegments,
        programs: &[AssignmentProgram<'_>],
    ) -> Result<Self, SharedValueError> {
        let mut output_ends = vec![0usize];
        for program in programs {
            let mut count = 0usize;
            for op in program.ops {
                count = count
                    .checked_add(ScalarProgramBlock::program_output_count(
                        std::slice::from_ref(op),
                    ))
                    .ok_or(SharedValueError::Unevaluable)?;
            }
            if count != program.targets.len() {
                return Err(SharedValueError::OutputCount);
            }
            let end = output_ends
                .last()
                .copied()
                .unwrap_or(0)
                .checked_add(count)
                .ok_or(SharedValueError::Unevaluable)?;
            output_ends.push(end);
        }
        let mut expected = 0;
        let mut barriers = Vec::new();
        for (index, segment) in shared.segments.iter().enumerate() {
            let end = match shared.segments.get(index + 1) {
                Some(next) => next.first_program,
                None => programs.len(),
            };
            if segment.first_program != expected || end <= expected || end > programs.len() {
                return Err(SharedValueError::Unevaluable);
            }
            if check_native_segment(segment, &programs[expected..end])? {
                barriers.push((index, end));
            }
            expected = end;
        }
        if expected != programs.len() {
            return Err(SharedValueError::Unevaluable);
        }
        Ok(Self {
            barriers,
            output_ends,
        })
    }
}

fn check_native_segment(
    segment: &super::SharedValueSegment,
    programs: &[AssignmentProgram<'_>],
) -> Result<bool, SharedValueError> {
    for source in programs {
        if !native_range(source.ops) {
            continue;
        }
        if programs.len() != 1 || !exact_identity(segment, source) {
            return Err(SharedValueError::Unevaluable);
        }
        return Ok(true);
    }
    Ok(false)
}

fn exact_identity(segment: &super::SharedValueSegment, source: &AssignmentProgram<'_>) -> bool {
    if segment.targets != source.targets || ScalarProgramRegisterFlow::derive(source.ops).is_err() {
        return false;
    }
    // The complete canonical operation wire includes register/input/output
    // identities, typed call owners, nested regions and IEEE literal bits.
    // Ordinary floating-point PartialEq cannot distinguish signed zeros.
    match (
        bincode::serialize(&segment.ops),
        bincode::serialize(source.ops),
    ) {
        (Ok(candidate), Ok(original)) => candidate == original,
        _ => false,
    }
}

impl SharedValueSegments {
    /// Prove complete output/slot equality over the checked source partition.
    /// Native programs execute unchanged; each intervening scalar interval
    /// proves equality for arbitrary incoming slots through the original
    /// symbolic checker. Their ordered composition is therefore equivalent.
    pub fn check(&self, programs: &[AssignmentProgram<'_>]) -> Result<(), SharedValueError> {
        let partition = CheckedPartition::construct(self, programs)?;
        let mut first_program = 0;
        let mut first_segment = 0;
        for (segment, end) in partition.barriers {
            check_interval(
                &programs[first_program..end - 1],
                &self.segments[first_segment..segment],
                partition.output_ends[first_program],
            )?;
            first_program = end;
            first_segment = segment + 1;
        }
        check_interval(
            &programs[first_program..],
            &self.segments[first_segment..],
            partition.output_ends[first_program],
        )
    }
}

fn check_interval(
    programs: &[AssignmentProgram<'_>],
    segments: &[super::SharedValueSegment],
    output_start: usize,
) -> Result<(), SharedValueError> {
    match SharedValueSegments::check_symbolically(programs, segments) {
        Err(SharedValueError::Output { index }) => {
            let index = output_start
                .checked_add(index)
                .ok_or(SharedValueError::Unevaluable)?;
            Err(SharedValueError::Output { index })
        }
        result => result,
    }
}
