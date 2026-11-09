//! Function chunks of one exact refresh assignment schedule.
//!
//! A backend emits each chunk as its own function and calls them in order, so
//! no single emitted function grows with the whole schedule. Chunks only cut
//! between complete programs: every program keeps its order, targets and
//! commit, and nothing is shared or reordered. The plan is issued here, from
//! source operation counts, so a backend prints it without a size policy.

use super::{
    ComputeBlock, ContinuousRefreshConstructionError, ExactRefreshAssignmentProgram,
    ExactRefreshAssignmentProgramId, scalar_source_program,
};

/// Source operations one chunk holds before the next program starts another.
pub const EXACT_ASSIGNMENT_CHUNK_OPS: usize = 1024;

/// Exclusive ends of consecutive program ranges, each within the operation
/// budget unless one program alone exceeds it. Empty for an empty schedule.
pub(super) fn chunk_ends(
    block: &ComputeBlock,
    programs: &[ExactRefreshAssignmentProgram],
    program_ids: &[ExactRefreshAssignmentProgramId],
) -> Result<Box<[usize]>, ContinuousRefreshConstructionError> {
    let mut operations = Vec::with_capacity(program_ids.len());
    for id in program_ids {
        let program = usize::try_from(id.0)
            .ok()
            .and_then(|index| programs.get(index))
            .ok_or_else(|| ContinuousRefreshConstructionError {
                reason: "exact assignment chunk refers to a missing program".to_string(),
            })?;
        operations.push(
            scalar_source_program(block, program.source())?
                .map_or(1, |(operations, _)| operations.len().max(1)),
        );
    }
    Ok(group_ends(operations, EXACT_ASSIGNMENT_CHUNK_OPS))
}

/// Greedy consecutive groups: a program joins the open group unless that
/// would pass `budget`, and a program larger than `budget` forms its own.
fn group_ends(operations: impl IntoIterator<Item = usize>, budget: usize) -> Box<[usize]> {
    let mut ends = Vec::new();
    let (mut held, mut count) = (0usize, 0usize);
    for operations in operations {
        if held > 0 && held.saturating_add(operations) > budget {
            ends.push(count);
            held = 0;
        }
        held = held.saturating_add(operations);
        count += 1;
    }
    if count > 0 {
        ends.push(count);
    }
    ends.into_boxed_slice()
}

#[cfg(test)]
mod tests {
    use super::group_ends;

    #[test]
    fn groups_cut_only_between_programs_and_stay_within_the_budget() {
        assert_eq!(&*group_ends([], 10), &[] as &[usize]);
        assert_eq!(&*group_ends([3, 3, 3], 10), &[3]);
        assert_eq!(&*group_ends([6, 6, 6], 10), &[1, 2, 3]);
        assert_eq!(&*group_ends([4, 6, 1, 9], 10), &[2, 3, 4]);
        // A program over the budget is its own group; nothing is split inside it.
        assert_eq!(&*group_ends([2, 25, 2], 10), &[1, 2, 3]);
    }
}
