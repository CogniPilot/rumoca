//! Execution dependencies admit stages in the issued order; they never reorder.
use super::*;

pub(super) fn refresh_row_execution_dependency_positions<'ops>(
    row: &AlgebraicRefreshRow,
    operations: &'ops [solve::LinearOp],
    state_count: usize,
    targets: &BTreeMap<usize, usize>,
    cache: &mut RowDependencyCache<'ops>,
) -> Vec<usize> {
    let mut positions =
        refresh_row_dependency_positions(row, operations, state_count, targets, cache)
            .into_iter()
            .collect::<BTreeSet<_>>();
    let prefix_len = row.assignment_shape().map_or(
        operations.len(),
        solve::TargetAssignmentShape::expr_eval_len,
    );
    let intervals = cache
        .execution
        .entry(RowDependencyCache::key(operations))
        .or_insert_with(|| solve::ScalarProgramExecutionYDependency::new(operations))
        .prefix(prefix_len)
        .map(|reads| reads.intervals().collect::<Vec<_>>());
    // A prefix whose execution the walk refuses reads every loaded coordinate.
    let intervals = intervals.unwrap_or_else(|| {
        cache
            .input_ranges(operations)
            .iter()
            .filter(|range| !range.is_empty())
            .map(|range| range.start..=range.end - 1)
            .collect()
    });
    let row_target = row.target_index();
    for interval in intervals {
        positions.extend(
            targets
                .range(interval)
                .filter(|(target, _)| **target >= state_count && **target != row_target)
                .map(|(_, position)| *position),
        );
    }
    positions.into_iter().collect()
}
