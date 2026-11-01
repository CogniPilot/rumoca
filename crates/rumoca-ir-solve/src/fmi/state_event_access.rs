//! Source-bound FMI 3 Event Mode state access.
//!
//! A state can be set in Event Mode only when its declaration does not carry
//! `reinit`. Derive that fact from the same retained event targets that execute
//! state updates, once when the C profile is constructed. An aggregate state
//! reference carries `reinit` if any of its scalar elements may be reset.

use crate::{ScalarSlot, SolveProblem, SolveVariableStorageRole};

use super::{FmiStorageColumn, FmiVariable};

pub(super) fn derive(problem: &SolveProblem, variables: &[FmiVariable]) -> Vec<bool> {
    variables
        .iter()
        .map(|variable| {
            if variable.role() != Some(SolveVariableStorageRole::State) {
                return false;
            }
            let Some(storage) = variable.storage() else {
                return true;
            };
            if storage.column() != FmiStorageColumn::Y {
                return true;
            }
            state_run_may_reset(problem, storage.base(), storage.scalar_count())
        })
        .collect()
}

fn state_run_may_reset(problem: &SolveProblem, base: usize, count: usize) -> bool {
    let continuous = &problem.continuous;
    // A chart transfer can replace the exposed state basis without a discrete
    // assignment target. Such state changes are owned by the retained chart
    // and manifold plans, so target-only absence cannot authorize a write.
    if !continuous.manifold_projection_plan.is_empty()
        || continuous
            .reduced_chart_set
            .charts
            .iter()
            .any(|chart| chart.plan.is_some())
        || continuous
            .algebraic_projection_plan
            .blocks
            .scalar_blocks()
            .iter()
            .any(|block| !block.alternate_charts.is_empty())
    {
        return true;
    }
    let discrete = &problem.discrete;
    // The current C profile refuses these owners. Keep state access closed
    // if a future profile admits one without also exposing its exact targets.
    if !discrete.event_transactions.is_empty() || !discrete.structured_updates.is_empty() {
        return true;
    }
    let scalar_targets = discrete
        .update_targets
        .iter()
        .chain(&discrete.runtime_assignment_targets)
        .chain(&discrete.post_commit_assignment_targets)
        .chain(problem.events.root_relation_memory_targets.iter().flatten());
    scalar_targets
        .copied()
        .any(|target| overlaps(target, 1, base, count))
        || discrete.guarded_assignments.iter().any(|owner| {
            owner
                .target_ranges()
                .iter()
                .any(|range| overlaps(range.base(), range.count(), base, count))
        })
}

fn overlaps(target: ScalarSlot, length: usize, base: usize, count: usize) -> bool {
    let ScalarSlot::Y { index, .. } = target else {
        return false;
    };
    if length == 0 || count == 0 {
        return false;
    }
    let (Some(end), Some(state_end)) = (index.checked_add(length), base.checked_add(count)) else {
        return true;
    };
    index < state_end && base < end
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scalar_and_partial_array_state_writes_close_the_whole_reference() {
        let mut problem = SolveProblem::default();
        assert!(!state_run_may_reset(&problem, 3, 2));
        problem.discrete.runtime_assignment_targets = vec![ScalarSlot::Y {
            index: 4,
            byte_offset: 32,
        }];
        assert!(state_run_may_reset(&problem, 3, 2));
        assert!(!state_run_may_reset(&problem, 5, 2));
        problem.discrete.runtime_assignment_targets.clear();
        problem.discrete.post_commit_assignment_targets = vec![ScalarSlot::Y {
            index: 3,
            byte_offset: 24,
        }];
        assert!(state_run_may_reset(&problem, 3, 2));
    }

    #[test]
    fn parameter_and_time_targets_do_not_reset_a_state() {
        let mut problem = SolveProblem::default();
        problem.discrete.runtime_assignment_targets = vec![ScalarSlot::P {
            index: 3,
            byte_offset: 24,
        }];
        assert!(!state_run_may_reset(&problem, 3, 2));
        assert!(!overlaps(ScalarSlot::Time, 1, 0, 2));
    }

    #[test]
    fn a_retained_basis_transfer_is_not_hidden_by_empty_discrete_targets() {
        let mut problem = SolveProblem::default();
        assert!(!state_run_may_reset(&problem, 0, 1));
        problem
            .continuous
            .reduced_chart_set
            .charts
            .push(crate::ReducedChart {
                plan: Some(crate::ReducedChartPlan::default()),
                ..Default::default()
            });
        assert!(state_run_may_reset(&problem, 0, 1));
        problem.continuous.reduced_chart_set.charts.clear();
        problem.continuous.manifold_projection_plan.blocks =
            vec![crate::AlgebraicProjectionBlock::default()].into();
        assert!(state_run_may_reset(&problem, 0, 1));
    }

    #[test]
    fn ranges_keep_exact_boundaries_and_refuse_overflow() {
        let slot = |index| ScalarSlot::Y {
            index,
            byte_offset: 0,
        };
        assert!(!overlaps(slot(1), 2, 3, 2));
        assert!(overlaps(slot(2), 2, 3, 2));
        assert!(overlaps(slot(4), 2, 3, 2));
        assert!(!overlaps(slot(5), 2, 3, 2));
        assert!(!overlaps(slot(3), 0, 3, 2));
        assert!(!overlaps(slot(3), 1, 3, 0));
        assert!(overlaps(slot(usize::MAX), 1, 3, 2));
        assert!(overlaps(slot(3), 1, usize::MAX, 1));
    }
}
