//! Effects of a chart's issued continuous refresh and its event history.
use std::collections::BTreeSet;

use super::{RefreshPlan, StagedRefreshStep};
use crate::{ContinuousStructuralArtifacts, PreParamSource, SolveLayout, SolveProblem};

/// Solver coordinates an assignment-only refresh can write, grouped into contiguous ranges.
pub struct DirectRefreshEffects {
    runs: Vec<[usize; 2]>,
    history_independent: bool,
}

impl DirectRefreshEffects {
    /// Each pair is the first coordinate and the number of coordinates to save.
    pub fn rollback_runs(&self) -> &[[usize; 2]] {
        &self.runs
    }
}

impl SolveProblem {
    /// Certify the rollback set of this problem's own refresh. Partial projections and
    /// uncertified fallback paths require preserving the full incoming state.
    pub fn direct_refresh_effects(
        &self,
        plan: &RefreshPlan,
        structural: &ContinuousStructuralArtifacts,
    ) -> Option<DirectRefreshEffects> {
        let owners = &self.continuous.refresh_owners;
        if !std::ptr::eq(plan, owners.algebraic()) && !std::ptr::eq(plan, owners.derivative()) {
            return None;
        }
        let steps = owners
            .staged_refresh_steps(plan, plan.stage_schedule(structural))
            .ok()?;
        if !steps.is_empty() && !plan.causal_solution_certified {
            return None;
        }
        let history = self.solve_layout.pre_destinations();
        let mut history_independent = true;
        let mut targets = BTreeSet::new();
        for step in steps {
            let StagedRefreshStep::Assignments(schedule) = step else {
                return None;
            };
            for id in schedule.program_ids() {
                let program = owners.exact_assignment_program(*id)?;
                targets.extend(program.target_indices().iter().copied());
                if history_independent && !history.is_empty() {
                    let bound = program.bind_source(&self.continuous.implicit_rhs).ok()?;
                    history_independent =
                        crate::program_parameter_indices(bound.source_prefix().ok()?)
                            .iter()
                            .all(|index| !history.contains(index));
                }
            }
        }
        let mut runs: Vec<[usize; 2]> = Vec::new();
        for target in targets {
            if let Some([first, len]) = runs.last_mut()
                && target - *first == *len
            {
                *len += 1;
            } else {
                runs.push([target, 1]);
            }
        }
        Some(DirectRefreshEffects {
            runs,
            history_independent,
        })
    }

    /// The integrator has settled this chart and subsequently advanced only pre() lanes.
    /// The caller must invalidate the settled state at an event, input write or new time/state.
    pub fn projection_valid_after_history_copy(
        &self,
        structural: &ContinuousStructuralArtifacts,
    ) -> bool {
        let owners = &self.continuous.refresh_owners;
        !self
            .continuous
            .reduced_chart_set
            .charts
            .iter()
            .any(|chart| chart.plan.is_some())
            && self
                .direct_refresh_effects(owners.algebraic(), structural)
                .is_some_and(|effects| effects.history_independent)
    }
}

impl SolveLayout {
    fn pre_destinations(&self) -> BTreeSet<usize> {
        self.pre_param_bindings
            .iter()
            .map(|binding| binding.dest_p_index)
            .collect()
    }

    /// Between events and external writes, repeated copies of unchanged P sources are
    /// idempotent only if none is overwritten by another history binding.
    pub fn repeatable_parameter_history(&self) -> bool {
        let targets = self.pre_destinations();
        !targets.is_empty() && self.pre_param_bindings.iter().all(|binding| {
            matches!(binding.source, PreParamSource::P { index } if !targets.contains(&index))
        })
    }
}
