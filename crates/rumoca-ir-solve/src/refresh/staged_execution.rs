//! Ordered executable reading of one continuous value refresh.
//!
//! The linked ME kernel executes a refresh plan through its evaluator; a
//! generated component executes the same plan as straight-line code. Both read
//! the plan through this one checked projection, which follows the evaluator's
//! dispatch exactly: an empty plan does nothing, a certified causal solution
//! replays its causal seed rows, certified value stages run in order, and any
//! other plan replays its causal seed rows as a warm start and then projects
//! the complete simultaneous plan.

use super::{
    AlgebraicRefreshRow, ContinuousRefreshOwners, ExactAssignmentCoverage,
    ExactRefreshAssignmentSchedule, RefreshPlan, RefreshRowSelection, RefreshSequenceId,
    RefreshStage,
};
use crate::ContinuousStructuralArtifacts;

/// One step of a continuous value refresh as an executable backend reads it.
///
/// A non-finite value stored by an `Assignments` step, or by the seeds of a
/// `Project` step that its block projection then cannot settle, restores the
/// incoming coordinate and projects the complete simultaneous plan instead of
/// the remaining steps.
#[derive(Clone, Copy, Debug)]
pub enum StagedRefreshStep<'a> {
    /// Execute the programs of this exact-assignment schedule in order.
    Assignments(&'a ExactRefreshAssignmentSchedule),
    /// Evaluate the optional seed schedule, then project this canonical
    /// algebraic projection block.
    Project {
        block_index: usize,
        seeds: Option<&'a ExactRefreshAssignmentSchedule>,
        /// The seed rows, whose targets [`projection_seed_rescue_targets`]
        /// restores when a seed is unavailable.
        seed_rows: &'a RefreshRowSelection,
    },
    /// Project the complete simultaneous plan from the current coordinate.
    ProjectComplete,
}

/// Which dispatch the evaluator takes for a plan with a residual system.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RefreshStageSchedule {
    /// Construction certified that the ordered value stages settle the plan.
    Certified,
    /// The causal seed rows only warm-start the complete simultaneous plan.
    Uncertified,
}

impl RefreshPlan {
    /// Every dispatch the evaluator can take for this plan under some
    /// artifacts: an admissibility check without artifacts accepts only a plan
    /// executable under each of them. The artifact-independent part of the
    /// certificate already excludes the certified dispatch here.
    #[must_use]
    pub fn possible_stage_schedules(&self) -> &'static [RefreshStageSchedule] {
        if self.value_stages.is_empty()
            || self.simultaneous_block_indices.len() != self.simultaneous_plan.blocks.len()
        {
            &[RefreshStageSchedule::Uncertified]
        } else {
            &[
                RefreshStageSchedule::Certified,
                RefreshStageSchedule::Uncertified,
            ]
        }
    }

    /// The dispatch the evaluator takes for this plan under `structural`.
    #[must_use]
    pub fn stage_schedule(
        &self,
        structural: &ContinuousStructuralArtifacts,
    ) -> RefreshStageSchedule {
        if self.value_stage_schedule_is_certified(structural) {
            RefreshStageSchedule::Certified
        } else {
            RefreshStageSchedule::Uncertified
        }
    }
}

/// Why a refresh plan has no straight-line executable form.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StagedRefreshRefusal {
    /// A stage selects rows that no issued exact-assignment schedule replays
    /// exactly, or a projection stage does not own exactly one block.
    NonExactStage,
    /// The ordered stages leave a coordinate of the plan's inventory unsettled.
    IncompleteCoverage,
}

impl ContinuousRefreshOwners {
    /// The exact ordered steps that execute `plan` under `schedule`.
    ///
    /// An executor MUST pass [`RefreshPlan::stage_schedule`] of the artifacts
    /// it executes with; an admissibility gate without artifacts checks every
    /// entry of [`RefreshPlan::possible_stage_schedules`].
    pub fn staged_refresh_steps<'a>(
        &'a self,
        plan: &'a RefreshPlan,
        schedule: RefreshStageSchedule,
    ) -> Result<Vec<StagedRefreshStep<'a>>, StagedRefreshRefusal> {
        if plan.rows.is_empty() && plan.simultaneous_plan.is_empty() {
            return Ok(Vec::new());
        }
        if plan.causal_solution_certified || schedule == RefreshStageSchedule::Uncertified {
            let mut steps = Vec::new();
            self.append_assignments(
                plan,
                [
                    (plan.static_causal_sequence, &plan.static_causal_seed_rows),
                    (plan.dynamic_causal_sequence, &plan.dynamic_causal_seed_rows),
                ],
                &mut steps,
            )?;
            if !plan.causal_solution_certified {
                steps.push(StagedRefreshStep::ProjectComplete);
            }
            return Ok(steps);
        }
        let mut coverage = ExactAssignmentCoverage::new(
            plan,
            &self.exact_assignment_programs,
            &self.exact_assignment_schedules,
        );
        let mut steps = Vec::with_capacity(plan.value_stages.len());
        for stage in &plan.value_stages {
            if !coverage.mark_stage(stage) {
                return Err(StagedRefreshRefusal::NonExactStage);
            }
            self.append_stage_steps(plan, stage, &mut steps)?;
        }
        if coverage.available == coverage.target_inventory {
            Ok(steps)
        } else {
            Err(StagedRefreshRefusal::IncompleteCoverage)
        }
    }

    fn append_stage_steps<'a>(
        &'a self,
        plan: &'a RefreshPlan,
        stage: &'a RefreshStage,
        steps: &mut Vec<StagedRefreshStep<'a>>,
    ) -> Result<(), StagedRefreshRefusal> {
        match stage {
            RefreshStage::CausalSeedSweep {
                static_sequence,
                dynamic_sequence,
                static_rows,
                dynamic_rows,
            }
            | RefreshStage::ExactAssignments {
                static_sequence,
                dynamic_sequence,
                static_rows,
                dynamic_rows,
            } => self.append_assignments(
                plan,
                [
                    (*static_sequence, static_rows),
                    (*dynamic_sequence, dynamic_rows),
                ],
                steps,
            )?,
            RefreshStage::ProjectionBlock {
                seed_sequence,
                block_index,
                plan: block_plan,
                seed_rows,
            } => {
                if block_plan.blocks.len() != 1 {
                    return Err(StagedRefreshRefusal::NonExactStage);
                }
                steps.push(StagedRefreshStep::Project {
                    block_index: *block_index,
                    seeds: self.replaying_schedule(plan, *seed_sequence, seed_rows)?,
                    seed_rows,
                });
            }
        }
        Ok(())
    }

    /// Append the schedules replaying the static then dynamic selections.
    fn append_assignments<'a>(
        &'a self,
        plan: &RefreshPlan,
        selections: [(RefreshSequenceId, &RefreshRowSelection); 2],
        steps: &mut Vec<StagedRefreshStep<'a>>,
    ) -> Result<(), StagedRefreshRefusal> {
        for (sequence, selection) in selections {
            if let Some(schedule) = self.replaying_schedule(plan, sequence, selection)? {
                steps.push(StagedRefreshStep::Assignments(schedule));
            }
        }
        Ok(())
    }

    /// The schedule replaying exactly `selection` in order: `Ok(None)` for an
    /// empty selection.
    fn replaying_schedule<'a>(
        &'a self,
        plan: &RefreshPlan,
        sequence: RefreshSequenceId,
        selection: &RefreshRowSelection,
    ) -> Result<Option<&'a ExactRefreshAssignmentSchedule>, StagedRefreshRefusal> {
        if selection.is_empty() {
            return Ok(None);
        }
        let schedule = self
            .exact_assignment_schedule(sequence)
            .ok_or(StagedRefreshRefusal::NonExactStage)?;
        let mut replayed = Vec::with_capacity(selection.len());
        for id in schedule.program_ids() {
            let program = self
                .exact_assignment_program(*id)
                .ok_or(StagedRefreshRefusal::NonExactStage)?;
            replayed.extend_from_slice(program.row_owners());
        }
        let expected = plan
            .selected_rows(selection)
            .iter()
            .map(AlgebraicRefreshRow::owner_id)
            .collect::<Vec<_>>();
        if replayed == expected {
            Ok(Some(schedule))
        } else {
            Err(StagedRefreshRefusal::NonExactStage)
        }
    }
}

/// The coordinates a failed projection-stage seed restores before the stage
/// projects its own block: every seed target and every block unknown.
///
/// No earlier stage writes either set, so restoring them from the refresh's
/// incoming coordinate recovers exactly the pre-stage state while every
/// certified earlier stage keeps its values. The linked ME kernel and every
/// generated component rescue a failed seed through this one set, and fall
/// back to the complete simultaneous projection only when the block's own
/// projection then fails.
#[must_use]
pub fn projection_seed_rescue_targets(
    seed_rows: super::RefreshRows<'_>,
    block: &crate::AlgebraicProjectionBlock,
) -> Vec<usize> {
    let mut targets = seed_rows
        .iter()
        .map(AlgebraicRefreshRow::target_index)
        .chain(block.y_indices.iter().copied())
        .collect::<Vec<_>>();
    targets.sort_unstable();
    targets.dedup();
    targets
}
