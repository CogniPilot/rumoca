//! The complete source-derived stateless refresh, without scalar row catalogs.

use super::*;
use crate::{ProjectionBlocks, ProjectionStage, SolveProblem};

impl ContinuousRefreshOwners {
    /// Admit the complete stateless source recipe before constructing scalar
    /// refresh catalogs. Native refusals remain attached to this source owner;
    /// a scalar consumer must request its execution boundary explicitly.
    pub fn try_compact_for_problem(
        problem: &SolveProblem,
    ) -> Result<Option<Self>, ContinuousRefreshConstructionError> {
        let Some(mut owners) = Self::compact_source_recipe(problem)? else {
            return Ok(None);
        };
        owners.native_assignment = Some(native_assignment::derive_for_problem(problem));
        Ok(Some(owners))
    }

    /// Construction-only context from the finalized problem and actual table.
    pub fn try_compact_with_calls(
        problem: &SolveProblem,
        calls: &crate::SolvePureCallTable,
    ) -> Result<Option<Self>, ContinuousRefreshConstructionError> {
        let Some(mut owners) = Self::compact_source_recipe(problem)? else {
            return Ok(None);
        };
        owners.native_assignment = Some(native_assignment::derive_for_sources(problem, calls));
        Ok(Some(owners))
    }

    fn compact_source_recipe(
        problem: &SolveProblem,
    ) -> Result<Option<Self>, ContinuousRefreshConstructionError> {
        let projection = &problem.continuous.algebraic_projection_plan;
        if problem.solve_layout.state_scalar_count() != 0
            || !problem.clocks.periodic_event_schedules.is_empty()
            || !projection
                .blocks
                .stages()
                .iter()
                .any(|stage| matches!(stage, ProjectionStage::SingletonDeclarationRun { .. }))
        {
            return Ok(None);
        }
        crate::projection_stages::validate_root_inventory(problem).map_err(|error| {
            ContinuousRefreshConstructionError {
                reason: error.to_string(),
            }
        })?;
        Ok(Some(Self::compact_from_projection(&projection.blocks)))
    }

    fn compact_from_projection(blocks: &ProjectionBlocks) -> Self {
        // These plans retain the complete residual inventory. They cannot be
        // mistaken for empty scalar refreshes; scalar dispatch refuses below.
        let plan = RefreshPlan {
            simultaneous_plan: AlgebraicProjectionPlan {
                blocks: blocks.clone(),
            },
            value_projection_plan: AlgebraicProjectionPlan {
                blocks: blocks.clone(),
            },
            ..RefreshPlan::default()
        };
        Self {
            compact_projection: Some(blocks.clone()),
            algebraic: plan.clone(),
            derivative: plan.clone(),
            root: plan.clone(),
            event: plan,
            ..Self::default()
        }
    }

    pub(crate) fn checked_assignment_artifact(
        &self,
        problem: &SolveProblem,
    ) -> Result<Option<crate::CheckedAssignmentDecomposition>, &'static str> {
        if !self.assignment_source_is_eligible(problem)?
            || native_assignment::derive_for_problem(problem).is_err()
        {
            return Ok(None);
        }
        Ok(crate::CheckedAssignmentDecomposition::issue(problem))
    }

    pub(crate) fn checked_assignment_artifact_with_calls(
        &self,
        problem: &SolveProblem,
        calls: &crate::SolvePureCallTable,
    ) -> Result<Option<crate::CheckedAssignmentDecomposition>, &'static str> {
        if !self.assignment_source_is_eligible(problem)? {
            return Ok(None);
        }
        if !self
            .native_assignment_schedule()
            .is_some_and(|schedule| schedule.matches_call_table(calls))
        {
            return Err("assignment artifacts belong to another model call table");
        }
        if native_assignment::derive_for_sources(problem, calls).is_err() {
            return Ok(None);
        }
        Ok(crate::CheckedAssignmentDecomposition::issue(problem))
    }

    fn assignment_source_is_eligible(&self, problem: &SolveProblem) -> Result<bool, &'static str> {
        if !self.has_compact_source_recipe()
            || self.native_assignment_schedule().is_none()
            || problem.solve_layout.state_scalar_count() != 0
            || !problem.continuous.derivative_rhs.is_empty()
            || !problem.continuous.manifold_residual.is_empty()
            || !problem
                .continuous
                .manifold_projection_plan
                .blocks
                .is_empty()
            || problem
                .continuous
                .algebraic_projection_plan
                .blocks
                .explicit_blocks()
                .any(|block| block.y_indices.len() > 1)
        {
            return Ok(false);
        }
        self.validate_compact_root(problem)
            .map_err(|_| "assignment artifacts have a foreign compact root")?;
        crate::projection_stages::validate_root_inventory(problem)
            .map_err(|_| "assignment artifact source/target inventory is invalid")?;
        Ok(true)
    }

    pub fn has_compact_source_recipe(&self) -> bool {
        self.compact_projection.is_some()
    }

    pub(crate) fn compact_projection(&self) -> Option<&ProjectionBlocks> {
        self.compact_projection.as_ref()
    }

    pub(crate) fn from_compact_wire(
        blocks: ProjectionBlocks,
        plans: [RefreshPlan; 4],
        clock_events: Vec<RefreshPlan>,
    ) -> Result<Self, ContinuousRefreshConstructionError> {
        let expected = Self::compact_from_projection(&blocks);
        if !clock_events.is_empty()
            || plans
                != [
                    expected.algebraic.clone(),
                    expected.derivative.clone(),
                    expected.root.clone(),
                    expected.event.clone(),
                ]
        {
            return refresh_error(
                "compact refresh wire plans do not replay their source recipe".into(),
            );
        }
        Ok(expected)
    }

    pub(crate) fn validate_compact_root(
        &self,
        problem: &SolveProblem,
    ) -> Result<(), ContinuousRefreshConstructionError> {
        let Some(blocks) = &self.compact_projection else {
            return Ok(());
        };
        if problem.solve_layout.state_scalar_count() != 0
            || blocks != &problem.continuous.algebraic_projection_plan.blocks
            || !problem
                .continuous
                .implicit_row_targets
                .shares_projection_owner(blocks)
            || !problem.clocks.periodic_event_schedules.is_empty()
            || self.native_assignment.is_none()
        {
            return refresh_error(
                "compact refresh has no source-derived execution disposition".into(),
            );
        }
        Ok(())
    }

    pub(crate) fn rebind_compact_projection(&mut self, canonical: &ProjectionBlocks) {
        if self
            .compact_projection
            .as_ref()
            .is_some_and(|blocks| blocks == canonical)
        {
            self.compact_projection = Some(canonical.clone());
            for plan in [
                &mut self.algebraic,
                &mut self.derivative,
                &mut self.root,
                &mut self.event,
            ] {
                plan.simultaneous_plan.blocks = canonical.clone();
                plan.value_projection_plan.blocks = canonical.clone();
            }
        }
    }
}
