use std::collections::{BTreeMap, BTreeSet};

use super::{
    AlgebraicProjectionPlan, ContinuousRefreshConstructionError, ContinuousRefreshOwners,
    RefreshPlan, RefreshRowSelection, RefreshStage, refresh_error,
};

impl ContinuousRefreshOwners {
    pub(super) fn omit_affine_projection_seeds(&mut self) {
        for plan in [
            &mut self.algebraic,
            &mut self.derivative,
            &mut self.root,
            &mut self.event,
        ]
        .into_iter()
        .chain(self.clock_events.iter_mut())
        .chain(
            self.root_after_derivative
                .iter_mut()
                .map(|relation| &mut relation.remainder),
        )
        .chain(
            self.algebraic_after_derivative
                .iter_mut()
                .map(|relation| &mut relation.remainder),
        )
        .chain(
            self.clock_events_after_event
                .iter_mut()
                .map(|relation| &mut relation.remainder),
        ) {
            omit_affine_seeds(plan, &self.projection_affinities);
        }
    }

    pub(crate) fn validate_projection_ownership(
        &self,
        canonical: &AlgebraicProjectionPlan,
    ) -> Result<(), ContinuousRefreshConstructionError> {
        if let Some(blocks) = self.compact_projection() {
            return if blocks == &canonical.blocks {
                Ok(())
            } else {
                refresh_error("compact refresh recipe belongs to another projection".into())
            };
        }
        if !self.is_issued() {
            return Ok(());
        }
        [&self.algebraic, &self.derivative, &self.root, &self.event]
            .into_iter()
            .chain(self.clock_events.iter())
            .try_for_each(|plan| validate_projection_plan(plan, canonical))
    }

    /// Degree proof issued from the canonical algebraic block and its residuals.
    #[must_use]
    pub fn algebraic_projection_block_is_affine(&self, block_index: usize) -> bool {
        self.projection_affinities
            .get(&block_index)
            .copied()
            .unwrap_or(false)
    }
}

fn omit_affine_seeds(plan: &mut RefreshPlan, affinities: &BTreeMap<usize, bool>) {
    for stage in &mut plan.value_stages {
        if let RefreshStage::ProjectionBlock {
            block_index,
            seed_rows,
            ..
        } = stage
            && affinities.get(block_index) == Some(&true)
        {
            // The affine solve evaluates at the arithmetic origin. A scalar
            // seed cannot affect its solution and can divide by zero even
            // when the complete coupled matrix is nonsingular.
            *seed_rows = RefreshRowSelection::default();
        }
    }
}

fn validate_projection_plan(
    plan: &RefreshPlan,
    canonical: &AlgebraicProjectionPlan,
) -> Result<(), ContinuousRefreshConstructionError> {
    let mut seen = BTreeSet::new();
    for (local, &index) in plan.simultaneous_block_indices.iter().enumerate() {
        let block = plan.simultaneous_plan.blocks.scalar_block_at(local);
        if !seen.insert(index)
            || block.is_none()
            || canonical.blocks.scalar_block_at(index) != block
        {
            return refresh_error(
                "continuous refresh owner does not replay its canonical projection block"
                    .to_string(),
            );
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ProjectionBlocks, ProjectionStage, RefreshScalarProgramSource, ScalarSlot,
        SolveVariableStorageRole, SolveVariableStorageRun, SolveVariableValueKind,
    };

    #[test]
    fn scalar_refresh_replays_requested_ranged_receipt_and_rejects_changed_target() {
        let canonical = AlgebraicProjectionPlan {
            blocks: ProjectionBlocks::from_stages(vec![ProjectionStage::SingletonDeclarationRun {
                source: RefreshScalarProgramSource::checked(0, 0).unwrap(),
                first_row: 7,
                declaration: 0,
                storage: SolveVariableStorageRun {
                    base: ScalarSlot::Y {
                        index: 3,
                        byte_offset: 24,
                    },
                    scalar_count: 65536,
                    role: SolveVariableStorageRole::Algebraic,
                    value_kind: SolveVariableValueKind::Real,
                },
            }])
            .unwrap(),
        };
        let requested = canonical
            .blocks
            .scalar_block_at(65535)
            .unwrap()
            .into_owned();
        let mut plan = RefreshPlan::default();
        plan.simultaneous_plan.blocks = vec![requested.clone()].into();
        plan.simultaneous_block_indices = vec![65535];
        assert!(validate_projection_plan(&plan, &canonical).is_ok());
        let mut changed = requested;
        changed.y_indices[0] -= 1;
        plan.simultaneous_plan.blocks = vec![changed].into();
        assert!(validate_projection_plan(&plan, &canonical).is_err());
        assert_eq!(canonical.blocks.stages().len(), 1);
    }
}
