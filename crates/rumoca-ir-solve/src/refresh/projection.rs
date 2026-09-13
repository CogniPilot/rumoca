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
    for (&index, block) in plan
        .simultaneous_block_indices
        .iter()
        .zip(&plan.simultaneous_plan.blocks)
    {
        if !seen.insert(index) || canonical.blocks.get(index) != Some(block) {
            return refresh_error(
                "continuous refresh owner does not replay its canonical projection block"
                    .to_string(),
            );
        }
    }
    Ok(())
}
