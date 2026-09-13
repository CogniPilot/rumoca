use std::collections::BTreeSet;

use super::{
    AlgebraicProjectionPlan, ContinuousRefreshConstructionError, ContinuousRefreshOwners,
    RefreshPlan, refresh_error,
};

impl ContinuousRefreshOwners {
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
