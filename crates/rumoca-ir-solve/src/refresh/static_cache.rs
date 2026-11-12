//! The original global parameter-key domain shared by every refresh family.
use super::*;
use std::collections::BTreeSet;

impl ContinuousRefreshOwners {
    /// Borrowing operation construction passes this same root's source. Read
    /// complete original programs, as the scalar runtime does, not just the
    /// prefix an assignment materializes or just the derivative family.
    pub fn static_cache_parameters(
        &self,
        source: &ComputeBlock,
    ) -> Result<Box<[usize]>, ContinuousRefreshConstructionError> {
        let mut programs = BTreeSet::new();
        for plan in [&self.algebraic, &self.derivative, &self.root, &self.event]
            .into_iter()
            .chain(self.clock_events.iter())
        {
            programs.extend(
                plan.static_causal_rows()
                    .iter()
                    .map(AlgebraicRefreshRow::source),
            );
            for static_rows in plan.value_stages.iter().filter_map(static_selection) {
                programs.extend(
                    plan.selected_rows(static_rows)
                        .iter()
                        .map(AlgebraicRefreshRow::source),
                );
            }
        }
        let mut parameters = BTreeSet::new();
        for program in programs {
            let (operations, _) = scalar_source_program(source, program)?.ok_or_else(|| {
                ContinuousRefreshConstructionError {
                    reason: "global static cache requires its original complete source program"
                        .into(),
                }
            })?;
            parameters.extend(crate::program_parameter_indices(operations));
        }
        Ok(parameters.into_iter().collect())
    }
}

fn static_selection(stage: &RefreshStage) -> Option<&RefreshRowSelection> {
    match stage {
        RefreshStage::CausalSeedSweep { static_rows, .. }
        | RefreshStage::ExactAssignments { static_rows, .. } => Some(static_rows),
        RefreshStage::ProjectionBlock { .. } => None,
    }
}
