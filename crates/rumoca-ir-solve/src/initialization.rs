//! Checked ownership of the complete initialization residual and its solve.

#[cfg(test)]
mod tests;

use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

use crate::{
    ComputeBlock, InitializationProjectionPlan, InitializationRowRole, ScalarProgramBlock,
    ScalarSlot, SolveProblemShapeContractError, projection_unknown_key, validate_count,
    validate_initial_projection_plan,
};

/// Inputs to one checked initialization system. Plans address the full residual,
/// whose suffix contains every retained manifold row in canonical owner order.
#[derive(Clone, Debug, Default)]
pub struct InitializationSystemInput {
    pub residual: ComputeBlock,
    pub row_roles: Vec<InitializationRowRole>,
    pub projection_plan: InitializationProjectionPlan,
    pub update_rhs: ScalarProgramBlock,
    pub update_targets: Vec<ScalarSlot>,
    pub manifold_row_count: usize,
    pub given_state_indices: Vec<usize>,
}

/// One simultaneous MLS §8.6 solve. Derived row targets and unknown inventories
/// cannot be supplied independently of the checked projection plan.
#[derive(Clone, Debug, Default, Serialize)]
pub struct InitializationSolveSystem {
    residual: ComputeBlock,
    row_roles: Vec<InitializationRowRole>,
    projection_plan: InitializationProjectionPlan,
    update_rhs: ScalarProgramBlock,
    update_targets: Vec<ScalarSlot>,
    manifold_row_count: usize,
    given_state_indices: Vec<usize>,
    #[serde(skip)]
    row_targets: Vec<Option<ScalarSlot>>,
    #[serde(skip)]
    projection_unknowns: Vec<ScalarSlot>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct InitializationSystemWire {
    residual: ComputeBlock,
    row_roles: Vec<InitializationRowRole>,
    projection_plan: InitializationProjectionPlan,
    update_rhs: ScalarProgramBlock,
    update_targets: Vec<ScalarSlot>,
    manifold_row_count: usize,
    given_state_indices: Vec<usize>,
}

impl<'de> Deserialize<'de> for InitializationSolveSystem {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let wire = InitializationSystemWire::deserialize(deserializer)?;
        Self::construct(InitializationSystemInput {
            residual: wire.residual,
            row_roles: wire.row_roles,
            projection_plan: wire.projection_plan,
            update_rhs: wire.update_rhs,
            update_targets: wire.update_targets,
            manifold_row_count: wire.manifold_row_count,
            given_state_indices: wire.given_state_indices,
        })
        .map_err(serde::de::Error::custom)
    }
}

impl InitializationSolveSystem {
    pub fn construct(
        input: InitializationSystemInput,
    ) -> Result<Self, SolveProblemShapeContractError> {
        input
            .residual
            .validate_shape_contract("initialization.residual")?;
        let count = input.residual.len()?;
        validate_count("initialization.row_roles", count, input.row_roles.len())?;
        validate_count(
            "initialization.update_targets",
            input.update_rhs.len(),
            input.update_targets.len(),
        )?;
        if input.manifold_row_count > count {
            return Err(invalid(
                "retained manifold row count exceeds the complete residual",
            ));
        }
        validate_initial_projection_plan(
            "initialization.projection_plan",
            &input.projection_plan,
            count,
            usize::MAX,
            usize::MAX,
        )?;
        let mut row_targets = vec![None; count];
        let mut projection_unknowns = Vec::new();
        for block in &input.projection_plan.blocks {
            for (&row, &unknown) in block.rows.iter().zip(&block.unknowns) {
                row_targets[row] = Some(unknown);
                projection_unknowns.push(unknown);
            }
        }
        for (target, role) in row_targets.iter().zip(&input.row_roles) {
            if target.is_some()
                != matches!(
                    role,
                    InitializationRowRole::Solved
                        | InitializationRowRole::SolvedThroughAlgebraicRefresh
                )
            {
                return Err(invalid("row role disagrees with its projection owner"));
            }
        }
        let owned: BTreeSet<_> = projection_unknowns
            .iter()
            .filter_map(|slot| projection_unknown_key(*slot))
            .collect();
        let mut updated = BTreeSet::new();
        for &slot in &input.update_targets {
            let key = projection_unknown_key(slot)
                .ok_or_else(|| invalid("initialization update has no storage slot"))?;
            if owned.contains(&key) || !updated.insert(key) {
                return Err(invalid("initialization coordinate has more than one owner"));
            }
        }
        let mut given = BTreeSet::new();
        for &index in &input.given_state_indices {
            let key = crate::ProjectionUnknownKey::Y(index);
            if owned.contains(&key) || updated.contains(&key) || !given.insert(index) {
                return Err(invalid("a given initial state cannot have another writer"));
            }
        }
        Ok(Self {
            residual: input.residual,
            row_roles: input.row_roles,
            projection_plan: input.projection_plan,
            update_rhs: input.update_rhs,
            update_targets: input.update_targets,
            manifold_row_count: input.manifold_row_count,
            given_state_indices: input.given_state_indices,
            row_targets,
            projection_unknowns,
        })
    }

    pub fn residual(&self) -> &ComputeBlock {
        &self.residual
    }
    pub fn row_roles(&self) -> &[InitializationRowRole] {
        &self.row_roles
    }
    pub fn projection_plan(&self) -> &InitializationProjectionPlan {
        &self.projection_plan
    }
    pub fn update_rhs(&self) -> &ScalarProgramBlock {
        &self.update_rhs
    }
    pub fn update_targets(&self) -> &[ScalarSlot] {
        &self.update_targets
    }
    pub fn row_targets(&self) -> &[Option<ScalarSlot>] {
        &self.row_targets
    }
    pub fn projection_unknowns(&self) -> &[ScalarSlot] {
        &self.projection_unknowns
    }
    pub fn given_state_indices(&self) -> &[usize] {
        &self.given_state_indices
    }
    pub fn manifold_row_count(&self) -> usize {
        self.manifold_row_count
    }

    /// Consume the checked owner before replacing any of its coupled inputs.
    pub fn into_input(self) -> InitializationSystemInput {
        InitializationSystemInput {
            residual: self.residual,
            row_roles: self.row_roles,
            projection_plan: self.projection_plan,
            update_rhs: self.update_rhs,
            update_targets: self.update_targets,
            manifold_row_count: self.manifold_row_count,
            given_state_indices: self.given_state_indices,
        }
    }
}

fn invalid(detail: &'static str) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::InitializationOwnership { detail }
}
