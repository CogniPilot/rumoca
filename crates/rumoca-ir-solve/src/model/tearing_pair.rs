use super::{AffineEliminationLayout, AlgebraicProjectionBlock, BlockTearing, JacobianStructure};

/// Identity of one member of the fixed, constructor-issued attempt order.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TearingCandidate {
    Guarded,
    Primary,
}

impl AlgebraicProjectionBlock {
    pub fn tearing_candidates(&self) -> impl Iterator<Item = (TearingCandidate, &BlockTearing)> {
        [
            (TearingCandidate::Guarded, self.guarded_tearing.as_ref()),
            (TearingCandidate::Primary, self.tearing.as_ref()),
        ]
        .into_iter()
        .filter_map(|(kind, plan)| plan.map(|plan| (kind, plan)))
    }

    /// Normalization can make previously distinct structural candidates equal.
    pub fn deduplicate_tearing(&mut self) {
        if self.guarded_tearing.is_some() && self.guarded_tearing == self.tearing {
            self.guarded_tearing = None;
        }
    }
}

impl JacobianStructure {
    pub fn affine_elimination_candidate(
        &self,
        candidate: TearingCandidate,
    ) -> Option<&AffineEliminationLayout> {
        match candidate {
            TearingCandidate::Guarded => self.guarded_affine_elimination.as_ref(),
            TearingCandidate::Primary => self.affine_elimination.as_ref(),
        }
    }
}

/// Shared exact block-binding checker used by wire admission and affine layout derivation.
pub(super) struct LocalTearingPartition {
    pub residuals: Box<[usize]>,
    pub tears: Box<[usize]>,
    pub causal: Box<[(usize, usize)]>,
}

impl LocalTearingPartition {
    pub(super) fn derive(block: &AlgebraicProjectionBlock, plan: &BlockTearing) -> Option<Self> {
        let n = block.rows.len();
        if n != block.y_indices.len() || plan.residual_rows.len() != plan.tear_y_indices.len() {
            return None;
        }
        let rows = local_positions(&block.rows)?;
        let columns = local_positions(&block.y_indices)?;
        let residuals = plan
            .residual_rows
            .iter()
            .map(|row| rows.get(row).copied())
            .collect::<Option<Box<[_]>>>()?;
        let tears = plan
            .tear_y_indices
            .iter()
            .map(|column| columns.get(column).copied())
            .collect::<Option<Box<[_]>>>()?;
        let causal = plan
            .causal_steps
            .iter()
            .map(|step| Some((*rows.get(&step.row)?, *columns.get(&step.y_index)?)))
            .collect::<Option<Box<[_]>>>()?;
        let mut seen_rows = vec![false; n];
        let mut seen_columns = vec![false; n];
        for (row, column) in residuals
            .iter()
            .copied()
            .zip(tears.iter().copied())
            .chain(causal.iter().copied())
        {
            claim(&mut seen_rows, row)?;
            claim(&mut seen_columns, column)?;
        }
        if !seen_rows.iter().chain(&seen_columns).all(|seen| *seen) {
            return None;
        }
        Some(Self {
            residuals,
            tears,
            causal,
        })
    }
}

impl AlgebraicProjectionBlock {
    pub fn has_valid_tearing_partitions(&self) -> bool {
        !(self.guarded_tearing.is_some() && self.guarded_tearing == self.tearing)
            && self
                .tearing_candidates()
                .all(|(_, plan)| LocalTearingPartition::derive(self, plan).is_some())
    }
}

use std::collections::BTreeMap;
fn local_positions(indices: &[usize]) -> Option<BTreeMap<usize, usize>> {
    let mut positions = BTreeMap::new();
    for (local, &source) in indices.iter().enumerate() {
        if positions.insert(source, local).is_some() {
            return None;
        }
    }
    Some(positions)
}

fn claim(seen: &mut [bool], index: usize) -> Option<()> {
    let previous = std::mem::replace(seen.get_mut(index)?, true);
    (!previous).then_some(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::CausalStep;

    fn pair() -> AlgebraicProjectionBlock {
        AlgebraicProjectionBlock {
            rows: vec![5, 8],
            y_indices: vec![3, 9],
            guarded_tearing: Some(BlockTearing {
                tear_y_indices: vec![3],
                residual_rows: vec![5],
                causal_steps: vec![CausalStep { row: 8, y_index: 9 }],
            }),
            tearing: Some(BlockTearing {
                tear_y_indices: vec![9],
                residual_rows: vec![8],
                causal_steps: vec![CausalStep { row: 5, y_index: 3 }],
            }),
            alternate_charts: Vec::new(),
        }
    }

    #[test]
    fn wire_pair_omission_duplicate_and_foreign_targets_fail_shared_admission() {
        let original = pair();
        assert!(original.has_valid_tearing_partitions());
        for (guarded, mutation) in [false, true]
            .into_iter()
            .flat_map(|guarded| (0..4).map(move |mutation| (guarded, mutation)))
        {
            let mut block = original.clone();
            let plan = if guarded {
                block.guarded_tearing.as_mut()
            } else {
                block.tearing.as_mut()
            }
            .unwrap();
            match mutation {
                0 => {
                    plan.causal_steps.clear();
                }
                1 => {
                    plan.causal_steps[0].y_index = plan.tear_y_indices[0];
                }
                2 => {
                    plan.causal_steps[0].row = 99;
                }
                _ => {
                    plan.causal_steps[0].y_index = 99;
                }
            }
            let wire = serde_json::to_string(&block).unwrap();
            let restored: AlgebraicProjectionBlock = serde_json::from_str(&wire).unwrap();
            assert!(!restored.has_valid_tearing_partitions());
            let projection = crate::AlgebraicProjectionPlan {
                blocks: vec![restored],
            };
            assert!(crate::validate_projection_plan("test wire", &projection, 100, 100).is_err());
        }
    }

    #[test]
    fn equal_candidates_are_deduplicated_without_reordering_primary() {
        let mut block = pair();
        block.guarded_tearing = block.tearing.clone();
        assert!(!block.has_valid_tearing_partitions());
        block.deduplicate_tearing();
        assert!(block.has_valid_tearing_partitions());
        assert_eq!(
            block
                .tearing_candidates()
                .map(|(kind, _)| kind)
                .collect::<Vec<_>>(),
            vec![TearingCandidate::Primary]
        );
    }
}
