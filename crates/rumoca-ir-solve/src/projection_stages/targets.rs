//! Targets derived from the same immutable ordered paired projection owner.

use super::{ProjectionBlocks, ProjectionStage};
use crate::ScalarSlot;
use serde::{Deserialize, Serialize, Serializer};

#[derive(Clone, Debug)]
pub struct ImplicitRowTargets {
    kind: TargetSource,
}

#[derive(Clone, Debug)]
enum TargetSource {
    Explicit(Vec<Option<ScalarSlot>>),
    Projection(ProjectionBlocks),
}

/// Private wire input; a projection marker is resolved against the decoded plan.
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub(crate) enum ImplicitRowTargetsWire {
    Explicit(Vec<Option<ScalarSlot>>),
    Projection,
}

impl Default for ImplicitRowTargets {
    fn default() -> Self {
        Vec::new().into()
    }
}

impl From<Vec<Option<ScalarSlot>>> for ImplicitRowTargets {
    fn from(values: Vec<Option<ScalarSlot>>) -> Self {
        Self {
            kind: TargetSource::Explicit(values),
        }
    }
}

impl ImplicitRowTargets {
    /// The exact correlated program/run input retained by this same owner.
    pub(crate) fn paired_target_run(
        &self,
        source: crate::RefreshScalarProgramSource,
        outputs: std::ops::Range<usize>,
    ) -> Option<std::ops::Range<usize>> {
        let TargetSource::Projection(blocks) = &self.kind else {
            return None;
        };
        blocks.stages().iter().find_map(|stage| {
            let ProjectionStage::SingletonDeclarationRun {
                source: owner,
                first_row,
                storage,
                ..
            } = stage
            else {
                return None;
            };
            if *owner != source
                || *first_row != outputs.start
                || storage.scalar_count != outputs.len()
            {
                return None;
            }
            let ScalarSlot::Y { index, .. } = storage.base else {
                return None;
            };
            Some(index..index.checked_add(storage.scalar_count)?)
        })
    }
    pub(crate) fn wire(&self) -> ImplicitRowTargetsWire {
        match &self.kind {
            TargetSource::Explicit(values) => ImplicitRowTargetsWire::Explicit(values.clone()),
            TargetSource::Projection(_) => ImplicitRowTargetsWire::Projection,
        }
    }
    pub fn from_projection(blocks: &ProjectionBlocks) -> Self {
        Self {
            kind: TargetSource::Projection(blocks.clone()),
        }
    }

    pub fn len(&self) -> usize {
        match &self.kind {
            TargetSource::Explicit(values) => values.len(),
            TargetSource::Projection(blocks) => blocks.row_bound(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Exact value query; it never requests a legacy scalar slice.
    pub fn target_at(&self, row: usize) -> Option<ScalarSlot> {
        match &self.kind {
            TargetSource::Explicit(values) => values.get(row).copied().flatten(),
            TargetSource::Projection(blocks) => blocks.target_at_row(row),
        }
    }

    pub fn shares_projection_owner(&self, blocks: &ProjectionBlocks) -> bool {
        matches!(&self.kind, TargetSource::Projection(source) if source.shares_stage_owner(blocks))
    }

    pub(crate) fn from_wire(wire: ImplicitRowTargetsWire, blocks: &ProjectionBlocks) -> Self {
        match wire {
            ImplicitRowTargetsWire::Explicit(values) => values.into(),
            ImplicitRowTargetsWire::Projection => Self::from_projection(blocks),
        }
    }
}

fn materialized_targets(blocks: &ProjectionBlocks) -> &Vec<Option<ScalarSlot>> {
    blocks.data.legacy_targets.get_or_init(|| {
        (0..blocks.row_bound())
            .map(|row| blocks.target_at_row(row))
            .collect()
    })
}

impl ImplicitRowTargets {
    /// Explicit scalar-consumer boundary; never canonical storage.
    pub fn scalar_targets(&self) -> &[Option<ScalarSlot>] {
        match &self.kind {
            TargetSource::Explicit(values) => values,
            TargetSource::Projection(blocks) => materialized_targets(blocks),
        }
    }
}

impl Serialize for ImplicitRowTargets {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match &self.kind {
            TargetSource::Explicit(values) => serializer.serialize_newtype_variant(
                "ImplicitRowTargetsWire",
                0,
                "Explicit",
                values,
            ),
            TargetSource::Projection(_) => {
                serializer.serialize_unit_variant("ImplicitRowTargetsWire", 1, "Projection")
            }
        }
    }
}

impl PartialEq for ImplicitRowTargets {
    fn eq(&self, other: &Self) -> bool {
        match (&self.kind, &other.kind) {
            (TargetSource::Projection(lhs), TargetSource::Projection(rhs)) => lhs == rhs,
            (TargetSource::Explicit(lhs), TargetSource::Explicit(rhs)) => lhs == rhs,
            _ => {
                self.len() == other.len()
                    && (0..self.len()).all(|row| self.target_at(row) == other.target_at(row))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{AlgebraicProjectionBlock, scalar_slot_y};

    #[test]
    fn projection_targets_materialize_one_slot_per_row_up_to_the_row_bound() {
        let blocks = ProjectionBlocks::from(vec![AlgebraicProjectionBlock {
            rows: vec![0, 2],
            y_indices: vec![5, 6],
            ..Default::default()
        }]);
        let targets = ImplicitRowTargets::from_projection(&blocks);
        assert_eq!(
            targets.scalar_targets(),
            [Some(scalar_slot_y(5)), None, Some(scalar_slot_y(6))]
        );
        assert_eq!(targets.len(), 3);
        assert_eq!(targets.target_at(2), Some(scalar_slot_y(6)));
        assert_eq!(targets.target_at(1), None);
    }
}
