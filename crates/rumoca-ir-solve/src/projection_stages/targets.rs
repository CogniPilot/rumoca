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

    /// The projection stage owner these targets resolve against, if any.
    pub(crate) fn projection_owner(&self) -> Option<&ProjectionBlocks> {
        match &self.kind {
            TargetSource::Projection(blocks) => Some(blocks),
            TargetSource::Explicit(_) => None,
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

    fn overlapping_blocks() -> ProjectionBlocks {
        ProjectionBlocks::from(vec![
            // Row 1 is claimed here first; row 7 pairs no Y index.
            AlgebraicProjectionBlock {
                rows: vec![0, 1, 7],
                y_indices: vec![5, 6],
                ..Default::default()
            },
            // A later claim on row 1 and a row repeated inside one block.
            AlgebraicProjectionBlock {
                rows: vec![1, 3, 3],
                y_indices: vec![7, 8, 9],
                ..Default::default()
            },
        ])
    }

    /// A projected plan that is a separate owner over a subset of the blocks,
    /// as the refresh planner's filtered value plan is.
    fn filtered_copy(blocks: &ProjectionBlocks) -> ProjectionBlocks {
        ProjectionBlocks::from(blocks.scalar_blocks()[1..].to_vec())
    }

    #[test]
    fn a_projected_owner_binds_the_canonical_targets_of_another_owner() {
        let canonical = overlapping_blocks();
        let targets = ImplicitRowTargets::from_projection(&canonical);
        for projected in [canonical.clone(), filtered_copy(&canonical)] {
            for block in projected.scalar_blocks() {
                let bound = projected
                    .bound_row_targets(block, &targets)
                    .expect("a receipt of the projected owner binds");
                let per_row: Vec<_> = block
                    .rows
                    .iter()
                    .map(|&row| targets.target_at(row))
                    .collect();
                assert_eq!(bound, per_row.as_slice());
            }
        }
        let copy = filtered_copy(&canonical);
        assert_eq!(
            copy.bound_row_targets(&copy.scalar_blocks()[0], &targets)
                .unwrap(),
            [
                Some(scalar_slot_y(6)),
                Some(scalar_slot_y(8)),
                Some(scalar_slot_y(8))
            ]
        );
    }

    #[test]
    fn a_binding_answers_only_its_own_receipts_and_first_target_owner() {
        let canonical = overlapping_blocks();
        let targets = ImplicitRowTargets::from_projection(&canonical);
        let projected = filtered_copy(&canonical);
        let equal = projected.stages().iter().find_map(|stage| match stage {
            ProjectionStage::Explicit(block) => Some(block.clone()),
            ProjectionStage::SingletonDeclarationRun { .. } => None,
        });
        // No catalog yet: nothing binds and nothing is materialized.
        assert!(
            projected
                .bound_row_targets(equal.as_ref().unwrap(), &targets)
                .is_none()
        );
        assert!(projected.data.legacy.get().is_none());
        let receipt = &projected.scalar_blocks()[0];
        assert!(projected.bound_row_targets(receipt, &targets).is_some());
        // An equal block stored elsewhere is not a receipt.
        assert_eq!(equal.as_ref(), Some(receipt));
        assert!(
            projected
                .bound_row_targets(&equal.unwrap(), &targets)
                .is_none()
        );
        // A second, equal target owner is foreign to the first binding.
        let other_canonical = overlapping_blocks();
        let other_targets = ImplicitRowTargets::from_projection(&other_canonical);
        assert!(
            projected
                .bound_row_targets(receipt, &other_targets)
                .is_none()
        );
        assert!(projected.bound_row_targets(receipt, &targets).is_some());
        // Explicit tables never bind.
        let explicit = ImplicitRowTargets::from(vec![Some(scalar_slot_y(5)); 8]);
        let fresh = filtered_copy(&canonical);
        assert!(
            fresh
                .bound_row_targets(&fresh.scalar_blocks()[0], &explicit)
                .is_none()
        );
        assert!(fresh.data.row_targets.get().is_none());
    }

    /// Eight threads query `projected` at once, alternating the two target
    /// owners; each answer says whether that query was bound.
    fn race_first_queries(
        projected: &ProjectionBlocks,
        receipt: &AlgebraicProjectionBlock,
        targets: &[ImplicitRowTargets; 2],
    ) -> Vec<bool> {
        let query = |index: usize| {
            projected
                .bound_row_targets(receipt, &targets[index % 2])
                .is_some()
        };
        std::thread::scope(|scope| {
            let handles: Vec<_> = (0..8)
                .map(|index| scope.spawn(move || query(index)))
                .collect();
            handles
                .into_iter()
                .map(|handle| handle.join().unwrap())
                .collect()
        })
    }

    #[test]
    fn racing_first_queries_settle_one_target_owner() {
        let first = overlapping_blocks();
        let second = overlapping_blocks();
        let targets = [
            ImplicitRowTargets::from_projection(&first),
            ImplicitRowTargets::from_projection(&second),
        ];
        let projected = filtered_copy(&first);
        let receipt = &projected.scalar_blocks()[0];
        let answers = race_first_queries(&projected, receipt, &targets);
        let winner = usize::from(projected.bound_row_targets(receipt, &targets[0]).is_none());
        assert!(
            projected
                .bound_row_targets(receipt, &targets[winner])
                .is_some()
        );
        assert!(
            projected
                .bound_row_targets(receipt, &targets[1 - winner])
                .is_none()
        );
        for (query, answered) in answers.into_iter().enumerate() {
            assert_eq!(answered, query % 2 == winner, "query {query}");
        }
    }

    #[test]
    fn bindings_keep_no_owner_alive() {
        // Self-binding: the projected owner is the target owner.
        let canonical = overlapping_blocks();
        let weak = std::sync::Arc::downgrade(&canonical.data);
        {
            let targets = ImplicitRowTargets::from_projection(&canonical);
            assert!(
                canonical
                    .bound_row_targets(&canonical.scalar_blocks()[0], &targets)
                    .is_some()
            );
        }
        drop(canonical);
        assert!(weak.upgrade().is_none(), "a self-binding must not cycle");
        // Two owners bound to each other.
        let (a, b) = (overlapping_blocks(), overlapping_blocks());
        let (weak_a, weak_b) = (
            std::sync::Arc::downgrade(&a.data),
            std::sync::Arc::downgrade(&b.data),
        );
        {
            let (targets_a, targets_b) = (
                ImplicitRowTargets::from_projection(&a),
                ImplicitRowTargets::from_projection(&b),
            );
            assert!(
                a.bound_row_targets(&a.scalar_blocks()[0], &targets_b)
                    .is_some()
            );
            assert!(
                b.bound_row_targets(&b.scalar_blocks()[0], &targets_a)
                    .is_some()
            );
        }
        drop(a);
        drop(b);
        assert!(weak_a.upgrade().is_none() && weak_b.upgrade().is_none());
        // A dropped target owner never matches again.
        let projected = filtered_copy(&overlapping_blocks());
        {
            let gone = overlapping_blocks();
            let targets = ImplicitRowTargets::from_projection(&gone);
            assert!(
                projected
                    .bound_row_targets(&projected.scalar_blocks()[0], &targets)
                    .is_some()
            );
        }
        let later = overlapping_blocks();
        let later_targets = ImplicitRowTargets::from_projection(&later);
        assert!(
            projected
                .bound_row_targets(&projected.scalar_blocks()[0], &later_targets)
                .is_none()
        );
    }

    #[test]
    fn decoded_owners_bind_against_decoded_targets() {
        let canonical = overlapping_blocks();
        let targets = ImplicitRowTargets::from_projection(&canonical);
        let projected = filtered_copy(&canonical);
        let before = projected.scalar_blocks()[0].clone();
        let decoded_canonical: ProjectionBlocks =
            serde_json::from_str(&serde_json::to_string(&canonical).unwrap()).unwrap();
        let decoded_targets = ImplicitRowTargets::from_wire(targets.wire(), &decoded_canonical);
        let decoded_projected: ProjectionBlocks =
            serde_json::from_str(&serde_json::to_string(&projected).unwrap()).unwrap();
        let receipt = &decoded_projected.scalar_blocks()[0];
        assert_eq!(*receipt, before);
        assert!(
            decoded_projected
                .bound_row_targets(&before, &decoded_targets)
                .is_none()
        );
        let per_row: Vec<_> = receipt
            .rows
            .iter()
            .map(|&row| decoded_targets.target_at(row))
            .collect();
        assert_eq!(
            decoded_projected
                .bound_row_targets(receipt, &decoded_targets)
                .unwrap(),
            per_row.as_slice()
        );
    }
}
