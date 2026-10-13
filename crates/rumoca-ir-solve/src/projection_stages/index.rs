//! Lookup tables fixed when a projection stage owner is constructed.

use super::ProjectionStage;
use crate::{ScalarSlot, scalar_slot_y};
use std::collections::BTreeMap;

/// A maximal run of rows whose solver-Y targets are consecutive.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct RowSegment {
    start: usize,
    len: usize,
    /// Target of `start`; `None` when the owning block pairs no Y index.
    y_start: Option<usize>,
}

impl RowSegment {
    fn end(&self) -> usize {
        self.start.saturating_add(self.len)
    }
}

/// Row-to-target and logical-block-to-stage tables of one stage owner.
///
/// Invariants, established by [`StageIndex::build`] and never mutated:
/// - `rows` is sorted by `start` and its segments are pairwise disjoint, with
///   the first stage in order owning any row several stages claim and the first
///   position owning a row repeated inside one block;
/// - `blocks` lists, in stage order, the first logical block ordinal and stage
///   ordinal of every stage that owns at least one block, so ordinals ascend.
///
/// Both lookups are binary searches over these tables; no query walks stages.
#[derive(Debug)]
pub(super) struct StageIndex {
    rows: Box<[RowSegment]>,
    blocks: Box<[(usize, usize)]>,
}

impl StageIndex {
    pub(super) fn build(stages: &[ProjectionStage]) -> Self {
        let mut claimed = BTreeMap::<usize, RowSegment>::new();
        let mut blocks = Vec::new();
        let mut ordinal = 0usize;
        for (stage_index, stage) in stages.iter().enumerate() {
            let count = stage_block_count(stage);
            for segment in stage_segments(stage) {
                claim(&mut claimed, segment);
            }
            if count > 0 {
                blocks.push((ordinal, stage_index));
            }
            ordinal = ordinal.saturating_add(count);
        }
        Self {
            rows: claimed.into_values().collect(),
            blocks: blocks.into_boxed_slice(),
        }
    }

    pub(super) fn target_at(&self, row: usize) -> Option<ScalarSlot> {
        let after = self.rows.partition_point(|segment| segment.start <= row);
        let segment = self.rows.get(after.checked_sub(1)?)?;
        let offset = row - segment.start;
        if offset >= segment.len {
            return None;
        }
        segment.y_start?.checked_add(offset).map(scalar_slot_y)
    }

    /// The stage owning logical block `index` and the block's offset inside it.
    pub(super) fn stage_of_block<'a>(
        &self,
        stages: &'a [ProjectionStage],
        index: usize,
    ) -> Option<(&'a ProjectionStage, usize)> {
        let after = self.blocks.partition_point(|&(first, _)| first <= index);
        let (first, stage_index) = *self.blocks.get(after.checked_sub(1)?)?;
        let stage = stages.get(stage_index)?;
        let count = stage_block_count(stage);
        let offset = index - first;
        (offset < count).then_some((stage, offset))
    }
}

fn stage_block_count(stage: &ProjectionStage) -> usize {
    match stage {
        ProjectionStage::Explicit(_) => 1,
        ProjectionStage::SingletonDeclarationRun { storage, .. } => storage.scalar_count,
    }
}

/// The row claims of one stage, in the stage's own position order.
fn stage_segments(stage: &ProjectionStage) -> Vec<RowSegment> {
    match stage {
        ProjectionStage::Explicit(block) => block
            .rows
            .iter()
            .enumerate()
            .map(|(position, &row)| RowSegment {
                start: row,
                len: 1,
                y_start: block.y_indices.get(position).copied(),
            })
            .collect(),
        ProjectionStage::SingletonDeclarationRun {
            first_row, storage, ..
        } => {
            let ScalarSlot::Y { index, .. } = storage.base else {
                unreachable!("construction checked ranged Y storage");
            };
            vec![RowSegment {
                start: *first_row,
                len: storage.scalar_count,
                y_start: Some(index),
            }]
        }
    }
}

/// Claim the rows of `segment` that no earlier claim owns.
fn claim(claimed: &mut BTreeMap<usize, RowSegment>, segment: RowSegment) {
    let end = segment.end();
    let mut overlapping = claimed
        .range(..end)
        .rev()
        .take_while(|(_, existing)| existing.end() > segment.start)
        .map(|(_, existing)| *existing)
        .collect::<Vec<_>>();
    overlapping.reverse();
    let mut cursor = segment.start;
    let mut pieces = Vec::new();
    for existing in overlapping {
        if existing.start > cursor {
            pieces.push((cursor, existing.start));
        }
        cursor = cursor.max(existing.end());
    }
    if cursor < end {
        pieces.push((cursor, end));
    }
    for (start, stop) in pieces {
        claimed.insert(
            start,
            RowSegment {
                start,
                len: stop - start,
                y_start: segment
                    .y_start
                    .and_then(|y| y.checked_add(start - segment.start)),
            },
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        AlgebraicProjectionBlock, ProjectionBlocks, RefreshScalarProgramSource,
        SolveVariableStorageRole, SolveVariableStorageRun, SolveVariableValueKind,
    };

    /// Reference semantics for one stage: its claim on `row`, if any.
    fn stage_claim(stage: &ProjectionStage, row: usize) -> Option<Option<ScalarSlot>> {
        match stage {
            ProjectionStage::Explicit(block) => {
                let position = block.rows.iter().position(|r| *r == row)?;
                Some(block.y_indices.get(position).copied().map(scalar_slot_y))
            }
            ProjectionStage::SingletonDeclarationRun {
                first_row, storage, ..
            } => {
                let offset = row
                    .checked_sub(*first_row)
                    .filter(|offset| *offset < storage.scalar_count)?;
                let ScalarSlot::Y { index, .. } = storage.base else {
                    unreachable!();
                };
                Some(index.checked_add(offset).map(scalar_slot_y))
            }
        }
    }

    /// The reference semantics: walk the stages in order, first claim wins.
    fn walked_target(stages: &[ProjectionStage], row: usize) -> Option<ScalarSlot> {
        stages
            .iter()
            .find_map(|stage| stage_claim(stage, row))
            .flatten()
    }

    fn block(rows: &[usize], ys: &[usize]) -> ProjectionStage {
        ProjectionStage::Explicit(AlgebraicProjectionBlock {
            rows: rows.to_vec(),
            y_indices: ys.to_vec(),
            ..Default::default()
        })
    }

    fn run(first_row: usize, base: usize, count: usize) -> ProjectionStage {
        ProjectionStage::SingletonDeclarationRun {
            source: RefreshScalarProgramSource::checked(0, 0).unwrap(),
            first_row,
            declaration: 0,
            storage: SolveVariableStorageRun {
                base: scalar_slot_y(base),
                scalar_count: count,
                role: SolveVariableStorageRole::Algebraic,
                value_kind: SolveVariableValueKind::Real,
            },
        }
    }

    #[test]
    fn indexed_lookups_equal_the_ordered_stage_walk() {
        let stages = vec![
            block(&[7, 3, 7], &[40, 41, 42]),
            run(10, 100, 5),
            block(&[12, 20, 21], &[50, 51]),
            run(0, 200, 0),
            run(11, 300, 8),
            block(&[], &[]),
            run(30, 400, 3),
        ];
        let blocks = ProjectionBlocks::from_stages(stages.clone()).unwrap();
        for row in 0..40 {
            assert_eq!(
                blocks.target_at_row(row),
                walked_target(&stages, row),
                "row {row}"
            );
        }
        // Logical block ordinals: stage 0 -> 0, run -> 1..=5, stage 2 -> 6,
        // empty run -> none, run -> 7..=14, empty block -> 15, run -> 16..=18.
        let expect_explicit = [(0, true), (6, true), (15, true), (1, false), (19, false)];
        for (ordinal, is_explicit) in expect_explicit {
            assert_eq!(
                blocks.explicit_block_at(ordinal).is_some(),
                is_explicit,
                "{ordinal}"
            );
        }
        let reference = blocks.scalar_blocks();
        assert_eq!(reference.len(), blocks.len());
        for (ordinal, expected) in reference.iter().enumerate() {
            assert_eq!(blocks.scalar_block_at(ordinal).as_deref(), Some(expected));
        }
        assert!(blocks.scalar_block_at(reference.len()).is_none());
    }
}
