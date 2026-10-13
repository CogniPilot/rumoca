//! Ordered projection storage; ranged stages remain independent singleton SCCs.

mod coverage;
mod index;
mod targets;
pub(crate) use coverage::{compact_source_rows, validate_root_inventory};

pub use targets::ImplicitRowTargets;
pub(crate) use targets::ImplicitRowTargetsWire;

use crate::{
    AlgebraicProjectionBlock, RefreshScalarProgramSource, ScalarSlot,
    SolveProblemShapeContractError, SolveVariableStorageRun,
};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::sync::{Arc, OnceLock};

/// Construction inputs; contextual source and layout checks belong to the root.
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub enum ProjectionStage {
    Explicit(AlgebraicProjectionBlock),
    SingletonDeclarationRun {
        source: RefreshScalarProgramSource,
        first_row: usize,
        declaration: usize,
        storage: SolveVariableStorageRun,
    },
}

impl ProjectionStage {
    /// Find a complete canonical scalar program by its actual output placement.
    /// This lookup grants no pairing certificate; `checked_native_run` binds it.
    pub fn source_for_row_run(
        source: &crate::ComputeBlock,
        rows: std::ops::Range<usize>,
    ) -> Result<Option<RefreshScalarProgramSource>, SolveProblemShapeContractError> {
        if compact_source_rows(source)?.is_none() {
            return Ok(None);
        }
        crate::refresh::projection_run_source(source, rows)
    }
    /// Bind this run to the actual source and canonical declaration layout.
    /// The value kernel is issued by the existing native ranged isolator.
    pub fn checked_native_run(
        &self,
        source: &crate::ComputeBlock,
        declarations: &crate::SolveLayout,
        layout: &crate::VarLayout,
    ) -> Result<Option<crate::NativeRefreshAssignmentStage>, crate::NativeRefreshAssignmentRefusal>
    {
        match self {
            Self::Explicit(_) => Ok(None),
            Self::SingletonDeclarationRun { .. } => {
                crate::refresh::derive_projection_run(self, source, declarations, layout).map(Some)
            }
        }
    }
}

#[derive(Debug)]
struct ProjectionStageData {
    stages: Box<[ProjectionStage]>,
    block_count: usize,
    row_bound: usize,
    legacy: OnceLock<Vec<AlgebraicProjectionBlock>>,
    legacy_targets: OnceLock<Vec<Option<ScalarSlot>>>,
    /// Fixed with `stages`; every constructor derives it from them.
    index: index::StageIndex,
}

/// One canonical ordered source of blocks and correlated target claims.
#[derive(Clone, Debug)]
pub struct ProjectionBlocks {
    data: Arc<ProjectionStageData>,
}

impl Default for ProjectionBlocks {
    fn default() -> Self {
        Vec::<AlgebraicProjectionBlock>::new().into()
    }
}

impl PartialEq for ProjectionBlocks {
    fn eq(&self, other: &Self) -> bool {
        self.data.stages == other.data.stages
    }
}

impl Eq for ProjectionBlocks {}

impl From<Vec<AlgebraicProjectionBlock>> for ProjectionBlocks {
    fn from(blocks: Vec<AlgebraicProjectionBlock>) -> Self {
        let row_bound = blocks
            .iter()
            .flat_map(|block| &block.rows)
            .fold(0, |bound, row| bound.max(row.saturating_add(1)));
        let block_count = blocks.len();
        let stages: Box<[ProjectionStage]> =
            blocks.into_iter().map(ProjectionStage::Explicit).collect();
        Self {
            data: Arc::new(ProjectionStageData {
                block_count,
                row_bound,
                index: index::StageIndex::build(&stages),
                stages,
                legacy: OnceLock::new(),
                legacy_targets: OnceLock::new(),
            }),
        }
    }
}

impl ProjectionBlocks {
    pub fn from_stages(
        stages: Vec<ProjectionStage>,
    ) -> Result<Self, SolveProblemShapeContractError> {
        let mut block_count = 0usize;
        let mut row_bound = 0usize;
        for stage in &stages {
            let (count, bound) = stage_bounds(stage)?;
            block_count = block_count.checked_add(count).ok_or_else(stage_overflow)?;
            row_bound = row_bound.max(bound);
        }
        Ok(Self {
            data: Arc::new(ProjectionStageData {
                index: index::StageIndex::build(&stages),
                stages: stages.into_boxed_slice(),
                block_count,
                row_bound,
                legacy: OnceLock::new(),
                legacy_targets: OnceLock::new(),
            }),
        })
    }

    pub fn explicit_blocks(&self) -> impl Iterator<Item = &AlgebraicProjectionBlock> {
        self.stages().iter().filter_map(|stage| match stage {
            ProjectionStage::Explicit(block) => Some(block),
            ProjectionStage::SingletonDeclarationRun { .. } => None,
        })
    }

    /// Consumes this input owner for a checked derived construction.
    pub fn into_stage_inputs(self) -> Vec<ProjectionStage> {
        match Arc::try_unwrap(self.data) {
            Ok(data) => data.stages.into_vec(),
            Err(data) => data.stages.to_vec(),
        }
    }

    pub fn stages(&self) -> &[ProjectionStage] {
        &self.data.stages
    }

    /// Rewrite only explicit coupled-block drafts, retaining every ranged owner.
    /// Borrow an authored coupled block at its original logical block index.
    /// Independent declaration runs deliberately have no scalar block receipt.
    pub fn explicit_block_at(&self, index: usize) -> Option<&AlgebraicProjectionBlock> {
        match self.data.index.stage_of_block(self.stages(), index)? {
            (ProjectionStage::Explicit(block), _) => Some(block),
            (ProjectionStage::SingletonDeclarationRun { .. }, _) => None,
        }
    }

    /// One scalar execution receipt derived from this canonical stage owner.
    /// Ranged stages allocate only the requested singleton, never a block catalog.
    pub(crate) fn scalar_block_at(
        &self,
        index: usize,
    ) -> Option<std::borrow::Cow<'_, AlgebraicProjectionBlock>> {
        match self.data.index.stage_of_block(self.stages(), index)? {
            (ProjectionStage::Explicit(block), _) => Some(std::borrow::Cow::Borrowed(block)),
            (
                ProjectionStage::SingletonDeclarationRun {
                    first_row, storage, ..
                },
                offset,
            ) => singleton_block(*first_row, *storage, offset).map(std::borrow::Cow::Owned),
        }
    }

    pub fn map_explicit_blocks<E>(
        &self,
        mut transform: impl FnMut(&mut AlgebraicProjectionBlock) -> Result<(), E>,
    ) -> Result<Self, E> {
        let mut stages = self.stages().to_vec();
        for stage in &mut stages {
            if let ProjectionStage::Explicit(block) = stage {
                transform(block)?;
            }
        }
        Ok(Self {
            data: Arc::new(ProjectionStageData {
                block_count: self.len(),
                row_bound: self.row_bound(),
                index: index::StageIndex::build(&stages),
                stages: stages.into_boxed_slice(),
                legacy: OnceLock::new(),
                legacy_targets: OnceLock::new(),
            }),
        })
    }

    pub fn len(&self) -> usize {
        self.data.block_count
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Solver-Y target of `row` from the construction-time index.
    pub(crate) fn target_at_row(&self, row: usize) -> Option<ScalarSlot> {
        self.data.index.target_at(row)
    }

    pub fn row_bound(&self) -> usize {
        self.data.row_bound
    }

    pub fn shares_stage_owner(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.data, &other.data)
    }

    pub fn scalar_blocks(&self) -> &[AlgebraicProjectionBlock] {
        self.scalar_vec()
    }

    fn scalar_vec(&self) -> &Vec<AlgebraicProjectionBlock> {
        self.data.legacy.get_or_init(|| {
            let mut blocks = Vec::with_capacity(self.len());
            for stage in self.stages() {
                append_scalar_stage(stage, &mut blocks);
            }
            blocks
        })
    }
}

impl Serialize for ProjectionBlocks {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.stages().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ProjectionBlocks {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let stages = Vec::<ProjectionStage>::deserialize(deserializer)?;
        Self::from_stages(stages).map_err(serde::de::Error::custom)
    }
}

fn stage_error(detail: &str) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::ContinuousRefreshOwner {
        detail: detail.into(),
    }
}

fn stage_overflow() -> SolveProblemShapeContractError {
    stage_error("projection stage ordinal overflows host range")
}

fn stage_bounds(stage: &ProjectionStage) -> Result<(usize, usize), SolveProblemShapeContractError> {
    Ok(match stage {
        ProjectionStage::Explicit(block) => (
            1,
            block.rows.iter().try_fold(0usize, |bound, row| {
                row.checked_add(1)
                    .map(|end| bound.max(end))
                    .ok_or_else(stage_overflow)
            })?,
        ),
        ProjectionStage::SingletonDeclarationRun {
            first_row, storage, ..
        } => {
            let ScalarSlot::Y { index, .. } = storage.base else {
                return Err(stage_error("singleton declaration run requires Y storage"));
            };
            index
                .checked_add(storage.scalar_count)
                .ok_or_else(stage_overflow)?;
            (
                storage.scalar_count,
                first_row
                    .checked_add(storage.scalar_count)
                    .ok_or_else(stage_overflow)?,
            )
        }
    })
}

fn singleton_block(
    first_row: usize,
    storage: SolveVariableStorageRun,
    offset: usize,
) -> Option<AlgebraicProjectionBlock> {
    let ScalarSlot::Y { index: base, .. } = storage.base else {
        return None;
    };
    Some(AlgebraicProjectionBlock {
        rows: vec![first_row.checked_add(offset)?],
        y_indices: vec![base.checked_add(offset)?],
        tearing: None,
        alternate_charts: Vec::new(),
    })
}

fn append_scalar_stage(stage: &ProjectionStage, blocks: &mut Vec<AlgebraicProjectionBlock>) {
    match stage {
        ProjectionStage::Explicit(block) => blocks.push(block.clone()),
        ProjectionStage::SingletonDeclarationRun {
            first_row, storage, ..
        } => {
            let ScalarSlot::Y { index, .. } = storage.base else {
                unreachable!("construction checked singleton storage");
            };
            blocks.extend(
                (0..storage.scalar_count).map(|offset| AlgebraicProjectionBlock {
                    rows: vec![first_row + offset],
                    y_indices: vec![index + offset],
                    tearing: None,
                    alternate_charts: Vec::new(),
                }),
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_row_past_the_host_range_has_no_projection_stage_bound() {
        let block = AlgebraicProjectionBlock {
            rows: vec![usize::MAX],
            ..Default::default()
        };
        let error = ProjectionBlocks::from_stages(vec![ProjectionStage::Explicit(block)])
            .expect_err("the row bound of usize::MAX overflows");
        assert!(
            error
                .to_string()
                .contains("projection stage ordinal overflows host range")
        );
    }
}
