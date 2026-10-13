//! Borrow the canonical target owner without requesting a scalar target slice.

use super::*;
use crate::{ImplicitRowTargets, RefreshScalarProgramSource, ScalarSlot};

#[derive(Clone, Copy)]
pub(super) enum TargetInventory<'a> {
    Scalar(&'a [Option<ScalarSlot>]),
    Projection(&'a ImplicitRowTargets),
}

impl TargetInventory<'_> {
    pub(super) fn len(self) -> usize {
        match self {
            Self::Scalar(targets) => targets.len(),
            Self::Projection(targets) => targets.len(),
        }
    }

    pub(super) fn get(self, row: usize) -> Option<ScalarSlot> {
        match self {
            Self::Scalar(targets) => targets.get(row).copied().flatten(),
            Self::Projection(targets) => targets.target_at(row),
        }
    }

    pub(super) fn dense_run(
        self,
        source: RefreshScalarProgramSource,
        outputs: Range<usize>,
        layout: &VarLayout,
    ) -> Checked<Range<usize>> {
        if let Self::Projection(targets) = self
            && let Some(target) = targets.paired_target_run(source, outputs.clone())
        {
            return Ok(target);
        }
        if outputs.end > self.len() {
            return refused("native tensor outputs exceed the target inventory");
        }
        let index = |row| match self.get(row) {
            Some(ScalarSlot::Y { index, byte_offset })
                if index.checked_mul(8) == Some(byte_offset) =>
            {
                Some(index)
            }
            _ => None,
        };
        let start = index(outputs.start).ok_or(NativeRefreshAssignmentRefusal(
            "native tensor output has no owned Y target",
        ))?;
        let end = start
            .checked_add(outputs.len())
            .ok_or(NativeRefreshAssignmentRefusal(
                "native tensor target range overflows",
            ))?;
        if end > layout.y_scalars()
            || outputs
                .enumerate()
                .any(|(offset, row)| index(row) != start.checked_add(offset))
        {
            return refused("native tensor targets are not one bounded dense injective range");
        }
        Ok(start..end)
    }
}
