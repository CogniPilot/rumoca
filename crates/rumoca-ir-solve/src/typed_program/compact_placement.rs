//! Placement facts of a compact model-Jacobian node and their dense host
//! materialization.
//!
//! The model-Jacobian constructor issues one map per compact node; its result
//! is `[extents.., partials]` and this placement names where each point and
//! partial lands in the state Jacobian. The selected target layout decides
//! whether points are expanded; today only the dense host layout exists, and
//! it expands every placement into the one `[n, n]` aggregate.

mod receipt;
#[cfg(test)]
mod tests;
pub use receipt::CompactJacobianPlacementReceipt;

use super::{ProgramRegister, SolveProgramConstructionError, TypedProgramBuilder};
use crate::StoredStateJacobianOutputOrder;
use rumoca_core::Span;

/// `base + Σ strides[d]·k_d` over a node's 0-based ordinal domain.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CompactAffineIndex {
    pub(crate) base: i64,
    pub(crate) strides: Vec<i64>,
}

impl CompactAffineIndex {
    /// Exact `[min, max]` over the ordinal box; each term is monotone in its
    /// own ordinal, so the extremes sit at the box corners.
    fn range(&self, extents: &[u32]) -> (i128, i128) {
        let (mut low, mut high) = (i128::from(self.base), i128::from(self.base));
        for (stride, extent) in self.strides.iter().zip(extents) {
            let reach = i128::from(*stride) * (i128::from(*extent) - 1);
            if reach < 0 {
                low += reach;
            } else {
                high += reach;
            }
        }
        (low, high)
    }

    /// The index at these ordinals; `None` only if it does not fit `usize`,
    /// which `CompactJacobianPlacement::new` already rules out for its points.
    fn at(&self, ordinals: &[u32]) -> Option<usize> {
        let value = self
            .strides
            .iter()
            .zip(ordinals)
            .fold(i128::from(self.base), |sum, (stride, k)| {
                sum + i128::from(*stride) * i128::from(*k)
            });
        usize::try_from(value).ok()
    }
}

/// Where each point and partial of one compact Jacobian node lands: point `k`
/// writes row `row(k)`, and its partial `j` writes column `columns[j](k)`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompactJacobianPlacement {
    pub(crate) extents: Vec<u32>,
    pub(crate) row: CompactAffineIndex,
    pub(crate) columns: Vec<CompactAffineIndex>,
    /// The state domain every index was proven against.
    pub(crate) states: usize,
    identity: std::sync::Arc<()>,
}

impl CompactJacobianPlacement {
    /// Check a placement over `states` coordinates: a nonempty ordinal box
    /// and every row and column index of every point inside `0..states`.
    /// A node that reads no state has no partials and places nothing; its
    /// map still runs for its primal checks. Overlap between placements is
    /// refused when they are materialized.
    #[must_use]
    pub fn new(
        extents: Vec<u32>,
        row: (i64, Vec<i64>),
        columns: Vec<(i64, Vec<i64>)>,
        states: usize,
    ) -> Option<Self> {
        let index = |(base, strides): (i64, Vec<i64>)| {
            (strides.len() == extents.len()).then_some(CompactAffineIndex { base, strides })
        };
        let row = index(row)?;
        let columns = columns.into_iter().map(index).collect::<Option<Vec<_>>>()?;
        let bound = i128::try_from(states).ok()?;
        let inside = |index: &CompactAffineIndex| {
            let (low, high) = index.range(&extents);
            low >= 0 && high < bound
        };
        if extents.is_empty()
            || extents.contains(&0)
            || !inside(&row)
            || !columns.iter().all(inside)
        {
            return None;
        }
        Some(Self {
            extents,
            row,
            columns,
            states,
            identity: std::sync::Arc::new(()),
        })
    }

    /// The map result's issued shape: `[extents.., partials]`, or the
    /// scalar-per-point `[extents..]` of a node with no partials.
    #[must_use]
    pub(crate) fn value_dimensions(&self) -> Option<Vec<u32>> {
        let mut dimensions = self.extents.clone();
        if !self.columns.is_empty() {
            dimensions.push(u32::try_from(self.columns.len()).ok()?);
        }
        Some(dimensions)
    }

    /// Visit every point's 0-based ordinals in row-major order.
    fn for_each_point(
        &self,
        mut visit: impl FnMut(&[u32]) -> Result<(), SolveProgramConstructionError>,
    ) -> Result<(), SolveProgramConstructionError> {
        let mut ordinals = vec![0u32; self.extents.len()];
        loop {
            visit(&ordinals)?;
            if !advance_row_major(&mut ordinals, &self.extents) {
                return Ok(());
            }
        }
    }
}

/// Step `ordinals` to the next row-major point; `false` after the last one.
fn advance_row_major(ordinals: &mut [u32], extents: &[u32]) -> bool {
    for axis in (0..ordinals.len()).rev() {
        ordinals[axis] += 1;
        if ordinals[axis] < extents[axis] {
            return true;
        }
        ordinals[axis] = 0;
    }
    false
}

/// Dense host materialization of the state Jacobian: each compact placement
/// expands into its cells, the scalar cells keep theirs, absent cells read
/// `zero`, and the one `[n, n]` aggregate follows the order's output axes.
/// Evaluation order is unchanged; only projections and the aggregate are
/// issued here. A placement proven over another state domain, and any
/// overlapping cell, refuse. Reached only through a selected target. The
/// aggregate's scalar cells are returned with it, in its row-major order.
pub(crate) fn materialize_dense_model_jacobian<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    compact: &[(&CompactJacobianPlacement, ProgramRegister<'program>)],
    scalar: &[Option<ProgramRegister<'program>>],
    zero: ProgramRegister<'program>,
    order: &StoredStateJacobianOutputOrder<'_>,
    provenance: Span,
) -> Result<
    (ProgramRegister<'program>, Vec<ProgramRegister<'program>>),
    SolveProgramConstructionError,
> {
    let invalid = || SolveProgramConstructionError::InvalidTensorAlgebra { provenance };
    let states = order.input_labels().len();
    if scalar.len() != order.cells() || states.checked_mul(states) != Some(scalar.len()) {
        return Err(invalid());
    }
    let mut cells = scalar.to_vec();
    for (placement, value) in compact {
        if placement.states != states {
            return Err(invalid());
        }
        let dimensions = placement.value_dimensions().ok_or_else(invalid)?;
        if builder.value_type_of(*value, provenance)?.dimensions() != dimensions.as_slice() {
            return Err(invalid());
        }
        placement.for_each_point(|ordinals| {
            place_point(
                builder,
                (placement, *value),
                ordinals,
                &mut cells,
                provenance,
            )
        })?;
    }
    let mut values = Vec::with_capacity(cells.len());
    for row in order.row_coordinates() {
        for column in order.column_coordinates() {
            values.push(cells[row * states + column].unwrap_or(zero));
        }
    }
    let width = u32::try_from(states).map_err(|_| invalid())?;
    let value = builder.construct_aggregate(&values, vec![width, width], provenance)?;
    Ok((value, values))
}

/// Project one point's partials into their cells; a filled cell refuses.
fn place_point<'program>(
    builder: &mut TypedProgramBuilder<'program>,
    (placement, value): (&CompactJacobianPlacement, ProgramRegister<'program>),
    ordinals: &[u32],
    cells: &mut [Option<ProgramRegister<'program>>],
    provenance: Span,
) -> Result<(), SolveProgramConstructionError> {
    let invalid = || SolveProgramConstructionError::InvalidTensorAlgebra { provenance };
    let row = placement.row.at(ordinals).ok_or_else(invalid)?;
    for (partial, column) in placement.columns.iter().enumerate() {
        let column = column.at(ordinals).ok_or_else(invalid)?;
        let cell = &mut cells[row * placement.states + column];
        if cell.is_some() {
            return Err(invalid());
        }
        let mut indices = ordinals.to_vec();
        indices.push(u32::try_from(partial).map_err(|_| invalid())?);
        *cell = Some(builder.project_element(value, indices, provenance)?);
    }
    Ok(())
}
