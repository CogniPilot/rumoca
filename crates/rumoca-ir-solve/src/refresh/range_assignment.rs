//! Native ranged assignments issued from canonical ranged residual stores.

use super::{AlgebraicRefreshRow, ScalarProgramOutputStores};
use crate::{LinearOp, Reg, ScalarProgramOutputSpan, TargetAssignmentShape};

#[derive(Clone, Debug)]
pub(super) enum ExactAssignmentStore {
    Scalar(usize),
    Range {
        start: Reg,
        stride: usize,
        targets: ScalarProgramOutputSpan,
    },
}

/// Preserve the source's ranged-store boundaries and the issued row order.
/// Rows were already checked against their canonical output and isolator;
/// this construction proves one ranged value and one output span reproduce
/// those exact Direct assignments. Other shapes retain their scalar view.
pub(super) fn construct(
    source: &[LinearOp],
    rows: &[&AlgebraicRefreshRow],
    shapes: &[TargetAssignmentShape],
) -> Box<[ExactAssignmentStore]> {
    let stores = ScalarProgramOutputStores::new(source);
    let mut issued = Vec::new();
    let mut first = 0;
    while first < rows.len() {
        let range = stores
            .as_ref()
            .and_then(|stores| stores.range(rows[first].output_offset));
        let native = range.and_then(|range| {
            let mut end = first + 1;
            while end < rows.len()
                && rows[end].output_offset < range.end
                && rows[end].output_offset >= range.first
                && rows[end].output_offset == rows[end - 1].output_offset + 1
            {
                end += 1;
            }
            native_range(
                source,
                range.position,
                &rows[first..end],
                &shapes[first..end],
            )
            .map(|store| (end, store))
        });
        if let Some((end, store)) = native {
            issued.push(store);
            first = end;
        } else {
            issued.push(ExactAssignmentStore::Scalar(first));
            first += 1;
        }
    }
    issued.into_boxed_slice()
}

fn native_range(
    source: &[LinearOp],
    store_position: usize,
    rows: &[&AlgebraicRefreshRow],
    shapes: &[TargetAssignmentShape],
) -> Option<ExactAssignmentStore> {
    if rows.len() < 2
        || !matches!(
            source.get(store_position)?,
            LinearOp::StoreOutputRange { .. }
        )
    {
        return None;
    }
    let direct = |shape: &TargetAssignmentShape| match shape {
        TargetAssignmentShape::Direct { expr_reg, .. } => Some(*expr_reg),
        _ => None,
    };
    let start = direct(shapes.first()?)?;
    let stride = usize::try_from(direct(shapes.get(1)?)?.checked_sub(start)?).ok()?;
    if !shapes.iter().enumerate().all(|(ordinal, shape)| {
        let expected = ordinal
            .checked_mul(stride)
            .and_then(|offset| Reg::try_from(offset).ok())
            .and_then(|offset| start.checked_add(offset));
        expected.is_some_and(|register| direct(shape) == Some(register))
    }) {
        return None;
    }
    let indices = rows.iter().map(|row| row.target_index).collect::<Vec<_>>();
    let targets = ScalarProgramOutputSpan::checked(&indices)?;
    Some(ExactAssignmentStore::Range {
        start,
        stride,
        targets,
    })
}
