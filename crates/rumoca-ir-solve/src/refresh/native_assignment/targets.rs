//! Exact affine bijections from domain order to one owned Y progression.

use super::*;
use crate::{AffineStencilIndexStrideTerm, ScalarSlot};

pub(super) struct TargetMap {
    pub(super) coverage: coverage::Coverage,
    pub(super) first: usize,
    pub(super) output: TensorOutputMap,
}

pub(super) fn derive(
    selected: &[Option<ScalarSlot>],
    extents: &[usize],
    capacity: usize,
) -> Checked<TargetMap> {
    let indices = selected
        .iter()
        .map(|slot| match slot {
            Some(ScalarSlot::Y { index, byte_offset })
                if index.checked_mul(8) == Some(*byte_offset) && *index < capacity =>
            {
                Some(*index)
            }
            _ => None,
        })
        .collect::<Option<Vec<_>>>()
        .ok_or(NativeRefreshAssignmentRefusal(
            "native assignment has an invalid owned Y target",
        ))?;
    let first = *indices.first().ok_or(NativeRefreshAssignmentRefusal(
        "native assignment has no owned Y target",
    ))?;
    let mut sorted = indices.clone();
    sorted.sort_unstable();
    // `first` proves the targets nonempty; folds seeded by it are total.
    let start = indices.iter().copied().fold(first, usize::min);
    let end = indices
        .iter()
        .copied()
        .fold(first, usize::max)
        .checked_add(1)
        .ok_or(NativeRefreshAssignmentRefusal(
            "native target range overflows",
        ))?;
    let width = sorted
        .iter()
        .enumerate()
        .take_while(|(i, target)| start.checked_add(*i) == Some(**target))
        .count();
    let stride = sorted.get(width).map_or(width, |next| next - start);
    if stride < width
        || indices.len() % width != 0
        || end > capacity
        || sorted.iter().enumerate().any(|(i, target)| {
            (i / width)
                .checked_mul(stride)
                .and_then(|offset| offset.checked_add(i % width))
                .and_then(|offset| start.checked_add(offset))
                != Some(*target)
        })
    {
        return refused("native targets do not bijectively cover bounded periodic blocks");
    }
    let (strides, domain_strides) = derive_strides(&indices, extents)?;
    if !indices.iter().enumerate().all(|(ordinal, target)| {
        let address = strides.iter().zip(&domain_strides).try_fold(
            first as i128,
            |address, (term, dense)| {
                let coordinate = ordinal / dense % extents[term.dimension];
                address.checked_add((coordinate as i128).checked_mul(term.stride as i128)?)
            },
        );
        address == Some(*target as i128)
    }) {
        return refused("native target permutation has no exact compact affine map");
    }
    Ok(TargetMap {
        coverage: if width == indices.len() {
            coverage::Coverage::dense(start..end)
        } else {
            coverage::Coverage {
                span: start..end,
                stride,
                width,
                count: indices.len(),
            }
        },
        first,
        output: TensorOutputMap {
            start: first - start,
            strides,
        },
    })
}

fn derive_strides(
    indices: &[usize],
    extents: &[usize],
) -> Checked<(Vec<AffineStencilIndexStrideTerm>, Vec<usize>)> {
    let mut strides = Vec::with_capacity(extents.len());
    let mut domain_strides = Vec::with_capacity(extents.len());
    let mut dense = 1usize;
    for (dimension, extent) in extents.iter().enumerate().rev() {
        let stride = if *extent > 1 {
            let next = indices.get(dense).ok_or(NativeRefreshAssignmentRefusal(
                "native target domain differs from its structural inventory",
            ))?;
            isize::try_from(*next as i128 - indices[0] as i128).map_err(|_| {
                NativeRefreshAssignmentRefusal("native target affine stride overflows")
            })?
        } else {
            // Preserve the historical dense map for singleton dimensions.
            isize::try_from(dense).map_err(|_| {
                NativeRefreshAssignmentRefusal("native target affine stride overflows")
            })?
        };
        strides.push(AffineStencilIndexStrideTerm { dimension, stride });
        domain_strides.push(dense);
        dense = dense
            .checked_mul(*extent)
            .ok_or(NativeRefreshAssignmentRefusal(
                "native target domain cardinality overflows",
            ))?;
    }
    if dense != indices.len() {
        return refused("native target domain differs from its structural inventory");
    }
    strides.reverse();
    domain_strides.reverse();
    Ok((strides, domain_strides))
}
