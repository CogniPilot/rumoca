//! Row-major scalar-view mechanics over checked typed shapes.
use super::{Elem, Val};

pub(super) fn row_major_offset(dims: &[u32], coordinate: &[u32]) -> Option<usize> {
    if dims.len() != coordinate.len() {
        return None;
    }
    dims.iter()
        .zip(coordinate)
        .try_fold(0usize, |offset, (extent, index)| {
            (index < extent).then(|| offset * *extent as usize + *index as usize)
        })
}

/// Every row-major coordinate of `dims`, in order.
pub(super) fn coordinates(dims: &[u32]) -> impl Iterator<Item = Vec<u32>> + '_ {
    let count = dims
        .iter()
        .map(|extent| *extent as usize)
        .product::<usize>();
    let mut coordinate = vec![0u32; dims.len()];
    (0..count).map(move |ordinal| {
        if ordinal > 0 {
            advance(&mut coordinate, dims);
        }
        coordinate.clone()
    })
}

/// The next row-major coordinate of `dims`.
fn advance(coordinate: &mut [u32], dims: &[u32]) {
    for axis in (0..coordinate.len()).rev() {
        coordinate[axis] += 1;
        if coordinate[axis] < dims[axis] {
            return;
        }
        coordinate[axis] = 0;
    }
}

/// MLS transpose of the first two axes, retaining every trailing axis.
pub(super) fn transpose(operand: &Val) -> Option<Vec<Elem>> {
    let [rows, columns, trailing @ ..] = operand.dims.as_slice() else {
        return None;
    };
    let (rows, columns) = (*rows as usize, *columns as usize);
    let width = trailing
        .iter()
        .map(|extent| *extent as usize)
        .product::<usize>();
    (0..columns)
        .flat_map(|column| (0..rows).map(move |row| (row * columns + column) * width))
        .map(|start| operand.elems.get(start..start + width))
        .collect::<Option<Vec<_>>>()
        .map(|blocks| blocks.concat())
}

/// Axis concatenation of compact same-rank operands into `dims`.
pub(super) fn concatenate(dims: &[u32], axis: usize, operands: &[Val]) -> Option<Vec<Elem>> {
    let extent = |dims: &[u32]| dims.iter().map(|e| *e as usize).product::<usize>();
    let inner = extent(dims.get(axis + 1..)?);
    let outer = extent(dims.get(..axis)?);
    (0..outer)
        .flat_map(|block| operands.iter().map(move |operand| (block, operand)))
        .map(|(block, operand)| {
            let width = *operand.dims.get(axis).unwrap_or(&1) as usize * inner;
            operand.elems.get(block * width..(block + 1) * width)
        })
        .collect::<Option<Vec<_>>>()
        .map(|blocks| blocks.concat())
}
