use super::{Degree, LinearOp, Registers};
use crate::{TensorIndex, TensorUpdateSubscript};

pub(super) fn degree(operation: &LinearOp, registers: &Registers) -> Option<Degree> {
    match operation {
        LinearOp::TensorConcatenate {
            sources, lanes: 1, ..
        } => sources
            .iter()
            .try_fold(Degree::Independent, |degree, source| {
                Some(degree.max(registers.read(source.start, extent(&source.dimensions)?)?))
            }),
        LinearOp::TensorTranspose {
            src_start,
            rows,
            columns,
            element_width,
            lanes: 1,
            ..
        } => registers.read(
            *src_start,
            rows.checked_mul(*columns)?.checked_mul(*element_width)?,
        ),
        LinearOp::TensorUpdate {
            base_start,
            value_start,
            dimensions,
            subscripts,
            lanes: 1,
            ..
        } => {
            let (count, selector) = patch(registers, dimensions, subscripts)?;
            let base = registers.read(*base_start, extent(dimensions)?)?;
            let value = registers.read(*value_start, count)?;
            Some(if selector == Degree::Independent {
                base.max(value)
            } else {
                Degree::Nonlinear
            })
        }
        _ => None,
    }
}

fn extent(dimensions: &[u32]) -> Option<usize> {
    dimensions
        .iter()
        .try_fold(1usize, |size, &extent| size.checked_mul(extent as usize))
}

fn patch(
    registers: &Registers,
    dimensions: &[u32],
    subscripts: &[TensorUpdateSubscript],
) -> Option<(usize, Degree)> {
    if dimensions.len() != subscripts.len() {
        return None;
    }
    let mut count = 1usize;
    let mut selector = Degree::Independent;
    for (&dimension, subscript) in dimensions.iter().zip(subscripts) {
        match subscript {
            TensorUpdateSubscript::Whole => count = count.checked_mul(dimension as usize)?,
            TensorUpdateSubscript::Index(TensorIndex::Constant(_)) => {}
            TensorUpdateSubscript::Index(TensorIndex::Runtime(register)) => {
                selector = selector.max(registers.read(*register, 1)?);
            }
            TensorUpdateSubscript::Slice { start, dimensions } => {
                let size = extent(dimensions)?;
                count = count.checked_mul(size)?;
                selector = selector.max(registers.read(*start, size)?);
            }
        }
    }
    Some((count, selector))
}
