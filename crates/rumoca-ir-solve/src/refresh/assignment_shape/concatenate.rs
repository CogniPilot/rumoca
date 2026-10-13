//! Lane forwarding through dense tensor concatenation.

use super::{ProgramPrefix, producer_position};
use crate::{LinearOp, Reg};

/// The source register a concatenation lane copies: the inverse of the
/// row-major placement of each source along the concatenation axis. `None`
/// when `register` is not a lane of a well-formed concatenation.
pub(super) fn source_register(program: ProgramPrefix<'_>, register: Reg) -> Option<Reg> {
    let LinearOp::TensorConcatenate {
        dst_start,
        sources,
        dimensions,
        axis,
        lanes,
    } = program.operation(producer_position(program, register)?)?
    else {
        return None;
    };
    let extent = |dimensions: &[u32]| {
        dimensions
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
    };
    let inner = extent(dimensions.get(axis.checked_add(1)?..)?)?;
    let result_axis = *dimensions.get(*axis)? as usize;
    let lanes = *lanes;
    let offset = usize::try_from(register.checked_sub(*dst_start)?).ok()?;
    let block = result_axis.checked_mul(inner)?;
    if lanes == 0 || block == 0 || offset >= extent(dimensions)?.checked_mul(lanes)? {
        return None;
    }
    let (element, lane) = (offset / lanes, offset % lanes);
    let (outer, within) = (element / block, element % block);
    let axis_position = within / inner;
    let mut axis_offset = 0usize;
    for source in sources.iter() {
        let source_axis = *source.dimensions.get(*axis)? as usize;
        if axis_position < axis_offset.checked_add(source_axis)? {
            let source_element = outer
                .checked_mul(source_axis)?
                .checked_add(axis_position - axis_offset)?
                .checked_mul(inner)?
                .checked_add(within % inner)?;
            let source_offset = source_element.checked_mul(lanes)?.checked_add(lane)?;
            return source.start.checked_add(u32::try_from(source_offset).ok()?);
        }
        axis_offset += source_axis;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::super::producers::UniqueProgram;
    use super::*;
    use crate::TensorConcatenateSource;

    fn concatenate(axis: usize, dimensions: &[u32], sources: &[(Reg, &[u32])]) -> LinearOp {
        LinearOp::TensorConcatenate {
            dst_start: 100,
            sources: sources
                .iter()
                .map(|(start, dimensions)| TensorConcatenateSource {
                    start: *start,
                    dimensions: (*dimensions).into(),
                })
                .collect(),
            dimensions: dimensions.into(),
            axis,
            lanes: 1,
        }
    }

    fn forwarded(operation: &LinearOp, count: Reg) -> Vec<Option<Reg>> {
        let operations = [operation.clone()];
        let program = UniqueProgram::new(&operations).unwrap();
        (100..100 + count)
            .map(|register| source_register(program.view(), register))
            .collect()
    }

    #[test]
    fn vector_sources_forward_in_order() {
        let operation = concatenate(0, &[3], &[(10, &[1]), (20, &[2])]);
        assert_eq!(
            forwarded(&operation, 4),
            [Some(10), Some(20), Some(21), None]
        );
    }

    #[test]
    fn matrix_columns_interleave_with_each_row() {
        let operation = concatenate(1, &[2, 3], &[(10, &[2, 1]), (20, &[2, 2])]);
        assert_eq!(
            forwarded(&operation, 6),
            [Some(10), Some(20), Some(21), Some(11), Some(22), Some(23)]
        );
    }
}
