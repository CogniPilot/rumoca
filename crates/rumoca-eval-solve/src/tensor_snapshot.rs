//! Compact read-before-write admission shared by native and scalar consumers.

use rumoca_ir_solve::{LinearOp, Reg, TensorIndex, TensorUpdateSubscript};

/// Issued before writes from the operation's checked compact storage geometry.
pub(super) struct Snapshot {
    width: usize,
    result_width: usize,
}

fn intersects(
    dst: Reg,
    count: usize,
    src: Reg,
    elements: usize,
    stride: usize,
    lanes: usize,
) -> Option<bool> {
    if count == 0 || elements == 0 || lanes == 0 {
        return Some(false);
    }
    let first = usize::try_from(src).ok()?;
    let step = stride.checked_mul(lanes)?;
    let first_end = first.checked_add(lanes.checked_sub(1)?)?;
    let last = first_end.checked_add(elements.checked_sub(1)?.checked_mul(step)?)?;
    Reg::try_from(last).ok()?;
    let low = usize::try_from(dst).ok()?;
    let high = low.checked_add(count.checked_sub(1)?)?;
    Reg::try_from(high).ok()?;
    if last < low || first > high {
        return Some(false);
    }
    if step == 0 || first_end >= low {
        return Some(true);
    }
    let distance = low.checked_sub(first_end)?;
    let index = distance / step + usize::from(distance % step != 0);
    Some(index < elements && first.checked_add(index.checked_mul(step)?)? <= high)
}

impl Snapshot {
    pub(super) fn width(&self) -> usize {
        self.width
    }

    pub(super) fn result_width(&self) -> usize {
        self.result_width
    }

    pub(super) fn issue(op: &LinearOp) -> Option<Self> {
        let Some(dst) = op.dst_register() else {
            return Some(Self {
                width: 0,
                result_width: 0,
            });
        };
        let count = result_width(op)?;
        if count == 0 {
            return Some(Self {
                width: 0,
                result_width: 0,
            });
        }
        let overlap = match op {
            LinearOp::TensorBinary {
                lhs_start,
                rhs_start,
                count: reads,
                lhs_stride,
                rhs_stride,
                lanes,
                ..
            } => {
                intersects(dst, count, *lhs_start, *reads, *lhs_stride, *lanes)?
                    | intersects(dst, count, *rhs_start, *reads, *rhs_stride, *lanes)?
            }
            LinearOp::MatrixMultiply {
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes,
                ..
            } => {
                intersects(dst, count, *lhs_start, rows.checked_mul(*inner)?, 1, *lanes)?
                    | intersects(
                        dst,
                        count,
                        *rhs_start,
                        inner.checked_mul(*columns)?,
                        1,
                        *lanes,
                    )?
            }
            LinearOp::TensorTranspose { src_start, .. } => {
                intersects(dst, count, *src_start, count, 1, 1)?
            }
            LinearOp::TensorCross {
                lhs_start,
                rhs_start,
                lanes,
                ..
            } => {
                intersects(dst, count, *lhs_start, 3, 1, *lanes)?
                    | intersects(dst, count, *rhs_start, 3, 1, *lanes)?
            }
            LinearOp::TensorConcatenate { .. }
            | LinearOp::TensorUpdate { .. }
            | LinearOp::TensorFill { .. } => copy_overlap(op, dst, count)?,
            _ => false,
        };
        Some(Self {
            width: if overlap { count } else { 0 },
            result_width: count,
        })
    }
}

fn result_width(op: &LinearOp) -> Option<usize> {
    match op {
        LinearOp::MatrixMultiply {
            rows,
            columns,
            lanes,
            ..
        } => rows.checked_mul(*columns)?.checked_mul(*lanes),
        LinearOp::TensorBinary { count, lanes, .. } | LinearOp::TensorFill { count, lanes, .. } => {
            count.checked_mul(*lanes)
        }
        LinearOp::TensorConcatenate {
            dimensions, lanes, ..
        }
        | LinearOp::TensorUpdate {
            dimensions, lanes, ..
        } => shape_width(dimensions, *lanes),
        LinearOp::TensorTranspose {
            rows,
            columns,
            element_width,
            lanes,
            ..
        } => rows
            .checked_mul(*columns)?
            .checked_mul(*element_width)?
            .checked_mul(*lanes),
        LinearOp::TensorCross { lanes, .. } => 3usize.checked_mul(*lanes),
        _ => Some(0),
    }
}

fn shape_width(dimensions: &[u32], lanes: usize) -> Option<usize> {
    dimensions
        .iter()
        .try_fold(lanes, |width, extent| width.checked_mul(*extent as usize))
}

fn copy_overlap(op: &LinearOp, dst: Reg, count: usize) -> Option<bool> {
    match op {
        LinearOp::TensorConcatenate { sources, lanes, .. } => {
            let mut overlap = false;
            for source in sources {
                let width = shape_width(&source.dimensions, *lanes)?;
                overlap |= intersects(dst, count, source.start, width, 1, 1)?;
            }
            Some(overlap)
        }
        LinearOp::TensorFill {
            value_start, lanes, ..
        } => intersects(dst, count, *value_start, *lanes, 1, 1),
        LinearOp::TensorUpdate {
            base_start,
            value_start,
            dimensions,
            subscripts,
            lanes,
            ..
        } => {
            let mut overlap = intersects(dst, count, *base_start, count, 1, 1)?;
            let mut values = *lanes;
            for (&extent, subscript) in dimensions.iter().zip(subscripts) {
                let (width, coordinates) = update_axis(extent, subscript)?;
                values = values.checked_mul(width)?;
                if let Some((start, width)) = coordinates {
                    overlap |= intersects(dst, count, start, width, 1, 1)?;
                }
            }
            Some(overlap | intersects(dst, count, *value_start, values, 1, 1)?)
        }
        _ => unreachable!("copy overlap accepts only concatenate, update and fill"),
    }
}

fn update_axis(
    extent: u32,
    subscript: &TensorUpdateSubscript,
) -> Option<(usize, Option<(Reg, usize)>)> {
    Some(match subscript {
        TensorUpdateSubscript::Whole => (extent as usize, None),
        TensorUpdateSubscript::Index(TensorIndex::Constant(_)) => (1, None),
        TensorUpdateSubscript::Index(TensorIndex::Runtime(register)) => (1, Some((*register, 1))),
        TensorUpdateSubscript::Slice { start, dimensions } => {
            let width = shape_width(dimensions, 1)?;
            (width, Some((*start, width)))
        }
    })
}
