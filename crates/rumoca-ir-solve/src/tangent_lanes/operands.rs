//! Register operands of the operations a tangent-lane program replicates.

use crate::{LinearOp, Reg, TensorIndex, TensorUpdateSubscript};

/// The registers one operand field reads, relative to its start register.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum Operand {
    /// One scalar register.
    Single,
    /// `count` registers at `stride` apart; `lanes` is the interleaved
    /// primal/tangent width of the aggregate it belongs to (1 or 2).
    Strided {
        count: usize,
        stride: usize,
        lanes: usize,
    },
}

impl Operand {
    const fn range(len: usize, lanes: usize) -> Self {
        Self::Strided {
            count: len,
            stride: 1,
            lanes,
        }
    }

    /// The registers read from `start`, or `None` when they overflow.
    pub(super) fn registers(self, start: Reg) -> Option<Vec<Reg>> {
        match self {
            Self::Single => Some(vec![start]),
            Self::Strided { count, stride, .. } => (0..count)
                .map(|ordinal| {
                    let offset = Reg::try_from(ordinal.checked_mul(stride)?).ok()?;
                    start.checked_add(offset)
                })
                .collect(),
        }
    }
}

fn product(dimensions: &[u32]) -> usize {
    dimensions.iter().fold(1usize, |count, extent| {
        count.saturating_mul(*extent as usize)
    })
}

fn strided_len(count: usize, stride: usize, lanes: usize) -> usize {
    count
        .saturating_sub(1)
        .saturating_mul(stride)
        .saturating_add(1)
        .saturating_mul(lanes)
}

/// Visit every register operand of `op` in a fixed order, with its shape.
///
/// Returns `false` for an operation whose operands this visitor does not
/// enumerate (nested programs, random streams, and loads that only occur
/// inside nested programs); such an operation can only run once, unreplicated.
// SPEC_0021: Exception - one exhaustive operand table over every LinearOp variant.
#[allow(clippy::too_many_lines)]
pub(super) fn visit_operands(op: &mut LinearOp, visit: &mut impl FnMut(&mut Reg, Operand)) -> bool {
    match op {
        LinearOp::Const { .. }
        | LinearOp::LoadTime { .. }
        | LinearOp::LoadY { .. }
        | LinearOp::LoadP { .. }
        | LinearOp::LoadSeed { .. }
        | LinearOp::TensorIdentity { .. }
        | LinearOp::TensorLoad { .. } => {}
        LinearOp::LoadIndexedP { index, .. }
        | LinearOp::LoadIndexedSeed { index, .. }
        | LinearOp::Move { src: index, .. }
        | LinearOp::Unary { arg: index, .. }
        | LinearOp::StoreOutput { src: index }
        | LinearOp::TableBounds {
            table_id: index, ..
        } => visit(index, Operand::Single),
        LinearOp::LoadIndexedRegister {
            base,
            stride,
            dimensions,
            indices,
            ..
        } => {
            visit(
                base,
                Operand::Strided {
                    count: product(dimensions),
                    stride: *stride,
                    lanes: 1,
                },
            );
            for index in indices.iter_mut() {
                if let TensorIndex::Runtime(register) = index {
                    visit(register, Operand::Single);
                }
            }
        }
        LinearOp::StoreOutputRange {
            start,
            count,
            stride,
        } => visit(
            start,
            Operand::Strided {
                count: *count,
                stride: *stride,
                lanes: 1,
            },
        ),
        LinearOp::LinearSolveComponent {
            matrix_start,
            rhs_start,
            n,
            ..
        } => {
            visit(matrix_start, Operand::range(n.saturating_mul(*n), 1));
            visit(rhs_start, Operand::range(*n, 1));
        }
        LinearOp::DotProduct {
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            ..
        } => {
            visit(
                lhs_start,
                Operand::Strided {
                    count: *count,
                    stride: *lhs_stride,
                    lanes: 1,
                },
            );
            visit(
                rhs_start,
                Operand::Strided {
                    count: *count,
                    stride: *rhs_stride,
                    lanes: 1,
                },
            );
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
            let lanes = *lanes;
            visit(
                lhs_start,
                Operand::range(rows.saturating_mul(*inner).saturating_mul(lanes), lanes),
            );
            visit(
                rhs_start,
                Operand::range(inner.saturating_mul(*columns).saturating_mul(lanes), lanes),
            );
        }
        LinearOp::TensorBinary {
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes,
            ..
        } => {
            let lanes = *lanes;
            visit(
                lhs_start,
                Operand::range(strided_len(*count, *lhs_stride, lanes), lanes),
            );
            visit(
                rhs_start,
                Operand::range(strided_len(*count, *rhs_stride, lanes), lanes),
            );
        }
        LinearOp::TensorCross {
            lhs_start,
            rhs_start,
            lanes,
            ..
        } => {
            let lanes = *lanes;
            visit(lhs_start, Operand::range(3 * lanes, lanes));
            visit(rhs_start, Operand::range(3 * lanes, lanes));
        }
        LinearOp::TensorTranspose {
            src_start,
            rows,
            columns,
            element_width,
            lanes,
            ..
        } => {
            let len = rows
                .saturating_mul(*columns)
                .saturating_mul(*element_width)
                .saturating_mul(*lanes);
            visit(src_start, Operand::range(len, *lanes));
        }
        LinearOp::TensorConcatenate { sources, lanes, .. } => {
            let lanes = *lanes;
            for source in sources.iter_mut() {
                let len = product(&source.dimensions).saturating_mul(lanes);
                visit(&mut source.start, Operand::range(len, lanes));
            }
        }
        LinearOp::TensorUpdate {
            base_start,
            value_start,
            dimensions,
            subscripts,
            lanes,
            ..
        } => {
            let lanes = *lanes;
            visit(
                base_start,
                Operand::range(product(dimensions).saturating_mul(lanes), lanes),
            );
            let mut value_count = lanes;
            for (&extent, subscript) in dimensions.iter().zip(subscripts.iter_mut()) {
                match subscript {
                    TensorUpdateSubscript::Whole => {
                        value_count = value_count.saturating_mul(extent as usize);
                    }
                    TensorUpdateSubscript::Index(TensorIndex::Constant(_)) => {}
                    TensorUpdateSubscript::Index(TensorIndex::Runtime(register)) => {
                        visit(register, Operand::Single);
                    }
                    TensorUpdateSubscript::Slice { start, dimensions } => {
                        let count = product(dimensions);
                        value_count = value_count.saturating_mul(count);
                        visit(start, Operand::range(count, 1));
                    }
                }
            }
            visit(value_start, Operand::range(value_count, lanes));
        }
        LinearOp::TensorFill {
            value_start, lanes, ..
        } => visit(value_start, Operand::range(*lanes, *lanes)),
        LinearOp::TableLookup {
            table_id,
            column,
            input,
            ..
        }
        | LinearOp::TableLookupSlope {
            table_id,
            column,
            input,
            ..
        } => {
            visit(table_id, Operand::Single);
            visit(column, Operand::Single);
            visit(input, Operand::Single);
        }
        LinearOp::TableNextEvent { table_id, time, .. } => {
            visit(table_id, Operand::Single);
            visit(time, Operand::Single);
        }
        LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
            visit(lhs, Operand::Single);
            visit(rhs, Operand::Single);
        }
        LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => {
            visit(cond, Operand::Single);
            visit(if_true, Operand::Single);
            visit(if_false, Operand::Single);
        }
        LinearOp::PureCall {
            input_starts, site, ..
        } => {
            for (start, value_type) in input_starts.iter_mut().zip(site.inputs()) {
                visit(start, Operand::range(value_type.scalar_count() as usize, 1));
            }
        }
        LinearOp::PureCallDirectional {
            input_starts, site, ..
        } => {
            for (start, value_type) in input_starts.iter_mut().zip(site.inputs()) {
                visit(start, Operand::range(value_type.scalar_count() as usize, 1));
            }
        }
        LinearOp::LoadIndexedFoldCarried { .. }
        | LinearOp::LoadIndexedFoldCapture { .. }
        | LinearOp::LoadFoldCarried { .. }
        | LinearOp::LoadFoldIndex { .. }
        | LinearOp::LoadFoldCapture { .. }
        | LinearOp::LoadFunctionConditionalCapture { .. }
        | LinearOp::LoadFunctionConditionalCaptureRange { .. }
        | LinearOp::RandomInitialState { .. }
        | LinearOp::RandomResult { .. }
        | LinearOp::RandomState { .. }
        | LinearOp::ImpureRandomInit { .. }
        | LinearOp::ImpureRandom { .. }
        | LinearOp::ImpureRandomInteger { .. }
        | LinearOp::FunctionFold { .. }
        | LinearOp::GuardedFunctionFold { .. }
        | LinearOp::FunctionConditional { .. }
        | LinearOp::StoreOutputFoldTensorUpdate { .. }
        | LinearOp::StoreOutputFunctionFold { .. } => return false,
    }
    true
}

/// The destination start register of `op`, mutably.
pub(super) fn destination(op: &mut LinearOp) -> Option<&mut Reg> {
    match op {
        LinearOp::Const { dst, .. }
        | LinearOp::LoadTime { dst }
        | LinearOp::LoadY { dst, .. }
        | LinearOp::LoadP { dst, .. }
        | LinearOp::LoadIndexedP { dst, .. }
        | LinearOp::LoadIndexedRegister { dst, .. }
        | LinearOp::LoadIndexedFoldCarried { dst, .. }
        | LinearOp::LoadIndexedFoldCapture { dst, .. }
        | LinearOp::LoadSeed { dst, .. }
        | LinearOp::LoadIndexedSeed { dst, .. }
        | LinearOp::LoadFoldCarried { dst, .. }
        | LinearOp::LoadFoldIndex { dst, .. }
        | LinearOp::LoadFoldCapture { dst, .. }
        | LinearOp::LoadFunctionConditionalCapture { dst, .. }
        | LinearOp::Move { dst, .. }
        | LinearOp::LinearSolveComponent { dst, .. }
        | LinearOp::DotProduct { dst, .. }
        | LinearOp::TableBounds { dst, .. }
        | LinearOp::TableLookup { dst, .. }
        | LinearOp::TableLookupSlope { dst, .. }
        | LinearOp::TableNextEvent { dst, .. }
        | LinearOp::RandomInitialState { dst, .. }
        | LinearOp::RandomResult { dst, .. }
        | LinearOp::RandomState { dst, .. }
        | LinearOp::ImpureRandomInit { dst, .. }
        | LinearOp::ImpureRandom { dst, .. }
        | LinearOp::ImpureRandomInteger { dst, .. }
        | LinearOp::Unary { dst, .. }
        | LinearOp::Binary { dst, .. }
        | LinearOp::Compare { dst, .. }
        | LinearOp::Select { dst, .. } => Some(dst),
        LinearOp::LoadFunctionConditionalCaptureRange { dst_start, .. }
        | LinearOp::FunctionFold { dst_start, .. }
        | LinearOp::GuardedFunctionFold { dst_start, .. }
        | LinearOp::FunctionConditional { dst_start, .. }
        | LinearOp::PureCall { dst_start, .. }
        | LinearOp::PureCallDirectional { dst_start, .. }
        | LinearOp::MatrixMultiply { dst_start, .. }
        | LinearOp::TensorBinary { dst_start, .. }
        | LinearOp::TensorCross { dst_start, .. }
        | LinearOp::TensorTranspose { dst_start, .. }
        | LinearOp::TensorConcatenate { dst_start, .. }
        | LinearOp::TensorUpdate { dst_start, .. }
        | LinearOp::TensorFill { dst_start, .. }
        | LinearOp::TensorIdentity { dst_start, .. }
        | LinearOp::TensorLoad { dst_start, .. } => Some(dst_start),
        LinearOp::StoreOutputFoldTensorUpdate { .. }
        | LinearOp::StoreOutputFunctionFold { .. }
        | LinearOp::StoreOutputRange { .. }
        | LinearOp::StoreOutput { .. } => None,
    }
}
