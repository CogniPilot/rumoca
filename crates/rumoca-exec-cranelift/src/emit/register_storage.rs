//! Bounded target-local storage selection for compact row tensor operations.

use super::LinearOp;

/// The largest tensor operation (in destination registers, or in scalar
/// multiply-adds for a product) a row may hold and still keep every register
/// in native values instead of a memory tape. A 6x6 dual tensor (72
/// registers) fits.
const MAX_DIRECT_REGISTER_WORK: usize = 128;

pub(super) fn can_use_direct_registers(row: &[LinearOp]) -> bool {
    row.iter().all(operation_can_use_direct_registers)
}

fn operation_can_use_direct_registers(operation: &LinearOp) -> bool {
    match operation {
        LinearOp::MatrixMultiply {
            rows,
            inner,
            columns,
            lanes,
            ..
        } => rows
            .checked_mul(*inner)
            .and_then(|count| count.checked_mul(*columns))
            .and_then(|count| count.checked_mul(*lanes))
            .is_some_and(|count| count <= MAX_DIRECT_REGISTER_WORK),
        LinearOp::DotProduct { count, .. } => *count <= MAX_DIRECT_REGISTER_WORK,
        LinearOp::TensorUpdate { subscripts, .. } => {
            operation.dst_register_count() <= MAX_DIRECT_REGISTER_WORK
                && !subscripts.iter().any(|subscript| {
                    matches!(
                        subscript,
                        rumoca_ir_solve::TensorUpdateSubscript::Slice { .. }
                    )
                })
        }
        LinearOp::TensorBinary { .. }
        | LinearOp::TensorTranspose { .. }
        | LinearOp::TensorConcatenate { .. }
        | LinearOp::TensorFill { .. }
        | LinearOp::TensorIdentity { .. }
        | LinearOp::TensorLoad { .. } => operation.dst_register_count() <= MAX_DIRECT_REGISTER_WORK,
        _ => true,
    }
}
