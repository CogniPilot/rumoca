//! Bounded target-local storage selection for compact row tensor operations.

use super::{LinearOp, MAX_STATIC_MATRIX_WORK};

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
            .is_some_and(|count| count <= MAX_STATIC_MATRIX_WORK),
        LinearOp::DotProduct { count, .. } => *count <= MAX_STATIC_MATRIX_WORK,
        LinearOp::TensorUpdate { subscripts, .. } => {
            operation.dst_register_count() <= MAX_STATIC_MATRIX_WORK
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
        | LinearOp::TensorLoad { .. } => operation.dst_register_count() <= MAX_STATIC_MATRIX_WORK,
        _ => true,
    }
}
