//! Bounded target-local storage selection for compact row tensor operations.

use super::LinearOp;
use rumoca_ir_solve::IntegerPower;

/// The largest tensor operation (in destination registers, or in scalar
/// multiply-adds for a product) a row may hold and still keep every register
/// in native values instead of a memory tape. A 6x6 dual tensor (72
/// registers) fits.
const MAX_DIRECT_REGISTER_WORK: usize = 128;

pub(super) fn can_use_direct_registers(row: &[LinearOp]) -> bool {
    row.iter().all(operation_can_use_direct_registers)
}

/// Multiplications an elementwise literal power may unroll on direct
/// registers: its extent times its plan's products, the most the
/// `TensorBinary` chain it stands for unrolls when each of its ranges keeps
/// direct registers (128 values times 124 products).
const MAX_UNROLLED_POWER_WORK: usize = MAX_DIRECT_REGISTER_WORK * IntegerPower::MAX_PRODUCTS;

fn unrolled_power_work(operation: &LinearOp) -> Option<usize> {
    match operation {
        LinearOp::TensorIntegerPower { power, .. } => Some(
            operation
                .dst_register_count()
                .saturating_mul(power.product_count()),
        ),
        _ => None,
    }
}

/// Whether a fold body must take the register tape for an elementwise
/// literal power, whose unrolled multiplications would pass what its chain
/// unrolls on direct registers.
pub(super) fn integer_power_needs_tape(operation: &LinearOp) -> bool {
    unrolled_power_work(operation).is_some_and(|work| work > MAX_UNROLLED_POWER_WORK)
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
        // Its destination is bounded as a `TensorBinary` range is, so its
        // unrolled products stay within `MAX_UNROLLED_POWER_WORK`, the bound
        // a fold body checks (`integer_power_needs_tape`).
        LinearOp::TensorIntegerPower { .. } => {
            operation.dst_register_count() <= MAX_DIRECT_REGISTER_WORK
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
