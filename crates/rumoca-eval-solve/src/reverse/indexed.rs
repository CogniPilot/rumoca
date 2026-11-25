//! Runtime-indexed register reads use exactly the index checks of the evaluator.
use super::{EvalSolveError, Reg, reg, unsupported};
use rumoca_ir_solve::TensorIndex;

pub(super) fn source(
    base: Reg,
    stride: usize,
    dimensions: &[u32],
    indices: &[TensorIndex],
    regs: &[f64],
) -> Result<Reg, EvalSolveError> {
    let i = crate::tensor_register_offset(dimensions, indices, None, |r| Ok(reg(regs, r)))?;
    i.checked_mul(stride)
        .and_then(|offset| Reg::try_from(offset).ok())
        .and_then(|offset| base.checked_add(offset))
        .ok_or_else(|| unsupported("indexed register offset overflow"))
}
