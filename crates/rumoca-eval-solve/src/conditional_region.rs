#[cfg(test)]
mod tests;

use super::*;

/// Interpret a single scalar producer and its exact output directly. The
/// checked region still selects the arm; this only avoids a register frame for
/// a result that consists of one current input read or one literal.
pub(super) fn direct_value(
    input: PreparedRowEval<'_, '_>,
    region: &[LinearOp],
    register_count: usize,
) -> Option<Result<f64, EvalSolveError>> {
    let [operation, LinearOp::StoreOutput { src }] = region else {
        return None;
    };
    if operation.dst_register() != Some(*src)
        || operation.dst_register_count() != 1
        || *src as usize >= register_count
    {
        return None;
    }
    Some(match operation {
        LinearOp::Const { value, .. } => Ok(*value),
        LinearOp::LoadTime { .. } => Ok(input.t),
        LinearOp::LoadY { index, .. } => input.read_input("y", input.y, *index),
        LinearOp::LoadP { index, .. } => input.read_input("p", input.p, *index),
        _ => return None,
    })
}
