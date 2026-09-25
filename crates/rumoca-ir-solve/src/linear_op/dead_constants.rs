//! Removal of literal stores no later op reads.
//!
//! Liveness is read off the same source checks that prove a program's register
//! flow, so an op kind cannot read a register this pass misses: a literal
//! register stays pending until an op's source check would fail without it.

use std::collections::HashMap;

use super::*;

/// Drop every `Const` op of a top-level scalar program whose register no later
/// op reads before it is redefined. A program the register-flow checks reject
/// is returned unchanged.
#[must_use]
pub fn prune_dead_constants(program: Vec<LinearOp>) -> Vec<LinearOp> {
    let Some(live) = live_constant_ops(&program) else {
        return program;
    };
    program
        .into_iter()
        .zip(live)
        .filter_map(|(op, live)| live.then_some(op))
        .collect()
}

/// Per op, whether it is kept: every non-`Const` op, and each `Const` op whose
/// register a later op reads.
fn live_constant_ops(program: &[LinearOp]) -> Option<Vec<bool>> {
    let mut live = program
        .iter()
        .map(|op| !matches!(op, LinearOp::Const { .. }))
        .collect::<Vec<_>>();
    let mut initialized = Vec::new();
    let mut pending: HashMap<Reg, usize> = HashMap::new();
    let mut validation = ScalarProgramValidationCache::default();
    for (op_index, op) in program.iter().enumerate() {
        loop {
            match validate_op_sources(op, op_index, &initialized, None, None, &mut validation) {
                Ok(_) => break,
                Err(ScalarProgramRegisterError::UndefinedRegister { register, .. }) => {
                    let defining = pending.remove(&register)?;
                    live[defining] = true;
                    mark_register_initialized(&mut initialized, register);
                }
                Err(_) => return None,
            }
        }
        let Some(dst) = op.dst_register() else {
            continue;
        };
        for offset in 0..op.dst_register_count() {
            pending.remove(&(dst + offset as Reg));
        }
        if matches!(op, LinearOp::Const { .. }) {
            pending.insert(dst, op_index);
        } else {
            mark_register_range_initialized(&mut initialized, dst, op.dst_register_count());
        }
    }
    Some(live)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_unread_literal_is_dropped_and_a_read_one_kept() {
        let program = vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const { dst: 1, value: 2.0 },
            LinearOp::LoadTime { dst: 2 },
            LinearOp::Binary {
                dst: 3,
                op: BinaryOp::Mul,
                lhs: 1,
                rhs: 2,
            },
            LinearOp::StoreOutput { src: 3 },
        ];
        let pruned = prune_dead_constants(program);
        assert_eq!(pruned.len(), 4);
        assert!(!pruned.contains(&LinearOp::Const { dst: 0, value: 1.0 }));
        assert!(ScalarProgramRegisterFlow::derive(&pruned).is_ok());
    }

    #[test]
    fn a_literal_read_through_a_range_is_kept() {
        let program = vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const { dst: 1, value: 0.0 },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ];
        assert_eq!(prune_dead_constants(program.clone()), program);
    }

    #[test]
    fn a_redefined_literal_register_keeps_only_the_read_definition() {
        let program = vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutput { src: 0 },
        ];
        assert_eq!(
            prune_dead_constants(program),
            [
                LinearOp::Const { dst: 0, value: 2.0 },
                LinearOp::StoreOutput { src: 0 },
            ]
        );
    }
}
