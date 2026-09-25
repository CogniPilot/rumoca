//! Removal of literal stores and unary ops no later op reads.
//!
//! Liveness is read off the same source checks that prove a program's register
//! flow, so an op kind cannot read a register this pass misses: a literal
//! register stays pending until an op's source check would fail without it.

use std::collections::HashMap;

use super::*;

/// Drop every `Const` and `Unary` op of a top-level scalar program whose
/// register no later op reads before it is redefined. Both are pure register
/// computations, so an unread one has no effect. Dropping an unread unary op can
/// leave its operand unread in turn (the inner negation of a folded `-(-x)`), so
/// pruning repeats until nothing changes. A program the register-flow checks
/// reject is returned unchanged.
#[must_use]
pub fn prune_dead_constants(mut program: Vec<LinearOp>) -> Vec<LinearOp> {
    loop {
        let Some(live) = live_pure_ops(&program) else {
            return program;
        };
        if live.iter().all(|&live| live) {
            return program;
        }
        program = program
            .into_iter()
            .zip(live)
            .filter_map(|(op, live)| live.then_some(op))
            .collect();
    }
}

/// A pure register computation this pass may drop when unread.
fn prunable(op: &LinearOp) -> bool {
    matches!(op, LinearOp::Const { .. } | LinearOp::Unary { .. })
}

/// Per op, whether it is kept: every op this pass cannot drop, and each
/// prunable op whose register a later op reads.
fn live_pure_ops(program: &[LinearOp]) -> Option<Vec<bool>> {
    let mut live = program.iter().map(|op| !prunable(op)).collect::<Vec<_>>();
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
        if prunable(op) {
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
    fn an_unread_negation_chain_is_dropped_with_its_operands() {
        // `-(-x)` folded to `x` leaves the inner negation and its literal unread.
        let program = vec![
            LinearOp::Const { dst: 0, value: 3.0 },
            LinearOp::Unary {
                dst: 1,
                op: UnaryOp::Neg,
                arg: 0,
            },
            LinearOp::LoadTime { dst: 2 },
            LinearOp::StoreOutput { src: 2 },
        ];
        let pruned = prune_dead_constants(program);
        assert_eq!(
            pruned,
            [
                LinearOp::LoadTime { dst: 2 },
                LinearOp::StoreOutput { src: 2 }
            ]
        );
        assert!(ScalarProgramRegisterFlow::derive(&pruned).is_ok());
    }

    #[test]
    fn a_read_negation_keeps_its_operand() {
        let program = vec![
            LinearOp::LoadTime { dst: 0 },
            LinearOp::Unary {
                dst: 1,
                op: UnaryOp::Neg,
                arg: 0,
            },
            LinearOp::StoreOutput { src: 1 },
        ];
        assert_eq!(prune_dead_constants(program.clone()), program);
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
