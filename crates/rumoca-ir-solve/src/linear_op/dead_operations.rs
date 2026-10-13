//! Removal of pure register computations no later operation reads.
//!
//! Liveness is read off the canonical register versions of the program: every
//! operand piece names the one operation that wrote it, so a computation is
//! live exactly when a live operation, or an operation this pass never drops,
//! reads a value it wrote.

use super::*;
use crate::RegisterVersionProgram;

/// Drop every pure, fault-free computation of a straight-line program whose
/// registers no kept operation reads, including the chains that feed only
/// dropped operations. Calls, gathers, conditionals, folds, table and random
/// operations, and stores are never dropped. A program without canonical
/// register versions is returned unchanged.
#[must_use]
pub fn prune_unread_pure_operations(program: Vec<LinearOp>) -> Vec<LinearOp> {
    let Some(versions) = RegisterVersionProgram::derive(&program) else {
        return program;
    };
    let mut live = program.iter().map(|op| !prunable(op)).collect::<Vec<_>>();
    // Every producer precedes its readers, so one backward sweep reaches all.
    for index in (0..program.len()).rev() {
        if !live[index] {
            continue;
        }
        for piece in versions.operations()[index].operands() {
            live[piece.producer()] = true;
        }
    }
    if live.iter().all(|&live| live) {
        return program;
    }
    program
        .into_iter()
        .zip(live)
        .filter_map(|(op, live)| live.then_some(op))
        .collect()
}

/// A pure register computation that cannot fail and has no effect, so an
/// unread one may be dropped.
fn prunable(op: &LinearOp) -> bool {
    op.fault_class().is_prunable()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn residual_program() -> Vec<LinearOp> {
        vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::P,
                input_start: 0,
                count: 4,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorLoad {
                dst_start: 4,
                input: TensorInputKind::Y,
                input_start: 0,
                count: 4,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::TensorBinary {
                dst_start: 8,
                op: BinaryOp::Sub,
                lhs_start: 4,
                rhs_start: 0,
                count: 4,
                lhs_stride: 1,
                rhs_stride: 1,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 4,
                stride: 1,
            },
        ]
    }

    #[test]
    fn an_unread_residual_over_a_tensor_load_is_dropped_with_its_target_load() {
        let pruned = prune_unread_pure_operations(residual_program());
        assert_eq!(pruned.len(), 2);
        assert!(matches!(
            pruned[0],
            LinearOp::TensorLoad { dst_start: 0, .. }
        ));
        assert!(matches!(
            pruned[1],
            LinearOp::StoreOutputRange { start: 0, .. }
        ));
        assert!(ScalarProgramRegisterFlow::derive(&pruned).is_ok());
    }

    #[test]
    fn a_residual_a_store_reads_keeps_its_whole_chain() {
        let mut program = residual_program();
        program[3] = LinearOp::StoreOutputRange {
            start: 8,
            count: 4,
            stride: 1,
        };
        assert_eq!(prune_unread_pure_operations(program.clone()), program);
    }

    #[test]
    fn a_partly_read_range_keeps_its_producer() {
        let mut program = residual_program();
        program[3] = LinearOp::StoreOutput { src: 10 };
        let pruned = prune_unread_pure_operations(program);
        assert_eq!(pruned.len(), 4);
        assert!(ScalarProgramRegisterFlow::derive(&pruned).is_ok());
    }

    #[test]
    fn a_redefined_register_keeps_only_the_read_definition() {
        let program = vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::LoadP { dst: 0, index: 1 },
            LinearOp::StoreOutput { src: 0 },
        ];
        assert_eq!(
            prune_unread_pure_operations(program),
            [
                LinearOp::LoadP { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ]
        );
    }
}
