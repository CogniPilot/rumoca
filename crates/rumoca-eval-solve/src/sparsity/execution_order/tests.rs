use std::sync::Arc;

use rumoca_ir_solve::IndexIntervals;

use super::*;

/// One singleton block per `(row, target)`, in issued order.
fn singletons(targets: &[usize]) -> Vec<AlgebraicProjectionBlock> {
    targets
        .iter()
        .enumerate()
        .map(|(row, &target)| AlgebraicProjectionBlock {
            rows: vec![row],
            y_indices: vec![target],
            tearing: None,
            alternate_charts: Vec::new(),
        })
        .collect()
}

fn order(targets: &[usize], reads: &[&[usize]]) -> Option<Vec<usize>> {
    let blocks = singletons(targets);
    let reads = reads
        .iter()
        .map(|row| Some(Arc::new(IndexIntervals::of(row.iter().copied()))))
        .collect::<Vec<_>>();
    execution_order(&blocks.iter().collect::<Vec<_>>(), &reads)
}

/// `der(x[1])` (target 10) is issued first and loads `uu[2]` (target 11) with
/// its tensor program; `uu[2] = x[1]` reads neither.
#[test]
fn a_block_runs_after_the_later_block_its_execution_reads() {
    assert_eq!(order(&[10, 11], &[&[11], &[]]), Some(vec![1, 0]));
}

#[test]
fn an_order_that_already_produces_every_read_is_kept() {
    assert_eq!(order(&[10, 11, 12], &[&[], &[10], &[10, 11]]), None);
}

#[test]
fn blocks_no_read_moves_keep_their_issued_order() {
    assert_eq!(
        order(&[10, 11, 12, 13], &[&[12], &[], &[], &[11]]),
        Some(vec![2, 0, 1, 3])
    );
}

/// Each block reads the other's target: no order produces both reads, so the
/// issued order and its refusal stay.
#[test]
fn an_execution_cycle_keeps_the_issued_order() {
    assert_eq!(order(&[10, 11], &[&[11], &[10]]), None);
}

#[test]
fn a_cycle_does_not_stop_the_other_blocks_from_moving() {
    assert_eq!(
        order(&[10, 11, 12, 13], &[&[11], &[10], &[13], &[]]),
        Some(vec![0, 1, 3, 2])
    );
}

#[test]
fn a_read_of_a_value_no_block_produces_orders_nothing() {
    assert_eq!(order(&[10, 11], &[&[3, 99], &[10]]), None);
}
