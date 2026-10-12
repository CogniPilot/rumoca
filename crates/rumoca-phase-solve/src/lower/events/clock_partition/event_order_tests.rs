//! Ordinary iterative ordering remains owned by the SOLVE-C58 exchange.

use super::*;

fn member(target: u32, reads: &[u32], entry_reads: &[u32]) -> SameTickExchangeMember<'static> {
    SameTickExchangeMember {
        event_reads: None,
        targets: vec![target],
        reads: reads.iter().copied().collect(),
        entry_reads: entry_reads.iter().copied().collect(),
    }
}

#[test]
fn ordinary_observation_cycles_retain_event_iteration_order() {
    let exchange = SameTickExchange::new(&[member(0, &[1], &[]), member(1, &[0], &[])]);
    assert_eq!(exchange.producer_order(), [0, 1]);
    assert!(!exchange.all_fusable());
    let reversed = SameTickExchange::new(&[member(1, &[0], &[]), member(0, &[1], &[])]);
    assert_eq!(reversed.producer_order(), [0, 1]);
}

#[test]
fn acyclic_transitive_chains_remain_producer_before_reader() {
    let exchange = SameTickExchange::new(&[
        member(2, &[1], &[]),
        member(1, &[0], &[]),
        member(0, &[], &[]),
    ]);
    assert_eq!(exchange.producer_order(), [2, 1, 0]);
    assert!(!exchange.all_fusable());
}

#[test]
fn alias_closed_ordinary_cycles_retain_event_iteration_order() {
    // The alias-closed read facts reach the opposite target; the exchange
    // orders members, while actual alias refreshes belong to their compiler.
    let exchange = SameTickExchange::new(&[member(0, &[2, 1], &[]), member(1, &[0], &[])]);
    assert_eq!(exchange.producer_order(), [0, 1]);
    assert!(!exchange.all_fusable());
}

#[test]
fn an_external_entry_reader_precedes_the_observation_cycles_stable_order() {
    let exchange = SameTickExchange::new(&[
        member(0, &[2, 1], &[]),
        member(1, &[0], &[]),
        member(3, &[1], &[0]),
    ]);
    assert_eq!(exchange.producer_order(), [1, 2, 0]);
}
