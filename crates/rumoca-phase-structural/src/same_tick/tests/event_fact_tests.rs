//! Final task scheduling retains the ordinary event proof and history context.

use super::*;

fn ordinary<'dae>(
    probe: &Probe<'dae>,
    target: &str,
    current: &[&str],
    entry: &[&str],
) -> SameTickProducer<'dae> {
    let indexes = |names: &[&str]| {
        names
            .iter()
            .map(|name| probe.variable(name).index())
            .collect()
    };
    let facts = probe
        .definitions
        .issue_event_read_facts(probe.view, indexes(current), indexes(entry))
        .unwrap();
    let mut producer = probe.producer(target, &[]);
    producer.read_context = SameTickReadContext::OrdinaryEvent(facts);
    producer
}

#[test]
fn ordinary_current_facts_order_an_unconditional_synchronous_producer() {
    probe(Wiring::Aliases, |probe| {
        let producers = [
            ordinary(probe, "b", &["aAlias"], &[]),
            probe.producer("a", &[]),
        ];
        assert_eq!(probe.issue_names(&producers), ["a", "b"]);
    });
}

#[test]
fn ordinary_and_clocked_alias_consumers_keep_separate_refresh_liveness() {
    probe(Wiring::Aliases, |probe| {
        let producers = [
            ordinary(probe, "b", &["aAlias"], &[]),
            probe.producer("n", &[probe.read_of("aAlias")]),
            probe.producer("a", &[]),
        ];
        let schedule = issue_same_tick_schedule(
            probe.view,
            &probe.definitions,
            &producers,
            &BTreeSet::new(),
            &ClockCoincidence::never(),
        )
        .unwrap();
        assert_eq!(
            schedule.intermediate_consumers,
            BTreeMap::from([(
                (
                    probe.variable("aAlias").index(),
                    SameTickReadBoundary::ClockPartition
                ),
                vec![1]
            )])
        );
        assert_eq!(probe.issue_names(&producers), ["a", "b", "<aAlias>", "n"]);
    });
}

#[test]
fn ordinary_observation_cycles_retain_event_iteration_order() {
    probe(Wiring::Aliases, |probe| {
        let producers = [
            ordinary(probe, "b", &["a"], &[]),
            ordinary(probe, "a", &["b"], &[]),
        ];
        assert_eq!(probe.issue_names(&producers), ["b", "a"]);
    });
}

#[test]
fn an_entry_activation_read_precedes_its_rescheduler_even_with_a_current_body_read() {
    probe(Wiring::Aliases, |probe| {
        let producers = [
            ordinary(probe, "a", &[], &[]),
            ordinary(probe, "b", &["a"], &["a"]),
        ];
        assert_eq!(probe.issue_names(&producers), ["b", "a"]);
    });
}

#[test]
fn an_ordinary_non_inline_value_alias_gets_its_current_refresh() {
    probe(Wiring::Aliases, |probe| {
        let producers = [
            ordinary(probe, "b", &["nAlias"], &[]),
            probe.producer("n", &[]),
        ];
        let schedule = issue_same_tick_schedule(
            probe.view,
            &probe.definitions,
            &producers,
            &BTreeSet::new(),
            &ClockCoincidence::never(),
        )
        .unwrap();
        assert_eq!(
            schedule.intermediate_consumers,
            BTreeMap::from([(
                (
                    probe.variable("nAlias").index(),
                    SameTickReadBoundary::Current
                ),
                vec![0]
            )])
        );
        assert_eq!(probe.issue_names(&producers), ["n", "<nAlias>", "b"]);
    });
}

#[test]
fn an_ordinary_opaque_producer_reachable_read_stays_unrefreshable() {
    probe(Wiring::AliasesAndAmbiguousWriter, |probe| {
        let producers = [
            ordinary(probe, "b", &["ambiguous"], &[]),
            probe.producer("a", &[]),
        ];
        assert!(matches!(
            probe.issue(&producers),
            Err(SameTickOrderError::UnrefreshableRead { .. })
        ));
    });
}
