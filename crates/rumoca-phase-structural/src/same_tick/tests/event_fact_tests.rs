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
        assert_eq!(schedule.intermediate_consumers.len(), 1);
        let (identity, boundary) = schedule
            .steps
            .iter()
            .find_map(|step| match step {
                SameTickStep::IntermediateDefinition {
                    identity,
                    read_boundary,
                    ..
                } => Some((*identity, *read_boundary)),
                SameTickStep::Producer(_) => None,
            })
            .unwrap();
        assert_eq!(boundary, SameTickReadBoundary::ClockPartition);
        assert_eq!(schedule.intermediate_consumers[&identity], [1]);
        assert_eq!(probe.issue_names(&producers), ["a", "b", "<aAlias>", "n"]);
    });
}

#[test]
fn periodic_observation_cycles_are_rejected_instead_of_iterated() {
    probe(Wiring::Aliases, |probe| {
        let producers = [
            ordinary(probe, "b", &["a"], &[]),
            ordinary(probe, "a", &["b"], &[]),
        ];
        assert!(matches!(
            probe.issue(&producers),
            Err(SameTickOrderError::Cycle { .. })
        ));
    });
}

#[test]
fn scheduled_current_cycles_require_a_history_boundary_at_coincident_ticks() {
    probe(Wiring::Aliases, |probe| {
        let mut producers = [
            ordinary(probe, "b", &["aAlias"], &[]),
            ordinary(probe, "a", &["b"], &[]),
        ];
        producers[0].clock_domains = vec![1];
        producers[1].clock_domains = vec![2];
        assert_eq!(
            probe.issue_names_with(&producers, &ClockCoincidence::never()),
            ["b", "a"]
        );
        assert!(matches!(
            probe.issue_with(&producers, &ClockCoincidence::of_pairs([(1, 2)])),
            Err(SameTickOrderError::Cycle { span, .. }) if span == probe.span()
        ));
        producers[1].clock_domains = vec![1];
        assert!(matches!(
            probe.issue(&producers),
            Err(SameTickOrderError::Cycle { span, .. }) if span == probe.span()
        ));
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
        assert_eq!(schedule.intermediate_consumers.len(), 1);
        let (identity, boundary) = schedule
            .steps
            .iter()
            .find_map(|step| match step {
                SameTickStep::IntermediateDefinition {
                    identity,
                    read_boundary,
                    ..
                } => Some((*identity, *read_boundary)),
                SameTickStep::Producer(_) => None,
            })
            .unwrap();
        assert_eq!(boundary, SameTickReadBoundary::Current);
        assert_eq!(schedule.intermediate_consumers[&identity], [0]);
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

#[test]
fn an_entry_read_orders_a_non_inline_current_alias_before_its_rescheduler() {
    probe(Wiring::Aliases, |probe| {
        let producers = [
            ordinary(probe, "b", &["nAlias"], &["n"]),
            ordinary(probe, "n", &[], &[]),
        ];
        assert_eq!(probe.issue_names(&producers), ["<nAlias>", "b", "n"]);
    });
}

#[test]
fn distinct_ordinary_alias_consumers_retain_their_current_and_entry_contexts() {
    probe(Wiring::Aliases, |probe| {
        let producers = [
            ordinary(probe, "b", &["nAlias"], &["n"]),
            ordinary(probe, "a", &["nAlias"], &[]),
            ordinary(probe, "n", &[], &[]),
        ];
        assert_eq!(
            probe.issue_names(&producers),
            ["<nAlias>", "b", "n", "<nAlias>", "a"]
        );
    });
}

#[test]
fn a_periodic_observation_cycle_through_a_non_inline_alias_is_rejected() {
    probe(Wiring::Aliases, |probe| {
        let producers = [
            ordinary(probe, "b", &["nAlias"], &[]),
            ordinary(probe, "n", &["b"], &[]),
        ];
        assert!(matches!(
            probe.issue(&producers),
            Err(SameTickOrderError::Cycle { .. })
        ));
    });
}

#[test]
fn an_external_entry_reader_does_not_admit_a_periodic_observation_cycle() {
    probe(Wiring::Aliases, |probe| {
        let producers = [
            ordinary(probe, "b", &["nAlias"], &[]),
            ordinary(probe, "n", &["b"], &[]),
            ordinary(probe, "a", &["n"], &["b"]),
        ];
        assert!(matches!(
            probe.issue(&producers),
            Err(SameTickOrderError::Cycle { .. })
        ));
    });
}
