//! Ordinary iterative ordering remains owned by the SOLVE-C58 exchange.

use super::*;

#[test]
fn clock_intermediate_projects_the_retained_family_point() {
    use crate::tests::TestSource;
    use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};

    let text = "for i in 3:-1:2, j in 2:2:6 loop middle[i,j] = input[i,j]; end for;";
    let source = TestSource::new(text);
    let at = source.at(0, text.len());
    let binder = |id, name: &str, lower, upper, step| StructuredIndexBinder {
        provenance: IndexBinderProvenance::Generated,
        id,
        display_name: name.into(),
        lower,
        upper,
        step,
    };
    let dae = dae::Dae::construct(source.map, |model| {
        model.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![binder(0, "i", 3, 2, -1), binder(1, "j", 2, 6, 2)],
                },
                at,
            )
        })?;
        Ok(())
    })
    .unwrap();

    dae.inspect(|view| {
        let domain = view.domain_id(0).unwrap();
        let span = at.span();
        assert_eq!(
            intermediate_definition_point(view, domain, 3, span).unwrap(),
            (domain, vec![2, 2])
        );
        let error = intermediate_definition_point(view, domain, 6, span).unwrap_err();
        assert_eq!(error.source_span(), Some(span));
        assert!(matches!(error, LowerError::ContractViolation { reason, .. }
            if reason == "definition family does not cover the intermediate scalar"));
    });
}

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
