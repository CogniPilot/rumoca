use super::*;

fn scope() -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            id: 0,
            display_name: "i".to_string(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    }
}

pub(in crate::projection) fn domains_model() -> dae::Dae {
    let text = "for i in 1:2 loop for i in 1:2 loop end for; end for;";
    let mut sources = SourceMap::new();
    let source = sources.add("context_shadowing.mo", text);
    let at = provenance(source, 0, text.len());
    dae::Dae::construct(sources, |model| {
        model.domains(|domains| {
            let outer = domains.structured(scope(), at)?;
            domains.nested(outer, scope(), at)?;
            domains.nested(outer, scope(), at)?;
            domains.structured(scope(), at)?;
            Ok(())
        })
    })
    .unwrap()
}

// Original exhaustive lexical filtering, independent of the context interner.
fn reference_context<'dae>(
    view: dae::DaeView<'dae>,
    points: &[(dae::DomainId<'dae>, Vec<i64>)],
    mut domain: Option<dae::DomainId<'dae>>,
) -> Vec<(u32, Vec<i64>)> {
    let mut ancestors = Vec::new();
    while let Some(current) = domain {
        ancestors.push(current);
        domain = view.domain(current).unwrap().parent();
    }
    points
        .iter()
        .filter(|(domain, _)| ancestors.contains(domain))
        .map(|(domain, point)| (domain.index(), point.clone()))
        .collect()
}

fn check_exhaustive_contexts(view: dae::DaeView<'_>) {
    let domains = (0..4)
        .map(|i| view.domain_id(i).unwrap())
        .collect::<Vec<_>>();
    let mut contexts = crate::projection::domain_context::DomainContexts::default();
    let mut expected_ids = HashMap::default();
    let mut observed_values = HashMap::default();
    // Every stack of length 0..4 over four typed domains and two points.
    // Includes siblings, unrelated scopes, repeated IDs, and reversed order.
    for length in 0..=4_u32 {
        for encoded in 0..8_usize.pow(length) {
            let mut remainder = encoded;
            let points = (0..length)
                .map(|_| {
                    let digit = remainder % 8;
                    remainder /= 8;
                    (domains[digit / 2], vec![(digit % 2 + 1) as i64])
                })
                .collect::<Vec<_>>();
            while !contexts.points.is_empty() {
                contexts.pop();
            }
            for (domain, point) in &points {
                contexts.push(*domain, point.clone());
            }
            let full = contexts.full_context(true).unwrap();
            let expected = points
                .iter()
                .map(|(domain, point)| (domain.index(), point.clone()))
                .collect::<Vec<_>>();
            assert_eq!(contexts.snapshot(full).as_ref(), &expected);
            assert_eq!(*expected_ids.entry(expected.clone()).or_insert(full), full);
            assert_eq!(contexts.full_context(false), Some(full));
            assert_eq!(
                observed_values.entry(full).or_insert(expected.clone()),
                &expected
            );
            for domain in std::iter::once(None).chain(domains.iter().copied().map(Some)) {
                let expected = reference_context(view, &points, domain);
                let actual = contexts.for_domain(view, domain);
                assert_eq!(contexts.snapshot(actual).as_ref(), &expected);
                assert_eq!(
                    *expected_ids.entry(expected.clone()).or_insert(actual),
                    actual
                );
                assert_eq!(
                    observed_values.entry(actual).or_insert(expected.clone()),
                    &expected
                );
            }
        }
    }
    assert_eq!(expected_ids.len(), observed_values.len());
}

#[test]
fn interned_contexts_match_exhaustive_lexical_stacks_with_shadowing_and_repeats() {
    domains_model().inspect(check_exhaustive_contexts);
}

#[test]
fn unchanged_stack_builds_one_context_and_push_pop_restores_exact_identity() {
    domains_model().inspect(|view| {
        let outer = view.domain_id(0).unwrap();
        let inner = view.domain_id(1).unwrap();
        let sibling = view.domain_id(2).unwrap();
        let mut contexts =
            crate::projection::domain_context::DomainContexts::new(vec![(outer, vec![1])]);
        let first = contexts.for_domain(view, Some(inner));
        for _ in 0..1000 {
            assert_eq!(contexts.for_domain(view, Some(inner)), first);
        }
        assert_eq!(
            contexts.builds, 1,
            "no point-vector clone per expression visit"
        );
        contexts.push(sibling, vec![2]);
        assert_eq!(contexts.for_domain(view, Some(inner)), first);
        contexts.push(inner, vec![1]);
        let second = contexts.for_domain(view, Some(inner));
        assert_ne!(first, second);
        contexts.push(inner, vec![2]);
        assert_ne!(contexts.for_domain(view, Some(inner)), second);
        contexts.pop();
        assert_eq!(contexts.for_domain(view, Some(inner)), second);
        contexts.pop();
        contexts.pop();
        assert_eq!(contexts.for_domain(view, Some(inner)), first);
    });
}
