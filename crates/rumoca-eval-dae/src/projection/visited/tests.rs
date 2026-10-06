use super::*;
use crate::projection::{domain_context::DomainContexts, tests::domain_context::domains_model};

fn coordinate_keys() -> impl Iterator<Item = (u32, Option<usize>, usize)> {
    [0, 1, 65_535, 65_536, u32::MAX]
        .into_iter()
        .flat_map(|expression| {
            [None, Some(0), Some(3)].into_iter().flat_map(move |field| {
                [0, 1, 63, 64, 511, 512, 4096, 65_535, 65_536, usize::MAX]
                    .into_iter()
                    .map(move |scalar| (expression, field, scalar))
            })
        })
}

fn compare_all_lexical_keys(
    actual: &mut Visited,
    expected: &mut HashSet<ScalarExpressionDependency>,
    identities: &[DomainContextId],
) {
    for round in 0..3 {
        for (expression, field, scalar) in coordinate_keys() {
            for context in identities
                .iter()
                .copied()
                .cycle()
                .skip(round)
                .take(identities.len())
            {
                let key = ScalarExpressionDependency {
                    activation: crate::projection::Activation::Guaranteed,
                    expression,
                    field,
                    scalar,
                    domain_context: context,
                };
                assert_eq!(actual.insert(key.clone()), expected.insert(key));
            }
        }
    }
}

#[test]
fn exact_membership_matches_hashset_across_fields_scalars_and_lexical_contexts() {
    domains_model().inspect(|view| {
        let mut contexts = DomainContexts::default();
        let mut identities = vec![DomainContextId::default()];
        for ordinal in 0..4 {
            let domain = view.domain_id(ordinal).unwrap();
            for point in [1, 2] {
                contexts.push(domain, vec![point]);
                identities.push(contexts.for_domain(view, Some(domain)));
                contexts.pop();
            }
        }
        let mut actual = Visited {
            word_limit: 16,
            scope_limit: 3,
            ..Default::default()
        };
        let mut expected = HashSet::default();
        compare_all_lexical_keys(&mut actual, &mut expected, &identities);
        assert!(actual.words <= 16);
        assert!(actual.scopes <= 3);
        assert!(actual.expressions.len() <= DENSE_LIMIT);
        assert!(!actual.sparse.is_empty());
    });
}

#[test]
fn sparse_fallback_cannot_duplicate_a_key_after_later_dense_growth() {
    let mut actual = Visited::default();
    let mut expected = HashSet::default();
    // First1024 is too far for a fresh bitmap and uses the sparse set. Grow
    // the bitmap afterward: membership must still account for that old key.
    for scalar in [1024].into_iter().chain(0..=1024).chain([1024, 64, 1023]) {
        let key = ScalarExpressionDependency {
            activation: crate::projection::Activation::Guaranteed,
            expression: 3,
            field: None,
            scalar,
            domain_context: Default::default(),
        };
        assert_eq!(actual.insert(key.clone()), expected.insert(key));
    }
}

#[test]
fn fold_node_clear_replays_membership_without_carrying_previous_node_addresses() {
    let mut actual = Visited {
        word_limit: 3,
        ..Default::default()
    };
    let mut expected = HashSet::default();
    for round in 0..8 {
        for (expression, field, scalar) in coordinate_keys() {
            let key = ScalarExpressionDependency {
                activation: crate::projection::Activation::Guaranteed,
                expression,
                field,
                scalar,
                domain_context: Default::default(),
            };
            assert_eq!(actual.insert(key.clone()), expected.insert(key));
        }
        if round % 2 == 0 {
            actual.clear();
            expected.clear();
        }
        assert!(actual.words <= 3);
    }
    actual.generation = u64::MAX;
    actual.clear();
    expected.clear();
    for (expression, field, scalar) in coordinate_keys() {
        let key = ScalarExpressionDependency {
            activation: crate::projection::Activation::Guaranteed,
            expression,
            field,
            scalar,
            domain_context: Default::default(),
        };
        assert_eq!(actual.insert(key.clone()), expected.insert(key));
    }
}

#[test]
fn context_pages_pack_exact_identities_with_bounded_sparse_high_addresses() {
    let mut actual = Visited {
        scope_limit: 3,
        word_limit: 256,
        ..Default::default()
    };
    let mut expected = HashSet::default();
    let contexts = (1..=160).chain([usize::MAX, 64, 63, 1, 0]);
    let keys = contexts
        .flat_map(|context| {
            [(3, 0), (3, 1), (4, 0)].map(|(expression, scalar)| ScalarExpressionDependency {
                activation: crate::projection::Activation::Guaranteed,
                expression,
                field: None,
                scalar,
                domain_context: DomainContextId::test_identity(context),
            })
        })
        .collect::<Vec<_>>();
    for round in 0..4 {
        for key in keys.iter().chain(keys.iter().rev()) {
            assert_eq!(actual.insert(key.clone()), expected.insert(key.clone()));
        }
        assert_eq!(actual.scopes, 3);
        assert!(actual.words <= 256);
        assert!(actual.expressions.len() <= 5);
        if round % 2 == 0 {
            actual.clear();
            expected.clear();
        }
    }
}

#[test]
fn packed_context_pages_keep_expression_and_scalar_separate() {
    let mut actual = Visited::default();
    let mut expected = HashSet::default();
    for scalar in [0, 5, 65_535, usize::MAX] {
        for expression in [0, 1, u32::MAX] {
            for context in [0, 1, 63, 64, 65, 127, 128, usize::MAX] {
                let key = ScalarExpressionDependency {
                    activation: crate::projection::Activation::Guaranteed,
                    expression,
                    field: None,
                    scalar,
                    domain_context: DomainContextId::test_identity(context),
                };
                assert_eq!(actual.insert(key.clone()), expected.insert(key.clone()));
                assert_eq!(actual.insert(key.clone()), expected.insert(key));
            }
        }
    }
}

#[test]
fn full_scoped_storage_is_reclaimed_only_after_membership_clear() {
    let mut actual = Visited {
        scope_limit: 2,
        word_limit: 129,
        ..Default::default()
    };
    let mut expected = HashSet::default();
    for generation in 0..5 {
        // Keep a plain allocation and use disjoint scoped pages each round.
        for context in [0, 1 + generation * 256, 64 + generation * 256, usize::MAX] {
            let key = ScalarExpressionDependency {
                activation: crate::projection::Activation::Guaranteed,
                expression: 3,
                field: None,
                scalar: 0,
                domain_context: DomainContextId::test_identity(context),
            };
            assert_eq!(actual.insert(key.clone()), expected.insert(key.clone()));
            assert_eq!(actual.insert(key.clone()), expected.insert(key));
        }
        assert_eq!(actual.scopes, 2);
        assert_eq!(actual.words, 4);
        actual.clear();
        expected.clear();
        assert_eq!(actual.scopes, 0);
        assert_eq!(actual.words, 1);
        assert!(actual.expressions[&3].scoped.is_empty());
    }
}

#[test]
fn context_tile_word_budget_cannot_duplicate_sparse_rows_after_lower_row_growth() {
    let mut actual = Visited {
        scope_limit: 8,
        word_limit: 4,
        ..Default::default()
    };
    let mut expected = HashSet::default();
    for _ in 0..4 {
        for context in [63, 1, 2, 3, 63, 4, 1, 64, usize::MAX] {
            for scalar in [0, 5, 63, 64] {
                let key = ScalarExpressionDependency {
                    activation: crate::projection::Activation::Guaranteed,
                    expression: 7,
                    field: None,
                    scalar,
                    domain_context: DomainContextId::test_identity(context),
                };
                assert_eq!(actual.insert(key.clone()), expected.insert(key.clone()));
                assert_eq!(actual.insert(key.clone()), expected.insert(key));
            }
        }
        assert!(actual.words <= 4);
        assert!(actual.scopes <= 8);
        actual.clear();
        expected.clear();
    }
}

#[test]
fn activation_retains_dense_scoped_and_sparse_membership_in_both_orders() {
    use crate::projection::Activation;
    for reverse in [false, true] {
        let modes = if reverse {
            [Activation::Guaranteed, Activation::Conditional]
        } else {
            [Activation::Conditional, Activation::Guaranteed]
        };
        let mut visited = Visited::default();
        for (expression, field, scalar, context) in [
            (3, None, 7, 0),
            (3, None, 7, 65),
            (3, Some(0), 7, 65),
            (u32::MAX, None, 7, 0),
            (3, None, usize::MAX, 0),
        ] {
            for activation in modes {
                let key = ScalarExpressionDependency {
                    activation,
                    expression,
                    field,
                    scalar,
                    domain_context: DomainContextId::test_identity(context),
                };
                assert!(
                    visited.insert(key.clone()),
                    "mode changes semantic membership"
                );
                assert!(!visited.insert(key));
            }
        }
        visited.clear();
        for activation in modes {
            assert!(visited.insert(ScalarExpressionDependency {
                activation,
                expression: 3,
                field: None,
                scalar: 7,
                domain_context: DomainContextId::default()
            }));
        }
    }
}
