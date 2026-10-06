//! Compare replay with the original checked guard walker, not a numeric oracle.
mod fixture;
use super::*;

fn roots(view: dae::DaeView<'_>) -> Vec<dae::ExprId<'_>> {
    (0..view.expression_count())
        .filter_map(|i| view.expression_id(i))
        .filter(|expr| {
            matches!(
                view.expression(*expr).unwrap().operation(),
                dae::ExpressionOperation::Call { .. }
            )
        })
        .rev()
        .take(2)
        .collect()
}

fn project<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ExprId<'dae>,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
    filtered: bool,
) -> Result<Vec<(dae::CoordinateView<'dae>, usize)>, String> {
    let mut values = Vec::new();
    let result = if filtered {
        for_each_scalar_coordinate_filtered_cached(
            view,
            root,
            0,
            None,
            cache,
            |_| false,
            |coordinate, scalar| values.push((coordinate, scalar)),
        )
    } else {
        for_each_scalar_coordinate_cached(view, root, 0, None, cache, |coordinate, scalar| {
            values.push((coordinate, scalar))
        })
    };
    result
        .map(|()| values)
        .map_err(|error| format!("{error:?}"))
}

fn compare_caches<'dae>(
    a: &ScalarCoordinateProjectionCache<'dae>,
    b: &ScalarCoordinateProjectionCache<'dae>,
) {
    assert_eq!(
        a.function_results, b.function_results,
        "complete formal capture order"
    );
    assert_eq!(
        a.completed_folds, b.completed_folds,
        "complete reachable closures"
    );
    assert_eq!(
        a.fold_edges, b.fold_edges,
        "ordered node discovery and exact edges"
    );
}

fn check_case(offset: i64, only_outer: bool, call: bool) -> (u64, bool) {
    check_model(fixture::model(offset, only_outer, call))
}

fn check_model(model: dae::Dae) -> (u64, bool) {
    let mut hits = 0;
    let mut failed = false;
    model.inspect(|view| {
        let mut optimized = ScalarCoordinateProjectionCache::default();
        let mut reference = ScalarCoordinateProjectionCache {
            uncached_guard_memo: true,
            ..Default::default()
        };
        for root in roots(view).into_iter().cycle().take(4) {
            let expected = project(view, root, &mut reference, true);
            failed |= expected.is_err();
            assert_eq!(project(view, root, &mut optimized, true), expected);
            assert_eq!(
                project(view, root, &mut optimized, false),
                project(view, root, &mut reference, false)
            );
        }
        compare_caches(&optimized, &reference);
        assert_eq!(reference.guard_memo_hits, 0);
        assert_eq!(
            optimized.query_validation.len(),
            reference.query_validation.len()
        );
        for (function, cache) in &optimized.query_validation {
            compare_caches(cache, reference.query_validation.get(function).unwrap());
        }
        hits = optimized.guard_memo_hits;
    });
    (hits, failed)
}

#[test]
fn guard_replay_retains_nested_shadowed_parents_edges_and_first_callback_order() {
    for only_outer in [false, true] {
        for call in [false, true] {
            let (hits, failed) = check_case(0, only_outer, call);
            assert!(!failed, "unused invalid result is not newly visited");
            assert!(
                hits > 0,
                "guard effects are actually replayed across carried nodes"
            );
        }
    }
}

#[test]
fn guard_replay_does_not_publish_failed_bounds_after_prior_successful_points() {
    for offset in [3, 4, -1] {
        for call in [false, true] {
            let (_, failed) = check_case(offset, false, call);
            assert_eq!(failed, offset != 3);
        }
    }
}

#[test]
fn size_guard_replay_preserves_dimension_capture_nested_size_and_complete_lexical_graph() {
    for mode in [
        fixture::SizeMode::Literal,
        fixture::SizeMode::Selected,
        fixture::SizeMode::Nested,
    ] {
        let (hits, failed) = check_model(fixture::size_model(0, mode));
        assert!(!failed);
        assert!(hits > 0, "Size guards must actually replay effects");
    }
}

#[test]
fn size_guard_dimension_selector_bounds_retain_early_and_late_errors() {
    for mode in [fixture::SizeMode::Selected, fixture::SizeMode::Nested] {
        for offset in [-1, 1, 4] {
            let (_, failed) = check_model(fixture::size_model(offset, mode));
            assert!(failed, "dimension input indices retain their bounds error");
        }
    }
}

#[test]
fn floor_guard_replay_preserves_helpers_shadowed_parents_and_complete_captures() {
    for call in [false, true] {
        let (hits, failed) = check_model(fixture::floor_model(0, call));
        assert!(!failed);
        assert!(hits > 0, "Floor guard must replay dependency effects");
    }
}

#[test]
fn floor_guard_replay_retains_early_late_errors_and_unused_result_selection() {
    for offset in [-1, 3, 4] {
        for call in [false, true] {
            let (_, failed) = check_model(fixture::floor_model(offset, call));
            assert_eq!(failed, offset != 3);
        }
    }
}

#[test]
fn abs_guard_replay_preserves_helpers_shadowed_parents_and_complete_captures() {
    for call in [false, true] {
        let (hits, failed) = check_model(fixture::abs_model(0, call));
        assert!(!failed);
        assert!(hits > 0, "Abs guard must replay dependency effects");
    }
}

#[test]
fn abs_guard_replay_retains_early_late_errors_and_unused_result_selection() {
    for offset in [-1, 3, 4] {
        for call in [false, true] {
            let (_, failed) = check_model(fixture::abs_model(offset, call));
            assert_eq!(failed, offset != 3);
        }
    }
}
