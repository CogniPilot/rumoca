//! Exact indexed-write projection and authoritative traversal controls.
mod fixture;
use super::*;
use fixture::{Case, Guard, Update};

fn project<'dae>(
    view: dae::DaeView<'dae>,
    scalar: usize,
    cache: &mut ScalarCoordinateProjectionCache<'dae>,
) -> Result<Vec<(u32, usize)>, ProjectionError> {
    let root = view.expression_id(view.expression_count() - 1).unwrap();
    let mut values = Vec::new();
    for_each_scalar_coordinate_cached(view, root, scalar, None, cache, |coordinate, scalar| {
        let dae::CoordinateView::Algebraic(variable) = coordinate else {
            panic!("algebraic fixture");
        };
        values.push((variable.index(), scalar));
    })?;
    Ok(values)
}

fn compare(case: Case, selected: &[usize], shortcut: bool) {
    fixture::model(&case).unwrap().inspect(|view| {
        let mut optimized = ScalarCoordinateProjectionCache::default();
        let mut reference = ScalarCoordinateProjectionCache {
            uncached_indexed_write_folds: true,
            ..Default::default()
        };
        for &scalar in selected {
            assert_eq!(
                project(view, scalar, &mut optimized).unwrap(),
                project(view, scalar, &mut reference).unwrap()
            );
            assert_eq!(optimized.function_results, reference.function_results);
            assert_eq!(optimized.completed_folds, reference.completed_folds);
            assert_eq!(
                optimized.fold_edges, reference.fold_edges,
                "exact source graph edge order"
            );
        }
        assert_eq!(optimized.indexed_write_folds > 0, shortcut);
        assert_eq!(reference.indexed_write_folds, 0);
        optimized.function_results.clear();
        reference.function_results.clear();
        for &scalar in selected {
            assert_eq!(
                project(view, scalar, &mut optimized).unwrap(),
                project(view, scalar, &mut reference).unwrap()
            );
        }
    });
}

#[test]
fn full14400_nonliteral_owned_write_keeps_exact_late_scalar_dependencies() {
    compare(
        Case {
            width: 14400,
            upper: 14400,
            update: Update::DirectNonLiteral,
            ..Default::default()
        },
        &[0, 1, 7199, 14399],
        true,
    );
}
#[test]
fn source_direction_stride_empty_and_singleton_keep_exact_edges() {
    for (lower, step, upper, width) in [
        (1, 1, 4, 4),
        (4, -1, 1, 4),
        (1, 2, 4, 4),
        (1, 1, 0, 4),
        (1, 1, 1, 1),
    ] {
        let selected = (0..width as usize).collect::<Vec<_>>();
        compare(
            Case {
                width,
                lower,
                step,
                upper,
                update: Update::DirectNonLiteral,
                ..Default::default()
            },
            &selected,
            true,
        );
    }
}
#[test]
fn other_carried_dependency_is_projected_not_evaluated_as_independent_pixels() {
    compare(
        Case {
            update: Update::DirectOtherCarry,
            ..Default::default()
        },
        &[0, 1, 3],
        true,
    );
}
#[test]
fn conditional_alias_and_no_write_paths_keep_authoritative_traversal() {
    for update in [
        Update::NonLiteral,
        Update::OtherCarry,
        Update::AliasIndex,
        Update::Literal,
    ] {
        compare(
            Case {
                update,
                ..Default::default()
            },
            &[0, 2, 3],
            false,
        );
    }
    compare(
        Case {
            guard: Guard::Call,
            ..Default::default()
        },
        &[0, 3],
        false,
    );
}
#[test]
fn failed_selected_replacement_or_out_of_range_write_never_publishes() {
    for case in [
        Case {
            update: Update::DirectNonLiteral,
            guard_offset: -1,
            ..Default::default()
        },
        Case {
            update: Update::DirectNonLiteral,
            upper: 5,
            ..Default::default()
        },
    ] {
        fixture::model(&case).unwrap().inspect(|view| {
            let mut errors = Vec::new();
            for disabled in [false, true] {
                errors.extend(repeated_fault(view, disabled));
            }
            assert!(errors.windows(2).all(|pair| pair[0] == pair[1]));
        });
    }
}

#[test]
fn invalid_unselected_replacement_is_not_newly_traversed() {
    compare(
        Case {
            update: Update::DirectNonLiteral,
            guard_offset: -1,
            ..Default::default()
        },
        &[1, 2, 3],
        true,
    );
    compare(
        Case {
            update: Update::WithoutPassthrough,
            ..Default::default()
        },
        &[0, 3],
        true,
    );
}
#[test]
fn invalid_model_coordinate_retains_the_constructor_refusal() {
    let error = fixture::model(&Case {
        guard: Guard::ModelCoordinate,
        ..Default::default()
    })
    .unwrap_err();
    assert!(matches!(
        error,
        dae::DaeConstructionError::InvalidFunctionCoordinate {
            coordinate: "algebraic",
            ..
        }
    ));
}

#[test]
fn nested_parent_points_keep_distinct_exact_checked_closures() {
    let case = Case {
        nested: true,
        update: Update::DirectNonLiteral,
        ..Default::default()
    };
    compare(
        Case {
            nested: true,
            update: Update::DirectNonLiteral,
            ..Default::default()
        },
        &[0, 1, 3],
        true,
    );
    fixture::model(&case).unwrap().inspect(|view| {
        let mut cache = ScalarCoordinateProjectionCache::default();
        project(view, 0, &mut cache).unwrap();
        let parents = cache
            .completed_folds
            .keys()
            .filter(|node| node.fold.ordinal() == 1)
            .map(|node| node.parent.as_ref().clone())
            .collect::<HashSet<_>>();
        assert_eq!(
            parents.len(),
            2,
            "both source outer binder points remain distinct"
        );
    });
}
#[test]
fn multiaxis_tensor_uses_the_full_reference_owner() {
    compare(
        Case {
            matrix: true,
            update: Update::DirectNonLiteral,
            ..Default::default()
        },
        &[0, 1, 3],
        false,
    );
}

mod record;
#[test]
fn record_field_fold_keeps_authoritative_exact_outer_and_field_coordinates() {
    record::model().inspect(|view| {
        let mut optimized = ScalarCoordinateProjectionCache::default();
        let mut reference = ScalarCoordinateProjectionCache {
            uncached_indexed_write_folds: true,
            ..Default::default()
        };
        for scalar in [0, 1] {
            assert_eq!(
                project(view, scalar, &mut optimized).unwrap(),
                project(view, scalar, &mut reference).unwrap()
            );
            assert_eq!(optimized.function_results, reference.function_results);
            assert_eq!(optimized.completed_folds, reference.completed_folds);
            assert_eq!(optimized.fold_edges, reference.fold_edges);
        }
        assert_eq!(optimized.indexed_write_folds, 0);
    });
}

#[test]
fn conditional_replacement_stays_under_its_exact_selected_source_point() {
    compare(
        Case {
            update: Update::DirectConditional,
            ..Default::default()
        },
        &[0, 2, 3],
        true,
    );
    compare(
        Case {
            update: Update::DirectConditional,
            guard_offset: -1,
            ..Default::default()
        },
        &[1, 2, 3],
        true,
    );
    compare(
        Case {
            update: Update::DirectAliasIndex,
            ..Default::default()
        },
        &[0, 2, 3],
        false,
    );
}

fn repeated_fault<'dae>(view: dae::DaeView<'dae>, disabled: bool) -> Vec<(i64, u32, Span)> {
    let mut cache = ScalarCoordinateProjectionCache {
        uncached_indexed_write_folds: disabled,
        ..Default::default()
    };
    let mut errors = Vec::new();
    for _ in 0..2 {
        let ProjectionError::IndexOutOfBounds {
            index,
            extent,
            span,
        } = project(view, 0, &mut cache).unwrap_err()
        else {
            panic!("original exact address error");
        };
        errors.push((index, extent, span));
        assert!(cache.function_results.is_empty());
        assert!(cache.completed_folds.is_empty());
    }
    errors
}
