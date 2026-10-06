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
            panic!("fixture uses checked algebraic actual inputs");
        };
        values.push((variable.index(), scalar));
    })?;
    Ok(values)
}

fn compare_success(case: Case, expected_sweep: bool) {
    fixture::model(&case).unwrap().inspect(|view| {
        let mut optimized = ScalarCoordinateProjectionCache::default();
        let mut original = ScalarCoordinateProjectionCache {
            uncached_literal_update_sweeps: true,
            uncached_parameter_fragments: true,
            ..Default::default()
        };
        assert_eq!(
            project(view, 0, &mut optimized).unwrap(),
            project(view, 0, &mut original).unwrap(),
            "exact callback order"
        );
        assert_eq!(
            optimized.function_results, original.function_results,
            "complete ordered function dependencies"
        );
        assert_eq!(
            optimized.completed_folds, original.completed_folds,
            "every scalar/parent closure remains exact"
        );
        assert_eq!(
            optimized.fold_edges, original.fold_edges,
            "every checked node and ordered edge remains exact"
        );
        assert_eq!(optimized.sweep_hits > 0, expected_sweep);
        assert_eq!(original.sweep_hits, 0);
    });
}

#[test]
fn literal_update_guard_sweep_matches_full_walker_after_width_and_domain_edits() {
    for width in [3, 4, 6] {
        compare_success(
            Case {
                width,
                upper: i64::from(width),
                ..Default::default()
            },
            true,
        );
    }
    compare_success(
        Case {
            lower: 2,
            upper: 3,
            ..Default::default()
        },
        true,
    );
}

#[test]
fn empty_negative_step_nonliteral_alias_and_call_shapes_keep_the_full_walker() {
    compare_success(
        Case {
            lower: 1,
            upper: 0,
            ..Default::default()
        },
        false,
    );
    compare_success(
        Case {
            lower: 4,
            upper: 1,
            step: -1,
            ..Default::default()
        },
        false,
    );
    for update in [
        Update::NonLiteral,
        Update::OtherCarry,
        Update::AliasIndex,
        Update::WithoutPassthrough,
    ] {
        compare_success(
            Case {
                update,
                ..Default::default()
            },
            false,
        );
    }
    compare_success(
        Case {
            guard: Guard::Call,
            ..Default::default()
        },
        false,
    );
    compare_success(
        Case {
            width: 1,
            upper: 1,
            update: Update::WithoutPassthrough,
            ..Default::default()
        },
        false,
    );
}

#[test]
fn model_coordinate_guard_remains_a_checked_function_constructor_refusal() {
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

fn compare_fault(case: Case) {
    fixture::model(&case).unwrap().inspect(|view| {
        let mut errors = Vec::new();
        for reference in [false, true] {
            let mut cache = ScalarCoordinateProjectionCache {
                uncached_literal_update_sweeps: reference,
                uncached_parameter_fragments: reference,
                ..Default::default()
            };
            let error = project(view, 0, &mut cache).unwrap_err();
            let ProjectionError::IndexOutOfBounds {
                index,
                extent,
                span,
            } = error
            else {
                panic!("this control requires the original exact address refusal");
            };
            errors.push((index, extent, span));
            assert!(cache.function_results.is_empty());
            assert!(cache.completed_folds.is_empty());
            assert!(cache.fold_edges.is_empty());
        }
        assert_eq!(
            errors[0], errors[1],
            "failed sweeps never publish or alter the exact source/address diagnostic"
        );
    });
}

#[test]
fn guard_and_update_address_faults_match_original_before_any_completed_publication() {
    compare_fault(Case {
        guard_offset: -1,
        ..Default::default()
    });
    compare_fault(Case {
        guard_offset: 1,
        ..Default::default()
    });
    compare_fault(Case {
        upper: 5,
        ..Default::default()
    });
}
