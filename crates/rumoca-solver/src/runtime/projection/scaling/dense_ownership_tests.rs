use super::*;
use crate::runtime::projection::jacobian_values::CompactFixture;

thread_local! {
    static LU_STORAGE: std::cell::Cell<(usize, usize)> = const { std::cell::Cell::new((0, 0)) };
}

pub(super) fn observe_lu_storage(input: *const f64, factor: *const f64) {
    let (calls, copies) = LU_STORAGE.get();
    LU_STORAGE.set((calls + 1, copies + usize::from(input != factor)));
}

#[test]
fn dense_lu_consumes_the_actual_scaled_matrix_allocation() {
    let matrix = DMatrix::from_row_slice(2, 2, &[0.0, 2.0, 3.0, 4.0]);
    let (calls, copies) = LU_STORAGE.get();
    let delta = scaled_newton_delta(ScaledNewtonSystem {
        jacobian: &matrix,
        residual: &[-4.0, -11.0],
        row_scales: &[2.0, 3.0],
        variable_scales: &[5.0, 7.0],
        structure: None,
        tolerance: 1e-10,
    })
    .unwrap();
    assert!((delta[0] - 1.0).abs() < 1e-12);
    assert!((delta[1] - 2.0).abs() < 1e-12);
    assert_eq!(LU_STORAGE.get().0, calls + 1);
    assert_eq!(
        LU_STORAGE.get().1,
        copies,
        "LU must consume the scaled buffer, not copy it"
    );
}

// Frozen pre-change dense policy: preserve the scaled matrix by cloning for LU.
fn old_dense_reference(system: ScaledNewtonSystem<'_>, allow_svd: bool) -> Option<DVector<f64>> {
    let matrix = scaled_jacobian(system.jacobian, system.row_scales, system.variable_scales)?;
    let rhs = scaled_newton_rhs(system.residual, system.row_scales);
    let direct = if matrix.is_square() {
        matrix.clone().lu().solve(&rhs)
    } else {
        None
    };
    let delta = if allow_svd {
        direct.or_else(|| matrix.svd(true, true).solve(&rhs, system.tolerance).ok())?
    } else {
        direct?
    };
    Some(unscale_newton_delta(&delta, system.variable_scales))
}

fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}

#[test]
fn compact_scaling_matches_dense_and_rejects_foreign_pattern_without_dense_storage() {
    let provenance = solve::PatternProvenance::derived(
        solve::PatternDerivation::DependencyPropagation,
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("compact_scaling.mo"),
            0,
            1,
        ),
    )
    .unwrap();
    let pattern = solve::StructuralPattern::from_row_dependencies(
        3,
        3,
        &[vec![0, 2], vec![1], vec![0, 2]],
        provenance,
    )
    .unwrap();
    let foreign = solve::StructuralPattern::from_row_dependencies(
        3,
        3,
        &[vec![0, 1], vec![1], vec![0, 2]],
        provenance,
    )
    .unwrap();
    let dense = DMatrix::from_row_slice(3, 3, &[4.0, 0.0, -0.0, 0.0, -2.0, 0.0, 1.0, 0.0, 3.0]);
    let compact = CompactFixture::from_dense(pattern.clone(), &dense);
    assert_eq!(compact.stored_len(), 5);
    let variable = [0.5, 3.0, 2.0];
    let fallback = [10.0, 11.0, 12.0];
    let before = crate::runtime::projection::jacobian_allocation_count();
    let scales = jacobian_row_scales(&compact, &variable, &fallback, Some(&pattern)).unwrap();
    assert_eq!(
        scales,
        jacobian_row_scales(&dense, &variable, &fallback, Some(&pattern)).unwrap()
    );
    assert_eq!(
        scales,
        jacobian_row_scales(&compact, &variable, &fallback, None).unwrap()
    );
    assert!(jacobian_row_scales(&compact, &variable, &fallback, Some(&foreign)).is_err());
    assert_eq!(
        crate::runtime::projection::jacobian_allocation_count(),
        before
    );
    assert_eq!(
        bits(
            scaled_jacobian(&compact, &scales, &variable)
                .unwrap()
                .as_slice()
        ),
        bits(
            scaled_jacobian(&dense, &scales, &variable)
                .unwrap()
                .as_slice()
        )
    );
    let rhs = DVector::from_column_slice(&[-1.0, 2.0, -3.0]);
    let compact_delta =
        sparse_scaled_newton_delta(&compact, &rhs, &scales, &variable, &pattern, None).unwrap();
    let dense_delta =
        sparse_scaled_newton_delta(&dense, &rhs, &scales, &variable, &pattern, None).unwrap();
    assert_eq!(bits(compact_delta.as_slice()), bits(dense_delta.as_slice()));
    assert_eq!(
        crate::runtime::projection::jacobian_allocation_count(),
        before
    );
    let mut nonfinite = dense.clone();
    nonfinite[(0, 0)] = f64::NAN;
    nonfinite[(1, 1)] = f64::INFINITY;
    let compact_nonfinite = CompactFixture::from_dense(pattern.clone(), &nonfinite);
    assert_eq!(
        jacobian_row_scales(&compact_nonfinite, &variable, &fallback, Some(&pattern)).unwrap(),
        jacobian_row_scales(&nonfinite, &variable, &fallback, Some(&pattern)).unwrap()
    );
    assert_eq!(
        bits(
            scaled_jacobian(&compact_nonfinite, &scales, &variable)
                .unwrap()
                .as_slice()
        ),
        bits(
            scaled_jacobian(&nonfinite, &scales, &variable)
                .unwrap()
                .as_slice()
        )
    );
}

#[test]
fn compact_newton_rejects_foreign_pattern_before_sparse_or_dense_fallback() {
    let provenance = solve::PatternProvenance::derived(
        solve::PatternDerivation::DependencyPropagation,
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("foreign_newton_pattern.mo"),
            0,
            1,
        ),
    )
    .unwrap();
    for dimension in [2, 17] {
        let mut expected_rows: Vec<_> = (0..dimension).map(|row| vec![row]).collect();
        expected_rows[0].push(1);
        let expected = solve::StructuralPattern::from_row_dependencies(
            dimension,
            dimension,
            &expected_rows,
            provenance,
        )
        .unwrap();
        let mut foreign_rows = expected_rows;
        foreign_rows[0] = vec![1];
        foreign_rows[1] = vec![0, 1];
        let foreign = solve::StructuralPattern::from_row_dependencies(
            dimension,
            dimension,
            &foreign_rows,
            provenance,
        )
        .unwrap();
        assert_ne!(expected, foreign);
        assert_eq!(
            select_linear_solve_kernel(dimension, &expected).unwrap(),
            if dimension == 2 {
                LinearSolveKernel::SmallDense
            } else {
                LinearSolveKernel::SparseCandidate
            }
        );
        let mut dense = DMatrix::identity(dimension, dimension);
        dense[(0, 0)] = 0.0;
        dense[(0, 1)] = 1.0;
        dense[(1, 0)] = 1.0;
        let compact = CompactFixture::from_dense(foreign.clone(), &dense);
        assert_eq!(compact.stored_len(), dimension + 1);
        let residual = vec![-1.0; dimension];
        let scales = vec![1.0; dimension];
        let system = |structure| ScaledNewtonSystem {
            jacobian: &compact,
            residual: &residual,
            row_scales: &scales,
            variable_scales: &scales,
            structure,
            tolerance: 1e-10,
        };
        assert!(scaled_newton_delta(system(Some(&foreign))).is_some());
        let before_dense = SCALED_DENSE_ALLOCATIONS.get();
        let before_lu = LU_STORAGE.get();
        for structure in [Some(&expected), None] {
            for allow_svd in [false, true] {
                assert!(scaled_newton_delta_impl(system(structure), None, allow_svd).is_none());
            }
            let mut cache = SparseNewtonCache::default();
            assert!(scaled_newton_delta_with_cache(system(structure), &mut cache).is_none());
        }
        assert_eq!(SCALED_DENSE_ALLOCATIONS.get(), before_dense);
        assert_eq!(LU_STORAGE.get(), before_lu);
    }
}

fn assert_old_reference(system: ScaledNewtonSystem<'_>, allow_svd: bool) -> Option<DVector<f64>> {
    let before = [
        bits(system.jacobian.as_slice()),
        bits(system.residual),
        bits(system.row_scales),
        bits(system.variable_scales),
    ];
    let expected = old_dense_reference(system, allow_svd);
    let actual = scaled_newton_delta_impl(system, None, allow_svd);
    assert_eq!(
        actual.as_ref().map(|v| bits(v.as_slice())),
        expected.as_ref().map(|v| bits(v.as_slice()))
    );
    assert_eq!(
        before,
        [
            bits(system.jacobian.as_slice()),
            bits(system.residual),
            bits(system.row_scales),
            bits(system.variable_scales)
        ]
    );
    actual
}

#[test]
fn owned_lu_matches_old_pivoting_solution_and_preserves_inputs() {
    let matrix = DMatrix::from_row_slice(3, 3, &[0.0, 2.0, -1.0, 3.0, 4.0, 2.0, 2.0, -1.0, 5.0]);
    for allow_svd in [false, true] {
        let delta = assert_old_reference(
            ScaledNewtonSystem {
                jacobian: &matrix,
                residual: &[-7.0, -5.0, 15.0],
                row_scales: &[0.25, 7.0, 3.0],
                variable_scales: &[4.0, 0.5, 9.0],
                structure: None,
                tolerance: 1e-10,
            },
            allow_svd,
        )
        .unwrap();
        assert!((&matrix * delta + DVector::from_column_slice(&[-7.0, -5.0, 15.0])).amax() < 1e-12);
    }
}

#[test]
fn failed_owned_lu_rebuilds_original_svd_input_and_preserves_disabled_policy() {
    let matrix = DMatrix::from_row_slice(2, 2, &[1.0, 2.0, 2.0, 4.0]);
    for tolerance in [1e-10, 10.0] {
        let system = ScaledNewtonSystem {
            jacobian: &matrix,
            residual: &[-3.0, -6.0],
            row_scales: &[2.0, 0.5],
            variable_scales: &[0.25, 4.0],
            structure: None,
            tolerance,
        };
        assert!(assert_old_reference(system, false).is_none());
        let delta = assert_old_reference(system, true).unwrap();
        if tolerance < 1.0 {
            assert!((&matrix * delta - DVector::from_column_slice(&[3.0, 6.0])).amax() < 1e-12);
        }
    }
}

#[test]
fn rectangular_dense_system_preserves_square_guard_and_svd_policy() {
    for matrix in [
        DMatrix::from_row_slice(2, 3, &[1.0, 0.0, 1.0, 0.0, 2.0, 1.0]),
        DMatrix::from_row_slice(3, 2, &[1.0, 0.0, 0.0, 2.0, 1.0, 1.0]),
    ] {
        let residual = vec![-2.0; matrix.nrows()];
        let rows = vec![2.0; matrix.nrows()];
        let columns = vec![0.5; matrix.ncols()];
        let system = ScaledNewtonSystem {
            jacobian: &matrix,
            residual: &residual,
            row_scales: &rows,
            variable_scales: &columns,
            structure: None,
            tolerance: 1e-10,
        };
        let before = LU_STORAGE.get();
        assert!(assert_old_reference(system, false).is_none());
        assert!(assert_old_reference(system, true).is_some());
        assert_eq!(
            LU_STORAGE.get(),
            before,
            "rectangular input must never enter LU"
        );
    }
}

#[test]
fn owned_lu_keeps_some_nonfinite_without_new_fallback() {
    for value in [f64::NAN, f64::INFINITY] {
        let matrix = DMatrix::from_element(1, 1, value);
        for allow_svd in [false, true] {
            let delta = assert_old_reference(
                ScaledNewtonSystem {
                    jacobian: &matrix,
                    residual: &[f64::NAN],
                    row_scales: &[1.0],
                    variable_scales: &[1.0],
                    structure: None,
                    tolerance: 1e-10,
                },
                allow_svd,
            )
            .unwrap();
            assert!(delta[0].is_nan());
        }
    }
}

#[test]
fn owned_lu_preserves_sparse_success_and_decline_to_dense() {
    let n = 17;
    let mut dependencies: Vec<_> = (0..n).map(|row| vec![row]).collect();
    dependencies[0].push(1);
    let provenance = solve::PatternProvenance::derived(
        solve::PatternDerivation::DependencyPropagation,
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("sparse_decline.mo"),
            0,
            1,
        ),
    )
    .unwrap();
    let pattern =
        solve::StructuralPattern::from_row_dependencies(n, n, &dependencies, provenance).unwrap();
    assert_eq!(
        select_linear_solve_kernel(n, &pattern).unwrap(),
        LinearSolveKernel::SparseCandidate
    );
    let mut matrix = DMatrix::identity(n, n);
    matrix[(0, 1)] = 0.5;
    let residual = vec![-1.0; n];
    let scales = vec![1.0; n];
    for singular in [false, true] {
        matrix[(n - 1, n - 1)] = if singular { 0.0 } else { 1.0 };
        let system = ScaledNewtonSystem {
            jacobian: &matrix,
            residual: &residual,
            row_scales: &scales,
            variable_scales: &scales,
            structure: Some(&pattern),
            tolerance: 1e-10,
        };
        let rhs = scaled_newton_rhs(&residual, &scales);
        assert_eq!(
            sparse_scaled_newton_delta(&matrix, &rhs, &scales, &scales, &pattern, None).is_none(),
            singular
        );
        let before = LU_STORAGE.get().0;
        assert!(assert_old_reference(system, true).is_some());
        assert_eq!(LU_STORAGE.get().0 - before, usize::from(singular));
    }
}
