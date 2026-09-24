use faer::{Col, prelude::Solve};
use rumoca_core::{SourceId, Span};
use rumoca_ir_solve::{PatternDerivation, PatternProvenance};

use super::*;

fn pattern(dimension: usize, upper: bool) -> StructuralPattern {
    let rows = (0..dimension)
        .map(|row| {
            let neighbor = if upper {
                (row + 1) % dimension
            } else {
                (row + dimension - 1) % dimension
            };
            vec![row, neighbor]
        })
        .collect::<Vec<_>>();
    StructuralPattern::from_row_dependencies(
        dimension,
        dimension,
        &rows,
        PatternProvenance::derived(
            PatternDerivation::DependencyPropagation,
            Span::from_offsets(SourceId::from_source_name("SparseProjection.mo"), 0, 1),
        )
        .unwrap(),
    )
    .unwrap()
}

fn matrix(pattern: &StructuralPattern, diagonal: f64) -> DMatrix<f64> {
    let mut matrix = DMatrix::zeros(pattern.rows() as usize, pattern.columns() as usize);
    for (row, column) in pattern.nonzero_coordinates() {
        matrix[(row, column)] = if row == column { diagonal } else { -0.5 };
    }
    matrix
}

fn fresh_solution(
    matrix: &DMatrix<f64>,
    rhs: &DVector<f64>,
    row_scales: &[f64],
    variable_scales: &[f64],
    pattern: &StructuralPattern,
) -> DVector<f64> {
    try_fresh_solution(matrix, rhs, row_scales, variable_scales, pattern).unwrap()
}

fn try_fresh_solution(
    matrix: &DMatrix<f64>,
    rhs: &DVector<f64>,
    row_scales: &[f64],
    variable_scales: &[f64],
    pattern: &StructuralPattern,
) -> Option<DVector<f64>> {
    let triplets = pattern
        .nonzero_coordinates()
        .into_iter()
        .map(|(row, column)| {
            Triplet::new(
                row,
                column,
                matrix[(row, column)] * valid_variable_scale(variable_scales[column])
                    / valid_variable_scale(row_scales[row]),
            )
        })
        .collect::<Vec<_>>();
    let sparse =
        SparseColMat::try_new_from_triplets(matrix.nrows(), matrix.ncols(), &triplets).unwrap();
    let solution = sparse
        .sp_lu()
        .ok()?
        .solve(&Col::from_fn(rhs.len(), |row| rhs[row]));
    solution
        .iter()
        .all(|value| value.is_finite())
        .then(|| DVector::from_iterator(rhs.len(), solution.iter().copied()))
}

fn assert_same_bits(actual: &DVector<f64>, expected: &DVector<f64>) {
    assert_eq!(actual.len(), expected.len());
    for (actual, expected) in actual.iter().zip(expected.iter()) {
        assert_eq!(actual.to_bits(), expected.to_bits());
    }
}

#[test]
fn retained_sparse_storage_matches_fresh_factors_with_changing_coefficients_and_rhs() {
    let pattern = pattern(24, false);
    let mut cache = SparseNewtonCache::default();
    let mut workspace_address = None;
    for iteration in 0..40 {
        let matrix = matrix(&pattern, if iteration % 4 < 2 { 4.0 } else { 0.125 });
        let rhs = DVector::from_fn(24, |row, _| (row + iteration) as f64 - 3.25);
        let row_scales = vec![if iteration % 7 == 0 { 2.0 } else { 1.0 }; 24];
        let variable_scales = vec![if iteration % 9 == 0 { 0.25 } else { 1.0 }; 24];
        let actual = cache
            .solve_scaled(&matrix, &rhs, &row_scales, &variable_scales, &pattern)
            .unwrap();
        let expected = fresh_solution(&matrix, &rhs, &row_scales, &variable_scales, &pattern);
        assert_same_bits(&actual, &expected);
        let address = cache.system.as_ref().unwrap().workspace.storage.as_ptr() as usize;
        assert_eq!(*workspace_address.get_or_insert(address), address);
    }
}

#[test]
fn singular_refactor_never_reuses_successful_factors_and_recovers() {
    let pattern = pattern(24, false);
    let matrix = matrix(&pattern, 4.0);
    let mut cache = SparseNewtonCache::default();
    let rhs = DVector::from_element(24, 1.0);
    let scales = vec![1.0; 24];
    let expected = fresh_solution(&matrix, &rhs, &scales, &scales, &pattern);
    assert_same_bits(
        &cache
            .solve_scaled(&matrix, &rhs, &scales, &scales, &pattern)
            .unwrap(),
        &expected,
    );
    let zero = DMatrix::zeros(24, 24);
    for _ in 0..2 {
        assert!(
            cache
                .solve_scaled(&zero, &rhs, &scales, &scales, &pattern)
                .is_none()
        );
    }
    let mut rejected_clone = cache.clone();
    assert!(
        rejected_clone
            .solve_scaled(&zero, &rhs, &scales, &scales, &pattern)
            .is_none()
    );
    for recovered in [&mut cache, &mut rejected_clone] {
        assert_same_bits(
            &recovered
                .solve_scaled(&matrix, &rhs, &scales, &scales, &pattern)
                .unwrap(),
            &expected,
        );
    }
}

#[test]
fn changed_pattern_and_dimension_reconstruct_the_sparse_owner() {
    let mut cache = SparseNewtonCache::default();
    for pattern in [pattern(24, false), pattern(24, true), pattern(19, true)] {
        let matrix = matrix(&pattern, 3.0);
        let dimension = matrix.nrows();
        let rhs = DVector::from_fn(dimension, |row, _| row as f64 - 2.75);
        let scales = vec![1.0; dimension];
        let actual = cache
            .solve_scaled(&matrix, &rhs, &scales, &scales, &pattern)
            .unwrap();
        let expected = fresh_solution(&matrix, &rhs, &scales, &scales, &pattern);
        assert_same_bits(&actual, &expected);
    }
}

#[test]
fn nonfinite_rhs_or_matrix_does_not_poison_later_finite_solves() {
    let pattern = pattern(24, false);
    let matrix = matrix(&pattern, 4.0);
    let rhs = DVector::from_element(24, 1.0);
    let scales = vec![1.0; 24];
    let expected = fresh_solution(&matrix, &rhs, &scales, &scales, &pattern);
    let mut cache = SparseNewtonCache::default();
    for invalid in [f64::NAN, f64::INFINITY] {
        let mut bad_rhs = rhs.clone();
        bad_rhs[4] = invalid;
        assert!(
            cache
                .solve_scaled(&matrix, &bad_rhs, &scales, &scales, &pattern)
                .is_none()
        );
        assert_same_bits(
            &cache
                .solve_scaled(&matrix, &rhs, &scales, &scales, &pattern)
                .unwrap(),
            &expected,
        );
        let mut bad_matrix = matrix.clone();
        bad_matrix[(4, 4)] = invalid;
        let actual = cache.solve_scaled(&bad_matrix, &rhs, &scales, &scales, &pattern);
        let fresh = try_fresh_solution(&bad_matrix, &rhs, &scales, &scales, &pattern);
        assert_eq!(actual.is_some(), fresh.is_some());
        if let (Some(actual), Some(fresh)) = (actual, fresh) {
            assert_same_bits(&actual, &fresh);
        }
        assert_same_bits(
            &cache
                .solve_scaled(&matrix, &rhs, &scales, &scales, &pattern)
                .unwrap(),
            &expected,
        );
    }
}

#[test]
fn cloned_successful_cache_owns_independent_numeric_storage() {
    let pattern = pattern(24, false);
    let original = matrix(&pattern, 4.0);
    let changed = matrix(&pattern, 0.125);
    let rhs = DVector::from_element(24, 1.0);
    let scales = vec![1.0; 24];
    let mut cache = SparseNewtonCache::default();
    let expected = cache
        .solve_scaled(&original, &rhs, &scales, &scales, &pattern)
        .unwrap();
    let mut cloned = cache.clone();
    assert_ne!(
        cache.system.as_ref().unwrap().workspace.storage.as_ptr(),
        cloned.system.as_ref().unwrap().workspace.storage.as_ptr(),
    );
    let actual = cloned
        .solve_scaled(&changed, &rhs, &scales, &scales, &pattern)
        .unwrap();
    assert_same_bits(
        &actual,
        &fresh_solution(&changed, &rhs, &scales, &scales, &pattern),
    );
    assert_same_bits(
        &cache
            .solve_scaled(&original, &rhs, &scales, &scales, &pattern)
            .unwrap(),
        &expected,
    );
}

/// Deterministic pseudo-random entries in `[-1, 1)`.
fn pseudo_random(seed: &mut u64) -> f64 {
    *seed = seed
        .wrapping_mul(6_364_136_223_846_793_005)
        .wrapping_add(1_442_695_040_888_963_407);
    ((*seed >> 11) as f64 / (1u64 << 53) as f64) * 2.0 - 1.0
}

fn dense_system(dimension: usize, seed: &mut u64) -> (DMatrix<f64>, Vec<f64>, Vec<f64>) {
    let jacobian = DMatrix::from_fn(dimension, dimension, |row, column| {
        pseudo_random(seed) + if row == column { 4.0 } else { 0.0 }
    });
    let row_scales = (0..dimension)
        .map(|_| 1.0 + pseudo_random(seed).abs())
        .collect();
    let variable_scales = (0..dimension)
        .map(|_| 0.5 + pseudo_random(seed).abs())
        .collect();
    (jacobian, row_scales, variable_scales)
}

fn dense_delta(
    jacobian: &DMatrix<f64>,
    residual: &[f64],
    scales: (&[f64], &[f64]),
    cache: Option<&mut SparseNewtonCache>,
) -> Option<DVector<f64>> {
    let system = crate::runtime::projection::ScaledNewtonSystem {
        jacobian,
        residual,
        row_scales: scales.0,
        variable_scales: scales.1,
        structure: None,
        tolerance: 1.0e-10,
    };
    match cache {
        Some(cache) => crate::runtime::projection::scaled_newton_delta_with_cache(system, cache),
        None => crate::runtime::projection::scaled_newton_delta(system),
    }
}

fn assert_same_option_bits(actual: Option<DVector<f64>>, expected: Option<DVector<f64>>) {
    match (actual, expected) {
        (Some(actual), Some(expected)) => assert_same_bits(&actual, &expected),
        (None, None) => {}
        (actual, expected) => panic!("cached {actual:?} but fresh {expected:?}"),
    }
}

#[test]
fn dense_factor_reuse_is_bit_identical_across_repeated_right_hand_sides() {
    let mut seed = 0x5eed_u64;
    let (jacobian, row_scales, variable_scales) = dense_system(16, &mut seed);
    let scales = (row_scales.as_slice(), variable_scales.as_slice());
    let mut cache = SparseNewtonCache::default();
    // The affine solve factors once and then refines against several
    // residuals with the same matrix; every solve must equal a fresh one.
    for _ in 0..4 {
        let residual = (0..16)
            .map(|_| pseudo_random(&mut seed))
            .collect::<Vec<_>>();
        assert_same_option_bits(
            dense_delta(&jacobian, &residual, scales, Some(&mut cache)),
            dense_delta(&jacobian, &residual, scales, None),
        );
    }
    // A changed coefficient or scale refactors instead of reusing.
    let mut changed = jacobian.clone();
    changed[(3, 5)] += 0.25;
    let residual = vec![1.0; 16];
    assert_same_option_bits(
        dense_delta(&changed, &residual, scales, Some(&mut cache)),
        dense_delta(&changed, &residual, scales, None),
    );
    let mut rescaled = row_scales.clone();
    rescaled[7] *= 2.0;
    let scales = (rescaled.as_slice(), variable_scales.as_slice());
    assert_same_option_bits(
        dense_delta(&changed, &residual, scales, Some(&mut cache)),
        dense_delta(&changed, &residual, scales, None),
    );
}

#[test]
fn dense_factor_guards_match_fresh_rectangular_singular_and_nonfinite_systems() {
    let mut cache = SparseNewtonCache::default();
    let rectangular = DMatrix::from_element(4, 3, 1.0);
    assert_same_option_bits(
        dense_delta(
            &rectangular,
            &[1.0; 4],
            (&[1.0; 4], &[1.0; 3]),
            Some(&mut cache),
        ),
        dense_delta(&rectangular, &[1.0; 4], (&[1.0; 4], &[1.0; 3]), None),
    );
    let zero = DMatrix::zeros(4, 4);
    for _ in 0..2 {
        assert_same_option_bits(
            dense_delta(
                &zero,
                &[1.0, 0.0, -1.0, 2.0],
                (&[1.0; 4], &[1.0; 4]),
                Some(&mut cache),
            ),
            dense_delta(&zero, &[1.0, 0.0, -1.0, 2.0], (&[1.0; 4], &[1.0; 4]), None),
        );
    }
    let mut nonfinite = DMatrix::identity(4, 4);
    nonfinite[(1, 2)] = f64::NAN;
    assert_same_option_bits(
        dense_delta(
            &nonfinite,
            &[1.0; 4],
            (&[1.0; 4], &[1.0; 4]),
            Some(&mut cache),
        ),
        dense_delta(&nonfinite, &[1.0; 4], (&[1.0; 4], &[1.0; 4]), None),
    );
    let identity = DMatrix::identity(4, 4);
    assert_same_option_bits(
        dense_delta(
            &identity,
            &[1.0; 4],
            (&[1.0; 4], &[1.0; 4]),
            Some(&mut cache),
        ),
        dense_delta(&identity, &[1.0; 4], (&[1.0; 4], &[1.0; 4]), None),
    );
}
