//! In-place tear promotion of the torn affine elimination.
//!
//! The cyclic fixture solves its coordinates in reverse causal order from one
//! issued tear (`y[23]`). Row `r < 23` reads `y[r]` with its diagonal pivot and
//! `y[r + 1]` with `-1`; a vanished diagonal leaves `-y[r + 1]` alone, so the
//! full system stays regular while the causal pivot is unusable.

use super::*;
use rumoca_eval_solve::projection_policy::torn_promotion_capacity;

fn expected() -> DVector<f64> {
    DVector::from_fn(DIMENSION, |row, _| 1.0 + row as f64 / 8.0)
}

fn row_scales() -> Vec<f64> {
    (0..DIMENSION).map(|i| 1.0 + (i % 5) as f64).collect()
}

fn variable_scales() -> Vec<f64> {
    (0..DIMENSION).map(|i| 1.0 + (i % 3) as f64 / 2.0).collect()
}

/// The torn and the full-system scaled Newton deltas of `matrix` from the
/// origin toward `expected`, with the reduced size the torn solve used.
fn torn_delta(
    model: &CyclicAffine,
    matrix: &DMatrix<f64>,
) -> (Option<DVector<f64>>, Option<usize>) {
    torn_delta_from(model, matrix, &BlockJacobian::dense(matrix.clone()))
}

/// [`torn_delta`] with `jacobian` holding `matrix` in a storage of its own.
fn torn_delta_from(
    model: &CyclicAffine,
    matrix: &DMatrix<f64>,
    jacobian: &BlockJacobian,
) -> (Option<DVector<f64>>, Option<usize>) {
    let layout = model.structures.algebraic_projection()[0]
        .affine_elimination()
        .unwrap();
    let residual = -(matrix * expected());
    let delta = scaled_newton_delta_with_tearing(
        ScaledNewtonSystem {
            revision: None,
            jacobian,
            residual: residual.as_slice(),
            row_scales: &row_scales(),
            variable_scales: &variable_scales(),
            structure: Some(layout.pattern()),
            tolerance: 1e-10,
        },
        &mut model.cache.borrow_mut(),
        layout,
    );
    (delta, model.cache.borrow().torn_reduced_size())
}

fn full_delta(model: &CyclicAffine, matrix: &DMatrix<f64>) -> DVector<f64> {
    let layout = model.structures.algebraic_projection()[0]
        .affine_elimination()
        .unwrap();
    let residual = -(matrix * expected());
    scaled_newton_delta(ScaledNewtonSystem {
        revision: None,
        jacobian: &BlockJacobian::dense(matrix.clone()),
        residual: residual.as_slice(),
        row_scales: &row_scales(),
        variable_scales: &variable_scales(),
        structure: Some(layout.pattern()),
        tolerance: 1e-10,
    })
    .expect("the full system is regular")
}

/// Per-coordinate differential bound against the full-system fallback.
fn assert_matches_fallback(label: &str, torn: &DVector<f64>, full: &DVector<f64>) {
    let scales = variable_scales();
    for (index, (a, b)) in torn.iter().zip(full.iter()).enumerate() {
        let bound = 1e-9 * a.abs().max(scales[index]);
        assert!(
            (a - b).abs() <= bound,
            "{label} y[{index}]: torn {a:e} vs full {b:e}"
        );
    }
}

fn cycle_with_diagonal(pivots: &[(usize, f64)]) -> (CyclicAffine, DMatrix<f64>) {
    let model = CyclicAffine::new(&[], &expected());
    let mut matrix = model.matrix.clone();
    for &(row, value) in pivots {
        matrix[(row, row)] = value;
    }
    (model, matrix)
}

#[test]
fn an_exactly_zero_pivot_with_a_regular_alternative_promotes_one_tear() {
    let (model, matrix) = cycle_with_diagonal(&[(7, 0.0)]);
    let (torn, size) = torn_delta(&model, &matrix);
    let torn = torn.expect("the promoted reduction solves the regular block");
    assert_eq!(size, Some(2));
    assert_matches_fallback("zero pivot", &torn, &full_delta(&model, &matrix));
}

#[test]
fn a_roundoff_pivot_promotes_like_an_exact_zero() {
    for pivot in [1e-19, 5.8e-13, -1e-9] {
        let (model, matrix) = cycle_with_diagonal(&[(7, pivot)]);
        let (torn, size) = torn_delta(&model, &matrix);
        let Some(torn) = torn else {
            panic!("pivot {pivot:e} must promote");
        };
        assert_eq!(size, Some(2), "{pivot:e}");
        assert_matches_fallback(
            &format!("pivot {pivot:e}"),
            &torn,
            &full_delta(&model, &matrix),
        );
    }
}

#[test]
fn a_hidden_second_pivot_promotes_in_the_same_elimination() {
    // Reverse causal order meets row 15 before row 7.
    let (model, matrix) = cycle_with_diagonal(&[(15, 0.0), (7, 1e-14)]);
    let (torn, size) = torn_delta(&model, &matrix);
    let torn = torn.expect("both weak pivots promote");
    assert_eq!(size, Some(3));
    assert_matches_fallback("hidden pivot", &torn, &full_delta(&model, &matrix));
}

#[test]
fn promotion_declines_at_the_policy_capacity() {
    let capacity = torn_promotion_capacity(1).unwrap();
    let rows = [3, 7, 11, 15, 19, 21];
    assert!(rows.len() > capacity - 1);
    let (model, at_capacity) = cycle_with_diagonal(
        &rows[..capacity - 1]
            .iter()
            .map(|&r| (r, 0.0))
            .collect::<Vec<_>>(),
    );
    let (torn, size) = torn_delta(&model, &at_capacity);
    assert_eq!(size, Some(capacity));
    assert_matches_fallback(
        "at capacity",
        &torn.expect("promotion up to the capacity solves"),
        &full_delta(&model, &at_capacity),
    );
    let (model, beyond) = cycle_with_diagonal(
        &rows[..capacity]
            .iter()
            .map(|&r| (r, 0.0))
            .collect::<Vec<_>>(),
    );
    assert_eq!(torn_delta(&model, &beyond), (None, None));
}

#[test]
fn a_singular_nonfinite_or_rectangular_system_still_declines() {
    // Rank deficiency survives promotion: the residual row vanishes.
    for promoted in [false, true] {
        let (model, mut matrix) = cycle_with_diagonal(&[(7, if promoted { 0.0 } else { 1.0 })]);
        matrix[(DIMENSION - 1, 0)] = 0.0;
        matrix[(DIMENSION - 1, DIMENSION - 1)] = 0.0;
        assert_eq!(torn_delta(&model, &matrix), (None, None), "{promoted}");
    }
    // A nonfinite coefficient in a causal row declines instead of promoting.
    for value in [f64::NAN, f64::INFINITY] {
        let (model, mut matrix) = cycle_with_diagonal(&[(7, 0.0)]);
        matrix[(12, 13)] = value;
        assert_eq!(torn_delta(&model, &matrix).0, None, "{value}");
    }
    // A Jacobian whose shape differs from the layout's never reaches it.
    let (model, matrix) = cycle_with_diagonal(&[]);
    let layout = model.structures.algebraic_projection()[0]
        .affine_elimination()
        .unwrap();
    let rectangular = matrix.columns(0, DIMENSION - 1).into_owned();
    let residual = vec![0.0; DIMENSION];
    let delta = scaled_newton_delta_with_tearing(
        ScaledNewtonSystem {
            revision: None,
            jacobian: &BlockJacobian::dense(rectangular.clone()),
            residual: &residual,
            row_scales: &[1.0; DIMENSION],
            variable_scales: &[1.0; DIMENSION - 1],
            structure: Some(layout.pattern()),
            tolerance: 1e-10,
        },
        &mut model.cache.borrow_mut(),
        layout,
    );
    assert!(delta.is_none());
}

#[test]
fn guard_promotions_at_the_capacity_decline() {
    let expected = expected();
    let original = CyclicAffine::new(&[], &expected);
    let mut dependencies: Vec<_> = (0..DIMENSION)
        .map(|row| vec![row, (row + 1) % DIMENSION])
        .collect();
    // Six rows early in the reverse order read coordinates solved later.
    let guarded = [(22, 0), (21, 2), (20, 4), (19, 6), (18, 8), (17, 10)];
    for &(row, column) in &guarded {
        dependencies[row].insert(0, column);
    }
    let model = CyclicAffine::from_system(
        original.matrix.clone(),
        original.rhs,
        original.plan,
        dependencies,
    );
    let layout = model.structures.algebraic_projection()[0]
        .affine_elimination()
        .unwrap();
    assert_eq!(layout.zero_guards().len(), guarded.len());
    let capacity = torn_promotion_capacity(1).unwrap();
    for hits in [0, capacity - 1, capacity] {
        let mut matrix = original.matrix.clone();
        for &(row, column) in &guarded[..hits] {
            matrix[(row, column)] = 0.25;
        }
        let (torn, size) = torn_delta(&model, &matrix);
        if hits < capacity {
            assert_eq!(size, Some(1 + hits));
            assert_matches_fallback(
                &format!("{hits} guards"),
                &torn.expect("guard promotions within the capacity solve"),
                &full_delta(&model, &matrix),
            );
        } else {
            assert_eq!((torn, size), (None, None));
        }
    }
}

#[test]
fn promotions_are_rederived_on_every_factorization() {
    let (model, promoted) = cycle_with_diagonal(&[(7, 0.0)]);
    let (_, regular) = cycle_with_diagonal(&[]);
    let first = torn_delta(&model, &promoted);
    assert_eq!(first.1, Some(2));
    assert_eq!(torn_delta(&model, &regular).1, Some(1));
    let again = torn_delta(&model, &promoted);
    let (fresh_model, _) = cycle_with_diagonal(&[]);
    let fresh = torn_delta(&fresh_model, &promoted);
    let bits = |delta: &Option<DVector<f64>>| {
        delta
            .as_ref()
            .map(|delta| delta.iter().map(|x| x.to_bits()).collect::<Vec<_>>())
    };
    assert_eq!(bits(&first.0), bits(&again.0));
    assert_eq!(bits(&first.0), bits(&fresh.0));
    assert_eq!(again.1, Some(2));
}

/// FNV-1a over the bit patterns of a delta.
fn fingerprint(delta: &DVector<f64>) -> u64 {
    delta.iter().fold(0xcbf2_9ce4_8422_2325, |hash, value| {
        value
            .to_bits()
            .to_le_bytes()
            .iter()
            .fold(hash, |hash, &byte| {
                (hash ^ u64::from(byte)).wrapping_mul(0x0100_0000_01b3)
            })
    })
}

/// A reduction that never promotes keeps the issued arithmetic bit for bit:
/// only the recovery stride changed. The pins were produced by the reduction
/// without promotion support.
#[test]
fn a_reduction_without_promotion_keeps_its_bits() {
    let (model, matrix) = cycle_with_diagonal(&[]);
    let (delta, size) = torn_delta(&model, &matrix);
    assert_eq!(size, Some(1));
    assert_eq!(fingerprint(&delta.unwrap()), UNPROMOTED_CYCLE_PIN);
    let (model, matrix) = cycle_with_diagonal(&[(4, 3.5), (9, -0.75), (16, 1e-3)]);
    let (delta, size) = torn_delta(&model, &matrix);
    assert_eq!(size, Some(1));
    assert_eq!(fingerprint(&delta.unwrap()), UNPROMOTED_SCALED_CYCLE_PIN);
}

const UNPROMOTED_CYCLE_PIN: u64 = 10_087_628_548_452_685_031;
const UNPROMOTED_SCALED_CYCLE_PIN: u64 = 12_940_558_722_059_867_671;

/// Conditioning only the entries whose source value or scales changed gives
/// the bits a fresh cache gives, across a sequence of one-entry changes and a
/// repeated matrix.
#[test]
fn incremental_conditioning_matches_a_fresh_cache() {
    let (model, _) = cycle_with_diagonal(&[]);
    let steps: [&[(usize, f64)]; 5] = [
        &[(4, 3.5)],
        &[(4, 3.5), (9, -0.75)],
        &[(4, 3.5), (9, -0.75)],
        &[(9, -0.75), (7, 0.0)],
        &[],
    ];
    for pivots in steps {
        let (fresh_model, matrix) = cycle_with_diagonal(pivots);
        let reused = torn_delta(&model, &matrix);
        let fresh = torn_delta(&fresh_model, &matrix);
        assert_eq!(reused.1, fresh.1);
        assert_eq!(
            reused.0.as_ref().map(fingerprint),
            fresh.0.as_ref().map(fingerprint),
            "pivots {pivots:?}"
        );
    }
}

/// A source stored in the block pattern's compact layout is read by slot and
/// gives the bits a dense source gives.
#[test]
fn compact_and_dense_sources_condition_the_same_bits() {
    let steps: [&[(usize, f64)]; 3] = [&[], &[(7, 0.0)], &[(4, 3.5), (9, -0.75)]];
    let (compact_model, _) = cycle_with_diagonal(&[]);
    for pivots in steps {
        let (dense_model, matrix) = cycle_with_diagonal(pivots);
        let structure = &compact_model.structures.algebraic_projection()[0];
        let mut compact = BlockJacobian::compact(structure.compact_layout());
        for row in 0..DIMENSION {
            structure.pattern().visit_row_columns(row, &mut |column| {
                compact[(row, column)] = matrix[(row, column)]
            });
        }
        let dense = torn_delta(&dense_model, &matrix);
        let read = torn_delta_from(&compact_model, &matrix, &compact);
        assert_eq!(read.1, dense.1);
        assert_eq!(
            read.0.as_ref().map(fingerprint),
            dense.0.as_ref().map(fingerprint),
            "pivots {pivots:?}"
        );
    }
}

/// The torn delta toward `expected` through `residual_matrix`, factored from
/// `jacobian_matrix` under `revision`.
fn delta_under(
    model: &CyclicAffine,
    (jacobian_matrix, residual_matrix): (&DMatrix<f64>, &DMatrix<f64>),
    revision: Option<u64>,
) -> u64 {
    let layout = model.structures.algebraic_projection()[0]
        .affine_elimination()
        .unwrap();
    let residual = -(residual_matrix * expected());
    let delta = scaled_newton_delta_with_tearing(
        ScaledNewtonSystem {
            revision,
            jacobian: &BlockJacobian::dense(jacobian_matrix.clone()),
            residual: residual.as_slice(),
            row_scales: &row_scales(),
            variable_scales: &variable_scales(),
            structure: Some(layout.pattern()),
            tolerance: 1e-10,
        },
        &mut model.cache.borrow_mut(),
        layout,
    );
    fingerprint(&delta.expect("the torn system is regular"))
}

/// A revision names the Jacobian and scales: a solve under the revision the
/// cache last conditioned reuses that conditioning and factor without reading
/// the source again, and any other revision conditions the source afresh.
#[test]
fn a_held_revision_reuses_its_factor_and_a_new_one_reconditions() {
    let (model, original) = cycle_with_diagonal(&[]);
    let (fresh_model, changed) = cycle_with_diagonal(&[(4, 3.5)]);
    let held = Some(u64::MAX - 1);
    let factored = delta_under(&model, (&original, &changed), held);
    assert_eq!(delta_under(&model, (&changed, &changed), held), factored);
    let renamed = delta_under(&model, (&changed, &changed), Some(u64::MAX));
    assert_eq!(
        renamed,
        delta_under(&fresh_model, (&changed, &changed), None)
    );
    assert_ne!(renamed, factored);
}
