use super::*;
use crate::runtime::fallbacks::{ProjectionSite, projection_fallbacks, reset_projection_fallbacks};

const SITE: usize = 7;

/// Build the cycle's sensitivity linearization and solve one right-hand
/// side, returning the solution, its dense reference, and the site's
/// `seed_dense` count and call count.
fn seed_solve(zero_pivots: &[usize]) -> (DVector<f64>, DVector<f64>, u64, u64) {
    let expected = DVector::from_fn(DIMENSION, |row, _| 1.0 + row as f64 / 8.0);
    let model = CyclicAffine::new(zero_pivots, &expected);
    reset_projection_fallbacks();
    let linearization = SeedBlockLinearization::build(
        &model,
        (0, Some(SITE)),
        &model.plan.blocks.scalar_blocks()[0],
        expected.as_slice(),
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 0,
            tolerance: 1e-10,
        },
    )
    .unwrap();
    let rhs = DVector::from_fn(DIMENSION, |row, _| (row as f64 * 0.37).sin() + 0.5);
    let solution = linearization.solve(&rhs).expect("the cycle is invertible");
    let reference = model.matrix.clone().lu().solve(&rhs).unwrap();
    let counts = projection_fallbacks()
        .sites
        .get(&ProjectionSite::Block(SITE))
        .copied()
        .unwrap_or_default();
    (
        solution,
        reference,
        counts.count(ProjectionFallback::SeedDense),
        counts.calls,
    )
}

fn assert_close(solution: &DVector<f64>, reference: &DVector<f64>) {
    for (actual, expected) in solution.iter().zip(reference.iter()) {
        assert!(
            (actual - expected).abs() <= 1e-12 * expected.abs().max(1.0),
            "{actual} differs from the dense solution {expected}"
        );
    }
}

/// A sparse-candidate block's sensitivity solve goes through its admitted
/// torn layout: the dense solution, one counted call, no fallback.
#[test]
fn a_sparse_block_sensitivity_solves_through_its_torn_layout() {
    let (solution, reference, fallbacks, calls) = seed_solve(&[]);
    assert_close(&solution, &reference);
    assert_eq!((fallbacks, calls), (0, 1));
}

/// A torn sensitivity solve that exhausts its promotion capacity declines to
/// the block's dense factorization and is counted as `seed_dense`.
#[test]
fn a_declined_torn_sensitivity_solve_is_a_counted_dense_fallback() {
    let (solution, reference, fallbacks, calls) =
        seed_solve(&(0..DIMENSION - 1).collect::<Vec<_>>());
    assert_close(&solution, &reference);
    assert_eq!((fallbacks, calls), (1, 1));
}
