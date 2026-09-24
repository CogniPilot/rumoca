use super::super::super::jacobian_values::read_preparation_count;
use super::super::super::seed_linearization::{seed_dense_factorization_count, seed_torn_attempts};
use super::*;

#[test]
fn seed_factor_uses_issued_pair_and_reuses_dense_fallback_when_busy() {
    let model = paired_model(4, &[2]);
    let block = &model.plan.blocks[0];
    let before_factor = seed_dense_factorization_count();
    let linearization = SeedBlockLinearization::build(
        &model,
        0,
        block,
        &[0.0; 4],
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 0,
            tolerance: 1e-10,
        },
    )
    .unwrap();
    let first = DVector::from_vec(vec![1.0, 2.0, 3.0, 4.0]);
    let second = DVector::from_vec(vec![-2.0, 1.0, 0.5, 3.0]);
    let first_rhs = &model.matrix * &first;
    let second_rhs = &model.matrix * &second;
    let before_attempts = seed_torn_attempts().len();
    let before_reads = read_preparation_count();
    for (rhs, expected) in [(&first_rhs, &first), (&second_rhs, &second)] {
        let actual = linearization.solve(rhs).unwrap();
        assert!((&actual - expected).norm() < 1e-10);
    }
    assert_eq!(seed_dense_factorization_count(), before_factor);
    assert_eq!(read_preparation_count(), before_reads + 2);
    assert_eq!(
        &seed_torn_attempts()[before_attempts..],
        &[
            solve::TearingCandidate::Guarded,
            solve::TearingCandidate::Primary,
            solve::TearingCandidate::Guarded,
            solve::TearingCandidate::Primary,
        ]
    );
    for (rhs, expected) in [(&first_rhs, &first), (&second_rhs, &second)] {
        let actual = linearization
            .with_busy_torn_cache(|| linearization.solve(rhs))
            .unwrap();
        assert!((&actual - expected).norm() < 1e-10);
    }
    assert_eq!(seed_dense_factorization_count(), before_factor + 1);
    let adjacent = f64::from_bits(1.0_f64.to_bits() + 1);
    let fresh = SeedBlockLinearization::build(
        &model,
        0,
        block,
        &[adjacent, 0.0, 0.0, 0.0],
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 0,
            tolerance: 1e-10,
        },
    )
    .unwrap();
    assert!((&fresh.solve(&first_rhs).unwrap() - &first).norm() < 1e-10);
    assert_eq!(read_preparation_count(), before_reads + 4);
    assert_eq!(seed_dense_factorization_count(), before_factor + 1);
}

#[test]
fn seed_factor_captures_only_issued_layouts_after_borrowed_assembly() {
    let mut no_candidate = paired_model(4, &[2]);
    no_candidate.plan.blocks[0].guarded_tearing = None;
    no_candidate.plan.blocks[0].tearing = None;
    let before_attempts = seed_torn_attempts().len();
    let before_factor = seed_dense_factorization_count();
    let args = AlgebraicProjectionArgs {
        parameters: &[],
        time: 0.0,
        state_count: 0,
        tolerance: 1e-10,
    };
    let no_torn = SeedBlockLinearization::build(
        &no_candidate,
        0,
        &no_candidate.plan.blocks[0],
        &[0.0; 4],
        args,
    )
    .unwrap();
    assert_eq!(seed_dense_factorization_count(), before_factor);
    let expected = DVector::from_vec(vec![1.0, 2.0, 3.0, 4.0]);
    let rhs = &no_candidate.matrix * &expected;
    assert!((&no_torn.solve(&rhs).unwrap() - &expected).norm() < 1e-10);
    assert_eq!(seed_torn_attempts().len(), before_attempts);
    assert_eq!(seed_dense_factorization_count(), before_factor + 1);

    let issued = paired_model(4, &[2]);
    let rhs = &issued.matrix * &expected;
    let linearization =
        SeedBlockLinearization::build(&issued, 0, &issued.plan.blocks[0], &[0.0; 4], args).unwrap();
    drop(issued);
    let before_attempts = seed_torn_attempts().len();
    let before_factor = seed_dense_factorization_count();
    assert!((&linearization.solve(&rhs).unwrap() - &expected).norm() < 1e-10);
    assert_eq!(
        &seed_torn_attempts()[before_attempts..],
        &[
            solve::TearingCandidate::Guarded,
            solve::TearingCandidate::Primary
        ]
    );
    assert_eq!(seed_dense_factorization_count(), before_factor);
}

#[test]
fn compact_affine_projection_keeps_only_stored_values_through_torn_solve() {
    let expected = DVector::from_element(DIMENSION, 2.0);
    let mut model = CyclicAffine::new(&[], &expected);
    model.prepared = true;
    let structure = &model.structures.algebraic_projection()[0];
    let application = structure.jacobian_application().unwrap();
    assert!(application.output_len() < DIMENSION * DIMENSION);
    model.storage = Some(std::cell::RefCell::new(JacobianStorage::new(
        structure, DIMENSION, DIMENSION,
    )));
    let before = jacobian_allocation_count();
    let scaled_before = scaling::scaled_dense_allocation_count();
    let mut y = vec![-10.0; DIMENSION];
    let update =
        affine::project_affine_block(&model, &mut y, &[], 0.0, &model.plan.blocks[0], 0, 1e-10)
            .unwrap();
    assert!(update.settled);
    assert!(
        y.iter()
            .zip(expected.iter())
            .all(|(a, b)| (a - b).abs() < 1e-10)
    );
    let prepared = jacobian_values::read_preparation_count();
    let mut second = vec![100.0; DIMENSION];
    assert!(
        affine::project_affine_block(
            &model,
            &mut second,
            &[],
            0.0,
            &model.plan.blocks[0],
            0,
            1e-10
        )
        .unwrap()
        .settled
    );
    assert!(
        second
            .iter()
            .zip(expected.iter())
            .all(|(a, b)| (a - b).abs() < 1e-10)
    );
    assert_eq!(jacobian_values::read_preparation_count(), prepared);
    let storage = model.storage.as_ref().unwrap().borrow();
    assert!(
        matches!(&*storage, JacobianStorage::Pattern { values, .. } if values.len() == application.output_len())
    );
    assert_eq!(jacobian_allocation_count(), before);
    assert_eq!(scaling::scaled_dense_allocation_count(), scaled_before);
}

#[test]
fn guarded_decline_retains_primary_compact_reads_across_projections() {
    let mut model = paired_model(DIMENSION, &[DIMENSION - 2]);
    model.prepared = true;
    let structure = &model.structures.algebraic_projection()[0];
    let original_identity = structure.jacobian_application().unwrap().identity().clone();
    model.storage = Some(std::cell::RefCell::new(JacobianStorage::new(
        structure, DIMENSION, DIMENSION,
    )));
    let before = jacobian_values::read_preparation_count();
    for attempt in 0..2 {
        let mut y = vec![-10.0 + attempt as f64; DIMENSION];
        assert!(
            project_affine_once(&model, &mut y, attempt as f64)
                .unwrap()
                .settled
        );
        assert!(y.iter().all(|value| (*value - 2.0).abs() < 1e-10));
        assert_eq!(
            jacobian_values::read_preparation_count() - before,
            2,
            "the issued guarded and primary readers should each prepare once"
        );
    }
    model.matrix *= 2.0;
    model.rhs *= 2.0;
    let mut y = vec![13.0; DIMENSION];
    assert!(project_affine_once(&model, &mut y, 2.0).unwrap().settled);
    assert!(y.iter().all(|value| (*value - 2.0).abs() < 1e-10));
    assert_eq!(jacobian_values::read_preparation_count() - before, 2);

    let replacement = paired_model(DIMENSION, &[DIMENSION - 2]);
    model.structures = replacement.structures;
    let structure = &model.structures.algebraic_projection()[0];
    assert!(!std::sync::Arc::ptr_eq(
        &original_identity,
        structure.jacobian_application().unwrap().identity()
    ));
    model.storage = Some(std::cell::RefCell::new(JacobianStorage::new(
        structure, DIMENSION, DIMENSION,
    )));
    let mut y = vec![14.0; DIMENSION];
    assert!(project_affine_once(&model, &mut y, 3.0).unwrap().settled);
    assert!(y.iter().all(|value| (*value - 2.0).abs() < 1e-10));
    assert_eq!(
        jacobian_values::read_preparation_count() - before,
        4,
        "a new application owner must rebind both candidate readers"
    );
    assert_eq!(model.full_solves.get(), 0);
    let calls = model.candidate_calls.borrow();
    assert_eq!(calls[0].0, solve::TearingCandidate::Guarded);
    assert_eq!(calls[1].0, solve::TearingCandidate::Primary);
}

#[test]
fn owned_jacobian_rejects_malformed_structure_before_allocating() {
    let expected = DVector::from_element(DIMENSION, 2.0);
    let model = CyclicAffine::new(&[], &expected);
    let block = &model.plan.blocks[0];
    let before = jacobian_allocation_count();
    assert!(
        algebraic_block_jacobian(
            &model,
            expected.as_slice(),
            &[],
            0.0,
            &block.rows[..DIMENSION - 1],
            &block.y_indices,
            model.algebraic_projection_block_structure(0),
        )
        .is_err()
    );
    assert_eq!(jacobian_allocation_count(), before);
}

#[test]
fn a_large_reduced_system_retains_the_existing_sparse_policy() {
    let expected = DVector::from_element(DIMENSION, 2.0);
    let original = CyclicAffine::new(&[], &expected);
    let mut plan = original.plan;
    plan.blocks[0].tearing = Some(solve::BlockTearing {
        tear_y_indices: (0..17).collect(),
        residual_rows: (0..17).collect(),
        causal_steps: (17..DIMENSION)
            .rev()
            .map(|row| solve::CausalStep { row, y_index: row })
            .collect(),
    });
    let dependencies = (0..DIMENSION)
        .map(|row| vec![row, (row + 1) % DIMENSION])
        .collect();
    let model = CyclicAffine::from_system(original.matrix, original.rhs, plan, dependencies);
    let y = project(&model, 1e-10);
    assert!(model.full_solves.get() > 0);
    assert!(y.iter().all(|x| (x - 2.0).abs() < 1e-10));
}

#[test]
fn future_dependency_guards_recheck_raw_coefficients_before_cached_solve() {
    for dimension in [4, DIMENSION] {
        check_future_dependency_guards_recheck_raw_coefficients_before_cached_solve(dimension);
    }
}

fn check_future_dependency_guards_recheck_raw_coefficients_before_cached_solve(dimension: usize) {
    let expected = DVector::from_element(dimension, 2.0);
    let original = CyclicAffine::new(&[], &expected);
    let mut dependencies: Vec<_> = (0..dimension)
        .map(|row| vec![row, (row + 1) % dimension])
        .collect();
    dependencies[dimension - 2].insert(0, 0);
    let model = CyclicAffine::from_system(
        original.matrix.clone(),
        original.rhs,
        original.plan,
        dependencies,
    );
    let layout = model.structures.algebraic_projection()[0]
        .affine_elimination()
        .unwrap();
    assert_eq!(layout.zero_guards(), &[(dimension - 2, 0)]);
    let mut matrix = model.matrix.clone();
    let mut row_scales = vec![1.0; dimension];
    row_scales[dimension - 2] = 1e100;
    for coefficient in [0.0, 1e-310, 0.5, f64::NAN, 0.0] {
        matrix[(dimension - 2, 0)] = coefficient;
        let residual = -(&matrix * &expected);
        let delta = scaled_newton_delta_with_tearing(
            ScaledNewtonSystem {
                jacobian: &matrix,
                residual: residual.as_slice(),
                row_scales: &row_scales,
                variable_scales: &vec![1.0; dimension],
                structure: Some(layout.pattern()),
                tolerance: 1e-10,
            },
            &mut model.cache.borrow_mut(),
            solve::TearingCandidate::Primary,
            layout,
        );
        assert_eq!(delta.is_some(), coefficient == 0.0);
        if let Some(delta) = delta {
            assert!(delta.iter().all(|x| (x - 2.0).abs() < 1e-10));
        }
    }
}

pub(super) fn project(model: &CyclicAffine, tolerance: f64) -> Vec<f64> {
    let mut y = vec![0.0; model.matrix.ncols()];
    project_algebraics_with_plan_certified(
        model,
        &model.plan,
        &mut y,
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 0,
            tolerance,
        },
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
    .unwrap();
    y
}

#[test]
fn failed_reduced_refinement_restarts_the_original_full_system() {
    for dimension in [4, DIMENSION] {
        check_failed_reduced_refinement_restarts_the_original_full_system(dimension);
    }
}

fn check_failed_reduced_refinement_restarts_the_original_full_system(dimension: usize) {
    let expected = DVector::from_element(dimension, 2.0);
    let mut model = CyclicAffine::new(&[], &expected);
    model.invalid_torn_correction = true;
    let y = project(&model, 1e-10);
    assert!(model.full_solves.get() > 0);
    assert!(
        y.iter()
            .zip(expected.iter())
            .all(|(a, b)| (a - b).abs() < 1e-10)
    );
}

#[test]
fn affine_lease_survives_declined_fill_refinement_and_dense_retry() {
    let expected = DVector::from_element(DIMENSION, 2.0);
    let mut model = CyclicAffine::new(&[], &expected);
    model.invalid_torn_correction = true;
    model.storage = Some(std::cell::RefCell::new(JacobianStorage::Dense(
        DMatrix::from_element(DIMENSION, DIMENSION, f64::NAN),
    )));
    // Call the actual affine owner: outer plan certification also reads residuals
    // before/after this lease, independently of the affine certificate.
    let mut y = vec![-10.0; DIMENSION];
    let update =
        affine::project_affine_block(&model, &mut y, &[], 0.0, &model.plan.blocks[0], 0, 1e-10)
            .unwrap();
    assert!(update.settled);
    assert!(model.full_solves.get() > 0);
    assert!(y.iter().all(|value| (*value - 2.0).abs() < 1e-10));
    let matrix = model.storage.as_ref().unwrap().try_borrow_mut().unwrap();
    let JacobianStorage::Dense(dense) = &*matrix else {
        panic!("declined provider must leave dense interpreted storage")
    };
    assert_eq!(*dense, model.matrix);
}

#[test]
fn cached_reduction_tracks_coefficients_scaling_rhs_and_rejected_factors() {
    for dimension in [4, DIMENSION] {
        check_cached_reduction_tracks_coefficients_scaling_rhs_and_rejected_factors(dimension);
    }
}

fn check_cached_reduction_tracks_coefficients_scaling_rhs_and_rejected_factors(dimension: usize) {
    let expected = DVector::from_fn(dimension, |row, _| 1.0 + row as f64 / 8.0);
    let model = CyclicAffine::new(&[], &expected);
    let layout = model.structures.algebraic_projection()[0]
        .affine_elimination()
        .unwrap();
    let mut matrix = model.matrix.clone();
    for (iteration, diagonal) in [1.0, 2.0, 2.0, 0.0, 1e-12, f64::NAN, f64::INFINITY, 1.0]
        .into_iter()
        .enumerate()
    {
        matrix[(0, 0)] = diagonal;
        let expected = &expected * (iteration as f64 + 1.0);
        let residual = -(&matrix * &expected);
        let row_scales: Vec<_> = (0..dimension)
            .map(|i| 1.0 + (i + iteration) as f64)
            .collect();
        let variable_scales: Vec<_> = (0..dimension)
            .map(|i| 1.0 + (2 * i + iteration) as f64)
            .collect();
        let delta = scaled_newton_delta_with_tearing(
            ScaledNewtonSystem {
                jacobian: &matrix,
                residual: residual.as_slice(),
                row_scales: &row_scales,
                variable_scales: &variable_scales,
                structure: Some(layout.pattern()),
                tolerance: 1e-10,
            },
            &mut model.cache.borrow_mut(),
            solve::TearingCandidate::Primary,
            layout,
        );
        if diagonal.abs() < 1e-8 || !diagonal.is_finite() {
            assert!(delta.is_none());
        } else {
            let delta =
                delta.expect("fresh nonsingular coefficients must replace rejected factors");
            assert!(
                delta
                    .iter()
                    .zip(expected.iter())
                    .all(|(a, b)| (a - b).abs() < 1e-9)
            );
        }
    }
}

#[test]
fn reduced_affine_refinement_preserves_tiny_switching_voltage() {
    let mut matrix = DMatrix::identity(DIMENSION, DIMENSION);
    let small = [
        [1.0, -1.0, 0.0, 0.0],
        [0.0, 0.0, 1.0, 1e-5],
        [0.0, 1.0, -1.0, 0.0],
        [-1e-5, -2e-5, 0.0, 1.0],
    ];
    for row in 0..4 {
        for column in 0..4 {
            matrix[(row, column)] = small[row][column];
        }
    }
    let mut rhs = DVector::zeros(DIMENSION);
    rhs[0] = 50.0;
    rhs[1] = -50.0;
    rhs[3] = 0.002 + 1e-12 - 50.0 * 2e-5;
    let dependencies = (0..DIMENSION)
        .map(|row| {
            (0..DIMENSION)
                .filter(|&column| matrix[(row, column)] != 0.0)
                .collect()
        })
        .collect();
    let causal_steps = [(0, 1), (2, 2), (1, 3)]
        .into_iter()
        .chain((4..DIMENSION).map(|i| (i, i)))
        .map(|(row, y_index)| solve::CausalStep { row, y_index })
        .collect();
    let plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: (0..DIMENSION).collect(),
            y_indices: (0..DIMENSION).collect(),
            tearing: Some(solve::BlockTearing {
                tear_y_indices: vec![0],
                residual_rows: vec![3],
                causal_steps,
            }),
            guarded_tearing: None,
            alternate_charts: Vec::new(),
        }],
    };
    let mut model = CyclicAffine::from_system(matrix, rhs, plan, dependencies);
    model.residual_override = Some(super::super::affine_coordinates::offset_port_residual);
    let y = project(&model, 1e-10);
    let expected = (0.002 - (0.002 + 1e-12)) / (100000.0 + 3e-5);
    assert!(
        (y[0] / expected - 1.0).abs() < 1e-3,
        "junction voltage must stay negative: {y:?}"
    );
    assert_eq!(
        model.full_solves.get(),
        0,
        "reduction must retain full-coordinate refinement"
    );
}

#[test]
fn affine_pair_pivot_decline_and_certificate_failure_reach_primary_from_origin() {
    for dimension in [4, DIMENSION] {
        check_affine_pair_pivot_decline_and_certificate_failure_reach_primary_from_origin(
            dimension,
        );
    }
}

fn check_affine_pair_pivot_decline_and_certificate_failure_reach_primary_from_origin(
    dimension: usize,
) {
    for bad_certificate in [false, true] {
        let zero_pivots = [dimension - 2];
        let mut model = paired_model(dimension, if bad_certificate { &[] } else { &zero_pivots });
        model.invalid_guarded_correction = bad_certificate;
        let y = project(&model, 1e-10);
        assert!(y.iter().all(|v| (*v - 2.0).abs() < 1e-10));
        assert_eq!(
            model.full_solves.get(),
            0,
            "the primary candidate must precede untorn fallback"
        );
        let calls = model.candidate_calls.borrow();
        assert_eq!(calls[0].0, solve::TearingCandidate::Guarded);
        let first_primary = calls
            .iter()
            .find(|(kind, _)| *kind == solve::TearingCandidate::Primary)
            .unwrap();
        assert_eq!(
            first_primary.1,
            (-&model.rhs).as_slice(),
            "certificate failure must reset the arithmetic origin"
        );
    }
}

fn paired_model(dimension: usize, zero_pivots: &[usize]) -> CyclicAffine {
    let expected = DVector::from_element(dimension, 2.0);
    let original = CyclicAffine::new(zero_pivots, &expected);
    let mut plan = original.plan;
    let block = &mut plan.blocks[0];
    block.guarded_tearing = block.tearing.clone();
    let primary = block.tearing.as_mut().unwrap();
    primary.tear_y_indices.push(dimension - 2);
    primary.residual_rows.push(dimension - 2);
    primary
        .causal_steps
        .retain(|step| step.row != dimension - 2);
    let dependencies = (0..dimension)
        .map(|row| vec![row, (row + 1) % dimension])
        .collect();
    CyclicAffine::from_system(original.matrix, original.rhs, plan, dependencies)
}

#[test]
fn affine_pair_shares_origin_residual_only_within_one_projection() {
    for dimension in [4, DIMENSION] {
        check_affine_pair_shares_origin_residual_only_within_one_projection(dimension);
    }
}

fn check_affine_pair_shares_origin_residual_only_within_one_projection(dimension: usize) {
    for zero_pivots in [&[dimension - 2][..], &[0, dimension - 2][..]] {
        let model = paired_model(dimension, zero_pivots);
        for call in 1..=2 {
            let mut y = vec![-9.0; dimension];
            let update = project_affine_once(&model, &mut y, call as f64).unwrap();
            assert!(update.settled);
            assert!(y.iter().all(|value| (*value - 2.0).abs() < 1e-10));
            assert_eq!(model.origin_evaluations.get(), call);
            assert!(model.certificate_evaluations.get() >= call);
        }
        assert_eq!(model.full_solves.get() > 0, zero_pivots.len() == 2);
    }
}

fn project_affine_once(
    model: &CyclicAffine,
    y: &mut [f64],
    time: f64,
) -> Result<ProjectionBlockUpdate, RuntimeSolveError> {
    super::super::super::affine::project_affine_block(
        model,
        y,
        &[],
        time,
        &model.plan.blocks[0],
        0,
        1e-10,
    )
}

#[test]
fn affine_pair_residual_errors_stop_attempts_without_publishing() {
    for dimension in [4, DIMENSION] {
        check_affine_pair_residual_errors_stop_attempts_without_publishing(dimension);
    }
}

fn check_affine_pair_residual_errors_stop_attempts_without_publishing(dimension: usize) {
    for at_origin in [true, false] {
        let mut model = paired_model(dimension, &[]);
        model.residual_error_at_origin = Some(at_origin);
        let mut y = vec![-9.0; dimension];
        let error = project_affine_once(&model, &mut y, 0.0).unwrap_err();
        assert!(error.to_string().contains("affine residual witness"));
        assert_eq!(y, vec![-9.0; dimension]);
        assert_eq!(model.origin_evaluations.get(), 1);
        let candidates = model.candidate_calls.borrow();
        assert_eq!(candidates.len(), usize::from(!at_origin));
        if !at_origin {
            assert_eq!(candidates[0].0, solve::TearingCandidate::Guarded);
        }
    }
}
