use super::*;

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
            .map(|row| solve::CausalStep {
                row,
                y_index: row,
                ..Default::default()
            })
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

/// A nonzero raw guard, even one conditioning underflows to zero, promotes
/// the step solving its column instead of declining; a nonfinite one still
/// declines, and a later zero guard restores the issued reduction.
#[test]
fn future_dependency_guards_recheck_raw_coefficients_before_cached_solve() {
    let expected = DVector::from_element(DIMENSION, 2.0);
    let original = CyclicAffine::new(&[], &expected);
    let mut dependencies: Vec<_> = (0..DIMENSION)
        .map(|row| vec![row, (row + 1) % DIMENSION])
        .collect();
    dependencies[22].insert(0, 0);
    let model = CyclicAffine::from_system(
        original.matrix.clone(),
        original.rhs,
        original.plan,
        dependencies,
    );
    let layout = model.structures.algebraic_projection()[0]
        .affine_elimination()
        .unwrap();
    assert_eq!(layout.zero_guards(), &[(22, 0)]);
    let mut matrix = model.matrix.clone();
    let mut row_scales = vec![1.0; DIMENSION];
    row_scales[22] = 1e100;
    for coefficient in [0.0, 1e-310, 0.5, f64::NAN, 0.0] {
        matrix[(22, 0)] = coefficient;
        let residual = -(&matrix * &expected);
        let delta = scaled_newton_delta_with_tearing(
            ScaledNewtonSystem {
                jacobian: &matrix,
                residual: residual.as_slice(),
                row_scales: &row_scales,
                variable_scales: &[1.0; DIMENSION],
                structure: Some(layout.pattern()),
                tolerance: 1e-10,
            },
            &mut model.cache.borrow_mut(),
            layout,
        );
        assert_eq!(delta.is_some(), coefficient.is_finite(), "{coefficient}");
        if let Some(delta) = delta {
            assert!(delta.iter().all(|x| (x - 2.0).abs() < 1e-10));
            let promoted = usize::from(coefficient != 0.0);
            assert_eq!(
                model.cache.borrow().torn_reduced_size(),
                Some(1 + promoted),
                "{coefficient}"
            );
        }
    }
}

fn project(model: &CyclicAffine, tolerance: f64) -> Vec<f64> {
    let mut y = vec![0.0; DIMENSION];
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
    let expected = DVector::from_element(DIMENSION, 2.0);
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
fn cached_reduction_tracks_coefficients_scaling_rhs_and_rejected_factors() {
    let expected = DVector::from_fn(DIMENSION, |row, _| 1.0 + row as f64 / 8.0);
    let model = CyclicAffine::new(&[], &expected);
    let layout = model.structures.algebraic_projection()[0]
        .affine_elimination()
        .unwrap();
    let mut matrix = model.matrix.clone();
    for (iteration, diagonal) in [1.0, 2.0, 2.0, 0.0, f64::NAN, 1.0].into_iter().enumerate() {
        matrix[(0, 0)] = diagonal;
        let expected = &expected * (iteration as f64 + 1.0);
        let residual = -(&matrix * &expected);
        let row_scales: Vec<_> = (0..DIMENSION)
            .map(|i| 1.0 + (i + iteration) as f64)
            .collect();
        let variable_scales: Vec<_> = (0..DIMENSION)
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
            layout,
        );
        if !diagonal.is_finite() {
            assert!(delta.is_none());
        } else {
            // A zero diagonal promotes the last causal step to a tear.
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
        .map(|(row, y_index)| solve::CausalStep {
            row,
            y_index,
            ..Default::default()
        })
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
