use super::*;
use rumoca_eval_solve::tensor_policy::{LinearSolveKernel, select_linear_solve_kernel};

fn layout(model: &CyclicAffine) -> &solve::AffineEliminationLayout {
    model.structures.algebraic_projection()[0]
        .affine_elimination()
        .unwrap()
}

fn solve_at(
    model: &CyclicAffine,
    matrix: &DMatrix<f64>,
    expected: &DVector<f64>,
) -> Option<DVector<f64>> {
    let residual = -(matrix * expected);
    let rows: Vec<_> = (0..matrix.nrows()).map(|i| 2.0 + i as f64).collect();
    let columns: Vec<_> = (0..matrix.ncols()).map(|i| 0.5 + i as f64).collect();
    scaled_newton_delta_with_tearing(
        ScaledNewtonSystem {
            jacobian: matrix,
            residual: residual.as_slice(),
            row_scales: &rows,
            variable_scales: &columns,
            structure: Some(layout(model).pattern()),
            tolerance: 1e-10,
        },
        &mut model.cache.borrow_mut(),
        solve::TearingCandidate::Primary,
        layout(model),
    )
}

#[test]
fn recovery_scans_each_issued_causal_row_once_across_tears() {
    use crate::runtime::projection::sparse_newton::recovery_dependency_scan_count;

    let expected = DVector::from_vec(vec![1.25, -0.75, 2.5, 3.25]);
    let original = CyclicAffine::new(&[], &expected);
    let mut plan = original.plan;
    let tearing = plan.blocks[0].tearing.as_mut().unwrap();
    tearing.tear_y_indices.push(2);
    tearing.residual_rows.push(2);
    tearing.causal_steps.retain(|step| step.row != 2);
    let dependencies = (0..4).map(|row| vec![row, (row + 1) % 4]).collect();
    let model = CyclicAffine::from_system(original.matrix, original.rhs, plan, dependencies);
    let layout = layout(&model);
    assert_eq!(layout.tears().len(), 2);
    let expected_scans: usize = layout
        .causal()
        .iter()
        .map(|&(row, _)| layout.row_columns(row).len())
        .sum();
    let before = recovery_dependency_scan_count();
    let solution = solve_at(&model, &model.matrix, &expected).unwrap();
    assert!((&solution - &expected).amax() < 1e-9);
    assert_eq!(recovery_dependency_scan_count() - before, expected_scans);
}

#[test]
fn issued_affine_layout_admits_each_full_kernel_class() {
    for (dimension, kernel) in [
        (4, LinearSolveKernel::SmallDense),
        (24, LinearSolveKernel::SparseCandidate),
        (24, LinearSolveKernel::Dense),
        (4, LinearSolveKernel::Diagonal),
    ] {
        let expected = DVector::from_fn(dimension, |i, _| 1.0 + i as f64 / 8.0);
        let original = CyclicAffine::new(&[], &expected);
        let matrix = match kernel {
            LinearSolveKernel::Diagonal => DMatrix::from_diagonal(&expected),
            LinearSolveKernel::Dense => DMatrix::from_fn(dimension, dimension, |r, c| {
                if r == c {
                    2.0
                } else if c > r || (r == dimension - 1 && c == 0) {
                    0.125
                } else {
                    0.0
                }
            }),
            _ => original.matrix * 2.5,
        };
        let dependencies = (0..dimension)
            .map(|r| (0..dimension).filter(|&c| matrix[(r, c)] != 0.0).collect())
            .collect();
        let rhs = &matrix * &expected;
        let model = CyclicAffine::from_system(matrix, rhs, original.plan, dependencies);
        assert_eq!(
            select_linear_solve_kernel(dimension, layout(&model).pattern()).unwrap(),
            kernel
        );
        assert_eq!(
            select_linear_solve_kernel(1, layout(&model).reduced_pattern()).unwrap(),
            LinearSolveKernel::SmallDense
        );
        let solution = solve_at(&model, &model.matrix, &expected)
            .expect("issued reduced solve is eligible independently of full kernel");
        assert!((&solution - &expected).amax() < 1e-9);
        assert!((&model.matrix * solution - &model.rhs).amax() < 1e-9);
        let projected = controls::project(&model, 1e-10);
        assert!((&DVector::from_vec(projected) - &expected).amax() < 1e-9);
        assert_eq!(
            model.full_solves.get(),
            0,
            "actual affine owner must use reduction"
        );
    }
}

#[test]
fn small_full_admission_rejects_mismatched_inputs_and_preserves_usable_cache() {
    let expected = DVector::from_element(4, 2.0);
    let model = CyclicAffine::new(&[], &expected);
    assert!(solve_at(&model, &model.matrix, &expected).is_some());
    let different =
        solve::StructuralPattern::full(4, 4, layout(&model).pattern().provenance()).unwrap();
    for mutation in 0..7 {
        let mut matrix = model.matrix.clone();
        let mut residual = (-&model.rhs).as_slice().to_vec();
        let mut rows = vec![1.0; 4];
        let mut columns = vec![1.0; 4];
        let mut pattern = Some(layout(&model).pattern());
        match mutation {
            0 => {
                residual.pop();
            }
            1 => {
                rows.pop();
            }
            2 => {
                columns.pop();
            }
            3 => {
                matrix = matrix.resize(3, 4, 0.0);
                residual.pop();
                rows.pop();
            }
            4 => {
                matrix = matrix.resize(4, 3, 0.0);
                columns.pop();
            }
            5 => pattern = Some(&different),
            _ => pattern = None,
        }
        assert!(
            scaled_newton_delta_with_tearing(
                ScaledNewtonSystem {
                    jacobian: &matrix,
                    residual: &residual,
                    row_scales: &rows,
                    variable_scales: &columns,
                    structure: pattern,
                    tolerance: 1e-10
                },
                &mut model.cache.borrow_mut(),
                solve::TearingCandidate::Primary,
                layout(&model),
            )
            .is_none(),
            "malformed case {mutation}"
        );
        let new_expected = &expected * (mutation + 2) as f64;
        let solution = solve_at(&model, &model.matrix, &new_expected).unwrap();
        assert!((solution - new_expected).amax() < 1e-9);
    }
}

#[test]
fn small_full_singular_reduced_factor_declines_then_recovers() {
    let expected = DVector::from_element(4, 2.0);
    let model = CyclicAffine::new(&[], &expected);
    assert!(solve_at(&model, &model.matrix, &expected).is_some());
    let mut singular = model.matrix.clone();
    // An exact zero reduced row tests factor decline without cancellation.
    singular.row_mut(3).fill(0.0);
    assert!(solve_at(&model, &singular, &expected).is_none());
    let changed_rhs = &expected * 3.0;
    let solution = solve_at(&model, &model.matrix, &changed_rhs).unwrap();
    assert!((solution - changed_rhs).amax() < 1e-9);
}

#[test]
fn complete_zero_tear_partition_cannot_issue_affine_layout() {
    let expected = DVector::from_element(4, 2.0);
    let original = CyclicAffine::new(&[], &expected);
    let mut plan = original.plan;
    let tearing = plan.blocks[0].tearing.as_mut().unwrap();
    tearing.tear_y_indices.clear();
    tearing.residual_rows.clear();
    tearing
        .causal_steps
        .push(solve::CausalStep { row: 3, y_index: 3 });
    assert!(plan.blocks[0].has_valid_tearing_partitions());
    let dependencies = (0..4).map(|r| vec![r, (r + 1) % 4]).collect();
    let model = CyclicAffine::from_system(original.matrix, original.rhs, plan, dependencies);
    assert!(
        model.structures.algebraic_projection()[0]
            .affine_elimination()
            .is_none()
    );
}

#[test]
fn small_full_rechecks_scaled_pivot_before_reusing_a_factor() {
    let expected = DVector::from_element(4, 2.0);
    let model = CyclicAffine::new(&[], &expected);
    assert!(solve_at(&model, &model.matrix, &expected).is_some());
    let residual = -&model.rhs;
    assert!(
        scaled_newton_delta_with_tearing(
            ScaledNewtonSystem {
                jacobian: &model.matrix,
                residual: residual.as_slice(),
                row_scales: &[1.0; 4],
                variable_scales: &[1e-12, 1.0, 1.0, 1.0],
                structure: Some(layout(&model).pattern()),
                tolerance: 1e-10,
            },
            &mut model.cache.borrow_mut(),
            solve::TearingCandidate::Primary,
            layout(&model),
        )
        .is_none(),
        "unchanged coefficients do not excuse a newly unsuitable scaled pivot"
    );
    let solution = solve_at(&model, &model.matrix, &expected).unwrap();
    assert!((solution - expected).amax() < 1e-9);
}

#[test]
fn refactor_checks_each_causal_pivot_once_and_unchanged_factor_checks_afresh() {
    use crate::runtime::projection::sparse_newton::pivot_check_count;
    let expected = DVector::from_fn(4, |i, _| 1.0 + i as f64 / 8.0);
    let mut model = CyclicAffine::new(&[], &expected);
    model.matrix *= 2.5;
    let causal_count = layout(&model).causal().len();
    for multiple in [1.0, 2.0] {
        let before = pivot_check_count();
        let target = &expected * multiple;
        let solution = solve_at(&model, &model.matrix, &target).unwrap();
        assert!((&solution - &target).amax() < 1e-9);
        assert!((&model.matrix * (&solution - &target)).amax() < 1e-9);
        assert_eq!(
            pivot_check_count() - before,
            causal_count,
            "refactor consumes this invocation's complete preflight; unchanged factors still preflight"
        );
    }
}

#[test]
fn same_size_reordered_layout_binds_fresh_pivots_to_the_replacement() {
    let expected = DVector::from_fn(4, |i, _| 2.0 + i as f64);
    let original = CyclicAffine::new(&[], &expected);
    let matrix = DMatrix::from_diagonal(&expected);
    let dependencies = (0..4).map(|row| vec![row]).collect();
    let mut model = CyclicAffine::from_system(
        matrix.clone(),
        &matrix * &expected,
        original.plan,
        dependencies,
    );
    let solution = solve_at(&model, &model.matrix, &expected).unwrap();
    assert!((solution - &expected).amax() < 1e-9);
    let original_order = layout(&model).causal().to_vec();
    let mut plan = model.plan.clone();
    plan.blocks[0]
        .tearing
        .as_mut()
        .unwrap()
        .causal_steps
        .reverse();
    let replacement = CyclicAffine::from_system(
        matrix.clone(),
        &matrix * &expected,
        plan,
        (0..4).map(|row| vec![row]).collect(),
    );
    assert_ne!(original_order, layout(&replacement).causal());
    // Retain the populated numerical cache while replacing the issued order.
    model.plan = replacement.plan;
    model.structures = replacement.structures;
    let target = &expected * 3.0;
    let solution = solve_at(&model, &model.matrix, &target).unwrap();
    assert!((&solution - &target).amax() < 1e-9);
    assert!((&matrix * (solution - target)).amax() < 1e-9);
}

#[test]
fn late_pivot_refusal_cannot_publish_partial_preflight() {
    use crate::runtime::projection::sparse_newton::pivot_check_count;
    let expected = DVector::from_element(4, 2.0);
    let model = CyclicAffine::new(&[], &expected);
    assert!(solve_at(&model, &model.matrix, &expected).is_some());
    let &(row, target) = layout(&model).causal().last().unwrap();
    let mut invalid = model.matrix.clone();
    invalid[(row, target)] = 0.0;
    let before = pivot_check_count();
    assert!(solve_at(&model, &invalid, &expected).is_none());
    assert_eq!(pivot_check_count() - before, layout(&model).causal().len());
    let next = &expected * 2.0;
    let solution = solve_at(&model, &model.matrix, &next).unwrap();
    assert!((solution - next).amax() < 1e-9);
}
