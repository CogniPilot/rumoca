//! A singular scalar isolation must not derail a nonsingular coupled solve.

use super::*;

#[test]
fn nonlinear_projection_keeps_its_initial_guess_schedule() {
    let mut model = mode_dependent_repivot_model();
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![
                sqrt_assignment_residual_row(0, 1),
                scale_and_offset_assignment_residual_row(1, 0, 0.5, 1.0),
            ],
            "nonlinear_stage_seed.mo",
        ));
    model.problem.continuous.implicit_row_targets[0] = Some(solve::scalar_slot_y(0));
    derive_test_structural_artifacts(&mut model);
    let runtime = SolveRuntime::new_fixture(&model).unwrap();
    assert!(runtime.algebraic_refresh.value_stages.iter().any(|stage| {
        matches!(stage, solve::RefreshStage::ProjectionBlock { seed_rows, .. } if !seed_rows.is_empty())
    }));
}

#[test]
fn affine_stage_seeds_do_not_divide_by_a_vanishing_scalar_pivot() {
    // y - k*x = 1, x + y = 3. Isolating x divides by k, but the
    // coupled determinant is -(k+1), including at the valid point k=0.
    let mut model = mode_dependent_repivot_model();
    model.problem.continuous.implicit_row_targets[0] = Some(solve::scalar_slot_y(0));
    derive_test_structural_artifacts(&mut model);
    let runtime = SolveRuntime::new_fixture(&model).unwrap();
    assert!(runtime.value_stage_schedule_is_certified(&runtime.algebraic_refresh));
    let stage = runtime
        .algebraic_refresh
        .value_stages
        .iter()
        .find(|stage| matches!(stage, solve::RefreshStage::ProjectionBlock { .. }))
        .expect("the coupled system must retain its projection stage");
    let solve::RefreshStage::ProjectionBlock {
        seed_rows, plan, ..
    } = stage
    else {
        unreachable!()
    };
    assert_eq!(plan.blocks[0].y_indices.len(), 2);
    assert!(
        seed_rows.is_empty(),
        "the affine solve is independent of a seed that can divide by zero"
    );

    let mut solver_y = model.initial_y.clone();
    for k in [0.0, 1.0, 3.0, 0.0] {
        runtime
            .refresh_algebraic_and_output_slots_certified(1.0, &mut solver_y, &[k], 1e-10, 4)
            .expect("the same prepared block must solve at changing coefficients");
        assert!((solver_y[0] - 2.0 / (1.0 + k)).abs() < 1e-9);
        assert!((solver_y[1] - (3.0 - solver_y[0])).abs() < 1e-9);
    }
}
