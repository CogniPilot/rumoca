use super::super::*;

struct AffineCoordinates {
    drive: f64,
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for AffineCoordinates {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] + y[1] - self.drive;
        out[1] = y[0] - y[1];
        Ok(())
    }

    fn eval_jacobian_v(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = v[0] + v[1];
        out[1] = v[0] - v[1];
        Ok(())
    }

    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        (row < 2).then(|| solve::scalar_slot_y(row))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row: usize) -> Option<&str> {
        None
    }

    fn algebraic_projection_block_is_affine(&self, _block: usize) -> bool {
        true
    }
}

#[test]
fn affine_projection_computes_coordinates_below_residual_tolerance() {
    for drive in [-1e-20, 1e-20, -1e-8, 1e-8] {
        let model = AffineCoordinates {
            drive,
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![0, 1],
                    y_indices: vec![0, 1],
                    tearing: None,
                }],
            },
        };
        let mut y = [-drive, -drive];
        project_algebraics_with_plan_certified(
            &model,
            &model.plan,
            &mut y,
            AlgebraicProjectionArgs {
                parameters: &[],
                time: 0.0,
                state_count: 0,
                tolerance: 1e-6,
            },
            ALGEBRAIC_PROJECTION_MAX_ITERS,
        )
        .unwrap();
        for value in y {
            assert!(
                (value / drive - 0.5).abs() < 1e-12,
                "drive={drive}, y={y:?}"
            );
        }
    }
}

struct OpposingCurrents {
    plan: solve::AlgebraicProjectionPlan,
}

struct LeakageCurrent {
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for LeakageCurrent {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        // z, u, i1, i2: two low resistances meet a small leakage conductance.
        out[0] = y[2] + y[3] - y[0];
        out[1] = 1e-5 * y[2] - y[1];
        out[2] = 1e-5 * y[3] - y[1] + 60.0;
        out[3] = y[0] - 1e-5 * y[1];
        Ok(())
    }

    fn eval_jacobian_v(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = v[2] + v[3] - v[0];
        out[1] = 1e-5 * v[2] - v[1];
        out[2] = 1e-5 * v[3] - v[1];
        out[3] = v[0] - 1e-5 * v[1];
        Ok(())
    }

    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        (row < 4).then(|| solve::scalar_slot_y(row))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }
    fn target_name_for_row(&self, _row: usize) -> Option<&str> {
        None
    }
    fn algebraic_projection_block_is_affine(&self, _block: usize) -> bool {
        true
    }
}

#[test]
fn affine_projection_refines_small_coordinates_in_a_mixed_scale_loop() {
    let model = LeakageCurrent {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1, 2, 3],
                y_indices: vec![0, 1, 2, 3],
                tearing: None,
            }],
        },
    };
    let mut y = [0.0; 4];
    project_algebraics_with_plan_certified(
        &model,
        &model.plan,
        &mut y,
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 0,
            tolerance: 1e-12,
        },
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
    .unwrap();
    let expected_u = 60.0 / (2.0 - 1e-10);
    assert!(
        (y[0] - 1e-5 * expected_u).abs() < 1e-12,
        "leakage current: {y:?}"
    );
}

impl ImplicitProjectionModel for OpposingCurrents {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = 1e-5 * y[0] - y[2];
        out[1] = 1e-5 * y[1] - y[2] + 60.0;
        out[2] = y[0] + y[1] - 0.007236541;
        Ok(())
    }

    fn eval_jacobian_v(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = 1e-5 * v[0] - v[2];
        out[1] = 1e-5 * v[1] - v[2];
        out[2] = v[0] + v[1];
        Ok(())
    }

    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        (row < 3).then(|| solve::scalar_slot_y(row))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row: usize) -> Option<&str> {
        None
    }

    fn algebraic_projection_block_is_affine(&self, _block: usize) -> bool {
        true
    }
}

#[test]
fn affine_projection_certifies_residual_at_the_solution_scale() {
    let model = OpposingCurrents {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1, 2],
                y_indices: vec![0, 1, 2],
                tearing: None,
            }],
        },
    };
    let mut y = [0.0; 3];
    project_algebraics_with_plan_certified(
        &model,
        &model.plan,
        &mut y,
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 0,
            tolerance: 1e-10,
        },
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
    .unwrap();
    let expected = [
        3e6 + 0.007236541 / 2.0,
        -3e6 + 0.007236541 / 2.0,
        30.0 + 1e-5 * 0.007236541 / 2.0,
    ];
    for (actual, expected) in y.into_iter().zip(expected) {
        assert!(
            (actual - expected).abs() <= 1e-14 * expected.abs(),
            "y={y:?}"
        );
    }
}
