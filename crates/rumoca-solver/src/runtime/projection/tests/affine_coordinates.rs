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
