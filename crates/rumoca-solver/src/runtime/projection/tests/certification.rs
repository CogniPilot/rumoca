//! Accepted-state algebraic projection certificates.
//!
//! Split from `projection/tests.rs` under the SPEC_0021 file-size gate.

use super::super::*;

struct IllConditionedCoupledProjectionModel;

impl ImplicitProjectionModel for IllConditionedCoupledProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] + y[1];
        out[1] = y[0] + (1.0 + 1.0e-4) * y[1];
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
        out[1] = v[0] + (1.0 + 1.0e-4) * v[1];
        Ok(())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        static PLAN: std::sync::OnceLock<solve::AlgebraicProjectionPlan> =
            std::sync::OnceLock::new();
        PLAN.get_or_init(|| solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![0, 1],
            }],
        })
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        None
    }
}

#[test]
fn certified_projection_rejects_small_residual_with_large_coordinate_error() {
    let model = IllConditionedCoupledProjectionModel;
    let mut y = vec![1.0e-3, -1.0e-3];

    project_algebraics_with_plan_certified(
        &model,
        model.algebraic_projection_plan(),
        &mut y,
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 0,
            tolerance: 1.0e-6,
        },
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
    .expect("accepted projections must certify coordinate convergence");

    assert!(
        y.iter().all(|value| value.abs() <= 1.0e-12),
        "small row residual concealed a large coordinate error: {y:?}"
    );
}
