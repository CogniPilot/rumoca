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
                tearing: None,
                alternate_charts: Vec::new(),
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

/// i = 0.5*v/R + drive, v = R*i; hence i = 2*drive, v = 2*R*drive.
struct AmplifiedCausalProjection {
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for AmplifiedCausalProjection {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[1] - 1e6 * y[0];
        out[1] = y[0] - 0.5e-6 * y[1] - 1e-11;
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let mut residual = [0.0; 2];
        self.eval_residual(y, p, t, &mut residual)?;
        Ok(residual.get(row).copied())
    }

    fn eval_jacobian_v(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = v[1] - 1e6 * v[0];
        out[1] = v[0] - 0.5e-6 * v[1];
        Ok(())
    }

    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        [1, 0].get(row).copied().map(solve::scalar_slot_y)
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, row: usize) -> Option<&str> {
        ["v", "i"].get(row).copied()
    }

    fn implicit_target_assignment_is_exact(&self, row: usize, target: usize) -> bool {
        (row, target) == (0, 1)
    }

    fn eval_implicit_target_value(
        &self,
        row: usize,
        target: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(((row, target) == (0, 1)).then(|| 1e6 * y[0]))
    }
}

#[test]
fn certified_torn_projection_checks_recovered_coordinates() {
    let tearing = solve::BlockTearing {
        tear_y_indices: vec![0],
        residual_rows: vec![1],
        causal_steps: vec![solve::CausalStep { row: 0, y_index: 1 }],
    };
    let model = AmplifiedCausalProjection {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![0, 1],
                tearing: Some(tearing.clone()),
                alternate_charts: Vec::new(),
            }],
        },
    };
    let mut y = [0.0, 0.0];
    let update =
        tearing::project_torn_algebraic_block(&model, &mut y, &[], 0.0, &tearing, 1e-10, true)
            .unwrap()
            .expect("the affine torn block converges without a dense fallback");
    assert!(update.settled);
    assert!((y[0] - 2e-11).abs() < 1e-18, "current: {y:?}");
    assert!((y[1] - 2e-5).abs() < 1e-12, "voltage: {y:?}");
}
