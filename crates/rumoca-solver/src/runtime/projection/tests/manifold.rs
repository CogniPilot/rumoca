use super::*;

struct PendulumManifoldModel {
    plan: solve::AlgebraicProjectionPlan,
}

impl ManifoldProjectionModel for PendulumManifoldModel {
    fn eval_manifold_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] * y[0] + y[1] * y[1] - 1.0;
        out[1] = y[0] * y[2] + y[1] * y[3];
        Ok(())
    }

    fn eval_manifold_jacobian_v(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = 2.0 * y[0] * v[0] + 2.0 * y[1] * v[1];
        out[1] = y[2] * v[0] + y[3] * v[1] + y[0] * v[2] + y[1] * v[3];
        Ok(())
    }

    fn manifold_residual_len(&self) -> usize {
        2
    }

    fn manifold_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }
}

#[test]
fn manifold_projection_uses_rectangular_minimum_norm_state_correction() {
    let model = PendulumManifoldModel {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![0, 1, 2, 3],
            }],
        },
    };
    let mut y = vec![1.02, 0.03, 0.10, 0.04];

    let changed = project_state_manifold(&model, &mut y, &[], 12.0, 4, 1.0e-12)
        .expect("the rectangular pendulum manifold has full row rank");

    assert!(changed);
    assert!(
        (y[0] * y[0] + y[1] * y[1] - 1.0).abs() <= 1.0e-12,
        "holonomic residual was not projected: {y:?}"
    );
    assert!(
        (y[0] * y[2] + y[1] * y[3]).abs() <= 1.0e-12,
        "velocity residual was not projected: {y:?}"
    );
}

struct ImpossibleManifoldModel {
    plan: solve::AlgebraicProjectionPlan,
}

impl ManifoldProjectionModel for ImpossibleManifoldModel {
    fn eval_manifold_residual(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = 1.0;
        Ok(())
    }

    fn eval_manifold_jacobian_v(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out.fill(0.0);
        Ok(())
    }

    fn manifold_residual_len(&self) -> usize {
        1
    }

    fn manifold_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }
}

#[test]
fn failed_manifold_projection_rolls_back_every_y_slot() {
    let model = ImpossibleManifoldModel {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0, 1],
            }],
        },
    };
    let original = vec![0.25, -0.75, 42.0];
    let mut y = original.clone();

    let error = project_state_manifold(&model, &mut y, &[], 3.0, 2, 1.0e-12)
        .expect_err("a constant nonzero residual cannot be projected");

    assert!(
        error.to_string().contains("did not converge"),
        "unexpected projection failure: {error}"
    );
    assert_eq!(y, original, "failed projection must restore all Y slots");
}
