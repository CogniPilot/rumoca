use super::*;

struct LinearSensitivity {
    plan: solve::AlgebraicProjectionPlan,
    corrupt_combined_direction: bool,
}

impl ImplicitProjectionModel for LinearSensitivity {
    fn eval_residual(
        &self,
        y: &[f64],
        _: &[f64],
        _: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = 17.0 * y[1] + 19.0 * y[2] - 23.0 * y[0];
        out[1] = 31.0 * y[1] + 7.0 * y[2] - 37.0 * y[0];
        Ok(())
    }

    fn eval_jacobian_v(
        &self,
        _: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_residual(v, p, t, out)?;
        if self.corrupt_combined_direction && v[0] != 0.0 && v[1] != 0.0 {
            out[0] += 1e-5 * v[0];
        }
        Ok(())
    }

    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row + 1))
    }
    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }
    fn target_name_for_row(&self, _: usize) -> Option<&str> {
        None
    }
}

fn model() -> LinearSensitivity {
    LinearSensitivity {
        corrupt_combined_direction: false,
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![1, 2],
                tearing: None,
            }],
        },
    }
}

fn project_seed(model: &LinearSensitivity, seed: &mut [f64]) -> Result<(), RuntimeSolveError> {
    project_algebraic_seed_with_plan(
        model,
        &model.plan,
        &[1.0, 271.0 / 235.0, 42.0 / 235.0],
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 1,
            tolerance: 1e-10,
        },
        seed,
    )
}

#[test]
fn directional_projection_accuracy_is_homogeneous_in_the_seed() {
    let model = model();
    for scale in [1.0, 1e8, 1e12, -1e12] {
        let mut seed = [scale, 0.0, 0.0];
        project_seed(&model, &mut seed).unwrap();
        assert!((seed[1] / scale - 271.0 / 235.0).abs() < 1e-14);
        assert!((seed[2] / scale - 42.0 / 235.0).abs() < 1e-14);
    }
}

#[test]
fn overflowing_direction_restores_the_incoming_unknown_seeds() {
    let model = model();
    let mut seed = [1e308, 4.0, 5.0];
    assert!(project_seed(&model, &mut seed).is_err());
    assert_eq!(seed, [1e308, 4.0, 5.0]);
}

#[test]
fn inconsistent_jvp_is_rejected_at_both_seed_scales() {
    let mut model = model();
    model.corrupt_combined_direction = true;
    for scale in [1.0, 1e12] {
        let mut seed = [scale, 4.0, 5.0];
        assert!(project_seed(&model, &mut seed).is_err());
        assert_eq!(seed, [scale, 4.0, 5.0]);
    }
}
