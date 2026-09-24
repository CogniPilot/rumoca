use super::*;

// A free rotor's axial equations require acceleration = torque = 0,
// regardless of the large direction in the coupled load equation.
const INERTIA: f64 = 0.015118914645400883;
const LOAD: f64 = 100_000_028.1;

struct AxialSensitivity {
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for AxialSensitivity {
    fn eval_residual(
        &self,
        y: &[f64],
        _: &[f64],
        _: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = INERTIA * y[1] - y[2];
        out[1] = y[2];
        out[2] = y[1] + y[3] - LOAD * y[0];
        out[3] = y[4] - 3.0 * y[1];
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
        self.eval_residual(v, p, t, out)
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

#[test]
fn a_large_direction_does_not_pollute_exact_zero_acceleration() {
    let model = AxialSensitivity {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![
                solve::AlgebraicProjectionBlock {
                    rows: vec![0, 1, 2],
                    y_indices: vec![1, 2, 3],
                    tearing: None,
                    guarded_tearing: None,
                    alternate_charts: Vec::new(),
                },
                solve::AlgebraicProjectionBlock {
                    rows: vec![3],
                    y_indices: vec![4],
                    tearing: None,
                    guarded_tearing: None,
                    alternate_charts: Vec::new(),
                },
            ],
        },
    };
    for direction in [1.0, -1.0] {
        let mut seed = [direction, 3.0, 4.0, 5.0, 6.0];
        project_algebraic_seed_with_plan(
            &model,
            &model.plan,
            &[1.0, 0.0, 0.0, LOAD, 0.0],
            AlgebraicProjectionArgs {
                parameters: &[],
                time: 0.0,
                state_count: 1,
                tolerance: 1e-10,
            },
            &mut seed,
        )
        .unwrap();
        assert_eq!(seed[0], direction);
        assert!(seed[1].abs() < 1e-12, "zero acceleration: {seed:?}");
        assert!(seed[2].abs() < 1e-12, "zero axial torque: {seed:?}");
        assert_eq!(seed[3], LOAD * direction);
        assert!(seed[4].abs() < 1e-12, "downstream direction: {seed:?}");
    }
}
