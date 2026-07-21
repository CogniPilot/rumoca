use std::cell::Cell;

use super::*;

struct BlockProjectionModel {
    plan: solve::AlgebraicProjectionPlan,
    initial_residual_len: usize,
}

struct PoorlyScaledProjectionModel {
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for PoorlyScaledProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = 1.0e-6 * y[0] + y[0].powi(3) - 1.0;
        Ok(())
    }

    fn eval_jacobian_v(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = (1.0e-6 + 3.0 * y[0].powi(2)) * v[0];
        Ok(())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        Some("x")
    }
}

struct ContinuousCausalAssignmentModel {
    residual_calls: Cell<usize>,
    residual_row_calls: Cell<usize>,
    jacobian_calls: Cell<usize>,
    target_value: f64,
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for ContinuousCausalAssignmentModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.residual_calls.set(self.residual_calls.get() + 1);
        out[0] = y[0] - 5.0;
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
        self.jacobian_calls.set(self.jacobian_calls.get() + 1);
        out[0] = v[0];
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        _row_idx: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.residual_row_calls
            .set(self.residual_row_calls.get() + 1);
        Ok(Some(y[0] - 5.0))
    }

    fn eval_implicit_target_value(
        &self,
        _row_idx: usize,
        _target_y_index: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(Some(self.target_value))
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        Some("x")
    }
}

impl ImplicitProjectionModel for BlockProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] - 2.0;
        out[1] = y[1] - 3.0;
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
        out.copy_from_slice(v);
        Ok(())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        None
    }
}

impl AlgebraicProjectionModel for BlockProjectionModel {
    fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_residual(y, p, t, out)
    }

    fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_jacobian_v(y, p, t, v, out)
    }

    fn initial_residual_len(&self) -> usize {
        self.initial_residual_len
    }

    fn initial_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
        None
    }
}

struct RectInitialProjectionModel;

struct InitialCausalAssignmentModel {
    initial_residual_calls: Cell<usize>,
    initial_residual_row_calls: Cell<usize>,
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for InitialCausalAssignmentModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] - 5.0;
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
        out[0] = v[0];
        Ok(())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        Some("x")
    }
}

impl AlgebraicProjectionModel for InitialCausalAssignmentModel {
    fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.initial_residual_calls
            .set(self.initial_residual_calls.get() + 1);
        self.eval_residual(y, p, t, out)
    }

    fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_jacobian_v(y, p, t, v, out)
    }

    fn initial_residual_len(&self) -> usize {
        1
    }

    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn eval_initial_target_value(
        &self,
        _row_idx: usize,
        _target_y_index: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(Some(5.0))
    }

    fn eval_initial_residual_row(
        &self,
        _row_idx: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.initial_residual_row_calls
            .set(self.initial_residual_row_calls.get() + 1);
        Ok(Some(y[0] - 5.0))
    }
}

impl ImplicitProjectionModel for RectInitialProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] - 2.0;
        out[1] = 2.0 * y[0] - 4.0;
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
        out[0] = v[0];
        out[1] = 2.0 * v[0];
        Ok(())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        static PLAN: std::sync::OnceLock<solve::AlgebraicProjectionPlan> =
            std::sync::OnceLock::new();
        PLAN.get_or_init(solve::AlgebraicProjectionPlan::default)
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        None
    }
}

impl AlgebraicProjectionModel for RectInitialProjectionModel {
    fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_residual(y, p, t, out)
    }

    fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_jacobian_v(y, p, t, v, out)
    }

    fn initial_residual_len(&self) -> usize {
        2
    }

    fn initial_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
        None
    }
}

struct TargetedInitialProjectionModel;

impl ImplicitProjectionModel for TargetedInitialProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] + y[1] - 2.0;
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
        Ok(())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        static PLAN: std::sync::OnceLock<solve::AlgebraicProjectionPlan> =
            std::sync::OnceLock::new();
        PLAN.get_or_init(solve::AlgebraicProjectionPlan::default)
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        Some("target")
    }
}

impl AlgebraicProjectionModel for TargetedInitialProjectionModel {
    fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_residual(y, p, t, out)
    }

    fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_jacobian_v(y, p, t, v, out)
    }

    fn initial_residual_len(&self) -> usize {
        1
    }

    fn initial_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(0))
    }
}

struct CoupledTargetedInitialProjectionModel;

impl ImplicitProjectionModel for CoupledTargetedInitialProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] + y[1] - 1.0;
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
        Some("target")
    }
}

impl AlgebraicProjectionModel for CoupledTargetedInitialProjectionModel {
    fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_residual(y, p, t, out)
    }

    fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_jacobian_v(y, p, t, v, out)
    }

    fn initial_residual_len(&self) -> usize {
        2
    }

    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }
}

#[test]
fn project_algebraics_uses_solve_projection_plan_blocks() {
    let model = BlockProjectionModel {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![
                solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                },
                solve::AlgebraicProjectionBlock {
                    rows: vec![1],
                    y_indices: vec![1],
                },
            ],
        },
        initial_residual_len: 0,
    };
    let mut y = vec![0.0, 0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-12)
        .expect("block projection should converge");

    assert_eq!(y, vec![2.0, 3.0]);
}

#[test]
fn project_algebraics_backtracks_to_variable_resolution() {
    let model = PoorlyScaledProjectionModel {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
    };
    let mut y = vec![0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-10)
        .expect("damped Newton projection should converge");

    assert!((1.0e-6 * y[0] + y[0].powi(3) - 1.0).abs() <= 1.0e-10);
}

#[test]
fn algebraic_step_resolution_uses_actual_candidate_ulps() {
    assert!(algebraic_step_at_resolution(0.0, 0.0_f64.next_up()));
    assert!(algebraic_step_at_resolution(1.0, 1.0_f64.next_down()));
    assert!(!algebraic_step_at_resolution(0.0, 1.0e184));
    assert!(!algebraic_step_at_resolution(
        1.0,
        1.0_f64.next_up().next_up()
    ));
}

#[test]
fn continuous_singleton_assignment_avoids_jacobian_projection() {
    let model = ContinuousCausalAssignmentModel {
        residual_calls: Cell::new(0),
        residual_row_calls: Cell::new(0),
        jacobian_calls: Cell::new(0),
        target_value: 5.0,
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
    };
    let mut y = vec![0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-12)
        .expect("singleton assignment should project");

    assert_eq!(y, vec![5.0]);
    assert_eq!(model.residual_calls.get(), 2);
    assert_eq!(model.residual_row_calls.get(), 2);
    assert_eq!(model.jacobian_calls.get(), 0);
}

#[test]
fn continuous_singleton_assignment_does_not_accept_inexact_improvement() {
    let model = ContinuousCausalAssignmentModel {
        residual_calls: Cell::new(0),
        residual_row_calls: Cell::new(0),
        jacobian_calls: Cell::new(0),
        target_value: 4.0,
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
    };
    let mut y = vec![0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-12)
        .expect("Newton should finish an inexact assignment candidate");

    assert_eq!(y, vec![5.0]);
    assert!(model.jacobian_calls.get() > 0);
}

#[test]
fn initial_singleton_assignment_is_certified_by_complete_residual() {
    let model = InitialCausalAssignmentModel {
        initial_residual_calls: Cell::new(0),
        initial_residual_row_calls: Cell::new(0),
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
    };
    let mut y = vec![0.0];

    project_initial_variables_with_plan(&model, &mut y, &[], 0.0, &model.plan, 1.0e-12)
        .expect("singleton initial assignment should project");

    assert_eq!(y, vec![5.0]);
    assert_eq!(
        model.initial_residual_calls.get(),
        2,
        "the plan is certified against the complete initial residual"
    );
    assert_eq!(model.initial_residual_row_calls.get(), 2);
}

#[test]
fn initial_projection_rejects_omitted_residual_and_restores_candidate() {
    let model = BlockProjectionModel {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
        initial_residual_len: 2,
    };
    let mut y = vec![0.0, 0.0];

    let err = project_initial_variables_with_plan(&model, &mut y, &[], 0.0, &model.plan, 1.0e-12)
        .expect_err("an omitted nonzero initial residual must reject the candidate");

    assert!(err.to_string().contains("complete residual system"));
    assert_eq!(y, vec![0.0, 0.0]);
}

#[test]
fn project_algebraics_rejects_state_count_past_y_length() {
    let model = BlockProjectionModel {
        plan: solve::AlgebraicProjectionPlan::default(),
        initial_residual_len: 0,
    };
    let mut y = vec![1.0];

    let err = project_algebraics(&model, &mut y, &[], 0.0, 2, 1.0e-12)
        .expect_err("state count beyond y length should fail");

    assert!(
        err.to_string()
            .contains("state count 2 exceeds vector length 1")
    );
}

#[test]
fn projection_residual_tail_rejects_state_count_past_rhs_length() {
    let err = projection_residual_tail(&[0.0], 2)
        .expect_err("state count beyond residual length should fail");

    assert!(
        err.to_string()
            .contains("state count 2 exceeds vector length 1")
    );
}

#[test]
fn project_algebraic_block_rejects_rectangular_inventory() {
    let model = BlockProjectionModel {
        plan: solve::AlgebraicProjectionPlan::default(),
        initial_residual_len: 0,
    };
    let block = solve::AlgebraicProjectionBlock {
        rows: vec![0],
        y_indices: vec![0, 1],
    };
    let mut y = vec![0.0, 0.0];

    let err = project_algebraic_block(&model, &mut y, &[], 0.0, &block, 1.0e-12)
        .expect_err("rectangular projection inventory must be rejected");

    assert!(err.to_string().contains("1 residual rows but 2 unknowns"));
    assert_eq!(y, vec![0.0, 0.0]);
}

#[test]
fn project_algebraic_block_rejects_row_outside_residual_vector() {
    let model = BlockProjectionModel {
        plan: solve::AlgebraicProjectionPlan::default(),
        initial_residual_len: 0,
    };
    let block = solve::AlgebraicProjectionBlock {
        rows: vec![2],
        y_indices: vec![0],
    };
    let mut y = vec![0.0, 0.0];

    let err = project_algebraic_block(&model, &mut y, &[], 0.0, &block, 1.0e-12)
        .expect_err("invalid projection row should bubble a runtime error");

    assert!(
        err.to_string()
            .contains("references residual row 2, but the model evaluated only 2")
    );
}

#[test]
fn project_algebraics_applies_sub_tolerance_correction_until_residual_converges() {
    let model = ScaledResidualProjectionModel;
    let mut y = vec![0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-6)
        .expect("projection should certify the corrected residual");

    assert!((y[0] + 1.0e-7).abs() <= f64::EPSILON);
}

#[test]
fn project_initial_variables_applies_sub_tolerance_correction_until_residual_converges() {
    let model = ScaledResidualProjectionModel;
    let mut y = vec![0.0];

    project_initial_variables_with_plan(
        &model,
        &mut y,
        &[],
        0.0,
        model.algebraic_projection_plan(),
        1.0e-6,
    )
    .expect("initial projection should certify the corrected residual");

    assert!((y[0] + 1.0e-7).abs() <= f64::EPSILON);
}

struct ScaledResidualProjectionModel;

impl ImplicitProjectionModel for ScaledResidualProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = 1.0e3 * y[0] + 1.0e-4;
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
        out[0] = 1.0e3 * v[0];
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
                rows: vec![0],
                y_indices: vec![0],
            }],
        })
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        None
    }
}

impl AlgebraicProjectionModel for ScaledResidualProjectionModel {
    fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_residual(y, p, t, out)
    }

    fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_jacobian_v(y, p, t, v, out)
    }

    fn initial_residual_len(&self) -> usize {
        1
    }

    fn initial_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
        None
    }
}

#[test]
fn project_initial_block_rejects_rectangular_inventory() {
    let model = RectInitialProjectionModel;
    let block = solve::AlgebraicProjectionBlock {
        rows: vec![0, 1],
        y_indices: vec![0],
    };
    let mut y = vec![0.0, 0.0];

    let err = project_initial_block(&model, &mut y, &[], 0.0, &block, 1.0e-12)
        .expect_err("rectangular initial projection inventory must be rejected");

    assert!(err.to_string().contains("2 residual rows but 1 unknowns"));
    assert_eq!(y, vec![0.0, 0.0]);
}

#[test]
fn project_initial_block_rejects_rectangular_targeted_inventory() {
    let model = TargetedInitialProjectionModel;
    let block = solve::AlgebraicProjectionBlock {
        rows: vec![0],
        y_indices: vec![0, 1],
    };
    let mut y = vec![0.0, 0.0];

    let err = project_initial_block(&model, &mut y, &[], 0.0, &block, 1.0e-12)
        .expect_err("row targets must not bypass the square-block contract");

    assert!(err.to_string().contains("1 residual rows but 2 unknowns"));
    assert_eq!(y, vec![0.0, 0.0]);
}

#[test]
fn project_initial_variables_solves_coupled_targeted_block_as_block() {
    let model = CoupledTargetedInitialProjectionModel;
    let mut y = vec![0.0, 0.0];

    project_initial_variables_with_plan(
        &model,
        &mut y,
        &[],
        0.0,
        model.algebraic_projection_plan(),
        1.0e-12,
    )
    .expect("coupled targeted block should use the coupled solve, not greedy row relaxation");

    assert!((y[0] - 0.5).abs() < 1.0e-9);
    assert!((y[1] - 0.5).abs() < 1.0e-9);
}

#[test]
fn project_initial_variables_uses_compiler_plan_indices() {
    let model = CoupledTargetedInitialProjectionModel;
    let mut y = vec![0.0, 0.0];

    project_initial_variables_with_plan(
        &model,
        &mut y,
        &[],
        0.0,
        model.algebraic_projection_plan(),
        1.0e-12,
    )
    .expect("non-empty plan should run even without projection indices");

    assert!((y[0] - 0.5).abs() < 1.0e-9);
    assert!((y[1] - 0.5).abs() < 1.0e-9);
}

#[test]
fn project_initial_variables_rejects_plan_rows_outside_residual_vector() {
    let model = CoupledTargetedInitialProjectionModel;
    let plan = solve::AlgebraicProjectionPlan {
        blocks: vec![solve::AlgebraicProjectionBlock {
            rows: vec![2],
            y_indices: vec![0],
        }],
    };
    let mut y = vec![0.0, 0.0];

    let err = project_initial_variables_with_plan(&model, &mut y, &[], 0.0, &plan, 1.0e-12)
        .expect_err("invalid plan row must not default to zero residual");

    assert!(err.to_string().contains("residual row 2 is outside 0..2"));
}
