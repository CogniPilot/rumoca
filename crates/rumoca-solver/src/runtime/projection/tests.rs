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

struct RowSelectiveBlockProjectionModel {
    full_jacobian_calls: Cell<usize>,
    residual_row_calls: Cell<usize>,
    jacobian_row_calls: Cell<usize>,
    plan: solve::AlgebraicProjectionPlan,
}

struct SparseRowSelectiveProjectionModel {
    full_jacobian_calls: Cell<usize>,
    jacobian_row_calls: Cell<usize>,
    plan: solve::AlgebraicProjectionPlan,
}

struct ReverseRowProjectionModel {
    forward_jvp_calls: Cell<usize>,
    reverse_row_calls: Cell<usize>,
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for ReverseRowProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] + y[1] - 5.0;
        out[1] = y[0] - y[1] + 1.0;
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
        self.forward_jvp_calls.set(self.forward_jvp_calls.get() + 1);
        out[0] = v[0] + v[1];
        out[1] = v[0] - v[1];
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row_idx: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(Some(match row_idx {
            0 => y[0] + y[1] - 5.0,
            1 => y[0] - y[1] + 1.0,
            _ => return Ok(None),
        }))
    }

    fn eval_implicit_jacobian_row(
        &self,
        row_idx: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        gradient: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        self.reverse_row_calls.set(self.reverse_row_calls.get() + 1);
        gradient.fill(0.0);
        gradient[0] = 1.0;
        gradient[1] = if row_idx == 0 { 1.0 } else { -1.0 };
        Ok(row_idx < 2)
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

impl ImplicitProjectionModel for SparseRowSelectiveProjectionModel {
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
        self.full_jacobian_calls
            .set(self.full_jacobian_calls.get() + 1);
        out.copy_from_slice(v);
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row_idx: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(Some(y[row_idx] - [2.0, 3.0][row_idx]))
    }

    fn eval_implicit_jacobian_v_row(
        &self,
        row_idx: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.jacobian_row_calls
            .set(self.jacobian_row_calls.get() + 1);
        Ok(Some(v[row_idx]))
    }

    fn implicit_jacobian_v_row_depends_on(&self, row_idx: usize, seed_index: usize) -> bool {
        row_idx == seed_index
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

impl ImplicitProjectionModel for RowSelectiveBlockProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] + y[1] - 5.0;
        out[1] = y[0] - y[1] + 1.0;
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
        self.full_jacobian_calls
            .set(self.full_jacobian_calls.get() + 1);
        out[0] = v[0] + v[1];
        out[1] = v[0] - v[1];
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row_idx: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.residual_row_calls
            .set(self.residual_row_calls.get() + 1);
        Ok(Some(match row_idx {
            0 => y[0] + y[1] - 5.0,
            1 => y[0] - y[1] + 1.0,
            _ => return Ok(None),
        }))
    }

    fn eval_implicit_jacobian_v_row(
        &self,
        row_idx: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.jacobian_row_calls
            .set(self.jacobian_row_calls.get() + 1);
        Ok(Some(match row_idx {
            0 => v[0] + v[1],
            1 => v[0] - v[1],
            _ => return Ok(None),
        }))
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

#[test]
fn coupled_projection_uses_selected_residual_and_jacobian_rows() {
    let model = RowSelectiveBlockProjectionModel {
        full_jacobian_calls: Cell::new(0),
        residual_row_calls: Cell::new(0),
        jacobian_row_calls: Cell::new(0),
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![0, 1],
                tearing: None,
            }],
        },
    };
    let mut y = vec![0.0, 0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-12)
        .expect("coupled projection should converge");

    assert!((y[0] - 2.0).abs() <= f64::EPSILON);
    assert!((y[1] - 3.0).abs() <= f64::EPSILON);
    assert!(model.residual_row_calls.get() > 0);
    assert_eq!(model.jacobian_row_calls.get(), 4);
    assert_eq!(model.full_jacobian_calls.get(), 0);
}

#[test]
fn coupled_projection_skips_structurally_zero_jacobian_entries() {
    let model = SparseRowSelectiveProjectionModel {
        full_jacobian_calls: Cell::new(0),
        jacobian_row_calls: Cell::new(0),
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![0, 1],
                tearing: None,
            }],
        },
    };
    let mut y = vec![0.0, 0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-12)
        .expect("sparse coupled projection should converge");

    assert_eq!(y, vec![2.0, 3.0]);
    assert_eq!(model.jacobian_row_calls.get(), 2);
    assert_eq!(model.full_jacobian_calls.get(), 0);
}

#[test]
fn coupled_projection_sensitivity_uses_selected_jacobian_rows() {
    let model = RowSelectiveBlockProjectionModel {
        full_jacobian_calls: Cell::new(0),
        residual_row_calls: Cell::new(0),
        jacobian_row_calls: Cell::new(0),
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![0, 1],
                tearing: None,
            }],
        },
    };
    let y = vec![2.0, 3.0];
    let mut seed = vec![0.0; 2];
    let mut unit_seed = vec![0.0; 2];

    project_algebraic_seed_with_plan(
        &model,
        &model.plan,
        &y,
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 0,
            tolerance: 1.0e-12,
        },
        &mut seed,
        &mut unit_seed,
    )
    .expect("coupled sensitivity projection should evaluate selected scalar rows");

    assert_eq!(seed, vec![0.0, 0.0]);
    assert_eq!(model.full_jacobian_calls.get(), 0);
    assert_eq!(model.jacobian_row_calls.get(), 8);
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
                tearing: None,
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
                    tearing: None,
                },
                solve::AlgebraicProjectionBlock {
                    rows: vec![1],
                    y_indices: vec![1],
                    tearing: None,
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
                tearing: None,
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
                tearing: None,
            }],
        },
    };
    let mut y = vec![0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-12)
        .expect("singleton assignment should project");

    assert_eq!(y, vec![5.0]);
    assert_eq!(model.residual_calls.get(), 0);
    // One residual checks the current value and one certifies the assigned
    // value. The block-local settled result avoids duplicate whole-plan sweeps.
    assert_eq!(model.residual_row_calls.get(), 2);
    assert_eq!(model.jacobian_calls.get(), 0);
}

#[test]
fn coupled_projection_prefers_complete_reverse_row_gradients() {
    let model = ReverseRowProjectionModel {
        forward_jvp_calls: Cell::new(0),
        reverse_row_calls: Cell::new(0),
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![0, 1],
                tearing: None,
            }],
        },
    };
    let mut y = vec![0.0, 0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-12)
        .expect("reverse-row Jacobian should solve the coupled block");

    assert!((y[0] - 2.0).abs() <= f64::EPSILON);
    assert!((y[1] - 3.0).abs() <= f64::EPSILON);
    assert_eq!(model.reverse_row_calls.get(), 2);
    assert_eq!(model.forward_jvp_calls.get(), 0);
}

#[test]
fn partial_projection_ignores_unselected_residuals_and_unknowns() {
    let model = BlockProjectionModel {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
                tearing: None,
            }],
        },
        initial_residual_len: 0,
    };
    let mut y = vec![0.0, 999.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-12)
        .expect("a dependency projection should require only its selected residual rows");

    assert!((y[0] - 2.0).abs() <= f64::EPSILON);
    assert_eq!(y[1], 999.0);
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
                tearing: None,
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
                tearing: None,
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
                tearing: None,
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
fn project_algebraic_block_rejects_rectangular_inventory() {
    let model = BlockProjectionModel {
        plan: solve::AlgebraicProjectionPlan::default(),
        initial_residual_len: 0,
    };
    let block = solve::AlgebraicProjectionBlock {
        rows: vec![0],
        y_indices: vec![0, 1],
        tearing: None,
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
        tearing: None,
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
                tearing: None,
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
        tearing: None,
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
        tearing: None,
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
            tearing: None,
        }],
    };
    let mut y = vec![0.0, 0.0];

    let err = project_initial_variables_with_plan(&model, &mut y, &[], 0.0, &plan, 1.0e-12)
        .expect_err("invalid plan row must not default to zero residual");

    assert!(err.to_string().contains("residual row 2 is outside 0..2"));
}

struct TornProjectionModel {
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for TornProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] - y[1].powi(2);
        out[1] = y[0] + y[1] - 6.0;
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
        out[0] = v[0] - 2.0 * y[1] * v[1];
        out[1] = v[0] + v[1];
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let mut out = [0.0; 2];
        self.eval_residual(y, p, t, &mut out)?;
        Ok(out.get(row_idx).copied())
    }

    fn eval_implicit_jacobian_v_row(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let mut out = [0.0; 2];
        self.eval_jacobian_v(y, p, t, v, &mut out)?;
        Ok(out.get(row_idx).copied())
    }

    fn eval_implicit_target_value(
        &self,
        _row_idx: usize,
        _target_y_index: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(None)
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        (row_idx == 0).then(|| solve::scalar_slot_y(0))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        None
    }
}

#[test]
fn torn_projection_uses_total_reduced_jacobian() {
    let model = TornProjectionModel {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![0, 1],
                tearing: Some(solve::AlgebraicTearingPlan {
                    residual_rows: vec![1],
                    tear_y_indices: vec![1],
                    causal_steps: vec![solve::AlgebraicCausalStep {
                        row: 0,
                        target_y_index: 0,
                        target_residual_coefficient: Some(1.0),
                    }],
                }),
            }],
        },
    };
    let mut y = vec![1.0, 1.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-12)
        .expect("reduced projection should converge");

    assert!((y[0] - 4.0).abs() <= 1.0e-10);
    assert!((y[1] - 2.0).abs() <= 1.0e-10);
}

struct TornSensitivityModel {
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for TornSensitivityModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[1] = y[1] - y[0] * y[2];
        out[2] = y[1] + y[2] - 3.0;
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
        out[1] = v[1] - y[2] * v[0] - y[0] * v[2];
        out[2] = v[1] + v[2];
        Ok(())
    }

    fn eval_implicit_jacobian_v_row(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let mut out = [0.0; 3];
        self.eval_jacobian_v(y, p, t, v, &mut out)?;
        Ok(out.get(row_idx).copied())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        (row_idx == 1).then(|| solve::scalar_slot_y(1))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        None
    }
}

#[test]
fn torn_projection_propagates_exact_reduced_sensitivity() {
    let model = TornSensitivityModel {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![1, 2],
                y_indices: vec![1, 2],
                tearing: Some(solve::AlgebraicTearingPlan {
                    residual_rows: vec![2],
                    tear_y_indices: vec![2],
                    causal_steps: vec![solve::AlgebraicCausalStep {
                        row: 1,
                        target_y_index: 1,
                        target_residual_coefficient: Some(1.0),
                    }],
                }),
            }],
        },
    };
    let y = vec![2.0, 2.0, 1.0];
    let mut seed = vec![1.0, 0.0, 0.0];
    let mut unit_seed = vec![0.0; 3];

    project_algebraic_seed_with_plan(
        &model,
        &model.plan,
        &y,
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 1,
            tolerance: 1.0e-12,
        },
        &mut seed,
        &mut unit_seed,
    )
    .expect("reduced sensitivity should solve");

    assert!((seed[1] - 1.0 / 3.0).abs() <= 1.0e-12);
    assert!((seed[2] + 1.0 / 3.0).abs() <= 1.0e-12);
}
