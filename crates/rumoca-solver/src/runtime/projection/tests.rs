use std::cell::Cell;

use super::*;

fn project_initial_y_plan<M: AlgebraicProjectionModel>(
    model: &M,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    plan: &solve::AlgebraicProjectionPlan,
    tol: f64,
) -> Result<(), RuntimeSolveError> {
    let plan = solve::InitializationProjectionPlan {
        blocks: plan
            .blocks
            .iter()
            .map(|block| solve::InitializationProjectionBlock {
                rows: block.rows.clone(),
                unknowns: block
                    .y_indices
                    .iter()
                    .copied()
                    .map(solve::scalar_slot_y)
                    .collect(),
            })
            .collect(),
    };
    let mut params = p.to_vec();
    project_initial_variables_with_plan(model, y, &mut params, t, &plan, tol)
}

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

struct ScaledContinuousAssignmentModel {
    coefficient: f64,
    rhs: f64,
    target_value: f64,
    variable_scale: f64,
    plan: solve::AlgebraicProjectionPlan,
}

struct ReverseOrderedResistorModel {
    voltage: f64,
    resistance: f64,
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for ReverseOrderedResistorModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[1] - self.resistance * y[0];
        out[1] = y[1] - self.voltage;
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
        out[0] = v[1] - self.resistance * v[0];
        out[1] = v[1];
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row_idx: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(match row_idx {
            0 => Some(y[1] - self.resistance * y[0]),
            1 => Some(y[1] - self.voltage),
            _ => None,
        })
    }

    fn eval_implicit_target_value(
        &self,
        row_idx: usize,
        target_y_index: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(match (row_idx, target_y_index) {
            (0, 0) => Some(y[1] / self.resistance),
            (1, 1) => Some(self.voltage),
            _ => None,
        })
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        match row_idx {
            0 => Some("current"),
            1 => Some("voltage"),
            _ => None,
        }
    }
}

impl ImplicitProjectionModel for ScaledContinuousAssignmentModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = self.coefficient * y[0] - self.rhs;
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
        out[0] = self.coefficient * v[0];
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        _row_idx: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(Some(self.coefficient * y[0] - self.rhs))
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
        Some("current")
    }

    fn variable_scale_for_y_index(&self, _y_index: usize) -> f64 {
        self.variable_scale
    }
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
    assert_eq!(model.residual_calls.get(), 0);
    // One residual checks the current value and one certifies the assigned
    // value. A one-block plan has no later producer that can invalidate it.
    assert_eq!(model.residual_row_calls.get(), 2);
    assert_eq!(model.jacobian_calls.get(), 0);
}

#[test]
fn resistor_assignment_reports_sub_tolerance_current_as_semantic_progress() {
    let voltage = 9.332_043_908_269_583e-3;
    let resistance = 10_000.0;
    let model = ScaledContinuousAssignmentModel {
        coefficient: resistance,
        rhs: voltage,
        target_value: voltage / resistance,
        variable_scale: 1.0,
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
    };
    let mut current = vec![0.0];
    let update = project_algebraic_singleton_assignment(
        &model,
        &mut current,
        &[],
        0.0,
        &model.plan.blocks[0],
        1.0e-6,
    )
    .expect("resistor assignment should evaluate")
    .expect("resistor row has an isolated current assignment");

    assert_eq!(current[0], voltage / resistance);
    assert!(
        update.changed,
        "an exact write must drive a dependent sweep even below variable tolerance"
    );
    assert!(update.settled);
}

#[test]
fn reverse_ordered_resistor_blocks_revisit_a_locally_settled_current() {
    let voltage = 9.332_043_908_269_583e-3;
    let resistance = 10_000.0;
    let model = ReverseOrderedResistorModel {
        voltage,
        resistance,
        plan: solve::AlgebraicProjectionPlan {
            // The current row is intentionally visited before its voltage
            // producer, matching a valid non-causal BLT fallback order.
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
    };
    let mut y = vec![0.0, 0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-6)
        .expect("the complete reverse-ordered plan should reach a fixed point");

    assert_eq!(y[1], voltage);
    assert_eq!(y[0], voltage / resistance);
    assert!((y[1] - resistance * y[0]).abs() <= f64::EPSILON);
}

#[test]
fn algebraic_seed_certifies_residual_in_row_units() {
    let model = ScaledContinuousAssignmentModel {
        coefficient: 1.0e-3,
        rhs: 1.0e-3,
        target_value: 1.0 + 1.0e-5,
        variable_scale: 1.0,
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
    };
    let mut y = vec![0.0];
    let context = AlgebraicSeedContext {
        model: &model,
        parameters: &[],
        time: 0.0,
        y_indices: &[0],
        tolerance: 1.0e-6,
    };

    let seeded =
        try_seed_algebraic_target(&context, &mut y, 0, 0).expect("seed candidate should evaluate");

    assert_eq!(
        seeded, None,
        "an inexact row must not be certified in target units"
    );
    assert_eq!(
        y,
        vec![0.0],
        "a rejected seed restores its input coordinate"
    );
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

    project_initial_y_plan(&model, &mut y, &[], 0.0, &model.plan, 1.0e-12)
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

    let err = project_initial_y_plan(&model, &mut y, &[], 0.0, &model.plan, 1.0e-12)
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
    };
    let mut y = vec![0.0, 0.0];

    let err = project_algebraic_block(&model, &mut y, &[], 0.0, &block, 1.0e-12, StepLimit::None)
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

    let err = project_algebraic_block(&model, &mut y, &[], 0.0, &block, 1.0e-12, StepLimit::None)
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

    project_initial_y_plan(
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

    fn variable_scale_for_y_index(&self, _y_index: usize) -> f64 {
        1.0e-9
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

    project_initial_y_plan(
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

    project_initial_y_plan(
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

    let err = project_initial_y_plan(&model, &mut y, &[], 0.0, &plan, 1.0e-12)
        .expect_err("invalid plan row must not default to zero residual");

    assert!(err.to_string().contains("residual row 2 is outside 0..2"));
}

struct ParameterInitialProjectionModel {
    algebraic_plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for ParameterInitialProjectionModel {
    fn eval_residual(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        Ok(())
    }

    fn eval_jacobian_v(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _v: &[f64],
        _out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        Ok(())
    }

    fn implicit_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
        None
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.algebraic_plan
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        Some("p")
    }
}

impl AlgebraicProjectionModel for ParameterInitialProjectionModel {
    fn eval_initial_residual(
        &self,
        _y: &[f64],
        p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = p[0] * p[0] - 4.0;
        Ok(())
    }

    fn initial_residual_len(&self) -> usize {
        1
    }

    fn initial_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_p(0))
    }

    fn eval_initial_jacobian_v(
        &self,
        _y: &[f64],
        p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = 2.0 * p[0] * v[0];
        Ok(())
    }
}

#[test]
fn project_initial_variables_solves_fixed_false_parameter_unknown() {
    let model = ParameterInitialProjectionModel {
        algebraic_plan: solve::AlgebraicProjectionPlan::default(),
    };
    let plan = solve::InitializationProjectionPlan {
        blocks: vec![solve::InitializationProjectionBlock {
            rows: vec![0],
            unknowns: vec![solve::scalar_slot_p(0)],
        }],
    };
    let mut y = Vec::new();
    let mut p = vec![3.0];

    project_initial_variables_with_plan(&model, &mut y, &mut p, 0.0, &plan, 1.0e-12)
        .expect("nonlinear fixed=false parameter should project");

    assert!((p[0] - 2.0).abs() <= 1.0e-10);
}

struct NominalScaledProjectionModel {
    plan: solve::AlgebraicProjectionPlan,
    coefficient: f64,
    rhs: f64,
    variable_scale: f64,
}

impl ImplicitProjectionModel for NominalScaledProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = self.coefficient * y[0] - self.rhs;
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
        out[0] = self.coefficient * v[0];
        Ok(())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        Some("small")
    }

    fn variable_scale_for_y_index(&self, _y_index: usize) -> f64 {
        self.variable_scale
    }
}

#[test]
fn nominal_scaled_projection_corrects_small_physical_residual() {
    let model = NominalScaledProjectionModel {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
        coefficient: 1.0,
        rhs: 0.0,
        variable_scale: 1.0e-9,
    };
    let mut y = vec![5.0e-13];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-6)
        .expect("nominal-scaled tolerance should require the small residual correction");

    assert!(
        y[0].abs() <= 1.0e-15,
        "scaled residual was not corrected: {y:?}"
    );
}

#[test]
fn projection_row_scale_includes_jacobian_coefficient() {
    let model = NominalScaledProjectionModel {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
        coefficient: 1.0e-12,
        rhs: 1.0e-3,
        variable_scale: 1.0e12,
    };
    let mut y = vec![0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-6)
        .expect("Jacobian-scaled residual should not accept the unprojected initial value");

    assert!(
        (y[0] - 1.0e9).abs() <= 1.0e-6,
        "unexpected projection: {y:?}"
    );
}

#[test]
fn scaled_newton_system_normalizes_mixed_magnitude_columns() {
    let jacobian = DMatrix::from_diagonal(&DVector::from_vec(vec![1.0e-12, 1.0e12]));
    let delta = scaled_newton_delta(
        &jacobian,
        &[-1.0, -1.0],
        &[1.0, 1.0],
        &[1.0e12, 1.0e-12],
        1.0e-12,
    )
    .expect("scaled diagonal system should solve");

    assert!((delta[0] - 1.0e12).abs() <= 1.0e-4);
    assert!((delta[1] - 1.0e-12).abs() <= f64::EPSILON);
}

/// `eps = 100*Modelica.Constants.eps` feeding a guarded division, the shape
/// `Modelica.Magnetic.FluxTubes.Basic.ElectroMagneticConverter` uses for
/// `L_stat = noEvent(if abs(i) > eps then abs(Psi/i) else abs(Psi/eps))`.
///
/// Row 0 assigns the tiny constant; its residual at the `0.0` seed is already
/// far below any usable tolerance, so a "must strictly improve the residual"
/// rule rejects the assignment and leaves the divisor at zero. Row 1 then
/// evaluates `0/0`.
struct TinyConstantDivisorInitialModel {
    divisor: f64,
    plan: solve::AlgebraicProjectionPlan,
}

impl TinyConstantDivisorInitialModel {
    fn residual_row(&self, row_idx: usize, y: &[f64]) -> Option<f64> {
        match row_idx {
            0 => Some(y[0] - self.divisor),
            1 => Some(y[1] - 0.0 / y[0]),
            _ => None,
        }
    }
}

impl ImplicitProjectionModel for TinyConstantDivisorInitialModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for (row, slot) in out.iter_mut().enumerate() {
            *slot = self.residual_row(row, y).unwrap_or(0.0);
        }
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

impl AlgebraicProjectionModel for TinyConstantDivisorInitialModel {
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

    fn eval_initial_target_value(
        &self,
        row_idx: usize,
        _target_y_index: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(match row_idx {
            0 => Some(self.divisor),
            1 => Some(0.0 / y[0]),
            _ => None,
        })
    }

    fn eval_initial_residual_row(
        &self,
        row_idx: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(self.residual_row(row_idx, y))
    }
}

#[test]
fn initial_singleton_assignment_writes_sub_tolerance_constant_divisor() {
    let model = TinyConstantDivisorInitialModel {
        divisor: 2.220_446_049_250_313e-14,
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
    };
    let mut y = vec![0.0, 0.0];

    project_initial_y_plan(&model, &mut y, &[], 0.0, &model.plan, 1.0e-6)
        .expect("a sub-tolerance constant assignment must still be written");

    assert!(
        (y[0] - model.divisor).abs() <= f64::EPSILON * model.divisor,
        "divisor left at its seed: {y:?}"
    );
    assert!(
        y[1].is_finite(),
        "guarded quotient stayed non-finite: {y:?}"
    );
}

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

#[path = "tests/saturation.rs"]
mod saturation;
