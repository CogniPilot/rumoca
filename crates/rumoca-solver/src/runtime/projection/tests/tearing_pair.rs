use super::super::*;
use std::cell::{Cell, RefCell};

struct PairModel {
    plan: solve::AlgebraicProjectionPlan,
    entries: RefCell<Vec<(usize, Vec<f64>)>>,
    fail: Cell<bool>,
    decline_primary: Cell<bool>,
}
impl PairModel {
    fn new() -> Self {
        Self {
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![0, 1, 2],
                    y_indices: vec![0, 1, 2],
                    alternate_charts: vec![],
                    guarded_tearing: Some(solve::BlockTearing {
                        tear_y_indices: vec![2],
                        residual_rows: vec![2],
                        causal_steps: vec![
                            solve::CausalStep { row: 0, y_index: 0 },
                            solve::CausalStep { row: 1, y_index: 1 },
                        ],
                    }),
                    tearing: Some(solve::BlockTearing {
                        tear_y_indices: vec![1],
                        residual_rows: vec![1],
                        causal_steps: vec![
                            solve::CausalStep { row: 2, y_index: 0 },
                            solve::CausalStep { row: 0, y_index: 2 },
                        ],
                    }),
                }],
            },
            entries: RefCell::new(Vec::new()),
            fail: Cell::new(false),
            decline_primary: Cell::new(false),
        }
    }
}
impl ImplicitProjectionModel for PairModel {
    fn eval_residual(
        &self,
        y: &[f64],
        p: &[f64],
        _: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] - y[2];
        out[1] = p[0] * y[1] + y[2] - 2.0;
        out[2] = y[0] + y[1] * y[1] - 6.0;
        Ok(())
    }
    fn eval_implicit_residual_row(
        &self,
        row: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let mut out = [0.0; 3];
        self.eval_residual(y, p, t, &mut out)?;
        Ok(out.get(row).copied())
    }
    fn eval_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        _: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = v[0] - v[2];
        out[1] = p[0] * v[1] + v[2];
        out[2] = v[0] + 2.0 * y[1] * v[1];
        Ok(())
    }
    fn eval_implicit_target_value(
        &self,
        row: usize,
        target: usize,
        y: &[f64],
        p: &[f64],
        _: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(match (row, target) {
            (0, 0) => Some(y[2]),
            (0, 2) => Some(y[0]),
            (1, 1) => (p[0] != 0.0).then(|| (2.0 - y[2]) / p[0]),
            (2, 0) => Some(6.0 - y[1] * y[1]),
            _ => None,
        })
    }
    fn implicit_target_assignment_is_exact(&self, row: usize, target: usize) -> bool {
        matches!((row, target), (0, 0) | (0, 2) | (1, 1) | (2, 0))
    }
    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row))
    }
    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }
    fn target_name_for_row(&self, _: usize) -> Option<&str> {
        None
    }
    fn torn_block_sweep(
        &self,
        plan: &solve::BlockTearing,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        self.entries
            .borrow_mut()
            .push((plan.tear_y_indices[0], y.to_vec()));
        if self.fail.get() {
            y[0] = -777.0;
            return Err(RuntimeSolveError::solve_ir("native execution witness"));
        }
        if self.decline_primary.get() && plan.tear_y_indices == [1] {
            y[0] = -888.0;
            return Ok(false);
        }
        tearing::per_row_torn_block_sweep(self, plan, y, p, t, out)
    }
}

#[test]
fn guarded_nonlinear_decline_restores_all_writes_before_primary_and_untorn() {
    for decline_primary in [false, true] {
        let model = PairModel::new();
        model.decline_primary.set(decline_primary);
        let original = [9.0, 3.0, 0.0, 123.0];
        let mut y = original;
        project_algebraics_with_plan_certified(
            &model,
            &model.plan,
            &mut y,
            AlgebraicProjectionArgs {
                parameters: &[0.0],
                time: 0.0,
                state_count: 0,
                tolerance: 1e-10,
            },
            32,
        )
        .unwrap();
        let entries = model.entries.borrow();
        assert_eq!(
            entries.iter().find(|(kind, _)| *kind == 1).unwrap().1,
            original
        );
        assert_eq!(y[3], 123.0);
        let mut residual = [0.0; 3];
        model.eval_residual(&y, &[0.0], 0.0, &mut residual).unwrap();
        assert!(
            residual.iter().all(|v| v.abs() < 1e-9),
            "{y:?} {residual:?}"
        );
    }
}

#[test]
fn guarded_nonlinear_error_restores_snapshot_and_never_attempts_primary() {
    let model = PairModel::new();
    model.fail.set(true);
    let original = [9.0, 3.0, 0.0, 123.0];
    let mut y = original;
    let result = project_algebraics_with_plan_certified(
        &model,
        &model.plan,
        &mut y,
        AlgebraicProjectionArgs {
            parameters: &[0.0],
            time: 0.0,
            state_count: 0,
            tolerance: 1e-10,
        },
        32,
    );
    assert!(
        result
            .unwrap_err()
            .to_string()
            .contains("native execution witness")
    );
    assert_eq!(y, original);
    assert_eq!(model.entries.borrow().len(), 1);
}
