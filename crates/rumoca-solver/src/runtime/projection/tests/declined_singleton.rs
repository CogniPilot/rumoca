//! A one-row block whose singleton assignment declines after evaluating its
//! row hands that residual to the dense path, which does not evaluate the
//! row again at the same coordinates.

use std::cell::RefCell;

use super::*;

/// `y + 0.3 sin(y) = 1`, with no isolated assignment, recording the
/// coordinate of every row evaluation.
struct UnisolatedSingletonModel {
    evaluations: RefCell<Vec<f64>>,
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for UnisolatedSingletonModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] + 0.3 * y[0].sin() - 1.0;
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
        out[0] = (1.0 + 0.3 * y[0].cos()) * v[0];
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row_idx: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        assert_eq!(row_idx, 0);
        self.evaluations.borrow_mut().push(y[0]);
        Ok(Some(y[0] + 0.3 * y[0].sin() - 1.0))
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }
}

#[test]
fn a_declined_singleton_hands_its_residual_to_the_dense_path() {
    let model = UnisolatedSingletonModel {
        evaluations: RefCell::new(Vec::new()),
        plan: solve::AlgebraicProjectionPlan {
            blocks: (vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
                tearing: None,
                alternate_charts: Vec::new(),
            }])
            .into(),
        },
    };
    let mut y = vec![0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-12)
        .expect("the dense path should solve the unisolated singleton");

    let mut residual = [f64::NAN];
    model
        .eval_residual(&y, &[], 0.0, &mut residual)
        .expect("the residual evaluates at the root");
    assert!(residual[0].abs() <= 1.0e-12, "{residual:?}");
    let evaluations = model.evaluations.borrow();
    assert_eq!(
        evaluations[0], 0.0,
        "the singleton assignment evaluates the row at the start point"
    );
    assert!(
        evaluations[1] != evaluations[0],
        "the dense path reuses that residual instead of evaluating the row again \
         at the same point: {evaluations:?}"
    );
}
