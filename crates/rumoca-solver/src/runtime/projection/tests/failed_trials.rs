//! A block path that writes a trial value into `y` and then fails to evaluate
//! it returns the error with `y` as the path found it.

use super::*;

/// Rows `y[i] - 5` whose evaluation fails once `y[0]` reaches the trial value
/// 5; row 0 is not finite at 1 when `nan_at_start` is set, which sends the
/// dense path through its seed.
struct TrialFault {
    plan: solve::AlgebraicProjectionPlan,
    isolates: bool,
    nan_at_start: bool,
}

impl TrialFault {
    fn new(
        n: usize,
        tearing: Option<solve::BlockTearing>,
        isolates: bool,
        nan_at_start: bool,
    ) -> Self {
        Self {
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![block(n, tearing)].into(),
            },
            isolates,
            nan_at_start,
        }
    }

    fn row(&self, row: usize, y: &[f64]) -> Result<f64, RuntimeSolveError> {
        if y[0] == 5.0 {
            return Err(RuntimeSolveError::solve_ir(
                "row evaluation fails at the trial",
            ));
        }
        if row == 0 && self.nan_at_start && y[0] == 1.0 {
            return Ok(f64::NAN);
        }
        Ok(y[row] - 5.0)
    }
}

fn block(n: usize, tearing: Option<solve::BlockTearing>) -> solve::AlgebraicProjectionBlock {
    solve::AlgebraicProjectionBlock {
        rows: (0..n).collect(),
        y_indices: (0..n).collect(),
        tearing,
        alternate_charts: Vec::new(),
    }
}

impl ImplicitProjectionModel for TrialFault {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for (row, value) in out.iter_mut().enumerate().take(y.len()) {
            *value = self.row(row, y)?;
        }
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.row(row, y).map(Some)
    }

    fn eval_jacobian_v(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out.copy_from_slice(&v[..out.len()]);
        Ok(())
    }

    fn eval_implicit_target_value(
        &self,
        row: usize,
        target: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok((self.isolates && row == target).then_some(5.0))
    }

    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }
}

/// Project block 0 of `model` from `incoming`: the error, and `y` after it.
fn project(model: &TrialFault, incoming: &[f64]) -> (RuntimeSolveError, Vec<f64>) {
    let mut y = incoming.to_vec();
    let block = &model.algebraic_projection_plan().blocks.scalar_blocks()[0];
    let error = project_algebraic_block(
        model,
        &mut y,
        &[],
        0.0,
        block,
        0,
        AlgebraicBlockProjectionPolicy {
            tolerance: 1.0e-12,
            step_limit: StepLimit::None,
            certify_coordinates: false,
            owner: None,
        },
    )
    .expect_err("the trial evaluation fails");
    (error, y)
}

fn assert_restored(model: &TrialFault, incoming: &[f64]) {
    let (error, y) = project(model, incoming);
    assert!(error.to_string().contains("fails at the trial"), "{error}");
    let bits = |values: &[f64]| {
        values
            .iter()
            .map(|value| value.to_bits())
            .collect::<Vec<_>>()
    };
    assert_eq!(bits(&y), bits(incoming));
    let mut residual = vec![f64::NAN; y.len()];
    model
        .eval_residual(&y, &[], 0.0, &mut residual)
        .expect("the restored point evaluates again");
}

/// The singleton assignment writes the isolated value and evaluates its row.
#[test]
fn a_failed_singleton_trial_restores_its_coordinate() {
    assert_restored(&TrialFault::new(1, None, true, false), &[1.0]);
}

/// A non-finite row seeds its target from the isolated value first.
#[test]
fn a_failed_seed_trial_restores_its_coordinate() {
    assert_restored(&TrialFault::new(2, None, true, true), &[1.0, 0.0]);
}

/// The dense Newton step lands on the failing point.
#[test]
fn a_failed_dense_step_restores_its_coordinate() {
    assert_restored(&TrialFault::new(2, None, false, false), &[1.0, 0.0]);
}

/// The torn reduced Newton step over the single tear lands on it.
#[test]
fn a_failed_torn_step_restores_its_coordinate() {
    let tearing = solve::BlockTearing {
        tear_y_indices: vec![0],
        residual_rows: vec![0],
        causal_steps: Vec::new(),
    };
    assert_restored(&TrialFault::new(1, Some(tearing), false, false), &[1.0]);
}
