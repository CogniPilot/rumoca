//! Projection behavior when a certified isolator is singular at the iterate.
//!
//! The `Modelica.Electrical.Analog.Examples.OpAmps.DifferentialAmplifier`
//! opamp loop carries an exact isolation whose coefficient evaluates to `-0`
//! at the start state. That singularity is a property of the iterate, not of
//! the model: the block is still solvable by residual iteration or by the
//! dense block Newton. The projection must therefore decline the isolation
//! and fall back rather than abort with `RefreshTargetSingular`. These tests
//! pin that contract at every projection consumer of `isolation_value` and at
//! the provided trait method itself.

use std::cell::Cell;

use super::super::*;

fn singular_isolator_error(row: usize, target_y_index: usize) -> RuntimeSolveError {
    RuntimeSolveError::RefreshTargetSingular {
        row,
        target_y_index,
        // The differential-amplifier coefficient is exactly this negative zero.
        coefficient: -0.0,
        span: None,
    }
}

/// A two-unknown coupled block torn on `y[0]` whose lone causal step
/// `y[1] = 2*y[0]` is certified exact but whose isolator is singular at every
/// iterate. Back-substitution can never proceed, so the torn solve must
/// decline and leave the block to the dense fallback, which solves the linear
/// system `y[0] + y[1] = 3`, `y[1] = 2*y[0]` at `(1, 2)`.
struct TornSingularIsolatorModel {
    singular_isolator_calls: Cell<usize>,
    plan: solve::AlgebraicProjectionPlan,
}

impl TornSingularIsolatorModel {
    fn new() -> Self {
        Self {
            singular_isolator_calls: Cell::new(0),
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![0, 1],
                    y_indices: vec![0, 1],
                    tearing: Some(solve::BlockTearing {
                        tear_y_indices: vec![0],
                        residual_rows: vec![0],
                        causal_steps: vec![solve::CausalStep { row: 1, y_index: 1 }],
                    }),
                }],
            },
        }
    }
}

impl ImplicitProjectionModel for TornSingularIsolatorModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] + y[1] - 3.0;
        out[1] = y[1] - 2.0 * y[0];
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
        out[1] = v[1] - 2.0 * v[0];
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
            0 => Some(y[0] + y[1] - 3.0),
            1 => Some(y[1] - 2.0 * y[0]),
            _ => None,
        })
    }

    fn implicit_target_assignment_is_exact(&self, row_idx: usize, target_y_index: usize) -> bool {
        row_idx == 1 && target_y_index == 1
    }

    fn eval_implicit_target_value(
        &self,
        row_idx: usize,
        target_y_index: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        if row_idx == 1 && target_y_index == 1 {
            self.singular_isolator_calls
                .set(self.singular_isolator_calls.get() + 1);
            return Err(singular_isolator_error(1, 1));
        }
        Ok(None)
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        match row_idx {
            0 => Some("loop.split"),
            1 => Some("loop.gain"),
            _ => None,
        }
    }
}

/// A single-row block whose certified exact assignment for `y[0]` has a
/// singular isolator at every iterate. The exact-assignment fast path must
/// report the block unsettled instead of aborting, leaving the outer sweep's
/// residual check to accept the already consistent value.
struct SingletonSingularIsolatorModel {
    singular_isolator_calls: Cell<usize>,
    plan: solve::AlgebraicProjectionPlan,
}

impl SingletonSingularIsolatorModel {
    fn new() -> Self {
        Self {
            singular_isolator_calls: Cell::new(0),
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                    tearing: None,
                }],
            },
        }
    }
}

impl ImplicitProjectionModel for SingletonSingularIsolatorModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] - 1.0;
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

    fn eval_implicit_residual_row(
        &self,
        _row_idx: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(Some(y[0] - 1.0))
    }

    fn implicit_target_assignment_is_exact(&self, row_idx: usize, target_y_index: usize) -> bool {
        row_idx == 0 && target_y_index == 0
    }

    fn eval_implicit_target_value(
        &self,
        _row_idx: usize,
        _target_y_index: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.singular_isolator_calls
            .set(self.singular_isolator_calls.get() + 1);
        Err(singular_isolator_error(0, 0))
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

/// What the isolator probe reports from `eval_implicit_target_value`, so the
/// provided `isolation_value` mapping can be tested case by case.
enum IsolatorOutcome {
    Singular,
    Value(f64),
    EvaluationFailure,
}

struct IsolatorProbeModel {
    outcome: IsolatorOutcome,
    plan: solve::AlgebraicProjectionPlan,
}

impl IsolatorProbeModel {
    fn new(outcome: IsolatorOutcome) -> Self {
        Self {
            outcome,
            plan: solve::AlgebraicProjectionPlan { blocks: vec![] },
        }
    }
}

impl ImplicitProjectionModel for IsolatorProbeModel {
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

    fn eval_implicit_target_value(
        &self,
        row_idx: usize,
        target_y_index: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        match self.outcome {
            IsolatorOutcome::Singular => Err(singular_isolator_error(row_idx, target_y_index)),
            IsolatorOutcome::Value(value) => Ok(Some(value)),
            IsolatorOutcome::EvaluationFailure => {
                Err(RuntimeSolveError::solve_ir("isolator evaluation failed"))
            }
        }
    }

    fn implicit_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
        None
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        None
    }
}

fn projection_args<'a>() -> AlgebraicProjectionArgs<'a> {
    AlgebraicProjectionArgs {
        parameters: &[],
        time: 0.0,
        state_count: 0,
        tolerance: 1.0e-9,
    }
}

/// A torn block whose causal isolator is singular at the iterate must still be
/// solved: the torn solve declines and the dense block Newton finishes.
#[test]
fn torn_block_with_singular_causal_isolator_falls_back_to_dense_solve() {
    let model = TornSingularIsolatorModel::new();
    let mut y = vec![0.0, 0.0];
    project_algebraics_with_plan(
        &model,
        model.algebraic_projection_plan(),
        &mut y,
        projection_args(),
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
    .expect("a singular causal isolator must decline to the dense solve, not abort");
    assert!(
        model.singular_isolator_calls.get() > 0,
        "the torn solve never consulted the singular isolator, so this test \
         no longer exercises the decline path"
    );
    assert!(
        (y[0] - 1.0).abs() <= 1.0e-6 && (y[1] - 2.0).abs() <= 1.0e-6,
        "the dense fallback should solve the block at (1, 2); got ({}, {})",
        y[0],
        y[1]
    );
}

/// An exact singleton assignment whose isolator is singular must leave the
/// block unsettled rather than abort; the outer residual check then accepts
/// the already consistent iterate.
#[test]
fn singleton_exact_assignment_with_singular_isolator_declines_without_error() {
    let model = SingletonSingularIsolatorModel::new();
    let mut y = vec![1.0];
    project_algebraics_with_plan(
        &model,
        model.algebraic_projection_plan(),
        &mut y,
        projection_args(),
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
    .expect("a singular exact-assignment isolator must not abort the projection");
    assert!(
        model.singular_isolator_calls.get() > 0,
        "the singleton path never consulted the singular isolator, so this \
         test no longer exercises the decline path"
    );
    assert_eq!(
        y,
        vec![1.0],
        "declining the isolation must leave the consistent iterate untouched"
    );
}

#[test]
fn isolation_value_maps_a_singular_isolator_to_no_isolation() {
    let model = IsolatorProbeModel::new(IsolatorOutcome::Singular);
    let value = model
        .isolation_value(3, 49, &[0.0], &[], 0.0)
        .expect("a singular isolator is unavailability at this iterate, not a failure");
    assert_eq!(value, None);
}

#[test]
fn isolation_value_filters_non_finite_isolations() {
    for non_finite in [f64::INFINITY, f64::NEG_INFINITY, f64::NAN] {
        let model = IsolatorProbeModel::new(IsolatorOutcome::Value(non_finite));
        let value = model
            .isolation_value(0, 0, &[0.0], &[], 0.0)
            .expect("a non-finite isolation is unavailability, not a failure");
        assert_eq!(value, None, "isolation {non_finite} must be filtered");
    }
    // A finite isolation passes through untouched.
    let model = IsolatorProbeModel::new(IsolatorOutcome::Value(2.5));
    let value = model
        .isolation_value(0, 0, &[0.0], &[], 0.0)
        .expect("a finite isolation evaluates cleanly");
    assert_eq!(value, Some(2.5));
}

#[test]
fn isolation_value_propagates_genuine_evaluation_failures() {
    let model = IsolatorProbeModel::new(IsolatorOutcome::EvaluationFailure);
    let error = model
        .isolation_value(0, 0, &[0.0], &[], 0.0)
        .expect_err("only the singular coefficient maps to unavailability");
    assert!(
        matches!(error, RuntimeSolveError::SolveIr { .. }),
        "the evaluation failure must survive the mapping; got {error}"
    );
}
