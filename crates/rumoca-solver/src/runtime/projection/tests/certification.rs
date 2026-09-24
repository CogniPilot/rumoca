//! Accepted-state algebraic projection certificates.
//!
//! Split from `projection/tests.rs` under the SPEC_0021 file-size gate.

use super::super::*;
use std::cell::Cell;

struct IllConditionedCoupledProjectionModel;

impl ImplicitProjectionModel for IllConditionedCoupledProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] + y[1];
        out[1] = y[0] + (1.0 + 1.0e-4) * y[1];
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
        out[1] = v[0] + (1.0 + 1.0e-4) * v[1];
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
                guarded_tearing: None,
                alternate_charts: Vec::new(),
            }],
        })
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        None
    }
}

#[test]
fn certified_projection_rejects_small_residual_with_large_coordinate_error() {
    let model = IllConditionedCoupledProjectionModel;
    let mut y = vec![1.0e-3, -1.0e-3];

    project_algebraics_with_plan_certified(
        &model,
        model.algebraic_projection_plan(),
        &mut y,
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 0,
            tolerance: 1.0e-6,
        },
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
    .expect("accepted projections must certify coordinate convergence");

    assert!(
        y.iter().all(|value| value.abs() <= 1.0e-12),
        "small row residual concealed a large coordinate error: {y:?}"
    );
}

/// i = 0.5*v/R + drive, v = R*i; hence i = 2*drive, v = 2*R*drive.
struct AmplifiedCausalProjection {
    plan: solve::AlgebraicProjectionPlan,
}

impl ImplicitProjectionModel for AmplifiedCausalProjection {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[1] - 1e6 * y[0];
        out[1] = y[0] - 0.5e-6 * y[1] - 1e-11;
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let mut residual = [0.0; 2];
        self.eval_residual(y, p, t, &mut residual)?;
        Ok(residual.get(row).copied())
    }

    fn eval_jacobian_v(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = v[1] - 1e6 * v[0];
        out[1] = v[0] - 0.5e-6 * v[1];
        Ok(())
    }

    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        [1, 0].get(row).copied().map(solve::scalar_slot_y)
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, row: usize) -> Option<&str> {
        ["v", "i"].get(row).copied()
    }

    fn implicit_target_assignment_is_exact(&self, row: usize, target: usize) -> bool {
        (row, target) == (0, 1)
    }

    fn eval_implicit_target_value(
        &self,
        row: usize,
        target: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(((row, target) == (0, 1)).then(|| 1e6 * y[0]))
    }
}

#[test]
fn certified_torn_projection_checks_recovered_coordinates() {
    let tearing = solve::BlockTearing {
        tear_y_indices: vec![0],
        residual_rows: vec![1],
        causal_steps: vec![solve::CausalStep { row: 0, y_index: 1 }],
    };
    let model = AmplifiedCausalProjection {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1],
                y_indices: vec![0, 1],
                tearing: Some(tearing.clone()),
                guarded_tearing: None,
                alternate_charts: Vec::new(),
            }],
        },
    };
    let mut y = [0.0, 0.0];
    let update =
        tearing::project_torn_algebraic_block(&model, &mut y, &[], 0.0, &tearing, 1e-10, true)
            .unwrap()
            .expect("the affine torn block converges without a dense fallback");
    assert!(update.settled);
    assert!((y[0] - 2e-11).abs() < 1e-18, "current: {y:?}");
    assert!((y[1] - 2e-5).abs() < 1e-12, "voltage: {y:?}");
}

struct AmplifiedNonlinearRoot {
    target: f64,
    output_gain: f64,
    fail_after_sweeps: Cell<Option<usize>>,
    fallback_after_sweeps: Cell<Option<usize>>,
    fallback_rejections_remaining: Cell<Option<usize>>,
    sweep_count: Cell<usize>,
    plan: solve::AlgebraicProjectionPlan,
}

impl AmplifiedNonlinearRoot {
    fn new(target: f64, output_gain: f64) -> Self {
        Self {
            target,
            output_gain,
            fail_after_sweeps: Cell::new(None),
            fallback_after_sweeps: Cell::new(None),
            fallback_rejections_remaining: Cell::new(None),
            sweep_count: Cell::new(0),
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                    tearing: Some(solve::BlockTearing {
                        tear_y_indices: vec![0],
                        residual_rows: vec![0],
                        causal_steps: Vec::new(),
                    }),
                    guarded_tearing: None,
                    alternate_charts: Vec::new(),
                }],
            },
        }
    }

    fn fail_after_sweeps(self, count: usize) -> Self {
        self.fail_after_sweeps.set(Some(count));
        self
    }

    fn fallback_after_sweeps(self, count: usize) -> Self {
        self.fallback_after_sweeps.set(Some(count));
        self
    }

    fn output_error(&self, y: f64) -> f64 {
        self.output_gain * (y - self.target)
    }
}

impl ImplicitProjectionModel for AmplifiedNonlinearRoot {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] * y[0] - self.target * self.target;
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let mut residual = [0.0];
        self.eval_residual(y, p, t, &mut residual)?;
        Ok((row == 0).then_some(residual[0]))
    }

    fn eval_jacobian_v(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = 2.0 * y[0] * v[0];
        Ok(())
    }

    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        (row == 0).then(|| solve::scalar_slot_y(0))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row: usize) -> Option<&str> {
        None
    }

    fn torn_block_sweep(
        &self,
        tearing: &solve::BlockTearing,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        residual_out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        self.sweep_count.set(self.sweep_count.get() + 1);
        if let Some(remaining) = self.fallback_rejections_remaining.get() {
            if remaining == 0 {
                self.fallback_rejections_remaining.set(None);
            } else {
                self.fallback_rejections_remaining.set(Some(remaining - 1));
                return Ok(false);
            }
        }
        if let Some(remaining) = self.fallback_after_sweeps.get() {
            if remaining == 0 {
                self.fallback_after_sweeps.set(None);
                self.fallback_rejections_remaining.set(Some(0));
                return Ok(false);
            }
            self.fallback_after_sweeps.set(Some(remaining - 1));
        }
        if let Some(remaining) = self.fail_after_sweeps.get() {
            if remaining == 0 {
                return Err(RuntimeSolveError::solve_ir(
                    "analytic refinement sweep failure",
                ));
            }
            self.fail_after_sweeps.set(Some(remaining - 1));
        }
        tearing::per_row_torn_block_sweep(self, tearing, y, p, t, residual_out)
    }
}

#[test]
fn torn_refinement_applies_bounded_correction_for_amplified_rhs() {
    let model = AmplifiedNonlinearRoot::new(1.0, 1.0e8);
    let initial = 1.0 + 1.0e-11;
    let mut y = [initial];
    let tearing = model.plan.blocks[0].tearing.as_ref().unwrap();
    let update =
        tearing::project_torn_algebraic_block(&model, &mut y, &[], 0.0, tearing, 1.0e-10, true)
            .unwrap()
            .expect("the nonlinear root should settle");
    assert!(update.settled);
    assert!(
        model.output_error(y[0]).abs() <= 1.0e-10,
        "amplified RHS error was not corrected: y={y:?}, error={}",
        model.output_error(y[0])
    );
}

#[test]
fn torn_refinement_keeps_exact_root_and_zero_step_unchanged() {
    let model = AmplifiedNonlinearRoot::new(1.0, 1.0e8);
    let mut y = [1.0];
    let before = y;
    let tearing = model.plan.blocks[0].tearing.as_ref().unwrap();
    let update =
        tearing::project_torn_algebraic_block(&model, &mut y, &[], 0.0, tearing, 1.0e-10, true)
            .unwrap()
            .expect("the exact root should settle");
    assert!(update.settled);
    assert_eq!(y, before, "a zero correction must not perturb the root");
    assert_eq!(model.output_error(y[0]), 0.0);
}

#[test]
fn torn_refinement_propagates_failed_candidate_sweep() {
    let model = AmplifiedNonlinearRoot::new(1.0, 1.0e8).fail_after_sweeps(2);
    let mut y = [1.0 + 1.0e-11];
    let tearing = model.plan.blocks[0].tearing.as_ref().unwrap();
    let result =
        tearing::project_torn_algebraic_block(&model, &mut y, &[], 0.0, tearing, 1.0e-10, true);
    assert!(
        result.is_err(),
        "a failed candidate sweep must remain typed"
    );
}

#[test]
fn torn_refinement_refunds_token_after_fallback() {
    let model = AmplifiedNonlinearRoot::new(1.0, 1.0e8).fallback_after_sweeps(3);
    let mut y = [1.0 + 1.0e-11];
    let tearing = model.plan.blocks[0].tearing.as_ref().unwrap();
    let mut token = tearing::RefinementToken::new(true);
    let first = tearing::project_torn_algebraic_block_with_context(
        &model,
        &mut y,
        &[],
        0.0,
        tearing,
        tearing::TornProjectionContext {
            tolerance: 1.0e-10,
            certify_coordinates: true,
            refinement_token: &mut token,
        },
    )
    .unwrap();
    assert!(
        first.is_none(),
        "the callback must force dense fallback after {} sweeps",
        model.sweep_count.get()
    );
    let second = tearing::project_torn_algebraic_block_with_context(
        &model,
        &mut y,
        &[],
        0.0,
        tearing,
        tearing::TornProjectionContext {
            tolerance: 1.0e-10,
            certify_coordinates: true,
            refinement_token: &mut token,
        },
    )
    .unwrap();
    assert!(second.is_some(), "the refunded token must allow a retry");
    assert!(
        model.output_error(y[0]).abs() <= 1.0e-10,
        "the retry must apply the corrective root step: y={y:?}, error={}",
        model.output_error(y[0])
    );
}

struct NonsingularInvalidatingProjection {
    plan: solve::AlgebraicProjectionPlan,
}

impl NonsingularInvalidatingProjection {
    fn new() -> Self {
        let tearing = |index, row| solve::BlockTearing {
            tear_y_indices: vec![index],
            residual_rows: vec![row],
            causal_steps: Vec::new(),
        };
        Self {
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![
                    solve::AlgebraicProjectionBlock {
                        rows: vec![0],
                        y_indices: vec![0],
                        tearing: Some(tearing(0, 0)),
                        guarded_tearing: None,
                        alternate_charts: Vec::new(),
                    },
                    solve::AlgebraicProjectionBlock {
                        rows: vec![1],
                        y_indices: vec![1],
                        tearing: Some(tearing(1, 1)),
                        guarded_tearing: None,
                        alternate_charts: Vec::new(),
                    },
                ],
            },
        }
    }
}

impl ImplicitProjectionModel for NonsingularInvalidatingProjection {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let root_1 = 3.0_f64.sqrt();
        out[0] = y[0] * y[0] - 2.0 + 1.0e-8 * (y[1] - root_1);
        out[1] = y[1] * y[1] - 3.0;
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
        out[0] = 2.0 * y[0] * v[0] + 1.0e-8 * v[1];
        out[1] = 2.0 * y[1] * v[1];
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let mut residual = [0.0; 2];
        self.eval_residual(y, p, t, &mut residual)?;
        Ok(residual.get(row).copied())
    }

    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row))
    }

    fn algebraic_projection_block_invalidates_earlier(&self, block_index: usize) -> bool {
        block_index == 1
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row: usize) -> Option<&str> {
        None
    }
}

#[test]
fn refinement_tokens_survive_invalidating_multisweep_independently() {
    let model = NonsingularInvalidatingProjection::new();
    let mut y = [1.0, 1.0];
    project_algebraics_with_plan_certified(
        &model,
        &model.plan,
        &mut y,
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 0,
            tolerance: 1.0e-10,
        },
        8,
    )
    .expect("a later invalidating block must not consume block zero's token");
    assert!((y[0] - 2.0_f64.sqrt()).abs() <= 1.0e-10, "y0={y:?}");
    assert!((y[1] - 3.0_f64.sqrt()).abs() <= 1.0e-10, "y1={y:?}");
}
