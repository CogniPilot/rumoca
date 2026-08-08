use super::{AlgebraicProjectionModel, RuntimeSolveError, project_initial_variables_with_plan};

const INITIAL_CONTINUATION_STEP: f64 = 0.125;
const MIN_CONTINUATION_STEP: f64 = 1.0 / 16_384.0;
const MAX_CONTINUATION_ATTEMPTS: usize = 256;

/// Solve the initialization system while advancing `homotopy(actual,
/// simplified)` from the simplified system (λ = 0) to the actual one (λ = 1).
///
/// `continuation_dependents` re-solves every *other* system the sweep must
/// carry along with `plan` — in the runtime that is the algebraic refresh, whose
/// rows are where a model like `IdealizedOpAmpLimited` actually parks its
/// homotopy expression. Passing a step that does not cover a λ-reading solve
/// makes the sweep steer nothing: it walks λ to one, reports success, and leaves
/// that solve to find whichever root its cold guess happens to sit on. The
/// caller therefore states the covered solves explicitly; `SolveRuntime` derives
/// them from the same `InitialContinuationCoverage` that its acceptance check
/// validates the model against.
///
/// `homotopy_parameter_index` is `Some` exactly when the caller has a solve for
/// the sweep to steer, so the sweep always runs from λ = 0 when it is `Some`.
/// The incoming value of `p[λ]` is *not* consulted: λ is seeded to `1.0` so that
/// every evaluation outside this function reads `actual` (MLS 3.6 §3.7.4.3's
/// trivial implementation), and a value-based skip would silently turn the
/// continuation off for exactly the models that need it. Every exit path leaves
/// λ at `1.0` — success by walking there, failure by restoring the caller's
/// vector.
pub(super) fn project_initial_variables_with_homotopy<M, F>(
    system: super::InitialHomotopySystem<'_, M>,
    y: &mut [f64],
    p: &mut [f64],
    mut continuation_dependents: F,
) -> Result<(), RuntimeSolveError>
where
    M: AlgebraicProjectionModel,
    F: FnMut(&mut [f64], &[f64]) -> Result<(), RuntimeSolveError>,
{
    let super::InitialHomotopySystem {
        model,
        t,
        plan,
        homotopy_parameter_index,
        tol,
    } = system;
    let Some(lambda_index) = homotopy_parameter_index else {
        return project_initial_variables_with_plan(model, y, p, t, plan, tol);
    };
    let parameter_count = p.len();
    if lambda_index >= parameter_count {
        return Err(RuntimeSolveError::solve_ir(format!(
            "initial homotopy parameter index {lambda_index} is outside {parameter_count} parameters"
        )));
    }

    let mut solve_at_lambda = |y: &mut [f64], p: &mut [f64]| -> Result<(), RuntimeSolveError> {
        project_initial_variables_with_plan(model, y, p, t, plan, tol)?;
        continuation_dependents(y, p)
    };

    let original_y = y.to_vec();
    let original_p = p.to_vec();
    p[lambda_index] = 0.0;
    if let Err(source) = solve_at_lambda(y, p) {
        y.copy_from_slice(&original_y);
        p.copy_from_slice(&original_p);
        return Err(continuation_error(0.0, INITIAL_CONTINUATION_STEP, source));
    }

    let mut accepted_lambda = 0.0;
    let mut step = INITIAL_CONTINUATION_STEP;
    for _ in 0..MAX_CONTINUATION_ATTEMPTS {
        if accepted_lambda == 1.0 {
            p[lambda_index] = 1.0;
            return Ok(());
        }
        let candidate_lambda = f64::min(1.0, accepted_lambda + step);
        let checkpoint = y.to_vec();
        p[lambda_index] = candidate_lambda;
        match solve_at_lambda(y, p) {
            Ok(()) => {
                accepted_lambda = candidate_lambda;
                step = f64::min(
                    f64::max(step * 1.5, MIN_CONTINUATION_STEP),
                    1.0 - accepted_lambda,
                );
            }
            Err(source) => {
                y.copy_from_slice(&checkpoint);
                p[lambda_index] = accepted_lambda;
                step *= 0.5;
                if step < MIN_CONTINUATION_STEP {
                    y.copy_from_slice(&original_y);
                    p.copy_from_slice(&original_p);
                    return Err(continuation_error(candidate_lambda, step, source));
                }
            }
        }
    }

    y.copy_from_slice(&original_y);
    p.copy_from_slice(&original_p);
    Err(RuntimeSolveError::solve_ir(format!(
        "initial homotopy continuation exceeded {MAX_CONTINUATION_ATTEMPTS} attempts at \
         lambda={accepted_lambda:.9} step={step:.3e}"
    )))
}

fn continuation_error(lambda: f64, step: f64, source: RuntimeSolveError) -> RuntimeSolveError {
    let span = source.source_span();
    RuntimeSolveError::solve_ir_with_span(
        format!(
            "initial homotopy continuation failed at lambda={lambda:.9} step={step:.3e}: {source}"
        ),
        span,
    )
}

#[cfg(test)]
mod tests {
    use std::cell::{Cell, RefCell};

    use rumoca_ir_solve as solve;

    use super::*;
    use crate::{AlgebraicProjectionModel, ImplicitProjectionModel};

    struct StepLimitedModel {
        plan: solve::AlgebraicProjectionPlan,
        accepted_lambda: Cell<f64>,
        attempted_lambdas: RefCell<Vec<f64>>,
    }

    impl StepLimitedModel {
        fn new() -> Self {
            Self {
                plan: solve::AlgebraicProjectionPlan {
                    blocks: vec![solve::AlgebraicProjectionBlock {
                        rows: vec![0],
                        y_indices: vec![0],
                    }],
                },
                accepted_lambda: Cell::new(0.0),
                attempted_lambdas: RefCell::new(Vec::new()),
            }
        }

        fn accept_lambda(&self, p: &[f64]) -> Result<f64, RuntimeSolveError> {
            let lambda = p[0];
            self.attempted_lambdas.borrow_mut().push(lambda);
            if lambda - self.accepted_lambda.get() > 0.2 {
                return Err(RuntimeSolveError::solve_ir(
                    "continuation step exceeds model convergence radius".to_string(),
                ));
            }
            self.accepted_lambda.set(lambda);
            Ok(lambda)
        }
    }

    impl ImplicitProjectionModel for StepLimitedModel {
        fn eval_residual(
            &self,
            y: &[f64],
            p: &[f64],
            _t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            out[0] = y[0] - p[0];
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

        fn implicit_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
            Some(solve::scalar_slot_y(0))
        }

        fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
            &self.plan
        }

        fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
            Some("x")
        }
    }

    impl AlgebraicProjectionModel for StepLimitedModel {
        fn eval_initial_residual(
            &self,
            y: &[f64],
            p: &[f64],
            _t: f64,
            out: &mut [f64],
        ) -> Result<(), RuntimeSolveError> {
            let lambda = self.accept_lambda(p)?;
            out[0] = y[0] - lambda;
            Ok(())
        }

        fn initial_residual_len(&self) -> usize {
            1
        }

        fn initial_target(&self, _row_idx: usize) -> Option<solve::ScalarSlot> {
            Some(solve::scalar_slot_y(0))
        }

        fn eval_initial_jacobian_v(
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
    }

    #[test]
    fn continuation_halves_rejected_steps_and_reaches_actual_system() {
        let model = StepLimitedModel::new();
        let mut y = vec![0.0];
        let mut p = vec![0.0];
        let initial_plan = solve::InitializationProjectionPlan {
            blocks: vec![solve::InitializationProjectionBlock {
                rows: vec![0],
                unknowns: vec![solve::scalar_slot_y(0)],
            }],
        };

        project_initial_variables_with_homotopy(
            super::super::InitialHomotopySystem {
                model: &model,
                t: 0.0,
                plan: &initial_plan,
                homotopy_parameter_index: Some(0),
                tol: 1.0e-10,
            },
            &mut y,
            &mut p,
            |_, _| Ok(()),
        )
        .expect("adaptive continuation should stay within the convergence radius");

        assert_eq!(p, vec![1.0]);
        assert!((y[0] - 1.0).abs() <= 1.0e-10);
        assert!(
            model
                .attempted_lambdas
                .borrow()
                .windows(2)
                .any(|pair| pair[1] > pair[0] + 0.2),
            "the regression must exercise a rejected oversized continuation step"
        );
    }
}
