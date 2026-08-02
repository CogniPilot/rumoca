use crate::{
    AlgebraicProjectionModel, ImplicitProjectionModel, InitialHomotopySystem, RuntimeSolveError,
    project_initial_variables_with_homotopy,
};

use super::initial_continuation::InitialContinuationCoverage;

use super::*;

struct InitialProjectionModel<'a> {
    runtime: &'a SolveRuntime,
    tol: f64,
    max_iters: usize,
    refreshes_algebraic_reads: bool,
}

impl ImplicitProjectionModel for InitialProjectionModel<'_> {
    fn eval_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.runtime
            .implicit_rhs
            .eval_with_context(y, p, t, self.runtime.row_eval_context(), out)
            .map_err(Into::into)
    }

    fn eval_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.runtime
            .implicit_projection_jacobian_v
            .eval_with_context(
                y,
                p,
                t,
                RowEvalContext {
                    seed: Some(v),
                    ..self.runtime.row_eval_context()
                },
                out,
            )
            .map_err(Into::into)
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.runtime
            .model
            .problem
            .continuous
            .implicit_row_targets
            .get(row_idx)
            .copied()
            .flatten()
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self
            .runtime
            .model
            .problem
            .continuous
            .algebraic_projection_plan
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        self.runtime
            .model
            .problem
            .continuous
            .implicit_row_targets
            .get(row_idx)
            .copied()
            .flatten()
            .and_then(|slot| match slot {
                solve::ScalarSlot::Y { index, .. } => Some(index),
                _ => None,
            })
            .and_then(|index| {
                self.runtime
                    .model
                    .problem
                    .solve_layout
                    .solver_maps
                    .names
                    .get(index)
            })
            .map(String::as_str)
    }

    fn variable_name_for_y_index(&self, y_index: usize) -> Option<&str> {
        self.runtime
            .model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(y_index)
            .map(String::as_str)
    }

    fn variable_scale_for_y_index(&self, y_index: usize) -> f64 {
        self.runtime.model.solver_variable_scale(y_index)
    }
}

impl AlgebraicProjectionModel for InitialProjectionModel<'_> {
    fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        // An initialization row observes the simultaneous continuous system at
        // the initial instant (MLS 3.6 §8.6), never the declaration seeds of
        // algebraic/output coordinates.  Refresh those derived coordinates on
        // an evaluation-local view: rows that the projection can solve remain
        // the only writers of `y`, while the complete-residual certificate sees
        // the values the continuous equations actually determine.
        let mut settled_y = Vec::new();
        let residual_y = if self.refreshes_algebraic_reads {
            settled_y.extend_from_slice(y);
            self.runtime.refresh_algebraic_and_output_slots(
                t,
                &mut settled_y,
                p,
                self.tol,
                self.max_iters,
            )?;
            settled_y.as_slice()
        } else {
            y
        };
        self.runtime
            .initial_residual
            .eval_with_context(residual_y, p, t, self.runtime.row_eval_context(), out)
            .map_err(Into::into)
    }

    fn initial_residual_len(&self) -> usize {
        self.runtime.initial_residual.len()
    }

    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.runtime
            .model
            .problem
            .initialization
            .row_targets
            .get(row_idx)
            .copied()
            .flatten()
    }

    fn initial_row_role(&self, row_idx: usize) -> Option<solve::InitializationRowRole> {
        self.runtime
            .model
            .problem
            .initialization
            .row_roles
            .get(row_idx)
            .copied()
    }

    fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.runtime
            .initial_residual_jacobian_v
            .eval_with_context(
                y,
                p,
                t,
                RowEvalContext {
                    seed: Some(v),
                    ..self.runtime.row_eval_context()
                },
                out,
            )
            .map_err(Into::into)
    }

    fn eval_initial_target_value(
        &self,
        output_index: usize,
        target_y_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(row_idx) = self
            .runtime
            .initial_scalar_residual
            .single_output_row_for_output_index(output_index)
        else {
            return Ok(None);
        };
        self.runtime
            .initial_scalar_residual
            .eval_target_assignment_row_unchecked_with_context(
                row_idx,
                target_y_index,
                y,
                p,
                t,
                self.runtime.row_eval_context(),
            )
            .map_err(Into::into)
    }

    fn eval_initial_residual_row(
        &self,
        output_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(row_idx) = self
            .runtime
            .initial_scalar_residual
            .single_output_row_for_output_index(output_index)
        else {
            return Ok(None);
        };
        self.runtime
            .initial_scalar_residual
            .eval_row_output_unchecked_with_context(
                row_idx,
                0,
                y,
                p,
                t,
                self.runtime.row_eval_context(),
            )
            .map(Some)
            .map_err(Into::into)
    }
}

impl SolveRuntime {
    /// Project the initialization unknowns, sweeping the homotopy continuation
    /// parameter when the model has a solve for it to steer.
    ///
    /// The sweep drives exactly the solves named by the runtime's certified
    /// `InitialContinuationCoverage`: the initialization projection plan and —
    /// whenever that coverage includes implicit rows — the algebraic refresh.
    /// The algebraic refresh has to run *inside* the sweep because that is where
    /// the Solve lowering parks a `homotopy(...)` written in an `equation`
    /// section; refreshing only after the sweep leaves the algebraic solve to
    /// pick a root from a cold guess, which is how
    /// `Modelica.Electrical.Analog.Examples.OpAmps.SignalGenerator` settled onto
    /// the trivial all-zero equilibrium instead of the ±15 V branch the
    /// simplified expression selects.
    ///
    /// When the coverage names no steered solve — every λ read sits in a
    /// derivative, discrete, or unowned initialization row — there is nothing to
    /// sweep and λ stays at its seeded `1.0`, so those rows evaluate to `actual`
    /// (MLS 3.6 §3.7.4.3's trivial implementation).
    pub fn project_initial_variables(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        let drives_algebraic_refresh = self
            .initial_continuation
            .as_ref()
            .is_some_and(InitialContinuationCoverage::drives_algebraic_refresh);
        project_initial_variables_with_homotopy(
            InitialHomotopySystem {
                model: &InitialProjectionModel {
                    runtime: self,
                    tol,
                    max_iters,
                    refreshes_algebraic_reads: self
                        .model
                        .problem
                        .initialization
                        .row_roles
                        .iter()
                        .any(|role| {
                            matches!(
                                role,
                                solve::InitializationRowRole::UnownedCoordinate(
                                    solve::InitializationCoordinateKind::Algebraic
                                )
                            )
                        }),
                },
                t,
                plan: &self.model.problem.initialization.projection_plan,
                homotopy_parameter_index: self
                    .initial_continuation
                    .as_ref()
                    .and_then(InitialContinuationCoverage::sweep_parameter_index),
                tol,
            },
            y,
            p,
            |y, p| {
                if !drives_algebraic_refresh {
                    return Ok(());
                }
                self.refresh_algebraic_and_output_slots(t, y, p, tol, max_iters)
            },
        )
    }

    /// Settle the complete initialization system, including direct
    /// initialization updates such as generated condition-memory equations.
    ///
    /// Updates run once before projection so the first residual evaluation sees
    /// the initial-time branch, then again afterward because a condition may
    /// depend on a projected algebraic. All solver backends use this shared
    /// fixed-point so state and zero-state models observe identical MLS §8.6
    /// initialization semantics.
    pub fn settle_initialization_system(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        for _ in 0..max_iters {
            self.apply_initialization_updates(y, p, t, tol, max_iters)?;
            self.project_initial_variables(y, p, t, tol, max_iters)?;
            if !self.apply_initialization_updates(y, p, t, tol, max_iters)? {
                return Ok(());
            }
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "initial algebraic/update projection did not converge at t={t}"
        )))
    }
}

#[cfg(test)]
mod tests {
    use indexmap::IndexMap;
    use rumoca_core::{BytePos, SourceId, Span};

    use super::*;

    fn test_span() -> Span {
        Span::new(
            SourceId::from_source_name("initial_homotopy_runtime.mo"),
            BytePos(0),
            BytePos(1),
        )
    }

    fn block(programs: Vec<Vec<solve::LinearOp>>) -> solve::ComputeBlock {
        solve::ComputeBlock::from_scalar_program_block(
            solve::ScalarProgramBlock::with_source_span(
                programs,
                test_span()
                    .require_provenance("initial-projection runtime fixture")
                    .expect("fixture span is source-backed"),
            )
            .expect("fixture program is computable"),
        )
    }

    #[test]
    fn runtime_initial_projection_finishes_with_actual_homotopy_system() {
        let residual = block(vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::LoadP { dst: 1, index: 0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]]);
        let jacobian = block(vec![vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]]);
        let implicit = block(vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]]);
        let model = solve::SolveModel {
            problem: solve::SolveProblem {
                solve_layout: solve::SolveLayout {
                    solver_maps: solve::SolverNameIndexMaps {
                        names: vec!["x".to_string()],
                        name_to_idx: IndexMap::from([("x".to_string(), 0)]),
                        base_to_indices: IndexMap::from([("x".to_string(), vec![0])]),
                    },
                    state_scalar_count: 1,
                    compiled_parameter_len: 1,
                    initial_homotopy_parameter_index: Some(0),
                    ..Default::default()
                },
                continuous: solve::ContinuousSolveSystem {
                    implicit_rhs: implicit,
                    implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                    ..Default::default()
                },
                initialization: solve::InitializationSolveSystem {
                    residual,
                    row_targets: vec![Some(solve::scalar_slot_y(0))],
                    projection_unknowns: vec![solve::scalar_slot_y(0)],
                    projection_plan: solve::InitializationProjectionPlan {
                        blocks: vec![solve::InitializationProjectionBlock {
                            rows: vec![0],
                            unknowns: vec![solve::scalar_slot_y(0)],
                        }],
                    },
                    ..Default::default()
                },
                ..Default::default()
            },
            artifacts: solve::SolveArtifacts {
                continuous: solve::ContinuousSolveArtifacts {
                    implicit_jacobian_v: jacobian.clone(),
                    implicit_jacobian_v_scalar: to_scalar_program_block(&jacobian)
                        .expect("test Jacobian should scalarize"),
                    ..Default::default()
                },
                initialization: solve::InitializationSolveArtifacts {
                    residual_jacobian_v: jacobian,
                    ..Default::default()
                },
            },
            initial_y: vec![0.0],
            parameters: vec![0.0],
            ..Default::default()
        };
        let runtime = SolveRuntime::new(&model).expect("runtime should prepare");
        let mut y = model.initial_y.clone();
        let mut p = model.parameters.clone();

        runtime
            .project_initial_variables(&mut y, &mut p, 0.0, 1.0e-10, 8)
            .expect("continuation should project the actual system");

        assert_eq!(p, vec![1.0]);
        assert!((y[0] - 1.0).abs() <= 1.0e-10);
    }
}
