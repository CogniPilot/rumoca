use crate::RuntimeSolveError;
use crate::runtime::projection::{
    AlgebraicProjectionModel, ImplicitProjectionModel, InitialHomotopySystem, ScaledNewtonSystem,
    project_initial_variables_with_homotopy,
};
use nalgebra::DVector;

use super::initial_continuation::InitialContinuationCoverage;

use super::*;

struct InitialProjectionModel<'a> {
    runtime: &'a SolveRuntime,
}

impl ImplicitProjectionModel for InitialProjectionModel<'_> {
    fn eval_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        match &self.runtime.execution_plan.implicit_rhs {
            ExecutionArm::Native(compiled) => compiled.call(y, p, t, out).map_err(|reason| {
                RuntimeSolveError::native_call(NativeExecutionOwner::ImplicitResidual, reason)
            })?,
            ExecutionArm::Interpreter(selected_arm) => self
                .runtime
                .implicit_rhs
                .eval_with_context(y, p, t, selected_arm.row_eval_context(self.runtime), out)
                .map_err(RuntimeSolveError::from)?,
        }
        self.runtime
            .report_nonfinite_implicit_residual_inputs(t, y, out);
        Ok(())
    }

    fn eval_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        match &self.runtime.execution_plan.implicit_projection_jacobian {
            ExecutionArm::Native(compiled) => compiled.call(y, p, t, v, out).map_err(|reason| {
                RuntimeSolveError::native_call(
                    NativeExecutionOwner::ImplicitProjectionJacobian,
                    reason,
                )
            }),
            ExecutionArm::Interpreter(selected_arm) => self
                .runtime
                .implicit_projection_jacobian_v
                .eval_with_context(
                    y,
                    p,
                    t,
                    selected_arm.seeded_row_eval_context(self.runtime, v),
                    out,
                )
                .map_err(Into::into),
        }
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.runtime
            .model
            .problem()
            .continuous()
            .implicit_row_targets()
            .get(row_idx)
            .copied()
            .flatten()
    }

    #[cfg(test)]
    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        self.runtime
            .model
            .problem()
            .continuous()
            .algebraic_projection_plan()
    }

    fn algebraic_projection_plan_is_validated(&self) -> bool {
        false
    }

    fn algebraic_projection_block_structure(
        &self,
        block_index: usize,
    ) -> Option<&solve::JacobianStructure> {
        self.runtime
            .continuous_structural
            .algebraic_projection()
            .get(block_index)
    }

    fn algebraic_projection_block_invalidates_earlier(&self, block_index: usize) -> bool {
        self.runtime
            .continuous_structural
            .algebraic_invalidates_earlier(block_index)
            .unwrap_or(true)
    }

    fn solve_algebraic_newton_delta(
        &self,
        block_index: usize,
        system: ScaledNewtonSystem<'_>,
    ) -> Option<DVector<f64>> {
        self.runtime
            .algebraic_newton_caches
            .get(block_index)
            .map_or_else(
                || crate::runtime::projection::scaled_newton_delta(system),
                |cache| {
                    crate::runtime::projection::scaled_newton_delta_with_cache(
                        system,
                        &mut cache.borrow_mut(),
                    )
                },
            )
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        self.runtime
            .model
            .problem()
            .continuous()
            .implicit_row_targets()
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
                    .problem()
                    .solve_layout()
                    .solver_maps
                    .names
                    .get(index)
            })
            .map(String::as_str)
    }

    fn implicit_target_assignment_is_exact(&self, row_idx: usize, target_y_index: usize) -> bool {
        self.runtime
            .implicit_scalar_rhs
            .row_output_position(row_idx)
            .is_some_and(|(program_idx, output_offset)| {
                self.runtime
                    .implicit_scalar_rhs
                    .certifies_exact_target_assignment_output(
                        program_idx,
                        output_offset,
                        target_y_index,
                    )
            })
    }

    fn variable_name_for_y_index(&self, y_index: usize) -> Option<&str> {
        self.runtime
            .model
            .problem()
            .solve_layout()
            .solver_maps
            .names
            .get(y_index)
            .map(String::as_str)
    }

    fn variable_scale_for_y_index(&self, y_index: usize) -> f64 {
        self.runtime.model.solver_variable_scales()[y_index]
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
        let (residual_y, residual_p) = (y, p);
        match &self.runtime.execution_plan.initial_residual {
            ExecutionArm::Native(compiled) => compiled
                .call(residual_y, residual_p, t, out)
                .map_err(|reason| {
                    RuntimeSolveError::native_call(NativeExecutionOwner::InitialResidual, reason)
                }),
            ExecutionArm::Interpreter(selected_arm) => self
                .runtime
                .initial_residual
                .eval_with_context(
                    residual_y,
                    residual_p,
                    t,
                    selected_arm.row_eval_context(self.runtime),
                    out,
                )
                .map_err(Into::into),
        }
    }

    fn initial_residual_len(&self) -> usize {
        self.runtime.initial_residual.len()
    }

    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.runtime
            .model
            .problem()
            .initialization()
            .row_targets()
            .get(row_idx)
            .copied()
            .flatten()
    }

    fn initial_projection_block_structure(
        &self,
        block_index: usize,
    ) -> Option<&solve::JacobianStructure> {
        self.runtime
            .initialization_structural
            .projection()
            .get(block_index)
    }

    fn initial_row_role(&self, row_idx: usize) -> Option<solve::InitializationRowRole> {
        self.runtime
            .model
            .problem()
            .initialization()
            .row_roles()
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
        match &self.runtime.execution_plan.initial_residual_jacobian {
            ExecutionArm::Native(compiled) => compiled.call(y, p, t, v, out).map_err(|reason| {
                RuntimeSolveError::native_call(
                    NativeExecutionOwner::InitialResidualJacobian,
                    reason,
                )
            }),
            ExecutionArm::Interpreter(selected_arm) => self
                .runtime
                .initial_residual_jacobian_v
                .eval_with_context(
                    y,
                    p,
                    t,
                    selected_arm.seeded_row_eval_context(self.runtime, v),
                    out,
                )
                .map_err(Into::into),
        }
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
                self.runtime
                    .execution_plan
                    .interpreter
                    .initial_projection_rows
                    .row_eval_context(self.runtime),
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
                self.runtime
                    .execution_plan
                    .interpreter
                    .initial_projection_rows
                    .row_eval_context(self.runtime),
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
                model: &InitialProjectionModel { runtime: self },
                t,
                plan: self.model.problem().initialization().projection_plan(),
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
            // MLS 3.6 §3.7.2 defines delay(u, ...) = u throughout
            // initialization. The delayed-value P slots are runtime storage,
            // so keep them synchronized with the current initialization
            // coordinate before either updates or residuals observe them.
            self.refresh_delay_values(t, y, p)?;
            self.apply_initialization_updates(y, p, t, tol, max_iters)?;
            self.project_initial_variables(y, p, t, tol, max_iters)?;
            // Projection can move a delay source. Refresh its identity value
            // before deciding that the coupled initialization fixed point has
            // converged; otherwise declaration starts can leak into the first
            // event even though the source itself was settled correctly.
            let before_delay_refresh = p.to_vec();
            self.refresh_delay_values(t, y, p)?;
            let delay_changed = crate::runtime_values_changed(&before_delay_refresh, p, tol);
            let update_changed = self.apply_initialization_updates(y, p, t, tol, max_iters)?;
            if !delay_changed && !update_changed {
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

    use crate::test_support::empty_binary64_first_product_model;

    fn test_span() -> Span {
        Span::new(
            SourceId::from_source_name("initial_homotopy_runtime.mo"),
            BytePos(0),
            BytePos(1),
        )
    }

    fn scalar_block(programs: Vec<Vec<solve::LinearOp>>) -> solve::ScalarProgramBlock {
        solve::ScalarProgramBlock::with_source_span(
            programs,
            test_span()
                .require_provenance("initial-projection runtime fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("fixture program is computable")
    }

    fn block(programs: Vec<Vec<solve::LinearOp>>) -> solve::ComputeBlock {
        solve::ComputeBlock::from_scalar_program_block(scalar_block(programs))
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
        let solve_layout = solve::SolveLayout {
            solver_maps: solve::SolverNameIndexMaps {
                names: vec!["x".to_string()],
                name_to_idx: IndexMap::from([("x".to_string(), 0)]),
                base_to_indices: IndexMap::from([("x".to_string(), vec![0])]),
            },
            state_scalar_count: 1,
            compiled_parameter_len: 1,
            initial_homotopy_parameter_index: Some(0),
            ..Default::default()
        };
        let initialization = solve::InitializationSolveSystem::construct(
            residual,
            vec![Some(solve::scalar_slot_y(0))],
            vec![solve::InitializationRowRole::Solved],
            1,
            vec![solve::scalar_slot_y(0)],
            solve::InitializationProjectionPlan {
                blocks: vec![solve::InitializationProjectionBlock {
                    rows: vec![0],
                    unknowns: vec![solve::scalar_slot_y(0)],
                }],
            },
            (solve::ScalarProgramBlock::default(), Vec::new()),
        )
        .expect("the homotopy fixture initialization system is exactly correlated");
        let discrete = solve::DiscreteSolveSystem::default();
        let events = solve::SolveEventPartition::default();
        let clocks = solve::SolveClockPartition::default();
        let continuous = crate::test_support::ContinuousSystemFixture {
            derivative_rhs: crate::test_support::zero_derivative_rhs(1, test_span()),
            ..crate::test_support::ContinuousSystemFixture::empty()
        };
        let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
        let model = crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(IndexMap::new(), 1, 1),
                solve_layout,
                continuous,
                initialization,
                discrete,
                events,
                clocks,
            )
            .expect("initial projection fixture satisfies the checked root contract"),
            initial_y: vec![0.0],
            solver_nominals: vec![1.0],
            parameters: vec![0.0],
            ..empty_binary64_first_product_model()
        };
        let model = std::sync::Arc::new(model);
        let runtime =
            SolveRuntime::new(std::sync::Arc::clone(&model)).expect("runtime should prepare");
        let mut y = model.initial_y().to_vec();
        let mut p = model.parameters().to_vec();

        runtime
            .project_initial_variables(&mut y, &mut p, 0.0, 1.0e-10, 8)
            .expect("continuation should project the actual system");

        assert_eq!(p, vec![1.0]);
        assert!((y[0] - 1.0).abs() <= 1.0e-10);
    }

    fn delay_identity_projection_model() -> solve::SolveModel {
        let initial = block(vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 1, value: 2.0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]]);
        let delay_time = scalar_block(vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]]);
        let solve_layout = solve::SolveLayout {
            solver_maps: solve::SolverNameIndexMaps {
                names: vec!["source".to_string()],
                name_to_idx: IndexMap::from([("source".to_string(), 0)]),
                base_to_indices: IndexMap::from([("source".to_string(), vec![0])]),
            },
            state_scalar_count: 1,
            compiled_parameter_len: 1,
            ..Default::default()
        };
        let initialization = solve::InitializationSolveSystem::construct(
            initial,
            vec![Some(solve::scalar_slot_y(0))],
            vec![solve::InitializationRowRole::Solved],
            1,
            vec![solve::scalar_slot_y(0)],
            solve::InitializationProjectionPlan {
                blocks: vec![solve::InitializationProjectionBlock {
                    rows: vec![0],
                    unknowns: vec![solve::scalar_slot_y(0)],
                }],
            },
            (solve::ScalarProgramBlock::default(), Vec::new()),
        )
        .expect("the source fixture initialization system is exactly correlated");
        let discrete = solve::DiscreteSolveSystem::default();
        let events = solve::SolveEventPartition {
            delays: solve::SolveDelayPartition {
                source_rhs: scalar_block(vec![vec![
                    solve::LinearOp::LoadY { dst: 0, index: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ]]),
                delay_time_rhs: delay_time.clone(),
                delay_max_rhs: delay_time,
                value_parameter_indices: vec![0],
                source_is_discrete: vec![false],
            },
            ..Default::default()
        };
        let clocks = solve::SolveClockPartition::default();
        let continuous = crate::test_support::ContinuousSystemFixture {
            derivative_rhs: crate::test_support::zero_derivative_rhs(1, test_span()),
            ..crate::test_support::ContinuousSystemFixture::empty()
        };
        let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
        crate::test_support::checked_solve_model! {
            problem: crate::test_support::checked_solve_problem!(
                solve::VarLayout::from_parts(IndexMap::new(), 1, 1),
                solve_layout,
                continuous,
                initialization,
                discrete,
                events,
                clocks,
            )
            .expect("delay projection fixture satisfies the checked root contract"),
            initial_y: vec![0.0],
            solver_nominals: vec![1.0],
            parameters: vec![0.0],
            ..empty_binary64_first_product_model()
        }
    }

    #[test]
    fn settled_initialization_refreshes_delay_identity_from_the_projected_source() {
        let model = delay_identity_projection_model();
        let model = std::sync::Arc::new(model);
        let runtime =
            SolveRuntime::new(std::sync::Arc::clone(&model)).expect("runtime should prepare");
        let mut y = model.initial_y().to_vec();
        let mut p = model.parameters().to_vec();
        runtime
            .initialize_delay_history(0.0, &y, &mut p)
            .expect("delay history should seed from the declaration start");

        runtime
            .settle_initialization_system(&mut y, &mut p, 0.0, 1.0e-10, 8)
            .expect("initialization and delay identity should settle together");

        assert!((y[0] - 2.0).abs() <= 1.0e-10);
        assert!((p[0] - 2.0).abs() <= 1.0e-10);
    }
}
