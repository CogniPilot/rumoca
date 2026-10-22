//! Fallible boundary to the existing scalar evaluator.

use super::*;
use rumoca_eval_solve::dense_basis::DependentConditioning;

impl SolveRuntime {
    pub fn full_solver_y(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        self.scalar_execution()?
            .full_solver_y(t, state, params, tol, max_iters)
    }

    pub fn full_solver_y_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        solver_y: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .full_solver_y_into(t, state, params, tol, max_iters, solver_y)
    }

    pub fn full_solver_y_with_guess(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .full_solver_y_with_guess(t, state, params, guess, tol, max_iters)
    }

    pub(crate) fn full_solver_y_with_guess_certified(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .full_solver_y_with_guess_certified(t, state, params, guess, tol, max_iters)
    }

    pub fn eval_state_derivatives(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        self.scalar_execution()?
            .eval_state_derivatives(t, state, params, tol, max_iters)
    }

    pub fn eval_state_derivatives_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_state_derivatives_into(t, state, params, tol, max_iters, out)
    }

    pub fn eval_state_derivatives_with_guess(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        self.scalar_execution()?
            .eval_state_derivatives_with_guess(t, state, params, guess, tol, max_iters)
    }

    // SPEC_0021: Exception - public runtime API mirrors the solver callback inputs and forwards them unchanged.
    #[allow(clippy::too_many_arguments)]
    pub fn eval_state_derivatives_with_guess_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_state_derivatives_with_guess_into(t, state, params, guess, tol, max_iters, out)
    }

    pub fn eval_root_conditions(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        self.scalar_execution()?
            .eval_root_conditions(t, state, params, tol, max_iters)
    }

    pub fn eval_root_conditions_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_root_conditions_into(t, state, params, tol, max_iters, out)
    }

    pub fn eval_root_search_conditions_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_root_search_conditions_into(t, state, params, tol, max_iters, out)
    }

    // SPEC_0021: Exception - public runtime API mirrors the solver callback inputs and forwards them unchanged.
    #[allow(clippy::too_many_arguments)]
    pub fn eval_root_search_conditions_with_guess_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_root_search_conditions_with_guess_into(
                t, state, params, guess, tol, max_iters, out,
            )
    }

    pub fn eval_root_search_conditions_after_derivative_settle_into(
        &self,
        t: f64,
        params: &[f64],
        solver_y: &mut [f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_root_search_conditions_after_derivative_settle_into(
                t, params, solver_y, tol, max_iters, out,
            )
    }

    pub(crate) fn eval_root_search_conditions_after_algebraic_settle_into(
        &self,
        t: f64,
        params: &[f64],
        solver_y: &mut [f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_root_search_conditions_after_algebraic_settle_into(
                t, params, solver_y, tol, max_iters, out,
            )
    }

    pub fn apply_unfiltered_discrete_rows_once(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        _tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        self.scalar_execution()?
            .apply_unfiltered_discrete_rows_once(y, p, t, _tol)
    }

    pub fn seed_condition_memory_for_initialization(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
    ) -> Result<Vec<SeededConditionMemory>, RuntimeSolveError> {
        self.scalar_execution()?
            .seed_condition_memory_for_initialization(y, p, t, tol)
    }

    pub fn refresh_observation_discrete_rows(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        self.scalar_execution()?
            .refresh_observation_discrete_rows(y, p, t, tol, max_iters)
    }

    pub fn execute_event_transaction(
        &self,
        transaction_index: usize,
        y: &mut [f64],
        p: &mut [f64],
        time: f64,
    ) -> Result<EventTransactionExecution, RuntimeSolveError> {
        self.scalar_execution()?
            .execute_event_transaction(transaction_index, y, p, time)
    }

    pub fn eval_initial_residual_jacobian_v(
        &self,
        (y, p, t): (&[f64], &[f64], f64),
        settle: AlgebraicSettle,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_initial_residual_jacobian_v((y, p, t), settle, v, out)
    }

    pub fn eval_initialization_update_jacobian_v(
        &self,
        (y, p, t): (&[f64], &[f64], f64),
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_initialization_update_jacobian_v((y, p, t), v, out)
    }

    pub fn project_initial_variables(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .project_initial_variables(y, p, t, tol, max_iters)
    }

    pub fn settle_initialization_system(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .settle_initialization_system(y, p, t, tol, max_iters)
    }

    pub fn update_relation_memory_from_state(
        &self,
        t: f64,
        state: &[f64],
        params: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        self.scalar_execution()?
            .update_relation_memory_from_state(t, state, params, tol, max_iters)
    }

    pub fn eval_dynamic_time_event_rows_into(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_dynamic_time_event_rows_into(t, solver_y, params, out)
    }

    pub fn apply_initialization_updates(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        _tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        self.scalar_execution()?
            .apply_initialization_updates(y, p, t, _tol, max_iters)
    }

    pub fn apply_runtime_assignments_until_stable(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        _tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        self.scalar_execution()?
            .apply_runtime_assignments_until_stable(y, p, t, _tol, max_iters)
    }

    pub fn apply_post_commit_assignments_until_stable(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        _tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        self.scalar_execution()?
            .apply_post_commit_assignments_until_stable(y, p, t, _tol, max_iters)
    }

    pub fn apply_projected_event_update<P>(
        &self,
        input: ProjectedEventUpdateInput<'_>,
        project_algebraics: P,
    ) -> Result<EventActionOutcome, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        self.scalar_execution()?
            .apply_projected_event_update(input, project_algebraics)
    }

    pub(crate) fn update_algebraic_relation_memory_from_solver_y_except_overrides(
        &self,
        t: f64,
        y: &[f64],
        p: &mut [f64],
        root_relation_overrides: &mut Vec<(usize, f64)>,
    ) -> Result<bool, RuntimeSolveError> {
        self.scalar_execution()?
            .update_algebraic_relation_memory_from_solver_y_except_overrides(
                t,
                y,
                p,
                root_relation_overrides,
            )
    }

    pub fn eval_root_conditions_from_solver_y(
        &self,
        t: f64,
        y: &[f64],
        p: &[f64],
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        self.scalar_execution()?
            .eval_root_conditions_from_solver_y(t, y, p)
    }

    pub fn eval_event_actions(
        &self,
        y: &[f64],
        p: &[f64],
        event_pre_p: &[f64],
        t: f64,
        row_filter: EventUpdateRowFilter,
    ) -> Result<EventActionOutcome, RuntimeSolveError> {
        self.scalar_execution()?
            .eval_event_actions(y, p, event_pre_p, t, row_filter)
    }

    pub fn record_visible_sample(
        &self,
        data: &mut [Vec<f64>],
        solver_y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .record_visible_sample(data, solver_y, params, t)
    }

    pub fn visible_values(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        self.scalar_execution()?.visible_values(y, params, t)
    }

    pub fn visible_values_for_names(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
        names: &[String],
    ) -> Result<IndexMap<String, f64>, RuntimeSolveError> {
        self.scalar_execution()?
            .visible_values_for_names(y, params, t, names)
    }

    pub fn eval_state_jacobian_v_ad_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_state_jacobian_v_ad_into(lin, state, seed, out)
    }

    pub fn eval_state_jacobian_v_ad_with_guess_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        seed: &[f64],
        solver_y_guess: &mut [f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_state_jacobian_v_ad_with_guess_into(lin, state, seed, solver_y_guess, out)
    }

    /// The parameter-static state Jacobian products of this runtime, when its
    /// derivative refresh and derivative programs carry the certificate.
    pub(crate) fn state_jacobian_memo(&self) -> Option<&state_jacobian_memo::StateJacobianMemo> {
        match &self.execution {
            RuntimeExecutionInventory::Scalar(scalar) => scalar.state_jacobian_memo.as_deref(),
            #[cfg(feature = "native-wasm")]
            RuntimeExecutionInventory::Native(_) => None,
        }
    }

    pub(crate) fn eval_state_jacobian_v_at_settled_solver_y_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        solver_y: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_state_jacobian_v_at_settled_solver_y_into(lin, solver_y, seed, out)
    }

    pub fn eval_full_jacobian_v_ad_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_full_jacobian_v_ad_into(lin, state, seed, out)
    }

    pub fn eval_forward_sensitivity_column_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        sens_column: &[f64],
        param_slot: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_forward_sensitivity_column_into(lin, state, sens_column, param_slot, out)
    }

    pub fn eval_derivatives_and_sensitivities_into(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        layout: rumoca_ir_solve::SensitivityLayout,
        param_slots: &[usize],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .eval_derivatives_and_sensitivities_into(lin, state, layout, param_slots, out)
    }

    pub fn project_state_sensitivity_to_solver_y(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        state_sensitivity: &[f64],
        param_slot: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .project_state_sensitivity_to_solver_y(lin, state, state_sensitivity, param_slot, out)
    }

    pub fn project_state_tangent_to_solver_y(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        state_sensitivity: &[f64],
        param_slot: Option<usize>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?.project_state_tangent_to_solver_y(
            lin,
            state,
            state_sensitivity,
            param_slot,
            out,
        )
    }

    pub fn reverse_state_derivative_vjp(
        &self,
        lin: AlgebraicLinearization<'_>,
        state: &[f64],
        output_cotangents: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .reverse_state_derivative_vjp(lin, state, output_cotangents, out)
    }

    pub fn reverse_implicit_residual_vjp(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        output_cotangents: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?.reverse_implicit_residual_vjp(
            t,
            solver_y,
            params,
            output_cotangents,
            out,
        )
    }

    pub fn apply_steady_residual_transpose(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        lambda: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .apply_steady_residual_transpose(t, solver_y, params, lambda, out)
    }

    pub fn observe_warnings(&self, y: &[f64], p: &[f64], t: f64) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?.observe_warnings(y, p, t)
    }

    pub fn apply_projected_post_initial_event_update<P>(
        &self,
        input: ProjectedPostInitialEventInput<'_>,
        project_algebraics: P,
    ) -> Result<EventActionOutcome, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        self.scalar_execution()?
            .apply_projected_post_initial_event_update(input, project_algebraics)
    }

    pub fn apply_projected_initial_event_boundary<P>(
        &self,
        input: ProjectedInitialEventInput<'_>,
        project_algebraics: P,
    ) -> Result<ProjectedInitialEventOutcome, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64], f64) -> Result<bool, RuntimeSolveError>,
    {
        self.scalar_execution()?
            .apply_projected_initial_event_boundary(input, project_algebraics)
    }

    pub fn reduced_chart_dependent_conditioning(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        folding_rows: &[usize],
        group_cols: &[usize],
        chart_dependent_positions: &[Vec<usize>],
    ) -> Result<Vec<DependentConditioning>, RuntimeSolveError> {
        self.scalar_execution()?
            .reduced_chart_dependent_conditioning(
                t,
                solver_y,
                params,
                folding_rows,
                group_cols,
                chart_dependent_positions,
            )
    }

    pub fn evaluate_implicit_residual_rows(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        rows: &[usize],
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        self.scalar_execution()?
            .evaluate_implicit_residual_rows(t, solver_y, params, rows)
    }

    pub fn implicit_state_binding_rows(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        state_count: usize,
    ) -> Result<Vec<usize>, RuntimeSolveError> {
        self.scalar_execution()?
            .implicit_state_binding_rows(t, solver_y, params, state_count)
    }

    pub fn refresh_algebraic_and_output_slots_certified(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .refresh_algebraic_and_output_slots_certified(t, solver_y, params, tol, max_iters)
    }

    pub(crate) fn complete_algebraic_after_derivative(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .complete_algebraic_after_derivative(t, solver_y, params, tol, max_iters)
    }

    pub fn refresh_event_dependency_slots_certified(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .refresh_event_dependency_slots_certified(t, solver_y, params, tol, max_iters)
    }

    pub fn refresh_algebraic_and_output_slots(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .refresh_algebraic_and_output_slots(t, solver_y, params, tol, max_iters)
    }

    pub fn project_state_manifold(
        &self,
        solver_y: &mut [f64],
        params: &[f64],
        t: f64,
        tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        self.scalar_execution()?
            .project_state_manifold(solver_y, params, t, tol)
    }

    pub fn certify_state_manifold(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tol: f64,
    ) -> Result<(), RuntimeSolveError> {
        self.scalar_execution()?
            .certify_state_manifold(y, p, t, tol)
    }

    /// Whether any initialization update row reads an algebraic coordinate.
    ///
    /// States occupy the first `state_scalar_count` solver-vector slots and
    /// algebraic/output coordinates occupy the remainder, so an update program
    /// that loads a solver slot at or beyond the state count depends on a
    /// coordinate the continuous equations determine rather than a state the
    /// projection owns. Such an update needs the algebraics reconstructed into
    /// the coordinate vector before it re-reads them (see the caller). Both the
    /// scalar solver-vector load and the packed tensor load reach the algebraic
    /// range; a vector-valued binding (an analytic-loop geometry read, say) is
    /// lowered to the tensor form, so testing only the scalar load would miss
    /// exactly the reads that select an assembly branch. Startup quotient Maps
    /// are update producers too and are tested alongside the update rows.
    pub fn initialization_updates_read_algebraic_slots(&self) -> bool {
        initial_projection::updates_read_algebraic_slots(&self.model)
            || self
                .scalar_execution()
                .is_ok_and(|view| view.initial_quotient_maps_read_algebraic_slots())
    }

    pub fn dynamic_time_event_stop_reads_solver_y(&self) -> bool {
        !self.model.problem.events.dynamic_time_event_rhs.is_empty()
    }

    pub fn reset_diagnostics(&self) {
        *self.warning_log.borrow_mut() = warnings::WarningLog::default();
    }

    pub fn diagnostics(&self) -> Vec<crate::SimDiagnostic> {
        self.warning_log.borrow().diagnostics()
    }

    pub fn diagnostics_since(&self, first: usize) -> (Vec<crate::SimDiagnostic>, usize) {
        self.warning_log.borrow().diagnostics_since(first)
    }

    pub fn set_initial_event_flag(&self, p: &mut [f64], value: bool) {
        super::set_initial_event_flag(&self.model, p, value);
    }

    pub fn requires_state_manifold_projection(&self) -> bool {
        !self
            .model
            .problem
            .continuous
            .manifold_projection_plan
            .is_empty()
    }
}

impl SolveRuntime {
    pub fn solver_variable_index(&self, name: &str) -> Option<usize> {
        self.model.problem.solver_index(name)
    }
    pub fn implicit_residual_row_count(&self) -> usize {
        self.model.problem.continuous.implicit_row_targets.len()
    }
    pub fn derivative_settled_coordinate_can_refresh_roots(
        &self,
    ) -> Result<bool, RuntimeSolveError> {
        Ok(self
            .scalar_execution()?
            .derivative_settled_coordinate_can_refresh_roots())
    }
    pub(crate) fn algebraic_settled_coordinate_can_refresh_roots(
        &self,
    ) -> Result<bool, RuntimeSolveError> {
        Ok(self
            .scalar_execution()?
            .algebraic_settled_coordinate_can_refresh_roots())
    }
    pub(crate) fn binding_row_source(
        &self,
        row: usize,
        state: usize,
    ) -> Result<Option<usize>, RuntimeSolveError> {
        Ok(self.scalar_execution()?.binding_row_source(row, state))
    }
    pub fn interpreted_schedule_count(&self) -> usize {
        match &self.execution {
            RuntimeExecutionInventory::Scalar(scalar) => ScalarExecutionView {
                runtime: self,
                scalar,
            }
            .interpreted_schedule_count(),
            #[cfg(feature = "native-wasm")]
            RuntimeExecutionInventory::Native(_) => 0,
        }
    }
}
