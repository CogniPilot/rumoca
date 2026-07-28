//! Runtime orchestration for projection, events, and visible outputs.

use indexmap::IndexMap;
use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;
use std::{
    cell::RefCell,
    collections::{BTreeSet, HashMap},
};

use crate::runtime::delay::DelayRuntime;
use crate::runtime::solve_events::{
    apply_discrete_slot_values, current_dynamic_time_event_stop, eval_event_actions_with_context,
    next_runtime_event_stop, visible_values_with_context,
};
use crate::{
    EventActionOutcome, ImplicitProjectionModel, ManifoldProjectionModel, RuntimeEventStop,
    RuntimeSolveError, SolveStopSchedule, project_algebraic_seed_with_plan,
    project_algebraics_with_plan, push_visible_values, replace_last_visible_values,
    timeline::sample_time_match_with_tol, update_relation_memory_slots,
};
use rumoca_eval_solve::refresh_plan::{
    AlgebraicRefreshRow, RefreshPlan, build_algebraic_refresh_plan, build_derivative_refresh_plan,
    build_root_refresh_plan, trace_refresh_plan,
};
use rumoca_eval_solve::{
    EvalSolveError, PreparedComputeBlock, PreparedScalarProgramBlock, RowEvalContext,
    to_scalar_program_block,
};

mod coupled_event;
mod discrete_rows;
mod event_update;
mod initial_event;
mod initial_projection;
mod plans;
mod refresh_batch;
mod sensitivity;
mod support;
use event_update::{DiscretePreSnapshot, DiscreteRowsSettleInput};
pub use event_update::{EventUpdateRowFilter, ProjectedEventUpdateInput};
pub use initial_event::{
    InitialEventObservation, ProjectedInitialEventInput, ProjectedInitialEventOutcome,
};
use plans::{
    RootConditionPlan, RootConditionPlanEntry, VisibleValuePlan, VisibleValuePlanEntry,
    copy_grouped_expression_values, direct_time_root_search_default, direct_time_root_time,
    direct_time_root_value, direct_visible_value, root_condition_plan, visible_value_plan,
};
use support::{
    copy_runtime_values, copy_runtime_values_into, reserve_runtime_index_map_capacity,
    reserve_runtime_vec_capacity, resize_runtime_values, zero_runtime_values,
};

mod refresh_projection;
use refresh_projection::*;

impl From<solve_eval::EvalSolveError> for RuntimeSolveError {
    fn from(value: solve_eval::EvalSolveError) -> Self {
        Self::solve_ir_with_span(value.to_string(), value.source_span())
    }
}

#[derive(Clone)]
pub struct SolveRuntime {
    pub model: solve::SolveModel,
    pub state_count: usize,
    pub solver_count: usize,
    implicit_rhs: PreparedComputeBlock,
    implicit_projection_jacobian_v: PreparedComputeBlock,
    implicit_projection_scalar_jacobian_v: PreparedScalarProgramBlock,
    implicit_scalar_rhs: PreparedScalarProgramBlock,
    manifold_residual: PreparedComputeBlock,
    manifold_jacobian_v: PreparedComputeBlock,
    initial_residual: PreparedComputeBlock,
    initial_residual_jacobian_v: PreparedComputeBlock,
    initial_scalar_residual: PreparedScalarProgramBlock,
    derivative_rhs: PreparedComputeBlock,
    /// Forward-mode AD Jacobian-vector product of `derivative_rhs`
    /// (`d(der)/d(y)·v`), lowered to `LinearOp`s with `LoadSeed`. Applied — with a
    /// seed completed by `seed_refresh_derivative_dependencies` — to form
    /// the exact state Jacobian for the state-only BDF path.
    derivative_jacobian_v: PreparedScalarProgramBlock,
    /// Primal state-derivative scalar program `der = f(solver_y, p, t)`. Reversed
    /// by [`Self::reverse_state_derivative_vjp`] to form the reverse-mode VJP
    /// `(∂der/∂[solver_y|p])ᵀ·λ` (Track A scalar reverse core).
    derivative_scalar: PreparedScalarProgramBlock,
    /// Per-row forward-mode AD Jacobian-vector product of `implicit_rhs`
    /// (`d(residual_row)/d[y|p]·v`). Used to propagate state and parameter seeds
    /// through the algebraic projection row by row.
    implicit_jacobian_v: PreparedScalarProgramBlock,
    algebraic_refresh: RefreshPlan,
    derivative_refresh: RefreshPlan,
    root_refresh: RefreshPlan,
    root_condition_rows: PreparedScalarProgramBlock,
    root_condition_plan: Option<RootConditionPlan>,
    discrete_rhs: PreparedScalarProgramBlock,
    visible_name_index: HashMap<String, usize>,
    visible_value_rows: PreparedScalarProgramBlock,
    visible_value_plan: Option<VisibleValuePlan>,
    visible_scratch: RefCell<Vec<f64>>,
    refresh_probe_scratch: RefCell<Vec<f64>>,
    refresh_tensor_scratch: RefCell<Vec<f64>>,
    static_refresh_cache: RefCell<StaticRefreshCache>,
    runtime_state: solve_eval::SimulationRuntimeState,
    delay_runtime: DelayRuntime,
    root_condition_count: usize,
    derivative_scratch: RefCell<StateDerivativeScratch>,
    root_scratch: RefCell<Vec<f64>>,
    /// Reusable register tape / adjoint buffers for the reverse-mode VJP sweep,
    /// kept across calls so a hot reverse loop stays allocation-free.
    reverse_scratch: RefCell<solve_eval::reverse::ReverseScratch>,
}

fn prepare_manifold_projection_programs(
    model: &solve::SolveModel,
) -> Result<(PreparedComputeBlock, PreparedComputeBlock), EvalSolveError> {
    Ok((
        PreparedComputeBlock::new_with_label(
            &model.problem.continuous.manifold_residual,
            "runtime_manifold_residual",
        )?,
        PreparedComputeBlock::new_with_label(
            &model.artifacts.continuous.manifold_jacobian_v,
            "runtime_manifold_jacobian_v",
        )?,
    ))
}

impl SolveRuntime {
    pub fn new(model: &solve::SolveModel) -> Result<Self, EvalSolveError> {
        let implicit_scalar_programs =
            to_scalar_program_block(&model.problem.continuous.implicit_rhs)?;
        let implicit_scalar_rhs = PreparedScalarProgramBlock::new(implicit_scalar_programs)?;
        let (manifold_residual, manifold_jacobian_v) = prepare_manifold_projection_programs(model)?;
        let derivative_scalar_rhs =
            to_scalar_program_block(&model.problem.continuous.derivative_rhs)?;
        let algebraic_refresh = build_algebraic_refresh_plan(model, &implicit_scalar_rhs)?;
        let derivative_refresh =
            build_derivative_refresh_plan(model, &derivative_scalar_rhs, &algebraic_refresh)?;
        let root_refresh = build_root_refresh_plan(model, &algebraic_refresh)?;
        trace_refresh_plan(model, "algebraic", &algebraic_refresh);
        trace_refresh_plan(model, "derivative", &derivative_refresh);
        trace_refresh_plan(model, "root", &root_refresh);
        trace_reverse_projection_coverage(model, &implicit_scalar_rhs);
        let visible_value_plan = visible_value_plan(model);
        let root_condition_plan = root_condition_plan(model, &root_refresh);
        let delay_runtime = DelayRuntime::new(&model.problem.events.delays)?;
        let root_condition_count = model
            .problem
            .events
            .root_conditions
            .len()
            .checked_add(delay_runtime.event_root_count())
            .ok_or_else(|| EvalSolveError::ShapeContract {
                message: "combined model and delay root count exceeds host index range".to_string(),
                span: None,
            })?;
        Ok(Self {
            model: model.clone(),
            state_count: model.state_scalar_count(),
            solver_count: model.solver_scalar_count(),
            implicit_rhs: PreparedComputeBlock::new_with_label(
                &model.problem.continuous.implicit_rhs,
                "runtime_implicit_rhs",
            )?,
            implicit_projection_jacobian_v: PreparedComputeBlock::new_with_label(
                &model.artifacts.continuous.implicit_jacobian_v,
                "runtime_implicit_projection_jacobian_v",
            )?,
            implicit_projection_scalar_jacobian_v: PreparedScalarProgramBlock::new(
                to_scalar_program_block(&model.artifacts.continuous.implicit_jacobian_v)?,
            )?,
            implicit_scalar_rhs,
            manifold_residual,
            manifold_jacobian_v,
            initial_residual: PreparedComputeBlock::new_with_label(
                &model.problem.initialization.residual,
                "runtime_initial_residual",
            )?,
            initial_residual_jacobian_v: PreparedComputeBlock::new_with_label(
                &model.artifacts.initialization.residual_jacobian_v,
                "runtime_initial_residual_jacobian_v",
            )?,
            initial_scalar_residual: PreparedScalarProgramBlock::new(to_scalar_program_block(
                &model.problem.initialization.residual,
            )?)?,
            derivative_rhs: PreparedComputeBlock::new_with_label(
                &model.problem.continuous.derivative_rhs,
                "runtime_derivative_rhs",
            )?,
            derivative_jacobian_v: PreparedScalarProgramBlock::new(
                model.artifacts.continuous.full_jacobian_v.clone(),
            )?,
            derivative_scalar: PreparedScalarProgramBlock::new(derivative_scalar_rhs)?,
            implicit_jacobian_v: PreparedScalarProgramBlock::new(
                model
                    .artifacts
                    .continuous
                    .implicit_jacobian_v_scalar
                    .clone(),
            )?,
            algebraic_refresh,
            derivative_refresh,
            root_refresh,
            root_condition_rows: PreparedScalarProgramBlock::new(
                model.problem.events.root_conditions.clone(),
            )?,
            root_condition_plan,
            discrete_rhs: PreparedScalarProgramBlock::new(model.problem.discrete.rhs.clone())?,
            visible_name_index: model
                .visible_names
                .iter()
                .enumerate()
                .map(|(idx, name)| (name.clone(), idx))
                .collect(),
            visible_value_rows: PreparedScalarProgramBlock::new(model.visible_value_rows.clone())?,
            visible_value_plan,
            visible_scratch: RefCell::new(Vec::new()),
            refresh_probe_scratch: RefCell::new(Vec::new()),
            refresh_tensor_scratch: RefCell::new(Vec::new()),
            static_refresh_cache: RefCell::new(StaticRefreshCache::default()),
            runtime_state: solve_eval::SimulationRuntimeState::new(),
            delay_runtime,
            root_condition_count,
            derivative_scratch: RefCell::new(StateDerivativeScratch::default()),
            root_scratch: RefCell::new(Vec::new()),
            reverse_scratch: RefCell::new(solve_eval::reverse::ReverseScratch::default()),
        })
    }

    pub fn row_eval_context(&self) -> RowEvalContext<'_> {
        RowEvalContext {
            external_tables: Some(self.model.external_tables.as_slice()),
            runtime_state: Some(&self.runtime_state),
            ..Default::default()
        }
    }

    pub fn has_delay_channels(&self) -> bool {
        !self.delay_runtime.is_empty()
    }

    pub fn reset_delay_history(&self) {
        self.delay_runtime.reset();
    }

    pub fn initialize_delay_history(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.delay_runtime
            .initialize(time, solver_y, params, self.row_eval_context())
            .map_err(Into::into)
    }

    pub fn refresh_delay_values(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &mut [f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        self.delay_runtime
            .refresh(time, solver_y, params, self.row_eval_context())
            .map_err(Into::into)
    }

    pub fn commit_delay_history(
        &self,
        time: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        self.delay_runtime
            .commit(time, solver_y, params, self.row_eval_context())
            .map_err(Into::into)
    }

    /// Commit an accepted delay-history point whose source expression must be
    /// evaluated at a different time coordinate.
    ///
    /// Event left limits are owned by the event instant in accepted history,
    /// but their expressions are evaluated at the previous representable time.
    pub fn commit_delay_history_evaluated_at(
        &self,
        accepted_time: f64,
        evaluation_time: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        self.delay_runtime
            .commit_evaluated_at(
                accepted_time,
                evaluation_time,
                solver_y,
                params,
                self.row_eval_context(),
            )
            .map_err(Into::into)
    }

    pub fn delay_step_limit(&self) -> Option<f64> {
        self.delay_runtime.step_limit()
    }

    pub fn root_condition_count(&self) -> usize {
        self.root_condition_count
    }

    pub fn full_solver_y(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut solver_y = Vec::new();
        self.populate_solver_y_from_state(&mut solver_y, state)?;
        self.refresh_algebraic_and_output_slots(t, &mut solver_y, params, tol, max_iters)?;
        Ok(solver_y)
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
        self.populate_solver_y_from_state(solver_y, state)?;
        self.refresh_algebraic_and_output_slots(t, solver_y, params, tol, max_iters)
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
        self.update_solver_y_guess_from_state(guess, state)?;
        self.refresh_algebraic_and_output_slots(t, guess, params, tol, max_iters)
    }

    /// Update an established full-layout guess for state-derivative evaluation.
    ///
    /// State-only integrators use this after an accepted step to preserve a
    /// warm start for the next RHS/Jacobian call. Only the compiler-proven
    /// derivative dependency closure is refreshed; observation-only
    /// algebraics are reconstructed at output or event boundaries instead of
    /// entering the integration hot loop.
    pub fn refresh_derivative_solver_y_with_guess(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.update_solver_y_guess_from_state(guess, state)?;
        self.refresh_derivative_dependencies(t, guess, params, tol, max_iters)
    }

    fn refresh_derivative_dependencies(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_slots_with_plan(
            &self.derivative_refresh,
            RefreshSlotArgs {
                t,
                solver_y,
                params,
                tol,
                max_iters,
            },
        )
    }

    pub fn refresh_algebraic_and_output_slots(
        &self,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_slots_with_plan(
            &self.algebraic_refresh,
            RefreshSlotArgs {
                t,
                solver_y,
                params,
                tol,
                max_iters,
            },
        )
    }

    fn refresh_slots_with_plan(
        &self,
        plan: &RefreshPlan,
        mut args: RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        self.validate_refresh_inputs(args.solver_y, args.params)?;
        if plan.rows.is_empty() && plan.simultaneous_plan.is_empty() {
            return Ok(());
        }
        let incoming = copy_runtime_values(args.solver_y, "algebraic projection snapshot")?;
        let mut causal_refresh_succeeded = plan.rows.is_empty();
        let mut causal_seed_failed = false;
        if !plan.causal_seed_rows.is_empty() {
            match self.refresh_causal_seed_rows(plan, &mut args) {
                Ok(()) => causal_refresh_succeeded = true,
                Err(error) => {
                    restore_after_causal_seed_error(error, args.solver_y, &incoming)?;
                    causal_seed_failed = true;
                }
            }
        }
        if causal_refresh_succeeded && plan.causal_solution_certified {
            return Ok(());
        }
        let result = self.project_refresh_slots(plan, &mut args, causal_seed_failed);
        if result.is_err() {
            args.solver_y.copy_from_slice(&incoming);
        }
        result
    }

    fn refresh_causal_seed_rows(
        &self,
        plan: &RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_parameter_static_seed_rows(
            &plan.static_causal_seed_rows,
            args.t,
            args.solver_y,
            args.params,
        )?;
        self.refresh_slots_once(
            &plan.dynamic_causal_seed_rows,
            args.t,
            args.solver_y,
            args.params,
        )
    }

    fn refresh_parameter_static_seed_rows(
        &self,
        rows: &[AlgebraicRefreshRow],
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if rows.is_empty() {
            return Ok(());
        }
        let mut cache = self.static_refresh_cache.borrow_mut();
        let params_match = cache.valid
            && cache.params.len() == params.len()
            && cache
                .params
                .iter()
                .zip(params)
                .all(|(lhs, rhs)| lhs.to_bits() == rhs.to_bits());
        if !params_match {
            cache.valid = true;
            cache.params.clear();
            cache.params.extend_from_slice(params);
            cache.values.clear();
            cache.values.resize(solver_y.len(), None);
        }
        let fully_cached = rows.iter().all(|row| {
            cache
                .values
                .get(row.target_index)
                .is_some_and(Option::is_some)
        });
        if fully_cached {
            for row in rows {
                solver_y[row.target_index] = cached_static_refresh_value(&cache, row.target_index)?;
            }
            return Ok(());
        }
        drop(cache);

        self.refresh_slots_once(rows, t, solver_y, params)?;
        let mut cache = self.static_refresh_cache.borrow_mut();
        for row in rows {
            cache.values[row.target_index] = Some(solver_y[row.target_index]);
        }
        Ok(())
    }

    fn project_refresh_slots(
        &self,
        plan: &RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
        use_complete_plan: bool,
    ) -> Result<(), RuntimeSolveError> {
        let projection_plan = if use_complete_plan {
            &plan.simultaneous_plan
        } else {
            &plan.value_projection_plan
        };
        let projection_model = RefreshProjectionModel {
            runtime: self,
            plan: projection_plan,
            jacobian_v: ProjectionJacobian::SolverY {
                block: &self.implicit_projection_jacobian_v,
                scalar: &self.implicit_projection_scalar_jacobian_v,
            },
        };
        project_algebraics_with_plan(
            &projection_model,
            projection_plan,
            args.solver_y,
            crate::AlgebraicProjectionArgs {
                parameters: args.params,
                time: args.t,
                state_count: self.state_count,
                tolerance: args.tol,
            },
            args.max_iters,
        )
    }

    /// Project accepted state values onto lower-order constraints retained by
    /// structural index reduction.
    pub fn project_state_manifold(
        &self,
        solver_y: &mut [f64],
        params: &[f64],
        t: f64,
        tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let projection_model = RuntimeManifoldProjection { runtime: self };
        crate::project_state_manifold(
            &projection_model,
            solver_y,
            params,
            t,
            self.state_count,
            tol,
        )
    }

    fn validate_refresh_inputs(
        &self,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if self.implicit_rhs.len() < self.solver_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "implicit RHS has {} rows for {} solver variables",
                self.implicit_rhs.len(),
                self.solver_count
            )));
        }
        solve_eval::validate_input_requirements(
            self.implicit_scalar_rhs.requirements(),
            solver_y,
            params,
            None,
        )?;
        Ok(())
    }

    fn eval_refresh_row(
        &self,
        row: &AlgebraicRefreshRow,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let index = row.target_index;
        let value = self.eval_refresh_row_value(row, t, solver_y, params)?;
        // Catch non-finite results here (where the variable is known) and raise
        // a spanned diagnostic; otherwise a NaN slips through the iteration (the
        // `delta > max_delta` check is false for NaN) and only surfaces later as
        // an opaque "step size too small".
        if !value.is_finite() {
            return Err(self.non_finite_value_error(index, value));
        }
        Ok(value)
    }

    /// Solver slot name for diagnostics.
    fn solver_name(&self, index: usize) -> &str {
        self.model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(index)
            .map_or("<unnamed>", String::as_str)
    }

    /// Build a spanned non-finite-value error, resolving the solver slot's name
    /// and source span (from `variable_meta`) so the failure is traceable.
    fn non_finite_value_error(&self, index: usize, value: f64) -> RuntimeSolveError {
        let name = self
            .model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(index)
            .cloned()
            .unwrap_or_else(|| format!("y[{index}]"));
        let span = self.solver_source_span(index);
        let kind = if value.is_nan() { "NaN" } else { "inf" };
        RuntimeSolveError::NonFiniteValue { name, kind, span }
    }

    fn solver_source_span(&self, index: usize) -> Option<rumoca_core::Span> {
        let name = self
            .model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(index)?;
        self.model
            .variable_meta
            .iter()
            .find(|meta| &meta.name == name)
            .map(|meta| meta.source_span)
    }

    fn eval_refresh_row_value(
        &self,
        row: &AlgebraicRefreshRow,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let index = row.target_index;
        // The assignment fast path is only valid when this plan entry updates
        // the row's own implicit target; for a cross-paired row (a coupled
        // block solved a residual row for one of its other unknowns) the
        // assignment value belongs to a different variable.
        if row.assignment_target == Some(index)
            && row.output_offset == 0
            && let Some(value) = self
                .implicit_scalar_rhs
                .eval_target_assignment_row_unchecked_with_context(
                    row.row_idx,
                    index,
                    solver_y,
                    params,
                    t,
                    self.row_eval_context(),
                )?
        {
            return Ok(value);
        }
        let residual = self.refresh_row_residual(row, t, solver_y, params)?;
        self.solve_refresh_residual_row(row, residual, t, solver_y, params)
    }

    /// Evaluate one scalar view of the canonical implicit residual system.
    fn refresh_row_residual(
        &self,
        row: &AlgebraicRefreshRow,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        self.implicit_scalar_rhs
            .eval_row_output_unchecked_with_context(
                row.row_idx,
                row.output_offset,
                solver_y,
                params,
                t,
                self.row_eval_context(),
            )
            .map_err(Into::into)
    }

    fn solve_refresh_residual_row(
        &self,
        row: &AlgebraicRefreshRow,
        residual: f64,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let index = row.target_index;
        let current = solver_y[index];
        let mut probe_y = self.refresh_probe_scratch.borrow_mut();
        probe_y.clear();
        reserve_runtime_vec_capacity(&mut probe_y, solver_y.len(), "refresh residual probe")?;
        probe_y.extend_from_slice(solver_y);
        probe_y[index] = current + 1.0;
        let probe_residual = self.refresh_row_residual(row, t, &probe_y, params)?;
        let slope = probe_residual - residual;
        if slope.is_finite() && slope.abs() > 1.0e-12 {
            return Ok(current - residual / slope);
        }
        // A residual that does not respond to the paired variable means the
        // refresh plan paired this row with a variable it cannot determine.
        // Nudging the value by the residual (the old fallback) converges to a
        // wrong but stable solution; fail loudly instead.
        Err(RuntimeSolveError::RefreshTargetUnassignable {
            row: row.row_idx,
            target: self.solver_name(index).to_string(),
            span: self.solver_source_span(index),
        })
    }

    fn refresh_slots_once(
        &self,
        plan: &[AlgebraicRefreshRow],
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if self.can_batch_assignment_refresh(plan) {
            self.implicit_scalar_rhs
                .apply_target_assignment_rows_unchecked_with_context(
                    plan,
                    solver_y,
                    params,
                    t,
                    self.row_eval_context(),
                )
                .map_err(RuntimeSolveError::from)?;
            self.validate_refresh_values(plan, solver_y)?;
            return Ok(());
        }
        let mut row_outputs = Vec::new();
        let mut row_pos = 0usize;
        while row_pos < plan.len() {
            if let Some(next_pos) =
                self.try_refresh_tensor_output_segment(plan, row_pos, t, solver_y, params)?
            {
                row_pos = next_pos;
                continue;
            }
            if let Some(next_pos) = self.try_refresh_shapeless_output_segment(
                plan,
                row_pos,
                t,
                solver_y,
                params,
                &mut row_outputs,
            )? {
                row_pos = next_pos;
                continue;
            }
            let refresh_row = &plan[row_pos];
            let index = refresh_row.target_index;
            let value = self.eval_refresh_row(refresh_row, t, solver_y, params)?;
            solver_y[index] = value;
            row_pos += 1;
        }
        Ok(())
    }

    fn validate_refresh_values(
        &self,
        plan: &[AlgebraicRefreshRow],
        solver_y: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        for row in plan {
            let value = solver_y[row.target_index];
            if !value.is_finite() {
                return Err(self.non_finite_value_error(row.target_index, value));
            }
        }
        Ok(())
    }

    pub fn eval_state_derivatives(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut derivative = zero_runtime_values(self.state_count, "state derivative output")?;
        self.eval_state_derivatives_into(t, state, params, tol, max_iters, &mut derivative)?;
        Ok(derivative)
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
        let mut scratch = self.derivative_scratch.borrow_mut();
        let solver_y = &mut scratch.solver_y;
        self.populate_solver_y_from_state(solver_y, state)?;
        self.eval_state_derivatives_at_solver_y(t, params, tol, max_iters, solver_y, out)
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
        let mut derivative = zero_runtime_values(self.state_count, "state derivative output")?;
        self.eval_state_derivatives_with_guess_into(
            t,
            state,
            params,
            guess,
            tol,
            max_iters,
            &mut derivative,
        )?;
        Ok(derivative)
    }

    // SPEC_0021: Exception - public runtime API mirrors solver callback inputs
    // without hiding mutable scratch/output buffers behind allocation.
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
        self.update_solver_y_guess_from_state(guess, state)?;
        self.refresh_derivative_dependencies(t, guess, params, tol, max_iters)?;
        self.eval_derivative_rhs_from_solver_y(t, guess, params, out)
    }

    pub fn eval_root_conditions(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return Ok(Vec::new());
        }
        let mut values = zero_runtime_values(root_count, "root condition output")?;
        self.eval_root_conditions_into(t, state, params, tol, max_iters, &mut values)?;
        Ok(values)
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
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return fill_inactive_root_output(out);
        }
        validate_runtime_output_len("root condition output", root_count, out.len())?;
        let model_root_count = self.model.problem.events.root_conditions.len();
        let mut solver_y = self.root_scratch.borrow_mut();
        self.populate_solver_y_from_state(&mut solver_y, state)?;
        if self.delay_runtime.event_root_count() > 0 {
            self.refresh_algebraic_and_output_slots(t, &mut solver_y, params, tol, max_iters)?;
        }
        self.refresh_slots_with_plan(
            &self.root_refresh,
            RefreshSlotArgs {
                t,
                solver_y: &mut solver_y,
                params,
                tol,
                max_iters,
            },
        )?;
        self.eval_root_conditions_from_refreshed_solver_y(
            t,
            &solver_y,
            params,
            &mut out[..model_root_count],
        )?;
        self.delay_runtime
            .evaluate_event_roots(
                t,
                &solver_y,
                params,
                self.row_eval_context(),
                &mut out[model_root_count..],
            )
            .map_err(RuntimeSolveError::from)?;
        validate_finite_runtime_output("root condition output", out)
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
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return fill_inactive_root_output(out);
        }
        validate_runtime_output_len("root search output", root_count, out.len())?;
        let model_root_count = self.model.problem.events.root_conditions.len();
        let mut solver_y = self.root_scratch.borrow_mut();
        self.populate_solver_y_from_state(&mut solver_y, state)?;
        if self.delay_runtime.event_root_count() > 0 {
            self.refresh_algebraic_and_output_slots(t, &mut solver_y, params, tol, max_iters)?;
        }
        if model_root_count > 0 {
            let model_out = &mut out[..model_root_count];
            let Some(plan) = &self.root_condition_plan else {
                self.refresh_slots_with_plan(
                    &self.root_refresh,
                    RefreshSlotArgs {
                        t,
                        solver_y: &mut solver_y,
                        params,
                        tol,
                        max_iters,
                    },
                )?;
                self.eval_root_conditions_from_refreshed_solver_y(t, &solver_y, params, model_out)?;
                self.delay_runtime
                    .evaluate_event_roots(
                        t,
                        &solver_y,
                        params,
                        self.row_eval_context(),
                        &mut out[model_root_count..],
                    )
                    .map_err(RuntimeSolveError::from)?;
                return validate_finite_runtime_output("root search output", out);
            };
            self.validate_root_plan_output_len(plan, model_out)?;
            if plan.search_rows.is_empty() {
                self.write_planned_root_search_defaults(plan, params, t, model_out)?;
            } else {
                self.refresh_slots_with_plan(
                    &self.root_refresh,
                    RefreshSlotArgs {
                        t,
                        solver_y: &mut solver_y,
                        params,
                        tol,
                        max_iters,
                    },
                )?;
                self.write_planned_root_search_conditions(plan, &solver_y, params, t, model_out)?;
            }
        }
        self.delay_runtime
            .evaluate_event_roots(
                t,
                &solver_y,
                params,
                self.row_eval_context(),
                &mut out[model_root_count..],
            )
            .map_err(RuntimeSolveError::from)?;
        validate_finite_runtime_output("root search output", out)
    }

    pub fn next_planned_time_root(
        &self,
        params: &[f64],
        current_t: f64,
        target: f64,
        tol: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(plan) = &self.root_condition_plan else {
            return Ok(None);
        };
        let mut next = None;
        for entry in &plan.entries {
            let RootConditionPlanEntry::DirectTime(root) = entry else {
                continue;
            };
            let event_time = direct_time_root_time(*root, params)?;
            if !event_time.is_finite() {
                continue;
            }
            if event_time > current_t + tol
                && (event_time < target || sample_time_match_with_tol(event_time, target))
            {
                next = Some(next.map_or(event_time, |current: f64| current.min(event_time)));
            }
        }
        Ok(next)
    }

    fn eval_root_conditions_from_refreshed_solver_y(
        &self,
        t: f64,
        y: &[f64],
        p: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if let Some(plan) = &self.root_condition_plan {
            return self.write_planned_root_conditions(plan, y, p, t, out);
        }
        self.root_condition_rows
            .eval_with_context(y, p, t, self.row_eval_context(), out)
            .map_err(Into::into)
    }

    fn write_planned_root_conditions(
        &self,
        plan: &RootConditionPlan,
        y: &[f64],
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.validate_root_plan_output_len(plan, out)?;
        for (slot, entry) in out.iter_mut().zip(plan.entries.iter().copied()) {
            *slot = match entry {
                RootConditionPlanEntry::ConstantNonZero(value) => value,
                RootConditionPlanEntry::DirectTime(root) => {
                    direct_time_root_value(root, params, t)?
                }
                RootConditionPlanEntry::StaticParameter => 0.0,
                RootConditionPlanEntry::Dynamic => 0.0,
            };
        }
        self.eval_planned_root_rows(&plan.evaluated_rows, y, params, t, out)
    }

    fn write_planned_root_search_conditions(
        &self,
        plan: &RootConditionPlan,
        y: &[f64],
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.write_planned_root_search_defaults(plan, params, t, out)?;
        self.eval_planned_root_rows(&plan.search_rows, y, params, t, out)
    }

    fn write_planned_root_search_defaults(
        &self,
        plan: &RootConditionPlan,
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.validate_root_plan_output_len(plan, out)?;
        for (slot, entry) in out.iter_mut().zip(plan.entries.iter().copied()) {
            *slot = match entry {
                RootConditionPlanEntry::ConstantNonZero(_)
                | RootConditionPlanEntry::StaticParameter
                | RootConditionPlanEntry::Dynamic => 1.0,
                RootConditionPlanEntry::DirectTime(root) => {
                    direct_time_root_search_default(root, params, t)?
                }
            };
        }
        Ok(())
    }

    fn eval_planned_root_rows(
        &self,
        row_indices: &[usize],
        y: &[f64],
        params: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if row_indices.is_empty() {
            return Ok(());
        }
        self.root_condition_rows
            .eval_single_output_rows_unchecked_with_context(
                row_indices,
                y,
                params,
                t,
                self.row_eval_context(),
                out,
            )
            .map_err(Into::into)
    }

    fn validate_root_plan_output_len(
        &self,
        plan: &RootConditionPlan,
        out: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if out.len() >= plan.entries.len() {
            return Ok(());
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "root condition plan output index {} out of bounds for {} values",
            plan.entries.len().saturating_sub(1),
            out.len()
        )))
    }

    pub fn update_relation_memory_from_state(
        &self,
        t: f64,
        state: &[f64],
        params: &mut [f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        let relation_memory_indices = &self
            .model
            .problem
            .solve_layout
            .relation_memory_parameter_indices;
        if relation_memory_indices.is_empty() {
            return Ok(false);
        }
        let roots = self.eval_root_conditions(t, state, params, tol, max_iters)?;
        Ok(update_relation_memory_slots(
            &roots,
            params,
            relation_memory_indices,
        ))
    }

    pub fn eval_dynamic_time_event_rows(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let block = &self.model.problem.events.dynamic_time_event_rhs;
        if block.is_empty() {
            return Ok(Vec::new());
        }
        self.eval_scalar_program_block(block, solver_y, params, t)
    }

    pub fn current_dynamic_time_event_stop(
        &self,
        y: &[f64],
        params: &[f64],
        current_t: f64,
    ) -> Result<Option<RuntimeEventStop>, RuntimeSolveError> {
        current_dynamic_time_event_stop(&self.model, &self.runtime_state, y, params, current_t)
    }

    pub fn next_runtime_event_stop(
        &self,
        y: &[f64],
        params: &[f64],
        stop_schedule: &mut SolveStopSchedule,
        current_t: f64,
        target: f64,
    ) -> Result<(f64, Option<RuntimeEventStop>), RuntimeSolveError> {
        next_runtime_event_stop(
            &self.model,
            &self.runtime_state,
            y,
            params,
            stop_schedule,
            current_t,
            target,
        )
    }

    pub fn eval_scalar_program_block(
        &self,
        block: &solve::ScalarProgramBlock,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut values = zero_runtime_values(block.len(), "scalar program block output")?;
        solve_eval::eval_scalar_program_block_with_context(
            block,
            y,
            p,
            t,
            self.row_eval_context(),
            &mut values,
        )?;
        Ok(values)
    }

    pub fn apply_initialization_updates(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        solve_eval::eval_and_apply_update_rows(solve_eval::UpdateRowApplication {
            block: &self.model.problem.initialization.update_rhs,
            targets: &self.model.problem.initialization.update_targets,
            y,
            p,
            t,
            context: self.row_eval_context(),
            tol,
            max_iters,
        })
        .map_err(Into::into)
    }

    pub fn apply_runtime_assignments_once(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
    ) -> Result<(), RuntimeSolveError> {
        let rows = &self.model.problem.discrete.runtime_assignment_rhs;
        if rows.is_empty() {
            return Ok(());
        }
        if rows.len() != self.model.problem.discrete.runtime_assignment_targets.len() {
            return Err(RuntimeSolveError::solve_ir(format!(
                "runtime assignment row count {} does not match target count {}",
                rows.len(),
                self.model.problem.discrete.runtime_assignment_targets.len()
            )));
        }
        let values = self.eval_scalar_program_block(rows, y, p, t)?;
        apply_discrete_slot_values(
            &self.model.problem.discrete.runtime_assignment_targets,
            &values,
            y,
            p,
            0.0,
        )
    }

    pub fn apply_runtime_assignments_until_stable(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        solve_eval::eval_and_apply_update_rows(solve_eval::UpdateRowApplication {
            block: &self.model.problem.discrete.runtime_assignment_rhs,
            targets: &self.model.problem.discrete.runtime_assignment_targets,
            y,
            p,
            t,
            context: self.row_eval_context(),
            tol,
            max_iters,
        })
        .map_err(Into::into)
    }

    pub fn settle_runtime_assignments_and_relation_memory(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        for _ in 0..max_iters {
            let mut changed =
                self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            changed |= self.update_relation_memory_from_solver_y(t, y, p, tol)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            if !changed {
                return Ok(());
            }
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "runtime assignments and relation memory did not converge at t={t}"
        )))
    }

    pub fn settle_projected_runtime_and_relation_memory<P>(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
        mut project_algebraics: P,
    ) -> Result<(), RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        for _ in 0..max_iters {
            let mut changed =
                self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            changed |= project_algebraics(y, p)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            changed |= self.update_relation_memory_from_solver_y(t, y, p, tol)?;
            if !changed {
                return Ok(());
            }
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "projected runtime assignments and relation memory did not converge at t={t}"
        )))
    }

    pub fn seed_initial_discrete_values(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        self.validate_discrete_event_rows()?;
        if self.model.problem.discrete.rhs.is_empty() {
            return Ok(());
        }
        let event_pre_y = copy_runtime_values(y, "event pre y snapshot")?;
        let event_pre_p = copy_runtime_values(p, "event pre p snapshot")?;
        for event_iteration in 0..max_iters {
            let iter_pre_y = copy_runtime_values(y, "event iteration y snapshot")?;
            let iter_pre_p = copy_runtime_values(p, "event iteration p snapshot")?;
            let snapshot = DiscretePreSnapshot {
                event_pre_y: &event_pre_y,
                event_pre_p: &event_pre_p,
                iter_pre_y: iter_pre_y.as_slice(),
                iter_pre_p: iter_pre_p.as_slice(),
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &[],
                event_iteration,
            };
            let changed =
                self.apply_constant_discrete_rows_for_pre_snapshot(&snapshot, y, p, t, tol)?;
            if !changed {
                return Ok(());
            }
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "initial discrete equations did not converge at t={t}"
        )))
    }

    pub fn apply_projected_event_update<P>(
        &self,
        input: ProjectedEventUpdateInput<'_>,
        mut project_algebraics: P,
    ) -> Result<EventActionOutcome, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        self.validate_discrete_event_rows()?;
        let ProjectedEventUpdateInput {
            y,
            p,
            t,
            tol,
            event_pre_y,
            event_pre_p,
            max_iters,
            row_filter,
            root_relation_overrides,
        } = input;
        if self.model.problem.discrete.rhs.is_empty() {
            self.apply_root_relation_memory_overrides(root_relation_overrides, y, p, tol)?;
            return self.settle_runtime_assignments_and_projection(
                y,
                p,
                t,
                tol,
                max_iters,
                &mut project_algebraics,
            );
        }

        for event_iteration in 0..max_iters {
            let mut changed =
                self.apply_root_relation_memory_overrides(root_relation_overrides, y, p, tol)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            changed |= project_algebraics(y, p)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            let iter_pre_y = copy_runtime_values(y, "projected event iteration y snapshot")?;
            let iter_pre_p = copy_runtime_values(p, "projected event iteration p snapshot")?;
            let snapshot = DiscretePreSnapshot {
                event_pre_y,
                event_pre_p,
                iter_pre_y: iter_pre_y.as_slice(),
                iter_pre_p: iter_pre_p.as_slice(),
                row_filter,
                root_relation_overrides,
                event_iteration,
            };
            {
                let mut settle_input = DiscreteRowsSettleInput {
                    y,
                    p,
                    t,
                    tol,
                    max_iters,
                };
                changed |= self.settle_discrete_rows_for_pre_snapshot(
                    &snapshot,
                    &mut settle_input,
                    &mut project_algebraics,
                )?;
            }
            if root_relation_overrides.is_empty() {
                changed |= self.update_relation_memory_from_solver_y(t, y, p, tol)?;
            }
            changed |=
                self.apply_root_relation_memory_overrides(root_relation_overrides, y, p, tol)?;
            changed |= project_algebraics(y, p)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            if !changed {
                return self.eval_event_actions(y, p, t);
            }
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "event update iteration did not converge at t={t}"
        )))
    }

    fn validate_discrete_event_rows(&self) -> Result<(), RuntimeSolveError> {
        let rows = self.model.problem.discrete.rhs.len();
        let targets = self.model.problem.discrete.update_targets.len();
        if rows == targets {
            return Ok(());
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "discrete RHS row count {rows} does not match target count {targets}"
        )))
    }

    fn settle_runtime_assignments_and_projection<P>(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
        project_algebraics: &mut P,
    ) -> Result<EventActionOutcome, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        for _ in 0..max_iters {
            let mut changed =
                self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            changed |= project_algebraics(y, p)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            if !changed {
                return self.eval_event_actions(y, p, t);
            }
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "event runtime assignments did not converge at t={t}"
        )))
    }

    fn override_relation_memory_row_values(
        &self,
        root_relation_overrides: &[(usize, f64)],
        row_values: &mut [(solve::ScalarSlot, f64)],
    ) {
        for (root_idx, value) in root_relation_overrides {
            let Some(Some(target)) = self
                .model
                .problem
                .events
                .root_relation_memory_targets
                .get(*root_idx)
                .copied()
            else {
                continue;
            };
            if let Some((_, row_value)) = row_values
                .iter_mut()
                .find(|(row_target, _)| *row_target == target)
            {
                *row_value = *value;
            }
        }
    }

    fn apply_root_relation_memory_overrides(
        &self,
        root_relation_overrides: &[(usize, f64)],
        y: &mut [f64],
        p: &mut [f64],
        tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let mut changed = false;
        for (root_idx, value) in root_relation_overrides {
            let Some(Some(target)) = self
                .model
                .problem
                .events
                .root_relation_memory_targets
                .get(*root_idx)
                .copied()
            else {
                continue;
            };
            changed |= solve_eval::apply_scalar_slot_value(target, *value, y, p, tol)?;
        }
        Ok(changed)
    }

    pub fn update_relation_memory_from_solver_y(
        &self,
        t: f64,
        y: &[f64],
        p: &mut [f64],
        _tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let relation_memory_indices = &self
            .model
            .problem
            .solve_layout
            .relation_memory_parameter_indices;
        if relation_memory_indices.is_empty() {
            return Ok(false);
        }
        let roots = self.eval_root_conditions_from_solver_y(t, y, p)?;
        Ok(update_relation_memory_slots(
            &roots,
            p,
            relation_memory_indices,
        ))
    }

    pub fn eval_root_conditions_from_solver_y(
        &self,
        t: f64,
        y: &[f64],
        p: &[f64],
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return Ok(Vec::new());
        }
        let model_root_count = self.model.problem.events.root_conditions.len();
        let mut values = zero_runtime_values(root_count, "root condition output")?;
        self.eval_root_conditions_from_refreshed_solver_y(
            t,
            y,
            p,
            &mut values[..model_root_count],
        )?;
        self.delay_runtime.evaluate_event_roots(
            t,
            y,
            p,
            self.row_eval_context(),
            &mut values[model_root_count..],
        )?;
        validate_finite_runtime_output("root condition output", &values)?;
        Ok(values)
    }

    pub fn eval_event_actions(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<EventActionOutcome, RuntimeSolveError> {
        eval_event_actions_with_context(
            &self.model.problem.events,
            y,
            p,
            t,
            self.row_eval_context(),
        )
    }

    pub fn record_visible_sample(
        &self,
        data: &mut [Vec<f64>],
        solver_y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<(), RuntimeSolveError> {
        let mut values = self.visible_scratch.borrow_mut();
        self.visible_values_into(solver_y, params, t, &mut values)?;
        push_visible_values(data, &values)
    }

    pub fn record_visible_sample_if_new(
        &self,
        recorded_times: &mut Vec<f64>,
        data: &mut [Vec<f64>],
        solver_y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<(), RuntimeSolveError> {
        let mut values = self.visible_scratch.borrow_mut();
        self.visible_values_into(solver_y, params, t, &mut values)?;
        if recorded_times
            .last()
            .is_some_and(|last| sample_time_match_with_tol(*last, t))
        {
            if let Some(last) = recorded_times.last_mut() {
                *last = t;
            }
            replace_last_visible_values(data, &values)?;
            return Ok(());
        }
        reserve_runtime_vec_capacity(recorded_times, 1, "recorded sample times")?;
        recorded_times.push(t);
        push_visible_values(data, &values)
    }

    pub fn visible_values(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut values = Vec::new();
        self.visible_values_into(y, params, t, &mut values)?;
        Ok(values)
    }

    fn visible_values_into(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
        values: &mut Vec<f64>,
    ) -> Result<(), RuntimeSolveError> {
        if let Some(plan) = &self.visible_value_plan {
            resize_runtime_values(values, plan.entries.len(), 0.0, "visible values")?;
            self.write_planned_visible_values(plan, y, params, t, values)?;
            return Ok(());
        }
        if self.visible_value_rows.len() == self.model.visible_names.len() {
            resize_runtime_values(values, self.visible_value_rows.len(), 0.0, "visible values")?;
            self.visible_value_rows.eval_with_context(
                y,
                params,
                t,
                self.row_eval_context(),
                values,
            )?;
            return Ok(());
        }
        let computed =
            visible_values_with_context(&self.model, y, params, t, self.row_eval_context())?;
        copy_runtime_values_into(values, &computed, "visible values")
    }

    fn write_planned_visible_values(
        &self,
        plan: &VisibleValuePlan,
        y: &[f64],
        params: &[f64],
        t: f64,
        values: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for (slot, entry) in values.iter_mut().zip(plan.entries.iter().copied()) {
            if let VisibleValuePlanEntry::Direct(source) = entry {
                *slot = direct_visible_value(source, y, params, t)?;
            }
        }
        if !plan.expression_rows.is_empty() {
            self.visible_value_rows
                .eval_single_output_rows_unchecked_with_context(
                    &plan.expression_rows,
                    y,
                    params,
                    t,
                    self.row_eval_context(),
                    values,
                )?;
            copy_grouped_expression_values(plan, values)?;
        }
        Ok(())
    }

    pub fn visible_values_for_names(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
        names: &[String],
    ) -> Result<IndexMap<String, f64>, RuntimeSolveError> {
        if self.visible_value_rows.len() == self.model.visible_names.len() {
            return self.visible_values_for_names_from_rows(y, params, t, names);
        }
        let all_values = self.visible_values(y, params, t)?;
        let mut values = IndexMap::new();
        reserve_runtime_index_map_capacity(&mut values, names.len(), "visible name values")?;
        for name in names {
            let Some(idx) = self.visible_name_index.get(name).copied() else {
                continue;
            };
            let value = all_values.get(idx).copied().ok_or_else(|| {
                visible_value_index_error(name, idx, all_values.len(), "visible values")
            })?;
            values.insert(name.clone(), value);
        }
        Ok(values)
    }

    fn visible_values_for_names_from_rows(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
        names: &[String],
    ) -> Result<IndexMap<String, f64>, RuntimeSolveError> {
        let mut values = IndexMap::new();
        reserve_runtime_index_map_capacity(&mut values, names.len(), "visible row name values")?;
        for name in names {
            if let Some(value) = self.visible_value_from_row(name, y, params, t)? {
                values.insert(name.clone(), value);
            }
        }
        Ok(values)
    }

    fn visible_value_from_row(
        &self,
        name: &str,
        y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(idx) = self.visible_name_index.get(name).copied() else {
            return Ok(None);
        };
        if idx >= self.visible_value_rows.len() {
            return Err(visible_value_index_error(
                name,
                idx,
                self.visible_value_rows.len(),
                "visible value rows",
            ));
        }
        let value = self.visible_value_rows.eval_row_with_context(
            idx,
            y,
            params,
            t,
            self.row_eval_context(),
        )?;
        Ok(Some(value))
    }

    fn populate_solver_y_from_state(
        &self,
        solver_y: &mut Vec<f64>,
        state: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        copy_runtime_values_into(solver_y, &self.model.initial_y, "solver y initial values")?;
        resize_runtime_values(solver_y, self.solver_count, 0.0, "solver y")?;
        for (dst, src) in solver_y.iter_mut().zip(state.iter().copied()) {
            *dst = src;
        }
        Ok(())
    }

    fn update_solver_y_guess_from_state(
        &self,
        solver_y: &mut [f64],
        state: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if solver_y.len() != self.solver_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "algebraic warm-start length mismatch: expected {}, got {}",
                self.solver_count,
                solver_y.len()
            )));
        }
        for (dst, src) in solver_y.iter_mut().zip(state.iter().copied()) {
            *dst = src;
        }
        Ok(())
    }

    fn eval_state_derivatives_at_solver_y(
        &self,
        t: f64,
        params: &[f64],
        tol: f64,
        max_iters: usize,
        solver_y: &mut [f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.refresh_derivative_dependencies(t, solver_y, params, tol, max_iters)?;
        // `eval_derivative_rhs_from_solver_y` fills `out` and *then* rejects
        // non-finite derivatives, so trace before propagating: on failure `out`
        // and `solver_y` still hold the offending values to name for the user.
        let eval_result = self.eval_derivative_rhs_from_solver_y(t, solver_y, params, out);
        solve_eval::nan_trace::report_state_derivative(&self.model, t, solver_y, out);
        eval_result
    }

    fn eval_derivative_rhs_from_solver_y(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        validate_derivative_output_len(out, self.state_count)?;
        self.derivative_rhs
            .eval_with_context(solver_y, params, t, self.row_eval_context(), out)?;
        self.validate_finite_derivatives(out)
    }

    fn validate_finite_derivatives(&self, derivative: &[f64]) -> Result<(), RuntimeSolveError> {
        for (idx, value) in derivative.iter().enumerate() {
            if !value.is_finite() {
                let state_name = self
                    .model
                    .visible_names
                    .get(idx)
                    .cloned()
                    .unwrap_or_else(|| format!("state[{idx}]"));
                return Err(RuntimeSolveError::NonFiniteDerivative { state_name });
            }
        }
        Ok(())
    }
}

fn fill_inactive_root_output(out: &mut [f64]) -> Result<(), RuntimeSolveError> {
    if let Some(first) = out.first_mut() {
        *first = 1.0;
    }
    Ok(())
}

fn validate_runtime_output_len(
    context: &str,
    expected: usize,
    actual: usize,
) -> Result<(), RuntimeSolveError> {
    if actual == expected {
        return Ok(());
    }
    Err(RuntimeSolveError::solve_ir(format!(
        "{context} expected {expected} values, got {actual}"
    )))
}

fn validate_finite_runtime_output(context: &str, values: &[f64]) -> Result<(), RuntimeSolveError> {
    if let Some((index, value)) = values
        .iter()
        .copied()
        .enumerate()
        .find(|(_, value)| !value.is_finite())
    {
        return Err(RuntimeSolveError::solve_ir(format!(
            "{context} produced non-finite value {value} at index {index}"
        )));
    }
    Ok(())
}

fn restore_after_causal_seed_error(
    error: RuntimeSolveError,
    solver_y: &mut [f64],
    incoming: &[f64],
) -> Result<(), RuntimeSolveError> {
    solver_y.copy_from_slice(incoming);
    if !seed_error_allows_projection(&error) {
        return Err(error);
    }
    tracing::debug!(
        target: "rumoca_eval_solve::refresh",
        "causal algebraic seed was unavailable; projecting the preserved residual system: {error}"
    );
    Ok(())
}

#[derive(Clone, Default)]
struct StateDerivativeScratch {
    /// Full solver vector reconstructed from the state slots, reused across
    /// derivative and Jacobian evaluations to avoid per-call allocation.
    solver_y: Vec<f64>,
    /// State-space probe direction expanded to a full solver-length seed, with
    /// the algebraic slots completed by the projection forward-sensitivity, for
    /// the AD Jacobian-vector product.
    seed_buf: Vec<f64>,
    /// Scratch unit seed used to read a single residual row's diagonal
    /// sensitivity `∂g_row/∂y_target`; kept all-zero between uses.
    unit_seed: Vec<f64>,
}

/// Tolerances for the algebraic projection's fixed-point settle (shared by the
/// value refresh and the seed/forward-sensitivity refresh).
#[derive(Debug, Clone, Copy)]
pub struct AlgebraicSettle {
    pub tol: f64,
    pub max_iters: usize,
}

/// Shared linearization context for the reconstruct-then-JVP entry points: the
/// evaluation time, the parameter vector, and the algebraic-settle tolerance
/// used to project algebraics from the state before linearizing. Bundling these
/// keeps the sensitivity entry points within the argument-count budget and threads
/// the same context through every layer without repetition.
#[derive(Debug, Clone, Copy)]
pub struct AlgebraicLinearization<'a> {
    pub t: f64,
    pub params: &'a [f64],
    pub settle: AlgebraicSettle,
}

/// Diagonal magnitude below which a seed residual row is treated as singular for
/// its paired target slot, matching the value refresh's residual-slope check.
fn validate_derivative_output_len(
    out: &[f64],
    state_count: usize,
) -> Result<(), RuntimeSolveError> {
    if out.len() == state_count {
        return Ok(());
    }
    Err(RuntimeSolveError::solve_ir(format!(
        "state derivative output length {} does not match state count {}",
        out.len(),
        state_count
    )))
}

fn visible_value_index_error(
    name: &str,
    index: usize,
    len: usize,
    context: &'static str,
) -> RuntimeSolveError {
    RuntimeSolveError::solve_ir(format!(
        "{context} for visible name `{name}` reference index {index}, but only {len} values are available"
    ))
}

pub fn apply_discrete_slot_value(
    target: solve::ScalarSlot,
    value: f64,
    y: &mut [f64],
    p: &mut [f64],
    tol: f64,
) -> Result<bool, EvalSolveError> {
    solve_eval::apply_scalar_slot_value(target, value, y, p, tol)
}

#[cfg(test)]
mod tests;
