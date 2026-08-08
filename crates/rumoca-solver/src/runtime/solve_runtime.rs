//! Runtime orchestration for projection, events, and visible outputs.

use indexmap::IndexMap;
use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;
use std::{
    cell::RefCell,
    collections::{BTreeSet, HashMap},
};

use crate::runtime::delay::{DelayRuntime, DelayRuntimeSnapshot};
use crate::runtime::pre_params::{
    advance_event_iteration_pre_params, event_iteration_plan_settled, seed_event_entry_pre_params,
};
use crate::runtime::solve_events::{
    apply_discrete_slot_values, current_dynamic_time_event_stop, eval_event_actions_with_context,
    next_runtime_event_stop, visible_values_with_context,
};
use crate::runtime::solve_ops::write_clock_activation_params;
use crate::{
    EventActionOutcome, ImplicitProjectionModel, ManifoldProjectionModel, RuntimeEventStop,
    RuntimeSolveError, SolveStopSchedule, project_algebraic_seed_with_plan,
    project_algebraics_with_plan, project_algebraics_with_plan_certified, push_visible_values,
    relation_memory_value_from_root, replace_last_visible_values,
    timeline::sample_time_match_with_tol,
};
use rumoca_eval_solve::refresh_plan::{
    AlgebraicRefreshRow, RefreshPlan, RefreshStage, build_algebraic_refresh_plan,
    build_derivative_refresh_plan, build_root_refresh_plan, trace_refresh_plan,
};
use rumoca_eval_solve::{
    EvalSolveError, PreparedComputeBlock, PreparedScalarProgramBlock, RowEvalContext,
    to_scalar_program_block,
};

mod coupled_event;
mod discrete_rows;
mod event_update;
mod initial_continuation;
mod initial_event;
mod initial_projection;
mod plans;
mod refresh_batch;
mod refresh_projection;
mod relation_memory;
mod sensitivity;
mod support;
use discrete_rows::PreparedStructuredDiscreteRows;
pub use discrete_rows::SeededConditionMemory;
#[cfg(test)]
pub(crate) use discrete_rows::{
    ConditionMemorySeedInput, seed_condition_memory_for_initialization_core,
};
use event_update::{DiscretePreSnapshot, DiscreteRowsSettleInput};
pub use event_update::{EventUpdateRowFilter, ProjectedEventUpdateInput};
use initial_continuation::InitialContinuationCoverage;
pub use initial_event::{
    InitialEventObservation, ProjectedInitialEventInput, ProjectedInitialEventOutcome,
    ProjectedPostInitialEventInput,
};
use plans::{
    RootConditionPlan, RootConditionPlanEntry, VisibleValuePlan, VisibleValuePlanEntry,
    copy_grouped_expression_values, direct_time_root_search_default, direct_time_root_time,
    direct_time_root_value, direct_visible_value, prepare_manifold_projection_programs,
    root_condition_plan, total_root_condition_count, visible_value_plan,
};
use refresh_projection::*;
use support::{
    copy_runtime_values, copy_runtime_values_into, reserve_runtime_index_map_capacity,
    reserve_runtime_vec_capacity, resize_runtime_values, zero_runtime_values,
};

impl From<solve_eval::EvalSolveError> for RuntimeSolveError {
    fn from(value: solve_eval::EvalSolveError) -> Self {
        match value {
            EvalSolveError::SingularTargetAssignment {
                row,
                target_y_index,
                coefficient,
                span,
            } => Self::RefreshTargetSingular {
                row,
                target_y_index,
                coefficient,
                span,
            },
            error => Self::solve_ir_with_span(error.to_string(), error.source_span()),
        }
    }
}

fn set_initial_event_flag(model: &solve::SolveModel, p: &mut [f64], value: bool) {
    let Some(index) = model.problem.solve_layout.initial_event_parameter_index else {
        return;
    };
    if let Some(slot) = p.get_mut(index) {
        *slot = f64::from(value);
    }
}

fn validate_discrete_event_rows(model: &solve::SolveModel) -> Result<(), RuntimeSolveError> {
    let rows = model.problem.discrete.rhs.len();
    let targets = model.problem.discrete.update_targets.len();
    let roles = model.problem.discrete.row_roles.len();
    let pre_modes = model.problem.discrete.pre_modes.len();
    let observation = model.problem.discrete.observation_refresh.len();
    let clock_owners = model.problem.discrete.clock_owners.len();
    if rows == targets
        && rows == roles
        && rows == pre_modes
        && rows == observation
        && rows == clock_owners
    {
        return Ok(());
    }
    Err(RuntimeSolveError::solve_ir(format!(
        "discrete row columns differ: rhs={rows}, targets={targets}, roles={roles}, \
         pre_modes={pre_modes}, observation_refresh={observation}, \
         clock_owners={clock_owners}"
    )))
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
    continuous_structural: solve::ContinuousStructuralArtifacts,
    initialization_structural: solve::InitializationStructuralArtifacts,
    algebraic_newton_caches: Vec<RefCell<crate::runtime::projection::SparseNewtonCache>>,
    algebraic_refresh: RefreshPlan,
    derivative_refresh: RefreshPlan,
    root_refresh: RefreshPlan,
    root_refresh_after_derivative: Option<RefreshPlan>,
    /// Certified coverage for the initialization homotopy continuation; the
    /// single source of truth shared by the sweep driver and the acceptance
    /// check in [`InitialContinuationCoverage::certify`].
    initial_continuation: Option<InitialContinuationCoverage>,
    root_condition_rows: PreparedScalarProgramBlock,
    root_condition_plan: Option<RootConditionPlan>,
    discrete_rhs: PreparedScalarProgramBlock,
    structured_discrete_rows: PreparedStructuredDiscreteRows,
    visible_name_index: HashMap<String, usize>,
    visible_value_rows: PreparedScalarProgramBlock,
    visible_value_plan: Option<VisibleValuePlan>,
    visible_scratch: RefCell<Vec<f64>>,
    refresh_probe_scratch: RefCell<Vec<f64>>,
    refresh_tensor_scratch: RefCell<Vec<f64>>,
    static_refresh_cache: RefCell<StaticRefreshCache>,
    parameter_static_gradient_cache: RefCell<ParameterStaticGradientCache>,
    runtime_state: solve_eval::SimulationRuntimeState,
    delay_runtime: DelayRuntime,
    root_condition_count: usize,
    derivative_scratch: RefCell<StateDerivativeScratch>,
    root_scratch: RefCell<Vec<f64>>,
    /// Reusable register tape / adjoint buffers for the reverse-mode VJP sweep,
    /// kept across calls so a hot reverse loop stays allocation-free.
    reverse_scratch: RefCell<solve_eval::reverse::ReverseScratch>,
}

/// Opaque mutable state shared by the evaluators behind one ME component.
#[derive(Clone)]
pub(crate) struct SolveRuntimeSnapshot {
    static_refresh_cache: StaticRefreshCache,
    evaluator: solve_eval::SimulationRuntimeStateSnapshot,
    delay: DelayRuntimeSnapshot,
}

impl SolveRuntime {
    pub fn new(model: &solve::SolveModel) -> Result<Self, EvalSolveError> {
        let continuous_structural = model.artifacts.continuous.structural.clone();
        let initialization_structural = model.artifacts.initialization.structural.clone();
        let algebraic_newton_caches = (0..continuous_structural.algebraic_projection().len())
            .map(|_| RefCell::new(crate::runtime::projection::SparseNewtonCache::default()))
            .collect();
        let implicit_scalar_programs =
            to_scalar_program_block(&model.problem.continuous.implicit_rhs)?;
        let implicit_scalar_rhs = PreparedScalarProgramBlock::new(implicit_scalar_programs)?;
        let (manifold_residual, manifold_jacobian_v) = prepare_manifold_projection_programs(model)?;
        let derivative_scalar_rhs =
            to_scalar_program_block(&model.problem.continuous.derivative_rhs)?;
        let (algebraic_refresh, derivative_refresh, root_refresh) =
            build_runtime_refresh_plans(model, &implicit_scalar_rhs, &derivative_scalar_rhs)?;
        let root_refresh_after_derivative =
            root_refresh.certified_value_remainder_after(&derivative_refresh);
        trace_reverse_projection_coverage(model, &implicit_scalar_rhs);
        let visible_value_plan = visible_value_plan(model);
        let root_condition_plan = root_condition_plan(model, &root_refresh);
        let (initial_scalar_residual, initial_continuation) =
            InitialContinuationCoverage::certify_runtime_blocks(
                model,
                &implicit_scalar_rhs,
                &algebraic_refresh,
            )?;
        let delay_runtime = DelayRuntime::new(&model.problem.events.delays)?;
        let root_condition_count =
            total_root_condition_count(model, delay_runtime.event_root_count())?;
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
            initial_scalar_residual: PreparedScalarProgramBlock::new(initial_scalar_residual)?,
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
            continuous_structural,
            initialization_structural,
            algebraic_newton_caches,
            algebraic_refresh,
            derivative_refresh,
            root_refresh,
            root_refresh_after_derivative,
            initial_continuation,
            root_condition_rows: PreparedScalarProgramBlock::new(
                model.problem.events.root_conditions.clone(),
            )?,
            root_condition_plan,
            discrete_rhs: PreparedScalarProgramBlock::new(model.problem.discrete.rhs.clone())?,
            structured_discrete_rows: PreparedStructuredDiscreteRows::new(model)?,
            visible_name_index: build_visible_name_index(model),
            visible_value_rows: PreparedScalarProgramBlock::new(model.visible_value_rows.clone())?,
            visible_value_plan,
            visible_scratch: RefCell::new(Vec::new()),
            refresh_probe_scratch: RefCell::new(Vec::new()),
            refresh_tensor_scratch: RefCell::new(Vec::new()),
            static_refresh_cache: RefCell::new(StaticRefreshCache::default()),
            parameter_static_gradient_cache: RefCell::new(ParameterStaticGradientCache::default()),
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

    pub(crate) fn snapshot(&self) -> SolveRuntimeSnapshot {
        SolveRuntimeSnapshot {
            static_refresh_cache: self.static_refresh_cache.borrow().clone(),
            evaluator: self.runtime_state.snapshot(),
            delay: self.delay_runtime.snapshot(),
        }
    }

    pub(crate) fn restore(&self, snapshot: &SolveRuntimeSnapshot) {
        self.static_refresh_cache
            .borrow_mut()
            .clone_from(&snapshot.static_refresh_cache);
        self.runtime_state.restore(&snapshot.evaluator);
        self.delay_runtime.restore(&snapshot.delay);
    }

    #[cfg(test)]
    pub(crate) fn matches_snapshot(&self, snapshot: &SolveRuntimeSnapshot) -> bool {
        self.static_refresh_cache
            .borrow()
            .bit_eq(&snapshot.static_refresh_cache)
            && self.runtime_state.matches_snapshot(&snapshot.evaluator)
            && self.delay_runtime.matches_snapshot(&snapshot.delay)
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

    pub fn derivative_settled_coordinate_can_refresh_roots(&self) -> bool {
        self.root_refresh_after_derivative.is_some() && self.delay_runtime.event_root_count() == 0
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
                certify_coordinates: false,
            },
        )
    }

    pub fn refresh_algebraic_and_output_slots_certified(
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
                certify_coordinates: true,
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
                certify_coordinates: false,
            },
        )
    }

    fn refresh_slots_with_plan(
        &self,
        plan: &RefreshPlan,
        mut args: RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        if plan.rows.is_empty() && plan.simultaneous_plan.is_empty() {
            return Ok(());
        }
        self.validate_refresh_inputs(args.solver_y, args.params)?;
        let incoming = copy_runtime_values(args.solver_y, "algebraic projection snapshot")?;
        if self.value_stage_schedule_is_certified(plan) {
            let result = self.refresh_slots_with_stages(plan, &mut args, &incoming);
            if result.is_err() {
                args.solver_y.copy_from_slice(&incoming);
            }
            return result;
        }
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
        self.prepare_static_refresh_cache(params, solver_y.len());
        self.refresh_prepared_static_rows(rows, t, solver_y, params)
    }

    fn prepare_static_refresh_cache(&self, params: &[f64], solver_len: usize) {
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
            cache.values.resize(solver_len, None);
        }
    }

    fn refresh_prepared_static_rows(
        &self,
        rows: &[AlgebraicRefreshRow],
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        let fully_cached = {
            let cache = self.static_refresh_cache.borrow();
            rows.iter().all(|row| {
                cache
                    .values
                    .get(row.target_index)
                    .is_some_and(Option::is_some)
            })
        };
        if fully_cached {
            let cache = self.static_refresh_cache.borrow();
            for row in rows {
                solver_y[row.target_index] = cached_static_refresh_value(&cache, row.target_index)?;
            }
            return Ok(());
        }

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
            block_indices: &plan.simultaneous_block_indices,
            plan_validated: false,
            jacobian_v: ProjectionJacobian::SolverY {
                block: &self.implicit_projection_jacobian_v,
                scalar: &self.implicit_projection_scalar_jacobian_v,
            },
        };
        let projection_args = crate::AlgebraicProjectionArgs {
            parameters: args.params,
            time: args.t,
            state_count: self.state_count,
            tolerance: args.tol,
        };
        if args.certify_coordinates {
            project_algebraics_with_plan_certified(
                &projection_model,
                projection_plan,
                args.solver_y,
                projection_args,
                args.max_iters,
            )
        } else {
            project_algebraics_with_plan(
                &projection_model,
                projection_plan,
                args.solver_y,
                projection_args,
                args.max_iters,
            )
        }
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

    /// Whether checked Solve IR retained any lower-order state constraints.
    ///
    /// An empty projection artifact is a construction-time certificate that
    /// projecting continuous states cannot change them. FMI hosts use this to
    /// avoid reconstructing observation algebraics merely to discover that
    /// there is no manifold system to evaluate.
    pub fn requires_state_manifold_projection(&self) -> bool {
        !self
            .model
            .problem
            .continuous
            .manifold_projection_plan
            .is_empty()
    }

    fn validate_refresh_inputs(
        &self,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        // Refresh-plan construction already proves one implicit output for
        // every algebraic coordinate. Explicit states are owned by derivative
        // rows and therefore need no placeholder implicit rows.
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
                certify_coordinates: false,
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
        let mut solver_y = self.root_scratch.borrow_mut();
        self.populate_solver_y_from_state(&mut solver_y, state)?;
        self.eval_root_search_conditions_at_solver_y(t, params, tol, max_iters, out, &mut solver_y)
    }

    // SPEC_0021: Exception - public runtime API mirrors solver callback inputs
    // without hiding the certified warm-start and output buffers.
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
        self.update_solver_y_guess_from_state(guess, state)?;
        self.eval_root_search_conditions_at_solver_y(t, params, tol, max_iters, out, guess)
    }

    fn eval_root_search_conditions_at_solver_y(
        &self,
        t: f64,
        params: &[f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
        solver_y: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return fill_inactive_root_output(out);
        }
        validate_runtime_output_len("root search output", root_count, out.len())?;
        let model_root_count = self.model.problem.events.root_conditions.len();
        if self.delay_runtime.event_root_count() > 0 {
            self.refresh_algebraic_and_output_slots(t, solver_y, params, tol, max_iters)?;
        }
        if model_root_count > 0 {
            let model_out = &mut out[..model_root_count];
            let Some(plan) = &self.root_condition_plan else {
                self.refresh_slots_with_plan(
                    &self.root_refresh,
                    RefreshSlotArgs {
                        t,
                        solver_y,
                        params,
                        tol,
                        max_iters,
                        certify_coordinates: false,
                    },
                )?;
                self.eval_root_conditions_from_refreshed_solver_y(t, solver_y, params, model_out)?;
                self.delay_runtime
                    .evaluate_event_roots(
                        t,
                        solver_y,
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
                        solver_y,
                        params,
                        tol,
                        max_iters,
                        certify_coordinates: false,
                    },
                )?;
                self.write_planned_root_search_conditions(plan, solver_y, params, t, model_out)?;
            }
        }
        self.delay_runtime
            .evaluate_event_roots(
                t,
                solver_y,
                params,
                self.row_eval_context(),
                &mut out[model_root_count..],
            )
            .map_err(RuntimeSolveError::from)?;
        validate_finite_runtime_output("root search output", out)
    }

    #[allow(clippy::too_many_arguments)]
    pub fn eval_root_search_conditions_after_derivative_settle_into(
        &self,
        t: f64,
        params: &[f64],
        solver_y: &mut [f64],
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if !self.derivative_settled_coordinate_can_refresh_roots() {
            return Err(RuntimeSolveError::solve_ir(
                "root refresh has no certified derivative-settled remainder".to_string(),
            ));
        }
        if solver_y.len() != self.solver_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "derivative-settled solver-y length mismatch: expected {}, got {}",
                self.solver_count,
                solver_y.len()
            )));
        }
        let remainder = self.root_refresh_after_derivative.as_ref().ok_or_else(|| {
            RuntimeSolveError::solve_ir(
                "root refresh derivative-settled remainder disappeared".to_string(),
            )
        })?;
        if !remainder.value_stages.is_empty() {
            self.refresh_slots_with_plan(
                remainder,
                RefreshSlotArgs {
                    t,
                    solver_y,
                    params,
                    tol,
                    max_iters,
                    certify_coordinates: false,
                },
            )?;
        }
        let root_count = self.root_condition_count();
        if root_count == 0 {
            return fill_inactive_root_output(out);
        }
        validate_runtime_output_len("root search output", root_count, out.len())?;
        let model_root_count = self.model.problem.events.root_conditions.len();
        if model_root_count > 0 {
            let model_out = &mut out[..model_root_count];
            match &self.root_condition_plan {
                Some(plan) if plan.search_rows.is_empty() => {
                    self.validate_root_plan_output_len(plan, model_out)?;
                    self.write_planned_root_search_defaults(plan, params, t, model_out)?;
                }
                Some(plan) => {
                    self.validate_root_plan_output_len(plan, model_out)?;
                    self.write_planned_root_search_conditions(
                        plan, solver_y, params, t, model_out,
                    )?;
                }
                None => self
                    .eval_root_conditions_from_refreshed_solver_y(t, solver_y, params, model_out)?,
            }
        }
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
}

fn build_visible_name_index(model: &solve::SolveModel) -> HashMap<String, usize> {
    model
        .visible_names
        .iter()
        .enumerate()
        .map(|(idx, name)| (name.clone(), idx))
        .collect()
}

fn build_runtime_refresh_plans(
    model: &solve::SolveModel,
    implicit: &PreparedScalarProgramBlock,
    derivative: &solve::ScalarProgramBlock,
) -> Result<(RefreshPlan, RefreshPlan, RefreshPlan), EvalSolveError> {
    let algebraic = build_algebraic_refresh_plan(model, implicit)?;
    let derivative = build_derivative_refresh_plan(model, derivative, implicit, &algebraic)?;
    let root = build_root_refresh_plan(model, implicit, &algebraic)?;
    trace_refresh_plan(model, "algebraic", &algebraic);
    trace_refresh_plan(model, "derivative", &derivative);
    trace_refresh_plan(model, "root", &root);
    Ok((algebraic, derivative, root))
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

#[cfg(test)]
mod tests;
