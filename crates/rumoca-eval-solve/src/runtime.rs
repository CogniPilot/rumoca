use rumoca_ir_solve as solve;
use rumoca_solver::{
    EventActionOutcome, EventPreMode, EventPreSources, RuntimeEventStop, RuntimeSolveError,
    SolveStopSchedule, discrete_row_pre_mode, event_eval_params_for_row_pre_mode,
    push_visible_values, replace_last_visible_values, row_reads_solver_or_time,
    timeline::sample_time_match_with_tol, update_relation_memory_slots,
};
use std::{cell::RefCell, collections::HashMap};

use crate::refresh_plan::{
    AlgebraicRefreshRow, RefreshPlan, build_algebraic_refresh_plan, build_derivative_refresh_plan,
    build_root_refresh_plan, trace_refresh_plan,
};
use crate::runtime_events::{
    apply_discrete_slot_values, current_dynamic_time_event_stop, eval_event_actions_with_context,
    event_eval_params_with_relation_overrides, next_runtime_event_stop,
    visible_values_with_context,
};
use crate::{
    self as solve_eval, PreparedComputeBlock, PreparedScalarProgramBlock, RowEvalContext,
    to_scalar_program_block,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum EventUpdateRowFilter {
    All,
    FollowCurrentOnly,
}

impl EventUpdateRowFilter {
    fn accepts(self, mode: EventPreMode) -> bool {
        matches!(self, Self::All)
            || matches!(
                (self, mode),
                (Self::FollowCurrentOnly, EventPreMode::FollowCurrent)
            )
    }
}

pub struct ProjectedEventUpdateInput<'a> {
    pub y: &'a mut [f64],
    pub p: &'a mut [f64],
    pub t: f64,
    pub tol: f64,
    pub event_pre_y: &'a [f64],
    pub event_pre_p: &'a [f64],
    pub max_iters: usize,
    pub row_filter: EventUpdateRowFilter,
    pub root_relation_overrides: &'a [(usize, f64)],
}

struct DiscretePreSnapshot<'a> {
    event_pre_y: &'a [f64],
    event_pre_p: &'a [f64],
    iter_pre_y: &'a [f64],
    iter_pre_p: &'a [f64],
    row_filter: EventUpdateRowFilter,
    root_relation_overrides: &'a [(usize, f64)],
    event_iteration: usize,
}

impl<'a> DiscretePreSnapshot<'a> {
    fn event_pre_sources(&self) -> EventPreSources<'a> {
        EventPreSources {
            event_pre_y: self.event_pre_y,
            event_pre_p: self.event_pre_p,
            iter_pre_y: self.iter_pre_y,
            iter_pre_p: self.iter_pre_p,
            event_iteration: self.event_iteration,
        }
    }
}

struct DiscreteRowsSettleInput<'a> {
    y: &'a mut [f64],
    p: &'a mut [f64],
    t: f64,
    tol: f64,
    max_iters: usize,
}

#[derive(Default)]
struct EventEvalParamCache {
    event_entry: Option<Vec<f64>>,
    fixed: Option<Vec<f64>>,
    follow_current: Option<Vec<f64>>,
}

impl EventEvalParamCache {
    fn params<'a>(
        &'a mut self,
        model: &solve::SolveModel,
        base_p: &[f64],
        mode: EventPreMode,
        sources: &EventPreSources<'_>,
        tol: f64,
    ) -> &'a [f64] {
        let slot = match mode {
            EventPreMode::EventEntry => &mut self.event_entry,
            EventPreMode::Fixed => &mut self.fixed,
            EventPreMode::FollowCurrent => &mut self.follow_current,
        };
        slot.get_or_insert_with(|| {
            event_eval_params_for_row_pre_mode(model, base_p, mode, sources, tol)
        })
    }
}

struct RefreshSlotArgs<'a> {
    t: f64,
    solver_y: &'a mut [f64],
    params: &'a [f64],
    tol: f64,
    max_iters: usize,
}

struct RefreshIterationMax {
    delta: f64,
    target: Option<(usize, usize, f64)>,
}

impl From<solve_eval::EvalSolveError> for RuntimeSolveError {
    fn from(value: solve_eval::EvalSolveError) -> Self {
        Self::SolveIr(value.to_string())
    }
}

#[derive(Clone)]
pub struct SolveRuntime {
    pub model: solve::SolveModel,
    pub state_count: usize,
    pub solver_count: usize,
    implicit_rhs: PreparedComputeBlock,
    implicit_scalar_rhs: PreparedScalarProgramBlock,
    derivative_rhs: PreparedComputeBlock,
    /// Forward-mode AD Jacobian-vector product of `derivative_rhs`
    /// (`d(der)/d(y)·v`), lowered to `LinearOp`s with `LoadSeed`. Applied — with a
    /// seed completed by `seed_refresh_derivative_dependencies` — to form
    /// the exact state Jacobian for the state-only BDF path.
    derivative_jacobian_v: PreparedScalarProgramBlock,
    /// Per-row forward-mode AD Jacobian-vector product of `implicit_rhs`
    /// (`d(residual_row)/d(y)·v`). Used to propagate the state seed through the
    /// algebraic projection (`d(alg)/d(state)`) row by row.
    implicit_jacobian_v: PreparedScalarProgramBlock,
    algebraic_refresh: RefreshPlan,
    derivative_refresh: RefreshPlan,
    root_refresh: RefreshPlan,
    visible_name_index: HashMap<String, usize>,
    visible_value_rows: PreparedScalarProgramBlock,
    runtime_state: solve_eval::SimulationRuntimeState,
    derivative_scratch: RefCell<StateDerivativeScratch>,
    root_scratch: RefCell<Vec<f64>>,
}

impl SolveRuntime {
    pub fn new(model: &solve::SolveModel) -> Self {
        let implicit_scalar_programs =
            to_scalar_program_block(&model.problem.continuous.implicit_rhs);
        let implicit_scalar_rhs = PreparedScalarProgramBlock::new(implicit_scalar_programs);
        let derivative_scalar_rhs =
            to_scalar_program_block(&model.problem.continuous.derivative_rhs);
        let algebraic_refresh = build_algebraic_refresh_plan(model, &implicit_scalar_rhs);
        let derivative_refresh =
            build_derivative_refresh_plan(model, &derivative_scalar_rhs, &algebraic_refresh);
        let root_refresh = build_root_refresh_plan(model, &algebraic_refresh);
        trace_refresh_plan(model, "algebraic", &algebraic_refresh);
        trace_refresh_plan(model, "derivative", &derivative_refresh);
        trace_refresh_plan(model, "root", &root_refresh);
        Self {
            model: model.clone(),
            state_count: model.state_scalar_count(),
            solver_count: model.solver_scalar_count(),
            implicit_rhs: PreparedComputeBlock::new_with_label(
                &model.problem.continuous.implicit_rhs,
                "runtime_implicit_rhs",
            ),
            implicit_scalar_rhs,
            derivative_rhs: PreparedComputeBlock::new_with_label(
                &model.problem.continuous.derivative_rhs,
                "runtime_derivative_rhs",
            ),
            derivative_jacobian_v: PreparedScalarProgramBlock::new(
                model.artifacts.continuous.full_jacobian_v.clone(),
            ),
            implicit_jacobian_v: PreparedScalarProgramBlock::new(
                model
                    .artifacts
                    .continuous
                    .implicit_jacobian_v_scalar
                    .clone(),
            ),
            algebraic_refresh,
            derivative_refresh,
            root_refresh,
            visible_name_index: model
                .visible_names
                .iter()
                .enumerate()
                .map(|(idx, name)| (name.clone(), idx))
                .collect(),
            visible_value_rows: PreparedScalarProgramBlock::new(model.visible_value_rows.clone()),
            runtime_state: solve_eval::SimulationRuntimeState::new(),
            derivative_scratch: RefCell::new(StateDerivativeScratch::default()),
            root_scratch: RefCell::new(Vec::new()),
        }
    }

    pub fn row_eval_context(&self) -> RowEvalContext<'_> {
        RowEvalContext {
            external_tables: Some(self.model.external_tables.as_slice()),
            runtime_state: Some(&self.runtime_state),
            ..Default::default()
        }
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
        self.populate_solver_y_from_state(&mut solver_y, state);
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
        self.populate_solver_y_from_state(solver_y, state);
        self.refresh_algebraic_and_output_slots(t, solver_y, params, tol, max_iters)
    }

    pub fn full_solver_y_with_guess(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut Vec<f64>,
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        if guess.len() != self.solver_count {
            *guess = self.model.initial_y.clone();
            guess.resize(self.solver_count, 0.0);
        }
        self.populate_solver_y_from_state(guess, state);
        self.refresh_algebraic_and_output_slots(t, guess, params, tol, max_iters)
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
        args: RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        self.validate_refresh_plan(plan, args.solver_y, args.params)?;
        if plan.rows.is_empty() {
            return Ok(());
        }
        if !plan.iterative {
            self.refresh_slots_once(&plan.rows, args.t, args.solver_y, args.params)?;
            return Ok(());
        }
        self.refresh_slots_iterative(&plan.rows, args)
    }

    fn validate_refresh_plan(
        &self,
        plan: &RefreshPlan,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if !plan.missing_dependencies.is_empty() {
            return Err(RuntimeSolveError::SolveIr(format!(
                "refresh plan requires algebraic/output dependencies without producer rows: {}",
                self.missing_dependency_names(&plan.missing_dependencies)
            )));
        }
        if self.implicit_rhs.len() < self.solver_count {
            return Err(RuntimeSolveError::SolveIr(format!(
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

    fn refresh_slots_iterative(
        &self,
        rows: &[AlgebraicRefreshRow],
        args: RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        let RefreshSlotArgs {
            t,
            solver_y,
            params,
            tol,
            max_iters,
        } = args;
        let mut last_max = RefreshIterationMax {
            delta: 0.0,
            target: None,
        };
        for iter_idx in 0..max_iters {
            last_max = self.refresh_slots_iteration(rows, t, solver_y, params)?;
            self.trace_refresh_iteration(iter_idx, &last_max);
            if last_max.delta <= tol {
                return Ok(());
            }
        }
        Err(self.refresh_convergence_error(max_iters, &last_max))
    }

    fn refresh_slots_iteration(
        &self,
        rows: &[AlgebraicRefreshRow],
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<RefreshIterationMax, RuntimeSolveError> {
        let mut max_delta: f64 = 0.0;
        let mut max_target = None;
        for refresh_row in rows {
            let row_idx = refresh_row.row_idx;
            let index = refresh_row.target_index;
            let value = self.eval_refresh_row(row_idx, index, t, solver_y, params)?;
            let delta = (solver_y[index] - value).abs();
            if delta > max_delta {
                max_delta = delta;
                max_target = Some((index, row_idx, value));
            }
            solver_y[index] = value;
        }
        Ok(RefreshIterationMax {
            delta: max_delta,
            target: max_target,
        })
    }

    fn eval_refresh_row(
        &self,
        row_idx: usize,
        index: usize,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let value = self.eval_refresh_row_value(row_idx, index, t, solver_y, params)?;
        // Catch non-finite results here (where the variable is known) and raise
        // a spanned diagnostic; otherwise a NaN slips through the iteration (the
        // `delta > max_delta` check is false for NaN) and only surfaces later as
        // an opaque "step size too small".
        if !value.is_finite() {
            return Err(self.non_finite_value_error(index, value));
        }
        Ok(value)
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
        let span = self
            .model
            .variable_meta
            .iter()
            .find(|meta| meta.name == name)
            .map(|meta| meta.source_span);
        let kind = if value.is_nan() { "NaN" } else { "inf" };
        RuntimeSolveError::NonFiniteValue { name, kind, span }
    }

    fn eval_refresh_row_value(
        &self,
        row_idx: usize,
        index: usize,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        if let Some(value) = self
            .implicit_scalar_rhs
            .eval_target_assignment_row_unchecked_with_context(
                row_idx,
                index,
                solver_y,
                params,
                t,
                self.row_eval_context(),
            )?
        {
            return Ok(value);
        }
        let residual = self.implicit_scalar_rhs.eval_row_unchecked_with_context(
            row_idx,
            solver_y,
            params,
            t,
            self.row_eval_context(),
        )?;
        self.solve_refresh_residual_row(row_idx, index, residual, t, solver_y, params)
    }

    fn solve_refresh_residual_row(
        &self,
        row_idx: usize,
        index: usize,
        residual: f64,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let current = solver_y[index];
        let mut probe_y = solver_y.to_vec();
        probe_y[index] = current + 1.0;
        let probe_residual = self.implicit_scalar_rhs.eval_row_unchecked_with_context(
            row_idx,
            &probe_y,
            params,
            t,
            self.row_eval_context(),
        )?;
        let slope = probe_residual - residual;
        if slope.is_finite() && slope.abs() > 1.0e-12 {
            return Ok(current - residual / slope);
        }
        Ok(current + residual)
    }

    fn trace_refresh_iteration(&self, iter_idx: usize, max: &RefreshIterationMax) {
        // `tracing::debug!` self-gates; the only off-path work is a name lookup.
        if let Some((index, row_idx, value)) = max.target {
            let name = self
                .model
                .problem
                .solve_layout
                .solver_maps
                .names
                .get(index)
                .map_or("<unnamed>", String::as_str);
            tracing::debug!(
                target: "rumoca_eval_solve::refresh",
                "refresh iter {iter_idx}: max_delta={:.6e} target={name} y[{index}] row={row_idx} value={value:.6e}",
                max.delta
            );
        } else {
            tracing::debug!(target: "rumoca_eval_solve::refresh", "refresh iter {iter_idx}: no targeted algebraics");
        }
    }

    fn refresh_convergence_error(
        &self,
        max_iters: usize,
        max: &RefreshIterationMax,
    ) -> RuntimeSolveError {
        RuntimeSolveError::UnsupportedModel {
            reason: match max.target {
                Some((index, row_idx, _)) => {
                    let name = self
                        .model
                        .problem
                        .solve_layout
                        .solver_maps
                        .names
                        .get(index)
                        .map_or("<unnamed>", String::as_str);
                    format!(
                        "explicit algebraic/output Solve-IR rows did not converge after {max_iters} iterations; max_delta={:.6e} at {name} from row {row_idx}",
                        max.delta
                    )
                }
                None => "explicit algebraic/output Solve-IR rows did not converge".to_string(),
            },
        }
    }

    fn refresh_slots_once(
        &self,
        plan: &[AlgebraicRefreshRow],
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        for refresh_row in plan {
            let row_idx = refresh_row.row_idx;
            let index = refresh_row.target_index;
            let value = self.eval_refresh_row(row_idx, index, t, solver_y, params)?;
            solver_y[index] = value;
        }
        Ok(())
    }

    fn missing_dependency_names(&self, dependencies: &[usize]) -> String {
        dependencies
            .iter()
            .map(|index| {
                self.model
                    .problem
                    .solve_layout
                    .solver_maps
                    .names
                    .get(*index)
                    .cloned()
                    .unwrap_or_else(|| format!("y[{index}]"))
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    pub fn eval_state_derivatives(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut derivative = vec![0.0; self.state_count];
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
        self.eval_state_derivatives_with_solver_y(t, state, params, tol, max_iters, solver_y, out)
    }

    pub fn eval_state_derivatives_with_guess(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        guess: &mut Vec<f64>,
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut derivative = vec![0.0; self.state_count];
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
        guess: &mut Vec<f64>,
        tol: f64,
        max_iters: usize,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if guess.len() != self.solver_count {
            *guess = self.model.initial_y.clone();
            guess.resize(self.solver_count, 0.0);
        }
        for (dst, src) in guess.iter_mut().zip(state.iter().copied()) {
            *dst = src;
        }
        self.refresh_derivative_dependencies(t, guess, params, tol, max_iters)?;
        self.eval_derivative_rhs_from_solver_y(t, guess, params, out)
    }

    /// Exact state Jacobian-vector product `d(der)/d(state)·v` for the state-only
    /// BDF path, accounting for the algebraic projection.
    ///
    /// The state-only path integrates the reduced ODE `der = f(state, alg(state))`,
    /// where `alg(state)` is recovered each step by the algebraic projection. The
    /// total state Jacobian is therefore
    /// `∂f/∂state·v + ∂f/∂alg · (d(alg)/d(state)·v)`. We compute it in three steps:
    ///
    /// 1. reconstruct the linearization point `solver_y` (states + projected
    ///    algebraics) via the value refresh;
    /// 2. propagate the state seed `v` through the same projection
    ///    (`seed_refresh_derivative_dependencies`) to fill the algebraic
    ///    seeds `d(alg)/d(state)·v`;
    /// 3. apply the derivative JVP `derivative_jacobian_v` to the completed seed.
    ///
    /// The result is exact (true structural zeros stay exactly zero, so diffsol's
    /// NaN-sparsity probe recovers the correct pattern) and uses no finite
    /// differences. For pure ODEs step 2 is a no-op and this reduces to the plain
    /// derivative JVP.
    pub fn eval_state_jacobian_v_ad_into(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        seed: &[f64],
        settle: AlgebraicSettle,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        validate_derivative_output_len(out, self.state_count)?;
        let mut scratch = self.derivative_scratch.borrow_mut();
        let StateDerivativeScratch {
            solver_y,
            seed_buf,
            unit_seed,
        } = &mut *scratch;
        // (1) Linearization point: project the algebraics from the state.
        self.populate_solver_y_from_state(solver_y, state);
        self.refresh_derivative_dependencies(t, solver_y, params, settle.tol, settle.max_iters)?;
        // The JVP rows seed both solver-y and parameters (`SeedMode::SolverYAndP`),
        // so the seed vector spans `[solver-y | parameter]` space. We differentiate
        // with respect to the state only, so all non-state seeds (algebraics and
        // parameters) start at zero; the algebraic seeds are then filled by the
        // projection forward-sensitivity below.
        let seed_len = self
            .derivative_jacobian_v
            .requirements()
            .seed_len
            .max(self.implicit_jacobian_v.requirements().seed_len)
            .max(self.solver_count);
        seed_buf.clear();
        seed_buf.resize(seed_len, 0.0);
        let n = self.state_count.min(seed.len());
        seed_buf[..n].copy_from_slice(&seed[..n]);
        unit_seed.clear();
        unit_seed.resize(seed_len, 0.0);
        // (2) Forward-propagate the seed through the algebraic projection.
        self.seed_refresh_derivative_dependencies(
            t, solver_y, params, seed_buf, unit_seed, settle,
        )?;
        // (3) Total state Jacobian-vector product via the derivative JVP.
        let context = RowEvalContext {
            seed: Some(seed_buf.as_slice()),
            ..self.row_eval_context()
        };
        self.derivative_jacobian_v
            .eval_with_context(solver_y, params, t, context, out)
            .map_err(|err| RuntimeSolveError::SolveIr(err.to_string()))
    }

    /// Forward-sensitivity ("seed") refresh: with `solver_y` at the linearization
    /// point and the state seed already written into `seed[..state_count]`, fill
    /// the algebraic slots of `seed` with `d(alg)/d(state)·v` by propagating the
    /// seed through the same projection rows used for the value refresh. This
    /// mirrors [`Self::refresh_derivative_dependencies`] but linearized.
    fn seed_refresh_derivative_dependencies(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        seed: &mut [f64],
        unit_seed: &mut [f64],
        settle: AlgebraicSettle,
    ) -> Result<(), RuntimeSolveError> {
        let plan = &self.derivative_refresh;
        if plan.rows.is_empty() {
            return Ok(());
        }
        if !plan.iterative {
            // Causal single pass: each target is solved after its dependencies.
            for row in &plan.rows {
                self.seed_refresh_row(t, solver_y, params, seed, unit_seed, row)?;
            }
            return Ok(());
        }
        // Algebraic loop: Gauss–Seidel on the (linear) seed system, mirroring the
        // value iteration. Since the value refresh already converged at this
        // point, the seed iteration converges at the same rate.
        for _ in 0..settle.max_iters.max(1) {
            let mut max_delta = 0.0_f64;
            for row in &plan.rows {
                let before = seed[row.target_index];
                self.seed_refresh_row(t, solver_y, params, seed, unit_seed, row)?;
                max_delta = max_delta.max((seed[row.target_index] - before).abs());
            }
            if max_delta <= settle.tol {
                break;
            }
        }
        Ok(())
    }

    /// Solve one residual row `g(y)=0` (which defines algebraic slot `target`) for
    /// its seed via the implicit-function theorem:
    /// `seed[target] = -(∂g/∂others · seed) / (∂g/∂target)`, both directional
    /// derivatives read from the per-row implicit JVP.
    fn seed_refresh_row(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        seed: &mut [f64],
        unit_seed: &mut [f64],
        row: &AlgebraicRefreshRow,
    ) -> Result<(), RuntimeSolveError> {
        let target = row.target_index;
        // Off-diagonal term: JVP of the row with the target's own seed held at 0.
        let saved = seed[target];
        seed[target] = 0.0;
        let off_diagonal =
            self.eval_implicit_jacobian_row(row.row_idx, solver_y, params, t, seed)?;
        seed[target] = saved;
        // Diagonal term ∂g/∂target via a unit seed isolated to the target slot.
        unit_seed[target] = 1.0;
        let diagonal =
            self.eval_implicit_jacobian_row(row.row_idx, solver_y, params, t, unit_seed)?;
        unit_seed[target] = 0.0;
        seed[target] = if diagonal.is_finite() && diagonal.abs() > SEED_DIAGONAL_EPS {
            -off_diagonal / diagonal
        } else {
            // The row does not constrain its target through its own value (a true
            // structural zero on the diagonal); the seed contribution is zero.
            0.0
        };
        Ok(())
    }

    /// Directional derivative `∂g_row/∂y · seed` of implicit residual row
    /// `row_idx`, evaluated at `solver_y` with the given seed.
    fn eval_implicit_jacobian_row(
        &self,
        row_idx: usize,
        solver_y: &[f64],
        params: &[f64],
        t: f64,
        seed: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        self.implicit_jacobian_v
            .eval_row_unchecked_with_context(
                row_idx,
                solver_y,
                params,
                t,
                RowEvalContext {
                    seed: Some(seed),
                    ..self.row_eval_context()
                },
            )
            .map_err(|err| RuntimeSolveError::SolveIr(err.to_string()))
    }

    pub fn eval_root_conditions(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let roots = &self.model.problem.events.root_conditions;
        if roots.is_empty() {
            return Ok(Vec::new());
        }
        let mut values = vec![0.0; roots.len()];
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
        let roots = &self.model.problem.events.root_conditions;
        if roots.is_empty() {
            if let Some(first) = out.first_mut() {
                *first = 1.0;
            }
            return Ok(());
        }
        let mut solver_y = self.root_scratch.borrow_mut();
        self.populate_solver_y_from_state(&mut solver_y, state);
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
        solve_eval::eval_scalar_program_block_with_context(
            roots,
            &solver_y,
            params,
            t,
            self.row_eval_context(),
            out,
        )?;
        Ok(())
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
        let mut values = vec![0.0; block.len()];
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
            return Err(RuntimeSolveError::SolveIr(format!(
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
        Err(RuntimeSolveError::SolveIr(format!(
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
        Err(RuntimeSolveError::SolveIr(format!(
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
        let event_pre_y = y.to_vec();
        let event_pre_p = p.to_vec();
        for event_iteration in 0..max_iters {
            let iter_pre_y = y.to_vec();
            let iter_pre_p = p.to_vec();
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
        Err(RuntimeSolveError::SolveIr(format!(
            "initial discrete equations did not converge at t={t}"
        )))
    }

    pub fn apply_projected_post_initial_event_update<P>(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
        project_algebraics: P,
    ) -> Result<EventActionOutcome, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        let event_pre_y = y.to_vec();
        let event_pre_p = p.to_vec();
        self.apply_projected_event_update(
            ProjectedEventUpdateInput {
                y,
                p,
                t,
                tol,
                event_pre_y: &event_pre_y,
                event_pre_p: &event_pre_p,
                max_iters,
                row_filter: EventUpdateRowFilter::FollowCurrentOnly,
                root_relation_overrides: &[],
            },
            project_algebraics,
        )
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
            let iter_pre_y = y.to_vec();
            let iter_pre_p = p.to_vec();
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
            changed |= self.update_relation_memory_from_solver_y(t, y, p, tol)?;
            changed |=
                self.apply_root_relation_memory_overrides(root_relation_overrides, y, p, tol)?;
            changed |= project_algebraics(y, p)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            if !changed {
                return self.eval_event_actions(y, p, t);
            }
        }
        Err(RuntimeSolveError::SolveIr(format!(
            "event update iteration did not converge at t={t}"
        )))
    }

    fn validate_discrete_event_rows(&self) -> Result<(), RuntimeSolveError> {
        let rows = self.model.problem.discrete.rhs.len();
        let targets = self.model.problem.discrete.update_targets.len();
        if rows == targets {
            return Ok(());
        }
        Err(RuntimeSolveError::SolveIr(format!(
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
        Err(RuntimeSolveError::SolveIr(format!(
            "event runtime assignments did not converge at t={t}"
        )))
    }

    fn settle_discrete_rows_for_pre_snapshot<P>(
        &self,
        snapshot: &DiscretePreSnapshot<'_>,
        input: &mut DiscreteRowsSettleInput<'_>,
        project_algebraics: &mut P,
    ) -> Result<bool, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        let mut changed_any = false;
        for _ in 0..input.max_iters {
            let mut pass_changed = self.apply_discrete_rows_for_pre_snapshot(
                snapshot, input.y, input.p, input.t, input.tol, false,
            )?;
            pass_changed |= self.apply_runtime_assignments_until_stable(
                input.y,
                input.p,
                input.t,
                input.tol,
                input.max_iters,
            )?;
            pass_changed |= project_algebraics(input.y, input.p)?;
            pass_changed |= self.apply_runtime_assignments_until_stable(
                input.y,
                input.p,
                input.t,
                input.tol,
                input.max_iters,
            )?;
            if !pass_changed {
                return Ok(changed_any);
            }
            changed_any = true;
        }
        Err(RuntimeSolveError::SolveIr(format!(
            "discrete event equations did not converge at t={}",
            input.t
        )))
    }

    fn apply_constant_discrete_rows_for_pre_snapshot(
        &self,
        snapshot: &DiscretePreSnapshot<'_>,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        self.apply_discrete_rows_for_pre_snapshot(snapshot, y, p, t, tol, true)
    }

    fn apply_discrete_rows_for_pre_snapshot(
        &self,
        snapshot: &DiscretePreSnapshot<'_>,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        skip_solver_or_time_rows: bool,
    ) -> Result<bool, RuntimeSolveError> {
        let eval_y = y.to_vec();
        let eval_p = p.to_vec();
        let sources = snapshot.event_pre_sources();
        let mut eval_p_cache = EventEvalParamCache::default();
        let mut row_values = Vec::with_capacity(self.model.problem.discrete.rhs.programs.len());
        for (row_idx, row) in self.model.problem.discrete.rhs.programs.iter().enumerate() {
            if skip_solver_or_time_rows && row_reads_solver_or_time(row) {
                continue;
            }
            let row_pre_mode = discrete_row_pre_mode(&self.model, row_idx);
            if !snapshot.row_filter.accepts(row_pre_mode) {
                continue;
            }
            let row_p = eval_p_cache.params(&self.model, &eval_p, row_pre_mode, &sources, tol);
            let row_p_with_root_overrides;
            let row_p = if snapshot.root_relation_overrides.is_empty() {
                row_p
            } else {
                row_p_with_root_overrides = event_eval_params_with_relation_overrides(
                    &self.model.problem.events.root_relation_memory_targets,
                    snapshot.root_relation_overrides,
                    row_p,
                );
                &row_p_with_root_overrides
            };
            let value =
                solve_eval::eval_row_with_context(row, &eval_y, row_p, t, self.row_eval_context())?;
            row_values.push((self.model.problem.discrete.update_targets[row_idx], value));
        }
        self.override_relation_memory_row_values(snapshot.root_relation_overrides, &mut row_values);
        let mut changed = false;
        for (target, value) in row_values {
            changed |= solve_eval::apply_scalar_slot_value(target, value, y, p, tol)?;
        }
        Ok(changed)
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
        let roots = &self.model.problem.events.root_conditions;
        if roots.is_empty() {
            return Ok(Vec::new());
        }
        let mut values = vec![0.0; roots.len()];
        solve_eval::eval_scalar_program_block_with_context(
            roots,
            y,
            p,
            t,
            self.row_eval_context(),
            &mut values,
        )?;
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
        let values = self.visible_values(solver_y, params, t)?;
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
        let values = self.visible_values(solver_y, params, t)?;
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
        recorded_times.push(t);
        push_visible_values(data, &values)
    }

    pub fn visible_values(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        if self.visible_value_rows.len() == self.model.visible_names.len() {
            let mut values = vec![0.0; self.visible_value_rows.len()];
            self.visible_value_rows.eval_with_context(
                y,
                params,
                t,
                self.row_eval_context(),
                &mut values,
            )?;
            return Ok(values);
        }
        visible_values_with_context(&self.model, y, params, t, self.row_eval_context())
    }

    pub fn visible_values_for_names(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
        names: &[String],
    ) -> Result<HashMap<String, f64>, RuntimeSolveError> {
        if self.visible_value_rows.len() == self.model.visible_names.len() {
            return self.visible_values_for_names_from_rows(y, params, t, names);
        }
        let all_values = self.visible_values(y, params, t)?;
        let mut values = HashMap::with_capacity(names.len());
        for name in names {
            if let Some(idx) = self.visible_name_index.get(name)
                && let Some(value) = all_values.get(*idx).copied()
            {
                values.insert(name.clone(), value);
            }
        }
        Ok(values)
    }

    fn visible_values_for_names_from_rows(
        &self,
        y: &[f64],
        params: &[f64],
        t: f64,
        names: &[String],
    ) -> Result<HashMap<String, f64>, RuntimeSolveError> {
        let mut values = HashMap::with_capacity(names.len());
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
            return Ok(None);
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

    fn populate_solver_y_from_state(&self, solver_y: &mut Vec<f64>, state: &[f64]) {
        solver_y.clone_from(&self.model.initial_y);
        solver_y.resize(self.solver_count, 0.0);
        for (dst, src) in solver_y.iter_mut().zip(state.iter().copied()) {
            *dst = src;
        }
    }

    // SPEC_0021: Exception - private derivative helper shares the public solver
    // callback shape while threading caller-owned scratch/output buffers.
    #[allow(clippy::too_many_arguments)]
    fn eval_state_derivatives_with_solver_y(
        &self,
        t: f64,
        state: &[f64],
        params: &[f64],
        tol: f64,
        max_iters: usize,
        solver_y: &mut Vec<f64>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.populate_solver_y_from_state(solver_y, state);
        self.refresh_derivative_dependencies(t, solver_y, params, tol, max_iters)?;
        // `eval_derivative_rhs_from_solver_y` fills `out` and *then* rejects
        // non-finite derivatives, so trace before propagating: on failure `out`
        // and `solver_y` still hold the offending values to name for the user.
        let eval_result = self.eval_derivative_rhs_from_solver_y(t, solver_y, params, out);
        crate::nan_trace::report_state_derivative(&self.model, t, solver_y, out);
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

/// Diagonal magnitude below which a residual row is treated as not constraining
/// its own target slot (a structural zero on the seed diagonal).
const SEED_DIAGONAL_EPS: f64 = 1.0e-12;

fn validate_derivative_output_len(
    out: &[f64],
    state_count: usize,
) -> Result<(), RuntimeSolveError> {
    if out.len() == state_count {
        return Ok(());
    }
    Err(RuntimeSolveError::SolveIr(format!(
        "state derivative output length {} does not match state count {}",
        out.len(),
        state_count
    )))
}

pub fn apply_discrete_slot_value(
    target: solve::ScalarSlot,
    value: f64,
    y: &mut [f64],
    p: &mut [f64],
    tol: f64,
) -> bool {
    solve_eval::apply_scalar_slot_value(target, value, y, p, tol).unwrap_or(false)
}

#[cfg(test)]
mod tests;
