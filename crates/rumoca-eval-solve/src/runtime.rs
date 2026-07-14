use indexmap::IndexMap;
use rumoca_ir_solve as solve;
use rumoca_solver::{
    EventActionOutcome, RuntimeEventStop, RuntimeSolveError, SolveStopSchedule,
    push_visible_values, relation_memory_value_from_root, replace_last_visible_values,
    timeline::sample_time_match_with_tol, update_relation_memory_slots,
};
use std::{cell::RefCell, collections::HashMap};

use crate::refresh_plan::{
    AlgebraicRefreshRow, RefreshPlan, build_algebraic_refresh_plan, build_derivative_refresh_plan,
    build_root_refresh_plan, trace_refresh_plan,
};
use crate::runtime_events::{
    apply_discrete_slot_values, current_dynamic_time_event_stop, eval_event_actions_with_context,
    next_runtime_event_stop, visible_values_with_context,
};
use crate::{
    self as solve_eval, EvalSolveError, PreparedComputeBlock, PreparedScalarProgramBlock,
    RowEvalContext, to_scalar_program_block,
};

mod discrete_rows;
mod event_update;
mod initial_event;
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
    NewtonProbe, copy_runtime_values, copy_runtime_values_into, reserve_runtime_index_map_capacity,
    reserve_runtime_vec_capacity, resize_runtime_values, write_refresh_targets,
    zero_runtime_values,
};

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

pub struct RootSearchInput<'a> {
    pub t: f64,
    pub state: &'a [f64],
    pub params: &'a [f64],
    pub guess: &'a mut Vec<f64>,
    pub tol: f64,
    pub max_iters: usize,
    pub out: &'a mut [f64],
}

pub struct ProjectedRuntimeSettleInput<'a> {
    pub y: &'a mut [f64],
    pub p: &'a mut [f64],
    pub t: f64,
    pub tol: f64,
    pub max_iters: usize,
    pub root_relation_overrides: &'a [(usize, f64)],
}

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
    implicit_scalar_rhs: PreparedScalarProgramBlock,
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
    /// (`d(residual_row)/d(y)·v`). Used to propagate the state seed through the
    /// algebraic projection (`d(alg)/d(state)`) row by row.
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
    runtime_state: solve_eval::SimulationRuntimeState,
    derivative_scratch: RefCell<StateDerivativeScratch>,
    root_scratch: RefCell<Vec<f64>>,
    /// Reusable register tape / adjoint buffers for the reverse-mode VJP sweep,
    /// kept across calls so a hot reverse loop stays allocation-free.
    reverse_scratch: RefCell<crate::reverse::ReverseScratch>,
}

impl SolveRuntime {
    pub fn new(model: &solve::SolveModel) -> Result<Self, EvalSolveError> {
        let implicit_scalar_programs =
            to_scalar_program_block(&model.problem.continuous.implicit_rhs)?;
        let implicit_scalar_rhs = PreparedScalarProgramBlock::new(implicit_scalar_programs)?;
        let derivative_scalar_rhs =
            to_scalar_program_block(&model.problem.continuous.derivative_rhs)?;
        let algebraic_refresh = build_algebraic_refresh_plan(model, &implicit_scalar_rhs)?;
        let derivative_refresh =
            build_derivative_refresh_plan(model, &derivative_scalar_rhs, &algebraic_refresh)?;
        let root_refresh = build_root_refresh_plan(model, &algebraic_refresh)?;
        trace_refresh_plan(model, "algebraic", &algebraic_refresh);
        trace_refresh_plan(model, "derivative", &derivative_refresh);
        trace_refresh_plan(model, "root", &root_refresh);
        let visible_value_plan = visible_value_plan(model);
        let root_condition_plan = root_condition_plan(model);
        Ok(Self {
            model: model.clone(),
            state_count: model.state_scalar_count(),
            solver_count: model.solver_scalar_count(),
            implicit_rhs: PreparedComputeBlock::new_with_label(
                &model.problem.continuous.implicit_rhs,
                "runtime_implicit_rhs",
            )?,
            implicit_scalar_rhs,
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
            runtime_state: solve_eval::SimulationRuntimeState::new(),
            derivative_scratch: RefCell::new(StateDerivativeScratch::default()),
            root_scratch: RefCell::new(Vec::new()),
            reverse_scratch: RefCell::new(crate::reverse::ReverseScratch::default()),
        })
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
        guess: &mut Vec<f64>,
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        if guess.len() != self.solver_count {
            copy_runtime_values_into(guess, &self.model.initial_y, "initial solver guess")?;
            resize_runtime_values(guess, self.solver_count, 0.0, "initial solver guess")?;
        }
        self.overwrite_state_slots_preserving_algebraics(guess, state)?;
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
            return Err(RuntimeSolveError::solve_ir(format!(
                "refresh plan requires algebraic/output dependencies without producer rows: {}",
                self.missing_dependency_names(&plan.missing_dependencies)
            )));
        }
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
        // Coupled projection blocks may have multiple fixed points. Iterating
        // their assignment map can converge cleanly to a remote, non-physical
        // root even when the caller supplied an accepted local branch seed.
        // Solve the coupled residual directly from that seed so branch
        // continuity, rather than fixed-point attraction, selects the root.
        let snapshot = self.refresh_target_snapshot(rows, solver_y)?;
        self.refresh_slots_newton(
            rows,
            &snapshot,
            RefreshSlotArgs {
                t,
                solver_y,
                params,
                tol,
                max_iters,
            },
        )
    }

    fn refresh_target_snapshot(
        &self,
        rows: &[AlgebraicRefreshRow],
        solver_y: &[f64],
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let mut snapshot = Vec::new();
        reserve_runtime_vec_capacity(&mut snapshot, rows.len(), "Newton snapshot")?;
        snapshot.extend(rows.iter().map(|row| solver_y[row.target_index]));
        Ok(snapshot)
    }

    /// Solve the coupled refresh subsystem `x = F(x)` with damped Newton on
    /// `G(x) = x - F(x)` using a finite-difference Jacobian. Gauss-Seidel
    /// sweeps diverge whenever the algebraic dependency cycle has gain > 1
    /// (any geared torque loop); Newton solves linear cycles exactly in one
    /// step and handles mildly nonlinear ones.
    fn refresh_slots_newton(
        &self,
        rows: &[AlgebraicRefreshRow],
        snapshot: &[f64],
        args: RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        let RefreshSlotArgs {
            t,
            solver_y,
            params,
            tol,
            max_iters: requested_max_iters,
        } = args;
        const MAX_NEWTON_REFRESH_ROWS: usize = 256;
        const MAX_NEWTON_ITERS: usize = 25;
        let m = rows.len();
        tracing::debug!(target: "rumoca_eval_solve::refresh", "newton fallback: rows={m}");
        if m == 0 || m > MAX_NEWTON_REFRESH_ROWS {
            return Err(self.refresh_convergence_error(
                0,
                &RefreshIterationMax {
                    delta: f64::INFINITY,
                    target: None,
                },
            ));
        }

        let mut x = Vec::new();
        reserve_runtime_vec_capacity(&mut x, snapshot.len(), "Newton iterate")?;
        x.extend(snapshot);
        let max_newton_iters = requested_max_iters.max(MAX_NEWTON_ITERS);
        let mut convergence_polish_steps = 0usize;
        for _ in 0..max_newton_iters {
            let f_base = self.refresh_newton_sweep(rows, &x, t, solver_y, params)?;
            let mut residual = Vec::new();
            reserve_runtime_vec_capacity(&mut residual, x.len(), "Newton residual")?;
            residual.extend(x.iter().zip(&f_base).map(|(xi, fi)| xi - fi));
            let max_residual = residual.iter().fold(0.0_f64, |acc, r| acc.max(r.abs()));
            tracing::debug!(target: "rumoca_eval_solve::refresh", "newton residual={max_residual:e}");
            let converged = max_residual <= tol;
            let probe = NewtonProbe {
                rows,
                x: &x,
                f_base: &f_base,
                residual: &residual,
                t,
                params,
            };
            let mut augmented = self.refresh_newton_augmented(probe, solver_y)?;
            let solved = crate::linear_solve::gaussian_eliminate(&mut augmented).is_some();
            if !solved && converged {
                write_refresh_targets(rows, &x, solver_y);
                return Ok(());
            }
            if !solved {
                return Err(self.refresh_newton_failure(rows));
            }
            if converged && !newton_step_stays_near_accepted_branch(&augmented, &x, tol) {
                write_refresh_targets(rows, &x, solver_y);
                return Ok(());
            }
            let improved = self.apply_damped_refresh_newton_step(
                rows,
                &mut x,
                &augmented,
                max_residual,
                t,
                solver_y,
                params,
            )?;
            // A tolerance-converged coupled solve still carries the residual
            // directly into reconstructed connection-flow observations. One
            // final Newton correction preserves the already-selected branch
            // while polishing affine conservation identities (for example a
            // grounded pin current reconstructed as the difference of two
            // equal branch currents) to floating-point consistency.
            if converged && (!improved || max_residual == 0.0 || convergence_polish_steps >= 3) {
                write_refresh_targets(rows, &x, solver_y);
                return Ok(());
            }
            if converged {
                convergence_polish_steps += 1;
                continue;
            }
            if !improved {
                return Err(self.refresh_newton_failure(rows));
            }
        }
        Err(self.refresh_newton_failure(rows))
    }

    #[allow(clippy::too_many_arguments)]
    fn apply_damped_refresh_newton_step(
        &self,
        rows: &[AlgebraicRefreshRow],
        x: &mut Vec<f64>,
        augmented: &crate::linear_solve::AugmentedMatrix,
        base_norm: f64,
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<bool, RuntimeSolveError> {
        const MAX_BACKTRACKS: usize = 24;
        const ARMIJO_SLOPE: f64 = 1.0e-4;
        let m = x.len();
        let mut alpha = 1.0;
        for _ in 0..MAX_BACKTRACKS {
            let mut trial = x.clone();
            if (0..m).any(|j| !augmented.get(j, m).is_finite()) {
                write_refresh_targets(rows, x, solver_y);
                return Ok(false);
            }
            for (j, value) in trial.iter_mut().enumerate() {
                let step = augmented.get(j, m);
                *value += alpha * step;
            }
            let f_trial = match self.refresh_newton_sweep(rows, &trial, t, solver_y, params) {
                Ok(values) => values,
                Err(_) => {
                    alpha *= 0.5;
                    continue;
                }
            };
            let trial_norm = trial
                .iter()
                .zip(&f_trial)
                .map(|(xi, fi)| (xi - fi).abs())
                .fold(0.0_f64, f64::max);
            if trial_norm.is_finite() && trial_norm <= base_norm * (1.0 - ARMIJO_SLOPE * alpha) {
                *x = trial;
                return Ok(true);
            }
            alpha *= 0.5;
        }
        write_refresh_targets(rows, x, solver_y);
        Ok(false)
    }

    /// Evaluate the refresh map `F` at `x` (writing `x` into the target slots
    /// first).
    fn refresh_newton_sweep(
        &self,
        rows: &[AlgebraicRefreshRow],
        x: &[f64],
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        write_refresh_targets(rows, x, solver_y);
        let mut values = Vec::new();
        reserve_runtime_vec_capacity(&mut values, rows.len(), "Newton sweep values")?;
        for row in rows {
            let value = self.eval_refresh_row_value(row, t, solver_y, params)?;
            if !value.is_finite() {
                return Err(self.non_finite_value_error(row.target_index, value));
            }
            values.push(value);
        }
        Ok(values)
    }

    /// `J = I - dF/dx` by forward differences, augmented with `-residual`.
    fn refresh_newton_augmented(
        &self,
        probe: NewtonProbe<'_>,
        solver_y: &mut [f64],
    ) -> Result<crate::linear_solve::AugmentedMatrix, RuntimeSolveError> {
        let NewtonProbe {
            rows,
            x,
            f_base,
            residual,
            t,
            params,
        } = probe;
        let m = rows.len();
        let mut augmented =
            crate::linear_solve::AugmentedMatrix::zeroed(m).map_err(RuntimeSolveError::from)?;
        for j in 0..m {
            let eps = 1.0e-8_f64.max(1.0e-8 * x[j].abs());
            let mut probe_x = Vec::new();
            reserve_runtime_vec_capacity(&mut probe_x, x.len(), "Newton probe")?;
            probe_x.extend(x);
            probe_x[j] += eps;
            let f_probe = self.refresh_newton_sweep(rows, &probe_x, t, solver_y, params)?;
            for i in 0..m {
                let df = (f_probe[i] - f_base[i]) / eps;
                augmented.set(i, j, f64::from(i == j) - df);
            }
        }
        for (i, value) in residual.iter().enumerate() {
            augmented.set(i, m, -*value);
        }
        Ok(augmented)
    }

    fn refresh_newton_failure(&self, rows: &[AlgebraicRefreshRow]) -> RuntimeSolveError {
        let _ = rows;
        tracing::debug!(target: "rumoca_eval_solve::refresh", "newton fallback FAILED");
        self.refresh_convergence_error(
            0,
            &RefreshIterationMax {
                delta: f64::INFINITY,
                target: None,
            },
        )
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
        if let Some(value) = self.solve_refresh_residual_row(row, residual, t, solver_y, params)? {
            return Ok(value);
        }
        for candidate in &row.alternatives {
            if candidate.row_idx == row.row_idx && candidate.output_offset == row.output_offset {
                continue;
            }
            let candidate_row = AlgebraicRefreshRow {
                row_idx: candidate.row_idx,
                output_offset: candidate.output_offset,
                target_index: row.target_index,
                assignment_target: candidate.assignment_target,
                alternatives: Vec::new(),
            };
            let residual = self.refresh_row_residual(&candidate_row, t, solver_y, params)?;
            if let Some(value) =
                self.solve_refresh_residual_row(&candidate_row, residual, t, solver_y, params)?
            {
                return Ok(value);
            }
        }
        Err(self.refresh_row_independent_error(row))
    }

    /// Residual of a refresh row at the current point. Rows lowered with an
    /// assignment shape evaluate the residual directly; shapeless rows with an
    /// implicit target evaluate the target value, so the residual is
    /// `value - current_target`.
    fn refresh_row_residual(
        &self,
        row: &AlgebraicRefreshRow,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<f64, RuntimeSolveError> {
        let raw = self
            .implicit_scalar_rhs
            .eval_row_output_unchecked_with_context(
                row.row_idx,
                row.output_offset,
                solver_y,
                params,
                t,
                self.row_eval_context(),
            )?;
        if let Some(target) = self.refresh_row_subtracted_target_index(row) {
            return Ok(raw - solver_y[target]);
        }
        Ok(raw)
    }

    /// Target slot subtracted from raw row output when a shapeless row evaluates
    /// an implicit target value instead of a residual. The seed/JVP refresh uses
    /// the same predicate so value projection and sensitivity projection solve
    /// the same residual.
    fn refresh_row_subtracted_target_index(&self, row: &AlgebraicRefreshRow) -> Option<usize> {
        let own = row.assignment_target?;
        // `raw - current_target` is only valid when the row evaluates to the
        // target's *value* (an expression in the other unknowns). A row that
        // reads its own target is already a residual in it — e.g. a flow-sum
        // `... + own + ... = 0` whose `raw` is affine in `own` with a +1
        // coefficient. Subtracting `own` there cancels that dependence and
        // leaves a residual with zero slope, so the linear solve reports the
        // target as undeterminable. Use the bare residual in that case (same
        // as assignment-shape rows), which Newton-solves correctly.
        if !self
            .implicit_scalar_rhs
            .row_has_assignment_shape(row.row_idx)
            && !self.implicit_scalar_rhs.row_reads_y(row.row_idx, own)
        {
            return Some(own);
        }
        None
    }

    fn solve_refresh_residual_row(
        &self,
        row: &AlgebraicRefreshRow,
        residual: f64,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
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
            return Ok(Some(current - residual / slope));
        }
        Ok(None)
    }

    fn refresh_row_independent_error(&self, row: &AlgebraicRefreshRow) -> RuntimeSolveError {
        // A residual that does not respond to the paired variable means the
        // refresh plan paired this row with a variable it cannot determine.
        // Nudging the value by the residual (the old fallback) converges to a
        // wrong but stable solution; fail loudly instead.
        RuntimeSolveError::UnsupportedModel {
            reason: format!(
                "algebraic refresh row {} cannot be solved for '{}': the residual does not depend on it",
                row.row_idx,
                self.solver_name(row.target_index)
            ),
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
        if self.can_batch_assignment_refresh(plan) {
            return self
                .implicit_scalar_rhs
                .apply_target_assignment_rows_unchecked_with_context(
                    plan,
                    solver_y,
                    params,
                    t,
                    self.row_eval_context(),
                )
                .map_err(Into::into);
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
        let mut values = zero_runtime_values(roots.len(), "root condition output")?;
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
        self.populate_solver_y_from_state(&mut solver_y, state)?;
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
        self.eval_root_conditions_from_refreshed_solver_y(t, &solver_y, params, out)?;
        Ok(())
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
        let roots = &self.model.problem.events.root_conditions;
        if roots.is_empty() {
            if let Some(first) = out.first_mut() {
                *first = 1.0;
            }
            return Ok(());
        }
        let Some(plan) = &self.root_condition_plan else {
            return self.eval_root_conditions_into(t, state, params, tol, max_iters, out);
        };
        self.validate_root_plan_output_len(plan, out)?;
        if plan.search_rows.is_empty() {
            self.write_planned_root_search_defaults(plan, params, t, out)?;
            return Ok(());
        }
        let mut solver_y = self.root_scratch.borrow_mut();
        self.populate_solver_y_from_state(&mut solver_y, state)?;
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
        self.write_planned_root_search_conditions(plan, &solver_y, params, t, out)
    }

    /// Evaluate root-search values from an explicit algebraic branch seed.
    ///
    /// The caller owns `guess`: state slots are replaced by `state`, while the
    /// remaining solver slots retain the supplied branch before the root
    /// dependency projection is settled. This keeps adaptive-solver trial
    /// callbacks free of hidden mutable runtime state.
    pub fn eval_root_search_conditions_with_guess_into(
        &self,
        input: RootSearchInput<'_>,
    ) -> Result<(), RuntimeSolveError> {
        let RootSearchInput {
            t,
            state,
            params,
            guess,
            tol,
            max_iters,
            out,
        } = input;
        let roots = &self.model.problem.events.root_conditions;
        if roots.is_empty() {
            if let Some(first) = out.first_mut() {
                *first = 1.0;
            }
            return Ok(());
        }
        if guess.len() != self.solver_count {
            copy_runtime_values_into(guess, &self.model.initial_y, "root solver guess")?;
            resize_runtime_values(guess, self.solver_count, 0.0, "root solver guess")?;
        }
        self.overwrite_state_slots_preserving_algebraics(guess, state)?;
        let Some(plan) = &self.root_condition_plan else {
            self.refresh_slots_with_plan(
                &self.root_refresh,
                RefreshSlotArgs {
                    t,
                    solver_y: guess,
                    params,
                    tol,
                    max_iters,
                },
            )?;
            return self.eval_root_conditions_from_refreshed_solver_y(t, guess, params, out);
        };
        self.validate_root_plan_output_len(plan, out)?;
        if plan.search_rows.is_empty() {
            return self.write_planned_root_search_defaults(plan, params, t, out);
        }
        self.refresh_slots_with_plan(
            &self.root_refresh,
            RefreshSlotArgs {
                t,
                solver_y: guess,
                params,
                tol,
                max_iters,
            },
        )?;
        self.write_planned_root_search_conditions(plan, guess, params, t, out)
    }

    pub fn neutralize_initial_root_search_values(
        &self,
        params: &[f64],
        tol: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let root_count = self.model.problem.events.root_conditions.output_count();
        let targets = &self.model.problem.events.root_relation_memory_targets;
        if targets.len() != root_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "root relation metadata length {} does not match root output count {root_count}",
                targets.len()
            )));
        }
        if out.len() < root_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "root search output has {} values for {root_count} roots",
                out.len()
            )));
        }
        for (root_index, target) in targets.iter().copied().enumerate() {
            if out[root_index] != 0.0 {
                continue;
            }
            let Some(target) = target else {
                continue;
            };
            let solve::ScalarSlot::P { index, .. } = target else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "root crossing index {root_index} has non-parameter relation memory target"
                )));
            };
            let current = params.get(index).copied().ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "root crossing index {root_index} relation memory parameter {index} is outside parameter storage"
                ))
            })?;
            out[root_index] = if current.abs() <= tol {
                1.0
            } else if (current - 1.0).abs() <= tol {
                -1.0
            } else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "root crossing index {root_index} relation memory value {current} is not boolean"
                )));
            };
        }
        Ok(())
    }

    pub fn apply_consumed_root_search_overrides(
        &self,
        params: &[f64],
        tol: f64,
        overrides: &[(usize, f64)],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let root_count = self.model.problem.events.root_conditions.output_count();
        let targets = &self.model.problem.events.root_relation_memory_targets;
        if targets.len() != root_count || out.len() < root_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "root relation metadata/output shape does not match {root_count} roots"
            )));
        }
        for &(root_index, post) in overrides {
            let target = targets.get(root_index).copied().flatten().ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "consumed root index {root_index} has no relation memory target"
                ))
            })?;
            let solve::ScalarSlot::P { index, .. } = target else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "consumed root index {root_index} has non-parameter relation memory target"
                )));
            };
            let current = params.get(index).copied().ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "consumed root index {root_index} relation memory parameter {index} is outside parameter storage"
                ))
            })?;
            if (current - post).abs() > tol {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "consumed root index {root_index} post-side {post} does not match relation memory {current}"
                )));
            }
            out[root_index] = if post.abs() <= tol {
                1.0
            } else if (post - 1.0).abs() <= tol {
                -1.0
            } else {
                return Err(RuntimeSolveError::solve_ir(format!(
                    "consumed root index {root_index} post-side {post} is not boolean"
                )));
            };
        }
        Ok(())
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
        self.settle_projected_runtime_and_relation_memory_with_overrides(
            ProjectedRuntimeSettleInput {
                y,
                p,
                t,
                tol,
                max_iters,
                root_relation_overrides: &[],
            },
            &mut project_algebraics,
        )
    }

    pub fn settle_projected_runtime_and_relation_memory_with_overrides<P>(
        &self,
        input: ProjectedRuntimeSettleInput<'_>,
        mut project_algebraics: P,
    ) -> Result<(), RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        let ProjectedRuntimeSettleInput {
            y,
            p,
            t,
            tol,
            max_iters,
            root_relation_overrides,
        } = input;
        for _ in 0..max_iters {
            let mut changed =
                self.apply_root_relation_memory_overrides(root_relation_overrides, y, p, tol)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            changed |= project_algebraics(y, p)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            changed |= self.update_relation_memory_from_solver_y_excluding(
                t,
                y,
                p,
                root_relation_overrides,
            )?;
            changed |=
                self.apply_root_relation_memory_overrides(root_relation_overrides, y, p, tol)?;
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
            return self.settle_runtime_assignments_and_projection(
                ProjectedRuntimeSettleInput {
                    y,
                    p,
                    t,
                    tol,
                    max_iters,
                    root_relation_overrides,
                },
                &mut project_algebraics,
            );
        }

        for event_iteration in 0..max_iters {
            let mut changed =
                self.apply_root_relation_memory_overrides(root_relation_overrides, y, p, tol)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            changed |= project_algebraics(y, p)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            changed |= self.update_relation_memory_from_solver_y_excluding(
                t,
                y,
                p,
                root_relation_overrides,
            )?;
            changed |=
                self.apply_root_relation_memory_overrides(root_relation_overrides, y, p, tol)?;
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
            changed |= self.update_relation_memory_from_solver_y_excluding(
                t,
                y,
                p,
                root_relation_overrides,
            )?;
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
        input: ProjectedRuntimeSettleInput<'_>,
        project_algebraics: &mut P,
    ) -> Result<EventActionOutcome, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        let ProjectedRuntimeSettleInput {
            y,
            p,
            t,
            tol,
            max_iters,
            root_relation_overrides,
        } = input;
        for _ in 0..max_iters {
            let mut changed =
                self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            changed |= project_algebraics(y, p)?;
            changed |= self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
            changed |= self.update_relation_memory_from_solver_y_excluding(
                t,
                y,
                p,
                root_relation_overrides,
            )?;
            changed |=
                self.apply_root_relation_memory_overrides(root_relation_overrides, y, p, tol)?;
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

    fn update_relation_memory_from_solver_y_excluding(
        &self,
        t: f64,
        y: &[f64],
        p: &mut [f64],
        excluded_roots: &[(usize, f64)],
    ) -> Result<bool, RuntimeSolveError> {
        let relation_memory_indices = &self
            .model
            .problem
            .solve_layout
            .relation_memory_parameter_indices;
        let roots = self.eval_root_conditions_from_solver_y(t, y, p)?;
        let mut changed = false;
        for (root_index, (root, parameter_index)) in
            roots.iter().zip(relation_memory_indices).enumerate()
        {
            if excluded_roots.iter().any(|(index, _)| *index == root_index) {
                continue;
            }
            if let Some(slot) = p.get_mut(*parameter_index) {
                let value = relation_memory_value_from_root(*root);
                changed |= *slot != value;
                *slot = value;
            }
        }
        Ok(changed)
    }

    pub fn eval_root_conditions_from_solver_y(
        &self,
        t: f64,
        y: &[f64],
        p: &[f64],
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        if self.model.problem.events.root_conditions.is_empty() {
            return Ok(Vec::new());
        }
        let mut values =
            zero_runtime_values(self.root_condition_rows.len(), "root condition output")?;
        self.eval_root_conditions_from_refreshed_solver_y(t, y, p, &mut values)?;
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
        self.overwrite_state_slots_preserving_algebraics(solver_y, state)
    }

    fn overwrite_state_slots_preserving_algebraics(
        &self,
        solver_y: &mut [f64],
        state: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        if solver_y.len() != self.solver_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "solver y has {} values, expected {}",
                solver_y.len(),
                self.solver_count
            )));
        }
        if state.len() < self.state_count {
            return Err(RuntimeSolveError::solve_ir(format!(
                "state has {} values, expected at least {}",
                state.len(),
                self.state_count
            )));
        }
        for (dst, src) in solver_y[..self.state_count]
            .iter_mut()
            .zip(state.iter().copied())
        {
            *dst = src;
        }
        Ok(())
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
        self.populate_solver_y_from_state(solver_y, state)?;
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
const SEED_DIAGONAL_EPS: f64 = 1.0e-12;

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

fn newton_step_stays_near_accepted_branch(
    augmented: &crate::linear_solve::AugmentedMatrix,
    accepted: &[f64],
    tol: f64,
) -> bool {
    const POLISH_TRUST_FACTOR: f64 = 8.0;
    let rhs_column = accepted.len();
    accepted.iter().enumerate().all(|(row, value)| {
        let step = augmented.get(row, rhs_column).abs();
        let limit = POLISH_TRUST_FACTOR * tol * value.abs().max(1.0);
        step.is_finite() && step <= limit
    })
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
