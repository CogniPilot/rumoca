use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;

use crate::runtime::solve_events::event_eval_params_with_relation_overrides;
use crate::{
    RuntimeSolveError, discrete_row_active_at, discrete_row_pre_mode, row_reads_solver_or_time,
};

use super::SolveRuntime;
use super::event_update::{
    DiscretePreSnapshot, DiscreteRowEvalInput, DiscreteRowsSettleInput, EventEvalParamCache,
    EventUpdateRowFilter,
};
use super::support::{copy_runtime_values, reserve_runtime_vec_capacity};

#[derive(Clone)]
pub(super) struct PreparedStructuredDiscreteRows {
    rhs: solve_eval::PreparedScalarProgramBlock,
    rows: Vec<PreparedStructuredDiscreteRow>,
}

#[derive(Clone, Copy)]
pub(super) struct PreparedStructuredDiscreteRow {
    pub(super) source_row: usize,
    pub(super) target: solve::ScalarSlot,
    pub(super) role: solve::DiscreteRowRole,
    pub(super) pre_mode: solve::DiscreteEventPreMode,
    pub(super) observation_refresh: bool,
    pub(super) clock_owner: Option<solve::PeriodicClockId>,
}

pub(super) struct StructuredDiscreteRowEvalInput<'a, 'snapshot> {
    pub(super) snapshot: &'snapshot DiscretePreSnapshot<'a>,
    pub(super) row: PreparedStructuredDiscreteRow,
    pub(super) eval_y: &'a [f64],
    pub(super) eval_p: &'a [f64],
    pub(super) t: f64,
}

impl PreparedStructuredDiscreteRows {
    pub(super) fn new(model: &solve::SolveModel) -> Result<Self, solve_eval::EvalSolveError> {
        let scalar = solve_eval::to_scalar_program_block(&model.problem.discrete.structured_rhs)?;
        let rhs = solve_eval::PreparedScalarProgramBlock::new(scalar)?;
        let mut rows = Vec::new();
        for (update_index, update) in model.problem.discrete.structured_updates.iter().enumerate() {
            for (target, source_lane) in model
                .problem
                .discrete
                .structured_assignments(update_index)?
            {
                let source_row = rhs.single_output_row_for_output_index(source_lane).ok_or(
                    solve_eval::EvalSolveError::ShapeContract {
                        message: format!(
                            "structured discrete update {update_index} output lane {source_lane} \
                             does not have one scalar adapter row"
                        ),
                        span: None,
                    },
                )?;
                rows.push(PreparedStructuredDiscreteRow {
                    source_row,
                    target,
                    role: update.role,
                    pre_mode: update.pre_mode,
                    observation_refresh: update.observation_refresh,
                    clock_owner: update.clock_owner,
                });
            }
        }
        Ok(Self { rhs, rows })
    }

    pub(super) fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }

    pub(super) fn rows(&self) -> &[PreparedStructuredDiscreteRow] {
        &self.rows
    }
}

impl SolveRuntime {
    /// Evaluate and apply every scalar and structured discrete definition once.
    ///
    /// This is the scalar-view adapter used by deadline ticks that intentionally
    /// do not perform event filtering. The Solve IR keeps structured maps
    /// compact; expansion to `(ScalarSlot, value)` pairs exists only here.
    pub fn apply_unfiltered_discrete_rows_once(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        _tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let eval_y = copy_runtime_values(y, "unfiltered discrete y snapshot")?;
        let eval_p = copy_runtime_values(p, "unfiltered discrete p snapshot")?;
        let mut values = vec![0.0; self.discrete_rhs.len()];
        self.discrete_rhs.eval_with_context(
            &eval_y,
            &eval_p,
            t,
            self.row_eval_context(),
            &mut values,
        )?;
        let mut assignments = self
            .model
            .problem
            .discrete
            .update_targets
            .iter()
            .copied()
            .zip(values)
            .collect::<Vec<_>>();
        for row in self.structured_discrete_rows.rows().iter().copied() {
            let value = self
                .structured_discrete_rows
                .rhs
                .eval_row_unchecked_with_context(
                    row.source_row,
                    &eval_y,
                    &eval_p,
                    t,
                    self.row_eval_context(),
                )?;
            assignments.push((row.target, value));
        }
        let mut changed = false;
        for (target, value) in assignments {
            changed |= solve_eval::apply_scalar_slot_value_exact(target, value, y, p)?;
        }
        Ok(changed)
    }

    pub(super) fn structured_discrete_row_active_at(
        &self,
        row: PreparedStructuredDiscreteRow,
        t: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let Some(owner) = row.clock_owner else {
            return Ok(true);
        };
        let schedule = self
            .model
            .problem
            .clocks
            .periodic_schedule(owner)
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "structured discrete row refers to periodic clock {} outside the clock partition",
                    owner.index()
                ))
            })?;
        Ok(crate::timeline::periodic_schedule_matches_time(schedule, t))
    }

    pub(super) fn eval_structured_discrete_row_for_pre_snapshot(
        &self,
        input: StructuredDiscreteRowEvalInput<'_, '_>,
        eval_p_cache: &mut EventEvalParamCache,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let StructuredDiscreteRowEvalInput {
            snapshot,
            row,
            eval_y,
            eval_p,
            t,
        } = input;
        if row.clock_owner.is_some() && snapshot.event_iteration != 0 {
            return Ok(None);
        }
        if !self.structured_discrete_row_active_at(row, t)? {
            return Ok(None);
        }
        let pre_mode = crate::EventPreMode::from(row.pre_mode);
        if !snapshot
            .row_filter
            .accepts(pre_mode, row.clock_owner.is_some())
        {
            return Ok(None);
        }
        let row_p = eval_p_cache.params(eval_p);
        let row_p_with_root_overrides;
        let row_p = if snapshot.root_relation_overrides.is_empty() {
            row_p
        } else {
            row_p_with_root_overrides = event_eval_params_with_relation_overrides(
                &self.model.problem.events.root_relation_memory_targets,
                snapshot.root_relation_overrides,
                row_p,
            )?;
            &row_p_with_root_overrides
        };
        self.structured_discrete_rows
            .rhs
            .eval_row_unchecked_with_context(
                row.source_row,
                eval_y,
                row_p,
                t,
                self.row_eval_context(),
            )
            .map(Some)
            .map_err(Into::into)
    }
}

#[derive(Clone, Copy)]
struct DiscreteRowEvalScope {
    skip_solver_or_time_rows: bool,
    observation_only: bool,
    initialization_equations_only: bool,
}

/// One activation buffer seeded at the initialization instant.
#[derive(Clone, Copy)]
pub struct SeededConditionMemory {
    /// The parameter slot the buffer occupies.
    pub index: usize,
    /// The condition's value at the initialization instant.
    pub value: f64,
}

/// The production inputs required to seed initialization condition memory.
pub(crate) struct ConditionMemorySeedInput<'a> {
    pub(crate) model: &'a solve::SolveModel,
    pub(crate) discrete_rhs: &'a solve_eval::PreparedScalarProgramBlock,
    pub(crate) row_eval_context: solve_eval::RowEvalContext<'a>,
    pub(crate) y: &'a mut [f64],
    pub(crate) p: &'a mut [f64],
    pub(crate) t: f64,
    pub(crate) tol: f64,
}

/// The single production implementation of initialization condition-memory
/// seeding, factored from [`SolveRuntime`] so formal proofs need not construct
/// unrelated runtime state.
pub(crate) fn seed_condition_memory_for_initialization_core(
    input: ConditionMemorySeedInput<'_>,
) -> Result<Vec<SeededConditionMemory>, RuntimeSolveError> {
    let ConditionMemorySeedInput {
        model,
        discrete_rhs,
        row_eval_context,
        y,
        p,
        t,
        tol,
    } = input;
    if model
        .problem
        .events
        .condition_memory_parameter_indices
        .is_empty()
        || model.problem.discrete.rhs.is_empty()
    {
        return Ok(Vec::new());
    }
    super::validate_discrete_event_rows(model)?;
    // MLS §8.6: "Before the start of the integration, it must be guaranteed
    // that for all variables `v`, `v = pre(v)`." A condition that reads
    // `pre(s)` must therefore be seeded against `s` itself, not against
    // whatever the `pre` slot happens to hold — the lowered `pre` slots are
    // committed from the settled values only *after* the initial event
    // (`commit_pre_params_after_event`), so reading them raw here gives a
    // buffer seeded against `0.0` and `when pre(s) > 2` with `s.start = 5`
    // finds a rising edge that §8.6 says is not there.
    let mut seed_p = crate::event_eval_params_for_pre_mode(model, p, y, p, t, tol);
    // `initial()` is true only at the initial event, so the buffered value
    // it enters that event with is the value it has everywhere else.
    super::set_initial_event_flag(model, &mut seed_p, false);
    let mut seeded = Vec::new();
    let mut writes = Vec::new();
    for row_idx in 0..model.problem.discrete.rhs.len() {
        if model.problem.discrete.row_roles[row_idx] != solve::DiscreteRowRole::ConditionMemory {
            continue;
        }
        // A clocked buffer is only defined on its own ticks (MLS §16.5).
        if !discrete_row_active_at(model, row_idx, t)? {
            continue;
        }
        let value = discrete_rhs.eval_row_unchecked_with_context(
            row_idx,
            y,
            &seed_p,
            t,
            row_eval_context,
        )?;
        let target = model.problem.discrete.update_targets[row_idx];
        let solve::ScalarSlot::P { index, .. } = target else {
            return Err(RuntimeSolveError::solve_ir(format!(
                "condition-memory row {row_idx} does not target a parameter slot"
            )));
        };
        seeded.push(SeededConditionMemory { index, value });
        writes.push((target, value));
    }
    for (target, value) in writes {
        solve_eval::apply_scalar_slot_value_exact(target, value, y, p)?;
    }
    Ok(seeded)
}

impl SolveRuntime {
    /// Seed every activation buffer with the condition's value at the
    /// initialization instant, so no already-true activation has a rising edge
    /// at the initial event.
    ///
    /// MLS §8.3.5.1 gives the buffer a *value*, not a default: `when x > 2 then
    /// v1 = expr1; end when` is conceptually
    ///
    /// ```text
    /// Boolean b(start = x.start > 2);
    /// b  = x > 2;
    /// v1 = if edge(b) then expr1 else pre(v1);
    /// ```
    ///
    /// — the buffer starts at the condition evaluated on the initial values, and
    /// §8.6 then requires `v = pre(v)` before the start of the integration, so
    /// `edge(b) = b and not pre(b)` is false for a condition that is already true
    /// there. Leaving the buffer at `false` instead manufactures a rising edge at
    /// the initial event for every such condition, which is what made
    /// `when time < 0.5 then y = 1` and `when x < 2 then y = 1` run their bodies
    /// at `t = 0` where OpenModelica leaves `y = 0`. §8.6 permits exactly one
    /// `when` to run there — *"The equations of a when-clause are active during
    /// initialization, if and only if they are explicitly enabled with
    /// `initial()`"* — and that one keeps its edge because the seed is taken with
    /// the `initial()` flag cleared: `initial()` is false everywhere except the
    /// initial event, so its buffer seeds to false and its edge is the initial
    /// event itself.
    ///
    /// The seed is also what stops a *falling* condition from activating at a
    /// later instant. `when not (time > 0.5)` is true from the start; with an
    /// unseeded buffer the first event it saw — the instant `t = 0.5`, where the
    /// §8.5 buffered relation `time > 0.5` still reads false — looked like a
    /// rising edge and ran the body. Seeded, there is no edge to find.
    ///
    /// Returns the slots it wrote, so a caller holding a separate event-entry
    /// parameter snapshot can carry the same seed into it.
    pub fn seed_condition_memory_for_initialization(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
    ) -> Result<Vec<SeededConditionMemory>, RuntimeSolveError> {
        let mut seeded = seed_condition_memory_for_initialization_core(ConditionMemorySeedInput {
            model: &self.model,
            discrete_rhs: &self.discrete_rhs,
            row_eval_context: self.row_eval_context(),
            y,
            p,
            t,
            tol,
        })?;
        if self.structured_discrete_rows.is_empty() {
            return Ok(seeded);
        }
        let mut seed_p = crate::event_eval_params_for_pre_mode(&self.model, p, y, p, t, tol);
        super::set_initial_event_flag(&self.model, &mut seed_p, false);
        let mut writes = Vec::new();
        for row in self.structured_discrete_rows.rows().iter().copied() {
            if row.role != solve::DiscreteRowRole::ConditionMemory
                || !self.structured_discrete_row_active_at(row, t)?
            {
                continue;
            }
            let value = self
                .structured_discrete_rows
                .rhs
                .eval_row_unchecked_with_context(
                    row.source_row,
                    y,
                    &seed_p,
                    t,
                    self.row_eval_context(),
                )?;
            let solve::ScalarSlot::P { index, .. } = row.target else {
                return Err(RuntimeSolveError::solve_ir(
                    "structured condition-memory row does not target a parameter slot",
                ));
            };
            seeded.push(SeededConditionMemory { index, value });
            writes.push((row.target, value));
        }
        for (target, value) in writes {
            solve_eval::apply_scalar_slot_value_exact(target, value, y, p)?;
        }
        Ok(seeded)
    }

    pub(super) fn settle_discrete_rows_for_pre_snapshot<P>(
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
                snapshot,
                input.y,
                input.p,
                input.t,
                input.tol,
                DiscreteRowEvalScope {
                    skip_solver_or_time_rows: false,
                    observation_only: false,
                    initialization_equations_only: false,
                },
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
        self.solve_coupled_event_rows(snapshot, input)
            .map(|changed| changed_any | changed)
    }

    pub(super) fn apply_constant_discrete_rows_for_pre_snapshot(
        &self,
        snapshot: &DiscretePreSnapshot<'_>,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        self.apply_discrete_rows_for_pre_snapshot(
            snapshot,
            y,
            p,
            t,
            tol,
            DiscreteRowEvalScope {
                skip_solver_or_time_rows: true,
                observation_only: false,
                initialization_equations_only: true,
            },
        )
    }

    fn apply_discrete_rows_for_pre_snapshot(
        &self,
        snapshot: &DiscretePreSnapshot<'_>,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        _tol: f64,
        scope: DiscreteRowEvalScope,
    ) -> Result<bool, RuntimeSolveError> {
        self.validate_discrete_row_eval_scope(scope)?;
        let eval_y = copy_runtime_values(y, "discrete row eval y snapshot")?;
        let eval_p = copy_runtime_values(p, "discrete row eval p snapshot")?;
        let mut eval_p_cache = EventEvalParamCache::default();
        let mut row_values = Vec::new();
        reserve_runtime_vec_capacity(
            &mut row_values,
            self.model.problem.discrete.rhs.programs().len(),
            "discrete row values",
        )?;
        for (row_idx, row) in self
            .model
            .problem
            .discrete
            .rhs
            .programs()
            .iter()
            .enumerate()
        {
            if scope.observation_only && !self.observation_refresh_row(row_idx)? {
                continue;
            }
            if scope.initialization_equations_only
                && self.model.problem.discrete.row_roles[row_idx]
                    != solve::DiscreteRowRole::Equation
            {
                continue;
            }
            if scope.skip_solver_or_time_rows && row_reads_solver_or_time(row) {
                continue;
            }
            let Some(value) = self.eval_discrete_row_for_pre_snapshot(
                DiscreteRowEvalInput {
                    snapshot,
                    row_idx,
                    eval_y: &eval_y,
                    eval_p: &eval_p,
                    t,
                },
                &mut eval_p_cache,
            )?
            else {
                continue;
            };
            row_values.push((self.model.problem.discrete.update_targets[row_idx], value));
        }
        for row in self.structured_discrete_rows.rows().iter().copied() {
            if scope.observation_only && !row.observation_refresh {
                continue;
            }
            if scope.initialization_equations_only && row.role != solve::DiscreteRowRole::Equation {
                continue;
            }
            let source_program =
                &self.structured_discrete_rows.rhs.block().programs()[row.source_row];
            if scope.skip_solver_or_time_rows && row_reads_solver_or_time(source_program) {
                continue;
            }
            let Some(value) = self.eval_structured_discrete_row_for_pre_snapshot(
                StructuredDiscreteRowEvalInput {
                    snapshot,
                    row,
                    eval_y: &eval_y,
                    eval_p: &eval_p,
                    t,
                },
                &mut eval_p_cache,
            )?
            else {
                continue;
            };
            row_values.push((row.target, value));
        }
        self.override_relation_memory_row_values(snapshot.root_relation_overrides, &mut row_values);
        let mut changed = false;
        for (target, value) in row_values {
            changed |= solve_eval::apply_scalar_slot_value_exact(target, value, y, p)?;
        }
        Ok(changed)
    }

    pub(super) fn eval_discrete_row_for_pre_snapshot(
        &self,
        input: DiscreteRowEvalInput<'_, '_>,
        eval_p_cache: &mut EventEvalParamCache,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let DiscreteRowEvalInput {
            snapshot,
            row_idx,
            eval_y,
            eval_p,
            t,
        } = input;
        if !discrete_row_active_at(&self.model, row_idx, t)? {
            return Ok(None);
        }
        let row_pre_mode = discrete_row_pre_mode(&self.model, row_idx)?;
        let clock_owned = self.model.problem.discrete.clock_owners[row_idx].is_some();
        if clock_owned && snapshot.event_iteration != 0 {
            return Ok(None);
        }
        if !snapshot.row_filter.accepts(row_pre_mode, clock_owned) {
            return Ok(None);
        }
        let row_p = eval_p_cache.params(eval_p);
        let row_p_with_root_overrides;
        let row_p = if snapshot.root_relation_overrides.is_empty() {
            row_p
        } else {
            row_p_with_root_overrides = event_eval_params_with_relation_overrides(
                &self.model.problem.events.root_relation_memory_targets,
                snapshot.root_relation_overrides,
                row_p,
            )?;
            &row_p_with_root_overrides
        };
        self.discrete_rhs
            .eval_row_unchecked_with_context(row_idx, eval_y, row_p, t, self.row_eval_context())
            .map(Some)
            .map_err(Into::into)
    }

    pub fn refresh_observation_discrete_rows(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        if self.model.problem.discrete.observation_refresh.is_empty()
            && !self
                .structured_discrete_rows
                .rows()
                .iter()
                .any(|row| row.observation_refresh)
        {
            return Ok(false);
        }
        self.validate_observation_refresh_rows()?;
        let mut changed_any = false;
        for _ in 0..max_iters {
            let snapshot = DiscretePreSnapshot {
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &[],
                // Preserve the existing observation-refresh policy: fixed
                // rows keep the observation-entry pre snapshot for the whole
                // refresh loop.
                event_iteration: 0,
            };
            let changed = self.apply_discrete_rows_for_pre_snapshot(
                &snapshot,
                y,
                p,
                t,
                tol,
                DiscreteRowEvalScope {
                    skip_solver_or_time_rows: false,
                    observation_only: true,
                    initialization_equations_only: false,
                },
            )?;
            if !changed {
                return Ok(changed_any);
            }
            changed_any = true;
        }
        Err(RuntimeSolveError::solve_ir(
            "observation-time discrete refresh did not converge",
        ))
    }

    fn validate_discrete_row_eval_scope(
        &self,
        scope: DiscreteRowEvalScope,
    ) -> Result<(), RuntimeSolveError> {
        if scope.observation_only {
            self.validate_observation_refresh_rows()?;
        }
        Ok(())
    }

    fn validate_observation_refresh_rows(&self) -> Result<(), RuntimeSolveError> {
        let observation_rows = self.model.problem.discrete.observation_refresh.len();
        let rhs_rows = self.model.problem.discrete.rhs.len();
        if observation_rows == rhs_rows {
            return Ok(());
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "discrete observation-refresh row count {observation_rows} does not match discrete RHS row count {rhs_rows}"
        )))
    }

    fn observation_refresh_row(&self, row_idx: usize) -> Result<bool, RuntimeSolveError> {
        self.model
            .problem
            .discrete
            .observation_refresh
            .get(row_idx)
            .copied()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "discrete observation-refresh row index {row_idx} is out of bounds"
                ))
            })
    }
}
