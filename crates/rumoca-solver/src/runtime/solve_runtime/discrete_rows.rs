use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;

use crate::runtime::solve_events::event_eval_params_with_relation_overrides;
use crate::{RuntimeSolveError, discrete_row_active_at, row_reads_solver_or_time};

use super::SolveRuntime;
use super::event_update::{
    DiscretePreSnapshot, DiscreteRowEvalInput, DiscreteRowsSettleInput, EventEvalParamCache,
    EventUpdateRowFilter,
};
use super::support::{
    copy_runtime_values, copy_runtime_values_into, reserve_runtime_vec_capacity,
    resize_runtime_values,
};

#[derive(Clone)]
pub(super) struct PreparedStructuredDiscreteRows {
    rhs: solve_eval::PreparedScalarProgramBlock,
    rows: Vec<PreparedStructuredDiscreteRow>,
}

#[derive(Clone, Copy)]
pub(super) struct PreparedStructuredDiscreteRow {
    pub(super) update_index: usize,
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

#[derive(Clone, Copy)]
struct DiscreteSnapshotEvalInput<'snapshot, 'pre, 'values> {
    snapshot: &'snapshot DiscretePreSnapshot<'pre>,
    eval_y: &'values [f64],
    eval_p: &'values [f64],
    t: f64,
    scope: DiscreteRowEvalScope,
}

type DiscreteRowValue = (solve::ScalarSlot, f64);
type GuardedRowValues = (usize, Vec<f64>);

impl PreparedStructuredDiscreteRows {
    pub(super) fn new(
        model: &solve::SolveModel,
        scalar: solve::ScalarProgramBlock,
    ) -> Result<Self, solve_eval::EvalSolveError> {
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
                    update_index,
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
    /// Deadline ticks call this path when they intentionally skip event filtering.
    /// Guarded assignments remain compact owner tuples until their checked target
    /// ranges are traversed at the final runtime storage-write boundary below.
    pub fn apply_unfiltered_discrete_rows_once(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        _tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let eval_y = copy_runtime_values(y, "unfiltered discrete y snapshot")?;
        let eval_p = copy_runtime_values(p, "unfiltered discrete p snapshot")?;
        let ordered_clocked = self.clock_partition_owns_clocked_rows();
        let mut evaluated_transactions = Vec::new();
        for transaction_index in 0..self.event_transaction_programs.len() {
            if self.model.problem.discrete.event_transactions[transaction_index].is_clock_owned() {
                continue;
            }
            self.eval_event_transaction_outputs(transaction_index, &eval_y, &eval_p, t)?;
            evaluated_transactions.push(transaction_index);
        }
        let mut assignments = Vec::new();
        let mut guarded_values = Vec::with_capacity(self.guarded_assignment_programs.len());
        // SOLVE-C57: clock-owned producers execute in issued causal order over
        // private work state, so same-tick reads observe this tick's values on
        // the deadline path exactly as they do under event iteration.
        self.execute_clock_partition_steps(
            ClockPartitionPassMode::Unfiltered,
            ClockPartitionEntry {
                eval_y: &eval_y,
                eval_p: &eval_p,
            },
            t,
            ClockPartitionOutputs {
                row_values: &mut assignments,
                guarded_values: &mut guarded_values,
                evaluated_transactions: &mut evaluated_transactions,
            },
        )?;
        for row_idx in 0..self.model.problem.discrete.rhs.len() {
            if self.event_transaction_coverage.discrete_rows[row_idx] {
                continue;
            }
            if ordered_clocked && self.model.problem.discrete.clock_owners[row_idx].is_some() {
                continue;
            }
            let (program, output) = self
                .discrete_rhs
                .row_output_position(row_idx)
                .ok_or_else(|| RuntimeSolveError::solve_ir("discrete output has no producer"))?;
            let value = self.discrete_rhs.eval_row_output_unchecked_with_context(
                program,
                output,
                &eval_y,
                &eval_p,
                t,
                self.row_eval_context(),
            )?;
            assignments.push((self.model.problem.discrete.update_targets[row_idx], value));
        }
        for program_index in 0..self.guarded_assignment_programs.len() {
            if self.event_transaction_coverage.guarded_assignments[program_index] {
                continue;
            }
            if ordered_clocked
                && self.model.problem.discrete.guarded_assignments[program_index]
                    .clock_owner()
                    .is_some()
            {
                continue;
            }
            if !self.guarded_assignment_active_at(program_index, t)? {
                continue;
            }
            let mut values = Vec::new();
            self.eval_guarded_assignment_outputs(program_index, &eval_y, &eval_p, t, &mut values)?;
            guarded_values.push((program_index, values));
        }
        for row in self.structured_discrete_rows.rows().iter().copied() {
            if self.event_transaction_coverage.structured_updates[row.update_index] {
                continue;
            }
            if ordered_clocked && row.clock_owner.is_some() {
                continue;
            }
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
        for (program_index, values) in guarded_values {
            changed |= self.apply_guarded_assignment_outputs(program_index, &values, &[], y, p)?;
        }
        changed |= self.commit_successful_event_transactions(evaluated_transactions, y, p)?;
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

/// How one SOLVE-C57 ordered clock-partition pass admits its steps.
#[derive(Clone, Copy)]
enum ClockPartitionPassMode<'a, 'snapshot> {
    /// Ordinary event iteration: every existing per-row admission rule
    /// (activity, row filter, scope, first-iteration-only) applies unchanged;
    /// only the execution coordinate changes from a frozen snapshot to the
    /// issued causal order over private work state.
    Filtered {
        snapshot: &'a DiscretePreSnapshot<'snapshot>,
        scope: DiscreteRowEvalScope,
    },
    /// Deadline ticks that intentionally skip event filtering
    /// ([`SolveRuntime::apply_unfiltered_discrete_rows_once`]). Scalar and
    /// structured steps run unconditionally; a guarded owner keeps its clock
    /// activity check, exactly as the unfiltered path always has.
    Unfiltered,
}

/// The pass-entry state the private work buffers are seeded from.
#[derive(Clone, Copy)]
struct ClockPartitionEntry<'a> {
    eval_y: &'a [f64],
    eval_p: &'a [f64],
}

/// Where the issued producers append the values the pass commits atomically.
struct ClockPartitionOutputs<'a> {
    row_values: &'a mut Vec<DiscreteRowValue>,
    guarded_values: &'a mut Vec<GuardedRowValues>,
    evaluated_transactions: &'a mut Vec<usize>,
}

impl SolveRuntime {
    /// Execute the issued SOLVE-C57 clock-partition schedule once, in rank
    /// order, against private work state seeded from the pass-entry snapshot.
    ///
    /// An ordinary same-instant read therefore observes this tick's value of
    /// every earlier producer (SPEC_0046 SDO-002), while `pre`/`previous`/
    /// `sample(u)` reads keep their history lanes: those lanes live in
    /// dedicated parameter slots no producer targets. Producer results are
    /// appended to `row_values`/`guarded_values`, so the complete final target
    /// tuple still commits atomically with the rest of the pass; an
    /// intermediate-definition refresh writes work state only and is never
    /// committed. Once-per-tick execution is preserved — ordering replaces
    /// snapshotting, not iteration.
    fn execute_clock_partition_steps(
        &self,
        mode: ClockPartitionPassMode<'_, '_>,
        entry: ClockPartitionEntry<'_>,
        t: f64,
        outputs: ClockPartitionOutputs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        let ClockPartitionEntry { eval_y, eval_p } = entry;
        let ClockPartitionOutputs {
            row_values,
            guarded_values,
            evaluated_transactions,
        } = outputs;
        let order = &self.model.problem.discrete.clock_partition_order;
        if order.is_empty() {
            return Ok(());
        }
        if let ClockPartitionPassMode::Filtered { snapshot, .. } = mode {
            // A clock partition is solved once per tick, in the first event
            // iteration of that tick (MLS App B / SPEC_0022 SIM-010).
            if snapshot.event_iteration != 0 {
                return Ok(());
            }
            // Every filtered admission rule below gates its producer on that
            // producer's own periodic clock, so an instant on which no clock of
            // this partition ticks admits no producer at all. Seeding the work
            // buffers and refreshing intermediates for such an instant would
            // compute values nothing reads, at every event of the run — the
            // partition is skipped instead, which is exactly equivalent.
            if !self.clock_partition_ticks_at(t)? {
                return Ok(());
            }
        }
        // The work buffers are pass-private but not pass-owned: reusing the
        // runtime's scratch across passes keeps a tick off the allocator.
        let mut work_y_cell = self.clock_partition_work_y.borrow_mut();
        let mut work_p_cell = self.clock_partition_work_p.borrow_mut();
        let work_y = &mut *work_y_cell;
        let work_p = &mut *work_p_cell;
        copy_runtime_values_into(work_y, eval_y, "clock partition work y")?;
        copy_runtime_values_into(work_p, eval_p, "clock partition work p")?;
        if let ClockPartitionPassMode::Filtered { snapshot, .. } = mode {
            self.seed_root_relation_overrides(snapshot, work_p);
        }
        for step in order {
            match *step {
                solve::ClockPartitionStep::ScalarRows { start_row, count } => {
                    self.execute_clock_partition_scalar_rows(
                        mode, start_row, count, t, work_y, work_p, row_values,
                    )?;
                }
                solve::ClockPartitionStep::GuardedAssignment { program_index } => {
                    self.execute_clock_partition_guarded(
                        mode,
                        program_index,
                        t,
                        work_y,
                        work_p,
                        guarded_values,
                    )?;
                }
                solve::ClockPartitionStep::StructuredUpdate { update_index } => {
                    self.execute_clock_partition_structured(
                        mode,
                        update_index,
                        t,
                        work_y,
                        work_p,
                        row_values,
                    )?;
                }
                solve::ClockPartitionStep::EventTransaction { program_index } => {
                    self.execute_clock_partition_transaction(
                        mode,
                        program_index,
                        t,
                        work_y,
                        work_p,
                        evaluated_transactions,
                    )?;
                }
                solve::ClockPartitionStep::Intermediate { row } => {
                    self.execute_clock_partition_intermediate(mode, row, t, work_y, work_p)?;
                }
            }
        }
        Ok(())
    }

    fn execute_clock_partition_transaction(
        &self,
        mode: ClockPartitionPassMode<'_, '_>,
        program_index: usize,
        t: f64,
        work_y: &mut [f64],
        work_p: &mut [f64],
        evaluated_transactions: &mut Vec<usize>,
    ) -> Result<(), RuntimeSolveError> {
        if let ClockPartitionPassMode::Filtered { snapshot, scope } = mode
            && (scope.observation_only
                || !snapshot
                    .row_filter
                    .accepts(crate::EventPreMode::EventEntry, true)
                || (scope.skip_solver_or_time_rows
                    && self.event_transaction_reads_solver_or_time(program_index)?))
        {
            return Ok(());
        }
        if !self.event_transaction_active_at(program_index, t)? {
            return Ok(());
        }
        self.eval_event_transaction_outputs(program_index, work_y, work_p, t)?;
        if self
            .failed_event_transaction_assertion(program_index)?
            .is_none()
        {
            self.commit_event_transaction_targets(program_index, work_y, work_p)?;
        }
        evaluated_transactions.push(program_index);
        Ok(())
    }

    /// Carry the pass-entry root-relation overrides into the private work
    /// parameters, so an ordered producer sees the same relation memory the
    /// snapshot pass would have shown it.
    fn seed_root_relation_overrides(&self, snapshot: &DiscretePreSnapshot<'_>, work_p: &mut [f64]) {
        for (root_idx, value) in snapshot.root_relation_overrides {
            let Some(Some(solve::ScalarSlot::P { index, .. })) = self
                .model
                .problem
                .events
                .root_relation_memory_targets
                .get(*root_idx)
                .copied()
            else {
                continue;
            };
            if let Some(slot) = work_p.get_mut(index) {
                *slot = *value;
            }
        }
    }

    // SPEC_0021: Exception - validated boundary keeps proof-relevant inputs explicit.
    #[allow(clippy::too_many_arguments)]
    fn execute_clock_partition_scalar_rows(
        &self,
        mode: ClockPartitionPassMode<'_, '_>,
        start_row: usize,
        count: usize,
        t: f64,
        work_y: &mut [f64],
        work_p: &mut [f64],
        row_values: &mut Vec<DiscreteRowValue>,
    ) -> Result<(), RuntimeSolveError> {
        let mut cached_program = None;
        let mut outputs = Vec::new();
        let end = start_row.checked_add(count).ok_or_else(|| {
            RuntimeSolveError::solve_ir("clock partition scalar step row overflow")
        })?;
        for row_idx in start_row..end {
            if self
                .event_transaction_coverage
                .discrete_rows
                .get(row_idx)
                .copied()
                .unwrap_or(true)
            {
                continue;
            }
            if let ClockPartitionPassMode::Filtered { snapshot, scope } = mode
                && !self.scalar_row_admitted(row_idx, snapshot, scope, t)?
            {
                continue;
            }
            let (program, output) =
                self.discrete_rhs
                    .row_output_position(row_idx)
                    .ok_or_else(|| {
                        RuntimeSolveError::solve_ir(format!(
                            "discrete output {row_idx} has no producing program"
                        ))
                    })?;
            if cached_program != Some(program) {
                self.eval_discrete_program_outputs(program, work_y, work_p, t, &mut outputs)?;
                cached_program = Some(program);
            }
            let value = outputs.get(output).copied().ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "discrete program {program} omitted output offset {output}"
                ))
            })?;
            let target = self.model.problem.discrete.update_targets[row_idx];
            row_values.push((target, value));
            solve_eval::apply_scalar_slot_value_exact(target, value, work_y, work_p)?;
        }
        Ok(())
    }

    /// Every existing per-row admission rule for one scalar discrete row,
    /// unchanged by SOLVE-C57: only the execution coordinate moved.
    fn scalar_row_admitted(
        &self,
        row_idx: usize,
        snapshot: &DiscretePreSnapshot<'_>,
        scope: DiscreteRowEvalScope,
        t: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let role = self.model.problem.discrete.row_roles[row_idx];
        if scope.observation_only && !self.observation_refresh_row(row_idx)? {
            return Ok(false);
        }
        if scope.initialization_equations_only && role != solve::DiscreteRowRole::Equation {
            return Ok(false);
        }
        if scope.skip_solver_or_time_rows && self.discrete_row_reads_solver_or_time(row_idx)? {
            return Ok(false);
        }
        if !self.discrete_row_active_at(row_idx, t)? {
            return Ok(false);
        }
        let pre_mode = crate::EventPreMode::from(self.model.problem.discrete.pre_modes[row_idx]);
        Ok(snapshot.row_filter.accepts(pre_mode, true))
    }

    fn execute_clock_partition_guarded(
        &self,
        mode: ClockPartitionPassMode<'_, '_>,
        program_index: usize,
        t: f64,
        work_y: &mut [f64],
        work_p: &mut [f64],
        guarded_values: &mut Vec<GuardedRowValues>,
    ) -> Result<(), RuntimeSolveError> {
        if self
            .event_transaction_coverage
            .guarded_assignments
            .get(program_index)
            .copied()
            .unwrap_or(true)
        {
            return Ok(());
        }
        let owner = &self.model.problem.discrete.guarded_assignments[program_index];
        match mode {
            ClockPartitionPassMode::Filtered { snapshot, scope } => {
                if scope.observation_only && !owner.observation_refresh() {
                    return Ok(());
                }
                if scope.initialization_equations_only
                    && owner.role() != solve::DiscreteRowRole::Equation
                {
                    return Ok(());
                }
                if scope.skip_solver_or_time_rows && row_reads_solver_or_time(owner.program()) {
                    return Ok(());
                }
                if !self.guarded_assignment_accepts_snapshot(program_index, snapshot, t)? {
                    return Ok(());
                }
            }
            ClockPartitionPassMode::Unfiltered => {
                if !self.guarded_assignment_active_at(program_index, t)? {
                    return Ok(());
                }
            }
        }
        let mut values = Vec::new();
        self.eval_guarded_assignment_outputs(program_index, work_y, work_p, t, &mut values)?;
        self.apply_guarded_assignment_outputs(program_index, &values, &[], work_y, work_p)?;
        guarded_values.push((program_index, values));
        Ok(())
    }

    fn execute_clock_partition_structured(
        &self,
        mode: ClockPartitionPassMode<'_, '_>,
        update_index: usize,
        t: f64,
        work_y: &mut [f64],
        work_p: &mut [f64],
        row_values: &mut Vec<DiscreteRowValue>,
    ) -> Result<(), RuntimeSolveError> {
        if self
            .event_transaction_coverage
            .structured_updates
            .get(update_index)
            .copied()
            .unwrap_or(true)
        {
            return Ok(());
        }
        // The rows of one structured update are indexed once at preparation:
        // rescanning every structured row for every structured step made the
        // pass quadratic in the partition size.
        let rows = self
            .clock_partition_structured_rows
            .get(update_index)
            .map_or(&[][..], Vec::as_slice);
        for &row_index in rows {
            let row = self.structured_discrete_rows.rows()[row_index];
            if let ClockPartitionPassMode::Filtered { snapshot, scope } = mode
                && !self.structured_row_admitted(row, snapshot, scope, t)?
            {
                continue;
            }
            let value = self
                .structured_discrete_rows
                .rhs
                .eval_row_unchecked_with_context(
                    row.source_row,
                    work_y,
                    work_p,
                    t,
                    self.row_eval_context(),
                )?;
            row_values.push((row.target, value));
            solve_eval::apply_scalar_slot_value_exact(row.target, value, work_y, work_p)?;
        }
        Ok(())
    }

    /// Every existing per-row admission rule for one structured B.1c row,
    /// unchanged by SOLVE-C57: only the execution coordinate moved.
    fn structured_row_admitted(
        &self,
        row: PreparedStructuredDiscreteRow,
        snapshot: &DiscretePreSnapshot<'_>,
        scope: DiscreteRowEvalScope,
        t: f64,
    ) -> Result<bool, RuntimeSolveError> {
        if scope.observation_only && !row.observation_refresh {
            return Ok(false);
        }
        if scope.initialization_equations_only && row.role != solve::DiscreteRowRole::Equation {
            return Ok(false);
        }
        let source_program = &self.structured_discrete_rows.rhs.block().programs()[row.source_row];
        if scope.skip_solver_or_time_rows && row_reads_solver_or_time(source_program) {
            return Ok(false);
        }
        if !self.structured_discrete_row_active_at(row, t)? {
            return Ok(false);
        }
        let pre_mode = crate::EventPreMode::from(row.pre_mode);
        Ok(snapshot.row_filter.accepts(pre_mode, true))
    }

    fn execute_clock_partition_intermediate(
        &self,
        mode: ClockPartitionPassMode<'_, '_>,
        row: usize,
        t: f64,
        work_y: &mut [f64],
        work_p: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if matches!(mode, ClockPartitionPassMode::Filtered { .. })
            && !self.clock_partition_intermediate_active_at(row, t)?
        {
            return Ok(());
        }
        let target = self
            .model
            .problem
            .discrete
            .clock_partition_intermediate_targets
            .get(row)
            .copied()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir("clock partition intermediate row is out of bounds")
            })?;
        if let ClockPartitionPassMode::Filtered { scope, .. } = mode
            && scope.skip_solver_or_time_rows
        {
            let (program, _) = self
                .clock_partition_intermediates
                .row_output_position(row)
                .ok_or_else(|| {
                    RuntimeSolveError::solve_ir(
                        "clock partition intermediate row has no producing program",
                    )
                })?;
            if row_reads_solver_or_time(
                &self.clock_partition_intermediates.block().programs()[program],
            ) {
                return Ok(());
            }
        }
        // An intermediate refresh runs once per row per tick on the deadline
        // path, so it takes the same native specialization route the discrete
        // rows take; the reference interpreter is the fallback, not the plan.
        let value = {
            let mut scratch = self.clock_partition_intermediate_scratch.borrow_mut();
            if scratch.len() <= row {
                resize_runtime_values(
                    &mut scratch,
                    self.clock_partition_intermediates.len(),
                    0.0,
                    "clock partition intermediate scratch",
                )?;
            }
            self.eval_single_output_rows_with_native(
                &self.clock_partition_intermediates,
                &self.compiled_clock_partition_intermediates,
                &self.failed_clock_partition_intermediates,
                &[row],
                work_y,
                work_p,
                t,
                &mut scratch,
            )?;
            scratch[row]
        };
        // Work state only: intermediates become visible to later producers
        // without committing unrelated targets.
        tracing::trace!(
            target: "rumoca_solver::clock_partition",
            row,
            target = ?target,
            value,
            "refresh clock-partition intermediate"
        );
        solve_eval::apply_scalar_slot_value_exact(target, value, work_y, work_p)?;
        Ok(())
    }

    /// Replay the construction-issued consumer-domain set for one
    /// intermediate. The structural proof, not runtime program inspection,
    /// decided which clocked producers can read this row.
    fn clock_partition_intermediate_active_at(
        &self,
        row: usize,
        t: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let clocks = self
            .model
            .problem
            .discrete
            .clock_partition_intermediate_clocks
            .get(row)
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(
                    "clock partition intermediate consumer row is out of bounds",
                )
            })?;
        for &clock in clocks {
            if self.periodic_clock_active(clock, t, "clock partition intermediate")? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// Whether the issued clock-partition order owns the clock-owned members
    /// of this pass (it does whenever it is non-empty; construction validates
    /// exact coverage).
    fn clock_partition_owns_clocked_rows(&self) -> bool {
        !self.model.problem.discrete.clock_partition_order.is_empty()
    }

    /// Whether any periodic clock the issued order carries ticks at this exact
    /// instant. The clock set is collected once from the issued order; nothing
    /// here rediscovers membership from targets, names, or program shape.
    fn clock_partition_ticks_at(&self, t: f64) -> Result<bool, RuntimeSolveError> {
        for clock in &self.clock_partition_clocks {
            if self.periodic_clock_active(*clock, t, "clock partition")? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}

/// The periodic clocks the issued clock-partition order carries, in issued
/// order and deduplicated.
pub(super) fn clock_partition_clocks(
    discrete: &solve::DiscreteSolveSystem,
) -> Vec<solve::PeriodicClockId> {
    let mut clocks = Vec::new();
    for step in &discrete.clock_partition_order {
        let owner = match *step {
            solve::ClockPartitionStep::ScalarRows { start_row, .. } => {
                discrete.clock_owners.get(start_row).copied().flatten()
            }
            solve::ClockPartitionStep::GuardedAssignment { program_index } => discrete
                .guarded_assignments
                .get(program_index)
                .and_then(solve::GuardedAssignmentProgram::clock_owner),
            solve::ClockPartitionStep::StructuredUpdate { update_index } => discrete
                .structured_updates
                .get(update_index)
                .and_then(|update| update.clock_owner),
            solve::ClockPartitionStep::EventTransaction { .. } => None,
            solve::ClockPartitionStep::Intermediate { .. } => None,
        };
        if let Some(owner) = owner
            && !clocks.contains(&owner)
        {
            clocks.push(owner);
        }
        if let solve::ClockPartitionStep::EventTransaction { program_index } = *step
            && let Some(transaction) = discrete.event_transactions.get(program_index)
        {
            for &clock in transaction.clock_owners() {
                if !clocks.contains(&clock) {
                    clocks.push(clock);
                }
            }
        }
    }
    clocks
}

/// One structured-update index -> its prepared structured row indices.
pub(super) fn clock_partition_structured_rows(
    updates: usize,
    rows: &[PreparedStructuredDiscreteRow],
) -> Vec<Vec<usize>> {
    let mut by_update = vec![Vec::new(); updates];
    for (index, row) in rows.iter().enumerate() {
        if let Some(slot) = by_update.get_mut(row.update_index) {
            slot.push(index);
        }
    }
    by_update
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
        let (program, output) = discrete_rhs.row_output_position(row_idx).ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "condition-memory output {row_idx} has no producing program"
            ))
        })?;
        let value = discrete_rhs.eval_row_output_unchecked_with_context(
            program,
            output,
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
        for settle_iteration in 0..input.max_iters {
            // A clocked equation executes once at its superdense tick. The
            // remaining passes settle unclocked equations and algebraic
            // projection around that held result; re-running the clock owner
            // here both violates MLS clock semantics and repeats potentially
            // large controller/estimator programs.
            let settle_snapshot = DiscretePreSnapshot {
                row_filter: snapshot.row_filter,
                root_relation_overrides: snapshot.root_relation_overrides,
                event_iteration: snapshot.event_iteration.max(settle_iteration),
            };
            let mut pass_changed = self.apply_discrete_rows_for_pre_snapshot(
                &settle_snapshot,
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
            if !pass_changed {
                // The caller supplies a projected coordinate with runtime
                // assignments stable. If neither the active discrete owners
                // nor their runtime assignments changed it, that certificate
                // still holds and a full projection cannot add information.
                return Ok(changed_any);
            }
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

    #[cfg(test)]
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
        let mut evaluated_transactions = self.evaluate_event_transactions_for_snapshot(
            snapshot,
            scope.observation_only,
            scope.skip_solver_or_time_rows,
            &eval_y,
            &eval_p,
            t,
        )?;
        let input = DiscreteSnapshotEvalInput {
            snapshot,
            eval_y: &eval_y,
            eval_p: &eval_p,
            t,
            scope,
        };
        let mut row_values = Vec::new();
        reserve_runtime_vec_capacity(
            &mut row_values,
            self.model.problem.discrete.rhs.len(),
            "discrete row values",
        )?;
        // SOLVE-C57: clock-owned producers execute once, in issued causal
        // order, over private work state; unclocked rows keep the frozen
        // pass-entry snapshot and their ordinary Appendix-B iteration.
        let mut guarded_values = Vec::new();
        self.execute_clock_partition_steps(
            ClockPartitionPassMode::Filtered { snapshot, scope },
            ClockPartitionEntry {
                eval_y: &eval_y,
                eval_p: &eval_p,
            },
            t,
            ClockPartitionOutputs {
                row_values: &mut row_values,
                guarded_values: &mut guarded_values,
                evaluated_transactions: &mut evaluated_transactions,
            },
        )?;
        self.collect_scalar_discrete_row_values(input, &mut eval_p_cache, &mut row_values)?;
        guarded_values.extend(self.collect_guarded_discrete_row_values(input, &mut eval_p_cache)?);
        self.collect_structured_discrete_row_values(input, &mut eval_p_cache, &mut row_values)?;
        self.override_relation_memory_row_values(snapshot.root_relation_overrides, &mut row_values);
        let mut changed = false;
        for (target, value) in row_values {
            changed |= solve_eval::apply_scalar_slot_value_exact(target, value, y, p)?;
        }
        for (program_index, values) in guarded_values {
            changed |= self.apply_guarded_assignment_outputs(
                program_index,
                &values,
                snapshot.root_relation_overrides,
                y,
                p,
            )?;
        }
        changed |= self.commit_successful_event_transactions(evaluated_transactions, y, p)?;
        Ok(changed)
    }

    fn collect_scalar_discrete_row_values(
        &self,
        input: DiscreteSnapshotEvalInput<'_, '_, '_>,
        eval_p_cache: &mut EventEvalParamCache,
        row_values: &mut Vec<DiscreteRowValue>,
    ) -> Result<(), RuntimeSolveError> {
        let ordered_clocked = self.clock_partition_owns_clocked_rows();
        for row_idx in 0..self.model.problem.discrete.rhs.len() {
            if self.event_transaction_coverage.discrete_rows[row_idx] {
                continue;
            }
            if ordered_clocked && self.model.problem.discrete.clock_owners[row_idx].is_some() {
                continue;
            }
            let role = self.model.problem.discrete.row_roles[row_idx];
            if input.scope.observation_only && !self.observation_refresh_row(row_idx)? {
                continue;
            }
            if input.scope.initialization_equations_only && role != solve::DiscreteRowRole::Equation
            {
                continue;
            }
            if input.scope.skip_solver_or_time_rows
                && self.discrete_row_reads_solver_or_time(row_idx)?
            {
                continue;
            }
            let Some(value) = self.eval_discrete_row_for_pre_snapshot(
                DiscreteRowEvalInput {
                    snapshot: input.snapshot,
                    row_idx,
                    eval_y: input.eval_y,
                    eval_p: input.eval_p,
                    t: input.t,
                },
                eval_p_cache,
            )?
            else {
                continue;
            };
            row_values.push((self.model.problem.discrete.update_targets[row_idx], value));
        }
        Ok(())
    }

    fn discrete_row_reads_solver_or_time(&self, row_idx: usize) -> Result<bool, RuntimeSolveError> {
        let (program, _) = self
            .discrete_rhs
            .row_output_position(row_idx)
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "discrete output {row_idx} has no producing program"
                ))
            })?;
        Ok(row_reads_solver_or_time(
            &self.discrete_rhs.block().programs()[program],
        ))
    }

    fn collect_guarded_discrete_row_values(
        &self,
        input: DiscreteSnapshotEvalInput<'_, '_, '_>,
        eval_p_cache: &mut EventEvalParamCache,
    ) -> Result<Vec<GuardedRowValues>, RuntimeSolveError> {
        let mut guarded_values = Vec::new();
        let ordered_clocked = self.clock_partition_owns_clocked_rows();
        for program_index in 0..self.guarded_assignment_programs.len() {
            if self.event_transaction_coverage.guarded_assignments[program_index] {
                continue;
            }
            let owner = &self.model.problem.discrete.guarded_assignments[program_index];
            if ordered_clocked && owner.clock_owner().is_some() {
                continue;
            }
            if input.scope.observation_only && !owner.observation_refresh() {
                continue;
            }
            if input.scope.initialization_equations_only
                && owner.role() != solve::DiscreteRowRole::Equation
            {
                continue;
            }
            if input.scope.skip_solver_or_time_rows && row_reads_solver_or_time(owner.program()) {
                continue;
            }
            if !self.guarded_assignment_accepts_snapshot(program_index, input.snapshot, input.t)? {
                continue;
            }
            let row_p = eval_p_cache.params(input.eval_p);
            let row_p_with_root_overrides;
            let row_p = if input.snapshot.root_relation_overrides.is_empty() {
                row_p
            } else {
                row_p_with_root_overrides = event_eval_params_with_relation_overrides(
                    &self.model.problem.events.root_relation_memory_targets,
                    input.snapshot.root_relation_overrides,
                    row_p,
                )?;
                &row_p_with_root_overrides
            };
            let mut values = Vec::new();
            self.eval_guarded_assignment_outputs(
                program_index,
                input.eval_y,
                row_p,
                input.t,
                &mut values,
            )?;
            guarded_values.push((program_index, values));
        }
        Ok(guarded_values)
    }

    fn collect_structured_discrete_row_values(
        &self,
        input: DiscreteSnapshotEvalInput<'_, '_, '_>,
        eval_p_cache: &mut EventEvalParamCache,
        row_values: &mut Vec<DiscreteRowValue>,
    ) -> Result<(), RuntimeSolveError> {
        let ordered_clocked = self.clock_partition_owns_clocked_rows();
        for row in self.structured_discrete_rows.rows().iter().copied() {
            if self.event_transaction_coverage.structured_updates[row.update_index] {
                continue;
            }
            if ordered_clocked && row.clock_owner.is_some() {
                continue;
            }
            if input.scope.observation_only && !row.observation_refresh {
                continue;
            }
            if input.scope.initialization_equations_only
                && row.role != solve::DiscreteRowRole::Equation
            {
                continue;
            }
            let source_program =
                &self.structured_discrete_rows.rhs.block().programs()[row.source_row];
            if input.scope.skip_solver_or_time_rows && row_reads_solver_or_time(source_program) {
                continue;
            }
            let Some(value) = self.eval_structured_discrete_row_for_pre_snapshot(
                StructuredDiscreteRowEvalInput {
                    snapshot: input.snapshot,
                    row,
                    eval_y: input.eval_y,
                    eval_p: input.eval_p,
                    t: input.t,
                },
                eval_p_cache,
            )?
            else {
                continue;
            };
            row_values.push((row.target, value));
        }
        Ok(())
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
        if !self.discrete_row_active_at(row_idx, t)? {
            return Ok(None);
        }
        let pre_mode = self
            .model
            .problem
            .discrete
            .pre_modes
            .get(row_idx)
            .copied()
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "discrete scalar-view row index {row_idx} is out of bounds"
                ))
            })?;
        let row_pre_mode = crate::EventPreMode::from(pre_mode);
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
        let (program, output) =
            self.discrete_rhs
                .row_output_position(row_idx)
                .ok_or_else(|| {
                    RuntimeSolveError::solve_ir(format!(
                        "discrete output {row_idx} has no producing program"
                    ))
                })?;
        if eval_p_cache.program != Some(program) {
            self.eval_discrete_program_outputs(
                program,
                eval_y,
                row_p,
                t,
                &mut eval_p_cache.outputs,
            )?;
            eval_p_cache.program = Some(program);
        }
        let value = eval_p_cache.outputs.get(output).copied().ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "discrete program {program} omitted output offset {output}"
            ))
        })?;
        tracing::trace!(
            target: "rumoca_solver::discrete_rows",
            row_idx,
            target = ?self.model.problem.discrete.update_targets[row_idx],
            value,
            event_iteration = snapshot.event_iteration,
            ?row_pre_mode,
            clock_owned,
            "evaluate discrete row"
        );
        Ok(Some(value))
    }

    pub fn refresh_observation_discrete_rows(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        if !self
            .model
            .problem
            .discrete
            .observation_refresh
            .iter()
            .copied()
            .any(std::convert::identity)
            && !self
                .model
                .problem
                .discrete
                .guarded_assignments
                .iter()
                .any(|owner| owner.observation_refresh())
            && !self
                .structured_discrete_rows
                .rows()
                .iter()
                .any(|row| row.observation_refresh)
        {
            return Ok(false);
        }
        self.validate_observation_refresh_rows()?;
        if self.uncoupled_scalar_observation_refresh_is_complete() {
            return self.refresh_uncoupled_scalar_observation_rows(y, p, t, max_iters);
        }
        let mut changed_any = false;
        for refresh_iteration in 0..max_iters {
            let snapshot = DiscretePreSnapshot {
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &[],
                // Preserve the existing observation-refresh policy: fixed
                // rows keep the observation-entry pre snapshot for the whole
                // refresh loop.
                event_iteration: refresh_iteration,
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

    fn uncoupled_scalar_observation_refresh_is_complete(&self) -> bool {
        !self.model.problem.discrete.observation_refresh_reads_y
            && !self
                .model
                .problem
                .discrete
                .guarded_assignments
                .iter()
                .any(|owner| owner.observation_refresh())
            && !self
                .structured_discrete_rows
                .rows()
                .iter()
                .any(|row| row.observation_refresh)
            && self
                .observation_refresh_scalar_rows
                .iter()
                .all(|&row| !self.event_transaction_coverage.discrete_rows[row])
    }

    fn refresh_uncoupled_scalar_observation_rows(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        max_iters: usize,
    ) -> Result<bool, RuntimeSolveError> {
        let mut changed_any = false;
        for event_iteration in 0..max_iters {
            let mut eval_p = self.observation_refresh_p_scratch.borrow_mut();
            copy_runtime_values_into(&mut eval_p, p, "observation refresh P snapshot")?;
            let snapshot = DiscretePreSnapshot {
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &[],
                event_iteration,
            };
            let mut eval_p_cache = EventEvalParamCache::default();
            let mut values = self.observation_refresh_values_scratch.borrow_mut();
            values.clear();
            if values.capacity() < self.observation_refresh_scalar_rows.len() {
                let missing = self
                    .observation_refresh_scalar_rows
                    .len()
                    .saturating_sub(values.capacity());
                reserve_runtime_vec_capacity(
                    &mut values,
                    missing,
                    "observation refresh row values",
                )?;
            }
            self.collect_uncoupled_scalar_observation_values(
                y,
                &eval_p,
                t,
                &snapshot,
                &mut eval_p_cache,
                &mut values,
            )?;
            drop(eval_p);
            let mut changed = false;
            for (target, value) in values.iter().copied() {
                changed |= solve_eval::apply_scalar_slot_value_exact(target, value, y, p)?;
            }
            if !changed {
                return Ok(changed_any);
            }
            changed_any = true;
        }
        Err(RuntimeSolveError::solve_ir(
            "observation-time scalar refresh did not converge",
        ))
    }

    fn collect_uncoupled_scalar_observation_values(
        &self,
        y: &[f64],
        eval_p: &[f64],
        t: f64,
        snapshot: &DiscretePreSnapshot<'_>,
        eval_p_cache: &mut EventEvalParamCache,
        values: &mut Vec<(solve::ScalarSlot, f64)>,
    ) -> Result<(), RuntimeSolveError> {
        for &row in self.observation_refresh_scalar_rows.iter() {
            let value = self.eval_discrete_row_for_pre_snapshot(
                DiscreteRowEvalInput {
                    snapshot,
                    row_idx: row,
                    eval_y: y,
                    eval_p,
                    t,
                },
                eval_p_cache,
            )?;
            if let Some(value) = value {
                values.push((self.model.problem.discrete.update_targets[row], value));
            }
        }
        Ok(())
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
        let rhs_rows = self.discrete_rhs.block().len();
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
