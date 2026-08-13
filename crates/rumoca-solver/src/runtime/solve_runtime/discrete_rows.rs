use std::collections::BTreeSet;

use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;

use crate::runtime::solve_events::event_eval_params_with_relation_overrides;
use crate::{RuntimeSolveError, discrete_row_active_at, row_reads_solver_or_time};

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

/// The semantic child the runtime invokes.
///
/// An owner is one executable program — a controller, an estimator, a guarded
/// group, a transaction. It is deliberately *not* a scalar row: one invocation
/// projects onto many rows, and counting rows would report a call count that
/// scales with representation width rather than with work actually done.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum DiscreteOwnerId {
    /// One program of the discrete scalar block, named by the program position
    /// its outputs resolve to rather than by any single row.
    ScalarProgram(usize),
    GuardedAssignment(usize),
    StructuredUpdate(usize),
    EventTransaction(usize),
}

/// What one discrete-row pass admitted and what it actually ran.
///
/// Two quantities, never conflated. *Activations* are compact semantic owners,
/// deduplicated inside the pass. *Executions* are the prepared-row, native, or
/// interpreter invocations those owners cost — a structured owner scalarized to
/// one prepared row per coordinate is one activation and `n` executions, so a
/// million-row scalar adapter cannot report itself as a single call and earn
/// tensor-native credit it has not done the work for.
#[derive(Debug, Default)]
pub(super) struct DiscreteRowPassTally {
    owners: BTreeSet<DiscreteOwnerId>,
    scheduled_owners: BTreeSet<DiscreteOwnerId>,
    executions: usize,
    scheduled_executions: usize,
    projected_outputs: usize,
}

impl DiscreteRowPassTally {
    /// `scheduled` covers both once-only families the runtime currently keeps
    /// in one `clock_owner` field: MLS §16 synchronous clock partitions and
    /// Boolean `sample(start, interval)` scheduled events. Collapsing the two
    /// is migration debt — nothing here decides ordering for either, it only
    /// counts.
    pub(super) fn admit(&mut self, owner: DiscreteOwnerId, scheduled: bool, outputs: usize) {
        self.owners.insert(owner);
        self.executions += 1;
        if scheduled {
            self.scheduled_owners.insert(owner);
            self.scheduled_executions += 1;
        }
        self.projected_outputs += outputs;
    }
}

/// What one discrete settle instant actually did.
///
/// This is evidence, not enforcement. Every field is counted at the site where
/// the decision is made, so a test can show that the restored settle loop
/// invokes what it should instead of inferring that from the values left in
/// storage — values cannot distinguish "ran once" from "ran twice and agreed".
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct DiscreteSettleObservation {
    /// Passes the instant took to reach its fixpoint.
    pub passes: usize,
    /// `project_algebraics` invocations across those passes.
    pub projections: usize,
    /// Compact semantic owners the activity gates and the row filter admitted
    /// on the first pass, deduplicated.
    pub eligible_owners: usize,
    /// Scalar outputs those first-pass owners project onto, so representation
    /// width stays separate from call cardinality.
    pub eligible_projected_outputs: usize,
    /// Compact owner activations summed over every pass, deduplicated within
    /// each pass.
    pub owner_activations: usize,
    /// Prepared-row, native, or interpreter invocations those activations cost.
    /// Exceeds `owner_activations` exactly by scalarization width.
    pub owner_executions: usize,
    /// Once-only scheduled owners admitted on the first pass, deduplicated. The
    /// admitted set is fixed by `t`, the row filter, and the event iteration,
    /// so this is how many the instant activates.
    pub scheduled_owners_activated: usize,
    /// Scheduled-owner activations across every pass, deduplicated per pass.
    ///
    /// A settle instant that needs `n` passes activates each admitted owner `n`
    /// times, so this exceeds `scheduled_owners_activated` whenever `passes >
    /// 1`. Every owner production lowering emits reads a held `pre`/`previous`
    /// lane, so the repeat reproduces the same value; what it cannot make safe
    /// is an owner with an effect — a checked assertion or an external call.
    /// Collapsing the repeat needs a construction-issued causal plan for the
    /// event instant, able to order each owner after the producers it reads. A
    /// runtime result cache cannot stand in: replaying a first-pass result
    /// keeps a stale value when a producer only becomes fresh later, and
    /// recomputing repeats the effect. The counter keeps the residue measured.
    pub scheduled_owner_activations: usize,
    /// Prepared invocations those scheduled activations cost.
    pub scheduled_owner_executions: usize,
}

/// Cumulative invocation ledger for the checked event owners.
///
/// Every entry is incremented at an execution boundary. Nothing here is
/// recovered from stored values, so a test can pin exact cardinalities across a
/// whole run — including the ones a value comparison cannot see, such as a
/// transaction's checked assertions running twice.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct EventOwnerExecutionLedger {
    /// Transaction payload evaluations, indexed by transaction.
    pub transaction_evaluations: Vec<usize>,
    /// Transaction target-tuple commits, indexed by transaction.
    pub transaction_commits: Vec<usize>,
    /// Transaction admissions, indexed by transaction and then by
    /// [`EventUpdateRowFilter::ledger_index`].
    pub transaction_admissions: Vec<[usize; EventUpdateRowFilter::LEDGER_SLOTS]>,
    /// Checked assertion-predicate evaluations, indexed by transaction. A
    /// transaction's assertions are evaluated with its payload, so a repeated
    /// payload evaluation repeats every one of them — an effect no value
    /// comparison can see.
    pub transaction_assertion_evaluations: Vec<usize>,
    /// Event-action condition evaluations, indexed by action.
    pub event_action_evaluations: Vec<usize>,
    /// Invocations of owners a transaction supersedes. A superseded producer is
    /// never the runtime's executable child, so this must stay zero.
    pub superseded_producer_invocations: usize,
}

impl EventOwnerExecutionLedger {
    pub(super) fn sized_for(transactions: usize, actions: usize) -> Self {
        Self {
            transaction_evaluations: vec![0; transactions],
            transaction_commits: vec![0; transactions],
            transaction_admissions: vec![[0; EventUpdateRowFilter::LEDGER_SLOTS]; transactions],
            transaction_assertion_evaluations: vec![0; transactions],
            event_action_evaluations: vec![0; actions],
            superseded_producer_invocations: 0,
        }
    }

    /// Admissions of `transaction` under `filter`.
    #[must_use]
    pub fn admissions(&self, transaction: usize, filter: EventUpdateRowFilter) -> usize {
        self.transaction_admissions
            .get(transaction)
            .map_or(0, |slots| slots[filter.ledger_index()])
    }
}

impl SolveRuntime {
    /// Invocation counts accumulated since the last reset.
    #[must_use]
    pub fn owner_execution_ledger(&self) -> EventOwnerExecutionLedger {
        self.owner_execution_ledger.borrow().clone()
    }

    /// Restart the invocation ledger, so a test can scope it to one interval.
    pub fn reset_owner_execution_ledger(&self) {
        *self.owner_execution_ledger.borrow_mut() = EventOwnerExecutionLedger::sized_for(
            self.event_transaction_programs.len(),
            self.model.problem.events.actions.len(),
        );
    }

    pub(super) fn ledger_record_transaction_assertions(&self, index: usize, assertions: usize) {
        if let Some(count) = self
            .owner_execution_ledger
            .borrow_mut()
            .transaction_assertion_evaluations
            .get_mut(index)
        {
            *count += assertions;
        }
    }

    pub(super) fn ledger_record_event_action_evaluation(&self, action: usize) {
        if let Some(count) = self
            .owner_execution_ledger
            .borrow_mut()
            .event_action_evaluations
            .get_mut(action)
        {
            *count += 1;
        }
    }

    pub(super) fn ledger_admit_transaction(&self, index: usize, filter: EventUpdateRowFilter) {
        if let Some(slots) = self
            .owner_execution_ledger
            .borrow_mut()
            .transaction_admissions
            .get_mut(index)
        {
            slots[filter.ledger_index()] += 1;
        }
    }

    pub(super) fn ledger_record_transaction_evaluation(&self, index: usize) {
        if let Some(count) = self
            .owner_execution_ledger
            .borrow_mut()
            .transaction_evaluations
            .get_mut(index)
        {
            *count += 1;
        }
    }

    pub(super) fn ledger_record_transaction_commit(&self, index: usize) {
        if let Some(count) = self
            .owner_execution_ledger
            .borrow_mut()
            .transaction_commits
            .get_mut(index)
        {
            *count += 1;
        }
    }

    /// Note one invocation of `owner`, counting it if a transaction already
    /// supersedes it. A superseded producer is never the runtime's executable
    /// child, so reaching this boundary is a coverage escape and is recorded
    /// rather than passing unseen.
    pub(super) fn ledger_note_owner_invocation(&self, owner: DiscreteOwnerId) {
        let superseded = match owner {
            DiscreteOwnerId::ScalarProgram(_) | DiscreteOwnerId::EventTransaction(_) => false,
            DiscreteOwnerId::GuardedAssignment(index) => self
                .event_transaction_coverage
                .guarded_assignments
                .get(index)
                .copied()
                .unwrap_or(false),
            DiscreteOwnerId::StructuredUpdate(index) => self
                .event_transaction_coverage
                .structured_updates
                .get(index)
                .copied()
                .unwrap_or(false),
        };
        if superseded {
            self.owner_execution_ledger
                .borrow_mut()
                .superseded_producer_invocations += 1;
        }
    }
}

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
        let mut assignments = Vec::new();
        for row_idx in 0..self.model.problem.discrete.rhs.len() {
            if self.event_transaction_coverage.discrete_rows[row_idx] {
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
        let mut evaluated_transactions = Vec::new();
        for transaction_index in 0..self.event_transaction_programs.len() {
            self.eval_event_transaction_outputs(transaction_index, &eval_y, &eval_p, t)?;
            evaluated_transactions.push(transaction_index);
        }
        let mut guarded_values = Vec::with_capacity(self.guarded_assignment_programs.len());
        for program_index in 0..self.guarded_assignment_programs.len() {
            if self.event_transaction_coverage.guarded_assignments[program_index] {
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
        if !self.structured_discrete_row_active_at(row, t)? {
            return Ok(None);
        }
        if !snapshot.admits(
            crate::EventPreMode::from(row.pre_mode),
            row.clock_owner.is_some(),
        ) {
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

    /// What the most recent discrete settle instant did.
    ///
    /// Every field is counted where the decision is made, so a test can hold
    /// the settle loop to its once-per-tick and projection obligations instead
    /// of inferring them from the values left in storage.
    pub fn last_discrete_settle(&self) -> DiscreteSettleObservation {
        *self.discrete_settle_observation.borrow()
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
        let mut observation = DiscreteSettleObservation::default();
        let settled = self.settle_discrete_rows_counted(
            snapshot,
            input,
            project_algebraics,
            &mut observation,
        );
        *self.discrete_settle_observation.borrow_mut() = observation;
        settled
    }

    /// The settle instant recorded in [`DiscreteSettleObservation`].
    ///
    /// Kept separate from its caller only so every exit stores the record.
    fn settle_discrete_rows_counted<P>(
        &self,
        snapshot: &DiscretePreSnapshot<'_>,
        input: &mut DiscreteRowsSettleInput<'_>,
        project_algebraics: &mut P,
        observation: &mut DiscreteSettleObservation,
    ) -> Result<bool, RuntimeSolveError>
    where
        P: FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError>,
    {
        let mut changed_any = false;
        for _ in 0..input.max_iters {
            // Every settle pass sees the caller's snapshot unchanged. A pass
            // index bumped into `event_iteration` made a scheduled owner
            // first-pass-only *inside one instant*, which froze it on inputs
            // that instant had not produced yet — the owner's producer is an
            // unclocked algebraic value that only the projection below makes
            // fresh. `event_iteration` names the event iteration, and only the
            // caller advances it; it is not a settle-pass counter.
            //
            // Invoking an admitted owner on each pass is what this loop did
            // before the regression, and the counters below measure it. Doing
            // it exactly once needs a construction-issued causal order for the
            // instant, not a runtime cache of the first pass.
            let mut tally = DiscreteRowPassTally::default();
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
                &mut tally,
            )?;
            if observation.passes == 0 {
                observation.eligible_owners = tally.owners.len();
                observation.eligible_projected_outputs = tally.projected_outputs;
                observation.scheduled_owners_activated = tally.scheduled_owners.len();
            }
            observation.passes += 1;
            observation.owner_activations += tally.owners.len();
            observation.owner_executions += tally.executions;
            observation.scheduled_owner_activations += tally.scheduled_owners.len();
            observation.scheduled_owner_executions += tally.scheduled_executions;
            pass_changed |= self.apply_runtime_assignments_until_stable(
                input.y,
                input.p,
                input.t,
                input.tol,
                input.max_iters,
            )?;
            // No projection is skipped here. Skipping one would need a
            // construction-issued certificate that the incoming coordinate is
            // already projected with a zero remainder; this callee is handed no
            // such capability, and a runtime "nothing changed" test is not one.
            pass_changed |= project_algebraics(input.y, input.p)?;
            observation.projections += 1;
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
            &mut DiscreteRowPassTally::default(),
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
        tally: &mut DiscreteRowPassTally,
    ) -> Result<bool, RuntimeSolveError> {
        self.validate_discrete_row_eval_scope(scope)?;
        let eval_y = copy_runtime_values(y, "discrete row eval y snapshot")?;
        let eval_p = copy_runtime_values(p, "discrete row eval p snapshot")?;
        let mut eval_p_cache = EventEvalParamCache::default();
        let evaluated_transactions = self.evaluate_event_transactions_for_snapshot(
            snapshot,
            scope.observation_only,
            scope.skip_solver_or_time_rows,
            &eval_y,
            &eval_p,
            t,
            tally,
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
        self.collect_scalar_discrete_row_values(input, &mut eval_p_cache, &mut row_values, tally)?;
        let guarded_values =
            self.collect_guarded_discrete_row_values(input, &mut eval_p_cache, tally)?;
        self.collect_structured_discrete_row_values(
            input,
            &mut eval_p_cache,
            &mut row_values,
            tally,
        )?;
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
        tally: &mut DiscreteRowPassTally,
    ) -> Result<(), RuntimeSolveError> {
        for row_idx in 0..self.model.problem.discrete.rhs.len() {
            if self.event_transaction_coverage.discrete_rows[row_idx] {
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
            let (program, _) = self
                .discrete_rhs
                .row_output_position(row_idx)
                .ok_or_else(|| {
                    RuntimeSolveError::solve_ir(format!(
                        "discrete output {row_idx} has no producing program"
                    ))
                })?;
            tally.admit(
                DiscreteOwnerId::ScalarProgram(program),
                self.model.problem.discrete.clock_owners[row_idx].is_some(),
                1,
            );
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
        tally: &mut DiscreteRowPassTally,
    ) -> Result<Vec<GuardedRowValues>, RuntimeSolveError> {
        let mut guarded_values = Vec::new();
        for program_index in 0..self.guarded_assignment_programs.len() {
            if self.event_transaction_coverage.guarded_assignments[program_index] {
                continue;
            }
            let owner = &self.model.problem.discrete.guarded_assignments[program_index];
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
            self.ledger_note_owner_invocation(DiscreteOwnerId::GuardedAssignment(program_index));
            tally.admit(
                DiscreteOwnerId::GuardedAssignment(program_index),
                owner.clock_owner().is_some(),
                values.len(),
            );
            guarded_values.push((program_index, values));
        }
        Ok(guarded_values)
    }

    fn collect_structured_discrete_row_values(
        &self,
        input: DiscreteSnapshotEvalInput<'_, '_, '_>,
        eval_p_cache: &mut EventEvalParamCache,
        row_values: &mut Vec<DiscreteRowValue>,
        tally: &mut DiscreteRowPassTally,
    ) -> Result<(), RuntimeSolveError> {
        for row in self.structured_discrete_rows.rows().iter().copied() {
            if self.event_transaction_coverage.structured_updates[row.update_index] {
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
            self.ledger_note_owner_invocation(DiscreteOwnerId::StructuredUpdate(row.update_index));
            tally.admit(
                DiscreteOwnerId::StructuredUpdate(row.update_index),
                row.clock_owner.is_some(),
                1,
            );
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
        if !snapshot.admits(row_pre_mode, clock_owned) {
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
            self.ledger_note_owner_invocation(DiscreteOwnerId::ScalarProgram(program));
        }
        eval_p_cache
            .outputs
            .get(output)
            .copied()
            .map(Some)
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "discrete program {program} omitted output offset {output}"
                ))
            })
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
                &mut DiscreteRowPassTally::default(),
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
