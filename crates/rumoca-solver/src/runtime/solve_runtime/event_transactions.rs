//! Atomic runtime boundary for checked tensor-native event transactions.

use rumoca_eval_solve as solve_eval;
use rumoca_ir_solve as solve;

use crate::RuntimeSolveError;

use super::SolveRuntime;
use super::discrete_rows::{DiscreteOwnerId, DiscreteRowPassTally};
use super::event_update::DiscretePreSnapshot;

/// Result of one atomic transaction invocation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct EventTransactionExecution {
    pub changed: bool,
    pub failed_assertion: Option<usize>,
}

#[derive(Clone)]
pub(super) struct PreparedEventTransactionCoverage {
    pub(super) discrete_rows: Box<[bool]>,
    pub(super) guarded_assignments: Box<[bool]>,
    pub(super) structured_updates: Box<[bool]>,
    pub(super) event_actions: Box<[bool]>,
    /// The one activation identity each transaction inherits.
    ///
    /// A transaction replaces its superseded owners' *execution*, not the rules
    /// that decide when they execute, so the event-update row filter has to
    /// reach it through the same identity. Without this a scheduled transaction
    /// ran in the `UnownedOnly` initialization pass that exists precisely to
    /// hold it back, and again at the `PostInitialClockTick` pass it belongs to
    /// — one `x := pre(x) + 1` applied twice with a `pre` commit between them.
    transaction_activation: Box<[TransactionActivation]>,
}

/// The single proven activation identity of one transaction.
///
/// Recovered from the owners the transaction supersedes and proven unanimous
/// across every scalar of every one of them. This earns regression credit only:
/// it is not C55/C57/VBC closure, and it is explicitly a migration-period
/// recovery. Deletion edge: when the transaction carries a construction-issued
/// activation capability of its own, this recovery, its prepared fact, and
/// [`proven_transaction_activation`] are removed in the same cutover.
///
/// A mixture is *unpreparable* rather than resolved here, because choosing
/// among disagreeing owners would decide execution.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct TransactionActivation {
    pre_mode: crate::EventPreMode,
    /// The exact schedule identity, not merely whether one exists: two
    /// different schedules are two different activations.
    clock_owner: Option<solve::PeriodicClockId>,
}

impl PreparedEventTransactionCoverage {
    pub(super) fn new(model: &solve::SolveModel) -> Result<Self, solve_eval::EvalSolveError> {
        let mut discrete_rows = vec![false; model.problem.discrete.rhs.len()];
        let mut guarded_assignments = vec![false; model.problem.discrete.guarded_assignments.len()];
        let mut structured_updates = vec![false; model.problem.discrete.structured_updates.len()];
        let mut event_actions = vec![false; model.problem.events.actions.len()];
        let mut transaction_activation = Vec::new();
        // Every transaction is proven before anything is marked, and each mark
        // is bounds-checked into the typed error. A malformed coverage link
        // therefore reports the contract violation instead of panicking, and no
        // partial coverage survives a rejected model.
        for (transaction_index, transaction) in
            model.problem.discrete.event_transactions.iter().enumerate()
        {
            transaction_activation.push(proven_transaction_activation(
                model,
                transaction_index,
                transaction,
            )?);
        }
        for (transaction_index, transaction) in
            model.problem.discrete.event_transactions.iter().enumerate()
        {
            mark_superseded_coverage(
                transaction,
                transaction_index,
                &mut discrete_rows,
                &mut guarded_assignments,
                &mut structured_updates,
            )?;
            mark_action_coverage(model, transaction, transaction_index, &mut event_actions)?;
        }
        Ok(Self {
            discrete_rows: discrete_rows.into_boxed_slice(),
            guarded_assignments: guarded_assignments.into_boxed_slice(),
            structured_updates: structured_updates.into_boxed_slice(),
            event_actions: event_actions.into_boxed_slice(),
            transaction_activation: transaction_activation.into_boxed_slice(),
        })
    }
}

/// The scalar rows one `ScalarRows` coverage link covers, or the typed error if
/// the run leaves the block.
fn superseded_scalar_rows(
    row_count: usize,
    start_row: usize,
    width: usize,
    transaction: &solve::EventTransactionProgram,
    transaction_index: usize,
) -> Result<std::ops::Range<usize>, solve_eval::EvalSolveError> {
    let end = start_row.checked_add(width).ok_or_else(|| {
        activation_contract_error(
            transaction,
            format!("event transaction {transaction_index} has a scalar row run that overflows"),
        )
    })?;
    if end > row_count {
        return Err(activation_contract_error(
            transaction,
            format!(
                "event transaction {transaction_index} supersedes scalar rows \
                 {start_row}..{end} but the discrete block has {row_count}"
            ),
        ));
    }
    Ok(start_row..end)
}

/// Prove that every owner `transaction` supersedes — across the full scalar
/// width of every one of them — shares one activation identity, and return it.
fn proven_transaction_activation(
    model: &solve::SolveModel,
    transaction_index: usize,
    transaction: &solve::EventTransactionProgram,
) -> Result<TransactionActivation, solve_eval::EvalSolveError> {
    let discrete = &model.problem.discrete;
    let targets = transaction.targets();
    let owners = transaction.legacy_owners();
    if targets.len() != owners.len() {
        return Err(activation_contract_error(
            transaction,
            format!(
                "event transaction {transaction_index} has {} targets for {} coverage links",
                targets.len(),
                owners.len()
            ),
        ));
    }
    let mut proven: Option<TransactionActivation> = None;
    for (target, owner) in targets.iter().zip(owners) {
        let width = target.value_type().scalar_count() as usize;
        match *owner {
            solve::EventTransactionLegacyOwner::ScalarRows { start_row } => {
                // Every scalar of the run, not only its first row: a width-two
                // target whose second row carries another mode is a mixture.
                for (offset, row) in superseded_scalar_rows(
                    discrete.rhs.len(),
                    start_row,
                    width,
                    transaction,
                    transaction_index,
                )?
                .enumerate()
                {
                    let produced = *discrete
                        .update_targets
                        .get(row)
                        .ok_or_else(|| unresolved_owner_error(transaction, transaction_index))?;
                    prove_same_target(
                        produced,
                        target.base(),
                        offset,
                        transaction,
                        transaction_index,
                    )?;
                    let pre_mode = *discrete
                        .pre_modes
                        .get(row)
                        .ok_or_else(|| unresolved_owner_error(transaction, transaction_index))?;
                    let clock_owner = *discrete
                        .clock_owners
                        .get(row)
                        .ok_or_else(|| unresolved_owner_error(transaction, transaction_index))?;
                    prove_one(
                        &mut proven,
                        pre_mode,
                        clock_owner,
                        transaction,
                        transaction_index,
                    )?;
                }
            }
            solve::EventTransactionLegacyOwner::StructuredUpdate { update_index } => {
                let update = discrete
                    .structured_updates
                    .get(update_index)
                    .ok_or_else(|| unresolved_owner_error(transaction, transaction_index))?;
                let assignments = discrete
                    .structured_assignments(update_index)
                    .map_err(|_| unresolved_owner_error(transaction, transaction_index))?;
                // Cardinality first. A projection that covers only a prefix of
                // the target would otherwise prove that prefix and still mark
                // the whole structured owner superseded, silencing the
                // coordinates it never replaced.
                if assignments.len() != width {
                    return Err(mismatched_target_error(transaction, transaction_index));
                }
                for (offset, (produced, _)) in assignments.into_iter().enumerate() {
                    prove_same_target(
                        produced,
                        target.base(),
                        offset,
                        transaction,
                        transaction_index,
                    )?;
                }
                prove_one(
                    &mut proven,
                    update.pre_mode,
                    update.clock_owner,
                    transaction,
                    transaction_index,
                )?;
            }
            solve::EventTransactionLegacyOwner::GuardedAssignment {
                program_index,
                target_range_index,
            } => {
                let program = discrete
                    .guarded_assignments
                    .get(program_index)
                    .ok_or_else(|| unresolved_owner_error(transaction, transaction_index))?;
                let range = program
                    .target_ranges()
                    .get(target_range_index)
                    .ok_or_else(|| unresolved_owner_error(transaction, transaction_index))?;
                if range.count() != width {
                    return Err(mismatched_target_error(transaction, transaction_index));
                }
                for offset in 0..width {
                    let produced =
                        super::guarded_assignments::guarded_target_at(range.base(), offset)
                            .map_err(|_| unresolved_owner_error(transaction, transaction_index))?;
                    prove_same_target(
                        produced,
                        target.base(),
                        offset,
                        transaction,
                        transaction_index,
                    )?;
                }
                prove_one(
                    &mut proven,
                    program.pre_mode(),
                    program.clock_owner(),
                    transaction,
                    transaction_index,
                )?;
            }
        }
    }
    let proven = proven.ok_or_else(|| {
        activation_contract_error(
            transaction,
            format!(
                "event transaction {transaction_index} supersedes no owner, so its activation is \
                 unproven"
            ),
        )
    })?;
    if proven.clock_owner != transaction.clock_owner() {
        return Err(activation_contract_error(
            transaction,
            format!(
                "event transaction {transaction_index} disagrees with the owners it supersedes \
                 about scheduled ownership"
            ),
        ));
    }
    Ok(proven)
}

/// The storage the linked owner writes must be the very storage the
/// transaction target replaces. Without this an owner of the same width, mode,
/// and schedule can be swapped into the link and silently suppress a different
/// program than the one the transaction stands in for.
fn prove_same_target(
    produced: solve::ScalarSlot,
    target_base: solve::ScalarSlot,
    offset: usize,
    transaction: &solve::EventTransactionProgram,
    transaction_index: usize,
) -> Result<(), solve_eval::EvalSolveError> {
    let expected = super::guarded_assignments::guarded_target_at(target_base, offset)
        .map_err(|_| mismatched_target_error(transaction, transaction_index))?;
    if produced == expected {
        return Ok(());
    }
    Err(mismatched_target_error(transaction, transaction_index))
}

fn mismatched_target_error(
    transaction: &solve::EventTransactionProgram,
    transaction_index: usize,
) -> solve_eval::EvalSolveError {
    activation_contract_error(
        transaction,
        format!(
            "event transaction {transaction_index} links an owner that writes storage other \
             than the target it replaces"
        ),
    )
}

fn prove_one(
    proven: &mut Option<TransactionActivation>,
    pre_mode: solve::DiscreteEventPreMode,
    clock_owner: Option<solve::PeriodicClockId>,
    transaction: &solve::EventTransactionProgram,
    transaction_index: usize,
) -> Result<(), solve_eval::EvalSolveError> {
    let activation = TransactionActivation {
        pre_mode: crate::EventPreMode::from(pre_mode),
        clock_owner,
    };
    match proven {
        Some(existing) if *existing != activation => Err(activation_contract_error(
            transaction,
            format!(
                "event transaction {transaction_index} supersedes owners with more than one \
                 activation identity, so no single rule admits it"
            ),
        )),
        Some(_) => Ok(()),
        None => {
            *proven = Some(activation);
            Ok(())
        }
    }
}

fn unresolved_owner_error(
    transaction: &solve::EventTransactionProgram,
    transaction_index: usize,
) -> solve_eval::EvalSolveError {
    activation_contract_error(
        transaction,
        format!("event transaction {transaction_index} supersedes an owner that does not resolve"),
    )
}

fn activation_contract_error(
    transaction: &solve::EventTransactionProgram,
    message: String,
) -> solve_eval::EvalSolveError {
    solve_eval::EvalSolveError::ShapeContract {
        message,
        span: Some(transaction.span()),
    }
}

fn mark_superseded_coverage(
    transaction: &solve::EventTransactionProgram,
    transaction_index: usize,
    discrete_rows: &mut [bool],
    guarded_assignments: &mut [bool],
    structured_updates: &mut [bool],
) -> Result<(), solve_eval::EvalSolveError> {
    for (target, owner) in transaction
        .targets()
        .iter()
        .zip(transaction.legacy_owners())
    {
        let width = target.value_type().scalar_count() as usize;
        match *owner {
            solve::EventTransactionLegacyOwner::ScalarRows { start_row } => {
                let run = superseded_scalar_rows(
                    discrete_rows.len(),
                    start_row,
                    width,
                    transaction,
                    transaction_index,
                )?;
                discrete_rows[run].fill(true);
            }
            solve::EventTransactionLegacyOwner::StructuredUpdate { update_index } => {
                *structured_updates
                    .get_mut(update_index)
                    .ok_or_else(|| unresolved_owner_error(transaction, transaction_index))? = true;
            }
            solve::EventTransactionLegacyOwner::GuardedAssignment { program_index, .. } => {
                *guarded_assignments
                    .get_mut(program_index)
                    .ok_or_else(|| unresolved_owner_error(transaction, transaction_index))? = true;
            }
        }
    }
    Ok(())
}

/// Mark the model actions this transaction's own assertions project onto.
///
/// Each linked action must match the assertion the transaction carries, not
/// merely be declared at that index: suppressing an unrelated action would
/// silence a check the model still owns.
///
/// The match is *structural* — kind, message, span, origin, schedule. That is
/// not proof of source occurrence, and two equal cloned entries would be
/// interchangeable, so an ambiguous occurrence is rejected outright rather than
/// resolved by position. Deletion edge: when actions carry a
/// construction-issued identity, this comparison and its ambiguity rejection
/// are replaced by that identity in the same cutover.
fn mark_action_coverage(
    model: &solve::SolveModel,
    transaction: &solve::EventTransactionProgram,
    transaction_index: usize,
    event_actions: &mut [bool],
) -> Result<(), solve_eval::EvalSolveError> {
    let assertions = transaction.assertions();
    for (assertion_index, indices) in transaction.assertion_action_indices().iter().enumerate() {
        let assertion = assertions.get(assertion_index).ok_or_else(|| {
            activation_contract_error(
                transaction,
                format!(
                    "event transaction {transaction_index} projects assertion \
                     {assertion_index}, which it does not carry"
                ),
            )
        })?;
        for &action_index in indices.iter() {
            let declared = model
                .problem
                .events
                .actions
                .get(action_index)
                .ok_or_else(|| {
                    activation_contract_error(
                        transaction,
                        format!(
                            "event transaction {transaction_index} supersedes event action \
                         {action_index}, which the model does not declare"
                        ),
                    )
                })?;
            if declared != assertion {
                return Err(activation_contract_error(
                    transaction,
                    format!(
                        "event transaction {transaction_index} supersedes event action \
                         {action_index}, which is not the assertion it carries"
                    ),
                ));
            }
            if model
                .problem
                .events
                .actions
                .iter()
                .filter(|candidate| *candidate == assertion)
                .count()
                != 1
            {
                return Err(activation_contract_error(
                    transaction,
                    format!(
                        "event transaction {transaction_index} projects assertion \
                         {assertion_index} onto an action the model declares more than once, \
                         so the occurrence it supersedes is not determined"
                    ),
                ));
            }
            event_actions[action_index] = true;
        }
    }
    Ok(())
}

impl SolveRuntime {
    pub(super) fn evaluate_event_transactions_for_snapshot(
        &self,
        snapshot: &DiscretePreSnapshot<'_>,
        observation_only: bool,
        skip_solver_or_time: bool,
        y: &[f64],
        p: &[f64],
        time: f64,
        tally: &mut DiscreteRowPassTally,
    ) -> Result<Vec<usize>, RuntimeSolveError> {
        if snapshot.event_iteration != 0 || observation_only {
            return Ok(Vec::new());
        }
        let mut evaluated = Vec::new();
        for transaction_index in 0..self.event_transaction_programs.len() {
            if !self.event_transaction_active_at(transaction_index, time)? {
                continue;
            }
            if !self.event_transaction_accepts_snapshot(transaction_index, snapshot)? {
                continue;
            }
            if skip_solver_or_time
                && self.event_transaction_reads_solver_or_time(transaction_index)?
            {
                continue;
            }
            let owner = DiscreteOwnerId::EventTransaction(transaction_index);
            let scheduled = self
                .model
                .problem
                .discrete
                .event_transactions
                .get(transaction_index)
                .is_some_and(|program| program.clock_owner().is_some());
            let targets = self
                .event_transaction(transaction_index)?
                .target_scalar_count();
            self.ledger_admit_transaction(transaction_index, snapshot.row_filter);
            self.eval_event_transaction_outputs(transaction_index, y, p, time)?;
            self.ledger_record_transaction_evaluation(transaction_index);
            self.note_transaction_assertions(transaction_index);
            tally.admit(owner, scheduled, targets);
            evaluated.push(transaction_index);
        }
        Ok(evaluated)
    }

    /// Whether `row_filter` admits this transaction, through the one activation
    /// identity every owner it supersedes was proven to share.
    fn event_transaction_accepts_snapshot(
        &self,
        transaction_index: usize,
        snapshot: &DiscretePreSnapshot<'_>,
    ) -> Result<bool, RuntimeSolveError> {
        let activation = self
            .event_transaction_coverage
            .transaction_activation
            .get(transaction_index)
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "event transaction {transaction_index} has no proven activation identity"
                ))
            })?;
        Ok(snapshot.admits(activation.pre_mode, activation.clock_owner.is_some()))
    }

    pub(super) fn commit_successful_event_transactions(
        &self,
        transaction_indices: impl IntoIterator<Item = usize>,
        y: &mut [f64],
        p: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let mut changed = false;
        for transaction_index in transaction_indices {
            if self
                .failed_event_transaction_assertion(transaction_index)?
                .is_none()
            {
                changed |= self.commit_event_transaction_targets(transaction_index, y, p)?;
            }
        }
        Ok(changed)
    }

    pub(super) fn project_event_transaction_action_values(
        &self,
        time: f64,
        values: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for (transaction_index, transaction) in self
            .model
            .problem
            .discrete
            .event_transactions
            .iter()
            .enumerate()
        {
            if !self.event_transaction_active_at(transaction_index, time)? {
                continue;
            }
            self.project_one_event_transaction_action_values(
                transaction_index,
                transaction,
                values,
            )?;
        }
        Ok(())
    }

    fn project_one_event_transaction_action_values(
        &self,
        transaction_index: usize,
        transaction: &solve::EventTransactionProgram,
        values: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for (assertion_index, action_indices) in
            transaction.assertion_action_indices().iter().enumerate()
        {
            let failed =
                self.event_transaction_assertion_failed(transaction_index, assertion_index)?;
            let value = if failed { 1.0 } else { 0.0 };
            for &action_index in action_indices.iter() {
                values[action_index] = value;
            }
        }
        Ok(())
    }

    pub(super) fn event_transaction_active_at(
        &self,
        transaction_index: usize,
        time: f64,
    ) -> Result<bool, RuntimeSolveError> {
        let owner = self
            .model
            .problem
            .discrete
            .event_transactions
            .get(transaction_index)
            .ok_or_else(|| RuntimeSolveError::solve_ir("event transaction is out of bounds"))?;
        let Some(clock) = owner.clock_owner() else {
            return Ok(true);
        };
        let schedule = self
            .model
            .problem
            .clocks
            .periodic_schedule(clock)
            .ok_or_else(|| RuntimeSolveError::solve_ir("transaction clock is out of bounds"))?;
        Ok(crate::timeline::periodic_schedule_matches_time(
            schedule, time,
        ))
    }

    pub(super) fn event_transaction_reads_solver_or_time(
        &self,
        transaction_index: usize,
    ) -> Result<bool, RuntimeSolveError> {
        let program = self
            .model
            .problem
            .discrete
            .event_transactions
            .get(transaction_index)
            .ok_or_else(|| RuntimeSolveError::solve_ir("event transaction is out of bounds"))?;
        Ok(program.inputs().iter().any(|input| {
            matches!(
                input.source(),
                solve::ScalarSlot::Y { .. } | solve::ScalarSlot::Time
            )
        }))
    }

    /// Evaluate, check, and atomically commit one already-selected transaction.
    /// A failed predicate returns its action ordinal and commits no target.
    pub fn execute_event_transaction(
        &self,
        transaction_index: usize,
        y: &mut [f64],
        p: &mut [f64],
        time: f64,
    ) -> Result<EventTransactionExecution, RuntimeSolveError> {
        self.eval_event_transaction_outputs(transaction_index, y, p, time)?;
        let failed_assertion = self.failed_event_transaction_assertion(transaction_index)?;
        if failed_assertion.is_some() {
            return Ok(EventTransactionExecution {
                changed: false,
                failed_assertion,
            });
        }
        let changed = self.commit_event_transaction_targets(transaction_index, y, p)?;
        Ok(EventTransactionExecution {
            changed,
            failed_assertion: None,
        })
    }

    /// Evaluate one complete target/predicate tuple. Native and interpreter
    /// paths consume the same checked owner and aggregate input ordering.
    pub(super) fn eval_event_transaction_outputs(
        &self,
        transaction_index: usize,
        y: &[f64],
        p: &[f64],
        time: f64,
    ) -> Result<(), RuntimeSolveError> {
        let transaction = self.event_transaction(transaction_index)?;
        let mut input = self.event_transaction_input_scratch.borrow_mut();
        transaction.load_input_payload(y, p, time, &mut input)?;
        let mut outputs = self.event_transaction_output_scratch.borrow_mut();
        let output = outputs.get_mut(transaction_index).ok_or_else(|| {
            RuntimeSolveError::solve_ir("event transaction output scratch is out of bounds")
        })?;
        if let Some(compiled) = self
            .compiled_event_transactions
            .get(transaction_index)
            .and_then(Option::as_ref)
        {
            compiled.call(&input, output).map_err(|error| {
                RuntimeSolveError::solve_ir(format!(
                    "compiled event transaction {transaction_index} failed: {error}"
                ))
            })?;
            return Ok(());
        }
        transaction.eval_payload(&self.model.pure_calls, &input, output)?;
        Ok(())
    }

    /// Count the checked assertions one payload evaluation just re-evaluated.
    fn note_transaction_assertions(&self, transaction_index: usize) {
        let assertions = self
            .model
            .problem
            .discrete
            .event_transactions
            .get(transaction_index)
            .map_or(0, |program| program.assertions().len());
        self.ledger_record_transaction_assertions(transaction_index, assertions);
    }

    /// Return the first failed checked assertion without committing any
    /// target. Predicate values are the scalar suffix proved by SOLVE-C55.
    pub(super) fn failed_event_transaction_assertion(
        &self,
        transaction_index: usize,
    ) -> Result<Option<usize>, RuntimeSolveError> {
        let transaction = self.event_transaction(transaction_index)?;
        let outputs = self.event_transaction_output_scratch.borrow();
        let output = outputs.get(transaction_index).ok_or_else(|| {
            RuntimeSolveError::solve_ir("event transaction output scratch is out of bounds")
        })?;
        if output.len() != transaction.output_scalar_count() {
            return Err(RuntimeSolveError::solve_ir(
                "event transaction output scratch is not initialized",
            ));
        }
        Ok(output[transaction.target_scalar_count()..]
            .iter()
            .position(|predicate| *predicate == 0.0))
    }

    pub(super) fn event_transaction_assertion_failed(
        &self,
        transaction_index: usize,
        assertion_index: usize,
    ) -> Result<bool, RuntimeSolveError> {
        let transaction = self.event_transaction(transaction_index)?;
        if assertion_index >= transaction.program().assertion_count() {
            return Err(RuntimeSolveError::solve_ir(
                "event transaction assertion index is out of bounds",
            ));
        }
        let outputs = self.event_transaction_output_scratch.borrow();
        let output = outputs.get(transaction_index).ok_or_else(|| {
            RuntimeSolveError::solve_ir("event transaction output scratch is out of bounds")
        })?;
        Ok(output[transaction.target_scalar_count() + assertion_index] == 0.0)
    }

    /// Commit the complete target tuple after every predicate has succeeded.
    /// All destination bounds are checked before the first write, making a
    /// partially committed transaction unrepresentable at this boundary.
    pub(super) fn commit_event_transaction_targets(
        &self,
        transaction_index: usize,
        y: &mut [f64],
        p: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let transaction = self.event_transaction(transaction_index)?;
        if self
            .failed_event_transaction_assertion(transaction_index)?
            .is_some()
        {
            return Err(RuntimeSolveError::solve_ir(
                "event transaction targets cannot commit after a failed assertion",
            ));
        }
        let outputs = self.event_transaction_output_scratch.borrow();
        let output = outputs.get(transaction_index).ok_or_else(|| {
            RuntimeSolveError::solve_ir("event transaction output scratch is out of bounds")
        })?;
        let targets = transaction.scalar_targets();
        if targets.len() != transaction.target_scalar_count() {
            return Err(RuntimeSolveError::solve_ir(
                "event transaction target adapter disagrees with its checked range count",
            ));
        }
        prevalidate_target_storage(targets, y, p)?;
        self.ledger_record_transaction_commit(transaction_index);
        solve_eval::apply_scalar_slot_values_exact(
            targets,
            &output[..transaction.target_scalar_count()],
            y,
            p,
        )
        .map_err(Into::into)
    }

    fn event_transaction(
        &self,
        transaction_index: usize,
    ) -> Result<&solve_eval::PreparedEventTransactionProgram, RuntimeSolveError> {
        self.event_transaction_programs
            .get(transaction_index)
            .ok_or_else(|| {
                RuntimeSolveError::solve_ir(format!(
                    "event transaction {transaction_index} is out of bounds"
                ))
            })
    }
}

fn prevalidate_target_storage(
    targets: &[solve::ScalarSlot],
    y: &[f64],
    p: &[f64],
) -> Result<(), RuntimeSolveError> {
    for target in targets {
        let valid = match target {
            solve::ScalarSlot::Y { index, .. } => *index < y.len(),
            solve::ScalarSlot::P { index, .. } => *index < p.len(),
            solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => false,
        };
        if !valid {
            return Err(RuntimeSolveError::solve_ir(
                "event transaction target is outside mutable runtime storage",
            ));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::num::NonZeroU64;

    use indexmap::IndexMap;
    use rumoca_core::{SourceId, Span};

    use super::*;

    fn transaction_model() -> solve::SolveModel {
        let span = Span::from_offsets(
            SourceId::from_source_name("event_transaction_runtime.mo"),
            0,
            1,
        );
        let provenance = span
            .require_provenance("event transaction runtime fixture")
            .unwrap();
        let integer_domain = solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap();
        let arithmetic = solve::SolveArithmeticProfile::construct(
            solve::SolveRealFormat::Binary64,
            solve::SolveRoundingMode::NearestTiesToEven,
            integer_domain,
        );
        let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
        let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
        let mut owner_id = None;
        let pure_calls = solve::SolvePureCallTable::construct(arithmetic, |table| {
            owner_id = Some(table.add_owner(
                solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
                vec![real.clone(), boolean.clone()],
                vec![
                    solve::SolvePureCallOutput::result(real.clone()),
                    solve::SolvePureCallOutput::assertion_predicate(),
                ],
                span,
                |builder, inputs, outputs| {
                    let value = builder.load(inputs[0], span)?;
                    let predicate = builder.load(inputs[1], span)?;
                    builder.store(outputs[0], value, span)?;
                    builder.store(outputs[1], predicate, span)
                },
            )?);
            Ok(())
        })
        .unwrap();
        let site = pure_calls
            .owner(owner_id.unwrap())
            .expect("owner resolves")
            .call_site();
        let transaction = solve::EventTransactionProgram::checked(
            solve::EventTransactionConstruction {
                site,
                inputs: vec![
                    (solve::scalar_slot_p(0), real.clone()),
                    (solve::scalar_slot_p(1), boolean),
                ],
                targets: vec![(solve::scalar_slot_p(2), real)],
                legacy_owners: vec![solve::EventTransactionLegacyOwner::ScalarRows {
                    start_row: 0,
                }],
                assertions: vec![solve::SolveEventAction {
                    kind: solve::SolveEventActionKind::Assert,
                    message: solve::SolveEventMessage {
                        parts: vec![solve::SolveEventMessagePart::Text("predicate".into())],
                    },
                    span,
                    origin: "fixture".into(),
                    clock_owner: None,
                }],
                assertion_action_indices: vec![vec![0]],
                statement_count: 1,
                clock_owner: None,
            },
            provenance,
        )
        .unwrap();
        let mut problem = solve::SolveProblem::default();
        problem.layout = solve::VarLayout::from_parts(IndexMap::new(), 0, 4);
        problem.solve_layout = solve::SolveLayout {
            variable_storage_runs: vec![solve::SolveVariableStorageRun {
                base: solve::scalar_slot_p(2),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::DiscreteReal,
                value_kind: solve::SolveVariableValueKind::Real,
            }],
            variable_declarations: vec![solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::DiscreteReal,
                solve::SolveVariableValueKind::Real,
            )],
            compiled_parameter_len: 4,
            discrete_real_scalar_names: vec!["target".into()],
            pre_param_bindings: vec![solve::PreParamBinding {
                dest_p_index: 3,
                source: solve::PreParamSource::P { index: 2 },
                clock_schedule: None,
            }],
            ..solve::SolveLayout::default()
        };
        problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            provenance,
        )
        .unwrap();
        problem.discrete.update_targets = vec![solve::scalar_slot_p(2)];
        problem.discrete.row_roles = vec![solve::DiscreteRowRole::Equation];
        problem.discrete.pre_modes = vec![solve::DiscreteEventPreMode::FollowCurrent];
        problem.discrete.observation_refresh = vec![false];
        problem.discrete.integrator_history_effects =
            vec![solve::IntegratorHistoryEffect::Preserve];
        problem.discrete.clock_owners = vec![None];
        problem.discrete.event_iteration_plan = solve::EventIterationPlan {
            runs: vec![solve::EventIterationRun {
                variable: 0,
                pre_binding_start: 0,
                owner: solve::EventIterationOwner::EventTransaction {
                    program_index: 0,
                    target_index: 0,
                },
            }],
        };
        problem.events.actions = transaction.assertions().to_vec();
        problem.events.action_conditions = solve::ScalarProgramBlock::with_source_span(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            provenance,
        )
        .unwrap();
        problem.discrete.event_transactions.push(transaction);
        let model = solve::SolveModel {
            problem,
            pure_calls,
            parameters: vec![0.0; 4],
            ..solve::SolveModel::default()
        };
        model.validate().unwrap();
        model
    }

    /// A two-owner transaction whose superseded owners can be given
    /// disagreeing activation identities.
    ///
    /// `model.validate()` is deliberately not called: these fixtures probe the
    /// preparation boundary, which must reject the mixture on its own rather
    /// than relying on an earlier gate to have done it.
    fn two_owner_transaction_model(
        pre_modes: [solve::DiscreteEventPreMode; 2],
        clock_owners: [Option<solve::PeriodicClockId>; 2],
        legacy_owners: Vec<solve::EventTransactionLegacyOwner>,
    ) -> solve::SolveModel {
        let span = Span::from_offsets(
            SourceId::from_source_name("event_transaction_activation.mo"),
            0,
            1,
        );
        let provenance = span
            .require_provenance("event transaction activation fixture")
            .unwrap();
        let integer_domain = solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap();
        let arithmetic = solve::SolveArithmeticProfile::construct(
            solve::SolveRealFormat::Binary64,
            solve::SolveRoundingMode::NearestTiesToEven,
            integer_domain,
        );
        let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
        let mut owner_id = None;
        let pure_calls = solve::SolvePureCallTable::construct(arithmetic, |table| {
            owner_id = Some(table.add_owner(
                solve::SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
                vec![real.clone(), real.clone()],
                vec![
                    solve::SolvePureCallOutput::result(real.clone()),
                    solve::SolvePureCallOutput::result(real.clone()),
                ],
                span,
                |builder, inputs, outputs| {
                    let first = builder.load(inputs[0], span)?;
                    let second = builder.load(inputs[1], span)?;
                    builder.store(outputs[0], first, span)?;
                    builder.store(outputs[1], second, span)
                },
            )?);
            Ok(())
        })
        .unwrap();
        let site = pure_calls
            .owner(owner_id.unwrap())
            .expect("owner resolves")
            .call_site();
        let transaction = solve::EventTransactionProgram::checked(
            solve::EventTransactionConstruction {
                site,
                inputs: vec![
                    (solve::scalar_slot_p(0), real.clone()),
                    (solve::scalar_slot_p(1), real.clone()),
                ],
                targets: vec![
                    (solve::scalar_slot_p(2), real.clone()),
                    (solve::scalar_slot_p(3), real),
                ],
                legacy_owners,
                assertions: Vec::new(),
                assertion_action_indices: Vec::new(),
                statement_count: 1,
                clock_owner: None,
            },
            provenance,
        )
        .unwrap();
        let mut problem = solve::SolveProblem::default();
        problem.layout = solve::VarLayout::from_parts(IndexMap::new(), 0, 4);
        problem.solve_layout = solve::SolveLayout {
            compiled_parameter_len: 4,
            ..solve::SolveLayout::default()
        };
        problem.discrete.rhs = solve::ScalarProgramBlock::with_source_span(
            vec![
                vec![
                    solve::LinearOp::Const { dst: 0, value: 0.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    solve::LinearOp::Const { dst: 0, value: 0.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            ],
            provenance,
        )
        .unwrap();
        problem.discrete.update_targets = vec![solve::scalar_slot_p(2), solve::scalar_slot_p(3)];
        problem.discrete.row_roles = vec![solve::DiscreteRowRole::Equation; 2];
        problem.discrete.pre_modes = pre_modes.to_vec();
        problem.discrete.observation_refresh = vec![false; 2];
        problem.discrete.integrator_history_effects =
            vec![solve::IntegratorHistoryEffect::Preserve; 2];
        problem.discrete.clock_owners = clock_owners.to_vec();
        problem.discrete.event_transactions.push(transaction);
        solve::SolveModel {
            problem,
            pure_calls,
            parameters: vec![0.0; 4],
            ..solve::SolveModel::default()
        }
    }

    /// Two distinct schedules, so a negative can use two identities that both
    /// exist rather than only `None` against `Some`.
    fn two_schedule_partition() -> solve::SolveClockPartition {
        let schedule = |seconds: f64| {
            solve::PeriodicEventSchedule::new(
                rumoca_core::ClockLattice::from_seconds(seconds, 0.0)
                    .expect("positive phase-zero lattice"),
            )
            .expect("phase-zero schedule")
        };
        solve::SolveClockPartition {
            periodic_event_schedules: vec![schedule(0.1), schedule(0.2)],
            activation_parameter_indices: vec![0, 1],
        }
    }

    /// A schedule identity for the mixed-ownership negative.
    fn scheduled_clock() -> Option<solve::PeriodicClockId> {
        two_schedule_partition().periodic_clock_id(0)
    }

    fn scalar_owners() -> Vec<solve::EventTransactionLegacyOwner> {
        vec![
            solve::EventTransactionLegacyOwner::ScalarRows { start_row: 0 },
            solve::EventTransactionLegacyOwner::ScalarRows { start_row: 1 },
        ]
    }

    fn preparation_error(model: &solve::SolveModel) -> String {
        SolveRuntime::new_fixture(model)
            .err()
            .expect("a transaction with an unproven activation must not prepare")
            .to_string()
    }

    #[test]
    fn a_uniform_two_owner_transaction_prepares() {
        let model = two_owner_transaction_model(
            [solve::DiscreteEventPreMode::FollowCurrent; 2],
            [None, None],
            scalar_owners(),
        );

        SolveRuntime::new_fixture(&model)
            .expect("owners that agree on one activation identity prepare");
    }

    #[test]
    fn mixed_superseded_pre_modes_do_not_prepare() {
        let model = two_owner_transaction_model(
            [
                solve::DiscreteEventPreMode::FollowCurrent,
                solve::DiscreteEventPreMode::Fixed,
            ],
            [None, None],
            scalar_owners(),
        );

        assert!(
            preparation_error(&model).contains("more than one activation identity"),
            "unexpected message: {}",
            preparation_error(&model)
        );
    }

    #[test]
    fn mixed_superseded_clock_ownership_does_not_prepare() {
        let model = two_owner_transaction_model(
            [solve::DiscreteEventPreMode::FollowCurrent; 2],
            [None, scheduled_clock()],
            scalar_owners(),
        );

        assert!(
            preparation_error(&model).contains("more than one activation identity"),
            "unexpected message: {}",
            preparation_error(&model)
        );
    }

    /// The transaction's own schedule and its owners' must be the same
    /// identity, not merely both present or both absent.
    #[test]
    fn a_transaction_that_disagrees_with_its_owners_about_scheduling_does_not_prepare() {
        let clock = scheduled_clock();
        let model = two_owner_transaction_model(
            [solve::DiscreteEventPreMode::FollowCurrent; 2],
            [clock, clock],
            scalar_owners(),
        );

        assert!(
            preparation_error(&model).contains("about scheduled ownership"),
            "unexpected message: {}",
            preparation_error(&model)
        );
    }

    #[test]
    fn a_superseded_owner_link_that_does_not_resolve_does_not_prepare() {
        let model = two_owner_transaction_model(
            [solve::DiscreteEventPreMode::FollowCurrent; 2],
            [None, None],
            vec![
                solve::EventTransactionLegacyOwner::ScalarRows { start_row: 0 },
                solve::EventTransactionLegacyOwner::ScalarRows { start_row: 99 },
            ],
        );

        assert!(
            preparation_error(&model).contains("but the discrete block has"),
            "unexpected message: {}",
            preparation_error(&model)
        );
    }

    /// Both links name a *real* producer of the right width, mode, and
    /// schedule — only the correlation is wrong. Nothing but an exact target
    /// comparison rejects this, and without it the transaction would suppress
    /// the other producer and leave its own running.
    /// A real structured owner whose projection covers *more* coordinates than
    /// the target it is linked to. Only a cardinality check rejects it; a
    /// per-offset comparison alone proves the overlap and marks the whole
    /// structured owner superseded, silencing the coordinates it never
    /// replaced. The strict-prefix case fails the same comparison.
    #[test]
    fn a_structured_link_that_does_not_cover_its_target_exactly_does_not_prepare() {
        let mut model = two_owner_transaction_model(
            [solve::DiscreteEventPreMode::FollowCurrent; 2],
            [None, None],
            vec![
                solve::EventTransactionLegacyOwner::StructuredUpdate { update_index: 0 },
                solve::EventTransactionLegacyOwner::ScalarRows { start_row: 1 },
            ],
        );
        let span = Span::from_offsets(
            SourceId::from_source_name("event_transaction_activation.mo"),
            0,
            1,
        );
        let domain = rumoca_core::StructuredIndexDomain {
            binders: vec![rumoca_core::StructuredIndexBinder {
                id: 0,
                display_name: "i".to_string(),
                lower: 1,
                upper: 2,
                step: 1,
            }],
        };
        model.problem.discrete.structured_rhs = solve::ComputeBlock {
            nodes: vec![solve::ComputeNode::Map {
                output_map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                    .expect("two-point output map is valid"),
                domain: domain.clone(),
                base_ops: vec![
                    solve::LinearOp::Const { dst: 0, value: 7.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                load_strides: Vec::new(),
                const_strides: Vec::new(),
                metadata: solve::TensorNodeMetadata::default(),
                span,
            }],
        };
        // Two coordinates projected onto a width-one transaction target.
        model
            .problem
            .discrete
            .structured_updates
            .push(solve::StructuredDiscreteUpdate {
                node_index: 0,
                target: solve::StructuredDiscreteTargetMap {
                    base: solve::scalar_slot_p(2),
                    map: solve::TensorOutputMap::dense_contiguous(0, &domain)
                        .expect("two-point target map is valid"),
                },
                role: solve::DiscreteRowRole::Equation,
                pre_mode: solve::DiscreteEventPreMode::FollowCurrent,
                observation_refresh: false,
                integrator_history_effect: solve::IntegratorHistoryEffect::Preserve,
                clock_owner: None,
            });

        assert!(
            preparation_error(&model).contains("writes storage other than the target it replaces"),
            "unexpected message: {}",
            preparation_error(&model)
        );
    }

    #[test]
    fn a_superseded_owner_link_swapped_to_a_real_sibling_does_not_prepare() {
        let model = two_owner_transaction_model(
            [solve::DiscreteEventPreMode::FollowCurrent; 2],
            [None, None],
            vec![
                solve::EventTransactionLegacyOwner::ScalarRows { start_row: 1 },
                solve::EventTransactionLegacyOwner::ScalarRows { start_row: 0 },
            ],
        );

        assert!(
            preparation_error(&model).contains("writes storage other than the target it replaces"),
            "unexpected message: {}",
            preparation_error(&model)
        );
    }

    /// Two schedules that both exist are still two activations. A boolean
    /// "is scheduled" cannot tell them apart.
    #[test]
    fn two_distinct_schedules_do_not_prepare() {
        let clocks = two_schedule_partition();
        let model = two_owner_transaction_model(
            [solve::DiscreteEventPreMode::FollowCurrent; 2],
            [clocks.periodic_clock_id(0), clocks.periodic_clock_id(1)],
            scalar_owners(),
        );

        assert!(
            preparation_error(&model).contains("more than one activation identity"),
            "unexpected message: {}",
            preparation_error(&model)
        );
    }

    #[test]
    fn a_superseded_owner_link_swapped_to_another_family_does_not_prepare() {
        let model = two_owner_transaction_model(
            [solve::DiscreteEventPreMode::FollowCurrent; 2],
            [None, None],
            vec![
                solve::EventTransactionLegacyOwner::ScalarRows { start_row: 0 },
                // The producer is a scalar row; this names a guarded program
                // the model does not have.
                solve::EventTransactionLegacyOwner::GuardedAssignment {
                    program_index: 0,
                    target_range_index: 0,
                },
            ],
        );

        assert!(
            preparation_error(&model).contains("does not resolve"),
            "unexpected message: {}",
            preparation_error(&model)
        );
    }

    #[test]
    fn failed_predicate_commits_no_target_and_success_commits_whole_tuple() {
        let model = transaction_model();
        let runtime = SolveRuntime::new_fixture(&model).unwrap();
        let mut y = Vec::new();
        let mut p = vec![4.5, 0.0, 9.0, 9.0];

        let failed = runtime
            .execute_event_transaction(0, &mut y, &mut p, 0.0)
            .unwrap();
        assert_eq!(failed.failed_assertion, Some(0));
        assert!(!failed.changed);
        assert_eq!(p[2], 9.0, "a failed tuple cannot partially commit");

        p[1] = 1.0;
        let committed = runtime
            .execute_event_transaction(0, &mut y, &mut p, 0.0)
            .unwrap();
        assert_eq!(committed.failed_assertion, None);
        assert!(committed.changed);
        assert_eq!(p[2], 4.5);
    }

    #[test]
    fn checked_transaction_replaces_legacy_row_and_executes_only_on_first_pass() {
        let model = transaction_model();
        let runtime = SolveRuntime::new_fixture(&model).unwrap();
        let mut y = Vec::new();
        let mut p = vec![7.25, 1.0, 9.0, 9.0];
        let first = super::super::event_update::DiscretePreSnapshot {
            row_filter: super::super::event_update::EventUpdateRowFilter::All,
            root_relation_overrides: &[],
            event_iteration: 0,
        };
        assert!(
            runtime
                .apply_constant_discrete_rows_for_pre_snapshot(&first, &mut y, &mut p, 0.0, 0.0,)
                .unwrap()
        );
        assert_eq!(
            p[2], 7.25,
            "the aggregate owner, not legacy Const(0), commits"
        );

        p[0] = 8.5;
        let later = super::super::event_update::DiscretePreSnapshot {
            event_iteration: 1,
            ..first
        };
        assert!(
            !runtime
                .apply_constant_discrete_rows_for_pre_snapshot(&later, &mut y, &mut p, 0.0, 0.0,)
                .unwrap()
        );
        assert_eq!(p[2], 7.25, "later event passes consume the held commit");
    }
}
