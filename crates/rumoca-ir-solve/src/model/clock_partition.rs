//! Validation of the SPEC_0040 SOLVE-C57 issued clock-partition order.
//!
//! The order is a construction-issued value: the runtime replays it and never
//! reconstructs membership from targets, names, spans, provenance, or program
//! shape. This module is the one place that checks the value is well formed,
//! kept beside [`DiscreteSolveSystem`] rather than inside it so the discrete
//! system stays a shape declaration.

use super::{ClockPartitionStep, DiscreteSolveSystem, EventTransactionProducerOwner};
use crate::SolveProblemShapeContractError;

/// One flag per clock-partition producer, used both for the transaction-owned
/// exclusion set and for the coverage the issued order claims.
struct ClockPartitionOwnerMask {
    rows: Vec<bool>,
    guarded: Vec<bool>,
    structured: Vec<bool>,
    intermediates: Vec<bool>,
}

impl ClockPartitionOwnerMask {
    fn sized_for(discrete: &DiscreteSolveSystem) -> Self {
        Self {
            rows: vec![false; discrete.rhs.len()],
            guarded: vec![false; discrete.guarded_assignments.len()],
            structured: vec![false; discrete.structured_updates.len()],
            intermediates: vec![false; discrete.clock_partition_intermediates.len()],
        }
    }

    fn claim_transaction_owner(
        &mut self,
        owner: EventTransactionProducerOwner,
        scalars: usize,
        row_count: usize,
    ) -> Result<(), SolveProblemShapeContractError> {
        match owner {
            EventTransactionProducerOwner::ScalarRows { start_row } => {
                let end = start_row
                    .checked_add(scalars)
                    .filter(|&end| end <= row_count)
                    .ok_or_else(|| {
                        order_error(start_row, "transaction scalar coverage overflows")
                    })?;
                self.rows[start_row..end].fill(true);
            }
            EventTransactionProducerOwner::StructuredUpdate { update_index } => {
                *self.structured.get_mut(update_index).ok_or_else(|| {
                    order_error(update_index, "transaction structured owner out of bounds")
                })? = true;
            }
            EventTransactionProducerOwner::GuardedAssignment { program_index, .. } => {
                *self.guarded.get_mut(program_index).ok_or_else(|| {
                    order_error(program_index, "transaction guarded owner out of bounds")
                })? = true;
            }
        }
        Ok(())
    }
}

/// Claim the one issued step of an intermediate-definition refresh row.
fn admit_intermediate_step(
    row: usize,
    covered: &mut ClockPartitionOwnerMask,
) -> Result<(), SolveProblemShapeContractError> {
    let claimed = covered
        .intermediates
        .get_mut(row)
        .ok_or_else(|| order_error(row, "intermediate step out of bounds"))?;
    if std::mem::replace(claimed, true) {
        return Err(order_error(row, "intermediate row scheduled twice"));
    }
    Ok(())
}

/// One typed contract error shape for every clock-partition order failure.
fn order_error(row: usize, detail: &'static str) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::DiscreteCertificate {
        context: "clock_partition_order",
        row,
        detail,
        span: None,
    }
}

impl DiscreteSolveSystem {
    /// Validate the SOLVE-C57 issued clock-partition order.
    ///
    /// Every clock-owned producer that is not owned by an event transaction
    /// must be named by exactly one step, no step may name an unclocked or
    /// transaction-owned producer, and every intermediate-definition row must
    /// be scheduled exactly once with a row-aligned work-state target.
    pub fn validate_clock_partition_order(&self) -> Result<(), SolveProblemShapeContractError> {
        let excluded = self.transaction_owned_producers()?;
        let covered = self.issued_step_coverage(&excluded)?;
        self.validate_clock_partition_coverage(&excluded, &covered)
    }

    /// Mark every producer a DAE-C21/SOLVE-C55 transaction owns: those are
    /// excluded from clock-partition admission, so they are neither schedulable
    /// nor required to carry a step.
    fn transaction_owned_producers(
        &self,
    ) -> Result<ClockPartitionOwnerMask, SolveProblemShapeContractError> {
        let mut mask = ClockPartitionOwnerMask::sized_for(self);
        for transaction in &self.event_transactions {
            for (target, owner) in transaction
                .targets()
                .iter()
                .zip(transaction.producer_owners())
            {
                let scalars = target.value_type().scalar_count() as usize;
                mask.claim_transaction_owner(*owner, scalars, self.rhs.len())?;
            }
        }
        Ok(mask)
    }

    /// Walk the issued order once, rejecting any step that names an unclocked
    /// or transaction-owned producer, or that schedules one twice.
    fn issued_step_coverage(
        &self,
        excluded: &ClockPartitionOwnerMask,
    ) -> Result<ClockPartitionOwnerMask, SolveProblemShapeContractError> {
        let mut covered = ClockPartitionOwnerMask::sized_for(self);
        for step in &self.clock_partition_order {
            match *step {
                ClockPartitionStep::ScalarRows { start_row, count } => {
                    self.admit_scalar_step(start_row, count, excluded, &mut covered)?;
                }
                ClockPartitionStep::GuardedAssignment { program_index } => {
                    self.admit_guarded_step(program_index, excluded, &mut covered)?;
                }
                ClockPartitionStep::StructuredUpdate { update_index } => {
                    self.admit_structured_step(update_index, excluded, &mut covered)?;
                }
                ClockPartitionStep::Intermediate { row } => {
                    admit_intermediate_step(row, &mut covered)?;
                }
            }
        }
        Ok(covered)
    }

    fn admit_scalar_step(
        &self,
        start_row: usize,
        count: usize,
        excluded: &ClockPartitionOwnerMask,
        covered: &mut ClockPartitionOwnerMask,
    ) -> Result<(), SolveProblemShapeContractError> {
        if count == 0 {
            return Err(order_error(start_row, "scalar step covers no rows"));
        }
        let end = start_row
            .checked_add(count)
            .filter(|&end| end <= self.rhs.len())
            .ok_or_else(|| order_error(start_row, "scalar step row overflow"))?;
        for row in start_row..end {
            if self.clock_owners[row].is_none() {
                return Err(order_error(row, "scalar step names an unclocked row"));
            }
            if excluded.rows[row] {
                return Err(order_error(
                    row,
                    "scalar step names a transaction-owned row",
                ));
            }
            if std::mem::replace(&mut covered.rows[row], true) {
                return Err(order_error(row, "scalar row scheduled twice"));
            }
        }
        Ok(())
    }

    fn admit_guarded_step(
        &self,
        program_index: usize,
        excluded: &ClockPartitionOwnerMask,
        covered: &mut ClockPartitionOwnerMask,
    ) -> Result<(), SolveProblemShapeContractError> {
        let owner = self
            .guarded_assignments
            .get(program_index)
            .ok_or_else(|| order_error(program_index, "guarded step out of bounds"))?;
        if owner.clock_owner().is_none() {
            return Err(order_error(
                program_index,
                "guarded step names an unclocked owner",
            ));
        }
        if excluded.guarded[program_index] {
            return Err(order_error(
                program_index,
                "guarded step names a transaction-owned owner",
            ));
        }
        if std::mem::replace(&mut covered.guarded[program_index], true) {
            return Err(order_error(program_index, "guarded owner scheduled twice"));
        }
        Ok(())
    }

    fn admit_structured_step(
        &self,
        update_index: usize,
        excluded: &ClockPartitionOwnerMask,
        covered: &mut ClockPartitionOwnerMask,
    ) -> Result<(), SolveProblemShapeContractError> {
        let update = self
            .structured_updates
            .get(update_index)
            .ok_or_else(|| order_error(update_index, "structured step out of bounds"))?;
        if update.clock_owner.is_none() {
            return Err(order_error(
                update_index,
                "structured step names an unclocked owner",
            ));
        }
        if excluded.structured[update_index] {
            return Err(order_error(
                update_index,
                "structured step names a transaction-owned owner",
            ));
        }
        if std::mem::replace(&mut covered.structured[update_index], true) {
            return Err(order_error(
                update_index,
                "structured owner scheduled twice",
            ));
        }
        Ok(())
    }

    /// Every clock-owned producer outside a transaction, and every issued
    /// intermediate row, must be scheduled exactly once.
    fn validate_clock_partition_coverage(
        &self,
        excluded: &ClockPartitionOwnerMask,
        covered: &ClockPartitionOwnerMask,
    ) -> Result<(), SolveProblemShapeContractError> {
        for (row, owner) in self.clock_owners.iter().enumerate() {
            if owner.is_some() && !excluded.rows[row] && !covered.rows[row] {
                return Err(order_error(
                    row,
                    "clock-owned scalar row has no issued step",
                ));
            }
        }
        for (program_index, owner) in self.guarded_assignments.iter().enumerate() {
            if owner.clock_owner().is_some()
                && !excluded.guarded[program_index]
                && !covered.guarded[program_index]
            {
                return Err(order_error(
                    program_index,
                    "clock-owned guarded owner has no issued step",
                ));
            }
        }
        for (update_index, update) in self.structured_updates.iter().enumerate() {
            if update.clock_owner.is_some()
                && !excluded.structured[update_index]
                && !covered.structured[update_index]
            {
                return Err(order_error(
                    update_index,
                    "clock-owned structured owner has no issued step",
                ));
            }
        }
        if let Some(row) = covered.intermediates.iter().position(|claimed| !claimed) {
            return Err(order_error(row, "intermediate row has no issued step"));
        }
        if self.clock_partition_intermediate_targets.len()
            != self.clock_partition_intermediates.len()
        {
            return Err(order_error(
                self.clock_partition_intermediate_targets.len(),
                "intermediate targets are not row-aligned",
            ));
        }
        Ok(())
    }
}
