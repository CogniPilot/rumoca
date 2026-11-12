//! Execution-order checks over an already-issued value schedule.
//!
//! This fact never adds an incidence/Jacobian edge or reorders a row. Each
//! dispatch that certifies a plan executes its rows in its own order, so each
//! asks its own question. A causal solution executes the rows once, in issued
//! order. The staged value schedule executes its stages in order: an
//! assignment stage replays its rows in order, and a projection stage solves
//! its block's targets together. A guard that needs a target its dispatch
//! produces later invalidates that dispatch's certificate; the existing
//! uncertified complete-projection path remains the executor.

use super::*;

/// Why an issued value order cannot certify complete execution.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RefreshExecutionRefusal {
    /// The row's actual execution prefix has no checked read proof.
    Unanalysable { row: RefreshRowOwnerId },
    /// A guard reads a target before the issued schedule produces it.
    ForwardRead {
        row: RefreshRowOwnerId,
        target: usize,
        read: usize,
    },
}

impl RefreshPlan {
    /// Derive both execution-order facts from the canonical source, retaining
    /// row order. Checked owner construction and wire reconstruction repeat
    /// this check on the final stages.
    pub fn check_execution_order<'a>(
        &mut self,
        mut lookup: impl FnMut(RefreshScalarProgramSource) -> Option<&'a [LinearOp]>,
    ) {
        self.check_causal_execution_order(&mut lookup);
        self.check_staged_execution_order(lookup);
    }

    /// A causal solution executes its rows once, in issued order: a row whose
    /// execution prefix reads a target produced at or after it clears the
    /// causal certificate. Construction asks this before it issues stages,
    /// which depend on the certificate.
    pub fn check_causal_execution_order<'a>(
        &mut self,
        lookup: impl FnMut(RefreshScalarProgramSource) -> Option<&'a [LinearOp]>,
    ) {
        if self.causal_solution_certified && self.causal_execution_refusal(lookup).is_some() {
            self.causal_solution_certified = false;
        }
    }

    /// The staged value schedule may read targets of earlier stages and, in a
    /// projection stage, the targets its block solves together; a read of any
    /// other target refuses the schedule. Construction asks this once the
    /// stages are issued.
    pub fn check_staged_execution_order<'a>(
        &mut self,
        lookup: impl FnMut(RefreshScalarProgramSource) -> Option<&'a [LinearOp]>,
    ) {
        self.execution_order_refusal = self.staged_execution_refusal(lookup);
    }

    fn causal_execution_refusal<'a>(
        &self,
        lookup: impl FnMut(RefreshScalarProgramSource) -> Option<&'a [LinearOp]>,
    ) -> Option<RefreshExecutionRefusal> {
        let producers = self
            .rows
            .iter()
            .enumerate()
            .map(|(position, row)| (row.target_index(), position))
            .collect::<BTreeMap<_, _>>();
        let mut reads = ExecutionReads::new(lookup);
        self.rows
            .iter()
            .enumerate()
            .find_map(|(position, row)| reads.first_unsettled_read(row, &producers, position))
    }

    fn staged_execution_refusal<'a>(
        &self,
        lookup: impl FnMut(RefreshScalarProgramSource) -> Option<&'a [LinearOp]>,
    ) -> Option<RefreshExecutionRefusal> {
        let order = StagedOrder::new(self);
        let mut reads = ExecutionReads::new(lookup);
        order
            .executed
            .iter()
            .find_map(|&(row, limit)| reads.first_unsettled_read(row, &order.first_producer, limit))
    }
}

/// Execution-read proofs of canonical sources, each analysed once.
struct ExecutionReads<L> {
    lookup: L,
    proofs: BTreeMap<RefreshScalarProgramSource, crate::ScalarProgramExecutionYDependency>,
}

impl<'a, L: FnMut(RefreshScalarProgramSource) -> Option<&'a [LinearOp]>> ExecutionReads<L> {
    fn new(lookup: L) -> Self {
        Self {
            lookup,
            proofs: BTreeMap::new(),
        }
    }

    /// The refusal of `row` when its execution prefix is unanalysable or reads
    /// a target whose producer in `producers` is ordered at or after `limit`.
    fn first_unsettled_read(
        &mut self,
        row: &AlgebraicRefreshRow,
        producers: &BTreeMap<usize, usize>,
        limit: usize,
    ) -> Option<RefreshExecutionRefusal> {
        let unanalysable = RefreshExecutionRefusal::Unanalysable {
            row: row.owner_id(),
        };
        let Some(program) = (self.lookup)(row.source()) else {
            return Some(unanalysable);
        };
        let len = row
            .assignment_shape()
            .map_or(program.len(), TargetAssignmentShape::expr_eval_len);
        let proof = self
            .proofs
            .entry(row.source())
            .or_insert_with(|| crate::ScalarProgramExecutionYDependency::new(program));
        let Some(reads) = proof.prefix(len) else {
            return Some(unanalysable);
        };
        let read = reads.intervals().find_map(|interval| {
            producers
                .range(interval)
                .find(|(_, producer)| **producer >= limit)
                .map(|(&read, _)| read)
        })?;
        Some(RefreshExecutionRefusal::ForwardRead {
            row: row.owner_id(),
            target: row.target_index(),
            read,
        })
    }
}

/// The order the staged value schedule executes rows in. Each assignment row
/// and each projection stage takes the next slot; `first_producer` holds the
/// first slot producing each refresh target (`usize::MAX` when no stage does).
/// An assignment row may read targets produced before its slot: earlier
/// stages and earlier rows of its own stage, never its own target or a later
/// row's. A projection stage's rows, the plan rows solved for its block's
/// targets, may read targets of earlier stages and the block's own unknowns,
/// which its projection solves simultaneously; a target that a later stage or
/// no stage produces still holds its incoming value there.
struct StagedOrder<'a> {
    first_producer: BTreeMap<usize, usize>,
    executed: Vec<(&'a AlgebraicRefreshRow, usize)>,
    rows_by_target: BTreeMap<usize, Vec<&'a AlgebraicRefreshRow>>,
    next_slot: usize,
}

impl<'a> StagedOrder<'a> {
    fn new(plan: &'a RefreshPlan) -> Self {
        let mut rows_by_target = BTreeMap::<_, Vec<_>>::new();
        for row in &plan.rows {
            rows_by_target
                .entry(row.target_index())
                .or_default()
                .push(row);
        }
        let mut order = Self {
            first_producer: rows_by_target
                .keys()
                .map(|&target| (target, usize::MAX))
                .collect(),
            executed: Vec::new(),
            rows_by_target,
            next_slot: 0,
        };
        for stage in &plan.value_stages {
            order.push_stage(plan, stage);
        }
        order
    }

    fn push_stage(&mut self, plan: &'a RefreshPlan, stage: &'a RefreshStage) {
        match stage {
            RefreshStage::CausalSeedSweep {
                static_rows,
                dynamic_rows,
                ..
            }
            | RefreshStage::ExactAssignments {
                static_rows,
                dynamic_rows,
                ..
            } => {
                self.push_assignments(plan.selected_rows(static_rows));
                self.push_assignments(plan.selected_rows(dynamic_rows));
            }
            RefreshStage::ProjectionBlock {
                plan: block_plan, ..
            } => self.push_projection(block_plan),
        }
    }

    /// One slot per row, in replay order.
    fn push_assignments(&mut self, rows: RefreshRows<'a>) {
        for row in (0..rows.len()).filter_map(|position| rows.get(position)) {
            self.produce(row.target_index());
            self.executed.push((row, self.next_slot));
            self.next_slot += 1;
        }
    }

    /// One slot for the whole block, whose rows also read the slot's targets.
    fn push_projection(&mut self, block_plan: &AlgebraicProjectionPlan) {
        let readable = self.next_slot + 1;
        for block in block_plan.blocks.explicit_blocks() {
            for &target in &block.y_indices {
                self.produce(target);
                let rows = self.rows_by_target.get(&target).into_iter().flatten();
                self.executed.extend(rows.map(|&row| (row, readable)));
            }
        }
        self.next_slot = readable;
    }

    fn produce(&mut self, target: usize) {
        let slot = self.next_slot;
        let first = self.first_producer.entry(target).or_insert(slot);
        *first = (*first).min(slot);
    }
}

impl ContinuousRefreshOwners {
    pub(super) fn rederive_execution_orders(
        &mut self,
        source: &ComputeBlock,
    ) -> Result<(), ContinuousRefreshConstructionError> {
        for plan in [
            &mut self.algebraic,
            &mut self.derivative,
            &mut self.root,
            &mut self.event,
        ]
        .into_iter()
        .chain(self.clock_events.iter_mut())
        {
            plan.check_execution_order(|owner| {
                scalar_source_program(source, owner)
                    .ok()
                    .flatten()
                    .map(|(program, _)| program)
            });
        }
        // Reissue remainders after refusals are known: a value-only causal
        // claim must not remove rows that the checked execution did not settle.
        let rebuilt = Self::checked(
            self.algebraic.clone(),
            self.derivative.clone(),
            self.root.clone(),
            self.event.clone(),
            self.clock_events.clone(),
        )?;
        self.root_after_derivative = rebuilt.root_after_derivative;
        self.root_after_algebraic = rebuilt.root_after_algebraic;
        self.algebraic_after_derivative = rebuilt.algebraic_after_derivative;
        self.clock_events_after_event = rebuilt.clock_events_after_event;
        Ok(())
    }
}

pub(super) fn validate_order(
    plan: &RefreshPlan,
    source: &ComputeBlock,
) -> Result<(), ContinuousRefreshConstructionError> {
    let mut lookup = |owner| {
        scalar_source_program(source, owner)
            .ok()
            .flatten()
            .map(|(program, _)| program)
    };
    if plan.causal_solution_certified && plan.causal_execution_refusal(&mut lookup).is_some()
        || plan.execution_order_refusal != plan.staged_execution_refusal(lookup)
    {
        return refresh_error(
            "refresh execution-order certificate differs from its canonical source".to_string(),
        );
    }
    Ok(())
}
