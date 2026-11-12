//! Execution-order checks over an already-issued value schedule.
//!
//! This fact never adds an incidence/Jacobian edge or reorders a row. A guard
//! that needs a target produced later invalidates the shortcut's certificate;
//! the existing uncertified complete-projection path remains the executor.

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
    /// Derive execution ordering from the canonical source, retaining row order.
    /// Checked owner construction and wire reconstruction repeat this check.
    pub fn check_execution_order<'a>(
        &mut self,
        lookup: impl FnMut(RefreshScalarProgramSource) -> Option<&'a [LinearOp]>,
    ) {
        self.execution_order_refusal = self.derive_execution_order_refusal(lookup);
        if self.execution_order_refusal.is_some() {
            self.causal_solution_certified = false;
        }
    }

    fn derive_execution_order_refusal<'a>(
        &self,
        mut lookup: impl FnMut(RefreshScalarProgramSource) -> Option<&'a [LinearOp]>,
    ) -> Option<RefreshExecutionRefusal> {
        let producers = self
            .rows
            .iter()
            .enumerate()
            .map(|(position, row)| (row.target_index(), position))
            .collect::<BTreeMap<_, _>>();
        let mut proofs: BTreeMap<
            RefreshScalarProgramSource,
            crate::ScalarProgramExecutionYDependency,
        > = BTreeMap::new();
        for (position, row) in self.rows.iter().enumerate() {
            let Some(program) = lookup(row.source()) else {
                return Some(RefreshExecutionRefusal::Unanalysable {
                    row: row.owner_id(),
                });
            };
            let len = row
                .assignment_shape()
                .map_or(program.len(), TargetAssignmentShape::expr_eval_len);
            let proof = proofs
                .entry(row.source())
                .or_insert_with(|| crate::ScalarProgramExecutionYDependency::new(program));
            let Some(reads) = proof.prefix(len) else {
                return Some(RefreshExecutionRefusal::Unanalysable {
                    row: row.owner_id(),
                });
            };
            let forward = reads.intervals().find_map(|interval| {
                producers
                    .range(interval)
                    .find(|(_, producer)| **producer >= position)
                    .map(|(&read, _)| read)
            });
            if let Some(read) = forward {
                return Some(RefreshExecutionRefusal::ForwardRead {
                    row: row.owner_id(),
                    target: row.target_index(),
                    read,
                });
            }
        }
        None
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
    let expected = plan.derive_execution_order_refusal(|owner| {
        scalar_source_program(source, owner)
            .ok()
            .flatten()
            .map(|(program, _)| program)
    });
    if plan.execution_order_refusal != expected
        || expected.is_some() && plan.causal_solution_certified
    {
        return refresh_error(
            "refresh execution-order certificate differs from its canonical source".to_string(),
        );
    }
    Ok(())
}
