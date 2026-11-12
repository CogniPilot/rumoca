//! Event- and clock-specific continuous refresh dependency construction.

use std::collections::{BTreeMap, BTreeSet};

use rumoca_ir_solve as solve;

use crate::EvalSolveError;
use crate::sparsity::from_pattern_error;

use super::dependency_domain::CompactYDependencySet;
use super::{
    compact_dependency_error, extend_compute_node_dependencies, extend_program_dependencies,
    extend_scalar_block_dependencies, first_block_span, output_row_positions,
};

/// What the event consumers of one model read.
///
/// The solver coordinates seed the event refresh plans. The parameter slots
/// are collected by the same walk, and only for the input reach
/// (`solve::InputEventReach`), so both answers come from one consumer list.
///
/// Both are execution reads: what can change the outcome of evaluating the
/// consumer, a guard without a result included, not only what its values
/// depend on. Whole programs contribute every solver coordinate they load;
/// a selected output contributes its value reads and its program's
/// execution obligations.
#[derive(Default)]
pub(super) struct EventConsumerReads {
    y: CompactYDependencySet,
    p: Option<BTreeSet<usize>>,
    /// Set by a consumer whose parameter reads the dependency owner does not
    /// analyse: a structured update or an event transaction.
    opaque: bool,
}

impl EventConsumerReads {
    /// Reads that also collect parameter slots.
    pub(super) fn with_parameters() -> Self {
        Self {
            p: Some(BTreeSet::new()),
            ..Self::default()
        }
    }

    /// The non-state solver coordinates, the parameter slots when these reads
    /// collected them, and whether the parameter reads of some consumer are
    /// not analysable.
    pub(super) fn into_parts(self) -> (CompactYDependencySet, Option<BTreeSet<usize>>, bool) {
        (self.y, self.p, self.opaque)
    }

    pub(super) fn extend_block(
        &mut self,
        block: &solve::ScalarProgramBlock,
        state_count: usize,
    ) -> Result<(), EvalSolveError> {
        extend_scalar_block_dependencies(&mut self.y, block, state_count)?;
        for (index, program) in block.programs().iter().enumerate() {
            self.extend_parameters(program, block.program_span(index))?;
        }
        Ok(())
    }

    fn extend_program(
        &mut self,
        program: &[solve::LinearOp],
        state_count: usize,
        span: Option<rumoca_core::Span>,
    ) -> Result<(), EvalSolveError> {
        extend_program_dependencies(&mut self.y, program, state_count, span)?;
        self.extend_parameters(program, span)
    }

    pub(super) fn extend_parameters(
        &mut self,
        program: &[solve::LinearOp],
        span: Option<rumoca_core::Span>,
    ) -> Result<(), EvalSolveError> {
        let Some(parameters) = &mut self.p else {
            return Ok(());
        };
        parameters.extend(
            solve::StructuralPattern::derive_execution_p_dependency_union(program, span)
                .map_err(|error| from_pattern_error(error, span))?,
        );
        Ok(())
    }

    pub(super) fn insert_parameter(&mut self, index: usize) {
        if let Some(parameters) = &mut self.p {
            parameters.insert(index);
        }
    }

    pub(super) fn mark_opaque(&mut self) {
        self.opaque = true;
    }
}

pub(super) fn event_consumer_dependencies(
    problem: &solve::SolveProblem,
    state_count: usize,
    clock_owner: Option<solve::PeriodicClockId>,
) -> Result<CompactYDependencySet, EvalSolveError> {
    let mut reads = EventConsumerReads::default();
    extend_event_consumer_reads(&mut reads, problem, state_count, clock_owner)?;
    Ok(reads.y)
}

/// The one list of event consumers: every owner the event iteration of
/// `clock_owner` (the unclocked iteration for `None`) evaluates.
pub(super) fn extend_event_consumer_reads(
    dependencies: &mut EventConsumerReads,
    problem: &solve::SolveProblem,
    state_count: usize,
    clock_owner: Option<solve::PeriodicClockId>,
) -> Result<(), EvalSolveError> {
    if clock_owner.is_none() {
        extend_unclocked_dependencies(dependencies, problem, state_count)?;
    }
    extend_selected_scalar_output_dependencies(
        dependencies,
        &problem.discrete.rhs,
        problem
            .discrete
            .clock_owners
            .iter()
            .enumerate()
            .filter_map(|(output, owner)| (*owner == clock_owner).then_some(output)),
        state_count,
    )?;
    extend_selected_scalar_output_dependencies(
        dependencies,
        &problem.events.action_conditions,
        problem
            .events
            .actions
            .iter()
            .enumerate()
            .filter_map(|(output, action)| (action.clock_owner == clock_owner).then_some(output)),
        state_count,
    )?;
    extend_structured_update_dependencies(dependencies, problem, state_count, clock_owner)?;
    extend_guarded_assignment_dependencies(dependencies, problem, state_count, clock_owner)?;
    extend_transaction_dependencies(dependencies, problem, state_count, clock_owner)?;
    extend_action_message_dependencies(dependencies, problem, state_count, clock_owner)
}

fn extend_unclocked_dependencies(
    dependencies: &mut EventConsumerReads,
    problem: &solve::SolveProblem,
    state_count: usize,
) -> Result<(), EvalSolveError> {
    for block in [
        &problem.discrete.runtime_assignment_rhs,
        &problem.discrete.post_commit_assignment_rhs,
        &problem.events.root_conditions,
    ] {
        dependencies.extend_block(block, state_count)?;
    }
    Ok(())
}

fn extend_structured_update_dependencies(
    dependencies: &mut EventConsumerReads,
    problem: &solve::SolveProblem,
    state_count: usize,
    clock_owner: Option<solve::PeriodicClockId>,
) -> Result<(), EvalSolveError> {
    problem
        .discrete
        .structured_rhs
        .validate_shape_contract("continuous clock refresh dependency certificate")?;
    for update in problem
        .discrete
        .structured_updates
        .iter()
        .filter(|update| update.clock_owner == clock_owner)
    {
        let node = problem
            .discrete
            .structured_rhs
            .nodes
            .get(update.node_index)
            .ok_or_else(|| EvalSolveError::InvalidRow {
                message: "structured refresh owner refers to a missing compute node".to_string(),
                span: None,
            })?;
        let mut node_dependencies = CompactYDependencySet::default();
        extend_compute_node_dependencies(&mut node_dependencies, node, state_count)?;
        dependencies.mark_opaque();
        dependencies
            .y
            .extend(node_dependencies)
            .map_err(|error| compact_dependency_error(error, None))?;
    }
    Ok(())
}

fn extend_guarded_assignment_dependencies(
    dependencies: &mut EventConsumerReads,
    problem: &solve::SolveProblem,
    state_count: usize,
    clock_owner: Option<solve::PeriodicClockId>,
) -> Result<(), EvalSolveError> {
    for program in &problem.discrete.guarded_assignments {
        if program.clock_owner() == clock_owner {
            dependencies.extend_program(program.program(), state_count, Some(program.span()))?;
        }
    }
    Ok(())
}

fn extend_transaction_dependencies(
    dependencies: &mut EventConsumerReads,
    problem: &solve::SolveProblem,
    state_count: usize,
    clock_owner: Option<solve::PeriodicClockId>,
) -> Result<(), EvalSolveError> {
    for transaction in problem
        .discrete
        .event_transactions
        .iter()
        .filter(|transaction| match clock_owner {
            Some(clock) => transaction.clock_owners().binary_search(&clock).is_ok(),
            None => !transaction.is_clock_owned(),
        })
    {
        dependencies.mark_opaque();
        for input in transaction.inputs() {
            let solve::ScalarSlot::Y { index, .. } = input.source() else {
                continue;
            };
            let count = input.value_type().scalar_count() as usize;
            let end = index
                .checked_add(count)
                .ok_or_else(|| EvalSolveError::InvalidRow {
                    message: "event-transaction input dependency range overflows".to_string(),
                    span: Some(transaction.span()),
                })?;
            if end <= state_count {
                continue;
            }
            dependencies
                .y
                .insert_range(index.max(state_count)..end)
                .map_err(|error| compact_dependency_error(error, Some(transaction.span())))?;
        }
    }
    Ok(())
}

fn extend_action_message_dependencies(
    dependencies: &mut EventConsumerReads,
    problem: &solve::SolveProblem,
    state_count: usize,
    clock_owner: Option<solve::PeriodicClockId>,
) -> Result<(), EvalSolveError> {
    for action in problem
        .events
        .actions
        .iter()
        .filter(|action| action.clock_owner == clock_owner)
    {
        for part in &action.message.parts {
            let solve::SolveEventMessagePart::Conversion { value, .. } = part else {
                continue;
            };
            dependencies.extend_program(value, state_count, Some(action.span))?;
        }
    }
    Ok(())
}

fn extend_selected_scalar_output_dependencies(
    dependencies: &mut EventConsumerReads,
    block: &solve::ScalarProgramBlock,
    outputs: impl IntoIterator<Item = usize>,
    state_count: usize,
) -> Result<(), EvalSolveError> {
    let positions = output_row_positions(block)?;
    let mut by_program = BTreeMap::new();
    for output in outputs {
        let position =
            positions
                .get(&output)
                .copied()
                .ok_or_else(|| EvalSolveError::InvalidRow {
                    message: format!("event consumer output {output} has no scalar program owner"),
                    span: first_block_span(block),
                })?;
        let output_dependencies = match by_program.entry(position.program_index) {
            std::collections::btree_map::Entry::Occupied(entry) => entry.into_mut(),
            std::collections::btree_map::Entry::Vacant(entry) => {
                // Parameter reads are taken per program: a selected output
                // counts every parameter its program reads.
                dependencies.extend_parameters(
                    &block.programs()[position.program_index],
                    block.program_span(position.program_index),
                )?;
                let span = block.program_span(position.program_index);
                entry.insert(
                    solve::StructuralPattern::derive_output_execution_y_dependency_ranges(
                        &block.programs()[position.program_index],
                        span,
                    )
                    .map_err(|error| from_pattern_error(error, span))?,
                )
            }
        };
        let selected = output_dependencies
            .get(position.output_offset)
            .ok_or_else(|| EvalSolveError::InvalidRow {
                message: format!("event consumer output {output} has no dependency projection"),
                span: block.program_span(position.program_index),
            })?;
        dependencies
            .y
            .extend_explicit(selected.iter().filter(|index| *index >= state_count))
            .map_err(|error| {
                compact_dependency_error(error, block.program_span(position.program_index))
            })?;
    }
    Ok(())
}
