//! Event- and clock-specific continuous refresh dependency construction.

use std::collections::BTreeMap;

use rumoca_ir_solve as solve;

use crate::EvalSolveError;
use crate::sparsity::from_pattern_error;

use super::dependency_domain::CompactYDependencySet;
use super::{
    compact_dependency_error, extend_compute_node_dependencies, extend_program_dependencies,
    extend_scalar_block_dependencies, first_block_span, output_row_positions,
};

/// The solver coordinates the event consumers of one model read: the seeds
/// of the event refresh plans. They are execution reads: what can change the
/// outcome of evaluating the consumer, a guard without a result included, not
/// only what its values depend on. Whole programs contribute every solver
/// coordinate they load; a selected output contributes its value reads and
/// its program's execution obligations. What each owner reads, parameters
/// included, is the problem's issued `solve::EventOwnerReach`.
#[derive(Default)]
struct EventConsumerReads {
    y: CompactYDependencySet,
}

impl EventConsumerReads {
    fn extend_block(
        &mut self,
        block: &solve::ScalarProgramBlock,
        state_count: usize,
    ) -> Result<(), EvalSolveError> {
        extend_scalar_block_dependencies(&mut self.y, block, state_count)
    }

    fn extend_program(
        &mut self,
        program: &[solve::LinearOp],
        state_count: usize,
        span: Option<rumoca_core::Span>,
    ) -> Result<(), EvalSolveError> {
        extend_program_dependencies(&mut self.y, program, state_count, span)
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

/// The event consumers the event iteration of `clock_owner` (the unclocked
/// iteration for `None`) evaluates.
fn extend_event_consumer_reads(
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
