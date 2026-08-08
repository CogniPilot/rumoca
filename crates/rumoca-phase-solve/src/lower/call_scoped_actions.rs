//! Call-specialized schedules for function assertions.
//!
//! Scalar programs remain pure. Each representable call assertion contributes
//! a guarded root surface and a row-aligned guarded assertion action instead.

use rumoca_ir_solve as solve;

use super::LoweredLayout;
use crate::LowerError;

pub(super) struct CollectedCallAssertion {
    pub(super) root_program: Option<Vec<solve::LinearOp>>,
    pub(super) action_program: Vec<solve::LinearOp>,
    pub(super) action: solve::SolveEventAction,
}

#[derive(Default)]
pub(crate) struct CallScopedActionCollector {
    actions: Vec<CollectedCallAssertion>,
}

impl CallScopedActionCollector {
    pub(super) fn insert(&mut self, action: CollectedCallAssertion) {
        let duplicate = self.actions.iter().any(|existing| {
            existing.action.span == action.action.span
                && existing.root_program == action.root_program
                && existing.action_program == action.action_program
        });
        if !duplicate {
            self.actions.push(action);
        }
    }
}

pub(super) fn append_collected_actions(
    layout: &LoweredLayout<'_>,
    discrete: &solve::DiscreteSolveSystem,
    events: &mut solve::SolveEventPartition,
) -> Result<(), LowerError> {
    let collected = std::mem::take(&mut layout.call_scoped_actions.borrow_mut().actions);
    if collected.is_empty() {
        return Ok(());
    }
    append_roots(events, &collected)?;
    append_actions(events, collected)?;
    events.root_relation_refresh_roles = solve::derive_root_relation_refresh_roles(
        &events.root_conditions,
        &discrete.runtime_assignment_rhs,
        &discrete.runtime_assignment_targets,
        layout.solve_layout.state_scalar_count,
        layout.solve_layout.solver_scalar_count(),
    )?;
    Ok(())
}

fn append_roots(
    events: &mut solve::SolveEventPartition,
    collected: &[CollectedCallAssertion],
) -> Result<(), LowerError> {
    let Some(first_span) = collected
        .iter()
        .find_map(|action| action.root_program.as_ref().map(|_| action.action.span))
    else {
        return Ok(());
    };
    let mut programs = events.root_conditions.programs().to_vec();
    let mut spans = events.root_conditions.program_spans().to_vec();
    let mut outputs = events.root_conditions.output_indices().to_vec();
    for action in collected {
        let Some(root_program) = &action.root_program else {
            continue;
        };
        programs.push(root_program.clone());
        spans.push(action.action.span);
        outputs.push(events.root_zero_domains.len());
        events
            .root_zero_domains
            .push(solve::RootZeroDomain::Previous);
        events.root_relation_memory_targets.push(None);
    }
    events.root_conditions =
        solve::ScalarProgramBlock::with_output_indices(programs, spans, outputs)
            .map_err(|error| LowerError::contract(error.to_string(), first_span))?;
    Ok(())
}

fn append_actions(
    events: &mut solve::SolveEventPartition,
    collected: Vec<CollectedCallAssertion>,
) -> Result<(), LowerError> {
    let first_span = collected[0].action.span;
    let mut programs = events.action_conditions.programs().to_vec();
    let mut spans = events.action_conditions.program_spans().to_vec();
    let mut outputs = events.action_conditions.output_indices().to_vec();
    for action in collected {
        programs.push(action.action_program);
        spans.push(action.action.span);
        outputs.push(events.actions.len());
        events.actions.push(action.action);
    }
    events.action_conditions =
        solve::ScalarProgramBlock::with_output_indices(programs, spans, outputs)
            .map_err(|error| LowerError::contract(error.to_string(), first_span))?;
    Ok(())
}
