//! Call-specialized schedules for function assertions.
//!
//! Scalar programs remain pure. Each representable call assertion contributes
//! a guarded root surface and a row-aligned guarded assertion action instead.

use rumoca_ir_solve as solve;
use std::collections::HashMap;
use std::sync::Arc;

use super::{LoweredLayout, clocks::LoweredClocks};
use crate::LowerError;
use crate::lower::scalar::{DeferredCallAssertion, ScalarCompiler};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct CallAssertionProjection {
    pub(super) owner: solve::SolvePureCallOwnerId,
    pub(super) output_offset: usize,
}

#[derive(PartialEq)]
pub(super) enum CollectedCallAssertionProgram<'dae> {
    Ready(Vec<solve::LinearOp>),
    Shared {
        owner: solve::SolvePureCallOwnerId,
        program: Arc<[solve::LinearOp]>,
        output_offset: usize,
    },
    Deferred(DeferredCallAssertion<'dae>),
}

#[derive(PartialEq)]
pub(super) enum CollectedCallAssertionRoot {
    Ready(Vec<solve::LinearOp>),
    Shared {
        owner: solve::SolvePureCallOwnerId,
        program: Arc<[solve::LinearOp]>,
        output_offset: usize,
    },
}

pub(super) struct CollectedCallAssertion<'dae> {
    pub(super) root_program: Option<CollectedCallAssertionRoot>,
    pub(super) action_program: CollectedCallAssertionProgram<'dae>,
    pub(super) action: solve::SolveEventAction,
    pub(super) clock_index: Option<usize>,
    pub(super) projection: Option<CallAssertionProjection>,
}

#[derive(Default)]
pub(crate) struct CallScopedActionCollector<'dae> {
    actions: Vec<CollectedCallAssertion<'dae>>,
}

impl<'dae> CallScopedActionCollector<'dae> {
    pub(super) fn insert(&mut self, action: CollectedCallAssertion<'dae>) {
        let duplicate = self.actions.iter().any(|existing| {
            existing.action.span == action.action.span
                && existing.projection == action.projection
                && existing.root_program == action.root_program
                && match (&existing.action_program, &action.action_program) {
                    (
                        CollectedCallAssertionProgram::Ready(existing),
                        CollectedCallAssertionProgram::Ready(action),
                    ) => existing == action,
                    (
                        CollectedCallAssertionProgram::Deferred(existing),
                        CollectedCallAssertionProgram::Deferred(action),
                    ) => existing.same_specialization(action),
                    (
                        CollectedCallAssertionProgram::Shared {
                            owner: existing_owner,
                            program: existing_program,
                            output_offset: existing_offset,
                        },
                        CollectedCallAssertionProgram::Shared {
                            owner: action_owner,
                            program: action_program,
                            output_offset: action_offset,
                        },
                    ) => {
                        existing_owner == action_owner
                            && existing_offset == action_offset
                            && existing_program == action_program
                    }
                    _ => false,
                }
        });
        if !duplicate {
            self.actions.push(action);
        }
    }
}

pub(super) fn append_collected_actions<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    events: &mut solve::SolveEventPartition,
    transactions: Vec<crate::lower::typed_functions::model_events::PendingEventTransaction<'dae>>,
) -> Result<Vec<solve::EventTransactionProgram>, LowerError> {
    let collected = std::mem::take(&mut layout.call_scoped_actions.borrow_mut().actions);
    let action_indices = if collected.is_empty() {
        HashMap::new()
    } else {
        append_roots(events, &collected)?;
        append_actions(view, layout, events, clocks, collected)?
    };
    transactions
        .into_iter()
        .map(|transaction| transaction.finish(&action_indices))
        .collect()
}

fn append_roots(
    events: &mut solve::SolveEventPartition,
    collected: &[CollectedCallAssertion<'_>],
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
    let mut shared: Vec<SharedProgram> = Vec::new();
    for action in collected {
        let Some(root_program) = &action.root_program else {
            continue;
        };
        let root_index = events.root_zero_domains.len();
        match root_program {
            CollectedCallAssertionRoot::Ready(program) => {
                programs.push(program.clone());
                spans.push(action.action.span);
                outputs.push(root_index);
            }
            CollectedCallAssertionRoot::Shared {
                owner,
                program,
                output_offset,
            } => push_shared_program(
                &mut shared,
                *owner,
                Arc::clone(program),
                *output_offset,
                root_index,
                action.action.span,
            )?,
        }
        events
            .root_zero_domains
            .push(solve::RootZeroDomain::Previous);
        events.root_relation_memory_targets.push(None);
    }
    append_shared_programs(&mut programs, &mut spans, &mut outputs, shared)?;
    events.root_conditions =
        solve::ScalarProgramBlock::with_output_indices(programs, spans, outputs)
            .map_err(|error| LowerError::contract(error.to_string(), first_span))?;
    Ok(())
}

fn append_actions<'dae>(
    view: rumoca_ir_dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    events: &mut solve::SolveEventPartition,
    clocks: &LoweredClocks<'dae>,
    collected: Vec<CollectedCallAssertion<'dae>>,
) -> Result<HashMap<CallAssertionProjection, Vec<usize>>, LowerError> {
    let first_span = collected[0].action.span;
    let mut programs = events.action_conditions.programs().to_vec();
    let mut spans = events.action_conditions.program_spans().to_vec();
    let mut outputs = events.action_conditions.output_indices().to_vec();
    let mut deferred = Vec::new();
    let mut shared: Vec<SharedProgram> = Vec::new();
    let mut action_indices = HashMap::new();
    for mut action in collected {
        let action_index = events.actions.len();
        if let Some(projection) = action.projection {
            action_indices
                .entry(projection)
                .or_insert_with(Vec::new)
                .push(action_index);
        }
        match action.action_program {
            CollectedCallAssertionProgram::Ready(program) => {
                programs.push(program);
                spans.push(action.action.span);
                outputs.push(action_index);
            }
            CollectedCallAssertionProgram::Deferred(recipe) => {
                deferred.push((recipe, action.action.span, action_index));
            }
            CollectedCallAssertionProgram::Shared {
                owner,
                program,
                output_offset,
            } => {
                push_shared_program(
                    &mut shared,
                    owner,
                    program,
                    output_offset,
                    action_index,
                    action.action.span,
                )?;
            }
        }
        action.action.clock_owner = action
            .clock_index
            .map(|index| clocks.clock_index(index))
            .transpose()?;
        events.actions.push(action.action);
    }
    append_shared_programs(&mut programs, &mut spans, &mut outputs, shared)?;
    for group in deferred_action_groups(deferred) {
        let span = group[0].1;
        let program = ScalarCompiler::deferred_call_action_program(
            view,
            layout,
            group.iter().map(|entry| &entry.0),
        )?;
        programs.push(program);
        spans.push(span);
        outputs.extend(group.iter().map(|entry| entry.2));
    }
    events.action_conditions =
        solve::ScalarProgramBlock::with_output_indices(programs, spans, outputs)
            .map_err(|error| LowerError::contract(error.to_string(), first_span))?;
    Ok(action_indices)
}

struct SharedProgram {
    owner: solve::SolvePureCallOwnerId,
    program: Arc<[solve::LinearOp]>,
    span: rumoca_core::Span,
    outputs: Vec<(usize, usize)>,
}

fn push_shared_program(
    shared: &mut Vec<SharedProgram>,
    owner: solve::SolvePureCallOwnerId,
    program: Arc<[solve::LinearOp]>,
    output_offset: usize,
    destination: usize,
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    if let Some(group) = shared
        .iter_mut()
        .find(|group| Arc::ptr_eq(&group.program, &program))
    {
        if group.owner != owner {
            return Err(LowerError::contract(
                "one shared call construction has multiple owners",
                span,
            ));
        }
        group.outputs.push((output_offset, destination));
    } else {
        shared.push(SharedProgram {
            owner,
            program,
            span,
            outputs: vec![(output_offset, destination)],
        });
    }
    Ok(())
}

fn append_shared_programs(
    programs: &mut Vec<Vec<solve::LinearOp>>,
    spans: &mut Vec<rumoca_core::Span>,
    outputs: &mut Vec<usize>,
    shared: Vec<SharedProgram>,
) -> Result<(), LowerError> {
    for mut group in shared {
        group.outputs.sort_unstable_by_key(|&(offset, _)| offset);
        if !group
            .outputs
            .iter()
            .enumerate()
            .all(|(expected, &(actual, _))| expected == actual)
        {
            return Err(LowerError::contract(
                "shared call outputs do not form one complete ordered tuple",
                group.span,
            ));
        }
        programs.push(group.program.to_vec());
        spans.push(group.span);
        outputs.extend(
            group
                .outputs
                .into_iter()
                .map(|(_, destination)| destination),
        );
    }
    Ok(())
}

fn deferred_action_groups<'dae>(
    deferred: Vec<(DeferredCallAssertion<'dae>, rumoca_core::Span, usize)>,
) -> Vec<Vec<(DeferredCallAssertion<'dae>, rumoca_core::Span, usize)>> {
    let mut groups: Vec<Vec<(DeferredCallAssertion<'dae>, rumoca_core::Span, usize)>> = Vec::new();
    for entry in deferred {
        if let Some(group) = groups
            .iter_mut()
            .find(|group| group[0].0.active_clock() == entry.0.active_clock())
        {
            group.push(entry);
        } else {
            groups.push(vec![entry]);
        }
    }
    groups
}
