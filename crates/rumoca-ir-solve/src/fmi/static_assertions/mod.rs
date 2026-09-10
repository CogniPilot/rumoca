//! Admission proof for parameter-dependent assertion partitions in generated C.

mod memory_reads;
pub(super) mod scalar_dependencies;
mod typed_dependencies;

use crate::{DiscreteRowRole, RefreshStage, ScalarSlot, SolveEventActionKind, SolveModel};

pub(super) fn validate(model: &SolveModel) -> Result<(), &'static str> {
    let problem = &model.problem;
    let events = &problem.events;
    let discrete = &problem.discrete;
    if crate::solve_has_runtime_events(problem)
        || crate::solve_has_clocks(problem)
        || !events.scheduled_root_conditions.is_empty()
        || !events.scheduled_time_events.is_empty()
        || !events.dynamic_time_event_rhs.is_empty()
        || !events.dynamic_time_event_names.is_empty()
        || events
            .root_relation_memory_targets
            .iter()
            .any(Option::is_some)
        || events.actions.is_empty()
        || events
            .actions
            .iter()
            .any(|a| a.kind != SolveEventActionKind::Assert || a.clock_owner.is_some())
    {
        return Err("only unscheduled assertions without relation memory are supported");
    }
    if !discrete.event_iteration_plan.runs.is_empty()
        || !discrete.runtime_assignment_rhs.is_empty()
        || !discrete.post_commit_assignment_rhs.is_empty()
        || !discrete.structured_rhs.is_empty()
        || !discrete.structured_updates.is_empty()
        || !discrete.guarded_assignments.is_empty()
        || !discrete.event_transactions.is_empty()
        || discrete
            .row_roles
            .iter()
            .any(|role| *role != DiscreteRowRole::ConditionMemory)
        || discrete
            .update_targets
            .iter()
            .any(|slot| !matches!(slot, ScalarSlot::P { .. }))
    {
        return Err("assertion profile cannot execute discrete state updates");
    }
    if !problem.solve_layout.pre_param_bindings.is_empty() {
        return Err("assertion profile cannot execute pre-value bindings");
    }
    memory_reads::validate(model)?;
    let mut y = vec![false; problem.layout.y_scalars()];
    let mut p = super::parameter_updates::stable_parameters(problem);
    static_algebraics(model, &mut y, &p)?;
    // Condition memories are private event data, constant between mode changes.
    // Their current-value producers must themselves be parameter-dependent.
    require_static(model, &discrete.rhs, &y, &p)?;
    for index in &events.condition_memory_parameter_indices {
        p[*index] = true;
    }
    for slot in &discrete.update_targets {
        if let ScalarSlot::P { index, .. } = slot {
            p[*index] = true;
        }
    }
    require_static(model, &events.root_conditions, &y, &p)?;
    require_static(model, &events.action_conditions, &y, &p)?;
    Ok(())
}

fn require_static(
    model: &SolveModel,
    block: &crate::ScalarProgramBlock,
    y: &[bool],
    p: &[bool],
) -> Result<(), &'static str> {
    match scalar_dependencies::outputs(&model.pure_calls, block, y, p) {
        Some(outputs) if outputs.iter().all(|v| *v) => Ok(()),
        _ => Err(
            "assertion depends on time, a continuous state, an input, or an unsupported dependence operation",
        ),
    }
}

fn static_algebraics(model: &SolveModel, y: &mut [bool], p: &[bool]) -> Result<(), &'static str> {
    let continuous = &model.problem.continuous;
    let owners = &continuous.refresh_owners;
    for stage in &owners.algebraic().value_stages {
        let (a, b) = match stage {
            RefreshStage::CausalSeedSweep { .. } => continue,
            RefreshStage::ExactAssignments {
                static_sequence,
                dynamic_sequence,
                ..
            } => (*static_sequence, *dynamic_sequence),
            RefreshStage::ProjectionBlock { .. } => {
                return Err("assertion profile requires exact algebraic assignments");
            }
        };
        for sequence in [a, b] {
            let Some(schedule) = owners.exact_assignment_schedule(sequence) else {
                continue;
            };
            for id in schedule.program_ids() {
                let issued = owners
                    .exact_assignment_program(*id)
                    .ok_or("missing exact assignment")?;
                transfer_assignment(model, issued, y, p)?;
            }
        }
    }
    Ok(())
}

fn transfer_assignment(
    model: &SolveModel,
    issued: &crate::ExactRefreshAssignmentProgram,
    y: &mut [bool],
    p: &[bool],
) -> Result<(), &'static str> {
    let block = issued
        .final_scalar_program(&model.problem.continuous.implicit_rhs)
        .map_err(|_| "invalid exact assignment")?;
    let values = scalar_dependencies::outputs(&model.pure_calls, &block, y, p)
        .ok_or("unsupported algebraic dependence operation")?;
    for (target, value) in issued.target_indices().iter().zip(values) {
        y[*target] = value;
    }
    Ok(())
}
