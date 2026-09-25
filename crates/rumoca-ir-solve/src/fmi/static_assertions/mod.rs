//! Admission proof for parameter-determined event partitions in generated C.
//!
//! The C profile executes a model's events only where every one is fixed
//! between parameter changes: error assertions whose predicates, their
//! condition memories, and every discrete equation depend on parameters and
//! constants alone. Such a partition changes nothing between events, so the
//! component evaluates it at initialization and at each event, exactly as the
//! runtime's event iteration would settle it in one pass.

mod memory_reads;
pub(super) mod scalar_dependencies;
#[cfg(test)]
mod tests;
mod typed_dependencies;

use crate::{DiscreteRowRole, RefreshStage, ScalarSlot, SolveEventActionKind, SolveModel};

/// Admit a parameter-determined event partition and return the order in which
/// the component evaluates the discrete rows.
pub(super) fn validate(model: &SolveModel) -> Result<DiscreteOrder, &'static str> {
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
        || events
            .actions
            .iter()
            .any(|a| a.kind != SolveEventActionKind::Assert || a.clock_owner.is_some())
    {
        return Err("only unscheduled assertions without relation memory are supported");
    }
    if !discrete.runtime_assignment_rhs.is_empty()
        || !discrete.post_commit_assignment_rhs.is_empty()
        || !discrete.structured_rhs.is_empty()
        || !discrete.structured_updates.is_empty()
        || !discrete.guarded_assignments.is_empty()
        || !discrete.event_transactions.is_empty()
        || discrete
            .update_targets
            .iter()
            .any(|slot| !matches!(slot, ScalarSlot::P { .. }))
    {
        return Err(
            "the C profile executes only parameter-determined discrete equations; runtime, guarded, structured, or transactional discrete updates need event iteration",
        );
    }
    if discrete.row_roles.contains(&DiscreteRowRole::EventAction) {
        return Err("the C profile cannot execute event-edge discrete actions");
    }
    let pre = &problem.solve_layout.pre_param_bindings;
    if pre.iter().any(|binding| binding.clock_schedule.is_some()) {
        return Err("the C profile cannot execute clocked previous() history");
    }
    memory_reads::validate(model)?;
    let mut y = vec![false; problem.layout.y_scalars()];
    let mut p = super::parameter_updates::stable_parameters(problem);
    for binding in pre {
        *p.get_mut(binding.dest_p_index)
            .ok_or("a pre() binding names storage outside the parameters")? = false;
    }
    let order = discrete_order(model, &mut y, &mut p)?;
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
    Ok(order)
}

/// The two evaluation orders of the discrete rows. Parameter-determined
/// discrete equations settle with the parameter bindings, before any
/// algebraic refresh, so they read only parameters, constants, and equations
/// already settled. Condition memories settle after the refresh and may also
/// read algebraics the exact assignments determine from parameters. No row
/// reads a pre() value. Evaluating the rows once in these orders yields the
/// event iteration's fixed point bit for bit, because each row then reads the
/// final values of its inputs.
#[derive(Debug, Default)]
pub(in crate::fmi) struct DiscreteOrder {
    pub(in crate::fmi) equations: Vec<usize>,
    pub(in crate::fmi) memories: Vec<usize>,
}

fn discrete_order(
    model: &SolveModel,
    y: &mut [bool],
    p: &mut [bool],
) -> Result<DiscreteOrder, &'static str> {
    let discrete = &model.problem.discrete;
    let programs = discrete.rhs.programs();
    let (mut equations, mut memories) = (Vec::new(), Vec::new());
    let mut outputs = Vec::with_capacity(programs.len());
    let mut cursor = 0;
    for (index, program) in programs.iter().enumerate() {
        let count = crate::ScalarProgramBlock::program_output_count(program);
        let rows = discrete
            .rhs
            .output_indices()
            .get(cursor..cursor + count)
            .ok_or("a discrete row has no output index")?;
        let written = rows
            .iter()
            .map(|output| match discrete.update_targets.get(*output) {
                Some(ScalarSlot::P { index, .. }) => Some(*index),
                _ => None,
            })
            .collect::<Option<Vec<_>>>()
            .ok_or("a discrete row writes outside the parameters")?;
        let role = |output: &usize| discrete.row_roles.get(*output).copied();
        if rows
            .iter()
            .all(|row| role(row) == Some(DiscreteRowRole::Equation))
        {
            equations.push(index);
        } else if rows
            .iter()
            .all(|row| role(row) == Some(DiscreteRowRole::ConditionMemory))
        {
            memories.push(index);
        } else {
            return Err("a discrete program mixes equations and condition memories");
        }
        outputs.push(written);
        cursor += count;
    }
    let unsettled = vec![false; y.len()];
    let calls = &model.pure_calls;
    let equations = scalar_dependencies::dependency_order(calls, programs, &equations, &outputs, &unsettled, p)
        .map(|(order, _)| order)
        .map_err(|_| "a discrete equation depends on time, a state, an input, an algebraic, or a pre() value")?;
    static_algebraics(model, y, p)?;
    let memories = scalar_dependencies::dependency_order(calls, programs, &memories, &outputs, y, p)
        .map(|(order, _)| order)
        .map_err(|_| {
        "assertion depends on time, a continuous state, an input, or an unsupported dependence operation"
    })?;
    Ok(DiscreteOrder {
        equations,
        memories,
    })
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
            // A projected unknown is never proved static: it keeps the
            // time-dependent mark every coordinate starts with.
            RefreshStage::ProjectionBlock { .. } => continue,
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
