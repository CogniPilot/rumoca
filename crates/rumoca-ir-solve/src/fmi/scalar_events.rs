//! Admission proof for state events in generated C (SPEC_0044 ME-EVENT-002).
//!
//! The scalar event profile executes Appendix B event iteration over scalar
//! discrete rows: root relations with relation memory, discrete equations and
//! condition memories that read `pre` values following the current pass, and
//! unscheduled assertions. Everything the component decides comes from Solve
//! IR facts: the searched roots and their indicator table, the relation-memory
//! targets and zero domains, the event iteration schedule, and the pre-binding
//! lanes of each iteration run. Runtime, post-commit, and unclocked guarded
//! assignments run as their row blocks. Clocks, scheduled or dynamic time
//! events, delays, structured updates, and event transactions are refused
//! here.

use serde::Serialize;

use crate::{
    DiscreteEventPreMode, DiscreteRowRole, EventIterationOwner, PreParamSource, RootSearchPlan,
    RootSearchRole, RootZeroDomain, ScalarSlot, SolveEventActionKind, SolveModel,
};

/// The Solve IR facts the generated component reads to execute events.
#[derive(Debug, Serialize)]
pub(super) struct ScalarEventProfile {
    /// Per root output: whether and how it is evaluated in Event Mode.
    roots: Vec<ScalarEventRoot>,
    /// Destination and source of every pre binding, in binding order.
    pre_bindings: Vec<PreLane>,
    /// The pre bindings (indices into `pre_bindings`) an event pass advances
    /// and whose settlement ends the iteration.
    iteration_lanes: Vec<usize>,
    /// Per discrete output: its parameter target.
    discrete_targets: Vec<usize>,
    /// Per discrete output: whether it is a condition memory, the rows
    /// initialization seeds with `pre` following the current value.
    condition_memory_rows: Vec<bool>,
    /// Targets of the runtime assignments and of the post-commit
    /// assignments, in output order.
    runtime_targets: Vec<Slot>,
    /// Per guarded-assignment output, in program order: its target.
    guarded_targets: Vec<Slot>,
    post_commit_targets: Vec<Slot>,
    /// The parameters actions read at their event-entry value.
    condition_memories: Vec<usize>,
    /// Per root output: the outputs reading a coordinate it reads, itself
    /// included (ME-EVENT-008 mode search).
    root_neighborhoods: Vec<Vec<usize>>,
    /// The event iteration schedule the component walks (ME-EVENT-006).
    schedule: crate::EventIterationSchedule,
}

#[derive(Debug, Serialize)]
struct ScalarEventRoot {
    /// `search`, `announced_time`, or `static`.
    role: &'static str,
    /// The relation-memory parameter this root writes, if any.
    memory: Option<usize>,
    /// `positive`, `non_positive`, or `previous`.
    zero: &'static str,
    /// Whether the relation memory is refreshed from the algebraic
    /// coordinate after the event commit.
    algebraic_dependent: bool,
}

#[derive(Debug, Serialize)]
struct Slot {
    /// `y` or `p`.
    column: &'static str,
    index: usize,
}

fn slots(targets: &[ScalarSlot]) -> Result<Vec<Slot>, &'static str> {
    targets
        .iter()
        .map(|slot| match slot {
            ScalarSlot::Y { index, .. } => Ok(Slot {
                column: "y",
                index: *index,
            }),
            ScalarSlot::P { index, .. } => Ok(Slot {
                column: "p",
                index: *index,
            }),
            _ => Err("an assignment writes neither a solver coordinate nor a parameter"),
        })
        .collect()
}

#[derive(Debug, Serialize)]
struct PreLane {
    dest: usize,
    /// `y` or `p`.
    column: &'static str,
    source: usize,
}

/// Admit the scalar event profile, or say why the model needs more.
pub(super) fn validate(model: &SolveModel) -> Result<ScalarEventProfile, &'static str> {
    let problem = &model.problem;
    refuse_unsupported_owners(model)?;
    let events = &problem.events;
    let discrete = &problem.discrete;
    let search = RootSearchPlan::derive(problem);
    let count = events.root_conditions.output_count();
    if search.len() != count
        || events.root_zero_domains.len() != count
        || events.root_relation_memory_targets.len() != count
        || events.root_relation_refresh_roles.len() != count
    {
        return Err("the root tables disagree on the root count");
    }
    let roots = (0..count)
        .map(|index| scalar_event_root(problem, &search, index))
        .collect::<Result<Vec<_>, _>>()?;
    let discrete_targets = discrete
        .update_targets
        .iter()
        .map(|slot| match slot {
            ScalarSlot::P { index, .. } => Ok(*index),
            _ => Err("a discrete row writes outside the parameters"),
        })
        .collect::<Result<Vec<_>, _>>()?;
    if discrete.row_roles.iter().any(|role| {
        !matches!(
            role,
            DiscreteRowRole::Equation | DiscreteRowRole::ConditionMemory
        )
    }) {
        return Err("the C profile executes only discrete equations and condition memories");
    }
    if discrete
        .pre_modes
        .iter()
        .any(|mode| *mode != DiscreteEventPreMode::FollowCurrent)
    {
        return Err("the C profile executes only discrete rows whose pre follows the current pass");
    }
    let layout = &problem.solve_layout;
    let pre_bindings = layout
        .pre_param_bindings
        .iter()
        .map(|binding| match binding.source {
            PreParamSource::Y { index } => PreLane {
                dest: binding.dest_p_index,
                column: "y",
                source: index,
            },
            PreParamSource::P { index } => PreLane {
                dest: binding.dest_p_index,
                column: "p",
                source: index,
            },
        })
        .collect::<Vec<_>>();
    let mut iteration_lanes = Vec::new();
    for run in &discrete.event_iteration_plan.runs {
        if !matches!(run.owner, EventIterationOwner::ScalarRows { .. }) {
            return Err("the C profile iterates only scalar discrete rows");
        }
        let storage = layout
            .variable_storage_runs
            .get(run.variable)
            .ok_or("an event iteration run names no storage")?;
        let lanes = run.pre_binding_start..run.pre_binding_start + storage.scalar_count;
        if lanes.end > pre_bindings.len() {
            return Err("an event iteration run names a pre binding outside the layout");
        }
        iteration_lanes.extend(lanes);
    }
    Ok(ScalarEventProfile {
        roots,
        pre_bindings,
        iteration_lanes,
        condition_memory_rows: discrete
            .row_roles
            .iter()
            .map(|role| *role == DiscreteRowRole::ConditionMemory)
            .collect(),
        discrete_targets,
        runtime_targets: slots(&discrete.runtime_assignment_targets)?,
        guarded_targets: guarded_targets(discrete)?,
        post_commit_targets: slots(&discrete.post_commit_assignment_targets)?,
        condition_memories: events.condition_memory_parameter_indices.clone(),
        schedule: discrete.event_iteration_plan.schedule.clone(),
        root_neighborhoods: crate::root_neighborhoods(&events.root_conditions)
            .map_err(|_| "a root condition's coordinate reads cannot be derived")?,
    })
}

fn refuse_unsupported_owners(model: &SolveModel) -> Result<(), &'static str> {
    let problem = &model.problem;
    let events = &problem.events;
    let discrete = &problem.discrete;
    if crate::solve_has_runtime_events(problem) || crate::solve_has_clocks(problem) {
        return Err("the C profile cannot execute delays, terminal events, or clocks");
    }
    if !events.scheduled_root_conditions.is_empty()
        || !events.scheduled_time_events.is_empty()
        || !events.dynamic_time_event_rhs.is_empty()
    {
        return Err("the C profile cannot execute time events");
    }
    if events.actions.iter().any(|action| {
        !matches!(
            action.kind,
            SolveEventActionKind::Assert | SolveEventActionKind::Warning
        ) || action.clock_owner.is_some()
    }) {
        return Err("the C profile executes only unscheduled assertions");
    }
    if !discrete.structured_rhs.is_empty()
        || !discrete.structured_updates.is_empty()
        || !discrete.event_transactions.is_empty()
    {
        return Err("the C profile cannot execute structured or transactional discrete updates");
    }
    if problem
        .solve_layout
        .pre_param_bindings
        .iter()
        .any(|binding| binding.clock_schedule.is_some())
    {
        return Err("the C profile cannot execute clocked previous() history");
    }
    if problem.solve_layout.initial_event_parameter_index.is_some() {
        return Err("the C profile cannot execute an initial event yet");
    }
    Ok(())
}

fn scalar_event_root(
    problem: &crate::SolveProblem,
    search: &RootSearchPlan,
    index: usize,
) -> Result<ScalarEventRoot, &'static str> {
    let events = &problem.events;
    let role = match search.roles()[index] {
        RootSearchRole::Search => "search",
        RootSearchRole::AnnouncedTime { .. } => "announced_time",
        RootSearchRole::Static => "static",
    };
    let memory = match events.root_relation_memory_targets[index] {
        None => None,
        Some(ScalarSlot::P { index, .. }) => Some(index),
        Some(_) => return Err("a relation memory lives outside the parameters"),
    };
    let zero = match events.root_zero_domains[index] {
        RootZeroDomain::Positive => "positive",
        RootZeroDomain::NonPositive => "non_positive",
        RootZeroDomain::Previous => "previous",
    };
    Ok(ScalarEventRoot {
        role,
        memory,
        zero,
        algebraic_dependent: events.root_relation_refresh_roles[index]
            == crate::RootRelationRefreshRole::AlgebraicDependent,
    })
}

/// The guarded assignments' targets, expanded from their compact ranges in
/// program and range order; an owner under a clock is refused.
fn guarded_targets(discrete: &crate::DiscreteSolveSystem) -> Result<Vec<Slot>, &'static str> {
    let mut targets = Vec::new();
    for program in &discrete.guarded_assignments {
        if program.clock_owner().is_some() {
            return Err("the C profile cannot execute clocked guarded assignments");
        }
        for range in program.target_ranges() {
            let (column, base) = match range.base() {
                ScalarSlot::Y { index, .. } => ("y", index),
                ScalarSlot::P { index, .. } => ("p", index),
                _ => {
                    return Err(
                        "a guarded assignment writes neither a solver coordinate nor a parameter",
                    );
                }
            };
            targets.extend((0..range.count()).map(|offset| Slot {
                column,
                index: base + offset,
            }));
        }
    }
    Ok(targets)
}
