//! Admission proof for state events in generated C (SPEC_0044 ME-EVENT-002).
//!
//! The scalar event profile executes Appendix B event iteration over scalar
//! discrete rows: root relations with relation memory, discrete equations and
//! condition memories that read `pre` values following the current pass, and
//! unscheduled assertions. Everything the component decides comes from Solve
//! IR facts: the searched roots and their indicator table, the relation-memory
//! targets and zero domains, the event iteration schedule, and the pre-binding
//! lanes of each iteration run. Runtime, post-commit, and unclocked guarded
//! assignments run as their row blocks; a guarded assignment under a periodic
//! clock runs on that clock's ticks. The static instants and the periodic
//! clock schedules are the component's time events. Dynamic time events,
//! delays, structured updates, and event transactions are refused here.

#[cfg(test)]
mod tests;

use serde::Serialize;

use crate::{
    DiscreteEventPreMode, DiscreteRowRole, PreParamSource, RootSearchPlan, RootSearchRole,
    RootZeroDomain, ScalarSlot, SolveEventActionKind, SolveModel,
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
    /// Per discrete output: whether a public observation recomputes it from
    /// the observed coordinate (an unread B.1c owner, SPEC_0022 EXPR-012).
    observation_rows: Vec<bool>,
    /// Whether an observation row reads a solver coordinate, so the algebraic
    /// refresh and the rows alternate until they agree.
    observation_reads_y: bool,
    /// Targets of the runtime assignments and of the post-commit
    /// assignments, in output order.
    runtime_targets: Vec<Slot>,
    /// Per guarded-assignment output, in program order: its target.
    guarded_targets: Vec<Slot>,
    /// Per guarded-assignment output: the periodic clock that owns it, which
    /// gates it to that clock's ticks and to the first pass of an event.
    guarded_clocks: Vec<Option<usize>>,
    /// The time events the component announces and stops at.
    time_events: super::time_events::TimeEvents,
    /// Source storage runs of discrete-valued external inputs. A
    /// discrete-time input changes only at events (MLS §4.5), so a
    /// change the importer makes between Co-Simulation steps is an event at
    /// the next step's first instant.
    discrete_input_runs: DiscreteInputRuns,
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
        // A clock-owned run advances its history on its tick when the event
        // commits and takes no part in the pass-to-pass fixed point.
        if discrete.event_iteration_run_clock(run)?.is_some() {
            continue;
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
        observation_rows: discrete.observation_refresh.clone(),
        observation_reads_y: discrete.observation_refresh_reads_y,
        runtime_targets: slots(&discrete.runtime_assignment_targets)?,
        guarded_targets: guarded_targets(discrete)?,
        guarded_clocks: guarded_clocks(
            discrete,
            model.problem.clocks.periodic_event_schedules.len(),
        )?,
        time_events: super::time_events::derive(model)?,
        discrete_input_runs: discrete_inputs(problem)?,
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
    if crate::solve_has_runtime_events(problem) {
        return Err("the C profile cannot execute delays or terminal events");
    }
    if discrete.clock_owners.iter().any(Option::is_some)
        || !discrete.clock_partition_intermediates.is_empty()
    {
        return Err("the C profile executes clocks only through guarded assignments");
    }
    if !events.scheduled_root_conditions.is_empty()
        || !events.dynamic_time_event_rhs.is_empty()
        || !events.dynamic_time_event_names.is_empty()
    {
        return Err("the C profile cannot execute dynamic or scheduled-root time events");
    }
    if events.actions.iter().any(|action| {
        !matches!(
            action.kind,
            SolveEventActionKind::Assert | SolveEventActionKind::Warning
        ) || action.clock_owner.is_some()
    }) {
        return Err("the C profile executes only unscheduled assertions");
    }
    if discrete
        .guarded_assignments
        .iter()
        .any(crate::GuardedAssignmentProgram::observation_refresh)
    {
        return Err("the C profile cannot observe a guarded assignment through a refresh");
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
/// program and range order.
fn guarded_targets(discrete: &crate::DiscreteSolveSystem) -> Result<Vec<Slot>, &'static str> {
    let mut targets = Vec::new();
    for program in &discrete.guarded_assignments {
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

/// The periodic clock of every guarded-assignment output, in the order of
/// [`guarded_targets`].
fn guarded_clocks(
    discrete: &crate::DiscreteSolveSystem,
    clock_count: usize,
) -> Result<Vec<Option<usize>>, &'static str> {
    let mut clocks = Vec::new();
    for program in &discrete.guarded_assignments {
        let clock = program.clock_owner().map(crate::PeriodicClockId::index);
        if clock.is_some_and(|clock| clock >= clock_count) {
            return Err("a guarded assignment names a clock outside the clock partition");
        }
        let outputs: usize = program
            .target_ranges()
            .iter()
            .map(|range| range.count())
            .sum();
        clocks.extend(std::iter::repeat_n(clock, outputs));
    }
    Ok(clocks)
}

/// A checked projection of the same problem's discrete input storage runs.
#[derive(Debug, Serialize)]
struct DiscreteInputRuns {
    runs: Vec<DiscreteInputRun>,
    scalar_count: usize,
}

#[derive(Debug, Serialize)]
struct DiscreteInputRun {
    p_base: usize,
    count: usize,
    seen_offset: usize,
}

/// Preserve storage order and repeated mappings; each owns its seen lanes.
fn discrete_inputs(problem: &crate::SolveProblem) -> Result<DiscreteInputRuns, &'static str> {
    let mut inputs = DiscreteInputRuns {
        runs: Vec::new(),
        scalar_count: 0,
    };
    for run in &problem.solve_layout.variable_storage_runs {
        let discrete = matches!(
            run.value_kind,
            crate::SolveVariableValueKind::Integer
                | crate::SolveVariableValueKind::Boolean
                | crate::SolveVariableValueKind::Enumeration
        );
        if run.role != crate::SolveVariableStorageRole::ExternalInput || !discrete {
            continue;
        }
        let ScalarSlot::P { index, .. } = run.base else {
            return Err("a discrete input lives outside the parameters");
        };
        let end = index
            .checked_add(run.scalar_count)
            .ok_or("a discrete input parameter range overflowed")?;
        if end > problem.layout.p_scalars() {
            return Err("a discrete input parameter range exceeds its storage");
        }
        let seen_end = inputs
            .scalar_count
            .checked_add(run.scalar_count)
            .ok_or("the discrete input history size overflowed")?;
        inputs.runs.push(DiscreteInputRun {
            p_base: index,
            count: run.scalar_count,
            seen_offset: inputs.scalar_count,
        });
        inputs.scalar_count = seen_end;
    }
    Ok(inputs)
}
