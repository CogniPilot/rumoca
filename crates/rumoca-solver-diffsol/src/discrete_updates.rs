use rumoca_eval_solve::{
    self as solve_eval, RowEvalContext, apply_discrete_slot_value, eval_event_actions_with_context,
};
use rumoca_ir_solve as solve;
use rumoca_solver::{
    EventActionOutcome, EventPreMode, discrete_row_pre_mode, event_eval_params_for_pre_mode,
    row_reads_solver_or_time,
};

use crate::{EVENT_UPDATE_MAX_ITERS, OdeModel, SimError, project_algebraics};

pub(crate) fn settle_algebraics_and_relation_memory(
    model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    state_count: usize,
    relation_memory_indices: &[usize],
    tol: f64,
) -> Result<(), SimError> {
    for _ in 0..EVENT_UPDATE_MAX_ITERS {
        let mut changed = apply_runtime_assignments(model, y, p, t, tol)?;
        project_algebraics(model, y, p, t, state_count, tol)?;
        changed |= apply_runtime_assignments(model, y, p, t, tol)?;
        changed |= crate::update_relation_memory_values(model, y, p, t, relation_memory_indices)?;
        if !changed {
            return Ok(());
        }
    }
    Ok(())
}

pub(crate) fn apply_event_updates(
    model: &solve::SolveModel,
    ode_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
    _pre_mode: EventPreMode,
) -> Result<(), SimError> {
    let event_pre_y = y.to_vec();
    let event_pre_p = p.to_vec();
    apply_event_updates_with_event_pre(EventUpdateInput {
        model,
        ode_model,
        y,
        p,
        t,
        tol,
        event_pre_y: &event_pre_y,
        event_pre_p: &event_pre_p,
    })
}

pub(crate) struct EventUpdateInput<'a> {
    pub(crate) model: &'a solve::SolveModel,
    pub(crate) ode_model: &'a OdeModel,
    pub(crate) y: &'a mut [f64],
    pub(crate) p: &'a mut [f64],
    pub(crate) t: f64,
    pub(crate) tol: f64,
    pub(crate) event_pre_y: &'a [f64],
    pub(crate) event_pre_p: &'a [f64],
}

pub(crate) fn apply_event_updates_with_event_pre(
    input: EventUpdateInput<'_>,
) -> Result<(), SimError> {
    apply_event_updates_with_filter(input, DiscreteRowFilter::All)
}

fn event_update_input_for_current_values<'a>(
    model: &'a solve::SolveModel,
    ode_model: &'a OdeModel,
    y: &'a mut [f64],
    p: &'a mut [f64],
    t: f64,
    tol: f64,
    event_pre: &'a EventPreBuffers,
) -> EventUpdateInput<'a> {
    EventUpdateInput {
        model,
        ode_model,
        y,
        p,
        t,
        tol,
        event_pre_y: &event_pre.y,
        event_pre_p: &event_pre.p,
    }
}

pub(crate) fn apply_post_initial_event_updates(
    model: &solve::SolveModel,
    ode_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), SimError> {
    let event_pre = EventPreBuffers::from_current(y, p);
    apply_event_updates_with_filter(
        event_update_input_for_current_values(model, ode_model, y, p, t, tol, &event_pre),
        DiscreteRowFilter::FollowCurrentOnly,
    )
}

struct EventPreBuffers {
    y: Vec<f64>,
    p: Vec<f64>,
}

impl EventPreBuffers {
    fn from_current(y: &[f64], p: &[f64]) -> Self {
        Self {
            y: y.to_vec(),
            p: p.to_vec(),
        }
    }
}

fn apply_event_updates_with_filter(
    input: EventUpdateInput<'_>,
    row_filter: DiscreteRowFilter,
) -> Result<(), SimError> {
    let EventUpdateInput {
        model,
        ode_model,
        y,
        p,
        t,
        tol,
        event_pre_y,
        event_pre_p,
    } = input;

    if model.problem.discrete.rhs.is_empty() {
        for _ in 0..EVENT_UPDATE_MAX_ITERS {
            let mut changed = apply_runtime_assignments(ode_model, y, p, t, tol)?;
            project_algebraics(ode_model, y, p, t, model.state_scalar_count(), tol)?;
            changed |= apply_runtime_assignments(ode_model, y, p, t, tol)?;
            if !changed {
                apply_event_actions(model, ode_model, y, p, t)?;
                return Ok(());
            }
        }
        return Err(SimError::SolveIr(format!(
            "event runtime assignments did not converge at t={t}"
        )));
    }
    if model.problem.discrete.rhs.len() != model.problem.discrete.update_targets.len() {
        return Err(SimError::SolveIr(format!(
            "discrete RHS row count {} does not match target count {}",
            model.problem.discrete.rhs.len(),
            model.problem.discrete.update_targets.len()
        )));
    }

    for _ in 0..EVENT_UPDATE_MAX_ITERS {
        let mut changed = apply_runtime_assignments(ode_model, y, p, t, tol)?;
        project_algebraics(ode_model, y, p, t, model.state_scalar_count(), tol)?;
        changed |= apply_runtime_assignments(ode_model, y, p, t, tol)?;
        let iter_pre_y = y.to_vec();
        let iter_pre_p = p.to_vec();
        let pre_snapshot = DiscretePreSnapshot {
            model,
            ode_model,
            state_count: model.state_scalar_count(),
            event_pre_y,
            event_pre_p,
            iter_pre_y: iter_pre_y.as_slice(),
            iter_pre_p: iter_pre_p.as_slice(),
            row_filter,
        };
        changed |= settle_discrete_rows_for_pre_snapshot(&pre_snapshot, y, p, t, tol)?;
        changed |= crate::update_relation_memory_values(
            ode_model,
            y,
            p,
            t,
            &model.problem.solve_layout.relation_memory_parameter_indices,
        )?;
        project_algebraics(ode_model, y, p, t, model.state_scalar_count(), tol)?;
        changed |= apply_runtime_assignments(ode_model, y, p, t, tol)?;
        if !changed {
            apply_event_actions(model, ode_model, y, p, t)?;
            return Ok(());
        }
    }
    Err(SimError::SolveIr(format!(
        "event update iteration did not converge at t={t}"
    )))
}

fn apply_event_actions(
    model: &solve::SolveModel,
    ode_model: &OdeModel,
    y: &[f64],
    p: &[f64],
    t: f64,
) -> Result<(), SimError> {
    let actions = &model.problem.events.actions;
    if actions.is_empty() {
        return Ok(());
    }
    match eval_event_actions_with_context(
        &model.problem.events,
        y,
        p,
        t,
        RowEvalContext {
            external_tables: Some(model.external_tables.as_slice()),
            runtime_state: Some(&ode_model.runtime_state),
            ..Default::default()
        },
    )
    .map_err(|err| SimError::SolveIr(err.to_string()))?
    {
        EventActionOutcome::Continue => Ok(()),
        EventActionOutcome::AssertionFailed { message, .. } => {
            Err(SimError::AssertionFailed { time: t, message })
        }
        EventActionOutcome::Terminated { message, .. } => {
            Err(SimError::Terminated { time: t, message })
        }
    }
}

#[derive(Clone, Copy)]
enum DiscreteRowFilter {
    All,
    FollowCurrentOnly,
}

impl DiscreteRowFilter {
    fn accepts(self, mode: EventPreMode) -> bool {
        matches!(self, Self::All)
            || matches!(
                (self, mode),
                (Self::FollowCurrentOnly, EventPreMode::FollowCurrent)
            )
    }
}

struct DiscretePreSnapshot<'a> {
    model: &'a solve::SolveModel,
    ode_model: &'a OdeModel,
    state_count: usize,
    event_pre_y: &'a [f64],
    event_pre_p: &'a [f64],
    iter_pre_y: &'a [f64],
    iter_pre_p: &'a [f64],
    row_filter: DiscreteRowFilter,
}

fn settle_discrete_rows_for_pre_snapshot(
    snapshot: &DiscretePreSnapshot<'_>,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<bool, SimError> {
    let mut changed_any = false;
    for _ in 0..EVENT_UPDATE_MAX_ITERS {
        let mut pass_changed = false;
        let eval_y = y.to_vec();
        let eval_p = p.to_vec();
        let mut fixed_eval_p = None;
        let mut follow_eval_p = None;
        let mut row_values = Vec::with_capacity(snapshot.model.problem.discrete.rhs.programs.len());
        for (row_idx, row) in snapshot
            .model
            .problem
            .discrete
            .rhs
            .programs
            .iter()
            .enumerate()
        {
            let row_pre_mode = discrete_row_pre_mode(snapshot.model, row_idx);
            if !snapshot.row_filter.accepts(row_pre_mode) {
                continue;
            }
            let row_p = match row_pre_mode {
                EventPreMode::Fixed => fixed_eval_p.get_or_insert_with(|| {
                    event_eval_params_for_pre_mode(
                        snapshot.model,
                        &eval_p,
                        snapshot.event_pre_y,
                        snapshot.event_pre_p,
                        tol,
                    )
                }),
                EventPreMode::FollowCurrent => follow_eval_p.get_or_insert_with(|| {
                    event_eval_params_for_pre_mode(
                        snapshot.model,
                        &eval_p,
                        snapshot.iter_pre_y,
                        snapshot.iter_pre_p,
                        tol,
                    )
                }),
            };
            let value = solve_eval::eval_row_with_context(
                row,
                &eval_y,
                row_p,
                t,
                RowEvalContext {
                    external_tables: Some(snapshot.model.external_tables.as_slice()),
                    runtime_state: Some(&snapshot.ode_model.runtime_state),
                    ..Default::default()
                },
            )
            .map_err(|err| SimError::SolveIr(err.to_string()))?;
            row_values.push((
                snapshot.model.problem.discrete.update_targets[row_idx],
                value,
            ));
        }
        for (target, value) in row_values {
            pass_changed |= apply_discrete_value(target, value, y, p, tol);
        }
        pass_changed |= apply_runtime_assignments(snapshot.ode_model, y, p, t, tol)?;
        pass_changed |= project_algebraics_and_detect_changes(
            snapshot.ode_model,
            y,
            p,
            t,
            snapshot.state_count,
            tol,
        )?;
        pass_changed |= apply_runtime_assignments(snapshot.ode_model, y, p, t, tol)?;
        if !pass_changed {
            return Ok(changed_any);
        }
        changed_any = true;
    }
    Err(SimError::SolveIr(format!(
        "discrete event equations did not converge at t={t}"
    )))
}

pub(crate) fn seed_initial_discrete_values(
    model: &solve::SolveModel,
    ode_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), SimError> {
    if model.problem.discrete.rhs.is_empty() {
        return Ok(());
    }
    if model.problem.discrete.rhs.len() != model.problem.discrete.update_targets.len() {
        return Err(SimError::SolveIr(format!(
            "discrete RHS row count {} does not match target count {}",
            model.problem.discrete.rhs.len(),
            model.problem.discrete.update_targets.len()
        )));
    }

    let event_pre_y = y.to_vec();
    let event_pre_p = p.to_vec();
    for _ in 0..EVENT_UPDATE_MAX_ITERS {
        let iter_pre_y = y.to_vec();
        let iter_pre_p = p.to_vec();
        let snapshot = DiscretePreSnapshot {
            model,
            ode_model,
            state_count: model.state_scalar_count(),
            event_pre_y: &event_pre_y,
            event_pre_p: &event_pre_p,
            iter_pre_y: iter_pre_y.as_slice(),
            iter_pre_p: iter_pre_p.as_slice(),
            row_filter: DiscreteRowFilter::All,
        };
        let changed = apply_discrete_rows_for_pre_snapshot(&snapshot, y, p, t, tol)?;
        if !changed {
            return Ok(());
        }
    }
    Err(SimError::SolveIr(format!(
        "initial discrete equations did not converge at t={t}"
    )))
}

pub(crate) fn apply_initialization_updates(
    model: &solve::SolveModel,
    ode_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<bool, SimError> {
    solve_eval::eval_and_apply_update_rows(solve_eval::UpdateRowApplication {
        block: &model.problem.initialization.update_rhs,
        targets: &model.problem.initialization.update_targets,
        y,
        p,
        t,
        context: RowEvalContext {
            external_tables: Some(model.external_tables.as_slice()),
            runtime_state: Some(&ode_model.runtime_state),
            ..Default::default()
        },
        tol,
        max_iters: EVENT_UPDATE_MAX_ITERS,
    })
    .map_err(|err| SimError::SolveIr(err.to_string()))
}

fn apply_discrete_rows_for_pre_snapshot(
    snapshot: &DiscretePreSnapshot<'_>,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<bool, SimError> {
    let eval_y = y.to_vec();
    let eval_p = p.to_vec();
    let mut fixed_eval_p = None;
    let mut follow_eval_p = None;
    let mut row_values = Vec::with_capacity(snapshot.model.problem.discrete.rhs.programs.len());
    for (row_idx, row) in snapshot
        .model
        .problem
        .discrete
        .rhs
        .programs
        .iter()
        .enumerate()
    {
        if row_reads_solver_or_time(row) {
            continue;
        }
        let row_pre_mode = discrete_row_pre_mode(snapshot.model, row_idx);
        let row_p = match row_pre_mode {
            EventPreMode::Fixed => fixed_eval_p.get_or_insert_with(|| {
                event_eval_params_for_pre_mode(
                    snapshot.model,
                    &eval_p,
                    snapshot.event_pre_y,
                    snapshot.event_pre_p,
                    tol,
                )
            }),
            EventPreMode::FollowCurrent => follow_eval_p.get_or_insert_with(|| {
                event_eval_params_for_pre_mode(
                    snapshot.model,
                    &eval_p,
                    snapshot.iter_pre_y,
                    snapshot.iter_pre_p,
                    tol,
                )
            }),
        };
        let value = solve_eval::eval_row_with_context(
            row,
            &eval_y,
            row_p,
            t,
            RowEvalContext {
                external_tables: Some(snapshot.model.external_tables.as_slice()),
                runtime_state: Some(&snapshot.ode_model.runtime_state),
                ..Default::default()
            },
        )
        .map_err(|err| SimError::SolveIr(err.to_string()))?;
        row_values.push((
            snapshot.model.problem.discrete.update_targets[row_idx],
            value,
        ));
    }
    let mut changed = false;
    for (target, value) in row_values {
        changed |= apply_discrete_value(target, value, y, p, tol);
    }
    Ok(changed)
}

fn project_algebraics_and_detect_changes(
    model: &OdeModel,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    state_count: usize,
    tol: f64,
) -> Result<bool, SimError> {
    let before = y.to_vec();
    project_algebraics(model, y, p, t, state_count, tol)?;
    Ok(before
        .iter()
        .zip(y.iter())
        .any(|(old, new)| (old - new).abs() > tol))
}

pub(crate) fn apply_runtime_assignments(
    model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<bool, SimError> {
    if model.runtime_assignment_rhs.is_empty() {
        return Ok(false);
    }
    let mut values = vec![0.0; model.runtime_assignment_rhs.len()];
    let mut changed_any = false;
    for _ in 0..EVENT_UPDATE_MAX_ITERS {
        solve_eval::eval_scalar_program_block_with_context(
            &model.runtime_assignment_rhs,
            y,
            p,
            t,
            RowEvalContext {
                external_tables: Some(model.external_tables.as_slice()),
                runtime_state: Some(&model.runtime_state),
                ..Default::default()
            },
            &mut values,
        )
        .map_err(|err| SimError::SolveIr(err.to_string()))?;
        let changed = apply_discrete_values(&model.runtime_assignment_targets, &values, y, p, tol);
        if !changed {
            return Ok(changed_any);
        }
        changed_any = true;
    }
    Ok(changed_any)
}

fn apply_discrete_values(
    targets: &[solve::ScalarSlot],
    values: &[f64],
    y: &mut [f64],
    p: &mut [f64],
    tol: f64,
) -> bool {
    let mut changed = false;
    for (target, value) in targets.iter().zip(values.iter().copied()) {
        changed |= apply_discrete_value(*target, value, y, p, tol);
    }
    changed
}

pub(crate) fn apply_discrete_value(
    target: solve::ScalarSlot,
    value: f64,
    y: &mut [f64],
    p: &mut [f64],
    tol: f64,
) -> bool {
    apply_discrete_slot_value(target, value, y, p, tol)
}
