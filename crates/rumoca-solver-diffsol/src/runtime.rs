use super::*;
use rumoca_eval_solve::{
    EventUpdateRowFilter, ProjectedEventUpdateInput, apply_discrete_slot_value,
};
use rumoca_solver::{
    EventActionOutcome, EventPreMode, NoStateEventStep, NoStateOrchestrationBackend,
    NoStateScheduledStop, RuntimeSolveError, build_sim_result_from_solve_model,
    project_algebraics_and_detect_changes, run_no_state_output_schedule,
};

pub(crate) fn settle_algebraics_and_relation_memory(
    runtime: &SolveRuntime,
    model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    state_count: usize,
    tol: f64,
) -> Result<(), SimError> {
    runtime
        .settle_projected_runtime_and_relation_memory(
            y,
            p,
            t,
            tol,
            EVENT_UPDATE_MAX_ITERS,
            move |y, p| project_algebraics_and_detect_changes(model, y, p, t, state_count, tol),
        )
        .map_err(Into::into)
}

pub(crate) fn apply_event_updates(
    runtime: &SolveRuntime,
    ode_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), SimError> {
    let event_pre_y = y.to_vec();
    let event_pre_p = p.to_vec();
    apply_event_updates_with_event_pre(EventUpdateInput {
        runtime,
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
    pub(crate) runtime: &'a SolveRuntime,
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
    apply_event_updates_with_filter(input, EventUpdateRowFilter::All)
}

pub(crate) fn apply_post_initial_event_updates(
    runtime: &SolveRuntime,
    ode_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), SimError> {
    let outcome = runtime.apply_projected_post_initial_event_update(
        y,
        p,
        t,
        tol,
        EVENT_UPDATE_MAX_ITERS,
        project_algebraics_callback(ode_model, t, tol),
    )?;
    event_action_outcome_to_result(outcome, t)
}

fn apply_event_updates_with_filter(
    input: EventUpdateInput<'_>,
    row_filter: EventUpdateRowFilter,
) -> Result<(), SimError> {
    let EventUpdateInput {
        runtime,
        ode_model,
        y,
        p,
        t,
        tol,
        event_pre_y,
        event_pre_p,
    } = input;
    let outcome = runtime.apply_projected_event_update(
        ProjectedEventUpdateInput {
            y,
            p,
            t,
            tol,
            event_pre_y,
            event_pre_p,
            max_iters: EVENT_UPDATE_MAX_ITERS,
            row_filter,
            root_relation_overrides: &[],
        },
        project_algebraics_callback(ode_model, t, tol),
    )?;
    event_action_outcome_to_result(outcome, t)
}

pub(crate) fn seed_initial_discrete_values(
    runtime: &SolveRuntime,
    _ode_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), SimError> {
    runtime.seed_initial_discrete_values(y, p, t, tol, EVENT_UPDATE_MAX_ITERS)?;
    Ok(())
}

pub(crate) fn apply_initialization_updates(
    runtime: &SolveRuntime,
    _ode_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<bool, SimError> {
    runtime
        .apply_initialization_updates(y, p, t, tol, EVENT_UPDATE_MAX_ITERS)
        .map_err(Into::into)
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

fn project_algebraics_callback(
    model: &OdeModel,
    t: f64,
    tol: f64,
) -> impl FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError> + '_ {
    move |y, p| {
        project_algebraics_and_detect_changes(
            model,
            y,
            p,
            t,
            model.state_count_for_projection(),
            tol,
        )
    }
}

fn event_action_outcome_to_result(
    outcome: EventActionOutcome,
    event_t: f64,
) -> Result<(), SimError> {
    match outcome {
        EventActionOutcome::Continue => Ok(()),
        EventActionOutcome::AssertionFailed { message, .. } => Err(SimError::AssertionFailed {
            time: event_t,
            message,
        }),
        EventActionOutcome::Terminated { message, .. } => Err(SimError::Terminated {
            time: event_t,
            message,
        }),
    }
}

pub(crate) fn simulate_no_state_solve_ir(
    model: &solve::SolveModel,
    opts: &SimOptions,
) -> Result<SimResult, SimError> {
    let dt = opts.dt.unwrap_or((opts.t_end - opts.t_start).abs() / 500.0);
    let times = rumoca_solver::timeline::build_output_times(opts.t_start, opts.t_end, dt);
    let mut runtime = initialize_no_state_runtime(model, opts, times.len())?;
    let tol = opts.atol.max(1.0e-10);

    run_no_state_output_schedule(
        &mut DiffsolNoStateOrchestration {
            model,
            opts,
            runtime: &mut runtime,
        },
        times,
        tol,
    )?;

    Ok(build_sim_result_from_solve_model(
        model,
        runtime.recorded_times,
        runtime.data,
        None,
    ))
}

pub(crate) fn check_no_state_initialization(
    model: &solve::SolveModel,
    opts: &SimOptions,
) -> Result<(), SimError> {
    initialize_no_state_runtime(model, opts, 1).map(|_| ())
}

struct DiffsolNoStateOrchestration<'a> {
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    runtime: &'a mut NoStateRuntime,
}

impl NoStateOrchestrationBackend for DiffsolNoStateOrchestration<'_> {
    type Error = SimError;

    fn current_time(&self) -> f64 {
        self.runtime.current_t
    }

    fn set_current_time(&mut self, time: f64) {
        self.runtime.current_t = time;
    }

    fn next_scheduled_stop(&mut self, target: f64) -> Result<NoStateScheduledStop, Self::Error> {
        let (stop_time, event_stop) = next_runtime_event_stop(
            self.model,
            &self.runtime.equilibrium_model.runtime_state,
            &self.runtime.current_y,
            &self.runtime.params,
            &mut self.runtime.stop_schedule,
            self.runtime.current_t,
            target,
        )?;
        Ok(NoStateScheduledStop {
            stop_time,
            event_stop,
        })
    }

    fn next_root_event_time(&mut self, target: f64, tol: f64) -> Result<Option<f64>, Self::Error> {
        next_no_state_root_event_time(
            &self.runtime.equilibrium_model,
            &self.runtime.current_y,
            &self.runtime.params,
            self.runtime.current_t,
            target,
            tol,
        )
    }

    fn handle_event_step(&mut self, step: NoStateEventStep) -> Result<(), Self::Error> {
        apply_no_state_event_step(self.model, self.opts, self.runtime, step)
    }

    fn settle_and_record_output(&mut self) -> Result<(), Self::Error> {
        settle_and_record_no_state_output(self.model, self.opts, self.runtime)
    }
}

fn apply_no_state_event_step(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &mut NoStateRuntime,
    step: NoStateEventStep,
) -> Result<(), SimError> {
    runtime.current_t = if step.root_event {
        root_event_application_time(step.event_time(), step.target)
    } else {
        step.event_time()
    };
    let pre_mode = step.pre_mode();
    if let Some(event) = step.event_stop
        && !step.root_event
    {
        prepare_fixed_event_left_limit(
            model,
            &runtime.equilibrium_model,
            &mut runtime.current_y,
            &mut runtime.params,
            runtime.current_t,
            step.tol,
            event,
        )?;
    }
    let event_pre_y = runtime.current_y.clone();
    let event_pre_p = runtime.params.clone();
    apply_event_updates(
        &runtime.runtime,
        &runtime.equilibrium_model,
        &mut runtime.current_y,
        &mut runtime.params,
        runtime.current_t,
        step.tol,
    )?;
    let event_tol = step.tol;
    record_no_state_event_step(
        model,
        opts,
        runtime,
        step,
        pre_mode,
        &event_pre_y,
        &event_pre_p,
    )?;
    crate::commit_pre_params_after_event(model, &runtime.current_y, &mut runtime.params, event_tol);
    runtime.stop_schedule.advance_past(runtime.current_t);
    Ok(())
}

fn record_no_state_event_step(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &mut NoStateRuntime,
    step: NoStateEventStep,
    pre_mode: EventPreMode,
    event_pre_y: &[f64],
    event_pre_p: &[f64],
) -> Result<(), SimError> {
    if step.root_event {
        refresh_observation_discrete_rows(
            model,
            &runtime.equilibrium_model.runtime_state,
            &mut runtime.current_y,
            &mut runtime.params,
            runtime.current_t,
            step.tol,
        )?;
        record_sample_if_new(
            None,
            model,
            &runtime.current_y,
            &runtime.params,
            &mut runtime.recorded_times,
            &mut runtime.data,
            runtime.current_t,
        )?;
        runtime.current_t = runtime_root_event_application_time(runtime.current_t, step.target);
        return Ok(());
    }
    let event = step.event_stop.unwrap_or(RuntimeEventStop {
        pre_mode,
        observe_right_limit: false,
    });
    runtime.current_t = EventObservation {
        runtime: &runtime.runtime,
        model,
        equilibrium_model: &runtime.equilibrium_model,
        y: &mut runtime.current_y,
        params: &mut runtime.params,
        tol: step.tol,
        recorded_times: &mut runtime.recorded_times,
        data: &mut runtime.data,
        event_pre_y,
        event_pre_p,
    }
    .record_time_event(
        runtime.current_t,
        runtime_event_horizon(event, step.target, opts.t_end),
        event,
    )?;
    Ok(())
}

struct NoStateRuntime {
    runtime: SolveRuntime,
    params: Vec<f64>,
    current_y: Vec<f64>,
    current_t: f64,
    data: Vec<Vec<f64>>,
    recorded_times: Vec<f64>,
    equilibrium_model: OdeModel,
    stop_schedule: SolveStopSchedule,
}

fn settle_and_record_no_state_output(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &mut NoStateRuntime,
) -> Result<(), SimError> {
    settle_algebraics_and_relation_memory(
        &runtime.runtime,
        &runtime.equilibrium_model,
        &mut runtime.current_y,
        &mut runtime.params,
        runtime.current_t,
        0,
        opts.atol.max(1.0e-10),
    )?;
    refresh_observation_discrete_rows(
        model,
        &runtime.equilibrium_model.runtime_state,
        &mut runtime.current_y,
        &mut runtime.params,
        runtime.current_t,
        opts.atol.max(1.0e-10),
    )?;
    record_sample_if_new(
        None,
        model,
        &runtime.current_y,
        &runtime.params,
        &mut runtime.recorded_times,
        &mut runtime.data,
        runtime.current_t,
    )
}

fn initialize_no_state_runtime(
    model: &solve::SolveModel,
    opts: &SimOptions,
    output_count: usize,
) -> Result<NoStateRuntime, SimError> {
    let mut params = model.parameters.clone();
    let mut current_y = model.initial_y.clone();
    let current_t = opts.t_start;
    let tol = opts.atol.max(1.0e-10);
    let runtime = SolveRuntime::new(model);
    let equilibrium_model = OdeModel::new(model)?;
    set_initial_event_flag(model, &mut params, true);
    settle_algebraics_and_relation_memory(
        &runtime,
        &equilibrium_model,
        &mut current_y,
        &mut params,
        current_t,
        0,
        tol,
    )?;
    let initial_event = initial_runtime_event_stop(&model.problem, current_t, None);
    apply_event_updates(
        &runtime,
        &equilibrium_model,
        &mut current_y,
        &mut params,
        current_t,
        tol,
    )?;
    set_initial_event_flag(model, &mut params, false);
    if initial_event.is_some() {
        apply_post_initial_event_updates(
            &runtime,
            &equilibrium_model,
            &mut current_y,
            &mut params,
            current_t,
            tol,
        )?;
    }
    commit_pre_params_after_event(model, &current_y, &mut params, tol);
    Ok(NoStateRuntime {
        runtime,
        params,
        current_y,
        current_t,
        data: vec![Vec::with_capacity(output_count); model.visible_names.len()],
        recorded_times: Vec::with_capacity(output_count),
        equilibrium_model,
        stop_schedule: SolveStopSchedule::new(&model.problem, opts.t_start, opts.t_end),
    })
}

fn next_no_state_root_event_time(
    model: &OdeModel,
    y: &[f64],
    p: &[f64],
    current_t: f64,
    target: f64,
    tol: f64,
) -> Result<Option<f64>, SimError> {
    let Some(root_time) = first_root_crossing_time(model, y, p, current_t, target, tol)? else {
        return Ok(None);
    };
    if root_time > current_t + tol
        && (root_time < target || sample_time_match_with_tol(root_time, target))
    {
        Ok(Some(root_time))
    } else {
        Ok(None)
    }
}

fn root_event_application_time(root_time: f64, target: f64) -> f64 {
    runtime_root_event_application_time(root_time, target)
}

pub(crate) fn prepare_fixed_event_left_limit(
    model: &solve::SolveModel,
    equilibrium_model: &OdeModel,
    y: &mut [f64],
    params: &mut [f64],
    event_t: f64,
    tol: f64,
    event: RuntimeEventStop,
) -> Result<(), SimError> {
    if !matches!(
        event.pre_mode,
        EventPreMode::EventEntry | EventPreMode::Fixed
    ) {
        return Ok(());
    }
    let left_t = event_left_limit_time(event_t);
    refresh_observation_discrete_rows(
        model,
        &equilibrium_model.runtime_state,
        y,
        params,
        left_t,
        tol,
    )?;
    project_algebraics(
        equilibrium_model,
        y,
        params,
        left_t,
        model.state_scalar_count(),
        tol,
    )?;
    Ok(())
}

fn event_left_limit_time(t: f64) -> f64 {
    t - (1.0e-6 * (1.0 + t.abs())).max(f64::EPSILON * (1.0 + t.abs()))
}

const ROOT_BISECTION_ITERS: usize = 64;

fn first_root_crossing_time(
    model: &OdeModel,
    y: &[f64],
    p: &[f64],
    t_start: f64,
    t_end: f64,
    tol: f64,
) -> Result<Option<f64>, SimError> {
    let mut start = vec![0.0; model.root_conditions.len()];
    let mut end = vec![0.0; model.root_conditions.len()];
    model.eval_roots(y, p, t_start, &mut start)?;
    model.eval_roots(y, p, t_end, &mut end)?;

    let mut crossing = None;
    for (a, b) in start.iter().zip(end.iter()) {
        if root_surface_crossed_or_near(*a, *b, tol) {
            let root = bisect_first_root(model, y, p, t_start, t_end, tol)?;
            crossing = Some(crossing.map_or(root, |current| f64::min(current, root)));
        }
    }
    Ok(crossing)
}

fn root_surface_crossed_or_near(a: f64, b: f64, tol: f64) -> bool {
    root_surface_near_zero(a, tol) || root_surface_near_zero(b, tol) || a.signum() != b.signum()
}

fn root_surface_near_zero(value: f64, tol: f64) -> bool {
    value.abs() <= tol
}

fn bisect_first_root(
    model: &OdeModel,
    y: &[f64],
    p: &[f64],
    mut lo: f64,
    mut hi: f64,
    tol: f64,
) -> Result<f64, SimError> {
    let mut lo_roots = vec![0.0; model.root_conditions.len()];
    model.eval_roots(y, p, lo, &mut lo_roots)?;
    for _ in 0..ROOT_BISECTION_ITERS {
        let mid = lo + 0.5 * (hi - lo);
        let mut mid_roots = vec![0.0; model.root_conditions.len()];
        model.eval_roots(y, p, mid, &mut mid_roots)?;
        if lo_roots
            .iter()
            .zip(mid_roots.iter())
            .any(|(a, b)| a.signum() != b.signum() || root_surface_near_zero(*b, tol))
        {
            hi = mid;
        } else {
            lo = mid;
            lo_roots = mid_roots;
        }
    }
    Ok(hi)
}
