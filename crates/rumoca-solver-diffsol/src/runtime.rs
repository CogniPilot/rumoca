use super::*;
use rumoca_solver::{
    EventActionOutcome, EventPreMode, EventUpdateRowFilter, NoStateEventStep,
    NoStateOrchestrationBackend, NoStateRootSearchScratch, NoStateScheduledStop,
    ProjectedEventUpdateInput, RuntimeSolveError, apply_discrete_slot_values,
    build_sim_result_from_solve_model, first_no_state_root_crossing,
    no_state_root_scan_step_ceiling, project_algebraics_and_detect_changes,
    run_no_state_output_schedule, timeline::event_left_probe_time,
};

pub(crate) fn settle_algebraics_and_relation_memory(
    runtime: &SolveRuntime,
    _model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    _state_count: usize,
    tol: f64,
) -> Result<(), SimError> {
    runtime
        .settle_projected_runtime_and_relation_memory(
            y,
            p,
            t,
            tol,
            EVENT_UPDATE_MAX_ITERS,
            move |y, p| refresh_algebraics_and_detect_changes(runtime, y, p, t, tol),
        )
        .map_err(Into::into)
}

pub(crate) fn refresh_algebraics_and_detect_changes(
    runtime: &SolveRuntime,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<bool, RuntimeSolveError> {
    let before = y.to_vec();
    runtime.refresh_delay_values(t, y, p)?;
    runtime.project_state_manifold(y, p, t, tol)?;
    runtime.refresh_algebraic_and_output_slots(t, y, p, tol, EVENT_UPDATE_MAX_ITERS)?;
    Ok(runtime_values_changed(&before, y, tol))
}

pub(crate) fn apply_event_updates(
    runtime: &SolveRuntime,
    _ode_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), SimError> {
    let event_pre_y = y.to_vec();
    let event_pre_p = p.to_vec();
    apply_event_updates_with_event_pre(EventUpdateInput {
        runtime,
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

fn apply_event_updates_with_filter(
    input: EventUpdateInput<'_>,
    row_filter: EventUpdateRowFilter,
) -> Result<(), SimError> {
    let EventUpdateInput {
        runtime,
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
        project_algebraics_callback(runtime, t, tol),
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

fn project_algebraics_callback(
    runtime: &SolveRuntime,
    t: f64,
    tol: f64,
) -> impl FnMut(&mut [f64], &mut [f64]) -> Result<bool, RuntimeSolveError> + '_ {
    move |y, p| refresh_algebraics_and_detect_changes(runtime, y, p, t, tol)
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
    let times = rumoca_solver::timeline::try_build_output_times(opts.t_start, opts.t_end, dt)
        .map_err(|error| SimError::SolverError(error.to_string()))?;
    let mut runtime = initialize_no_state_runtime(model, opts, times.len(), true)?;
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
    initialize_no_state_runtime(model, opts, 1, true).map(|_| ())
}

pub(crate) fn advance_no_state_runtime_to(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &mut NoStateRuntime,
    target: f64,
    tol: f64,
) -> Result<(), SimError> {
    run_no_state_output_schedule(
        &mut DiffsolNoStateOrchestration {
            model,
            opts,
            runtime,
        },
        [target],
        tol,
    )
}

pub(crate) fn apply_no_state_deadline_tick(
    model: &solve::SolveModel,
    runtime: &mut NoStateRuntime,
    target: f64,
    tol: f64,
) -> Result<(), SimError> {
    runtime.current_t = target;
    let values = runtime.runtime.eval_scalar_program_block(
        &model.problem.discrete.rhs,
        &runtime.current_y,
        &runtime.params,
        target,
    )?;
    apply_discrete_slot_values(
        &model.problem.discrete.update_targets,
        &values,
        &mut runtime.current_y,
        &mut runtime.params,
        tol,
    )?;
    runtime.runtime.apply_runtime_assignments_once(
        &mut runtime.current_y,
        &mut runtime.params,
        target,
    )?;
    settle_algebraics_and_relation_memory(
        &runtime.runtime,
        &runtime.equilibrium_model,
        &mut runtime.current_y,
        &mut runtime.params,
        target,
        0,
        tol,
    )?;
    crate::commit_pre_params_after_event_at(
        model,
        &runtime.current_y,
        &mut runtime.params,
        Some(target),
        tol,
    );
    runtime
        .runtime
        .commit_delay_history(target, &runtime.current_y, &runtime.params)?;
    Ok(())
}

pub(crate) struct DiffsolNoStateOrchestration<'a> {
    pub(crate) model: &'a solve::SolveModel,
    pub(crate) opts: &'a SimOptions,
    pub(crate) runtime: &'a mut NoStateRuntime,
}

impl NoStateOrchestrationBackend for DiffsolNoStateOrchestration<'_> {
    type Error = SimError;

    fn current_time(&self) -> f64 {
        self.runtime.current_t
    }

    fn set_current_time(&mut self, time: f64) {
        self.runtime.current_t = time;
    }

    fn max_accepted_step_size(&self) -> Option<f64> {
        no_state_root_scan_step_ceiling(self.opts.dt, self.runtime.runtime.delay_step_limit())
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
            NoStateRootEvaluation {
                runtime: &self.runtime.runtime,
                y: &self.runtime.current_y,
                p: &self.runtime.params,
                params_scratch: &mut self.runtime.root_params_scratch,
                root_scratch: &mut self.runtime.root_search_scratch,
            },
            self.runtime.current_t,
            target,
            tol,
        )
    }

    fn handle_event_step(&mut self, step: NoStateEventStep) -> Result<(), Self::Error> {
        apply_no_state_event_step(self.model, self.opts, self.runtime, step)
    }

    fn settle_accepted_step(&mut self) -> Result<(), Self::Error> {
        settle_no_state_accepted_step(self.model, self.opts, self.runtime)
    }

    fn record_output(&mut self) -> Result<(), Self::Error> {
        record_no_state_output(self.model, self.runtime)
    }
}

fn apply_no_state_event_step(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &mut NoStateRuntime,
    step: NoStateEventStep,
) -> Result<(), SimError> {
    let event_t = step.event_time();
    runtime.last_event_t = Some(event_t);
    runtime.current_t = event_t;
    let pre_mode = step.pre_mode();
    if step.event_stop.is_some_and(|event| event.terminal)
        && let Some(index) = model.problem.solve_layout.terminal_event_parameter_index
        && let Some(slot) = runtime.params.get_mut(index)
    {
        *slot = 1.0;
    }
    let prepared_left_limit = if let Some(event) = step.event_stop
        && !step.root_event
        && matches!(
            event.pre_mode,
            EventPreMode::EventEntry | EventPreMode::Fixed
        ) {
        prepare_fixed_event_left_limit(FixedEventLeftLimitInput {
            model,
            runtime: &runtime.runtime,
            equilibrium_model: &runtime.equilibrium_model,
            y: &mut runtime.current_y,
            params: &mut runtime.params,
            event_t: runtime.current_t,
            tol: step.tol,
            event,
        })?;
        true
    } else {
        false
    };
    if !prepared_left_limit {
        refresh_observation_rows_and_relation_memory(
            model,
            &runtime.runtime,
            &runtime.equilibrium_model,
            &mut runtime.current_y,
            &mut runtime.params,
            event_t,
            step.tol,
        )?;
    }
    // Preserve the accepted left limit before any event update changes the
    // source. The post-event commit below then creates a same-time pair, which
    // is how DelayRuntime represents a continuous source discontinuity.
    if prepared_left_limit {
        runtime.runtime.commit_delay_history_evaluated_at(
            event_t,
            event_left_probe_time(event_t, step.tol),
            &runtime.current_y,
            &runtime.params,
        )?;
    } else {
        runtime
            .runtime
            .commit_delay_history(event_t, &runtime.current_y, &runtime.params)?;
    }
    let event_pre_y = runtime.current_y.clone();
    let event_pre_p = runtime.params.clone();
    runtime.current_t = step
        .root_boundary()
        .map_or(event_t, |boundary| boundary.evaluation_time);
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
    crate::commit_pre_params_after_event_at(
        model,
        &runtime.current_y,
        &mut runtime.params,
        Some(event_t),
        event_tol,
    );
    runtime
        .runtime
        .commit_delay_history(event_t, &runtime.current_y, &runtime.params)?;
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
        let boundary = step
            .root_boundary()
            .expect("root event step constructs one root boundary");
        refresh_observation_rows_and_relation_memory(
            model,
            &runtime.runtime,
            &runtime.equilibrium_model,
            &mut runtime.current_y,
            &mut runtime.params,
            runtime.current_t,
            step.tol,
        )?;
        let mut samples = SampleRecorder {
            runtime: Some(&runtime.runtime),
            model,
            recorded_times: &mut runtime.recorded_times,
            data: &mut runtime.data,
        };
        record_sample_if_new(
            &mut samples,
            SamplePoint {
                y: &runtime.current_y,
                params: &runtime.params,
                t: boundary.continuation_time,
            },
        )?;
        runtime.current_t = boundary.continuation_time;
        return Ok(());
    }
    let event = step.event_stop.unwrap_or(RuntimeEventStop {
        pre_mode,
        observe_right_limit: false,
        terminal: false,
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

pub(crate) struct NoStateRuntime {
    pub(crate) runtime: SolveRuntime,
    pub(crate) params: Vec<f64>,
    pub(crate) current_y: Vec<f64>,
    pub(crate) current_t: f64,
    pub(crate) last_event_t: Option<f64>,
    pub(crate) data: Vec<Vec<f64>>,
    pub(crate) recorded_times: Vec<f64>,
    pub(crate) equilibrium_model: OdeModel,
    pub(crate) stop_schedule: SolveStopSchedule,
    root_params_scratch: Vec<f64>,
    root_search_scratch: NoStateRootSearchScratch,
}

fn settle_no_state_accepted_step(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &mut NoStateRuntime,
) -> Result<(), SimError> {
    refresh_observation_rows_and_relation_memory(
        model,
        &runtime.runtime,
        &runtime.equilibrium_model,
        &mut runtime.current_y,
        &mut runtime.params,
        runtime.current_t,
        opts.atol.max(1.0e-10),
    )?;
    runtime
        .runtime
        .commit_delay_history(runtime.current_t, &runtime.current_y, &runtime.params)?;
    Ok(())
}

fn record_no_state_output(
    model: &solve::SolveModel,
    runtime: &mut NoStateRuntime,
) -> Result<(), SimError> {
    let mut samples = SampleRecorder {
        runtime: Some(&runtime.runtime),
        model,
        recorded_times: &mut runtime.recorded_times,
        data: &mut runtime.data,
    };
    record_sample_if_new(
        &mut samples,
        SamplePoint {
            y: &runtime.current_y,
            params: &runtime.params,
            t: runtime.current_t,
        },
    )
}

pub(crate) fn initialize_no_state_runtime(
    model: &solve::SolveModel,
    opts: &SimOptions,
    output_count: usize,
    apply_without_initial_event: bool,
) -> Result<NoStateRuntime, SimError> {
    let mut params = model.parameters.clone();
    let mut current_y = model.initial_y.clone();
    let mut current_t = opts.t_start;
    let tol = opts.atol.max(1.0e-10);
    let runtime = SolveRuntime::new(model)?;
    let equilibrium_model = OdeModel::new(model)?;
    runtime.initialize_delay_history(current_t, &current_y, &mut params)?;
    runtime.set_initial_event_flag(&mut params, true);
    // `pre()` at the initial event is defined by the declared/start values.
    // Initialization projection may settle current condition memory before the
    // event update runs, so preserve the pre-event snapshot first.
    let event_pre_y = current_y.clone();
    let event_pre_p = params.clone();
    runtime.settle_initialization_system(
        &mut current_y,
        &mut params,
        current_t,
        tol,
        EVENT_UPDATE_MAX_ITERS,
    )?;
    settle_algebraics_and_relation_memory(
        &runtime,
        &equilibrium_model,
        &mut current_y,
        &mut params,
        current_t,
        0,
        tol,
    )?;
    let dynamic_event = runtime.current_dynamic_time_event_stop(&current_y, &params, current_t)?;
    let outcome = runtime.apply_projected_initial_event_boundary(
        rumoca_solver::ProjectedInitialEventInput {
            y: &mut current_y,
            p: &mut params,
            t_start: current_t,
            t_end: opts.t_end,
            tol,
            event_pre_y: &event_pre_y,
            event_pre_p: &event_pre_p,
            max_iters: EVENT_UPDATE_MAX_ITERS,
            dynamic_event,
            apply_without_initial_event,
        },
        |y, p, t| project_algebraics_and_detect_changes(&equilibrium_model, y, p, t, 0, tol),
    )?;
    event_action_outcome_to_result(outcome.action, outcome.final_t)?;
    current_t = outcome.final_t;
    if apply_without_initial_event || !outcome.observations.is_empty() {
        refresh_observation_rows_and_relation_memory(
            model,
            &runtime,
            &equilibrium_model,
            &mut current_y,
            &mut params,
            current_t,
            tol,
        )?;
    }
    runtime.commit_delay_history(current_t, &current_y, &params)?;
    let root_count = runtime.root_condition_count();
    let mut data = vec![Vec::with_capacity(output_count); model.visible_names.len()];
    let mut recorded_times = Vec::with_capacity(output_count);
    for observation in &outcome.observations {
        let mut samples = SampleRecorder {
            runtime: Some(&runtime),
            model,
            recorded_times: &mut recorded_times,
            data: &mut data,
        };
        record_prepared_observation_sample(
            &mut samples,
            &runtime,
            &equilibrium_model,
            tol,
            SamplePoint {
                y: &observation.y,
                params: &observation.p,
                t: observation.t,
            },
        )?;
    }
    Ok(NoStateRuntime {
        runtime,
        params,
        current_y,
        current_t,
        last_event_t: None,
        data,
        recorded_times,
        equilibrium_model,
        stop_schedule: SolveStopSchedule::new(&model.problem, opts.t_start, opts.t_end),
        root_params_scratch: vec![0.0; model.parameters.len()],
        root_search_scratch: NoStateRootSearchScratch::new(root_count),
    })
}

struct NoStateRootEvaluation<'a> {
    runtime: &'a SolveRuntime,
    y: &'a [f64],
    p: &'a [f64],
    params_scratch: &'a mut Vec<f64>,
    root_scratch: &'a mut NoStateRootSearchScratch,
}

fn next_no_state_root_event_time(
    input: NoStateRootEvaluation<'_>,
    current_t: f64,
    target: f64,
    tol: f64,
) -> Result<Option<f64>, SimError> {
    let NoStateRootEvaluation {
        runtime,
        y,
        p,
        params_scratch,
        root_scratch,
    } = input;
    let planned_root = runtime.next_planned_time_root(p, current_t, target, tol)?;
    let search_target = planned_root.unwrap_or(target);
    let root_count = runtime.root_condition_count();
    let Some(root_time) = first_no_state_root_crossing(
        root_scratch,
        root_count,
        current_t,
        search_target,
        tol,
        |t, out| eval_refreshed_roots(runtime, y, p, params_scratch, t, tol, out),
    )?
    else {
        return Ok(planned_root);
    };
    if root_time > current_t + tol
        && (root_time < target || sample_time_match_with_tol(root_time, target))
    {
        Ok(Some(root_time))
    } else {
        Ok(planned_root)
    }
}

pub(crate) struct FixedEventLeftLimitInput<'a> {
    model: &'a solve::SolveModel,
    runtime: &'a SolveRuntime,
    equilibrium_model: &'a OdeModel,
    y: &'a mut [f64],
    params: &'a mut [f64],
    event_t: f64,
    tol: f64,
    event: RuntimeEventStop,
}

pub(crate) fn prepare_fixed_event_left_limit(
    input: FixedEventLeftLimitInput<'_>,
) -> Result<(), SimError> {
    if !matches!(
        input.event.pre_mode,
        EventPreMode::EventEntry | EventPreMode::Fixed
    ) {
        return Ok(());
    }
    let left_t = event_left_probe_time(input.event_t, input.tol);
    input
        .runtime
        .refresh_delay_values(left_t, input.y, input.params)?;
    input
        .runtime
        .project_state_manifold(input.y, input.params, left_t, input.tol)?;
    input.runtime.refresh_observation_discrete_rows(
        input.y,
        input.params,
        left_t,
        input.tol,
        EVENT_UPDATE_MAX_ITERS,
    )?;
    project_algebraics(
        input.equilibrium_model,
        input.y,
        input.params,
        left_t,
        input.model.state_scalar_count(),
        input.tol,
    )?;
    Ok(())
}

fn eval_refreshed_roots(
    runtime: &SolveRuntime,
    y: &[f64],
    p: &[f64],
    params: &mut Vec<f64>,
    t: f64,
    tol: f64,
    out: &mut [f64],
) -> Result<(), SimError> {
    params.clear();
    params.extend_from_slice(p);
    runtime.refresh_delay_values(t, y, params)?;
    runtime
        .eval_root_search_conditions_into(t, y, params, tol, EVENT_UPDATE_MAX_ITERS, out)
        .map_err(Into::into)
}
