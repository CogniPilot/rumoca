use super::*;
use rumoca_solver::{
    EventPreMode, build_sim_result_from_solve_model, timeline::event_right_limit_time,
};

use crate::root_search::first_root_crossing_time;

pub(crate) fn simulate_no_state_solve_ir(
    model: &solve::SolveModel,
    opts: &SimOptions,
) -> Result<SimResult, SimError> {
    let dt = opts.dt.unwrap_or((opts.t_end - opts.t_start).abs() / 500.0);
    let times = rumoca_solver::timeline::build_output_times(opts.t_start, opts.t_end, dt);
    let mut runtime = initialize_no_state_runtime(model, opts, times.len())?;

    for target in times {
        advance_no_state_to_target(model, opts, &mut runtime, target)?;
        settle_and_record_no_state_output(model, opts, &mut runtime)?;
    }

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

fn advance_no_state_to_target(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &mut NoStateRuntime,
    target: f64,
) -> Result<(), SimError> {
    let tol = opts.atol.max(1.0e-10);
    while target > runtime.current_t + tol {
        let (stop_time, event_stop) = next_runtime_event_stop(
            model,
            &runtime.equilibrium_model.runtime_state,
            &runtime.current_y,
            &runtime.params,
            &mut runtime.stop_schedule,
            runtime.current_t,
            target,
        )?;
        let root_event_time = next_no_state_root_event_time(
            &runtime.equilibrium_model,
            &runtime.current_y,
            &runtime.params,
            runtime.current_t,
            target,
            tol,
        )?;
        let root_event = root_event_time
            .map(|root_time| event_stop.is_none() || root_time < stop_time)
            .unwrap_or(false);
        if event_stop.is_none() && !root_event {
            runtime.current_t = target;
            break;
        }
        apply_no_state_event_step(
            model,
            opts,
            runtime,
            NoStateEventStep {
                target,
                stop_time,
                event_stop,
                root_event_time,
                root_event,
                tol,
            },
        )?;
    }
    if runtime.current_t < target {
        runtime.current_t = target;
    }
    Ok(())
}

struct NoStateEventStep {
    target: f64,
    stop_time: f64,
    event_stop: Option<RuntimeEventStop>,
    root_event_time: Option<f64>,
    root_event: bool,
    tol: f64,
}

fn apply_no_state_event_step(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &mut NoStateRuntime,
    step: NoStateEventStep,
) -> Result<(), SimError> {
    runtime.current_t = if step.root_event {
        root_event_application_time(step.root_event_time.unwrap_or(step.stop_time), step.target)
    } else {
        step.stop_time
    };
    let pre_mode = no_state_event_pre_mode(step.root_event, step.event_stop);
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
    apply_event_updates(
        model,
        &runtime.equilibrium_model,
        &mut runtime.current_y,
        &mut runtime.params,
        runtime.current_t,
        step.tol,
        pre_mode,
    )?;
    record_no_state_event_step(model, opts, runtime, step, pre_mode)?;
    runtime.stop_schedule.advance_past(runtime.current_t);
    Ok(())
}

fn no_state_event_pre_mode(root_event: bool, event_stop: Option<RuntimeEventStop>) -> EventPreMode {
    if root_event {
        EventPreMode::FollowCurrent
    } else {
        event_stop
            .map(|event| event.pre_mode)
            .unwrap_or(EventPreMode::FollowCurrent)
    }
}

fn record_no_state_event_step(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &mut NoStateRuntime,
    step: NoStateEventStep,
    pre_mode: EventPreMode,
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
        runtime.current_t = event_right_limit_time(runtime.current_t).min(step.target);
        return Ok(());
    }
    let event = step.event_stop.unwrap_or(RuntimeEventStop {
        pre_mode,
        observe_right_limit: false,
    });
    runtime.current_t = EventObservation {
        runtime: None,
        model,
        equilibrium_model: &runtime.equilibrium_model,
        y: &mut runtime.current_y,
        params: &mut runtime.params,
        tol: step.tol,
        recorded_times: &mut runtime.recorded_times,
        data: &mut runtime.data,
    }
    .record_time_event(
        runtime.current_t,
        time_event_right_limit_cap(event, step.target, opts.t_end),
        event,
    )?;
    Ok(())
}

struct NoStateRuntime {
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
        &runtime.equilibrium_model,
        &mut runtime.current_y,
        &mut runtime.params,
        runtime.current_t,
        0,
        &model.problem.solve_layout.relation_memory_parameter_indices,
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
    let equilibrium_model = OdeModel::new(model)?;
    set_initial_event_flag(model, &mut params, true);
    settle_algebraics_and_relation_memory(
        &equilibrium_model,
        &mut current_y,
        &mut params,
        current_t,
        0,
        &model.problem.solve_layout.relation_memory_parameter_indices,
        opts.atol.max(1.0e-10),
    )?;
    let initial_event_mode = initial_static_event_pre_mode(&model.problem, current_t);
    apply_event_updates(
        model,
        &equilibrium_model,
        &mut current_y,
        &mut params,
        current_t,
        opts.atol.max(1.0e-10),
        initial_event_mode.unwrap_or(EventPreMode::FollowCurrent),
    )?;
    set_initial_event_flag(model, &mut params, false);
    if initial_event_mode.is_some() {
        apply_post_initial_event_updates(
            model,
            &equilibrium_model,
            &mut current_y,
            &mut params,
            current_t,
            opts.atol.max(1.0e-10),
        )?;
    }
    Ok(NoStateRuntime {
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
    if sample_time_match_with_tol(root_time, target) {
        target
    } else {
        event_right_limit_time(root_time).min(target)
    }
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
    if event.pre_mode != EventPreMode::Fixed {
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
