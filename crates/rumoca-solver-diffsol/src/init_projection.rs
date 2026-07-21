//! Initial-value projection for the state simulation paths.
//!
//! Settles the initial event/`pre` chain, projects algebraics and initial
//! unknowns against the model's initialization plan, and seeds the runtime
//! parameter vector before time integration starts.

use super::*;
use rumoca_solver::EventActionOutcome;

pub(crate) fn initialize_state_runtime_values(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &SolveRuntime,
    equilibrium_model: &OdeModel,
    current_y: &mut [f64],
    params: &mut [f64],
    current_t: &mut f64,
) -> Result<Vec<solve_eval::InitialEventObservation>, SimError> {
    let tol = opts.atol.max(1.0e-10);
    runtime.set_initial_event_flag(params, true);
    let event_pre = InitialEventPreValues::snapshot(current_y, params);
    let t_start = *current_t;
    let initial_projection_params = state_initial_projection_params(
        model,
        runtime,
        equilibrium_model,
        current_y,
        params,
        t_start,
        tol,
    )?;
    params.copy_from_slice(&initial_projection_params);
    let dynamic_event = current_dynamic_time_event_stop(
        model,
        &equilibrium_model.runtime_state,
        current_y,
        params,
        t_start,
    )?;
    settle_algebraics_and_relation_memory(
        runtime,
        equilibrium_model,
        current_y,
        params,
        t_start,
        model.state_scalar_count(),
        tol,
    )?;
    let outcome = apply_state_initial_event_updates(StateInitialEventUpdates {
        model,
        opts,
        runtime,
        current_y,
        params,
        current_t: t_start,
        tol,
        dynamic_event,
        event_pre: &event_pre,
    })?;
    initial_event_action_to_result(outcome.action, outcome.final_t)?;
    *current_t = outcome.final_t;
    Ok(outcome.observations)
}

struct InitialEventPreValues {
    y: Vec<f64>,
    p: Vec<f64>,
}

impl InitialEventPreValues {
    fn snapshot(y: &[f64], p: &[f64]) -> Self {
        Self {
            y: y.to_vec(),
            p: p.to_vec(),
        }
    }
}

fn state_initial_projection_params(
    model: &solve::SolveModel,
    runtime: &SolveRuntime,
    equilibrium_model: &OdeModel,
    current_y: &mut [f64],
    params: &[f64],
    current_t: f64,
    tol: f64,
) -> Result<Vec<f64>, SimError> {
    let mut projection_params = params.to_vec();
    seed_initial_discrete_values(
        runtime,
        equilibrium_model,
        current_y,
        &mut projection_params,
        current_t,
        tol,
    )?;
    runtime.settle_runtime_assignments_and_relation_memory(
        current_y,
        &mut projection_params,
        current_t,
        tol,
        EVENT_UPDATE_MAX_ITERS,
    )?;
    project_initial_unknowns(
        model,
        equilibrium_model,
        current_y,
        &projection_params,
        current_t,
        tol,
    )?;
    seed_initial_discrete_values(
        runtime,
        equilibrium_model,
        current_y,
        &mut projection_params,
        current_t,
        tol,
    )?;
    project_initial_algebraics_and_updates(
        model,
        runtime,
        equilibrium_model,
        current_y,
        &mut projection_params,
        current_t,
        tol,
    )?;
    Ok(projection_params)
}

struct StateInitialEventUpdates<'a> {
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    runtime: &'a SolveRuntime,
    current_y: &'a mut [f64],
    params: &'a mut [f64],
    current_t: f64,
    tol: f64,
    dynamic_event: Option<RuntimeEventStop>,
    event_pre: &'a InitialEventPreValues,
}

fn apply_state_initial_event_updates(
    ctx: StateInitialEventUpdates<'_>,
) -> Result<solve_eval::ProjectedInitialEventOutcome, SimError> {
    let StateInitialEventUpdates {
        model,
        opts,
        runtime,
        current_y,
        params,
        current_t,
        tol,
        dynamic_event,
        event_pre,
    } = ctx;
    let outcome = runtime.apply_projected_initial_event_boundary(
        solve_eval::ProjectedInitialEventInput {
            y: current_y,
            p: params,
            t_start: current_t,
            t_end: opts.t_end,
            tol,
            event_pre_y: &event_pre.y,
            event_pre_p: &event_pre.p,
            max_iters: EVENT_UPDATE_MAX_ITERS,
            dynamic_event,
            apply_without_initial_event: true,
        },
        |y, p, t| refresh_algebraics_and_detect_changes(runtime, y, p, t, tol),
    )?;
    commit_pre_params_after_event(model, current_y, params, tol);
    Ok(outcome)
}

fn initial_event_action_to_result(
    outcome: EventActionOutcome,
    event_t: f64,
) -> Result<(), SimError> {
    match outcome {
        EventActionOutcome::Continue => Ok(()),
        EventActionOutcome::AssertionFailed { time, message } => Err(SimError::AssertionFailed {
            time: if time.is_finite() { time } else { event_t },
            message,
        }),
        EventActionOutcome::Terminated { time, message } => Err(SimError::Terminated {
            time: if time.is_finite() { time } else { event_t },
            message,
        }),
    }
}

fn project_initial_algebraics_and_updates(
    model: &solve::SolveModel,
    runtime: &SolveRuntime,
    equilibrium_model: &OdeModel,
    current_y: &mut [f64],
    params: &mut [f64],
    current_t: f64,
    tol: f64,
) -> Result<(), SimError> {
    for _ in 0..EVENT_UPDATE_MAX_ITERS {
        project_initial_unknowns(model, equilibrium_model, current_y, params, current_t, tol)?;
        let updates_changed = apply_initialization_updates(
            runtime,
            equilibrium_model,
            current_y,
            params,
            current_t,
            tol,
        )?;
        if !updates_changed {
            return Ok(());
        }
    }
    Err(SimError::SolveIr(format!(
        "initial algebraic/update projection did not converge at t={current_t}"
    )))
}

fn project_initial_unknowns(
    model: &solve::SolveModel,
    equilibrium_model: &OdeModel,
    current_y: &mut [f64],
    params: &[f64],
    current_t: f64,
    tol: f64,
) -> Result<(), SimError> {
    project_initial_variables_with_plan(
        equilibrium_model,
        current_y,
        params,
        current_t,
        &model.problem.initialization.projection_plan,
        tol,
    )
    .map_err(SimError::from)
}

pub(crate) struct EventObservation<'a> {
    pub(crate) runtime: &'a SolveRuntime,
    pub(crate) model: &'a solve::SolveModel,
    pub(crate) equilibrium_model: &'a OdeModel,
    pub(crate) y: &'a mut [f64],
    pub(crate) params: &'a mut [f64],
    pub(crate) tol: f64,
    pub(crate) recorded_times: &'a mut Vec<f64>,
    pub(crate) data: &'a mut [Vec<f64>],
    /// Solver/parameter state captured *before* the event update was first
    /// applied at `event_t`. Modelica `pre()` is frozen for the whole event
    /// instant, so the right-limit re-application must read `pre()` from this
    /// snapshot rather than re-snapshotting the already-updated state (which
    /// would double-count `pre()`-accumulators such as `count = pre(count)+1`).
    pub(crate) event_pre_y: &'a [f64],
    pub(crate) event_pre_p: &'a [f64],
}

impl EventObservation<'_> {
    pub(crate) fn record_time_event(
        &mut self,
        event_t: f64,
        horizon_t: f64,
        event: RuntimeEventStop,
    ) -> Result<f64, SimError> {
        let outcome = process_runtime_event_boundary(
            RuntimeEventBoundary {
                event_t,
                horizon_t,
                event,
            },
            self,
        )?;
        Ok(outcome.final_t)
    }
}

impl RuntimeEventBoundaryHandler for EventObservation<'_> {
    type Error = SimError;

    fn on_event_time(&mut self, event_t: f64, _event: RuntimeEventStop) -> Result<(), Self::Error> {
        refresh_observation_rows_and_relation_memory(
            self.model,
            self.runtime,
            self.equilibrium_model,
            self.y,
            self.params,
            event_t,
            self.tol,
        )?;
        let mut samples = SampleRecorder {
            runtime: Some(self.runtime),
            model: self.model,
            recorded_times: &mut *self.recorded_times,
            data: &mut *self.data,
        };
        record_sample_if_new(
            &mut samples,
            SamplePoint {
                y: self.y,
                params: self.params,
                t: event_t,
            },
        )?;
        Ok(())
    }

    fn on_event_right_limit(
        &mut self,
        right_t: f64,
        _event: RuntimeEventStop,
    ) -> Result<(), Self::Error> {
        apply_event_updates_with_event_pre(EventUpdateInput {
            runtime: self.runtime,
            y: self.y,
            p: self.params,
            t: right_t,
            tol: self.tol,
            event_pre_y: self.event_pre_y,
            event_pre_p: self.event_pre_p,
        })?;
        refresh_observation_rows_and_relation_memory(
            self.model,
            self.runtime,
            self.equilibrium_model,
            self.y,
            self.params,
            right_t,
            self.tol,
        )?;
        let mut samples = SampleRecorder {
            runtime: Some(self.runtime),
            model: self.model,
            recorded_times: &mut *self.recorded_times,
            data: &mut *self.data,
        };
        record_sample_if_new(
            &mut samples,
            SamplePoint {
                y: self.y,
                params: self.params,
                t: right_t,
            },
        )?;
        Ok(())
    }
}
