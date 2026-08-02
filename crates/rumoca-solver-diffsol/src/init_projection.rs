//! Initial-value projection for the state simulation paths.
//!
//! Settles the initial event/`pre` chain, projects algebraics and initial
//! unknowns against the model's initialization plan, and seeds the runtime
//! parameter vector before time integration starts.

use super::*;
use crate::SimFailureStage;
use rumoca_solver::EventActionOutcome;

pub(crate) fn initialize_state_runtime_values(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &SolveRuntime,
    equilibrium_model: &OdeModel,
    current_y: &mut [f64],
    params: &mut [f64],
    current_t: &mut f64,
) -> Result<Vec<rumoca_solver::InitialEventObservation>, SimError> {
    initialize_state_runtime_values_inner(
        model,
        opts,
        runtime,
        equilibrium_model,
        current_y,
        params,
        current_t,
    )
    .map_err(|error| error.at_stage(SimFailureStage::Initialization))
}

/// Everything settled before time integration starts. Failures are annotated as
/// [`SimFailureStage::Initialization`] by the wrapper above so downstream
/// classification never has to recognise an initialization message by its text.
fn initialize_state_runtime_values_inner(
    model: &solve::SolveModel,
    opts: &SimOptions,
    runtime: &SolveRuntime,
    equilibrium_model: &OdeModel,
    current_y: &mut [f64],
    params: &mut [f64],
    current_t: &mut f64,
) -> Result<Vec<rumoca_solver::InitialEventObservation>, SimError> {
    let tol = opts.atol.max(1.0e-10);
    runtime.initialize_delay_history(*current_t, current_y, params)?;
    runtime.set_initial_event_flag(params, true);
    let t_start = *current_t;
    let initial_projection_params = state_initial_projection_params(
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
    let event_pre = InitialEventPreValues::snapshot(current_y, params);
    let outcome = apply_state_initial_event_updates(StateInitialEventUpdates {
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
    runtime.commit_delay_history(outcome.final_t, current_y, params)?;
    Ok(outcome.observations)
}

/// The `pre()` the event iteration at the initial time reads.
///
/// MLS 3.6 §8.6: "Before the start of the integration, it must be guaranteed
/// that for all variables `v`, `v = pre(v)`. If this is not the case for some
/// variables `vi`, `pre(vi) := vi` must be set and an event iteration at the
/// initial time must follow, so the model is re-evaluated, until this condition
/// is fulfilled."
///
/// `vi` there is the value initialization settled, so this snapshot is taken
/// *after* the initialization system, the discrete initialization fixed point,
/// and the algebraic/relation-memory settle have run — never from the declared
/// `start` values they replaced. Taking it earlier makes every `pre()` read of
/// the initial event fall back to `start`, which silently discards the whole
/// §8.6 initialization result for any coordinate initialization moved.
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
    runtime.settle_initialization_system(
        current_y,
        &mut projection_params,
        current_t,
        tol,
        EVENT_UPDATE_MAX_ITERS,
    )?;
    seed_initial_discrete_values(
        runtime,
        equilibrium_model,
        current_y,
        &mut projection_params,
        current_t,
        tol,
    )?;
    runtime.settle_initialization_system(
        current_y,
        &mut projection_params,
        current_t,
        tol,
        EVENT_UPDATE_MAX_ITERS,
    )?;
    Ok(projection_params)
}

struct StateInitialEventUpdates<'a> {
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
) -> Result<rumoca_solver::ProjectedInitialEventOutcome, SimError> {
    let StateInitialEventUpdates {
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
        rumoca_solver::ProjectedInitialEventInput {
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
        boundary_tolerance: f64,
        event: RuntimeEventStop,
    ) -> Result<f64, SimError> {
        let outcome = process_runtime_event_boundary(
            RuntimeEventBoundary {
                event_t,
                horizon_t,
                tolerance: boundary_tolerance,
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
        // An event right limit is a distinct superdense observation even when
        // its timestamp is the next representable value. Ordinary output-time
        // deduplication would collapse it back into the event instant.
        self.recorded_times.push(right_t);
        self.runtime
            .record_visible_sample(self.data, self.y, self.params, right_t)
            .map_err(|error| SimError::SolveIr(error.to_string()))?;
        Ok(())
    }
}
