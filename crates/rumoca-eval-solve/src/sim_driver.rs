//! Backend-neutral simulation output / event / root driver.
//!
//! This is the simulation orchestration shared by ODE backends: it walks the
//! output grid, lets the solver step (free dense-output or clamped onto each
//! output point), locates zero-crossing roots, applies scheduled time events,
//! and records the visible trajectory. Everything solver-specific — taking a
//! step, interpolating, the native-state↔full-solver_y mapping, the post-event
//! reset, the algebraic-projection event kernels — is delegated to a
//! [`SolverStepper`]. The driver therefore carries no backend types and is
//! identical for the implicit (diffsol) and explicit (rk-like) solvers; each
//! backend only provides a `SolverStepper` adapter.

use std::cell::RefCell;
use std::rc::Rc;

use rumoca_ir_solve as solve;
use rumoca_solver::{
    RuntimeEventStop, RuntimeSolveError, SimOptions, SolveStopSchedule,
    commit_pre_params_after_event, runtime_event_horizon, runtime_root_event_application_time,
    timeline::sample_time_match_with_tol,
};

use crate::{SimulationRuntimeState, next_runtime_event_stop};

/// Shared runtime-parameter cell captured by the solver closures.
pub type RuntimeParameters = Rc<RefCell<Vec<f64>>>;

/// Result of a single solver step (mirrors a stop-reason minus the unused
/// `Finished`).
pub enum StepOutcome {
    /// Reached the requested stop time (`set_stop_time`).
    Stop,
    /// Took an internal adaptive step (did not reach a stop/root).
    Internal,
    /// A zero-crossing root was located at `t_root`.
    Root { t_root: f64 },
}

/// Error surfaced by the driver. Backend (`SolverStepper`) failures arrive as
/// [`SimDriverError::Backend`]; runtime evaluation failures convert in via
/// `From<RuntimeSolveError>`. `Terminated` is preserved so the backend's
/// finalization can replay Modelica `terminate()` semantics.
#[derive(Debug, thiserror::Error)]
pub enum SimDriverError {
    #[error("{0}")]
    Runtime(#[from] RuntimeSolveError),
    #[error("{0}")]
    Backend(String),
    #[error("{0}")]
    SolveIr(String),
    #[error("terminated at t={time}: {message}")]
    Terminated { time: f64, message: String },
}

/// Inputs for recording a scheduled time event.
pub struct ScheduledEventRecord<'a> {
    pub current_t: f64,
    pub horizon: f64,
    pub event: RuntimeEventStop,
    pub event_pre_y: &'a [f64],
    pub event_pre_p: &'a [f64],
}

/// The backend-specific operations the driver needs. diffsol implements this
/// over its `OdeSolverMethod` + `OdeModel` (encapsulating the General/StateOnly
/// integration mode here, so the driver never sees it); an explicit rk-like
/// backend can implement the same contract. Each method maps to an existing,
/// unchanged backend routine, so the numerics are preserved.
pub trait SolverStepper {
    // --- solver stepping ---
    fn time(&self) -> f64;
    fn native_y(&self) -> Vec<f64>;
    fn step(&mut self) -> Result<StepOutcome, SimDriverError>;
    fn set_stop_time(&mut self, stop_time: f64) -> Result<(), SimDriverError>;
    fn interpolate(&mut self, t: f64) -> Result<Vec<f64>, SimDriverError>;
    fn state_mut_back(&mut self, t: f64) -> Result<(), SimDriverError>;

    // --- native-state <-> full-solver_y mapping (encapsulates the backend's
    //     integration mode: identity for a full system, projection for a
    //     reduced-state system) ---
    /// Map the solver's native state at time `t` to the full solver_y.
    fn native_to_full_y(
        &self,
        native: &[f64],
        t: f64,
        params: &[f64],
    ) -> Result<Vec<f64>, SimDriverError>;
    /// Native state + derivative guess to reload after an event, from the
    /// post-event full solver_y at time `t`.
    fn reset_vectors(
        &self,
        current_y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<(Vec<f64>, Vec<f64>), SimDriverError>;
    fn reset(
        &mut self,
        native_y: &[f64],
        native_dy: &[f64],
        params: &[f64],
        t: f64,
        h_cap: f64,
    ) -> Result<(), SimDriverError>;
    /// Whether output points should be reached by stepping the solver exactly
    /// onto them (true) rather than by dense-output interpolation (false), for
    /// the model being simulated. (Reduced-state backends whose interpolation
    /// re-projects algebraics return true near discontinuities.)
    fn prefer_exact_output_steps(&self) -> bool;

    // --- event application (delegated to the backend's projection kernels) ---
    fn apply_event_updates_with_event_pre(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        event_pre_y: &[f64],
        event_pre_p: &[f64],
    ) -> Result<(), SimDriverError>;
    fn apply_event_updates(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
    ) -> Result<(), SimDriverError>;
    fn prepare_fixed_event_left_limit(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        event: RuntimeEventStop,
    ) -> Result<(), SimDriverError>;
    fn settle_prepared_state(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
    ) -> Result<(), SimDriverError>;
    fn settle_algebraics_and_relation_memory(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        state_count: usize,
        tol: f64,
    ) -> Result<(), SimDriverError>;
    fn advance_state_to_event_limits(
        &self,
        event_pre_y: &mut [f64],
        y: &mut [f64],
        p: &[f64],
        root_t: f64,
        right_t: f64,
    ) -> Result<(), SimDriverError>;
    /// Record the observation sample(s) for a scheduled time event; returns the
    /// post-event time the driver should adopt as `current_t`.
    fn record_time_event(
        &self,
        record: ScheduledEventRecord<'_>,
        y: &mut [f64],
        p: &mut [f64],
        recorded_times: &mut Vec<f64>,
        data: &mut [Vec<f64>],
    ) -> Result<f64, SimDriverError>;

    // --- observation / recording ---
    fn record_sample(
        &self,
        recorded_times: &mut Vec<f64>,
        data: &mut [Vec<f64>],
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<(), SimDriverError>;
    fn refresh_observation(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
    ) -> Result<(), SimDriverError>;

    // --- debug trace hooks (no-op unless tracing is enabled) ---
    fn trace_step_failure(
        &self,
        y: &[f64],
        params: &[f64],
        current_t: f64,
        solver_t: f64,
        error: &str,
    );
    fn trace_post_event_state(&self, y: &[f64], params: &[f64], t: f64);
}

const EVENT_TRACE_TARGET: &str = "rumoca_solver_diffsol::bdf";

#[derive(Clone, Copy)]
struct AdvanceContext<'a> {
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    runtime_params: &'a RuntimeParameters,
}

struct AdvanceState<'a> {
    current_y: &'a mut [f64],
    params: &'a mut [f64],
    current_t: &'a mut f64,
}

struct ObservationBuffers<'a> {
    recorded_times: &'a mut Vec<f64>,
    data: &'a mut [Vec<f64>],
}

enum PendingRootAction {
    None,
    Break,
    Continue,
}

/// Buffers for one full [`simulate_state_targets`] run.
pub struct StateTrajectory<'a> {
    pub params: &'a mut Vec<f64>,
    pub data: &'a mut Vec<Vec<f64>>,
    pub recorded_times: &'a mut Vec<f64>,
    pub current_t: &'a mut f64,
    pub current_y: &'a mut Vec<f64>,
    pub runtime_state: &'a SimulationRuntimeState,
}

/// Write the full solver_y for `native` at `t` into `current_y`.
fn write_full_y<St: SolverStepper + ?Sized>(
    stepper: &St,
    native: &[f64],
    t: f64,
    current_y: &mut [f64],
    params: &[f64],
) -> Result<(), SimDriverError> {
    let full = stepper.native_to_full_y(native, t, params)?;
    current_y.copy_from_slice(&full);
    Ok(())
}

// SPEC_0021: Exception - central event loop keeps step advancement,
// zero-crossing handling, and sample recording in a single ordered routine.
#[allow(clippy::too_many_lines)]
pub fn simulate_state_targets<St: SolverStepper + ?Sized>(
    model: &solve::SolveModel,
    opts: &SimOptions,
    times: &[f64],
    runtime_params: &RuntimeParameters,
    stepper: &mut St,
    state: StateTrajectory<'_>,
) -> Result<(), SimDriverError> {
    let runtime_state = state.runtime_state;
    let mut stop_schedule = SolveStopSchedule::new(&model.problem, opts.t_start, opts.t_end);
    let mut pending_root_t: Option<f64> = None;
    let make_ctx = || AdvanceContext {
        model,
        opts,
        runtime_params,
    };

    for &target in times {
        if state
            .recorded_times
            .last()
            .is_some_and(|last| sample_time_match_with_tol(*last, target))
        {
            continue;
        }
        let tol = opts.atol.max(1.0e-12);
        while target > *state.current_t + tol {
            match resolve_pending_root(
                &mut pending_root_t,
                make_ctx(),
                AdvanceState {
                    current_y: state.current_y,
                    params: state.params,
                    current_t: state.current_t,
                },
                target,
                stepper,
                &mut stop_schedule,
            )? {
                PendingRootAction::Break => break,
                PendingRootAction::Continue => continue,
                PendingRootAction::None => {}
            }

            let (stop_time, event_stop) = next_runtime_event_stop(
                model,
                runtime_state,
                state.current_y,
                state.params,
                &mut stop_schedule,
                *state.current_t,
                target,
            )?;
            let mut deferred_root: Option<f64> = None;
            let hit_root = advance_to_target_once(
                make_ctx(),
                AdvanceState {
                    current_y: state.current_y,
                    params: state.params,
                    current_t: state.current_t,
                },
                stop_time,
                event_stop,
                stepper,
                &mut deferred_root,
            )?;
            if let Some(prt) = deferred_root {
                pending_root_t = Some(prt);
            }
            if let Some(event) = event_stop
                && sample_time_match_with_tol(*state.current_t, stop_time)
            {
                apply_scheduled_time_event(
                    make_ctx(),
                    AdvanceState {
                        current_y: state.current_y,
                        params: state.params,
                        current_t: state.current_t,
                    },
                    event,
                    target,
                    stepper,
                    ObservationBuffers {
                        recorded_times: state.recorded_times,
                        data: state.data,
                    },
                )?;
                stop_schedule.advance_past(*state.current_t);
            }
            if !hit_root && event_stop.is_none() {
                break;
            }
        }
        stepper.refresh_observation(state.current_y, state.params, *state.current_t)?;
        runtime_params.borrow_mut().copy_from_slice(state.params);
        stepper.record_sample(
            state.recorded_times,
            state.data,
            state.current_y,
            state.params,
            *state.current_t,
        )?;
    }

    Ok(())
}

fn resolve_pending_root<St: SolverStepper + ?Sized>(
    pending_root_t: &mut Option<f64>,
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    stepper: &mut St,
    stop_schedule: &mut SolveStopSchedule,
) -> Result<PendingRootAction, SimDriverError> {
    let Some(prt) = *pending_root_t else {
        return Ok(PendingRootAction::None);
    };
    if !sample_time_match_with_tol(target, prt) && target < prt {
        let y_at = stepper.interpolate(target)?;
        *state.current_t = target;
        state
            .params
            .copy_from_slice(ctx.runtime_params.borrow().as_slice());
        write_full_y(stepper, &y_at, target, state.current_y, state.params)?;
        refresh_interpolated_sample_state(ctx, state, target, stepper)?;
        return Ok(PendingRootAction::Break);
    }

    *pending_root_t = None;
    handle_root_crossing(ctx, state, prt, target, stepper)?;
    stop_schedule.advance_past(stepper.time());
    Ok(PendingRootAction::Continue)
}

fn refresh_interpolated_sample_state<St: SolverStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    stepper: &mut St,
) -> Result<(), SimDriverError> {
    stepper.settle_prepared_state(state.current_y, state.params, target)?;
    stepper.refresh_observation(state.current_y, state.params, target)?;
    ctx.runtime_params
        .borrow_mut()
        .copy_from_slice(state.params);
    Ok(())
}

fn apply_scheduled_time_event<St: SolverStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    event: RuntimeEventStop,
    target: f64,
    stepper: &mut St,
    observations: ObservationBuffers<'_>,
) -> Result<(), SimDriverError> {
    let tol = ctx.opts.atol.max(1.0e-10);
    stepper.prepare_fixed_event_left_limit(
        state.current_y,
        state.params,
        *state.current_t,
        tol,
        event,
    )?;
    let event_pre_y = state.current_y.to_vec();
    let event_pre_p = state.params.to_vec();
    stepper.apply_event_updates(state.current_y, state.params, *state.current_t, tol)?;
    *state.current_t = stepper.record_time_event(
        ScheduledEventRecord {
            current_t: *state.current_t,
            horizon: runtime_event_horizon(event, target, ctx.opts.t_end),
            event,
            event_pre_y: &event_pre_y,
            event_pre_p: &event_pre_p,
        },
        state.current_y,
        state.params,
        observations.recorded_times,
        observations.data,
    )?;
    commit_pre_params_after_event(ctx.model, state.current_y, state.params, tol);
    reinitialize_solver_after_time_event(ctx, state, stepper, tol)
}

fn reinitialize_solver_after_time_event<St: SolverStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    stepper: &mut St,
    tol: f64,
) -> Result<(), SimDriverError> {
    let t_right = *state.current_t;
    stepper.settle_algebraics_and_relation_memory(
        state.current_y,
        state.params,
        t_right,
        ctx.model.state_scalar_count(),
        tol,
    )?;
    let (native_y, native_dy) = stepper.reset_vectors(state.current_y, state.params, t_right)?;
    stepper.reset(
        &native_y,
        &native_dy,
        state.params,
        *state.current_t,
        rumoca_solver::event_solver_step_cap(ctx.opts.dt),
    )
}

fn advance_to_target_once<St: SolverStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    event_stop: Option<RuntimeEventStop>,
    stepper: &mut St,
    deferred_root: &mut Option<f64>,
) -> Result<bool, SimDriverError> {
    if event_stop.is_some() {
        return advance_to_scheduled_stop(ctx, state, target, stepper);
    }
    advance_output_interval(ctx, state, target, stepper, deferred_root)
}

fn advance_to_scheduled_stop<St: SolverStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    stepper: &mut St,
) -> Result<bool, SimDriverError> {
    if stepper.time() > target {
        stepper.state_mut_back(target)?;
    }
    if sample_time_match_with_tol(stepper.time(), target) {
        *state.current_t = target;
        state
            .params
            .copy_from_slice(ctx.runtime_params.borrow().as_slice());
        let native = stepper.native_y();
        write_full_y(stepper, &native, target, state.current_y, state.params)?;
        return Ok(false);
    }
    stepper.set_stop_time(target)?;
    loop {
        let outcome = match stepper.step() {
            Ok(outcome) => outcome,
            Err(e) => {
                stepper.trace_step_failure(
                    state.current_y,
                    state.params,
                    *state.current_t,
                    stepper.time(),
                    &e.to_string(),
                );
                return Err(e);
            }
        };
        match outcome {
            StepOutcome::Stop => {
                let stop_t = stepper.time();
                *state.current_t = stop_t;
                state
                    .params
                    .copy_from_slice(ctx.runtime_params.borrow().as_slice());
                let native = stepper.native_y();
                write_full_y(stepper, &native, stop_t, state.current_y, state.params)?;
                return Ok(false);
            }
            StepOutcome::Internal => continue,
            StepOutcome::Root { t_root } => {
                trace_step_event("scheduled-root", stepper.time(), Some(t_root));
                return handle_root_crossing(ctx, state, t_root, target, stepper);
            }
        }
    }
}

fn advance_output_interval<St: SolverStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    stepper: &mut St,
    deferred_root: &mut Option<f64>,
) -> Result<bool, SimDriverError> {
    // Backends whose interpolation re-projects algebraics (reduced-state) ask to
    // land exactly on each output point near discontinuities; otherwise we keep
    // free dense-output stepping so a multi-step controller is not starved by a
    // fine output grid.
    if stepper.prefer_exact_output_steps() {
        return advance_output_interval_clamped(ctx, state, target, stepper);
    }
    loop {
        if stepper.time() >= target {
            let y_at_target = stepper.interpolate(target)?;
            *state.current_t = target;
            state
                .params
                .copy_from_slice(ctx.runtime_params.borrow().as_slice());
            write_full_y(stepper, &y_at_target, target, state.current_y, state.params)?;
            return Ok(false);
        }
        let outcome = match stepper.step() {
            Ok(outcome) => outcome,
            Err(e) => {
                stepper.trace_step_failure(
                    state.current_y,
                    state.params,
                    *state.current_t,
                    stepper.time(),
                    &e.to_string(),
                );
                return Err(e);
            }
        };
        match outcome {
            StepOutcome::Stop | StepOutcome::Internal => {}
            StepOutcome::Root { t_root } => {
                trace_step_event("output-root", stepper.time(), Some(t_root));
                let root_after_target =
                    t_root > target && !sample_time_match_with_tol(t_root, target);
                if !root_after_target {
                    return handle_root_crossing(ctx, state, t_root, target, stepper);
                }
                let y_at_target = stepper.interpolate(target)?;
                *state.current_t = target;
                state
                    .params
                    .copy_from_slice(ctx.runtime_params.borrow().as_slice());
                write_full_y(stepper, &y_at_target, target, state.current_y, state.params)?;
                *deferred_root = Some(t_root);
                return Ok(false);
            }
        }
    }
}

fn advance_output_interval_clamped<St: SolverStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    stepper: &mut St,
) -> Result<bool, SimDriverError> {
    if stepper.time() >= target {
        let y_at_target = stepper.interpolate(target)?;
        *state.current_t = target;
        state
            .params
            .copy_from_slice(ctx.runtime_params.borrow().as_slice());
        write_full_y(stepper, &y_at_target, target, state.current_y, state.params)?;
        return Ok(false);
    }
    stepper.set_stop_time(target)?;
    loop {
        let outcome = match stepper.step() {
            Ok(outcome) => outcome,
            Err(e) => {
                stepper.trace_step_failure(
                    state.current_y,
                    state.params,
                    *state.current_t,
                    stepper.time(),
                    &e.to_string(),
                );
                return Err(e);
            }
        };
        match outcome {
            StepOutcome::Stop => {
                let stop_t = stepper.time();
                *state.current_t = stop_t;
                state
                    .params
                    .copy_from_slice(ctx.runtime_params.borrow().as_slice());
                let native = stepper.native_y();
                write_full_y(stepper, &native, stop_t, state.current_y, state.params)?;
                return Ok(false);
            }
            StepOutcome::Internal => continue,
            StepOutcome::Root { t_root } => {
                trace_step_event("output-root-clamped", stepper.time(), Some(t_root));
                return handle_root_crossing(ctx, state, t_root, target, stepper);
            }
        }
    }
}

fn handle_root_crossing<St: SolverStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    t_root: f64,
    target: f64,
    stepper: &mut St,
) -> Result<bool, SimDriverError> {
    let tol = ctx.opts.atol.max(1.0e-10);
    stepper.state_mut_back(t_root)?;
    let root_t = stepper.time();
    let native_at_root = stepper.native_y();
    let event_pre_p = ctx.runtime_params.borrow().as_slice().to_vec();
    let right_t = runtime_root_event_application_time(root_t, target);
    *state.current_t = right_t;
    state
        .params
        .copy_from_slice(ctx.runtime_params.borrow().as_slice());
    let mut event_pre_y = vec![0.0; state.current_y.len()];
    write_full_y(stepper, &native_at_root, root_t, &mut event_pre_y, state.params)?;
    state.current_y.copy_from_slice(&event_pre_y);
    stepper.advance_state_to_event_limits(
        &mut event_pre_y,
        state.current_y,
        state.params,
        root_t,
        right_t,
    )?;
    stepper.apply_event_updates_with_event_pre(
        state.current_y,
        state.params,
        *state.current_t,
        tol,
        &event_pre_y,
        &event_pre_p,
    )?;
    stepper.settle_prepared_state(state.current_y, state.params, *state.current_t)?;
    commit_pre_params_after_event(ctx.model, state.current_y, state.params, tol);
    stepper.trace_post_event_state(state.current_y, state.params, *state.current_t);
    let (native_y, native_dy) =
        stepper.reset_vectors(state.current_y, state.params, *state.current_t)?;
    stepper.reset(
        &native_y,
        &native_dy,
        state.params,
        *state.current_t,
        rumoca_solver::event_solver_step_cap(ctx.opts.dt),
    )?;
    Ok(true)
}

fn trace_step_event(kind: &str, solver_t: f64, root_t: Option<f64>) {
    if !tracing::enabled!(target: EVENT_TRACE_TARGET, tracing::Level::DEBUG) {
        return;
    }
    tracing::debug!(target: EVENT_TRACE_TARGET, "{kind} solver_t={solver_t:.12} root_t={root_t:?}");
}
