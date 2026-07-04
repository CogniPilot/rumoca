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
    EventActionOutcome, EventPreMode, RuntimeEventBoundary, RuntimeEventBoundaryHandler,
    RuntimeEventStop, RuntimeSolveError, SimOptions, SolveStopSchedule,
    commit_pre_params_after_event, process_runtime_event_boundary, runtime_event_horizon,
    runtime_root_event_application_time,
    timeline::{event_left_limit_time, sample_time_match_with_tol},
};

use crate::{
    EventUpdateRowFilter, ProjectedEventUpdateInput, SimulationRuntimeState, SolveRuntime,
    next_runtime_event_stop,
};

const EVENT_UPDATE_MAX_ITERS: usize = 256;

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

    // --- the two backend-specific event primitives the shared event kernels
    //     need; everything else (apply_projected_event_update, settle, the
    //     left/right-limit sequencing via `process_runtime_event_boundary`) is
    //     shared in eval-solve ---
    /// Recover the algebraics for the current state (backend projection): the
    /// mass-matrix projection plan (diffsol) or a re-solve from state (rk-like).
    /// Returns whether any value changed (drives the event/settle fixpoint).
    fn project_algebraics(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
    ) -> Result<bool, RuntimeSolveError>;
    /// Full-solver_y derivative guess `dy` used to advance state across the
    /// [root, right-limit] interval (mass-matrix solve for diffsol; `[der; 0]`
    /// for an explicit reduced-state backend).
    fn derivative_guess(&self, y: &[f64], p: &[f64], t: f64) -> Result<Vec<f64>, SimDriverError>;

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
    runtime: &'a SolveRuntime,
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
    pub runtime: &'a SolveRuntime,
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
    let runtime = state.runtime;
    let runtime_state = state.runtime_state;
    let mut stop_schedule = SolveStopSchedule::new(&model.problem, opts.t_start, opts.t_end);
    let mut pending_root_t: Option<f64> = None;
    let make_ctx = || AdvanceContext {
        model,
        opts,
        runtime,
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

/// Map an `EventActionOutcome` (Modelica `assert`/`terminate`) to the driver error.
fn event_action_to_result(outcome: EventActionOutcome, t: f64) -> Result<(), SimDriverError> {
    match outcome {
        EventActionOutcome::Continue => Ok(()),
        EventActionOutcome::AssertionFailed { message, .. } => Err(SimDriverError::Backend(
            format!("Modelica assert failed at t={t:.9}: {message}"),
        )),
        EventActionOutcome::Terminated { message, .. } => {
            Err(SimDriverError::Terminated { time: t, message })
        }
    }
}

/// Frozen `pre()` snapshot for a discrete-event update.
struct EventPre<'a> {
    y: &'a [f64],
    p: &'a [f64],
}

/// Apply the projected discrete-event update at `t`, using the backend's
/// `project_algebraics` as the projection callback (shared by every backend).
fn apply_event_update_kernel<St: SolverStepper + ?Sized>(
    runtime: &SolveRuntime,
    stepper: &St,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
    pre: EventPre<'_>,
) -> Result<(), SimDriverError> {
    let outcome = runtime.apply_projected_event_update(
        ProjectedEventUpdateInput {
            y,
            p,
            t,
            tol,
            event_pre_y: pre.y,
            event_pre_p: pre.p,
            max_iters: EVENT_UPDATE_MAX_ITERS,
            row_filter: EventUpdateRowFilter::All,
            root_relation_overrides: &[],
        },
        |y, p| stepper.project_algebraics(y, p, t, tol),
    )?;
    event_action_to_result(outcome, t)
}

/// Re-settle the projected runtime + relation memory at `t`.
fn settle_kernel<St: SolverStepper + ?Sized>(
    runtime: &SolveRuntime,
    stepper: &St,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), SimDriverError> {
    runtime.settle_projected_runtime_and_relation_memory(
        y,
        p,
        t,
        tol,
        EVENT_UPDATE_MAX_ITERS,
        |y, p| stepper.project_algebraics(y, p, t, tol),
    )?;
    Ok(())
}

/// Bracket a zero-crossing: extrapolate `event_pre_y` to the left limit and `y`
/// to the right limit across the tiny `[root_t, right_t]` interval, using the
/// backend's full-solver_y derivative guess.
fn bracket_event_limits_kernel<St: SolverStepper + ?Sized>(
    stepper: &St,
    event_pre_y: &mut [f64],
    y: &mut [f64],
    p: &[f64],
    root_t: f64,
    right_t: f64,
) -> Result<(), SimDriverError> {
    let dt = right_t - root_t;
    if dt <= 0.0 || sample_time_match_with_tol(root_t, right_t) {
        return Ok(());
    }
    let dy = stepper.derivative_guess(y, p, root_t)?;
    for (slot, d) in event_pre_y.iter_mut().zip(dy.iter().copied()) {
        *slot -= dt * d;
    }
    for (slot, d) in y.iter_mut().zip(dy) {
        *slot += dt * d;
    }
    Ok(())
}

/// Transient [`RuntimeEventBoundaryHandler`] that applies the Modelica left/right
/// limit event semantics using only the backend-neutral kernels + the stepper's
/// `project_algebraics` / `derivative_guess` / `refresh_observation` /
/// `record_sample`. Both the diffsol and rk-like backends drive events through
/// this same handler.
struct EventBoundary<'a, St: SolverStepper + ?Sized> {
    stepper: &'a St,
    runtime: &'a SolveRuntime,
    y: &'a mut [f64],
    p: &'a mut [f64],
    event_pre_y: Vec<f64>,
    event_pre_p: Vec<f64>,
    tol: f64,
    /// `Some(root_t)` for a zero-crossing (bracket the continuous state across
    /// `[root_t, right_t]` and settle); `None` for a scheduled time event
    /// (already at the event instant, apply at the left limit too).
    root_t: Option<f64>,
    /// Observation buffers for events that fall on the output schedule; absent
    /// for zero-crossings between output points.
    recorded_times: Option<&'a mut Vec<f64>>,
    data: Option<&'a mut [Vec<f64>]>,
}

impl<St: SolverStepper + ?Sized> EventBoundary<'_, St> {
    fn record(&mut self, t: f64) -> Result<(), SimDriverError> {
        self.stepper.refresh_observation(self.y, self.p, t)?;
        if let (Some(times), Some(data)) =
            (self.recorded_times.as_deref_mut(), self.data.as_deref_mut())
        {
            self.stepper.record_sample(times, data, self.y, self.p, t)?;
        }
        Ok(())
    }
}

impl<St: SolverStepper + ?Sized> RuntimeEventBoundaryHandler for EventBoundary<'_, St> {
    type Error = SimDriverError;

    fn on_event_time(
        &mut self,
        event_t: f64,
        event: RuntimeEventStop,
    ) -> Result<(), SimDriverError> {
        if self.root_t.is_none() {
            // Scheduled time event: prepare the fixed left limit, freeze `pre`,
            // and apply the event at the left limit (event_t).
            if matches!(
                event.pre_mode,
                EventPreMode::EventEntry | EventPreMode::Fixed
            ) {
                let left_t = event_left_limit_time(event_t);
                self.stepper.refresh_observation(self.y, self.p, left_t)?;
                self.stepper
                    .project_algebraics(self.y, self.p, left_t, self.tol)?;
            }
            self.event_pre_y.copy_from_slice(self.y);
            self.event_pre_p.copy_from_slice(self.p);
            apply_event_update_kernel(
                self.runtime,
                self.stepper,
                self.y,
                self.p,
                event_t,
                self.tol,
                EventPre {
                    y: &self.event_pre_y,
                    p: &self.event_pre_p,
                },
            )?;
            self.record(event_t)?;
        }
        Ok(())
    }

    fn on_event_right_limit(
        &mut self,
        right_t: f64,
        _event: RuntimeEventStop,
    ) -> Result<(), SimDriverError> {
        if let Some(root_t) = self.root_t {
            bracket_event_limits_kernel(
                self.stepper,
                &mut self.event_pre_y,
                self.y,
                self.p,
                root_t,
                right_t,
            )?;
        }
        apply_event_update_kernel(
            self.runtime,
            self.stepper,
            self.y,
            self.p,
            right_t,
            self.tol,
            EventPre {
                y: &self.event_pre_y,
                p: &self.event_pre_p,
            },
        )?;
        if self.root_t.is_some() {
            settle_kernel(
                self.runtime,
                self.stepper,
                self.y,
                self.p,
                right_t,
                self.tol,
            )?;
        } else {
            self.record(right_t)?;
        }
        Ok(())
    }
}

fn refresh_interpolated_sample_state<St: SolverStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    stepper: &mut St,
) -> Result<(), SimDriverError> {
    settle_kernel(
        ctx.runtime,
        stepper,
        state.current_y,
        state.params,
        target,
        ctx.opts.atol.max(1.0e-10),
    )?;
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
    let event_pre_y = vec![0.0; state.current_y.len()];
    let event_pre_p = vec![0.0; state.params.len()];
    let outcome = {
        let mut handler = EventBoundary {
            stepper: &*stepper,
            runtime: ctx.runtime,
            y: state.current_y,
            p: state.params,
            event_pre_y,
            event_pre_p,
            tol,
            root_t: None,
            recorded_times: Some(observations.recorded_times),
            data: Some(observations.data),
        };
        process_runtime_event_boundary(
            RuntimeEventBoundary {
                event_t: *state.current_t,
                horizon_t: runtime_event_horizon(event, target, ctx.opts.t_end),
                event,
            },
            &mut handler,
        )?
    };
    *state.current_t = outcome.final_t;
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
    settle_kernel(
        ctx.runtime,
        stepper,
        state.current_y,
        state.params,
        t_right,
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
    // A zero-crossing is a single-apply event: pin to the root, bracket the
    // continuous state across [root_t, right_t], apply at the right limit, and
    // settle — all via the backend-neutral kernels (shared with the scheduled
    // path and every backend through the stepper callbacks).
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
    write_full_y(
        stepper,
        &native_at_root,
        root_t,
        &mut event_pre_y,
        state.params,
    )?;
    state.current_y.copy_from_slice(&event_pre_y);
    bracket_event_limits_kernel(
        stepper,
        &mut event_pre_y,
        state.current_y,
        state.params,
        root_t,
        right_t,
    )?;
    apply_event_update_kernel(
        ctx.runtime,
        stepper,
        state.current_y,
        state.params,
        right_t,
        tol,
        EventPre {
            y: &event_pre_y,
            p: &event_pre_p,
        },
    )?;
    settle_kernel(
        ctx.runtime,
        stepper,
        state.current_y,
        state.params,
        right_t,
        tol,
    )?;
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
