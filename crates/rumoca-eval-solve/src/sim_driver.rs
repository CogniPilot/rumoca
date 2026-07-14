//! Backend-neutral simulation output / event / root driver.
//!
//! This is the simulation orchestration shared by ODE backends: it walks the
//! output grid, lets the solver step (free dense-output or clamped onto each
//! output point), locates zero-crossing roots, applies scheduled time events,
//! and records the visible trajectory. Everything solver-specific — taking a
//! step, interpolating, the native-state↔full-solver_y mapping, the post-event
//! reset, the algebraic-projection event kernels — is delegated to a
//! [`SolverAdvanceBackend`]. The driver therefore carries no backend types and is
//! identical for the implicit (diffsol) and explicit (rk-like) solvers; each
//! backend only provides a `SolverAdvanceBackend` adapter.

use std::cell::RefCell;
use std::collections::BTreeSet;
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
    Root {
        t_root: f64,
        root_indices: Vec<usize>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum RootStartBoundary<'a> {
    Scheduled,
    Root(&'a [(usize, f64)]),
}

/// Error surfaced by the driver. Backend (`SolverAdvanceBackend`) failures arrive as
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
pub trait SolverAdvanceBackend {
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
        boundary: RootStartBoundary<'_>,
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

#[derive(Clone)]
struct PendingRoot {
    t: f64,
    indices: Vec<usize>,
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
fn write_full_y<St: SolverAdvanceBackend + ?Sized>(
    backend: &St,
    native: &[f64],
    t: f64,
    current_y: &mut [f64],
    params: &[f64],
) -> Result<(), SimDriverError> {
    let full = backend.native_to_full_y(native, t, params)?;
    current_y.copy_from_slice(&full);
    Ok(())
}

// SPEC_0021: Exception - central event loop keeps step advancement,
// zero-crossing handling, and sample recording in a single ordered routine.
#[allow(clippy::too_many_lines)]
pub fn simulate_state_targets<St: SolverAdvanceBackend + ?Sized>(
    model: &solve::SolveModel,
    opts: &SimOptions,
    times: &[f64],
    runtime_params: &RuntimeParameters,
    backend: &mut St,
    state: StateTrajectory<'_>,
) -> Result<(), SimDriverError> {
    let runtime = state.runtime;
    let runtime_state = state.runtime_state;
    let mut stop_schedule = SolveStopSchedule::new(&model.problem, opts.t_start, opts.t_end);
    let mut pending_root: Option<PendingRoot> = None;
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
                &mut pending_root,
                make_ctx(),
                AdvanceState {
                    current_y: state.current_y,
                    params: state.params,
                    current_t: state.current_t,
                },
                target,
                backend,
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
            // Time-event equations may select their post-event branch at the
            // exact boundary. Stop continuous integration at the previous
            // representable instant so an implicit solver never evaluates the
            // discontinuous right-side RHS before the event update is applied.
            let solver_stop_time =
                event_stop.map_or(stop_time, |_| event_left_limit_time(stop_time));
            let mut deferred_root: Option<PendingRoot> = None;
            let hit_root = advance_to_target_once(
                make_ctx(),
                AdvanceState {
                    current_y: state.current_y,
                    params: state.params,
                    current_t: state.current_t,
                },
                solver_stop_time,
                event_stop,
                backend,
                &mut deferred_root,
            )?;
            if let Some(root) = deferred_root {
                pending_root = Some(root);
            }
            let event_stop_reached = event_stop.is_some()
                && sample_time_match_with_tol(*state.current_t, solver_stop_time);
            if let Some(event) = event_stop
                && event_stop_reached
                && !hit_root
            {
                *state.current_t = stop_time;
                apply_scheduled_time_event(
                    make_ctx(),
                    AdvanceState {
                        current_y: state.current_y,
                        params: state.params,
                        current_t: state.current_t,
                    },
                    event,
                    target,
                    backend,
                    ObservationBuffers {
                        recorded_times: state.recorded_times,
                        data: state.data,
                    },
                )?;
                // A deferred root belongs to the continuous trajectory that
                // the scheduled-event reset just replaced.
                pending_root = None;
            }
            if event_stop_reached {
                stop_schedule.advance_past(*state.current_t);
            }
            if !hit_root && event_stop.is_none() {
                break;
            }
        }
        backend.refresh_observation(state.current_y, state.params, *state.current_t)?;
        runtime_params.borrow_mut().copy_from_slice(state.params);
        backend.record_sample(
            state.recorded_times,
            state.data,
            state.current_y,
            state.params,
            *state.current_t,
        )?;
    }

    Ok(())
}

fn resolve_pending_root<St: SolverAdvanceBackend + ?Sized>(
    pending_root: &mut Option<PendingRoot>,
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    backend: &mut St,
    stop_schedule: &mut SolveStopSchedule,
) -> Result<PendingRootAction, SimDriverError> {
    let Some(root) = pending_root.clone() else {
        return Ok(PendingRootAction::None);
    };
    let prt = root.t;
    if !sample_time_match_with_tol(target, prt) && target < prt {
        let y_at = backend.interpolate(target)?;
        *state.current_t = target;
        state
            .params
            .copy_from_slice(ctx.runtime_params.borrow().as_slice());
        write_full_y(backend, &y_at, target, state.current_y, state.params)?;
        refresh_interpolated_sample_state(ctx, state, target, backend)?;
        return Ok(PendingRootAction::Break);
    }

    *pending_root = None;
    handle_root_crossing(ctx, state, prt, &root.indices, target, backend)?;
    stop_schedule.advance_past(backend.time());
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
fn apply_event_update_kernel<St: SolverAdvanceBackend + ?Sized>(
    runtime: &SolveRuntime,
    backend: &St,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
    root_relation_overrides: &[(usize, f64)],
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
            root_relation_overrides,
        },
        |y, p| backend.project_algebraics(y, p, t, tol),
    )?;
    event_action_to_result(outcome, t)
}

/// Re-settle the projected runtime + relation memory at `t`.
fn settle_kernel<St: SolverAdvanceBackend + ?Sized>(
    runtime: &SolveRuntime,
    backend: &St,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
    root_relation_overrides: &[(usize, f64)],
) -> Result<(), SimDriverError> {
    runtime.settle_projected_runtime_and_relation_memory_with_overrides(
        y,
        p,
        t,
        tol,
        EVENT_UPDATE_MAX_ITERS,
        root_relation_overrides,
        |y, p| backend.project_algebraics(y, p, t, tol),
    )?;
    Ok(())
}

/// Bracket a zero-crossing: extrapolate `event_pre_y` to the left limit and `y`
/// to the right limit across the tiny `[root_t, right_t]` interval, using the
/// backend's full-solver_y derivative guess.
fn bracket_event_limits_kernel<St: SolverAdvanceBackend + ?Sized>(
    backend: &St,
    event_pre_y: &mut [f64],
    y: &mut [f64],
    p: &[f64],
    root_t: f64,
    right_t: f64,
) -> Result<(), SimDriverError> {
    let dt = right_t - root_t;
    if dt <= 0.0 {
        return Ok(());
    }
    let dy = backend.derivative_guess(y, p, root_t)?;
    for (slot, d) in event_pre_y.iter_mut().zip(dy.iter().copied()) {
        *slot -= dt * d;
    }
    for (slot, d) in y.iter_mut().zip(dy) {
        *slot += dt * d;
    }
    Ok(())
}

/// Transient [`RuntimeEventBoundaryHandler`] that applies the Modelica left/right
/// limit event semantics using only the backend-neutral kernels + the backend's
/// `project_algebraics` / `derivative_guess` / `refresh_observation` /
/// `record_sample`. Both the diffsol and rk-like backends drive events through
/// this same handler.
struct EventBoundary<'a, St: SolverAdvanceBackend + ?Sized> {
    backend: &'a St,
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

impl<St: SolverAdvanceBackend + ?Sized> EventBoundary<'_, St> {
    fn record(&mut self, t: f64) -> Result<(), SimDriverError> {
        self.backend.refresh_observation(self.y, self.p, t)?;
        if let (Some(times), Some(data)) =
            (self.recorded_times.as_deref_mut(), self.data.as_deref_mut())
        {
            self.backend.record_sample(times, data, self.y, self.p, t)?;
        }
        Ok(())
    }
}

impl<St: SolverAdvanceBackend + ?Sized> RuntimeEventBoundaryHandler for EventBoundary<'_, St> {
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
                self.backend.refresh_observation(self.y, self.p, left_t)?;
                self.backend
                    .project_algebraics(self.y, self.p, left_t, self.tol)?;
            }
            self.event_pre_y.copy_from_slice(self.y);
            self.event_pre_p.copy_from_slice(self.p);
            apply_event_update_kernel(
                self.runtime,
                self.backend,
                self.y,
                self.p,
                event_t,
                self.tol,
                &[],
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
                self.backend,
                &mut self.event_pre_y,
                self.y,
                self.p,
                root_t,
                right_t,
            )?;
        }
        apply_event_update_kernel(
            self.runtime,
            self.backend,
            self.y,
            self.p,
            right_t,
            self.tol,
            &[],
            EventPre {
                y: &self.event_pre_y,
                p: &self.event_pre_p,
            },
        )?;
        if self.root_t.is_some() {
            settle_kernel(
                self.runtime,
                self.backend,
                self.y,
                self.p,
                right_t,
                self.tol,
                &[],
            )?;
        } else {
            self.record(right_t)?;
        }
        Ok(())
    }
}

fn refresh_interpolated_sample_state<St: SolverAdvanceBackend + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    backend: &mut St,
) -> Result<(), SimDriverError> {
    settle_kernel(
        ctx.runtime,
        backend,
        state.current_y,
        state.params,
        target,
        ctx.opts.atol.max(1.0e-10),
        &[],
    )?;
    backend.refresh_observation(state.current_y, state.params, target)?;
    ctx.runtime_params
        .borrow_mut()
        .copy_from_slice(state.params);
    Ok(())
}

fn apply_scheduled_time_event<St: SolverAdvanceBackend + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    event: RuntimeEventStop,
    target: f64,
    backend: &mut St,
    observations: ObservationBuffers<'_>,
) -> Result<(), SimDriverError> {
    let tol = ctx.opts.atol.max(1.0e-10);
    let event_pre_y = vec![0.0; state.current_y.len()];
    let event_pre_p = vec![0.0; state.params.len()];
    let outcome = {
        let mut handler = EventBoundary {
            backend: &*backend,
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
    reinitialize_solver_after_time_event(ctx, state, backend, tol)
}

fn reinitialize_solver_after_time_event<St: SolverAdvanceBackend + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    backend: &mut St,
    tol: f64,
) -> Result<(), SimDriverError> {
    let t_right = *state.current_t;
    settle_kernel(
        ctx.runtime,
        backend,
        state.current_y,
        state.params,
        t_right,
        tol,
        &[],
    )?;
    ctx.runtime_params
        .borrow_mut()
        .copy_from_slice(state.params);
    let (native_y, native_dy) = backend.reset_vectors(state.current_y, state.params, t_right)?;
    backend.reset(
        &native_y,
        &native_dy,
        state.params,
        *state.current_t,
        rumoca_solver::event_solver_step_cap(ctx.opts.dt),
        RootStartBoundary::Scheduled,
    )
}

fn advance_to_target_once<St: SolverAdvanceBackend + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    event_stop: Option<RuntimeEventStop>,
    backend: &mut St,
    deferred_root: &mut Option<PendingRoot>,
) -> Result<bool, SimDriverError> {
    if event_stop.is_some() {
        return advance_to_scheduled_stop(ctx, state, target, backend, deferred_root);
    }
    advance_output_interval(ctx, state, target, backend, deferred_root)
}

fn advance_to_scheduled_stop<St: SolverAdvanceBackend + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    backend: &mut St,
    _deferred_root: &mut Option<PendingRoot>,
) -> Result<bool, SimDriverError> {
    // Scheduled equations may switch branch at the exact event time. Clamp the
    // solver to the previous representable instant so it cannot evaluate the
    // post-event RHS before the runtime applies the event update. A root before
    // that stop is processed immediately; the following loop iteration installs
    // a fresh stop on the reset solver, so no stale tstop survives a reset.
    advance_output_interval_clamped(ctx, state, target, backend)
}

fn advance_output_interval<St: SolverAdvanceBackend + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    backend: &mut St,
    deferred_root: &mut Option<PendingRoot>,
) -> Result<bool, SimDriverError> {
    // Backends whose interpolation re-projects algebraics (reduced-state) ask to
    // land exactly on each output point near discontinuities; otherwise we keep
    // free dense-output stepping so a multi-step controller is not starved by a
    // fine output grid.
    if backend.prefer_exact_output_steps() {
        return advance_output_interval_clamped(ctx, state, target, backend);
    }
    loop {
        if backend.time() >= target {
            let y_at_target = backend.interpolate(target)?;
            *state.current_t = target;
            state
                .params
                .copy_from_slice(ctx.runtime_params.borrow().as_slice());
            write_full_y(backend, &y_at_target, target, state.current_y, state.params)?;
            return Ok(false);
        }
        let outcome = match backend.step() {
            Ok(outcome) => outcome,
            Err(e) => {
                backend.trace_step_failure(
                    state.current_y,
                    state.params,
                    *state.current_t,
                    backend.time(),
                    &e.to_string(),
                );
                return Err(e);
            }
        };
        match outcome {
            StepOutcome::Stop | StepOutcome::Internal => {}
            StepOutcome::Root {
                t_root,
                root_indices,
            } => {
                trace_step_event("output-root", backend.time(), Some(t_root));
                let root_after_target =
                    t_root > target && !sample_time_match_with_tol(t_root, target);
                if !root_after_target {
                    return handle_root_crossing(
                        ctx,
                        state,
                        t_root,
                        &root_indices,
                        target,
                        backend,
                    );
                }
                let y_at_target = backend.interpolate(target)?;
                *state.current_t = target;
                state
                    .params
                    .copy_from_slice(ctx.runtime_params.borrow().as_slice());
                write_full_y(backend, &y_at_target, target, state.current_y, state.params)?;
                *deferred_root = Some(PendingRoot {
                    t: t_root,
                    indices: root_indices,
                });
                return Ok(false);
            }
        }
    }
}

fn advance_output_interval_clamped<St: SolverAdvanceBackend + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    backend: &mut St,
) -> Result<bool, SimDriverError> {
    if backend.time() >= target {
        let y_at_target = backend.interpolate(target)?;
        *state.current_t = target;
        state
            .params
            .copy_from_slice(ctx.runtime_params.borrow().as_slice());
        write_full_y(backend, &y_at_target, target, state.current_y, state.params)?;
        return Ok(false);
    }
    backend.set_stop_time(target)?;
    loop {
        let outcome = match backend.step() {
            Ok(outcome) => outcome,
            Err(e) => {
                backend.trace_step_failure(
                    state.current_y,
                    state.params,
                    *state.current_t,
                    backend.time(),
                    &e.to_string(),
                );
                return Err(e);
            }
        };
        match outcome {
            StepOutcome::Stop => {
                let stop_t = backend.time();
                *state.current_t = stop_t;
                state
                    .params
                    .copy_from_slice(ctx.runtime_params.borrow().as_slice());
                let native = backend.native_y();
                write_full_y(backend, &native, stop_t, state.current_y, state.params)?;
                return Ok(false);
            }
            StepOutcome::Internal => continue,
            StepOutcome::Root {
                t_root,
                root_indices,
            } => {
                trace_step_event("output-root-clamped", backend.time(), Some(t_root));
                return handle_root_crossing(ctx, state, t_root, &root_indices, target, backend);
            }
        }
    }
}

fn handle_root_crossing<St: SolverAdvanceBackend + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    t_root: f64,
    root_indices: &[usize],
    target: f64,
    backend: &mut St,
) -> Result<bool, SimDriverError> {
    // A zero-crossing is a single-apply event: pin to the root, bracket the
    // continuous state across [root_t, right_t], apply at the right limit, and
    // settle — all via the backend-neutral kernels (shared with the scheduled
    // path and every backend through the backend callbacks).
    let tol = ctx.opts.atol.max(1.0e-10);
    let solver_t = backend.time();
    let pin_t = if t_root > solver_t {
        let root_pin_tol = tol * (1.0 + t_root.abs().max(solver_t.abs()));
        if (t_root - solver_t).abs() <= root_pin_tol {
            solver_t
        } else {
            return Err(SimDriverError::Backend(format!(
                "root time {t_root:.12} is after backend time {solver_t:.12}"
            )));
        }
    } else {
        t_root
    };
    backend.state_mut_back(pin_t)?;
    let root_t = backend.time();
    let native_at_root = backend.native_y();
    let event_pre_p = ctx.runtime_params.borrow().as_slice().to_vec();
    let right_t = runtime_root_event_application_time(root_t, target);
    *state.current_t = right_t;
    state
        .params
        .copy_from_slice(ctx.runtime_params.borrow().as_slice());
    let mut event_pre_y = vec![0.0; state.current_y.len()];
    write_full_y(
        backend,
        &native_at_root,
        root_t,
        &mut event_pre_y,
        state.params,
    )?;
    state.current_y.copy_from_slice(&event_pre_y);
    let root_relation_overrides =
        post_root_relation_overrides(ctx.model, root_indices, &event_pre_p, tol)?;
    bracket_event_limits_kernel(
        backend,
        &mut event_pre_y,
        state.current_y,
        state.params,
        root_t,
        right_t,
    )?;
    apply_event_update_kernel(
        ctx.runtime,
        backend,
        state.current_y,
        state.params,
        right_t,
        tol,
        &root_relation_overrides,
        EventPre {
            y: &event_pre_y,
            p: &event_pre_p,
        },
    )?;
    settle_kernel(
        ctx.runtime,
        backend,
        state.current_y,
        state.params,
        right_t,
        tol,
        &root_relation_overrides,
    )?;
    commit_pre_params_after_event(ctx.model, state.current_y, state.params, tol);
    ctx.runtime_params
        .borrow_mut()
        .copy_from_slice(state.params);
    backend.trace_post_event_state(state.current_y, state.params, *state.current_t);
    let (native_y, native_dy) =
        backend.reset_vectors(state.current_y, state.params, *state.current_t)?;
    backend.reset(
        &native_y,
        &native_dy,
        state.params,
        *state.current_t,
        rumoca_solver::event_solver_step_cap(ctx.opts.dt),
        RootStartBoundary::Root(&root_relation_overrides),
    )?;
    Ok(true)
}

pub fn post_root_relation_overrides(
    model: &solve::SolveModel,
    root_indices: &[usize],
    params: &[f64],
    tol: f64,
) -> Result<Vec<(usize, f64)>, RuntimeSolveError> {
    let root_count = model.problem.events.root_conditions.output_count();
    let targets = &model.problem.events.root_relation_memory_targets;
    if root_count != targets.len() {
        return Err(RuntimeSolveError::solve_ir(format!(
            "root relation metadata length {} does not match root output count {root_count}",
            targets.len()
        )));
    }
    let mut overrides = Vec::new();
    for root_index in root_indices.iter().copied().collect::<BTreeSet<_>>() {
        if root_index >= root_count || root_index >= targets.len() {
            return Err(RuntimeSolveError::solve_ir(format!(
                "root crossing index {root_index} is outside root metadata (roots={root_count}, targets={})",
                targets.len()
            )));
        }
        let Some(target) = targets[root_index] else {
            continue;
        };
        let solve::ScalarSlot::P { index, .. } = target else {
            return Err(RuntimeSolveError::solve_ir(format!(
                "root crossing index {root_index} has non-parameter relation memory target"
            )));
        };
        let current = params.get(index).copied().ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "root crossing index {root_index} relation memory parameter {index} is outside parameter storage"
            ))
        })?;
        let post = if current.abs() <= tol {
            1.0
        } else if (current - 1.0).abs() <= tol {
            0.0
        } else {
            return Err(RuntimeSolveError::solve_ir(format!(
                "root crossing index {root_index} relation memory value {current} is not boolean"
            )));
        };
        overrides.push((root_index, post));
    }
    Ok(overrides)
}

fn trace_step_event(kind: &str, solver_t: f64, root_t: Option<f64>) {
    if !tracing::enabled!(target: EVENT_TRACE_TARGET, tracing::Level::DEBUG) {
        return;
    }
    tracing::debug!(target: EVENT_TRACE_TARGET, "{kind} solver_t={solver_t:.12} root_t={root_t:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    fn relation_root_model(target: Option<solve::ScalarSlot>) -> solve::SolveModel {
        let mut model = solve::SolveModel::default();
        model.problem.events.root_conditions = solve::ScalarProgramBlock {
            programs: vec![vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            program_spans: vec![rumoca_core::Span::DUMMY],
            output_indices: vec![0],
        };
        model.problem.events.root_relation_memory_targets = vec![target];
        model
    }

    #[test]
    fn root_relation_overrides_deduplicate_indices_and_toggle_parameter_memory() {
        let model = relation_root_model(Some(solve::scalar_slot_p(0)));

        let overrides = post_root_relation_overrides(&model, &[0, 0], &[0.0], 1.0e-12)
            .expect("confirmed root should toggle relation memory exactly once");

        assert_eq!(overrides, vec![(0, 1.0)]);
    }

    #[test]
    fn root_relation_overrides_fail_closed_for_non_parameter_target() {
        let model = relation_root_model(Some(solve::scalar_slot_y(0)));

        let error = post_root_relation_overrides(&model, &[0], &[0.0], 1.0e-12)
            .expect_err("non-parameter relation memory must not infer a crossing direction");

        assert!(error.to_string().contains("non-parameter"));
    }

    enum TrajectoryStep {
        Internal {
            solver_t: f64,
        },
        Stop {
            solver_t: f64,
        },
        Root {
            solver_t: f64,
            t_root: f64,
            root_indices: Vec<usize>,
        },
    }

    struct TrajectoryGenerationBackend {
        time: f64,
        steps: std::collections::VecDeque<TrajectoryStep>,
        generation: usize,
        step_generations: Vec<usize>,
        pinned_times: Vec<f64>,
        stop_times: Vec<f64>,
        reset_count: usize,
        exact_output_steps: bool,
    }

    impl TrajectoryGenerationBackend {
        fn new(steps: impl IntoIterator<Item = TrajectoryStep>) -> Self {
            Self {
                time: 0.0,
                steps: steps.into_iter().collect(),
                generation: 0,
                step_generations: Vec::new(),
                pinned_times: Vec::new(),
                stop_times: Vec::new(),
                reset_count: 0,
                exact_output_steps: false,
            }
        }
    }

    impl SolverAdvanceBackend for TrajectoryGenerationBackend {
        fn time(&self) -> f64 {
            self.time
        }

        fn native_y(&self) -> Vec<f64> {
            Vec::new()
        }

        fn step(&mut self) -> Result<StepOutcome, SimDriverError> {
            let step = self
                .steps
                .pop_front()
                .ok_or_else(|| SimDriverError::Backend("test backend ran out of steps".into()))?;
            self.step_generations.push(self.generation);
            match step {
                TrajectoryStep::Internal { solver_t } => {
                    self.time = solver_t;
                    Ok(StepOutcome::Internal)
                }
                TrajectoryStep::Stop { solver_t } => {
                    self.time = solver_t;
                    Ok(StepOutcome::Stop)
                }
                TrajectoryStep::Root {
                    solver_t,
                    t_root,
                    root_indices,
                } => {
                    self.time = solver_t;
                    Ok(StepOutcome::Root {
                        t_root,
                        root_indices,
                    })
                }
            }
        }

        fn set_stop_time(&mut self, stop_time: f64) -> Result<(), SimDriverError> {
            self.stop_times.push(stop_time);
            Ok(())
        }

        fn interpolate(&mut self, t: f64) -> Result<Vec<f64>, SimDriverError> {
            if t > self.time && !sample_time_match_with_tol(t, self.time) {
                return Err(SimDriverError::Backend(format!(
                    "cannot interpolate forward from {:.12} to {t:.12}",
                    self.time
                )));
            }
            Ok(Vec::new())
        }

        fn state_mut_back(&mut self, t: f64) -> Result<(), SimDriverError> {
            if t > self.time && !sample_time_match_with_tol(t, self.time) {
                return Err(SimDriverError::Backend(format!(
                    "cannot pin forward from {:.12} to {t:.12}",
                    self.time
                )));
            }
            self.time = t;
            self.pinned_times.push(t);
            Ok(())
        }

        fn native_to_full_y(
            &self,
            native: &[f64],
            _t: f64,
            _params: &[f64],
        ) -> Result<Vec<f64>, SimDriverError> {
            Ok(native.to_vec())
        }

        fn reset_vectors(
            &self,
            current_y: &[f64],
            _params: &[f64],
            _t: f64,
        ) -> Result<(Vec<f64>, Vec<f64>), SimDriverError> {
            Ok((current_y.to_vec(), vec![0.0; current_y.len()]))
        }

        fn reset(
            &mut self,
            _native_y: &[f64],
            _native_dy: &[f64],
            _params: &[f64],
            t: f64,
            _h_cap: f64,
            _boundary: RootStartBoundary<'_>,
        ) -> Result<(), SimDriverError> {
            self.time = t;
            self.generation += 1;
            self.reset_count += 1;
            Ok(())
        }

        fn prefer_exact_output_steps(&self) -> bool {
            self.exact_output_steps
        }

        fn project_algebraics(
            &self,
            _y: &mut [f64],
            _p: &mut [f64],
            _t: f64,
            _tol: f64,
        ) -> Result<bool, RuntimeSolveError> {
            Ok(false)
        }

        fn derivative_guess(
            &self,
            y: &[f64],
            _p: &[f64],
            _t: f64,
        ) -> Result<Vec<f64>, SimDriverError> {
            Ok(vec![0.0; y.len()])
        }

        fn record_sample(
            &self,
            recorded_times: &mut Vec<f64>,
            _data: &mut [Vec<f64>],
            _y: &[f64],
            _p: &[f64],
            t: f64,
        ) -> Result<(), SimDriverError> {
            recorded_times.push(t);
            Ok(())
        }

        fn refresh_observation(
            &self,
            _y: &mut [f64],
            _p: &mut [f64],
            _t: f64,
        ) -> Result<(), SimDriverError> {
            Ok(())
        }

        fn trace_step_failure(
            &self,
            _y: &[f64],
            _params: &[f64],
            _current_t: f64,
            _solver_t: f64,
            _error: &str,
        ) {
        }

        fn trace_post_event_state(&self, _y: &[f64], _params: &[f64], _t: f64) {}
    }

    fn run_trajectory_driver(
        model: &solve::SolveModel,
        times: &[f64],
        steps: impl IntoIterator<Item = TrajectoryStep>,
        exact_output_steps: bool,
    ) -> (
        Result<(), SimDriverError>,
        TrajectoryGenerationBackend,
        Vec<f64>,
        f64,
        Vec<f64>,
    ) {
        let runtime = SolveRuntime::new(model).expect("empty runtime should prepare");
        let runtime_state = SimulationRuntimeState::new();
        let opts = SimOptions {
            atol: 1.0e-12,
            ..SimOptions::default()
        };
        let runtime_params = Rc::new(RefCell::new(model.parameters.clone()));
        let mut current_y = Vec::new();
        let mut params = model.parameters.clone();
        let mut data = Vec::new();
        let mut recorded_times = Vec::new();
        let mut current_t = 0.0;
        let mut backend = TrajectoryGenerationBackend::new(steps);
        backend.exact_output_steps = exact_output_steps;

        let result = simulate_state_targets(
            model,
            &opts,
            times,
            &runtime_params,
            &mut backend,
            StateTrajectory {
                params: &mut params,
                data: &mut data,
                recorded_times: &mut recorded_times,
                current_t: &mut current_t,
                current_y: &mut current_y,
                runtime: &runtime,
                runtime_state: &runtime_state,
            },
        );

        (result, backend, recorded_times, current_t, params)
    }

    #[test]
    fn scheduled_event_invalidates_deferred_future_root_from_pre_event_trajectory() {
        let mut model = solve::SolveModel::default();
        model.problem.events.scheduled_time_events.push(0.5);
        let left_t = event_left_limit_time(0.5);

        let (result, backend, recorded_times, current_t, _params) = run_trajectory_driver(
            &model,
            &[1.0],
            [
                TrajectoryStep::Stop { solver_t: left_t },
                TrajectoryStep::Internal { solver_t: 1.0 },
            ],
            false,
        );

        result.expect("scheduled reset must replace the prior trajectory at its left limit");
        assert!(backend.pinned_times.is_empty());
        assert_eq!(backend.step_generations, vec![0, 1]);
        assert_eq!(backend.stop_times, vec![left_t]);
        assert_eq!(backend.reset_count, 1);
        assert_eq!(recorded_times.last(), Some(&1.0));
        assert_eq!(current_t, 1.0);
    }

    #[test]
    fn root_before_scheduled_event_reinstalls_stop_after_reset() {
        let mut model = solve::SolveModel::default();
        model.problem.events.scheduled_time_events.push(0.5);
        let left_t = event_left_limit_time(0.5);
        let root_t = 0.49;

        let (result, backend, recorded_times, current_t, _params) = run_trajectory_driver(
            &model,
            &[1.0],
            [
                TrajectoryStep::Root {
                    solver_t: root_t,
                    t_root: root_t,
                    root_indices: Vec::new(),
                },
                TrajectoryStep::Stop { solver_t: left_t },
                TrajectoryStep::Internal { solver_t: 1.0 },
            ],
            false,
        );

        result.expect("root reset before a scheduled event must not retain a stale stop");
        assert_eq!(backend.stop_times, vec![left_t, left_t]);
        assert_eq!(backend.step_generations, vec![0, 1, 2]);
        assert_eq!(backend.reset_count, 2);
        assert_eq!(recorded_times.last(), Some(&1.0));
        assert_eq!(current_t, 1.0);
    }

    #[test]
    fn ordinary_output_boundary_preserves_deferred_future_root() {
        const ROOT_T: f64 = 0.500_033_856_224;
        let model = solve::SolveModel::default();

        let (result, backend, recorded_times, current_t, _params) = run_trajectory_driver(
            &model,
            &[0.5, 1.0],
            [
                TrajectoryStep::Root {
                    solver_t: ROOT_T,
                    t_root: ROOT_T,
                    root_indices: Vec::new(),
                },
                TrajectoryStep::Internal { solver_t: 1.0 },
            ],
            false,
        );

        result.expect("ordinary dense output must preserve the deferred root");
        assert_eq!(backend.pinned_times, vec![ROOT_T]);
        assert_eq!(backend.step_generations, vec![0, 1]);
        assert_eq!(backend.reset_count, 1);
        assert_eq!(recorded_times, vec![0.5, 1.0]);
        assert_eq!(current_t, 1.0);
    }

    #[test]
    fn deferred_root_preserves_relation_override_indices() {
        const ROOT_T: f64 = 0.500_033_856_224;
        let mut model = relation_root_model(Some(solve::scalar_slot_p(0)));
        model.parameters = vec![0.0];

        let (result, _backend, _recorded_times, _current_t, params) = run_trajectory_driver(
            &model,
            &[0.5, 1.0],
            [
                TrajectoryStep::Root {
                    solver_t: ROOT_T,
                    t_root: ROOT_T,
                    root_indices: vec![0],
                },
                TrajectoryStep::Internal { solver_t: 1.0 },
            ],
            false,
        );

        result.expect("deferred root should retain relation override metadata");
        assert_eq!(params, vec![1.0]);
    }

    #[test]
    fn free_and_clamped_root_paths_apply_relation_override_indices() {
        for exact_output_steps in [false, true] {
            let mut model = relation_root_model(Some(solve::scalar_slot_p(0)));
            model.parameters = vec![0.0];
            let followup = if exact_output_steps {
                TrajectoryStep::Stop { solver_t: 1.0 }
            } else {
                TrajectoryStep::Internal { solver_t: 1.0 }
            };

            let (result, _backend, _recorded_times, _current_t, params) = run_trajectory_driver(
                &model,
                &[1.0],
                [
                    TrajectoryStep::Root {
                        solver_t: 0.5,
                        t_root: 0.5,
                        root_indices: vec![0],
                    },
                    followup,
                ],
                exact_output_steps,
            );

            result.expect("root path should apply relation override metadata");
            assert_eq!(params, vec![1.0]);
        }
    }

    struct ForwardInterpolationRejectingBackend {
        time: f64,
        pinned_times: Vec<f64>,
    }

    impl SolverAdvanceBackend for ForwardInterpolationRejectingBackend {
        fn time(&self) -> f64 {
            self.time
        }

        fn native_y(&self) -> Vec<f64> {
            Vec::new()
        }

        fn step(&mut self) -> Result<StepOutcome, SimDriverError> {
            unreachable!("root handler test does not step the backend")
        }

        fn set_stop_time(&mut self, _stop_time: f64) -> Result<(), SimDriverError> {
            unreachable!("root handler test does not set a stop time")
        }

        fn interpolate(&mut self, _t: f64) -> Result<Vec<f64>, SimDriverError> {
            unreachable!("root handler test does not interpolate output")
        }

        fn state_mut_back(&mut self, t: f64) -> Result<(), SimDriverError> {
            if t > self.time {
                return Err(SimDriverError::Backend(
                    "Interpolation time is not within current step".to_string(),
                ));
            }
            self.time = t;
            self.pinned_times.push(t);
            Ok(())
        }

        fn native_to_full_y(
            &self,
            native: &[f64],
            _t: f64,
            _params: &[f64],
        ) -> Result<Vec<f64>, SimDriverError> {
            Ok(native.to_vec())
        }

        fn reset_vectors(
            &self,
            current_y: &[f64],
            _params: &[f64],
            _t: f64,
        ) -> Result<(Vec<f64>, Vec<f64>), SimDriverError> {
            Ok((current_y.to_vec(), vec![0.0; current_y.len()]))
        }

        fn reset(
            &mut self,
            _native_y: &[f64],
            _native_dy: &[f64],
            _params: &[f64],
            t: f64,
            _h_cap: f64,
            _boundary: RootStartBoundary<'_>,
        ) -> Result<(), SimDriverError> {
            self.time = t;
            Ok(())
        }

        fn prefer_exact_output_steps(&self) -> bool {
            false
        }

        fn project_algebraics(
            &self,
            _y: &mut [f64],
            _p: &mut [f64],
            _t: f64,
            _tol: f64,
        ) -> Result<bool, RuntimeSolveError> {
            Ok(false)
        }

        fn derivative_guess(
            &self,
            y: &[f64],
            _p: &[f64],
            _t: f64,
        ) -> Result<Vec<f64>, SimDriverError> {
            Ok(vec![0.0; y.len()])
        }

        fn record_sample(
            &self,
            _recorded_times: &mut Vec<f64>,
            _data: &mut [Vec<f64>],
            _y: &[f64],
            _p: &[f64],
            _t: f64,
        ) -> Result<(), SimDriverError> {
            unreachable!("root handler test does not record samples")
        }

        fn refresh_observation(
            &self,
            _y: &mut [f64],
            _p: &mut [f64],
            _t: f64,
        ) -> Result<(), SimDriverError> {
            unreachable!("root handler test does not refresh observations")
        }

        fn trace_step_failure(
            &self,
            _y: &[f64],
            _params: &[f64],
            _current_t: f64,
            _solver_t: f64,
            _error: &str,
        ) {
        }

        fn trace_post_event_state(&self, _y: &[f64], _params: &[f64], _t: f64) {}
    }

    fn handle_test_root(
        t_root: f64,
    ) -> (
        Result<bool, SimDriverError>,
        ForwardInterpolationRejectingBackend,
        f64,
    ) {
        let model = solve::SolveModel::default();
        let runtime = SolveRuntime::new(&model).expect("empty runtime should prepare");
        let opts = SimOptions {
            atol: 1.0e-12,
            ..SimOptions::default()
        };
        let runtime_params = Rc::new(RefCell::new(Vec::new()));
        let mut current_y = Vec::new();
        let mut params = Vec::new();
        let mut current_t = 1.0;
        let mut backend = ForwardInterpolationRejectingBackend {
            time: 1.0,
            pinned_times: Vec::new(),
        };

        let result = handle_root_crossing(
            AdvanceContext {
                model: &model,
                opts: &opts,
                runtime: &runtime,
                runtime_params: &runtime_params,
            },
            AdvanceState {
                current_y: &mut current_y,
                params: &mut params,
                current_t: &mut current_t,
            },
            t_root,
            &[],
            1.0,
            &mut backend,
        );
        (result, backend, current_t)
    }

    #[test]
    fn near_future_root_within_tolerance_pins_to_backend_time() {
        let (result, backend, current_t) = handle_test_root(1.0 + 5.0e-13);
        let handled = result.expect("near-future root should be handled at backend time");

        assert!(handled);
        assert_eq!(backend.pinned_times, vec![1.0]);
        assert_eq!(current_t, 1.0);
    }

    #[test]
    fn root_handler_applies_relation_override_through_event_update_and_settle() {
        let mut model = relation_root_model(Some(solve::scalar_slot_p(0)));
        model.parameters = vec![0.0, 0.0];
        model.problem.discrete.rhs = solve::ScalarProgramBlock {
            programs: vec![vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            program_spans: vec![rumoca_core::Span::DUMMY],
            output_indices: vec![0],
        };
        model.problem.discrete.update_targets = vec![solve::scalar_slot_p(1)];
        model.problem.discrete.pre_modes = vec![solve::DiscreteEventPreMode::FollowCurrent];
        model.problem.discrete.observation_refresh = vec![false];
        model.problem.solve_layout.relation_memory_parameter_indices = vec![0];
        let runtime = SolveRuntime::new(&model).expect("relation runtime should prepare");
        let opts = SimOptions {
            atol: 1.0e-12,
            ..SimOptions::default()
        };
        let runtime_params = Rc::new(RefCell::new(vec![0.0, 0.0]));
        let mut current_y = Vec::new();
        let mut params = vec![0.0, 0.0];
        let mut current_t = 1.0;
        let mut backend = ForwardInterpolationRejectingBackend {
            time: 1.0,
            pinned_times: Vec::new(),
        };

        handle_root_crossing(
            AdvanceContext {
                model: &model,
                opts: &opts,
                runtime: &runtime,
                runtime_params: &runtime_params,
            },
            AdvanceState {
                current_y: &mut current_y,
                params: &mut params,
                current_t: &mut current_t,
            },
            1.0,
            &[0],
            2.0,
            &mut backend,
        )
        .expect("root handler should apply the confirmed post-crossing relation memory");

        assert_eq!(params, vec![1.0, 1.0]);
    }

    #[test]
    fn future_root_outside_tolerance_returns_structured_error() {
        let (result, backend, current_t) = handle_test_root(1.0 + 5.0e-10);
        let error = result.expect_err("future root outside tolerance should be rejected");

        assert_eq!(
            error.to_string(),
            "root time 1.000000000500 is after backend time 1.000000000000"
        );
        assert!(backend.pinned_times.is_empty());
        assert_eq!(current_t, 1.0);
    }
}
