//! Diffsol wiring for solver-facing IR.
//!
//! This crate intentionally does not depend on DAE-IR or compiler phases.
//! DAE-to-Solve lowering must happen before a `SolveModel`
//! reaches this backend.

// Diffsol problem closures are single-threaded here but require cloneable shared
// handles that live with the leaked solver problem.
#![allow(clippy::arc_with_non_send_sync)]

mod bdf;
mod error;
mod init_projection;
mod ode;
mod prepared;
mod runtime;
pub mod stepper;

use std::{cell::RefCell, rc::Rc, sync::Arc};

use bdf::can_use_state_only_bdf;
pub(crate) use bdf::{
    bdf_derivative_guess, initial_bdf_state, initial_rk_state, reset_solver_state, solver_call,
    write_state_to_solver,
};
use diffsol::{
    BacktrackingLineSearch, BdfState, FaerSparseLU, FaerSparseMat, MatrixCommon,
    NewtonNonlinearSolver, OdeEquations, OdeSolverMethod, OdeSolverState, OdeSolverStopReason,
    VectorHost,
};
use init_projection::{EventObservation, initialize_state_runtime_values, set_initial_event_flag};
use rumoca_eval_solve::{
    self as solve_eval, RowEvalContext, SolveRuntime, current_dynamic_time_event_stop,
    next_runtime_event_stop, visible_values_with_context,
};
use rumoca_ir_solve as solve;
use rumoca_solver::{
    DiffsolMethod, EventPreMode, RuntimeEventBoundary, RuntimeEventBoundaryHandler,
    RuntimeEventStop, SimOptions, SimResult, SimTermination, SolveStopSchedule,
    build_sim_result_from_solve_model, commit_pre_params_after_event, discrete_row_pre_mode,
    initial_runtime_event_stop, process_runtime_event_boundary, push_visible_values,
    replace_last_visible_values, runtime_event_horizon, runtime_root_event_application_time,
    timeline::sample_time_match_with_tol, write_pre_params_from_sources,
};
pub(crate) use runtime::{
    EventUpdateInput, apply_discrete_value, apply_event_updates,
    apply_event_updates_with_event_pre, apply_initialization_updates,
    apply_post_initial_event_updates, seed_initial_discrete_values,
    settle_algebraics_and_relation_memory,
};
use runtime::{
    check_no_state_initialization, prepare_fixed_event_left_limit, simulate_no_state_solve_ir,
};

type Matrix = FaerSparseMat<f64>;
type Vector = <Matrix as MatrixCommon>::V;
type Scalar = <Matrix as MatrixCommon>::T;
pub(crate) type LinearSolver = FaerSparseLU<f64>;
pub(crate) type RuntimeParameters = Rc<RefCell<Vec<f64>>>;
pub use error::SimError;
pub(crate) use ode::{
    OdeModel, build_ode_problem_with_runtime_params_and_initial,
    build_state_ode_problem_with_runtime_params_and_initial, new_bdf_eval_counters,
    trace_bdf_eval_counter_snapshot, validate_model,
};
pub use prepared::PreparedSimulation;
use prepared::PreparedSimulationState;
use rumoca_solver::{project_algebraics, project_initial_variables_with_plan};

const EVENT_UPDATE_MAX_ITERS: usize = 256;

pub fn build_simulation(
    model: &solve::SolveModel,
    opts: &SimOptions,
) -> Result<PreparedSimulation, SimError> {
    let runtime_context = solve_eval::SimulationContext::new();
    runtime_context.hydrate_solve_model(model);
    validate_model(model)?;
    let state = if model.state_scalar_count() == 0 {
        tracing::debug!(target: "rumoca_solver_diffsol::bdf_path", "no-state path");
        PreparedSimulationState::NoState
    } else if opts.diffsol_method == DiffsolMethod::Bdf && can_use_state_only_bdf(model)? {
        // SDIRK tableaus are wired only on the general/implicit path, so a
        // non-BDF request routes through `General` even when the model is
        // state-only eligible.
        tracing::debug!(
            target: "rumoca_solver_diffsol::bdf_path",
            states = model.state_scalar_count(),
            "state-only BDF path (pure ODE, AD state Jacobian)"
        );
        PreparedSimulationState::StateOnly {
            equilibrium_model: Arc::new(OdeModel::new(model)?),
            runtime: Arc::new(SolveRuntime::new(model)?),
        }
    } else {
        tracing::debug!(
            target: "rumoca_solver_diffsol::bdf_path",
            states = model.state_scalar_count(),
            "general/implicit BDF path (AD implicit Jacobian)"
        );
        PreparedSimulationState::General {
            equilibrium_model: Arc::new(OdeModel::new(model)?),
            runtime: Arc::new(SolveRuntime::new(model)?),
        }
    };
    Ok(PreparedSimulation {
        model: model.clone(),
        opts: opts.clone(),
        state,
    })
}

pub fn run_prepared_simulation(prepared: &PreparedSimulation) -> Result<SimResult, SimError> {
    simulate_prepared(prepared)
}

pub fn check_prepared_initialization(prepared: &PreparedSimulation) -> Result<(), SimError> {
    prepared.check_initialization()
}

pub fn check_initialization(model: &solve::SolveModel, opts: &SimOptions) -> Result<(), SimError> {
    let runtime_context = solve_eval::SimulationContext::new();
    runtime_context.hydrate_solve_model(model);
    validate_model(model)?;
    if model.state_scalar_count() == 0 {
        return check_no_state_initialization(model, opts);
    }

    let equilibrium_model = Arc::new(OdeModel::new(model)?);
    let mut current_y = model.initial_y.clone();
    let mut params = model.parameters.clone();
    initialize_state_runtime_values(
        model,
        opts,
        &SolveRuntime::new(model)?,
        &equilibrium_model,
        &mut current_y,
        &mut params,
        opts.t_start,
    )?;
    let runtime_params: RuntimeParameters = Rc::new(RefCell::new(params.clone()));
    let problem = build_ode_problem_with_runtime_params_and_initial(
        model,
        opts,
        runtime_params,
        opts.t_start,
        current_y.clone(),
        equilibrium_model.clone(),
    )?;
    initial_bdf_state(model, &equilibrium_model, &problem, &current_y, &params).map(|_| ())
}

pub fn simulate(model: &solve::SolveModel, opts: &SimOptions) -> Result<SimResult, SimError> {
    let prepared = build_simulation(model, opts)?;
    run_prepared_simulation(&prepared)
}

fn simulate_prepared(prepared: &PreparedSimulation) -> Result<SimResult, SimError> {
    let model = &prepared.model;
    let opts = &prepared.opts;
    solve_eval::reset_solve_row_eval_trace();
    let result = match &prepared.state {
        PreparedSimulationState::NoState => simulate_no_state_solve_ir(model, opts),
        PreparedSimulationState::StateOnly {
            equilibrium_model,
            runtime,
        } => {
            let dt = opts.dt.unwrap_or((opts.t_end - opts.t_start).abs() / 500.0);
            let times = rumoca_solver::timeline::build_output_times(opts.t_start, opts.t_end, dt);
            simulate_state_only_bdf(model, opts, &times, equilibrium_model, runtime)
        }
        PreparedSimulationState::General {
            equilibrium_model,
            runtime,
        } => {
            let dt = opts.dt.unwrap_or((opts.t_end - opts.t_start).abs() / 500.0);
            let times = rumoca_solver::timeline::build_output_times(opts.t_start, opts.t_end, dt);
            simulate_with_states(model, opts, times, equilibrium_model, runtime)
        }
    };
    solve_eval::trace_solve_row_eval_snapshot("bdf");
    result
}

fn simulate_with_states(
    model: &solve::SolveModel,
    opts: &SimOptions,
    times: Vec<f64>,
    equilibrium_model: &Arc<OdeModel>,
    runtime: &Arc<SolveRuntime>,
) -> Result<SimResult, SimError> {
    let mut params = model.parameters.clone();
    let mut data = vec![Vec::with_capacity(times.len()); model.visible_names.len()];
    let mut recorded_times = Vec::with_capacity(times.len());
    let mut current_t = opts.t_start;
    let mut current_y = model.initial_y.clone();
    initialize_state_runtime_values(
        model,
        opts,
        runtime,
        equilibrium_model,
        &mut current_y,
        &mut params,
        current_t,
    )?;
    record_sample_if_new(
        Some(runtime),
        model,
        &current_y,
        &params,
        &mut recorded_times,
        &mut data,
        current_t,
    )?;

    // Shared runtime params captured by ODE closures and updated by event handlers.
    let runtime_params: RuntimeParameters = Rc::new(RefCell::new(params.clone()));
    // Build the ODE problem once — the persistent BDF solver borrows it for the
    // full simulation lifetime.
    let problem = build_ode_problem_with_runtime_params_and_initial(
        model,
        opts,
        runtime_params.clone(),
        current_t,
        current_y.clone(),
        equilibrium_model.clone(),
    )?;
    // The solver borrows `problem` for the full simulation. `Bdf` and `Sdirk` are
    // distinct concrete types, so we build whichever one was requested and box it
    // behind `BdfStepper` to run the single (backend-neutral) driver once. ESDIRK34
    // / TR-BDF2 reuse the same projection-aware consistent IC via `initial_rk_state`.
    let mut stepper: Box<dyn BdfStepper + '_> = match opts.diffsol_method {
        DiffsolMethod::Bdf => {
            let state = initial_bdf_state(model, equilibrium_model, &problem, &current_y, &params)?;
            let nl_solver = NewtonNonlinearSolver::new(
                LinearSolver::default(),
                BacktrackingLineSearch::default(),
            );
            let solver = solver_call("BDF new", || {
                diffsol::Bdf::<_, _, _, diffsol::NoAug<_>>::new(&problem, state, nl_solver)
            })?;
            Box::new(DiffsolStepper::new(
                solver,
                model,
                equilibrium_model,
                runtime_params.clone(),
            ))
        }
        method @ (DiffsolMethod::Esdirk34 | DiffsolMethod::TrBdf2) => {
            let state = initial_rk_state(model, equilibrium_model, &problem, &current_y, &params)?;
            let solver = solver_call("SDIRK new", || match method {
                DiffsolMethod::Esdirk34 => problem.esdirk34_solver::<LinearSolver>(state),
                _ => problem.tr_bdf2_solver::<LinearSolver>(state),
            })?;
            Box::new(DiffsolStepper::new(
                solver,
                model,
                equilibrium_model,
                runtime_params.clone(),
            ))
        }
    };
    let result = simulate_state_targets(
        model,
        opts,
        &times,
        equilibrium_model,
        &runtime_params,
        stepper.as_mut(),
        StateTrajectory {
            params: &mut params,
            data: &mut data,
            recorded_times: &mut recorded_times,
            current_t: &mut current_t,
            current_y: &mut current_y,
            runtime,
            mode: BdfMode::General,
        },
    );

    finalize_state_simulation(
        result,
        StateSimFinalize {
            model,
            opts,
            equilibrium_model,
            runtime,
            runtime_params: &runtime_params,
            params,
            data,
            recorded_times,
            current_y,
        },
    )
}

/// Owned trajectory buffers + context needed to turn a `simulate_state_targets`
/// outcome into a `SimResult`, shared by every solver arm of
/// [`simulate_with_states`].
struct StateSimFinalize<'a> {
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    equilibrium_model: &'a Arc<OdeModel>,
    runtime: &'a Arc<SolveRuntime>,
    runtime_params: &'a RuntimeParameters,
    params: Vec<f64>,
    data: Vec<Vec<f64>>,
    recorded_times: Vec<f64>,
    current_y: Vec<f64>,
}

fn finalize_state_simulation(
    result: Result<(), SimError>,
    mut fin: StateSimFinalize<'_>,
) -> Result<SimResult, SimError> {
    match result {
        Ok(()) => Ok(build_sim_result_from_solve_model(
            fin.model,
            fin.recorded_times,
            fin.data,
            None,
        )),
        Err(SimError::Terminated { time, message }) => {
            refresh_observation_discrete_rows(
                fin.model,
                &fin.equilibrium_model.runtime_state,
                &mut fin.current_y,
                &mut fin.params,
                time,
                fin.opts.atol.max(1.0e-10),
            )?;
            fin.runtime_params.borrow_mut().copy_from_slice(&fin.params);
            record_sample_if_new(
                Some(fin.runtime),
                fin.model,
                &fin.current_y,
                &fin.params,
                &mut fin.recorded_times,
                &mut fin.data,
                time,
            )?;
            Ok(build_sim_result_from_solve_model(
                fin.model,
                fin.recorded_times,
                fin.data,
                Some(SimTermination { time, message }),
            ))
        }
        Err(error) => Err(error),
    }
}

fn simulate_state_only_bdf(
    model: &solve::SolveModel,
    opts: &SimOptions,
    times: &[f64],
    equilibrium_model: &Arc<OdeModel>,
    runtime: &Arc<SolveRuntime>,
) -> Result<SimResult, SimError> {
    let mut params = model.parameters.clone();
    let mut data = vec![Vec::with_capacity(times.len()); model.visible_names.len()];
    let mut recorded_times = Vec::with_capacity(times.len());
    let mut current_t = opts.t_start;
    let mut current_y = model.initial_y.clone();
    initialize_state_runtime_values(
        model,
        opts,
        runtime,
        equilibrium_model,
        &mut current_y,
        &mut params,
        current_t,
    )?;
    let current_state = current_y[..model.state_scalar_count()].to_vec();
    record_sample_if_new(
        Some(runtime),
        model,
        &current_y,
        &params,
        &mut recorded_times,
        &mut data,
        current_t,
    )?;

    let runtime_params: RuntimeParameters = Rc::new(RefCell::new(params.clone()));
    let eval_counters = new_bdf_eval_counters();
    let problem = build_state_ode_problem_with_runtime_params_and_initial(
        model,
        opts,
        runtime_params.clone(),
        current_t,
        current_state.clone(),
        eval_counters.clone(),
        runtime.clone(),
    )?;
    let state = initial_state_only_bdf_state(runtime, &problem, &current_state, &params, opts)?;
    let nl_solver =
        NewtonNonlinearSolver::new(LinearSolver::default(), BacktrackingLineSearch::default());
    let solver = solver_call("BDF new", || {
        diffsol::Bdf::<_, _, _, diffsol::NoAug<_>>::new(&problem, state, nl_solver)
    })?;
    let mut stepper =
        DiffsolStepper::new(solver, model, equilibrium_model, runtime_params.clone());

    // Drive the reduced state-only solver through the *same* output / event /
    // root loop as the general path; `BdfMode::StateOnly` projects the solver's
    // reduced state to the full solver_y for recording and event evaluation.
    let result = simulate_state_targets(
        model,
        opts,
        times,
        equilibrium_model,
        &runtime_params,
        &mut stepper,
        StateTrajectory {
            params: &mut params,
            data: &mut data,
            recorded_times: &mut recorded_times,
            current_t: &mut current_t,
            current_y: &mut current_y,
            runtime,
            mode: BdfMode::StateOnly,
        },
    );

    trace_bdf_eval_counter_snapshot("state-only-bdf", &eval_counters);

    finalize_state_simulation(
        result,
        StateSimFinalize {
            model,
            opts,
            equilibrium_model,
            runtime,
            runtime_params: &runtime_params,
            params,
            data,
            recorded_times,
            current_y,
        },
    )
}

fn initial_state_only_bdf_state<Eqn>(
    runtime: &SolveRuntime,
    problem: &diffsol::OdeSolverProblem<Eqn>,
    state_y: &[f64],
    params: &[f64],
    opts: &SimOptions,
) -> Result<BdfState<Vector>, SimError>
where
    Eqn: diffsol::OdeEquationsImplicit<
            M = Matrix,
            V = Vector,
            T = Scalar,
            C = <Matrix as MatrixCommon>::C,
        >,
{
    let mut state = BdfState::<Vector>::new_without_initialise(problem)
        .map_err(|err| SimError::SolverError(format!("BDF state init: {err}")))?;
    let dy = runtime.eval_state_derivatives(problem.t0, state_y, params, opts.atol, 256)?;
    {
        let state_ref = state.as_mut();
        state_ref.y.as_mut_slice().copy_from_slice(state_y);
        state_ref.dy.as_mut_slice().copy_from_slice(&dy);
        *state_ref.t = problem.t0;
    }
    state.set_step_size(problem.h0, &problem.atol, problem.rtol, &problem.eqn, 1);
    Ok(state)
}

/// Result of a single solver step, backend-neutral (mirrors diffsol's
/// `OdeSolverStopReason` minus the unused `Finished`).
enum BdfStepOutcome {
    /// Reached the requested stop time (`set_stop_time`).
    Stop,
    /// Took an internal adaptive step (did not reach a stop/root).
    Internal,
    /// A zero-crossing root was located at `t_root`.
    Root { t_root: f64 },
}

/// The backend-specific stepping operations the shared BDF output/event/root
/// driver needs. diffsol's `OdeSolverMethod` (and the `OdeModel` it integrates)
/// are abstracted behind this so the driver itself carries no diffsol types.
/// The solver native vector is whatever the underlying problem integrates
/// (reduced state for `StateOnly`, full solver_y for `General`); the driver maps
/// it to/from the full solver_y via [`BdfMode`].
trait BdfStepper {
    fn time(&self) -> f64;
    fn native_y(&self) -> Vec<f64>;
    fn step(&mut self) -> Result<BdfStepOutcome, SimError>;
    fn set_stop_time(&mut self, stop_time: f64) -> Result<(), SimError>;
    fn interpolate(&mut self, t: f64) -> Result<Vec<f64>, SimError>;
    fn state_mut_back(&mut self, t: f64) -> Result<(), SimError>;
    /// Reload the solver state after an event (native state + derivative guess).
    fn reset(
        &mut self,
        native_y: &[f64],
        native_dy: &[f64],
        params: &[f64],
        t: f64,
        h_cap: f64,
    ) -> Result<(), SimError>;
    /// Mass-matrix derivative guess `dy` for a full solver_y (general reset path).
    fn derivative_guess(&self, y: &[f64], params: &[f64], t: f64) -> Result<Vec<f64>, SimError>;
    /// Debug-only trace hooks (diffsol resolves these against `OdeModel`).
    fn trace_step_failure(&self, y: &[f64], params: &[f64], current_t: f64, solver_t: f64, error: &str);
    fn trace_post_event_state(&self, y: &[f64], params: &[f64], t: f64);
}

/// diffsol adapter implementing [`BdfStepper`] over an `OdeSolverMethod` plus the
/// `OdeModel`/runtime context its reset and derivative-guess need.
struct DiffsolStepper<'a, Eqn, S> {
    solver: S,
    model: &'a solve::SolveModel,
    equilibrium_model: &'a OdeModel,
    runtime_params: RuntimeParameters,
    _eqn: std::marker::PhantomData<fn() -> Eqn>,
}

impl<'a, Eqn, S> DiffsolStepper<'a, Eqn, S>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    fn new(
        solver: S,
        model: &'a solve::SolveModel,
        equilibrium_model: &'a OdeModel,
        runtime_params: RuntimeParameters,
    ) -> Self {
        Self {
            solver,
            model,
            equilibrium_model,
            runtime_params,
            _eqn: std::marker::PhantomData,
        }
    }
}

impl<'a, Eqn, S> BdfStepper for DiffsolStepper<'a, Eqn, S>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    fn time(&self) -> f64 {
        self.solver.state().t
    }

    fn native_y(&self) -> Vec<f64> {
        self.solver.state().y.as_slice().to_vec()
    }

    fn step(&mut self) -> Result<BdfStepOutcome, SimError> {
        match solver_call("BDF step", || self.solver.step())? {
            OdeSolverStopReason::TstopReached => Ok(BdfStepOutcome::Stop),
            OdeSolverStopReason::InternalTimestep => Ok(BdfStepOutcome::Internal),
            OdeSolverStopReason::RootFound(t_root, _) => Ok(BdfStepOutcome::Root { t_root }),
        }
    }

    fn set_stop_time(&mut self, stop_time: f64) -> Result<(), SimError> {
        set_solver_stop_time(&mut self.solver, stop_time)
    }

    fn interpolate(&mut self, t: f64) -> Result<Vec<f64>, SimError> {
        self.solver
            .interpolate(t)
            .map(|v| v.as_slice().to_vec())
            .map_err(|e| SimError::SolverError(format!("interpolate: {e}")))
    }

    fn state_mut_back(&mut self, t: f64) -> Result<(), SimError> {
        self.solver
            .state_mut_back(t)
            .map_err(|e| SimError::SolverError(format!("state_mut_back: {e}")))
    }

    fn reset(
        &mut self,
        native_y: &[f64],
        native_dy: &[f64],
        params: &[f64],
        t: f64,
        h_cap: f64,
    ) -> Result<(), SimError> {
        reset_solver_state(
            &mut self.solver,
            &self.runtime_params,
            native_y,
            native_dy,
            params,
            t,
            h_cap,
        )
    }

    fn derivative_guess(&self, y: &[f64], params: &[f64], t: f64) -> Result<Vec<f64>, SimError> {
        bdf_derivative_guess(self.model, self.equilibrium_model, y, params, t)
    }

    fn trace_step_failure(&self, y: &[f64], params: &[f64], current_t: f64, solver_t: f64, error: &str) {
        trace_bdf_step_failure(self.equilibrium_model, y, params, current_t, solver_t, error);
    }

    fn trace_post_event_state(&self, y: &[f64], params: &[f64], t: f64) {
        trace_bdf_post_event_state(self.equilibrium_model, self.model, y, params, t);
    }
}

struct StateTrajectory<'a> {
    params: &'a mut Vec<f64>,
    data: &'a mut Vec<Vec<f64>>,
    recorded_times: &'a mut Vec<f64>,
    current_t: &'a mut f64,
    current_y: &'a mut Vec<f64>,
    runtime: &'a SolveRuntime,
    /// Which system the solver integrates (`General` full solver_y vs `StateOnly`
    /// reduced state), selecting how native↔full conversions behave in the loop.
    mode: BdfMode,
}

// SPEC_0021: Exception - central BDF event loop keeps step advancement,
// zero-crossing handling, and sample recording in a single ordered routine.
#[allow(clippy::too_many_lines)]
fn simulate_state_targets<St: BdfStepper + ?Sized>(
    model: &solve::SolveModel,
    opts: &SimOptions,
    times: &[f64],
    equilibrium_model: &OdeModel,
    runtime_params: &RuntimeParameters,
    stepper: &mut St,
    state: StateTrajectory<'_>,
) -> Result<(), SimError> {
    let mode = state.mode;
    let mut stop_schedule = SolveStopSchedule::new(&model.problem, opts.t_start, opts.t_end);
    let mut pending_root_t: Option<f64> = None;

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
                advance_context(
                    model,
                    opts,
                    equilibrium_model,
                    runtime_params,
                    state.runtime,
                    mode,
                ),
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
                &equilibrium_model.runtime_state,
                state.current_y,
                state.params,
                &mut stop_schedule,
                *state.current_t,
                target,
            )?;
            let mut deferred_root: Option<f64> = None;
            let hit_root = advance_to_target_once(
                advance_context(
                    model,
                    opts,
                    equilibrium_model,
                    runtime_params,
                    state.runtime,
                    mode,
                ),
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
                    advance_context(
                        model,
                        opts,
                        equilibrium_model,
                        runtime_params,
                        state.runtime,
                        mode,
                    ),
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
        refresh_observation_discrete_rows(
            model,
            &equilibrium_model.runtime_state,
            state.current_y,
            state.params,
            *state.current_t,
            opts.atol.max(1.0e-10),
        )?;
        runtime_params.borrow_mut().copy_from_slice(state.params);
        record_sample_if_new(
            Some(state.runtime),
            model,
            state.current_y,
            state.params,
            state.recorded_times,
            state.data,
            *state.current_t,
        )?;
    }

    Ok(())
}

fn record_sample_if_new(
    runtime: Option<&SolveRuntime>,
    model: &solve::SolveModel,
    y: &[f64],
    params: &[f64],
    recorded_times: &mut Vec<f64>,
    data: &mut [Vec<f64>],
    t: f64,
) -> Result<(), SimError> {
    let values = if let Some(runtime) = runtime {
        runtime
            .visible_values(y, params, t)
            .map_err(|err| SimError::SolveIr(err.to_string()))?
    } else {
        visible_values(model, y, params, t)?
    };
    if recorded_times
        .last()
        .is_some_and(|last| sample_time_match_with_tol(*last, t))
    {
        if let Some(last) = recorded_times.last_mut() {
            *last = t;
        }
        replace_last_visible_values(data, &values)?;
        return Ok(());
    }
    recorded_times.push(t);
    push_visible_values(data, &values)?;
    Ok(())
}

fn refresh_observation_discrete_rows(
    model: &solve::SolveModel,
    runtime_state: &solve_eval::SimulationRuntimeState,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<bool, SimError> {
    if model.problem.discrete.observation_refresh.is_empty() {
        return Ok(false);
    }
    let mut changed_any = false;
    let event_pre_y = y.to_vec();
    let event_pre_p = p.to_vec();
    for _ in 0..EVENT_UPDATE_MAX_ITERS {
        let changed = apply_observation_discrete_refresh_pass(
            model,
            ObservationRefreshPass {
                runtime_state,
                event_pre_y: event_pre_y.as_slice(),
                event_pre_p: event_pre_p.as_slice(),
                tol,
            },
            y,
            p,
            t,
        )?;
        if !changed {
            return Ok(changed_any);
        }
        changed_any = true;
    }
    Err(SimError::SolveIr(
        "observation-time discrete refresh did not converge".to_string(),
    ))
}

struct ObservationRefreshPass<'a> {
    runtime_state: &'a solve_eval::SimulationRuntimeState,
    event_pre_y: &'a [f64],
    event_pre_p: &'a [f64],
    tol: f64,
}

fn apply_observation_discrete_refresh_pass(
    model: &solve::SolveModel,
    ctx: ObservationRefreshPass<'_>,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
) -> Result<bool, SimError> {
    if model.problem.discrete.observation_refresh.len() != model.problem.discrete.rhs.len() {
        return Err(SimError::SolveIr(format!(
            "discrete observation-refresh row count {} does not match discrete RHS row count {}",
            model.problem.discrete.observation_refresh.len(),
            model.problem.discrete.rhs.len()
        )));
    }
    let mut changed = false;
    for (row_idx, row) in model.problem.discrete.rhs.programs.iter().enumerate() {
        if !model.problem.discrete.observation_refresh[row_idx] {
            continue;
        }
        refresh_observation_pre_params(model, row_idx, &ctx, y, p);
        let value = solve_eval::eval_row_with_context(
            row,
            y,
            p,
            t,
            RowEvalContext {
                external_tables: Some(model.external_tables.as_slice()),
                runtime_state: Some(ctx.runtime_state),
                ..Default::default()
            },
        )
        .map_err(|err| SimError::SolveIr(err.to_string()))?;
        changed |= apply_discrete_value(
            model.problem.discrete.update_targets[row_idx],
            value,
            y,
            p,
            ctx.tol,
        )?;
    }
    Ok(changed)
}

fn refresh_observation_pre_params(
    model: &solve::SolveModel,
    row_idx: usize,
    ctx: &ObservationRefreshPass<'_>,
    y: &[f64],
    p: &mut [f64],
) {
    match discrete_row_pre_mode(model, row_idx) {
        EventPreMode::EventEntry | EventPreMode::Fixed => {
            write_pre_params_from_sources(model, ctx.event_pre_y, ctx.event_pre_p, p, ctx.tol);
        }
        EventPreMode::FollowCurrent => {
            let snapshot_p = p.to_vec();
            write_pre_params_from_sources(model, y, snapshot_p.as_slice(), p, ctx.tol);
        }
    }
}

fn visible_values(
    model: &solve::SolveModel,
    y: &[f64],
    params: &[f64],
    t: f64,
) -> Result<Vec<f64>, SimError> {
    visible_values_with_context(
        model,
        y,
        params,
        t,
        RowEvalContext {
            external_tables: Some(model.external_tables.as_slice()),
            ..Default::default()
        },
    )
    .map_err(|err| SimError::SolveIr(err.to_string()))
}

/// Which system the BDF solver integrates. Both modes share the single output /
/// event / root driver (`simulate_state_targets`); they differ only in how the
/// solver's native state vector maps to the full `solver_y` used for recording,
/// observation, and event evaluation, and in the vector loaded back when the
/// solver is reset after an event.
///
/// `General` integrates the full augmented `solver_y` (states + algebraics with a
/// mass matrix), so the native vector *is* the `solver_y` and every conversion is
/// the identity — `General` models therefore behave bit-for-bit as before this
/// driver was unified. `StateOnly` integrates only the reduced state vector and
/// recovers the algebraics by the projection, so the conversions project /
/// restrict between the native (state) and full (`solver_y`) representations.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BdfMode {
    General,
    StateOnly,
}

impl BdfMode {
    /// Map the solver's native state vector at time `t` (from `state().y` or
    /// `interpolate()`) into the full `solver_y` written to `current_y`.
    fn write_full_y(
        self,
        ctx: &AdvanceContext<'_>,
        native: &[f64],
        t: f64,
        current_y: &mut [f64],
        params: &[f64],
    ) -> Result<(), SimError> {
        match self {
            BdfMode::General => {
                current_y.copy_from_slice(native);
                Ok(())
            }
            BdfMode::StateOnly => {
                let state_count = ctx.model.state_scalar_count().min(native.len());
                let full = ctx.runtime.full_solver_y(
                    t,
                    &native[..state_count],
                    params,
                    ctx.opts.atol.max(1.0e-10),
                    EVENT_UPDATE_MAX_ITERS,
                )?;
                current_y.copy_from_slice(&full);
                Ok(())
            }
        }
    }

    /// Native state + derivative guess to reload into the solver after an event,
    /// derived from the post-event full `solver_y` at time `t`.
    fn reset_native_vectors<St: BdfStepper + ?Sized>(
        self,
        ctx: &AdvanceContext<'_>,
        stepper: &St,
        current_y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<(Vec<f64>, Vec<f64>), SimError> {
        match self {
            BdfMode::General => {
                let dy = stepper.derivative_guess(current_y, params, t)?;
                Ok((current_y.to_vec(), dy))
            }
            BdfMode::StateOnly => {
                let state_count = ctx.model.state_scalar_count().min(current_y.len());
                let native = current_y[..state_count].to_vec();
                let dy = ctx.runtime.eval_state_derivatives(
                    t,
                    &native,
                    params,
                    ctx.opts.atol.max(1.0e-10),
                    EVENT_UPDATE_MAX_ITERS,
                )?;
                Ok((native, dy))
            }
        }
    }
}

#[derive(Clone, Copy)]
struct AdvanceContext<'a> {
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    equilibrium_model: &'a OdeModel,
    runtime_params: &'a RuntimeParameters,
    runtime: &'a SolveRuntime,
    mode: BdfMode,
}

struct AdvanceState<'a> {
    current_y: &'a mut [f64],
    params: &'a mut [f64],
    current_t: &'a mut f64,
}

fn advance_context<'a>(
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    equilibrium_model: &'a OdeModel,
    runtime_params: &'a RuntimeParameters,
    runtime: &'a SolveRuntime,
    mode: BdfMode,
) -> AdvanceContext<'a> {
    AdvanceContext {
        model,
        opts,
        equilibrium_model,
        runtime_params,
        runtime,
        mode,
    }
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

fn resolve_pending_root<St: BdfStepper + ?Sized>(
    pending_root_t: &mut Option<f64>,
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    stepper: &mut St,
    stop_schedule: &mut SolveStopSchedule,
) -> Result<PendingRootAction, SimError> {
    let Some(prt) = *pending_root_t else {
        return Ok(PendingRootAction::None);
    };
    if !sample_time_match_with_tol(target, prt) && target < prt {
        let y_at = stepper.interpolate(target)?;
        *state.current_t = target;
        state
            .params
            .copy_from_slice(ctx.runtime_params.borrow().as_slice());
        ctx.mode
            .write_full_y(&ctx, &y_at, target, state.current_y, state.params)?;
        refresh_interpolated_sample_state(ctx, state, target)?;
        return Ok(PendingRootAction::Break);
    }

    *pending_root_t = None;
    handle_root_crossing(ctx, state, prt, target, stepper)?;
    stop_schedule.advance_past(stepper.time());
    Ok(PendingRootAction::Continue)
}

fn refresh_interpolated_sample_state(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
) -> Result<(), SimError> {
    settle_prepared_state(
        ctx.runtime,
        ctx.model,
        ctx.equilibrium_model,
        state.current_y,
        state.params,
        target,
        ctx.opts,
    )?;
    refresh_observation_discrete_rows(
        ctx.model,
        &ctx.equilibrium_model.runtime_state,
        state.current_y,
        state.params,
        target,
        ctx.opts.atol.max(1.0e-10),
    )?;
    ctx.runtime_params
        .borrow_mut()
        .copy_from_slice(state.params);
    Ok(())
}

fn apply_scheduled_time_event<St: BdfStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    event: RuntimeEventStop,
    target: f64,
    stepper: &mut St,
    observations: ObservationBuffers<'_>,
) -> Result<(), SimError> {
    let tol = ctx.opts.atol.max(1.0e-10);
    prepare_fixed_event_left_limit(
        ctx.model,
        ctx.equilibrium_model,
        state.current_y,
        state.params,
        *state.current_t,
        tol,
        event,
    )?;
    let event_pre_y = state.current_y.to_vec();
    let event_pre_p = state.params.to_vec();
    apply_event_updates(
        ctx.runtime,
        ctx.equilibrium_model,
        state.current_y,
        state.params,
        *state.current_t,
        tol,
    )?;
    *state.current_t = EventObservation {
        runtime: ctx.runtime,
        model: ctx.model,
        equilibrium_model: ctx.equilibrium_model,
        y: state.current_y,
        params: state.params,
        tol,
        recorded_times: observations.recorded_times,
        data: observations.data,
        event_pre_y: &event_pre_y,
        event_pre_p: &event_pre_p,
    }
    .record_time_event(
        *state.current_t,
        runtime_event_horizon(event, target, ctx.opts.t_end),
        event,
    )?;
    commit_pre_params_after_event(ctx.model, state.current_y, state.params, tol);
    reinitialize_solver_after_time_event(ctx, state, stepper, tol)
}

fn reinitialize_solver_after_time_event<St: BdfStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    stepper: &mut St,
    tol: f64,
) -> Result<(), SimError> {
    let t_right = *state.current_t;
    settle_algebraics_and_relation_memory(
        ctx.runtime,
        ctx.equilibrium_model,
        state.current_y,
        state.params,
        t_right,
        ctx.model.state_scalar_count(),
        tol,
    )?;
    let (native_y, native_dy) =
        ctx.mode
            .reset_native_vectors(&ctx, stepper, state.current_y, state.params, t_right)?;
    stepper.reset(
        &native_y,
        &native_dy,
        state.params,
        *state.current_t,
        rumoca_solver::event_solver_step_cap(ctx.opts.dt),
    )
}

fn advance_to_target_once<St: BdfStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    event_stop: Option<RuntimeEventStop>,
    stepper: &mut St,
    deferred_root: &mut Option<f64>,
) -> Result<bool, SimError> {
    if event_stop.is_some() {
        return advance_to_scheduled_stop(ctx, state, target, stepper);
    }

    advance_output_interval(ctx, state, target, stepper, deferred_root)
}

fn advance_to_scheduled_stop<St: BdfStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    stepper: &mut St,
) -> Result<bool, SimError> {
    if stepper.time() > target {
        stepper.state_mut_back(target)?;
    }
    if sample_time_match_with_tol(stepper.time(), target) {
        *state.current_t = target;
        state
            .params
            .copy_from_slice(ctx.runtime_params.borrow().as_slice());
        let native = stepper.native_y();
        ctx.mode
            .write_full_y(&ctx, &native, target, state.current_y, state.params)?;
        return Ok(false);
    }
    stepper.set_stop_time(target)?;
    loop {
        let outcome = match stepper.step() {
            Ok(outcome) => outcome,
            Err(e) => {
                stepper.trace_step_failure(state.current_y, state.params, *state.current_t, stepper.time(), &e.to_string());
                return Err(e);
            }
        };
        match outcome {
            BdfStepOutcome::Stop => {
                let stop_t = stepper.time();
                *state.current_t = stop_t;
                state
                    .params
                    .copy_from_slice(ctx.runtime_params.borrow().as_slice());
                let native = stepper.native_y();
                ctx.mode
                    .write_full_y(&ctx, &native, stop_t, state.current_y, state.params)?;
                return Ok(false);
            }
            BdfStepOutcome::Internal => continue,
            BdfStepOutcome::Root { t_root } => {
                trace_bdf_step_event("scheduled-root", stepper.time(), Some(t_root));
                return handle_root_crossing(ctx, state, t_root, target, stepper);
            }
        }
    }
}

/// True when the model has no discontinuities (zero-crossing roots, scheduled
/// time events, or discrete `when` updates), so the BDF solution is smooth and
/// safe to dense-output / interpolate at arbitrary times.
fn model_is_event_free(model: &solve::SolveModel) -> bool {
    let events = &model.problem.events;
    let discrete = &model.problem.discrete;
    events.root_conditions.is_empty()
        && events.scheduled_time_events.is_empty()
        && discrete.update_targets.is_empty()
        && discrete.runtime_assignment_targets.is_empty()
}

fn advance_output_interval<St: BdfStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    stepper: &mut St,
    deferred_root: &mut Option<f64>,
) -> Result<bool, SimError> {
    // The `General` path integrates the full solver_y, so interpolating it at the
    // output time is exact. The `StateOnly` path interpolates the reduced state and
    // *re-projects* the algebraics, which is fragile near discontinuities (the
    // interpolated state need not be algebraically consistent). For event-bearing
    // state-only models we therefore step the solver exactly onto each output point
    // (clamp) and project from that consistent state; smooth state-only models keep
    // free dense-output stepping so the multi-step BDF controller is never starved
    // by a fine output grid (the trivial-ODE failure this whole change fixes).
    if ctx.mode == BdfMode::StateOnly && !model_is_event_free(ctx.model) {
        return advance_output_interval_clamped(ctx, state, target, stepper);
    }
    loop {
        if stepper.time() >= target {
            let y_at_target = stepper.interpolate(target)?;
            *state.current_t = target;
            state
                .params
                .copy_from_slice(ctx.runtime_params.borrow().as_slice());
            ctx.mode
                .write_full_y(&ctx, &y_at_target, target, state.current_y, state.params)?;
            return Ok(false);
        }

        let outcome = match stepper.step() {
            Ok(outcome) => outcome,
            Err(e) => {
                stepper.trace_step_failure(state.current_y, state.params, *state.current_t, stepper.time(), &e.to_string());
                return Err(e);
            }
        };
        match outcome {
            BdfStepOutcome::Stop | BdfStepOutcome::Internal => {}
            BdfStepOutcome::Root { t_root } => {
                trace_bdf_step_event("output-root", stepper.time(), Some(t_root));
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
                ctx.mode
                    .write_full_y(&ctx, &y_at_target, target, state.current_y, state.params)?;
                *deferred_root = Some(t_root);
                return Ok(false);
            }
        }
    }
}

/// Output advance that steps the solver exactly onto `target` (no interpolation)
/// so the recorded sample is projected from a consistent solver state. Used for
/// event-bearing state-only models where dense-output interpolation + algebraic
/// re-projection is unreliable near discontinuities.
fn advance_output_interval_clamped<St: BdfStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    stepper: &mut St,
) -> Result<bool, SimError> {
    if stepper.time() >= target {
        let y_at_target = stepper.interpolate(target)?;
        *state.current_t = target;
        state
            .params
            .copy_from_slice(ctx.runtime_params.borrow().as_slice());
        ctx.mode
            .write_full_y(&ctx, &y_at_target, target, state.current_y, state.params)?;
        return Ok(false);
    }
    stepper.set_stop_time(target)?;
    loop {
        let outcome = match stepper.step() {
            Ok(outcome) => outcome,
            Err(e) => {
                stepper.trace_step_failure(state.current_y, state.params, *state.current_t, stepper.time(), &e.to_string());
                return Err(e);
            }
        };
        match outcome {
            BdfStepOutcome::Stop => {
                let stop_t = stepper.time();
                *state.current_t = stop_t;
                state
                    .params
                    .copy_from_slice(ctx.runtime_params.borrow().as_slice());
                let native = stepper.native_y();
                ctx.mode
                    .write_full_y(&ctx, &native, stop_t, state.current_y, state.params)?;
                return Ok(false);
            }
            BdfStepOutcome::Internal => continue,
            BdfStepOutcome::Root { t_root } => {
                trace_bdf_step_event("output-root-clamped", stepper.time(), Some(t_root));
                return handle_root_crossing(ctx, state, t_root, target, stepper);
            }
        }
    }
}

fn trace_bdf_step_event(kind: &str, solver_t: f64, root_t: Option<f64>) {
    if !tracing::enabled!(target: "rumoca_solver_diffsol::bdf", tracing::Level::DEBUG) {
        return;
    }
    tracing::debug!(target: "rumoca_solver_diffsol::bdf", "{kind} solver_t={solver_t:.12} root_t={root_t:?}");
}

fn trace_bdf_step_failure(
    equilibrium_model: &OdeModel,
    y: &[f64],
    params: &[f64],
    current_t: f64,
    solver_t: f64,
    error: &str,
) {
    if !tracing::enabled!(target: "rumoca_solver_diffsol::bdf", tracing::Level::DEBUG) {
        return;
    }
    let mut roots = vec![0.0; equilibrium_model.root_conditions.len().max(1)];
    let root_summary = match equilibrium_model.eval_roots(y, params, current_t, &mut roots) {
        Ok(()) => roots
            .iter()
            .copied()
            .enumerate()
            .min_by(|(_, lhs), (_, rhs)| lhs.abs().total_cmp(&rhs.abs()))
            .map(|(idx, value)| format!("nearest_root[{idx}]={value:.12e}"))
            .unwrap_or_else(|| "no roots".to_string()),
        Err(err) => format!("root eval failed: {err}"),
    };
    tracing::debug!(
        target: "rumoca_solver_diffsol::bdf",
        "step-fail current_t={current_t:.12} solver_t={solver_t:.12} {root_summary} err={error}"
    );
}

fn handle_root_crossing<St: BdfStepper + ?Sized>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    t_root: f64,
    target: f64,
    stepper: &mut St,
) -> Result<bool, SimError> {
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
    // Full solver_y at the root (left limit); for StateOnly this projects the
    // reduced state through the algebraic projection, mirroring the old
    // state-only root handler.
    let mut event_pre_y = vec![0.0; state.current_y.len()];
    ctx.mode
        .write_full_y(&ctx, &native_at_root, root_t, &mut event_pre_y, state.params)?;
    state.current_y.copy_from_slice(&event_pre_y);
    advance_state_to_event_limits(
        ctx.model,
        ctx.equilibrium_model,
        &mut event_pre_y,
        state.current_y,
        state.params,
        root_t,
        right_t,
    )?;
    apply_event_updates_with_event_pre(EventUpdateInput {
        runtime: ctx.runtime,
        ode_model: ctx.equilibrium_model,
        y: state.current_y,
        p: state.params,
        t: *state.current_t,
        tol,
        event_pre_y: &event_pre_y,
        event_pre_p: &event_pre_p,
    })?;
    settle_prepared_state(
        ctx.runtime,
        ctx.model,
        ctx.equilibrium_model,
        state.current_y,
        state.params,
        *state.current_t,
        ctx.opts,
    )?;
    commit_pre_params_after_event(ctx.model, state.current_y, state.params, tol);
    stepper.trace_post_event_state(state.current_y, state.params, *state.current_t);
    let (native_y, native_dy) =
        ctx.mode
            .reset_native_vectors(&ctx, stepper, state.current_y, state.params, *state.current_t)?;
    stepper.reset(
        &native_y,
        &native_dy,
        state.params,
        *state.current_t,
        rumoca_solver::event_solver_step_cap(ctx.opts.dt),
    )?;
    Ok(true)
}

fn trace_bdf_post_event_state(
    equilibrium_model: &OdeModel,
    model: &solve::SolveModel,
    y: &[f64],
    params: &[f64],
    t: f64,
) {
    if !tracing::enabled!(target: "rumoca_solver_diffsol::bdf", tracing::Level::DEBUG) {
        return;
    }
    let mut rhs = vec![0.0; y.len()];
    let summary = match equilibrium_model.eval_residual(y, params, t, &mut rhs) {
        Ok(()) => {
            let state_count = model.state_scalar_count().min(rhs.len());
            let all = rhs.iter().copied().map(f64::abs).fold(0.0, f64::max);
            let alg = rhs[state_count..]
                .iter()
                .copied()
                .map(f64::abs)
                .fold(0.0, f64::max);
            format!("max_rhs={all:.6e} max_alg_residual={alg:.6e}")
        }
        Err(err) => format!("residual eval failed: {err}"),
    };
    tracing::debug!(
        target: "rumoca_solver_diffsol::bdf",
        "post-event current_t={t:.12} {summary}"
    );
}

fn advance_state_to_event_limits(
    solve_model: &solve::SolveModel,
    model: &OdeModel,
    event_pre_y: &mut [f64],
    y: &mut [f64],
    p: &[f64],
    root_t: f64,
    right_t: f64,
) -> Result<(), SimError> {
    let dt = right_t - root_t;
    if dt <= 0.0 || sample_time_match_with_tol(root_t, right_t) {
        return Ok(());
    }
    let dy = bdf_derivative_guess(solve_model, model, y, p, root_t)?;
    for (slot, derivative) in event_pre_y.iter_mut().zip(dy.iter().copied()) {
        *slot -= dt * derivative;
    }
    for (slot, derivative) in y.iter_mut().zip(dy) {
        *slot += dt * derivative;
    }
    Ok(())
}

fn settle_prepared_state(
    runtime: &SolveRuntime,
    model: &solve::SolveModel,
    equilibrium_model: &OdeModel,
    current_y: &mut [f64],
    params: &mut [f64],
    current_t: f64,
    opts: &SimOptions,
) -> Result<(), SimError> {
    settle_algebraics_and_relation_memory(
        runtime,
        equilibrium_model,
        current_y,
        params,
        current_t,
        model.state_scalar_count(),
        opts.atol.max(1.0e-10),
    )
}

fn set_solver_stop_time<'a, Eqn, S>(solver: &mut S, stop_time: f64) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    solver
        .set_stop_time(stop_time)
        .map_err(|err| SimError::SolverError(format!("Failed to set stop time: {err}")))
}

#[cfg(test)]
mod tests;
