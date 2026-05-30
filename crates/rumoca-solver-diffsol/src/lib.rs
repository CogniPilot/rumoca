//! Diffsol wiring for solver-facing IR.
//!
//! This crate intentionally does not depend on DAE-IR or compiler phases.
//! DAE-to-Solve lowering must happen before a `SolveModel`
//! reaches this backend.

// Diffsol problem closures are single-threaded here but require cloneable shared
// handles that live with the leaked solver problem.
#![allow(clippy::arc_with_non_send_sync)]

mod discrete_updates;
mod initialization;
mod no_state;
mod ode;
mod panic_capture;
mod projection;
mod root_search;
mod solver_state;
mod state_only_eligibility;
pub mod stepper;

use std::{cell::RefCell, rc::Rc, sync::Arc};

use diffsol::{
    BacktrackingLineSearch, BdfState, FaerSparseLU, FaerSparseMat, MatrixCommon,
    NewtonNonlinearSolver, OdeEquations, OdeSolverMethod, OdeSolverState, OdeSolverStopReason,
    VectorHost,
};
pub(crate) use discrete_updates::{
    EventUpdateInput, apply_discrete_value, apply_event_updates,
    apply_event_updates_with_event_pre, apply_initialization_updates,
    apply_post_initial_event_updates, apply_runtime_assignments, seed_initial_discrete_values,
    settle_algebraics_and_relation_memory,
};
pub(crate) use initialization::{bdf_derivative_guess, initial_bdf_state};
use no_state::{
    check_no_state_initialization, prepare_fixed_event_left_limit, simulate_no_state_solve_ir,
};
use rumoca_eval_solve::{
    self as solve_eval, RowEvalContext, SolveRuntime, next_runtime_event_stop,
    visible_values_with_context,
};
use rumoca_ir_solve as solve;
use rumoca_solver::{
    EventPreMode, RuntimeEventStop, RuntimeSolveError, SimBackend, SimOptions, SimResult,
    SimTermination, SolveStopSchedule, build_sim_result_from_solve_model, discrete_row_pre_mode,
    initial_static_event_pre_mode, push_visible_values, replace_last_visible_values,
    timeline::{event_right_limit_time, sample_time_match_with_tol},
    update_relation_memory_slots, write_pre_params_from_sources,
};
use solver_state::{reset_solver_state, write_state_to_solver};
use state_only_eligibility::can_use_state_only_bdf;

type Matrix = FaerSparseMat<f64>;
type Vector = <Matrix as MatrixCommon>::V;
type Scalar = <Matrix as MatrixCommon>::T;
pub(crate) type LinearSolver = FaerSparseLU<f64>;
pub(crate) type RuntimeParameters = Rc<RefCell<Vec<f64>>>;
pub(crate) use ode::{
    OdeModel, build_ode_problem_with_runtime_params_and_initial,
    build_state_ode_problem_with_runtime_params_and_initial, new_bdf_eval_counters,
    trace_bdf_eval_counter_snapshot, validate_model,
};
pub(crate) use panic_capture::{catch_solver_panic, solver_call};
pub(crate) use projection::settle_initial_projection_context;
use rumoca_solver::{project_algebraics, project_initial_variables_with_plan};

const EVENT_UPDATE_MAX_ITERS: usize = 256;

#[derive(Debug, thiserror::Error)]
pub enum SimError {
    #[error("empty system: no equations to simulate")]
    EmptySystem,

    #[error("solver error: {0}")]
    SolverError(String),

    #[error("solve-IR evaluation failed: {0}")]
    SolveIr(String),

    #[error("Modelica assert failed at t={time:.9}: {message}")]
    AssertionFailed { time: f64, message: String },

    #[error("Modelica terminate requested at t={time:.9}: {message}")]
    Terminated { time: f64, message: String },

    #[error("timeout after {seconds:.3}s")]
    Timeout { seconds: f64 },
}

impl SimError {
    pub fn source_span(&self) -> Option<()> {
        None
    }
}

impl From<RuntimeSolveError> for SimError {
    fn from(value: RuntimeSolveError) -> Self {
        match value {
            RuntimeSolveError::SolveIr(message) => Self::SolveIr(message),
            RuntimeSolveError::UnsupportedModel { reason } => Self::SolveIr(reason),
            RuntimeSolveError::NonFiniteDerivative { state_name } => Self::SolveIr(format!(
                "non-finite derivative evaluation for state '{state_name}'"
            )),
            non_finite @ RuntimeSolveError::NonFiniteValue { .. } => {
                Self::SolveIr(non_finite.to_string())
            }
        }
    }
}

pub struct PreparedSimulation {
    model: solve::SolveModel,
    opts: SimOptions,
    state: PreparedSimulationState,
}

enum PreparedSimulationState {
    NoState,
    StateOnly {
        equilibrium_model: Arc<OdeModel>,
        runtime: Arc<SolveRuntime>,
    },
    General {
        equilibrium_model: Arc<OdeModel>,
        runtime: Arc<SolveRuntime>,
    },
}

impl PreparedSimulation {
    pub fn backend(&self) -> SimBackend {
        SimBackend::Diffsol
    }

    pub fn run(&self) -> Result<SimResult, SimError> {
        run_prepared_simulation(self)
    }

    pub fn check_initialization(&self) -> Result<(), SimError> {
        check_initialization(&self.model, &self.opts)
    }

    pub fn model(&self) -> &solve::SolveModel {
        &self.model
    }
}

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
    } else if can_use_state_only_bdf(model) {
        tracing::debug!(
            target: "rumoca_solver_diffsol::bdf_path",
            states = model.state_scalar_count(),
            "state-only BDF path (pure ODE, AD state Jacobian)"
        );
        PreparedSimulationState::StateOnly {
            equilibrium_model: Arc::new(OdeModel::new(model)?),
            runtime: Arc::new(SolveRuntime::new(model)),
        }
    } else {
        tracing::debug!(
            target: "rumoca_solver_diffsol::bdf_path",
            states = model.state_scalar_count(),
            "general/implicit BDF path (AD implicit Jacobian)"
        );
        PreparedSimulationState::General {
            equilibrium_model: Arc::new(OdeModel::new(model)?),
            runtime: Arc::new(SolveRuntime::new(model)),
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
    let state = initial_bdf_state(model, equilibrium_model, &problem, &current_y, &params)?;
    let nl_solver =
        NewtonNonlinearSolver::new(LinearSolver::default(), BacktrackingLineSearch::default());
    // One BDF solver that lives for the whole simulation. After zero-crossing
    // events we pin the state to root time (state_mut_back), apply Modelica
    // event semantics, then write updated (t, y, params) back via state_mut()
    // so BDF history is preserved when only params change and naturally
    // reinitialised for state-jump events.
    let mut solver = solver_call("BDF new", || {
        diffsol::Bdf::<_, _, _, diffsol::NoAug<_>>::new(&problem, state, nl_solver)
    })?;

    match simulate_state_targets(
        model,
        opts,
        &times,
        equilibrium_model,
        &runtime_params,
        &mut solver,
        StateTrajectory {
            params: &mut params,
            data: &mut data,
            recorded_times: &mut recorded_times,
            current_t: &mut current_t,
            current_y: &mut current_y,
            runtime,
        },
    ) {
        Ok(()) => Ok(build_sim_result_from_solve_model(
            model,
            recorded_times,
            data,
            None,
        )),
        Err(SimError::Terminated { time, message }) => {
            current_t = time;
            refresh_observation_discrete_rows(
                model,
                &equilibrium_model.runtime_state,
                &mut current_y,
                &mut params,
                current_t,
                opts.atol.max(1.0e-10),
            )?;
            runtime_params.borrow_mut().copy_from_slice(&params);
            record_sample_if_new(
                Some(runtime),
                model,
                &current_y,
                &params,
                &mut recorded_times,
                &mut data,
                current_t,
            )?;
            Ok(build_sim_result_from_solve_model(
                model,
                recorded_times,
                data,
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
        equilibrium_model,
        &mut current_y,
        &mut params,
        current_t,
    )?;
    let mut current_state = current_y[..model.state_scalar_count()].to_vec();
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
    let mut solver = solver_call("BDF new", || {
        diffsol::Bdf::<_, _, _, diffsol::NoAug<_>>::new(&problem, state, nl_solver)
    })?;
    let mut stop_schedule = SolveStopSchedule::new(&model.problem, opts.t_start, opts.t_end);

    run_state_only_bdf_samples(StateOnlyBdfLoop {
        model,
        opts,
        runtime,
        equilibrium_model,
        runtime_params: &runtime_params,
        solver: &mut solver,
        current_state: &mut current_state,
        current_y: &mut current_y,
        params: &mut params,
        current_t: &mut current_t,
        stop_schedule: &mut stop_schedule,
        recorded_times: &mut recorded_times,
        data: &mut data,
        times,
    })?;

    trace_bdf_eval_counter_snapshot("state-only-bdf", &eval_counters);

    Ok(build_sim_result_from_solve_model(
        model,
        recorded_times,
        data,
        None,
    ))
}

struct StateOnlyBdfLoop<'a, S> {
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    runtime: &'a SolveRuntime,
    equilibrium_model: &'a OdeModel,
    runtime_params: &'a RuntimeParameters,
    solver: &'a mut S,
    current_state: &'a mut Vec<f64>,
    current_y: &'a mut Vec<f64>,
    params: &'a mut Vec<f64>,
    current_t: &'a mut f64,
    stop_schedule: &'a mut SolveStopSchedule,
    recorded_times: &'a mut Vec<f64>,
    data: &'a mut Vec<Vec<f64>>,
    times: &'a [f64],
}

fn run_state_only_bdf_samples<'a, Eqn, S>(mut ctx: StateOnlyBdfLoop<'_, S>) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    for &target in ctx.times {
        if ctx
            .recorded_times
            .last()
            .is_some_and(|last| sample_time_match_with_tol(*last, target))
        {
            continue;
        }
        advance_state_only_bdf_sample(&mut ctx, target)?;
    }
    Ok(())
}

fn advance_state_only_bdf_sample<'a, Eqn, S>(
    ctx: &mut StateOnlyBdfLoop<'_, S>,
    target: f64,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let tol = ctx.opts.atol.max(1.0e-12);
    while target > *ctx.current_t + tol {
        let (stop_time, event_stop) = next_runtime_event_stop(
            ctx.model,
            &ctx.equilibrium_model.runtime_state,
            ctx.current_y,
            ctx.params,
            ctx.stop_schedule,
            *ctx.current_t,
            target,
        )
        .map_err(|err| SimError::SolveIr(err.to_string()))?;
        advance_state_only_to_target(StateOnlyAdvance {
            model: ctx.model,
            opts: ctx.opts,
            runtime: ctx.runtime,
            equilibrium_model: ctx.equilibrium_model,
            runtime_params: ctx.runtime_params,
            solver: ctx.solver,
            current_state: ctx.current_state,
            current_y: ctx.current_y,
            params: ctx.params,
            current_t: ctx.current_t,
            target: stop_time,
            recorded_times: ctx.recorded_times,
            data: ctx.data,
        })?;
        if apply_state_only_bdf_event_if_due(ctx, event_stop, stop_time, target)? {
            continue;
        }
        if event_stop.is_none() {
            break;
        }
    }
    Ok(())
}

fn apply_state_only_bdf_event_if_due<'a, Eqn, S>(
    ctx: &mut StateOnlyBdfLoop<'_, S>,
    event_stop: Option<RuntimeEventStop>,
    stop_time: f64,
    target: f64,
) -> Result<bool, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let Some(event) = event_stop else {
        return Ok(false);
    };
    if !sample_time_match_with_tol(*ctx.current_t, stop_time) {
        return Ok(false);
    }
    apply_state_only_scheduled_time_event(
        StateOnlyEventContext {
            model: ctx.model,
            opts: ctx.opts,
            runtime: ctx.runtime,
            equilibrium_model: ctx.equilibrium_model,
            runtime_params: ctx.runtime_params,
            solver: ctx.solver,
            current_state: ctx.current_state,
            current_y: ctx.current_y,
            params: ctx.params,
            current_t: ctx.current_t,
        },
        event,
        target,
        ObservationBuffers {
            recorded_times: ctx.recorded_times,
            data: ctx.data,
        },
    )?;
    ctx.stop_schedule.advance_past(*ctx.current_t);
    Ok(true)
}

struct StateOnlyEventContext<'a, S> {
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    runtime: &'a SolveRuntime,
    equilibrium_model: &'a OdeModel,
    runtime_params: &'a RuntimeParameters,
    solver: &'a mut S,
    current_state: &'a mut Vec<f64>,
    current_y: &'a mut Vec<f64>,
    params: &'a mut Vec<f64>,
    current_t: &'a mut f64,
}

fn apply_state_only_scheduled_time_event<'a, Eqn, S>(
    ctx: StateOnlyEventContext<'_, S>,
    event: RuntimeEventStop,
    target: f64,
    observations: ObservationBuffers<'_>,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let tol = ctx.opts.atol.max(1.0e-10);
    prepare_fixed_event_left_limit(
        ctx.model,
        ctx.equilibrium_model,
        ctx.current_y,
        ctx.params,
        *ctx.current_t,
        tol,
        event,
    )?;
    apply_event_updates(
        ctx.model,
        ctx.equilibrium_model,
        ctx.current_y,
        ctx.params,
        *ctx.current_t,
        tol,
        event.pre_mode,
    )?;
    *ctx.current_t = EventObservation {
        runtime: Some(ctx.runtime),
        model: ctx.model,
        equilibrium_model: ctx.equilibrium_model,
        y: ctx.current_y,
        params: ctx.params,
        tol,
        recorded_times: observations.recorded_times,
        data: observations.data,
    }
    .record_time_event(
        *ctx.current_t,
        time_event_right_limit_cap(event, target, ctx.opts.t_end),
        event,
    )?;
    let t_right = *ctx.current_t;
    settle_algebraics_and_relation_memory(
        ctx.equilibrium_model,
        ctx.current_y,
        ctx.params,
        t_right,
        ctx.model.state_scalar_count(),
        &ctx.model
            .problem
            .solve_layout
            .relation_memory_parameter_indices,
        tol,
    )?;
    ctx.current_state
        .copy_from_slice(&ctx.current_y[..ctx.model.state_scalar_count()]);
    let dy = ctx.runtime.eval_state_derivatives(
        t_right,
        ctx.current_state,
        ctx.params,
        tol,
        EVENT_UPDATE_MAX_ITERS,
    )?;
    reset_solver_state(
        ctx.solver,
        ctx.runtime_params,
        ctx.current_state,
        &dy,
        ctx.params,
        *ctx.current_t,
        rumoca_solver::event_solver_step_cap(ctx.opts.dt),
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

struct StateOnlyAdvance<'a, S> {
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    runtime: &'a SolveRuntime,
    equilibrium_model: &'a OdeModel,
    runtime_params: &'a RuntimeParameters,
    solver: &'a mut S,
    current_state: &'a mut Vec<f64>,
    current_y: &'a mut Vec<f64>,
    params: &'a mut Vec<f64>,
    current_t: &'a mut f64,
    target: f64,
    recorded_times: &'a mut Vec<f64>,
    data: &'a mut Vec<Vec<f64>>,
}

fn advance_state_only_to_target<'a, Eqn, S>(ctx: StateOnlyAdvance<'_, S>) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    if ctx.target <= *ctx.current_t {
        return Ok(());
    }
    set_solver_stop_time(ctx.solver, ctx.target)?;
    loop {
        match solver_call("BDF step", || ctx.solver.step()) {
            Ok(OdeSolverStopReason::TstopReached) => {
                ctx.current_state
                    .copy_from_slice(ctx.solver.state().y.as_slice());
                *ctx.current_t = ctx.solver.state().t;
                *ctx.current_y = ctx.runtime.full_solver_y(
                    *ctx.current_t,
                    ctx.current_state,
                    ctx.params,
                    ctx.opts.atol.max(1.0e-10),
                    256,
                )?;
                refresh_observation_discrete_rows(
                    ctx.model,
                    &ctx.equilibrium_model.runtime_state,
                    ctx.current_y,
                    ctx.params,
                    *ctx.current_t,
                    ctx.opts.atol.max(1.0e-10),
                )?;
                record_sample_if_new(
                    Some(ctx.runtime),
                    ctx.model,
                    ctx.current_y,
                    ctx.params,
                    ctx.recorded_times,
                    ctx.data,
                    *ctx.current_t,
                )?;
                return Ok(());
            }
            Ok(OdeSolverStopReason::InternalTimestep) => continue,
            Ok(OdeSolverStopReason::RootFound(t_root, root_idx)) => {
                trace_bdf_state_root(ctx.runtime, ctx.current_state, ctx.params, t_root, root_idx);
                handle_state_only_root(ctx, t_root)?;
                return Ok(());
            }
            Err(err) => return Err(err),
        }
    }
}

fn handle_state_only_root<'a, Eqn, S>(
    ctx: StateOnlyAdvance<'_, S>,
    t_root: f64,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    ctx.solver
        .state_mut_back(t_root)
        .map_err(|e| SimError::SolverError(format!("state_mut_back: {e}")))?;
    ctx.current_state
        .copy_from_slice(ctx.solver.state().y.as_slice());
    *ctx.current_t = ctx.solver.state().t;
    *ctx.current_y = ctx.runtime.full_solver_y(
        *ctx.current_t,
        ctx.current_state,
        ctx.params,
        ctx.opts.atol.max(1.0e-10),
        256,
    )?;
    record_sample_if_new(
        Some(ctx.runtime),
        ctx.model,
        ctx.current_y,
        ctx.params,
        ctx.recorded_times,
        ctx.data,
        *ctx.current_t,
    )?;

    let event_pre_y = ctx.current_y.clone();
    let event_pre_p = ctx.params.clone();
    let right_t = event_right_limit_time(*ctx.current_t).min(ctx.target);
    let dt = right_t - *ctx.current_t;
    if dt > 0.0 {
        let dy = ctx.runtime.eval_state_derivatives(
            *ctx.current_t,
            ctx.current_state,
            ctx.params,
            ctx.opts.atol.max(1.0e-10),
            256,
        )?;
        for (slot, derivative) in ctx.current_state.iter_mut().zip(dy) {
            *slot += dt * derivative;
        }
        *ctx.current_t = right_t;
    }
    *ctx.current_y = ctx.runtime.full_solver_y(
        *ctx.current_t,
        ctx.current_state,
        ctx.params,
        ctx.opts.atol.max(1.0e-10),
        256,
    )?;
    apply_event_updates_with_event_pre(EventUpdateInput {
        model: ctx.model,
        ode_model: ctx.equilibrium_model,
        y: ctx.current_y,
        p: ctx.params,
        t: *ctx.current_t,
        tol: ctx.opts.atol.max(1.0e-10),
        event_pre_y: &event_pre_y,
        event_pre_p: &event_pre_p,
    })?;
    settle_prepared_state(
        ctx.model,
        ctx.equilibrium_model,
        ctx.current_y,
        ctx.params,
        *ctx.current_t,
        ctx.opts,
    )?;
    ctx.current_state
        .copy_from_slice(&ctx.current_y[..ctx.model.state_scalar_count()]);
    let dy = ctx.runtime.eval_state_derivatives(
        *ctx.current_t,
        ctx.current_state,
        ctx.params,
        ctx.opts.atol.max(1.0e-10),
        256,
    )?;
    reset_solver_state(
        ctx.solver,
        ctx.runtime_params,
        ctx.current_state,
        &dy,
        ctx.params,
        *ctx.current_t,
        rumoca_solver::event_solver_step_cap(ctx.opts.dt),
    )?;
    record_sample_if_new(
        Some(ctx.runtime),
        ctx.model,
        ctx.current_y,
        ctx.params,
        ctx.recorded_times,
        ctx.data,
        *ctx.current_t,
    )
}

fn trace_bdf_state_root(
    runtime: &SolveRuntime,
    state: &[f64],
    params: &[f64],
    root_t: f64,
    root_idx: usize,
) {
    if !tracing::enabled!(target: "rumoca_solver_diffsol::bdf", tracing::Level::DEBUG) {
        return;
    }
    let roots = runtime
        .eval_root_conditions(root_t, state, params, 1.0e-10, EVENT_UPDATE_MAX_ITERS)
        .unwrap_or_default();
    let near = roots
        .iter()
        .enumerate()
        .filter(|(_, value)| value.abs() <= 1.0e-5)
        .map(|(idx, value)| format!("{idx}:{value:.3e}"))
        .collect::<Vec<_>>()
        .join(", ");
    tracing::debug!(target: "rumoca_solver_diffsol::bdf", "state-root t={root_t:.12} root_idx={root_idx} near=[{near}]");
}

struct StateTrajectory<'a> {
    params: &'a mut Vec<f64>,
    data: &'a mut Vec<Vec<f64>>,
    recorded_times: &'a mut Vec<f64>,
    current_t: &'a mut f64,
    current_y: &'a mut Vec<f64>,
    runtime: &'a SolveRuntime,
}

// SPEC_0021: Exception - central BDF event loop keeps step advancement,
// zero-crossing handling, and sample recording in a single ordered routine.
#[allow(clippy::too_many_lines)]
fn simulate_state_targets<'a, Eqn, S>(
    model: &solve::SolveModel,
    opts: &SimOptions,
    times: &[f64],
    equilibrium_model: &OdeModel,
    runtime_params: &RuntimeParameters,
    solver: &mut S,
    state: StateTrajectory<'_>,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
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
                ),
                AdvanceState {
                    current_y: state.current_y,
                    params: state.params,
                    current_t: state.current_t,
                },
                target,
                solver,
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
                ),
                AdvanceState {
                    current_y: state.current_y,
                    params: state.params,
                    current_t: state.current_t,
                },
                stop_time,
                event_stop,
                solver,
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
                    ),
                    AdvanceState {
                        current_y: state.current_y,
                        params: state.params,
                        current_t: state.current_t,
                    },
                    event,
                    target,
                    solver,
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

fn initialize_state_runtime_values(
    model: &solve::SolveModel,
    opts: &SimOptions,
    equilibrium_model: &OdeModel,
    current_y: &mut [f64],
    params: &mut [f64],
    current_t: f64,
) -> Result<(), SimError> {
    set_initial_event_flag(model, params, true);
    let mut initial_projection_params = params.to_vec();
    seed_initial_discrete_values(
        model,
        equilibrium_model,
        current_y,
        &mut initial_projection_params,
        current_t,
        opts.atol.max(1.0e-10),
    )?;
    settle_initial_projection_context(
        model,
        equilibrium_model,
        current_y,
        &mut initial_projection_params,
        current_t,
        opts.atol.max(1.0e-10),
    )?;
    project_initial_unknowns(
        model,
        equilibrium_model,
        current_y,
        &initial_projection_params,
        current_t,
        opts.atol.max(1.0e-10),
    )?;
    seed_initial_discrete_values(
        model,
        equilibrium_model,
        current_y,
        &mut initial_projection_params,
        current_t,
        opts.atol.max(1.0e-10),
    )?;
    project_initial_algebraics_and_updates(
        model,
        equilibrium_model,
        current_y,
        &mut initial_projection_params,
        current_t,
        opts.atol.max(1.0e-10),
    )?;
    params.copy_from_slice(&initial_projection_params);
    settle_algebraics_and_relation_memory(
        equilibrium_model,
        current_y,
        params,
        current_t,
        model.state_scalar_count(),
        &model.problem.solve_layout.relation_memory_parameter_indices,
        opts.atol.max(1.0e-10),
    )?;
    let initial_event_mode = initial_static_event_pre_mode(&model.problem, current_t);
    apply_event_updates(
        model,
        equilibrium_model,
        current_y,
        params,
        current_t,
        opts.atol.max(1.0e-10),
        initial_event_mode.unwrap_or(EventPreMode::FollowCurrent),
    )?;
    set_initial_event_flag(model, params, false);
    if initial_event_mode.is_some() {
        apply_post_initial_event_updates(
            model,
            equilibrium_model,
            current_y,
            params,
            current_t,
            opts.atol.max(1.0e-10),
        )?;
    }
    Ok(())
}

fn project_initial_algebraics_and_updates(
    model: &solve::SolveModel,
    equilibrium_model: &OdeModel,
    current_y: &mut [f64],
    params: &mut [f64],
    current_t: f64,
    tol: f64,
) -> Result<(), SimError> {
    for _ in 0..EVENT_UPDATE_MAX_ITERS {
        project_initial_unknowns(model, equilibrium_model, current_y, params, current_t, tol)?;
        let updates_changed = apply_initialization_updates(
            model,
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
    let projection_indices = initial_projection_indices(model);
    project_initial_variables_with_plan(
        equilibrium_model,
        current_y,
        params,
        current_t,
        &projection_indices,
        &model.problem.initialization.projection_plan,
        tol,
    )
    .map_err(SimError::from)
}

fn initial_projection_indices(model: &solve::SolveModel) -> Vec<usize> {
    let layout = &model.problem.solve_layout;
    let state_count = layout.state_scalar_count();
    let algebraic_end = state_count + layout.algebraic_scalar_count();
    let meta_by_slot = solve_variable_meta_by_solver_slot(model);
    (0..layout.solver_scalar_count())
        .filter_map(|idx| {
            if idx >= state_count && idx < algebraic_end {
                return Some(idx);
            }
            let meta = meta_by_slot.get(idx).copied().flatten()?;
            (idx < state_count && !meta.is_state && meta.fixed != Some(true)).then_some(idx)
        })
        .collect()
}

fn solve_variable_meta_by_solver_slot(
    model: &solve::SolveModel,
) -> Vec<Option<&solve::SolveVariableMeta>> {
    let layout = &model.problem.solve_layout;
    let mut meta_by_slot = vec![None; layout.solver_scalar_count()];
    for meta in &model.variable_meta {
        let Some(idx) = layout.solver_idx_for_target(&meta.name) else {
            continue;
        };
        if let Some(slot) = meta_by_slot.get_mut(idx) {
            *slot = Some(meta);
        }
    }
    meta_by_slot
}

struct EventObservation<'a> {
    runtime: Option<&'a SolveRuntime>,
    model: &'a solve::SolveModel,
    equilibrium_model: &'a OdeModel,
    y: &'a mut [f64],
    params: &'a mut [f64],
    tol: f64,
    recorded_times: &'a mut Vec<f64>,
    data: &'a mut [Vec<f64>],
}

impl EventObservation<'_> {
    fn record_time_event(
        &mut self,
        event_t: f64,
        horizon_t: f64,
        event: RuntimeEventStop,
    ) -> Result<f64, SimError> {
        refresh_observation_discrete_rows(
            self.model,
            &self.equilibrium_model.runtime_state,
            self.y,
            self.params,
            event_t,
            self.tol,
        )?;
        record_sample_if_new(
            self.runtime,
            self.model,
            self.y,
            self.params,
            self.recorded_times,
            self.data,
            event_t,
        )?;
        let right_t = event_right_limit_time(event_t).min(horizon_t);
        if event.observe_right_limit
            && event.pre_mode == EventPreMode::FollowCurrent
            && right_t > event_t
            && !sample_time_match_with_tol(right_t, event_t)
        {
            apply_event_updates(
                self.model,
                self.equilibrium_model,
                self.y,
                self.params,
                right_t,
                self.tol,
                event.pre_mode,
            )?;
            refresh_observation_discrete_rows(
                self.model,
                &self.equilibrium_model.runtime_state,
                self.y,
                self.params,
                right_t,
                self.tol,
            )?;
            record_sample_if_new(
                self.runtime,
                self.model,
                self.y,
                self.params,
                self.recorded_times,
                self.data,
                right_t,
            )?;
        }
        if event.observe_right_limit {
            Ok(right_t)
        } else {
            Ok(event_t)
        }
    }
}

fn time_event_right_limit_cap(event: RuntimeEventStop, target: f64, horizon: f64) -> f64 {
    match (event.pre_mode, event.observe_right_limit) {
        (EventPreMode::Fixed, _) => target,
        (EventPreMode::FollowCurrent, true) => horizon,
        (EventPreMode::FollowCurrent, false) => target,
    }
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
        );
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
        EventPreMode::Fixed => {
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

fn set_initial_event_flag(model: &solve::SolveModel, params: &mut [f64], value: bool) {
    let Some(index) = model.problem.solve_layout.initial_event_parameter_index else {
        return;
    };
    if let Some(slot) = params.get_mut(index) {
        *slot = f64::from(value);
    }
}

#[derive(Clone, Copy)]
struct AdvanceContext<'a> {
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    equilibrium_model: &'a OdeModel,
    runtime_params: &'a RuntimeParameters,
    runtime: &'a SolveRuntime,
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
) -> AdvanceContext<'a> {
    AdvanceContext {
        model,
        opts,
        equilibrium_model,
        runtime_params,
        runtime,
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

fn resolve_pending_root<'a, Eqn, S>(
    pending_root_t: &mut Option<f64>,
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    solver: &mut S,
    stop_schedule: &mut SolveStopSchedule,
) -> Result<PendingRootAction, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let Some(prt) = *pending_root_t else {
        return Ok(PendingRootAction::None);
    };
    if !sample_time_match_with_tol(target, prt) && target < prt {
        let y_at = solver
            .interpolate(target)
            .map_err(|e| SimError::SolverError(format!("interpolate: {e}")))?;
        state.current_y.copy_from_slice(y_at.as_slice());
        *state.current_t = target;
        state
            .params
            .copy_from_slice(ctx.runtime_params.borrow().as_slice());
        refresh_interpolated_sample_state(ctx, state, target)?;
        return Ok(PendingRootAction::Break);
    }

    *pending_root_t = None;
    handle_root_crossing(ctx, state, prt, target, solver)?;
    stop_schedule.advance_past(solver.state().t);
    Ok(PendingRootAction::Continue)
}

fn refresh_interpolated_sample_state(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
) -> Result<(), SimError> {
    settle_prepared_state(
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

fn apply_scheduled_time_event<'a, Eqn, S>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    event: RuntimeEventStop,
    target: f64,
    solver: &mut S,
    observations: ObservationBuffers<'_>,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
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
    apply_event_updates(
        ctx.model,
        ctx.equilibrium_model,
        state.current_y,
        state.params,
        *state.current_t,
        tol,
        event.pre_mode,
    )?;
    *state.current_t = EventObservation {
        runtime: Some(ctx.runtime),
        model: ctx.model,
        equilibrium_model: ctx.equilibrium_model,
        y: state.current_y,
        params: state.params,
        tol,
        recorded_times: observations.recorded_times,
        data: observations.data,
    }
    .record_time_event(
        *state.current_t,
        time_event_right_limit_cap(event, target, ctx.opts.t_end),
        event,
    )?;
    reinitialize_solver_after_time_event(ctx, state, solver, tol)
}

fn reinitialize_solver_after_time_event<'a, Eqn, S>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    solver: &mut S,
    tol: f64,
) -> Result<(), SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    let t_right = *state.current_t;
    settle_algebraics_and_relation_memory(
        ctx.equilibrium_model,
        state.current_y,
        state.params,
        t_right,
        ctx.model.state_scalar_count(),
        &ctx.model
            .problem
            .solve_layout
            .relation_memory_parameter_indices,
        tol,
    )?;
    let dy_sched = bdf_derivative_guess(
        ctx.model,
        ctx.equilibrium_model,
        state.current_y,
        state.params,
        t_right,
    )?;
    reset_solver_state(
        solver,
        ctx.runtime_params,
        state.current_y,
        &dy_sched,
        state.params,
        *state.current_t,
        rumoca_solver::event_solver_step_cap(ctx.opts.dt),
    )
}

fn advance_to_target_once<'a, Eqn, S>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    event_stop: Option<RuntimeEventStop>,
    solver: &mut S,
    deferred_root: &mut Option<f64>,
) -> Result<bool, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    if event_stop.is_some() {
        return advance_to_scheduled_stop(ctx, state, target, solver);
    }

    advance_output_interval(ctx, state, target, solver, deferred_root)
}

fn advance_to_scheduled_stop<'a, Eqn, S>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    solver: &mut S,
) -> Result<bool, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    if solver.state().t > target {
        solver
            .state_mut_back(target)
            .map_err(|e| SimError::SolverError(format!("state_mut_back: {e}")))?;
    }
    if sample_time_match_with_tol(solver.state().t, target) {
        *state.current_t = target;
        state.current_y.copy_from_slice(solver.state().y.as_slice());
        state
            .params
            .copy_from_slice(ctx.runtime_params.borrow().as_slice());
        return Ok(false);
    }
    set_solver_stop_time(solver, target)?;
    loop {
        match solver_call("BDF step", || solver.step()) {
            Ok(OdeSolverStopReason::TstopReached) => {
                *state.current_t = solver.state().t;
                state.current_y.copy_from_slice(solver.state().y.as_slice());
                state
                    .params
                    .copy_from_slice(ctx.runtime_params.borrow().as_slice());
                return Ok(false);
            }
            Ok(OdeSolverStopReason::InternalTimestep) => continue,
            Ok(OdeSolverStopReason::RootFound(t_root, _)) => {
                trace_bdf_step_event("scheduled-root", solver.state().t, Some(t_root));
                return handle_root_crossing(ctx, state, t_root, target, solver);
            }
            Err(e) => {
                trace_bdf_step_failure(ctx, state, solver.state().t, &e.to_string());
                return Err(e);
            }
        }
    }
}

fn advance_output_interval<'a, Eqn, S>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    target: f64,
    solver: &mut S,
    deferred_root: &mut Option<f64>,
) -> Result<bool, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    loop {
        if solver.state().t >= target {
            let y_at_target = solver
                .interpolate(target)
                .map_err(|e| SimError::SolverError(format!("interpolate: {e}")))?;
            state.current_y.copy_from_slice(y_at_target.as_slice());
            *state.current_t = target;
            state
                .params
                .copy_from_slice(ctx.runtime_params.borrow().as_slice());
            return Ok(false);
        }

        match solver_call("BDF step", || solver.step()) {
            Ok(OdeSolverStopReason::TstopReached | OdeSolverStopReason::InternalTimestep) => {}
            Ok(OdeSolverStopReason::RootFound(t_root, _)) => {
                trace_bdf_step_event("output-root", solver.state().t, Some(t_root));
                let root_after_target =
                    t_root > target && !sample_time_match_with_tol(t_root, target);
                if !root_after_target {
                    return handle_root_crossing(ctx, state, t_root, target, solver);
                }
                let y_at_target = solver
                    .interpolate(target)
                    .map_err(|e| SimError::SolverError(format!("interpolate: {e}")))?;
                state.current_y.copy_from_slice(y_at_target.as_slice());
                *state.current_t = target;
                state
                    .params
                    .copy_from_slice(ctx.runtime_params.borrow().as_slice());
                *deferred_root = Some(t_root);
                return Ok(false);
            }
            Err(e) => {
                trace_bdf_step_failure(ctx, state, solver.state().t, &e.to_string());
                return Err(e);
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
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    solver_t: f64,
    error: &str,
) {
    if !tracing::enabled!(target: "rumoca_solver_diffsol::bdf", tracing::Level::DEBUG) {
        return;
    }
    let mut roots = vec![0.0; ctx.equilibrium_model.root_conditions.len().max(1)];
    let root_summary = match ctx.equilibrium_model.eval_roots(
        state.current_y,
        state.params,
        *state.current_t,
        &mut roots,
    ) {
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
        "step-fail current_t={:.12} solver_t={solver_t:.12} {root_summary} err={error}",
        *state.current_t
    );
}

fn handle_root_crossing<'a, Eqn, S>(
    ctx: AdvanceContext<'_>,
    state: AdvanceState<'_>,
    t_root: f64,
    target: f64,
    solver: &mut S,
) -> Result<bool, SimError>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    solver
        .state_mut_back(t_root)
        .map_err(|e| SimError::SolverError(format!("state_mut_back: {e}")))?;
    let mut event_pre_y = solver.state().y.as_slice().to_vec();
    let event_pre_p = ctx.runtime_params.borrow().as_slice().to_vec();
    let root_t = solver.state().t;
    let right_t = event_right_limit_time(root_t).min(target);
    *state.current_t = right_t;
    state.current_y.copy_from_slice(solver.state().y.as_slice());
    state
        .params
        .copy_from_slice(ctx.runtime_params.borrow().as_slice());
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
        model: ctx.model,
        ode_model: ctx.equilibrium_model,
        y: state.current_y,
        p: state.params,
        t: *state.current_t,
        tol: ctx.opts.atol.max(1.0e-10),
        event_pre_y: &event_pre_y,
        event_pre_p: &event_pre_p,
    })?;
    settle_prepared_state(
        ctx.model,
        ctx.equilibrium_model,
        state.current_y,
        state.params,
        *state.current_t,
        ctx.opts,
    )?;
    trace_bdf_post_event_state(ctx, &state);
    let dy_post = bdf_derivative_guess(
        ctx.model,
        ctx.equilibrium_model,
        state.current_y,
        state.params,
        *state.current_t,
    )?;
    reset_solver_state(
        solver,
        ctx.runtime_params,
        state.current_y,
        &dy_post,
        state.params,
        *state.current_t,
        rumoca_solver::event_solver_step_cap(ctx.opts.dt),
    )?;
    Ok(true)
}

fn trace_bdf_post_event_state(ctx: AdvanceContext<'_>, state: &AdvanceState<'_>) {
    if !tracing::enabled!(target: "rumoca_solver_diffsol::bdf", tracing::Level::DEBUG) {
        return;
    }
    let mut rhs = vec![0.0; state.current_y.len()];
    let summary = match ctx.equilibrium_model.eval_residual(
        state.current_y,
        state.params,
        *state.current_t,
        &mut rhs,
    ) {
        Ok(()) => {
            let state_count = ctx.model.state_scalar_count().min(rhs.len());
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
        "post-event current_t={:.12} {summary}",
        *state.current_t
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
    model: &solve::SolveModel,
    equilibrium_model: &OdeModel,
    current_y: &mut [f64],
    params: &mut [f64],
    current_t: f64,
    opts: &SimOptions,
) -> Result<(), SimError> {
    settle_algebraics_and_relation_memory(
        equilibrium_model,
        current_y,
        params,
        current_t,
        model.state_scalar_count(),
        &model.problem.solve_layout.relation_memory_parameter_indices,
        opts.atol.max(1.0e-10),
    )
}

fn update_relation_memory_values(
    model: &OdeModel,
    y: &[f64],
    p: &mut [f64],
    t: f64,
    relation_memory_indices: &[usize],
) -> Result<bool, SimError> {
    if relation_memory_indices.is_empty() {
        return Ok(false);
    }
    let mut roots = vec![0.0; model.root_conditions.len().max(1)];
    model.eval_roots(y, p, t, &mut roots)?;
    Ok(update_relation_memory_slots(
        roots.as_slice(),
        p,
        relation_memory_indices,
    ))
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
