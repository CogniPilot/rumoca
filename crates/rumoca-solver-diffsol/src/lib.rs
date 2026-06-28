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
    NewtonNonlinearSolver, OdeEquations, OdeEquationsImplicit, OdeSolverMethod, OdeSolverProblem,
    OdeSolverState, OdeSolverStopReason, VectorHost,
};
use init_projection::{EventObservation, initialize_state_runtime_values};
use rumoca_eval_solve::sim_driver::{
    SimDriverError, SolverStepper, StateTrajectory, StepOutcome, simulate_state_targets,
};
use rumoca_eval_solve::{
    self as solve_eval, RowEvalContext, SolveRuntime, current_dynamic_time_event_stop,
    next_runtime_event_stop, visible_values_with_context,
};
use rumoca_ir_solve as solve;
use rumoca_solver::{
    DiffsolMethod, EventPreMode, RuntimeEventBoundary, RuntimeEventBoundaryHandler,
    RuntimeEventStop, SimOptions, SimResult, SimTermination, SolveStopSchedule,
    build_sim_result_from_solve_model, commit_pre_params_after_event, discrete_row_pre_mode,
    process_runtime_event_boundary, push_visible_values, replace_last_visible_values,
    runtime_event_horizon, runtime_root_event_application_time,
    timeline::sample_time_match_with_tol, write_pre_params_from_sources,
};
pub(crate) use runtime::{
    EventUpdateInput, apply_discrete_value, apply_event_updates,
    apply_event_updates_with_event_pre, apply_initialization_updates, seed_initial_discrete_values,
    settle_algebraics_and_relation_memory,
};
use runtime::{check_no_state_initialization, simulate_no_state_solve_ir};

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
use rumoca_solver::{
    RuntimeSolveError, project_algebraics, project_algebraics_and_detect_changes,
    project_initial_variables_with_plan,
};

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
    let runtime = Arc::new(SolveRuntime::new(model)?);
    let mut current_y = model.initial_y.clone();
    let mut params = model.parameters.clone();
    let mut current_t = opts.t_start;
    initialize_state_runtime_values(
        model,
        opts,
        runtime.as_ref(),
        &equilibrium_model,
        &mut current_y,
        &mut params,
        &mut current_t,
    )?;
    let runtime_params: RuntimeParameters = Rc::new(RefCell::new(params.clone()));
    let problem = build_ode_problem_with_runtime_params_and_initial(
        model,
        opts,
        runtime_params,
        current_t,
        current_y.clone(),
        equilibrium_model.clone(),
        runtime,
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
    let initial_observations = initialize_state_runtime_values(
        model,
        opts,
        runtime,
        equilibrium_model,
        &mut current_y,
        &mut params,
        &mut current_t,
    )?;
    let mut samples = SampleRecorder {
        runtime: Some(runtime.as_ref()),
        model,
        recorded_times: &mut recorded_times,
        data: &mut data,
    };
    record_initial_samples(
        &mut samples,
        SamplePoint {
            y: &current_y,
            params: &params,
            t: current_t,
        },
        &initial_observations,
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
        runtime.clone(),
    )?;
    let mut stepper = build_general_stepper(GeneralStepperInput {
        model,
        opts,
        equilibrium_model,
        runtime,
        runtime_params: runtime_params.clone(),
        problem: &problem,
        current_y: &current_y,
        params: &params,
    })?;
    let result = simulate_state_targets(
        model,
        opts,
        &times,
        &runtime_params,
        stepper.as_mut(),
        StateTrajectory {
            params: &mut params,
            data: &mut data,
            recorded_times: &mut recorded_times,
            current_t: &mut current_t,
            current_y: &mut current_y,
            runtime,
            runtime_state: &equilibrium_model.runtime_state,
        },
    )
    .map_err(SimError::from);

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

struct GeneralStepperInput<'a, 'b, Eqn>
where
    Eqn: OdeEquations,
{
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
    equilibrium_model: &'a Arc<OdeModel>,
    runtime: &'a Arc<SolveRuntime>,
    runtime_params: RuntimeParameters,
    problem: &'a OdeSolverProblem<Eqn>,
    current_y: &'b [f64],
    params: &'b [f64],
}

fn build_general_stepper<'a, Eqn>(
    input: GeneralStepperInput<'a, '_, Eqn>,
) -> Result<Box<dyn SolverStepper + 'a>, SimError>
where
    Eqn:
        OdeEquationsImplicit<M = Matrix, V = Vector, T = f64, C = <Matrix as MatrixCommon>::C> + 'a,
    Eqn::V: VectorHost<T = f64>,
{
    let GeneralStepperInput {
        model,
        opts,
        equilibrium_model,
        runtime,
        runtime_params,
        problem,
        current_y,
        params,
    } = input;
    match opts.diffsol_method {
        DiffsolMethod::Bdf => {
            let state = initial_bdf_state(
                model,
                equilibrium_model.as_ref(),
                problem,
                current_y,
                params,
            )?;
            let nl_solver = NewtonNonlinearSolver::new(
                LinearSolver::default(),
                BacktrackingLineSearch::default(),
            );
            let solver = solver_call("BDF new", || {
                diffsol::Bdf::<_, _, _, diffsol::NoAug<_>>::new(problem, state, nl_solver)
            })?;
            Ok(Box::new(DiffsolStepper::new(DiffsolStepperInputs {
                solver,
                model,
                equilibrium_model: equilibrium_model.as_ref(),
                runtime: runtime.as_ref(),
                runtime_params,
                opts,
                mode: DiffsolMode::General,
            })))
        }
        method @ (DiffsolMethod::Esdirk34 | DiffsolMethod::TrBdf2) => {
            let state = initial_rk_state(
                model,
                equilibrium_model.as_ref(),
                problem,
                current_y,
                params,
            )?;
            let solver = solver_call("SDIRK new", || match method {
                DiffsolMethod::Esdirk34 => problem.esdirk34_solver::<LinearSolver>(state),
                _ => problem.tr_bdf2_solver::<LinearSolver>(state),
            })?;
            Ok(Box::new(DiffsolStepper::new(DiffsolStepperInputs {
                solver,
                model,
                equilibrium_model: equilibrium_model.as_ref(),
                runtime: runtime.as_ref(),
                runtime_params,
                opts,
                mode: DiffsolMode::General,
            })))
        }
    }
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
            let mut samples = SampleRecorder {
                runtime: Some(fin.runtime.as_ref()),
                model: fin.model,
                recorded_times: &mut fin.recorded_times,
                data: &mut fin.data,
            };
            record_sample_if_new(
                &mut samples,
                SamplePoint {
                    y: &fin.current_y,
                    params: &fin.params,
                    t: time,
                },
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
    let initial_observations = initialize_state_runtime_values(
        model,
        opts,
        runtime,
        equilibrium_model,
        &mut current_y,
        &mut params,
        &mut current_t,
    )?;
    let current_state = current_y[..model.state_scalar_count()].to_vec();
    let mut samples = SampleRecorder {
        runtime: Some(runtime.as_ref()),
        model,
        recorded_times: &mut recorded_times,
        data: &mut data,
    };
    record_initial_samples(
        &mut samples,
        SamplePoint {
            y: &current_y,
            params: &params,
            t: current_t,
        },
        &initial_observations,
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
    let mut stepper = DiffsolStepper::new(DiffsolStepperInputs {
        solver,
        model,
        equilibrium_model,
        runtime,
        runtime_params: runtime_params.clone(),
        opts,
        mode: DiffsolMode::StateOnly,
    });

    // Drive the reduced state-only solver through the *same* backend-neutral
    // output / event / root loop as the general path; `DiffsolMode::StateOnly`
    // (inside the stepper) projects the reduced state to the full solver_y.
    let result = simulate_state_targets(
        model,
        opts,
        times,
        &runtime_params,
        &mut stepper,
        StateTrajectory {
            params: &mut params,
            data: &mut data,
            recorded_times: &mut recorded_times,
            current_t: &mut current_t,
            current_y: &mut current_y,
            runtime,
            runtime_state: &equilibrium_model.runtime_state,
        },
    )
    .map_err(SimError::from);

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

/// Map an internal diffsol [`SimError`] into the backend-neutral driver error,
/// preserving `Terminated` so finalization can replay `terminate()` semantics.
fn sim_to_driver(error: SimError) -> SimDriverError {
    match error {
        SimError::Terminated { time, message } => SimDriverError::Terminated { time, message },
        SimError::SolveIr(message) => SimDriverError::SolveIr(message),
        other => SimDriverError::Backend(other.to_string()),
    }
}

impl From<SimDriverError> for SimError {
    fn from(error: SimDriverError) -> Self {
        match error {
            SimDriverError::Runtime(err) => SimError::SolveIr(err.to_string()),
            SimDriverError::Backend(message) => SimError::SolverError(message),
            SimDriverError::SolveIr(message) => SimError::SolveIr(message),
            SimDriverError::Terminated { time, message } => SimError::Terminated { time, message },
        }
    }
}

/// Which system the diffsol solver integrates (folded behind the stepper so the
/// shared driver never sees it).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DiffsolMode {
    /// Full augmented solver_y (identity native↔full conversions).
    General,
    /// Reduced state vector; algebraics recovered by projection.
    StateOnly,
}

/// diffsol adapter implementing the backend-neutral [`SolverStepper`] over an
/// `OdeSolverMethod` plus the `OdeModel` / runtime context its projection, reset,
/// and event kernels need.
struct DiffsolStepper<'a, Eqn, S> {
    solver: S,
    model: &'a solve::SolveModel,
    equilibrium_model: &'a OdeModel,
    runtime: &'a SolveRuntime,
    runtime_params: RuntimeParameters,
    opts: &'a SimOptions,
    mode: DiffsolMode,
    _eqn: std::marker::PhantomData<fn() -> Eqn>,
}

struct DiffsolStepperInputs<'a, S> {
    solver: S,
    model: &'a solve::SolveModel,
    equilibrium_model: &'a OdeModel,
    runtime: &'a SolveRuntime,
    runtime_params: RuntimeParameters,
    opts: &'a SimOptions,
    mode: DiffsolMode,
}

impl<'a, Eqn, S> DiffsolStepper<'a, Eqn, S>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    fn new(inputs: DiffsolStepperInputs<'a, S>) -> Self {
        Self {
            solver: inputs.solver,
            model: inputs.model,
            equilibrium_model: inputs.equilibrium_model,
            runtime: inputs.runtime,
            runtime_params: inputs.runtime_params,
            opts: inputs.opts,
            mode: inputs.mode,
            _eqn: std::marker::PhantomData,
        }
    }

    fn tol(&self) -> f64 {
        self.opts.atol.max(1.0e-10)
    }
}

impl<'a, Eqn, S> SolverStepper for DiffsolStepper<'a, Eqn, S>
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

    fn step(&mut self) -> Result<StepOutcome, SimDriverError> {
        match solver_call("BDF step", || self.solver.step()).map_err(sim_to_driver)? {
            OdeSolverStopReason::TstopReached => Ok(StepOutcome::Stop),
            OdeSolverStopReason::InternalTimestep => Ok(StepOutcome::Internal),
            OdeSolverStopReason::RootFound(t_root, _) => Ok(StepOutcome::Root { t_root }),
        }
    }

    fn set_stop_time(&mut self, stop_time: f64) -> Result<(), SimDriverError> {
        set_solver_stop_time(&mut self.solver, stop_time).map_err(sim_to_driver)
    }

    fn interpolate(&mut self, t: f64) -> Result<Vec<f64>, SimDriverError> {
        self.solver
            .interpolate(t)
            .map(|v| v.as_slice().to_vec())
            .map_err(|e| SimDriverError::Backend(format!("interpolate: {e}")))
    }

    fn state_mut_back(&mut self, t: f64) -> Result<(), SimDriverError> {
        self.solver
            .state_mut_back(t)
            .map_err(|e| SimDriverError::Backend(format!("state_mut_back: {e}")))
    }

    fn native_to_full_y(
        &self,
        native: &[f64],
        t: f64,
        params: &[f64],
    ) -> Result<Vec<f64>, SimDriverError> {
        match self.mode {
            DiffsolMode::General => Ok(native.to_vec()),
            DiffsolMode::StateOnly => {
                let state_count = self.model.state_scalar_count().min(native.len());
                Ok(self.runtime.full_solver_y(
                    t,
                    &native[..state_count],
                    params,
                    self.tol(),
                    EVENT_UPDATE_MAX_ITERS,
                )?)
            }
        }
    }

    fn reset_vectors(
        &self,
        current_y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<(Vec<f64>, Vec<f64>), SimDriverError> {
        match self.mode {
            DiffsolMode::General => {
                let dy =
                    bdf_derivative_guess(self.model, self.equilibrium_model, current_y, params, t)
                        .map_err(sim_to_driver)?;
                Ok((current_y.to_vec(), dy))
            }
            DiffsolMode::StateOnly => {
                let state_count = self.model.state_scalar_count().min(current_y.len());
                let native = current_y[..state_count].to_vec();
                let dy = self.runtime.eval_state_derivatives(
                    t,
                    &native,
                    params,
                    self.tol(),
                    EVENT_UPDATE_MAX_ITERS,
                )?;
                Ok((native, dy))
            }
        }
    }

    fn reset(
        &mut self,
        native_y: &[f64],
        native_dy: &[f64],
        params: &[f64],
        t: f64,
        h_cap: f64,
    ) -> Result<(), SimDriverError> {
        reset_solver_state(
            &mut self.solver,
            &self.runtime_params,
            native_y,
            native_dy,
            params,
            t,
            h_cap,
        )
        .map_err(sim_to_driver)
    }

    fn prefer_exact_output_steps(&self) -> bool {
        self.mode == DiffsolMode::StateOnly && !model_is_event_free(self.model)
    }

    fn project_algebraics(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        project_algebraics_and_detect_changes(
            self.equilibrium_model,
            y,
            p,
            t,
            self.equilibrium_model.state_count_for_projection(),
            tol,
        )
    }

    fn derivative_guess(&self, y: &[f64], p: &[f64], t: f64) -> Result<Vec<f64>, SimDriverError> {
        bdf_derivative_guess(self.model, self.equilibrium_model, y, p, t).map_err(sim_to_driver)
    }

    fn record_sample(
        &self,
        recorded_times: &mut Vec<f64>,
        data: &mut [Vec<f64>],
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<(), SimDriverError> {
        let mut samples = SampleRecorder {
            runtime: Some(self.runtime),
            model: self.model,
            recorded_times,
            data,
        };
        record_sample_if_new(&mut samples, SamplePoint { y, params: p, t }).map_err(sim_to_driver)
    }

    fn refresh_observation(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
    ) -> Result<(), SimDriverError> {
        refresh_observation_discrete_rows(
            self.model,
            &self.equilibrium_model.runtime_state,
            y,
            p,
            t,
            self.tol(),
        )
        .map(|_| ())
        .map_err(sim_to_driver)
    }

    fn trace_step_failure(
        &self,
        y: &[f64],
        params: &[f64],
        current_t: f64,
        solver_t: f64,
        error: &str,
    ) {
        trace_bdf_step_failure(
            self.equilibrium_model,
            y,
            params,
            current_t,
            solver_t,
            error,
        );
    }

    fn trace_post_event_state(&self, y: &[f64], params: &[f64], t: f64) {
        trace_bdf_post_event_state(self.equilibrium_model, self.model, y, params, t);
    }
}

pub(crate) struct SampleRecorder<'a> {
    pub(crate) runtime: Option<&'a SolveRuntime>,
    pub(crate) model: &'a solve::SolveModel,
    pub(crate) recorded_times: &'a mut Vec<f64>,
    pub(crate) data: &'a mut [Vec<f64>],
}

pub(crate) struct SamplePoint<'a> {
    pub(crate) y: &'a [f64],
    pub(crate) params: &'a [f64],
    pub(crate) t: f64,
}

pub(crate) fn record_sample_if_new(
    recorder: &mut SampleRecorder<'_>,
    sample: SamplePoint<'_>,
) -> Result<(), SimError> {
    let values = if let Some(runtime) = recorder.runtime {
        runtime
            .visible_values(sample.y, sample.params, sample.t)
            .map_err(|err| SimError::SolveIr(err.to_string()))?
    } else {
        visible_values(recorder.model, sample.y, sample.params, sample.t)?
    };
    if recorder
        .recorded_times
        .last()
        .is_some_and(|last| sample_time_match_with_tol(*last, sample.t))
    {
        if let Some(last) = recorder.recorded_times.last_mut() {
            *last = sample.t;
        }
        replace_last_visible_values(recorder.data, &values)?;
        return Ok(());
    }
    recorder.recorded_times.push(sample.t);
    push_visible_values(recorder.data, &values)?;
    Ok(())
}

fn record_initial_samples(
    recorder: &mut SampleRecorder<'_>,
    current: SamplePoint<'_>,
    observations: &[solve_eval::InitialEventObservation],
) -> Result<(), SimError> {
    if observations.is_empty() {
        return record_sample_if_new(recorder, current);
    }
    for observation in observations {
        record_sample_if_new(
            recorder,
            SamplePoint {
                y: &observation.y,
                params: &observation.p,
                t: observation.t,
            },
        )?;
    }
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
