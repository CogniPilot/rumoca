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
mod me;
mod ode;
mod prepared;
mod runtime;
pub mod session;

use std::{
    cell::{Cell, RefCell},
    rc::Rc,
    sync::Arc,
};

use bdf::require_state_only_bdf;
pub(crate) use bdf::{
    bdf_derivative_guess, initial_bdf_state, reset_solver_state, solver_call, write_state_to_solver,
};
use diffsol::{
    BacktrackingLineSearch, BdfState, FaerSparseLU, FaerSparseMat, MatrixCommon,
    NewtonNonlinearSolver, OdeEquations, OdeSolverMethod, OdeSolverState, OdeSolverStopReason,
    Vector as _, VectorHost,
};
use init_projection::initialize_state_runtime_values;
use me::{DiffsolMeHost, MeInitialState, MePostEventState, instantiate as instantiate_me_host};
use rumoca_eval_solve::{self as solve_eval, RowEvalContext};
use rumoca_ir_solve as solve;
use rumoca_solver::runtime::driver::{
    SimDriverError, SolverAdvanceBackend, StateTrajectory, StepOutcome, simulate_state_targets,
};
use rumoca_solver::{
    SimOptions, SimResult, SimTermination, SolveRuntime, TimeoutExceeded,
    build_sim_result_from_solve_model, current_dynamic_time_event_stop, push_visible_values,
    replace_last_visible_values, runtime_root_event_application_time, runtime_values_changed,
    stop_time_reached_with_tol, timeline::sample_time_match_with_tol, visible_values_with_context,
};
pub(crate) use runtime::{
    apply_event_updates, refresh_algebraics_and_detect_changes, seed_initial_discrete_values,
    settle_algebraics_and_relation_memory,
};

type Matrix = FaerSparseMat<f64>;
type Vector = <Matrix as MatrixCommon>::V;
type Scalar = <Matrix as MatrixCommon>::T;
pub(crate) type LinearSolver = FaerSparseLU<f64>;
pub(crate) type RuntimeParameters = Rc<RefCell<Vec<f64>>>;

/// Records which [`SimFailureStage`] the backend was running when it produced a
/// failure.
///
/// The backend-neutral driver in `rumoca-solver` funnels every backend failure
/// through `SimDriverError`, which carries no stage. Rather than re-derive the
/// stage downstream from the rendered message, the backend notes it here at the
/// moment it hands the error out, and the run entry point re-attaches it to the
/// `SimError` the caller sees.
pub(crate) type StageRecorder = Rc<Cell<Option<SimFailureStage>>>;

/// Note `stage` as the origin of `error` and hand the error straight back, so a
/// fallible call is annotated by appending `.map_err(|e| note(&r, stage, e))`.
fn note_stage<E>(recorder: &StageRecorder, stage: SimFailureStage, error: E) -> E {
    recorder.set(Some(stage));
    error
}

/// Attach the backend-recorded stage to a driver failure, defaulting to
/// [`SimFailureStage::Integration`] — the driver only ever runs the backend to
/// integrate, so an unnoted failure did surface during integration.
fn stage_driver_failure(recorder: &StageRecorder, error: SimDriverError) -> SimError {
    SimError::from(error).at_stage(recorder.get().unwrap_or(SimFailureStage::Integration))
}

#[derive(Clone)]
pub(crate) struct AlgebraicWarmStart(Rc<RefCell<Vec<f64>>>);

impl AlgebraicWarmStart {
    fn new(solver_y: Vec<f64>) -> Self {
        Self(Rc::new(RefCell::new(solver_y)))
    }

    fn speculative(&self) -> Vec<f64> {
        self.0.borrow().clone()
    }

    fn commit(&self, solver_y: Vec<f64>) {
        *self.0.borrow_mut() = solver_y;
    }
}
pub use error::{SimError, SimFailureStage, StateOnlyRejection};
pub(crate) use ode::{
    OdeModel, build_me_state_ode_problem, build_ode_problem_with_runtime_params_and_initial,
    build_state_ode_problem_with_runtime_params_and_initial, state_ode_problem_input,
    trace_bdf_eval_counter_snapshot, validate_model,
};
pub use prepared::PreparedSimulation;
use prepared::PreparedSimulationState;
use rumoca_solver::RuntimeSolveError;

const EVENT_UPDATE_MAX_ITERS: usize = 256;

pub fn build_simulation(
    model: &solve::SolveModel,
    opts: &SimOptions,
) -> Result<PreparedSimulation, SimError> {
    build_simulation_inner(model, opts)
        .map_err(|error| error.at_stage(SimFailureStage::BackendBuild))
}

/// Backend problem construction. Failures are annotated as
/// [`SimFailureStage::BackendBuild`] by the wrapper above: nothing has been
/// integrated yet, so these are never numeric-solver failures.
fn build_simulation_inner(
    model: &solve::SolveModel,
    opts: &SimOptions,
) -> Result<PreparedSimulation, SimError> {
    let runtime_context = solve_eval::SimulationContext::new();
    runtime_context.hydrate_solve_model(model);
    validate_model(model)?;
    let state = if model.state_scalar_count() == 0 {
        tracing::debug!(target: "rumoca_solver_diffsol::bdf_path", "no-state path");
        PreparedSimulationState::NoState
    } else {
        // The reduced state-only ODE is the only system a state-carrying model
        // is integrated as. A model that fails the contract is rejected by
        // name here; it is not re-expressed as a general implicit DAE.
        require_state_only_bdf(model)?;
        tracing::debug!(
            target: "rumoca_solver_diffsol::bdf_path",
            states = model.state_scalar_count(),
            "state-only BDF path (pure ODE, AD state Jacobian)"
        );
        PreparedSimulationState::StateOnly {
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
    check_initialization_inner(model, opts)
        .map_err(|error| error.at_stage(SimFailureStage::Initialization))
}

/// Settle initial conditions without integrating. Failures are annotated as
/// [`SimFailureStage::Initialization`] by the wrapper above; paths that already
/// recorded a more precise stage keep it.
fn check_initialization_inner(
    model: &solve::SolveModel,
    opts: &SimOptions,
) -> Result<(), SimError> {
    let runtime_context = solve_eval::SimulationContext::new();
    runtime_context.hydrate_solve_model(model);
    validate_model(model)?;
    if model.state_scalar_count() == 0 {
        return rumoca_solver::fmi_me::MeNoStateSession::check_initialization(
            rumoca_solver::fmi_me::MeModelSource::new(model),
            opts.clone(),
        )
        .map_err(Into::into);
    }
    // Same contract, same rejection, as [`build_simulation_inner`]: a model
    // that cannot be built cannot be initialization-checked either, and both
    // must agree on the system so a checked start point is the one the run uses.
    require_state_only_bdf(model)?;
    check_state_only_initialization(model, opts)
}

/// Settle initial conditions for a model that integrates on the reduced
/// state-only path.
///
/// Runs the same three steps [`simulate_state_only_bdf`] runs before its first
/// integration step — the initialization settle/projection pass, the reduced
/// ODE problem build, and the initial `BdfState` construction — so a model that
/// passes the check is a model whose simulation start point actually exists.
///
/// The initial algebraic residual verification that the retired general path
/// performed in `initial_bdf_state` is already inside step three here. The
/// reduced system has no algebraic residual rows of its own; instead
/// `initial_state_only_bdf_state` seeds `dy` from
/// `SolveRuntime::eval_state_derivatives_with_guess`, which solves the projected
/// algebraics at `t0` and ends in `validate_finite_derivatives` — so a start
/// point whose projection fails to converge, or whose derivative is non-finite,
/// is rejected there as `RuntimeSolveError::NonFiniteDerivative` and propagates
/// out of this function. No separate finiteness check is added here: it would be
/// unreachable.
fn check_state_only_initialization(
    model: &solve::SolveModel,
    opts: &SimOptions,
) -> Result<(), SimError> {
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
    let current_state = current_y[..model.state_scalar_count()].to_vec();
    let runtime_params: RuntimeParameters = Rc::new(RefCell::new(params.clone()));
    let algebraic_warm_start = AlgebraicWarmStart::new(current_y.clone());
    let (problem_input, _eval_counters) = state_ode_problem_input(
        &runtime_params,
        &algebraic_warm_start,
        current_t,
        &current_state,
        &runtime,
    );
    let problem =
        build_state_ode_problem_with_runtime_params_and_initial(model, opts, problem_input)?;
    initial_state_only_bdf_state(
        runtime.as_ref(),
        &problem,
        &current_state,
        &params,
        opts,
        &algebraic_warm_start,
        None,
    )
    .map(|_| ())
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
        PreparedSimulationState::NoState => rumoca_solver::fmi_me::MeNoStateSession::simulate(
            rumoca_solver::fmi_me::MeModelSource::new(model),
            opts.clone(),
        )
        .map_err(Into::into),
        PreparedSimulationState::StateOnly {
            equilibrium_model,
            runtime,
        } => {
            let dt = opts.dt.unwrap_or((opts.t_end - opts.t_start).abs() / 500.0);
            let times =
                rumoca_solver::timeline::try_build_output_times(opts.t_start, opts.t_end, dt)
                    .map_err(|error| SimError::SolverError(error.to_string()))?;
            simulate_state_only_bdf(model, opts, &times, equilibrium_model, runtime)
        }
    };
    solve_eval::trace_solve_row_eval_snapshot("bdf");
    result
}

/// Owned trajectory buffers + context needed to turn a `simulate_state_targets`
/// outcome into a `SimResult` for [`simulate_state_only_bdf`].
struct StateSimFinalize<'a> {
    model: &'a solve::SolveModel,
    opts: &'a SimOptions,
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
            fin.runtime.refresh_observation_discrete_rows(
                &mut fin.current_y,
                &mut fin.params,
                time,
                fin.opts.atol.max(1.0e-10),
                EVENT_UPDATE_MAX_ITERS,
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
    let StateOnlyInitialization {
        mut params,
        mut data,
        mut recorded_times,
        mut current_t,
        mut current_y,
        me_host,
    } = initialize_state_only_bdf(model, opts, times, equilibrium_model, runtime)?;
    let current_state = current_y[..model.state_scalar_count()].to_vec();
    let runtime_params: RuntimeParameters = Rc::new(RefCell::new(params.clone()));
    let algebraic_warm_start = AlgebraicWarmStart::new(current_y.clone());
    let ode_build =
        build_me_state_ode_problem(opts, me_host.clone(), current_t, current_state.clone())?;
    let eval_counters = ode_build.eval_counters.clone();
    let problem = ode_build.problem;
    // `OdeBuilder` probes RHS while constructing the problem, before the BDF
    // host has performed its one-time accepted-seed preparation.
    if let Some(error) = me_host.take_callback_error() {
        return Err(error.into());
    }
    let state = initial_state_only_bdf_state(
        runtime,
        &problem,
        &current_state,
        &params,
        opts,
        &algebraic_warm_start,
        Some(&me_host),
    )?;
    me_host.verify_frozen_compatibility_state(
        &algebraic_warm_start.speculative(),
        &params,
        rumoca_solver::fmi_me::MeStage::Initialization,
    )?;
    let nl_solver =
        NewtonNonlinearSolver::new(LinearSolver::default(), BacktrackingLineSearch::default());
    let solver = solver_call("BDF new", || {
        diffsol::Bdf::<_, _, _, diffsol::NoAug<_>>::new(&problem, state, nl_solver)
    });
    if let Some(error) = me_host.take_callback_error() {
        return Err(error.into());
    }
    let solver = solver?;
    let stage_recorder = StageRecorder::default();
    let mut backend = DiffsolAdvanceBackend::new(DiffsolAdvanceBackendInputs {
        solver,
        model,
        equilibrium_model,
        runtime,
        runtime_params: runtime_params.clone(),
        algebraic_warm_start,
        opts,
        stage_recorder: stage_recorder.clone(),
        me_host,
    });

    // Drive the reduced solver through the backend-neutral output / event /
    // root loop; the backend projects the reduced state up to the full
    // `solver_y` the driver reads.
    let result = simulate_state_targets(
        model,
        opts,
        times,
        &runtime_params,
        &mut backend,
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
    .map_err(|error| stage_driver_failure(&stage_recorder, error));

    trace_bdf_eval_counter_snapshot("state-only-bdf", &eval_counters);

    finalize_state_simulation(
        result,
        StateSimFinalize {
            model,
            opts,
            runtime,
            runtime_params: &runtime_params,
            params,
            data,
            recorded_times,
            current_y,
        },
    )
}

struct StateOnlyInitialization {
    params: Vec<f64>,
    data: Vec<Vec<f64>>,
    recorded_times: Vec<f64>,
    current_t: f64,
    current_y: Vec<f64>,
    me_host: DiffsolMeHost,
}

fn initialize_state_only_bdf(
    model: &solve::SolveModel,
    opts: &SimOptions,
    times: &[f64],
    equilibrium_model: &Arc<OdeModel>,
    runtime: &Arc<SolveRuntime>,
) -> Result<StateOnlyInitialization, SimError> {
    let mut params = model.parameters.clone();
    let mut data = vec![Vec::with_capacity(times.len()); model.visible_names.len()];
    let mut recorded_times = Vec::with_capacity(times.len());
    let mut current_t = opts.t_start;
    let mut current_y = model.initial_y.clone();
    let observations = initialize_state_runtime_values(
        model,
        opts,
        runtime,
        equilibrium_model,
        &mut current_y,
        &mut params,
        &mut current_t,
    )?;
    let me_host = instantiate_me_host(rumoca_solver::fmi_me::MeModelSource::new(model), opts)?;
    let current_state = &current_y[..model.state_scalar_count()];
    verify_me_initial_state(
        &me_host.initialize(&current_y, &params)?,
        current_t,
        current_state,
    )?;
    record_initial_samples(
        &mut SampleRecorder {
            runtime: Some(runtime.as_ref()),
            model,
            recorded_times: &mut recorded_times,
            data: &mut data,
        },
        runtime.as_ref(),
        equilibrium_model,
        opts.atol.max(1.0e-10),
        SamplePoint {
            y: &current_y,
            params: &params,
            t: current_t,
        },
        &observations,
    )?;
    Ok(StateOnlyInitialization {
        params,
        data,
        recorded_times,
        current_t,
        current_y,
        me_host,
    })
}

fn verify_me_initial_state(
    component: &MeInitialState,
    legacy_time: f64,
    legacy_states: &[f64],
) -> Result<(), SimError> {
    let diverged = component.time.to_bits() != legacy_time.to_bits()
        || component.states.len() != legacy_states.len()
        || component
            .states
            .iter()
            .zip(legacy_states)
            .any(|(lhs, rhs)| lhs.to_bits() != rhs.to_bits())
        || component.termination.is_some();
    if !diverged {
        return Ok(());
    }
    Err(SimError::RuntimeContract {
        reason: format!(
            "ME initialization diverged from the frozen Diffsol state: component_t={} \
             legacy_t={legacy_time} component_states={} legacy_states={} \
             component_observations={} terminated={}",
            component.time,
            component.states.len(),
            legacy_states.len(),
            component.observations.len(),
            component.termination.is_some(),
        ),
    }
    .at_stage(SimFailureStage::Initialization))
}

fn initial_state_only_bdf_state<Eqn>(
    runtime: &SolveRuntime,
    problem: &diffsol::OdeSolverProblem<Eqn>,
    state_y: &[f64],
    params: &[f64],
    opts: &SimOptions,
    algebraic_warm_start: &AlgebraicWarmStart,
    me_host: Option<&DiffsolMeHost>,
) -> Result<BdfState<Vector>, SimError>
where
    Eqn: diffsol::OdeEquationsImplicit<
            M = Matrix,
            V = Vector,
            T = Scalar,
            C = <Matrix as MatrixCommon>::C,
        >,
{
    let state = BdfState::<Vector>::new_without_initialise(problem)
        .map_err(|err| SimError::SolverError(format!("BDF state init: {err}")));
    if let Some(me_host) = me_host
        && let Some(error) = me_host.take_callback_error()
    {
        return Err(error.into());
    }
    let mut state = state?;
    let mut solver_y = algebraic_warm_start.speculative();
    let dy = runtime.eval_state_derivatives_with_guess(
        problem.t0,
        state_y,
        params,
        &mut solver_y,
        opts.atol,
        256,
    )?;
    algebraic_warm_start.commit(solver_y.clone());
    if let Some(me_host) = me_host {
        me_host.prepare_integrator_initial_seed(
            &solver_y,
            rumoca_solver::fmi_me::MeStage::Initialization,
        )?;
        if let Some(error) = me_host.take_callback_error() {
            return Err(error.into());
        }
    }
    {
        let state_ref = state.as_mut();
        state_ref.y.as_mut_slice().copy_from_slice(state_y);
        state_ref.dy.as_mut_slice().copy_from_slice(&dy);
        *state_ref.t = problem.t0;
    }
    state.set_step_size(problem.h0, &problem.atol, problem.rtol, &problem.eqn, 1);
    if let Some(me_host) = me_host
        && let Some(error) = me_host.take_callback_error()
    {
        return Err(error.into());
    }
    Ok(state)
}

/// Map an internal diffsol [`SimError`] into the backend-neutral driver error,
/// preserving typed outcomes so finalization and failure classification see
/// the same variant on the far side of the driver boundary.
fn sim_to_driver(error: SimError) -> SimDriverError {
    // Stage annotations are recorded separately (see [`StageRecorder`]) because
    // the driver error cannot carry them; peel them off so an annotated failure
    // maps exactly like the unannotated one.
    match error {
        SimError::Staged { inner, .. } => sim_to_driver(*inner),
        SimError::Terminated { time, message } => SimDriverError::Terminated { time, message },
        SimError::AssertionFailed { time, message } => {
            SimDriverError::AssertionFailed { time, message }
        }
        SimError::RuntimeContract { reason } => SimDriverError::RuntimeContract { reason },
        SimError::SolveIr(message) => SimDriverError::SolveIr(message),
        SimError::Timeout { seconds } => SimDriverError::Timeout(TimeoutExceeded { seconds }),
        other => SimDriverError::Backend(other.to_string()),
    }
}

fn staged_sim_to_driver(recorder: &StageRecorder, error: SimError) -> SimDriverError {
    if let Some(stage) = error.stage() {
        recorder.set(Some(stage));
    }
    sim_to_driver(error)
}

impl From<SimDriverError> for SimError {
    fn from(error: SimDriverError) -> Self {
        match error {
            SimDriverError::Runtime(err) => SimError::SolveIr(err.to_string()),
            SimDriverError::Backend(message) => SimError::SolverError(message),
            SimDriverError::SolveIr(message) => SimError::SolveIr(message),
            SimDriverError::Timeout(timeout) => SimError::Timeout {
                seconds: timeout.seconds,
            },
            SimDriverError::AssertionFailed { time, message } => {
                SimError::AssertionFailed { time, message }
            }
            SimDriverError::RuntimeContract { reason } => SimError::RuntimeContract { reason },
            SimDriverError::Terminated { time, message } => SimError::Terminated { time, message },
        }
    }
}

/// diffsol adapter implementing the backend-neutral [`SolverAdvanceBackend`] over an
/// `OdeSolverMethod` plus the `OdeModel` / runtime context its projection, reset,
/// and event kernels need.
///
/// The solver integrates the reduced state vector only; the full `solver_y` the
/// driver reads is recovered by projection, warm-started from
/// [`AlgebraicWarmStart`]. There is no second, full-vector mode: the
/// general/implicit DAE system this adapter used to serve as well is retired
/// (SPEC 0038).
struct DiffsolAdvanceBackend<'a, Eqn, S> {
    solver: S,
    model: &'a solve::SolveModel,
    equilibrium_model: &'a OdeModel,
    runtime: &'a SolveRuntime,
    runtime_params: RuntimeParameters,
    algebraic_warm_start: AlgebraicWarmStart,
    opts: &'a SimOptions,
    active_stop_time: Option<f64>,
    stage_recorder: StageRecorder,
    me_host: DiffsolMeHost,
    pending_component_event: Option<MePostEventState>,
    _eqn: std::marker::PhantomData<fn() -> Eqn>,
}

struct DiffsolAdvanceBackendInputs<'a, S> {
    solver: S,
    model: &'a solve::SolveModel,
    equilibrium_model: &'a OdeModel,
    runtime: &'a SolveRuntime,
    runtime_params: RuntimeParameters,
    algebraic_warm_start: AlgebraicWarmStart,
    opts: &'a SimOptions,
    stage_recorder: StageRecorder,
    me_host: DiffsolMeHost,
}

impl<'a, Eqn, S> DiffsolAdvanceBackend<'a, Eqn, S>
where
    Eqn: OdeEquations<T = f64> + 'a,
    Eqn::V: VectorHost<T = f64>,
    S: OdeSolverMethod<'a, Eqn>,
{
    fn new(inputs: DiffsolAdvanceBackendInputs<'a, S>) -> Self {
        Self {
            solver: inputs.solver,
            model: inputs.model,
            equilibrium_model: inputs.equilibrium_model,
            runtime: inputs.runtime,
            runtime_params: inputs.runtime_params,
            algebraic_warm_start: inputs.algebraic_warm_start,
            opts: inputs.opts,
            active_stop_time: None,
            stage_recorder: inputs.stage_recorder,
            me_host: inputs.me_host,
            pending_component_event: None,
            _eqn: std::marker::PhantomData,
        }
    }

    fn tol(&self) -> f64 {
        self.opts.atol.max(1.0e-10)
    }

    /// Record `stage` as the origin of `error` on the way out. See
    /// [`StageRecorder`].
    fn note<E>(&self, stage: SimFailureStage, error: E) -> E {
        note_stage(&self.stage_recorder, stage, error)
    }

    fn me_to_driver(&self, error: impl Into<SimError>) -> SimDriverError {
        staged_sim_to_driver(&self.stage_recorder, error.into())
    }

    fn take_me_callback_error(&self) -> Option<SimDriverError> {
        self.me_host
            .take_callback_error()
            .map(|error| self.me_to_driver(error))
    }

    fn native_at_time(&mut self, time: f64) -> Result<Vec<f64>, SimDriverError> {
        if sample_time_match_with_tol(self.solver.state().t, time) {
            return Ok(self.solver.state().y.as_slice().to_vec());
        }
        self.solver
            .interpolate(time)
            .map(|values| values.as_slice().to_vec())
            .map_err(|error| {
                self.note(
                    SimFailureStage::TargetIsolation,
                    SimDriverError::Backend(format!("ME event interpolation: {error}")),
                )
            })
    }

    fn retain_component_event(&mut self, event: MePostEventState) {
        self.pending_component_event = Some(event);
    }

    fn commit_state_only_warm_start(&self) -> Result<(), SimDriverError> {
        let warm_start = &self.algebraic_warm_start;
        let state = self.solver.state();
        let mut solver_y = warm_start.speculative();
        let state_len = state.y.len().min(solver_y.len());
        solver_y[..state_len].copy_from_slice(&state.y.as_slice()[..state_len]);
        let mut params = self.runtime_params.borrow().clone();
        self.runtime
            .refresh_delay_values(state.t, &solver_y, &mut params)?;
        self.runtime.full_solver_y_with_guess(
            state.t,
            state.y.as_slice(),
            &params,
            &mut solver_y,
            self.tol(),
            EVENT_UPDATE_MAX_ITERS,
        )?;
        warm_start.commit(solver_y);
        Ok(())
    }

    fn project_accepted_solver_state(&mut self) -> Result<(), SimDriverError> {
        let t = self.solver.state().t;
        let h_cap = self.solver.state().h.abs().max(1.0e-12);
        let native = self.solver.state().y.as_slice().to_vec();
        let mut params = self.runtime_params.borrow().clone();
        let mut solver_y = self.native_to_full_y(&native, t, &params)?;
        self.runtime
            .refresh_delay_values(t, &solver_y, &mut params)?;
        let projection_changed = self
            .runtime
            .project_state_manifold(&mut solver_y, &params, t, self.tol())
            .map_err(|error| self.note(SimFailureStage::ManifoldProjection, error))?;
        if projection_changed {
            self.runtime.refresh_algebraic_and_output_slots(
                t,
                &mut solver_y,
                &params,
                self.tol(),
                EVENT_UPDATE_MAX_ITERS,
            )?;
            let (native_y, native_dy) = self.reset_vectors(&solver_y, &params, t)?;
            self.reset(&native_y, &native_dy, &params, t, h_cap)?;
        } else {
            self.commit_state_only_warm_start()?;
        }
        let driver_state = self.solver.state().y.as_slice().to_vec();
        let frozen_solver_y = self.algebraic_warm_start.speculative();
        let frozen_parameters = self.runtime_params.borrow();
        let component_state = self
            .me_host
            .accept_continuous_step(t, &driver_state, &frozen_solver_y, &frozen_parameters)
            .map_err(|error| self.me_to_driver(error))?;
        if component_state.len() != driver_state.len()
            || component_state.iter().zip(&driver_state).any(|(lhs, rhs)| {
                (lhs - rhs).abs() > self.tol() * 8.0 * (1.0 + lhs.abs().max(rhs.abs()))
            })
        {
            return Err(self.note(
                SimFailureStage::ManifoldProjection,
                SimDriverError::Backend(format!(
                    "ME accepted-state projection diverged from the frozen driver: \
                     component={component_state:?} driver={driver_state:?}"
                )),
            ));
        }
        Ok(())
    }
}

impl<'a, Eqn, S> SolverAdvanceBackend for DiffsolAdvanceBackend<'a, Eqn, S>
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
        // A recovered failure from an earlier step (a `terminate()` carried as an
        // error, for example) must not be mistaken for the origin of a later
        // one, so each step starts from a clean slate.
        self.stage_recorder.set(None);
        let step = solver_call("BDF step", || self.solver.step())
            .map_err(|error| self.note(SimFailureStage::Integration, sim_to_driver(error)));
        if let Some(error) = self.take_me_callback_error() {
            return Err(error);
        }
        let outcome = match step? {
            OdeSolverStopReason::TstopReached => StepOutcome::Stop,
            OdeSolverStopReason::InternalTimestep => StepOutcome::Internal,
            OdeSolverStopReason::RootFound(t_root, root_index) => {
                StepOutcome::Root { t_root, root_index }
            }
        };
        if !matches!(outcome, StepOutcome::Root { .. }) {
            self.project_accepted_solver_state()?;
        }
        Ok(outcome)
    }

    fn set_stop_time(&mut self, stop_time: f64) -> Result<(), SimDriverError> {
        self.active_stop_time = Some(stop_time);
        set_solver_stop_time(&mut self.solver, stop_time)
            .map_err(|error| self.note(SimFailureStage::TargetIsolation, sim_to_driver(error)))
    }

    fn requires_exact_output_stop(&self) -> bool {
        !self
            .model
            .problem
            .continuous
            .manifold_projection_plan
            .is_empty()
    }

    fn interpolate(&mut self, t: f64) -> Result<Vec<f64>, SimDriverError> {
        self.solver
            .interpolate(t)
            .map(|v| v.as_slice().to_vec())
            .map_err(|e| {
                self.note(
                    SimFailureStage::TargetIsolation,
                    SimDriverError::Backend(format!("interpolate: {e}")),
                )
            })
    }

    fn state_mut_back(&mut self, t: f64) -> Result<(), SimDriverError> {
        self.solver.state_mut_back(t).map_err(|e| {
            self.note(
                SimFailureStage::TargetIsolation,
                SimDriverError::Backend(format!("state_mut_back: {e}")),
            )
        })
    }

    fn arm_component_time_event(
        &mut self,
        current_time: f64,
        event_time: f64,
        horizon: f64,
    ) -> Result<(), SimDriverError> {
        let states = self.native_at_time(current_time)?;
        self.me_host
            .arm_time_event(current_time, &states, event_time, horizon)
            .map_err(|error| self.me_to_driver(error))
    }

    fn process_component_time_event(
        &mut self,
        event_time: f64,
        horizon: f64,
    ) -> Result<(), SimDriverError> {
        let states = self.native_at_time(event_time)?;
        let event = self
            .me_host
            .process_time_event(event_time, &states, horizon)
            .map_err(|error| self.me_to_driver(error))?;
        self.retain_component_event(event);
        Ok(())
    }

    fn process_component_state_event(
        &mut self,
        root_time: f64,
        root_index: usize,
        root_states: &[f64],
        horizon: f64,
    ) -> Result<(), SimDriverError> {
        let right_time = runtime_root_event_application_time(root_time, horizon, self.tol());
        let event = self
            .me_host
            .process_state_event(root_time, root_index, root_states, right_time, horizon)
            .map_err(|error| self.me_to_driver(error))?;
        self.retain_component_event(event);
        Ok(())
    }

    fn validate_component_event_error(&mut self, error: SimDriverError) -> SimDriverError {
        let SimDriverError::Terminated { time, message } = &error else {
            let component_termination = self.pending_component_event.as_ref().and_then(|event| {
                event
                    .termination
                    .clone()
                    .map(|termination| (event.entry, termination))
            });
            let Some((entry, termination)) = component_termination else {
                return error;
            };
            self.pending_component_event.take();
            return self.note(
                SimFailureStage::EventIteration,
                SimDriverError::Backend(format!(
                    "ME component terminated after {entry:?} at time={} message={:?}, but the \
                     frozen driver returned {error}",
                    termination.time, termination.message
                )),
            );
        };
        let Some(component) = self.pending_component_event.take() else {
            return self.note(
                SimFailureStage::EventIteration,
                SimDriverError::Backend(format!(
                    "frozen driver terminated at time={time} message={message:?}, but the ME \
                     component retained no event result"
                )),
            );
        };
        let Some(termination) = component.termination else {
            return self.note(
                SimFailureStage::EventIteration,
                SimDriverError::Backend(format!(
                    "frozen driver terminated after {:?} at time={time} message={message:?}, but \
                     the ME component did not terminate",
                    component.entry
                )),
            );
        };
        if termination.time.to_bits() != time.to_bits() || termination.message != *message {
            return self.note(
                SimFailureStage::EventIteration,
                SimDriverError::Backend(format!(
                    "ME termination diverged from the frozen driver after {:?}: component_time={} \
                     component_message={:?} driver_time={time} driver_message={message:?}",
                    component.entry, termination.time, termination.message
                )),
            );
        }
        error
    }

    fn native_to_full_y(
        &self,
        native: &[f64],
        t: f64,
        params: &[f64],
    ) -> Result<Vec<f64>, SimDriverError> {
        let state_count = self.model.state_scalar_count().min(native.len());
        let mut solver_y = self.algebraic_warm_start.speculative();
        self.runtime.full_solver_y_with_guess(
            t,
            &native[..state_count],
            params,
            &mut solver_y,
            self.tol(),
            EVENT_UPDATE_MAX_ITERS,
        )?;
        Ok(solver_y)
    }

    fn reset_vectors(
        &self,
        current_y: &[f64],
        params: &[f64],
        t: f64,
    ) -> Result<(Vec<f64>, Vec<f64>), SimDriverError> {
        let state_count = self.model.state_scalar_count().min(current_y.len());
        let native = current_y[..state_count].to_vec();
        let mut solver_y = current_y.to_vec();
        let dy = self
            .runtime
            .eval_state_derivatives_with_guess(
                t,
                &native,
                params,
                &mut solver_y,
                self.tol(),
                EVENT_UPDATE_MAX_ITERS,
            )
            .map_err(SimDriverError::from)
            .map_err(|error| {
                if self.pending_component_event.is_some() {
                    self.note(SimFailureStage::EventIteration, error)
                } else {
                    self.note(SimFailureStage::ManifoldProjection, error)
                }
            })?;
        Ok((native, dy))
    }

    fn reset(
        &mut self,
        native_y: &[f64],
        native_dy: &[f64],
        params: &[f64],
        t: f64,
        h_cap: f64,
    ) -> Result<(), SimDriverError> {
        let event_reset = self.pending_component_event.is_some();
        let failure_stage = if event_reset {
            SimFailureStage::EventIteration
        } else {
            SimFailureStage::ManifoldProjection
        };
        let me_stage = if event_reset {
            rumoca_solver::fmi_me::MeStage::EventIteration
        } else {
            rumoca_solver::fmi_me::MeStage::ManifoldProjection
        };
        if let Some(component) = self.pending_component_event.take() {
            if let Some(termination) = component.termination {
                return Err(self.note(
                    SimFailureStage::EventIteration,
                    SimDriverError::Backend(format!(
                        "ME component terminated after {:?} at time={} message={:?}, but the \
                         frozen driver continued to its post-event reset at time={t}",
                        component.entry, termination.time, termination.message
                    )),
                ));
            }
            if component.time.to_bits() != t.to_bits()
                || component.states.len() != native_y.len()
                || component
                    .states
                    .iter()
                    .zip(native_y)
                    .any(|(lhs, rhs)| lhs.to_bits() != rhs.to_bits())
            {
                return Err(self.note(
                    SimFailureStage::EventIteration,
                    SimDriverError::Backend(format!(
                        "ME post-event state diverged from the frozen driver after {:?} at \
                         component_t={} driver_t={t}: component={:?} driver={native_y:?}",
                        component.entry, component.time, component.states
                    )),
                ));
            }
        }
        self.me_host
            .sync_continuous_point(t, native_y)
            .map_err(|error| self.note(failure_stage, self.me_to_driver(error)))?;
        let reset = reset_solver_state(
            &mut self.solver,
            &self.runtime_params,
            native_y,
            native_dy,
            params,
            t,
            h_cap,
        )
        .map_err(sim_to_driver)
        .map_err(|error| self.note(failure_stage, error));
        if let Some(error) = self.me_host.take_callback_error() {
            return Err(self.note(failure_stage, sim_to_driver(error.into())));
        }
        reset?;
        let stop_time = self.active_stop_time.unwrap_or(self.opts.t_end);
        if !stop_time_reached_with_tol(t, stop_time) {
            set_solver_stop_time(&mut self.solver, stop_time)
                .map_err(sim_to_driver)
                .map_err(|error| self.note(failure_stage, error))?;
        }
        self.commit_state_only_warm_start()
            .map_err(|error| self.note(failure_stage, error))?;
        let frozen_solver_y = self.algebraic_warm_start.speculative();
        let frozen_parameters = self.runtime_params.borrow();
        self.me_host
            .prepare_integrator_initial_seed(&frozen_solver_y, me_stage)
            .map_err(|error| self.me_to_driver(error))?;
        self.me_host
            .verify_frozen_compatibility_state(&frozen_solver_y, &frozen_parameters, me_stage)
            .map_err(|error| self.me_to_driver(error))
    }

    fn project_algebraics(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
    ) -> Result<bool, RuntimeSolveError> {
        // The driver calls this to re-establish consistency after an event, so
        // an unattributed failure here belongs to the event iteration; only the
        // manifold solve is separated out as its own stage.
        self.runtime
            .refresh_delay_values(t, y, p)
            .map_err(|error| self.note(SimFailureStage::EventIteration, error))?;
        let manifold_changed = self
            .runtime
            .project_state_manifold(y, p, t, tol)
            .map_err(|error| self.note(SimFailureStage::ManifoldProjection, error))?;
        let before = y.to_vec();
        self.runtime
            .refresh_algebraic_and_output_slots(t, y, p, tol, EVENT_UPDATE_MAX_ITERS)
            .map_err(|error| self.note(SimFailureStage::EventIteration, error))?;
        Ok(manifold_changed || runtime_values_changed(&before, y, tol))
    }

    fn derivative_guess(&self, y: &[f64], p: &[f64], t: f64) -> Result<Vec<f64>, SimDriverError> {
        let state_count = self.model.state_scalar_count().min(y.len());
        let state_dy = self.runtime.eval_state_derivatives(
            t,
            &y[..state_count],
            p,
            self.tol(),
            EVENT_UPDATE_MAX_ITERS,
        )?;
        let mut dy = vec![0.0; y.len()];
        dy[..state_dy.len()].copy_from_slice(&state_dy);
        Ok(dy)
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
        self.runtime.refresh_delay_values(t, y, p)?;
        self.runtime
            .refresh_observation_discrete_rows(y, p, t, self.tol(), EVENT_UPDATE_MAX_ITERS)
            .map(|_| ())
            .map_err(SimError::from)
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
    if let Some(runtime) = recorder.runtime {
        return runtime
            .record_visible_sample_if_new(
                recorder.recorded_times,
                recorder.data,
                sample.y,
                sample.params,
                sample.t,
            )
            .map_err(|err| SimError::SolveIr(err.to_string()));
    }
    let values = visible_values(recorder.model, sample.y, sample.params, sample.t)?;
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
    runtime: &SolveRuntime,
    equilibrium_model: &OdeModel,
    tol: f64,
    current: SamplePoint<'_>,
    observations: &[rumoca_solver::InitialEventObservation],
) -> Result<(), SimError> {
    if observations.is_empty() {
        return record_sample_if_new(recorder, current);
    }
    for observation in observations {
        record_prepared_observation_sample(
            recorder,
            runtime,
            equilibrium_model,
            tol,
            SamplePoint {
                y: &observation.y,
                params: &observation.p,
                t: observation.t,
            },
        )?;
    }
    Ok(())
}

fn record_prepared_observation_sample(
    recorder: &mut SampleRecorder<'_>,
    runtime: &SolveRuntime,
    equilibrium_model: &OdeModel,
    tol: f64,
    sample: SamplePoint<'_>,
) -> Result<(), SimError> {
    let mut y = sample.y.to_vec();
    let mut p = sample.params.to_vec();
    refresh_observation_rows_and_relation_memory(
        recorder.model,
        runtime,
        equilibrium_model,
        &mut y,
        &mut p,
        sample.t,
        tol,
    )?;
    record_sample_if_new(
        recorder,
        SamplePoint {
            y: &y,
            params: &p,
            t: sample.t,
        },
    )
}

fn refresh_observation_rows_and_relation_memory(
    model: &solve::SolveModel,
    runtime: &SolveRuntime,
    equilibrium_model: &OdeModel,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    tol: f64,
) -> Result<(), SimError> {
    let state_count = model.state_scalar_count();
    settle_algebraics_and_relation_memory(runtime, equilibrium_model, y, p, t, state_count, tol)?;
    if runtime.refresh_observation_discrete_rows(y, p, t, tol, EVENT_UPDATE_MAX_ITERS)? {
        settle_algebraics_and_relation_memory(
            runtime,
            equilibrium_model,
            y,
            p,
            t,
            state_count,
            tol,
        )?;
    }
    Ok(())
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
