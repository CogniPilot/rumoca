//! BDF numerical integration over the shared FMI 3 ME runtime.
//!
//! This module contains Diffsol operations only. Model lifecycle, event
//! iteration, relation memory, and output evaluation stay behind
//! [`MeRuntimeHost`].

use diffsol::{
    BacktrackingLineSearch, BdfState, DiffsolError, MatrixCommon, NewtonNonlinearSolver,
    OdeSolverMethod, OdeSolverState, OdeSolverStopReason, VectorHost, error::OdeSolverError,
};
use rumoca_solver::{
    SimOptions, SimResult, TimeoutBudget,
    fmi_me::{MeModelSource, MeRuntimeHost, MeRuntimeOutput},
    runtime_root_event_application_time,
    timeline::{sample_time_match_with_tol, try_build_output_times},
};

use crate::{
    LinearSolver, Matrix, Scalar, SimError, SimFailureStage, Vector, instantiate_me_host,
    ode::{StateOdeEquations, build_me_state_ode_problem, trace_bdf_eval_counter_snapshot},
    solver_call,
};

const MAX_STEPS_PER_OUTPUT: usize = 100_000;

pub(crate) fn check_initialization(
    source: MeModelSource<'_>,
    opts: &SimOptions,
) -> Result<(), SimError> {
    let host = instantiate_me_host(source, opts)?;
    let initial = host.initialize_component()?;
    if initial.termination.is_some() {
        return Ok(());
    }
    let ode_build =
        build_me_state_ode_problem(opts, host.clone(), initial.time, initial.states.clone())?;
    let derivative = host.derivatives(initial.time, &initial.states)?;
    initial_bdf_state(&ode_build.problem, &initial.states, &derivative).map(|_| ())
}

pub(crate) fn simulate(
    source: MeModelSource<'_>,
    opts: &SimOptions,
) -> Result<SimResult, SimError> {
    let host = instantiate_me_host(source, opts)?;
    let initial = host.initialize_component()?;
    if let Some(termination) = initial.termination {
        return Ok(empty_terminated_result(&host, termination));
    }
    let dt = opts.dt.unwrap_or((opts.t_end - opts.t_start).abs() / 500.0);
    let output_times = try_build_output_times(opts.t_start, opts.t_end, dt)
        .map_err(|error| SimError::SolverError(error.to_string()))?;
    let mut trace = TraceRecorder::new(&host, output_times.len());
    let budget = TimeoutBudget::new(opts.max_wall_seconds);
    let mut pending_root = None;
    let mut pending_time_event = None;
    for observation in &initial.observations {
        trace.record(host.outputs_for_observation(observation)?)?;
    }

    let ode_build =
        build_me_state_ode_problem(opts, host.clone(), initial.time, initial.states.clone())?;
    let counters = ode_build.eval_counters.clone();
    let problem = ode_build.problem;
    let derivative = host.derivatives(initial.time, &initial.states)?;
    let state = initial_bdf_state(&problem, &initial.states, &derivative)?;
    let nonlinear =
        NewtonNonlinearSolver::new(LinearSolver::default(), BacktrackingLineSearch::default());
    let mut solver = solver_call("BDF new", || {
        diffsol::Bdf::<_, _, _, diffsol::NoAug<_>>::new(&problem, state, nonlinear)
    })?;

    for target in output_times {
        budget
            .check()
            .map_err(|timeout| SimError::Timeout {
                seconds: timeout.seconds,
            })
            .map_err(|error| error.at_stage(SimFailureStage::Integration))?;
        let advance = advance_to(
            &host,
            &mut solver,
            &mut trace,
            &mut pending_root,
            &mut pending_time_event,
            target,
            opts,
            &budget,
        );
        match advance {
            Ok(()) => {}
            Err(SimError::Terminated { time, message }) => {
                return Ok(trace.finish(
                    host.state_count(),
                    Some(rumoca_solver::SimTermination { time, message }),
                ));
            }
            Err(error) => return Err(error),
        }
        let states = if sample_time_match_with_tol(solver.state().t, target) {
            solver.state().y.as_slice().to_vec()
        } else {
            solver_call("BDF output interpolation", || solver.interpolate(target))?
                .as_slice()
                .to_vec()
        };
        trace.record(host.observe_continuous_point(target, &states)?)?;
    }
    trace_bdf_eval_counter_snapshot("me-bdf", &counters);
    Ok(trace.finish(host.state_count(), None))
}

fn advance_to<'a, Eqn, S>(
    host: &MeRuntimeHost,
    solver: &mut S,
    trace: &mut TraceRecorder,
    pending_root: &mut Option<PendingRoot>,
    pending_time_event: &mut Option<f64>,
    target: f64,
    opts: &SimOptions,
    budget: &TimeoutBudget,
) -> Result<(), SimError>
where
    Eqn: StateOdeEquations + 'a,
    S: OdeSolverMethod<'a, Eqn>,
{
    let tolerance = opts.atol.max(1.0e-10);
    for _ in 0..MAX_STEPS_PER_OUTPUT {
        budget
            .check()
            .map_err(|timeout| SimError::Timeout {
                seconds: timeout.seconds,
            })
            .map_err(|error| error.at_stage(SimFailureStage::Integration))?;
        if let Some(event_time) = *pending_time_event {
            if event_time > target && !time_reached_with_tolerance(event_time, target, tolerance) {
                return Ok(());
            }
            let states = solver.state().y.as_slice().to_vec();
            let event = host.process_time_event(event_time, &states, target)?;
            *pending_time_event = None;
            if let Some(termination) = event.termination {
                return Err(SimError::Terminated {
                    time: termination.time,
                    message: termination.message,
                });
            }
            if let Some(observation) = event.observation {
                trace.record(observation)?;
            }
            reset_after_event(host, solver, event.time, &event.states)?;
            continue;
        }
        if let Some(root) = pending_root.take() {
            if root.time > target && !time_reached_with_tolerance(root.time, target, tolerance) {
                *pending_root = Some(root);
                return Ok(());
            }
            process_root_event(host, solver, trace, root, target, tolerance)?;
            continue;
        }
        let requested = host.next_event_stop(opts.t_end)?;
        let current = solver.state().t;
        if requested.is_event && time_reached_with_tolerance(current, requested.time, tolerance) {
            if requested.time > target
                && !time_reached_with_tolerance(requested.time, target, tolerance)
            {
                return Ok(());
            }
            let states = solver.state().y.as_slice().to_vec();
            let event = host.process_time_event(requested.time, &states, target)?;
            if let Some(termination) = event.termination {
                return Err(SimError::Terminated {
                    time: termination.time,
                    message: termination.message,
                });
            }
            if let Some(observation) = event.observation {
                trace.record(observation)?;
            }
            reset_after_event(host, solver, event.time, &event.states)?;
            continue;
        }
        if current >= target || time_reached_with_tolerance(current, target, tolerance) {
            return Ok(());
        }
        let stop_time = requested.time.min(opts.t_end);
        set_stop_time(solver, stop_time)?;
        match solver_call("BDF step", || solver.step())? {
            OdeSolverStopReason::InternalTimestep => accept_step(host, solver, tolerance)?,
            OdeSolverStopReason::TstopReached => {
                accept_step(host, solver, tolerance)?;
                if requested.is_event {
                    *pending_time_event = Some(requested.time);
                    continue;
                }
                return Ok(());
            }
            OdeSolverStopReason::RootFound(root_time, root_index) => {
                let root_states =
                    solver_call("BDF root interpolation", || solver.interpolate(root_time))?
                        .as_slice()
                        .to_vec();
                let root = PendingRoot {
                    time: root_time,
                    index: root_index,
                    states: root_states,
                };
                if root_time > target && !time_reached_with_tolerance(root_time, target, tolerance)
                {
                    *pending_root = Some(root);
                    return Ok(());
                }
                process_root_event(host, solver, trace, root, target, tolerance)?;
            }
        }
    }
    Err(SimError::SolverError(format!(
        "BDF exceeded {MAX_STEPS_PER_OUTPUT} ME steps before output t={target}"
    )))
}

struct PendingRoot {
    time: f64,
    index: usize,
    states: Vec<f64>,
}

fn process_root_event<'a, Eqn, S>(
    host: &MeRuntimeHost,
    solver: &mut S,
    trace: &mut TraceRecorder,
    root: PendingRoot,
    target: f64,
    tolerance: f64,
) -> Result<(), SimError>
where
    Eqn: StateOdeEquations + 'a,
    S: OdeSolverMethod<'a, Eqn>,
{
    let candidate = runtime_root_event_application_time(root.time, target, tolerance);
    let intervening = host.next_event_stop(candidate)?;
    let right_time =
        if intervening.is_event && intervening.time > root.time && intervening.time < candidate {
            intervening.time
        } else {
            candidate
        };
    let event =
        host.process_state_event(root.time, root.index, &root.states, right_time, target)?;
    if let Some(termination) = event.termination {
        return Err(SimError::Terminated {
            time: termination.time,
            message: termination.message,
        });
    }
    if let Some(observation) = event.observation {
        trace.record(observation)?;
    }
    reset_after_event(host, solver, event.time, &event.states)
}

fn time_reached_with_tolerance(current: f64, target: f64, tolerance: f64) -> bool {
    (current - target).abs() <= tolerance * (1.0 + current.abs().max(target.abs()))
}

fn accept_step<'a, Eqn, S>(
    host: &MeRuntimeHost,
    solver: &mut S,
    tolerance: f64,
) -> Result<(), SimError>
where
    Eqn: StateOdeEquations + 'a,
    S: OdeSolverMethod<'a, Eqn>,
{
    let time = solver.state().t;
    let states = solver.state().y.as_slice().to_vec();
    let projected = host.accept_integrator_step(time, &states)?;
    if projected.iter().zip(&states).any(|(left, right)| {
        (left - right).abs() > tolerance * 8.0 * (1.0 + left.abs().max(right.abs()))
    }) {
        reset_after_event(host, solver, time, &projected)?;
    }
    Ok(())
}

fn reset_after_event<'a, Eqn, S>(
    host: &MeRuntimeHost,
    solver: &mut S,
    time: f64,
    states: &[f64],
) -> Result<(), SimError>
where
    Eqn: StateOdeEquations + 'a,
    S: OdeSolverMethod<'a, Eqn>,
{
    let derivatives = host.derivatives(time, states)?;
    let h_cap = solver.state().h.abs().max(1.0e-12);
    let problem = solver.problem();
    let mut fresh = S::State::new_without_initialise(problem)
        .map_err(|error| SimError::SolverError(format!("BDF ME reset: {error}")))?;
    {
        let state = fresh.as_mut();
        state.y.as_mut_slice().copy_from_slice(states);
        state.dy.as_mut_slice().copy_from_slice(&derivatives);
        *state.t = time;
    }
    fresh.set_step_size(problem.h0, &problem.atol, problem.rtol, &problem.eqn, 1);
    solver.set_state(fresh);
    clear_stop_time_after_reset(solver, time)?;
    let state = solver.state_mut();
    if *state.h > h_cap {
        *state.h = h_cap;
    }
    Ok(())
}

fn clear_stop_time_after_reset<'a, Eqn, S>(solver: &mut S, time: f64) -> Result<(), SimError>
where
    Eqn: StateOdeEquations + 'a,
    S: OdeSolverMethod<'a, Eqn>,
{
    // Diffsol clears the stored deadline while returning this typed error when
    // the replacement deadline equals the newly reset state time.
    match solver.set_stop_time(time) {
        Ok(()) | Err(DiffsolError::OdeSolverError(OdeSolverError::StopTimeAtCurrentTime)) => Ok(()),
        Err(error) => Err(SimError::SolverError(format!(
            "clear BDF ME stop time after reset: {error}"
        ))),
    }
}

fn initial_bdf_state<Eqn>(
    problem: &diffsol::OdeSolverProblem<Eqn>,
    states: &[f64],
    derivatives: &[f64],
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
        .map_err(|error| SimError::SolverError(format!("BDF ME state init: {error}")))?;
    {
        let state_ref = state.as_mut();
        state_ref.y.as_mut_slice().copy_from_slice(states);
        state_ref.dy.as_mut_slice().copy_from_slice(derivatives);
        *state_ref.t = problem.t0;
    }
    state.set_step_size(problem.h0, &problem.atol, problem.rtol, &problem.eqn, 1);
    Ok(state)
}

fn set_stop_time<'a, Eqn, S>(solver: &mut S, stop_time: f64) -> Result<(), SimError>
where
    Eqn: StateOdeEquations + 'a,
    S: OdeSolverMethod<'a, Eqn>,
{
    solver
        .set_stop_time(stop_time)
        .map_err(|error| SimError::SolverError(format!("BDF ME stop time: {error}")))
}

struct TraceRecorder {
    names: Vec<String>,
    meta: Vec<rumoca_solver::SimVariableMeta>,
    times: Vec<f64>,
    data: Vec<Vec<f64>>,
}

impl TraceRecorder {
    fn new(host: &MeRuntimeHost, capacity: usize) -> Self {
        let names = host.output_names();
        Self {
            data: (0..names.len())
                .map(|_| Vec::with_capacity(capacity))
                .collect(),
            names,
            meta: host.output_meta(),
            times: Vec::with_capacity(capacity),
        }
    }

    fn record(&mut self, observation: MeRuntimeOutput) -> Result<(), SimError> {
        if observation.values.len() != self.data.len() {
            return Err(SimError::RuntimeContract {
                reason: format!(
                    "ME output width {} does not match trace width {}",
                    observation.values.len(),
                    self.data.len()
                ),
            });
        }
        if self
            .times
            .last()
            .is_some_and(|last| sample_time_match_with_tol(*last, observation.time))
        {
            if let Some(time) = self.times.last_mut() {
                *time = observation.time;
            }
            for (column, value) in self.data.iter_mut().zip(observation.values) {
                if let Some(slot) = column.last_mut() {
                    *slot = value;
                }
            }
            return Ok(());
        }
        self.times.push(observation.time);
        for (column, value) in self.data.iter_mut().zip(observation.values) {
            column.push(value);
        }
        Ok(())
    }

    fn finish(
        self,
        state_count: usize,
        termination: Option<rumoca_solver::SimTermination>,
    ) -> SimResult {
        SimResult {
            times: self.times,
            names: self.names,
            data: self.data,
            n_states: state_count,
            variable_meta: self.meta,
            termination,
        }
    }
}

fn empty_terminated_result(
    host: &MeRuntimeHost,
    termination: rumoca_solver::SimTermination,
) -> SimResult {
    SimResult {
        times: Vec::new(),
        names: host.output_names(),
        data: vec![Vec::new(); host.output_names().len()],
        n_states: host.state_count(),
        variable_meta: host.output_meta(),
        termination: Some(termination),
    }
}
