// SPEC_0021 file-size exception: RK45 currently combines stepping, event
// boundary handling, and output sampling. split plan: move event handling and
// dense-output/sample scheduling into focused solver modules.

use std::{cell::RefCell, time::Instant};

use indexmap::IndexMap;
use rumoca_eval_solve::{
    EventUpdateRowFilter, InitialEventObservation, ProjectedEventUpdateInput,
    ProjectedInitialEventInput, SolveRuntime,
};
use rumoca_ir_solve as solve;
use rumoca_solver::{
    BackendState, EventActionOutcome, EventPreMode, RootCrossing, RuntimeEventBoundary,
    RuntimeEventBoundaryHandler, RuntimeEventStop, RuntimeSolveError, SimOptions, SimResult,
    SimSolverMode, SimTermination, SimulationBackend, SolveStopSchedule, StepUntilOutcome,
    TimeoutBudget, TimeoutExceeded, commit_pre_params_after_event, convert_variable_meta,
    process_runtime_event_boundary, root_crossings_with_relation_memory, root_value_crossed,
    runtime_event_horizon, timeline,
};

mod reset;
mod trace;

use reset::Rk45ResetSnapshot;
use trace::{
    record_derivative_eval_trace, record_root_eval_trace, reset_rk_eval_trace,
    rk_eval_trace_enabled, trace_rk_eval_snapshot,
};

const MIN_STEP: f64 = 1.0e-12;
const ROOT_BISECTION_ITERS: usize = 48;
const UPDATE_MAX_ITERS: usize = 32;
const ALGEBRAIC_REFRESH_TOL: f64 = 1.0e-10;

#[derive(Debug, thiserror::Error)]
pub enum SimError {
    #[error("empty system: no state equations to simulate")]
    EmptySystem,

    #[error("rk45 backend does not support solver mode {requested:?}")]
    UnsupportedSolverMode { requested: SimSolverMode },

    #[error("rk45 backend only supports a narrow explicit ODE subset: {reason}")]
    UnsupportedModel { reason: String },

    #[error("non-finite derivative evaluation for state '{state_name}'")]
    NonFiniteDerivative { state_name: String },

    #[error("step size underflow while advancing toward t={target_t}")]
    StepSizeUnderflow { target_t: f64 },

    #[error("solve-IR evaluation failed: {0}")]
    SolveIr(String),

    #[error("rk45 runtime contract violation: {reason}")]
    RuntimeContract { reason: String },

    #[error("{context} allocation failed for {entries} entries")]
    Allocation {
        context: &'static str,
        entries: usize,
    },

    #[error("Modelica assert failed at t={time:.9}: {message}")]
    AssertionFailed { time: f64, message: String },

    #[error("timeout after {seconds:.3}s")]
    Timeout { seconds: f64 },
}

impl From<TimeoutExceeded> for SimError {
    fn from(value: TimeoutExceeded) -> Self {
        Self::Timeout {
            seconds: value.seconds,
        }
    }
}

impl From<rumoca_eval_solve::EvalSolveError> for SimError {
    fn from(value: rumoca_eval_solve::EvalSolveError) -> Self {
        Self::SolveIr(value.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct SessionState {
    pub time: f64,
    pub values: IndexMap<String, f64>,
}

pub struct SimulationSession {
    runtime: &'static SolveRuntime,
    backend: Rk45Backend<'static>,
    reset_snapshot: Rk45ResetSnapshot,
    input_values: IndexMap<String, f64>,
}

impl SimulationSession {
    pub fn new(model: &solve::SolveModel, opts: SimOptions) -> Result<Self, SimError> {
        match opts.solver_mode {
            SimSolverMode::Auto | SimSolverMode::RkLike => {}
            requested => return Err(SimError::UnsupportedSolverMode { requested }),
        }
        validate_explicit_solve_model(model)?;
        let runtime = Box::leak(Box::new(SolveRuntime::new(model)?));
        let mut backend = Rk45Backend::new(runtime, &opts)?;
        backend.init()?;
        let reset_snapshot = backend.reset_snapshot();
        Ok(Self {
            runtime,
            backend,
            reset_snapshot,
            input_values: IndexMap::new(),
        })
    }

    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        let Some(param_idx) = self
            .runtime
            .model
            .problem
            .solve_layout
            .input_parameter_index(name)
        else {
            return Err(SimError::SolveIr(format!("unknown input '{name}'")));
        };
        self.input_values.insert(name.to_string(), value);
        if let Some(slot) = self.backend.params.get_mut(param_idx) {
            *slot = value;
        }
        self.backend.clear_runtime_caches();
        Ok(())
    }

    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        for (name, value) in inputs {
            self.set_input(name, *value)?;
        }
        Ok(())
    }

    pub fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        let target_time = target_time.min(self.backend.t_end);
        if target_time <= self.backend.time {
            return Ok(());
        }
        advance_backend_to(&mut self.backend, target_time)
    }

    pub fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.input_values.clear();
        self.backend
            .reset_to_snapshot(&self.reset_snapshot, t_start);
        Ok(())
    }

    pub fn time(&self) -> f64 {
        self.backend.time
    }

    pub fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        if let Some(value) = self.input_values.get(name).copied() {
            return Ok(Some(value));
        }
        let solver_y = self.backend.current_solver_y()?;
        let Some(idx) = self
            .runtime
            .model
            .visible_names
            .iter()
            .position(|visible| visible == name)
        else {
            return Ok(None);
        };
        let values =
            self.runtime
                .visible_values(&solver_y, &self.backend.params, self.backend.time)?;
        values.get(idx).copied().map(Some).ok_or_else(|| {
            SimError::RuntimeContract {
                reason: format!(
                    "visible value '{name}' resolved to index {idx}, but runtime returned {} values",
                    values.len()
                ),
            }
        })
    }

    pub fn state(&self) -> Result<SessionState, SimError> {
        Ok(SessionState {
            time: self.time(),
            values: self.session_visible_values()?,
        })
    }

    pub fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        let visible_values = self.session_visible_values()?;
        let mut values = IndexMap::with_capacity(names.len());
        for name in names {
            if let Some(value) = visible_values.get(name).copied() {
                values.insert(name.clone(), value);
            }
        }
        Ok(values)
    }

    pub fn input_names(&self) -> &[String] {
        self.runtime.model.problem.solve_layout.input_scalar_names()
    }

    pub fn variable_names(&self) -> &[String] {
        &self.runtime.model.visible_names
    }

    fn session_visible_values(&self) -> Result<IndexMap<String, f64>, SimError> {
        let solver_y = self.backend.current_solver_y()?;
        let visible_values =
            self.runtime
                .visible_values(&solver_y, &self.backend.params, self.backend.time)?;
        let mut values = collect_visible_values(&self.runtime.model.visible_names, visible_values)?;
        values.extend(
            self.input_values
                .iter()
                .map(|(name, value)| (name.clone(), *value)),
        );
        Ok(values)
    }
}

fn record_rk_initial_samples(
    model: &SolveRuntime,
    backend: &Rk45Backend<'_>,
    data: &mut [Vec<f64>],
    times: &mut Vec<f64>,
    t_start: f64,
) -> Result<(), SimError> {
    if backend.initial_observations.is_empty() {
        let solver_y = backend.current_solver_y()?;
        model.record_visible_sample(data, &solver_y, &backend.params, t_start)?;
        times.push(t_start);
        return Ok(());
    }
    for observation in &backend.initial_observations {
        model.record_visible_sample(data, &observation.y, &observation.p, observation.t)?;
        times.push(observation.t);
    }
    Ok(())
}

fn collect_visible_values(
    names: &[String],
    values: Vec<f64>,
) -> Result<IndexMap<String, f64>, SimError> {
    if names.len() != values.len() {
        return Err(SimError::RuntimeContract {
            reason: format!(
                "runtime returned {} visible values for {} visible names",
                values.len(),
                names.len()
            ),
        });
    }
    Ok(names.iter().cloned().zip(values).collect())
}

impl From<RuntimeSolveError> for SimError {
    fn from(value: RuntimeSolveError) -> Self {
        match value {
            RuntimeSolveError::SolveIr { message, span } => {
                let message = match span {
                    Some(span) => format!("{message} @ {span:?}"),
                    None => message,
                };
                Self::SolveIr(message)
            }
            RuntimeSolveError::UnsupportedModel { reason } => Self::UnsupportedModel { reason },
            RuntimeSolveError::NonFiniteDerivative { state_name } => {
                Self::NonFiniteDerivative { state_name }
            }
            non_finite @ RuntimeSolveError::NonFiniteValue { .. } => {
                Self::SolveIr(non_finite.to_string())
            }
        }
    }
}

struct Rk45Backend<'a> {
    model: &'a SolveRuntime,
    time: f64,
    state: Vec<f64>,
    params: Vec<f64>,
    atol: f64,
    rtol: f64,
    next_step: f64,
    t_end: f64,
    budget: TimeoutBudget,
    stop_schedule: SolveStopSchedule,
    termination: Option<SimTermination>,
    pending_root_crossings: Vec<RootCrossing>,
    pending_event_pre_y: Option<Vec<f64>>,
    pending_event_pre_p: Option<Vec<f64>>,
    boundary_event_pre_y: Option<Vec<f64>>,
    boundary_event_pre_p: Option<Vec<f64>>,
    post_event_eval_time: Option<f64>,
    solver_y_guess: RefCell<Vec<f64>>,
    derivative_cache: RefCell<Option<CachedDerivative>>,
    root_cache: RefCell<Option<CachedRootConditions>>,
    initial_observations: Vec<InitialEventObservation>,
}

struct TrialStep {
    y_next: Vec<f64>,
    next_derivative: Option<Vec<f64>>,
    error_norm: f64,
}

struct StepAcceptanceContext<'a> {
    old_roots: &'a [f64],
    target_t: f64,
    event_boundary: Option<f64>,
}

struct CachedDerivative {
    time: f64,
    state: Vec<f64>,
    derivative: Vec<f64>,
}

struct CachedRootConditions {
    time: f64,
    state: Vec<f64>,
    values: Vec<f64>,
}

struct EventStop {
    time: f64,
    event: Option<RuntimeEventStop>,
}

struct LocatedRoot {
    time: f64,
    state: Vec<f64>,
    pre_state: Vec<f64>,
}

pub fn simulate(model: &solve::SolveModel, opts: &SimOptions) -> Result<SimResult, SimError> {
    reset_rk_eval_trace();
    rumoca_eval_solve::reset_solve_row_eval_trace();
    match opts.solver_mode {
        SimSolverMode::Auto | SimSolverMode::RkLike => {}
        requested => return Err(SimError::UnsupportedSolverMode { requested }),
    }

    validate_explicit_solve_model(model)?;
    let model = SolveRuntime::new(model)?;
    let sample_dt = default_output_dt(opts);
    let sample_times = timeline::build_output_times(opts.t_start, opts.t_end, sample_dt);
    let mut times = checked_vec_with_capacity(sample_times.len(), "RK45 output times")?;
    let mut backend = Rk45Backend::new(&model, opts)?;
    backend.init()?;
    let mut data =
        checked_vec_with_capacity(model.model.visible_names.len(), "RK45 output series")?;
    for _ in &model.model.visible_names {
        data.push(checked_vec_with_capacity(
            sample_times.len(),
            "RK45 output samples",
        )?);
    }
    record_rk_initial_samples(&model, &backend, &mut data, &mut times, opts.t_start)?;

    for &target_t in sample_times.iter().skip(1) {
        advance_backend_to(&mut backend, target_t)?;
        let solver_y = backend.current_solver_y()?;
        let sample_t = backend.time;
        if !times
            .last()
            .copied()
            .is_some_and(|last_t| time_match_with_tol(last_t, sample_t))
        {
            model.record_visible_sample(&mut data, &solver_y, &backend.params, sample_t)?;
            times.push(sample_t);
        }
        if backend.termination.is_some() {
            break;
        }
    }

    trace_rk_eval_snapshot("rk-like");
    rumoca_eval_solve::trace_solve_row_eval_snapshot("rk-like");
    Ok(SimResult {
        times,
        names: model.model.visible_names.clone(),
        data,
        n_states: model.state_count,
        variable_meta: convert_variable_meta(&model.model.variable_meta),
        termination: backend.termination,
    })
}

fn validate_explicit_solve_model(model: &solve::SolveModel) -> Result<(), SimError> {
    let layout = &model.problem.solve_layout;
    if layout.state_scalar_count == 0 {
        return Err(SimError::EmptySystem);
    }
    if model.initial_y.len() != model.solver_scalar_count() {
        return Err(SimError::SolveIr(format!(
            "initial vector length {} does not match solver layout {}",
            model.initial_y.len(),
            model.solver_scalar_count()
        )));
    }
    let implicit_rhs_len = model
        .problem
        .continuous
        .implicit_rhs
        .len()
        .map_err(|err| SimError::SolveIr(err.to_string()))?;
    if implicit_rhs_len < layout.state_scalar_count {
        return Err(SimError::SolveIr(format!(
            "implicit RHS has {} rows for {} states",
            implicit_rhs_len, layout.state_scalar_count
        )));
    }
    Ok(())
}

fn checked_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
) -> Result<Vec<T>, SimError> {
    let mut values = Vec::new();
    values
        .try_reserve(capacity)
        .map_err(|_| SimError::Allocation {
            context,
            entries: capacity,
        })?;
    Ok(values)
}

fn runtime_contract_violation(reason: impl Into<String>) -> SimError {
    SimError::RuntimeContract {
        reason: reason.into(),
    }
}

fn ensure_len(actual: usize, expected: usize, label: &str) -> Result<(), SimError> {
    if actual != expected {
        return Err(runtime_contract_violation(format!(
            "{label} {actual} does not match expected length {expected}"
        )));
    }
    Ok(())
}

fn root_value_at(values: &[f64], index: usize, label: &str) -> Result<f64, SimError> {
    values.get(index).copied().ok_or_else(|| {
        runtime_contract_violation(format!(
            "{label} root index {index} is outside {} root condition values",
            values.len()
        ))
    })
}

impl<'a> Rk45Backend<'a> {
    fn new(model: &'a SolveRuntime, opts: &SimOptions) -> Result<Self, SimError> {
        let state = model.model.initial_y[..model.state_count].to_vec();
        let next_step = default_step_size(opts);
        if !next_step.is_finite() || next_step <= 0.0 {
            return Err(SimError::StepSizeUnderflow {
                target_t: opts.t_end,
            });
        }
        Ok(Self {
            model,
            time: opts.t_start,
            state,
            params: model.model.parameters.clone(),
            atol: opts.atol.max(1.0e-12),
            rtol: opts.rtol.max(1.0e-12),
            next_step,
            t_end: opts.t_end,
            budget: TimeoutBudget::new(opts.max_wall_seconds),
            stop_schedule: SolveStopSchedule::new(&model.model.problem, opts.t_start, opts.t_end),
            termination: None,
            pending_root_crossings: Vec::new(),
            pending_event_pre_y: None,
            pending_event_pre_p: None,
            boundary_event_pre_y: None,
            boundary_event_pre_p: None,
            post_event_eval_time: None,
            solver_y_guess: RefCell::new(Vec::new()),
            derivative_cache: RefCell::new(None),
            root_cache: RefCell::new(None),
            initial_observations: Vec::new(),
        })
    }

    fn current_solver_y(&self) -> Result<Vec<f64>, SimError> {
        let mut guess = self.solver_y_guess.borrow_mut();
        self.model
            .full_solver_y_with_guess(
                self.public_time_eval_time(self.time),
                &self.state,
                &self.params,
                &mut guess,
                ALGEBRAIC_REFRESH_TOL,
                UPDATE_MAX_ITERS,
            )
            .map(|()| guess.clone())
            .map_err(Into::into)
    }

    fn copy_state_from_solver_y(&mut self, solver_y: &[f64]) {
        for (dst, src) in self.state.iter_mut().zip(solver_y.iter().copied()) {
            *dst = src;
        }
    }

    fn trial_step(&self, h: f64, event_boundary: Option<f64>) -> Result<TrialStep, SimError> {
        self.trial_step_from(self.time, &self.state, h, event_boundary)
    }

    fn derivatives_at(&self, time: f64, state: &[f64]) -> Result<Vec<f64>, SimError> {
        if let Some(derivative) = self.cached_derivative(time, state) {
            return Ok(derivative);
        }
        let start = rk_eval_trace_enabled().then(Instant::now);
        let mut guess = self.solver_y_guess.borrow_mut();
        let result = self
            .model
            .eval_state_derivatives_with_guess(
                time,
                state,
                &self.params,
                &mut guess,
                ALGEBRAIC_REFRESH_TOL,
                UPDATE_MAX_ITERS,
            )
            .map_err(Into::into);
        if let Some(start) = start {
            record_derivative_eval_trace(start);
        }
        result
    }

    fn cached_derivative(&self, time: f64, state: &[f64]) -> Option<Vec<f64>> {
        let cache = self.derivative_cache.borrow();
        let cached = cache.as_ref()?;
        if !time_match_with_tol(cached.time, time) || !state_values_match(&cached.state, state) {
            return None;
        }
        Some(cached.derivative.clone())
    }

    fn cache_derivative(&self, time: f64, state: &[f64], derivative: Vec<f64>) {
        *self.derivative_cache.borrow_mut() = Some(CachedDerivative {
            time,
            state: state.to_vec(),
            derivative,
        });
    }

    fn clear_derivative_cache(&self) {
        *self.derivative_cache.borrow_mut() = None;
    }

    fn clear_runtime_caches(&self) {
        self.clear_derivative_cache();
        *self.root_cache.borrow_mut() = None;
    }

    fn trial_step_from(
        &self,
        time: f64,
        state: &[f64],
        h: f64,
        event_boundary: Option<f64>,
    ) -> Result<TrialStep, SimError> {
        let k1 = self.derivatives_at(self.continuous_eval_time(time, event_boundary), state)?;
        let y2 = combine_stage(state, h, &[(&k1, 1.0 / 5.0)])?;
        let k2 = self.derivatives_at(
            self.continuous_eval_time(time + h * (1.0 / 5.0), event_boundary),
            &y2,
        )?;

        let y3 = combine_stage(state, h, &[(&k1, 3.0 / 40.0), (&k2, 9.0 / 40.0)])?;
        let k3 = self.derivatives_at(
            self.continuous_eval_time(time + h * (3.0 / 10.0), event_boundary),
            &y3,
        )?;

        let y4 = combine_stage(
            state,
            h,
            &[(&k1, 44.0 / 45.0), (&k2, -56.0 / 15.0), (&k3, 32.0 / 9.0)],
        )?;
        let k4 = self.derivatives_at(
            self.continuous_eval_time(time + h * (4.0 / 5.0), event_boundary),
            &y4,
        )?;

        let y5 = combine_stage(
            state,
            h,
            &[
                (&k1, 19372.0 / 6561.0),
                (&k2, -25360.0 / 2187.0),
                (&k3, 64448.0 / 6561.0),
                (&k4, -212.0 / 729.0),
            ],
        )?;
        let k5 = self.derivatives_at(
            self.continuous_eval_time(time + h * (8.0 / 9.0), event_boundary),
            &y5,
        )?;

        let y6 = combine_stage(
            state,
            h,
            &[
                (&k1, 9017.0 / 3168.0),
                (&k2, -355.0 / 33.0),
                (&k3, 46732.0 / 5247.0),
                (&k4, 49.0 / 176.0),
                (&k5, -5103.0 / 18656.0),
            ],
        )?;
        let k6 = self.derivatives_at(self.continuous_eval_time(time + h, event_boundary), &y6)?;

        let y5th = combine_stage(
            state,
            h,
            &[
                (&k1, 35.0 / 384.0),
                (&k3, 500.0 / 1113.0),
                (&k4, 125.0 / 192.0),
                (&k5, -2187.0 / 6784.0),
                (&k6, 11.0 / 84.0),
            ],
        )?;

        let y7 = y5th.clone();
        let k7 = self.derivatives_at(self.continuous_eval_time(time + h, event_boundary), &y7)?;
        let y4th = combine_stage(
            state,
            h,
            &[
                (&k1, 5179.0 / 57600.0),
                (&k3, 7571.0 / 16695.0),
                (&k4, 393.0 / 640.0),
                (&k5, -92097.0 / 339200.0),
                (&k6, 187.0 / 2100.0),
                (&k7, 1.0 / 40.0),
            ],
        )?;

        let error_norm = error_norm(state, &y5th, &y4th, self.atol, self.rtol)?;
        Ok(TrialStep {
            y_next: y5th,
            next_derivative: Some(k7),
            error_norm,
        })
    }

    fn advance_to(&mut self, target_t: f64) -> Result<StepUntilOutcome, SimError> {
        if self.termination.is_some() {
            return Ok(StepUntilOutcome::Finished);
        }
        if target_t <= self.time {
            return Ok(StepUntilOutcome::StopReached);
        }
        while self.time < target_t {
            let stop = self.next_event_stop(target_t)?;
            let event_boundary = stop.event.is_some().then_some(stop.time);
            let outcome = self.advance_continuous_to(stop.time, event_boundary)?;
            match self.process_event_boundary(outcome, &stop, target_t)? {
                Some(StepUntilOutcome::Finished) => return Ok(StepUntilOutcome::Finished),
                Some(_) => continue,
                None => {}
            }
        }
        Ok(StepUntilOutcome::StopReached)
    }

    fn process_event_boundary(
        &mut self,
        outcome: StepUntilOutcome,
        stop: &EventStop,
        target_t: f64,
    ) -> Result<Option<StepUntilOutcome>, SimError> {
        if matches!(outcome, StepUntilOutcome::RootFound { .. }) {
            return self.apply_events_and_continue_or_finish(self.time, target_t);
        }
        if let Some(event) = stop.event
            && time_match_with_tol(self.time, stop.time)
        {
            return self.apply_scheduled_events_and_continue_or_finish(stop.time, target_t, event);
        }
        Ok(None)
    }

    fn advance_continuous_to(
        &mut self,
        target_t: f64,
        event_boundary: Option<f64>,
    ) -> Result<StepUntilOutcome, SimError> {
        while self.time < target_t {
            self.budget.check()?;
            let old_t = self.time;
            let old_state = self.state.clone();
            let old_roots = self.eval_root_conditions(
                self.continuous_eval_time(old_t, event_boundary),
                &old_state,
            )?;
            let remaining = target_t - self.time;
            let h = self.next_step.min(remaining).max(MIN_STEP);
            let trial = self.trial_step(h, event_boundary)?;
            let step_context = StepAcceptanceContext {
                old_roots: &old_roots,
                target_t,
                event_boundary,
            };
            if let Some(outcome) =
                self.accept_trial_step(old_t, old_state, h, &trial, step_context)?
            {
                return Ok(outcome);
            }
            self.next_step = adapt_step(h, trial.error_norm);
            if self.next_step < MIN_STEP && self.time + MIN_STEP < target_t {
                return Err(SimError::StepSizeUnderflow { target_t });
            }
        }
        Ok(StepUntilOutcome::StopReached)
    }

    fn accept_trial_step(
        &mut self,
        old_t: f64,
        old_state: Vec<f64>,
        h: f64,
        trial: &TrialStep,
        context: StepAcceptanceContext<'_>,
    ) -> Result<Option<StepUntilOutcome>, SimError> {
        if trial.error_norm > 1.0 {
            return Ok(None);
        }
        let new_t = (self.time + h).min(context.target_t);
        let new_roots = self.eval_root_conditions(
            self.continuous_eval_time(new_t, context.event_boundary),
            &trial.y_next,
        )?;
        let crossings = root_crossings_with_relation_memory(
            context.old_roots,
            &new_roots,
            self.atol,
            &self.model.model.problem.events.root_relation_memory_targets,
            &self.params,
        );
        if let Some(crossing) = crossings.first().copied() {
            let root = self.bisect_root(
                old_t,
                old_state.clone(),
                new_t,
                crossing,
                context.event_boundary,
            )?;
            let simultaneous_crossings = self.locate_simultaneous_crossings(
                old_t,
                &old_state,
                new_t,
                root.time,
                &crossings,
                context.event_boundary,
            )?;
            if rk_eval_trace_enabled() {
                tracing::debug!(
                    target: "rumoca_solver_rk45::eval",
                    "event root old_t={old_t:.12} new_t={new_t:.12} root_t={:.12} roots={}",
                    root.time,
                    simultaneous_crossings
                        .iter()
                        .map(|crossing| format!(
                            "{}->{:.0}",
                            crossing.index, crossing.post_relation_memory_value
                        ))
                        .collect::<Vec<_>>()
                        .join(",")
                );
            }
            self.pending_event_pre_y = Some(self.model.full_solver_y(
                root.time,
                &root.pre_state,
                &self.params,
                ALGEBRAIC_REFRESH_TOL,
                UPDATE_MAX_ITERS,
            )?);
            self.pending_event_pre_p = Some(self.params.clone());
            self.pending_root_crossings = simultaneous_crossings.clone();
            self.time = root.time;
            self.state = root.state;
            self.post_event_eval_time = None;
            self.clear_runtime_caches();
            return Ok(Some(StepUntilOutcome::RootFound { t_root: root.time }));
        }
        self.time = new_t;
        self.state = trial.y_next.clone();
        self.post_event_eval_time = None;
        if let Some(next_derivative) = trial.next_derivative.clone() {
            self.cache_derivative(self.time, &self.state, next_derivative);
        }
        Ok(None)
    }

    fn bisect_root(
        &self,
        mut lo_t: f64,
        mut lo_state: Vec<f64>,
        mut hi_t: f64,
        crossing: RootCrossing,
        event_boundary: Option<f64>,
    ) -> Result<LocatedRoot, SimError> {
        let mut lo_roots =
            self.eval_root_conditions(self.continuous_eval_time(lo_t, event_boundary), &lo_state)?;
        let mut hi_state = self
            .trial_step_from(lo_t, &lo_state, hi_t - lo_t, event_boundary)?
            .y_next;
        for _ in 0..ROOT_BISECTION_ITERS {
            let mid_t = lo_t + 0.5 * (hi_t - lo_t);
            let mid_state = self
                .trial_step_from(lo_t, &lo_state, mid_t - lo_t, event_boundary)?
                .y_next;
            let mid_roots = self.eval_root_conditions(
                self.continuous_eval_time(mid_t, event_boundary),
                &mid_state,
            )?;
            let old = root_value_at(&lo_roots, crossing.index, "left bisection")?;
            let new = root_value_at(&mid_roots, crossing.index, "midpoint bisection")?;
            if root_value_crossed(old, new, self.atol) {
                hi_t = mid_t;
                hi_state = mid_state;
            } else {
                lo_t = mid_t;
                lo_state = mid_state;
                lo_roots = mid_roots;
            }
        }
        Ok(LocatedRoot {
            time: hi_t,
            state: hi_state,
            pre_state: lo_state,
        })
    }

    fn locate_simultaneous_crossings(
        &self,
        old_t: f64,
        old_state: &[f64],
        new_t: f64,
        event_t: f64,
        crossings: &[RootCrossing],
        event_boundary: Option<f64>,
    ) -> Result<Vec<RootCrossing>, SimError> {
        let mut simultaneous = Vec::new();
        for crossing in crossings {
            let located =
                self.bisect_root(old_t, old_state.to_vec(), new_t, *crossing, event_boundary)?;
            if time_match_with_tol(located.time, event_t) {
                simultaneous.push(*crossing);
            }
        }
        if simultaneous.is_empty() {
            simultaneous.extend(crossings.first().copied());
        }
        Ok(simultaneous)
    }

    fn eval_root_conditions(&self, t: f64, state: &[f64]) -> Result<Vec<f64>, SimError> {
        if let Some(values) = self.cached_root_conditions(t, state) {
            return Ok(values);
        }
        let start = rk_eval_trace_enabled().then(Instant::now);
        let result = self
            .model
            .eval_root_conditions(
                t,
                state,
                &self.params,
                ALGEBRAIC_REFRESH_TOL,
                UPDATE_MAX_ITERS,
            )
            .inspect(|values| {
                self.cache_root_conditions(t, state, values);
            })
            .map_err(Into::into);
        if let Some(start) = start {
            record_root_eval_trace(start);
        }
        result
    }

    fn cached_root_conditions(&self, time: f64, state: &[f64]) -> Option<Vec<f64>> {
        let cache = self.root_cache.borrow();
        let cached = cache.as_ref()?;
        if !time_match_with_tol(cached.time, time) || !state_values_match(&cached.state, state) {
            return None;
        }
        Some(cached.values.clone())
    }

    fn cache_root_conditions(&self, time: f64, state: &[f64], values: &[f64]) {
        *self.root_cache.borrow_mut() = Some(CachedRootConditions {
            time,
            state: state.to_vec(),
            values: values.to_vec(),
        });
    }

    fn next_event_stop(&mut self, target_t: f64) -> Result<EventStop, SimError> {
        let solver_y = self.current_solver_y()?;
        let (time, event) = self.model.next_runtime_event_stop(
            &solver_y,
            &self.params,
            &mut self.stop_schedule,
            self.time,
            target_t,
        )?;
        Ok(EventStop { time, event })
    }

    fn apply_discrete_event_updates(
        &mut self,
        event_time: f64,
        _event: RuntimeEventStop,
    ) -> Result<(), SimError> {
        let event_entry_y = self
            .pending_event_pre_y
            .take()
            .map(Ok)
            .unwrap_or_else(|| self.current_solver_y())?;
        let event_entry_p = self
            .pending_event_pre_p
            .take()
            .unwrap_or_else(|| self.params.clone());
        let mut solver_y = self.current_solver_y()?;
        let root_overrides = self
            .pending_root_crossings
            .drain(..)
            .map(|crossing| (crossing.index, crossing.post_relation_memory_value))
            .collect::<Vec<_>>();
        let runtime = self.model;
        let state_count = runtime.state_count;
        let tol = self.atol;
        let outcome = runtime.apply_projected_event_update(
            ProjectedEventUpdateInput {
                y: &mut solver_y,
                p: &mut self.params,
                t: event_time,
                tol,
                event_pre_y: &event_entry_y,
                event_pre_p: &event_entry_p,
                max_iters: UPDATE_MAX_ITERS,
                row_filter: EventUpdateRowFilter::All,
                root_relation_overrides: &root_overrides,
            },
            move |y, p| project_rk_algebraics(runtime, y, p, event_time, state_count, tol),
        )?;
        self.copy_state_from_solver_y(&solver_y);
        let post_event_y = self.current_solver_y()?;
        commit_pre_params_after_event(
            &self.model.model,
            &post_event_y,
            &mut self.params,
            self.atol,
        );
        self.apply_event_action_outcome(outcome, event_time)?;
        self.clear_runtime_caches();
        Ok(())
    }

    fn apply_events_and_continue_or_finish(
        &mut self,
        event_time: f64,
        target_t: f64,
    ) -> Result<Option<StepUntilOutcome>, SimError> {
        let outcome = process_runtime_event_boundary(
            RuntimeEventBoundary {
                event_t: event_time,
                horizon_t: event_time.min(target_t),
                event: RuntimeEventStop::static_event(EventPreMode::EventEntry),
            },
            self,
        )?;
        self.apply_event_actions(outcome.final_t)?;
        Ok(Some(
            self.termination
                .as_ref()
                .map_or(StepUntilOutcome::InternalStep, |_| {
                    StepUntilOutcome::Finished
                }),
        ))
    }

    fn apply_scheduled_events_and_continue_or_finish(
        &mut self,
        event_time: f64,
        target_t: f64,
        event: RuntimeEventStop,
    ) -> Result<Option<StepUntilOutcome>, SimError> {
        self.time = event_time.max(self.time);
        let outcome = process_runtime_event_boundary(
            RuntimeEventBoundary {
                event_t: event_time,
                horizon_t: runtime_event_horizon(event, target_t, self.t_end),
                event,
            },
            self,
        )?;
        self.post_event_eval_time = outcome.right_limit_t;
        self.apply_event_actions(outcome.final_t)?;
        self.clear_runtime_caches();
        Ok(Some(
            self.termination
                .as_ref()
                .map_or(StepUntilOutcome::InternalStep, |_| {
                    StepUntilOutcome::Finished
                }),
        ))
    }

    fn apply_event_actions(&mut self, event_time: f64) -> Result<(), SimError> {
        let solver_y = self.current_solver_y()?;
        match self
            .model
            .eval_event_actions(&solver_y, &self.params, event_time)?
        {
            EventActionOutcome::Continue => Ok(()),
            outcome => self.apply_event_action_outcome(outcome, event_time),
        }
    }

    fn apply_event_action_outcome(
        &mut self,
        outcome: EventActionOutcome,
        event_time: f64,
    ) -> Result<(), SimError> {
        match outcome {
            EventActionOutcome::Continue => Ok(()),
            EventActionOutcome::AssertionFailed { time, message } => {
                Err(SimError::AssertionFailed {
                    time: if time.is_finite() { time } else { event_time },
                    message,
                })
            }
            EventActionOutcome::Terminated { time, message } => {
                self.termination = Some(SimTermination {
                    time: if time.is_finite() { time } else { event_time },
                    message,
                });
                Ok(())
            }
        }
    }

    fn public_time_eval_time(&self, time: f64) -> f64 {
        match self.post_event_eval_time {
            Some(eval_time) if time_match_with_tol(time, self.time) => eval_time,
            _ => time,
        }
    }

    fn continuous_eval_time(&self, time: f64, event_boundary: Option<f64>) -> f64 {
        match event_boundary {
            Some(boundary) if time >= boundary => timeline::event_left_limit_time(boundary),
            _ => self.public_time_eval_time(time),
        }
    }
}

impl SimulationBackend for Rk45Backend<'_> {
    fn init(&mut self) -> Result<(), Self::Error> {
        self.model.set_initial_event_flag(&mut self.params, true);
        let startup_event_pre_y = self.current_solver_y()?;
        let startup_event_pre_p = self.params.clone();
        let mut solver_y = self.current_solver_y()?;
        self.model.apply_initialization_updates(
            &mut solver_y,
            &mut self.params,
            self.time,
            self.atol,
            UPDATE_MAX_ITERS,
        )?;
        self.copy_state_from_solver_y(&solver_y);
        self.model.update_relation_memory_from_state(
            self.time,
            &self.state,
            &mut self.params,
            self.atol,
            UPDATE_MAX_ITERS,
        )?;
        let dynamic_event =
            self.model
                .current_dynamic_time_event_stop(&solver_y, &self.params, self.time)?;
        let runtime = self.model;
        let state_count = runtime.state_count;
        let tol = self.atol;
        let outcome = runtime.apply_projected_initial_event_boundary(
            ProjectedInitialEventInput {
                y: &mut solver_y,
                p: &mut self.params,
                t_start: self.time,
                t_end: self.t_end,
                tol,
                event_pre_y: &startup_event_pre_y,
                event_pre_p: &startup_event_pre_p,
                max_iters: UPDATE_MAX_ITERS,
                dynamic_event,
                apply_without_initial_event: false,
            },
            move |y, p, t| project_rk_algebraics(runtime, y, p, t, state_count, tol),
        )?;
        self.copy_state_from_solver_y(&solver_y);
        self.time = outcome.final_t;
        self.initial_observations = outcome.observations;
        self.apply_event_action_outcome(outcome.action, outcome.final_t)?;
        let post_initial_y = self.current_solver_y()?;
        commit_pre_params_after_event(
            &self.model.model,
            &post_initial_y,
            &mut self.params,
            self.atol,
        );
        self.clear_runtime_caches();
        Ok(())
    }

    fn step_until(&mut self, stop_time: f64) -> Result<StepUntilOutcome, Self::Error> {
        self.advance_to(stop_time)
    }

    fn read_state(&self) -> BackendState {
        BackendState { t: self.time }
    }
}

impl RuntimeEventBoundaryHandler for Rk45Backend<'_> {
    type Error = SimError;

    fn on_event_time(
        &mut self,
        event_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), Self::Error> {
        self.time = event_time.max(self.time);
        let (event_pre_y, event_pre_p) = self.event_pre_for_update()?;
        self.boundary_event_pre_y = Some(event_pre_y.clone());
        self.boundary_event_pre_p = Some(event_pre_p.clone());
        self.pending_event_pre_y = Some(event_pre_y);
        self.pending_event_pre_p = Some(event_pre_p);
        self.apply_discrete_event_updates(self.time, event)?;
        Ok(())
    }

    fn on_event_right_limit(
        &mut self,
        right_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), Self::Error> {
        let event_pre_y = if let Some(event_pre_y) = self.boundary_event_pre_y.clone() {
            event_pre_y
        } else {
            self.current_solver_y()?
        };
        let event_pre_p = self
            .boundary_event_pre_p
            .clone()
            .unwrap_or_else(|| self.params.clone());
        self.pending_event_pre_y = Some(event_pre_y);
        self.pending_event_pre_p = Some(event_pre_p);
        self.apply_discrete_event_updates(right_time, event)?;
        self.post_event_eval_time = Some(right_time);
        Ok(())
    }
}

impl Rk45Backend<'_> {
    fn event_pre_for_update(&mut self) -> Result<(Vec<f64>, Vec<f64>), SimError> {
        let event_pre_y = self
            .pending_event_pre_y
            .take()
            .map(Ok)
            .unwrap_or_else(|| self.current_solver_y())?;
        let event_pre_p = self
            .pending_event_pre_p
            .take()
            .unwrap_or_else(|| self.params.clone());
        Ok((event_pre_y, event_pre_p))
    }
}

fn advance_backend_to(backend: &mut Rk45Backend<'_>, target_t: f64) -> Result<(), SimError> {
    match backend.advance_to(target_t)? {
        StepUntilOutcome::StopReached | StepUntilOutcome::Finished => Ok(()),
        StepUntilOutcome::InternalStep | StepUntilOutcome::RootFound { .. } => Ok(()),
    }
}

fn default_output_dt(opts: &SimOptions) -> f64 {
    opts.dt
        .filter(|dt| dt.is_finite() && *dt > 0.0)
        .unwrap_or_else(|| ((opts.t_end - opts.t_start).abs() / 500.0).max(1.0e-3))
}

fn default_step_size(opts: &SimOptions) -> f64 {
    opts.dt
        .filter(|dt| dt.is_finite() && *dt > 0.0)
        .map(|dt| dt.min(0.01))
        .unwrap_or(1.0e-3)
}

fn project_rk_algebraics(
    runtime: &SolveRuntime,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    state_count: usize,
    tol: f64,
) -> Result<bool, RuntimeSolveError> {
    let before = y.to_vec();
    let state = y[..state_count.min(y.len())].to_vec();
    let refreshed = runtime.full_solver_y(t, &state, p, ALGEBRAIC_REFRESH_TOL, UPDATE_MAX_ITERS)?;
    y.copy_from_slice(&refreshed);
    Ok(solver_values_changed(&before, y, tol))
}

fn solver_values_changed(before: &[f64], after: &[f64], tol: f64) -> bool {
    before.len() != after.len()
        || before
            .iter()
            .zip(after)
            .any(|(old, new)| old.to_bits() != new.to_bits() && (old - new).abs() > tol)
}

fn time_match_with_tol(a: f64, b: f64) -> bool {
    rumoca_solver::time_match_with_tol(a, b)
}

fn state_values_match(a: &[f64], b: &[f64]) -> bool {
    a.len() == b.len()
        && a.iter()
            .zip(b)
            .all(|(lhs, rhs)| lhs.to_bits() == rhs.to_bits())
}

fn combine_stage(y: &[f64], h: f64, stages: &[(&[f64], f64)]) -> Result<Vec<f64>, SimError> {
    for (stage_index, (stage, _)) in stages.iter().enumerate() {
        ensure_len(
            stage.len(),
            y.len(),
            &format!("RK45 stage {stage_index} length"),
        )?;
    }
    let mut combined = checked_vec_with_capacity(y.len(), "RK45 combined stage")?;
    for (idx, value) in y.iter().copied().enumerate() {
        let mut delta = 0.0;
        for (stage, coeff) in stages {
            delta += coeff * stage[idx];
        }
        combined.push(value + h * delta);
    }
    Ok(combined)
}

fn error_norm(
    y: &[f64],
    y_high: &[f64],
    y_low: &[f64],
    atol: f64,
    rtol: f64,
) -> Result<f64, SimError> {
    ensure_len(y_high.len(), y.len(), "RK45 high-order estimate length")?;
    ensure_len(y_low.len(), y.len(), "RK45 low-order estimate length")?;
    let mut max_norm = 0.0_f64;
    for (idx, value) in y.iter().enumerate() {
        let high = y_high[idx];
        let low = y_low[idx];
        let scale = atol + rtol * value.abs().max(high.abs());
        max_norm = max_norm.max((high - low).abs() / scale.max(1.0e-30));
    }
    Ok(max_norm)
}

fn adapt_step(h: f64, error_norm: f64) -> f64 {
    if error_norm <= 0.0 {
        return (h * 5.0).max(MIN_STEP);
    }
    let factor = (0.9 * error_norm.powf(-0.2)).clamp(0.2, 5.0);
    (h * factor).max(MIN_STEP)
}

#[cfg(test)]
mod tests;
