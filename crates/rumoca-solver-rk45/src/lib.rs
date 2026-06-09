use std::{cell::RefCell, collections::HashMap, time::Instant};

use rumoca_eval_solve::{EventUpdateRowFilter, ProjectedEventUpdateInput, SolveRuntime};
use rumoca_ir_solve as solve;
use rumoca_solver::{
    BackendState, EventActionOutcome, EventPreMode, RootCrossing, RuntimeEventBoundary,
    RuntimeEventBoundaryHandler, RuntimeEventStop, RuntimeSolveError, SimOptions, SimResult,
    SimSolverMode, SimTermination, SimulationBackend, SolveStopSchedule, StepUntilOutcome,
    TimeoutBudget, TimeoutExceeded, commit_pre_params_after_event, convert_variable_meta,
    initial_runtime_event_stop, process_runtime_event_boundary,
    root_crossings_with_relation_memory, root_value_crossed, runtime_event_horizon, timeline,
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

#[derive(Debug, Clone)]
pub struct StepperState {
    pub time: f64,
    pub values: HashMap<String, f64>,
}

pub struct SimStepper {
    runtime: &'static SolveRuntime,
    backend: Rk45Backend<'static>,
    reset_snapshot: Rk45ResetSnapshot,
    input_values: HashMap<String, f64>,
}

impl SimStepper {
    pub fn new(model: &solve::SolveModel, opts: SimOptions) -> Result<Self, SimError> {
        match opts.solver_mode {
            SimSolverMode::Auto | SimSolverMode::RkLike => {}
            requested => return Err(SimError::UnsupportedSolverMode { requested }),
        }
        validate_explicit_solve_model(model)?;
        let runtime = Box::leak(Box::new(SolveRuntime::new(model)));
        let mut backend = Rk45Backend::new(runtime, &opts)?;
        backend.init()?;
        let reset_snapshot = backend.reset_snapshot();
        Ok(Self {
            runtime,
            backend,
            reset_snapshot,
            input_values: HashMap::new(),
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

    pub fn step(&mut self, dt: f64) -> Result<(), SimError> {
        if dt <= 0.0 {
            return Ok(());
        }
        let target = self.backend.time + dt;
        advance_backend_to(&mut self.backend, target)
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

    pub fn get(&self, name: &str) -> Option<f64> {
        if let Some(value) = self.input_values.get(name).copied() {
            return Some(value);
        }
        let solver_y = self.backend.current_solver_y().ok()?;
        self.runtime
            .visible_values(&solver_y, &self.backend.params, self.backend.time)
            .ok()
            .and_then(|values| {
                self.runtime
                    .model
                    .visible_names
                    .iter()
                    .position(|visible| visible == name)
                    .and_then(|idx| values.get(idx).copied())
            })
    }

    pub fn state(&self) -> StepperState {
        let values = self.stepper_visible_values();
        StepperState {
            time: self.time(),
            values,
        }
    }

    pub fn values_for(&self, names: &[String]) -> HashMap<String, f64> {
        let mut values = self
            .backend
            .current_solver_y()
            .ok()
            .and_then(|solver_y| {
                self.runtime
                    .visible_values_for_names(
                        &solver_y,
                        &self.backend.params,
                        self.backend.time,
                        names,
                    )
                    .ok()
            })
            .unwrap_or_default();
        for name in names {
            if let Some(value) = self.input_values.get(name).copied() {
                values.insert(name.clone(), value);
            }
        }
        values
    }

    pub fn input_names(&self) -> &[String] {
        self.runtime.model.problem.solve_layout.input_scalar_names()
    }

    pub fn variable_names(&self) -> &[String] {
        &self.runtime.model.visible_names
    }

    fn stepper_visible_values(&self) -> HashMap<String, f64> {
        let mut values: HashMap<String, f64> = self
            .backend
            .current_solver_y()
            .ok()
            .and_then(|solver_y| {
                self.runtime
                    .visible_values(&solver_y, &self.backend.params, self.backend.time)
                    .ok()
            })
            .map(|visible_values| {
                self.runtime
                    .model
                    .visible_names
                    .iter()
                    .cloned()
                    .zip(visible_values)
                    .collect()
            })
            .unwrap_or_default();
        values.extend(
            self.input_values
                .iter()
                .map(|(name, value)| (name.clone(), *value)),
        );
        values
    }
}

impl From<RuntimeSolveError> for SimError {
    fn from(value: RuntimeSolveError) -> Self {
        match value {
            RuntimeSolveError::SolveIr(message) => Self::SolveIr(message),
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
    let model = SolveRuntime::new(model);
    let sample_dt = default_output_dt(opts);
    let sample_times = timeline::build_output_times(opts.t_start, opts.t_end, sample_dt);
    let mut times = Vec::with_capacity(sample_times.len());
    let mut backend = Rk45Backend::new(&model, opts)?;
    backend.init()?;
    let mut data = vec![Vec::with_capacity(sample_times.len()); model.model.visible_names.len()];
    let solver_y = backend.current_solver_y()?;
    model.record_visible_sample(&mut data, &solver_y, &backend.params, opts.t_start)?;
    times.push(opts.t_start);

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
    if model.problem.continuous.implicit_rhs.len() < layout.state_scalar_count {
        return Err(SimError::SolveIr(format!(
            "implicit RHS has {} rows for {} states",
            model.problem.continuous.implicit_rhs.len(),
            layout.state_scalar_count
        )));
    }
    Ok(())
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
        let y2 = combine_stage(state, h, &[(&k1, 1.0 / 5.0)]);
        let k2 = self.derivatives_at(
            self.continuous_eval_time(time + h * (1.0 / 5.0), event_boundary),
            &y2,
        )?;

        let y3 = combine_stage(state, h, &[(&k1, 3.0 / 40.0), (&k2, 9.0 / 40.0)]);
        let k3 = self.derivatives_at(
            self.continuous_eval_time(time + h * (3.0 / 10.0), event_boundary),
            &y3,
        )?;

        let y4 = combine_stage(
            state,
            h,
            &[(&k1, 44.0 / 45.0), (&k2, -56.0 / 15.0), (&k3, 32.0 / 9.0)],
        );
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
        );
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
        );
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
        );

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
        );

        let error_norm = error_norm(state, &y5th, &y4th, self.atol, self.rtol);
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
            let old = lo_roots.get(crossing.index).copied().unwrap_or(0.0);
            let new = mid_roots.get(crossing.index).copied().unwrap_or(0.0);
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
        let initial_event = initial_runtime_event_stop(
            &self.model.model.problem,
            self.time,
            self.model
                .current_dynamic_time_event_stop(&solver_y, &self.params, self.time)?,
        );
        if let Some(event) = initial_event {
            self.pending_event_pre_y = Some(startup_event_pre_y);
            self.pending_event_pre_p = Some(startup_event_pre_p);
            let outcome = process_runtime_event_boundary(
                RuntimeEventBoundary {
                    event_t: self.time,
                    horizon_t: self.time,
                    event,
                },
                self,
            )?;
            self.apply_event_actions(outcome.final_t)?;
        }
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

fn combine_stage(y: &[f64], h: f64, stages: &[(&[f64], f64)]) -> Vec<f64> {
    y.iter()
        .enumerate()
        .map(|(idx, value)| {
            value
                + h * stages
                    .iter()
                    .map(|(k, coeff)| coeff * k.get(idx).copied().unwrap_or(0.0))
                    .sum::<f64>()
        })
        .collect()
}

fn error_norm(y: &[f64], y_high: &[f64], y_low: &[f64], atol: f64, rtol: f64) -> f64 {
    let mut max_norm = 0.0_f64;
    for (idx, value) in y.iter().enumerate() {
        let high = y_high.get(idx).copied().unwrap_or(0.0);
        let low = y_low.get(idx).copied().unwrap_or(0.0);
        let scale = atol + rtol * value.abs().max(high.abs());
        max_norm = max_norm.max((high - low).abs() / scale.max(1.0e-30));
    }
    max_norm
}

fn adapt_step(h: f64, error_norm: f64) -> f64 {
    if error_norm <= 0.0 {
        return (h * 5.0).max(MIN_STEP);
    }
    let factor = (0.9 * error_norm.powf(-0.2)).clamp(0.2, 5.0);
    (h * factor).max(MIN_STEP)
}

#[cfg(test)]
mod tests {
    use indexmap::IndexMap;
    use rumoca_ir_solve::{
        ComputeBlock, LinearOp, ScalarProgramBlock, SolveLayout, SolveProblem, SolverNameIndexMaps,
    };

    use super::*;

    #[test]
    fn rk45_simulates_solve_ir_integrator() {
        let model = single_state_model(vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        let result = simulate(
            &model,
            &SimOptions {
                t_end: 0.2,
                dt: Some(0.01),
                solver_mode: SimSolverMode::RkLike,
                ..Default::default()
            },
        )
        .expect("rk45 simulation should succeed");

        let final_x = result.data[0][result.data[0].len() - 1];
        assert!((final_x - 0.2_f64.exp()).abs() <= 1.0e-4);
    }

    #[test]
    fn rk45_emits_one_series_per_visible_name() {
        let mut model = single_state_model(vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        model.problem.solve_layout.discrete_valued_scalar_names = vec!["c[1]".to_string()];
        model.parameters = vec![1.0];
        model.visible_names = vec!["x".to_string(), "c[1]".to_string()];

        let result = simulate(
            &model,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.05),
                solver_mode: SimSolverMode::RkLike,
                ..Default::default()
            },
        )
        .expect("rk45 simulation should succeed");

        assert_eq!(result.names, ["x", "c[1]"]);
        assert_eq!(
            result.data.len(),
            result.names.len(),
            "simulation payload requires one data series per visible name"
        );
        assert!(
            result.data[1]
                .iter()
                .all(|value| (*value - 1.0).abs() <= f64::EPSILON)
        );
    }

    #[test]
    fn rk45_refreshes_algebraic_solve_ir_layout() {
        let mut model = single_state_model(vec![
            vec![
                LinearOp::LoadY { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::LoadY { dst: 0, index: 0 },
                LinearOp::Const { dst: 1, value: 2.0 },
                LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                LinearOp::StoreOutput { src: 2 },
            ],
        ]);
        model.problem.solve_layout.algebraic_scalar_count = 1;
        model.problem.solve_layout.solver_maps.names = vec!["x".to_string(), "a".to_string()];
        model.problem.solve_layout.solver_maps.name_to_idx =
            IndexMap::from([("x".to_string(), 0), ("a".to_string(), 1)]);
        model.problem.solve_layout.solver_maps.base_to_indices =
            IndexMap::from([("x".to_string(), vec![0]), ("a".to_string(), vec![1])]);
        model.problem.continuous.implicit_row_targets =
            vec![Some(solve::scalar_slot_y(0)), Some(solve::scalar_slot_y(1))];
        model.initial_y = vec![1.0, 2.0];
        model.visible_names = vec!["x".to_string(), "a".to_string()];

        let result = simulate(
            &model,
            &SimOptions {
                t_end: 0.1,
                dt: Some(0.01),
                solver_mode: SimSolverMode::RkLike,
                ..Default::default()
            },
        )
        .expect("rk45 should refresh explicit algebraic slots");

        let final_x = result.data[0][result.data[0].len() - 1];
        let final_a = result.data[1][result.data[1].len() - 1];
        assert!((final_x - 0.2_f64.exp()).abs() <= 1.0e-4);
        assert!((final_a - 2.0 * final_x).abs() <= 1.0e-8);
    }

    #[test]
    fn rk45_snapshots_pre_params_before_event_updates() {
        let mut model = single_state_model(vec![vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        model.initial_y = vec![10.0];
        model.parameters = vec![0.0];
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.solve_layout.pre_param_bindings = vec![solve::PreParamBinding {
            dest_p_index: 0,
            source: solve::PreParamSource::Y { index: 0 },
        }];
        model.problem.events.scheduled_time_events = vec![0.05];
        model.problem.discrete.update_targets = vec![solve::scalar_slot_y(0)];
        model.problem.discrete.pre_modes = vec![solve::DiscreteEventPreMode::EventEntry];
        model.problem.discrete.rhs = ScalarProgramBlock::new(vec![vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::Const {
                dst: 1,
                value: -0.8,
            },
            LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]]);

        let result = simulate(
            &model,
            &SimOptions {
                solver_mode: SimSolverMode::RkLike,
                t_end: 0.1,
                dt: Some(0.05),
                ..Default::default()
            },
        )
        .expect("rk45 should snapshot pre(v) before applying event updates");

        assert_eq!(result.times, vec![0.0, 0.05, 0.1]);
        assert_eq!(result.data[0], vec![10.0, -8.0, -8.0]);
    }

    #[test]
    fn rk45_terminate_returns_partial_success() {
        let mut model = single_state_model(vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        model.initial_y = vec![0.0];
        model.problem.events.root_conditions = ScalarProgramBlock::new(vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Const {
                dst: 1,
                value: 0.05,
            },
            LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]]);
        model.problem.discrete.rhs = ScalarProgramBlock::default();
        model.problem.events.action_conditions = const_scalar_program_block(1.0);
        model.problem.events.actions = vec![solve::SolveEventAction {
            kind: solve::SolveEventActionKind::Terminate,
            message: solve::SolveEventMessage {
                parts: vec![solve::SolveEventMessagePart::Text("finished".to_string())],
            },
            span: Default::default(),
            origin: "terminate(\"finished\")".to_string(),
        }];

        let result = simulate(
            &model,
            &SimOptions {
                solver_mode: SimSolverMode::RkLike,
                t_end: 0.1,
                dt: Some(0.1),
                ..Default::default()
            },
        )
        .expect("Modelica terminate should return partial simulation data");

        let termination = result.termination.expect("termination metadata");
        assert_eq!(termination.message, "finished");
        assert!((termination.time - 0.05).abs() <= 1.0e-6);
        assert!((result.times[result.times.len() - 1] - 0.05).abs() <= 1.0e-6);
        assert_eq!(result.data[0].len(), result.times.len());
    }

    #[test]
    fn rk45_applies_scheduled_time_event_update() {
        let mut model = single_state_model(vec![vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
        model.problem.events.scheduled_time_events = vec![0.05];
        model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
        model.problem.discrete.rhs = const_scalar_program_block(2.0);
        model.parameters = vec![0.0];
        model.visible_names = vec!["x".to_string(), "m".to_string()];

        let result = simulate(
            &model,
            &SimOptions {
                solver_mode: SimSolverMode::RkLike,
                t_end: 0.1,
                dt: Some(0.05),
                ..Default::default()
            },
        )
        .expect("rk45 should handle scheduled Solve-IR events");

        assert_eq!(result.times, vec![0.0, 0.05, 0.1]);
        assert_eq!(result.data[1], vec![0.0, 2.0, 2.0]);
    }

    #[test]
    fn rk45_simulate_records_initialization_updates_at_start_sample() {
        let mut model = single_state_model(vec![vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        model.initial_y = vec![1.0];
        model.problem.initialization.update_targets = vec![solve::scalar_slot_y(0)];
        model.problem.initialization.update_rhs = const_scalar_program_block(4.0);

        let result = simulate(
            &model,
            &SimOptions {
                solver_mode: SimSolverMode::RkLike,
                t_end: 0.1,
                dt: Some(0.1),
                ..Default::default()
            },
        )
        .expect("rk45 simulate should apply initialization before first sample");

        assert_eq!(result.data[0], vec![4.0, 4.0]);
    }

    #[test]
    fn rk45_applies_root_event_update() {
        let mut model = single_state_model(vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        model.initial_y = vec![0.0];
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
        model.problem.events.root_conditions = ScalarProgramBlock::new(vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Const {
                dst: 1,
                value: 0.05,
            },
            LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]]);
        model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
        model.problem.discrete.rhs = const_scalar_program_block(2.0);
        model.parameters = vec![0.0];
        model.visible_names = vec!["x".to_string(), "m".to_string()];

        let result = simulate(
            &model,
            &SimOptions {
                solver_mode: SimSolverMode::RkLike,
                t_end: 0.1,
                dt: Some(0.1),
                ..Default::default()
            },
        )
        .expect("rk45 should locate root events with Solve-IR residual rows");

        assert!((result.data[0][1] - 0.1).abs() <= 1.0e-6);
        assert_eq!(result.data[1], vec![0.0, 2.0]);
    }

    #[test]
    fn rk45_root_event_updates_relation_memory_for_continuous_if_branch() {
        let mut model = single_state_model(vec![vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 0.5 },
            LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Gt,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::Const { dst: 3, value: 1.0 },
            LinearOp::Const {
                dst: 4,
                value: -1.0,
            },
            LinearOp::Select {
                dst: 5,
                cond: 2,
                if_true: 3,
                if_false: 4,
            },
            LinearOp::StoreOutput { src: 5 },
        ]]);
        model.initial_y = vec![0.1];
        model.parameters = vec![0.0];
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
        model.problem.discrete.rhs = ScalarProgramBlock::new(vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 0.0 },
            LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Lt,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ]]);
        model.problem.events.root_conditions = ScalarProgramBlock::new(vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        model.problem.events.root_relation_memory_targets = vec![Some(solve::scalar_slot_p(0))];

        let result = simulate(
            &model,
            &SimOptions {
                solver_mode: SimSolverMode::RkLike,
                t_end: 0.2,
                dt: Some(0.2),
                ..Default::default()
            },
        )
        .expect("rk45 should update relation memory after root events");

        let final_x = result.data[0][result.data[0].len() - 1];
        assert!(
            final_x > 0.05,
            "relation memory should flip the continuous branch after root crossing; x={final_x}"
        );
    }

    #[test]
    fn rk45_applies_periodic_event_update() {
        let mut model = single_state_model(vec![vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
        model.problem.clocks.periodic_event_schedules = vec![solve::PeriodicEventSchedule {
            period_seconds: 0.05,
            phase_seconds: 0.05,
        }];
        model.problem.discrete.update_targets = vec![solve::scalar_slot_p(0)];
        model.problem.discrete.rhs = const_scalar_program_block(3.0);
        model.parameters = vec![0.0];
        model.visible_names = vec!["x".to_string(), "m".to_string()];

        let result = simulate(
            &model,
            &SimOptions {
                solver_mode: SimSolverMode::RkLike,
                t_end: 0.1,
                dt: Some(0.1),
                ..Default::default()
            },
        )
        .expect("rk45 should handle periodic Solve-IR events");

        assert_eq!(result.data[1], vec![0.0, 3.0]);
    }

    #[test]
    fn rk45_applies_dynamic_time_event_update() {
        let mut model = single_state_model(vec![vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        model.problem.solve_layout.compiled_parameter_len = 2;
        model.problem.solve_layout.discrete_real_scalar_names = vec!["next".to_string()];
        model.problem.solve_layout.discrete_valued_scalar_names = vec!["m".to_string()];
        model.problem.events.dynamic_time_event_names = vec!["next".to_string()];
        model.problem.discrete.update_targets = vec![solve::scalar_slot_p(1)];
        model.problem.discrete.rhs = const_scalar_program_block(4.0);
        model.parameters = vec![0.05, 0.0];
        model.visible_names = vec!["x".to_string(), "next".to_string(), "m".to_string()];

        let result = simulate(
            &model,
            &SimOptions {
                solver_mode: SimSolverMode::RkLike,
                t_end: 0.1,
                dt: Some(0.1),
                ..Default::default()
            },
        )
        .expect("rk45 should handle named dynamic Solve-IR time events");

        assert_eq!(result.data[2], vec![0.0, 4.0]);
    }

    #[test]
    fn runtime_contract_step_until_advances_rk45_backend() {
        let prepared = single_state_model(vec![vec![
            LinearOp::Const { dst: 0, value: 2.0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        let model = SolveRuntime::new(&prepared);
        let mut backend = Rk45Backend::new(
            &model,
            &SimOptions {
                solver_mode: SimSolverMode::RkLike,
                dt: Some(0.01),
                ..Default::default()
            },
        )
        .expect("backend should build");

        backend.init().expect("init should succeed");
        let outcome = backend.step_until(0.1).expect("backend should step");

        assert_eq!(outcome, StepUntilOutcome::StopReached);
        assert!((backend.read_state().t - 0.1).abs() <= 1.0e-12);
        assert!((backend.state[0] - 1.2).abs() <= 1.0e-6);
    }

    #[test]
    fn rk45_stepper_reset_restores_cached_initial_state() {
        let mut model = single_state_model(vec![vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        model.problem.solve_layout.compiled_parameter_len = 1;
        model.problem.solve_layout.input_scalar_names = vec!["u".to_string()];
        model.parameters = vec![0.0];
        let mut stepper = SimStepper::new(
            &model,
            SimOptions {
                solver_mode: SimSolverMode::RkLike,
                dt: Some(0.01),
                ..Default::default()
            },
        )
        .expect("stepper should build");

        stepper.set_input("u", 4.0).expect("input should exist");
        stepper.step(0.1).expect("stepper should advance");
        assert!(stepper.get("x").unwrap_or_default() > 1.1);

        stepper
            .reset(12.5)
            .expect("reset should restore cached initial state");

        assert!((stepper.time() - 12.5).abs() <= 1.0e-12);
        assert!((stepper.get("x").unwrap_or_default() - 1.0).abs() <= 1.0e-12);
        assert_eq!(
            stepper.get("u"),
            None,
            "reset should clear stale input overrides"
        );
    }

    #[test]
    fn rk45_stepper_uses_adaptive_event_integration_for_stiff_contact() {
        let model = stiff_contact_model();
        let mut stepper = SimStepper::new(
            &model,
            SimOptions {
                solver_mode: SimSolverMode::RkLike,
                dt: Some(0.02),
                ..Default::default()
            },
        )
        .expect("stiff contact stepper should initialize");

        for _ in 0..50 {
            stepper.step(0.02).expect("stepper should advance");
        }

        let x = stepper.get("x").expect("x should be visible");
        let contact = stepper.get("contact").expect("contact should be visible");
        assert!(
            x > -0.01 && x < 0.03,
            "adaptive stepper should settle near the contact surface without frame-step penetration; x={x}"
        );
        assert_eq!(contact, 1.0);
    }

    #[test]
    fn rk45_stepper_redetects_contact_after_thrust_liftoff() {
        let model = stiff_contact_model();
        let mut stepper = SimStepper::new(
            &model,
            SimOptions {
                solver_mode: SimSolverMode::RkLike,
                dt: Some(0.02),
                ..Default::default()
            },
        )
        .expect("stiff contact stepper should initialize");

        for _ in 0..50 {
            stepper.step(0.02).expect("initial settle should advance");
        }
        stepper
            .set_input("thrust", 40.0)
            .expect("thrust input should exist");
        for _ in 0..10 {
            stepper.step(0.02).expect("liftoff should advance");
        }
        let lifted_x = stepper.get("x").unwrap_or_default();
        assert!(
            lifted_x > 0.05,
            "thrust phase should lift the mass above the contact surface; x={lifted_x}"
        );

        stepper
            .set_input("thrust", 0.0)
            .expect("thrust input should update");
        for _ in 0..180 {
            stepper.step(0.02).expect("descent should advance");
        }

        let x = stepper.get("x").expect("x should be visible");
        let contact = stepper.get("contact").expect("contact should be visible");
        assert!(
            x > -0.02 && x < 0.04,
            "re-contact after liftoff should not miss the ground event; x={x}"
        );
        assert_eq!(contact, 1.0);
    }

    // SPEC_0021: Exception - test fixture declares the full Solve-IR model
    // inline so contact/event semantics stay visible in one place.
    #[allow(clippy::too_many_lines)]
    fn stiff_contact_model() -> solve::SolveModel {
        let dx = vec![
            LinearOp::LoadY { dst: 0, index: 1 },
            LinearOp::StoreOutput { src: 0 },
        ];
        let dv = vec![
            LinearOp::LoadP { dst: 0, index: 2 },
            LinearOp::Const { dst: 1, value: 0.5 },
            LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Gt,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::Const {
                dst: 3,
                value: -9.81,
            },
            LinearOp::LoadP { dst: 4, index: 1 },
            LinearOp::Binary {
                dst: 5,
                op: solve::BinaryOp::Add,
                lhs: 3,
                rhs: 4,
            },
            LinearOp::LoadY { dst: 6, index: 0 },
            LinearOp::Const {
                dst: 7,
                value: -30000.0,
            },
            LinearOp::Binary {
                dst: 8,
                op: solve::BinaryOp::Mul,
                lhs: 6,
                rhs: 7,
            },
            LinearOp::LoadY { dst: 9, index: 1 },
            LinearOp::Const {
                dst: 10,
                value: -200.0,
            },
            LinearOp::Binary {
                dst: 11,
                op: solve::BinaryOp::Mul,
                lhs: 9,
                rhs: 10,
            },
            LinearOp::Binary {
                dst: 12,
                op: solve::BinaryOp::Add,
                lhs: 5,
                rhs: 8,
            },
            LinearOp::Binary {
                dst: 13,
                op: solve::BinaryOp::Add,
                lhs: 12,
                rhs: 11,
            },
            LinearOp::Select {
                dst: 14,
                cond: 2,
                if_true: 13,
                if_false: 5,
            },
            LinearOp::StoreOutput { src: 14 },
        ];
        solve::SolveModel {
            problem: SolveProblem {
                schema_version: solve::SOLVE_SCHEMA_VERSION,
                layout: solve::VarLayout::from_parts(Default::default(), 2, 1),
                continuous: solve::ContinuousSolveSystem {
                    implicit_rhs: ComputeBlock::from_scalar_program_block(ScalarProgramBlock::new(
                        vec![dx.clone(), dv.clone()],
                    )),
                    implicit_row_targets: vec![
                        Some(solve::scalar_slot_y(0)),
                        Some(solve::scalar_slot_y(1)),
                    ],
                    residual: ScalarProgramBlock::new(vec![dx.clone(), dv.clone()]),
                    derivative_rhs: ComputeBlock::from_scalar_program_block(
                        ScalarProgramBlock::new(vec![dx, dv]),
                    ),
                    algebraic_projection_plan: solve::AlgebraicProjectionPlan::default(),
                },
                initialization: solve::InitializationSolveSystem::default(),
                discrete: solve::DiscreteSolveSystem {
                    rhs: ScalarProgramBlock::new(vec![vec![
                        LinearOp::LoadY { dst: 0, index: 0 },
                        LinearOp::Const { dst: 1, value: 0.0 },
                        LinearOp::Compare {
                            dst: 2,
                            op: solve::CompareOp::Lt,
                            lhs: 0,
                            rhs: 1,
                        },
                        LinearOp::StoreOutput { src: 2 },
                    ]]),
                    update_targets: vec![solve::scalar_slot_p(2)],
                    ..Default::default()
                },
                events: solve::SolveEventPartition {
                    root_conditions: ScalarProgramBlock::new(vec![vec![
                        LinearOp::LoadY { dst: 0, index: 0 },
                        LinearOp::StoreOutput { src: 0 },
                    ]]),
                    root_relation_memory_targets: vec![Some(solve::scalar_slot_p(2))],
                    ..Default::default()
                },
                clocks: solve::SolveClockPartition::default(),
                solve_layout: SolveLayout {
                    solver_maps: SolverNameIndexMaps {
                        names: vec!["x".to_string(), "v".to_string()],
                        name_to_idx: IndexMap::from([("x".to_string(), 0), ("v".to_string(), 1)]),
                        base_to_indices: IndexMap::from([
                            ("x".to_string(), vec![0]),
                            ("v".to_string(), vec![1]),
                        ]),
                    },
                    state_scalar_count: 2,
                    algebraic_scalar_count: 0,
                    output_scalar_count: 0,
                    parameter_count: 1,
                    compiled_parameter_len: 3,
                    input_scalar_names: vec!["thrust".to_string()],
                    discrete_real_scalar_names: Vec::new(),
                    discrete_valued_scalar_names: vec!["contact".to_string()],
                    relation_memory_parameter_indices: Vec::new(),
                    initial_event_parameter_index: None,
                    pre_param_bindings: Vec::new(),
                },
            },
            artifacts: solve::SolveArtifacts {
                continuous: solve::ContinuousSolveArtifacts::default(),
            },
            initial_y: vec![0.02, 0.0],
            parameters: vec![0.0, 0.0, 0.0],
            external_tables: solve::ExternalTables::default(),
            visible_names: vec!["x".to_string(), "v".to_string(), "contact".to_string()],
            visible_value_rows: solve::ScalarProgramBlock::default(),
            variable_meta: Vec::new(),
        }
    }

    fn single_state_model(rhs_rows: Vec<Vec<LinearOp>>) -> solve::SolveModel {
        let derivative_rows = rhs_rows.iter().take(1).cloned().collect::<Vec<_>>();
        let zero = ScalarProgramBlock::new(vec![vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]]);
        solve::SolveModel {
            problem: SolveProblem {
                schema_version: solve::SOLVE_SCHEMA_VERSION,
                layout: solve::VarLayout::from_parts(Default::default(), 1, 1),
                continuous: solve::ContinuousSolveSystem {
                    implicit_rhs: ComputeBlock::from_scalar_program_block(ScalarProgramBlock::new(
                        rhs_rows.clone(),
                    )),
                    implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                    residual: ScalarProgramBlock::new(rhs_rows.clone()),
                    derivative_rhs: ComputeBlock::from_scalar_program_block(
                        ScalarProgramBlock::new(derivative_rows),
                    ),
                    algebraic_projection_plan: solve::AlgebraicProjectionPlan::default(),
                },
                initialization: solve::InitializationSolveSystem {
                    residual: zero.clone(),
                    row_targets: Vec::new(),
                    projection_indices: Vec::new(),
                    projection_plan: solve::AlgebraicProjectionPlan::default(),
                    update_rhs: solve::ScalarProgramBlock::default(),
                    update_targets: Vec::new(),
                },
                discrete: solve::DiscreteSolveSystem {
                    rhs: zero.clone(),
                    ..Default::default()
                },
                events: solve::SolveEventPartition::default(),
                clocks: solve::SolveClockPartition::default(),
                solve_layout: SolveLayout {
                    solver_maps: SolverNameIndexMaps {
                        names: vec!["x".to_string()],
                        name_to_idx: IndexMap::from([("x".to_string(), 0)]),
                        base_to_indices: IndexMap::from([("x".to_string(), vec![0])]),
                    },
                    state_scalar_count: 1,
                    algebraic_scalar_count: 0,
                    output_scalar_count: 0,
                    parameter_count: 0,
                    compiled_parameter_len: 0,
                    input_scalar_names: Vec::new(),
                    discrete_real_scalar_names: Vec::new(),
                    discrete_valued_scalar_names: Vec::new(),
                    relation_memory_parameter_indices: Vec::new(),
                    initial_event_parameter_index: None,
                    pre_param_bindings: Vec::new(),
                },
            },
            artifacts: solve::SolveArtifacts {
                continuous: solve::ContinuousSolveArtifacts {
                    mass_matrix: vec![vec![1.0]],
                    implicit_jacobian_v: ComputeBlock::from_scalar_program_block(zero.clone()),
                    implicit_jacobian_v_scalar: zero.clone(),
                    full_jacobian_v: zero.clone(),
                },
            },
            initial_y: vec![1.0],
            parameters: Vec::new(),
            external_tables: solve::ExternalTables::default(),
            visible_names: vec!["x".to_string()],
            visible_value_rows: solve::ScalarProgramBlock::default(),
            variable_meta: Vec::new(),
        }
    }

    fn const_scalar_program_block(value: f64) -> ScalarProgramBlock {
        ScalarProgramBlock::new(vec![vec![
            LinearOp::Const { dst: 0, value },
            LinearOp::StoreOutput { src: 0 },
        ]])
    }
}
