// SPEC_0021 file-size exception: RK45 currently combines stepping, event
// boundary handling, and output sampling. split plan: move event handling and
// dense-output/sample scheduling into focused solver modules.

//! The rk-like Model Exchange host.
//!
//! Per SPEC_0038 §Internal Solver Boundary this crate is an *integrator*: it
//! owns the Dormand-Prince tableau, step control, dense output, and root
//! localization, and reaches the model only through
//! [`rumoca_solver::fmi_me::ModelExchangeKernel`]. It does not depend on
//! `rumoca-ir-solve` or `rumoca-eval-solve` at all, so naming a Solve row,
//! layout, opcode, event, or runtime object here does not compile.

use std::time::Instant;

use indexmap::IndexMap;
use rumoca_solver::{
    BackendState, SimOptions, SimResult, SimSolverMode, SimTermination, SimulationBackend,
    StepUntilOutcome, TimeoutBudget, TimeoutExceeded,
    fmi_me::{
        MeError, MeEventCause, MeEventEntry, MeEventStop, MeIndicatorCrossing, MeInstanceConfig,
        MeModelSource, MeObservation, MeOutputSeries, MeRootProfile, MeTime, ModelExchangeKernel,
        SolveMeKernel, event_indicator_crossed,
    },
    timeline,
};

mod dense_output;
mod no_state;
mod reset;
mod trace;

use dense_output::Dopri5DenseOutput;
use no_state::NoStateSession;
use reset::Rk45ResetSnapshot;
use trace::{
    record_derivative_eval_trace, record_root_eval_trace, reset_rk_eval_trace,
    rk_eval_trace_enabled, trace_rk_eval_snapshot,
};

/// FMI `instanceName` for this host's component.
pub(crate) const INSTANCE_NAME: &str = "rk-like";

const MIN_STEP: f64 = 1.0e-12;
const ROOT_LOCALIZATION_MAX_ITERS: usize = 64;

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

impl From<MeError> for SimError {
    fn from(value: MeError) -> Self {
        // The component's stage annotation is machine metadata for a host that
        // buckets failures; this host reports failures by variant, so peeling it
        // keeps every mapping — and every rendered message — exactly what it was
        // before the component started minting stages.
        match value.into_kind() {
            MeError::NoContinuousStates => Self::EmptySystem,
            MeError::UnsupportedModel { reason } => Self::UnsupportedModel { reason },
            MeError::Evaluation { message } => Self::SolveIr(message),
            MeError::NonFiniteDerivative { state_name } => Self::NonFiniteDerivative { state_name },
            MeError::Contract { reason } => Self::RuntimeContract { reason },
            MeError::Assertion { time, message } => Self::AssertionFailed { time, message },
            MeError::Allocation { context, entries } => Self::Allocation { context, entries },
            // `into_kind` peels every annotation, so the annotated variant
            // cannot reach here; naming it keeps the match exhaustive without a
            // catch-all that would silently absorb a future variant.
            staged @ MeError::Staged { .. } => Self::RuntimeContract {
                reason: format!("stage annotation survived peeling: {staged}"),
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct SessionState {
    pub time: f64,
    pub values: IndexMap<String, f64>,
}

pub struct SimulationSession {
    inner: SimulationSessionInner,
}

enum SimulationSessionInner {
    NoState(Box<NoStateSession>),
    State(Box<StateSession>),
}

struct StateSession {
    backend: Rk45Backend,
    reset_snapshot: Rk45ResetSnapshot,
    input_values: IndexMap<String, f64>,
}

/// FMI instantiation arguments this host uses for every component it owns.
fn instance_config(opts: &SimOptions) -> MeInstanceConfig {
    MeInstanceConfig {
        instance_name: INSTANCE_NAME,
        tolerance: opts.atol.max(1.0e-12),
        start_time: opts.t_start,
        stop_time: opts.t_end,
        root_profile: MeRootProfile::Component,
        numerics_profile: rumoca_solver::fmi_me::MeNumericsProfile::Component,
    }
}

impl SimulationSession {
    pub fn new<'a>(
        model: impl Into<MeModelSource<'a>>,
        opts: SimOptions,
    ) -> Result<Self, SimError> {
        match opts.solver_mode {
            SimSolverMode::Auto | SimSolverMode::RkLike => {}
            requested => return Err(SimError::UnsupportedSolverMode { requested }),
        }
        let source = model.into();
        match SolveMeKernel::instantiate(source, &instance_config(&opts)) {
            // A model with no continuous states has no integrator to host;
            // the component's zero-state path owns it instead.
            Err(MeError::NoContinuousStates) => {
                NoStateSession::new(source, opts).map(|session| Self {
                    inner: SimulationSessionInner::NoState(Box::new(session)),
                })
            }
            Err(error) => Err(error.into()),
            Ok(kernel) => StateSession::new(kernel, &opts).map(|session| Self {
                inner: SimulationSessionInner::State(Box::new(session)),
            }),
        }
    }

    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        match &mut self.inner {
            SimulationSessionInner::NoState(session) => session.set_input(name, value),
            SimulationSessionInner::State(session) => session.set_input(name, value),
        }
    }

    pub fn set_inputs(&mut self, inputs: &[(&str, f64)]) -> Result<(), SimError> {
        for (name, value) in inputs {
            self.set_input(name, *value)?;
        }
        Ok(())
    }

    pub fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        match &mut self.inner {
            SimulationSessionInner::NoState(session) => session.advance_to(target_time),
            SimulationSessionInner::State(session) => session.advance_to(target_time),
        }
    }

    /// Ensure the finite integration horizon includes `target_time`.
    ///
    /// Batch callers keep the original `SimOptions::t_end`; live callers may
    /// extend the horizon as they advance without rebuilding model state.
    pub fn ensure_end_time(&mut self, target_time: f64) {
        match &mut self.inner {
            SimulationSessionInner::NoState(session) => session.ensure_end_time(target_time),
            SimulationSessionInner::State(session) => session.ensure_end_time(target_time),
        }
    }

    pub fn step(&mut self, dt: f64) -> Result<(), SimError> {
        match &mut self.inner {
            SimulationSessionInner::NoState(session) => session.step(dt),
            SimulationSessionInner::State(session) => session.step(dt),
        }
    }

    pub fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        match &mut self.inner {
            SimulationSessionInner::NoState(session) => session.reset(t_start),
            SimulationSessionInner::State(session) => session.reset(t_start),
        }
    }

    pub fn time(&self) -> f64 {
        match &self.inner {
            SimulationSessionInner::NoState(session) => session.time(),
            SimulationSessionInner::State(session) => session.time(),
        }
    }

    pub fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        match &self.inner {
            SimulationSessionInner::NoState(session) => session.get(name),
            SimulationSessionInner::State(session) => session.get(name),
        }
    }

    pub fn state(&self) -> Result<SessionState, SimError> {
        match &self.inner {
            SimulationSessionInner::NoState(session) => session.state(),
            SimulationSessionInner::State(session) => session.state(),
        }
    }

    pub fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        match &self.inner {
            SimulationSessionInner::NoState(session) => session.values_for(names),
            SimulationSessionInner::State(session) => session.values_for(names),
        }
    }

    pub fn input_names(&self) -> &[String] {
        match &self.inner {
            SimulationSessionInner::NoState(session) => session.input_names(),
            SimulationSessionInner::State(session) => session.input_names(),
        }
    }

    pub fn variable_names(&self) -> &[String] {
        match &self.inner {
            SimulationSessionInner::NoState(session) => session.variable_names(),
            SimulationSessionInner::State(session) => session.variable_names(),
        }
    }
}

impl StateSession {
    fn new(kernel: SolveMeKernel, opts: &SimOptions) -> Result<Self, SimError> {
        let mut backend = Rk45Backend::new(kernel, opts)?;
        backend.init()?;
        let reset_snapshot = backend.reset_snapshot();
        Ok(Self {
            backend,
            reset_snapshot,
            input_values: IndexMap::new(),
        })
    }

    fn set_input(&mut self, name: &str, value: f64) -> Result<(), SimError> {
        let Some(reference) = self.backend.kernel.value_reference(name) else {
            return Err(SimError::SolveIr(format!("unknown input '{name}'")));
        };
        self.input_values.insert(name.to_string(), value);
        self.backend.sync_kernel_to_current_point()?;
        self.backend.kernel.set_float64(&[reference], &[value])?;
        Ok(())
    }

    fn advance_to(&mut self, target_time: f64) -> Result<(), SimError> {
        let target_time = target_time.min(self.backend.t_end);
        if target_time <= self.backend.time {
            return Ok(());
        }
        advance_backend_to(&mut self.backend, target_time)?;
        self.backend.sync_kernel_to_current_point()
    }

    fn ensure_end_time(&mut self, target_time: f64) {
        if !target_time.is_finite() || target_time <= self.backend.t_end {
            return;
        }
        let t_end = target_time + (target_time - self.backend.time).max(1.0);
        if !t_end.is_finite() {
            return;
        }
        if self
            .backend
            .kernel
            .extend_stop_time(self.backend.time, t_end)
            .is_ok()
        {
            self.backend.t_end = t_end;
        }
    }

    fn step(&mut self, dt: f64) -> Result<(), SimError> {
        if dt <= 0.0 {
            return Ok(());
        }
        self.advance_to(self.backend.time + dt)
    }

    fn reset(&mut self, t_start: f64) -> Result<(), SimError> {
        self.input_values.clear();
        self.backend
            .reset_to_snapshot(&self.reset_snapshot, t_start)
    }

    fn time(&self) -> f64 {
        self.backend.time
    }

    fn get(&self, name: &str) -> Result<Option<f64>, SimError> {
        if let Some(value) = self.input_values.get(name).copied() {
            return Ok(Some(value));
        }
        let observation = self.backend.kernel.observe()?;
        let Some(idx) = self
            .backend
            .kernel
            .model_description()
            .output_names
            .iter()
            .position(|visible| visible == name)
        else {
            return Ok(None);
        };
        let mut values = Vec::new();
        self.backend
            .kernel
            .get_outputs(&observation, self.backend.time, &mut values)?;
        values.get(idx).copied().map(Some).ok_or_else(|| {
            SimError::RuntimeContract {
                reason: format!(
                    "visible value '{name}' resolved to index {idx}, but runtime returned {} values",
                    values.len()
                ),
            }
        })
    }

    fn state(&self) -> Result<SessionState, SimError> {
        Ok(SessionState {
            time: self.time(),
            values: self.session_visible_values()?,
        })
    }

    fn values_for(&self, names: &[String]) -> Result<IndexMap<String, f64>, SimError> {
        let visible_values = self.session_visible_values()?;
        let mut values = IndexMap::with_capacity(names.len());
        for name in names {
            if let Some(value) = visible_values.get(name).copied() {
                values.insert(name.clone(), value);
            }
        }
        Ok(values)
    }

    fn input_names(&self) -> &[String] {
        self.backend.kernel.model_description().input_names
    }

    fn variable_names(&self) -> &[String] {
        self.backend.kernel.model_description().output_names
    }

    fn session_visible_values(&self) -> Result<IndexMap<String, f64>, SimError> {
        let observation = self.backend.kernel.observe()?;
        let mut visible_values = Vec::new();
        self.backend
            .kernel
            .get_outputs(&observation, self.backend.time, &mut visible_values)?;
        let mut values = collect_visible_values(
            self.backend.kernel.model_description().output_names,
            visible_values,
        )?;
        values.extend(
            self.input_values
                .iter()
                .map(|(name, value)| (name.clone(), *value)),
        );
        Ok(values)
    }
}

fn record_rk_initial_samples(
    backend: &mut Rk45Backend,
    series: &mut MeOutputSeries,
    times: &mut Vec<f64>,
    t_start: f64,
) -> Result<(), SimError> {
    if backend.kernel.initial_observations().is_empty() {
        backend.sync_kernel_to_current_point()?;
        let observation = backend.kernel.observe()?;
        backend
            .kernel
            .record_outputs(&observation, t_start, series)?;
        times.push(t_start);
        return Ok(());
    }
    for index in 0..backend.kernel.initial_observations().len() {
        let observation: &MeObservation = &backend.kernel.initial_observations()[index];
        let sample_t = observation.time();
        if backend
            .kernel
            .initial_observations()
            .get(index + 1)
            .is_some_and(|next| time_match_with_tol(sample_t, next.time()))
        {
            // Initial event iteration may expose several superdense values at
            // one semantic start time. Batch traces own one column per time;
            // retain the settled (last) value for that instant, matching the
            // ordinary output-time replacement policy.
            continue;
        }
        backend
            .kernel
            .record_outputs(observation, sample_t, series)?;
        times.push(sample_t);
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

/// The Dormand-Prince host driving one ME component.
pub(crate) struct Rk45Backend {
    pub(crate) kernel: SolveMeKernel,
    pub(crate) time: f64,
    pub(crate) state: Vec<f64>,
    atol: f64,
    state_atol: Vec<f64>,
    rtol: f64,
    pub(crate) next_step: f64,
    t_end: f64,
    budget: TimeoutBudget,
    pub(crate) termination: Option<SimTermination>,
}

struct TrialStep {
    y_next: Vec<f64>,
    stages: [Vec<f64>; 7],
    error_norm: f64,
}

struct StepAcceptanceContext<'a> {
    old_roots: &'a [f64],
    target_t: f64,
    event_boundary: Option<f64>,
}

#[derive(Clone)]
struct LocatedRoot {
    time: f64,
    state: Vec<f64>,
    pre_state: Vec<f64>,
    time_tolerance: f64,
}

struct RootLocalizationInput<'a> {
    old_roots: &'a [f64],
    new_roots: &'a [f64],
    crossings: &'a [MeIndicatorCrossing],
    event_boundary: Option<f64>,
}

pub fn simulate<'a>(
    model: impl Into<MeModelSource<'a>>,
    opts: &SimOptions,
) -> Result<SimResult, SimError> {
    reset_rk_eval_trace();
    match opts.solver_mode {
        SimSolverMode::Auto | SimSolverMode::RkLike => {}
        requested => return Err(SimError::UnsupportedSolverMode { requested }),
    }

    let kernel = SolveMeKernel::instantiate(model.into(), &instance_config(opts))?;
    let sample_dt = default_output_dt(opts);
    let sample_times = timeline::try_build_output_times(opts.t_start, opts.t_end, sample_dt)
        .map_err(|error| SimError::RuntimeContract {
            reason: error.to_string(),
        })?;
    let mut times = checked_vec_with_capacity(sample_times.len(), "RK45 output times")?;
    let mut backend = Rk45Backend::new(kernel, opts)?;
    backend.init()?;
    let output_count = backend.kernel.model_description().output_names.len();
    let mut series = MeOutputSeries::with_capacity(output_count, sample_times.len())?;
    record_rk_initial_samples(&mut backend, &mut series, &mut times, opts.t_start)?;

    for &target_t in sample_times.iter().skip(1) {
        advance_backend_to(&mut backend, target_t)?;
        backend.sync_kernel_to_current_point()?;
        let observation = backend.kernel.observe()?;
        let sample_t = backend.time;
        if !times
            .last()
            .copied()
            .is_some_and(|last_t| time_match_with_tol(last_t, sample_t))
        {
            backend
                .kernel
                .record_outputs(&observation, sample_t, &mut series)?;
            times.push(sample_t);
        }
        if backend.termination.is_some() {
            break;
        }
    }

    trace_rk_eval_snapshot(INSTANCE_NAME);
    backend.kernel.terminate()?;
    let description = backend.kernel.model_description();
    let names = description.output_names.to_vec();
    let n_states = description.continuous_state_count;
    let variable_meta = description.output_meta.to_vec();
    Ok(SimResult {
        times,
        names,
        data: series.into_columns(),
        n_states,
        variable_meta,
        termination: backend.termination,
    })
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

fn located_dense_root(
    dense_output: &Dopri5DenseOutput,
    time: f64,
    time_tolerance: f64,
) -> Result<LocatedRoot, SimError> {
    let state = dense_output.evaluate(time)?;
    let pre_time = timeline::event_left_limit_time(time).max(dense_output.start_time());
    let pre_state = dense_output.evaluate(pre_time)?;
    Ok(LocatedRoot {
        time,
        state,
        pre_state,
        time_tolerance,
    })
}

fn safeguarded_root_candidate(lo_t: f64, hi_t: f64, lo_value: f64, hi_value: f64) -> (f64, bool) {
    let width = hi_t - lo_t;
    let denominator = hi_value - lo_value;
    let secant = hi_t - hi_value * width / denominator;
    let guard = 0.05 * width;
    if denominator.is_finite()
        && denominator != 0.0
        && secant.is_finite()
        && secant > lo_t + guard
        && secant < hi_t - guard
    {
        (secant, true)
    } else {
        (lo_t + 0.5 * width, false)
    }
}

fn root_time_tolerance(
    lo_t: f64,
    hi_t: f64,
    lo_value: f64,
    hi_value: f64,
    root_tolerance: f64,
) -> f64 {
    let midpoint = lo_t + 0.5 * (hi_t - lo_t);
    let ulp = (midpoint.next_up() - midpoint)
        .abs()
        .max((midpoint - midpoint.next_down()).abs());
    let slope = ((hi_value - lo_value) / (hi_t - lo_t)).abs();
    let residual_time = if slope.is_finite() && slope > 0.0 {
        root_tolerance / slope
    } else {
        0.0
    };
    (4.0 * ulp).max(residual_time)
}

fn roots_are_simultaneous(first: &LocatedRoot, candidate: &LocatedRoot) -> bool {
    time_match_with_tol(first.time, candidate.time)
        || (first.time - candidate.time).abs() <= first.time_tolerance.max(candidate.time_tolerance)
}

impl Rk45Backend {
    fn new(kernel: SolveMeKernel, opts: &SimOptions) -> Result<Self, SimError> {
        let state_count = kernel.model_description().continuous_state_count;
        let mut state = vec![0.0; state_count];
        kernel.get_continuous_states(&mut state)?;
        let next_step = default_step_size(opts);
        if !next_step.is_finite() || next_step <= 0.0 {
            return Err(SimError::StepSizeUnderflow {
                target_t: opts.t_end,
            });
        }
        let atol = opts.atol.max(1.0e-12);
        let mut nominals = vec![0.0; state_count];
        kernel.get_nominals_of_continuous_states(&mut nominals)?;
        Ok(Self {
            kernel,
            time: opts.t_start,
            state,
            atol,
            state_atol: nominals
                .into_iter()
                .map(|scale| (atol * scale).min(f64::MAX))
                .collect(),
            rtol: opts.rtol.max(1.0e-12),
            next_step,
            t_end: opts.t_end,
            budget: TimeoutBudget::new(opts.max_wall_seconds),
            termination: None,
        })
    }

    /// Put the component at the host's accepted point.
    ///
    /// `fmi3SetTime` + `fmi3SetContinuousStates`: the ordinary FMI discipline
    /// before reading variables or asking for the next event.
    pub(crate) fn sync_kernel_to_current_point(&mut self) -> Result<(), SimError> {
        self.kernel.set_time(MeTime::at(self.time))?;
        self.kernel.set_continuous_states(&self.state)?;
        Ok(())
    }

    fn project_accepted_state(&mut self, time: f64, state: &mut [f64]) -> Result<bool, SimError> {
        self.kernel.set_time(MeTime::at(time))?;
        Ok(self.kernel.project_continuous_states(state)?)
    }

    fn trial_step(&mut self, h: f64, event_boundary: Option<f64>) -> Result<TrialStep, SimError> {
        let time = self.time;
        let state = std::mem::take(&mut self.state);
        let result = self.trial_step_from(time, &state, h, event_boundary);
        self.state = state;
        result
    }

    /// `fmi3SetTime` + `fmi3SetContinuousStates` +
    /// `fmi3GetContinuousStateDerivatives` for one tableau stage.
    fn derivatives_at(
        &mut self,
        time: f64,
        state: &[f64],
        event_boundary: Option<f64>,
    ) -> Result<Vec<f64>, SimError> {
        self.kernel.set_time(MeTime::new(time, event_boundary))?;
        self.kernel.set_continuous_states(state)?;
        let start = rk_eval_trace_enabled().then(Instant::now);
        let mut derivatives = Vec::new();
        self.kernel
            .get_continuous_state_derivatives(&mut derivatives)?;
        if let Some(start) = start {
            record_derivative_eval_trace(start);
        }
        Ok(derivatives)
    }

    /// `fmi3SetTime` + `fmi3SetContinuousStates` + `fmi3GetEventIndicators`.
    fn event_indicators_at(
        &mut self,
        time: f64,
        state: &[f64],
        event_boundary: Option<f64>,
    ) -> Result<Vec<f64>, SimError> {
        self.kernel.set_time(MeTime::new(time, event_boundary))?;
        self.kernel.set_continuous_states(state)?;
        let start = rk_eval_trace_enabled().then(Instant::now);
        let mut indicators = Vec::new();
        self.kernel.get_event_indicators(&mut indicators)?;
        if let Some(start) = start {
            record_root_eval_trace(start);
        }
        Ok(indicators)
    }

    fn trial_step_from(
        &mut self,
        time: f64,
        state: &[f64],
        h: f64,
        event_boundary: Option<f64>,
    ) -> Result<TrialStep, SimError> {
        let k1 = self.derivatives_at(time, state, event_boundary)?;
        let y2 = combine_stage(state, h, &[(&k1, 1.0 / 5.0)])?;
        let k2 = self.derivatives_at(time + h * (1.0 / 5.0), &y2, event_boundary)?;

        let y3 = combine_stage(state, h, &[(&k1, 3.0 / 40.0), (&k2, 9.0 / 40.0)])?;
        let k3 = self.derivatives_at(time + h * (3.0 / 10.0), &y3, event_boundary)?;

        let y4 = combine_stage(
            state,
            h,
            &[(&k1, 44.0 / 45.0), (&k2, -56.0 / 15.0), (&k3, 32.0 / 9.0)],
        )?;
        let k4 = self.derivatives_at(time + h * (4.0 / 5.0), &y4, event_boundary)?;

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
        let k5 = self.derivatives_at(time + h * (8.0 / 9.0), &y5, event_boundary)?;

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
        let k6 = self.derivatives_at(time + h, &y6, event_boundary)?;

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
        let k7 = self.derivatives_at(time + h, &y7, event_boundary)?;
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

        let error_norm = error_norm(state, &y5th, &y4th, &self.state_atol, self.rtol)?;
        Ok(TrialStep {
            y_next: y5th,
            stages: [k1, k2, k3, k4, k5, k6, k7],
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
            let event_boundary = stop.is_event.then_some(stop.time);
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
        stop: &MeEventStop,
        target_t: f64,
    ) -> Result<Option<StepUntilOutcome>, SimError> {
        if matches!(outcome, StepUntilOutcome::RootFound { .. }) {
            return self.apply_events_and_continue_or_finish(self.time, target_t);
        }
        if stop.is_event && time_match_with_tol(self.time, stop.time) {
            return self.apply_scheduled_events_and_continue_or_finish(stop.time, target_t);
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
            let old_roots = self.event_indicators_at(old_t, &old_state, event_boundary)?;
            let h = trial_step_size(
                self.time,
                target_t,
                self.next_step,
                self.kernel.max_step_size(),
            )?;
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
            self.next_step = if self.time > old_t {
                adapt_step(h, trial.error_norm)
            } else {
                rejected_step_size(h, trial.error_norm, target_t)?
            };
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
        let mut projected_next = trial.y_next.clone();
        self.project_accepted_state(new_t, &mut projected_next)?;
        let new_roots = self.event_indicators_at(new_t, &projected_next, context.event_boundary)?;
        let mut crossings = Vec::new();
        self.kernel
            .event_indicator_crossings(context.old_roots, &new_roots, &mut crossings)?;
        if !crossings.is_empty() {
            let dense_output = Dopri5DenseOutput::new(
                old_t,
                h,
                &old_state,
                trial.stages.each_ref().map(Vec::as_slice),
            )?;
            let (mut root, simultaneous_crossings) = self.locate_step_roots(
                &dense_output,
                RootLocalizationInput {
                    old_roots: context.old_roots,
                    new_roots: &new_roots,
                    crossings: &crossings,
                    event_boundary: context.event_boundary,
                },
            )?;
            self.project_accepted_state(root.time, &mut root.state)?;
            let pre_time = timeline::event_left_limit_time(root.time).max(old_t);
            self.project_accepted_state(pre_time, &mut root.pre_state)?;
            if rk_eval_trace_enabled() {
                tracing::debug!(
                    target: "rumoca_solver_rk45::eval",
                    "event root old_t={old_t:.12} new_t={new_t:.12} root_t={:.12} roots={}",
                    root.time,
                    simultaneous_crossings
                        .iter()
                        .map(|crossing| format!(
                            "{}->{:.0}",
                            crossing.index, crossing.post_indicator_value
                        ))
                        .collect::<Vec<_>>()
                        .join(",")
                );
            }
            // Latch `pre(v)` from the located left limit, then hand the
            // component the crossings its relation buffers must record.
            self.kernel.set_time(MeTime::at(root.time))?;
            self.kernel.set_continuous_states(&root.pre_state)?;
            self.kernel.capture_pre_event_state()?;
            self.kernel.arm_state_event(&simultaneous_crossings)?;
            self.time = root.time;
            self.state = root.state;
            self.kernel.set_time(MeTime::at(self.time))?;
            self.kernel.set_continuous_states(&self.state)?;
            self.complete_integrator_step()?;
            return Ok(Some(StepUntilOutcome::RootFound { t_root: root.time }));
        }
        self.time = new_t;
        self.state = projected_next;
        self.kernel.set_time(MeTime::at(self.time))?;
        self.kernel.set_continuous_states(&self.state)?;
        self.complete_integrator_step()?;
        Ok(None)
    }

    fn complete_integrator_step(&mut self) -> Result<(), SimError> {
        if !self
            .kernel
            .model_description()
            .needs_completed_integrator_step
        {
            return Ok(());
        }
        let completed = self.kernel.completed_integrator_step(true)?;
        if completed.enter_event_mode || completed.terminate_simulation {
            return Err(SimError::RuntimeContract {
                reason: format!(
                    "RK45 host cannot yet consume completed-integrator-step outputs: \
                     enter_event_mode={} terminate_simulation={}",
                    completed.enter_event_mode, completed.terminate_simulation
                ),
            });
        }
        Ok(())
    }

    fn locate_step_roots(
        &mut self,
        dense_output: &Dopri5DenseOutput,
        input: RootLocalizationInput<'_>,
    ) -> Result<(LocatedRoot, Vec<MeIndicatorCrossing>), SimError> {
        let mut located = Vec::with_capacity(input.crossings.len());
        for crossing in input.crossings {
            let old_value = root_value_at(input.old_roots, crossing.index, "left dense endpoint")?;
            let new_value = root_value_at(input.new_roots, crossing.index, "right dense endpoint")?;
            let root = self.locate_dense_root(
                dense_output,
                *crossing,
                old_value,
                new_value,
                input.event_boundary,
            )?;
            located.push((*crossing, root));
        }
        let first = located
            .iter()
            .min_by(|(_, lhs), (_, rhs)| lhs.time.total_cmp(&rhs.time))
            .ok_or_else(|| runtime_contract_violation("root localization inventory is empty"))?;
        let event_root = first.1.clone();
        let simultaneous = located
            .iter()
            .filter(|(_, root)| roots_are_simultaneous(&event_root, root))
            .map(|(crossing, _)| *crossing)
            .collect();
        Ok((event_root, simultaneous))
    }

    fn locate_dense_root(
        &mut self,
        dense_output: &Dopri5DenseOutput,
        crossing: MeIndicatorCrossing,
        mut lo_value: f64,
        mut hi_value: f64,
        event_boundary: Option<f64>,
    ) -> Result<LocatedRoot, SimError> {
        let mut lo_t = dense_output.start_time();
        let mut hi_t = dense_output.end_time();
        for _ in 0..ROOT_LOCALIZATION_MAX_ITERS {
            let time_tolerance = root_time_tolerance(lo_t, hi_t, lo_value, hi_value, self.atol);
            if hi_t - lo_t <= time_tolerance {
                return located_dense_root(dense_output, hi_t, time_tolerance);
            }
            let (candidate_t, used_secant) =
                safeguarded_root_candidate(lo_t, hi_t, lo_value, hi_value);
            if candidate_t <= lo_t || candidate_t >= hi_t {
                return located_dense_root(dense_output, hi_t, time_tolerance);
            }
            let candidate_state = dense_output.evaluate(candidate_t)?;
            let candidate_roots =
                self.event_indicators_at(candidate_t, &candidate_state, event_boundary)?;
            let candidate_value =
                root_value_at(&candidate_roots, crossing.index, "dense root candidate")?;
            if used_secant && candidate_value.abs() <= self.atol {
                return located_dense_root(dense_output, candidate_t, time_tolerance);
            }
            if event_indicator_crossed(lo_value, candidate_value, self.atol) {
                hi_t = candidate_t;
                hi_value = candidate_value;
            } else {
                lo_t = candidate_t;
                lo_value = candidate_value;
            }
        }
        let time_tolerance = root_time_tolerance(lo_t, hi_t, lo_value, hi_value, self.atol);
        located_dense_root(dense_output, hi_t, time_tolerance)
    }

    fn next_event_stop(&mut self, target_t: f64) -> Result<MeEventStop, SimError> {
        self.sync_kernel_to_current_point()?;
        Ok(self.kernel.next_event_stop(target_t)?)
    }

    fn apply_events_and_continue_or_finish(
        &mut self,
        event_time: f64,
        target_t: f64,
    ) -> Result<Option<StepUntilOutcome>, SimError> {
        self.sync_kernel_to_current_point()?;
        self.kernel.enter_event_mode(MeEventEntry {
            cause: MeEventCause::StateEvent,
            event_time,
            horizon: target_t,
        })?;
        self.apply_event_mode_and_continue_or_finish()
    }

    fn apply_scheduled_events_and_continue_or_finish(
        &mut self,
        event_time: f64,
        target_t: f64,
    ) -> Result<Option<StepUntilOutcome>, SimError> {
        self.time = event_time.max(self.time);
        self.sync_kernel_to_current_point()?;
        self.kernel.enter_event_mode(MeEventEntry {
            cause: MeEventCause::TimeEvent,
            event_time,
            horizon: target_t,
        })?;
        self.apply_event_mode_and_continue_or_finish()
    }

    /// The FMI 3 discrete-state iteration: call `fmi3UpdateDiscreteStates`
    /// until the component stops asking, then re-read the continuous states
    /// it reports as changed.
    fn apply_event_mode_and_continue_or_finish(
        &mut self,
    ) -> Result<Option<StepUntilOutcome>, SimError> {
        loop {
            let discrete = self.kernel.update_discrete_states()?;
            if discrete.values_of_continuous_states_changed {
                self.kernel.get_continuous_states(&mut self.state)?;
            }
            if let Some(termination) = discrete.terminate_simulation {
                self.termination = Some(termination);
            }
            if !discrete.discrete_states_need_update {
                break;
            }
        }
        if self.termination.is_none() {
            self.kernel.enter_continuous_time_mode()?;
        }
        Ok(Some(
            self.termination
                .as_ref()
                .map_or(StepUntilOutcome::InternalStep, |_| {
                    StepUntilOutcome::Finished
                }),
        ))
    }
}

impl SimulationBackend for Rk45Backend {
    type Error = SimError;

    fn init(&mut self) -> Result<(), Self::Error> {
        self.sync_kernel_to_current_point()?;
        self.kernel.enter_initialization_mode()?;
        self.kernel.exit_initialization_mode()?;
        let discrete = self.kernel.update_discrete_states()?;
        self.kernel.get_continuous_states(&mut self.state)?;
        if let Some(termination) = discrete.terminate_simulation {
            self.termination = Some(termination);
        }
        self.kernel.enter_continuous_time_mode()?;
        Ok(())
    }

    fn step_until(&mut self, stop_time: f64) -> Result<StepUntilOutcome, Self::Error> {
        self.advance_to(stop_time)
    }

    fn read_state(&self) -> BackendState {
        BackendState { t: self.time }
    }
}

fn advance_backend_to(backend: &mut Rk45Backend, target_t: f64) -> Result<(), SimError> {
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

fn time_match_with_tol(a: f64, b: f64) -> bool {
    rumoca_solver::time_match_with_tol(a, b)
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
    absolute_tolerances: &[f64],
    rtol: f64,
) -> Result<f64, SimError> {
    ensure_len(y_high.len(), y.len(), "RK45 high-order estimate length")?;
    ensure_len(y_low.len(), y.len(), "RK45 low-order estimate length")?;
    ensure_len(
        absolute_tolerances.len(),
        y.len(),
        "RK45 absolute-tolerance length",
    )?;
    let mut max_norm = 0.0_f64;
    for (idx, value) in y.iter().enumerate() {
        let high = y_high[idx];
        let low = y_low[idx];
        let scale = absolute_tolerances[idx] + rtol * value.abs().max(high.abs());
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

fn trial_step_size(
    time: f64,
    target_t: f64,
    next_step: f64,
    delay_step_limit: Option<f64>,
) -> Result<f64, SimError> {
    let remaining = target_t - time;
    let proposed = delay_step_limit.map_or(next_step, |limit| next_step.min(limit));
    let h = proposed.min(remaining);
    if !h.is_finite() || h <= 0.0 || time + h == time {
        return Err(SimError::StepSizeUnderflow { target_t });
    }
    if h < MIN_STEP && h < remaining {
        return Err(SimError::StepSizeUnderflow { target_t });
    }
    Ok(h)
}

fn rejected_step_size(h: f64, error_norm: f64, target_t: f64) -> Result<f64, SimError> {
    if h <= MIN_STEP {
        return Err(SimError::StepSizeUnderflow { target_t });
    }
    Ok(adapt_step(h, error_norm))
}

#[cfg(test)]
mod tests;
