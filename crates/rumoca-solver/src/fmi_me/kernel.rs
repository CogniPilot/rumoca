// SPEC_0021 file-size exception: the ME component kernel owns the whole
// FMI 3 ME state machine for the Solve projection — lifecycle, time and
// continuous state, event mode, and batched variable access. split plan: move
// the event-mode boundary handler and the delay/observation machinery into
// focused sibling modules once the diffsol host joins in SPEC_0038 phase 2 and
// the shared surface is settled.

use std::{cell::RefCell, rc::Rc};

use super::{
    MeDiscreteStates, MeError, MeEventCause, MeEventEntry, MeEventStop, MeFmuState,
    MeIndicatorCrossing, MeInstanceConfig, MeModelDescription, MeModelSource, MeObservation,
    MeOutputSeries, MeStage, MeStepCompletion, MeTime, MeValueRef, ModelExchangeKernel,
};
use crate::runtime::event::{
    RuntimeEventBoundary, RuntimeEventBoundaryHandler, process_runtime_event_boundary,
    runtime_event_horizon,
};
use crate::runtime::pre_params::{
    clear_scheduled_root_relation_memory, commit_pre_params_after_event_at,
};
use crate::runtime::schedule::{RuntimeEventStop, SolveStopSchedule};
use crate::runtime::solve_ops::{
    EventActionOutcome, EventPreMode, RootCrossing, convert_variable_meta,
    filter_scheduled_root_crossings, root_crossings_with_relation_memory, runtime_values_changed,
};
use crate::runtime::solve_runtime::{
    AlgebraicLinearization, AlgebraicSettle, EventUpdateRowFilter, InitialEventObservation,
    ProjectedEventUpdateInput, ProjectedInitialEventInput, SolveRuntime,
};
use crate::runtime::time::time_match_with_tol;
use crate::solver::{SimTermination, SimVariableMeta};
use crate::timeline;

/// Residual tolerance for the component's internal algebraic refresh.
const ALGEBRAIC_REFRESH_TOL: f64 = 1.0e-10;
/// Iteration ceiling for the component's internal algebraic/event fixed points.
const UPDATE_MAX_ITERS: usize = 32;

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

/// Lifecycle position in the FMI 3.0 ME state machine.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MeState {
    Instantiated,
    InitializationMode,
    EventMode,
    ContinuousTimeMode,
    Terminated,
}

/// The one projection of a checked `SolveModel` into an FMI 3 ME component.
///
/// Every field below used to live in a backend. They are private here because
/// SPEC_0038 forbids integrators from reaching Solve rows, layouts, opcodes,
/// events, or runtime objects: the only way in is [`ModelExchangeKernel`].
pub struct SolveMeKernel {
    runtime: Rc<SolveRuntime>,
    instance_name: &'static str,
    lifecycle: MeState,

    /// FMI `tolerance`.
    tolerance: f64,
    /// FMI `stopTime`.
    stop_time: f64,

    /// The time `fmi3SetTime` last set.
    time: f64,
    /// The event instant the integrator is stepping toward, if any.
    event_boundary: Option<f64>,
    /// The evaluation time that represents the right limit of the last event.
    post_event_eval_time: Option<f64>,
    /// The component time the right limit above belongs to.
    event_anchor_time: f64,

    states: Vec<f64>,
    params: Vec<f64>,
    state_count: usize,

    stop_schedule: SolveStopSchedule,
    pending_event_entry: Option<MeEventEntry>,
    pending_event_stop: Option<RuntimeEventStop>,
    initial_event_pending: bool,

    pending_root_crossings: Vec<RootCrossing>,
    pending_event_pre_y: Option<Vec<f64>>,
    pending_event_pre_p: Option<Vec<f64>>,
    boundary_event_pre_y: Option<Vec<f64>>,
    boundary_event_pre_p: Option<Vec<f64>>,

    solver_y_guess: RefCell<Vec<f64>>,
    delay_params_scratch: RefCell<Vec<f64>>,
    delay_solver_y_scratch: RefCell<Vec<f64>>,
    derivative_cache: RefCell<Option<CachedDerivative>>,
    root_cache: RefCell<Option<CachedRootConditions>>,

    initial_observations: Vec<MeObservation>,
    delay_step_limit: Option<f64>,
    last_projection_changed: bool,
    termination: Option<SimTermination>,
    output_meta: Vec<SimVariableMeta>,
    /// The settled full solver vector `exit_initialization_mode` produced, so
    /// the initial `update_discrete_states` continues from the same vector
    /// instead of rebuilding one.
    settled_initialization_y: Option<Vec<f64>>,
}

impl SolveMeKernel {
    /// `fmi3InstantiateModelExchange`: project the checked kernel once.
    ///
    /// Rejects a model the component cannot represent before any evaluation,
    /// per SPEC_0038 "Unsupported lifecycle capability fails before execution".
    pub fn instantiate(
        source: MeModelSource<'_>,
        config: &MeInstanceConfig,
    ) -> Result<Self, MeError> {
        // `NoContinuousStates` is a routing answer, not a failure: a host reads
        // it to pick its zero-state path, so it stays unannotated.
        Self::instantiate_inner(source, config).map_err(|error| match error {
            routing @ MeError::NoContinuousStates => routing,
            failure => failure.at_stage(MeStage::Instantiate),
        })
    }

    fn instantiate_inner(
        source: MeModelSource<'_>,
        config: &MeInstanceConfig,
    ) -> Result<Self, MeError> {
        let model = source.model();
        rumoca_eval_solve::reset_solve_row_eval_trace();
        validate_explicit_solve_model(model)?;
        let runtime = Rc::new(SolveRuntime::new(model)?);
        let state_count = runtime.state_count;
        let states = runtime.model.initial_y[..state_count].to_vec();
        let params = runtime.model.parameters.clone();
        let stop_schedule =
            SolveStopSchedule::new(&runtime.model.problem, config.start_time, config.stop_time);
        let output_meta = convert_variable_meta(&runtime.model.variable_meta);
        Ok(Self {
            solver_y_guess: RefCell::new(runtime.model.initial_y.clone()),
            delay_params_scratch: RefCell::new(params.clone()),
            delay_solver_y_scratch: RefCell::new(runtime.model.initial_y.clone()),
            runtime,
            instance_name: config.instance_name,
            lifecycle: MeState::Instantiated,
            tolerance: config.tolerance,
            stop_time: config.stop_time,
            time: config.start_time,
            event_boundary: None,
            post_event_eval_time: None,
            event_anchor_time: config.start_time,
            states,
            params,
            state_count,
            stop_schedule,
            pending_event_entry: None,
            pending_event_stop: None,
            initial_event_pending: false,
            pending_root_crossings: Vec::new(),
            pending_event_pre_y: None,
            pending_event_pre_p: None,
            boundary_event_pre_y: None,
            boundary_event_pre_p: None,
            derivative_cache: RefCell::new(None),
            root_cache: RefCell::new(None),
            initial_observations: Vec::new(),
            delay_step_limit: None,
            last_projection_changed: false,
            termination: None,
            output_meta,
            settled_initialization_y: None,
        })
    }

    // -- internal time model ---------------------------------------------

    /// The evaluation time a variable read at the component's current time
    /// uses: after an event with a right limit, the right limit itself.
    fn public_time_eval_time(&self, time: f64) -> f64 {
        match self.post_event_eval_time {
            Some(eval_time) if time_match_with_tol(time, self.event_anchor_time) => eval_time,
            _ => time,
        }
    }

    /// The evaluation time derivative and event-indicator reads use.
    fn continuous_eval_time(&self) -> f64 {
        match self.event_boundary {
            Some(boundary) if self.time >= boundary => {
                timeline::event_left_probe_time(boundary, self.tolerance)
            }
            _ => self.public_time_eval_time(self.time),
        }
    }

    fn set_post_event_eval_time(&mut self, right_limit: Option<f64>) {
        self.post_event_eval_time = right_limit;
        self.event_anchor_time = self.time;
    }

    // -- internal solver vector ------------------------------------------

    fn current_solver_y(&self) -> Result<Vec<f64>, MeError> {
        self.solver_y_at_time(self.public_time_eval_time(self.time))
    }

    fn solver_y_at_time(&self, time: f64) -> Result<Vec<f64>, MeError> {
        self.with_delay_evaluation_params(time, &self.states, |params| {
            let mut guess = self.solver_y_guess.borrow_mut();
            self.runtime
                .full_solver_y_with_guess(
                    time,
                    &self.states,
                    params,
                    &mut guess,
                    ALGEBRAIC_REFRESH_TOL,
                    UPDATE_MAX_ITERS,
                )
                .map(|()| guess.clone())
                .map_err(MeError::from)
        })?
    }

    fn copy_states_from_solver_y(&mut self, solver_y: &[f64]) {
        for (dst, src) in self.states.iter_mut().zip(solver_y.iter().copied()) {
            *dst = src;
        }
    }

    fn with_delay_evaluation_params<R>(
        &self,
        time: f64,
        state: &[f64],
        f: impl FnOnce(&[f64]) -> R,
    ) -> Result<R, MeError> {
        if !self.runtime.has_delay_channels() {
            return Ok(f(&self.params));
        }
        let mut params = self.delay_params_scratch.borrow_mut();
        params.resize(self.params.len(), 0.0);
        params.copy_from_slice(&self.params);
        let mut solver_y = self.delay_solver_y_scratch.borrow_mut();
        {
            let guess = self.solver_y_guess.borrow();
            solver_y.resize(guess.len(), 0.0);
            solver_y.copy_from_slice(&guess);
        }
        if solver_y.len() < state.len() {
            return Err(contract(format!(
                "delay evaluation solver vector has {} entries for {} state values",
                solver_y.len(),
                state.len()
            )));
        }
        solver_y[..state.len()].copy_from_slice(state);
        self.runtime
            .refresh_delay_values(time, &solver_y, &mut params)?;
        Ok(f(&params))
    }

    fn commit_delay_point(&mut self) -> Result<(), MeError> {
        if !self.runtime.has_delay_channels() {
            return Ok(());
        }
        let mut solver_y = self.solver_y_guess.borrow_mut();
        if solver_y.len() < self.states.len() {
            return Err(contract(format!(
                "delay commit solver vector has {} entries for {} state values",
                solver_y.len(),
                self.states.len()
            )));
        }
        solver_y[..self.states.len()].copy_from_slice(&self.states);
        self.delay_step_limit =
            self.runtime
                .refresh_delay_values(self.time, &solver_y, &mut self.params)?;
        self.runtime.full_solver_y_with_guess(
            self.time,
            &self.states,
            &self.params,
            &mut solver_y,
            ALGEBRAIC_REFRESH_TOL,
            UPDATE_MAX_ITERS,
        )?;
        self.runtime
            .commit_delay_history(self.time, &solver_y, &self.params)?;
        Ok(())
    }

    // -- caches ------------------------------------------------------------

    fn cached_derivative(&self, time: f64, state: &[f64]) -> Option<Vec<f64>> {
        let cache = self.derivative_cache.borrow();
        let cached = cache.as_ref()?;
        if !time_match_with_tol(cached.time, time) || !state_values_match(&cached.state, state) {
            return None;
        }
        Some(cached.derivative.clone())
    }

    fn cache_derivative(&self, time: f64, state: &[f64], derivative: &[f64]) {
        *self.derivative_cache.borrow_mut() = Some(CachedDerivative {
            time,
            state: state.to_vec(),
            derivative: derivative.to_vec(),
        });
    }

    fn clear_derivative_cache(&self) {
        *self.derivative_cache.borrow_mut() = None;
    }

    fn clear_runtime_caches(&self) {
        self.clear_derivative_cache();
        *self.root_cache.borrow_mut() = None;
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

    // -- initialization ----------------------------------------------------

    /// `fmi3EnterInitializationMode`, unannotated; the trait method attaches
    /// [`MeStage::Initialization`].
    fn enter_initialization_mode_inner(&mut self) -> Result<(), MeError> {
        self.runtime.initialize_delay_history(
            self.time,
            &self.runtime.model.initial_y,
            &mut self.params,
        )?;
        self.runtime.set_initial_event_flag(&mut self.params, true);
        self.lifecycle = MeState::InitializationMode;
        Ok(())
    }

    /// `fmi3ExitInitializationMode`, unannotated; the trait method attaches
    /// [`MeStage::Initialization`].
    fn exit_initialization_mode_inner(&mut self) -> Result<(), MeError> {
        let mut solver_y = self.current_solver_y()?;
        self.runtime.settle_initialization_system(
            &mut solver_y,
            &mut self.params,
            self.time,
            self.tolerance,
            UPDATE_MAX_ITERS,
        )?;
        project_algebraics(
            &self.runtime,
            &mut solver_y,
            &self.params,
            self.time,
            self.state_count,
            self.tolerance,
        )?;
        self.copy_states_from_solver_y(&solver_y);
        self.runtime.update_relation_memory_from_state(
            self.time,
            &self.states,
            &mut self.params,
            self.tolerance,
            UPDATE_MAX_ITERS,
        )?;
        // MLS 3.6 §8.6: before integration, v = pre(v). The initial event
        // therefore reads the values the initialization system just settled,
        // never the declared starts that seeded that solve.
        self.pending_event_pre_y = Some(solver_y.clone());
        self.pending_event_pre_p = Some(self.params.clone());
        self.settled_initialization_y = Some(solver_y);
        self.initial_event_pending = true;
        self.lifecycle = MeState::EventMode;
        Ok(())
    }

    // -- continuous time mode ----------------------------------------------

    /// [`ModelExchangeKernel::project_continuous_states`], unannotated; the
    /// trait method attaches [`MeStage::ManifoldProjection`].
    fn project_continuous_states_inner(&mut self, states: &mut [f64]) -> Result<bool, MeError> {
        let time = self.time;
        let mut solver_y = self.runtime.full_solver_y(
            time,
            states,
            &self.params,
            ALGEBRAIC_REFRESH_TOL,
            UPDATE_MAX_ITERS,
        )?;
        let changed = self.runtime.project_state_manifold(
            &mut solver_y,
            &self.params,
            time,
            self.tolerance,
        )?;
        states.copy_from_slice(&solver_y[..self.state_count]);
        self.runtime.full_solver_y_with_guess(
            time,
            states,
            &self.params,
            &mut solver_y,
            ALGEBRAIC_REFRESH_TOL,
            UPDATE_MAX_ITERS,
        )?;
        *self.solver_y_guess.borrow_mut() = solver_y;
        self.last_projection_changed = changed;
        Ok(changed)
    }

    /// [`ModelExchangeKernel::next_event_stop`], unannotated; the trait method
    /// attaches [`MeStage::Integration`].
    fn next_event_stop_inner(&mut self, horizon: f64) -> Result<MeEventStop, MeError> {
        let solver_y = self.current_solver_y()?;
        let (time, event) = self.runtime.next_runtime_event_stop(
            &solver_y,
            &self.params,
            &mut self.stop_schedule,
            self.time,
            horizon,
        )?;
        self.pending_event_stop = event;
        Ok(MeEventStop {
            time,
            is_event: event.is_some(),
        })
    }

    // -- event boundary ----------------------------------------------------

    fn apply_discrete_event_updates(
        &mut self,
        event_time: f64,
        _event: RuntimeEventStop,
    ) -> Result<(), MeError> {
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
        let runtime = Rc::clone(&self.runtime);
        let projection_runtime = Rc::clone(&runtime);
        let state_count = runtime.state_count;
        let tol = self.tolerance;
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
            move |y, p| project_algebraics(&projection_runtime, y, p, event_time, state_count, tol),
        )?;
        self.copy_states_from_solver_y(&solver_y);
        let post_event_y = self.current_solver_y()?;
        commit_pre_params_after_event_at(
            &self.runtime.model,
            &post_event_y,
            &mut self.params,
            Some(event_time),
            self.tolerance,
        );
        self.commit_delay_point()?;
        self.record_event_action_outcome(outcome, event_time)?;
        self.clear_runtime_caches();
        Ok(())
    }

    fn record_event_action_outcome(
        &mut self,
        outcome: EventActionOutcome,
        event_time: f64,
    ) -> Result<(), MeError> {
        match outcome {
            EventActionOutcome::Continue => Ok(()),
            EventActionOutcome::AssertionFailed { time, message } => Err(MeError::Assertion {
                time: if time.is_finite() { time } else { event_time },
                message,
            }),
            EventActionOutcome::Terminated { time, message } => {
                self.termination = Some(SimTermination {
                    time: if time.is_finite() { time } else { event_time },
                    message,
                });
                Ok(())
            }
        }
    }

    fn event_pre_for_update(
        &mut self,
        event_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(Vec<f64>, Vec<f64>), MeError> {
        if let Some(event_pre_y) = self.pending_event_pre_y.take() {
            let event_pre_p = self
                .pending_event_pre_p
                .take()
                .unwrap_or_else(|| self.params.clone());
            return Ok((event_pre_y, event_pre_p));
        }
        let pre_time = match event.pre_mode {
            EventPreMode::EventEntry | EventPreMode::Fixed => {
                timeline::event_left_probe_time(event_time, self.tolerance)
            }
            EventPreMode::FollowCurrent => self.public_time_eval_time(self.time),
        };
        let event_pre_y = self.solver_y_at_time(pre_time)?;
        let event_pre_p = self.params.clone();
        Ok((event_pre_y, event_pre_p))
    }

    fn clear_event_entry_scheduled_root_relation_memory(
        &mut self,
        event_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), MeError> {
        if event.observe_right_limit || !matches!(event.pre_mode, EventPreMode::EventEntry) {
            return Ok(());
        }
        let root_indices = self.scheduled_root_indices_at_time(event_time);
        self.clear_scheduled_root_relation_memory(&root_indices)
    }

    fn clear_all_scheduled_root_relation_memory(&mut self) -> Result<(), MeError> {
        let root_indices = self
            .runtime
            .model
            .problem
            .events
            .scheduled_root_conditions
            .iter()
            .map(|root| root.root_index)
            .collect::<Vec<_>>();
        self.clear_scheduled_root_relation_memory(&root_indices)
    }

    fn clear_scheduled_root_relation_memory(
        &mut self,
        root_indices: &[usize],
    ) -> Result<(), MeError> {
        clear_scheduled_root_relation_memory(&self.runtime.model, root_indices, &mut self.params)
            .map_err(contract)
    }

    fn seed_scheduled_root_relation_overrides(&mut self, event_time: f64, event: RuntimeEventStop) {
        if event.observe_right_limit || !matches!(event.pre_mode, EventPreMode::EventEntry) {
            return;
        }
        for index in self.scheduled_root_indices_at_time(event_time) {
            self.pending_root_crossings.push(RootCrossing {
                index,
                post_relation_memory_value: 1.0,
            });
        }
    }

    fn scheduled_root_indices_at_time(&self, event_time: f64) -> Vec<usize> {
        timeline::scheduled_root_indices_at_time(
            &self.runtime.model.problem.events.scheduled_root_conditions,
            event_time,
        )
    }

    fn run_initial_event_boundary(&mut self) -> Result<MeDiscreteStates, MeError> {
        let mut solver_y = self
            .settled_initialization_y
            .take()
            .ok_or_else(|| contract("initial event boundary requires a settled solver vector"))?;
        let startup_event_pre_y = self
            .pending_event_pre_y
            .take()
            .ok_or_else(|| contract("initial event boundary requires a latched pre-event state"))?;
        let startup_event_pre_p = self
            .pending_event_pre_p
            .take()
            .unwrap_or_else(|| self.params.clone());
        let dynamic_event =
            self.runtime
                .current_dynamic_time_event_stop(&solver_y, &self.params, self.time)?;
        let runtime = Rc::clone(&self.runtime);
        let projection_runtime = Rc::clone(&runtime);
        let state_count = runtime.state_count;
        let tol = self.tolerance;
        let outcome = runtime.apply_projected_initial_event_boundary(
            ProjectedInitialEventInput {
                y: &mut solver_y,
                p: &mut self.params,
                t_start: self.time,
                t_end: self.stop_time,
                tol,
                event_pre_y: &startup_event_pre_y,
                event_pre_p: &startup_event_pre_p,
                max_iters: UPDATE_MAX_ITERS,
                dynamic_event,
                apply_without_initial_event: false,
            },
            move |y, p, t| project_algebraics(&projection_runtime, y, p, t, state_count, tol),
        )?;
        self.copy_states_from_solver_y(&solver_y);
        self.time = outcome.final_t;
        self.initial_observations = outcome
            .observations
            .iter()
            .map(observation_from_initial_event)
            .collect();
        self.record_event_action_outcome(outcome.action, outcome.final_t)?;
        self.initial_event_pending = false;
        Ok(MeDiscreteStates {
            discrete_states_need_update: false,
            terminate_simulation: self.termination.clone(),
            values_of_continuous_states_changed: true,
            time: self.time,
        })
    }

    fn run_runtime_event_boundary(
        &mut self,
        entry: MeEventEntry,
    ) -> Result<MeDiscreteStates, MeError> {
        let tolerance = self.tolerance.max(1.0e-10);
        match entry.cause {
            MeEventCause::StateEvent => {
                process_runtime_event_boundary(
                    RuntimeEventBoundary {
                        event_t: entry.event_time,
                        horizon_t: entry.event_time.min(entry.horizon),
                        tolerance,
                        event: RuntimeEventStop::static_event(EventPreMode::EventEntry),
                    },
                    self,
                )?;
                Ok(MeDiscreteStates {
                    discrete_states_need_update: false,
                    terminate_simulation: self.termination.clone(),
                    values_of_continuous_states_changed: true,
                    time: self.time,
                })
            }
            MeEventCause::TimeEvent => {
                let event = self.pending_event_stop.take().ok_or_else(|| {
                    contract("time event entered without a scheduled component event")
                })?;
                let outcome = process_runtime_event_boundary(
                    RuntimeEventBoundary {
                        event_t: entry.event_time,
                        horizon_t: runtime_event_horizon(event, entry.horizon, self.stop_time),
                        tolerance,
                        event,
                    },
                    self,
                )?;
                self.set_post_event_eval_time(outcome.right_limit_t);
                self.clear_event_entry_scheduled_root_relation_memory(outcome.final_t, event)?;
                self.clear_runtime_caches();
                Ok(MeDiscreteStates {
                    discrete_states_need_update: false,
                    terminate_simulation: self.termination.clone(),
                    values_of_continuous_states_changed: true,
                    time: self.time,
                })
            }
        }
    }
}

impl RuntimeEventBoundaryHandler for SolveMeKernel {
    type Error = MeError;

    fn on_event_time(
        &mut self,
        event_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), Self::Error> {
        self.time = event_time.max(self.time);
        if event.terminal
            && let Some(index) = self
                .runtime
                .model
                .problem
                .solve_layout
                .terminal_event_parameter_index
            && let Some(slot) = self.params.get_mut(index)
        {
            *slot = 1.0;
        }
        let (event_pre_y, event_pre_p) = self.event_pre_for_update(event_time, event)?;
        self.boundary_event_pre_y = Some(event_pre_y.clone());
        self.boundary_event_pre_p = Some(event_pre_p.clone());
        self.pending_event_pre_y = Some(event_pre_y);
        self.pending_event_pre_p = Some(event_pre_p);
        self.seed_scheduled_root_relation_overrides(event_time, event);
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
        self.set_post_event_eval_time(Some(right_time));
        Ok(())
    }
}

impl ModelExchangeKernel for SolveMeKernel {
    fn model_description(&self) -> MeModelDescription<'_> {
        MeModelDescription {
            continuous_state_count: self.state_count,
            event_indicator_count: self.runtime.root_condition_count(),
            output_names: &self.runtime.model.visible_names,
            input_names: self.runtime.model.problem.solve_layout.input_scalar_names(),
            output_meta: &self.output_meta,
        }
    }

    fn get_nominals_of_continuous_states(&self, nominals: &mut [f64]) -> Result<(), MeError> {
        if nominals.len() != self.state_count {
            return Err(contract(format!(
                "nominal buffer has {} entries for {} continuous states",
                nominals.len(),
                self.state_count
            )));
        }
        for (index, slot) in nominals.iter_mut().enumerate() {
            *slot = self.runtime.model.solver_variable_scale(index);
        }
        Ok(())
    }

    fn value_reference(&self, name: &str) -> Option<MeValueRef> {
        self.runtime
            .model
            .problem
            .solve_layout
            .input_parameter_index(name)
            .map(MeValueRef)
    }

    fn enter_initialization_mode(&mut self) -> Result<(), MeError> {
        self.enter_initialization_mode_inner()
            .map_err(|error| error.at_stage(MeStage::Initialization))
    }

    fn exit_initialization_mode(&mut self) -> Result<(), MeError> {
        self.exit_initialization_mode_inner()
            .map_err(|error| error.at_stage(MeStage::Initialization))
    }

    fn enter_event_mode(&mut self, entry: MeEventEntry) -> Result<(), MeError> {
        self.pending_event_entry = Some(entry);
        self.lifecycle = MeState::EventMode;
        Ok(())
    }

    fn update_discrete_states(&mut self) -> Result<MeDiscreteStates, MeError> {
        if self.initial_event_pending {
            return self
                .run_initial_event_boundary()
                .map_err(|error| error.at_stage(MeStage::Initialization));
        }
        let entry = self
            .pending_event_entry
            .take()
            .ok_or_else(|| contract("update_discrete_states called outside event mode"))
            .map_err(|error| error.at_stage(MeStage::EventIteration))?;
        self.run_runtime_event_boundary(entry)
            .map_err(|error| error.at_stage(MeStage::EventIteration))
    }

    fn enter_continuous_time_mode(&mut self) -> Result<(), MeError> {
        self.commit_delay_point()?;
        self.clear_all_scheduled_root_relation_memory()?;
        self.clear_runtime_caches();
        self.lifecycle = MeState::ContinuousTimeMode;
        Ok(())
    }

    fn terminate(&mut self) -> Result<(), MeError> {
        rumoca_eval_solve::trace_solve_row_eval_snapshot(self.instance_name);
        self.lifecycle = MeState::Terminated;
        Ok(())
    }

    fn set_time(&mut self, time: MeTime) {
        self.time = time.time;
        self.event_boundary = time.event_boundary;
    }

    fn set_continuous_states(&mut self, states: &[f64]) -> Result<(), MeError> {
        if states.len() != self.state_count {
            return Err(contract(format!(
                "continuous state buffer has {} entries for {} continuous states",
                states.len(),
                self.state_count
            )));
        }
        self.states.copy_from_slice(states);
        Ok(())
    }

    fn get_continuous_states(&self, states: &mut [f64]) -> Result<(), MeError> {
        if states.len() != self.state_count {
            return Err(contract(format!(
                "continuous state buffer has {} entries for {} continuous states",
                states.len(),
                self.state_count
            )));
        }
        states.copy_from_slice(&self.states);
        Ok(())
    }

    fn get_continuous_state_derivatives(&self, derivatives: &mut Vec<f64>) -> Result<(), MeError> {
        let time = self.continuous_eval_time();
        if let Some(cached) = self.cached_derivative(time, &self.states) {
            *derivatives = cached;
            return Ok(());
        }
        let values = self
            .with_delay_evaluation_params(time, &self.states, |params| {
                let mut guess = self.solver_y_guess.borrow_mut();
                self.runtime
                    .eval_state_derivatives_with_guess(
                        time,
                        &self.states,
                        params,
                        &mut guess,
                        ALGEBRAIC_REFRESH_TOL,
                        UPDATE_MAX_ITERS,
                    )
                    .map_err(MeError::from)
            })
            .map_err(|error| error.at_stage(MeStage::Integration))?
            .map_err(|error| error.at_stage(MeStage::Integration))?;
        *derivatives = values;
        Ok(())
    }

    fn get_directional_derivative(
        &self,
        seed: &[f64],
        sensitivity: &mut [f64],
    ) -> Result<(), MeError> {
        if seed.len() != self.state_count {
            return Err(contract(format!(
                "directional-derivative seed has {} entries for {} continuous states",
                seed.len(),
                self.state_count
            ))
            .at_stage(MeStage::Integration));
        }
        if sensitivity.len() != self.state_count {
            return Err(contract(format!(
                "directional-derivative sensitivity buffer has {} entries for {} state derivatives",
                sensitivity.len(),
                self.state_count
            ))
            .at_stage(MeStage::Integration));
        }
        // The same evaluation time and the same algebraic settle
        // `get_continuous_state_derivatives` uses, so the returned sensitivity
        // is the derivative of exactly the vector that operation reports rather
        // than of a differently-settled one.
        let time = self.continuous_eval_time();
        self.with_delay_evaluation_params(time, &self.states, |params| {
            let mut guess = self.solver_y_guess.borrow_mut();
            self.runtime
                .eval_state_jacobian_v_ad_with_guess_into(
                    AlgebraicLinearization {
                        t: time,
                        params,
                        settle: AlgebraicSettle {
                            tol: ALGEBRAIC_REFRESH_TOL,
                            max_iters: UPDATE_MAX_ITERS,
                        },
                    },
                    &self.states,
                    seed,
                    &mut guess,
                    sensitivity,
                )
                .map_err(MeError::from)
        })
        .map_err(|error| error.at_stage(MeStage::Integration))?
        .map_err(|error| error.at_stage(MeStage::Integration))
    }

    fn get_event_indicators(&self, indicators: &mut Vec<f64>) -> Result<(), MeError> {
        let time = self.continuous_eval_time();
        if let Some(cached) = self.cached_root_conditions(time, &self.states) {
            *indicators = cached;
            return Ok(());
        }
        let values = self
            .with_delay_evaluation_params(time, &self.states, |params| {
                self.runtime
                    .eval_root_conditions(
                        time,
                        &self.states,
                        params,
                        ALGEBRAIC_REFRESH_TOL,
                        UPDATE_MAX_ITERS,
                    )
                    .inspect(|values| {
                        self.cache_root_conditions(time, &self.states, values);
                    })
                    .map_err(MeError::from)
            })
            .map_err(|error| error.at_stage(MeStage::Integration))?
            .map_err(|error| error.at_stage(MeStage::Integration))?;
        *indicators = values;
        Ok(())
    }

    fn project_continuous_states(&mut self, states: &mut [f64]) -> Result<bool, MeError> {
        self.project_continuous_states_inner(states)
            .map_err(|error| error.at_stage(MeStage::ManifoldProjection))
    }

    fn completed_integrator_step(&mut self, step: MeStepCompletion<'_>) -> Result<(), MeError> {
        self.post_event_eval_time = None;
        match step {
            MeStepCompletion::AtStateEvent => self.clear_runtime_caches(),
            MeStepCompletion::Continuous {
                accepted_derivatives,
            } => {
                if self.last_projection_changed {
                    self.clear_derivative_cache();
                } else if let Some(derivatives) = accepted_derivatives {
                    self.cache_derivative(self.time, &self.states, derivatives);
                }
            }
        }
        self.commit_delay_point()
            .map_err(|error| error.at_stage(MeStage::Integration))
    }

    fn max_step_size(&self) -> Option<f64> {
        self.delay_step_limit
    }

    fn next_event_stop(&mut self, horizon: f64) -> Result<MeEventStop, MeError> {
        self.next_event_stop_inner(horizon)
            .map_err(|error| error.at_stage(MeStage::Integration))
    }

    fn event_indicator_crossings(
        &self,
        before: &[f64],
        after: &[f64],
        crossings: &mut Vec<MeIndicatorCrossing>,
    ) -> Result<(), MeError> {
        let mut located = root_crossings_with_relation_memory(
            before,
            after,
            self.tolerance,
            &self
                .runtime
                .model
                .problem
                .events
                .root_relation_memory_targets,
            &self.params,
        );
        filter_scheduled_root_crossings(
            &mut located,
            &self.runtime.model.problem.events.scheduled_root_conditions,
        );
        crossings.clear();
        crossings.extend(located.into_iter().map(|crossing| MeIndicatorCrossing {
            index: crossing.index,
            post_indicator_value: crossing.post_relation_memory_value,
        }));
        Ok(())
    }

    fn capture_pre_event_state(&mut self) -> Result<(), MeError> {
        let pre_y = self.runtime.full_solver_y(
            self.time,
            &self.states,
            &self.params,
            ALGEBRAIC_REFRESH_TOL,
            UPDATE_MAX_ITERS,
        )?;
        self.pending_event_pre_y = Some(pre_y);
        self.pending_event_pre_p = Some(self.params.clone());
        Ok(())
    }

    fn arm_state_event(&mut self, crossings: &[MeIndicatorCrossing]) -> Result<(), MeError> {
        self.pending_root_crossings.clear();
        self.pending_root_crossings
            .extend(crossings.iter().map(|crossing| RootCrossing {
                index: crossing.index,
                post_relation_memory_value: crossing.post_indicator_value,
            }));
        Ok(())
    }

    fn observe(&self) -> Result<MeObservation, MeError> {
        Ok(MeObservation {
            time: self.time,
            solver_y: self.current_solver_y()?,
            parameters: self.params.clone(),
        })
    }

    fn record_outputs(
        &self,
        observation: &MeObservation,
        sample_time: f64,
        series: &mut MeOutputSeries,
    ) -> Result<(), MeError> {
        self.runtime
            .record_visible_sample(
                series.columns_mut(),
                &observation.solver_y,
                &observation.parameters,
                sample_time,
            )
            .map_err(MeError::from)
    }

    fn get_outputs(
        &self,
        observation: &MeObservation,
        sample_time: f64,
        values: &mut Vec<f64>,
    ) -> Result<(), MeError> {
        *values = self.runtime.visible_values(
            &observation.solver_y,
            &observation.parameters,
            sample_time,
        )?;
        Ok(())
    }

    fn initial_observations(&self) -> &[MeObservation] {
        &self.initial_observations
    }

    fn set_float64(&mut self, refs: &[MeValueRef], values: &[f64]) -> Result<(), MeError> {
        if refs.len() != values.len() {
            return Err(contract(format!(
                "{} value references do not match {} values",
                refs.len(),
                values.len()
            )));
        }
        for (reference, value) in refs.iter().zip(values.iter().copied()) {
            if let Some(slot) = self.params.get_mut(reference.0) {
                *slot = value;
            }
        }
        self.clear_runtime_caches();
        self.commit_delay_point()
    }

    fn fmu_state(&self) -> MeFmuState {
        MeFmuState {
            parameters: self.params.clone(),
        }
    }

    fn reset_to_fmu_state(
        &mut self,
        saved: &MeFmuState,
        start_time: f64,
        states: &[f64],
    ) -> Result<(), MeError> {
        self.time = start_time;
        self.event_boundary = None;
        self.set_continuous_states(states)?;
        self.params.clone_from(&saved.parameters);
        self.stop_schedule =
            SolveStopSchedule::new(&self.runtime.model.problem, start_time, self.stop_time);
        self.termination = None;
        self.pending_root_crossings.clear();
        self.pending_event_pre_y = None;
        self.pending_event_pre_p = None;
        self.boundary_event_pre_y = None;
        self.boundary_event_pre_p = None;
        self.post_event_eval_time = None;
        self.event_anchor_time = start_time;
        self.pending_event_entry = None;
        self.pending_event_stop = None;
        self.solver_y_guess
            .borrow_mut()
            .clone_from(&self.runtime.model.initial_y);
        self.clear_runtime_caches();
        self.runtime.reset_delay_history();
        self.commit_delay_point()
    }

    fn extend_stop_time(&mut self, from_time: f64, stop_time: f64) {
        self.stop_time = stop_time;
        self.stop_schedule =
            SolveStopSchedule::new(&self.runtime.model.problem, from_time, stop_time);
    }
}

fn observation_from_initial_event(observation: &InitialEventObservation) -> MeObservation {
    MeObservation {
        time: observation.t,
        solver_y: observation.y.clone(),
        parameters: observation.p.clone(),
    }
}

fn contract(reason: impl Into<String>) -> MeError {
    MeError::Contract {
        reason: reason.into(),
    }
}

fn state_values_match(a: &[f64], b: &[f64]) -> bool {
    a.len() == b.len()
        && a.iter()
            .zip(b)
            .all(|(lhs, rhs)| lhs.to_bits() == rhs.to_bits())
}

fn project_algebraics(
    runtime: &SolveRuntime,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    state_count: usize,
    tol: f64,
) -> Result<bool, crate::runtime::solve_ops::RuntimeSolveError> {
    let before = y.to_vec();
    runtime.project_state_manifold(y, p, t, tol)?;
    let state = y[..state_count.min(y.len())].to_vec();
    let refreshed = runtime.full_solver_y(t, &state, p, ALGEBRAIC_REFRESH_TOL, UPDATE_MAX_ITERS)?;
    y.copy_from_slice(&refreshed);
    Ok(runtime_values_changed(&before, y, tol))
}

/// SPEC_0038 "Unsupported lifecycle capability fails before execution".
fn validate_explicit_solve_model(model: &rumoca_ir_solve::SolveModel) -> Result<(), MeError> {
    let layout = &model.problem.solve_layout;
    if layout.state_scalar_count == 0 {
        return Err(MeError::NoContinuousStates);
    }
    if model.initial_y.len() != model.solver_scalar_count() {
        return Err(MeError::Evaluation {
            message: format!(
                "initial vector length {} does not match solver layout {}",
                model.initial_y.len(),
                model.solver_scalar_count()
            ),
        });
    }
    let derivative_rhs_len = model
        .problem
        .continuous
        .derivative_rhs
        .len()
        .map_err(|err| MeError::Evaluation {
            message: err.to_string(),
        })?;
    if derivative_rhs_len != layout.state_scalar_count {
        return Err(MeError::Evaluation {
            message: format!(
                "derivative RHS has {} rows for {} states",
                derivative_rhs_len, layout.state_scalar_count
            ),
        });
    }
    Ok(())
}
