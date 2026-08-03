// SPEC_0021 file-size exception: the ME component kernel owns the whole
// FMI 3 ME state machine for the Solve projection — lifecycle, time and
// continuous state, event mode, and batched variable access. split plan: move
// the event-mode boundary handler and the delay/observation machinery into
// focused sibling modules once the diffsol host joins in SPEC_0038 phase 2 and
// the shared surface is settled.

use std::{cell::RefCell, rc::Rc};

use super::lifecycle::{MeLifecycle, MeLifecycleCommand, MeLifecycleViolation, MeState};
use super::{
    MeCompletedIntegratorStep, MeDiscreteStates, MeError, MeEventCause, MeEventEntry, MeEventStop,
    MeFmuState, MeIndicatorCrossing, MeInstanceConfig, MeModelDescription, MeModelSource,
    MeNumericsProfile, MeObservation, MeOutputSeries, MeRootProfile, MeStage, MeTime, MeValueRef,
    ModelExchangeKernel,
};
use crate::runtime::event::{
    RuntimeEventBoundary, RuntimeEventBoundaryHandler, advance_state_across_event_right_limit,
    process_runtime_event_boundary, runtime_event_horizon, runtime_root_event_application_time,
};
use crate::runtime::pre_params::{
    clear_scheduled_root_relation_memory, commit_pre_params_after_event_at,
};
use crate::runtime::schedule::{RuntimeEventStop, ScheduledEventConsumption, SolveStopSchedule};
use crate::runtime::solve_ops::{
    EventActionOutcome, EventPreMode, RootCrossing, convert_variable_meta,
    filter_scheduled_root_crossings, root_crossings_with_relation_memory, runtime_values_changed,
};
use crate::runtime::solve_runtime::{
    AlgebraicLinearization, AlgebraicSettle, EventUpdateRowFilter, InitialEventObservation,
    ProjectedEventUpdateInput, ProjectedInitialEventInput, SolveRuntime, SolveRuntimeSnapshot,
};
use crate::runtime::time::time_match_with_tol;
use crate::solver::{SimTermination, SimVariableMeta};
use crate::timeline;

/// Residual tolerance for the component's internal algebraic refresh.
const ALGEBRAIC_REFRESH_TOL: f64 = 1.0e-10;
/// Iteration ceiling for the component's internal algebraic/event fixed points.
const UPDATE_MAX_ITERS: usize = 32;

#[derive(Clone)]
struct CachedDerivative {
    time: f64,
    state: Vec<f64>,
    derivative: Vec<f64>,
}

#[derive(Clone)]
struct CachedRootConditions {
    time: f64,
    state: Vec<f64>,
    values: Vec<f64>,
}

#[derive(Clone, Copy)]
struct MeAlgebraicProjectionPolicy {
    state_count: usize,
    tolerance: f64,
    profile: MeNumericsProfile,
    settle: AlgebraicSettle,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StateTimeCoincidence {
    None,
    Unconsumed,
    Consumed,
}

impl StateTimeCoincidence {
    fn is_some(self) -> bool {
        !matches!(self, Self::None)
    }

    fn is_consumed(self) -> bool {
        matches!(self, Self::Consumed)
    }
}

/// The one projection of a checked `SolveModel` into an FMI 3 ME component.
///
/// Every field below used to live in a backend. They are private here because
/// SPEC_0038 forbids integrators from reaching Solve rows, layouts, opcodes,
/// events, or runtime objects: the only way in is [`ModelExchangeKernel`].
pub struct SolveMeKernel {
    runtime: Rc<SolveRuntime>,
    instance_brand: Rc<()>,
    instance_name: &'static str,
    lifecycle: MeLifecycle,

    /// FMI `tolerance`.
    tolerance: f64,
    /// FMI `stopTime`.
    stop_time: f64,
    /// Temporary phase-2 compatibility profile selected by the owning host.
    root_profile: MeRootProfile,
    /// Temporary phase-2 callback-numerics profile selected by the host.
    numerics_profile: MeNumericsProfile,

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
    last_event_entry: Option<MeEventEntry>,
    pending_event_stop: Option<(f64, RuntimeEventStop)>,
    advance_state_to_event_right_limit: bool,
    state_time_coincidence: StateTimeCoincidence,
    initial_event_pending: bool,
    skip_next_enter_continuous_delay_commit: bool,

    pending_root_crossings: Vec<RootCrossing>,
    pending_event_pre_y: Option<Vec<f64>>,
    pending_event_pre_p: Option<Vec<f64>>,
    boundary_event_pre_y: Option<Vec<f64>>,
    boundary_event_pre_p: Option<Vec<f64>>,
    frozen_event_accepted_seed: Option<Vec<f64>>,

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

/// Complete continuation state captured by `fmi3GetFMUState`.
///
/// This stays opaque outside the component implementation so a host cannot
/// synthesize a state that bypasses lifecycle or buffer invariants.
#[derive(Clone)]
pub(crate) struct MeKernelSnapshot {
    lifecycle: MeState,
    stop_time: f64,
    time: f64,
    event_boundary: Option<f64>,
    post_event_eval_time: Option<f64>,
    event_anchor_time: f64,
    states: Vec<f64>,
    params: Vec<f64>,
    stop_schedule: SolveStopSchedule,
    pending_event_entry: Option<MeEventEntry>,
    last_event_entry: Option<MeEventEntry>,
    pending_event_stop: Option<(f64, RuntimeEventStop)>,
    advance_state_to_event_right_limit: bool,
    state_time_coincidence: StateTimeCoincidence,
    initial_event_pending: bool,
    skip_next_enter_continuous_delay_commit: bool,
    pending_root_crossings: Vec<RootCrossing>,
    pending_event_pre_y: Option<Vec<f64>>,
    pending_event_pre_p: Option<Vec<f64>>,
    boundary_event_pre_y: Option<Vec<f64>>,
    boundary_event_pre_p: Option<Vec<f64>>,
    frozen_event_accepted_seed: Option<Vec<f64>>,
    solver_y_guess: Vec<f64>,
    delay_params_scratch: Vec<f64>,
    delay_solver_y_scratch: Vec<f64>,
    derivative_cache: Option<CachedDerivative>,
    root_cache: Option<CachedRootConditions>,
    initial_observations: Vec<MeObservation>,
    delay_step_limit: Option<f64>,
    last_projection_changed: bool,
    termination: Option<SimTermination>,
    settled_initialization_y: Option<Vec<f64>>,
    runtime: SolveRuntimeSnapshot,
}

impl SolveMeKernel {
    #[cfg(any(test, kani))]
    pub(crate) fn verification_observable_state(&self) -> (MeState, u64, Vec<u64>, Vec<u64>) {
        (
            self.lifecycle.state(),
            self.time.to_bits(),
            self.states.iter().map(|value| value.to_bits()).collect(),
            self.params.iter().map(|value| value.to_bits()).collect(),
        )
    }

    #[cfg(any(test, kani))]
    pub(crate) fn verification_matches_snapshot(&self, saved: &MeFmuState) -> bool {
        if !Rc::ptr_eq(&saved.instance_brand, &self.instance_brand) {
            return false;
        }
        let state = &saved.component;
        self.lifecycle.state() == state.lifecycle
            && self.stop_time.to_bits() == state.stop_time.to_bits()
            && self.time.to_bits() == state.time.to_bits()
            && option_float_bit_eq(self.event_boundary, state.event_boundary)
            && option_float_bit_eq(self.post_event_eval_time, state.post_event_eval_time)
            && self.event_anchor_time.to_bits() == state.event_anchor_time.to_bits()
            && float_slice_bit_eq(&self.states, &state.states)
            && float_slice_bit_eq(&self.params, &state.params)
            && self.stop_schedule.bit_eq(&state.stop_schedule)
            && option_event_entry_bit_eq(self.pending_event_entry, state.pending_event_entry)
            && option_event_entry_bit_eq(self.last_event_entry, state.last_event_entry)
            && option_event_stop_bit_eq(self.pending_event_stop, state.pending_event_stop)
            && self.advance_state_to_event_right_limit == state.advance_state_to_event_right_limit
            && self.state_time_coincidence == state.state_time_coincidence
            && self.initial_event_pending == state.initial_event_pending
            && self.skip_next_enter_continuous_delay_commit
                == state.skip_next_enter_continuous_delay_commit
            && root_crossings_bit_eq(&self.pending_root_crossings, &state.pending_root_crossings)
            && option_float_vec_bit_eq(&self.pending_event_pre_y, &state.pending_event_pre_y)
            && option_float_vec_bit_eq(&self.pending_event_pre_p, &state.pending_event_pre_p)
            && option_float_vec_bit_eq(&self.boundary_event_pre_y, &state.boundary_event_pre_y)
            && option_float_vec_bit_eq(&self.boundary_event_pre_p, &state.boundary_event_pre_p)
            && option_float_vec_bit_eq(
                &self.frozen_event_accepted_seed,
                &state.frozen_event_accepted_seed,
            )
            && float_slice_bit_eq(&self.solver_y_guess.borrow(), &state.solver_y_guess)
            && float_slice_bit_eq(
                &self.delay_params_scratch.borrow(),
                &state.delay_params_scratch,
            )
            && float_slice_bit_eq(
                &self.delay_solver_y_scratch.borrow(),
                &state.delay_solver_y_scratch,
            )
            && derivative_cache_bit_eq(
                self.derivative_cache.borrow().as_ref(),
                state.derivative_cache.as_ref(),
            )
            && root_cache_bit_eq(self.root_cache.borrow().as_ref(), state.root_cache.as_ref())
            && observations_bit_eq(&self.initial_observations, &state.initial_observations)
            && option_float_bit_eq(self.delay_step_limit, state.delay_step_limit)
            && self.last_projection_changed == state.last_projection_changed
            && termination_bit_eq(self.termination.as_ref(), state.termination.as_ref())
            && option_float_vec_bit_eq(
                &self.settled_initialization_y,
                &state.settled_initialization_y,
            )
            && self.runtime.matches_snapshot(&state.runtime)
    }

    fn require_lifecycle_transition(&self, command: MeLifecycleCommand) -> Result<(), MeError> {
        self.lifecycle
            .next(command)
            .map(|_| ())
            .map_err(lifecycle_contract)
    }

    fn commit_lifecycle_transition(&mut self, command: MeLifecycleCommand) -> Result<(), MeError> {
        self.lifecycle
            .transition(command)
            .map_err(lifecycle_contract)
    }

    fn require_active_lifecycle(&self, operation: &'static str) -> Result<(), MeError> {
        if self.lifecycle.is_terminated() {
            return Err(contract(format!(
                "{operation} called after the component was terminated"
            )));
        }
        Ok(())
    }

    fn require_observation_brand(&self, observation: &MeObservation) -> Result<(), MeError> {
        if !Rc::ptr_eq(&observation.instance_brand, &self.instance_brand) {
            return Err(contract("observation belongs to a different ME instance"));
        }
        Ok(())
    }

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

    /// Temporary phase-2 dual-run guard for the frozen Diffsol host.
    ///
    /// The frozen Diffsol driver still owns a full Solve vector during this
    /// migration step. Compare it inside the component so the adapter does not
    /// gain access to component-private algebraic storage. Delete this
    /// operation with [`MeNumericsProfile::DiffsolFrozen`].
    pub fn verify_frozen_compatibility_state(
        &self,
        expected_solver_y: &[f64],
        expected_parameters: &[f64],
        stage: MeStage,
    ) -> Result<(), MeError> {
        if !matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return Err(contract(
                "frozen compatibility state verification requires DiffsolFrozen numerics",
            )
            .at_stage(stage));
        }
        let actual_solver_y = self.solver_y_guess.borrow().clone();
        let state_slots: Vec<_> = (0..self.state_count).collect();
        if actual_solver_y.len() == expected_solver_y.len()
            && first_bit_mismatch_except(&actual_solver_y, expected_solver_y, &state_slots)
                .is_none()
            && self.frozen_parameters_match(expected_parameters)
        {
            return Ok(());
        }
        let solver_mismatch =
            first_bit_mismatch_except(&actual_solver_y, expected_solver_y, &state_slots);
        let parameter_mismatch = first_bit_mismatch_except(
            &self.params,
            expected_parameters,
            &self
                .runtime
                .model
                .problem
                .events
                .delays
                .value_parameter_indices,
        );
        let solver_name = solver_mismatch.and_then(|(index, _, _)| {
            self.runtime
                .model
                .problem
                .solve_layout
                .solver_maps
                .names
                .get(index)
        });
        Err(contract(format!(
            "frozen compatibility state diverged at {stage:?}: component_time={} \
             last_event={:?} component_solver_y={} \
             expected_solver_y={} component_parameters={} expected_parameters={} \
             solver_mismatch={solver_mismatch:?} solver_name={solver_name:?} \
             parameter_mismatch={parameter_mismatch:?}",
            self.time,
            self.last_event_entry,
            actual_solver_y.len(),
            expected_solver_y.len(),
            self.params.len(),
            expected_parameters.len(),
        ))
        .at_stage(stage))
    }

    fn frozen_parameters_match(&self, expected: &[f64]) -> bool {
        self.params.len() == expected.len()
            && self
                .params
                .iter()
                .zip(expected)
                .enumerate()
                .all(|(index, (actual, expected))| {
                    self.runtime
                        .model
                        .problem
                        .events
                        .delays
                        .value_parameter_indices
                        .contains(&index)
                        || actual.to_bits() == expected.to_bits()
                })
    }

    fn instantiate_inner(
        source: MeModelSource<'_>,
        config: &MeInstanceConfig,
    ) -> Result<Self, MeError> {
        validate_instance_config(config)?;
        let model = source.model();
        rumoca_eval_solve::reset_solve_row_eval_trace();
        validate_explicit_solve_model(model)?;
        let model = model
            .resolved_periodic_schedules_at(config.start_time)
            .map_err(|error| {
                contract(format!(
                    "periodic schedule cannot be anchored at FMI startTime: {error}"
                ))
            })?;
        let runtime = Rc::new(SolveRuntime::new(&model)?);
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
            instance_brand: Rc::new(()),
            instance_name: config.instance_name,
            lifecycle: MeLifecycle::instantiated(),
            tolerance: config.tolerance,
            stop_time: config.stop_time,
            root_profile: config.root_profile,
            numerics_profile: config.numerics_profile,
            time: config.start_time,
            event_boundary: None,
            post_event_eval_time: None,
            event_anchor_time: config.start_time,
            states,
            params,
            state_count,
            stop_schedule,
            pending_event_entry: None,
            last_event_entry: None,
            pending_event_stop: None,
            advance_state_to_event_right_limit: false,
            state_time_coincidence: StateTimeCoincidence::None,
            initial_event_pending: false,
            skip_next_enter_continuous_delay_commit: false,
            pending_root_crossings: Vec::new(),
            pending_event_pre_y: None,
            pending_event_pre_p: None,
            boundary_event_pre_y: None,
            boundary_event_pre_p: None,
            frozen_event_accepted_seed: None,
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
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return self.time;
        }
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

    fn numerics_settle(&self) -> AlgebraicSettle {
        match self.numerics_profile {
            MeNumericsProfile::Component => AlgebraicSettle {
                tol: ALGEBRAIC_REFRESH_TOL,
                max_iters: UPDATE_MAX_ITERS,
            },
            MeNumericsProfile::DiffsolFrozen => AlgebraicSettle {
                tol: self.tolerance.max(1.0e-10),
                max_iters: 256,
            },
        }
    }

    fn algebraic_projection_policy(&self) -> MeAlgebraicProjectionPolicy {
        MeAlgebraicProjectionPolicy {
            state_count: self.state_count,
            tolerance: self.tolerance,
            profile: self.numerics_profile,
            settle: self.numerics_settle(),
        }
    }

    fn initialization_solver_y(&self) -> Result<Vec<f64>, MeError> {
        match self.numerics_profile {
            MeNumericsProfile::Component => self.current_solver_y(),
            // The frozen Diffsol initialization starts from the declared
            // full-layout seed exactly as the retired driver did.  A
            // preliminary full refresh would prime runtime-owned evaluation
            // state in a different order even when its returned vector is
            // later overwritten by the initialization solve.
            MeNumericsProfile::DiffsolFrozen => Ok(self.solver_y_guess.borrow().clone()),
        }
    }

    fn with_callback_solver_y<R>(&self, f: impl FnOnce(&mut Vec<f64>) -> R) -> R {
        match self.numerics_profile {
            MeNumericsProfile::Component => f(&mut self.solver_y_guess.borrow_mut()),
            MeNumericsProfile::DiffsolFrozen => {
                let mut speculative = self.solver_y_guess.borrow().clone();
                f(&mut speculative)
            }
        }
    }

    // -- internal solver vector ------------------------------------------

    fn current_solver_y(&self) -> Result<Vec<f64>, MeError> {
        self.solver_y_at_time(self.public_time_eval_time(self.time))
    }

    fn solver_y_at_time(&self, time: f64) -> Result<Vec<f64>, MeError> {
        let settle = self.numerics_settle();
        self.with_delay_evaluation_params(time, &self.states, |params| {
            self.with_callback_solver_y(|guess| {
                self.runtime
                    .full_solver_y_with_guess(
                        time,
                        &self.states,
                        params,
                        guess,
                        settle.tol,
                        settle.max_iters,
                    )
                    .map(|()| guess.clone())
                    .map_err(MeError::from)
            })
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
        let settle = self.numerics_settle();
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
            settle.tol,
            settle.max_iters,
        )?;
        self.runtime
            .commit_delay_history(self.time, &solver_y, &self.params)?;
        Ok(())
    }

    // -- caches ------------------------------------------------------------

    fn cached_derivative(&self, time: f64, state: &[f64]) -> Option<Vec<f64>> {
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return None;
        }
        let cache = self.derivative_cache.borrow();
        let cached = cache.as_ref()?;
        if !time_match_with_tol(cached.time, time) || !state_values_match(&cached.state, state) {
            return None;
        }
        Some(cached.derivative.clone())
    }

    fn cache_derivative(&self, time: f64, state: &[f64], derivative: &[f64]) {
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return;
        }
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
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return None;
        }
        let cache = self.root_cache.borrow();
        let cached = cache.as_ref()?;
        if !time_match_with_tol(cached.time, time) || !state_values_match(&cached.state, state) {
            return None;
        }
        Some(cached.values.clone())
    }

    fn cache_root_conditions(&self, time: f64, state: &[f64], values: &[f64]) {
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return;
        }
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
        Ok(())
    }

    /// `fmi3ExitInitializationMode`, unannotated; the trait method attaches
    /// [`MeStage::Initialization`].
    fn exit_initialization_mode_inner(&mut self) -> Result<(), MeError> {
        let mut solver_y = self.initialization_solver_y()?;
        let policy = self.algebraic_projection_policy();
        let settle = policy.settle;
        match self.numerics_profile {
            MeNumericsProfile::Component => {
                self.runtime.settle_initialization_system(
                    &mut solver_y,
                    &mut self.params,
                    self.time,
                    self.tolerance,
                    settle.max_iters,
                )?;
                project_algebraics(
                    &self.runtime,
                    &mut solver_y,
                    &mut self.params,
                    self.time,
                    policy,
                )?;
                self.copy_states_from_solver_y(&solver_y);
                self.runtime.update_relation_memory_from_state(
                    self.time,
                    &self.states,
                    &mut self.params,
                    self.tolerance,
                    settle.max_iters,
                )?;
            }
            MeNumericsProfile::DiffsolFrozen => {
                self.runtime.seed_initial_discrete_values(
                    &mut solver_y,
                    &mut self.params,
                    self.time,
                    self.tolerance,
                    settle.max_iters,
                )?;
                self.runtime
                    .settle_runtime_assignments_and_relation_memory(
                        &mut solver_y,
                        &mut self.params,
                        self.time,
                        self.tolerance,
                        settle.max_iters,
                    )?;
                self.runtime.settle_initialization_system(
                    &mut solver_y,
                    &mut self.params,
                    self.time,
                    self.tolerance,
                    settle.max_iters,
                )?;
                self.runtime.seed_initial_discrete_values(
                    &mut solver_y,
                    &mut self.params,
                    self.time,
                    self.tolerance,
                    settle.max_iters,
                )?;
                self.runtime.settle_initialization_system(
                    &mut solver_y,
                    &mut self.params,
                    self.time,
                    self.tolerance,
                    settle.max_iters,
                )?;
                let runtime = Rc::clone(&self.runtime);
                let projection_runtime = Rc::clone(&runtime);
                let tol = policy.tolerance;
                let time = self.time;
                runtime.settle_projected_runtime_and_relation_memory(
                    &mut solver_y,
                    &mut self.params,
                    time,
                    tol,
                    settle.max_iters,
                    move |y, p| project_algebraics(&projection_runtime, y, p, time, policy),
                )?;
            }
        }
        self.copy_states_from_solver_y(&solver_y);
        *self.solver_y_guess.borrow_mut() = solver_y.clone();
        // MLS 3.6 §8.6: before integration, v = pre(v). The initial event
        // therefore reads the values the initialization system just settled,
        // never the declared starts that seeded that solve.
        self.pending_event_pre_y = Some(solver_y.clone());
        self.pending_event_pre_p = Some(self.params.clone());
        self.settled_initialization_y = Some(solver_y);
        self.initial_event_pending = true;
        Ok(())
    }

    // -- continuous time mode ----------------------------------------------

    /// [`ModelExchangeKernel::project_continuous_states`], unannotated; the
    /// trait method attaches [`MeStage::ManifoldProjection`].
    fn project_continuous_states_inner(&mut self, states: &mut [f64]) -> Result<bool, MeError> {
        let time = self.time;
        let settle = self.numerics_settle();
        let (mut solver_y, accepted_guess) = match self.numerics_profile {
            MeNumericsProfile::Component => (
                self.runtime.full_solver_y(
                    time,
                    states,
                    &self.params,
                    settle.tol,
                    settle.max_iters,
                )?,
                None,
            ),
            MeNumericsProfile::DiffsolFrozen => {
                let accepted_guess = self.solver_y_guess.borrow().clone();
                let mut projection_guess = accepted_guess.clone();
                self.runtime.full_solver_y_with_guess(
                    time,
                    states,
                    &self.params,
                    &mut projection_guess,
                    settle.tol,
                    settle.max_iters,
                )?;
                (projection_guess, Some(accepted_guess))
            }
        };
        let changed = self.runtime.project_state_manifold(
            &mut solver_y,
            &self.params,
            time,
            self.tolerance,
        )?;
        states.copy_from_slice(&solver_y[..self.state_count]);
        let mut committed_guess = accepted_guess.unwrap_or(solver_y);
        self.runtime.full_solver_y_with_guess(
            time,
            states,
            &self.params,
            &mut committed_guess,
            settle.tol,
            settle.max_iters,
        )?;
        *self.solver_y_guess.borrow_mut() = committed_guess;
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
        self.pending_event_stop = event.map(|event| (time, event));
        Ok(MeEventStop {
            time,
            is_event: event.is_some(),
        })
    }

    pub fn has_scheduled_event_at(&self, time: f64) -> bool {
        self.stop_schedule
            .scheduled_event_coincidence_at(time)
            .is_some()
            || self
                .pending_event_stop
                .is_some_and(|(event_time, _)| time_match_with_tol(event_time, time))
    }

    pub fn frozen_event_state_derivatives(
        &self,
        time: f64,
        states: &[f64],
    ) -> Result<Vec<f64>, MeError> {
        if !matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return Err(contract(
                "frozen event derivative evaluation requires DiffsolFrozen numerics",
            )
            .at_stage(MeStage::EventIteration));
        }
        self.runtime
            .eval_state_derivatives(time, states, &self.params, self.tolerance.max(1.0e-10), 256)
            .map_err(MeError::from)
            .map_err(|error| error.at_stage(MeStage::EventIteration))
    }

    /// Freeze the retired driver's full-vector ownership at a located root.
    ///
    /// The driver reconstructs every solver lane at the located state, then
    /// brackets the event by changing only the continuous-state prefix.  In
    /// particular, algebraic lanes in the left-limit snapshot still belong to
    /// the located root rather than to a fresh solve at the extrapolated left
    /// state.  This temporary phase-2 bridge preserves that exact ownership.
    pub fn capture_frozen_located_event_pre(&mut self, pre_states: &[f64]) -> Result<(), MeError> {
        if !matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return Err(
                contract("frozen located-event capture requires DiffsolFrozen numerics")
                    .at_stage(MeStage::EventIteration),
            );
        }
        if pre_states.len() != self.state_count {
            return Err(contract(format!(
                "frozen located-event pre-state has {} entries for {} continuous states",
                pre_states.len(),
                self.state_count
            ))
            .at_stage(MeStage::EventIteration));
        }
        let mut event_pre_y = self.solver_y_at_time(self.time)?;
        event_pre_y[..self.state_count].copy_from_slice(pre_states);
        self.pending_event_pre_y = Some(event_pre_y);
        self.pending_event_pre_p = Some(self.params.clone());
        Ok(())
    }

    pub fn prepare_frozen_bdf_initial_seed(
        &mut self,
        frozen_solver_y: &[f64],
    ) -> Result<(), MeError> {
        if !matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return Err(contract(
                "frozen BDF seed preparation requires DiffsolFrozen numerics",
            ));
        }
        if frozen_solver_y.len() != self.runtime.solver_count {
            return Err(contract(format!(
                "frozen BDF seed has {} entries for {} solver values",
                frozen_solver_y.len(),
                self.runtime.solver_count
            )));
        }
        self.solver_y_guess
            .borrow_mut()
            .copy_from_slice(frozen_solver_y);
        Ok(())
    }

    // -- event boundary ----------------------------------------------------

    fn apply_discrete_event_updates(
        &mut self,
        event_time: f64,
        _event: RuntimeEventStop,
        row_filter: EventUpdateRowFilter,
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
        let mut solver_y = match self.numerics_profile {
            MeNumericsProfile::Component => self.current_solver_y()?,
            // The frozen driver starts a located event from its dense-output
            // full vector, then replaces only the continuous-state prefix when
            // bracketing the right limit. Preserve that ownership here: a
            // tolerance-equal root may snap back to an output target, and
            // rebuilding every lane at that target can change strict relation
            // memory before the shared event iteration sees the located side.
            MeNumericsProfile::DiffsolFrozen => {
                let mut solver_y = event_entry_y.clone();
                solver_y[..self.state_count].copy_from_slice(&self.states);
                solver_y
            }
        };
        let pending_root_overrides = self
            .pending_root_crossings
            .drain(..)
            .map(|crossing| (crossing.index, crossing.post_relation_memory_value))
            .collect::<Vec<_>>();
        let has_typed_root_override = pending_root_overrides.iter().any(|(index, _)| {
            matches!(
                self.runtime
                    .model
                    .problem
                    .events
                    .root_relation_memory_targets
                    .get(*index),
                Some(Some(_))
            )
        });
        let root_overrides = match self.numerics_profile {
            MeNumericsProfile::Component => pending_root_overrides.as_slice(),
            MeNumericsProfile::DiffsolFrozen if has_typed_root_override => {
                pending_root_overrides.as_slice()
            }
            MeNumericsProfile::DiffsolFrozen => &[],
        };
        let runtime = Rc::clone(&self.runtime);
        let projection_runtime = Rc::clone(&runtime);
        let settle_projection_runtime = Rc::clone(&runtime);
        let policy = self.algebraic_projection_policy();
        let tol = policy.tolerance;
        let settle = policy.settle;
        let outcome = runtime.apply_projected_event_update(
            ProjectedEventUpdateInput {
                y: &mut solver_y,
                p: &mut self.params,
                t: event_time,
                tol,
                event_pre_y: &event_entry_y,
                event_pre_p: &event_entry_p,
                max_iters: settle.max_iters,
                row_filter,
                root_relation_overrides: root_overrides,
            },
            move |y, p| project_algebraics(&projection_runtime, y, p, event_time, policy),
        )?;
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen)
            && !has_typed_root_override
        {
            // The frozen compatibility settle reconstructs relation memory
            // from the numerical application point. A located crossing has a
            // stronger, typed post-side value that the event iteration above
            // has already settled; recomputing at the exact root would erase
            // that value for strict relations.
            runtime.settle_projected_runtime_and_relation_memory(
                &mut solver_y,
                &mut self.params,
                event_time,
                tol,
                settle.max_iters,
                move |y, p| {
                    project_algebraics(&settle_projection_runtime, y, p, event_time, policy)
                },
            )?;
        }
        self.copy_states_from_solver_y(&solver_y);
        *self.solver_y_guess.borrow_mut() = solver_y;
        if matches!(self.numerics_profile, MeNumericsProfile::Component) {
            let post_event_y = self.current_solver_y()?;
            commit_pre_params_after_event_at(
                &self.runtime.model,
                &post_event_y,
                &mut self.params,
                Some(event_time),
                self.tolerance,
            );
            self.commit_delay_point()?;
        }
        self.record_event_action_outcome(outcome, event_time)?;
        self.clear_runtime_caches();
        Ok(())
    }

    fn finish_frozen_runtime_event(&mut self, event_time: f64) -> Result<(), MeError> {
        if !matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return Ok(());
        }
        let post_event_y = self.current_solver_y()?;
        commit_pre_params_after_event_at(
            &self.runtime.model,
            &post_event_y,
            &mut self.params,
            Some(event_time),
            self.tolerance,
        );
        self.commit_delay_point()?;
        if let Some(accepted_seed) = self.frozen_event_accepted_seed.take() {
            *self.solver_y_guess.borrow_mut() = accepted_seed;
        }
        self.skip_next_enter_continuous_delay_commit = true;
        Ok(())
    }

    fn complete_coincident_root_right_limit(
        &mut self,
        entry: MeEventEntry,
        event: RuntimeEventStop,
        settled_right_limit: Option<f64>,
        tolerance: f64,
    ) -> Result<Option<f64>, MeError> {
        let right_time =
            runtime_root_event_application_time(entry.event_time, entry.horizon, tolerance);
        if settled_right_limit.map(f64::to_bits) == Some(right_time.to_bits()) {
            return Ok(settled_right_limit);
        }
        // The clock owner has completed at the semantic tick. The root's
        // numerical right-limit transition starts from that settled superdense
        // value and may execute only unowned rows; clock-owned rows cannot
        // sample the post-event state a second time.
        let event_pre_y = self.current_solver_y()?;
        self.boundary_event_pre_y = Some(event_pre_y);
        self.boundary_event_pre_p = Some(self.params.clone());
        RuntimeEventBoundaryHandler::on_event_right_limit(self, right_time, event)?;
        Ok(Some(right_time))
    }

    fn refresh_frozen_event_observation(&mut self, time: f64) -> Result<(), MeError> {
        if !matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            return Ok(());
        }
        let mut solver_y = self.solver_y_guess.borrow().clone();
        self.runtime
            .refresh_delay_values(time, &solver_y, &mut self.params)?;
        self.runtime.refresh_observation_discrete_rows(
            &mut solver_y,
            &mut self.params,
            time,
            self.tolerance.max(1.0e-10),
            256,
        )?;
        self.copy_states_from_solver_y(&solver_y);
        *self.solver_y_guess.borrow_mut() = solver_y;
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
                let time = if time.is_finite() { time } else { event_time };
                self.termination
                    .get_or_insert(SimTermination { time, message });
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
        let event_time = self.time;
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
        let policy = self.algebraic_projection_policy();
        let tol = policy.tolerance;
        let settle = policy.settle;
        let outcome = runtime.apply_projected_initial_event_boundary(
            ProjectedInitialEventInput {
                y: &mut solver_y,
                p: &mut self.params,
                t_start: self.time,
                t_end: self.stop_time,
                tol,
                event_pre_y: &startup_event_pre_y,
                event_pre_p: &startup_event_pre_p,
                max_iters: settle.max_iters,
                dynamic_event,
                apply_without_initial_event: self.root_profile.apply_without_initial_event(),
            },
            move |y, p, t| project_algebraics(&projection_runtime, y, p, t, policy),
        )?;
        self.copy_states_from_solver_y(&solver_y);
        *self.solver_y_guess.borrow_mut() = solver_y;
        self.time = outcome.final_t;
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen)
            && self.runtime.has_delay_channels()
        {
            let solver_y = self.solver_y_guess.borrow();
            self.runtime
                .commit_delay_history(self.time, &solver_y, &self.params)?;
            self.skip_next_enter_continuous_delay_commit = true;
        }
        self.initial_observations = outcome
            .observations
            .iter()
            .map(|observation| observation_from_initial_event(observation, &self.instance_brand))
            .collect();
        self.record_event_action_outcome(outcome.action, event_time)?;
        self.initial_event_pending = false;
        let right_limit = (outcome.final_t > event_time).then_some(outcome.final_t);
        self.time = event_time;
        self.set_post_event_eval_time(right_limit);
        self.discrete_states_after_update(true)
    }

    fn run_runtime_event_boundary(
        &mut self,
        entry: MeEventEntry,
    ) -> Result<MeDiscreteStates, MeError> {
        let tolerance = self.tolerance.max(1.0e-10);
        match entry.cause {
            MeEventCause::StateEvent => {
                self.advance_state_to_event_right_limit = false;
                let scheduled = self
                    .stop_schedule
                    .scheduled_event_coincidence_at(entry.event_time);
                let coincident_time_event = scheduled
                    .map(|coincidence| (coincidence.event.time, coincidence.event.event))
                    .or_else(|| {
                        self.pending_event_stop
                            .filter(|(time, _)| time_match_with_tol(*time, entry.event_time))
                    });
                self.state_time_coincidence = match scheduled.map(|value| value.consumption) {
                    Some(ScheduledEventConsumption::Unconsumed) => StateTimeCoincidence::Unconsumed,
                    Some(ScheduledEventConsumption::Consumed) => StateTimeCoincidence::Consumed,
                    None if coincident_time_event.is_some() => StateTimeCoincidence::Unconsumed,
                    None => StateTimeCoincidence::None,
                };
                let (event_time, event) = coincident_time_event.unwrap_or_else(|| {
                    (
                        entry.event_time,
                        RuntimeEventStop::static_event(EventPreMode::EventEntry),
                    )
                });
                let horizon_t = coincident_time_event
                    .map_or(entry.event_time.min(entry.horizon), |(_, event)| {
                        runtime_event_horizon(event, entry.horizon, self.stop_time)
                    });
                let outcome = process_runtime_event_boundary(
                    RuntimeEventBoundary {
                        event_t: event_time,
                        horizon_t,
                        tolerance,
                        event,
                    },
                    self,
                )?;
                let mut right_limit_t = outcome.right_limit_t;
                if coincident_time_event.is_some()
                    && matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen)
                {
                    right_limit_t = self.complete_coincident_root_right_limit(
                        entry,
                        event,
                        outcome.right_limit_t,
                        tolerance,
                    )?;
                }
                self.finish_frozen_runtime_event(entry.event_time)?;
                if coincident_time_event.is_some() {
                    self.stop_schedule.advance_past(event_time);
                    self.pending_event_stop = None;
                    self.set_post_event_eval_time(right_limit_t);
                    self.clear_event_entry_scheduled_root_relation_memory(outcome.final_t, event)?;
                    self.clear_runtime_caches();
                }
                self.state_time_coincidence = StateTimeCoincidence::None;
                self.discrete_states_after_update(true)
            }
            MeEventCause::TimeEvent => {
                self.advance_state_to_event_right_limit = true;
                self.state_time_coincidence = StateTimeCoincidence::None;
                let (_, event) = self.pending_event_stop.take().ok_or_else(|| {
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
                self.advance_state_to_event_right_limit = false;
                self.finish_frozen_runtime_event(entry.event_time)?;
                self.stop_schedule.advance_past(entry.event_time);
                self.set_post_event_eval_time(outcome.right_limit_t);
                self.clear_event_entry_scheduled_root_relation_memory(outcome.final_t, event)?;
                self.clear_runtime_caches();
                self.discrete_states_after_update(true)
            }
        }
    }

    /// Build the exact `fmi3UpdateDiscreteStates` output set after the event
    /// iteration has settled. Time remains importer-owned; the next scheduled
    /// event is announced here rather than exposed through a second component
    /// scheduling operation.
    fn discrete_states_after_update(
        &mut self,
        values_of_continuous_states_changed: bool,
    ) -> Result<MeDiscreteStates, MeError> {
        let next_event_time = if self.termination.is_some() || self.time >= self.stop_time {
            self.pending_event_stop = None;
            None
        } else {
            let stop = self.next_event_stop_inner(self.stop_time)?;
            stop.is_event.then_some(stop.time)
        };
        Ok(MeDiscreteStates {
            discrete_states_need_update: false,
            terminate_simulation: self.termination.clone(),
            values_of_continuous_states_changed,
            nominals_of_continuous_states_changed: false,
            next_event_time,
        })
    }
}

impl RuntimeEventBoundaryHandler for SolveMeKernel {
    type Error = MeError;

    fn on_event_time(
        &mut self,
        event_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), Self::Error> {
        if self.advance_state_to_event_right_limit {
            self.time = event_time;
        }
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
        let application_time = event_update_application_time(
            event_time,
            self.time,
            self.state_time_coincidence.is_some(),
        );
        let row_filter = if self.state_time_coincidence.is_consumed() {
            EventUpdateRowFilter::UnownedOnly
        } else {
            EventUpdateRowFilter::All
        };
        self.apply_discrete_event_updates(application_time, event, row_filter)?;
        if self.advance_state_to_event_right_limit {
            self.refresh_frozen_event_observation(event_time)?;
        }
        Ok(())
    }

    fn on_event_right_limit(
        &mut self,
        right_time: f64,
        event: RuntimeEventStop,
    ) -> Result<(), Self::Error> {
        if self.advance_state_to_event_right_limit || self.state_time_coincidence.is_some() {
            let event_time = self.time;
            let settle = self.numerics_settle();
            let derivatives = self.runtime.eval_state_derivatives(
                event_time,
                &self.states,
                &self.params,
                settle.tol,
                settle.max_iters,
            )?;
            advance_state_across_event_right_limit(
                &mut self.states,
                &derivatives,
                event_time,
                right_time,
            );
        }
        self.time = right_time;
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
        let row_filter = if self.state_time_coincidence.is_some() {
            EventUpdateRowFilter::UnownedOnly
        } else {
            EventUpdateRowFilter::All
        };
        self.apply_discrete_event_updates(right_time, event, row_filter)?;
        if self.advance_state_to_event_right_limit {
            self.refresh_frozen_event_observation(right_time)?;
        }
        self.set_post_event_eval_time(Some(right_time));
        Ok(())
    }
}

impl ModelExchangeKernel for SolveMeKernel {
    fn model_description(&self) -> MeModelDescription<'_> {
        MeModelDescription {
            continuous_state_count: self.state_count,
            event_indicator_count: self.runtime.root_condition_count(),
            // The linked kernel commits accepted-point history and invalidates
            // component caches here. Discrete-delay models need this even when
            // they have no continuous delay channel, so the current component
            // profile honestly requires the call for every model.
            needs_completed_integrator_step: true,
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
            .map(|index| MeValueRef {
                index,
                instance_brand: Rc::clone(&self.instance_brand),
            })
    }

    fn enter_initialization_mode(&mut self) -> Result<(), MeError> {
        self.require_lifecycle_transition(MeLifecycleCommand::EnterInitializationMode)
            .and_then(|()| self.enter_initialization_mode_inner())
            .and_then(|()| {
                self.commit_lifecycle_transition(MeLifecycleCommand::EnterInitializationMode)
            })
            .map_err(|error| error.at_stage(MeStage::Initialization))
    }

    fn exit_initialization_mode(&mut self) -> Result<(), MeError> {
        self.require_lifecycle_transition(MeLifecycleCommand::ExitInitializationMode)
            .and_then(|()| self.exit_initialization_mode_inner())
            .and_then(|()| {
                self.commit_lifecycle_transition(MeLifecycleCommand::ExitInitializationMode)
            })
            .map_err(|error| error.at_stage(MeStage::Initialization))
    }

    fn enter_event_mode(&mut self, entry: MeEventEntry) -> Result<(), MeError> {
        self.require_lifecycle_transition(MeLifecycleCommand::EnterEventMode)
            .map_err(|error| error.at_stage(MeStage::EventIteration))?;
        validate_event_entry(entry, self.tolerance)
            .map_err(|error| error.at_stage(MeStage::EventIteration))?;
        if matches!(self.numerics_profile, MeNumericsProfile::DiffsolFrozen) {
            self.frozen_event_accepted_seed = Some(self.solver_y_guess.borrow().clone());
        }
        // Entering Event Mode is the standard signal that any continuous-
        // mode accepted-point cache is no longer authoritative. This replaces
        // the retired Rumoca-only `AtStateEvent` completed-step variant.
        self.clear_runtime_caches();
        self.last_event_entry = Some(entry);
        self.pending_event_entry = Some(entry);
        self.commit_lifecycle_transition(MeLifecycleCommand::EnterEventMode)
            .map_err(|error| error.at_stage(MeStage::EventIteration))
    }

    fn update_discrete_states(&mut self) -> Result<MeDiscreteStates, MeError> {
        self.require_lifecycle_transition(MeLifecycleCommand::UpdateDiscreteStates)
            .map_err(|error| error.at_stage(MeStage::EventIteration))?;
        let result = if self.initial_event_pending {
            self.run_initial_event_boundary()
                .map_err(|error| error.at_stage(MeStage::Initialization))
        } else {
            let entry = self
                .pending_event_entry
                .take()
                .ok_or_else(|| contract("event mode has no pending event entry"))
                .map_err(|error| error.at_stage(MeStage::EventIteration))?;
            self.run_runtime_event_boundary(entry)
                .map_err(|error| error.at_stage(MeStage::EventIteration))
        }?;
        self.commit_lifecycle_transition(MeLifecycleCommand::UpdateDiscreteStates)
            .map_err(|error| error.at_stage(MeStage::EventIteration))?;
        Ok(result)
    }

    fn enter_continuous_time_mode(&mut self) -> Result<(), MeError> {
        self.require_lifecycle_transition(MeLifecycleCommand::EnterContinuousTimeMode)?;
        if self.initial_event_pending || self.pending_event_entry.is_some() {
            return Err(contract(
                "enter_continuous_time_mode requires the pending event update to complete",
            ));
        }
        if self.skip_next_enter_continuous_delay_commit {
            self.skip_next_enter_continuous_delay_commit = false;
        } else {
            self.commit_delay_point()?;
        }
        self.clear_all_scheduled_root_relation_memory()?;
        self.clear_runtime_caches();
        self.commit_lifecycle_transition(MeLifecycleCommand::EnterContinuousTimeMode)
    }

    fn terminate(&mut self) -> Result<(), MeError> {
        self.require_lifecycle_transition(MeLifecycleCommand::Terminate)?;
        rumoca_eval_solve::trace_solve_row_eval_snapshot(self.instance_name);
        self.commit_lifecycle_transition(MeLifecycleCommand::Terminate)
    }

    fn set_time(&mut self, time: MeTime) -> Result<(), MeError> {
        self.require_active_lifecycle("set_time")?;
        if !time.time.is_finite() {
            return Err(contract("set_time requires a finite time"));
        }
        if let Some(boundary) = time.event_boundary
            && (!boundary.is_finite() || boundary < time.time)
        {
            return Err(contract(
                "set_time event boundary must be finite and not earlier than time",
            ));
        }
        self.time = time.time;
        self.event_boundary = time.event_boundary;
        Ok(())
    }

    fn set_continuous_states(&mut self, states: &[f64]) -> Result<(), MeError> {
        self.require_active_lifecycle("set_continuous_states")?;
        if states.len() != self.state_count {
            return Err(contract(format!(
                "continuous state buffer has {} entries for {} continuous states",
                states.len(),
                self.state_count
            )));
        }
        if states.iter().any(|value| !value.is_finite()) {
            return Err(contract("continuous state values must all be finite"));
        }
        self.states.copy_from_slice(states);
        Ok(())
    }

    fn get_continuous_states(&self, states: &mut [f64]) -> Result<(), MeError> {
        self.require_active_lifecycle("get_continuous_states")?;
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
        self.require_active_lifecycle("get_continuous_state_derivatives")?;
        let time = self.continuous_eval_time();
        if let Some(cached) = self.cached_derivative(time, &self.states) {
            *derivatives = cached;
            return Ok(());
        }
        let settle = self.numerics_settle();
        let values = self
            .with_delay_evaluation_params(time, &self.states, |params| {
                self.with_callback_solver_y(|guess| {
                    let values = self
                        .runtime
                        .eval_state_derivatives_with_guess(
                            time,
                            &self.states,
                            params,
                            guess,
                            settle.tol,
                            settle.max_iters,
                        )
                        .map_err(MeError::from)?;
                    Ok::<_, MeError>(values)
                })
            })
            .map_err(|error| error.at_stage(MeStage::Integration))?
            .map_err(|error| error.at_stage(MeStage::Integration))?;
        self.cache_derivative(time, &self.states, &values);
        *derivatives = values;
        Ok(())
    }

    fn get_directional_derivative(
        &self,
        seed: &[f64],
        sensitivity: &mut [f64],
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("get_directional_derivative")?;
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
        let settle = self.numerics_settle();
        self.with_delay_evaluation_params(time, &self.states, |params| {
            self.with_callback_solver_y(|guess| {
                self.runtime
                    .eval_state_jacobian_v_ad_with_guess_into(
                        AlgebraicLinearization {
                            t: time,
                            params,
                            settle,
                        },
                        &self.states,
                        seed,
                        guess,
                        sensitivity,
                    )
                    .map_err(MeError::from)
            })
        })
        .map_err(|error| error.at_stage(MeStage::Integration))?
        .map_err(|error| error.at_stage(MeStage::Integration))
    }

    fn get_event_indicators(&self, indicators: &mut Vec<f64>) -> Result<(), MeError> {
        self.require_active_lifecycle("get_event_indicators")?;
        let time = self.continuous_eval_time();
        if let Some(cached) = self.cached_root_conditions(time, &self.states) {
            *indicators = cached;
            return Ok(());
        }
        let mut values = self
            .with_delay_evaluation_params(time, &self.states, |params| match self.root_profile {
                MeRootProfile::Component => self
                    .runtime
                    .eval_root_conditions(
                        time,
                        &self.states,
                        params,
                        ALGEBRAIC_REFRESH_TOL,
                        UPDATE_MAX_ITERS,
                    )
                    .map_err(MeError::from),
                MeRootProfile::DiffsolFrozen => {
                    let mut values = vec![0.0; self.runtime.root_condition_count()];
                    self.runtime
                        .eval_root_search_conditions_into(
                            time,
                            &self.states,
                            params,
                            self.tolerance.max(1.0e-10),
                            256,
                            &mut values,
                        )
                        .map_err(MeError::from)?;
                    Ok(values)
                }
            })
            .map_err(|error| error.at_stage(MeStage::Integration))?
            .map_err(|error| error.at_stage(MeStage::Integration))?;
        crate::orient_typed_root_zeros(
            &mut values,
            &self.runtime.model.problem.events.root_zero_domains,
        );
        self.cache_root_conditions(time, &self.states, &values);
        *indicators = values;
        Ok(())
    }

    fn project_continuous_states(&mut self, states: &mut [f64]) -> Result<bool, MeError> {
        self.require_active_lifecycle("project_continuous_states")?;
        self.project_continuous_states_inner(states)
            .map_err(|error| error.at_stage(MeStage::ManifoldProjection))
    }

    fn completed_integrator_step(
        &mut self,
        _no_set_fmu_state_prior_to_current_point: bool,
    ) -> Result<MeCompletedIntegratorStep, MeError> {
        self.require_active_lifecycle("completed_integrator_step")?;
        self.post_event_eval_time = None;
        if !self.pending_root_crossings.is_empty() {
            self.clear_runtime_caches();
        } else if self.last_projection_changed {
            self.clear_derivative_cache();
        } else {
            // FMI does not let an importer hand an FSAL stage into the FMU.
            // Keep the ordinary accepted-point cache private by evaluating it
            // through the same standard derivative operation the importer
            // could call here. A located event stays cache-free until Event
            // Mode consumes it.
            let mut accepted_derivatives = Vec::new();
            self.get_continuous_state_derivatives(&mut accepted_derivatives)?;
        }
        self.commit_delay_point()
            .map_err(|error| error.at_stage(MeStage::Integration))?;
        Ok(MeCompletedIntegratorStep::default())
    }

    fn max_step_size(&self) -> Option<f64> {
        self.delay_step_limit
    }

    fn next_event_stop(&mut self, horizon: f64) -> Result<MeEventStop, MeError> {
        self.require_active_lifecycle("next_event_stop")?;
        self.next_event_stop_inner(horizon)
            .map_err(|error| error.at_stage(MeStage::Integration))
    }

    fn event_indicator_crossings(
        &self,
        before: &[f64],
        after: &[f64],
        crossings: &mut Vec<MeIndicatorCrossing>,
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("event_indicator_crossings")?;
        let expected = self.runtime.root_condition_count();
        if before.len() != expected || after.len() != expected {
            return Err(contract(format!(
                "event-indicator crossing buffers have {}/{} entries for {expected} indicators",
                before.len(),
                after.len(),
            )));
        }
        if before.iter().chain(after).any(|value| !value.is_finite()) {
            return Err(contract(
                "event-indicator crossing buffers must contain finite values",
            ));
        }
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
        self.require_active_lifecycle("capture_pre_event_state")?;
        let pre_y = match self.numerics_profile {
            MeNumericsProfile::Component => self.runtime.full_solver_y(
                self.time,
                &self.states,
                &self.params,
                ALGEBRAIC_REFRESH_TOL,
                UPDATE_MAX_ITERS,
            )?,
            MeNumericsProfile::DiffsolFrozen => self.solver_y_at_time(self.time)?,
        };
        self.pending_event_pre_y = Some(pre_y);
        self.pending_event_pre_p = Some(self.params.clone());
        Ok(())
    }

    fn arm_state_event(&mut self, crossings: &[MeIndicatorCrossing]) -> Result<(), MeError> {
        self.require_active_lifecycle("arm_state_event")?;
        let indicator_count = self.runtime.root_condition_count();
        if let Some(crossing) = crossings.iter().find(|crossing| {
            crossing.index >= indicator_count
                || !crossing.post_indicator_value.is_finite()
                || !matches!(crossing.post_indicator_value, 0.0 | 1.0)
        }) {
            return Err(contract(format!(
                "state-event crossing index {} value {} is invalid for {indicator_count} indicators",
                crossing.index, crossing.post_indicator_value,
            )));
        }
        self.pending_root_crossings.clear();
        self.pending_root_crossings
            .extend(crossings.iter().map(|crossing| RootCrossing {
                index: crossing.index,
                post_relation_memory_value: crossing.post_indicator_value,
            }));
        Ok(())
    }

    fn observe(&self) -> Result<MeObservation, MeError> {
        self.require_active_lifecycle("observe")?;
        Ok(MeObservation {
            time: self.time,
            solver_y: self.current_solver_y()?,
            parameters: self.params.clone(),
            instance_brand: Rc::clone(&self.instance_brand),
        })
    }

    fn record_outputs(
        &self,
        observation: &MeObservation,
        sample_time: f64,
        series: &mut MeOutputSeries,
    ) -> Result<(), MeError> {
        self.require_active_lifecycle("record_outputs")?;
        self.require_observation_brand(observation)?;
        if series.columns_mut().len() != self.runtime.model.visible_names.len() {
            return Err(contract(format!(
                "output series has {} columns for {} visible outputs",
                series.columns_mut().len(),
                self.runtime.model.visible_names.len(),
            )));
        }
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
        self.require_active_lifecycle("get_outputs")?;
        self.require_observation_brand(observation)?;
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
        self.require_active_lifecycle("set_float64")?;
        if refs.len() != values.len() {
            return Err(contract(format!(
                "{} value references do not match {} values",
                refs.len(),
                values.len()
            )));
        }
        if let Some((reference, value)) =
            refs.iter()
                .zip(values.iter().copied())
                .find(|(reference, value)| {
                    !Rc::ptr_eq(&reference.instance_brand, &self.instance_brand)
                        || reference.index >= self.params.len()
                        || !value.is_finite()
                })
        {
            return Err(contract(format!(
                "Float64 value reference {} with value {value} is invalid for {} parameters",
                reference.index,
                self.params.len(),
            )));
        }
        let checkpoint = self.fmu_state();
        for (reference, value) in refs.iter().zip(values.iter().copied()) {
            self.params[reference.index] = value;
        }
        self.clear_runtime_caches();
        if let Err(error) = self.commit_delay_point() {
            self.reset_to_fmu_state(&checkpoint)
                .expect("a same-instance internal checkpoint is always restorable");
            return Err(error);
        }
        Ok(())
    }

    fn fmu_state(&self) -> MeFmuState {
        MeFmuState {
            component: MeKernelSnapshot {
                lifecycle: self.lifecycle.state(),
                stop_time: self.stop_time,
                time: self.time,
                event_boundary: self.event_boundary,
                post_event_eval_time: self.post_event_eval_time,
                event_anchor_time: self.event_anchor_time,
                states: self.states.clone(),
                params: self.params.clone(),
                stop_schedule: self.stop_schedule.clone(),
                pending_event_entry: self.pending_event_entry,
                last_event_entry: self.last_event_entry,
                pending_event_stop: self.pending_event_stop,
                advance_state_to_event_right_limit: self.advance_state_to_event_right_limit,
                state_time_coincidence: self.state_time_coincidence,
                initial_event_pending: self.initial_event_pending,
                skip_next_enter_continuous_delay_commit: self
                    .skip_next_enter_continuous_delay_commit,
                pending_root_crossings: self.pending_root_crossings.clone(),
                pending_event_pre_y: self.pending_event_pre_y.clone(),
                pending_event_pre_p: self.pending_event_pre_p.clone(),
                boundary_event_pre_y: self.boundary_event_pre_y.clone(),
                boundary_event_pre_p: self.boundary_event_pre_p.clone(),
                frozen_event_accepted_seed: self.frozen_event_accepted_seed.clone(),
                solver_y_guess: self.solver_y_guess.borrow().clone(),
                delay_params_scratch: self.delay_params_scratch.borrow().clone(),
                delay_solver_y_scratch: self.delay_solver_y_scratch.borrow().clone(),
                derivative_cache: self.derivative_cache.borrow().clone(),
                root_cache: self.root_cache.borrow().clone(),
                initial_observations: self.initial_observations.clone(),
                delay_step_limit: self.delay_step_limit,
                last_projection_changed: self.last_projection_changed,
                termination: self.termination.clone(),
                settled_initialization_y: self.settled_initialization_y.clone(),
                runtime: self.runtime.snapshot(),
            },
            instance_brand: Rc::clone(&self.instance_brand),
        }
    }

    fn reset_to_fmu_state(&mut self, saved: &MeFmuState) -> Result<(), MeError> {
        if !Rc::ptr_eq(&saved.instance_brand, &self.instance_brand) {
            return Err(contract(
                "component snapshot belongs to a different ME instance",
            ));
        }
        let state = &saved.component;
        self.stop_time = state.stop_time;
        self.time = state.time;
        self.event_boundary = state.event_boundary;
        self.post_event_eval_time = state.post_event_eval_time;
        self.event_anchor_time = state.event_anchor_time;
        self.states.clone_from(&state.states);
        self.params.clone_from(&state.params);
        self.stop_schedule.clone_from(&state.stop_schedule);
        self.pending_event_entry = state.pending_event_entry;
        self.last_event_entry = state.last_event_entry;
        self.pending_event_stop = state.pending_event_stop;
        self.advance_state_to_event_right_limit = state.advance_state_to_event_right_limit;
        self.state_time_coincidence = state.state_time_coincidence;
        self.initial_event_pending = state.initial_event_pending;
        self.skip_next_enter_continuous_delay_commit =
            state.skip_next_enter_continuous_delay_commit;
        self.pending_root_crossings
            .clone_from(&state.pending_root_crossings);
        self.pending_event_pre_y
            .clone_from(&state.pending_event_pre_y);
        self.pending_event_pre_p
            .clone_from(&state.pending_event_pre_p);
        self.boundary_event_pre_y
            .clone_from(&state.boundary_event_pre_y);
        self.boundary_event_pre_p
            .clone_from(&state.boundary_event_pre_p);
        self.frozen_event_accepted_seed
            .clone_from(&state.frozen_event_accepted_seed);
        self.solver_y_guess
            .borrow_mut()
            .clone_from(&state.solver_y_guess);
        self.delay_params_scratch
            .borrow_mut()
            .clone_from(&state.delay_params_scratch);
        self.delay_solver_y_scratch
            .borrow_mut()
            .clone_from(&state.delay_solver_y_scratch);
        self.derivative_cache
            .borrow_mut()
            .clone_from(&state.derivative_cache);
        self.root_cache.borrow_mut().clone_from(&state.root_cache);
        self.initial_observations
            .clone_from(&state.initial_observations);
        self.delay_step_limit = state.delay_step_limit;
        self.last_projection_changed = state.last_projection_changed;
        self.termination.clone_from(&state.termination);
        self.settled_initialization_y
            .clone_from(&state.settled_initialization_y);
        self.runtime.restore(&state.runtime);
        self.lifecycle.restore(state.lifecycle);
        Ok(())
    }

    fn restart_from_fmu_state(
        &mut self,
        saved: &MeFmuState,
        start_time: f64,
    ) -> Result<(), MeError> {
        if !start_time.is_finite() {
            return Err(contract("component restart requires a finite start time"));
        }
        self.reset_to_fmu_state(saved)?;
        self.time = start_time;
        self.event_boundary = None;
        self.stop_schedule =
            SolveStopSchedule::new(&self.runtime.model.problem, start_time, self.stop_time);
        self.termination = None;
        self.pending_root_crossings.clear();
        self.pending_event_pre_y = None;
        self.pending_event_pre_p = None;
        self.boundary_event_pre_y = None;
        self.boundary_event_pre_p = None;
        self.frozen_event_accepted_seed = None;
        self.post_event_eval_time = None;
        self.event_anchor_time = start_time;
        self.pending_event_entry = None;
        self.last_event_entry = None;
        self.pending_event_stop = None;
        self.advance_state_to_event_right_limit = false;
        self.state_time_coincidence = StateTimeCoincidence::None;
        self.skip_next_enter_continuous_delay_commit = false;
        self.initial_observations.clear();
        self.clear_runtime_caches();
        self.runtime.reset_delay_history();
        self.commit_delay_point()
    }

    fn extend_stop_time(&mut self, from_time: f64, stop_time: f64) -> Result<(), MeError> {
        self.require_active_lifecycle("extend_stop_time")?;
        if !from_time.is_finite() || !stop_time.is_finite() || stop_time < from_time {
            return Err(contract(
                "extended stop time requires finite values with stop_time >= from_time",
            ));
        }
        self.stop_time = stop_time;
        self.stop_schedule =
            SolveStopSchedule::new(&self.runtime.model.problem, from_time, stop_time);
        Ok(())
    }
}

#[cfg(any(test, kani))]
fn float_slice_bit_eq(left: &[f64], right: &[f64]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| left.to_bits() == right.to_bits())
}

#[cfg(any(test, kani))]
fn option_float_bit_eq(left: Option<f64>, right: Option<f64>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => left.to_bits() == right.to_bits(),
        (None, None) => true,
        _ => false,
    }
}

/// Select the time owned by the first event-update pass.
///
/// A coincident scheduled clock owns its exact semantic tick. An ordinary
/// located state event is applied where the host positioned the component —
/// normally the numerical right limit, or a target/horizon it snapped to.
pub(super) fn event_update_application_time(
    semantic_event_time: f64,
    component_time: f64,
    coincident_state_time_event: bool,
) -> f64 {
    if coincident_state_time_event {
        semantic_event_time
    } else {
        component_time
    }
}

#[cfg(any(test, kani))]
fn option_float_vec_bit_eq(left: &Option<Vec<f64>>, right: &Option<Vec<f64>>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => float_slice_bit_eq(left, right),
        (None, None) => true,
        _ => false,
    }
}

#[cfg(any(test, kani))]
fn option_event_entry_bit_eq(left: Option<MeEventEntry>, right: Option<MeEventEntry>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => {
            left.cause == right.cause
                && left.event_time.to_bits() == right.event_time.to_bits()
                && left.horizon.to_bits() == right.horizon.to_bits()
        }
        (None, None) => true,
        _ => false,
    }
}

#[cfg(any(test, kani))]
fn option_event_stop_bit_eq(
    left: Option<(f64, RuntimeEventStop)>,
    right: Option<(f64, RuntimeEventStop)>,
) -> bool {
    match (left, right) {
        (Some((left_time, left)), Some((right_time, right))) => {
            left_time.to_bits() == right_time.to_bits() && left == right
        }
        (None, None) => true,
        _ => false,
    }
}

#[cfg(any(test, kani))]
fn root_crossings_bit_eq(left: &[RootCrossing], right: &[RootCrossing]) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            left.index == right.index
                && left.post_relation_memory_value.to_bits()
                    == right.post_relation_memory_value.to_bits()
        })
}

#[cfg(any(test, kani))]
fn derivative_cache_bit_eq(
    left: Option<&CachedDerivative>,
    right: Option<&CachedDerivative>,
) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => {
            left.time.to_bits() == right.time.to_bits()
                && float_slice_bit_eq(&left.state, &right.state)
                && float_slice_bit_eq(&left.derivative, &right.derivative)
        }
        (None, None) => true,
        _ => false,
    }
}

#[cfg(any(test, kani))]
fn root_cache_bit_eq(
    left: Option<&CachedRootConditions>,
    right: Option<&CachedRootConditions>,
) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => {
            left.time.to_bits() == right.time.to_bits()
                && float_slice_bit_eq(&left.state, &right.state)
                && float_slice_bit_eq(&left.values, &right.values)
        }
        (None, None) => true,
        _ => false,
    }
}

#[cfg(any(test, kani))]
fn observations_bit_eq(left: &[MeObservation], right: &[MeObservation]) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            left.time.to_bits() == right.time.to_bits()
                && float_slice_bit_eq(&left.solver_y, &right.solver_y)
                && float_slice_bit_eq(&left.parameters, &right.parameters)
                && Rc::ptr_eq(&left.instance_brand, &right.instance_brand)
        })
}

#[cfg(any(test, kani))]
fn termination_bit_eq(left: Option<&SimTermination>, right: Option<&SimTermination>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => {
            left.time.to_bits() == right.time.to_bits() && left.message == right.message
        }
        (None, None) => true,
        _ => false,
    }
}

fn observation_from_initial_event(
    observation: &InitialEventObservation,
    instance_brand: &Rc<()>,
) -> MeObservation {
    MeObservation {
        time: observation.t,
        solver_y: observation.y.clone(),
        parameters: observation.p.clone(),
        instance_brand: Rc::clone(instance_brand),
    }
}

fn contract(reason: impl Into<String>) -> MeError {
    MeError::Contract {
        reason: reason.into(),
    }
}

fn lifecycle_contract(violation: MeLifecycleViolation) -> MeError {
    contract(format!(
        "{} is invalid in ME lifecycle state {}",
        violation.command.name(),
        violation.state.name(),
    ))
}

fn validate_instance_config(config: &MeInstanceConfig) -> Result<(), MeError> {
    if config.instance_name.is_empty() {
        return Err(contract("ME instance name must not be empty"));
    }
    if !config.tolerance.is_finite() || config.tolerance <= 0.0 {
        return Err(contract("ME tolerance must be finite and positive"));
    }
    if !config.start_time.is_finite()
        || !config.stop_time.is_finite()
        || config.stop_time < config.start_time
    {
        return Err(contract(
            "ME time horizon requires finite values with stop_time >= start_time",
        ));
    }
    Ok(())
}

fn validate_event_entry(entry: MeEventEntry, tolerance: f64) -> Result<(), MeError> {
    let order_tolerance =
        tolerance.max(1.0e-12 * (1.0 + entry.event_time.abs().max(entry.horizon.abs())));
    if !entry.event_time.is_finite()
        || !entry.horizon.is_finite()
        || (entry.horizon < entry.event_time && entry.event_time - entry.horizon > order_tolerance)
    {
        return Err(contract(format!(
            "event entry requires finite values with horizon >= event_time; cause={:?} event_time={} horizon={}",
            entry.cause, entry.event_time, entry.horizon
        )));
    }
    Ok(())
}

fn state_values_match(a: &[f64], b: &[f64]) -> bool {
    a.len() == b.len()
        && a.iter()
            .zip(b)
            .all(|(lhs, rhs)| lhs.to_bits() == rhs.to_bits())
}

fn first_bit_mismatch_except(
    a: &[f64],
    b: &[f64],
    excluded: &[usize],
) -> Option<(usize, Option<f64>, Option<f64>)> {
    let len = a.len().max(b.len());
    (0..len).find_map(|index| {
        if excluded.contains(&index) {
            return None;
        }
        let lhs = a.get(index).copied();
        let rhs = b.get(index).copied();
        (lhs.map(f64::to_bits) != rhs.map(f64::to_bits)).then_some((index, lhs, rhs))
    })
}

fn project_algebraics(
    runtime: &SolveRuntime,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    policy: MeAlgebraicProjectionPolicy,
) -> Result<bool, crate::runtime::solve_ops::RuntimeSolveError> {
    let MeAlgebraicProjectionPolicy {
        state_count,
        tolerance: tol,
        profile,
        settle,
    } = policy;
    match profile {
        MeNumericsProfile::Component => {
            let before = y.to_vec();
            runtime.project_state_manifold(y, p, t, tol)?;
            let state = y[..state_count.min(y.len())].to_vec();
            let refreshed =
                runtime.full_solver_y(t, &state, p, ALGEBRAIC_REFRESH_TOL, UPDATE_MAX_ITERS)?;
            y.copy_from_slice(&refreshed);
            Ok(runtime_values_changed(&before, y, tol))
        }
        MeNumericsProfile::DiffsolFrozen => {
            runtime.refresh_delay_values(t, y, p)?;
            let manifold_changed = runtime.project_state_manifold(y, p, t, tol)?;
            // Mirror the frozen driver's callback exactly: state-manifold
            // projection reports every bit-level state change, while the
            // scaled comparison applies only to the subsequent runtime-lane
            // refresh.  Dropping `manifold_changed` can stop an event fixed
            // point one pass early for large-magnitude states.
            let before_runtime_refresh = y.to_vec();
            runtime.refresh_algebraic_and_output_slots(t, y, p, settle.tol, settle.max_iters)?;
            Ok(frozen_projection_changed(
                manifold_changed,
                &before_runtime_refresh,
                y,
                tol,
            ))
        }
    }
}

pub(super) fn frozen_projection_changed(
    manifold_changed: bool,
    before_runtime_refresh: &[f64],
    after_runtime_refresh: &[f64],
    tolerance: f64,
) -> bool {
    manifold_changed
        || runtime_values_changed(before_runtime_refresh, after_runtime_refresh, tolerance)
}

/// SPEC_0038 "Unsupported lifecycle capability fails before execution".
fn validate_explicit_solve_model(model: &rumoca_ir_solve::SolveModel) -> Result<(), MeError> {
    super::validation::validate_explicit_solve_model(model)
}
