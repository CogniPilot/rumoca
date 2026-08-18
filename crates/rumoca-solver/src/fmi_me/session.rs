//! The sole FMI 3 Model Exchange master algorithm (SPEC_0044 §6, ME-HOST-001).
//!
//! `MeSimulationSession` owns initialization, the FMI modes, the discrete
//! iteration, the time-event cache, the timeout, inputs, reset, termination,
//! output roles and coordinates, and `SimResult` construction. A numerical
//! plugin supplies one accepted step, its native continuous extension, and
//! truncate/reset; nothing else crosses that boundary.
//!
//! One FMI instance is leased exclusively. [`MeRetainedComponent`] instantiates
//! the component once; [`MeRetainedComponent::lease`] borrows it **mutably** to
//! produce an [`MeComponentHost`], and every session derived from that host
//! carries the same borrow. A second host, a reset, or a mutation behind a live
//! session is therefore a compile error rather than a runtime hazard
//! (review finding \[341\]§1).

mod error;
mod host_state;
mod options;

use std::{
    cell::{Cell, RefCell},
    marker::PhantomData,
    rc::Rc,
};

use indexmap::IndexMap;

use host_state::{InitializationOutcome, MeHostState, run_fmi_initialization};

pub use error::{MePluginArity, MeSessionError, MeSessionLoss};
pub use options::{MeAdvanceOutcome, MeOutputCursor, MeSessionOptions, MeSessionOptionsInput};

use error::latched_failure;
use options::trace_capacity;

use super::{
    MeEventCause, MeExecutionBackend, MeInstanceConfig, MeModelSource, MeTime, MeValueRef,
    ModelExchangeKernel, SolveMeKernel,
    integrator::{
        MeAcceptedStep, MeAdvanceRequest, MeContinuousPoint, MeDerivativeController,
        MeIntegrationError, MeIntegratorBackend, MeNumericalSetup, MeStepProposal,
        accepted_step_roundoff, canonical_coordinate, reachable_bound, sample_complete,
        time_only::TimeOnlyIntegrator,
    },
    root::{
        MeRootApplication, MeRootSearchPolicy, RootScanTarget, accept_step, scan_accepted_interval,
    },
    trace::{MeTraceRecorder, TraceObservationRole},
};
use crate::{
    runtime::timeout::TimeoutBudget,
    solver::{SimResult, SimTermination, SimVariableMeta},
};

/// One instantiated FMI 3 ME component retained across runs.
///
/// Instantiation performs the eager compiles a component's runtime owns, so a
/// prepared simulation keeps exactly one instance and rewinds it
/// (`fmi3Reset` + `fmi3SetFMUState`) per run instead of re-instantiating.
///
/// It exposes no derivative handle and no host of its own: the only way to use
/// the instance is [`Self::lease`], which takes `&mut self`.
pub struct MeRetainedComponent {
    kernel: Rc<RefCell<SolveMeKernel>>,
    pristine: super::MeFmuState,
    state_count: usize,
}

impl MeRetainedComponent {
    /// `fmi3InstantiateModelExchange` plus the pristine snapshot every run
    /// rewinds to.
    pub fn instantiate(
        source: MeModelSource<'_>,
        config: &MeInstanceConfig,
        execution_backend: Option<MeExecutionBackend>,
    ) -> Result<Self, MeSessionError> {
        let kernel =
            SolveMeKernel::instantiate_with_execution_backend(source, config, execution_backend)?;
        let kernel = Rc::new(RefCell::new(kernel));
        let pristine = kernel.borrow().fmu_state();
        let state_count = kernel.borrow().model_description().continuous_state_count;
        Ok(Self {
            kernel,
            pristine,
            state_count,
        })
    }

    #[must_use]
    pub fn state_count(&self) -> usize {
        self.state_count
    }

    /// Lease the sole FMI instance for one run: rewind to the pristine
    /// snapshot and run FMI initialization.
    ///
    /// The exclusive `&mut self` borrow travels into the returned host and into
    /// every session built from it, so no second host can exist and the
    /// component cannot be reset behind a live session. Use this when a
    /// prepared simulation reuses one instance across runs.
    pub fn lease(
        &mut self,
        options: MeSessionOptions,
    ) -> Result<MeComponentHost<'_>, MeSessionError> {
        self.kernel
            .borrow_mut()
            .reset_to_fmu_state(&self.pristine)?;
        self.host_over_component(options)
    }

    /// Consume the component into a single host.
    ///
    /// The stronger of the two exclusivity proofs: after this call there is no
    /// other handle to the instance at all, which is what an open live session
    /// needs, because it must own its component rather than borrow one.
    pub fn into_lease(
        self,
        options: MeSessionOptions,
    ) -> Result<MeComponentHost<'static>, MeSessionError> {
        let host = build_host_state(self.kernel, self.pristine, options)?;
        Ok(MeComponentHost {
            host,
            lease: PhantomData,
        })
    }

    fn host_over_component(
        &self,
        options: MeSessionOptions,
    ) -> Result<MeComponentHost<'_>, MeSessionError> {
        let host = build_host_state(Rc::clone(&self.kernel), self.pristine.clone(), options)?;
        Ok(MeComponentHost {
            host,
            lease: PhantomData,
        })
    }
}

/// Construct the host state with every aggregate already valid.
///
/// The derivative controller is minted here, per lease, from the same single
/// instance: `MeRetainedComponent::lease(&mut self)` is what proves only one
/// lease — and therefore only one controller — exists at a time.
fn build_host_state(
    kernel: Rc<RefCell<SolveMeKernel>>,
    pristine: super::MeFmuState,
    options: MeSessionOptions,
) -> Result<MeHostState, MeSessionError> {
    let (state_count, indicator_count, names, meta, input_names) = {
        let borrowed = kernel.borrow();
        let description = borrowed.model_description();
        (
            description.continuous_state_count,
            description.event_indicator_count,
            description.output_names.to_vec(),
            description.output_meta.to_vec(),
            description.input_names.to_vec(),
        )
    };
    let capacity = trace_capacity(&options);
    let mut trace = MeTraceRecorder::new(names, meta, state_count, capacity)?;
    let outcome =
        run_fmi_initialization(&mut kernel.borrow_mut(), &options, options.records_trace())?;
    let policy = build_policy(&options, &outcome, state_count)?;
    if let Some(values) = &outcome.initial_values {
        trace.record_slice(
            TraceObservationRole::Initialization,
            options.start_time(),
            values,
        )?;
    }
    let terminated = outcome.termination.is_some();
    let mut states = outcome.states;
    states.resize(state_count, 0.0);
    let derivatives = MeDerivativeController::over_kernel(Rc::clone(&kernel));
    Ok(MeHostState {
        kernel,
        derivatives,
        policy,
        trace,
        budget: TimeoutBudget::new(options.max_wall_seconds()),
        time: options.start_time(),
        states,
        retained_indicators: Vec::new(),
        retained_interval: None,
        next_event_time: outcome.next_event_time,
        termination: outcome.termination,
        terminated,
        state_count,
        indicator_count,
        input_names,
        inputs: IndexMap::new(),
        pristine,
        event_streak_time: None,
        event_streak_count: 0,
        usability: Cell::new(None),
        options,
    })
}

fn build_policy(
    options: &MeSessionOptions,
    outcome: &InitializationOutcome,
    state_count: usize,
) -> Result<Option<MeRootSearchPolicy>, MeSessionError> {
    if outcome.termination.is_some() {
        // A session that terminated during initialization never scans, so it
        // holds no policy at all rather than an invented one.
        return Ok(None);
    }
    MeRootSearchPolicy::new(
        options.root_scan_resolution(),
        options.root_location_tolerance(),
        options.absolute_tolerance(),
        options.relative_tolerance(),
        outcome.nominals.clone(),
        state_count,
    )
    .map(Some)
}

/// An instantiated, initialized FMI 3 ME component awaiting its plugin.
pub struct MeComponentHost<'lease> {
    host: MeHostState,
    lease: PhantomData<&'lease ()>,
}

impl MeComponentHost<'_> {
    /// The checked numerical configuration a plugin may scale with.
    ///
    /// The host validated these nominals for its own root policy; the plugin
    /// never queries component nominal policy itself.
    pub fn numerical_setup(
        &self,
        initial_step_hint: Option<f64>,
    ) -> Result<MeNumericalSetup, MeSessionError> {
        let nominals = self.host.policy()?.nominals().to_vec();
        Ok(MeNumericalSetup::new(
            self.host.options.relative_tolerance(),
            self.host.options.absolute_tolerance(),
            nominals,
            self.host.state_count,
            initial_step_hint,
        )?)
    }

    #[must_use]
    pub fn state_count(&self) -> usize {
        self.host.state_count
    }

    #[must_use]
    pub fn is_terminated(&self) -> bool {
        self.host.terminated
    }

    /// The settled coordinate a plugin's history starts from.
    pub fn initial_point(&self) -> Result<MeContinuousPoint, MeSessionError> {
        self.host.checked_point()
    }

    #[must_use]
    pub fn termination(&self) -> Option<&SimTermination> {
        self.host.termination.as_ref()
    }

    pub fn output_names(&self) -> Vec<String> {
        self.host.output_names()
    }

    pub fn output_meta(&self) -> Vec<SimVariableMeta> {
        self.host.output_meta()
    }

    /// Publish the trace of a run that terminated during initialization.
    #[must_use]
    pub fn finish(self) -> SimResult {
        self.host.finish_trace()
    }

    /// Attach the numerical plugin and start the master algorithm.
    ///
    /// `None` selects the time-only plugin, which is the only admissible choice
    /// for a component with zero continuous states.
    pub fn into_session<'lease, 'plugin>(
        self,
        backend: Option<Box<dyn MeIntegratorBackend + 'plugin>>,
    ) -> Result<MeSimulationSession<'lease, 'plugin>, MeSessionError>
    where
        Self: 'lease,
    {
        let state_count = self.host.state_count;
        // ME-ZERO-001's arity rule, decided from the linked component's own
        // width and the presence of a plugin. It asks the backend nothing: the
        // thin ME-INT-001 surface has no identity operation to ask
        // (review finding [399]).
        let backend: Box<dyn MeIntegratorBackend + 'plugin> = match backend {
            Some(_) if state_count == 0 => {
                return Err(MeSessionError::PluginArity {
                    state_count,
                    mismatch: MePluginArity::RejectsANumericalPlugin,
                });
            }
            Some(backend) => backend,
            None if state_count == 0 => Box::new(TimeOnlyIntegrator::new()),
            None => {
                return Err(MeSessionError::PluginArity {
                    state_count,
                    mismatch: MePluginArity::RequiresANumericalPlugin,
                });
            }
        };
        let mut session = MeSimulationSession {
            host: self.host,
            backend,
            lease: PhantomData,
        };
        session.start_plugin()?;
        Ok(session)
    }
}

/// The one FMI 3 ME master algorithm.
pub struct MeSimulationSession<'lease, 'plugin> {
    host: MeHostState,
    backend: Box<dyn MeIntegratorBackend + 'plugin>,
    lease: PhantomData<&'lease ()>,
}

impl MeSimulationSession<'_, '_> {
    // -- public incremental surface ----------------------------------------

    #[must_use]
    pub fn time(&self) -> f64 {
        self.host.time
    }

    #[must_use]
    pub fn state_count(&self) -> usize {
        self.host.state_count
    }

    #[must_use]
    pub fn termination(&self) -> Option<&SimTermination> {
        self.host.termination.as_ref()
    }

    #[must_use]
    pub fn is_terminated(&self) -> bool {
        self.host.terminated
    }

    pub fn output_names(&self) -> Vec<String> {
        self.host.output_names()
    }

    pub fn input_names(&self) -> Vec<String> {
        self.host.input_names.clone()
    }

    pub fn output_meta(&self) -> Vec<SimVariableMeta> {
        self.host.output_meta()
    }

    /// Batched `fmi3GetFloat64` over every output at the current coordinate.
    pub fn output_values(&self) -> Result<Vec<f64>, MeSessionError> {
        self.require_live()?;
        Ok(self.host.observe_current()?)
    }

    /// Current visible values keyed by name, including host-applied inputs.
    pub fn visible_values(&self) -> Result<IndexMap<String, f64>, MeSessionError> {
        let names = self.output_names();
        let values = self.output_values()?;
        if names.len() != values.len() {
            return Err(MeSessionError::Contract {
                reason: format!(
                    "the component returned {} output values for {} declared outputs",
                    values.len(),
                    names.len()
                ),
            });
        }
        let mut visible: IndexMap<String, f64> = names.into_iter().zip(values).collect();
        for (name, value) in &self.host.inputs {
            visible.insert(name.clone(), *value);
        }
        Ok(visible)
    }

    /// Whether `name` is a declared **input** of this component.
    ///
    /// Review finding \[343\]: causality comes from the model description's input
    /// inventory, not from the mere existence of a value reference.
    #[must_use]
    pub fn has_input(&self, name: &str) -> bool {
        self.host.input_names.iter().any(|input| input == name)
    }

    /// Typed `fmi3SetFloat64` on a declared input.
    ///
    /// The transaction boundary is the first **committed** component write.
    /// The name and value-reference rejections, and `fmi3SetFloat64`'s own
    /// atomic batch rejection, all happen without mutating anything, so they
    /// return their typed failure and leave an ordinary live session
    /// (ME-BUF-001, review finding \[386\]). Only once the component holds the
    /// new value must the host's state vector, the visible input cache, the
    /// retained indicators, and the plugin's history catch up with it, so a
    /// failure from there on ends the session (review finding \[382\]).
    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), MeSessionError> {
        self.require_live()?;
        if !self.has_input(name) {
            return Err(MeSessionError::Contract {
                reason: format!("'{name}' is not a declared input of this component"),
            });
        }
        let reference: MeValueRef =
            self.host
                .kernel
                .borrow()
                .value_reference(name)
                .ok_or_else(|| MeSessionError::Contract {
                    reason: format!("declared input '{name}' has no value reference"),
                })?;
        // Pre-mutation: the component validates the whole batch before it
        // writes, so this rejection leaves every owner unchanged.
        self.write_input(&reference, value)?;
        let correlated = self.correlate_input(name, value);
        self.host
            .guard_mutation(MeSessionLoss::InputApplication, correlated)
    }

    /// The one committed component write, at the session's accepted point.
    fn write_input(&mut self, reference: &MeValueRef, value: f64) -> Result<(), MeSessionError> {
        let mut kernel = self.host.kernel.borrow_mut();
        kernel.set_time(MeTime::at(self.host.time))?;
        kernel.set_continuous_states(&self.host.states)?;
        Ok(kernel.set_float64(std::slice::from_ref(reference), &[value])?)
    }

    /// Bring every other owner up to the component's new value.
    fn correlate_input(&mut self, name: &str, value: f64) -> Result<(), MeSessionError> {
        self.host
            .kernel
            .borrow()
            .get_continuous_states(&mut self.host.states)?;
        self.host.inputs.insert(name.to_owned(), value);
        // An input mutation invalidates the plugin's history and the retained
        // indicator vector: both are conclusions about the pre-mutation model.
        self.restart_plugin_history()
    }

    /// Restart the session at `start_time` from the pristine component state.
    ///
    /// Every piece of session-owned evidence and every cache is discarded
    /// together: trace rows, inputs, termination, the time-event cache, the
    /// event streak, the timeout budget, and the plugin's history
    /// (review finding \[337\]§5).
    pub fn reset(&mut self, start_time: f64) -> Result<(), MeSessionError> {
        self.require_usable()?;
        // Rejecting an inadmissible restart coordinate moves nothing, so it
        // leaves an ordinary live session; everything after it does move the
        // component and the host caches together (review finding [382]).
        let options = self.host.options.restarted_at(start_time)?;
        let restarted = self.restart_session(options);
        self.host.guard_mutation(MeSessionLoss::Restart, restarted)
    }

    /// Discard every cache and rebuild the whole lifecycle.
    fn restart_session(&mut self, options: MeSessionOptions) -> Result<(), MeSessionError> {
        let pristine = self.host.pristine.clone();
        self.host
            .kernel
            .borrow_mut()
            .restart_from_fmu_state(&pristine, options.start_time())?;
        self.host.trace.clear();
        self.host.inputs.clear();
        self.host.termination = None;
        self.host.terminated = false;
        self.host.next_event_time = None;
        self.host.retained_indicators.clear();
        self.host.retained_interval = None;
        self.host.event_streak_time = None;
        self.host.event_streak_count = 0;
        self.host.budget = TimeoutBudget::new(options.max_wall_seconds());
        self.host.options = options;

        let outcome = run_fmi_initialization(
            &mut self.host.kernel.borrow_mut(),
            &self.host.options,
            self.host.options.records_trace(),
        )?;
        self.host.policy = build_policy(&self.host.options, &outcome, self.host.state_count)?;
        if let Some(values) = &outcome.initial_values {
            self.host
                .record_initialization(self.host.options.start_time(), values)?;
        }
        self.host.time = self.host.options.start_time();
        self.host.states = outcome.states;
        self.host.states.resize(self.host.state_count, 0.0);
        self.host.next_event_time = outcome.next_event_time;
        self.host.terminated = outcome.termination.is_some();
        self.host.termination = outcome.termination;
        self.start_plugin()
    }

    /// Advance the master algorithm to `yield_time`, materializing every soft
    /// observation the cursor requests on the way.
    pub fn advance_to(
        &mut self,
        yield_time: f64,
        cursor: &mut MeOutputCursor,
    ) -> Result<MeAdvanceOutcome, MeSessionError> {
        self.require_usable()?;
        if !yield_time.is_finite() {
            return Err(MeSessionError::Contract {
                reason: format!("advance_to requires a finite yield boundary, got {yield_time}"),
            });
        }
        let roundoff = accepted_step_roundoff(self.host.time, 0.0);
        if yield_time - self.host.time < -roundoff {
            return Err(MeSessionError::Contract {
                reason: format!(
                    "advance_to({yield_time}) would move the session backward from t={}",
                    self.host.time
                ),
            });
        }
        loop {
            if self.host.terminated {
                return Ok(MeAdvanceOutcome::Terminated);
            }
            self.host.budget.check()?;
            match self.resolve_without_advance(yield_time, cursor)? {
                LoopStep::Finished(outcome) => return Ok(outcome),
                LoopStep::Continue => {}
                LoopStep::Advance => self.advance_one_accepted_step(yield_time, cursor)?,
            }
        }
    }

    /// Drive the whole defined experiment through the same incremental session.
    ///
    /// There is no second event or integration loop here: the batch call only
    /// supplies the output cursor and the final yield boundary.
    pub fn run_to_stop(&mut self, cursor: &mut MeOutputCursor) -> Result<(), MeSessionError> {
        let stop = self
            .host
            .options
            .stop_time()
            .ok_or_else(|| MeSessionError::Contract {
                reason: "a batch run requires a defined experiment stop time".to_owned(),
            })?;
        self.advance_to(stop, cursor)?;
        Ok(())
    }

    /// Publish the trace the session owns.
    #[must_use]
    pub fn finish(self) -> SimResult {
        self.host.finish_trace()
    }

    fn require_live(&self) -> Result<(), MeSessionError> {
        self.require_usable()?;
        if self.host.terminated {
            return Err(MeSessionError::Contract {
                reason: "the component is Terminated; no further FMI call is legal".to_owned(),
            });
        }
        Ok(())
    }

    /// Refuse a session that stopped being usable.
    ///
    /// Terminal by construction: some pair of correlated owners names different
    /// states and nothing here can re-establish that, so the session is an
    /// explicit non-reusable failed session. Metadata inspection and consuming
    /// the already durable trace stay available; every mutating or evaluating
    /// call is refused (review findings \[373\], \[382\]).
    fn require_usable(&self) -> Result<(), MeSessionError> {
        match self.host.usability_loss() {
            Some(loss) => Err(MeSessionError::SessionNotReusable { loss }),
            None => Ok(()),
        }
    }

    // -- plugin lifecycle --------------------------------------------------

    fn start_plugin(&mut self) -> Result<(), MeSessionError> {
        if self.host.terminated {
            // A component that terminated during initialization is never asked
            // for another FMI value, and no plugin history is meaningful.
            return Ok(());
        }
        self.restart_plugin_history()
    }

    fn restart_plugin_history(&mut self) -> Result<(), MeSessionError> {
        let point = self.host.checked_point()?;
        let handle = self.host.derivatives.issue_handle();
        self.run_with_derivatives(|backend| backend.initialize(&point, handle))?;
        // A restarted plugin holds no accepted interval, so no left limit can
        // be sampled from one until the next accepted step (finding [368]).
        self.host.retained_interval = None;
        self.host.refresh_retained_indicators()
    }

    /// Run exactly one plugin call that may evaluate the component.
    ///
    /// This is the whole activation policy of SPEC_0044 §6's retained-handle
    /// ruling, owned once, in the order \[377\] fixes:
    ///
    /// 1. surface any already-latched failure and stop, because it is a
    ///    request the backend made from outside every host wrapper: the
    ///    capability was inactive, so nothing was evaluated and nothing moved,
    ///    and it must be reported rather than cleared (review finding \[381\]);
    /// 2. open the window by guard, for this call and nothing else;
    /// 3. execute the one plugin call;
    /// 4. close the window **infallibly**, including on a backend error or a
    ///    panic;
    /// 5. join the newly latched failure ahead of whatever the backend
    ///    returned, so a generic library error — or an `Ok` — can never hide
    ///    the typed component, discard, or misuse failure that caused it
    ///    (review findings \[340\]§4, \[377\], \[378\]); and
    /// 6. restore the accepted point fallibly under the dual-failure rule,
    ///    because the plugin evaluated derivatives at trial coordinates the
    ///    session never adopted (review finding \[373\]).
    ///
    /// A backend that panics exits through the same steps rather than around
    /// them: the window is closed by its guard, and steps 5 and 6 are the one
    /// caught-excursion close every host excursion shares, so the accepted point
    /// is restored through the same transaction, the session records why it is
    /// no longer callable, and then the **original** payload resumes unwinding.
    /// It is never rendered into prose and never converted into a library error,
    /// so an embedding that catches it sees exactly what its plugin threw and
    /// holds a session that refuses every later evaluation
    /// (review findings \[388\], \[390\]).
    fn run_with_derivatives<T>(
        &mut self,
        body: impl FnOnce(&mut dyn MeIntegratorBackend) -> Result<T, MeIntegrationError>,
    ) -> Result<T, MeSessionError> {
        self.run_with_derivatives_until(None, body)
    }

    fn run_with_derivatives_until<T>(
        &mut self,
        event_boundary: Option<f64>,
        body: impl FnOnce(&mut dyn MeIntegratorBackend) -> Result<T, MeIntegrationError>,
    ) -> Result<T, MeSessionError> {
        if let Some(stale) = self.host.derivatives.take_error() {
            // An inactive request cannot move the component, so this needs no
            // restoration transaction and must not open a window.
            return Err(latched_failure(stale));
        }
        let Self { host, backend, .. } = self;
        host.settle_caught_excursion(|| {
            let outcome = {
                let window = host.derivatives.activate_until(event_boundary);
                let outcome = body(backend.as_mut());
                drop(window);
                outcome
            };
            debug_assert!(
                !host.derivatives.is_active(),
                "the activation window closes before any fallible host work"
            );
            match host.derivatives.take_error() {
                Some(latched) => Err(latched_failure(latched)),
                None => outcome.map_err(MeSessionError::from),
            }
        })
    }

    /// Evaluate the plugin's native continuous extension.
    ///
    /// The session stands on its own accepted point here, so nothing above this
    /// call moved the component and nothing below it could: the sampler's own
    /// unwind has only the plugin's interior history to lose, and that loss is
    /// committed before the payload resumes (review findings \[388\], \[390\]).
    fn sample_states(&self, time: f64, states: &mut [f64]) -> Result<(), MeSessionError> {
        match caught_sample(self.backend.as_ref(), &self.host.derivatives, time, states) {
            Ok(sampled) => sampled,
            Err(payload) => {
                self.host.mark_unusable(MeSessionLoss::NumericalStep);
                std::panic::resume_unwind(payload);
            }
        }
    }

    // -- master algorithm --------------------------------------------------

    /// Resolve everything the session owes at, or behind, its own coordinate
    /// before it asks for a numerical advance.
    ///
    /// Coincidence is `|delta| <= roundoff` in both directions; a coordinate
    /// that lies *strictly* behind the session is a typed contract failure, not
    /// a coincidence to be processed backward (review finding \[337\]§1).
    fn resolve_without_advance(
        &mut self,
        yield_time: f64,
        cursor: &mut MeOutputCursor,
    ) -> Result<LoopStep, MeSessionError> {
        let now = self.host.time;
        let roundoff = accepted_step_roundoff(now, 0.0);

        if let Some(event) = self.host.next_event_time {
            if event - now < -roundoff {
                return Err(MeSessionError::Contract {
                    reason: format!("the cached next event {event} lies behind t={now}"),
                });
            }
            if event - now <= roundoff {
                self.process_event_boundary(MeEventCause::TimeEvent, event)?;
                return Ok(LoopStep::Continue);
            }
        }

        if let Some(observation) = cursor.peek() {
            if observation - now < -roundoff {
                return Err(MeSessionError::Contract {
                    reason: format!(
                        "the output schedule still requests t={observation} behind t={now}"
                    ),
                });
            }
            if observation - now <= roundoff {
                self.materialize_reached_observation(now)?;
                cursor.advance();
                return Ok(LoopStep::Continue);
            }
        }

        if let Some(stop) = self.host.options.stop_time()
            && stop - now <= roundoff
        {
            // The defined experiment is complete.  FMI termination is a real
            // component lifecycle transition, not merely a host-side flag;
            // perform it here after the final coincident observation/event has
            // been materialized and before reporting success.
            self.host.terminate_component(None)?;
            return Ok(LoopStep::Finished(MeAdvanceOutcome::ReachedStop));
        }
        if yield_time - now <= roundoff {
            return Ok(LoopStep::Finished(MeAdvanceOutcome::Yielded));
        }
        Ok(LoopStep::Advance)
    }

    /// One accepted numerical step, as an explicit transaction.
    ///
    /// Everything rejectable — the fresh maximum-duration read, the bound
    /// arithmetic, and the checked request — is proven **before** the plugin is
    /// asked to move, so those failures stay ordinary and leave a live session.
    /// From `backend.advance` onward the plugin's private history has no
    /// rollback proof, so any failure through the proposal proof, the scan and
    /// its refinement, truncation, the completed-step callback, Event Mode, or
    /// trace publication ends the session while returning the original typed
    /// failure (review finding \[387\]).
    fn advance_one_accepted_step(
        &mut self,
        yield_time: f64,
        cursor: &mut MeOutputCursor,
    ) -> Result<(), MeSessionError> {
        let request = self.build_advance_request(yield_time, cursor)?;
        let stepped = self.take_one_numerical_step(request, cursor);
        self.host
            .guard_mutation(MeSessionLoss::NumericalStep, stepped)
    }

    /// Lend the request for one advance, then consume it into the proposal.
    ///
    /// The plugin returns raw numbers, and `request` is the one value this
    /// stack frame owns: it is lent for exactly the `advance` call, cannot be
    /// cloned, and is then moved into [`MeStepProposal::bind`]. So the proposal
    /// that comes out holds the actual host-issued coordinates the plugin was
    /// serving, by construction rather than by comparison, and there is no
    /// foreign, cached, or plugin-supplied request it could have been built
    /// from instead. Every coordinate the candidate reports (its start,
    /// direction, finiteness, bounds, and state arity) is proved during that
    /// move, against the linked component's own width, before any later stage
    /// may consume it (review findings \[398\], \[399\], \[400\]).
    fn take_one_numerical_step(
        &mut self,
        request: MeAdvanceRequest,
        cursor: &mut MeOutputCursor,
    ) -> Result<(), MeSessionError> {
        let event_boundary = self.host.next_event_time;
        let candidate =
            self.run_with_derivatives_until(event_boundary, |backend| backend.advance(&request))?;
        let proposal = MeStepProposal::bind(request, candidate, self.host.state_count)?;
        self.consume_accepted_step(proposal, cursor)
    }

    /// Prove every coordinate of one request before anything moves.
    fn build_advance_request(
        &mut self,
        yield_time: f64,
        cursor: &MeOutputCursor,
    ) -> Result<MeAdvanceRequest, MeSessionError> {
        let now = self.host.time;
        let roundoff = accepted_step_roundoff(now, 0.0);
        let mut hard_stop = self
            .host
            .options
            .stop_time()
            .filter(|stop| *stop - now > roundoff);
        if let Some(event) = self
            .host
            .next_event_time
            .filter(|event| *event - now > roundoff)
        {
            hard_stop = Some(hard_stop.map_or(event, |stop| stop.min(event)));
        }
        let max_step_duration = self.host.read_max_step_duration()?;
        let latest = reachable_bound(now, hard_stop, yield_time, max_step_duration);
        let observation = cursor
            .peek()
            .filter(|time| *time - now > roundoff && *time - latest <= roundoff);
        Ok(MeAdvanceRequest::new(
            self.host.checked_point()?,
            hard_stop,
            yield_time,
            observation,
            max_step_duration,
        )?)
    }

    /// Validate the plugin's sampler over the whole accepted interval, then
    /// scan the resulting host-issued proof for the earliest domain change.
    ///
    /// Endpoint validation is unconditional: a model with no event indicators
    /// still owes the accepted-step sampler contract (review finding \[336\]§4),
    /// and only the validated [`MeSampledStep`] can be scanned at all
    /// (review finding \[351\]§1).
    fn consume_accepted_step(
        &mut self,
        proposal: MeStepProposal,
        cursor: &mut MeOutputCursor,
    ) -> Result<(), MeSessionError> {
        let step = self.accept_proposal(proposal)?;
        crate::runtime::hotpath_stats::inc_solver_step();
        debug_assert!(
            step.order() > 0,
            "a checked accepted step always carries a positive declared order"
        );
        // The plugin now holds a native continuous extension over exactly this
        // interval, and the host has proved it. Every left-limit materialization
        // below samples that retained interval, so it is tracked from here until
        // a truncate or a history restart destroys it.
        self.host.retained_interval = Some((step.previous().time(), step.accepted().time()));
        let retained = std::mem::take(&mut self.host.retained_indicators);
        let located = self.scan_with_retained(&step, &retained);
        self.host.retained_indicators = retained;
        match located? {
            Some(application) => {
                crate::runtime::hotpath_stats::inc_root_hit();
                self.apply_located_root(&step, &application, cursor)
            }
            None => self.commit_accepted_endpoint(&step, cursor),
        }
    }

    /// The scan's view of the component and the plugin's continuous extension.
    fn scan_target(&self) -> SessionScanTarget<'_> {
        SessionScanTarget {
            kernel: &self.host.kernel,
            derivatives: &self.host.derivatives,
            backend: self.backend.as_ref(),
            budget: &self.host.budget,
        }
    }

    /// Prove the plugin's sampler covers the proposed interval.
    ///
    /// Wrapped in one excursion because the whole validation is component work
    /// away from the accepted point (review finding \[373\]), and in the caught
    /// form because a sampler that unwinds out of the middle of it leaves that
    /// obligation to exactly this transaction (review finding \[390\]).
    /// The proposal carries the consumed request, so the proof that leaves this
    /// call is a proof about the request the session actually served: the
    /// correlation travels through acceptance inside the value rather than
    /// alongside it (review findings \[399\], \[400\]).
    fn accept_proposal(&self, proposal: MeStepProposal) -> Result<MeAcceptedStep, MeSessionError> {
        self.host.settle_caught_excursion(|| {
            let policy = self.host.policy()?;
            accept_step(&mut self.scan_target(), policy, proposal)
        })
    }

    /// Scan and refine one accepted interval.
    ///
    /// The excursion covers the **entire** scan and its refinement, not each
    /// coordinate: the scan deliberately walks the component forward, and it is
    /// the exit — at the first domain change, at a component error, at an
    /// exhausted budget, at a failed reservation — that owes the accepted point
    /// back (review finding \[373\]). A sampler that unwinds after an interior
    /// indicator evaluation already moved the component leaves through the same
    /// obligation, so this is the transaction that catches it, restores, and
    /// only then resumes (review finding \[390\]).
    fn scan_with_retained(
        &self,
        step: &MeAcceptedStep,
        retained: &[f64],
    ) -> Result<Option<MeRootApplication>, MeSessionError> {
        self.host.settle_caught_excursion(|| {
            let policy = self.host.policy()?;
            scan_accepted_interval(&mut self.scan_target(), policy, step, retained)
        })
    }

    /// Complete exactly one accepted endpoint.
    ///
    /// `fmi3CompletedIntegratorStep` commits the component's history, relation
    /// memory, and delay state, so every observation that must describe the
    /// *left* of an event at this endpoint is taken before the callback runs.
    /// The candidate is observed unconditionally, because the host cannot know
    /// before the callback whether a step event is coming; it is published
    /// immediately when the endpoint reaches the cached `nextEventTime` (the
    /// host already knows that is an event) and otherwise only if the callback
    /// returns `enterEventMode`. No FMI getter runs retroactively
    /// (review findings \[368\], \[370\]).
    fn commit_accepted_endpoint(
        &mut self,
        step: &MeAcceptedStep,
        cursor: &mut MeOutputCursor,
    ) -> Result<(), MeSessionError> {
        let accepted = step.accepted();
        self.materialize_nominals_before(accepted.time(), cursor)?;
        // Establish the raw endpoint as the accepted anchor before observing
        // its continuous-left neighbor. A failed observation must restore this
        // accepted endpoint, while the plugin's native interval is still live.
        self.host.adopt_point(accepted.time(), accepted.states())?;
        let mut pending = self.capture_endpoint_event_left(accepted.time())?;
        let projected = self
            .host
            .adopt_projected_point(accepted.time(), accepted.states())?;
        if projected {
            let point = self.host.checked_point()?;
            self.run_with_derivatives(|backend| backend.truncate_reset(&point))?;
            self.host.retained_interval = None;
        }
        let reaches_time_event = self.host.endpoint_reaches_cached_event(accepted.time());
        if reaches_time_event {
            self.publish_event_left(pending.take())?;
        }
        let completed = self.host.completed_integrator_step()?;
        self.host.refresh_retained_indicators()?;
        if completed.terminate_simulation {
            // Read the final row while the component is still live, then
            // transition; no getter runs afterwards.
            self.record_final_row()?;
            self.host.terminate_at_current_point(
                "the component requested termination at a completed integrator step",
            )?;
            return Ok(());
        }
        if completed.enter_event_mode {
            // The callback originated this event, so its pre-callback left
            // candidate is retained now. Every preceding observation is already
            // durable, so entering Event Mode here cannot strand evidence.
            self.publish_event_left(pending.take())?;
            let cause = if reaches_time_event {
                MeEventCause::TimeEvent
            } else {
                MeEventCause::StateEvent
            };
            self.process_event_boundary(cause, self.host.time)?;
        }
        // An endpoint that turned out not to be an event retains no candidate.
        Ok(())
    }

    /// Observe the event-left candidate of an accepted endpoint.
    ///
    /// This runs while the plugin's native interval still exists and before
    /// `fmi3CompletedIntegratorStep`, so the coordinate, the sampled states, and
    /// the observed outputs are all the same continuous-left point. Whether the
    /// row is admissible at all is decided here, before any FMI getter runs; the
    /// recorder makes its own atomic decision when the row is published.
    ///
    /// `None` means there is nothing to publish: the coordinate is inadmissible
    /// (SPEC_0050 leaves the row already standing at or behind it as the left
    /// evidence), or the plugin holds no retained interval covering it.
    fn capture_endpoint_event_left(
        &mut self,
        event_time: f64,
    ) -> Result<Option<PendingEventLeft>, MeSessionError> {
        if !self.host.options.records_trace() {
            return Ok(None);
        }
        let candidate = crate::timeline::event_left_limit_time(event_time);
        let Some(coordinate) = self.host.event_left_coordinate(candidate, event_time) else {
            return Ok(None);
        };
        let Some((previous, accepted)) = self.host.retained_interval else {
            return Ok(None);
        };
        if coordinate < previous || coordinate > accepted {
            return Ok(None);
        }
        let mut states = try_filled(self.host.state_count, 0.0, "event-left sample")?;
        self.sample_states(coordinate, &mut states)?;
        let values = self.host.observe_off_point(coordinate, &states)?;
        Ok(Some(PendingEventLeft { coordinate, values }))
    }

    /// Retain a captured left candidate as published evidence.
    fn publish_event_left(
        &mut self,
        pending: Option<PendingEventLeft>,
    ) -> Result<(), MeSessionError> {
        let Some(pending) = pending else {
            return Ok(());
        };
        self.host.record_observed(
            TraceObservationRole::EventLeft,
            pending.coordinate,
            &pending.values,
        )
    }

    fn apply_located_root(
        &mut self,
        step: &MeAcceptedStep,
        application: &MeRootApplication,
        cursor: &mut MeOutputCursor,
    ) -> Result<(), MeSessionError> {
        let event_time = application.application().time();
        if event_time < step.previous().time() || event_time > step.accepted().time() {
            return Err(MeSessionError::RootApplicationUnavailable {
                time: event_time,
                reason: format!(
                    "the located coordinate lies outside the accepted interval [{}, {}]",
                    step.previous().time(),
                    step.accepted().time()
                ),
            });
        }
        // SPEC_0044 §6's ordering, taken literally: everything that needs the
        // native interval is materialized while it still exists, in coordinate
        // order — pending nominals, then the *checked* event-left point, then
        // the application point.
        self.materialize_nominals_before(event_time, cursor)?;
        // The row coordinate and the component state/time read are the same
        // point: the checked `MeRootApplication::left()`. Labelling the
        // application observation as left evidence would invent a
        // coordinate/value pairing (review finding [368]).
        self.materialize_event_left(application.left(), event_time)?;
        self.host
            .adopt_projected_point(event_time, application.application().states())?;
        // Only now is the uncompleted trial endpoint discarded. No sample is
        // requested after this point. The plugin re-establishes its history
        // through the retained capability, so this is an activated host call
        // and the application point is restored on both its exits.
        let point = self.host.checked_point()?;
        self.run_with_derivatives(|backend| backend.truncate_reset(&point))?;
        self.host.retained_interval = None;
        let completed = self.host.completed_integrator_step()?;
        if completed.terminate_simulation {
            self.host
                .record_from_component(TraceObservationRole::Nominal, self.host.time)?;
            self.host.terminate_at_current_point(
                "the component requested termination at a located event",
            )?;
            return Ok(());
        }
        self.enter_event_mode_and_settle(MeEventCause::StateEvent, event_time)
    }

    /// Enter Event Mode at a coordinate the session already stands on.
    ///
    /// This runs *after* the completed-step callback, so it materializes no
    /// observation of its own: the left evidence of an endpoint event was
    /// already observed and published pre-callback by
    /// [`Self::commit_accepted_endpoint`], and an event the session stands on
    /// without an intervening accepted step has no retained interval to sample
    /// (review finding \[370\]).
    fn process_event_boundary(
        &mut self,
        cause: MeEventCause,
        event_time: f64,
    ) -> Result<(), MeSessionError> {
        self.host.record_event_streak(event_time)?;
        self.enter_event_mode_and_settle(cause, event_time)
    }

    /// Publish the checked left-limit row of a located root.
    fn materialize_event_left(
        &mut self,
        left: &MeContinuousPoint,
        event_time: f64,
    ) -> Result<(), MeSessionError> {
        let Some(coordinate) = self.host.event_left_coordinate(left.time(), event_time) else {
            return Ok(());
        };
        self.host
            .record_off_point(TraceObservationRole::EventLeft, coordinate, left.states())
    }

    /// The complete Event Mode transition, as one transaction.
    ///
    /// Not only the continuous refresh: a failure in the discrete iteration or
    /// in re-entering Continuous-Time Mode leaves the component in a mode the
    /// host is no longer tracking, so it ends the session too
    /// (review finding \[387\]).
    fn enter_event_mode_and_settle(
        &mut self,
        cause: MeEventCause,
        event_time: f64,
    ) -> Result<(), MeSessionError> {
        let settled = self.settle_event_mode(cause, event_time);
        self.host
            .guard_mutation(MeSessionLoss::EventRefresh, settled)
    }

    fn settle_event_mode(
        &mut self,
        cause: MeEventCause,
        event_time: f64,
    ) -> Result<(), MeSessionError> {
        // The session takes the exact event coordinate before the lifecycle
        // transition, so Event Mode is entered at the point the host names
        // rather than moving the component behind the host's back
        // (review finding [373]).
        self.host.adopt_time(event_time)?;
        let discrete = self.host.run_event_mode(cause, event_time)?;
        if let Some(termination) = discrete.terminate_simulation {
            // FMI: `terminateSimulation` ends the iteration immediately and
            // transitions to Terminated without another numerical request, even
            // when `discreteStatesNeedUpdate` is also true. The settled row is
            // read here, while Event Mode still makes the getters legal.
            self.host.record_settled(event_time)?;
            self.host.terminate_component(Some(termination))?;
            return Ok(());
        }
        self.host.kernel.borrow_mut().enter_continuous_time_mode()?;
        self.refresh_after_event(
            discrete.values_of_continuous_states_changed,
            discrete.nominals_of_continuous_states_changed,
            event_time,
        )?;
        // A superdense chain at one coordinate publishes exactly one settled
        // row, at the end of the chain. Publishing per iteration would mint a
        // second settled row at one coordinate, which SPEC_0050 does not
        // authorize as a replacement (review finding [350]§1).
        if !continues_at(discrete.next_event_time, event_time) {
            self.host.record_settled(event_time)?;
        }
        self.host.next_event_time = discrete.next_event_time;
        Ok(())
    }

    /// Publish a soft observation the session already stands on.
    ///
    /// The row carries the coordinate the session is actually on. If that
    /// coordinate is already published — the start row, or the settled row of an
    /// event at this instant — the schedule requests nothing new and no
    /// candidate is generated at all (review finding \[350\]§1).
    fn materialize_reached_observation(&mut self, now: f64) -> Result<(), MeSessionError> {
        if self.host.already_published_at(now) {
            return Ok(());
        }
        self.host
            .record_from_component(TraceObservationRole::Nominal, now)
    }

    /// The last row of a run that terminated at a continuous coordinate.
    fn record_final_row(&mut self) -> Result<(), MeSessionError> {
        if self.host.already_published_at(self.host.time) {
            return Ok(());
        }
        self.host
            .record_from_component(TraceObservationRole::Nominal, self.host.time)
    }

    /// Commit the two continuous refreshes Event Mode may require, atomically.
    ///
    /// Event Mode may already have changed the component's continuous states,
    /// so from here the component, the host's state vector, the root policy's
    /// nominals, and the plugin's history must become the post-event model
    /// together. Any failure inside ends the session instead of leaving one of
    /// them on the pre-event model (SPEC_0044 §6's atomic-refresh rule, review
    /// finding \[382\]). The surrounding Event Mode transaction records the loss
    /// for the whole transition, this step included.
    fn refresh_after_event(
        &mut self,
        values_changed: bool,
        nominals_changed: bool,
        event_time: f64,
    ) -> Result<(), MeSessionError> {
        let refreshed_states = if values_changed {
            let mut states = try_filled(self.host.state_count, 0.0, "refreshed continuous states")?;
            self.host
                .kernel
                .borrow()
                .get_continuous_states(&mut states)?;
            Some(states)
        } else {
            None
        };
        let refreshed_policy = if nominals_changed {
            let nominals = self.host.read_nominals()?;
            Some(self.host.policy()?.with_nominals(nominals)?)
        } else {
            None
        };
        // Both refreshes commit only after both reads succeeded, so a failed
        // read or an invalid nominal leaves no partially refreshed host or
        // plugin state. The state adoption goes first because it is the one
        // step that can still fail: a failed component move restores the
        // previous point and leaves the policy untouched.
        self.host.adopt_owned(event_time, refreshed_states)?;
        if let Some(policy) = refreshed_policy {
            self.host.policy = Some(policy);
        }
        // Every event may change discrete values the derivatives read, so no
        // plugin history may span the boundary.
        self.restart_plugin_history()
    }

    // -- observation ownership ---------------------------------------------

    /// Materialize every pending soft observation strictly before `boundary`.
    ///
    /// An observation that coincides with `boundary` is deliberately left in
    /// the cursor: the master loop revisits it after any event at that
    /// coordinate settles, so the exact coordinate carries the settled value.
    fn materialize_nominals_before(
        &mut self,
        boundary: f64,
        cursor: &mut MeOutputCursor,
    ) -> Result<(), MeSessionError> {
        let mut states = try_filled(self.host.state_count, 0.0, "soft observation sample")?;
        while let Some(observation) = cursor.peek() {
            if observation >= boundary {
                return Ok(());
            }
            self.sample_states(observation, &mut states)?;
            self.host
                .record_off_point(TraceObservationRole::Nominal, observation, &states)?;
            cursor.advance();
        }
        Ok(())
    }
}

#[cfg(test)]
impl MeSimulationSession<'_, '_> {
    /// The session's own accepted point, bit-exact.
    pub(super) fn verification_session_point(&self) -> (u64, Vec<u64>) {
        (
            self.host.time.to_bits(),
            self.host
                .states
                .iter()
                .map(|value| value.to_bits())
                .collect(),
        )
    }

    /// The coordinate the *component* is standing on, bit-exact.
    ///
    /// ME-BUF-001 makes this and [`Self::verification_session_point`] one fact;
    /// an ablation asserts they agree after every failure
    /// (review finding \[373\]).
    pub(super) fn verification_component_point(&self) -> (u64, Vec<u64>) {
        let (_, time, states, _) = self.host.kernel.borrow().verification_observable_state();
        (time, states)
    }

    /// Whether the component and not just the host flag reached FMI
    /// Terminated.  This is deliberately a boolean: the private lifecycle type
    /// does not become part of the session API merely to support an ablation.
    pub(super) fn verification_component_is_terminated(&self) -> bool {
        self.host.kernel.borrow().verification_observable_state().0
            == super::lifecycle::MeState::Terminated
    }

    /// Whether the retained derivative capability is currently reachable.
    pub(super) fn verification_capability_is_active(&self) -> bool {
        self.host.derivatives.is_active()
    }

    /// Issue `fmi3Terminate` on the component underneath the session.
    ///
    /// `fmi3Terminate` is an ordinary standard call; the master algorithm simply
    /// never issues it mid-run, so this is the only way to reach the cases where
    /// an excursion's own body *and* its restoration both fail. It moves nothing
    /// and decides nothing: every later operation an ablation drives is a
    /// production one.
    pub(super) fn verification_terminate_component(&mut self) {
        self.host
            .kernel
            .borrow_mut()
            .terminate()
            .expect("fmi3Terminate is legal once");
    }

    /// Run one off-point observation against a component that has been
    /// terminated underneath the session.
    ///
    /// The excursion is the production one — nothing here is a test-only branch
    /// inside the master algorithm.
    pub(super) fn verification_observe_off_point_after_terminate(
        &mut self,
        time: f64,
        states: &[f64],
    ) -> MeSessionError {
        self.verification_terminate_component();
        self.host
            .observe_off_point(time, states)
            .expect_err("a terminated component refuses the observation and the restoration")
    }
}

/// Reserve a component-sized buffer without ever aborting on a failed
/// reservation.
///
/// Owned once, here, so every host path that claims typed allocation failure
/// actually has one (review finding \[351\]§5). These are the *host's* buffers,
/// so exhaustion is [`MeSessionError::Allocation`] and never a component
/// failure (review finding \[370\]§3).
pub(super) fn try_filled(
    entries: usize,
    value: f64,
    context: &'static str,
) -> Result<Vec<f64>, MeSessionError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(entries)
        .map_err(|_| MeSessionError::Allocation { context, entries })?;
    values.resize(entries, value);
    Ok(values)
}

/// Fallibly copy a component-sized slice.
pub(super) fn try_copied(
    source: &[f64],
    context: &'static str,
) -> Result<Vec<f64>, MeSessionError> {
    let mut values = try_filled(0, 0.0, context)?;
    values
        .try_reserve_exact(source.len())
        .map_err(|_| MeSessionError::Allocation {
            context,
            entries: source.len(),
        })?;
    values.extend_from_slice(source);
    Ok(values)
}

/// Whether the component scheduled another event at exactly this coordinate.
fn continues_at(next_event_time: Option<f64>, event_time: f64) -> bool {
    let coordinate = canonical_coordinate(event_time);
    next_event_time.is_some_and(|next| canonical_coordinate(next).to_bits() == coordinate.to_bits())
}

/// The left-limit evidence of an accepted endpoint, observed before
/// `fmi3CompletedIntegratorStep` and retained only if that endpoint turns out to
/// be an event (review finding \[370\]).
struct PendingEventLeft {
    coordinate: f64,
    values: Vec<f64>,
}

/// One turn of the master loop.
enum LoopStep {
    /// Something was resolved without a numerical advance; re-enter the loop.
    Continue,
    /// The call is over.
    Finished(MeAdvanceOutcome),
    /// Ask the plugin for exactly one accepted internal step.
    Advance,
}

/// The scan's view of the component and the plugin's continuous extension.
///
/// It deliberately holds neither the accepted point nor the usability ledger:
/// the whole scan runs inside one
/// [`MeHostState::settle_caught_excursion`] transaction, so restoration and the
/// loss it implies are the caller's single obligation on every exit (returned
/// or unwound) rather than a per-coordinate one (review findings \[373\], \[390\]).
struct SessionScanTarget<'a> {
    kernel: &'a Rc<RefCell<SolveMeKernel>>,
    derivatives: &'a MeDerivativeController,
    backend: &'a dyn MeIntegratorBackend,
    budget: &'a TimeoutBudget,
}

impl RootScanTarget for SessionScanTarget<'_> {
    /// The scan has already walked the component off the accepted point, so a
    /// sampler that unwinds here must reach the enclosing scan transaction with
    /// nothing yet decided: it resumes the original payload untouched and lets
    /// that one transaction restore the point and record the loss its outcome
    /// implies (review finding \[390\]).
    fn sample_states(&mut self, time: f64, states: &mut [f64]) -> Result<(), MeSessionError> {
        match caught_sample(self.backend, self.derivatives, time, states) {
            Ok(sampled) => sampled,
            Err(payload) => std::panic::resume_unwind(payload),
        }
    }

    fn indicators_at(
        &mut self,
        time: f64,
        states: &[f64],
        indicators: &mut Vec<f64>,
    ) -> Result<(), MeSessionError> {
        let mut kernel = self.kernel.borrow_mut();
        kernel.set_time(MeTime::at(time))?;
        kernel.set_continuous_states(states)?;
        indicators.clear();
        kernel.get_event_indicators(indicators)?;
        Ok(())
    }

    fn check_budget(&self) -> Result<(), MeSessionError> {
        Ok(self.budget.check()?)
    }
}

/// The whole `sample` policy, owned once for both sampler wrappers.
///
/// The capability is deactivated throughout, so a plugin that reaches for the
/// component here latches the typed misuse failure instead of evaluating it,
/// and the host surfaces that rather than the sample.
///
/// A panic out of the sampler moves nothing for exactly that reason: with the
/// window closed the plugin could not have reached the component at all. It can
/// still leave the plugin's own interior history half written, and
/// `MeIntegratorBackend` promises no rollback of it, so the numerical step is
/// lost either way; *whether the accepted point is also lost* depends instead
/// on what the caller already did to the component, which only the caller knows.
/// The payload is therefore handed back rather than acted on: the direct
/// session sampler, which stands on its accepted point, commits the
/// numerical-history loss itself, while a sampler reached from inside a scan
/// leaves both decisions to the one enclosing transaction, so a failed
/// restoration is never hidden behind an already-written numerical step
/// (review findings \[381\], \[387\], \[388\], \[390\]).
fn caught_sample(
    backend: &dyn MeIntegratorBackend,
    derivatives: &MeDerivativeController,
    time: f64,
    states: &mut [f64],
) -> CaughtSample {
    let caught = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        sample_complete(backend, time, states)
    }));
    caught.map(|outcome| match derivatives.take_error() {
        Some(latched) => Err(latched_failure(latched)),
        None => outcome.map_err(MeSessionError::from),
    })
}

/// What one closed-capability sampler call produced: the typed sample outcome,
/// or the plugin's original panic payload for its caller to route.
type CaughtSample = Result<Result<(), MeSessionError>, Box<dyn std::any::Any + Send>>;

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::fmi_me::{MeDerivativeHandle, MeStepCandidate};

    use super::*;

    /// A hostile sampler that reports success after writing only a prefix.
    struct PrefixSampler {
        written: usize,
    }

    impl MeIntegratorBackend for PrefixSampler {
        fn initialize(
            &mut self,
            _point: &MeContinuousPoint,
            _derivatives: MeDerivativeHandle,
        ) -> Result<(), MeIntegrationError> {
            Err(MeIntegrationError::contract(
                "not used by this sampler test",
            ))
        }

        fn advance(
            &mut self,
            _request: &MeAdvanceRequest,
        ) -> Result<MeStepCandidate, MeIntegrationError> {
            Err(MeIntegrationError::contract(
                "not used by this sampler test",
            ))
        }

        fn sample(&self, _time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
            for state in states.iter_mut().take(self.written) {
                *state = 0.0;
            }
            Ok(())
        }

        fn truncate_reset(&mut self, _point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
            Err(MeIntegrationError::contract(
                "not used by this sampler test",
            ))
        }
    }

    /// A successful sampler must replace every entry, including entries whose
    /// correct value is zero. This is the adversarial case a zero-filled host
    /// buffer could not distinguish from a backend that wrote nothing.
    #[test]
    fn an_incomplete_sampler_write_cannot_reuse_a_zero_or_stale_buffer_entry() {
        let derivatives =
            MeDerivativeController::over_closure(2, Rc::new(|_time, _states| vec![0.0, 0.0]));
        for (written, missing) in [(0, 0), (1, 1)] {
            let mut states = [0.0, 7.0];
            let failure =
                caught_sample(&PrefixSampler { written }, &derivatives, 0.25, &mut states)
                    .expect("the hostile sampler returned rather than panicked")
                    .expect_err("a successful no-write or partial write is incomplete");
            assert!(matches!(
                failure,
                MeSessionError::Integration(MeIntegrationError::Contract { .. })
            ));
            assert!(
                failure.to_string().contains(&format!("index {missing}")),
                "the first omitted entry is identified: {failure}"
            );
        }

        let mut states = [7.0, 7.0];
        caught_sample(
            &PrefixSampler { written: 2 },
            &derivatives,
            0.25,
            &mut states,
        )
        .expect("the complete sampler returned rather than panicked")
        .expect("finite zeros are valid when every entry was actually written");
        assert_eq!(states, [0.0, 0.0]);
    }

    /// The host's own component-sized buffers are the same category: a failed
    /// reservation is never an FMI component failure.
    #[test]
    fn a_host_buffer_reservation_failure_is_the_same_allocation_category() {
        let failure = try_filled(usize::MAX, 0.0, "oversized host buffer")
            .expect_err("an unrepresentable reservation cannot succeed");
        assert!(matches!(
            failure,
            MeSessionError::Allocation {
                context: "oversized host buffer",
                ..
            }
        ));
    }
}
