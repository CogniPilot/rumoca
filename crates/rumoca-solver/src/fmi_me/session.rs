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

mod error;
mod host_state;
mod options;

use std::{
    cell::{Cell, RefCell},
    marker::PhantomData,
    rc::Rc,
};

use indexmap::IndexMap;

use host_state::{EventStreak, MeHostComponent, MeHostState, run_fmi_initialization};

pub use error::{MePluginArity, MeSessionError, MeSessionLoss};
pub use options::{MeAdvanceOutcome, MeOutputCursor, MeSessionOptions, MeSessionOptionsInput};

use error::latched_failure;
use options::trace_capacity;

use super::{
    MeExecutionSelection, MeInstanceConfig, MeModelSource, MeTime, MeValueRef, SolveMeKernel,
    integrator::{
        MeAcceptedStep, MeAdvanceRequest, MeContinuousPoint, MeDerivativeController,
        MeIntegrationError, MeIntegratorBackend, MeNumericalSetup, MeStepProposal,
        accepted_step_roundoff, canonical_coordinate, reachable_bound, sample_complete,
        time_only::TimeOnlyIntegrator,
    },
    root::MeRootApplication,
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
}

impl MeRetainedComponent {
    /// `fmi3InstantiateModelExchange` plus the pristine snapshot every run
    /// rewinds to.
    pub fn instantiate(
        source: MeModelSource,
        config: &MeInstanceConfig,
        execution: MeExecutionSelection,
    ) -> Result<Self, MeSessionError> {
        let kernel = SolveMeKernel::instantiate_with_execution(source, config, execution)?;
        let kernel = Rc::new(RefCell::new(kernel));
        let pristine = kernel.borrow_mut().fmu_state();
        Ok(Self { kernel, pristine })
    }

    #[must_use]
    pub fn state_count(&self) -> usize {
        self.kernel.borrow().continuous_state_domain().len()
    }

    /// Admit a complete batch request without changing any component state.
    ///
    /// The returned affine value keeps the exclusive borrow and owns the one
    /// checked output cursor. Consuming it is the only batch path into a host,
    /// so the grid is neither recomputed nor revalidated after initialization.
    pub fn admit_batch(
        &mut self,
        options: MeSessionOptions,
    ) -> Result<MeBatchAdmission<'_>, MeSessionError> {
        MeBatchAdmission::construct(RetainedBatchLease::Borrowed(self), options)
    }

    /// Consume this retained component into one complete batch admission.
    ///
    /// This is the owning counterpart of [`Self::admit_batch`]. It produces the
    /// same affine admission type, but permits the initialized host/grid owner
    /// to cross a facade boundary without a self-referential prepared value.
    pub fn into_batch_admission(
        self,
        options: MeSessionOptions,
    ) -> Result<MeBatchAdmission<'static>, MeSessionError> {
        MeBatchAdmission::construct(RetainedBatchLease::Owned(Box::new(self)), options)
    }

    fn admitted_start_and_cursor(
        &self,
        options: &MeSessionOptions,
    ) -> Result<(f64, MeOutputCursor), MeSessionError> {
        let start_time = self.pristine.component_time();
        options.admit_start_time(start_time)?;
        let stop = options.stop_time().ok_or_else(|| MeSessionError::Options {
            reason: "a batch experiment requires a defined stop time".to_owned(),
        })?;
        let times =
            crate::timeline::try_build_output_times(start_time, stop, options.output_interval())
                .map_err(|error| MeSessionError::Options {
                    reason: error.to_string(),
                })?;
        let cursor = MeOutputCursor::new(times)?;
        Ok((start_time, cursor))
    }

    /// Lease the sole FMI instance for one run: rewind to the pristine
    /// snapshot and run FMI initialization.
    ///
    /// The exclusive `&mut self` borrow travels into the returned host and into
    /// every session built from it, so no second host can exist and the
    /// component cannot be reset behind a live session. Use this when a
    /// prepared simulation reuses one instance across runs.
    ///
    /// ```compile_fail,E0499
    /// use rumoca_solver::fmi_me::session::{
    ///     MeRetainedComponent, MeSessionError, MeSessionOptions,
    /// };
    ///
    /// fn two_live_leases(
    ///     retained: &mut MeRetainedComponent,
    ///     options: MeSessionOptions,
    /// ) -> Result<(), MeSessionError> {
    ///     let first = retained.lease(options.clone())?;
    ///     let second = retained.lease(options)?;
    ///     let _both_remain_live = (first.state_count(), second.state_count());
    ///     Ok(())
    /// }
    /// ```
    ///
    /// ```compile_fail,E0499
    /// use rumoca_solver::fmi_me::session::{
    ///     MeRetainedComponent, MeSessionError, MeSessionOptions,
    /// };
    ///
    /// fn session_keeps_the_lease_live(
    ///     retained: &mut MeRetainedComponent,
    ///     options: MeSessionOptions,
    /// ) -> Result<(), MeSessionError> {
    ///     let host = retained.lease(options.clone())?;
    ///     let session = host.into_session(None)?;
    ///     let second = retained.lease(options)?;
    ///     let _both = (session.time(), second.state_count());
    ///     Ok(())
    /// }
    /// ```
    pub fn lease(
        &mut self,
        options: MeSessionOptions,
    ) -> Result<MeComponentHost<'_>, MeSessionError> {
        self.host_over_component(options)
    }

    /// Consume the component into a single host.
    ///
    /// The stronger of the two exclusivity proofs: after this call there is no
    /// other handle to the instance at all, which is what an open live session
    /// needs, because it must own its component rather than borrow one. The
    /// pristine snapshot is restored first, so consuming a component after a
    /// capability-probe lease starts the selected session from the same state
    /// as any other run.
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
        &mut self,
        options: MeSessionOptions,
    ) -> Result<MeComponentHost<'_>, MeSessionError> {
        let host = build_host_state(Rc::clone(&self.kernel), self.pristine.clone(), options)?;
        Ok(MeComponentHost {
            host,
            lease: PhantomData,
        })
    }

    fn host_over_admitted_component(
        &mut self,
        options: MeSessionOptions,
        start_time: f64,
    ) -> Result<MeComponentHost<'_>, MeSessionError> {
        let host = build_admitted_host_state(
            Rc::clone(&self.kernel),
            self.pristine.clone(),
            options,
            start_time,
        )?;
        Ok(MeComponentHost {
            host,
            lease: PhantomData,
        })
    }

    fn into_host_over_admitted_component(
        self,
        options: MeSessionOptions,
        start_time: f64,
    ) -> Result<MeComponentHost<'static>, MeSessionError> {
        let host = build_admitted_host_state(self.kernel, self.pristine, options, start_time)?;
        Ok(MeComponentHost {
            host,
            lease: PhantomData,
        })
    }
}

#[cfg(test)]
impl MeRetainedComponent {
    pub(super) fn verification_observable_state(
        &self,
    ) -> (super::lifecycle::MeState, u64, Vec<u64>, Vec<u64>) {
        self.kernel.borrow().verification_observable_state()
    }

    pub(super) fn verification_is_pristine(&self) -> bool {
        self.kernel
            .borrow()
            .verification_matches_snapshot(&self.pristine)
    }

    pub(super) fn verification_fail_next_exit_initialization(&mut self) {
        self.kernel
            .borrow_mut()
            .verification_fail_next_exit_initialization();
    }
}

/// One component-issued, complete batch admission.
///
/// This value is intentionally non-cloneable. It correlates the retained
/// pristine start, checked host options, and complete output grid under one
/// exclusive borrow or owned component, then is consumed into the host and
/// cursor together.
#[must_use]
pub struct MeBatchAdmission<'retained> {
    retained: RetainedBatchLease<'retained>,
    start_time: f64,
    options: MeSessionOptions,
    cursor: MeOutputCursor,
}

enum RetainedBatchLease<'retained> {
    Borrowed(&'retained mut MeRetainedComponent),
    Owned(Box<MeRetainedComponent>),
}

impl RetainedBatchLease<'_> {
    fn component(&self) -> &MeRetainedComponent {
        match self {
            Self::Borrowed(retained) => retained,
            Self::Owned(retained) => retained,
        }
    }
}

impl<'retained> MeBatchAdmission<'retained> {
    fn construct(
        retained: RetainedBatchLease<'retained>,
        options: MeSessionOptions,
    ) -> Result<Self, MeSessionError> {
        let (start_time, cursor) = retained.component().admitted_start_and_cursor(&options)?;
        Ok(Self {
            retained,
            start_time,
            options,
            cursor,
        })
    }

    /// Consume the admission into the sole owner of its host and cursor.
    ///
    /// ```compile_fail,E0599
    /// use rumoca_solver::fmi_me::session::MeBatchAdmission;
    ///
    /// fn cannot_dissolve(admission: MeBatchAdmission<'_>) {
    ///     let (_host, _cursor) = admission.into_host_and_cursor();
    /// }
    /// ```
    pub fn into_batch(self) -> Result<MeAdmittedBatch<'retained>, MeSessionError> {
        let host = match self.retained {
            RetainedBatchLease::Borrowed(retained) => {
                retained.host_over_admitted_component(self.options, self.start_time)?
            }
            RetainedBatchLease::Owned(retained) => {
                (*retained).into_host_over_admitted_component(self.options, self.start_time)?
            }
        };
        Ok(MeAdmittedBatch {
            host,
            cursor: self.cursor,
        })
    }
}

/// The indivisible host/grid owner produced by one batch admission.
#[must_use]
pub struct MeAdmittedBatch<'retained> {
    host: MeComponentHost<'retained>,
    cursor: MeOutputCursor,
}

impl MeAdmittedBatch<'_> {
    #[must_use]
    pub fn component_host(&self) -> &MeComponentHost<'_> {
        &self.host
    }

    #[must_use]
    pub fn is_terminated(&self) -> bool {
        self.host.is_terminated()
    }

    #[must_use]
    pub fn finish(self) -> SimResult {
        self.host.finish()
    }

    pub fn into_session<'retained, 'plugin>(
        self,
        backend: Option<Box<dyn MeIntegratorBackend + 'plugin>>,
    ) -> Result<MeBatchSession<'retained, 'plugin>, MeSessionError>
    where
        Self: 'retained,
    {
        Ok(MeBatchSession {
            session: self.host.into_session(backend)?,
            cursor: self.cursor,
        })
    }
}

/// A batch session permanently paired with its admitted output cursor.
#[must_use]
pub struct MeBatchSession<'retained, 'plugin> {
    session: MeSimulationSession<'retained, 'plugin>,
    cursor: MeOutputCursor,
}

impl MeBatchSession<'_, '_> {
    pub fn run_to_stop(&mut self) -> Result<(), MeSessionError> {
        self.session.run_to_stop(&mut self.cursor)
    }

    #[must_use]
    pub fn finish(self) -> SimResult {
        self.session.finish()
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
    let start_time = pristine.component_time();
    kernel.borrow_mut().reset_to_fmu_state(&pristine)?;
    let attempted = (|| {
        options.admit_start_time(start_time)?;
        build_restored_host_state(&kernel, pristine.clone(), options, start_time)
    })();
    restore_pristine_on_failure(&kernel, &pristine, attempted)
}

fn build_admitted_host_state(
    kernel: Rc<RefCell<SolveMeKernel>>,
    pristine: super::MeFmuState,
    options: MeSessionOptions,
    start_time: f64,
) -> Result<MeHostState, MeSessionError> {
    kernel.borrow_mut().reset_to_fmu_state(&pristine)?;
    let attempted = build_restored_host_state(&kernel, pristine.clone(), options, start_time);
    restore_pristine_on_failure(&kernel, &pristine, attempted)
}

fn build_restored_host_state(
    kernel: &Rc<RefCell<SolveMeKernel>>,
    pristine: super::MeFmuState,
    options: MeSessionOptions,
    start_time: f64,
) -> Result<MeHostState, MeSessionError> {
    let (state_domain, names, meta, input_names) = {
        let borrowed = kernel.borrow();
        let description = borrowed.model_description();
        (
            borrowed.continuous_state_domain(),
            description.output_names.to_vec(),
            description.output_meta.to_vec(),
            description.input_names.to_vec(),
        )
    };
    let state_count = state_domain.len();
    let capacity = trace_capacity(&options, start_time);
    let mut trace = MeTraceRecorder::new(names, meta, state_count, capacity)?;
    let outcome = run_fmi_initialization(
        &mut kernel.borrow_mut(),
        start_time,
        options.records_trace(),
    )?;
    let mut component = MeHostComponent::new(Rc::clone(kernel))?;
    if let Some(values) = &outcome.initial_values {
        trace.record_slice(TraceObservationRole::Initialization, start_time, values)?;
    }
    let initialized = component.finish_initialization(&options, outcome.status)?;
    let (states, next_event_time, termination, terminated) = match initialized {
        FinishedInitialization::Active {
            states,
            next_event_time,
        } => (states, next_event_time, None, false),
        FinishedInitialization::Terminated {
            states,
            termination,
        } => (states, None, Some(termination), true),
    };
    Ok(MeHostState {
        component,
        trace,
        budget: TimeoutBudget::new(options.max_wall_seconds()),
        start_time,
        time: start_time,
        states,
        retained_interval: None,
        next_event_time,
        termination,
        terminated,
        input_names,
        inputs: IndexMap::new(),
        pristine,
        event_streak: EventStreak::empty(),
        usability: Cell::new(None),
        options,
    })
}

fn restore_pristine_on_failure<T>(
    kernel: &RefCell<SolveMeKernel>,
    pristine: &super::MeFmuState,
    attempted: Result<T, MeSessionError>,
) -> Result<T, MeSessionError> {
    let Err(attempted) = attempted else {
        return attempted;
    };
    Err(close_failed_pristine_construction(
        attempted,
        kernel.borrow_mut().reset_to_fmu_state(pristine),
    ))
}

fn close_failed_pristine_construction(
    attempted: MeSessionError,
    restoration: Result<(), super::MeError>,
) -> MeSessionError {
    match restoration {
        Ok(()) => attempted,
        Err(restoration) => MeSessionError::PristineRestoreFailed {
            restoration: Box::new(restoration),
            attempted: Box::new(attempted),
        },
    }
}

/// The two representable initialized host states.
///
/// An active state may schedule another event and never carries termination;
/// a terminated state carries termination and cannot schedule numerical work.
enum FinishedInitialization {
    Active {
        states: Vec<f64>,
        next_event_time: Option<f64>,
    },
    Terminated {
        states: Vec<f64>,
        termination: SimTermination,
    },
}

impl MeHostState {
    fn install_finished_initialization(&mut self, initialized: FinishedInitialization) {
        match initialized {
            FinishedInitialization::Active {
                states,
                next_event_time,
            } => {
                self.states = states;
                self.next_event_time = next_event_time;
                self.termination = None;
                self.terminated = false;
            }
            FinishedInitialization::Terminated {
                states,
                termination,
            } => {
                self.states = states;
                self.next_event_time = None;
                self.termination = Some(termination);
                self.terminated = true;
            }
        }
    }
}

/// An instantiated, initialized FMI 3 ME component awaiting its plugin.
pub struct MeComponentHost<'lease> {
    host: MeHostState,
    lease: PhantomData<&'lease mut MeRetainedComponent>,
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
        let nominals = if self.host.terminated {
            // Terminated has no root-search policy, but FMI 3.0.2 §2.3.8
            // explicitly retains this final-value getter. A concrete backend
            // still needs an exact-width aggregate even though it will never
            // receive a numerical request.
            self.host.read_nominals()?
        } else {
            self.host.component.root_nominals()?.to_vec()
        };
        Ok(MeNumericalSetup::from_checked_host(
            self.host.options.tolerances(),
            nominals,
            initial_step_hint,
        )?)
    }

    #[must_use]
    pub fn state_count(&self) -> usize {
        self.host.state_domain().len()
    }

    #[must_use]
    pub fn is_terminated(&self) -> bool {
        self.host.terminated
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
        let state_domain = self.host.state_domain();
        let state_count = state_domain.len();
        let pristine = self.host.pristine.clone();
        // ME-ZERO-001's arity rule, decided from the linked component's own
        // width and the presence of a plugin. It asks the backend nothing: the
        // thin ME-INT-001 surface has no identity operation to ask
        let selected: Result<Box<dyn MeIntegratorBackend + 'plugin>, MeSessionError> = match backend
        {
            Some(_) if state_domain.is_empty() => Err(MeSessionError::PluginArity {
                state_count,
                mismatch: MePluginArity::RejectsANumericalPlugin,
            }),
            Some(backend) => Ok(backend),
            None if state_domain.is_empty() => Ok(Box::new(TimeOnlyIntegrator::new())),
            None => Err(MeSessionError::PluginArity {
                state_count,
                mismatch: MePluginArity::RequiresANumericalPlugin,
            }),
        };
        let backend = restore_pristine_on_failure(self.host.kernel(), &pristine, selected)?;
        let mut session = MeSimulationSession {
            host: self.host,
            backend,
            lease: PhantomData,
        };
        let started = session.start_plugin();
        restore_pristine_on_failure(session.host.kernel(), &pristine, started)?;
        Ok(session)
    }
}

/// The one FMI 3 ME master algorithm.
pub struct MeSimulationSession<'lease, 'plugin> {
    host: MeHostState,
    backend: Box<dyn MeIntegratorBackend + 'plugin>,
    lease: PhantomData<&'lease mut MeRetainedComponent>,
}

impl MeSimulationSession<'_, '_> {
    // -- public incremental surface ----------------------------------------

    #[must_use]
    pub fn time(&self) -> f64 {
        self.host.time
    }

    #[must_use]
    pub fn state_count(&self) -> usize {
        self.host.state_domain().len()
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
        self.require_usable()?;
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
    /// Causality comes from the model description's input
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
    /// (ME-BUF-001). Only once the component holds the
    /// new value must the host's state vector, the visible input cache, the
    /// retained indicators, and the plugin's history catch up with it, so a
    /// failure from there on ends the session.
    pub fn set_input(&mut self, name: &str, value: f64) -> Result<(), MeSessionError> {
        self.require_live()?;
        if !self.has_input(name) {
            return Err(MeSessionError::Contract {
                reason: format!("'{name}' is not a declared input of this component"),
            });
        }
        let reference: MeValueRef = self
            .host
            .kernel()
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
        let mut kernel = self.host.kernel().borrow_mut();
        kernel.set_time(MeTime::at(self.host.time))?;
        kernel.set_continuous_states(&self.host.states)?;
        Ok(kernel.set_float64(std::slice::from_ref(reference), &[value])?)
    }

    /// Bring every other owner up to the component's new value.
    fn correlate_input(&mut self, name: &str, value: f64) -> Result<(), MeSessionError> {
        let (component, states) = (&self.host.component, &mut self.host.states);
        component
            .kernel()
            .borrow_mut()
            .get_continuous_states(states)?;
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
    pub fn reset(&mut self) -> Result<(), MeSessionError> {
        let pristine_start = self.host.pristine.component_time();
        self.restart_at(pristine_start)
    }

    /// Explicitly restart the pristine component state at a new coordinate.
    /// This is retiming, not FMI reset/replay.
    pub fn retime(&mut self, start_time: f64) -> Result<(), MeSessionError> {
        self.restart_at(start_time)
    }

    fn restart_at(&mut self, start_time: f64) -> Result<(), MeSessionError> {
        self.require_usable()?;
        // Rejecting an inadmissible restart coordinate moves nothing, so it
        // leaves an ordinary live session; everything after it does move the
        // component and the host caches together.
        self.host.options.admit_start_time(start_time)?;
        let restarted = self.restart_session(start_time);
        self.host.guard_mutation(MeSessionLoss::Restart, restarted)
    }

    /// Discard every cache and rebuild the whole lifecycle.
    fn restart_session(&mut self, start_time: f64) -> Result<(), MeSessionError> {
        let pristine = self.host.pristine.clone();
        self.host
            .kernel()
            .borrow_mut()
            .reset_to_fmu_state(&pristine)?;
        self.host.trace.clear();
        self.host.inputs.clear();
        self.host.termination = None;
        self.host.terminated = false;
        self.host.next_event_time = None;
        // The scan workspace's retained indicator vector is refreshed in place
        // by `start_plugin` below before any scan can read it; a terminated
        // restart never scans, so no explicit clear is owed here.
        self.host.retained_interval = None;
        self.host.event_streak.reset();
        self.host.budget = TimeoutBudget::new(self.host.options.max_wall_seconds());

        let outcome = run_fmi_initialization(
            &mut self.host.kernel().borrow_mut(),
            start_time,
            self.host.options.records_trace(),
        )?;
        if let Some(values) = &outcome.initial_values {
            self.host.record_initialization(start_time, values)?;
        }
        let initialized = {
            let (component, options) = (&mut self.host.component, &self.host.options);
            component.finish_initialization(options, outcome.status)?
        };
        self.host.start_time = start_time;
        self.host.time = start_time;
        self.host.install_finished_initialization(initialized);
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
            let resolved = self.resolve_without_advance(yield_time, cursor)?;
            match resolved {
                LoopStep::Finished(outcome) => return Ok(outcome),
                LoopStep::Continue => {}
                LoopStep::Advance => {
                    self.advance_one_accepted_step(yield_time, cursor)?;
                }
            }
        }
    }

    /// Drive the whole defined experiment through the same incremental session.
    ///
    /// There is no second event or integration loop here: the batch call only
    /// supplies the output cursor and the final yield boundary.
    fn run_to_stop(&mut self, cursor: &mut MeOutputCursor) -> Result<(), MeSessionError> {
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
                reason:
                    "the component is Terminated; mutating and active-algorithm calls are not legal"
                        .to_owned(),
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
    /// call is refused.
    fn require_usable(&self) -> Result<(), MeSessionError> {
        match self.host.usability_loss() {
            Some(loss) => Err(MeSessionError::SessionNotReusable { loss }),
            None => Ok(()),
        }
    }

    // -- plugin lifecycle --------------------------------------------------

    fn start_plugin(&mut self) -> Result<(), MeSessionError> {
        if self.host.terminated {
            // A component that terminated during initialization has no live
            // numerical work, so no plugin history is meaningful.
            return Ok(());
        }
        self.restart_plugin_history()
    }

    fn restart_plugin_history(&mut self) -> Result<(), MeSessionError> {
        let point = self.host.checked_point()?;
        let handle = self.host.derivatives().issue_handle();
        self.run_with_derivatives(|backend| backend.initialize(&point, handle))?;
        // A restarted plugin holds no accepted interval, so no left limit can
        // be sampled from one until the next accepted step.
        self.host.retained_interval = None;
        self.refresh_retained_indicators()
    }

    /// Refresh the scan workspace's retained previous-completed-step indicator
    /// vector in place.
    ///
    /// The retained vector and every scan buffer are one construction fact owned
    /// by the root-search aggregate; this fills that vector without allocating and
    /// without replacing it, so the refreshed value and the seed the next scan
    /// reads are one storage.
    fn refresh_retained_indicators(&self) -> Result<(), MeSessionError> {
        self.host.settle_caught_excursion(|| {
            self.host
                .component
                .refresh_retained_indicators(self.host.time, &self.host.states)
        })
    }

    /// Run exactly one plugin call that may evaluate the component.
    ///
    /// This is the whole activation policy of SPEC_0044 §6's retained-handle
    /// ruling, owned once in this order:
    ///
    /// 1. surface any already-latched failure and stop, because it is a
    ///    request the backend made from outside every host wrapper: the
    ///    capability was inactive, so nothing was evaluated and nothing moved,
    ///    and it must be reported rather than cleared;
    /// 2. open the window by guard, for this call and nothing else;
    /// 3. execute the one plugin call;
    /// 4. close the window **infallibly**, including on a backend error or a
    ///    panic;
    /// 5. join the newly latched failure ahead of whatever the backend
    ///    returned, so a generic library error — or an `Ok` — can never hide
    ///    the typed component, discard, or misuse failure that caused it; and
    /// 6. restore the accepted point fallibly under the dual-failure rule,
    ///    because the plugin evaluated derivatives at trial coordinates the
    ///    session never adopted.
    ///
    /// A backend that panics exits through the same steps rather than around
    /// them: the window is closed by its guard, and steps 5 and 6 are the one
    /// caught-excursion close every host excursion shares, so the accepted point
    /// is restored through the same transaction, the session records why it is
    /// no longer callable, and then the **original** payload resumes unwinding.
    /// It is never rendered into prose and never converted into a library error,
    /// so an embedding that catches it sees exactly what its plugin threw and
    /// holds a session that refuses every later evaluation
    fn run_with_derivatives<T>(
        &mut self,
        body: impl FnOnce(&mut dyn MeIntegratorBackend) -> Result<T, MeIntegrationError>,
    ) -> Result<T, MeSessionError> {
        if let Some(stale) = self.host.derivatives().take_error() {
            // An inactive request cannot move the component, so this needs no
            // restoration transaction and must not open a window.
            return Err(latched_failure(stale));
        }
        let Self { host, backend, .. } = self;
        host.settle_caught_excursion(|| {
            let outcome = {
                let window = host.derivatives().activate();
                let outcome = body(backend.as_mut());
                drop(window);
                outcome
            };
            debug_assert!(
                !host.derivatives().is_active(),
                "the activation window closes before any fallible host work"
            );
            match host.derivatives().take_error() {
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
    /// committed before the payload resumes.
    fn sample_states(&self, time: f64, states: &mut [f64]) -> Result<(), MeSessionError> {
        match caught_sample(self.backend.as_ref(), self.host.derivatives(), time, states) {
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
    /// a coincidence to be processed backward.
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
                self.process_event_boundary(event)?;
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
    /// failure.
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
    /// may consume it.
    fn take_one_numerical_step(
        &mut self,
        request: MeAdvanceRequest,
        cursor: &mut MeOutputCursor,
    ) -> Result<(), MeSessionError> {
        let candidate = self.run_with_derivatives(|backend| backend.advance(&request))?;
        let proposal = MeStepProposal::bind(request, candidate)?;
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
    /// still owes the accepted-step sampler contract,
    /// and only the validated [`MeSampledStep`] can be scanned at all
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
        let located = self.scan_interval(&step);
        match located? {
            Some(application) => {
                crate::runtime::hotpath_stats::inc_root_hit();
                self.apply_located_root(&step, &application, cursor)
            }
            None => self.commit_accepted_endpoint(&step, cursor),
        }
    }

    /// Prove the plugin's sampler covers the proposed interval.
    ///
    /// Wrapped in one excursion because the whole validation is component work
    /// away from the accepted point, and in the caught
    /// form because a sampler that unwinds out of the middle of it leaves that
    /// obligation to exactly this transaction.
    /// The proposal carries the consumed request, so the proof that leaves this
    /// call is a proof about the request the session actually served: the
    /// correlation travels through acceptance inside the value rather than
    /// alongside it.
    fn accept_proposal(&self, proposal: MeStepProposal) -> Result<MeAcceptedStep, MeSessionError> {
        self.host.settle_caught_excursion(|| {
            self.host
                .component
                .accept_proposal(self.backend.as_ref(), &self.host.budget, proposal)
        })
    }

    /// Scan and refine one accepted interval.
    ///
    /// The excursion covers the **entire** scan and its refinement, not each
    /// coordinate: the scan deliberately walks the component forward, and it is
    /// the exit — at the first domain change, at a component error, at an
    /// exhausted budget, at a failed reservation — that owes the accepted point
    /// back. A sampler that unwinds after an interior
    /// indicator evaluation already moved the component leaves through the same
    /// obligation, so this is the transaction that catches it, restores, and
    /// only then resumes.
    fn scan_interval(
        &self,
        step: &MeAcceptedStep,
    ) -> Result<Option<MeRootApplication>, MeSessionError> {
        self.host.settle_caught_excursion(|| {
            self.host.component.scan_accepted_interval(
                self.backend.as_ref(),
                &self.host.budget,
                step,
            )
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
        let reached_time_event = self.host.reached_cached_event_time(accepted.time());
        let event_observation_time = reached_time_event.unwrap_or(accepted.time());
        let mut pending = self.capture_endpoint_event_left(event_observation_time)?;
        let reaches_time_event = reached_time_event.is_some();
        if reaches_time_event {
            self.publish_event_left(pending.take())?;
        }
        let completed = self.host.completed_integrator_step()?;
        self.refresh_retained_indicators()?;
        if completed.is_some_and(|completed| completed.terminate_simulation) {
            // Record the final row before the transition to preserve trace
            // ordering. FMI final-value getters remain legal afterwards.
            self.record_final_row()?;
            self.host.terminate_at_current_point(
                "the component requested termination at a completed integrator step",
            )?;
            return Ok(());
        }
        if completed.is_some_and(|completed| completed.enter_event_mode) {
            // The callback originated this event, so its pre-callback left
            // candidate is retained now. Every preceding observation is already
            // durable, so entering Event Mode here cannot strand evidence.
            self.publish_event_left(pending.take())?;
            let event_time = reached_time_event.unwrap_or(self.host.time);
            self.process_event_boundary(event_time)?;
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
        let mut states = try_filled(self.host.state_domain().len(), 0.0, "event-left sample")?;
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
        // coordinate/value pairing.
        self.materialize_event_left(application.left(), event_time)?;
        self.host
            .adopt_point(event_time, application.application().states())?;
        // Only now is the uncompleted trial endpoint discarded. No sample is
        // requested after this point. The plugin re-establishes its history
        // through the retained capability, so this is an activated host call
        // and the application point is restored on both its exits.
        let point = self.host.checked_point()?;
        self.run_with_derivatives(|backend| backend.truncate_reset(&point))?;
        self.host.retained_interval = None;
        let completed = self.host.completed_integrator_step()?;
        if completed.is_some_and(|completed| completed.terminate_simulation) {
            self.host
                .record_from_component(TraceObservationRole::Nominal, self.host.time)?;
            self.host.terminate_at_current_point(
                "the component requested termination at a located event",
            )?;
            return Ok(());
        }
        self.enter_event_mode_and_settle(event_time)
    }

    /// Enter Event Mode at a coordinate the session already stands on.
    ///
    /// This runs *after* the completed-step callback, so it materializes no
    /// observation of its own: the left evidence of an endpoint event was
    /// already observed and published pre-callback by
    /// [`Self::commit_accepted_endpoint`], and an event the session stands on
    /// without an intervening accepted step has no retained interval to sample
    fn process_event_boundary(&mut self, event_time: f64) -> Result<(), MeSessionError> {
        self.host.record_event_streak(event_time)?;
        self.enter_event_mode_and_settle(event_time)
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
    fn enter_event_mode_and_settle(&mut self, event_time: f64) -> Result<(), MeSessionError> {
        let settled = self.settle_event_mode(event_time);
        self.host
            .guard_mutation(MeSessionLoss::EventRefresh, settled)
    }

    fn settle_event_mode(&mut self, event_time: f64) -> Result<(), MeSessionError> {
        // The session takes the exact event coordinate before the lifecycle
        // transition, so Event Mode is entered at the point the host names
        // rather than moving the component behind the host's back
        self.host.adopt_time(event_time)?;
        let discrete = self.host.run_event_mode(event_time)?;
        if let Some(termination) = discrete.terminate_simulation {
            // FMI: `terminateSimulation` ends the iteration immediately and
            // transitions to Terminated without another numerical request, even
            // when `discreteStatesNeedUpdate` is also true. The settled row is
            // read here, while Event Mode still makes the getters legal.
            self.host.record_settled(event_time)?;
            self.host.terminate_component(Some(termination))?;
            return Ok(());
        }
        self.host
            .kernel()
            .borrow_mut()
            .enter_continuous_time_mode()?;
        self.refresh_after_event(
            discrete.values_of_continuous_states_changed,
            discrete.nominals_of_continuous_states_changed,
            event_time,
        )?;
        // A superdense chain at one coordinate publishes exactly one settled
        // row, at the end of the chain. Publishing per iteration would mint a
        // second settled row at one coordinate, which SPEC_0050 does not
        // authorize as a replacement.
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
    /// candidate is generated at all.
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
    /// them on the pre-event model (SPEC_0044 §6's atomic-refresh rule). The
    /// surrounding Event Mode transaction records the loss
    /// for the whole transition, this step included.
    fn refresh_after_event(
        &mut self,
        values_changed: bool,
        nominals_changed: bool,
        event_time: f64,
    ) -> Result<(), MeSessionError> {
        let refreshed_states = if values_changed {
            let mut states = try_filled(
                self.host.state_domain().len(),
                0.0,
                "refreshed continuous states",
            )?;
            self.host
                .kernel()
                .borrow_mut()
                .get_continuous_states(&mut states)?;
            Some(states)
        } else {
            None
        };
        let refreshed_nominals = if nominals_changed {
            Some(self.host.read_nominals()?)
        } else {
            None
        };
        // Both refreshes commit only after both reads succeeded, so a failed
        // read or an invalid nominal leaves no partially refreshed host or
        // plugin state. The state adoption goes first because it is the one
        // step that can still fail: a failed component move restores the
        // previous point and leaves the policy untouched.
        self.host.adopt_owned(event_time, refreshed_states)?;
        if let Some(nominals) = refreshed_nominals {
            self.host.component.refresh_root_nominals(nominals)?;
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
        let mut states = try_filled(
            self.host.state_domain().len(),
            0.0,
            "soft observation sample",
        )?;
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
pub(super) struct VerificationTerminalGetterVectors {
    pub(super) states: Vec<f64>,
    pub(super) nominals: Vec<f64>,
    pub(super) derivatives: Vec<f64>,
    pub(super) directional: Vec<f64>,
    pub(super) indicators: Vec<f64>,
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
    pub(super) fn verification_component_point(&self) -> (u64, Vec<u64>) {
        let (_, time, states, _) = self.host.kernel().borrow().verification_observable_state();
        (time, states)
    }

    pub(super) fn verification_component_snapshot(&self) -> super::MeFmuState {
        self.host.kernel().borrow_mut().fmu_state()
    }

    pub(super) fn verification_matches_component_snapshot(
        &self,
        snapshot: &super::MeFmuState,
    ) -> bool {
        self.host
            .kernel()
            .borrow()
            .verification_matches_snapshot(snapshot)
    }

    /// Whether the component and not just the host flag reached FMI
    /// Terminated.  This is deliberately a boolean: the private lifecycle type
    /// does not become part of the session API merely to support an ablation.
    pub(super) fn verification_component_is_terminated(&self) -> bool {
        self.host
            .kernel()
            .borrow()
            .verification_observable_state()
            .0
            == super::lifecycle::MeState::Terminated
    }

    /// Read the complete §2.3.8 numerical getter family from the real terminal
    /// component retained by a session.
    pub(super) fn verification_terminal_getter_vectors(&self) -> VerificationTerminalGetterVectors {
        let mut kernel = self.host.kernel().borrow_mut();
        let state_count = self.host.state_domain().len();
        let mut states = vec![f64::NAN; state_count];
        let mut nominals = vec![f64::NAN; state_count];
        let mut derivatives = vec![f64::NAN; state_count];
        let seed = vec![1.0; state_count];
        let mut directional = vec![f64::NAN; state_count];
        let indicator_count = kernel.root_scan_shape().indicator_width().len();
        let mut indicators = vec![f64::NAN; indicator_count];
        kernel
            .get_continuous_states(&mut states)
            .expect("terminal continuous states remain readable");
        kernel
            .get_nominals_of_continuous_states(&mut nominals)
            .expect("terminal state nominals remain readable");
        kernel
            .get_continuous_state_derivatives(&mut derivatives)
            .expect("terminal state derivatives remain readable");
        let knowns = kernel
            .continuous_state_value_references()
            .and_then(|references| kernel.directional_known_batch(references))
            .expect("terminal state known batch constructs");
        let unknowns = kernel
            .continuous_state_derivative_value_references()
            .and_then(|references| kernel.directional_unknown_batch(references))
            .expect("terminal state-derivative unknown batch constructs");
        kernel
            .get_directional_derivative(&unknowns, &knowns, &seed, &mut directional)
            .expect("terminal directional derivatives remain readable");
        kernel
            .get_event_indicators(&mut indicators)
            .expect("terminal event indicators remain readable");
        VerificationTerminalGetterVectors {
            states,
            nominals,
            derivatives,
            directional,
            indicators,
        }
    }

    /// Whether the retained derivative capability is currently reachable.
    pub(super) fn verification_capability_is_active(&self) -> bool {
        self.host.derivatives().is_active()
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
            .kernel()
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
/// actually has one. These are the *host's* buffers,
/// so exhaustion is [`MeSessionError::Allocation`] and never a component
/// failure.
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
/// be an event.
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

    #[test]
    fn pristine_restoration_failure_outranks_and_retains_the_attempted_failure() {
        let failure = close_failed_pristine_construction(
            MeSessionError::Options {
                reason: "attempted construction".to_owned(),
            },
            Err(super::super::MeError::Contract {
                reason: "restoration".to_owned(),
            }),
        );
        let MeSessionError::PristineRestoreFailed {
            restoration,
            attempted,
        } = failure
        else {
            panic!("restoration must be the outer typed failure");
        };
        assert!(matches!(
            *restoration,
            super::super::MeError::Contract { .. }
        ));
        assert!(matches!(*attempted, MeSessionError::Options { .. }));
    }
}
