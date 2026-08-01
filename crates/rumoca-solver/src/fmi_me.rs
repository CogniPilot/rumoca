//! The internal FMI 3.0 Model Exchange contract (SPEC_0038 §Internal Solver
//! Boundary).
//!
//! `SolveProblem` stays compiler IR. It is projected exactly once, by
//! [`SolveMeKernel`], into an FMI 3 ME component kernel. Integrators — the
//! rk-like session today, Diffsol/BDF in phase 2 — reach the model only
//! through [`ModelExchangeKernel`]. They MUST NOT inspect Solve rows,
//! layouts, opcodes, events, or private runtime objects.
//!
//! # Operation map
//!
//! Every operation on [`ModelExchangeKernel`] mirrors one FMI 3.0 ME entry
//! point. Where rumoca needs something FMI 3.0 does not name, the doc comment
//! says so explicitly and the report calls it an extension; there is no silent
//! second interface.
//!
//! | Kernel operation | FMI 3.0 ME |
//! |---|---|
//! | [`SolveMeKernel::instantiate`] | `fmi3InstantiateModelExchange` |
//! | [`ModelExchangeKernel::model_description`] | model description + `fmi3GetNumberOfContinuousStates` / `fmi3GetNumberOfEventIndicators` |
//! | [`ModelExchangeKernel::enter_initialization_mode`] | `fmi3EnterInitializationMode` |
//! | [`ModelExchangeKernel::exit_initialization_mode`] | `fmi3ExitInitializationMode` |
//! | [`ModelExchangeKernel::enter_event_mode`] | `fmi3EnterEventMode` |
//! | [`ModelExchangeKernel::update_discrete_states`] | `fmi3UpdateDiscreteStates` |
//! | [`ModelExchangeKernel::enter_continuous_time_mode`] | `fmi3EnterContinuousTimeMode` |
//! | [`ModelExchangeKernel::set_time`] | `fmi3SetTime` |
//! | [`ModelExchangeKernel::set_continuous_states`] | `fmi3SetContinuousStates` |
//! | [`ModelExchangeKernel::get_continuous_states`] | `fmi3GetContinuousStates` |
//! | [`ModelExchangeKernel::get_nominals_of_continuous_states`] | `fmi3GetNominalsOfContinuousStates` |
//! | [`ModelExchangeKernel::get_continuous_state_derivatives`] | `fmi3GetContinuousStateDerivatives` |
//! | [`ModelExchangeKernel::get_event_indicators`] | `fmi3GetEventIndicators` |
//! | [`ModelExchangeKernel::record_outputs`] / [`ModelExchangeKernel::get_outputs`] | batched `fmi3GetFloat64` |
//! | [`ModelExchangeKernel::value_reference`] / [`ModelExchangeKernel::set_float64`] | model description + batched `fmi3SetFloat64` |
//! | [`ModelExchangeKernel::fmu_state`] / [`ModelExchangeKernel::reset_to_fmu_state`] | `fmi3GetFMUState` / `fmi3Reset` + `fmi3SetFMUState` |
//! | [`ModelExchangeKernel::terminate`] | `fmi3Terminate` |
//!
//! ## Extensions beyond FMI 3.0 ME, and why
//!
//! - [`ModelExchangeKernel::project_continuous_states`]: FMI 3.0 forbids the
//!   FMU from changing continuous states in Continuous-Time Mode. Rumoca's
//!   index-reduced DAEs carry a constraint manifold the accepted point must be
//!   projected onto. The operation is separate from
//!   `completed_integrator_step` so the host sees the state change explicitly.
//! - [`ModelExchangeKernel::next_event_stop`]: FMI reports `nextEventTime`
//!   once per `fmi3UpdateDiscreteStates`. Rumoca's scheduled-event set is
//!   state dependent, so the host re-asks each outer step.
//! - [`ModelExchangeKernel::event_indicator_crossings`],
//!   [`ModelExchangeKernel::capture_pre_event_state`], and
//!   [`ModelExchangeKernel::arm_state_event`]: the FMU owns the relation
//!   memory an indicator sign change must write, and the pre-event (`pre(v)`)
//!   vector rumoca applies comes from the integrator's dense output rather
//!   than from the FMU's own last accepted point.
//! - [`MeTime::event_boundary`]: FMI 3.0 §3 (Model Exchange) requires the
//!   integrator not to step across a known event instant; rumoca additionally
//!   needs the FMU to evaluate the *left limit* of that instant, so the
//!   boundary travels with `fmi3SetTime`.
//! - [`ModelExchangeKernel::completed_integrator_step`]: occupies the
//!   lifecycle position of `fmi3CompletedIntegratorStep` but is **not** that
//!   call. It omits the `noSetFMUStatePriorToCurrentPoint` in-param (rumoca
//!   never rolls a component back to an earlier point) and both mandated
//!   out-params: `enterEventMode` (this host detects events from the event
//!   indicators it already reads, so the component never requests event mode
//!   here) and `terminateSimulation` (termination is reported by
//!   [`ModelExchangeKernel::update_discrete_states`] instead). They are absent
//!   rather than hardcoded so no host can mistake an unimplemented answer for
//!   a measured one; add them when a host needs them.
//! - [`MeStepCompletion::accepted_derivatives`]: the FSAL stage an explicit
//!   Runge-Kutta method already computed at the accepted point, handed back so
//!   the FMU does not recompute it.
//! - [`MeStepCompletion::AtStateEvent`]: FMI has no way for a host to say that
//!   the step it just completed lands *on* a located state event. Rumoca needs
//!   it because the component drops different caches in that case.
//! - [`MeEventEntry`]: `fmi3EnterEventMode` takes no arguments in FMI 3.0.
//!   Rumoca's component needs the event `cause` (state event versus its own
//!   scheduled instant), the `event_time` the host located, and the `horizon`
//!   it will not integrate past before its next output point, because the
//!   component clamps its right-limit evaluation to that horizon.
//! - [`ModelExchangeKernel::initial_observations`]: the initial event
//!   iteration can produce several observation points at and after
//!   `startTime`. FMI exposes no such queue; a host reads them to emit the
//!   same pre/post-event rows an FMI importer would have sampled itself.
//! - [`MeDiscreteStates::time`]: FMI's `fmi3UpdateDiscreteStates` returns no
//!   time. Rumoca's initial event boundary may advance the component's own
//!   time, so the host must read back where it landed.
//! - [`ModelExchangeKernel::observe`]: a refresh of the component's observable
//!   (algebraic) vector at the current time, returning that observation point.
//!   It is *not* `fmi3GetFMUState`: it snapshots no discrete state and cannot
//!   be restored. Only [`ModelExchangeKernel::fmu_state`] and
//!   [`ModelExchangeKernel::reset_to_fmu_state`] map to the FMU-state calls.
//! - [`ModelExchangeKernel::max_step_size`]: rumoca's delay channels bound the
//!   next step. FMI 3.0 ME has no counterpart at all — a host that ignores it
//!   would step over delay history.

mod kernel;
mod no_state;

pub use kernel::SolveMeKernel;
pub use no_state::MeNoStateSession;

use crate::solver::{SimTermination, SimVariableMeta};

/// The checked-kernel source an ME component is instantiated from.
///
/// This is the one place the FMI boundary names compiler IR: SPEC_0038
/// requires `SolveProblem` to be projected *once* into an ME component.
/// Hosts receive it as an opaque handle and can only hand it to
/// [`SolveMeKernel::instantiate`].
#[derive(Clone, Copy)]
pub struct MeModelSource<'a>(&'a rumoca_ir_solve::SolveModel);

impl<'a> MeModelSource<'a> {
    #[must_use]
    pub fn new(model: &'a rumoca_ir_solve::SolveModel) -> Self {
        Self(model)
    }

    pub(crate) fn model(self) -> &'a rumoca_ir_solve::SolveModel {
        self.0
    }
}

impl<'a> From<&'a rumoca_ir_solve::SolveModel> for MeModelSource<'a> {
    fn from(model: &'a rumoca_ir_solve::SolveModel) -> Self {
        Self::new(model)
    }
}

/// Failures an ME component reports to its host.
///
/// The variants are the ones the runtime actually distinguishes; a host maps
/// them onto its own error type without inspecting runtime internals.
#[derive(Debug, thiserror::Error)]
pub enum MeError {
    /// The model declares no continuous states, so no Model Exchange
    /// component with an integrator can be instantiated for it. Hosts route
    /// this to their zero-state execution path rather than treating it as a
    /// failure.
    #[error("empty system: no state equations to simulate")]
    NoContinuousStates,

    /// The component cannot represent this model at all.
    #[error("{reason}")]
    UnsupportedModel { reason: String },

    /// A model evaluation failed. `message` is already rendered.
    #[error("{message}")]
    Evaluation { message: String },

    /// A state derivative evaluated to a non-finite value.
    #[error("non-finite derivative evaluation for state '{state_name}'")]
    NonFiniteDerivative { state_name: String },

    /// The host used the component outside its contract, or the component
    /// produced a vector whose shape contradicts its own model description.
    #[error("{reason}")]
    Contract { reason: String },

    /// A Modelica `assert` failed inside an event action.
    #[error("Modelica assert failed at t={time:.9}: {message}")]
    Assertion { time: f64, message: String },

    /// A host or component buffer could not be reserved.
    #[error("{context} allocation failed for {entries} entries")]
    Allocation {
        context: &'static str,
        entries: usize,
    },
}

impl From<crate::runtime::solve_ops::RuntimeSolveError> for MeError {
    fn from(value: crate::runtime::solve_ops::RuntimeSolveError) -> Self {
        use crate::runtime::solve_ops::RuntimeSolveError as Runtime;
        match value {
            Runtime::SolveIr { message, span } => Self::Evaluation {
                message: match span {
                    Some(span) => format!("{message} @ {span:?}"),
                    None => message,
                },
            },
            Runtime::UnsupportedModel { reason } => Self::UnsupportedModel { reason },
            unassignable @ Runtime::RefreshTargetUnassignable { .. } => Self::Evaluation {
                message: unassignable.to_string(),
            },
            Runtime::NonFiniteDerivative { state_name } => Self::NonFiniteDerivative { state_name },
            non_finite @ Runtime::NonFiniteValue { .. } => Self::Evaluation {
                message: non_finite.to_string(),
            },
        }
    }
}

impl From<rumoca_eval_solve::EvalSolveError> for MeError {
    fn from(value: rumoca_eval_solve::EvalSolveError) -> Self {
        Self::Evaluation {
            message: value.to_string(),
        }
    }
}

/// `fmi3InstantiateModelExchange` arguments plus the tolerance and horizon
/// FMI 3.0 passes at `fmi3EnterInitializationMode`.
#[derive(Debug, Clone)]
pub struct MeInstanceConfig {
    /// FMI `instanceName`; also labels the component's eval-trace snapshot.
    pub instance_name: &'static str,
    /// FMI `tolerance` (`toleranceDefined = true`).
    pub tolerance: f64,
    /// FMI `startTime`.
    pub start_time: f64,
    /// FMI `stopTime` (`stopTimeDefined = true`).
    pub stop_time: f64,
}

/// The subset of the FMI model description an ME host needs.
#[derive(Debug, Clone, Copy)]
pub struct MeModelDescription<'a> {
    pub continuous_state_count: usize,
    pub event_indicator_count: usize,
    /// Names of every variable the component exposes through
    /// [`ModelExchangeKernel::get_outputs`], in value-reference order.
    pub output_names: &'a [String],
    /// Names of every writable input variable.
    pub input_names: &'a [String],
    /// Per-output metadata (causality/variability) for result reporting.
    pub output_meta: &'a [SimVariableMeta],
}

/// A resolved FMI value reference. Opaque: only the component interprets it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MeValueRef(pub(crate) usize);

/// `fmi3SetTime`, carrying the event instant the integrator is stepping
/// toward so the component can evaluate that instant's left limit.
#[derive(Debug, Clone, Copy)]
pub struct MeTime {
    pub time: f64,
    /// An upcoming event instant the integrator will not step across. When
    /// `time` reaches or passes it, the component evaluates the left limit of
    /// the boundary instead.
    pub event_boundary: Option<f64>,
}

impl MeTime {
    #[must_use]
    pub fn at(time: f64) -> Self {
        Self {
            time,
            event_boundary: None,
        }
    }

    #[must_use]
    pub fn new(time: f64, event_boundary: Option<f64>) -> Self {
        Self {
            time,
            event_boundary,
        }
    }
}

/// An event-indicator sign change the component classified.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MeIndicatorCrossing {
    /// Index into the event-indicator vector.
    pub index: usize,
    /// The value the component's relation buffer takes after the crossing.
    /// Hosts only report it in traces.
    pub post_indicator_value: f64,
}

/// Why the host is entering Event Mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MeEventCause {
    /// The integrator located an event-indicator sign change.
    StateEvent,
    /// The instant the component itself scheduled through
    /// [`ModelExchangeKernel::next_event_stop`].
    TimeEvent,
}

/// `fmi3EnterEventMode` arguments.
#[derive(Debug, Clone, Copy)]
pub struct MeEventEntry {
    pub cause: MeEventCause,
    /// The instant the event is applied at.
    pub event_time: f64,
    /// The upper bound the integrator will not pass before its next output
    /// point; the component clamps its right-limit evaluation to it.
    pub horizon: f64,
}

/// `fmi3UpdateDiscreteStates` outputs.
#[derive(Debug, Clone)]
pub struct MeDiscreteStates {
    /// FMI `discreteStatesNeedUpdate`. The rumoca component runs the whole
    /// discrete-state fixed point inside one call, so this is always `false`;
    /// a conforming host loop still terminates correctly.
    pub discrete_states_need_update: bool,
    /// FMI `terminateSimulation`, carrying the Modelica `terminate()` payload.
    pub terminate_simulation: Option<SimTermination>,
    /// FMI `valuesOfContinuousStatesChanged`. Always `true`: every rumoca
    /// event boundary re-solves the algebraic system, so the component cannot
    /// currently tell a host that the continuous states came back unchanged.
    /// A host that re-reads them unconditionally is therefore always correct,
    /// and one that trusts a `false` would never see one.
    pub values_of_continuous_states_changed: bool,
    /// The component's time after the boundary. Extension: see the module
    /// docs — FMI's `fmi3UpdateDiscreteStates` returns no time.
    pub time: f64,
    //
    // FMI's `nominalsOfContinuousStatesChanged` is deliberately absent:
    // rumoca's nominals are fixed at instantiation, so there is nothing for
    // the component to report and nothing for a host to act on. FMI's
    // `nextEventTime` is absent too — the component's scheduled-event set is
    // state dependent, so a host asks `next_event_stop` each outer step
    // instead of being told once here.
}

/// The next instant the component wants the integrator to stop at.
#[derive(Debug, Clone, Copy)]
pub struct MeEventStop {
    pub time: f64,
    /// `false` when `time` is just the requested horizon.
    pub is_event: bool,
}

/// [`ModelExchangeKernel::completed_integrator_step`] arguments.
///
/// Extension: see the module docs for how this differs from
/// `fmi3CompletedIntegratorStep`.
#[derive(Debug, Clone, Copy)]
pub enum MeStepCompletion<'a> {
    /// A plain accepted continuous step.
    Continuous {
        /// The FSAL stage derivative at the accepted point, if the method
        /// produced one; the component may reuse it instead of recomputing.
        accepted_derivatives: Option<&'a [f64]>,
    },
    /// The accepted step ends on a located state event. The component drops
    /// every cached evaluation because Event Mode will change discrete state.
    AtStateEvent,
}

/// An observation point: the component's refreshed observable state.
///
/// Produced by [`ModelExchangeKernel::observe`] and by the initial-event
/// iteration; consumed by the batched output getters. Opaque to hosts, which
/// is what makes `fmi3GetFloat64` a batched read rather than a row query.
#[derive(Debug, Clone)]
pub struct MeObservation {
    pub(crate) time: f64,
    pub(crate) solver_y: Vec<f64>,
    pub(crate) parameters: Vec<f64>,
}

impl MeObservation {
    #[must_use]
    pub fn time(&self) -> f64 {
        self.time
    }
}

/// An opaque saved component state (`fmi3GetFMUState`).
#[derive(Debug, Clone)]
pub struct MeFmuState {
    pub(crate) parameters: Vec<f64>,
}

/// Column-major output buffer a host accumulates batched reads into.
///
/// One column per model output, one row per recorded sample. Kept on the host
/// side of the boundary so the component never owns result storage.
#[derive(Debug, Default)]
pub struct MeOutputSeries {
    columns: Vec<Vec<f64>>,
}

impl MeOutputSeries {
    /// Reserve `outputs` columns of `samples` rows without ever aborting on a
    /// failed reservation.
    pub fn with_capacity(outputs: usize, samples: usize) -> Result<Self, MeError> {
        let mut columns: Vec<Vec<f64>> = Vec::new();
        columns
            .try_reserve(outputs)
            .map_err(|_| MeError::Allocation {
                context: "output series",
                entries: outputs,
            })?;
        for _ in 0..outputs {
            let mut column = Vec::new();
            column
                .try_reserve(samples)
                .map_err(|_| MeError::Allocation {
                    context: "output samples",
                    entries: samples,
                })?;
            columns.push(column);
        }
        Ok(Self { columns })
    }

    pub(crate) fn columns_mut(&mut self) -> &mut [Vec<f64>] {
        &mut self.columns
    }

    #[must_use]
    pub fn into_columns(self) -> Vec<Vec<f64>> {
        self.columns
    }
}

/// The internal FMI 3.0 Model Exchange contract every rumoca integrator uses.
///
/// See the module docs for the per-operation FMI mapping and the extensions.
pub trait ModelExchangeKernel {
    // -- model description ------------------------------------------------

    fn model_description(&self) -> MeModelDescription<'_>;

    /// `fmi3GetNominalsOfContinuousStates`.
    fn get_nominals_of_continuous_states(&self, nominals: &mut [f64]) -> Result<(), MeError>;

    /// Resolve an input variable name to its value reference. Model
    /// description lookup, not a runtime query.
    fn value_reference(&self, name: &str) -> Option<MeValueRef>;

    // -- lifecycle --------------------------------------------------------

    /// `fmi3EnterInitializationMode`.
    fn enter_initialization_mode(&mut self) -> Result<(), MeError>;

    /// `fmi3ExitInitializationMode`. Leaves the component in Event Mode with
    /// the initialization system settled.
    fn exit_initialization_mode(&mut self) -> Result<(), MeError>;

    /// `fmi3EnterEventMode`.
    fn enter_event_mode(&mut self, entry: MeEventEntry) -> Result<(), MeError>;

    /// `fmi3UpdateDiscreteStates`.
    fn update_discrete_states(&mut self) -> Result<MeDiscreteStates, MeError>;

    /// `fmi3EnterContinuousTimeMode`.
    fn enter_continuous_time_mode(&mut self) -> Result<(), MeError>;

    /// `fmi3Terminate`. Emits the component's eval-trace snapshot.
    fn terminate(&mut self) -> Result<(), MeError>;

    // -- time and continuous state ---------------------------------------

    /// `fmi3SetTime`.
    fn set_time(&mut self, time: MeTime);

    /// `fmi3SetContinuousStates`.
    fn set_continuous_states(&mut self, states: &[f64]) -> Result<(), MeError>;

    /// `fmi3GetContinuousStates`.
    fn get_continuous_states(&self, states: &mut [f64]) -> Result<(), MeError>;

    /// `fmi3GetContinuousStateDerivatives`.
    fn get_continuous_state_derivatives(&self, derivatives: &mut Vec<f64>) -> Result<(), MeError>;

    /// `fmi3GetEventIndicators`.
    fn get_event_indicators(&self, indicators: &mut Vec<f64>) -> Result<(), MeError>;

    /// Project `states` onto the model's constraint manifold at the current
    /// time and write the result back. Returns whether anything moved.
    ///
    /// Extension: see the module docs.
    fn project_continuous_states(&mut self, states: &mut [f64]) -> Result<bool, MeError>;

    /// Accept the step the host just took. Extension: see the module docs —
    /// this occupies the lifecycle position of `fmi3CompletedIntegratorStep`
    /// but omits its in-param and both of its out-params.
    fn completed_integrator_step(&mut self, step: MeStepCompletion<'_>) -> Result<(), MeError>;

    /// The largest step the component's internal history allows next, if any.
    ///
    /// Extension: see the module docs.
    fn max_step_size(&self) -> Option<f64>;

    // -- events -----------------------------------------------------------

    /// The next instant at or before `horizon` the component must be stopped
    /// at. Extension: see the module docs.
    fn next_event_stop(&mut self, horizon: f64) -> Result<MeEventStop, MeError>;

    /// Classify the event-indicator sign changes between two evaluations.
    /// Extension: see the module docs.
    fn event_indicator_crossings(
        &self,
        before: &[f64],
        after: &[f64],
        crossings: &mut Vec<MeIndicatorCrossing>,
    ) -> Result<(), MeError>;

    /// Latch the component's `pre(v)` vector from the currently set time and
    /// continuous states. Extension: see the module docs.
    fn capture_pre_event_state(&mut self) -> Result<(), MeError>;

    /// Hand the component the crossings the integrator located, so the next
    /// `enter_event_mode` writes the matching relation buffers.
    fn arm_state_event(&mut self, crossings: &[MeIndicatorCrossing]) -> Result<(), MeError>;

    // -- variable access (batched) ---------------------------------------

    /// Refresh the component's observable state at the current time and
    /// return that observation point (`fmi3GetFMUState`).
    fn observe(&self) -> Result<MeObservation, MeError>;

    /// Batched `fmi3GetFloat64` over every output, appended as one row of
    /// `series` and stamped `sample_time`.
    fn record_outputs(
        &self,
        observation: &MeObservation,
        sample_time: f64,
        series: &mut MeOutputSeries,
    ) -> Result<(), MeError>;

    /// Batched `fmi3GetFloat64` over every output into a flat host buffer.
    fn get_outputs(
        &self,
        observation: &MeObservation,
        sample_time: f64,
        values: &mut Vec<f64>,
    ) -> Result<(), MeError>;

    /// The observation points the initial-event iteration produced.
    fn initial_observations(&self) -> &[MeObservation];

    /// Batched `fmi3SetFloat64`.
    fn set_float64(&mut self, refs: &[MeValueRef], values: &[f64]) -> Result<(), MeError>;

    // -- saved state ------------------------------------------------------

    /// `fmi3GetFMUState`.
    fn fmu_state(&self) -> MeFmuState;

    /// `fmi3Reset` + `fmi3SetFMUState` + `fmi3SetTime` +
    /// `fmi3SetContinuousStates`, composed because rumoca's reset also
    /// rewinds the component's delay history to `start_time`.
    fn reset_to_fmu_state(
        &mut self,
        saved: &MeFmuState,
        start_time: f64,
        states: &[f64],
    ) -> Result<(), MeError>;

    /// Extend the component's horizon and reschedule its event set from
    /// `from_time`.
    ///
    /// Extension: FMI 3.0 fixes `stopTime` at
    /// `fmi3EnterInitializationMode`; a live rumoca session may run past it.
    fn extend_stop_time(&mut self, from_time: f64, stop_time: f64);
}

/// Whether two consecutive event-indicator values bracket a crossing.
///
/// Pure predicate on indicator values; hosts localizing a root need it and it
/// carries no model state.
#[must_use]
pub fn event_indicator_crossed(before: f64, after: f64, tolerance: f64) -> bool {
    crate::runtime::solve_ops::root_value_crossed(before, after, tolerance)
}
