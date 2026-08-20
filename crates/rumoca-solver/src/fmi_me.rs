//! The internal FMI 3.0 Model Exchange contract (SPEC_0038 §Internal Solver
//! Boundary).
//!
//! `SolveProblem` stays compiler IR. It is projected exactly once, by
//! [`SolveMeKernel`], into an FMI 3 ME component kernel. Integrators — the
//! rk-like session today, Diffsol/BDF in phase 2 — reach the model only
//! through [`SolveMeKernel`]. They MUST NOT inspect Solve rows,
//! layouts, opcodes, events, or private runtime objects.
//!
//! # Transitional operation map
//!
//! The standard operations below map to FMI 3.0 ME entry points. Remaining
//! non-standard operations are concrete host/kernel composition points rather
//! than a one-implementation public trait.
//!
//! | Kernel operation | FMI 3.0 ME |
//! |---|---|
//! | `SolveMeKernel::instantiate` | `fmi3InstantiateModelExchange` |
//! | `SolveMeKernel::model_description` | model description + `fmi3GetNumberOfContinuousStates` / `fmi3GetNumberOfEventIndicators` |
//! | `SolveMeKernel::enter_initialization_mode` | `fmi3EnterInitializationMode` |
//! | `SolveMeKernel::exit_initialization_mode` | `fmi3ExitInitializationMode` |
//! | `SolveMeKernel::enter_event_mode` | `fmi3EnterEventMode` |
//! | `SolveMeKernel::update_discrete_states` | `fmi3UpdateDiscreteStates` |
//! | `SolveMeKernel::enter_continuous_time_mode` | `fmi3EnterContinuousTimeMode` |
//! | `SolveMeKernel::set_time` | `fmi3SetTime` |
//! | `SolveMeKernel::set_continuous_states` | `fmi3SetContinuousStates` |
//! | `SolveMeKernel::get_continuous_states` | `fmi3GetContinuousStates` |
//! | `SolveMeKernel::get_nominals_of_continuous_states` | `fmi3GetNominalsOfContinuousStates` |
//! | `SolveMeKernel::get_continuous_state_derivatives` | `fmi3GetContinuousStateDerivatives` |
//! | `SolveMeKernel::get_directional_derivative` | `fmi3GetDirectionalDerivative` |
//! | `SolveMeKernel::get_event_indicators` | `fmi3GetEventIndicators` |
//! | `SolveMeKernel::get_outputs` | batched `fmi3GetFloat64` |
//! | `SolveMeKernel::value_reference` / `SolveMeKernel::set_float64` | model description + batched `fmi3SetFloat64` |
//! | `SolveMeKernel::fmu_state` / `SolveMeKernel::reset_to_fmu_state` | `fmi3GetFMUState` / `fmi3Reset` + `fmi3SetFMUState` |
//! | `SolveMeKernel::terminate` | `fmi3Terminate` |
//!
//! ## Non-standard operations scheduled for removal from this surface
//!
//! - `SolveMeKernel::project_continuous_states`: FMI 3.0 forbids the
//!   FMU from changing continuous states in Continuous-Time Mode. Rumoca's
//!   index-reduced DAEs carry a constraint manifold the accepted point must be
//!   projected onto. The operation is separate from
//!   `completed_integrator_step` so the host sees the state change explicitly.
//! - [`MeTime::event_boundary`]: FMI 3.0 §3 (Model Exchange) requires the
//!   integrator not to step across a known event instant; rumoca additionally
//!   needs the FMU to evaluate the *left limit* of that instant, so the
//!   boundary travels with `fmi3SetTime`.
//! - [`MeEventEntry`]: `fmi3EnterEventMode` takes no arguments in FMI 3.0.
//!   Rumoca's component needs the event `cause` (state event versus its own
//!   scheduled instant), the `event_time` the host located, and the `horizon`
//!   it will not integrate past before its next output point, because the
//!   component clamps its right-limit evaluation to that horizon.
//! - `SolveMeKernel::observe`: a refresh of the component's observable
//!   (algebraic) vector at the current time, returning that observation point.
//!   It is *not* `fmi3GetFMUState`: it snapshots no discrete state and cannot
//!   be restored. Only `SolveMeKernel::fmu_state` and
//!   `SolveMeKernel::reset_to_fmu_state` map to the FMU-state calls.
//! - `SolveMeKernel::max_step_size`: rumoca's delay channels bound the
//!   next step. FMI 3.0 ME has no counterpart at all — a host that ignores it
//!   would step over delay history.
//! - [`MeStage`]: FMI 3.0 reports one undifferentiated `fmi3Error`. rumoca's
//!   MSL harness buckets every failure by the sub-stage that raised it, so the
//!   component mints that stage where the failure happens rather than letting a
//!   host re-derive it from rendered text.

pub mod driver;
pub mod integrator;
mod kernel;
pub(crate) mod lifecycle;
/// Host-private root policy. SPEC_0044 §6 makes the scan/location policy, the
/// root application, and the domain classification host-private with no
/// unchecked constructor: none of it belongs in the solver-plugin API
mod root;
pub mod session;
#[cfg(test)]
mod tests;
/// Host-private trace policy. Roles, the recorder, and its violation type all
/// stay inside the master algorithm; the session maps a recorder failure onto
/// the public allocation and host-contract categories
/// (SPEC_0044 §6).
mod trace;
mod validation;

pub use integrator::{
    MeAcceptedStep, MeAdvanceRequest, MeContinuousPoint, MeDerivativeHandle, MeDerivativeRefused,
    MeIntegrationError, MeIntegratorBackend, MeNumericalFailure, MeNumericalSetup, MeStepCandidate,
    accepted_interval_contains, accepted_step_roundoff,
};
pub use kernel::SolveMeKernel;
pub use session::{
    MeAdvanceOutcome, MeComponentHost, MeOutputCursor, MePluginArity, MeRetainedComponent,
    MeSessionError, MeSessionLoss, MeSessionOptions, MeSessionOptionsInput, MeSimulationSession,
};

use std::rc::Rc;

use crate::solver::{SimTermination, SimVariableMeta};

/// The correlated FMI source an ME component is instantiated from.
///
/// Production construction can borrow this only from a checked
/// [`rumoca_ir_solve::fmi::FmiComponent`]. Hosts receive an opaque handle and
/// can only hand it to `SolveMeKernel::instantiate`, so a bare Solve root can
/// no longer bypass FMI construction or be paired with foreign metadata.
pub struct MeModelSource<'a>(MeModelSourceInner<'a>);

enum MeModelSourceInner<'a> {
    Correlated(rumoca_ir_solve::fmi::FmiRuntimeView<'a>),
    #[cfg(test)]
    Fixture(&'a rumoca_ir_solve::SolveModel),
}

impl<'a> MeModelSource<'a> {
    #[must_use]
    pub fn new(component: &'a rumoca_ir_solve::fmi::FmiComponent) -> Self {
        Self(MeModelSourceInner::Correlated(component.runtime_view()))
    }

    #[cfg(test)]
    pub(crate) fn fixture(model: &'a rumoca_ir_solve::SolveModel) -> Self {
        Self(MeModelSourceInner::Fixture(model))
    }

    pub(crate) fn into_parts(
        self,
    ) -> Result<
        (
            &'a rumoca_ir_solve::SolveModel,
            Vec<rumoca_ir_solve::fmi::FmiEventIndicatorSource>,
        ),
        rumoca_ir_solve::fmi::FmiComponentError,
    > {
        match self.0 {
            MeModelSourceInner::Correlated(view) => {
                let (model, inventory) = view.into_parts();
                Ok((model, inventory.sources().to_vec()))
            }
            #[cfg(test)]
            MeModelSourceInner::Fixture(model) => Ok((
                model,
                rumoca_ir_solve::fmi::FmiEventIndicatorInventory::derive(model)?
                    .sources()
                    .to_vec(),
            )),
        }
    }
}

impl<'a> From<&'a rumoca_ir_solve::fmi::FmiComponent> for MeModelSource<'a> {
    fn from(component: &'a rumoca_ir_solve::fmi::FmiComponent) -> Self {
        Self::new(component)
    }
}

/// The compiled-code execution backend an ME component may be instantiated
/// with, as an opaque host handle.
///
/// SPEC_0038 §Internal Solver Boundary: an integrator host *wires* a backend
/// through, it never compiles with one. Every method of the underlying
/// `SolveExecutionBackend` trait takes Solve IR — scalar program blocks,
/// continuous refresh owners, event-transaction programs — so naming that
/// trait from a host crate would put Solve IR back on the host's own public
/// API. Hosts name this handle instead, and can only hand it to
/// `SolveMeKernel::instantiate_with_execution_backend`.
#[derive(Clone)]
pub struct MeExecutionBackend(Rc<dyn crate::SolveExecutionBackend>);

impl MeExecutionBackend {
    #[must_use]
    pub fn new(backend: Rc<dyn crate::SolveExecutionBackend>) -> Self {
        Self(backend)
    }

    pub(crate) fn into_runtime_backend(self) -> Rc<dyn crate::SolveExecutionBackend> {
        self.0
    }
}

impl From<Rc<dyn crate::SolveExecutionBackend>> for MeExecutionBackend {
    fn from(backend: Rc<dyn crate::SolveExecutionBackend>) -> Self {
        Self::new(backend)
    }
}

/// Typed rejection for a contradictory execution request: the request's
/// execution policy forbids compiled native execution, yet a compiled
/// execution backend handle was supplied.
///
/// Honoring the handle would execute natively against an explicit interpreter
/// request; dropping it silently would let the caller believe it was honored.
/// Either way the interpreter side of the backend differential oracle stops
/// being trustworthy, so the contradiction is typed data, never a quiet
/// resolution in either direction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, thiserror::Error)]
#[error(
    "execution policy '{policy}' forbids compiled native execution, but a compiled \
     execution backend handle was supplied; withhold the handle or request the \
     'auto' policy"
)]
pub struct MeExecutionPolicyContradiction {
    /// The rendered label of the rejecting policy.
    pub policy: &'static str,
}

/// Admit a host-supplied opaque execution backend handle against the request's
/// execution policy.
///
/// This rule is owned ONCE, here at the ME contract boundary where the handle
/// meets [`crate::SimExecutionPolicy`], so every concrete integrator backend
/// rejects the identical contradictory input identically: the same public
/// request must not have backend-dependent semantics. Concrete crates call
/// this from their entry points and surface the typed contradiction through
/// their own error enums without rewording it.
pub fn admit_execution_backend(
    policy: crate::SimExecutionPolicy,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<Option<MeExecutionBackend>, MeExecutionPolicyContradiction> {
    if execution_backend.is_some() && !policy.allows_native() {
        return Err(MeExecutionPolicyContradiction {
            policy: policy.label(),
        });
    }
    Ok(execution_backend)
}

/// Owned checked model artifact that numerical solver plugins can retain
/// without gaining access to Solve IR.
///
/// The generic ME runtime is the only layer that can project this artifact
/// into a component. Concrete solver crates may store it and request an opaque
/// [`MeModelSource`], but cannot inspect rows, layouts, opcodes, or events.
pub struct MeModelArtifact(rumoca_ir_solve::fmi::FmiComponent);

impl MeModelArtifact {
    #[must_use]
    pub fn new(component: rumoca_ir_solve::fmi::FmiComponent) -> Self {
        Self(component)
    }

    #[must_use]
    pub fn source(&self) -> MeModelSource<'_> {
        MeModelSource::new(&self.0)
    }

    #[must_use]
    pub fn continuous_state_count(&self) -> usize {
        self.0.problem().solve_layout.state_scalar_count()
    }
}

impl From<rumoca_ir_solve::fmi::FmiComponent> for MeModelArtifact {
    fn from(component: rumoca_ir_solve::fmi::FmiComponent) -> Self {
        Self::new(component)
    }
}

/// The ME lifecycle stage a component failure was raised in.
///
/// Extension beyond FMI 3.0: see the module docs. This is *producer knowledge*
/// — the operation that fails attaches the stage it was running, so a host
/// never has to recognise a sub-stage by pattern-matching a rendered message.
/// Hosts map it onto their own failure buckets; the map is total in both
/// directions for the stages a component can be in, which is what keeps a
/// failure histogram stable across the SPEC_0038 migration.
///
/// Stages a *host* owns — isolating an output stop, interpolating, timing out —
/// are deliberately absent: the component is never running when they happen.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MeStage {
    /// `fmi3InstantiateModelExchange`: projecting the checked kernel, before
    /// any model evaluation.
    Instantiate,
    /// Initialization Mode, plus the initial event iteration MLS 3.6 §8.6
    /// requires before integration starts.
    Initialization,
    /// Event Mode: `fmi3UpdateDiscreteStates` and the boundary it runs.
    EventIteration,
    /// Projecting a point back onto the model's constraint manifold.
    ManifoldProjection,
    /// Continuous-Time Mode evaluation on behalf of the host's integrator.
    Integration,
}

/// Which stage an annotation resolves to when one is already recorded.
///
/// The innermost annotation wins: an outer lifecycle boundary sees failures
/// from every stage nested under it, and relabelling them with its own coarser
/// stage would destroy the precision the stage exists to carry.
///
/// Pure predicate over `Copy` value types so it can be proved rather than
/// exercised: `resolve` is idempotent and never invents a stage.
#[must_use]
pub fn resolve_me_stage(recorded: Option<MeStage>, incoming: MeStage) -> MeStage {
    match recorded {
        Some(stage) => stage,
        None => incoming,
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

    /// The value path is valid, but the local directional derivative required
    /// by a derivative-based importer does not exist.
    #[error("directional derivative is unavailable: {reason}")]
    DirectionalDerivativeUnavailable { reason: String },

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

    /// A failure annotated with the ME stage that raised it.
    ///
    /// The rendered form is exactly the inner failure's, so annotating a path
    /// never changes a user-visible message; the stage travels alongside for
    /// machine consumers. Hosts that switch on the failure *variant* go through
    /// [`MeError::kind`] / [`MeError::into_kind`], so an annotated path behaves
    /// exactly like the unannotated one.
    #[error("{inner}")]
    Staged { stage: MeStage, inner: Box<MeError> },
}

impl MeError {
    /// The stage the raising operation recorded, if any.
    #[must_use]
    pub fn stage(&self) -> Option<MeStage> {
        match self {
            Self::Staged { stage, .. } => Some(*stage),
            _ => None,
        }
    }

    /// The failure itself, with every stage annotation peeled off.
    #[must_use]
    pub fn kind(&self) -> &MeError {
        match self {
            Self::Staged { inner, .. } => inner.kind(),
            other => other,
        }
    }

    /// [`MeError::kind`] by value, for a host converting into its own error.
    #[must_use]
    pub fn into_kind(self) -> MeError {
        match self {
            Self::Staged { inner, .. } => inner.into_kind(),
            other => other,
        }
    }

    /// Annotate with the stage that raised this failure, innermost winning
    /// (see [`resolve_me_stage`]).
    #[must_use]
    pub fn at_stage(self, stage: MeStage) -> Self {
        let resolved = resolve_me_stage(self.stage(), stage);
        match self {
            Self::Staged { inner, .. } => Self::Staged {
                stage: resolved,
                inner,
            },
            other => Self::Staged {
                stage: resolved,
                inner: Box::new(other),
            },
        }
    }
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
            singular @ Runtime::RefreshTargetSingular { .. } => Self::Evaluation {
                message: singular.to_string(),
            },
            Runtime::NonFiniteDerivative { state_name } => Self::NonFiniteDerivative { state_name },
            Runtime::DirectionalDerivativeUnavailable { reason } => {
                Self::DirectionalDerivativeUnavailable { reason }
            }
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
    instance_name: &'static str,
    /// FMI relative integration `tolerance` (`toleranceDefined = true`).
    tolerance: f64,
    /// FMI `startTime`.
    start_time: f64,
    /// FMI `stopTime` (`stopTimeDefined = true`).
    stop_time: f64,
}

impl MeInstanceConfig {
    /// Construct the complete checked FMI Model Exchange instance request.
    ///
    /// The tolerance is FMI's relative integration tolerance. Keeping the
    /// fields private prevents a concrete solver adapter from substituting an
    /// absolute tolerance or bypassing the horizon proof.
    pub fn new(
        instance_name: &'static str,
        relative_tolerance: f64,
        start_time: f64,
        stop_time: f64,
    ) -> Result<Self, MeError> {
        if instance_name.is_empty() {
            return Err(MeError::Contract {
                reason: "ME instance name must not be empty".to_owned(),
            });
        }
        if !relative_tolerance.is_finite() || relative_tolerance <= 0.0 {
            return Err(MeError::Contract {
                reason: "ME relative tolerance must be finite and positive".to_owned(),
            });
        }
        if !start_time.is_finite() || !stop_time.is_finite() || stop_time < start_time {
            return Err(MeError::Contract {
                reason: "ME time horizon requires finite values with stop_time >= start_time"
                    .to_owned(),
            });
        }
        Ok(Self {
            instance_name,
            tolerance: relative_tolerance,
            start_time,
            stop_time,
        })
    }
}

/// Transitional subset of the FMI model description used by the linked host.
///
/// SPEC_0038 requires this to become the checked metadata artifact used to emit
/// the complete FMI 3.0.2 `modelDescription.xml`; this subset must not be
/// mistaken for that final component contract.
#[derive(Debug, Clone, Copy)]
pub struct MeModelDescription<'a> {
    pub continuous_state_count: usize,
    pub event_indicator_count: usize,
    /// FMI `<ModelExchange needsCompletedIntegratorStep="...">`.
    pub needs_completed_integrator_step: bool,
    /// Names of every variable the component exposes through
    /// `SolveMeKernel::get_outputs`, in value-reference order.
    pub output_names: &'a [String],
    /// Names of every writable input variable.
    pub input_names: &'a [String],
    /// Per-output metadata (causality/variability) for result reporting.
    pub output_meta: &'a [SimVariableMeta],
}

/// A resolved FMI value reference. Opaque: only the component interprets it.
#[derive(Debug, Clone)]
pub struct MeValueRef {
    pub(crate) index: usize,
    pub(crate) instance_brand: Rc<()>,
}

/// Transitional `fmi3SetTime` representation carrying the event instant the
/// integrator is stepping toward. FMI itself carries only `time`; the boundary
/// field remains phase-2 migration debt documented above.
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
    /// `SolveMeKernel::next_event_stop`.
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
    /// FMI `valuesOfContinuousStatesChanged`, derived by comparing the exact
    /// continuous-state vector before and after the settled event iteration.
    pub values_of_continuous_states_changed: bool,
    /// FMI `nominalsOfContinuousStatesChanged`. Rumoca's nominals are fixed
    /// at instantiation, so this is currently always `false`.
    pub nominals_of_continuous_states_changed: bool,
    /// FMI `nextEventTimeDefined` / `nextEventTime`. `None` represents
    /// `nextEventTimeDefined = false`.
    pub next_event_time: Option<f64>,
}

/// The next instant the component wants the integrator to stop at.
#[derive(Debug, Clone, Copy)]
pub struct MeEventStop {
    pub time: f64,
    /// `false` when `time` is just the requested horizon.
    pub is_event: bool,
}

/// `fmi3CompletedIntegratorStep` outputs.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct MeCompletedIntegratorStep {
    /// FMI `enterEventMode`: the component requests a step event.
    pub enter_event_mode: bool,
    /// FMI `terminateSimulation`: the component requests termination.
    pub terminate_simulation: bool,
}

/// An observation point: the component's refreshed observable state.
///
/// Produced by `SolveMeKernel::observe` and by the initial-event
/// iteration; consumed by the batched output getters. Opaque to hosts, which
/// is what makes `fmi3GetFloat64` a batched read rather than a row query.
#[derive(Debug, Clone)]
pub struct MeObservation {
    pub(crate) time: f64,
    pub(crate) solver_y: Vec<f64>,
    pub(crate) parameters: Vec<f64>,
    pub(crate) instance_brand: Rc<()>,
}

impl MeObservation {
    #[must_use]
    pub fn time(&self) -> f64 {
        self.time
    }
}

/// An opaque saved component state (`fmi3GetFMUState`).
#[derive(Clone)]
pub struct MeFmuState {
    pub(crate) component: kernel::MeKernelSnapshot,
    pub(crate) instance_brand: Rc<()>,
}

impl std::fmt::Debug for MeFmuState {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        formatter.debug_struct("MeFmuState").finish_non_exhaustive()
    }
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

    #[cfg(test)]
    pub(crate) fn columns_mut(&mut self) -> &mut [Vec<f64>] {
        &mut self.columns
    }

    #[must_use]
    pub fn into_columns(self) -> Vec<Vec<f64>> {
        self.columns
    }
}

/// Whether two consecutive event-indicator values bracket a crossing.
///
/// Pure predicate on indicator values; hosts localizing a root need it and it
/// carries no model state.
#[must_use]
pub fn event_indicator_crossed(before: f64, after: f64, tolerance: f64) -> bool {
    crate::runtime::solve_ops::root_value_crossed(before, after, tolerance)
}

/// Advance an integrator-owned state vector to an internal event-side probe.
///
/// The probe classifies the post side of a located event; it is not a trace
/// observation or a replacement for the semantic event time. Keeping this
/// arithmetic on the FMI-ME boundary gives every integrator one path without
/// exposing component-private runtime objects.
pub fn advance_states_to_event_probe(
    states: &mut [f64],
    derivatives: &[f64],
    event_time: f64,
    probe_time: f64,
) {
    let dt = probe_time - event_time;
    if dt <= 0.0 || crate::timeline::sample_time_match_with_tol(event_time, probe_time) {
        return;
    }
    debug_assert_eq!(states.len(), derivatives.len());
    for (state, derivative) in states.iter_mut().zip(derivatives.iter().copied()) {
        *state += dt * derivative;
    }
}
