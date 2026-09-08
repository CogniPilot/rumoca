//! The internal FMI 3.0 Model Exchange contract (SPEC_0038 §Internal Solver
//! Boundary).
//!
//! `SolveProblem` stays compiler IR. It is projected exactly once, by
//! [`SolveMeKernel`], into an FMI 3 ME component kernel. Every integrator
//! reaches the model only through [`SolveMeKernel`]. Integrators MUST NOT inspect Solve rows,
//! layouts, opcodes, events, or private runtime objects.
//!
//! # Operation map
//!
//! The operations below map exactly to FMI 3.0 ME entry points. Host policy is
//! composed outside this component surface.
//!
//! | Kernel operation | FMI 3.0 ME |
//! |---|---|
//! | `SolveMeKernel::instantiate` | `fmi3InstantiateModelExchange` |
//! | `SolveMeKernel::model_description` | model description + `fmi3GetNumberOfContinuousStates` / `fmi3GetNumberOfEventIndicators` |
//! | `SolveMeKernel::enter_configuration_mode` | `fmi3EnterConfigurationMode` |
//! | `SolveMeKernel::exit_configuration_mode` | `fmi3ExitConfigurationMode` |
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
//! | `SolveMeKernel::get_float64` | batched `fmi3GetFloat64` |
//! | `SolveMeKernel::value_reference` / `SolveMeKernel::set_float64` | model description + batched `fmi3SetFloat64` |
//! | `SolveMeKernel::fmu_state` / `SolveMeKernel::reset_to_fmu_state` | `fmi3GetFMUState` / `fmi3Reset` + `fmi3SetFMUState` |
//! | `SolveMeKernel::terminate` | `fmi3Terminate` |
//!
//! [`MeStage`] is diagnostic provenance, not an FMI operation: FMI reports one
//! undifferentiated error status, while the component retains the internal
//! stage that first produced it.

/// A feature-gated harness for driving a real numerical plugin through the
/// checked retained-handle and activation contract. Compiled only under test or
/// the `test-support` feature, so no production build carries it.
#[cfg(any(test, feature = "test-support"))]
pub mod backend_test_support;
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

pub use integrator::{
    MeAcceptedStep, MeAdvanceRequest, MeContinuousPoint, MeDerivativeHandle, MeDerivativeRefused,
    MeIntegrationError, MeIntegratorBackend, MeNumericalFailure, MeNumericalSetup, MeStepCandidate,
    accepted_interval_contains, accepted_step_roundoff,
};
pub use kernel::SolveMeKernel;
pub use session::{
    MeAdvanceOutcome, MeComponentHost, MePluginArity, MeRetainedComponent, MeSessionError,
    MeSessionLoss, MeSessionOptions, MeSessionOptionsInput, MeSimulationSession,
};

use std::rc::Rc;

use crate::solver::{SimTermination, SimVariableMeta};

/// The linked FMI component's continuous-state domain.
///
/// This is a role capability, not a caller-supplied count. Production values
/// come only from checked linked-runtime facts; host/root construction carries
/// the capability and projects its length only at allocation or public-report
/// boundaries.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct MeContinuousStateDomain(MeContinuousStateDomainSource);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum MeContinuousStateDomainSource {
    Linked(rumoca_ir_solve::fmi::FmiContinuousStateWidth),
    #[cfg(any(test, feature = "test-support"))]
    Verification(MeVerificationStateWidth),
}

/// State width issued by an explicitly non-production verification source.
///
/// This is intentionally distinct from the linked FMI width: backend
/// conformance tests may derive a domain from their complete derivative
/// vector, but they cannot pretend that vector was checked FMI metadata.
#[cfg(any(test, feature = "test-support"))]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct MeVerificationStateWidth(usize);

impl MeContinuousStateDomain {
    fn from_linked(width: rumoca_ir_solve::fmi::FmiContinuousStateWidth) -> Self {
        Self(MeContinuousStateDomainSource::Linked(width))
    }

    #[cfg(any(test, feature = "test-support"))]
    fn from_verification_rates(rates: &[f64]) -> Self {
        Self(MeContinuousStateDomainSource::Verification(
            MeVerificationStateWidth(rates.len()),
        ))
    }

    #[cfg(test)]
    pub(crate) const fn verification_fixture(len: usize) -> Self {
        Self(MeContinuousStateDomainSource::Verification(
            MeVerificationStateWidth(len),
        ))
    }

    #[must_use]
    pub(crate) const fn len(self) -> usize {
        match self.0 {
            MeContinuousStateDomainSource::Linked(width) => width.len(),
            #[cfg(any(test, feature = "test-support"))]
            MeContinuousStateDomainSource::Verification(width) => width.0,
        }
    }

    #[must_use]
    pub(crate) const fn is_empty(self) -> bool {
        self.len() == 0
    }
}

/// Positive finite relative/absolute tolerances admitted as one solver policy.
///
/// Session options and the explicit backend-verification boundary both use
/// this sole checker. Downstream root policy and numerical setup construction
/// retain the result and never revalidate or accept the two scalars
/// independently.
#[derive(Clone, Copy, Debug)]
pub(crate) struct MeSolverTolerances {
    relative: f64,
    absolute: f64,
}

impl MeSolverTolerances {
    fn check(relative: f64, absolute: f64) -> Result<Self, MeToleranceError> {
        for (role, value) in [("relative", relative), ("absolute", absolute)] {
            if !value.is_finite() || value <= 0.0 {
                return Err(MeToleranceError { role, value });
            }
        }
        Ok(Self { relative, absolute })
    }

    #[must_use]
    pub(crate) const fn relative(self) -> f64 {
        self.relative
    }

    #[must_use]
    pub(crate) const fn absolute(self) -> f64 {
        self.absolute
    }
}

#[derive(Debug, thiserror::Error)]
#[error("the solver {role} tolerance must be finite and positive, got {value}")]
pub(crate) struct MeToleranceError {
    role: &'static str,
    value: f64,
}

/// The correlated FMI source an ME component is instantiated from.
///
/// Production construction can borrow this only from a checked
/// [`rumoca_ir_solve::fmi::FmiComponent`]. Hosts receive an opaque handle and
/// can only hand it to `SolveMeKernel::instantiate`, so a bare Solve root can
/// no longer bypass FMI construction or be paired with foreign metadata.
pub struct MeModelSource(MeModelSourceInner);

enum MeModelSourceInner {
    Correlated(rumoca_ir_solve::fmi::FmiRuntimeView),
    #[cfg(test)]
    Fixture {
        runtime: rumoca_ir_solve::fmi::FmiRuntimeView,
        configuration: lifecycle::MeConfigurationCapability,
    },
}

/// The checked pieces an [`MeModelSource`] resolves to: the correlated
/// component, the opaque linked-runtime facts construction issued, and
/// structural configuration.
type MeModelParts = (
    rumoca_ir_solve::fmi::FmiRuntimeView,
    lifecycle::MeConfigurationCapability,
);

impl MeModelSource {
    #[must_use]
    pub fn new(component: rumoca_ir_solve::fmi::FmiComponent) -> Self {
        Self(MeModelSourceInner::Correlated(
            component.into_runtime_view(),
        ))
    }

    #[cfg(test)]
    pub(crate) fn fixture(component: rumoca_ir_solve::fmi::FmiComponent) -> Self {
        Self(MeModelSourceInner::Fixture {
            runtime: component.into_runtime_view(),
            configuration: lifecycle::MeConfigurationCapability::Absent,
        })
    }

    #[cfg(test)]
    pub(crate) fn configuration_fixture(
        component: rumoca_ir_solve::fmi::FmiComponent,
        configuration: lifecycle::MeConfigurationCapability,
    ) -> Self {
        Self(MeModelSourceInner::Fixture {
            runtime: component.into_runtime_view(),
            configuration,
        })
    }

    pub(crate) fn into_parts(
        self,
    ) -> Result<MeModelParts, rumoca_ir_solve::fmi::FmiComponentError> {
        match self.0 {
            MeModelSourceInner::Correlated(runtime) => {
                let configuration = match runtime.configuration_capability() {
                    rumoca_ir_solve::fmi::FmiConfigurationCapability::Absent => {
                        lifecycle::MeConfigurationCapability::Absent
                    }
                    rumoca_ir_solve::fmi::FmiConfigurationCapability::FixedStructuralParameter => {
                        lifecycle::MeConfigurationCapability::FixedStructuralParameter
                    }
                    rumoca_ir_solve::fmi::FmiConfigurationCapability::TunableStructuralParameter => {
                        lifecycle::MeConfigurationCapability::TunableStructuralParameter
                    }
                };
                Ok((runtime, configuration))
            }
            #[cfg(test)]
            MeModelSourceInner::Fixture {
                runtime,
                configuration,
            } => Ok((runtime, configuration)),
        }
    }
}

impl From<rumoca_ir_solve::fmi::FmiComponent> for MeModelSource {
    fn from(component: rumoca_ir_solve::fmi::FmiComponent) -> Self {
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
/// `SolveMeKernel::instantiate_with_execution`.
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

/// Closed execution alternative handed to one FMI component at construction.
#[derive(Clone)]
pub enum MeExecutionSelection {
    Interpreter,
    Native(MeExecutionBackend),
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

/// Select one closed execution alternative against the request's policy.
///
/// This rule is owned ONCE, here at the ME contract boundary where the handle
/// meets [`crate::SimExecutionPolicy`], so every concrete integrator backend
/// rejects the identical contradictory input identically: the same public
/// request must not have backend-dependent semantics. Concrete crates call
/// this from their entry points and surface the typed contradiction through
/// their own error enums without rewording it.
pub fn select_execution(
    policy: crate::SimExecutionPolicy,
    execution_backend: Option<MeExecutionBackend>,
) -> Result<MeExecutionSelection, MeExecutionPolicyContradiction> {
    match (policy.allows_native(), execution_backend) {
        (false, Some(_)) => Err(MeExecutionPolicyContradiction {
            policy: policy.label(),
        }),
        (true, Some(backend)) => Ok(MeExecutionSelection::Native(backend)),
        (_, None) => Ok(MeExecutionSelection::Interpreter),
    }
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
    pub fn into_source(self) -> MeModelSource {
        MeModelSource::new(self.0)
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
    /// The selected native execution arm failed to compile or execute.
    #[error("native {execution_stage} failed for {owner}: {reason}")]
    NativeExecution {
        execution_stage: crate::NativeExecutionStage,
        owner: crate::NativeExecutionOwner,
        reason: String,
    },

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

    /// An evaluator returned an invalid timestamp for a semantic event action.
    #[error("non-finite timestamp {time} for Modelica {action} action")]
    NonFiniteEventActionTime { action: &'static str, time: f64 },

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
            Runtime::NativeExecution {
                stage: execution_stage,
                owner,
                reason,
            } => Self::NativeExecution {
                execution_stage,
                owner,
                reason,
            },
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
    pub(crate) value_reference: u32,
    pub(crate) backing: MeFloat64Backing,
    pub(crate) access: MeFloat64AccessEvidence,
    pub(crate) instance_brand: Rc<()>,
}

/// One construction-issued value reference usable in an FMI 3 directional
/// derivative reference list.
///
/// The backing and instance brand are opaque. A host may only collect these
/// references into a list and ask the component to validate that list as the
/// `knowns` or `unknowns` argument of `fmi3GetDirectionalDerivative`.
#[derive(Debug, Clone)]
pub(crate) struct MeDirectionalValueRef {
    value_reference: u32,
    backing: MeDirectionalBacking,
    instance_brand: Rc<()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MeDirectionalBacking {
    ContinuousState { base: usize, width: usize },
    ContinuousStateDerivative { base: usize, width: usize },
}

/// A component-validated FMI `knowns[]` batch.
///
/// Construction is private to the component, so the serialized seed width and
/// every state-storage segment are correlated with one instance exactly once.
#[derive(Debug)]
pub(crate) struct MeDirectionalKnownBatch {
    references: Vec<MeDirectionalValueRef>,
    serialized_width: usize,
    instance_brand: Rc<()>,
}

/// A component-validated FMI `unknowns[]` batch.
///
/// Construction is private to the component, so the serialized sensitivity
/// width and every derivative-storage segment are correlated with one instance
/// exactly once.
#[derive(Debug)]
pub(crate) struct MeDirectionalUnknownBatch {
    references: Vec<MeDirectionalValueRef>,
    serialized_width: usize,
    instance_brand: Rc<()>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MeFloat64Backing {
    SolverVariable { base: usize, width: usize },
    Parameter { base: usize, width: usize },
    MaxStepDuration,
}

/// Construction-issued FMI variable facts carried by every value reference.
///
/// The dynamic setter consumes these facts directly. It never rebuilds
/// causality or write permission from a caller-supplied name. The write
/// permission is the decided FMI 3 mask, so admission and generated C read one
/// table; the causality is retained only to name the component's inputs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct MeFloat64AccessEvidence {
    pub(crate) causality: rumoca_ir_solve::fmi::FmiCausality,
    pub(crate) write_modes: rumoca_ir_solve::fmi::Fmi3WriteModes,
}

/// Exact `fmi3SetTime` argument.
#[derive(Debug, Clone, Copy)]
pub struct MeTime {
    pub time: f64,
}

impl MeTime {
    #[must_use]
    pub fn at(time: f64) -> Self {
        Self { time }
    }
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

/// `fmi3CompletedIntegratorStep` outputs.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MeCompletedIntegratorStep {
    /// FMI `enterEventMode`: the component requests a step event.
    pub enter_event_mode: bool,
    /// FMI `terminateSimulation`: the component requests termination.
    pub terminate_simulation: bool,
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
