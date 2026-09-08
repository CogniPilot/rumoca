use std::{cell::RefCell, rc::Rc};

mod component;
mod event_boundary;
mod indicator_plan;

use super::lifecycle::{MeLifecycle, MeLifecycleViolation, MeState};
use super::{
    MeCompletedIntegratorStep, MeContinuousStateDomain, MeDirectionalBacking,
    MeDirectionalKnownBatch, MeDirectionalUnknownBatch, MeDirectionalValueRef, MeDiscreteStates,
    MeError, MeFloat64AccessEvidence, MeFloat64Backing, MeFmuState, MeInstanceConfig,
    MeModelDescription, MeModelSource, MeStage, MeTime, MeValueRef, advance_states_to_event_probe,
};
use crate::runtime::pre_params::{
    clear_scheduled_root_relation_memory, commit_pre_params_after_event_at,
};
use crate::runtime::schedule::{RuntimeEventStop, ScheduledEventConsumption, SolveStopSchedule};
use crate::runtime::solve_ops::{
    EventActionOutcome, EventPreMode, RootCrossing, convert_variable_meta, runtime_values_changed,
    write_observation_clock_activation_params,
};
use crate::runtime::solve_runtime::{
    AlgebraicLinearization, AlgebraicSettle, EventUpdateRowFilter, ProjectedEventUpdateInput,
    ProjectedInitialEventInput, SolveRuntime, SolveRuntimeSnapshot,
};
use crate::runtime::time::time_match_with_tol;
use crate::solver::{SimTermination, SimVariableMeta};
use crate::timeline;
use component::event_storage::{
    DeadlineIndicatorValues, EventIndicatorStorage, EventIndicatorStorageSnapshot,
    EventVectorLatch, FmiPublicationIndicatorValues, RootIndicatorValues,
    WorkingPublishedIndicatorValues,
};
use indicator_plan::FmiIndicatorPlan;
use rumoca_ir_solve::{ScalarSlot, fmi::FmiPublishedIndicatorWidth};

/// Residual tolerance for the component's internal algebraic refresh.
const ALGEBRAIC_REFRESH_TOL: f64 = 1.0e-10;
/// Iteration ceiling for the component's internal algebraic/event fixed points.
const UPDATE_MAX_ITERS: usize = 32;

fn constructed_relation_memory_domain(
    params: &[f64],
    position: usize,
    target: ScalarSlot,
) -> Result<bool, MeError> {
    let ScalarSlot::P {
        index: parameter, ..
    } = target
    else {
        return Err(contract(format!(
            "indicator {position} relation-memory target is not parameter storage",
        )));
    };
    params.get(parameter).map(|memory| *memory <= 0.5).ok_or_else(|| {
        contract(format!(
            "indicator {position} relation-memory target P[{parameter}] is outside constructed storage",
        ))
    })
}

#[derive(Clone)]
struct CachedDerivative {
    valid: bool,
    time: f64,
    state: Vec<f64>,
    derivative: Vec<f64>,
}

impl CachedDerivative {
    fn copy_from(&mut self, source: &Self) {
        self.valid = source.valid;
        self.time = source.time;
        self.state.copy_from_slice(&source.state);
        self.derivative.copy_from_slice(&source.derivative);
    }
}

#[derive(Clone)]
struct CachedContinuousLinearization {
    valid: bool,
    time: f64,
    state: Vec<f64>,
    parameters: Vec<f64>,
}

impl CachedContinuousLinearization {
    fn matches(&self, time: f64, state: &[f64], parameters: &[f64]) -> bool {
        self.valid
            && self.time.to_bits() == time.to_bits()
            && state_values_match(&self.state, state)
            && state_values_match(&self.parameters, parameters)
    }

    fn copy_from(&mut self, source: &Self) {
        self.valid = source.valid;
        self.time = source.time;
        self.state.copy_from_slice(&source.state);
        self.parameters.copy_from_slice(&source.parameters);
    }
}

#[derive(Clone, Copy)]
struct MeAlgebraicProjectionPolicy {
    tolerance: f64,
    settle: AlgebraicSettle,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum StateTimeCoincidence {
    None,
    Unconsumed,
    Consumed,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PendingEventCause {
    State,
    Time,
}

#[derive(Clone, Copy, Debug)]
struct PendingEventEntry {
    cause: PendingEventCause,
    event_time: f64,
    horizon: f64,
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
/// events, or runtime objects: the only way in is [`SolveMeKernel`].
pub struct SolveMeKernel {
    lifecycle: MeLifecycle,
    body: MeKernelBody,
    event_stage: Box<MeKernelBody>,
    event_runtime_checkpoint: SolveRuntimeSnapshot,
}

/// Exact continuous-state width carried by the linked kernel into host root
/// scanning. Only the kernel can issue production values; the host may inspect
/// the width but cannot substitute an indicator width for it.
#[derive(Clone, Copy)]
pub(super) struct RootScanStateWidth(MeContinuousStateDomain);

impl RootScanStateWidth {
    const fn issue(state_domain: MeContinuousStateDomain) -> Self {
        Self(state_domain)
    }

    #[cfg(test)]
    pub(super) const fn verification_fixture(state_count: usize) -> Self {
        Self(MeContinuousStateDomain::verification_fixture(state_count))
    }

    pub(super) const fn len(self) -> usize {
        self.0.len()
    }
}

/// Exact published FMI event-indicator width carried into host root scanning.
/// Its production issuer accepts only the checked plan's published-width role.
#[derive(Clone, Copy)]
pub(super) struct RootScanIndicatorWidth(RootScanIndicatorWidthSource);

#[derive(Clone, Copy)]
enum RootScanIndicatorWidthSource {
    Linked(FmiPublishedIndicatorWidth),
    #[cfg(test)]
    Verification(usize),
}

impl RootScanIndicatorWidth {
    const fn issue(published_width: FmiPublishedIndicatorWidth) -> Self {
        Self(RootScanIndicatorWidthSource::Linked(published_width))
    }

    #[cfg(test)]
    pub(super) const fn verification_fixture(indicator_count: usize) -> Self {
        Self(RootScanIndicatorWidthSource::Verification(indicator_count))
    }

    pub(super) const fn len(self) -> usize {
        match self.0 {
            RootScanIndicatorWidthSource::Linked(width) => width.len(),
            #[cfg(test)]
            RootScanIndicatorWidthSource::Verification(width) => width,
        }
    }
}

/// One kernel-issued root-scan shape.
///
/// The role fields are distinct capabilities, so exchanging the two widths in
/// a scan-storage issuer is a compiler error. The aggregate is intentionally
/// neither `Clone` nor `Default`: one linked kernel issues it for one session.
pub(super) struct RootScanShape {
    state_width: RootScanStateWidth,
    indicator_width: RootScanIndicatorWidth,
}

impl RootScanShape {
    const fn issue(
        state_width: RootScanStateWidth,
        indicator_width: RootScanIndicatorWidth,
    ) -> Self {
        Self {
            state_width,
            indicator_width,
        }
    }

    #[cfg(test)]
    pub(super) const fn verification_fixture(
        state_width: RootScanStateWidth,
        indicator_width: RootScanIndicatorWidth,
    ) -> Self {
        Self::issue(state_width, indicator_width)
    }

    pub(super) const fn state_width(&self) -> RootScanStateWidth {
        self.state_width
    }

    pub(super) const fn indicator_width(&self) -> RootScanIndicatorWidth {
        self.indicator_width
    }
}

struct MeKernelBody {
    runtime: Rc<SolveRuntime>,
    instance_brand: Rc<()>,
    value_references: Vec<MeNamedFloat64Reference>,
    input_names: Vec<String>,
    directional_state_references: Vec<MeDirectionalReferenceDescriptor>,
    directional_derivative_references: Vec<MeDirectionalReferenceDescriptor>,
    instance_name: &'static str,
    /// FMI `tolerance`.
    tolerance: f64,
    /// FMI `stopTime`.
    stop_time: f64,
    /// The time `fmi3SetTime` last set.
    time: f64,
    /// Exact FMI 3.0.2 §3.2.1 lower-bound continuation for `fmi3SetTime`.
    set_time_bounds: MeSetTimeBounds,
    /// The evaluation time that represents the right limit of the last event.
    post_event_eval_time: Option<f64>,
    /// The component time the right limit above belongs to.
    event_anchor_time: f64,

    states: Vec<f64>,
    params: Vec<f64>,
    state_domain: MeContinuousStateDomain,

    stop_schedule: SolveStopSchedule,
    pending_event_entry: Option<PendingEventEntry>,
    pending_state_event_entry: bool,
    pending_event_stop: Option<(f64, RuntimeEventStop)>,
    advance_state_to_event_right_limit: bool,
    state_time_coincidence: StateTimeCoincidence,
    initial_event_pending: bool,
    pending_root_crossings: Vec<RootCrossing>,
    /// All mutable storage whose width is issued by the FMI indicator plan.
    indicator_storage: EventIndicatorStorage,
    pending_event_pre_y: EventVectorLatch,
    pending_event_pre_p: EventVectorLatch,
    boundary_event_pre_y: EventVectorLatch,
    boundary_event_pre_p: EventVectorLatch,
    event_solver_y_work: Vec<f64>,
    event_state_before: Vec<f64>,
    scheduled_root_index_scratch: Vec<usize>,
    root_override_scratch: Vec<(usize, f64)>,

    solver_y_guess: RefCell<Vec<f64>>,
    /// Construction-reserved caller-publication storage for
    /// `get_continuous_state_derivatives`; evaluation finishes here before a
    /// successful result is copied to the caller. Evaluator and delay
    /// workspaces are separate from this publication guarantee.
    derivative_output_scratch: RefCell<Vec<f64>>,
    /// Construction-reserved caller-publication-path storage holding the full
    /// state seed `get_directional_derivative` scatters an admitted known batch
    /// into. It is refilled with zeros on every call before the scatter, so an
    /// earlier batch's seed cannot leak into an uncovered position.
    directional_seed_scratch: RefCell<Vec<f64>>,
    /// Construction-reserved caller-publication-path storage holding the
    /// state-derivative sensitivity vector before serialization.
    directional_sensitivity_scratch: RefCell<Vec<f64>>,
    /// The construction-reserved caller-publication serialization buffer for
    /// `get_directional_derivative`, reserved at the serialized width of the
    /// entire issued directional table. An admitted unknown batch uses each
    /// issued value reference at most once, so its `serialized_width` never
    /// exceeds this construction fact; the getter consumes the exact
    /// `serialized_width` prefix.
    directional_serialized_scratch: RefCell<Vec<f64>>,
    accepted_derivative_scratch: RefCell<Vec<f64>>,
    delay_params_scratch: RefCell<Vec<f64>>,
    delay_solver_y_scratch: RefCell<Vec<f64>>,
    derivative_cache: RefCell<CachedDerivative>,
    continuous_linearization_cache: RefCell<CachedContinuousLinearization>,

    max_step_duration: Option<f64>,
    max_step_duration_value_reference: Option<u32>,
    termination: Option<SimTermination>,
    output_meta: Vec<SimVariableMeta>,
    /// The settled full solver vector `exit_initialization_mode` produced, so
    /// the initial `update_discrete_states` continues from the same vector
    /// instead of rebuilding one.
    settled_initialization_y: EventVectorLatch,
    /// One-shot FMI initialization failure injection for failure-atomicity
    /// verification. It is deliberately not component continuation state, so
    /// restoring an FMU snapshot does not re-arm a consumed injection.
    #[cfg(test)]
    verification_fail_next_enter_initialization: bool,
    #[cfg(test)]
    verification_fail_next_exit_initialization: bool,
    #[cfg(test)]
    verification_fail_next_update_discrete_states: bool,
    #[cfg(test)]
    verification_fail_next_completed_integrator_step: bool,
    #[cfg(test)]
    verification_fail_next_enter_continuous_time_mode: bool,
}

#[derive(Clone, Copy)]
struct MeSetTimeBounds {
    start_time: f64,
    previous_completed_integrator_step_time: Option<f64>,
    last_completed_integrator_step_time: Option<f64>,
    last_enter_event_mode_time: Option<f64>,
}

impl MeSetTimeBounds {
    const fn at_start(start_time: f64) -> Self {
        Self {
            start_time,
            previous_completed_integrator_step_time: None,
            last_completed_integrator_step_time: None,
            last_enter_event_mode_time: None,
        }
    }

    fn lower_bound(self) -> f64 {
        let mut bound = self.start_time;
        if let Some(time) = self.previous_completed_integrator_step_time {
            bound = bound.max(time);
        }
        if let Some(time) = self.last_enter_event_mode_time {
            bound = bound.max(time);
        }
        bound
    }

    fn record_completed_integrator_step(&mut self, time: f64) {
        self.previous_completed_integrator_step_time = self.last_completed_integrator_step_time;
        self.last_completed_integrator_step_time = Some(time);
    }

    fn record_enter_event_mode(&mut self, time: f64) {
        self.last_enter_event_mode_time = Some(time);
    }

    #[cfg(test)]
    fn bit_eq(self, other: Self) -> bool {
        self.start_time.to_bits() == other.start_time.to_bits()
            && option_float_bit_eq(
                self.previous_completed_integrator_step_time,
                other.previous_completed_integrator_step_time,
            )
            && option_float_bit_eq(
                self.last_completed_integrator_step_time,
                other.last_completed_integrator_step_time,
            )
            && option_float_bit_eq(
                self.last_enter_event_mode_time,
                other.last_enter_event_mode_time,
            )
    }
}

#[derive(Clone)]
struct MeNamedFloat64Reference {
    name: String,
    value_reference: u32,
    backing: MeFloat64Backing,
    access: MeFloat64AccessEvidence,
}

#[derive(Clone, Copy)]
struct MeDirectionalReferenceDescriptor {
    value_reference: u32,
    backing: MeDirectionalBacking,
}

#[derive(Clone, Copy)]
enum MeFloat64WriteTarget {
    State { base: usize, width: usize },
    Parameter { base: usize, width: usize },
}

struct MeFloat64Write {
    target: MeFloat64WriteTarget,
    value_offset: usize,
}

struct MeSetFloat64BatchAdmission {
    writes: Vec<MeFloat64Write>,
    values: Vec<f64>,
}

#[derive(Clone, Copy)]
enum MeFloat64ReadTarget {
    SolverVariable { base: usize, width: usize },
    Parameter { base: usize, width: usize },
    MaxStepDuration,
}

struct MeGetFloat64BatchAdmission {
    targets: Vec<MeFloat64ReadTarget>,
    serialized_width: usize,
}

/// Complete continuation state captured by `fmi3GetFMUState`.
///
/// This stays opaque outside the component implementation so a host cannot
/// synthesize a state that bypasses lifecycle or buffer invariants.
#[derive(Clone)]
pub(crate) struct MeKernelSnapshot {
    lifecycle: super::lifecycle::MeSavedLifecycle,
    stop_time: f64,
    time: f64,
    set_time_bounds: MeSetTimeBounds,
    post_event_eval_time: Option<f64>,
    event_anchor_time: f64,
    states: Vec<f64>,
    params: Vec<f64>,
    stop_schedule: SolveStopSchedule,
    pending_event_entry: Option<PendingEventEntry>,
    pending_state_event_entry: bool,
    pending_event_stop: Option<(f64, RuntimeEventStop)>,
    advance_state_to_event_right_limit: bool,
    state_time_coincidence: StateTimeCoincidence,
    initial_event_pending: bool,
    pending_root_crossings: Vec<RootCrossing>,
    indicator_storage: EventIndicatorStorageSnapshot,
    pending_event_pre_y: Option<Vec<f64>>,
    pending_event_pre_p: Option<Vec<f64>>,
    boundary_event_pre_y: Option<Vec<f64>>,
    boundary_event_pre_p: Option<Vec<f64>>,
    solver_y_guess: Vec<f64>,
    delay_params_scratch: Vec<f64>,
    delay_solver_y_scratch: Vec<f64>,
    derivative_cache: CachedDerivative,
    continuous_linearization_cache: CachedContinuousLinearization,
    max_step_duration: Option<f64>,
    termination: Option<SimTermination>,
    settled_initialization_y: Option<Vec<f64>>,
    runtime: SolveRuntimeSnapshot,
}

impl MeFmuState {
    /// The coordinate captured by the retained pristine instance.
    ///
    /// Session options deliberately do not carry another start-time author;
    /// the host derives its initial coordinate from this snapshot exactly
    /// once while constructing the leased session aggregate.
    pub(super) fn component_time(&self) -> f64 {
        self.component.time
    }
}

pub(super) fn event_right_limit_state_derivatives(
    runtime: &SolveRuntime,
    retained_solver_y: &[f64],
    time: f64,
    states: &[f64],
    params: &[f64],
    settle: AlgebraicSettle,
) -> Result<Vec<f64>, crate::runtime::solve_ops::RuntimeSolveError> {
    let mut solver_y_guess = retained_solver_y.to_vec();
    runtime.eval_state_derivatives_with_guess(
        time,
        states,
        params,
        &mut solver_y_guess,
        settle.tol,
        settle.max_iters,
    )
}

impl SolveMeKernel {
    /// Project the checked linked-runtime continuous-state domain for the host.
    pub(super) const fn continuous_state_domain(&self) -> MeContinuousStateDomain {
        self.body.state_domain
    }

    /// Issue the sole host root-scan storage shape from linked component facts.
    /// External code cannot mint or extract this construction capability:
    ///
    /// ```compile_fail,E0624
    /// use rumoca_solver::fmi_me::SolveMeKernel;
    ///
    /// fn reissue(kernel: &SolveMeKernel) {
    ///     let _ = kernel.root_scan_shape();
    /// }
    /// ```
    pub(super) fn root_scan_shape(&self) -> RootScanShape {
        RootScanShape::issue(
            RootScanStateWidth::issue(self.body.state_domain),
            RootScanIndicatorWidth::issue(self.body.indicator_plan().published_width()),
        )
    }

    pub(crate) fn model_description(&self) -> MeModelDescription<'_> {
        MeModelDescription {
            continuous_state_count: self.body.state_domain.len(),
            event_indicator_count: self.body.indicator_plan().len(),
            needs_completed_integrator_step: self
                .body
                .runtime
                .fmi_linked_runtime_facts()
                .needs_completed_integrator_step(),
            output_names: &self.body.runtime.output_names,
            input_names: &self.body.input_names,
            output_meta: &self.body.output_meta,
        }
    }

    pub(crate) fn get_nominals_of_continuous_states(
        &mut self,
        nominals: &mut [f64],
    ) -> Result<(), MeError> {
        let admission = self
            .lifecycle
            .admit_get_nominals_of_continuous_states()
            .map_err(lifecycle_contract)?;
        let state_nominals = self
            .body
            .runtime
            .fmi_linked_runtime_facts()
            .continuous_state_nominals();
        if nominals.len() != state_nominals.len() {
            return Err(contract(format!(
                "nominal buffer has {} entries for {} continuous states",
                nominals.len(),
                state_nominals.len()
            )));
        }
        nominals.copy_from_slice(state_nominals);
        admission.consume();
        Ok(())
    }

    pub(crate) fn value_reference(&self, name: &str) -> Option<MeValueRef> {
        self.body
            .value_references
            .iter()
            .find(|reference| reference.name == name)
            .map(|reference| MeValueRef {
                value_reference: reference.value_reference,
                backing: reference.backing,
                access: reference.access,
                instance_brand: Rc::clone(&self.body.instance_brand),
            })
    }

    pub(crate) fn max_step_duration_value_reference(&self) -> Option<MeValueRef> {
        self.body
            .max_step_duration_value_reference
            .and_then(|value_reference| {
                self.body.value_references.iter().find(|reference| {
                    reference.value_reference == value_reference
                        && reference.backing == MeFloat64Backing::MaxStepDuration
                })
            })
            .map(|reference| MeValueRef {
                value_reference: reference.value_reference,
                backing: reference.backing,
                access: reference.access,
                instance_brand: Rc::clone(&self.body.instance_brand),
            })
    }

    /// Construction-issued FMI value references for the component's
    /// continuous states, in model-description order.
    pub(crate) fn continuous_state_value_references(
        &self,
    ) -> Result<Vec<MeDirectionalValueRef>, MeError> {
        self.body.issue_directional_references(
            &self.body.directional_state_references,
            "continuous-state",
        )
    }

    /// Construction-issued FMI value references for the component's
    /// continuous-state derivatives, in model-description order.
    pub(crate) fn continuous_state_derivative_value_references(
        &self,
    ) -> Result<Vec<MeDirectionalValueRef>, MeError> {
        self.body.issue_directional_references(
            &self.body.directional_derivative_references,
            "continuous-state derivative",
        )
    }

    /// Validate a host-created FMI `knowns[]` list once and issue the branded
    /// batch consumed by `get_directional_derivative`.
    pub(crate) fn directional_known_batch(
        &self,
        references: Vec<MeDirectionalValueRef>,
    ) -> Result<MeDirectionalKnownBatch, MeError> {
        let serialized_width = self.body.admit_directional_references(
            &references,
            &self.body.directional_state_references,
            "known",
        )?;
        Ok(MeDirectionalKnownBatch {
            references,
            serialized_width,
            instance_brand: Rc::clone(&self.body.instance_brand),
        })
    }

    /// Validate a host-created FMI `unknowns[]` list once and issue the branded
    /// batch consumed by `get_directional_derivative`.
    pub(crate) fn directional_unknown_batch(
        &self,
        references: Vec<MeDirectionalValueRef>,
    ) -> Result<MeDirectionalUnknownBatch, MeError> {
        let serialized_width = self.body.admit_directional_references(
            &references,
            &self.body.directional_derivative_references,
            "unknown",
        )?;
        Ok(MeDirectionalUnknownBatch {
            references,
            serialized_width,
            instance_brand: Rc::clone(&self.body.instance_brand),
        })
    }

    pub fn enter_configuration_mode(&mut self) -> Result<(), MeError> {
        let admission = self
            .lifecycle
            .admit_enter_configuration_mode()
            .map_err(lifecycle_contract)?;
        admission.commit();
        Ok(())
    }

    pub fn exit_configuration_mode(&mut self) -> Result<(), MeError> {
        let admission = self
            .lifecycle
            .admit_exit_configuration_mode()
            .map_err(lifecycle_contract)?;
        admission.commit();
        Ok(())
    }

    pub(crate) fn enter_initialization_mode(&mut self, start_time: f64) -> Result<(), MeError> {
        #[cfg(test)]
        let inject_failure =
            std::mem::take(&mut self.body.verification_fail_next_enter_initialization);
        let admission = self
            .lifecycle
            .admit_enter_initialization_mode()
            .map_err(lifecycle_contract)
            .map_err(|error| error.at_stage(MeStage::Initialization))?;
        self.body
            .runtime
            .snapshot_into(&mut self.event_runtime_checkpoint);
        self.event_stage.prepare_event_stage_from(&self.body);
        if let Err(error) = self
            .event_stage
            .enter_initialization_mode_inner(start_time)
            .map_err(|error| error.at_stage(MeStage::Initialization))
        {
            self.body.runtime.restore(&self.event_runtime_checkpoint);
            return Err(error);
        }
        #[cfg(test)]
        if inject_failure {
            self.body.runtime.restore(&self.event_runtime_checkpoint);
            return Err(contract("injected fmi3EnterInitializationMode failure")
                .at_stage(MeStage::Initialization));
        }
        self.body.publish_event_stage(&mut self.event_stage);
        admission.commit();
        Ok(())
    }

    pub(crate) fn exit_initialization_mode(&mut self) -> Result<(), MeError> {
        #[cfg(test)]
        let inject_failure =
            std::mem::take(&mut self.body.verification_fail_next_exit_initialization);
        let admission = self
            .lifecycle
            .admit_exit_initialization_mode()
            .map_err(lifecycle_contract)
            .map_err(|error| error.at_stage(MeStage::Initialization))?;
        self.body
            .runtime
            .snapshot_into(&mut self.event_runtime_checkpoint);
        self.event_stage.prepare_event_stage_from(&self.body);
        if let Err(error) = self
            .event_stage
            .exit_initialization_mode_inner()
            .map_err(|error| error.at_stage(MeStage::Initialization))
        {
            self.body.runtime.restore(&self.event_runtime_checkpoint);
            return Err(error);
        }
        #[cfg(test)]
        if inject_failure {
            self.body.runtime.restore(&self.event_runtime_checkpoint);
            return Err(contract("injected fmi3ExitInitializationMode failure")
                .at_stage(MeStage::Initialization));
        }
        self.body.publish_event_stage(&mut self.event_stage);
        admission.commit();
        Ok(())
    }

    #[cfg(test)]
    pub(crate) fn verification_fail_next_enter_initialization(&mut self) {
        self.body.verification_fail_next_enter_initialization = true;
    }

    #[cfg(test)]
    pub(crate) fn verification_fail_next_exit_initialization(&mut self) {
        self.body.verification_fail_next_exit_initialization = true;
    }

    pub(crate) fn enter_event_mode(&mut self) -> Result<(), MeError> {
        let admission = self
            .lifecycle
            .admit_enter_event_mode()
            .map_err(lifecycle_contract)
            .map_err(|error| error.at_stage(MeStage::EventIteration))?;
        self.body
            .runtime
            .snapshot_into(&mut self.event_runtime_checkpoint);
        self.event_stage.prepare_event_stage_from(&self.body);
        let staged = &mut *self.event_stage;
        if let Err(error) = staged.classify_entered_state_event() {
            self.body.runtime.restore(&self.event_runtime_checkpoint);
            return Err(error.at_stage(MeStage::EventIteration));
        }
        let entry = staged.pending_event_entry();
        staged.clear_runtime_caches();
        staged.pending_event_entry = Some(entry);
        staged.pending_state_event_entry = false;
        staged.set_time_bounds.record_enter_event_mode(staged.time);
        self.body.publish_event_stage(staged);
        admission.commit();
        Ok(())
    }

    pub(crate) fn update_discrete_states(&mut self) -> Result<MeDiscreteStates, MeError> {
        let admission = self
            .lifecycle
            .admit_update_discrete_states()
            .map_err(lifecycle_contract)
            .map_err(|error| error.at_stage(MeStage::EventIteration))?;
        #[cfg(test)]
        let inject_failure =
            std::mem::take(&mut self.body.verification_fail_next_update_discrete_states);
        self.body
            .runtime
            .snapshot_into(&mut self.event_runtime_checkpoint);
        self.event_stage.prepare_event_stage_from(&self.body);
        let staged = &mut *self.event_stage;
        let evaluated = if staged.initial_event_pending {
            staged
                .run_initial_event_boundary()
                .map_err(|error| error.at_stage(MeStage::Initialization))
        } else {
            let entry = staged
                .pending_event_entry
                .take()
                .ok_or_else(|| contract("event mode has no pending event entry"))
                .map_err(|error| error.at_stage(MeStage::EventIteration))?;
            staged
                .run_runtime_event_boundary(entry)
                .map_err(|error| error.at_stage(MeStage::EventIteration))
        };
        let result = match evaluated {
            Ok(result) => result,
            Err(error) => {
                self.body.runtime.restore(&self.event_runtime_checkpoint);
                return Err(error);
            }
        };
        #[cfg(test)]
        if inject_failure {
            self.body.runtime.restore(&self.event_runtime_checkpoint);
            return Err(contract("injected fmi3UpdateDiscreteStates failure")
                .at_stage(MeStage::EventIteration));
        }
        self.body.publish_event_stage(staged);
        admission.commit();
        Ok(result)
    }

    #[cfg(test)]
    pub(crate) fn verification_fail_next_update_discrete_states(&mut self) {
        self.body.verification_fail_next_update_discrete_states = true;
    }

    pub(crate) fn enter_continuous_time_mode(&mut self) -> Result<(), MeError> {
        #[cfg(test)]
        let inject_failure =
            std::mem::take(&mut self.body.verification_fail_next_enter_continuous_time_mode);
        let admission = self
            .lifecycle
            .admit_enter_continuous_time_mode()
            .map_err(lifecycle_contract)?;
        if self.body.initial_event_pending || self.body.pending_event_entry.is_some() {
            return Err(contract(
                "enter_continuous_time_mode requires the pending event update to complete",
            ));
        }
        self.body
            .runtime
            .snapshot_into(&mut self.event_runtime_checkpoint);
        self.event_stage.prepare_event_stage_from(&self.body);
        let staged = &mut *self.event_stage;
        let transition = staged
            .commit_delay_point()
            .and_then(|()| staged.clear_all_scheduled_root_relation_memory())
            .and_then(|()| {
                staged.clear_callback_value_caches();
                staged.seed_settled_indicator_domains()
            });
        if let Err(error) = transition {
            self.body.runtime.restore(&self.event_runtime_checkpoint);
            return Err(error);
        }
        #[cfg(test)]
        if inject_failure {
            self.body.runtime.restore(&self.event_runtime_checkpoint);
            return Err(contract("injected fmi3EnterContinuousTimeMode failure")
                .at_stage(MeStage::EventIteration));
        }
        // Event Mode invalidates callback values, but `commit_delay_point`
        // has just certified the retained solver vector at this exact
        // coordinate. Preserve that proof for the root-domain seed. If
        // clearing scheduled relation memory changed a parameter slot, the
        // complete parameter-vector cache key forces the ordinary full solve.
        self.body.publish_event_stage(staged);
        admission.commit();
        Ok(())
    }

    #[cfg(test)]
    pub(crate) fn verification_fail_next_enter_continuous_time_mode(&mut self) {
        self.body.verification_fail_next_enter_continuous_time_mode = true;
    }

    pub(crate) fn terminate(&mut self) -> Result<(), MeError> {
        let admission = self
            .lifecycle
            .admit_terminate()
            .map_err(lifecycle_contract)?;
        rumoca_eval_solve::trace_solve_row_eval_snapshot(self.body.instance_name);
        admission.commit();
        Ok(())
    }

    pub(crate) fn set_time(&mut self, time: MeTime) -> Result<(), MeError> {
        let admission = self
            .lifecycle
            .admit_set_time()
            .map_err(lifecycle_contract)?;
        if !time.time.is_finite() {
            return Err(contract("set_time requires a finite time"));
        }
        let lower_bound = self.body.set_time_bounds.lower_bound();
        if time.time < lower_bound {
            return Err(contract(format!(
                "set_time {time} precedes the retained FMI lower bound {lower_bound}",
                time = time.time,
            )));
        }
        self.body.time = time.time;
        admission.consume();
        Ok(())
    }

    pub(crate) fn set_continuous_states(&mut self, states: &[f64]) -> Result<(), MeError> {
        let admission = self
            .lifecycle
            .admit_set_continuous_states()
            .map_err(lifecycle_contract)?;
        if states.len() != self.body.state_domain.len() {
            return Err(contract(format!(
                "continuous state buffer has {} entries for {} continuous states",
                states.len(),
                self.body.state_domain.len()
            )));
        }
        if states.iter().any(|value| !value.is_finite()) {
            return Err(contract("continuous state values must all be finite"));
        }
        self.body.states.copy_from_slice(states);
        admission.consume();
        Ok(())
    }

    pub(crate) fn get_continuous_states(&mut self, states: &mut [f64]) -> Result<(), MeError> {
        let admission = self
            .lifecycle
            .admit_get_continuous_states()
            .map_err(lifecycle_contract)?;
        if states.len() != self.body.state_domain.len() {
            return Err(contract(format!(
                "continuous state buffer has {} entries for {} continuous states",
                states.len(),
                self.body.state_domain.len()
            )));
        }
        states.copy_from_slice(&self.body.states);
        admission.consume();
        Ok(())
    }

    pub(crate) fn get_continuous_state_derivatives(
        &mut self,
        derivatives: &mut [f64],
    ) -> Result<(), MeError> {
        let admission = self
            .lifecycle
            .admit_get_continuous_state_derivatives()
            .map_err(lifecycle_contract)?;
        if derivatives.len() != self.body.state_domain.len() {
            return Err(contract(format!(
                "continuous-state derivative buffer has {} entries for {} states",
                derivatives.len(),
                self.body.state_domain.len(),
            )));
        }
        // Evaluate into construction-reserved caller-publication storage and
        // publish only after success, so a failing getter leaves every caller
        // slot unchanged (SPEC_0038). This storage is exactly `state_count`
        // wide; evaluator and delay workspaces retain their own behavior.
        let mut evaluated = self.body.derivative_output_scratch.borrow_mut();
        self.body
            .continuous_state_derivatives_into(&mut evaluated[..])?;
        derivatives.copy_from_slice(&evaluated[..]);
        admission.consume();
        Ok(())
    }

    pub(crate) fn get_directional_derivative(
        &mut self,
        unknowns: &MeDirectionalUnknownBatch,
        knowns: &MeDirectionalKnownBatch,
        seed: &[f64],
        sensitivity: &mut [f64],
    ) -> Result<(), MeError> {
        let admission = self
            .lifecycle
            .admit_get_directional_derivative()
            .map_err(lifecycle_contract)?;
        if !Rc::ptr_eq(&knowns.instance_brand, &self.body.instance_brand)
            || !Rc::ptr_eq(&unknowns.instance_brand, &self.body.instance_brand)
        {
            return Err(contract(
                "directional-derivative reference batch belongs to a different ME instance",
            ));
        }
        if seed.len() != knowns.serialized_width {
            return Err(contract(format!(
                "directional-derivative seed has {} entries for serialized known width {}",
                seed.len(),
                knowns.serialized_width
            ))
            .at_stage(MeStage::Integration));
        }
        if sensitivity.len() != unknowns.serialized_width {
            return Err(contract(format!(
                "directional-derivative sensitivity buffer has {} entries for serialized unknown width {}",
                sensitivity.len(),
                unknowns.serialized_width
            ))
            .at_stage(MeStage::Integration));
        }
        // Scatter the admitted seed into the construction-reserved full state
        // seed. The refill makes every position the batch does not cover a
        // zero seed, so nothing from an earlier call can leak into this one.
        let mut full_seed = self.body.directional_seed_scratch.borrow_mut();
        full_seed.fill(0.0);
        let mut seed_offset = 0;
        for reference in &knowns.references {
            let MeDirectionalBacking::ContinuousState { base, width } = reference.backing else {
                unreachable!("known batch construction admits only continuous states");
            };
            full_seed[base..base + width].copy_from_slice(&seed[seed_offset..seed_offset + width]);
            seed_offset += width;
        }
        // The same evaluation time and the same algebraic settle
        // `get_continuous_state_derivatives` uses, so the returned sensitivity
        // is the derivative of exactly the vector that operation reports rather
        // than of a differently-settled one.
        let time = self.body.continuous_eval_time();
        let settle = self.body.numerics_settle();
        // Evaluate into construction-reserved caller-publication storage and
        // publish only after success (SPEC_0038). `evaluated` is exactly
        // `state_count` wide, and `serialized` is reserved at the issued
        // directional table's full serialized width, of which this batch's
        // exact `serialized_width` prefix is consumed. Every prefix slot is
        // rewritten by the serialization loop below (the widths of the
        // admitted unknown references sum to `serialized_width`), so no
        // earlier call's value survives into the published sensitivity. The
        // production JVP evaluator and delay workspaces are not part of this
        // caller-publication reservation claim.
        let mut evaluated = self.body.directional_sensitivity_scratch.borrow_mut();
        self.body
            .with_delay_evaluation_params(time, &self.body.states, |params| {
                self.body.directional_derivative_at_parameters(
                    time,
                    params,
                    settle,
                    &full_seed[..],
                    &mut evaluated[..],
                )
            })
            .map_err(|error| error.at_stage(MeStage::Integration))?
            .map_err(|error| error.at_stage(MeStage::Integration))?;
        let mut serialized = self.body.directional_serialized_scratch.borrow_mut();
        let serialized = &mut serialized[..unknowns.serialized_width];
        let mut sensitivity_offset = 0;
        for reference in &unknowns.references {
            let MeDirectionalBacking::ContinuousStateDerivative { base, width } = reference.backing
            else {
                unreachable!("unknown batch construction admits only state derivatives");
            };
            serialized[sensitivity_offset..sensitivity_offset + width]
                .copy_from_slice(&evaluated[base..base + width]);
            sensitivity_offset += width;
        }
        sensitivity.copy_from_slice(serialized);
        admission.consume();
        Ok(())
    }

    pub(crate) fn get_event_indicators(&mut self, indicators: &mut [f64]) -> Result<(), MeError> {
        let admission = self
            .lifecycle
            .admit_get_event_indicators()
            .map_err(lifecycle_contract)?;
        let expected = self.body.indicator_plan().len();
        if indicators.len() != expected {
            return Err(contract(format!(
                "event-indicator buffer has {} entries for {} indicators",
                indicators.len(),
                expected,
            )));
        }
        // Evaluate into the construction-reserved scratch and publish to the
        // caller only after success, so a failing getter leaves every caller
        // slot unchanged (SPEC_0038). This getter body itself reserves nothing
        // (the scratch is exactly `expected` wide, so this copy never
        // resizes); transitive evaluation (`full_solver_y`, the
        // `cached_continuous_solver_y` clone on a linearization-cache hit, the
        // evaluator, delay workspaces) reserves on its own terms.
        let mut evaluated = self.body.indicator_storage.publication_values_mut();
        self.body.event_indicators_into(&mut evaluated)?;
        indicators.copy_from_slice(evaluated.as_slice());
        admission.consume();
        Ok(())
    }

    pub(crate) fn completed_integrator_step(
        &mut self,
        _no_set_fmu_state_prior_to_current_point: bool,
    ) -> Result<MeCompletedIntegratorStep, MeError> {
        let admission = self
            .lifecycle
            .admit_completed_integrator_step()
            .map_err(lifecycle_contract)?;
        #[cfg(test)]
        let inject_failure =
            std::mem::take(&mut self.body.verification_fail_next_completed_integrator_step);
        self.body
            .runtime
            .snapshot_into(&mut self.event_runtime_checkpoint);
        self.event_stage.prepare_event_stage_from(&self.body);
        let staged = &mut *self.event_stage;
        staged.post_event_eval_time = None;
        if let Err(error) = staged.freeze_completed_indicator_domains() {
            self.body.runtime.restore(&self.event_runtime_checkpoint);
            return Err(error);
        }
        // FMI does not let an importer hand an FSAL stage into the FMU. Keep
        // the accepted-point cache private by evaluating the standard
        // derivative operation. State and time events remain importer-owned;
        // this callback may request Event Mode only for an independent step
        // event, which the current checked component does not declare.
        if let Err(error) = staged.cache_accepted_derivatives() {
            self.body.runtime.restore(&self.event_runtime_checkpoint);
            return Err(error);
        }
        #[cfg(test)]
        if inject_failure {
            self.body.runtime.restore(&self.event_runtime_checkpoint);
            return Err(contract("injected fmi3CompletedIntegratorStep failure")
                .at_stage(MeStage::Integration));
        }
        if let Err(error) = staged.commit_delay_point() {
            self.body.runtime.restore(&self.event_runtime_checkpoint);
            return Err(error.at_stage(MeStage::Integration));
        }
        staged
            .set_time_bounds
            .record_completed_integrator_step(staged.time);
        self.body.publish_event_stage(staged);
        admission.consume();
        Ok(MeCompletedIntegratorStep {
            enter_event_mode: false,
            terminate_simulation: false,
        })
    }

    #[cfg(test)]
    pub(crate) fn verification_fail_next_completed_integrator_step(&mut self) {
        self.body.verification_fail_next_completed_integrator_step = true;
    }

    pub(crate) fn set_float64(
        &mut self,
        refs: &[MeValueRef],
        values: &[f64],
    ) -> Result<(), MeError> {
        let lifecycle = self
            .lifecycle
            .admit_set_float64()
            .map_err(lifecycle_contract)?;
        let checkpoint = self.body.fmu_state_inner(lifecycle.saved_lifecycle());
        let admission = self
            .body
            .admit_float64_writes(lifecycle.state(), refs, values)?;
        self.body.set_float64_admitted(admission, &checkpoint)?;
        lifecycle.consume();
        Ok(())
    }
}

impl MeKernelBody {
    fn issue_directional_references(
        &self,
        descriptors: &[MeDirectionalReferenceDescriptor],
        label: &'static str,
    ) -> Result<Vec<MeDirectionalValueRef>, MeError> {
        let mut references = Vec::new();
        references
            .try_reserve_exact(descriptors.len())
            .map_err(|error| {
                contract(format!(
                    "failed to reserve {} {label} value references: {error}",
                    descriptors.len()
                ))
            })?;
        references.extend(descriptors.iter().map(|descriptor| MeDirectionalValueRef {
            value_reference: descriptor.value_reference,
            backing: descriptor.backing,
            instance_brand: Rc::clone(&self.instance_brand),
        }));
        Ok(references)
    }

    fn admit_directional_references(
        &self,
        references: &[MeDirectionalValueRef],
        issued: &[MeDirectionalReferenceDescriptor],
        role: &'static str,
    ) -> Result<usize, MeError> {
        let mut serialized_width = 0usize;
        for (index, reference) in references.iter().enumerate() {
            if !Rc::ptr_eq(&reference.instance_brand, &self.instance_brand) {
                return Err(contract(format!(
                    "directional-derivative {role} reference belongs to a different ME instance"
                )));
            }
            if !issued.iter().any(|candidate| {
                candidate.value_reference == reference.value_reference
                    && candidate.backing == reference.backing
            }) {
                return Err(contract(format!(
                    "value reference {} is not an issued directional-derivative {role}",
                    reference.value_reference
                )));
            }
            if references[..index]
                .iter()
                .any(|earlier| earlier.value_reference == reference.value_reference)
            {
                return Err(contract(format!(
                    "directional-derivative {role} value reference {} is duplicated",
                    reference.value_reference
                )));
            }
            let width = match reference.backing {
                MeDirectionalBacking::ContinuousState { base, width }
                | MeDirectionalBacking::ContinuousStateDerivative { base, width }
                    if base
                        .checked_add(width)
                        .is_some_and(|end| end <= self.state_domain.len()) =>
                {
                    width
                }
                _ => {
                    return Err(contract(format!(
                        "directional-derivative {role} value reference {} has invalid storage",
                        reference.value_reference
                    )));
                }
            };
            serialized_width = serialized_width.checked_add(width).ok_or_else(|| {
                contract(format!(
                    "serialized directional-derivative {role} width overflows"
                ))
            })?;
        }
        Ok(serialized_width)
    }

    fn admit_float64_writes(
        &self,
        lifecycle_state: MeState,
        refs: &[MeValueRef],
        values: &[f64],
    ) -> Result<MeSetFloat64BatchAdmission, MeError> {
        let mut writes = Vec::new();
        writes.try_reserve_exact(refs.len()).map_err(|error| {
            contract(format!(
                "failed to reserve {} admitted Float64 writes: {error}",
                refs.len()
            ))
        })?;
        let mut serialized_width = 0usize;
        for reference in refs {
            if !Rc::ptr_eq(&reference.instance_brand, &self.instance_brand)
                || !self.float64_reference_is_issued(reference)
                || !lifecycle_state
                    .fmi3_write_mode()
                    .is_some_and(|mode| reference.access.write_modes.admits(mode))
            {
                return Err(contract(format!(
                    "Float64 value reference {} with backing {:?} is not writable in {}",
                    reference.value_reference,
                    reference.backing,
                    lifecycle_state.name(),
                )));
            }
            let target = match reference.backing {
                MeFloat64Backing::SolverVariable { base, width }
                    if base
                        .checked_add(width)
                        .is_some_and(|end| end <= self.state_domain.len()) =>
                {
                    MeFloat64WriteTarget::State { base, width }
                }
                MeFloat64Backing::Parameter { base, width }
                    if base
                        .checked_add(width)
                        .is_some_and(|end| end <= self.params.len()) =>
                {
                    MeFloat64WriteTarget::Parameter { base, width }
                }
                _ => {
                    return Err(contract(format!(
                        "Float64 value reference {:?} has no writable component storage",
                        reference.backing,
                    )));
                }
            };
            let width = match target {
                MeFloat64WriteTarget::State { width, .. }
                | MeFloat64WriteTarget::Parameter { width, .. } => width,
            };
            writes.push(MeFloat64Write {
                target,
                value_offset: serialized_width,
            });
            serialized_width = serialized_width
                .checked_add(width)
                .ok_or_else(|| contract("serialized Float64 write width overflows"))?;
        }
        if serialized_width != values.len() {
            return Err(contract(format!(
                "{} Float64 value references serialize to {serialized_width} values, not {}",
                refs.len(),
                values.len()
            )));
        }
        if values.iter().any(|value| !value.is_finite()) {
            return Err(contract("Float64 values must all be finite"));
        }
        Ok(MeSetFloat64BatchAdmission {
            writes,
            values: values.to_vec(),
        })
    }

    fn set_float64_admitted(
        &mut self,
        admission: MeSetFloat64BatchAdmission,
        checkpoint: &MeFmuState,
    ) -> Result<(), MeError> {
        let MeSetFloat64BatchAdmission { writes, values } = admission;
        for write in writes {
            match write.target {
                MeFloat64WriteTarget::State { base, width } => self.states[base..base + width]
                    .copy_from_slice(&values[write.value_offset..write.value_offset + width]),
                MeFloat64WriteTarget::Parameter { base, width } => self.params[base..base + width]
                    .copy_from_slice(&values[write.value_offset..write.value_offset + width]),
            }
        }
        self.clear_runtime_caches();
        if let Err(error) = self.refresh_current_delay_facts() {
            self.restore_snapshot_body(&checkpoint.component)?;
            return Err(error);
        }
        Ok(())
    }
}

impl SolveMeKernel {
    pub(crate) fn get_float64(
        &mut self,
        refs: &[MeValueRef],
        values: &mut [f64],
    ) -> Result<(), MeError> {
        let lifecycle = self
            .lifecycle
            .admit_get_float64()
            .map_err(lifecycle_contract)?;
        let admission = self.body.admit_float64_reads(refs, values.len())?;
        let evaluated = self.body.get_float64_admitted(admission)?;
        values.copy_from_slice(&evaluated);
        lifecycle.consume();
        Ok(())
    }
}

impl MeKernelBody {
    fn admit_float64_reads(
        &self,
        refs: &[MeValueRef],
        result_len: usize,
    ) -> Result<MeGetFloat64BatchAdmission, MeError> {
        let mut targets = Vec::new();
        targets.try_reserve_exact(refs.len()).map_err(|error| {
            contract(format!(
                "failed to reserve {} admitted Float64 reads: {error}",
                refs.len()
            ))
        })?;
        let mut serialized_width = 0usize;
        for reference in refs {
            if !Rc::ptr_eq(&reference.instance_brand, &self.instance_brand)
                || !self.float64_reference_is_issued(reference)
            {
                return Err(contract(
                    "Float64 value reference was not issued by this ME instance",
                ));
            }
            let target = match reference.backing {
                MeFloat64Backing::SolverVariable { base, width }
                    if base
                        .checked_add(width)
                        .is_some_and(|end| end <= self.runtime.model().initial_y().len()) =>
                {
                    MeFloat64ReadTarget::SolverVariable { base, width }
                }
                MeFloat64Backing::Parameter { base, width }
                    if base
                        .checked_add(width)
                        .is_some_and(|end| end <= self.params.len()) =>
                {
                    MeFloat64ReadTarget::Parameter { base, width }
                }
                MeFloat64Backing::MaxStepDuration
                    if self.max_step_duration_value_reference
                        == Some(reference.value_reference) =>
                {
                    MeFloat64ReadTarget::MaxStepDuration
                }
                MeFloat64Backing::MaxStepDuration => {
                    return Err(contract(
                        "maximum-step-duration value reference is undeclared",
                    ));
                }
                MeFloat64Backing::SolverVariable { base, width } => {
                    return Err(contract(format!(
                        "Float64 solver-variable run {base}..{} is outside {} variables",
                        base.saturating_add(width),
                        self.runtime.model().initial_y().len(),
                    )));
                }
                MeFloat64Backing::Parameter { base, width } => {
                    return Err(float64_param_out_of_range(
                        base.saturating_add(width).saturating_sub(1),
                        self.params.len(),
                    ));
                }
            };
            let width = match target {
                MeFloat64ReadTarget::SolverVariable { width, .. }
                | MeFloat64ReadTarget::Parameter { width, .. } => width,
                MeFloat64ReadTarget::MaxStepDuration => 1,
            };
            serialized_width = serialized_width
                .checked_add(width)
                .ok_or_else(|| contract("serialized Float64 read width overflows"))?;
            targets.push(target);
        }
        if serialized_width != result_len {
            return Err(contract(format!(
                "{} Float64 value references serialize to {serialized_width} result values, not {result_len}",
                refs.len(),
            )));
        }
        Ok(MeGetFloat64BatchAdmission {
            targets,
            serialized_width,
        })
    }

    fn get_float64_admitted(
        &self,
        admission: MeGetFloat64BatchAdmission,
    ) -> Result<Vec<f64>, MeError> {
        let MeGetFloat64BatchAdmission {
            targets,
            serialized_width,
        } = admission;
        let needs_coordinate = targets
            .iter()
            .any(|target| !matches!(target, MeFloat64ReadTarget::MaxStepDuration));
        let coordinate = needs_coordinate
            .then(|| self.observation_coordinate())
            .transpose()?;
        let mut values = Vec::new();
        values
            .try_reserve_exact(serialized_width)
            .map_err(|error| {
                contract(format!(
                    "failed to reserve {serialized_width} Float64 result values: {error}",
                ))
            })?;
        for target in targets {
            match target {
                MeFloat64ReadTarget::SolverVariable { base, width } => {
                    let solver_y = coordinate
                        .as_ref()
                        .map(|(solver_y, _)| solver_y)
                        .ok_or_else(|| contract("admitted Float64 solver run is unavailable"))?;
                    values.extend_from_slice(&solver_y[base..base + width]);
                }
                MeFloat64ReadTarget::Parameter { base, width } => {
                    let parameters = coordinate
                        .as_ref()
                        .map(|(_, parameters)| parameters)
                        .ok_or_else(|| contract("admitted Float64 parameter run is unavailable"))?;
                    values.extend_from_slice(&parameters[base..base + width]);
                }
                MeFloat64ReadTarget::MaxStepDuration => values.push(
                    self.max_step_duration
                        .unwrap_or(rumoca_ir_solve::fmi::MAX_STEP_DURATION_UNCONSTRAINED),
                ),
            }
        }
        Ok(values)
    }

    fn float64_reference_is_issued(&self, reference: &MeValueRef) -> bool {
        self.value_references.iter().any(|issued| {
            issued.value_reference == reference.value_reference
                && issued.backing == reference.backing
                && issued.access == reference.access
        })
    }
}

impl SolveMeKernel {
    pub(crate) fn fmu_state(&mut self) -> MeFmuState {
        let admission = self.lifecycle.admit_get_fmu_state();
        let state = self.body.fmu_state_inner(admission.saved_lifecycle());
        admission.consume();
        state
    }
}

impl MeKernelBody {
    fn fmu_state_inner(&self, lifecycle: super::lifecycle::MeSavedLifecycle) -> MeFmuState {
        MeFmuState {
            component: MeKernelSnapshot {
                lifecycle,
                stop_time: self.stop_time,
                time: self.time,
                set_time_bounds: self.set_time_bounds,
                post_event_eval_time: self.post_event_eval_time,
                event_anchor_time: self.event_anchor_time,
                states: self.states.clone(),
                params: self.params.clone(),
                stop_schedule: self.stop_schedule.clone(),
                pending_event_entry: self.pending_event_entry,
                pending_state_event_entry: self.pending_state_event_entry,
                pending_event_stop: self.pending_event_stop,
                advance_state_to_event_right_limit: self.advance_state_to_event_right_limit,
                state_time_coincidence: self.state_time_coincidence,
                initial_event_pending: self.initial_event_pending,
                pending_root_crossings: self.pending_root_crossings.clone(),
                indicator_storage: self.indicator_storage.snapshot(),
                pending_event_pre_y: self.pending_event_pre_y.snapshot(),
                pending_event_pre_p: self.pending_event_pre_p.snapshot(),
                boundary_event_pre_y: self.boundary_event_pre_y.snapshot(),
                boundary_event_pre_p: self.boundary_event_pre_p.snapshot(),
                solver_y_guess: self.solver_y_guess.borrow().clone(),
                delay_params_scratch: self.delay_params_scratch.borrow().clone(),
                delay_solver_y_scratch: self.delay_solver_y_scratch.borrow().clone(),
                derivative_cache: self.derivative_cache.borrow().clone(),
                continuous_linearization_cache: self
                    .continuous_linearization_cache
                    .borrow()
                    .clone(),
                max_step_duration: self.max_step_duration,
                termination: self.termination.clone(),
                settled_initialization_y: self.settled_initialization_y.snapshot(),
                runtime: self.runtime.snapshot(),
            },
            instance_brand: Rc::clone(&self.instance_brand),
        }
    }

    fn validate_snapshot_brand(&self, saved: &MeFmuState) -> Result<(), MeError> {
        if !Rc::ptr_eq(&saved.instance_brand, &self.instance_brand) {
            return Err(contract(
                "component snapshot belongs to a different ME instance",
            ));
        }
        Ok(())
    }

    fn restore_snapshot_body(&mut self, state: &MeKernelSnapshot) -> Result<(), MeError> {
        self.stop_time = state.stop_time;
        self.time = state.time;
        self.set_time_bounds = state.set_time_bounds;
        self.post_event_eval_time = state.post_event_eval_time;
        self.event_anchor_time = state.event_anchor_time;
        self.states.clone_from(&state.states);
        self.params.clone_from(&state.params);
        self.stop_schedule.clone_from(&state.stop_schedule);
        self.pending_event_entry = state.pending_event_entry;
        self.pending_state_event_entry = state.pending_state_event_entry;
        self.pending_event_stop = state.pending_event_stop;
        self.advance_state_to_event_right_limit = state.advance_state_to_event_right_limit;
        self.state_time_coincidence = state.state_time_coincidence;
        self.initial_event_pending = state.initial_event_pending;
        self.pending_root_crossings
            .clone_from(&state.pending_root_crossings);
        self.indicator_storage.restore(&state.indicator_storage);
        self.pending_event_pre_y
            .restore(state.pending_event_pre_y.as_ref())?;
        self.pending_event_pre_p
            .restore(state.pending_event_pre_p.as_ref())?;
        self.boundary_event_pre_y
            .restore(state.boundary_event_pre_y.as_ref())?;
        self.boundary_event_pre_p
            .restore(state.boundary_event_pre_p.as_ref())?;
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
            .copy_from(&state.derivative_cache);
        self.continuous_linearization_cache
            .borrow_mut()
            .copy_from(&state.continuous_linearization_cache);
        self.max_step_duration = state.max_step_duration;
        self.termination.clone_from(&state.termination);
        self.settled_initialization_y
            .restore(state.settled_initialization_y.as_ref())?;
        self.runtime.restore(&state.runtime);
        Ok(())
    }
}

impl SolveMeKernel {
    pub(crate) fn reset_to_fmu_state(&mut self, saved: &MeFmuState) -> Result<(), MeError> {
        let admission = self.lifecycle.admit_set_fmu_state();
        self.body.validate_snapshot_brand(saved)?;
        self.body.restore_snapshot_body(&saved.component)?;
        admission.restore(&saved.component.lifecycle);
        Ok(())
    }
}

fn float_slice_bit_eq(left: &[f64], right: &[f64]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| left.to_bits() == right.to_bits())
}

pub(super) fn continuous_state_values_changed(before: &[f64], after: &[f64]) -> bool {
    !float_slice_bit_eq(before, after)
}

#[cfg(test)]
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

#[cfg(test)]
fn option_event_entry_bit_eq(
    left: Option<PendingEventEntry>,
    right: Option<PendingEventEntry>,
) -> bool {
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

#[cfg(test)]
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

#[cfg(test)]
fn root_crossings_bit_eq(left: &[RootCrossing], right: &[RootCrossing]) -> bool {
    left.len() == right.len()
        && left.iter().zip(right).all(|(left, right)| {
            left.index == right.index
                && left.post_relation_memory_value.to_bits()
                    == right.post_relation_memory_value.to_bits()
        })
}

#[cfg(test)]
fn derivative_cache_bit_eq(left: &CachedDerivative, right: &CachedDerivative) -> bool {
    left.valid == right.valid
        && left.time.to_bits() == right.time.to_bits()
        && float_slice_bit_eq(&left.state, &right.state)
        && float_slice_bit_eq(&left.derivative, &right.derivative)
}

#[cfg(test)]
fn continuous_linearization_cache_bit_eq(
    left: &CachedContinuousLinearization,
    right: &CachedContinuousLinearization,
) -> bool {
    left.valid == right.valid
        && left.time.to_bits() == right.time.to_bits()
        && float_slice_bit_eq(&left.state, &right.state)
        && float_slice_bit_eq(&left.parameters, &right.parameters)
}

#[cfg(test)]
fn termination_bit_eq(left: Option<&SimTermination>, right: Option<&SimTermination>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => {
            left.time.to_bits() == right.time.to_bits() && left.message == right.message
        }
        (None, None) => true,
        _ => false,
    }
}

/// Error for a Float64 input value reference outside the parameter vector.
fn float64_param_out_of_range(index: usize, len: usize) -> MeError {
    contract(format!(
        "Float64 input value reference {index} is outside {len} parameters"
    ))
}

fn contract(reason: impl Into<String>) -> MeError {
    MeError::Contract {
        reason: reason.into(),
    }
}

fn lifecycle_contract(violation: MeLifecycleViolation) -> MeError {
    contract(format!(
        "{} is invalid in ME lifecycle state {}",
        violation.operation.name(),
        violation.state.name(),
    ))
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
    p: &mut [f64],
    t: f64,
    policy: MeAlgebraicProjectionPolicy,
) -> Result<bool, crate::runtime::solve_ops::RuntimeSolveError> {
    let tol = policy.tolerance;
    let before = y.to_vec();
    runtime.project_state_manifold(y, p, t, tol)?;
    runtime.refresh_algebraic_and_output_slots_certified(
        t,
        y,
        p,
        ALGEBRAIC_REFRESH_TOL,
        UPDATE_MAX_ITERS,
    )?;
    Ok(runtime_values_changed(&before, y, tol))
}

fn project_event_algebraics(
    runtime: &SolveRuntime,
    y: &mut [f64],
    p: &mut [f64],
    t: f64,
    policy: MeAlgebraicProjectionPolicy,
) -> Result<bool, crate::runtime::solve_ops::RuntimeSolveError> {
    let before = y.to_vec();
    runtime.project_state_manifold(y, p, t, policy.tolerance)?;
    runtime.refresh_event_dependency_slots_certified(
        t,
        y,
        p,
        policy.settle.tol,
        policy.settle.max_iters,
    )?;
    Ok(runtime_values_changed(&before, y, policy.tolerance))
}
