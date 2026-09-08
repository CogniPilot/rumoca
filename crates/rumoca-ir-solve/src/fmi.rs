//! The FMI description of the simulation problem.
//!
//! FMI Model Exchange standardises the component/host boundary while leaving
//! numerical integration to the host. Its component description is a checked
//! refinement of Solve rather than a parallel solver vocabulary. An
//! [`FmiComponent`] binds tensor-native source
//! declarations to the exact Solve storage runs that execute them, and FMI 2
//! scalar variables and FMI 3 aggregate value references are derived views of
//! this one checked object.
//!
//! One ordered inventory holds every addressable declaration. Every nonempty
//! numeric entry is backed by one Solve storage run; a delay-bearing kernel also
//! publishes the maximum-step-duration local named [`MAX_STEP_DURATION_NAME`],
//! which is an ordinary entry of that same inventory whose value the component
//! derives rather than reads from storage. Each entry names its one backing
//! owner, so a storage-only reader filters the inventory instead of consulting
//! a second collection.
//!
//! Value-reference identity is assigned here, once, because it is a contract
//! between artefacts that cannot re-derive it independently: the
//! `modelDescription.xml`, the generated C, and the in-process solver must all
//! address the same storage slot by the same number.
//!
//! The component owns the executable kernel it describes. It consumes one
//! complete [`SolveModel`] into a private `Arc`, exposes only borrowed views of
//! it, and hands codegen a correlated [`FmiCodegenView`] that no caller can
//! build from parts. Metadata therefore cannot be paired with a foreign kernel,
//! and no path returns an owned bare Solve root.
//!
//! A storage-backed rendering never receives the correlated view directly. It
//! receives [`FmiEventFreeCodegenView`], the narrowed type-state that exists
//! only where the kernel owns no semantic event class and every entry has the
//! storage run and `start` such a template reads, and which is also the only
//! whole-inventory encoding.

mod event_free;
mod linked_runtime;
mod max_step_duration;
mod metadata;
mod projection;
mod scalar_constant_derivative;
#[cfg(test)]
mod tests;
mod write_modes;

pub use event_free::{FmiEventFreeCodegenView, FmiEventFreeError};
pub use linked_runtime::{
    FmiContinuousStateWidth, FmiDeadlineWidth, FmiDelayCapability,
    FmiDirectionalReferenceDescriptor, FmiEventIndicatorEntry, FmiEventIndicatorPlan,
    FmiIndicatorDomainWidth, FmiIndicatorReading, FmiIndicatorWidth, FmiIndicatorZeroSide,
    FmiLinkedRuntimeFacts, FmiPublishedIndicatorWidth, FmiRootValueWidth, FmiRuntimeFloat64Backing,
    FmiRuntimeFloat64Descriptor,
};
pub use max_step_duration::{
    MAX_STEP_DURATION_DESCRIPTION, MAX_STEP_DURATION_NAME, MAX_STEP_DURATION_UNCONSTRAINED,
    MAX_STEP_DURATION_UNIT,
};
pub use metadata::{
    FmiCausality, FmiInitial, FmiStateInitial, FmiStateReinit, FmiStorageRun, FmiValueBacking,
    FmiVariability, FmiVariable, FmiWritePolicy,
};
pub use projection::{
    Fmi2DerivativeVariable, Fmi2Projection, Fmi2ScalarVariable, Fmi3DerivativeVariable,
    Fmi3Projection, Fmi3TensorVariable, FmiDerivativeLink, FmiDerivativeStorageRange,
    FmiModelStructureMembership, FmiProjectionError, FmiUnitDefinition,
};
pub use scalar_constant_derivative::{
    DerivativeKernelFacts, Fmi3InventoryEntryFact, Fmi3ScalarConstantDerivativeCarrier,
    Fmi3StateDerivativeFact, Fmi3StateVariableFact, KernelOperationFact,
    ScalarConstantDerivativeDisagreement, ScalarConstantDerivativeError,
    ScalarConstantDerivativeFmi3Facts, ScalarConstantDerivativeReceipt,
    ScalarConstantDerivativeSolveFacts, ScalarConstantDerivativeSystemFacts,
    ScalarConstantDerivativeUnsupported, SolveScalarVariableFact, SolveStorageFact, StartLeg,
    check_scalar_constant_derivative_projection, project_scalar_constant_derivative_fmi3_facts,
    project_scalar_constant_derivative_solve_facts,
};
pub use write_modes::{Fmi2WriteMode, Fmi2WriteModes, Fmi3WriteMode, Fmi3WriteModes};

/// Configuration-Mode capability declared by the checked FMI component.
///
/// The ordinary Modelica-to-ME projection currently declares no structural
/// FMI parameter, so its constructor yields [`Self::Absent`]. The enabled
/// variants belong to the same aggregate for layered profiles that construct
/// such a variable; the runtime consumes this typed fact rather than inferring
/// capability from an ordinary tunable parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FmiConfigurationCapability {
    Absent,
    FixedStructuralParameter,
    TunableStructuralParameter,
}

use crate::{
    ScalarSlot, SolveArtifacts, SolveModel, SolveProblem, SolveStateInitialization,
    SolveStorageColumn, SolveVariableCatalogEntry, SolveVariableCausality,
    SolveVariableStorageRole, SolveVariableVariability,
};
use rumoca_core::Span;
use std::collections::BTreeSet;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum FmiComponentError {
    #[error("FMI 3 value-reference space exceeds u32")]
    ValueReferenceOverflow,
    #[error("FMI state scalar count {actual} does not match Solve state count {expected}")]
    StateCount { actual: usize, expected: usize },
    #[error("source variable takes the `{name}` FMI local a delay-bearing kernel publishes")]
    ReservedMaxStepDurationName {
        name: &'static str,
        declaration: Span,
    },
    #[error("FMI event-indicator inventory is invalid: {message}")]
    EventIndicatorInventory { message: String, span: Option<Span> },
    #[error("FMI continuous-state reinit evidence is invalid: {message}")]
    StateReinitEvidence { message: String },
    #[error("FMI variable `{name}` has a causality/variability the catalog never issues")]
    UnclassifiedWritePolicy { name: String, span: Span },
}

impl FmiComponentError {
    #[must_use]
    pub const fn span(&self) -> Option<Span> {
        match self {
            Self::ReservedMaxStepDurationName { declaration, .. } => Some(*declaration),
            Self::EventIndicatorInventory { span, .. } => *span,
            Self::UnclassifiedWritePolicy { span, .. } => Some(*span),
            Self::ValueReferenceOverflow
            | Self::StateCount { .. }
            | Self::StateReinitEvidence { .. } => None,
        }
    }
}

/// One source in the ordered FMI Model Exchange event-indicator vector.
///
/// This is compiler metadata, not a host-side filter: each entry identifies
/// the checked semantic owner whose scalar value occupies the corresponding
/// FMI position.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FmiEventIndicatorSource {
    RootCondition { index: usize },
    DynamicTimeEvent { index: usize },
    DelayDiscontinuity { index: usize },
}

impl FmiEventIndicatorSource {
    #[must_use]
    pub const fn source_index(self) -> usize {
        match self {
            Self::RootCondition { index }
            | Self::DynamicTimeEvent { index }
            | Self::DelayDiscontinuity { index } => index,
        }
    }
}

/// How a row that reads `time` but no live continuous state is owned.
///
/// Solver-`Y` dependencies do not describe `time`, so the two blocks below
/// need different readings of an empty dependency set. A root condition *is*
/// its own indicator, so one that reads `time` sweeps continuously between
/// events and has to be monitored. A dynamic-time deadline is reported
/// relative to the evaluation time and its event is owned by the time
/// schedule, which announces a state-independent deadline exactly.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum TimeReadOwner {
    /// Reading `time` makes the row vary during integration: monitor it.
    Monitored,
    /// The time schedule announces this row exactly: do not monitor it.
    Announced,
}

#[derive(Debug)]
pub struct FmiEventIndicatorInventory {
    sources: Box<[FmiEventIndicatorSource]>,
}

impl FmiEventIndicatorInventory {
    pub fn derive(model: &SolveModel) -> Result<Self, FmiComponentError> {
        let static_y = model
            .problem()
            .continuous()
            .refresh_owners
            .root()
            .static_causal_rows()
            .iter()
            .map(|row| row.target_index())
            .collect::<BTreeSet<_>>();
        let scheduled = model
            .problem()
            .events()
            .scheduled_root_conditions
            .iter()
            .map(|root| root.root_index)
            .collect::<BTreeSet<_>>();
        let mut sources = dependency_backed_indicator_sources(
            &model.problem().events().root_conditions,
            &static_y,
            TimeReadOwner::Monitored,
            |index| {
                (!scheduled.contains(&index))
                    .then_some(FmiEventIndicatorSource::RootCondition { index })
            },
        )?;
        sources.extend(dependency_backed_indicator_sources(
            &model.problem().events().dynamic_time_event_rhs,
            &static_y,
            TimeReadOwner::Announced,
            |index| Some(FmiEventIndicatorSource::DynamicTimeEvent { index }),
        )?);
        sources.extend(
            (0..model
                .problem()
                .events()
                .delays
                .delay_time_rhs
                .output_count())
                .map(|index| FmiEventIndicatorSource::DelayDiscontinuity { index }),
        );
        Ok(Self {
            sources: sources.into_boxed_slice(),
        })
    }

    #[must_use]
    pub fn sources(&self) -> &[FmiEventIndicatorSource] {
        &self.sources
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.sources.len()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.sources.is_empty()
    }
}

/// Select the rows of `block` that can change value during continuous
/// integration, in checked output order.
///
/// A row varies between events when it reads a solver-`Y` coordinate the
/// static causality does not fix, and, for a [`TimeReadOwner::Monitored`]
/// block, when it reads `time`. The `time` half is not redundant: a `time`
/// relation whose instant the schedule cannot own, such as `time > 0` at the
/// start of the interval, reads no `Y` at all, so a `Y`-only reading would
/// call it invariant and drop the only surface its event has.
fn dependency_backed_indicator_sources(
    block: &crate::ScalarProgramBlock,
    static_y: &BTreeSet<usize>,
    time_reads: TimeReadOwner,
    mut source: impl FnMut(usize) -> Option<FmiEventIndicatorSource>,
) -> Result<Vec<FmiEventIndicatorSource>, FmiComponentError> {
    let mut sources = Vec::new();
    let mut output_ordinal = 0usize;
    for (program_index, program) in block.programs().iter().enumerate() {
        let span = block.program_span(program_index);
        let dependencies = crate::StructuralPattern::derive_output_y_dependencies(program, span)
            .map_err(|error| FmiComponentError::EventIndicatorInventory {
                message: error.to_string(),
                span,
            })?;
        let time_dependencies = match time_reads {
            TimeReadOwner::Monitored => crate::StructuralPattern::derive_output_time_dependencies(
                program, span,
            )
            .map_err(|error| FmiComponentError::EventIndicatorInventory {
                message: error.to_string(),
                span,
            })?,
            TimeReadOwner::Announced => vec![false; dependencies.len()],
        };
        if time_dependencies.len() != dependencies.len() {
            return Err(FmiComponentError::EventIndicatorInventory {
                message: "indicator solver-Y and time dependencies disagree on output count"
                    .to_string(),
                span,
            });
        }
        for (dependencies, reads_time) in dependencies.into_iter().zip(time_dependencies) {
            let output_index = block
                .output_indices()
                .get(output_ordinal)
                .copied()
                .ok_or_else(|| FmiComponentError::EventIndicatorInventory {
                    message: "indicator output has no checked scalar identity".to_string(),
                    span,
                })?;
            if (!dependencies.is_subset(static_y) || reads_time)
                && let Some(source) = source(output_index)
            {
                sources.push(source);
            }
            output_ordinal = output_ordinal.checked_add(1).ok_or_else(|| {
                FmiComponentError::EventIndicatorInventory {
                    message: "indicator output ordinal overflows".to_string(),
                    span,
                }
            })?;
        }
    }
    if output_ordinal != block.output_indices().len() {
        return Err(FmiComponentError::EventIndicatorInventory {
            message: "indicator programs and checked output identities disagree".to_string(),
            span: block.first_source_span(),
        });
    }
    sources.sort_by_key(|source| source.source_index());
    Ok(sources)
}

/// The checked FMI description of one component, with no kernel attached.
///
/// Every field is private and there is no public constructor, so the only way
/// to obtain this is from a [`FmiComponent`] that already proved the metadata
/// against its own kernel.
///
/// Deliberately not `Serialize`: a render context holds the proved type-state
/// of [`FmiCodegenView::try_event_free`], never this inventory itself.
#[derive(Debug)]
pub struct FmiMetadata {
    variables: Vec<FmiVariable>,
    state_variable_indices: Vec<usize>,
    derivative_value_reference_base_fmi3: u32,
}

impl FmiMetadata {
    /// The one ordered value-reference inventory both FMI versions project.
    ///
    /// One entry per addressable checked Solve storage run in storage order,
    /// followed by the derived maximum-step-duration local when the kernel is
    /// delay-bearing. Zero-scalar declarations are checked against their runs
    /// but own no value reference. Entry `index` holds value reference
    /// `index + 1`.
    #[must_use]
    pub fn variables(&self) -> &[FmiVariable] {
        &self.variables
    }

    /// The storage-backed entries of [`Self::variables`], borrowed in the same
    /// order.
    ///
    /// This is a filter over the one inventory, not a second collection: a
    /// reader that needs a storage run per entry uses it without losing the
    /// inventory's value-reference order.
    pub fn storage_variables(&self) -> impl Iterator<Item = &FmiVariable> {
        self.variables
            .iter()
            .filter(|variable| variable.storage().is_some())
    }

    /// The delay-bearing kernel's maximum-step-duration local, if it has one.
    ///
    /// The entry is an ordinary member of [`Self::variables`]; this borrows it
    /// so a reader need not rediscover which value reference it took.
    #[must_use]
    pub fn max_step_duration(&self) -> Option<&FmiVariable> {
        self.variables
            .iter()
            .find(|variable| matches!(variable.backing(), FmiValueBacking::MaxStepDuration))
    }

    #[must_use]
    pub fn state_variable_indices(&self) -> &[usize] {
        &self.state_variable_indices
    }

    #[must_use]
    pub const fn derivative_value_reference_base_fmi3(&self) -> u32 {
        self.derivative_value_reference_base_fmi3
    }
}

/// One checked FMI component: the metadata above bound to the exact executable
/// kernel it describes.
///
/// Deliberately not `Clone`. A second owner of the aggregate would be a second
/// place the FMI inventory could drift from its kernel; consumers borrow the
/// views below, and codegen consumes the whole thing into [`FmiCodegenView`].
#[derive(Debug)]
pub struct FmiComponent {
    metadata: FmiMetadata,
    event_indicators: FmiEventIndicatorInventory,
    linked_runtime_facts: Arc<FmiLinkedRuntimeFacts>,
    event_class: Option<crate::SolveEventClass>,
    model: Arc<SolveModel>,
}

impl FmiComponent {
    /// Bind one checked kernel to the FMI inventory that describes it.
    ///
    /// All declaration facts and evaluated values come from the sealed catalog
    /// retained by `model`. The maximum-step-duration local of SPEC_0044 §8 is
    /// derived here when the checked kernel is delay-bearing.
    pub fn construct(model: SolveModel) -> Result<Self, FmiComponentError> {
        let metadata = checked_metadata(&model)?;
        let event_indicators = FmiEventIndicatorInventory::derive(&model)?;
        let event_class = crate::solve_event_class(model.problem());
        let linked_runtime_facts = Arc::new(FmiLinkedRuntimeFacts::construct(
            &model,
            &metadata,
            &event_indicators,
            event_class.is_some(),
        )?);
        Ok(Self {
            metadata,
            event_indicators,
            linked_runtime_facts,
            event_class,
            model: Arc::new(model),
        })
    }

    #[must_use]
    pub const fn metadata(&self) -> &FmiMetadata {
        &self.metadata
    }

    #[must_use]
    pub const fn event_indicators(&self) -> &FmiEventIndicatorInventory {
        &self.event_indicators
    }

    #[must_use]
    pub fn variables(&self) -> &[FmiVariable] {
        self.metadata.variables()
    }

    pub fn storage_variables(&self) -> impl Iterator<Item = &FmiVariable> {
        self.metadata.storage_variables()
    }

    #[must_use]
    pub fn max_step_duration(&self) -> Option<&FmiVariable> {
        self.metadata.max_step_duration()
    }

    #[must_use]
    pub fn state_variable_indices(&self) -> &[usize] {
        self.metadata.state_variable_indices()
    }

    #[must_use]
    pub const fn derivative_value_reference_base_fmi3(&self) -> u32 {
        self.metadata.derivative_value_reference_base_fmi3()
    }

    /// Whether this component declares `needsCompletedIntegratorStep="true"`.
    ///
    /// SPEC_0044 §8 requires it of an event-bearing component, whose private
    /// indicator-domain cache advances only at a completed step, and of a
    /// delay-bearing component, whose accepted history commits only there. Both
    /// are semantic event classes, so the one
    /// [`crate::solve_event_class`] fact decides it.
    ///
    /// Component construction derives the event class once from the kernel it
    /// consumes and retains that closed fact. This accessor reads the retained
    /// construction fact; it does not traverse the kernel a second time. The
    /// callback's runtime behaviour belongs to the linked component and is not
    /// implemented here.
    #[must_use]
    pub fn needs_completed_integrator_step(&self) -> bool {
        self.event_class.is_some()
    }

    #[must_use]
    pub fn problem(&self) -> &SolveProblem {
        self.model.problem()
    }

    #[must_use]
    pub fn artifacts(&self) -> &SolveArtifacts {
        self.model.artifacts()
    }

    /// Borrow the executable root through the component that proved its FMI
    /// inventory.
    ///
    /// Runtime linking accepts this correlated view rather than an unrelated
    /// `SolveModel`, so metadata and execution cannot be paired after
    /// construction. The view is borrowed and has no public constructor or
    /// owned-root escape.
    #[must_use]
    pub fn runtime_model(&self) -> &SolveModel {
        self.model.as_ref()
    }

    /// Consume the checked component into the one sealed linked-runtime
    /// capability. The capability keeps the executable root and every issued
    /// FMI descriptor correlated; neither owned part can be extracted.
    #[must_use]
    pub fn into_runtime_view(self) -> FmiRuntimeView {
        FmiRuntimeView {
            model: self.model,
            linked_runtime_facts: self.linked_runtime_facts,
        }
    }

    /// Consume this component into the correlated codegen view.
    ///
    /// This is the only producer of [`FmiCodegenView`], so a renderer cannot
    /// assemble one from metadata and an unrelated kernel.
    #[must_use]
    pub fn into_codegen_view(self) -> FmiCodegenView {
        FmiCodegenView {
            metadata: self.metadata,
            event_indicators: self.event_indicators,
            event_class: self.event_class,
            model: self.model,
        }
    }
}

/// Sealed executable capability minted only by
/// [`FmiComponent::into_runtime_view`].
///
/// This is the runtime counterpart of [`FmiCodegenView`]: it keeps the
/// correlation proof and consumes the component. Deliberately not `Clone` or
/// `Copy`; the linked runtime retains this whole value for its lifetime.
///
/// An importer cannot recover the owned Solve root or reconstruct the
/// capability from independently held parts:
///
/// ```compile_fail
/// fn escape(view: rumoca_ir_solve::fmi::FmiRuntimeView) {
///     let _owned = view.shared_model();
/// }
/// ```
///
/// ```compile_fail
/// use std::sync::Arc;
/// use rumoca_ir_solve::fmi::{FmiLinkedRuntimeFacts, FmiRuntimeView};
/// fn escape_facts(view: &FmiRuntimeView) -> Arc<FmiLinkedRuntimeFacts> {
///     view.linked_runtime_facts()
/// }
/// ```
///
/// ```compile_fail
/// fn clone_generically<T: Clone>(value: &T) -> T { value.clone() }
/// fn duplicate(view: &rumoca_ir_solve::fmi::FmiRuntimeView) {
///     let _duplicate = clone_generically(view);
/// }
/// ```
#[derive(Debug)]
pub struct FmiRuntimeView {
    model: Arc<SolveModel>,
    linked_runtime_facts: Arc<FmiLinkedRuntimeFacts>,
}

impl FmiRuntimeView {
    #[must_use]
    pub const fn configuration_capability(&self) -> FmiConfigurationCapability {
        // The sole current constructor has no structural FMI variable. This
        // is an explicit absent capability, not a name-based inference from
        // ordinary Modelica parameters.
        FmiConfigurationCapability::Absent
    }

    /// The checked executable root borrowed from the correlated component.
    /// No owned bare `SolveModel` can be recovered through this view.
    #[must_use]
    pub fn model(&self) -> &SolveModel {
        self.model.as_ref()
    }

    /// Borrow the immutable FMI-to-runtime facts issued beside [`Self::model`].
    /// The owning handles remain sealed inside this capability.
    #[must_use]
    pub fn linked_runtime_facts(&self) -> &FmiLinkedRuntimeFacts {
        self.linked_runtime_facts.as_ref()
    }
}

/// The correlated view codegen consumes: the same metadata and the same kernel
/// handle, with no constructor of its own.
///
/// The retained `Arc` is what lets the lazy render objects be `'static` without
/// copying the program graph, and it is also where rendering reads
/// [`SolveArtifacts`] from, so no second artifacts argument can disagree with
/// the kernel the metadata was checked against.
///
/// This is the unrestricted correlated view: it can describe a component whose
/// kernel or inventory a storage-backed template has no rendering for, which is
/// why the current renderers take [`FmiEventFreeCodegenView`] instead. An
/// event-capable renderer admitted by ME-EVENT-002 consumes this one directly.
///
/// Deliberately not `Serialize`, for the same reason as [`FmiMetadata`].
#[derive(Debug)]
pub struct FmiCodegenView {
    metadata: FmiMetadata,
    event_indicators: FmiEventIndicatorInventory,
    event_class: Option<crate::SolveEventClass>,
    model: Arc<SolveModel>,
}

impl FmiCodegenView {
    #[must_use]
    pub const fn metadata(&self) -> &FmiMetadata {
        &self.metadata
    }

    #[must_use]
    pub const fn event_indicators(&self) -> &FmiEventIndicatorInventory {
        &self.event_indicators
    }

    #[must_use]
    pub fn problem(&self) -> &SolveProblem {
        self.model.problem()
    }

    #[must_use]
    pub fn artifacts(&self) -> &SolveArtifacts {
        self.model.artifacts()
    }
}

fn checked_metadata(model: &SolveModel) -> Result<FmiMetadata, FmiComponentError> {
    let solve = model.problem();
    let delay_bearing = !solve.events.delays.delay_time_rhs.is_empty();
    let reinitialized_state_slots = checked_reinitialized_state_slots(solve)?;

    let mut inventory = checked_storage_inventory(
        model.variable_catalog(),
        delay_bearing,
        &reinitialized_state_slots,
    )?;
    if inventory.state_scalar_count != solve.solve_layout.state_scalar_count {
        return Err(FmiComponentError::StateCount {
            actual: inventory.state_scalar_count,
            expected: solve.solve_layout.state_scalar_count,
        });
    }
    if delay_bearing {
        let value_reference = value_reference_fmi3(inventory.variables.len())?;
        inventory
            .variables
            .push(max_step_duration::derived_local(value_reference));
    }

    let derivative_value_reference_base_fmi3 = value_reference_fmi3(inventory.variables.len())?;
    Ok(FmiMetadata {
        variables: inventory.variables,
        state_variable_indices: inventory.state_variable_indices,
        derivative_value_reference_base_fmi3,
    })
}

/// The storage-backed prefix of the inventory, with the state facts proved
/// while it is built.
struct StorageInventory {
    variables: Vec<FmiVariable>,
    state_variable_indices: Vec<usize>,
    state_scalar_count: usize,
}

fn checked_storage_inventory(
    catalog: &crate::SolveVariableCatalog,
    delay_bearing: bool,
    reinitialized_state_slots: &BTreeSet<usize>,
) -> Result<StorageInventory, FmiComponentError> {
    let mut inventory = StorageInventory {
        variables: Vec::with_capacity(catalog.len()),
        state_variable_indices: Vec::new(),
        state_scalar_count: 0,
    };
    for entry in catalog.entries() {
        max_step_duration::reject_reserved_name(entry, delay_bearing)?;
        let run = entry.storage();
        let variable = checked_variable(
            entry,
            value_reference_fmi3(inventory.variables.len())?,
            reinitialized_state_slots,
        )?;
        if variable.role() == Some(SolveVariableStorageRole::State) {
            inventory
                .state_variable_indices
                .push(inventory.variables.len());
            inventory.state_scalar_count = inventory
                .state_scalar_count
                .checked_add(run.scalar_count)
                .ok_or(FmiComponentError::ValueReferenceOverflow)?;
        }
        inventory.variables.push(variable);
    }
    Ok(inventory)
}

/// Value reference zero is `time`, so the inventory's `index`-th declaration
/// takes the next number.
fn value_reference_fmi3(index: usize) -> Result<u32, FmiComponentError> {
    u32::try_from(index)
        .ok()
        .and_then(|value| value.checked_add(1))
        .ok_or(FmiComponentError::ValueReferenceOverflow)
}

fn checked_variable(
    entry: &SolveVariableCatalogEntry,
    value_reference_fmi3: u32,
    reinitialized_state_slots: &BTreeSet<usize>,
) -> Result<FmiVariable, FmiComponentError> {
    let run = entry.storage();
    let scalar_count = run.scalar_count;
    let column = run.base.column();
    let base = run.base.index();
    let causality = fmi_causality(entry.causality());
    let variability = fmi_variability(entry.variability());
    // One read of the state-initialization fact feeds both the `initial`
    // attribute and the write policy, so the two cannot describe the same
    // state differently.
    let state_initialization = entry.state_initialization();
    let initial = fmi_initial(entry, causality, state_initialization);
    let start = match initial {
        Some(FmiInitial::Calculated) => None,
        Some(FmiInitial::Exact | FmiInitial::Approx) | None => entry.start().map(<[f64]>::to_vec),
    };
    let state_reinit_false = if matches!(entry.role(), SolveVariableStorageRole::State)
        && matches!(column, SolveStorageColumn::Y)
    {
        let end = base.checked_add(scalar_count).ok_or_else(|| {
            FmiComponentError::StateReinitEvidence {
                message: format!("state `{}` storage range overflows Y", entry.name()),
            }
        })?;
        (base..end).all(|index| !reinitialized_state_slots.contains(&index))
    } else {
        false
    };
    let state = fmi_state_facts(state_initialization, state_reinit_false);
    let write_policy = fmi_write_policy(causality, variability, state).ok_or_else(|| {
        FmiComponentError::UnclassifiedWritePolicy {
            name: entry.name().to_string(),
            span: entry.provenance(),
        }
    })?;
    Ok(FmiVariable {
        source_id: Some(entry.id()),
        name: entry.name().to_string(),
        value_kind: entry.value_kind(),
        dimensions: entry.dimensions().to_vec(),
        backing: FmiValueBacking::SolveStorage {
            role: entry.role(),
            storage: FmiStorageRun {
                column,
                base,
                scalar_count,
            },
            scalar_names: entry.scalar_names().to_vec(),
        },
        start,
        minimum: entry.minimum().map(<[f64]>::to_vec),
        maximum: entry.maximum().map(<[f64]>::to_vec),
        nominal: entry.nominal().map(<[f64]>::to_vec),
        unit: entry.unit().map(str::to_string),
        description: entry.description().map(str::to_string),
        causality,
        variability,
        initial,
        write_policy,
        tunable: entry.is_tunable(),
        declaration: Some(entry.provenance()),
        value_reference_fmi3,
    })
}

const fn fmi_causality(causality: SolveVariableCausality) -> FmiCausality {
    match causality {
        SolveVariableCausality::Input => FmiCausality::Input,
        SolveVariableCausality::Output => FmiCausality::Output,
        SolveVariableCausality::Parameter => FmiCausality::Parameter,
        SolveVariableCausality::CalculatedParameter => FmiCausality::CalculatedParameter,
        SolveVariableCausality::Independent => FmiCausality::Independent,
        SolveVariableCausality::Local => FmiCausality::Local,
    }
}

const fn fmi_variability(variability: SolveVariableVariability) -> FmiVariability {
    match variability {
        SolveVariableVariability::Constant => FmiVariability::Constant,
        SolveVariableVariability::Fixed => FmiVariability::Fixed,
        SolveVariableVariability::Tunable => FmiVariability::Tunable,
        SolveVariableVariability::Discrete => FmiVariability::Discrete,
        SolveVariableVariability::Continuous => FmiVariability::Continuous,
    }
}

fn fmi_initial(
    entry: &SolveVariableCatalogEntry,
    causality: FmiCausality,
    state_initialization: SolveStateInitialization,
) -> Option<FmiInitial> {
    // FMI owns the initialization convention for inputs. A source start value
    // remains required and is projected, but `initial` is forbidden for both
    // FMI 2 and FMI 3 input causality.
    if matches!(causality, FmiCausality::Input | FmiCausality::Independent) {
        return None;
    }
    match state_initialization {
        SolveStateInitialization::Exact => return Some(FmiInitial::Exact),
        SolveStateInitialization::Approximate => return Some(FmiInitial::Approx),
        SolveStateInitialization::NotState => {}
    }
    let role = entry.role();
    if matches!(
        causality,
        FmiCausality::CalculatedParameter | FmiCausality::Output
    ) {
        return Some(FmiInitial::Calculated);
    }
    if matches!(
        role,
        SolveVariableStorageRole::Parameter
            | SolveVariableStorageRole::Constant
            | SolveVariableStorageRole::ExternalInput
    ) || matches!(causality, FmiCausality::Parameter)
    {
        return Some(FmiInitial::Exact);
    }
    if matches!(causality, FmiCausality::Local) {
        return Some(FmiInitial::Calculated);
    }
    None
}

/// Fold the single state-initialization read and the reinit evidence into the
/// state facts the write tables consume, present only for an actual continuous
/// state.
const fn fmi_state_facts(
    state_initialization: SolveStateInitialization,
    state_reinit_false: bool,
) -> Option<(FmiStateInitial, FmiStateReinit)> {
    let initial = match state_initialization {
        SolveStateInitialization::Exact => FmiStateInitial::Exact,
        SolveStateInitialization::Approximate => FmiStateInitial::Approx,
        SolveStateInitialization::NotState => return None,
    };
    let reinit = if state_reinit_false {
        FmiStateReinit::False
    } else {
        FmiStateReinit::Reinitializable
    };
    Some((initial, reinit))
}

/// Classify one projected variable's write facts, or `None` for a
/// causality/variability the Modelica-to-ME catalog never issues.
///
/// The state facts already carry `initial` and reinit evidence, and the
/// catalog enforces `tunable == (variability == Tunable)`, so neither a `role`
/// nor a `tunable` argument is a second carrier of a fact this function reads
/// off `variability`. A `None` result makes the caller fail closed rather than
/// invent a policy for a combination that cannot arise.
const fn fmi_write_policy(
    causality: FmiCausality,
    variability: FmiVariability,
    state: Option<(FmiStateInitial, FmiStateReinit)>,
) -> Option<FmiWritePolicy> {
    if let Some((initial, reinit)) = state {
        return Some(FmiWritePolicy::ContinuousState { initial, reinit });
    }
    match causality {
        FmiCausality::Input => match variability {
            FmiVariability::Continuous => Some(FmiWritePolicy::ContinuousInput),
            FmiVariability::Discrete => Some(FmiWritePolicy::DiscreteInput),
            FmiVariability::Constant | FmiVariability::Fixed | FmiVariability::Tunable => None,
        },
        FmiCausality::Parameter => match variability {
            FmiVariability::Tunable => Some(FmiWritePolicy::TunableParameter),
            FmiVariability::Fixed => Some(FmiWritePolicy::FixedParameter),
            FmiVariability::Constant | FmiVariability::Discrete | FmiVariability::Continuous => {
                None
            }
        },
        FmiCausality::Output
        | FmiCausality::CalculatedParameter
        | FmiCausality::Local
        | FmiCausality::Independent => Some(FmiWritePolicy::ReadOnly),
    }
}

/// Derive the exact Solve Y lanes owned by source `reinit` actions once, while
/// constructing correlated FMI metadata. Absence from this complete typed
/// owner inventory is the [`FmiStateReinit::False`] evidence a
/// [`FmiWritePolicy::ContinuousState`] carries.
fn checked_reinitialized_state_slots(
    solve: &SolveProblem,
) -> Result<BTreeSet<usize>, FmiComponentError> {
    let mut slots = BTreeSet::new();
    for (target, role) in solve
        .discrete
        .update_targets
        .iter()
        .copied()
        .zip(solve.discrete.row_roles.iter().copied())
    {
        if role == crate::DiscreteRowRole::EventAction
            && let ScalarSlot::Y { index } = target
        {
            slots.insert(index);
        }
    }
    for program in &solve.discrete.guarded_assignments {
        if program.role() != crate::DiscreteRowRole::EventAction {
            continue;
        }
        for range in program.target_ranges() {
            let ScalarSlot::Y { index: base } = range.base() else {
                continue;
            };
            let end = base.checked_add(range.count()).ok_or_else(|| {
                FmiComponentError::StateReinitEvidence {
                    message: "guarded reinit target range overflows Y storage".to_string(),
                }
            })?;
            slots.extend(base..end);
        }
    }
    for (update_index, update) in solve.discrete.structured_updates.iter().enumerate() {
        if update.role != crate::DiscreteRowRole::EventAction {
            continue;
        }
        let assignments = solve
            .discrete
            .structured_assignments(update_index)
            .map_err(|error| FmiComponentError::StateReinitEvidence {
                message: error.to_string(),
            })?;
        slots.extend(
            assignments
                .into_iter()
                .filter_map(|(target, _)| match target {
                    ScalarSlot::Y { index } => Some(index),
                    ScalarSlot::P { .. } | ScalarSlot::Time | ScalarSlot::Constant(_) => None,
                }),
        );
    }
    Ok(slots)
}
