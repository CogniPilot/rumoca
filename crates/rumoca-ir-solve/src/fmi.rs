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
mod max_step_duration;
mod metadata;
#[cfg(test)]
mod tests;

pub use event_free::{FmiEventFreeCodegenView, FmiEventFreeError};
pub use max_step_duration::{
    MAX_STEP_DURATION_DESCRIPTION, MAX_STEP_DURATION_NAME, MAX_STEP_DURATION_UNCONSTRAINED,
    MAX_STEP_DURATION_UNIT,
};
pub use metadata::{
    FmiCausality, FmiInitial, FmiStorageColumn, FmiStorageRun, FmiValueBacking, FmiVariability,
    FmiVariable, FmiVariableInput,
};

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
    ScalarSlot, SolveArtifacts, SolveModel, SolveProblem, SolveVariableDeclaration,
    SolveVariableStorageRole, SolveVariableStorageRun,
};
use rumoca_core::Span;
use std::collections::BTreeSet;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum FmiComponentError {
    #[error("Solve kernel is invalid: {0}")]
    InvalidSolve(String),
    #[error("FMI declaration count {variables} does not match Solve storage count {storage}")]
    VariableCount { variables: usize, storage: usize },
    #[error("FMI variable `{name}` has {actual} scalars but its shape requires {expected}")]
    ScalarCount {
        name: String,
        actual: usize,
        expected: usize,
        span: Span,
    },
    #[error("FMI variable `{name}` has duplicate source identity")]
    DuplicateName { name: String, span: Span },
    #[error("FMI variable `{name}` does not match its Solve declaration or storage role")]
    StorageTypeMismatch { name: String, span: Span },
    #[error("FMI variable `{name}` is stored in a non-addressable Solve slot")]
    NonAddressableStorage { name: String, span: Span },
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
}

impl FmiComponentError {
    #[must_use]
    pub const fn span(&self) -> Option<Span> {
        match self {
            Self::ScalarCount { span, .. }
            | Self::DuplicateName { span, .. }
            | Self::StorageTypeMismatch { span, .. }
            | Self::NonAddressableStorage { span, .. } => Some(*span),
            Self::ReservedMaxStepDurationName { declaration, .. } => Some(*declaration),
            Self::EventIndicatorInventory { span, .. } => *span,
            Self::InvalidSolve(_)
            | Self::VariableCount { .. }
            | Self::ValueReferenceOverflow
            | Self::StateCount { .. } => None,
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
            .problem
            .continuous
            .refresh_owners
            .root()
            .static_causal_rows()
            .iter()
            .map(|row| row.target_index())
            .collect::<BTreeSet<_>>();
        let scheduled = model
            .problem
            .events
            .scheduled_root_conditions
            .iter()
            .map(|root| root.root_index)
            .collect::<BTreeSet<_>>();
        let mut sources = dependency_backed_indicator_sources(
            &model.problem.events.root_conditions,
            &static_y,
            TimeReadOwner::Monitored,
            |index| {
                (!scheduled.contains(&index))
                    .then_some(FmiEventIndicatorSource::RootCondition { index })
            },
        )?;
        sources.extend(dependency_backed_indicator_sources(
            &model.problem.events.dynamic_time_event_rhs,
            &static_y,
            TimeReadOwner::Announced,
            |index| Some(FmiEventIndicatorSource::DynamicTimeEvent { index }),
        )?);
        sources.extend(
            (0..model.problem.events.delays.delay_time_rhs.output_count())
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
    model: Arc<SolveModel>,
}

impl FmiComponent {
    /// Bind one checked kernel to the FMI inventory that describes it.
    ///
    /// `inputs` is one entry per Solve storage run, and holds only facts the
    /// Modelica declaration owns. The maximum-step-duration local of
    /// SPEC_0044 §8 is not among them: this constructor derives it, exactly
    /// when the checked kernel is delay-bearing.
    pub fn construct(
        model: SolveModel,
        inputs: Vec<FmiVariableInput>,
    ) -> Result<Self, FmiComponentError> {
        model
            .validate()
            .map_err(|error| FmiComponentError::InvalidSolve(error.to_string()))?;
        let metadata = checked_metadata(&model.problem, inputs)?;
        let event_indicators = FmiEventIndicatorInventory::derive(&model)?;
        Ok(Self {
            metadata,
            event_indicators,
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
    /// This is derived on demand from the kernel this component owns rather
    /// than stored: a second copy of the fact is a second thing that can drift
    /// from the kernel. It is a construction-owned capability fact only; the
    /// callback's runtime behaviour belongs to the linked component and is not
    /// implemented here.
    #[must_use]
    pub fn needs_completed_integrator_step(&self) -> bool {
        crate::solve_event_class(self.problem()).is_some()
    }

    #[must_use]
    pub fn problem(&self) -> &SolveProblem {
        &self.model.problem
    }

    #[must_use]
    pub fn artifacts(&self) -> &SolveArtifacts {
        &self.model.artifacts
    }

    /// Borrow the executable root through the component that proved its FMI
    /// inventory.
    ///
    /// Runtime linking accepts this correlated view rather than an unrelated
    /// `SolveModel`, so metadata and execution cannot be paired after
    /// construction. The view is borrowed and has no public constructor or
    /// owned-root escape.
    #[must_use]
    pub fn runtime_view(&self) -> FmiRuntimeView<'_> {
        FmiRuntimeView {
            model: &self.model,
            metadata: &self.metadata,
            event_indicators: &self.event_indicators,
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
            model: self.model,
        }
    }
}

/// Borrowed executable view minted only by [`FmiComponent::runtime_view`].
///
/// This is the runtime counterpart of [`FmiCodegenView`]: it keeps the
/// correlation proof but does not consume the component. Deliberately not
/// `Clone` or `Copy`; a host lends it directly into one component instance.
#[derive(Debug)]
pub struct FmiRuntimeView<'component> {
    model: &'component SolveModel,
    metadata: &'component FmiMetadata,
    event_indicators: &'component FmiEventIndicatorInventory,
}

impl<'component> FmiRuntimeView<'component> {
    #[must_use]
    pub const fn configuration_capability(&self) -> FmiConfigurationCapability {
        // The sole current constructor has no structural FMI variable. This
        // is an explicit absent capability, not a name-based inference from
        // ordinary Modelica parameters.
        FmiConfigurationCapability::Absent
    }

    /// The checked executable root borrowed from the correlated component.
    /// No owned `SolveModel` can be recovered through this view.
    #[must_use]
    pub fn model(self) -> &'component SolveModel {
        self.model
    }

    #[must_use]
    pub fn event_indicators(&self) -> &'component FmiEventIndicatorInventory {
        self.event_indicators
    }

    /// The checked maximum-step-duration entry, when the component is
    /// delay-bearing.
    ///
    /// Runtime linking borrows this typed inventory entry once; it never
    /// rediscovers the annotation by name or inspects Solve operations.
    #[must_use]
    pub fn max_step_duration(&self) -> Option<&'component FmiVariable> {
        self.metadata.max_step_duration()
    }

    #[must_use]
    pub fn into_parts(
        self,
    ) -> (
        &'component SolveModel,
        &'component FmiMetadata,
        &'component FmiEventIndicatorInventory,
    ) {
        (self.model, self.metadata, self.event_indicators)
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
        &self.model.problem
    }

    #[must_use]
    pub fn artifacts(&self) -> &SolveArtifacts {
        &self.model.artifacts
    }
}

fn checked_metadata(
    solve: &SolveProblem,
    inputs: Vec<FmiVariableInput>,
) -> Result<FmiMetadata, FmiComponentError> {
    let runs = &solve.solve_layout.variable_storage_runs;
    let declarations = &solve.solve_layout.variable_declarations;
    if inputs.len() != runs.len() || inputs.len() != declarations.len() {
        return Err(FmiComponentError::VariableCount {
            variables: inputs.len(),
            storage: runs.len(),
        });
    }
    let delay_bearing = !solve.events.delays.delay_time_rhs.is_empty();

    let mut inventory = checked_storage_inventory(inputs, runs, declarations, delay_bearing)?;
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
    inputs: Vec<FmiVariableInput>,
    runs: &[SolveVariableStorageRun],
    declarations: &[SolveVariableDeclaration],
    delay_bearing: bool,
) -> Result<StorageInventory, FmiComponentError> {
    let mut names = BTreeSet::new();
    let mut inventory = StorageInventory {
        variables: Vec::with_capacity(inputs.len()),
        state_variable_indices: Vec::new(),
        state_scalar_count: 0,
    };
    for (index, (input, run)) in inputs.into_iter().zip(runs).enumerate() {
        max_step_duration::reject_reserved_name(&input, delay_bearing)?;
        if !names.insert(input.name.clone()) {
            return Err(FmiComponentError::DuplicateName {
                name: input.name,
                span: input.declaration,
            });
        }
        let variable = checked_variable(
            input,
            *run,
            declarations[index],
            value_reference_fmi3(inventory.variables.len())?,
        )?;
        if run.scalar_count == 0 {
            continue;
        }
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
    input: FmiVariableInput,
    run: SolveVariableStorageRun,
    declaration: SolveVariableDeclaration,
    value_reference_fmi3: u32,
) -> Result<FmiVariable, FmiComponentError> {
    let scalar_count = checked_scalar_count(&input)?;
    if scalar_count != run.scalar_count
        || input.role != run.role
        || input.value_kind != run.value_kind
        || input.role != declaration.role()
        || input.value_kind != declaration.value_kind()
    {
        return Err(FmiComponentError::StorageTypeMismatch {
            name: input.name,
            span: input.declaration,
        });
    }
    let (column, base) = match run.base {
        ScalarSlot::Y { index, .. } => (FmiStorageColumn::Y, index),
        ScalarSlot::P { index, .. } => (FmiStorageColumn::P, index),
        ScalarSlot::Time | ScalarSlot::Constant(_) => {
            return Err(FmiComponentError::NonAddressableStorage {
                name: input.name,
                span: input.declaration,
            });
        }
    };
    Ok(FmiVariable {
        name: input.name,
        value_kind: input.value_kind,
        dimensions: input.dimensions,
        backing: FmiValueBacking::SolveStorage {
            role: input.role,
            storage: FmiStorageRun {
                column,
                base,
                scalar_count,
            },
            scalar_names: input.scalar_names,
        },
        start: Some(input.start),
        minimum: input.minimum,
        maximum: input.maximum,
        nominal: input.nominal,
        unit: input.unit,
        description: input.description,
        causality: input.causality,
        variability: input.variability,
        initial: None,
        tunable: input.tunable,
        declaration: Some(input.declaration),
        value_reference_fmi3,
    })
}

fn checked_scalar_count(input: &FmiVariableInput) -> Result<usize, FmiComponentError> {
    let expected = input
        .dimensions
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
        .ok_or(FmiComponentError::ValueReferenceOverflow)?;
    let counts = [
        input.scalar_names.len(),
        input.start.len(),
        input.minimum.as_ref().map_or(expected, Vec::len),
        input.maximum.as_ref().map_or(expected, Vec::len),
        input.nominal.as_ref().map_or(expected, Vec::len),
    ];
    if let Some(actual) = counts.into_iter().find(|actual| *actual != expected) {
        return Err(FmiComponentError::ScalarCount {
            name: input.name.clone(),
            actual,
            expected,
            span: input.declaration,
        });
    }
    Ok(expected)
}
