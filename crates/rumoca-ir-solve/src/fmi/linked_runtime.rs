//! Opaque facts issued once by checked FMI-component construction.
//!
//! The linked runtime consumes these tables without traversing Solve or
//! repeating value-reference arithmetic. Their fields stay private so no
//! downstream crate can synthesize a partially checked table.

use super::{
    Fmi3WriteModes, FmiCausality, FmiComponentError, FmiEventIndicatorInventory,
    FmiEventIndicatorSource, FmiMetadata, FmiValueBacking,
};
use crate::{RootZeroDomain, ScalarSlot, SolveModel, SolveStorageColumn};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FmiIndicatorReading {
    RootValue { index: usize },
    DeadlineDistance { index: usize },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FmiIndicatorZeroSide {
    Positive,
    NonPositive,
    Frozen,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct FmiIndicatorWidth<const ROLE: u8> {
    len: usize,
}

impl<const ROLE: u8> FmiIndicatorWidth<ROLE> {
    const fn new(len: usize) -> Self {
        Self { len }
    }

    #[must_use]
    pub const fn len(self) -> usize {
        self.len
    }

    #[must_use]
    pub const fn is_empty(self) -> bool {
        self.len == 0
    }
}

pub type FmiPublishedIndicatorWidth = FmiIndicatorWidth<0>;
pub type FmiRootValueWidth = FmiIndicatorWidth<1>;
pub type FmiDeadlineWidth = FmiIndicatorWidth<2>;
pub type FmiIndicatorDomainWidth = FmiIndicatorWidth<3>;

/// Exact continuous-state width issued with the checked FMI linked-runtime
/// facts. Downstream FMI execution may inspect the width but cannot manufacture
/// one from an unrelated count.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct FmiContinuousStateWidth(usize);

impl FmiContinuousStateWidth {
    const fn new(len: usize) -> Self {
        Self(len)
    }

    #[must_use]
    pub const fn len(self) -> usize {
        self.0
    }

    #[must_use]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct FmiEventIndicatorEntry {
    reading: FmiIndicatorReading,
    zero_side: FmiIndicatorZeroSide,
    crossing_root_index: Option<usize>,
}

impl FmiEventIndicatorEntry {
    #[must_use]
    pub const fn reading(&self) -> FmiIndicatorReading {
        self.reading
    }

    #[must_use]
    pub const fn zero_side(&self) -> FmiIndicatorZeroSide {
        self.zero_side
    }

    #[must_use]
    pub const fn crossing_root_index(&self) -> Option<usize> {
        self.crossing_root_index
    }
}

#[derive(Debug)]
pub struct FmiEventIndicatorPlan {
    entries: Box<[FmiEventIndicatorEntry]>,
    relation_memory_targets: Box<[Option<ScalarSlot>]>,
    published_width: FmiPublishedIndicatorWidth,
    root_value_width: FmiRootValueWidth,
    deadline_width: FmiDeadlineWidth,
    domain_width: FmiIndicatorDomainWidth,
}

impl FmiEventIndicatorPlan {
    #[must_use]
    pub const fn len(&self) -> usize {
        self.published_width.len()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    #[must_use]
    pub const fn entries(&self) -> &[FmiEventIndicatorEntry] {
        &self.entries
    }

    #[must_use]
    pub const fn relation_memory_targets(&self) -> &[Option<ScalarSlot>] {
        &self.relation_memory_targets
    }

    #[must_use]
    pub const fn published_width(&self) -> FmiPublishedIndicatorWidth {
        self.published_width
    }

    #[must_use]
    /// The exact root-evaluation buffer width issued by this plan.
    ///
    /// Width roles are distinct capabilities rather than interchangeable
    /// integers:
    ///
    /// ```compile_fail
    /// use rumoca_ir_solve::fmi::{FmiEventIndicatorPlan, FmiRootValueWidth};
    ///
    /// fn swapped(plan: &FmiEventIndicatorPlan) -> FmiRootValueWidth {
    ///     plan.deadline_width()
    /// }
    /// ```
    pub const fn root_value_width(&self) -> FmiRootValueWidth {
        self.root_value_width
    }

    #[must_use]
    pub const fn deadline_width(&self) -> FmiDeadlineWidth {
        self.deadline_width
    }

    #[must_use]
    pub const fn domain_width(&self) -> FmiIndicatorDomainWidth {
        self.domain_width
    }

    #[must_use]
    pub const fn reads_root_values(&self) -> bool {
        !self.root_value_width.is_empty()
    }

    #[must_use]
    pub const fn reads_deadlines(&self) -> bool {
        !self.deadline_width.is_empty()
    }

    #[must_use]
    pub fn crossing_root_index(&self, position: usize) -> Option<usize> {
        self.entries
            .get(position)
            .and_then(FmiEventIndicatorEntry::crossing_root_index)
    }

    #[must_use]
    pub fn relation_memory_target(&self, position: usize) -> Option<ScalarSlot> {
        self.relation_memory_targets
            .get(position)
            .copied()
            .flatten()
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FmiDelayCapability {
    Absent,
    MaximumStepDuration { value_reference: u32 },
}

impl FmiDelayCapability {
    #[must_use]
    pub const fn value_reference(self) -> Option<u32> {
        match self {
            Self::Absent => None,
            Self::MaximumStepDuration { value_reference } => Some(value_reference),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct FmiDirectionalReferenceDescriptor {
    state_value_reference: u32,
    derivative_value_reference: u32,
    storage_base: usize,
    serialized_width: usize,
}

/// One complete Float64 reference descriptor issued by FMI construction.
///
/// Linked preparation consumes this opaque projection instead of reopening
/// `FmiVariable` or its storage vocabulary. The boxed name and descriptor
/// table are allocated once with the component and then shared unchanged by
/// every retained linked continuation.
#[derive(Debug)]
pub struct FmiRuntimeFloat64Descriptor {
    name: Box<str>,
    value_reference: u32,
    backing: FmiRuntimeFloat64Backing,
    causality: FmiCausality,
    write_modes: Fmi3WriteModes,
}

impl FmiRuntimeFloat64Descriptor {
    #[must_use]
    pub fn name(&self) -> &str {
        &self.name
    }

    #[must_use]
    pub const fn value_reference(&self) -> u32 {
        self.value_reference
    }

    #[must_use]
    pub const fn backing(&self) -> FmiRuntimeFloat64Backing {
        self.backing
    }

    #[must_use]
    pub const fn causality(&self) -> FmiCausality {
        self.causality
    }

    /// The FMI 3 admissible write modes decided for this reference. The dynamic
    /// setter reads this mask, so its admission and the generated C consult one
    /// table.
    #[must_use]
    pub const fn write_modes(&self) -> Fmi3WriteModes {
        self.write_modes
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum FmiRuntimeFloat64Backing {
    SolverVariable { base: usize, width: usize },
    Parameter { base: usize, width: usize },
    MaximumStepDuration,
}

impl FmiDirectionalReferenceDescriptor {
    #[must_use]
    pub const fn state_value_reference(self) -> u32 {
        self.state_value_reference
    }

    #[must_use]
    pub const fn derivative_value_reference(self) -> u32 {
        self.derivative_value_reference
    }

    #[must_use]
    pub const fn storage_base(self) -> usize {
        self.storage_base
    }

    #[must_use]
    pub const fn serialized_width(self) -> usize {
        self.serialized_width
    }
}

#[derive(Debug)]
pub struct FmiLinkedRuntimeFacts {
    delay: FmiDelayCapability,
    indicator_plan: FmiEventIndicatorPlan,
    float64_descriptors: Box<[FmiRuntimeFloat64Descriptor]>,
    directional_references: Box<[FmiDirectionalReferenceDescriptor]>,
    continuous_state_nominals: Box<[f64]>,
    continuous_state_width: FmiContinuousStateWidth,
    needs_completed_integrator_step: bool,
}

impl FmiLinkedRuntimeFacts {
    pub(super) fn construct(
        model: &SolveModel,
        metadata: &FmiMetadata,
        inventory: &FmiEventIndicatorInventory,
        needs_completed_integrator_step: bool,
    ) -> Result<Self, FmiComponentError> {
        Ok(Self {
            delay: delay_capability(metadata),
            indicator_plan: indicator_plan(model, inventory)?,
            float64_descriptors: float64_descriptors(metadata)?,
            directional_references: directional_references(metadata)?,
            continuous_state_nominals: model.state_nominal_values().to_vec().into_boxed_slice(),
            continuous_state_width: FmiContinuousStateWidth::new(
                model.problem().solve_layout().state_scalar_count,
            ),
            needs_completed_integrator_step,
        })
    }

    #[must_use]
    pub const fn delay(&self) -> FmiDelayCapability {
        self.delay
    }

    #[must_use]
    pub const fn indicator_plan(&self) -> &FmiEventIndicatorPlan {
        &self.indicator_plan
    }

    #[must_use]
    pub const fn directional_references(&self) -> &[FmiDirectionalReferenceDescriptor] {
        &self.directional_references
    }

    #[must_use]
    pub const fn float64_descriptors(&self) -> &[FmiRuntimeFloat64Descriptor] {
        &self.float64_descriptors
    }

    #[must_use]
    pub const fn continuous_state_width(&self) -> FmiContinuousStateWidth {
        self.continuous_state_width
    }

    /// Effective nominal values for the complete checked continuous-state
    /// domain. Component construction projects this vector once from the
    /// sealed Solve root; runtime getters borrow it without reopening Solve or
    /// applying the solver's start-sensitive scale policy.
    #[must_use]
    pub fn continuous_state_nominals(&self) -> &[f64] {
        &self.continuous_state_nominals
    }

    #[must_use]
    pub const fn needs_completed_integrator_step(&self) -> bool {
        self.needs_completed_integrator_step
    }
}

fn float64_descriptors(
    metadata: &FmiMetadata,
) -> Result<Box<[FmiRuntimeFloat64Descriptor]>, FmiComponentError> {
    let mut descriptors = Vec::new();
    descriptors
        .try_reserve_exact(metadata.variables.len())
        .map_err(|_| FmiComponentError::ValueReferenceOverflow)?;
    for variable in &metadata.variables {
        let backing = match variable.backing() {
            FmiValueBacking::SolveStorage { storage, .. } => match storage.column() {
                SolveStorageColumn::Y => FmiRuntimeFloat64Backing::SolverVariable {
                    base: storage.base(),
                    width: storage.scalar_count(),
                },
                SolveStorageColumn::P => FmiRuntimeFloat64Backing::Parameter {
                    base: storage.base(),
                    width: storage.scalar_count(),
                },
            },
            FmiValueBacking::MaxStepDuration => FmiRuntimeFloat64Backing::MaximumStepDuration,
        };
        descriptors.push(FmiRuntimeFloat64Descriptor {
            name: variable.name().into(),
            value_reference: variable.value_reference_fmi3(),
            backing,
            causality: variable.causality(),
            write_modes: Fmi3WriteModes::of(variable.write_policy()),
        });
    }
    Ok(descriptors.into_boxed_slice())
}

fn delay_capability(metadata: &FmiMetadata) -> FmiDelayCapability {
    match metadata.max_step_duration() {
        Some(variable) => FmiDelayCapability::MaximumStepDuration {
            value_reference: variable.value_reference_fmi3(),
        },
        None => FmiDelayCapability::Absent,
    }
}

fn directional_references(
    metadata: &FmiMetadata,
) -> Result<Box<[FmiDirectionalReferenceDescriptor]>, FmiComponentError> {
    let mut references = Vec::new();
    references
        .try_reserve_exact(metadata.state_variable_indices.len())
        .map_err(|_| FmiComponentError::ValueReferenceOverflow)?;
    for (ordinal, variable_index) in metadata.state_variable_indices.iter().copied().enumerate() {
        let variable = &metadata.variables[variable_index];
        let FmiValueBacking::SolveStorage { storage, .. } = variable.backing() else {
            return Err(FmiComponentError::StateReinitEvidence {
                message: format!("state `{}` has no Solve storage", variable.name()),
            });
        };
        if storage.column() != SolveStorageColumn::Y {
            return Err(FmiComponentError::StateReinitEvidence {
                message: format!("state `{}` is not stored in solver Y", variable.name()),
            });
        }
        let offset =
            u32::try_from(ordinal).map_err(|_| FmiComponentError::ValueReferenceOverflow)?;
        let derivative_value_reference = metadata
            .derivative_value_reference_base_fmi3
            .checked_add(offset)
            .ok_or(FmiComponentError::ValueReferenceOverflow)?;
        references.push(FmiDirectionalReferenceDescriptor {
            state_value_reference: variable.value_reference_fmi3(),
            derivative_value_reference,
            storage_base: storage.base(),
            serialized_width: storage.scalar_count(),
        });
    }
    Ok(references.into_boxed_slice())
}

fn indicator_plan(
    model: &SolveModel,
    inventory: &FmiEventIndicatorInventory,
) -> Result<FmiEventIndicatorPlan, FmiComponentError> {
    let events = model.problem().events();
    let model_root_count = events.root_conditions.output_count();
    let delay_count = events.delays.delay_time_rhs.output_count();
    let root_value_count = model_root_count
        .checked_add(delay_count)
        .ok_or(FmiComponentError::ValueReferenceOverflow)?;
    let deadline_count = events.dynamic_time_event_rhs.output_count();
    let mut entries = Vec::with_capacity(inventory.len());
    let mut relation_memory_targets = Vec::with_capacity(inventory.len());
    let mut reads_roots = false;
    let mut reads_deadlines = false;
    for (position, source) in inventory.sources().iter().copied().enumerate() {
        let (entry, relation_target) = match source {
            FmiEventIndicatorSource::RootCondition { index } => {
                let zero_domain = events.root_zero_domains.get(index).ok_or_else(|| {
                    indicator_error(position, "root source has no checked zero-domain fact")
                })?;
                let relation_target = events
                    .root_relation_memory_targets
                    .get(index)
                    .copied()
                    .ok_or_else(|| {
                        indicator_error(position, "root source has no checked relation-memory fact")
                    })?;
                let zero_side = match zero_domain {
                    RootZeroDomain::Positive => FmiIndicatorZeroSide::Positive,
                    RootZeroDomain::NonPositive => FmiIndicatorZeroSide::NonPositive,
                    RootZeroDomain::Previous => FmiIndicatorZeroSide::Frozen,
                };
                reads_roots = true;
                (
                    FmiEventIndicatorEntry {
                        reading: FmiIndicatorReading::RootValue { index },
                        zero_side,
                        crossing_root_index: Some(index),
                    },
                    relation_target,
                )
            }
            FmiEventIndicatorSource::DynamicTimeEvent { index } => {
                if index >= deadline_count {
                    return Err(indicator_error(
                        position,
                        "deadline source leaves its checked block",
                    ));
                }
                reads_deadlines = true;
                (
                    FmiEventIndicatorEntry {
                        reading: FmiIndicatorReading::DeadlineDistance { index },
                        zero_side: FmiIndicatorZeroSide::Frozen,
                        crossing_root_index: None,
                    },
                    None,
                )
            }
            FmiEventIndicatorSource::DelayDiscontinuity { index } => {
                let root_index = model_root_count.checked_add(index).ok_or_else(|| {
                    indicator_error(position, "delay source root position overflows")
                })?;
                if root_index >= root_value_count {
                    return Err(indicator_error(
                        position,
                        "delay source leaves its checked block",
                    ));
                }
                reads_roots = true;
                (
                    FmiEventIndicatorEntry {
                        reading: FmiIndicatorReading::RootValue { index: root_index },
                        zero_side: FmiIndicatorZeroSide::Frozen,
                        crossing_root_index: Some(root_index),
                    },
                    None,
                )
            }
        };
        entries.push(entry);
        relation_memory_targets.push(relation_target);
    }
    let published_width = FmiPublishedIndicatorWidth::new(entries.len());
    Ok(FmiEventIndicatorPlan {
        entries: entries.into_boxed_slice(),
        relation_memory_targets: relation_memory_targets.into_boxed_slice(),
        published_width,
        root_value_width: FmiRootValueWidth::new(if reads_roots { root_value_count } else { 0 }),
        deadline_width: FmiDeadlineWidth::new(if reads_deadlines { deadline_count } else { 0 }),
        domain_width: FmiIndicatorDomainWidth::new(published_width.len()),
    })
}

fn indicator_error(position: usize, reason: &str) -> FmiComponentError {
    FmiComponentError::EventIndicatorInventory {
        message: format!("position {position}: {reason}"),
        span: None,
    }
}
