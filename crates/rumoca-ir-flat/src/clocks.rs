//! Clock and synchronous structures (MLS §16).
//!
//! This module provides data structures for synchronous language elements:
//! - Clock types and definitions
//! - Base-clock partitions
//! - Sub-clock partitions
//! - Clock inference data

use indexmap::IndexMap;
use rumoca_core::{
    ClockLattice, ClockLatticeError, ClockLatticeErrorKind, ClockRational, ProvenanceSpan, Span,
};
use serde::{Deserialize, Deserializer, Serialize, Serializer, de::Error as _};

use crate::{Equation, VarName};

/// Result of an exact base-clock lattice query, spanned at the clock
/// expression that introduced the clock.
pub type ClockLatticeResult<T> = Result<T, ClockLatticeError>;

// =============================================================================
// Clock Partitions (MLS §16.7)
// =============================================================================

/// MLS §16.7: Clock Partitions.
///
/// After flattening, equations are partitioned into clock partitions:
/// - Base-clock partitions: sets that must execute together in one task
/// - Sub-clock partitions: subsets within a base partition with different sub-sampling
#[derive(Debug, Clone)]
pub struct ClockPartitions {
    /// Source span for the flattened model whose clock ownership is represented.
    source_span: ProvenanceSpan,
    /// Base-clock partitions (execute asynchronously from each other).
    base_partitions: Vec<BaseClockPartition>,
    /// Dense-position lookup for globally unique base-clock IDs.
    base_partition_index: IndexMap<u32, usize>,
    /// The continuous-time partition (if any).
    /// This contains equations without explicit clock associations.
    continuous_partition: Option<ContinuousPartition>,
    /// Authoritative variable-to-clock ownership, derived during insertion.
    variable_owners: IndexMap<VarName, VariableClockOwner>,
}

impl ClockPartitions {
    /// Construct the clock ownership root for one flattened model.
    ///
    /// ```compile_fail
    /// use rumoca_core::Span;
    /// use rumoca_ir_flat::ClockPartitions;
    ///
    /// let _ = ClockPartitions::construct(Span::DUMMY);
    /// ```
    pub fn construct(source_span: ProvenanceSpan) -> Self {
        Self {
            source_span,
            base_partitions: Vec::new(),
            base_partition_index: IndexMap::new(),
            continuous_partition: None,
            variable_owners: IndexMap::new(),
        }
    }

    pub fn source_span(&self) -> Span {
        self.source_span.span()
    }

    /// Add a checked base-clock partition and derive all variable associations.
    pub fn add_base_partition(
        &mut self,
        partition: BaseClockPartition,
    ) -> Result<(), ClockPartitionError> {
        if let Some(&index) = self.base_partition_index.get(&partition.id) {
            return Err(ClockPartitionError::DuplicateBasePartition {
                id: partition.id,
                first_span: self.base_partitions[index].source_span(),
                duplicate_span: partition.source_span(),
            });
        }
        let pending_owners = partition.variable_owners();
        for (name, owner) in &pending_owners {
            self.require_unowned(name, *owner)?;
        }
        let index = self.base_partitions.len();
        self.base_partition_index.insert(partition.id, index);
        self.variable_owners.extend(pending_owners);
        self.base_partitions.push(partition);
        Ok(())
    }

    pub fn num_base_partitions(&self) -> usize {
        self.base_partitions.len()
    }

    pub fn base_partitions(&self) -> &[BaseClockPartition] {
        &self.base_partitions
    }

    pub fn base_partition(&self, id: u32) -> Option<&BaseClockPartition> {
        self.base_partition_index
            .get(&id)
            .map(|&index| &self.base_partitions[index])
    }

    /// Define the single continuous-time owner, including an intentionally
    /// empty continuous partition.
    pub fn define_continuous_partition(
        &mut self,
        source_span: ProvenanceSpan,
    ) -> Result<(), ClockPartitionError> {
        if let Some(first) = &self.continuous_partition {
            return Err(ClockPartitionError::DuplicateContinuousPartition {
                first_span: first.source_span(),
                duplicate_span: source_span.span(),
            });
        }
        self.continuous_partition = Some(ContinuousPartition::new(source_span));
        Ok(())
    }

    /// Add one continuous-time variable and make its unclocked ownership
    /// authoritative.
    pub fn add_continuous_variable(
        &mut self,
        name: VarName,
        occurrence: ProvenanceSpan,
    ) -> Result<(), ClockPartitionError> {
        let owner = VariableClockOwner {
            association: ClockAssociation::Continuous,
            span: occurrence,
        };
        self.require_unowned(&name, owner)?;
        self.continuous_partition
            .get_or_insert_with(|| ContinuousPartition::new(occurrence))
            .variables
            .insert(name.clone(), occurrence);
        self.variable_owners.insert(name, owner);
        Ok(())
    }

    /// Add one source-proven continuous-time equation.
    pub fn add_continuous_equation(
        &mut self,
        equation: Equation,
    ) -> Result<(), ClockPartitionError> {
        let occurrence = equation
            .span
            .require_provenance("Flat continuous partition equation")
            .map_err(ClockPartitionError::MissingProvenance)?;
        self.continuous_partition
            .get_or_insert_with(|| ContinuousPartition::new(occurrence))
            .equations
            .push(equation);
        Ok(())
    }

    pub fn continuous_source_span(&self) -> Option<Span> {
        self.continuous_partition
            .as_ref()
            .map(ContinuousPartition::source_span)
    }

    pub fn continuous_variables(&self) -> impl Iterator<Item = &VarName> {
        self.continuous_partition
            .as_ref()
            .into_iter()
            .flat_map(|partition| partition.variables.keys())
    }

    pub fn continuous_equations(&self) -> &[Equation] {
        self.continuous_partition
            .as_ref()
            .map_or(&[], |partition| partition.equations.as_slice())
    }

    pub fn association(&self, name: &VarName) -> Option<ClockAssociation> {
        self.variable_owners
            .get(name)
            .map(|owner| owner.association)
    }

    pub fn association_span(&self, name: &VarName) -> Option<Span> {
        self.variable_owners
            .get(name)
            .map(|owner| owner.span.span())
    }

    fn require_unowned(
        &self,
        name: &VarName,
        pending: VariableClockOwner,
    ) -> Result<(), ClockPartitionError> {
        if let Some(first) = self.variable_owners.get(name) {
            return Err(ClockPartitionError::VariableInMultipleClockPartitions {
                name: name.clone(),
                first: first.association,
                second: pending.association,
                first_span: first.span.span(),
                duplicate_span: pending.span.span(),
            });
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ClockAssociation {
    Continuous,
    Base { base_id: u32 },
    Sub { base_id: u32, sub_id: u32 },
}

#[derive(Debug, Clone, Copy)]
struct VariableClockOwner {
    association: ClockAssociation,
    span: ProvenanceSpan,
}

/// MLS §16.7: Base-Clock Partition.
///
/// "A set of equations and variables which must be executed together in one task."
/// Different base-partitions can execute asynchronously with respect to each other.
#[derive(Debug, Clone)]
pub struct BaseClockPartition {
    /// Unique identifier for this partition.
    id: u32,
    /// Source span for the semantic owner that introduced this partition.
    source_span: ProvenanceSpan,
    /// The base clock for this partition.
    clock: BaseClock,
    /// Variables in this partition and their owning source occurrences.
    variables: IndexMap<VarName, ProvenanceSpan>,
    /// Equations in this partition.
    equations: Vec<Equation>,
    /// Sub-clock partitions within this base partition.
    sub_partitions: Vec<SubClockPartition>,
    /// Dense-position lookup for unique sub-clock IDs.
    sub_partition_index: IndexMap<u32, usize>,
    /// Variable occurrence and owning sub-clock ID.
    sub_variable_owners: IndexMap<VarName, (u32, ProvenanceSpan)>,
    /// Whether this is a discretized partition (contains der(), delay(), etc.).
    /// MLS §16.8.1: "If the partition contains a Discretized Variables, it is called discretized."
    discretized_at: Option<ProvenanceSpan>,
}

impl BaseClockPartition {
    /// Construct a base-clock partition owned by an exact source occurrence.
    ///
    /// ```compile_fail
    /// use rumoca_core::Span;
    /// use rumoca_ir_flat::{BaseClock, BaseClockPartition};
    ///
    /// let clock = BaseClock::inferred(Span::DUMMY);
    /// let _ = BaseClockPartition::construct(0, clock, Span::DUMMY);
    /// ```
    pub fn construct(id: u32, clock: BaseClock, source_span: ProvenanceSpan) -> Self {
        Self {
            id,
            source_span,
            clock,
            variables: IndexMap::new(),
            equations: Vec::new(),
            sub_partitions: Vec::new(),
            sub_partition_index: IndexMap::new(),
            sub_variable_owners: IndexMap::new(),
            discretized_at: None,
        }
    }

    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn source_span(&self) -> Span {
        self.source_span.span()
    }

    pub fn clock(&self) -> &BaseClock {
        &self.clock
    }

    pub fn variables(&self) -> impl ExactSizeIterator<Item = &VarName> {
        self.variables.keys()
    }

    pub fn variable_span(&self, name: &VarName) -> Option<Span> {
        self.variables.get(name).copied().map(ProvenanceSpan::span)
    }

    pub fn equations(&self) -> &[Equation] {
        &self.equations
    }

    pub fn sub_partitions(&self) -> &[SubClockPartition] {
        &self.sub_partitions
    }

    pub fn is_discretized(&self) -> bool {
        self.discretized_at.is_some()
    }

    pub fn discretized_span(&self) -> Option<Span> {
        self.discretized_at.map(ProvenanceSpan::span)
    }

    /// Add one variable occurrence to this partition.
    pub fn add_variable(
        &mut self,
        name: VarName,
        occurrence: ProvenanceSpan,
    ) -> Result<(), ClockPartitionError> {
        if let Some(first) = self.variables.get(&name) {
            return Err(ClockPartitionError::DuplicateVariable {
                name,
                first_span: first.span(),
                duplicate_span: occurrence.span(),
            });
        }
        self.variables.insert(name, occurrence);
        Ok(())
    }

    /// Add an equation whose source occurrence is known.
    pub fn add_equation(&mut self, eq: Equation) -> Result<(), ClockPartitionError> {
        eq.span
            .require_provenance("Flat base-clock partition equation")
            .map_err(ClockPartitionError::MissingProvenance)?;
        self.equations.push(eq);
        Ok(())
    }

    /// Add one checked sub-clock owner atomically.
    pub fn add_sub_partition(&mut self, sub: SubClockPartition) -> Result<(), ClockPartitionError> {
        if let Some(&index) = self.sub_partition_index.get(&sub.id) {
            return Err(ClockPartitionError::DuplicateSubPartition {
                id: sub.id,
                first_span: self.sub_partitions[index].source_span(),
                duplicate_span: sub.source_span(),
            });
        }
        for (name, occurrence) in &sub.variables {
            if let Some((_, first)) = self.sub_variable_owners.get(name) {
                return Err(ClockPartitionError::VariableInMultipleSubPartitions {
                    name: name.clone(),
                    first_span: first.span(),
                    duplicate_span: occurrence.span(),
                });
            }
        }
        for (name, occurrence) in &sub.variables {
            self.variables.entry(name.clone()).or_insert(*occurrence);
            self.sub_variable_owners
                .insert(name.clone(), (sub.id, *occurrence));
        }
        self.sub_partition_index
            .insert(sub.id, self.sub_partitions.len());
        self.sub_partitions.push(sub);
        Ok(())
    }

    /// Record the source construct that makes this partition discretized.
    pub fn mark_discretized(&mut self, responsible_span: ProvenanceSpan) {
        self.discretized_at.get_or_insert(responsible_span);
    }

    fn variable_owners(&self) -> IndexMap<VarName, VariableClockOwner> {
        self.variables
            .iter()
            .map(|(name, span)| {
                let (association, owner_span) = self.sub_variable_owners.get(name).map_or(
                    (ClockAssociation::Base { base_id: self.id }, *span),
                    |(sub_id, sub_span)| {
                        (
                            ClockAssociation::Sub {
                                base_id: self.id,
                                sub_id: *sub_id,
                            },
                            *sub_span,
                        )
                    },
                );
                (
                    name.clone(),
                    VariableClockOwner {
                        association,
                        span: owner_span,
                    },
                )
            })
            .collect()
    }
}

/// MLS §16.7: Sub-Clock Partition.
///
/// "A subset of equations and variables of a base-partition which are
/// partially synchronized with other sub-partitions of the same base-partition."
#[derive(Debug, Clone)]
pub struct SubClockPartition {
    /// Unique identifier within the base partition.
    id: u32,
    /// Source span for the semantic owner that introduced this partition.
    source_span: ProvenanceSpan,
    /// The sub-clock definition.
    sub_clock: SubClock,
    /// Variables in this sub-partition and their owning source occurrences.
    variables: IndexMap<VarName, ProvenanceSpan>,
    /// Equations in this sub-partition.
    equations: Vec<Equation>,
}

impl SubClockPartition {
    /// Construct a sub-clock partition owned by an exact source occurrence.
    pub fn construct(id: u32, sub_clock: SubClock, source_span: ProvenanceSpan) -> Self {
        Self {
            id,
            source_span,
            sub_clock,
            variables: IndexMap::new(),
            equations: Vec::new(),
        }
    }

    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn source_span(&self) -> Span {
        self.source_span.span()
    }

    pub fn sub_clock(&self) -> &SubClock {
        &self.sub_clock
    }

    pub fn variables(&self) -> impl ExactSizeIterator<Item = &VarName> {
        self.variables.keys()
    }

    pub fn variable_span(&self, name: &VarName) -> Option<Span> {
        self.variables.get(name).copied().map(ProvenanceSpan::span)
    }

    pub fn equations(&self) -> &[Equation] {
        &self.equations
    }

    pub fn add_variable(
        &mut self,
        name: VarName,
        occurrence: ProvenanceSpan,
    ) -> Result<(), ClockPartitionError> {
        if let Some(first) = self.variables.get(&name) {
            return Err(ClockPartitionError::DuplicateVariable {
                name,
                first_span: first.span(),
                duplicate_span: occurrence.span(),
            });
        }
        self.variables.insert(name, occurrence);
        Ok(())
    }

    pub fn add_equation(&mut self, eq: Equation) -> Result<(), ClockPartitionError> {
        eq.span
            .require_provenance("Flat sub-clock partition equation")
            .map_err(ClockPartitionError::MissingProvenance)?;
        self.equations.push(eq);
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClockPartitionError {
    DuplicateVariable {
        name: VarName,
        first_span: Span,
        duplicate_span: Span,
    },
    DuplicateSubPartition {
        id: u32,
        first_span: Span,
        duplicate_span: Span,
    },
    DuplicateBasePartition {
        id: u32,
        first_span: Span,
        duplicate_span: Span,
    },
    DuplicateContinuousPartition {
        first_span: Span,
        duplicate_span: Span,
    },
    VariableInMultipleSubPartitions {
        name: VarName,
        first_span: Span,
        duplicate_span: Span,
    },
    VariableInMultipleClockPartitions {
        name: VarName,
        first: ClockAssociation,
        second: ClockAssociation,
        first_span: Span,
        duplicate_span: Span,
    },
    MissingProvenance(rumoca_core::MissingProvenanceSpan),
}

impl std::fmt::Display for ClockPartitionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::DuplicateVariable { name, .. } => {
                write!(formatter, "duplicate clock-partition variable `{name}`")
            }
            Self::DuplicateSubPartition { id, .. } => {
                write!(formatter, "duplicate sub-clock partition id {id}")
            }
            Self::DuplicateBasePartition { id, .. } => {
                write!(formatter, "duplicate base-clock partition id {id}")
            }
            Self::DuplicateContinuousPartition { .. } => {
                write!(formatter, "duplicate continuous-time partition")
            }
            Self::VariableInMultipleSubPartitions { name, .. } => write!(
                formatter,
                "variable `{name}` belongs to multiple sibling sub-clock partitions"
            ),
            Self::VariableInMultipleClockPartitions {
                name,
                first,
                second,
                ..
            } => write!(
                formatter,
                "variable `{name}` belongs to both {first:?} and {second:?}"
            ),
            Self::MissingProvenance(error) => error.fmt(formatter),
        }
    }
}

impl std::error::Error for ClockPartitionError {}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ClockPartitionVariableWire {
    name: VarName,
    span: Span,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct SubClockPartitionWire {
    id: u32,
    source_span: Span,
    sub_clock: SubClock,
    variables: Vec<ClockPartitionVariableWire>,
    equations: Vec<Equation>,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct BaseClockPartitionWire {
    id: u32,
    source_span: Span,
    clock: BaseClock,
    variables: Vec<ClockPartitionVariableWire>,
    equations: Vec<Equation>,
    sub_partitions: Vec<SubClockPartitionWire>,
    discretized_at: Option<Span>,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ContinuousPartitionWire {
    source_span: Span,
    variables: Vec<ClockPartitionVariableWire>,
    equations: Vec<Equation>,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct ClockPartitionsWire {
    source_span: Span,
    base_partitions: Vec<BaseClockPartitionWire>,
    continuous_partition: Option<ContinuousPartitionWire>,
}

impl Serialize for ClockPartitions {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        ClockPartitionsWire {
            source_span: self.source_span(),
            base_partitions: self
                .base_partitions
                .iter()
                .map(BaseClockPartitionWire::from)
                .collect(),
            continuous_partition: self
                .continuous_partition
                .as_ref()
                .map(ContinuousPartitionWire::from),
        }
        .serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ClockPartitions {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        ClockPartitionsWire::deserialize(deserializer)?.reconstruct::<D::Error>()
    }
}

impl From<&ContinuousPartition> for ContinuousPartitionWire {
    fn from(partition: &ContinuousPartition) -> Self {
        Self {
            source_span: partition.source_span(),
            variables: partition
                .variables
                .iter()
                .map(|(name, span)| ClockPartitionVariableWire {
                    name: name.clone(),
                    span: span.span(),
                })
                .collect(),
            equations: partition.equations.clone(),
        }
    }
}

impl ClockPartitionsWire {
    fn reconstruct<E: serde::de::Error>(self) -> Result<ClockPartitions, E> {
        let source_span = self
            .source_span
            .require_provenance("Flat clock-partition root")
            .map_err(E::custom)?;
        let mut partitions = ClockPartitions::construct(source_span);
        for base_partition in self.base_partitions {
            partitions
                .add_base_partition(base_partition.reconstruct::<E>()?)
                .map_err(E::custom)?;
        }
        if let Some(continuous) = self.continuous_partition {
            let owner = continuous
                .source_span
                .require_provenance("Flat continuous partition owner")
                .map_err(E::custom)?;
            partitions
                .define_continuous_partition(owner)
                .map_err(E::custom)?;
            for variable in continuous.variables {
                let occurrence = variable
                    .span
                    .require_provenance("Flat continuous partition variable")
                    .map_err(E::custom)?;
                partitions
                    .add_continuous_variable(variable.name, occurrence)
                    .map_err(E::custom)?;
            }
            for equation in continuous.equations {
                partitions
                    .add_continuous_equation(equation)
                    .map_err(E::custom)?;
            }
        }
        Ok(partitions)
    }
}

impl Serialize for SubClockPartition {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        SubClockPartitionWire::from(self).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SubClockPartition {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        SubClockPartitionWire::deserialize(deserializer)?.reconstruct::<D::Error>()
    }
}

impl From<&SubClockPartition> for SubClockPartitionWire {
    fn from(partition: &SubClockPartition) -> Self {
        Self {
            id: partition.id,
            source_span: partition.source_span(),
            sub_clock: partition.sub_clock.clone(),
            variables: partition
                .variables
                .iter()
                .map(|(name, span)| ClockPartitionVariableWire {
                    name: name.clone(),
                    span: span.span(),
                })
                .collect(),
            equations: partition.equations.clone(),
        }
    }
}

impl SubClockPartitionWire {
    fn reconstruct<E: serde::de::Error>(self) -> Result<SubClockPartition, E> {
        let source_span = self
            .source_span
            .require_provenance("Flat sub-clock partition owner")
            .map_err(E::custom)?;
        let mut partition = SubClockPartition::construct(self.id, self.sub_clock, source_span);
        for variable in self.variables {
            let occurrence = variable
                .span
                .require_provenance("Flat sub-clock partition variable")
                .map_err(E::custom)?;
            partition
                .add_variable(variable.name, occurrence)
                .map_err(E::custom)?;
        }
        for equation in self.equations {
            partition.add_equation(equation).map_err(E::custom)?;
        }
        Ok(partition)
    }
}

impl Serialize for BaseClockPartition {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        BaseClockPartitionWire::from(self).serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for BaseClockPartition {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        BaseClockPartitionWire::deserialize(deserializer)?.reconstruct::<D::Error>()
    }
}

impl From<&BaseClockPartition> for BaseClockPartitionWire {
    fn from(partition: &BaseClockPartition) -> Self {
        Self {
            id: partition.id,
            source_span: partition.source_span(),
            clock: partition.clock.clone(),
            variables: partition
                .variables
                .iter()
                .map(|(name, span)| ClockPartitionVariableWire {
                    name: name.clone(),
                    span: span.span(),
                })
                .collect(),
            equations: partition.equations.clone(),
            sub_partitions: partition
                .sub_partitions
                .iter()
                .map(SubClockPartitionWire::from)
                .collect(),
            discretized_at: partition.discretized_span(),
        }
    }
}

impl BaseClockPartitionWire {
    fn reconstruct<E: serde::de::Error>(self) -> Result<BaseClockPartition, E> {
        let source_span = self
            .source_span
            .require_provenance("Flat base-clock partition owner")
            .map_err(E::custom)?;
        let mut partition = BaseClockPartition::construct(self.id, self.clock, source_span);
        for variable in self.variables {
            let occurrence = variable
                .span
                .require_provenance("Flat base-clock partition variable")
                .map_err(E::custom)?;
            partition
                .add_variable(variable.name, occurrence)
                .map_err(E::custom)?;
        }
        for equation in self.equations {
            partition.add_equation(equation).map_err(E::custom)?;
        }
        for sub_partition in self.sub_partitions {
            partition
                .add_sub_partition(sub_partition.reconstruct::<E>()?)
                .map_err(E::custom)?;
        }
        if let Some(discretized_at) = self.discretized_at {
            partition.mark_discretized(
                discretized_at
                    .require_provenance("Flat discretized clock partition")
                    .map_err(E::custom)?,
            );
        }
        Ok(partition)
    }
}

/// The continuous-time partition (non-clocked equations).
#[derive(Debug, Clone)]
struct ContinuousPartition {
    source_span: ProvenanceSpan,
    variables: IndexMap<VarName, ProvenanceSpan>,
    equations: Vec<Equation>,
}

impl ContinuousPartition {
    fn new(source_span: ProvenanceSpan) -> Self {
        Self {
            source_span,
            variables: IndexMap::new(),
            equations: Vec::new(),
        }
    }

    fn source_span(&self) -> Span {
        self.source_span.span()
    }
}

// =============================================================================
// Clock Types (MLS §16.3)
// =============================================================================

/// MLS §16.3: Base Clock.
///
/// A base clock determines when a partition is active.
#[derive(Debug, Clone)]
pub struct BaseClock {
    /// The kind of clock.
    kind: ClockKind,
    /// Source span for the clock expression that introduced this base clock.
    source_span: Span,
    /// Inferred base interval (for rational clocks).
    base_interval: Option<ClockInterval>,
}

impl BaseClock {
    /// Create a new inferred clock.
    pub fn inferred(source_span: Span) -> Self {
        Self {
            kind: ClockKind::Inferred,
            source_span,
            base_interval: None,
        }
    }

    /// Create a new periodic clock from a `Clock(interval)` argument.
    ///
    /// MLS §16.5 makes every derived clock an exact integer relation of its
    /// base clock, so the base interval is stored as an exact rational whenever
    /// `interval` has one. `ClockInterval::Seconds` is kept only for intervals
    /// with no reduced rational form; that is the inexact `Clock(interval)`
    /// case of §16.3 and it cannot participate in a lattice.
    pub fn periodic(interval: f64, source_span: Span) -> ClockLatticeResult<Self> {
        require_positive_seconds(interval, source_span)?;
        let base_interval = match ClockRational::from_seconds(interval) {
            Ok(rational) => ClockInterval::Rational(rational),
            Err(_) => ClockInterval::Seconds(interval),
        };
        Ok(Self {
            kind: ClockKind::Periodic { interval },
            source_span,
            base_interval: Some(base_interval),
        })
    }

    /// MLS §16.3 `Clock(intervalCounter, resolution)`: an exactly rational
    /// base clock of `intervalCounter / resolution` seconds.
    pub fn rational(
        interval_counter: i64,
        resolution: i64,
        source_span: Span,
    ) -> ClockLatticeResult<Self> {
        if resolution <= 0 {
            return Err(ClockLatticeErrorKind::NonPositiveFactor.at(source_span));
        }
        if interval_counter < 0 {
            return Err(ClockLatticeErrorKind::NonPositivePeriod.at(source_span));
        }
        if interval_counter == 0 {
            return Ok(Self {
                kind: ClockKind::Rational {
                    interval_counter,
                    resolution,
                },
                source_span,
                base_interval: None,
            });
        }
        let lattice = ClockLattice::from_interval_counter(interval_counter, resolution)
            .map_err(|kind| kind.at(source_span))?;
        Ok(Self {
            kind: ClockKind::Rational {
                interval_counter,
                resolution,
            },
            source_span,
            base_interval: Some(ClockInterval::Rational(lattice.period())),
        })
    }

    /// Create an event clock from `Clock(condition, startInterval)`.
    pub fn event(start_interval: Option<f64>, source_span: Span) -> Self {
        Self {
            kind: ClockKind::Event { start_interval },
            source_span,
            base_interval: None,
        }
    }

    /// Create a solver clock from `Clock(c, solverMethod)`.
    pub fn solver(solver_method: String, source_span: Span) -> Self {
        Self {
            kind: ClockKind::Solver { solver_method },
            source_span,
            base_interval: None,
        }
    }

    pub fn kind(&self) -> &ClockKind {
        &self.kind
    }

    pub fn source_span(&self) -> Span {
        self.source_span
    }

    pub fn base_interval(&self) -> Option<&ClockInterval> {
        self.base_interval.as_ref()
    }

    /// The exact lattice this base clock spans, or a spanned error when the
    /// clock is not a statically periodic rational clock.
    pub fn lattice(&self) -> ClockLatticeResult<ClockLattice> {
        let interval = self.base_interval.as_ref().ok_or_else(|| {
            ClockLatticeErrorKind::NotRationallyRepresentable.at(self.source_span)
        })?;
        let period = interval
            .exact_period()
            .map_err(|kind| kind.at(self.source_span))?;
        ClockLattice::new(period, ClockRational::ZERO).map_err(|kind| kind.at(self.source_span))
    }
}

fn require_positive_seconds(value: f64, source_span: Span) -> ClockLatticeResult<()> {
    if !value.is_finite() {
        return Err(ClockLatticeErrorKind::NonFiniteSeconds.at(source_span));
    }
    if value <= 0.0 {
        return Err(ClockLatticeErrorKind::NonPositivePeriod.at(source_span));
    }
    Ok(())
}

#[derive(Serialize)]
#[serde(deny_unknown_fields)]
struct BaseClockWireRef<'a> {
    kind: &'a ClockKind,
    source_span: Span,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct BaseClockWire {
    kind: ClockKind,
    source_span: Span,
}

impl Serialize for BaseClock {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        BaseClockWireRef {
            kind: &self.kind,
            source_span: self.source_span,
        }
        .serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for BaseClock {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let wire = BaseClockWire::deserialize(deserializer)?;
        let result = match wire.kind {
            ClockKind::Inferred => Ok(Self::inferred(wire.source_span)),
            ClockKind::Periodic { interval } => Self::periodic(interval, wire.source_span),
            ClockKind::Rational {
                interval_counter,
                resolution,
            } => Self::rational(interval_counter, resolution, wire.source_span),
            ClockKind::Event { start_interval } => {
                Ok(Self::event(start_interval, wire.source_span))
            }
            ClockKind::Solver { solver_method } => {
                Ok(Self::solver(solver_method, wire.source_span))
            }
        };
        result.map_err(D::Error::custom)
    }
}

/// MLS §16.3: Clock Kind.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub enum ClockKind {
    /// Clock inferred from context.
    #[default]
    Inferred,
    /// Clock(interval): Periodic clock with fixed interval.
    /// MLS §16.3: "interval must be strictly positive (interval > 0)"
    Periodic {
        /// Interval in seconds.
        interval: f64,
    },
    /// Clock(intervalCounter, resolution): Rational interval clock.
    /// MLS §16.3: "intervalCounter must be > 0"
    Rational {
        /// Interval counter (clocked Integer expression).
        interval_counter: i64,
        /// Resolution in ticks per second.
        resolution: i64,
    },
    /// Clock(condition, startInterval): Event clock.
    /// Ticks when condition becomes true.
    Event {
        /// Start interval for first tick.
        start_interval: Option<f64>,
    },
    /// Clock(c, solverMethod): Solver clock.
    /// For discretizing continuous-time equations.
    Solver {
        /// Solver method name.
        solver_method: String,
    },
}

/// MLS §16.5.2: Sub-Clock.
///
/// Defines the relationship between a sub-clock and its base clock.
#[derive(Debug, Clone)]
pub struct SubClock {
    /// Source span for the sub-clock expression.
    source_span: ProvenanceSpan,
    /// Source-ordered clock conversion operations.
    operations: Vec<SubClockOperation>,
}

#[derive(Debug, Clone)]
enum SubClockOperation {
    SubSample {
        factor: i64,
        span: ProvenanceSpan,
    },
    SuperSample {
        factor: i64,
        span: ProvenanceSpan,
    },
    ShiftSample {
        counter: i64,
        resolution: i64,
        span: ProvenanceSpan,
    },
    BackSample {
        counter: i64,
        resolution: i64,
        span: ProvenanceSpan,
    },
    NoClock {
        span: ProvenanceSpan,
    },
}

impl SubClock {
    /// Create the identity sub-clock used before clock inference adds
    /// conversions.
    ///
    /// Raw spans cannot bypass the provenance requirement:
    ///
    /// ```compile_fail
    /// use rumoca_core::Span;
    /// use rumoca_ir_flat::SubClock;
    ///
    /// let _ = SubClock::identity(Span::DUMMY);
    /// ```
    pub fn identity(source_span: ProvenanceSpan) -> Self {
        Self {
            source_span,
            operations: Vec::new(),
        }
    }

    /// Create a sub-sampled clock.
    pub fn sub_sample(factor: i64, source_span: ProvenanceSpan) -> ClockLatticeResult<Self> {
        Self::identity(source_span).then_sub_sample(factor, source_span)
    }

    /// Create a super-sampled clock.
    pub fn super_sample(factor: i64, source_span: ProvenanceSpan) -> ClockLatticeResult<Self> {
        Self::identity(source_span).then_super_sample(factor, source_span)
    }

    /// Create a shifted clock (MLS §16.5.2 `shiftSample`).
    pub fn shift_sample(
        counter: i64,
        resolution: i64,
        source_span: ProvenanceSpan,
    ) -> ClockLatticeResult<Self> {
        Self::identity(source_span).then_shift_sample(counter, resolution, source_span)
    }

    /// Create a back-shifted clock (MLS §16.5.2 `backSample`).
    pub fn back_sample(
        counter: i64,
        resolution: i64,
        source_span: ProvenanceSpan,
    ) -> ClockLatticeResult<Self> {
        Self::identity(source_span).then_back_sample(counter, resolution, source_span)
    }

    /// Create an inferred `noClock` conversion.
    pub fn no_clock(source_span: ProvenanceSpan) -> Self {
        Self {
            source_span,
            operations: vec![SubClockOperation::NoClock { span: source_span }],
        }
    }

    pub fn then_sub_sample(
        mut self,
        factor: i64,
        operation_span: ProvenanceSpan,
    ) -> ClockLatticeResult<Self> {
        require_nonnegative_factor(factor, operation_span)?;
        self.operations.push(SubClockOperation::SubSample {
            factor,
            span: operation_span,
        });
        Ok(self)
    }

    pub fn then_super_sample(
        mut self,
        factor: i64,
        operation_span: ProvenanceSpan,
    ) -> ClockLatticeResult<Self> {
        require_nonnegative_factor(factor, operation_span)?;
        self.operations.push(SubClockOperation::SuperSample {
            factor,
            span: operation_span,
        });
        Ok(self)
    }

    pub fn then_shift_sample(
        mut self,
        counter: i64,
        resolution: i64,
        operation_span: ProvenanceSpan,
    ) -> ClockLatticeResult<Self> {
        require_shift_arguments(counter, resolution, operation_span)?;
        self.operations.push(SubClockOperation::ShiftSample {
            counter,
            resolution,
            span: operation_span,
        });
        Ok(self)
    }

    pub fn then_back_sample(
        mut self,
        counter: i64,
        resolution: i64,
        operation_span: ProvenanceSpan,
    ) -> ClockLatticeResult<Self> {
        require_shift_arguments(counter, resolution, operation_span)?;
        self.operations.push(SubClockOperation::BackSample {
            counter,
            resolution,
            span: operation_span,
        });
        Ok(self)
    }

    pub fn then_no_clock(mut self, operation_span: ProvenanceSpan) -> Self {
        self.operations.push(SubClockOperation::NoClock {
            span: operation_span,
        });
        self
    }

    pub fn source_span(&self) -> Span {
        self.source_span.span()
    }

    /// Derive this sub-clock's exact lattice from its base clock.
    ///
    /// MLS §16.5.2 defines all four conversions as integer relations over the
    /// source clock, so the derivation is exact integer arithmetic and never a
    /// floating-point scaling. `noClock()` has no periodic lattice.
    pub fn derive(&self, base: ClockLattice) -> ClockLatticeResult<ClockLattice> {
        let mut derived = base;
        for operation in &self.operations {
            derived = operation.apply(derived)?;
        }
        Ok(derived)
    }
}

impl SubClockOperation {
    fn apply(&self, lattice: ClockLattice) -> ClockLatticeResult<ClockLattice> {
        match *self {
            Self::SubSample { factor: 0, span }
            | Self::SuperSample { factor: 0, span }
            | Self::NoClock { span } => {
                Err(ClockLatticeErrorKind::NotRationallyRepresentable.at(span.span()))
            }
            Self::SubSample { factor, span } => lattice
                .sub_sample(factor)
                .map_err(|kind| kind.at(span.span())),
            Self::SuperSample { factor, span } => lattice
                .super_sample(factor)
                .map_err(|kind| kind.at(span.span())),
            Self::ShiftSample {
                counter,
                resolution,
                span,
            } => lattice
                .shift_sample(counter, resolution)
                .map_err(|kind| kind.at(span.span())),
            Self::BackSample {
                counter,
                resolution,
                span,
            } => lattice
                .back_sample(counter, resolution)
                .map_err(|kind| kind.at(span.span())),
        }
    }
}

fn require_nonnegative_factor(factor: i64, span: ProvenanceSpan) -> ClockLatticeResult<()> {
    if factor < 0 {
        return Err(ClockLatticeErrorKind::NonPositiveFactor.at(span.span()));
    }
    Ok(())
}

fn require_shift_arguments(
    counter: i64,
    resolution: i64,
    span: ProvenanceSpan,
) -> ClockLatticeResult<()> {
    if counter < 0 || resolution <= 0 {
        return Err(ClockLatticeErrorKind::NonPositiveFactor.at(span.span()));
    }
    Ok(())
}

#[derive(Serialize, Deserialize)]
#[serde(tag = "operation", rename_all = "snake_case", deny_unknown_fields)]
enum SubClockOperationWire {
    SubSample {
        factor: i64,
        span: Span,
    },
    SuperSample {
        factor: i64,
        span: Span,
    },
    ShiftSample {
        counter: i64,
        resolution: i64,
        span: Span,
    },
    BackSample {
        counter: i64,
        resolution: i64,
        span: Span,
    },
    NoClock {
        span: Span,
    },
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct SubClockWire {
    source_span: Span,
    operations: Vec<SubClockOperationWire>,
}

impl Serialize for SubClock {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        SubClockWire {
            source_span: self.source_span.span(),
            operations: self
                .operations
                .iter()
                .map(SubClockOperationWire::from)
                .collect(),
        }
        .serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for SubClock {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let wire = SubClockWire::deserialize(deserializer)?;
        wire.reconstruct::<D::Error>()
    }
}

impl From<&SubClockOperation> for SubClockOperationWire {
    fn from(operation: &SubClockOperation) -> Self {
        match *operation {
            SubClockOperation::SubSample { factor, span } => Self::SubSample {
                factor,
                span: span.span(),
            },
            SubClockOperation::SuperSample { factor, span } => Self::SuperSample {
                factor,
                span: span.span(),
            },
            SubClockOperation::ShiftSample {
                counter,
                resolution,
                span,
            } => Self::ShiftSample {
                counter,
                resolution,
                span: span.span(),
            },
            SubClockOperation::BackSample {
                counter,
                resolution,
                span,
            } => Self::BackSample {
                counter,
                resolution,
                span: span.span(),
            },
            SubClockOperation::NoClock { span } => Self::NoClock { span: span.span() },
        }
    }
}

impl SubClockWire {
    fn reconstruct<E: serde::de::Error>(self) -> Result<SubClock, E> {
        let source_span = self
            .source_span
            .require_provenance("Flat sub-clock owner")
            .map_err(E::custom)?;
        let mut clock = SubClock::identity(source_span);
        for operation in self.operations {
            clock = match operation {
                SubClockOperationWire::SubSample { factor, span } => clock
                    .then_sub_sample(factor, operation_provenance(span).map_err(E::custom)?)
                    .map_err(E::custom)?,
                SubClockOperationWire::SuperSample { factor, span } => clock
                    .then_super_sample(factor, operation_provenance(span).map_err(E::custom)?)
                    .map_err(E::custom)?,
                SubClockOperationWire::ShiftSample {
                    counter,
                    resolution,
                    span,
                } => clock
                    .then_shift_sample(
                        counter,
                        resolution,
                        operation_provenance(span).map_err(E::custom)?,
                    )
                    .map_err(E::custom)?,
                SubClockOperationWire::BackSample {
                    counter,
                    resolution,
                    span,
                } => clock
                    .then_back_sample(
                        counter,
                        resolution,
                        operation_provenance(span).map_err(E::custom)?,
                    )
                    .map_err(E::custom)?,
                SubClockOperationWire::NoClock { span } => {
                    clock.then_no_clock(operation_provenance(span).map_err(E::custom)?)
                }
            };
        }
        Ok(clock)
    }
}

fn operation_provenance(span: Span) -> Result<ProvenanceSpan, rumoca_core::MissingProvenanceSpan> {
    span.require_provenance("Flat sub-clock operation")
}

/// Clock interval representation.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ClockInterval {
    /// Interval in seconds with no exact rational form (inexact §16.3
    /// `Clock(interval)`); such a clock cannot anchor a rational lattice.
    Seconds(f64),
    /// Exact rational interval in seconds.
    Rational(ClockRational),
}

impl ClockInterval {
    /// The exact rational period, or the reason it has none.
    pub fn exact_period(&self) -> Result<ClockRational, ClockLatticeErrorKind> {
        match self {
            Self::Rational(period) => Ok(*period),
            Self::Seconds(_) => Err(ClockLatticeErrorKind::NotRationallyRepresentable),
        }
    }

    /// The interval in seconds; exact rationals round exactly once.
    pub fn seconds(&self) -> f64 {
        match self {
            Self::Rational(period) => period.to_f64(),
            Self::Seconds(seconds) => *seconds,
        }
    }
}

#[cfg(test)]
mod tests;
