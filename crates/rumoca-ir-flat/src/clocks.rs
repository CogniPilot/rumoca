//! Clock and synchronous structures (MLS §16).
//!
//! This module provides data structures for synchronous language elements:
//! - Clock types and definitions
//! - Base-clock partitions
//! - Sub-clock partitions
//! - Clock inference data

use indexmap::{IndexMap, IndexSet};
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
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ClockPartitions {
    /// Base-clock partitions (execute asynchronously from each other).
    pub base_partitions: Vec<BaseClockPartition>,
    /// The continuous-time partition (if any).
    /// This contains equations without explicit clock associations.
    pub continuous_partition: Option<ContinuousPartition>,
}

impl ClockPartitions {
    /// Create a new empty clock partitions structure.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a base-clock partition.
    pub fn add_base_partition(&mut self, partition: BaseClockPartition) {
        self.base_partitions.push(partition);
    }

    /// Get the number of base partitions.
    pub fn num_base_partitions(&self) -> usize {
        self.base_partitions.len()
    }
}

/// MLS §16.7: Base-Clock Partition.
///
/// "A set of equations and variables which must be executed together in one task."
/// Different base-partitions can execute asynchronously with respect to each other.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BaseClockPartition {
    /// Unique identifier for this partition.
    pub id: u32,
    /// The base clock for this partition.
    pub clock: BaseClock,
    /// Variables in this partition.
    pub variables: IndexSet<VarName>,
    /// Equations in this partition.
    pub equations: Vec<Equation>,
    /// Sub-clock partitions within this base partition.
    pub sub_partitions: Vec<SubClockPartition>,
    /// Whether this is a discretized partition (contains der(), delay(), etc.).
    /// MLS §16.8.1: "If the partition contains a Discretized Variables, it is called discretized."
    pub is_discretized: bool,
}

impl BaseClockPartition {
    /// Create a new base-clock partition.
    pub fn new(id: u32, clock: BaseClock) -> Self {
        Self {
            id,
            clock,
            variables: IndexSet::new(),
            equations: Vec::new(),
            sub_partitions: Vec::new(),
            is_discretized: false,
        }
    }

    /// Add a variable to this partition.
    pub fn add_variable(&mut self, name: VarName) {
        self.variables.insert(name);
    }

    /// Add an equation to this partition.
    pub fn add_equation(&mut self, eq: Equation) {
        self.equations.push(eq);
    }

    /// Add a sub-clock partition.
    pub fn add_sub_partition(&mut self, sub: SubClockPartition) {
        self.sub_partitions.push(sub);
    }
}

/// MLS §16.7: Sub-Clock Partition.
///
/// "A subset of equations and variables of a base-partition which are
/// partially synchronized with other sub-partitions of the same base-partition."
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SubClockPartition {
    /// Unique identifier within the base partition.
    pub id: u32,
    /// The sub-clock definition.
    pub sub_clock: SubClock,
    /// Variables in this sub-partition.
    pub variables: IndexSet<VarName>,
    /// Equations in this sub-partition.
    pub equations: Vec<Equation>,
}

impl SubClockPartition {
    /// Create a new sub-clock partition.
    pub fn new(id: u32, sub_clock: SubClock) -> Self {
        Self {
            id,
            sub_clock,
            variables: IndexSet::new(),
            equations: Vec::new(),
        }
    }
}

/// The continuous-time partition (non-clocked equations).
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ContinuousPartition {
    /// Variables in the continuous partition.
    pub variables: IndexSet<VarName>,
    /// Equations in the continuous partition.
    pub equations: Vec<Equation>,
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

// =============================================================================
// Clock Variable Association (MLS §16.2.1)
// =============================================================================

/// MLS §16.2.1: Clock association for variables.
///
/// "Every clocked variable associates uniquely with exactly one clock."
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ClockAssociations {
    /// Map from variable name to its associated clock partition.
    pub variable_to_partition: IndexMap<VarName, (u32, Option<u32>)>,
}

impl ClockAssociations {
    /// Create a new empty associations map.
    pub fn new() -> Self {
        Self::default()
    }

    /// Associate a variable with a partition.
    /// `base_id` is the base partition ID, `sub_id` is the optional sub-partition ID.
    pub fn associate(&mut self, var: VarName, base_id: u32, sub_id: Option<u32>) {
        self.variable_to_partition.insert(var, (base_id, sub_id));
    }

    /// Get the partition for a variable.
    pub fn get(&self, var: &VarName) -> Option<(u32, Option<u32>)> {
        self.variable_to_partition.get(var).copied()
    }
}

#[cfg(test)]
mod tests;
