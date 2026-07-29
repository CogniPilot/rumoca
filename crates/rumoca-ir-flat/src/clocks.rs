//! Clock and synchronous structures (MLS §16).
//!
//! This module provides data structures for synchronous language elements:
//! - Clock types and definitions
//! - Base-clock partitions
//! - Sub-clock partitions
//! - Clock inference data

use indexmap::{IndexMap, IndexSet};
use rumoca_core::{ClockLattice, ClockLatticeError, ClockLatticeErrorKind, ClockRational, Span};
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SubClock {
    /// Source span for the sub-clock expression.
    pub source_span: Span,
    /// Sub-sampling factor (subSample).
    /// The sub-clock ticks every `sub_factor` ticks of the base clock.
    pub sub_factor: Option<i64>,
    /// Super-sampling factor (superSample).
    /// The sub-clock ticks `super_factor` times per base clock tick.
    pub super_factor: Option<i64>,
    /// Shift amount (shiftSample).
    /// Number of base clock ticks to shift.
    pub shift_counter: Option<i64>,
    /// Shift resolution.
    pub shift_resolution: Option<i64>,
    /// Back-sample amount (backSample).
    pub back_counter: Option<i64>,
    /// Back-sample resolution.
    pub back_resolution: Option<i64>,
    /// Whether noClock() was used.
    pub no_clock: bool,
}

impl SubClock {
    pub fn empty_with_span(source_span: Span) -> Self {
        Self {
            source_span,
            sub_factor: None,
            super_factor: None,
            shift_counter: None,
            shift_resolution: None,
            back_counter: None,
            back_resolution: None,
            no_clock: bool::default(),
        }
    }

    /// Create a sub-sampled clock.
    pub fn sub_sample(factor: i64, source_span: Span) -> Self {
        Self {
            sub_factor: Some(factor),
            ..Self::empty_with_span(source_span)
        }
    }

    /// Create a super-sampled clock.
    pub fn super_sample(factor: i64, source_span: Span) -> Self {
        Self {
            super_factor: Some(factor),
            ..Self::empty_with_span(source_span)
        }
    }

    /// Create a shifted clock (MLS §16.5.2 `shiftSample`).
    pub fn shift_sample(counter: i64, resolution: i64, source_span: Span) -> Self {
        Self {
            shift_counter: Some(counter),
            shift_resolution: Some(resolution),
            ..Self::empty_with_span(source_span)
        }
    }

    /// Create a back-shifted clock (MLS §16.5.2 `backSample`).
    pub fn back_sample(counter: i64, resolution: i64, source_span: Span) -> Self {
        Self {
            back_counter: Some(counter),
            back_resolution: Some(resolution),
            ..Self::empty_with_span(source_span)
        }
    }

    /// Derive this sub-clock's exact lattice from its base clock.
    ///
    /// MLS §16.5.2 defines all four conversions as integer relations over the
    /// source clock, so the derivation is exact integer arithmetic and never a
    /// floating-point scaling. `noClock()` has no periodic lattice.
    pub fn derive(&self, base: ClockLattice) -> ClockLatticeResult<ClockLattice> {
        if self.no_clock {
            return Err(ClockLatticeErrorKind::NotRationallyRepresentable.at(self.source_span));
        }
        let mut derived = base;
        if let Some(factor) = self.sub_factor {
            derived = self.apply(derived.sub_sample(factor))?;
        }
        if let Some(factor) = self.super_factor {
            derived = self.apply(derived.super_sample(factor))?;
        }
        // MLS §16.5.2 declares `resolution = 1` as the operator's own default,
        // so an absent resolution is the spec value rather than a substitute.
        if let Some(counter) = self.shift_counter {
            let resolution = self.shift_resolution.unwrap_or(1);
            derived = self.apply(derived.shift_sample(counter, resolution))?;
        }
        if let Some(counter) = self.back_counter {
            let resolution = self.back_resolution.unwrap_or(1);
            derived = self.apply(derived.back_sample(counter, resolution))?;
        }
        Ok(derived)
    }

    fn apply(
        &self,
        result: Result<ClockLattice, ClockLatticeErrorKind>,
    ) -> ClockLatticeResult<ClockLattice> {
        result.map_err(|kind| kind.at(self.source_span))
    }
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
