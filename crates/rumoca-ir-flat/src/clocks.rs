//! Clock and synchronous structures (MLS §16).
//!
//! This module provides data structures for synchronous language elements:
//! - Clock types and definitions
//! - Base-clock partitions
//! - Sub-clock partitions
//! - Clock inference data

use indexmap::{IndexMap, IndexSet};
use serde::{Deserialize, Serialize};

use crate::{Equation, VarName};

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
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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
            ..Default::default()
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
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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
            ..Default::default()
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
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BaseClock {
    /// The kind of clock.
    pub kind: ClockKind,
    /// Inferred base interval (for rational clocks).
    pub base_interval: Option<ClockInterval>,
}

impl Default for BaseClock {
    fn default() -> Self {
        Self {
            kind: ClockKind::Inferred,
            base_interval: None,
        }
    }
}

impl BaseClock {
    /// Create a new inferred clock.
    pub fn inferred() -> Self {
        Self::default()
    }

    /// Create a new periodic clock.
    pub fn periodic(interval: f64) -> Self {
        Self {
            kind: ClockKind::Periodic { interval },
            base_interval: Some(ClockInterval::Seconds(interval)),
        }
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
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SubClock {
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
    /// Create a sub-sampled clock.
    pub fn sub_sample(factor: i64) -> Self {
        Self {
            sub_factor: Some(factor),
            ..Default::default()
        }
    }

    /// Create a super-sampled clock.
    pub fn super_sample(factor: i64) -> Self {
        Self {
            super_factor: Some(factor),
            ..Default::default()
        }
    }
}

/// Clock interval representation.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum ClockInterval {
    /// Interval in seconds.
    Seconds(f64),
    /// Rational interval as (numerator, denominator).
    Rational { num: i64, denom: i64 },
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
