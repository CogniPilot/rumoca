//! The component's resolved reading of the checked FMI event-indicator
//! inventory (SPEC_0044 ME-EVENT-001).
//!
//! [`FmiEventIndicatorInventory`](rumoca_ir_solve::fmi::FmiEventIndicatorInventory)
//! names the semantic owner of each FMI position. This module resolves those
//! owners once, against the runtime vectors the component actually reads, into
//! a positional table with no remaining lookup, search, or projection. Every
//! accessor takes `&self` and returns `Copy` data, and [`FmiIndicatorPlan`] has
//! exactly one constructor, so an indicator read cannot rebuild or re-project
//! the inventory.

use std::fmt;

use rumoca_ir_solve::{self as solve, ScalarSlot};

/// Where one FMI event-indicator position reads its scalar value.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum IndicatorReading {
    /// Position `index` of the runtime root-condition vector.
    RootValue { index: usize },
    /// Dynamic-time deadline `index`, reported relative to evaluation time.
    DeadlineDistance { index: usize },
}

/// The side an exact zero is reported on.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum IndicatorZeroSide {
    Positive,
    NonPositive,
    /// The side the previous completed point froze.
    Frozen,
}

/// One fully resolved FMI event-indicator position.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct IndicatorEntry {
    reading: IndicatorReading,
    zero_side: IndicatorZeroSide,
    crossing_root_index: Option<usize>,
}

impl IndicatorEntry {
    pub(crate) const fn reading(&self) -> IndicatorReading {
        self.reading
    }

    pub(crate) const fn zero_side(&self) -> IndicatorZeroSide {
        self.zero_side
    }

    /// The root-condition position a located crossing arms, absent for a
    /// dynamic-time deadline, whose event is owned by the time schedule.
    pub(crate) const fn crossing_root_index(&self) -> Option<usize> {
        self.crossing_root_index
    }
}

/// Why one inventory could not be resolved into a positional table.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) enum IndicatorPlanRejection {
    RootIndexOutOfRange,
    DeadlineIndexOutOfRange,
    DelayIndexOutOfRange,
    SourceKindOutOfOrder,
    SourceIndexNotAscending,
}

impl fmt::Display for IndicatorPlanRejection {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reason = match self {
            Self::RootIndexOutOfRange => "root-condition source is outside the runtime root vector",
            Self::DeadlineIndexOutOfRange => {
                "dynamic-time source is outside the deadline row block"
            }
            Self::DelayIndexOutOfRange => {
                "delay-discontinuity source is outside the runtime root vector"
            }
            Self::SourceKindOutOfOrder => "indicator sources are not in checked inventory order",
            Self::SourceIndexNotAscending => {
                "indicator sources of one kind are not strictly ascending"
            }
        };
        formatter.write_str(reason)
    }
}

/// One rejected inventory position.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(crate) struct IndicatorPlanError {
    position: usize,
    rejection: IndicatorPlanRejection,
}

impl fmt::Display for IndicatorPlanError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            formatter,
            "FMI event-indicator {} is unusable: {}",
            self.position, self.rejection
        )
    }
}

/// The runtime vectors an inventory resolves against.
#[derive(Clone, Copy)]
pub(crate) struct IndicatorPlanInputs<'model> {
    /// Length of the runtime root-condition vector: model roots then delays.
    pub(crate) root_value_count: usize,
    /// Length of the model root prefix of that vector.
    pub(crate) model_root_count: usize,
    /// Number of dynamic-time deadline rows.
    pub(crate) deadline_count: usize,
    pub(crate) root_zero_domains: &'model [solve::RootZeroDomain],
    pub(crate) root_relation_memory_targets: &'model [Option<ScalarSlot>],
}

/// The component-side FMI event-indicator table, built once.
#[derive(Debug)]
pub(crate) struct FmiIndicatorPlan {
    entries: Box<[IndicatorEntry]>,
    relation_memory_targets: Box<[Option<ScalarSlot>]>,
    root_value_len: usize,
    deadline_len: usize,
}

impl FmiIndicatorPlan {
    /// Resolve the checked inventory into positional readings.
    ///
    /// This is the only constructor. It refuses an inventory whose sources
    /// leave the runtime vectors or abandon the checked order, so every later
    /// read is a positional lookup that cannot fall back to a raw vector.
    pub(crate) fn derive(
        sources: &[solve::fmi::FmiEventIndicatorSource],
        inputs: IndicatorPlanInputs<'_>,
    ) -> Result<Self, IndicatorPlanError> {
        let mut entries = Vec::with_capacity(sources.len());
        let mut relation_memory_targets = Vec::with_capacity(sources.len());
        let mut previous_kind = 0u8;
        let mut previous_index: Option<usize> = None;
        let mut root_values_read = false;
        let mut deadlines_read = false;
        for (position, source) in sources.iter().enumerate() {
            let kind = source_kind_rank(*source);
            if kind < previous_kind {
                return Err(IndicatorPlanError {
                    position,
                    rejection: IndicatorPlanRejection::SourceKindOutOfOrder,
                });
            }
            if kind != previous_kind {
                previous_index = None;
                previous_kind = kind;
            }
            let index = source.source_index();
            if previous_index.is_some_and(|previous| previous >= index) {
                return Err(IndicatorPlanError {
                    position,
                    rejection: IndicatorPlanRejection::SourceIndexNotAscending,
                });
            }
            previous_index = Some(index);
            let (entry, target) = resolve_source(*source, position, inputs)?;
            match entry.reading {
                IndicatorReading::RootValue { .. } => root_values_read = true,
                IndicatorReading::DeadlineDistance { .. } => deadlines_read = true,
            }
            entries.push(entry);
            relation_memory_targets.push(target);
        }
        Ok(Self {
            entries: entries.into_boxed_slice(),
            relation_memory_targets: relation_memory_targets.into_boxed_slice(),
            root_value_len: if root_values_read {
                inputs.root_value_count
            } else {
                0
            },
            deadline_len: if deadlines_read {
                inputs.deadline_count
            } else {
                0
            },
        })
    }

    pub(crate) const fn len(&self) -> usize {
        self.entries.len()
    }

    pub(crate) const fn entries(&self) -> &[IndicatorEntry] {
        &self.entries
    }

    /// Positional relation-memory targets, in FMI indicator order.
    #[cfg(test)]
    pub(crate) const fn relation_memory_targets(&self) -> &[Option<ScalarSlot>] {
        &self.relation_memory_targets
    }

    /// Root-condition buffer length this plan reads, zero when it reads none.
    pub(crate) const fn root_value_len(&self) -> usize {
        self.root_value_len
    }

    /// Deadline buffer length this plan reads, zero when it reads none.
    pub(crate) const fn deadline_len(&self) -> usize {
        self.deadline_len
    }

    /// Whether any position reads the runtime root-condition vector.
    pub(crate) const fn reads_root_values(&self) -> bool {
        self.root_value_len != 0
    }

    /// Whether any position reads a dynamic-time deadline row.
    pub(crate) const fn reads_deadlines(&self) -> bool {
        self.deadline_len != 0
    }

    pub(crate) fn crossing_root_index(&self, position: usize) -> Option<usize> {
        self.entries
            .get(position)
            .and_then(IndicatorEntry::crossing_root_index)
    }

    pub(crate) fn relation_memory_target(&self, position: usize) -> Option<ScalarSlot> {
        self.relation_memory_targets
            .get(position)
            .copied()
            .flatten()
    }
}

const fn source_kind_rank(source: solve::fmi::FmiEventIndicatorSource) -> u8 {
    match source {
        solve::fmi::FmiEventIndicatorSource::RootCondition { .. } => 0,
        solve::fmi::FmiEventIndicatorSource::DynamicTimeEvent { .. } => 1,
        solve::fmi::FmiEventIndicatorSource::DelayDiscontinuity { .. } => 2,
    }
}

fn resolve_source(
    source: solve::fmi::FmiEventIndicatorSource,
    position: usize,
    inputs: IndicatorPlanInputs<'_>,
) -> Result<(IndicatorEntry, Option<ScalarSlot>), IndicatorPlanError> {
    match source {
        solve::fmi::FmiEventIndicatorSource::RootCondition { index } => {
            if index >= inputs.model_root_count || index >= inputs.root_value_count {
                return Err(IndicatorPlanError {
                    position,
                    rejection: IndicatorPlanRejection::RootIndexOutOfRange,
                });
            }
            let zero_side = match inputs.root_zero_domains.get(index) {
                Some(solve::RootZeroDomain::Positive) => IndicatorZeroSide::Positive,
                Some(solve::RootZeroDomain::NonPositive) => IndicatorZeroSide::NonPositive,
                Some(solve::RootZeroDomain::Previous) | None => IndicatorZeroSide::Frozen,
            };
            Ok((
                IndicatorEntry {
                    reading: IndicatorReading::RootValue { index },
                    zero_side,
                    crossing_root_index: Some(index),
                },
                inputs
                    .root_relation_memory_targets
                    .get(index)
                    .copied()
                    .flatten(),
            ))
        }
        solve::fmi::FmiEventIndicatorSource::DynamicTimeEvent { index } => {
            if index >= inputs.deadline_count {
                return Err(IndicatorPlanError {
                    position,
                    rejection: IndicatorPlanRejection::DeadlineIndexOutOfRange,
                });
            }
            Ok((
                IndicatorEntry {
                    reading: IndicatorReading::DeadlineDistance { index },
                    zero_side: IndicatorZeroSide::Frozen,
                    crossing_root_index: None,
                },
                None,
            ))
        }
        solve::fmi::FmiEventIndicatorSource::DelayDiscontinuity { index } => {
            let Some(root_index) = inputs.model_root_count.checked_add(index) else {
                return Err(IndicatorPlanError {
                    position,
                    rejection: IndicatorPlanRejection::DelayIndexOutOfRange,
                });
            };
            if root_index >= inputs.root_value_count {
                return Err(IndicatorPlanError {
                    position,
                    rejection: IndicatorPlanRejection::DelayIndexOutOfRange,
                });
            }
            Ok((
                IndicatorEntry {
                    reading: IndicatorReading::RootValue { index: root_index },
                    zero_side: IndicatorZeroSide::Frozen,
                    crossing_root_index: Some(root_index),
                },
                None,
            ))
        }
    }
}

#[cfg(test)]
mod tests;
