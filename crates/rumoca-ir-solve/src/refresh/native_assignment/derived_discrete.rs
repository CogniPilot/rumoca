//! Stateless derived-discrete outputs of a native value schedule.
//!
//! A host-driven native evaluation has no event iteration: every call is one
//! instant at which the inputs hold their written values. A discrete row is
//! admitted into the schedule only when that instant fully determines it: an
//! always-active, unclocked B.1c equation that reads no history (`pre`,
//! `previous`, `initial()`), owns no relation memory, and belongs to a problem
//! with no events, clocks, states or initialization owners (MLS 3.7 Appendix B
//! then settles in one causal pass). Each admitted row is a derived-discrete
//! output: its value is computed in schedule order into a private work slot
//! that every later reader binds to, and it is published through a typed
//! output lane of its declared kind. Its Solve P slot is never read by the
//! schedule and never published.

mod program_output;

use program_output::integer_source;
pub(super) use program_output::row_program;
use std::collections::BTreeMap;

use super::*;
use crate::{
    DiscreteRowRole, EventIterationOwner, ScalarProgramBlock, ScalarSlot, SolveProblem,
    SolveScalarType, SolveVariableValueKind,
};

/// Why a problem has no stateless native value schedule. Each reason names
/// the semantic feature that needs an event, history or storage owner the
/// stateless evaluation does not have.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NativeEvaluationRefusal {
    ContinuousStates,
    InitializationEquations,
    Clocks,
    /// `delay` history or a terminal event.
    RuntimeEvents,
    /// A relation outside `noEvent` generates state events (MLS 3.7 §8.5).
    Relations,
    TimeEvents,
    EventActions,
    EventAlgorithms,
    /// Relation-driven or post-event runtime assignments.
    RuntimeAssignments,
    StructuredDiscreteOwners,
    GuardedAssignments,
    /// A `pre`, `previous` or `initial()` read.
    HistoryReads,
    /// A discrete row that is not a B.1c equation, such as condition memory.
    NonEquationDiscreteRow,
    /// A discrete row without exactly one scalar storage owner.
    UnownedDiscreteRow,
    /// A discrete program that stores more than one output.
    MultiOutputDiscreteProgram,
    /// An enumeration has no typed output lane yet.
    EnumerationOutput,
    /// A String has no numeric storage.
    StringOutput,
    /// An Integer row whose value is computed by Real register arithmetic has
    /// no exact Integer source to publish.
    IntegerComputedInReal,
    /// Program registers are Real; an Integer output read by a later stage
    /// would round above 2^53.
    IntegerReaderRequiresTypedRegisters,
    /// An indexed, tensor or compact family read of a derived output.
    DerivedOutputCompactRead,
    /// An operation whose register reads cannot be computed while a
    /// register holds an Integer input view or Integer call result, so no
    /// exact binding or check can be proven for it.
    UnresolvedTypedRead,
}

impl std::fmt::Display for NativeEvaluationRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::ContinuousStates => "native evaluation has no continuous states",
            Self::InitializationEquations => "native evaluation has no initialization equations",
            Self::Clocks => "native evaluation has no clock partitions",
            Self::RuntimeEvents => "native evaluation has no delay history or terminal event",
            Self::Relations => {
                "a relation outside noEvent generates events (MLS 3.7 §8.5), which native evaluation does not own"
            }
            Self::TimeEvents => "native evaluation has no time events",
            Self::EventActions => "native evaluation has no event actions",
            Self::EventAlgorithms => "native evaluation has no event algorithms",
            Self::RuntimeAssignments => "native evaluation has no event-driven runtime assignments",
            Self::StructuredDiscreteOwners => {
                "native evaluation has no structured discrete owners"
            }
            Self::GuardedAssignments => "native evaluation has no guarded discrete assignments",
            Self::HistoryReads => {
                "native evaluation has no pre, previous or initial() history"
            }
            Self::NonEquationDiscreteRow => {
                "native evaluation admits only discrete equations, not event memories or actions"
            }
            Self::UnownedDiscreteRow => "a discrete row has no unique scalar storage owner",
            Self::MultiOutputDiscreteProgram => {
                "a discrete program stores more than one output"
            }
            Self::EnumerationOutput => "an enumeration output has no typed native lane",
            Self::StringOutput => "a String output has no numeric storage",
            Self::IntegerComputedInReal => {
                "an Integer output computed by Real register arithmetic has no exact Integer source"
            }
            Self::IntegerReaderRequiresTypedRegisters => {
                "an Integer output read by a later stage requires typed program registers"
            }
            Self::DerivedOutputCompactRead => {
                "a derived discrete output is read through an indexed, tensor or compact family load"
            }
            Self::UnresolvedTypedRead => {
                "an operation with no computable read set may read an Integer value held in a Real register"
            }
        })
    }
}

impl std::error::Error for NativeEvaluationRefusal {}

/// Typed host lane of one derived output.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NativeOutputLane {
    /// Binary64.
    Real,
    /// Exact signed 64-bit Integer.
    Integer,
    /// One byte holding 0 or 1.
    Boolean,
}

impl NativeOutputLane {
    #[must_use]
    pub const fn width(self) -> usize {
        match self {
            Self::Real | Self::Integer => 8,
            Self::Boolean => 1,
        }
    }

    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::Real => "f64",
            Self::Integer => "i64",
            Self::Boolean => "u8",
        }
    }
}

/// Where a published Integer value comes from without a Real register hop.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NativeIntegerSource {
    /// Output cell `cell` (in scalar order) of the pure call at operation
    /// position `operation` of the stage program.
    CallCell {
        operation: usize,
        cell: u32,
    },
    Literal(i64),
    /// The typed Integer input lane at `lane_offset` bytes of the typed lane
    /// buffer (SPEC_0040 SOLVE-C69).
    Input {
        lane_offset: usize,
    },
}

/// One derived-discrete output: Solve discrete row `row` computed into private
/// work slot `work_index` and published at `lane_offset` bytes of the typed
/// output buffer.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NativeDerivedOutput {
    pub(super) row: usize,
    pub(super) p_index: usize,
    pub(super) work_index: usize,
    pub(super) lane: NativeOutputLane,
    pub(super) lane_offset: usize,
    pub(super) integer_source: Option<NativeIntegerSource>,
}

impl NativeDerivedOutput {
    #[must_use]
    pub fn row(&self) -> usize {
        self.row
    }
    /// The Solve P slot that the stateless evaluation neither reads nor publishes.
    #[must_use]
    pub fn p_index(&self) -> usize {
        self.p_index
    }
    #[must_use]
    pub fn work_index(&self) -> usize {
        self.work_index
    }
    #[must_use]
    pub fn lane(&self) -> NativeOutputLane {
        self.lane
    }
    #[must_use]
    pub fn lane_offset(&self) -> usize {
        self.lane_offset
    }
    #[must_use]
    pub fn integer_source(&self) -> Option<NativeIntegerSource> {
        self.integer_source
    }
}

/// The classified discrete rows of one problem, before stage derivation.
pub(super) struct DerivedDiscrete {
    pub(super) outputs: Vec<NativeDerivedOutput>,
    /// Solve P slot to private work slot, for every derived output.
    pub(super) rebinding: BTreeMap<usize, usize>,
    pub(super) lane_bytes: usize,
}

/// Classify every discrete and event owner of `problem`; derived outputs are
/// numbered after the `y` solver coordinates in canonical program/store order;
/// their lanes remain P-slot ordered, packed 8-byte values first, then bytes.
pub(super) fn classify(
    problem: &SolveProblem,
    inputs: &super::typed_inputs::TypedInputs,
) -> Result<DerivedDiscrete, NativeEvaluationRefusal> {
    refuse_event_owners(problem)?;
    let discrete = &problem.discrete;
    let rhs = &discrete.rhs;
    let owners = scalar_row_owners(problem)?;
    let mut rows = Vec::with_capacity(rhs.row_count());
    for row in 0..discrete.update_targets.len() {
        if discrete.row_roles.get(row) != Some(&DiscreteRowRole::Equation) {
            return Err(NativeEvaluationRefusal::NonEquationDiscreteRow);
        }
        let Some(ScalarSlot::P { index, .. }) = discrete.update_targets.get(row).copied() else {
            return Err(NativeEvaluationRefusal::UnownedDiscreteRow);
        };
        let kind = owners
            .get(&row)
            .copied()
            .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
        let lane = match kind {
            SolveVariableValueKind::Real => NativeOutputLane::Real,
            SolveVariableValueKind::Integer => NativeOutputLane::Integer,
            SolveVariableValueKind::Boolean => NativeOutputLane::Boolean,
            SolveVariableValueKind::Enumeration => {
                return Err(NativeEvaluationRefusal::EnumerationOutput);
            }
            SolveVariableValueKind::String => return Err(NativeEvaluationRefusal::StringOutput),
        };
        let integer_source = match lane {
            NativeOutputLane::Integer => Some(integer_source(rhs, row, inputs)?),
            NativeOutputLane::Real | NativeOutputLane::Boolean => {
                rhs.output_position(row)
                    .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
                None
            }
        };
        rows.push((index, row, lane, integer_source));
    }
    rows.sort_unstable_by_key(|&(index, ..)| index);
    let y = problem.layout.y_scalars();
    // Private work follows canonical program/store order; publication lanes
    // independently retain P-slot order, including interleaved record kinds.
    let mut work_indices = vec![usize::MAX; rows.len()];
    for (ordinal, binding) in rhs.output_bindings().enumerate() {
        let index = y
            .checked_add(ordinal)
            .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
        let slot = work_indices
            .get_mut(binding.logical_index)
            .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
        if *slot != usize::MAX {
            return Err(NativeEvaluationRefusal::UnownedDiscreteRow);
        }
        *slot = index;
    }
    let mut rebinding = BTreeMap::new();
    let mut outputs = Vec::with_capacity(rows.len());
    for (p_index, row, lane, integer_source) in rows {
        let work_index = *work_indices
            .get(row)
            .filter(|&&index| index != usize::MAX)
            .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
        if rebinding.insert(p_index, work_index).is_some() {
            return Err(NativeEvaluationRefusal::UnownedDiscreteRow);
        }
        outputs.push(NativeDerivedOutput {
            row,
            p_index,
            work_index,
            lane,
            lane_offset: 0,
            integer_source,
        });
    }
    let mut lane_bytes = 0usize;
    for wide in [true, false] {
        for output in outputs
            .iter_mut()
            .filter(|output| (output.lane.width() == 8) == wide)
        {
            output.lane_offset = lane_bytes;
            lane_bytes += output.lane.width();
        }
    }
    outputs.sort_unstable_by_key(|output| output.work_index);
    Ok(DerivedDiscrete {
        outputs,
        rebinding,
        lane_bytes,
    })
}

/// The constructor orders the immutable inventory by unique private work slot.
/// A borrowed query visits only the publication outputs a stage owns.
pub(super) fn work_range(
    outputs: &[NativeDerivedOutput],
    targets: std::ops::Range<usize>,
) -> &[NativeDerivedOutput] {
    let start = outputs.partition_point(|output| output.work_index < targets.start);
    let end = outputs.partition_point(|output| output.work_index < targets.end);
    &outputs[start..end.max(start)]
}

fn refuse_event_owners(problem: &SolveProblem) -> Result<(), NativeEvaluationRefusal> {
    use NativeEvaluationRefusal as R;
    let layout = &problem.solve_layout;
    let discrete = &problem.discrete;
    let events = &problem.events;
    let checks = [
        (layout.state_scalar_count() != 0, R::ContinuousStates),
        (
            crate::solve_has_initialization(problem),
            R::InitializationEquations,
        ),
        (crate::solve_has_clocks(problem), R::Clocks),
        (crate::solve_has_runtime_events(problem), R::RuntimeEvents),
        (
            !events.root_conditions.is_empty()
                || !events.condition_memory_parameter_indices.is_empty()
                || !layout.relation_memory_parameter_indices.is_empty(),
            R::Relations,
        ),
        (
            !events.scheduled_root_conditions.is_empty()
                || !events.scheduled_time_events.is_empty()
                || !events.dynamic_time_event_names.is_empty()
                || !events.dynamic_time_event_rhs.is_empty(),
            R::TimeEvents,
        ),
        (
            !events.action_conditions.is_empty() || !events.actions.is_empty(),
            R::EventActions,
        ),
        (!discrete.event_transactions.is_empty(), R::EventAlgorithms),
        (
            !discrete.runtime_assignment_rhs.is_empty()
                || !discrete.post_commit_assignment_rhs.is_empty(),
            R::RuntimeAssignments,
        ),
        (
            !discrete.structured_rhs.is_empty() || !discrete.structured_updates.is_empty(),
            R::StructuredDiscreteOwners,
        ),
        (
            !discrete.guarded_assignments.is_empty(),
            R::GuardedAssignments,
        ),
    ];
    if let Some((_, refusal)) = checks.into_iter().find(|(present, _)| *present) {
        return Err(refusal);
    }
    let history = layout
        .pre_param_bindings
        .iter()
        .map(|binding| binding.dest_p_index)
        .chain(layout.initial_event_parameter_index)
        .chain(layout.terminal_event_parameter_index)
        .collect::<Vec<_>>();
    let reads = crate::read_parameter_slots(problem);
    if history.iter().any(|slot| reads.contains(slot)) {
        return Err(R::HistoryReads);
    }
    Ok(())
}

/// Each discrete row's value kind, from the issued scalar-row iteration owners.
fn scalar_row_owners(
    problem: &SolveProblem,
) -> Result<BTreeMap<usize, SolveVariableValueKind>, NativeEvaluationRefusal> {
    let mut owners = BTreeMap::new();
    for run in &problem.discrete.event_iteration_plan.runs {
        let EventIterationOwner::ScalarRows { start_row } = run.owner else {
            return Err(NativeEvaluationRefusal::UnownedDiscreteRow);
        };
        let storage = problem
            .solve_layout
            .variable_storage_runs
            .get(run.variable)
            .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
        for row in start_row..start_row + storage.scalar_count {
            if owners.insert(row, storage.value_kind).is_some() {
                return Err(NativeEvaluationRefusal::UnownedDiscreteRow);
            }
        }
    }
    Ok(owners)
}
