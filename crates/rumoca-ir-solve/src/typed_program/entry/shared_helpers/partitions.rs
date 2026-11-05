//! One entry and one partition per reachable source-bound helper body.
//!
//! Planning walks each exact body once, never expands a call into its caller,
//! and retains every authored call occurrence with its own arguments, results
//! and assertion forwarding. Primal and directional bodies stay distinct. A
//! recursive call is an edge to an existing partition, not another traversal.
//! These are compiler-side call boundaries, not a target cost, numerical graph,
//! storage-reuse policy or permission to memoize an invocation's runtime values.

use std::collections::HashMap;

use super::{EntryCallOccurrence, EntryCallOccurrences, SharedHelperProgramEntry};
use crate::{
    BoundSolvePureCallBody, SolveProgramConstructionError, SolveStoragePlan, TypedProgram,
};

#[derive(Clone, Copy)]
enum Source<'entry> {
    Entry(&'entry SharedHelperProgramEntry),
    Helper(BoundSolvePureCallBody<'entry>),
}

impl<'entry> Source<'entry> {
    fn program(self) -> &'entry TypedProgram {
        match self {
            Self::Entry(entry) => entry.body(),
            Self::Helper(helper) => helper.body(),
        }
    }

    fn calls(self) -> EntryCallOccurrences<'entry> {
        match self {
            Self::Entry(entry) => entry.call_occurrences(),
            Self::Helper(helper) => helper.call_occurrences(),
        }
    }
}

/// One exact call site and its constructor-selected helper partition.
/// A target ordinal has meaning only within the original entry's plan;
/// [`SharedEntryCallPlan::resolve`] retains that nominal-owner check.
pub struct EntryPartitionCall<'entry> {
    entry: &'entry SharedHelperProgramEntry,
    occurrence: EntryCallOccurrence<'entry>,
    target: usize,
}

impl<'entry> EntryPartitionCall<'entry> {
    #[must_use]
    pub fn occurrence(&self) -> EntryCallOccurrence<'entry> {
        self.occurrence
    }
}

/// An immutable source body and its exact call occurrences in source order.
/// Nested structured regions remain in that body; their call sites retain the
/// original local program/register scope rather than becoming eager calls.
pub struct EntryCallPartition<'entry> {
    source: Source<'entry>,
    calls: Box<[EntryPartitionCall<'entry>]>,
}

impl<'entry> EntryCallPartition<'entry> {
    #[must_use]
    pub fn program(&self) -> &'entry TypedProgram {
        self.source.program()
    }

    /// The original program's existing placement, not a second storage planner.
    #[must_use]
    pub fn storage_plan(&self) -> &'entry SolveStoragePlan {
        self.program().storage_plan()
    }

    /// `None` for the entry partition, otherwise its exact source-bound body.
    #[must_use]
    pub fn helper(&self) -> Option<BoundSolvePureCallBody<'entry>> {
        match self.source {
            Source::Entry(_) => None,
            Source::Helper(helper) => Some(helper),
        }
    }

    #[must_use]
    pub fn calls(&self) -> &[EntryPartitionCall<'entry>] {
        &self.calls
    }
}

/// Borrowed compiler construction product: entry first, then reachable helper
/// bodies in deterministic first-discovery order. No caller supplies a body,
/// namespace, partition list or target ordinal. Nothing is serialized.
///
/// A consumer compiles these bodies once and follows the issued call bindings;
/// execution still invokes every reached source call, including repeated calls
/// to a shared helper. The source recursion policy remains authoritative.
pub struct SharedEntryCallPlan<'entry> {
    entry: &'entry SharedHelperProgramEntry,
    partitions: Box<[EntryCallPartition<'entry>]>,
    entry_calls: HashMap<(usize, usize), usize>,
}

fn helper_partition<'entry>(
    helpers: &mut HashMap<BoundSolvePureCallBody<'entry>, usize>,
    partitions: &mut Vec<EntryCallPartition<'entry>>,
    helper: BoundSolvePureCallBody<'entry>,
) -> usize {
    *helpers.entry(helper).or_insert_with(|| {
        let target = partitions.len();
        partitions.push(EntryCallPartition {
            source: Source::Helper(helper),
            calls: Box::new([]),
        });
        target
    })
}

fn record_entry_call(
    entry_calls: &mut HashMap<(usize, usize), usize>,
    cursor: usize,
    occurrence: EntryCallOccurrence<'_>,
    call_index: usize,
) {
    if cursor == 0 {
        let key = (
            std::ptr::from_ref(occurrence.program()).addr(),
            occurrence.operation_index(),
        );
        entry_calls.entry(key).or_insert(call_index);
    }
}

impl SharedHelperProgramEntry {
    /// Issue compiler call partitions from this exact complete entry. This
    /// planning step runs before target preparation, never during evaluation.
    pub fn partition_calls(
        &self,
    ) -> Result<SharedEntryCallPlan<'_>, SolveProgramConstructionError> {
        let mut partitions = vec![EntryCallPartition {
            source: Source::Entry(self),
            calls: Box::new([]),
        }];
        // This map is lookup-only: partition order comes from the source walk.
        let mut helpers = HashMap::new();
        let mut entry_calls = HashMap::new();
        let mut cursor = 0;
        while cursor < partitions.len() {
            let source = partitions[cursor].source;
            let mut calls = Vec::new();
            for occurrence in source.calls() {
                let occurrence = occurrence?;
                record_entry_call(&mut entry_calls, cursor, occurrence, calls.len());
                let helper = occurrence.callee();
                let target = helper_partition(&mut helpers, &mut partitions, helper);
                calls.push(EntryPartitionCall {
                    entry: self,
                    occurrence,
                    target,
                });
            }
            partitions[cursor].calls = calls.into_boxed_slice();
            cursor += 1;
        }
        Ok(SharedEntryCallPlan {
            entry: self,
            partitions: partitions.into_boxed_slice(),
            entry_calls,
        })
    }
}

impl<'entry> SharedEntryCallPlan<'entry> {
    /// Lookup of an actual root or nested-region call by nominal local scope.
    /// This index is built with the source walk, never by an evaluator. Its
    /// process-local addresses are lookup keys only; source borrows retain the
    /// actual program and no pointer is dereferenced or serialized.
    #[must_use]
    pub fn entry_call_at(
        &self,
        program: &TypedProgram,
        operation_index: usize,
    ) -> Option<&EntryPartitionCall<'entry>> {
        let key = (std::ptr::from_ref(program).addr(), operation_index);
        let index = *self.entry_calls.get(&key)?;
        let call = self.partitions.first()?.calls.get(index)?;
        let occurrence = call.occurrence();
        (std::ptr::eq(occurrence.program(), program)
            && occurrence.operation_index() == operation_index)
            .then_some(call)
    }

    #[must_use]
    pub fn entry(&self) -> &'entry SharedHelperProgramEntry {
        self.entry
    }

    #[must_use]
    pub fn partitions(&self) -> &[EntryCallPartition<'entry>] {
        &self.partitions
    }

    /// Resolve only a call issued for the same nominal entry. An independently
    /// constructed entry cannot substitute its call even with equal helpers.
    pub fn resolve(
        &self,
        call: &EntryPartitionCall<'_>,
    ) -> Result<&EntryCallPartition<'entry>, SolveProgramConstructionError> {
        let refused = || SolveProgramConstructionError::UnknownCallOwner {
            provenance: call.occurrence.operation().provenance(),
        };
        if !self.entry.shares_entry_owner(call.entry) {
            return Err(refused());
        }
        self.partitions.get(call.target).ok_or_else(refused)
    }
}
