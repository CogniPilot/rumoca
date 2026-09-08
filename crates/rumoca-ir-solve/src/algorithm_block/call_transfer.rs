//! Checked aggregate call-result placement.
//!
//! This module owns target-neutral placement only. A placement proof grants no
//! target-specific spelling or prepared-product capability.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::num::NonZeroU64;
use std::sync::Arc;

use crate::SolveValueType;
use rumoca_core::row_major_strides;

/// Compiler-issued identity of one exact invocation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallInvocationKey(NonZeroU64);

impl CallInvocationKey {
    #[cfg(test)]
    #[must_use]
    const fn issued(value: NonZeroU64) -> Self {
        Self(value)
    }
}

/// Compiler-issued identity of one exact result declaration.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallResultKey(NonZeroU64);

impl CallResultKey {
    #[cfg(test)]
    #[must_use]
    const fn issued(value: NonZeroU64) -> Self {
        Self(value)
    }
}

/// Exact correlation of an invocation with one of its result declarations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CallTransferKey {
    invocation: CallInvocationKey,
    result: CallResultKey,
}

impl CallTransferKey {
    #[cfg(test)]
    #[must_use]
    const fn issued(invocation: CallInvocationKey, result: CallResultKey) -> Self {
        Self { invocation, result }
    }

    #[must_use]
    pub const fn invocation(self) -> CallInvocationKey {
        self.invocation
    }

    #[must_use]
    pub const fn result(self) -> CallResultKey {
        self.result
    }
}

#[derive(Debug, PartialEq, Eq)]
struct CallTransferSubjectData {
    key: CallTransferKey,
    result_ordinal: u32,
    value_type: SolveValueType,
}

/// Exact ordered result projection expected from one invocation.
///
/// Cloning this handle is O(1); the aggregate type and shape remain owned once.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CallTransferSubject(Arc<CallTransferSubjectData>);

impl CallTransferSubject {
    #[cfg(test)]
    #[must_use]
    fn issued(key: CallTransferKey, result_ordinal: u32, value_type: SolveValueType) -> Self {
        Self(Arc::new(CallTransferSubjectData {
            key,
            result_ordinal,
            value_type,
        }))
    }

    #[must_use]
    pub fn key(&self) -> CallTransferKey {
        self.0.key
    }

    #[must_use]
    pub fn result_ordinal(&self) -> u32 {
        self.0.result_ordinal
    }

    #[must_use]
    pub fn value_type(&self) -> &SolveValueType {
        &self.0.value_type
    }
}

/// Compiler-issued identity of one target-neutral storage owner.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StorageOwnerKey(NonZeroU64);

impl StorageOwnerKey {
    #[cfg(test)]
    #[must_use]
    const fn issued(value: NonZeroU64) -> Self {
        Self(value)
    }
}

/// Checked half-open scalar range inside one storage owner.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct StorageRange {
    start: u64,
    end: u64,
}

impl StorageRange {
    pub fn construct(start: u64, scalar_count: u64) -> Result<Self, CallTransferConstructionError> {
        let end = start.checked_add(scalar_count).ok_or(
            CallTransferConstructionError::StorageRangeOverflow {
                start,
                scalar_count,
            },
        )?;
        Ok(Self { start, end })
    }

    #[must_use]
    pub const fn start(self) -> u64 {
        self.start
    }

    #[must_use]
    pub const fn end(self) -> u64 {
        self.end
    }

    #[must_use]
    pub const fn scalar_count(self) -> u64 {
        self.end - self.start
    }

    #[must_use]
    pub const fn is_empty(self) -> bool {
        self.start == self.end
    }

    #[must_use]
    const fn overlaps(self, other: Self) -> bool {
        !self.is_empty() && !other.is_empty() && self.start < other.end && other.start < self.end
    }
}

#[derive(Debug, PartialEq, Eq)]
struct StorageViewData {
    owner: StorageOwnerKey,
    range: StorageRange,
    value_type: SolveValueType,
    row_major_strides: Box<[u64]>,
}

/// One axis of a compact row-major traversal.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct StorageTraversalAxis {
    extent: u32,
    scalar_stride: u64,
}

impl StorageTraversalAxis {
    #[must_use]
    pub const fn extent(self) -> u32 {
        self.extent
    }

    #[must_use]
    pub const fn scalar_stride(self) -> u64 {
        self.scalar_stride
    }
}

/// Compact contiguous aggregate view with a checked row-major traversal.
///
/// No coordinate list is stored. Empty tensors use one canonical all-zero
/// stride vector because their traversal executes no domain point.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StorageView(Arc<StorageViewData>);

impl StorageView {
    pub fn construct(
        owner: StorageOwnerKey,
        start: u64,
        value_type: SolveValueType,
    ) -> Result<Self, CallTransferConstructionError> {
        let scalar_count = u64::from(value_type.scalar_count());
        let range = StorageRange::construct(start, scalar_count)?;
        let row_major_strides = storage_traversal_strides(&value_type)?;
        Ok(Self(Arc::new(StorageViewData {
            owner,
            range,
            value_type,
            row_major_strides,
        })))
    }

    #[must_use]
    pub fn owner(&self) -> StorageOwnerKey {
        self.0.owner
    }

    #[must_use]
    pub fn range(&self) -> StorageRange {
        self.0.range
    }

    #[must_use]
    pub fn value_type(&self) -> &SolveValueType {
        &self.0.value_type
    }

    #[must_use]
    pub fn axes(&self) -> impl ExactSizeIterator<Item = StorageTraversalAxis> + '_ {
        self.0
            .value_type
            .dimensions()
            .iter()
            .copied()
            .zip(self.0.row_major_strides.iter().copied())
            .map(|(extent, scalar_stride)| StorageTraversalAxis {
                extent,
                scalar_stride,
            })
    }

    #[must_use]
    pub fn overlaps(&self, other: &Self) -> bool {
        self.owner() == other.owner() && self.range().overlaps(other.range())
    }
}

fn storage_traversal_strides(
    value_type: &SolveValueType,
) -> Result<Box<[u64]>, CallTransferConstructionError> {
    let dimensions = value_type.dimensions();
    if value_type.scalar_count() == 0 {
        return Ok(vec![0; dimensions.len()].into_boxed_slice());
    }

    let dimensions = dimensions
        .iter()
        .map(|extent| usize::try_from(*extent))
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| CallTransferConstructionError::StorageTraversalOverflow)?;
    let strides = row_major_strides(&dimensions)
        .ok_or(CallTransferConstructionError::StorageTraversalOverflow)?;
    let scalar_count = dimensions
        .iter()
        .try_fold(1usize, |count, extent| count.checked_mul(*extent));
    if scalar_count != usize::try_from(value_type.scalar_count()).ok() {
        return Err(CallTransferConstructionError::StorageTraversalMismatch);
    }
    strides
        .into_iter()
        .map(|stride| {
            u64::try_from(stride)
                .map_err(|_| CallTransferConstructionError::StorageTraversalOverflow)
        })
        .collect::<Result<Vec<_>, _>>()
        .map(Vec::into_boxed_slice)
}

/// Opaque program position used only to establish ordering relations.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ProgramPoint(u32);

impl ProgramPoint {
    #[cfg(test)]
    #[must_use]
    const fn issued(ordinal: u32) -> Self {
        Self(ordinal)
    }
}

/// Compiler-issued identity of a lexical arena proved not to escape.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonescapingArenaKey(NonZeroU64);

impl NonescapingArenaKey {
    /// Reifies an upstream escape-analysis capability; the numeric value is
    /// identity only and has no semantic ordering.
    #[cfg(test)]
    #[must_use]
    const fn issued(value: NonZeroU64) -> Self {
        Self(value)
    }
}

/// Bounded lexical arena whose storage lifetime is proved not to escape.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NonescapingArena {
    key: NonescapingArenaKey,
    storage: StorageOwnerKey,
    scalar_capacity: u64,
    enters_at: ProgramPoint,
    exits_after: ProgramPoint,
}

impl NonescapingArena {
    pub fn construct(
        key: NonescapingArenaKey,
        storage: StorageOwnerKey,
        scalar_capacity: u64,
        enters_at: ProgramPoint,
        exits_after: ProgramPoint,
    ) -> Result<Self, CallTransferConstructionError> {
        if enters_at > exits_after {
            return Err(CallTransferConstructionError::InvalidArenaLifetime);
        }
        Ok(Self {
            key,
            storage,
            scalar_capacity,
            enters_at,
            exits_after,
        })
    }

    #[must_use]
    pub const fn key(&self) -> NonescapingArenaKey {
        self.key
    }

    #[must_use]
    pub const fn storage(&self) -> StorageOwnerKey {
        self.storage
    }

    #[must_use]
    pub const fn scalar_capacity(&self) -> u64 {
        self.scalar_capacity
    }

    #[must_use]
    pub const fn enters_at(&self) -> ProgramPoint {
        self.enters_at
    }

    #[must_use]
    pub const fn exits_after(&self) -> ProgramPoint {
        self.exits_after
    }
}

#[derive(Debug, PartialEq, Eq)]
struct WholeInvocationActualsData {
    invocation: CallInvocationKey,
    views: Box<[StorageView]>,
}

/// Complete actual-argument storage sequence for one invocation.
///
/// The upstream checked call ABI supplies `argument_count`; construction
/// rejects a dropped actual before this capability is issued.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WholeInvocationActuals(Arc<WholeInvocationActualsData>);

impl WholeInvocationActuals {
    pub fn construct(
        invocation: CallInvocationKey,
        argument_count: u32,
        views: Vec<StorageView>,
    ) -> Result<Self, CallTransferConstructionError> {
        let actual_count = u32::try_from(views.len())
            .map_err(|_| CallTransferConstructionError::ActualCardinalityOverflow)?;
        if actual_count != argument_count {
            return Err(CallTransferConstructionError::ActualCardinalityMismatch {
                expected: argument_count,
                actual: actual_count,
            });
        }
        Ok(Self(Arc::new(WholeInvocationActualsData {
            invocation,
            views: views.into_boxed_slice(),
        })))
    }

    #[must_use]
    pub fn invocation(&self) -> CallInvocationKey {
        self.0.invocation
    }

    #[must_use]
    pub fn views(&self) -> &[StorageView] {
        &self.0.views
    }
}

/// Complete owner-staged fallback for one exact call result.
#[derive(Debug, PartialEq, Eq)]
pub struct OwnerStagedPlacement {
    subject: CallTransferSubject,
    destination: StorageView,
    destination_available_at: ProgramPoint,
    destination_live_through: ProgramPoint,
    owner_result: StorageView,
    arena: NonescapingArena,
    invocation_at: ProgramPoint,
    transfer_at: ProgramPoint,
    live_actuals: WholeInvocationActuals,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct OwnerStagedLifetime {
    destination_available_at: ProgramPoint,
    destination_live_through: ProgramPoint,
    invocation_at: ProgramPoint,
    transfer_at: ProgramPoint,
}

impl OwnerStagedLifetime {
    #[must_use]
    pub const fn new(
        destination_available_at: ProgramPoint,
        destination_live_through: ProgramPoint,
        invocation_at: ProgramPoint,
        transfer_at: ProgramPoint,
    ) -> Self {
        Self {
            destination_available_at,
            destination_live_through,
            invocation_at,
            transfer_at,
        }
    }
}

impl OwnerStagedPlacement {
    pub fn construct(
        subject: CallTransferSubject,
        destination: StorageView,
        owner_result: StorageView,
        arena: NonescapingArena,
        lifetime: OwnerStagedLifetime,
        live_actuals: WholeInvocationActuals,
    ) -> Result<Self, CallTransferConstructionError> {
        let OwnerStagedLifetime {
            destination_available_at,
            destination_live_through,
            invocation_at,
            transfer_at,
        } = lifetime;
        if destination.value_type() != subject.value_type()
            || owner_result.value_type() != subject.value_type()
        {
            return Err(CallTransferConstructionError::StagedTypeOrShapeMismatch {
                key: subject.key(),
            });
        }
        if owner_result.owner() != arena.storage
            || owner_result.range().end() > arena.scalar_capacity
        {
            return Err(CallTransferConstructionError::OwnerResultOutsideArena {
                key: subject.key(),
            });
        }
        if arena.enters_at > invocation_at
            || invocation_at > transfer_at
            || transfer_at > arena.exits_after
            || destination_available_at > transfer_at
            || transfer_at > destination_live_through
        {
            return Err(CallTransferConstructionError::InvalidStagedLifetime {
                key: subject.key(),
            });
        }
        if live_actuals.invocation() != subject.key().invocation {
            return Err(CallTransferConstructionError::ActualInvocationMismatch {
                key: subject.key(),
            });
        }
        if owner_result.overlaps(&destination)
            || live_actuals
                .views()
                .iter()
                .any(|actual| owner_result.overlaps(actual))
        {
            return Err(
                CallTransferConstructionError::OwnerResultAliasesLiveStorage { key: subject.key() },
            );
        }
        Ok(Self {
            subject,
            destination,
            destination_available_at,
            destination_live_through,
            owner_result,
            arena,
            invocation_at,
            transfer_at,
            live_actuals,
        })
    }
}

/// Positive target-neutral proof bundle for attempting direct placement.
///
/// Construction checks dominance, lifetime, complete aggregate write, exact
/// ABI result correlation, and the complete invocation-actual catalog. The
/// session alone performs the final overlap decision so that overlap selects
/// the already checked owner-staged arm rather than failing open.
#[derive(Debug, PartialEq, Eq)]
pub struct DirectPlacementProof {
    subject: CallTransferSubject,
    destination: StorageView,
    destination_defined_at: ProgramPoint,
    invocation_at: ProgramPoint,
    destination_live_through: ProgramPoint,
    live_actuals: WholeInvocationActuals,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DirectPlacementLifetime {
    destination_defined_at: ProgramPoint,
    invocation_at: ProgramPoint,
    destination_live_through: ProgramPoint,
}

impl DirectPlacementLifetime {
    #[must_use]
    pub const fn new(
        destination_defined_at: ProgramPoint,
        invocation_at: ProgramPoint,
        destination_live_through: ProgramPoint,
    ) -> Self {
        Self {
            destination_defined_at,
            invocation_at,
            destination_live_through,
        }
    }
}

impl DirectPlacementProof {
    pub fn construct(
        subject: CallTransferSubject,
        destination: StorageView,
        lifetime: DirectPlacementLifetime,
        complete_write: StorageView,
        abi_result: CallTransferSubject,
        live_actuals: WholeInvocationActuals,
    ) -> Result<Self, CallTransferConstructionError> {
        let DirectPlacementLifetime {
            destination_defined_at,
            invocation_at,
            destination_live_through,
        } = lifetime;
        if destination_defined_at > invocation_at || invocation_at > destination_live_through {
            return Err(CallTransferConstructionError::InvalidDirectLifetime {
                key: subject.key(),
            });
        }
        if destination != complete_write {
            return Err(CallTransferConstructionError::IncompleteDirectWrite {
                key: subject.key(),
            });
        }
        if subject != abi_result || destination.value_type() != subject.value_type() {
            return Err(CallTransferConstructionError::DirectAbiMismatch { key: subject.key() });
        }
        if live_actuals.invocation() != subject.key().invocation {
            return Err(CallTransferConstructionError::ActualInvocationMismatch {
                key: subject.key(),
            });
        }
        Ok(Self {
            subject,
            destination,
            destination_defined_at,
            invocation_at,
            destination_live_through,
            live_actuals,
        })
    }
}

/// Named reason that a positive direct-placement proof is unavailable.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MissingDirectProof {
    Dominance,
    Lifetime,
    CompleteWrite,
    Abi,
    WholeLiveActualSet,
}

/// Direct-placement evidence supplied to the atomic construction session.
///
/// This is construction input, not the finalized arm. The finalized IR has
/// exactly the closed [`CallTransferPlan`] sum and no optional/default choice.
#[derive(Debug, PartialEq, Eq)]
pub enum DirectPlacementEvidence {
    Proven(DirectPlacementProof),
    Unavailable(MissingDirectProof),
}

/// One complete request. It always carries a valid owner-staged fallback.
#[derive(Debug, PartialEq, Eq)]
pub struct CallTransferRequest {
    subject: CallTransferSubject,
    destination: StorageView,
    owner_staged: OwnerStagedPlacement,
    direct: DirectPlacementEvidence,
}

impl CallTransferRequest {
    #[cfg(test)]
    #[must_use]
    fn issued(
        subject: CallTransferSubject,
        destination: StorageView,
        owner_staged: OwnerStagedPlacement,
        direct: DirectPlacementEvidence,
    ) -> Self {
        Self {
            subject,
            destination,
            owner_staged,
            direct,
        }
    }
}

/// Positive proof-bearing direct aggregate result placement.
#[derive(Debug, PartialEq, Eq)]
pub struct DirectCallTransfer {
    subject: CallTransferSubject,
    destination: StorageView,
    destination_defined_at: ProgramPoint,
    invocation_at: ProgramPoint,
    destination_live_through: ProgramPoint,
    live_actuals: WholeInvocationActuals,
}

impl DirectCallTransfer {
    #[must_use]
    pub fn subject(&self) -> &CallTransferSubject {
        &self.subject
    }

    #[must_use]
    pub fn destination(&self) -> &StorageView {
        &self.destination
    }

    #[must_use]
    pub fn live_actuals(&self) -> &WholeInvocationActuals {
        &self.live_actuals
    }

    #[must_use]
    pub const fn destination_available_at(&self) -> ProgramPoint {
        self.destination_defined_at
    }

    #[must_use]
    pub const fn invocation_at(&self) -> ProgramPoint {
        self.invocation_at
    }

    #[must_use]
    pub const fn destination_live_through(&self) -> ProgramPoint {
        self.destination_live_through
    }
}

/// Positive proof-bearing owner-staged aggregate result placement.
#[derive(Debug, PartialEq, Eq)]
pub struct OwnerStagedCallTransfer {
    subject: CallTransferSubject,
    destination: StorageView,
    destination_available_at: ProgramPoint,
    destination_live_through: ProgramPoint,
    owner_result: StorageView,
    arena: NonescapingArena,
    invocation_at: ProgramPoint,
    transfer_at: ProgramPoint,
    live_actuals: WholeInvocationActuals,
}

impl OwnerStagedCallTransfer {
    #[must_use]
    pub fn subject(&self) -> &CallTransferSubject {
        &self.subject
    }

    #[must_use]
    pub fn destination(&self) -> &StorageView {
        &self.destination
    }

    #[must_use]
    pub fn owner_result(&self) -> &StorageView {
        &self.owner_result
    }

    #[must_use]
    pub fn arena(&self) -> &NonescapingArena {
        &self.arena
    }

    #[must_use]
    pub const fn destination_available_at(&self) -> ProgramPoint {
        self.destination_available_at
    }

    #[must_use]
    pub const fn invocation_at(&self) -> ProgramPoint {
        self.invocation_at
    }

    #[must_use]
    pub const fn transfer_at(&self) -> ProgramPoint {
        self.transfer_at
    }

    #[must_use]
    pub const fn destination_live_through(&self) -> ProgramPoint {
        self.destination_live_through
    }

    #[must_use]
    pub fn live_actuals(&self) -> &WholeInvocationActuals {
        &self.live_actuals
    }
}

/// The only finalized call-transfer choice.
#[derive(Debug, PartialEq, Eq)]
pub enum CallTransferPlan {
    Direct(DirectCallTransfer),
    OwnerStaged(OwnerStagedCallTransfer),
}

/// One canonical subject/plan pair.
#[derive(Debug, PartialEq, Eq)]
pub struct CallTransferPlanEntry {
    plan: CallTransferPlan,
}

impl CallTransferPlanEntry {
    #[must_use]
    pub fn subject(&self) -> &CallTransferSubject {
        match &self.plan {
            CallTransferPlan::Direct(transfer) => transfer.subject(),
            CallTransferPlan::OwnerStaged(transfer) => transfer.subject(),
        }
    }

    #[must_use]
    pub fn plan(&self) -> &CallTransferPlan {
        &self.plan
    }
}

/// Atomically constructed complete call-transfer catalog.
///
/// Neither this root nor either finalized plan arm implements `Default` or
/// `Deserialize`. Failed construction exposes no catalog or partial entry.
#[derive(Debug, PartialEq, Eq)]
pub struct CallTransferPlanSet {
    entries: Box<[CallTransferPlanEntry]>,
}

impl CallTransferPlanSet {
    pub fn construct(
        expected: Vec<CallTransferSubject>,
        build: impl FnOnce(
            &mut CallTransferConstructionSession,
        ) -> Result<(), CallTransferConstructionError>,
    ) -> Result<Self, CallTransferConstructionError> {
        validate_expected_subjects(&expected)?;
        let positions = expected
            .iter()
            .enumerate()
            .map(|(position, subject)| (subject.key(), position))
            .collect();
        let mut session = CallTransferConstructionSession {
            expected: expected.into_boxed_slice(),
            positions,
            issued: BTreeSet::new(),
            entries: Vec::new(),
        };
        build(&mut session)?;
        session.finish()
    }

    #[must_use]
    pub fn entries(&self) -> &[CallTransferPlanEntry] {
        &self.entries
    }
}

/// Linear authority for one plan-set construction. It cannot expose or finish
/// a partial catalog.
pub struct CallTransferConstructionSession {
    expected: Box<[CallTransferSubject]>,
    positions: BTreeMap<CallTransferKey, usize>,
    issued: BTreeSet<CallTransferKey>,
    entries: Vec<CallTransferPlanEntry>,
}

impl CallTransferConstructionSession {
    pub fn issue(
        &mut self,
        request: CallTransferRequest,
    ) -> Result<(), CallTransferConstructionError> {
        let key = request.subject.key();
        if self.issued.contains(&key) {
            return Err(CallTransferConstructionError::DuplicateTransfer { key });
        }
        let Some(&position) = self.positions.get(&key) else {
            return Err(CallTransferConstructionError::ForeignTransfer { key });
        };
        let expected_position = self.entries.len();
        if position != expected_position {
            return Err(CallTransferConstructionError::ReorderedTransfer {
                expected: self.expected[expected_position].key(),
                found: key,
            });
        }
        let expected = &self.expected[expected_position];
        if request.subject.result_ordinal() != expected.result_ordinal() {
            return Err(CallTransferConstructionError::ResultOrdinalMismatch {
                key,
                expected: expected.result_ordinal(),
                actual: request.subject.result_ordinal(),
            });
        }
        if request.subject.value_type() != expected.value_type()
            || request.destination.value_type() != expected.value_type()
        {
            return Err(CallTransferConstructionError::ResultTypeOrShapeMismatch { key });
        }
        if request.owner_staged.subject != request.subject
            || request.owner_staged.destination != request.destination
        {
            return Err(CallTransferConstructionError::StagedCorrelationMismatch { key });
        }

        let plan = match request.direct {
            DirectPlacementEvidence::Unavailable(_reason) => {
                CallTransferPlan::OwnerStaged(owner_staged_transfer(request.owner_staged))
            }
            DirectPlacementEvidence::Proven(proof) => {
                if proof.subject != request.subject
                    || proof.destination != request.destination
                    || proof.live_actuals != request.owner_staged.live_actuals
                    || proof.invocation_at != request.owner_staged.invocation_at
                    || proof.destination_defined_at != request.owner_staged.destination_available_at
                    || proof.destination_live_through
                        != request.owner_staged.destination_live_through
                {
                    return Err(CallTransferConstructionError::DirectCorrelationMismatch { key });
                }
                if proof
                    .live_actuals
                    .views()
                    .iter()
                    .any(|actual| proof.destination.overlaps(actual))
                {
                    CallTransferPlan::OwnerStaged(owner_staged_transfer(request.owner_staged))
                } else {
                    CallTransferPlan::Direct(DirectCallTransfer {
                        subject: proof.subject,
                        destination: proof.destination,
                        destination_defined_at: proof.destination_defined_at,
                        invocation_at: proof.invocation_at,
                        destination_live_through: proof.destination_live_through,
                        live_actuals: proof.live_actuals,
                    })
                }
            }
        };

        self.issued.insert(key);
        self.entries.push(CallTransferPlanEntry { plan });
        Ok(())
    }

    fn finish(self) -> Result<CallTransferPlanSet, CallTransferConstructionError> {
        if let Some(missing) = self.expected.get(self.entries.len()) {
            return Err(CallTransferConstructionError::MissingTransfer { key: missing.key() });
        }
        Ok(CallTransferPlanSet {
            entries: self.entries.into_boxed_slice(),
        })
    }
}

fn owner_staged_transfer(placement: OwnerStagedPlacement) -> OwnerStagedCallTransfer {
    OwnerStagedCallTransfer {
        subject: placement.subject,
        destination: placement.destination,
        destination_available_at: placement.destination_available_at,
        destination_live_through: placement.destination_live_through,
        owner_result: placement.owner_result,
        arena: placement.arena,
        invocation_at: placement.invocation_at,
        transfer_at: placement.transfer_at,
        live_actuals: placement.live_actuals,
    }
}

fn validate_expected_subjects(
    expected: &[CallTransferSubject],
) -> Result<(), CallTransferConstructionError> {
    let mut keys = BTreeSet::new();
    let mut completed_invocations = BTreeSet::new();
    let mut active_invocation = None;
    let mut next_ordinal = 0u32;
    for subject in expected {
        if !keys.insert(subject.key()) {
            return Err(CallTransferConstructionError::DuplicateExpectedTransfer {
                key: subject.key(),
            });
        }
        let invocation = subject.key().invocation();
        if active_invocation != Some(invocation) {
            if let Some(previous) = active_invocation.replace(invocation) {
                completed_invocations.insert(previous);
            }
            if completed_invocations.contains(&invocation) {
                return Err(CallTransferConstructionError::NoncontiguousInvocation { invocation });
            }
            next_ordinal = 0;
        }
        if subject.result_ordinal() != next_ordinal {
            return Err(CallTransferConstructionError::ExpectedResultOrderMismatch {
                key: subject.key(),
                expected: next_ordinal,
                actual: subject.result_ordinal(),
            });
        }
        next_ordinal = next_ordinal
            .checked_add(1)
            .ok_or(CallTransferConstructionError::ResultCardinalityOverflow { invocation })?;
    }
    Ok(())
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CallTransferConstructionError {
    StorageRangeOverflow {
        start: u64,
        scalar_count: u64,
    },
    StorageTraversalOverflow,
    StorageTraversalMismatch,
    ActualCardinalityOverflow,
    ActualCardinalityMismatch {
        expected: u32,
        actual: u32,
    },
    InvalidArenaLifetime,
    StagedTypeOrShapeMismatch {
        key: CallTransferKey,
    },
    OwnerResultOutsideArena {
        key: CallTransferKey,
    },
    InvalidStagedLifetime {
        key: CallTransferKey,
    },
    ActualInvocationMismatch {
        key: CallTransferKey,
    },
    OwnerResultAliasesLiveStorage {
        key: CallTransferKey,
    },
    InvalidDirectLifetime {
        key: CallTransferKey,
    },
    IncompleteDirectWrite {
        key: CallTransferKey,
    },
    DirectAbiMismatch {
        key: CallTransferKey,
    },
    DuplicateExpectedTransfer {
        key: CallTransferKey,
    },
    NoncontiguousInvocation {
        invocation: CallInvocationKey,
    },
    ExpectedResultOrderMismatch {
        key: CallTransferKey,
        expected: u32,
        actual: u32,
    },
    ResultCardinalityOverflow {
        invocation: CallInvocationKey,
    },
    DuplicateTransfer {
        key: CallTransferKey,
    },
    ForeignTransfer {
        key: CallTransferKey,
    },
    ReorderedTransfer {
        expected: CallTransferKey,
        found: CallTransferKey,
    },
    ResultOrdinalMismatch {
        key: CallTransferKey,
        expected: u32,
        actual: u32,
    },
    ResultTypeOrShapeMismatch {
        key: CallTransferKey,
    },
    StagedCorrelationMismatch {
        key: CallTransferKey,
    },
    DirectCorrelationMismatch {
        key: CallTransferKey,
    },
    MissingTransfer {
        key: CallTransferKey,
    },
}

impl fmt::Display for CallTransferConstructionError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::StorageRangeOverflow { .. } => write!(formatter, "storage range overflows u64"),
            Self::StorageTraversalOverflow => {
                write!(formatter, "compact storage traversal overflows u64")
            }
            Self::StorageTraversalMismatch => {
                write!(
                    formatter,
                    "storage traversal disagrees with aggregate shape"
                )
            }
            Self::ActualCardinalityOverflow => {
                write!(formatter, "invocation actual cardinality overflows u32")
            }
            Self::ActualCardinalityMismatch { .. } => {
                write!(formatter, "invocation actual sequence is incomplete")
            }
            Self::InvalidArenaLifetime => write!(formatter, "arena lifetime is reversed"),
            Self::StagedTypeOrShapeMismatch { .. } => {
                write!(
                    formatter,
                    "owner-staged transfer type or shape is not exact"
                )
            }
            Self::OwnerResultOutsideArena { .. } => {
                write!(formatter, "owner result is outside its bounded arena")
            }
            Self::InvalidStagedLifetime { .. } => {
                write!(
                    formatter,
                    "owner-staged invocation/copy is outside arena lifetime"
                )
            }
            Self::ActualInvocationMismatch { .. } => {
                write!(formatter, "live actuals belong to another invocation")
            }
            Self::OwnerResultAliasesLiveStorage { .. } => {
                write!(formatter, "owner-staged result aliases live storage")
            }
            Self::InvalidDirectLifetime { .. } => {
                write!(formatter, "direct destination lacks dominance or lifetime")
            }
            Self::IncompleteDirectWrite { .. } => {
                write!(
                    formatter,
                    "direct result does not completely write its destination"
                )
            }
            Self::DirectAbiMismatch { .. } => {
                write!(
                    formatter,
                    "direct result does not match its exact ABI projection"
                )
            }
            Self::DuplicateExpectedTransfer { .. } => {
                write!(formatter, "expected transfer key is duplicated")
            }
            Self::NoncontiguousInvocation { .. } => {
                write!(formatter, "one invocation's results are not contiguous")
            }
            Self::ExpectedResultOrderMismatch { .. } => {
                write!(formatter, "expected results are not in exact ABI order")
            }
            Self::ResultCardinalityOverflow { .. } => {
                write!(formatter, "result cardinality overflows u32")
            }
            Self::DuplicateTransfer { .. } => write!(formatter, "transfer was issued twice"),
            Self::ForeignTransfer { .. } => {
                write!(formatter, "transfer does not belong to this construction")
            }
            Self::ReorderedTransfer { .. } => {
                write!(formatter, "transfer was issued out of exact result order")
            }
            Self::ResultOrdinalMismatch { .. } => {
                write!(formatter, "transfer result ordinal changed")
            }
            Self::ResultTypeOrShapeMismatch { .. } => {
                write!(formatter, "transfer result type or shape changed")
            }
            Self::StagedCorrelationMismatch { .. } => {
                write!(formatter, "owner-staged proof belongs to another result")
            }
            Self::DirectCorrelationMismatch { .. } => {
                write!(
                    formatter,
                    "direct proof belongs to another result or invocation"
                )
            }
            Self::MissingTransfer { .. } => write!(formatter, "expected transfer is missing"),
        }
    }
}

impl std::error::Error for CallTransferConstructionError {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SolveArithmeticProfile, SolveIntegerDomain, SolveRealFormat, SolveScalarType};
    use rumoca_core::RealMatrixMultiplySemantics;

    fn nonzero(value: u64) -> NonZeroU64 {
        NonZeroU64::new(value).unwrap()
    }

    fn profile() -> SolveArithmeticProfile {
        SolveArithmeticProfile::construct(
            SolveRealFormat::Binary64,
            SolveIntegerDomain::FULL,
            RealMatrixMultiplySemantics::SeparateMulAddAscendingFirstProduct,
        )
    }

    fn vector_type(extent: u32) -> SolveValueType {
        SolveValueType::tensor(SolveScalarType::real(profile()), vec![extent]).unwrap()
    }

    fn invocation(value: u64) -> CallInvocationKey {
        CallInvocationKey::issued(nonzero(value))
    }

    fn subject(invocation: CallInvocationKey, result: u64, ordinal: u32) -> CallTransferSubject {
        CallTransferSubject::issued(
            CallTransferKey::issued(invocation, CallResultKey::issued(nonzero(result))),
            ordinal,
            vector_type(4),
        )
    }

    fn view(owner: u64, start: u64, value_type: SolveValueType) -> StorageView {
        StorageView::construct(StorageOwnerKey::issued(nonzero(owner)), start, value_type).unwrap()
    }

    fn actuals(invocation: CallInvocationKey, views: Vec<StorageView>) -> WholeInvocationActuals {
        WholeInvocationActuals::construct(invocation, views.len() as u32, views).unwrap()
    }

    fn staged(
        subject: &CallTransferSubject,
        destination: &StorageView,
        invocation_at: ProgramPoint,
        live_actuals: WholeInvocationActuals,
    ) -> OwnerStagedPlacement {
        let owner_result = view(90, 0, subject.value_type().clone());
        let arena = NonescapingArena::construct(
            NonescapingArenaKey::issued(nonzero(1)),
            owner_result.owner(),
            u64::from(subject.value_type().scalar_count()),
            ProgramPoint::issued(0),
            ProgramPoint::issued(20),
        )
        .unwrap();
        OwnerStagedPlacement::construct(
            subject.clone(),
            destination.clone(),
            owner_result,
            arena,
            OwnerStagedLifetime::new(
                ProgramPoint::issued(1),
                ProgramPoint::issued(20),
                invocation_at,
                ProgramPoint::issued(invocation_at.0 + 1),
            ),
            live_actuals,
        )
        .unwrap()
    }

    fn direct(
        subject: &CallTransferSubject,
        destination: &StorageView,
        invocation_at: ProgramPoint,
        live_actuals: WholeInvocationActuals,
    ) -> DirectPlacementProof {
        DirectPlacementProof::construct(
            subject.clone(),
            destination.clone(),
            DirectPlacementLifetime::new(
                ProgramPoint::issued(1),
                invocation_at,
                ProgramPoint::issued(20),
            ),
            destination.clone(),
            subject.clone(),
            live_actuals,
        )
        .unwrap()
    }

    fn request(
        subject: &CallTransferSubject,
        destination: StorageView,
        direct_evidence: DirectPlacementEvidence,
        live_actuals: WholeInvocationActuals,
    ) -> CallTransferRequest {
        let invocation_at = ProgramPoint::issued(10);
        let owner_staged = staged(subject, &destination, invocation_at, live_actuals);
        CallTransferRequest::issued(subject.clone(), destination, owner_staged, direct_evidence)
    }

    fn request_with_fresh_direct(
        subject: &CallTransferSubject,
        destination: StorageView,
        live_actuals: WholeInvocationActuals,
    ) -> CallTransferRequest {
        let proof = direct(
            subject,
            &destination,
            ProgramPoint::issued(10),
            live_actuals.clone(),
        );
        request(
            subject,
            destination,
            DirectPlacementEvidence::Proven(proof),
            live_actuals,
        )
    }

    #[test]
    fn positive_direct_proof_constructs_only_the_direct_arm() {
        let invocation = invocation(1);
        let subject = subject(invocation, 1, 0);
        let destination = view(1, 20, subject.value_type().clone());
        let live_actuals = actuals(invocation, vec![view(2, 0, subject.value_type().clone())]);
        let plans = CallTransferPlanSet::construct(vec![subject.clone()], |session| {
            session.issue(request_with_fresh_direct(
                &subject,
                destination,
                live_actuals,
            ))
        })
        .unwrap();

        let [entry] = plans.entries() else {
            panic!("exactly one transfer must be issued");
        };
        assert!(matches!(entry.plan(), CallTransferPlan::Direct(_)));
    }

    #[test]
    fn destination_overlap_with_a_live_actual_selects_owner_staged() {
        let invocation = invocation(1);
        let subject = subject(invocation, 1, 0);
        let destination = view(1, 20, subject.value_type().clone());
        let live_actuals = actuals(invocation, vec![view(1, 22, subject.value_type().clone())]);
        let plans = CallTransferPlanSet::construct(vec![subject.clone()], |session| {
            session.issue(request_with_fresh_direct(
                &subject,
                destination,
                live_actuals,
            ))
        })
        .unwrap();

        assert!(matches!(
            plans.entries()[0].plan(),
            CallTransferPlan::OwnerStaged(_)
        ));
    }

    #[test]
    fn owner_staged_rejects_each_storage_type_or_shape_mismatch() {
        for (destination_extent, owner_extent) in [(3, 4), (4, 3)] {
            let invocation = invocation(1);
            let subject = subject(invocation, 1, 0);
            let destination = view(1, 0, vector_type(destination_extent));
            let owner_result = view(90, 0, vector_type(owner_extent));
            let arena = NonescapingArena::construct(
                NonescapingArenaKey::issued(nonzero(1)),
                owner_result.owner(),
                u64::from(owner_result.value_type().scalar_count()),
                ProgramPoint::issued(0),
                ProgramPoint::issued(20),
            )
            .expect("the adversarial arena is internally bounded");
            let live_actuals = actuals(invocation, vec![view(2, 0, subject.value_type().clone())]);

            let result = OwnerStagedPlacement::construct(
                subject.clone(),
                destination,
                owner_result,
                arena,
                OwnerStagedLifetime::new(
                    ProgramPoint::issued(1),
                    ProgramPoint::issued(20),
                    ProgramPoint::issued(10),
                    ProgramPoint::issued(11),
                ),
                live_actuals,
            );

            assert!(matches!(
                result,
                Err(CallTransferConstructionError::StagedTypeOrShapeMismatch { .. })
            ));
        }
    }

    #[test]
    fn every_named_missing_direct_proof_selects_owner_staged() {
        let reasons = [
            MissingDirectProof::Dominance,
            MissingDirectProof::Lifetime,
            MissingDirectProof::CompleteWrite,
            MissingDirectProof::Abi,
            MissingDirectProof::WholeLiveActualSet,
        ];
        for (index, reason) in reasons.into_iter().enumerate() {
            let invocation = invocation(index as u64 + 1);
            let subject = subject(invocation, 1, 0);
            let destination = view(1, 20, subject.value_type().clone());
            let live_actuals = actuals(invocation, vec![view(2, 0, subject.value_type().clone())]);
            let plans = CallTransferPlanSet::construct(vec![subject.clone()], |session| {
                session.issue(request(
                    &subject,
                    destination,
                    DirectPlacementEvidence::Unavailable(reason),
                    live_actuals,
                ))
            })
            .unwrap();
            assert!(matches!(
                plans.entries()[0].plan(),
                CallTransferPlan::OwnerStaged(_)
            ));
        }
    }

    #[test]
    fn reordered_dropped_and_duplicate_results_are_rejected() {
        let invocation = invocation(1);
        let first = subject(invocation, 1, 0);
        let second = subject(invocation, 2, 1);
        let make_request = |subject: &CallTransferSubject| {
            let destination = view(
                10 + u64::from(subject.result_ordinal()),
                0,
                subject.value_type().clone(),
            );
            let live_actuals = actuals(invocation, vec![view(2, 0, subject.value_type().clone())]);
            request(
                subject,
                destination,
                DirectPlacementEvidence::Unavailable(MissingDirectProof::Dominance),
                live_actuals,
            )
        };

        let reordered = CallTransferPlanSet::construct(vec![first.clone(), second.clone()], |s| {
            s.issue(make_request(&second))
        });
        assert!(matches!(
            reordered,
            Err(CallTransferConstructionError::ReorderedTransfer { .. })
        ));

        let dropped = CallTransferPlanSet::construct(vec![first.clone(), second.clone()], |s| {
            s.issue(make_request(&first))
        });
        assert!(matches!(
            dropped,
            Err(CallTransferConstructionError::MissingTransfer { .. })
        ));

        let duplicate = CallTransferPlanSet::construct(vec![first.clone(), second], |s| {
            s.issue(make_request(&first))?;
            s.issue(make_request(&first))
        });
        assert!(matches!(
            duplicate,
            Err(CallTransferConstructionError::DuplicateTransfer { .. })
        ));
    }

    #[test]
    fn zero_extent_is_a_vacuous_complete_direct_transfer() {
        let invocation = invocation(1);
        let value_type = vector_type(0);
        let subject = CallTransferSubject::issued(
            CallTransferKey::issued(invocation, CallResultKey::issued(nonzero(1))),
            0,
            value_type.clone(),
        );
        let destination = view(1, u64::MAX, value_type.clone());
        let live_actuals = actuals(invocation, vec![view(1, u64::MAX, value_type)]);
        assert!(destination.range().is_empty());
        assert_eq!(destination.axes().next().unwrap().scalar_stride(), 0);

        let plans = CallTransferPlanSet::construct(vec![subject.clone()], |session| {
            session.issue(request_with_fresh_direct(
                &subject,
                destination,
                live_actuals,
            ))
        })
        .unwrap();
        assert!(matches!(
            plans.entries()[0].plan(),
            CallTransferPlan::Direct(_)
        ));
    }

    #[test]
    fn million_element_view_stores_only_rank_sized_traversal() {
        let value_type =
            SolveValueType::tensor(SolveScalarType::real(profile()), vec![1_000_000, 1]).unwrap();
        let storage = view(1, 0, value_type);
        let axes = storage.axes().collect::<Vec<_>>();

        assert_eq!(storage.range().scalar_count(), 1_000_000);
        assert_eq!(axes.len(), 2);
        assert_eq!(axes[0].scalar_stride(), 1);
        assert_eq!(axes[1].scalar_stride(), 1);
        assert_eq!(
            std::mem::size_of::<StorageView>(),
            std::mem::size_of::<Arc<StorageViewData>>()
        );
    }

    #[test]
    fn rejected_issue_does_not_advance_and_failed_root_exposes_no_partial_plan() {
        let invocation = invocation(1);
        let expected = subject(invocation, 1, 0);
        let wrong = CallTransferSubject::issued(expected.key(), 1, expected.value_type().clone());
        let wrong_destination = view(1, 0, wrong.value_type().clone());
        let wrong_actuals = actuals(invocation, vec![view(2, 0, wrong.value_type().clone())]);
        let good_destination = view(1, 0, expected.value_type().clone());
        let good_actuals = actuals(invocation, vec![view(2, 0, expected.value_type().clone())]);

        let recovered = CallTransferPlanSet::construct(vec![expected.clone()], |session| {
            assert!(matches!(
                session.issue(request(
                    &wrong,
                    wrong_destination,
                    DirectPlacementEvidence::Unavailable(MissingDirectProof::Abi),
                    wrong_actuals,
                )),
                Err(CallTransferConstructionError::ResultOrdinalMismatch { .. })
            ));
            session.issue(request(
                &expected,
                good_destination,
                DirectPlacementEvidence::Unavailable(MissingDirectProof::Abi),
                good_actuals,
            ))
        })
        .unwrap();
        assert_eq!(recovered.entries().len(), 1);

        let failed: Result<CallTransferPlanSet, _> =
            CallTransferPlanSet::construct(vec![expected], |_session| {
                Err(CallTransferConstructionError::InvalidArenaLifetime)
            });
        assert!(failed.is_err());
    }

    #[test]
    fn expected_manifest_mutations_fail_before_construction() {
        let first_invocation = invocation(1);
        let second_invocation = invocation(2);
        let first = subject(first_invocation, 1, 0);
        let gap = CallTransferSubject::issued(
            CallTransferKey::issued(first_invocation, CallResultKey::issued(nonzero(2))),
            2,
            vector_type(4),
        );
        assert!(matches!(
            CallTransferPlanSet::construct(vec![first.clone(), gap], |_| Ok(())),
            Err(CallTransferConstructionError::ExpectedResultOrderMismatch { .. })
        ));
        assert!(matches!(
            CallTransferPlanSet::construct(vec![first.clone(), first.clone()], |_| Ok(())),
            Err(CallTransferConstructionError::DuplicateExpectedTransfer { .. })
        ));
        let other = subject(second_invocation, 1, 0);
        let resumed = subject(first_invocation, 2, 1);
        assert!(matches!(
            CallTransferPlanSet::construct(vec![first, other, resumed], |_| Ok(())),
            Err(CallTransferConstructionError::NoncontiguousInvocation { .. })
        ));
    }
}
