// SPEC_0021 file-size split plan: assignment-shape recognition and Y-dependency
// analysis now live in focused private submodules. If this owner returns above
// the action threshold, move `ExactAssignmentProgramBuilder` together with the
// `construct_`, `materialize_`, and `append_` exact-assignment helpers into
// `refresh/exact_assignment.rs`, leaving plan identity, stages, and validation
// here.
mod assignment_shape;
mod dependency;

use std::collections::BTreeMap;
use std::fmt;
use std::num::NonZeroU64;
use std::ops::Index;

use serde::{Deserialize, Deserializer, Serialize};

#[cfg(test)]
use assignment_shape::canonical_assignment_shape_for_output;
use assignment_shape::non_causal_assignment_operation;
pub use assignment_shape::{
    derive_target_assignment_shape_for_output, derive_target_assignment_shapes,
};
pub use dependency::ScalarProgramYDependency;
use dependency::{assignment_y_dependencies_for_shapes, shape_value_registers};

use crate::{
    AlgebraicProjectionPlan, BinaryOp, ComputeBlock, ComputeNode, LinearOp, ScalarProgramBlock,
    StructuralPattern, TargetAssignmentShape, UnaryOp, deserialize_required_option,
};

/// Exact canonical scalar program inside one tensor-aware [`crate::ComputeBlock`].
///
/// This is a construction identity, not a row in an evaluator scalar view.
/// Final adapters mechanically project it to their local instruction row.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, Serialize)]
pub struct RefreshScalarProgramSource {
    node: u32,
    program: u32,
}

impl RefreshScalarProgramSource {
    #[must_use]
    pub fn checked(node: usize, program: usize) -> Option<Self> {
        Some(Self {
            node: u32::try_from(node).ok()?,
            program: u32::try_from(program).ok()?,
        })
    }

    #[must_use]
    pub const fn node(self) -> u32 {
        self.node
    }

    #[must_use]
    pub const fn program(self) -> u32 {
        self.program
    }
}

/// Construction-issued identity of one canonical implicit row/output owner.
///
/// ```compile_fail
/// use rumoca_ir_solve::RefreshRowOwnerId;
///
/// let _ = RefreshRowOwnerId::default();
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, Serialize)]
#[serde(transparent)]
pub struct RefreshRowOwnerId(u32);

impl RefreshRowOwnerId {
    pub fn checked(index: usize) -> Option<Self> {
        u32::try_from(index).ok().map(Self)
    }
}

/// Compact construction-issued selection into one [`RefreshPlan`] row catalog.
///
/// The selection stores only canonical catalog positions. It never clones row
/// metadata and cannot name a row owned by another plan after checked replay.
///
/// ```compile_fail
/// use rumoca_ir_solve::RefreshRowSelection;
///
/// let _ = RefreshRowSelection::default();
/// ```
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
#[serde(transparent)]
pub struct RefreshRowSelection(Box<[u32]>);

impl RefreshRowSelection {
    /// Intentional selection containing no canonical rows.
    #[must_use]
    pub fn empty() -> Self {
        Self(Box::new([]))
    }

    pub fn checked(
        row_count: usize,
        indices: impl IntoIterator<Item = usize>,
    ) -> Result<Self, ContinuousRefreshConstructionError> {
        let mut selected = Vec::new();
        let mut seen = vec![false; row_count];
        for index in indices {
            let Some(slot) = seen.get_mut(index) else {
                return refresh_error(
                    "continuous refresh selection refers to an unowned canonical row".to_string(),
                );
            };
            if std::mem::replace(slot, true) {
                return refresh_error(
                    "continuous refresh selection repeats a canonical row".to_string(),
                );
            }
            selected.push(u32::try_from(index).map_err(|_| {
                ContinuousRefreshConstructionError {
                    reason: "continuous refresh row catalog exceeds u32".to_string(),
                }
            })?);
        }
        Ok(Self(selected.into_boxed_slice()))
    }

    pub fn all(row_count: usize) -> Result<Self, ContinuousRefreshConstructionError> {
        Self::checked(row_count, 0..row_count)
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    #[must_use]
    pub fn indices(&self) -> &[u32] {
        &self.0
    }
}

#[derive(Clone, Copy)]
pub struct RefreshRows<'a> {
    catalog: &'a [AlgebraicRefreshRow],
    indices: &'a [u32],
}

impl<'a> RefreshRows<'a> {
    #[must_use]
    pub const fn len(self) -> usize {
        self.indices.len()
    }

    #[must_use]
    pub const fn is_empty(self) -> bool {
        self.indices.is_empty()
    }

    #[must_use]
    pub fn get(self, position: usize) -> Option<&'a AlgebraicRefreshRow> {
        let index = usize::try_from(*self.indices.get(position)?).ok()?;
        self.catalog.get(index)
    }

    pub fn iter(
        self,
    ) -> impl ExactSizeIterator<Item = &'a AlgebraicRefreshRow> + DoubleEndedIterator {
        self.indices
            .iter()
            .map(|index| &self.catalog[*index as usize])
    }
}

impl Index<usize> for RefreshRows<'_> {
    type Output = AlgebraicRefreshRow;

    fn index(&self, position: usize) -> &Self::Output {
        &self.catalog[self.indices[position] as usize]
    }
}

/// Opaque construction-issued identity of one exact ordered row selection.
///
/// ```compile_fail
/// use rumoca_ir_solve::RefreshSequenceId;
///
/// let _ = RefreshSequenceId::default();
/// ```
///
/// ```compile_fail
/// use rumoca_ir_solve::RefreshSequenceId;
///
/// let _ = RefreshSequenceId(1);
/// ```
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RefreshSequenceId(NonZeroU64);

/// One exact scalar projection of a canonical continuous output owner.
#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct AlgebraicRefreshRow {
    owner_id: RefreshRowOwnerId,
    source: RefreshScalarProgramSource,
    equation_index: usize,
    output_offset: usize,
    target_index: usize,
    assignment_target: Option<usize>,
    assignment_shape: Option<TargetAssignmentShape>,
    direct_assignment_certified: bool,
    exact_assignment_certified: bool,
}

pub struct AlgebraicRefreshRowDraft {
    pub owner_id: RefreshRowOwnerId,
    pub source: RefreshScalarProgramSource,
    pub equation_index: usize,
    pub output_offset: usize,
    pub target_index: usize,
    pub assignment_target: Option<usize>,
    pub assignment_shape: Option<TargetAssignmentShape>,
    pub direct_assignment_certified: bool,
    pub exact_assignment_certified: bool,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct AlgebraicRefreshRowWire {
    owner_id: RefreshRowOwnerId,
    source: RefreshScalarProgramSource,
    equation_index: usize,
    output_offset: usize,
    target_index: usize,
    #[serde(deserialize_with = "deserialize_required_option")]
    assignment_target: Option<usize>,
    #[serde(deserialize_with = "deserialize_required_option")]
    assignment_shape: Option<TargetAssignmentShape>,
    direct_assignment_certified: bool,
    exact_assignment_certified: bool,
}

/// One exact assignment program mechanically issued from a checked refresh row.
///
/// The program is derived once from the canonical source and is intentionally
/// omitted from the wire format. Solve wire replay reconstructs it through the
/// same total constructor after the canonical `ComputeBlock` is available.
#[derive(Clone, Debug)]
pub struct ExactRefreshAssignmentProgram {
    id: ExactRefreshAssignmentProgramId,
    row_owners: Box<[RefreshRowOwnerId]>,
    source: RefreshScalarProgramSource,
    target_indices: Box<[usize]>,
    assignment_shapes: Box<[TargetAssignmentShape]>,
    assignment_y_dependencies: Box<[Box<[usize]>]>,
    final_program: ScalarProgramBlock,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ExactRefreshAssignmentProgramId(u32);

/// One construction-issued ordered exact-assignment execution schedule.
///
/// The schedule stores only issued program identities. The canonical programs
/// remain single-owned by [`ContinuousRefreshOwners`], and final execution
/// adapters resolve these identities without inspecting residual operations.
#[derive(Clone, Debug)]
pub struct ExactRefreshAssignmentSchedule {
    sequence_id: RefreshSequenceId,
    program_ids: Box<[ExactRefreshAssignmentProgramId]>,
}

impl ExactRefreshAssignmentSchedule {
    pub const fn sequence_id(&self) -> RefreshSequenceId {
        self.sequence_id
    }

    pub fn program_ids(&self) -> &[ExactRefreshAssignmentProgramId] {
        &self.program_ids
    }
}

impl ExactRefreshAssignmentProgram {
    pub const fn id(&self) -> ExactRefreshAssignmentProgramId {
        self.id
    }

    pub fn row_owners(&self) -> &[RefreshRowOwnerId] {
        &self.row_owners
    }

    pub const fn source(&self) -> RefreshScalarProgramSource {
        self.source
    }

    pub fn target_indices(&self) -> &[usize] {
        &self.target_indices
    }

    pub fn assignment_shapes(&self) -> &[TargetAssignmentShape] {
        &self.assignment_shapes
    }

    #[must_use]
    pub fn assignment_y_dependencies(&self, position: usize) -> Option<&[usize]> {
        self.assignment_y_dependencies
            .get(position)
            .map(Box::as_ref)
    }

    /// Exact final scalar program issued with this owner at construction.
    #[must_use]
    pub const fn final_program(&self) -> &ScalarProgramBlock {
        &self.final_program
    }
}

/// One construction-ordered stage in an algebraic value refresh.
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum RefreshStage {
    CausalSeedSweep {
        static_rows: RefreshRowSelection,
        dynamic_rows: RefreshRowSelection,
    },
    ExactAssignments {
        static_rows: RefreshRowSelection,
        dynamic_rows: RefreshRowSelection,
    },
    ProjectionBlock {
        block_index: usize,
        plan: AlgebraicProjectionPlan,
        seed_rows: RefreshRowSelection,
    },
}

/// Exact compiler-issued continuous refresh schedule.
///
/// Intentional absence is named [`RefreshPlan::empty`]; generic construction
/// cannot silently mint an executable-looking draft:
///
/// ```compile_fail
/// use rumoca_ir_solve::RefreshPlan;
///
/// let _ = RefreshPlan::default();
/// ```
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RefreshPlan {
    pub simultaneous_plan: AlgebraicProjectionPlan,
    pub simultaneous_block_indices: Vec<usize>,
    pub value_projection_plan: AlgebraicProjectionPlan,
    pub rows: Vec<AlgebraicRefreshRow>,
    pub causal_seed_rows: RefreshRowSelection,
    pub static_causal_seed_rows: RefreshRowSelection,
    pub dynamic_causal_seed_rows: RefreshRowSelection,
    pub value_stages: Vec<RefreshStage>,
}

impl RefreshPlan {
    /// Intentional semantic draft with no refresh work.
    #[must_use]
    pub fn empty() -> Self {
        Self {
            simultaneous_plan: AlgebraicProjectionPlan { blocks: Vec::new() },
            simultaneous_block_indices: Vec::new(),
            value_projection_plan: AlgebraicProjectionPlan { blocks: Vec::new() },
            rows: Vec::new(),
            causal_seed_rows: RefreshRowSelection::empty(),
            static_causal_seed_rows: RefreshRowSelection::empty(),
            dynamic_causal_seed_rows: RefreshRowSelection::empty(),
            value_stages: Vec::new(),
        }
    }
}

/// Construction-issued stage whose execution identities cannot be supplied by
/// a semantic refresh-plan producer or by wire replay.
#[derive(Clone, Debug, PartialEq)]
pub enum IssuedRefreshStage {
    CausalSeedSweep {
        static_sequence: RefreshSequenceId,
        dynamic_sequence: RefreshSequenceId,
        static_rows: RefreshRowSelection,
        dynamic_rows: RefreshRowSelection,
    },
    ExactAssignments {
        static_sequence: RefreshSequenceId,
        dynamic_sequence: RefreshSequenceId,
        static_rows: RefreshRowSelection,
        dynamic_rows: RefreshRowSelection,
    },
    ProjectionBlock {
        seed_sequence: RefreshSequenceId,
        block_index: usize,
        plan: AlgebraicProjectionPlan,
        seed_rows: RefreshRowSelection,
    },
}

/// Opaque executable refresh plan issued from one semantic [`RefreshPlan`].
#[derive(Clone, Debug, PartialEq)]
pub struct IssuedRefreshPlan {
    plan: PendingRefreshPlan,
    causal_solution_certified: bool,
}

/// Fully validated plan with construction-owned sequence identities, retained
/// only inside the sole issuer until its causal certificate has been derived.
#[derive(Clone, Debug, PartialEq)]
struct PendingRefreshPlan {
    static_causal_sequence: RefreshSequenceId,
    dynamic_causal_sequence: RefreshSequenceId,
    simultaneous_plan: AlgebraicProjectionPlan,
    simultaneous_block_indices: Vec<usize>,
    value_projection_plan: AlgebraicProjectionPlan,
    rows: Vec<AlgebraicRefreshRow>,
    causal_seed_rows: RefreshRowSelection,
    static_causal_seed_rows: RefreshRowSelection,
    dynamic_causal_seed_rows: RefreshRowSelection,
    value_stages: Vec<IssuedRefreshStage>,
}

/// Construction-issued proof that `remainder` is the exact ordered portion of
/// a required refresh not settled by another owner at the same coordinate.
#[derive(Clone, Debug)]
pub struct RefreshRemainderRelation {
    remainder: IssuedRefreshPlan,
}

/// Complete construction-issued continuous refresh inventory for one model.
///
/// Generic construction cannot issue the required canonical-row and ordered
/// remainder relations:
///
/// ```compile_fail
/// use rumoca_ir_solve::ContinuousRefreshOwners;
///
/// let _ = ContinuousRefreshOwners::default();
/// ```
#[derive(Clone, Debug)]
pub struct ContinuousRefreshOwners {
    algebraic: IssuedRefreshPlan,
    derivative: IssuedRefreshPlan,
    root: IssuedRefreshPlan,
    event: IssuedRefreshPlan,
    clock_events: Vec<IssuedRefreshPlan>,
    static_parameter_indices: Box<[usize]>,
    exact_assignment_programs: Vec<ExactRefreshAssignmentProgram>,
    exact_assignment_schedules: Vec<ExactRefreshAssignmentSchedule>,
    root_after_derivative: RefreshRemainderRelation,
    algebraic_after_derivative: RefreshRemainderRelation,
    clock_events_after_event: Vec<RefreshRemainderRelation>,
}

#[derive(Serialize)]
enum RefreshStageWireRef<'a> {
    CausalSeedSweep {
        static_rows: &'a RefreshRowSelection,
        dynamic_rows: &'a RefreshRowSelection,
    },
    ExactAssignments {
        static_rows: &'a RefreshRowSelection,
        dynamic_rows: &'a RefreshRowSelection,
    },
    ProjectionBlock {
        block_index: usize,
        plan: &'a AlgebraicProjectionPlan,
        seed_rows: &'a RefreshRowSelection,
    },
}

#[derive(Serialize)]
struct RefreshPlanWireRef<'a> {
    simultaneous_plan: &'a AlgebraicProjectionPlan,
    simultaneous_block_indices: &'a [usize],
    value_projection_plan: &'a AlgebraicProjectionPlan,
    rows: &'a [AlgebraicRefreshRow],
    causal_seed_rows: &'a RefreshRowSelection,
    static_causal_seed_rows: &'a RefreshRowSelection,
    dynamic_causal_seed_rows: &'a RefreshRowSelection,
    value_stages: Vec<RefreshStageWireRef<'a>>,
}

impl<'a> From<&'a IssuedRefreshPlan> for RefreshPlanWireRef<'a> {
    fn from(plan: &'a IssuedRefreshPlan) -> Self {
        let plan = &plan.plan;
        let value_stages = plan
            .value_stages
            .iter()
            .map(|stage| match stage {
                IssuedRefreshStage::CausalSeedSweep {
                    static_rows,
                    dynamic_rows,
                    ..
                } => RefreshStageWireRef::CausalSeedSweep {
                    static_rows,
                    dynamic_rows,
                },
                IssuedRefreshStage::ExactAssignments {
                    static_rows,
                    dynamic_rows,
                    ..
                } => RefreshStageWireRef::ExactAssignments {
                    static_rows,
                    dynamic_rows,
                },
                IssuedRefreshStage::ProjectionBlock {
                    block_index,
                    plan,
                    seed_rows,
                    ..
                } => RefreshStageWireRef::ProjectionBlock {
                    block_index: *block_index,
                    plan,
                    seed_rows,
                },
            })
            .collect();
        Self {
            simultaneous_plan: &plan.simultaneous_plan,
            simultaneous_block_indices: &plan.simultaneous_block_indices,
            value_projection_plan: &plan.value_projection_plan,
            rows: &plan.rows,
            causal_seed_rows: &plan.causal_seed_rows,
            static_causal_seed_rows: &plan.static_causal_seed_rows,
            dynamic_causal_seed_rows: &plan.dynamic_causal_seed_rows,
            value_stages,
        }
    }
}

#[derive(Serialize)]
struct ContinuousRefreshOwnersWireRef<'a> {
    algebraic: RefreshPlanWireRef<'a>,
    derivative: RefreshPlanWireRef<'a>,
    root: RefreshPlanWireRef<'a>,
    event: RefreshPlanWireRef<'a>,
    clock_events: Vec<RefreshPlanWireRef<'a>>,
}

impl Serialize for ContinuousRefreshOwners {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        ContinuousRefreshOwnersWireRef {
            algebraic: (&self.algebraic).into(),
            derivative: (&self.derivative).into(),
            root: (&self.root).into(),
            event: (&self.event).into(),
            clock_events: self.clock_events.iter().map(Into::into).collect(),
        }
        .serialize(serializer)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ContinuousRefreshConstructionError {
    reason: String,
}

impl fmt::Display for ContinuousRefreshConstructionError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.reason)
    }
}

impl std::error::Error for ContinuousRefreshConstructionError {}

/// Sole authority for sequence identities within one refresh-plan owner.
#[derive(Debug)]
struct RefreshSequenceAllocator {
    owner: u32,
    next: u32,
}

impl RefreshSequenceAllocator {
    fn for_owner(owner: usize) -> Result<Self, ContinuousRefreshConstructionError> {
        Ok(Self {
            owner: u32::try_from(owner).map_err(|_| ContinuousRefreshConstructionError {
                reason: "continuous refresh owner count exceeds u32".to_string(),
            })?,
            next: 0,
        })
    }

    fn allocate(&mut self) -> Result<RefreshSequenceId, ContinuousRefreshConstructionError> {
        let ordinal =
            self.next
                .checked_add(1)
                .ok_or_else(|| ContinuousRefreshConstructionError {
                    reason: "continuous refresh sequence count exceeds u32".to_string(),
                })?;
        let encoded = (u64::from(self.owner) << 32) | u64::from(ordinal);
        let identity =
            NonZeroU64::new(encoded).ok_or_else(|| ContinuousRefreshConstructionError {
                reason: "continuous refresh sequence identity is zero".to_string(),
            })?;
        self.next = ordinal;
        Ok(RefreshSequenceId(identity))
    }

    fn issue_plan(
        mut self,
        draft: RefreshPlan,
    ) -> Result<PendingRefreshPlan, ContinuousRefreshConstructionError> {
        let static_causal_sequence = self.allocate()?;
        let dynamic_causal_sequence = self.allocate()?;
        let mut value_stages = Vec::with_capacity(draft.value_stages.len());
        for stage in draft.value_stages {
            let issued = match stage {
                RefreshStage::CausalSeedSweep {
                    static_rows,
                    dynamic_rows,
                } => IssuedRefreshStage::CausalSeedSweep {
                    static_sequence: self.allocate()?,
                    dynamic_sequence: self.allocate()?,
                    static_rows,
                    dynamic_rows,
                },
                RefreshStage::ExactAssignments {
                    static_rows,
                    dynamic_rows,
                } => IssuedRefreshStage::ExactAssignments {
                    static_sequence: self.allocate()?,
                    dynamic_sequence: self.allocate()?,
                    static_rows,
                    dynamic_rows,
                },
                RefreshStage::ProjectionBlock {
                    block_index,
                    plan,
                    seed_rows,
                } => IssuedRefreshStage::ProjectionBlock {
                    seed_sequence: self.allocate()?,
                    block_index,
                    plan,
                    seed_rows,
                },
            };
            value_stages.push(issued);
        }
        Ok(PendingRefreshPlan {
            static_causal_sequence,
            dynamic_causal_sequence,
            simultaneous_plan: draft.simultaneous_plan,
            simultaneous_block_indices: draft.simultaneous_block_indices,
            value_projection_plan: draft.value_projection_plan,
            rows: draft.rows,
            causal_seed_rows: draft.causal_seed_rows,
            static_causal_seed_rows: draft.static_causal_seed_rows,
            dynamic_causal_seed_rows: draft.dynamic_causal_seed_rows,
            value_stages,
        })
    }
}

impl AlgebraicRefreshRow {
    pub fn checked(
        draft: AlgebraicRefreshRowDraft,
    ) -> Result<Self, ContinuousRefreshConstructionError> {
        if draft
            .assignment_target
            .is_some_and(|target| target != draft.target_index)
        {
            return refresh_error(
                "continuous refresh assignment target does not match its owned target".to_string(),
            );
        }
        if draft
            .assignment_shape
            .is_some_and(|shape| shape.target_y_index() != draft.target_index)
        {
            return refresh_error(
                "continuous refresh assignment certificate belongs to another target".to_string(),
            );
        }
        if draft.direct_assignment_certified
            && !matches!(
                draft.assignment_shape,
                Some(TargetAssignmentShape::Direct { .. })
            )
        {
            return refresh_error(
                "continuous refresh direct certificate has no direct assignment shape".to_string(),
            );
        }
        if draft.exact_assignment_certified && draft.assignment_shape.is_none() {
            return refresh_error(
                "continuous refresh exact certificate has no assignment shape".to_string(),
            );
        }
        if draft.direct_assignment_certified && !draft.exact_assignment_certified {
            return refresh_error("continuous refresh direct certificate is not exact".to_string());
        }
        if draft.assignment_target.is_none()
            && (draft.assignment_shape.is_some()
                || draft.direct_assignment_certified
                || draft.exact_assignment_certified)
        {
            return refresh_error(
                "continuous refresh row without an assignment target carries an assignment certificate"
                    .to_string(),
            );
        }
        Ok(Self {
            owner_id: draft.owner_id,
            source: draft.source,
            equation_index: draft.equation_index,
            output_offset: draft.output_offset,
            target_index: draft.target_index,
            assignment_target: draft.assignment_target,
            assignment_shape: draft.assignment_shape,
            direct_assignment_certified: draft.direct_assignment_certified,
            exact_assignment_certified: draft.exact_assignment_certified,
        })
    }

    pub const fn owner_id(&self) -> RefreshRowOwnerId {
        self.owner_id
    }

    pub const fn source(&self) -> RefreshScalarProgramSource {
        self.source
    }

    pub const fn equation_index(&self) -> usize {
        self.equation_index
    }

    pub const fn output_offset(&self) -> usize {
        self.output_offset
    }

    pub const fn target_index(&self) -> usize {
        self.target_index
    }

    pub const fn assignment_target(&self) -> Option<usize> {
        self.assignment_target
    }

    pub const fn assignment_shape(&self) -> Option<TargetAssignmentShape> {
        self.assignment_shape
    }

    pub const fn direct_assignment_certified(&self) -> bool {
        self.direct_assignment_certified
    }

    pub const fn exact_assignment_certified(&self) -> bool {
        self.exact_assignment_certified
    }
}

impl<'de> Deserialize<'de> for AlgebraicRefreshRow {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let wire = AlgebraicRefreshRowWire::deserialize(deserializer)?;
        Self::checked(AlgebraicRefreshRowDraft {
            owner_id: wire.owner_id,
            source: wire.source,
            equation_index: wire.equation_index,
            output_offset: wire.output_offset,
            target_index: wire.target_index,
            assignment_target: wire.assignment_target,
            assignment_shape: wire.assignment_shape,
            direct_assignment_certified: wire.direct_assignment_certified,
            exact_assignment_certified: wire.exact_assignment_certified,
        })
        .map_err(serde::de::Error::custom)
    }
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
pub(crate) struct ContinuousRefreshOwnersWire {
    algebraic: RefreshPlan,
    derivative: RefreshPlan,
    root: RefreshPlan,
    event: RefreshPlan,
    clock_events: Vec<RefreshPlan>,
}

impl ContinuousRefreshOwnersWire {
    pub(crate) fn into_inputs(self) -> ContinuousRefreshPlanInputs {
        ContinuousRefreshPlanInputs::new(
            self.algebraic,
            self.derivative,
            self.root,
            self.event,
            self.clock_events,
        )
    }
}

/// Unissued continuous refresh plans consumed atomically with their exact
/// canonical continuous programs.
///
/// This bundle contains no executable owner identity. Only
/// [`ContinuousSolveSystem::construct`](crate::ContinuousSolveSystem::construct)
/// can correlate and issue it.
///
/// ```compile_fail
/// use rumoca_ir_solve::ContinuousRefreshPlanInputs;
///
/// let _ = ContinuousRefreshPlanInputs::default();
/// ```
#[derive(Clone, Debug)]
pub struct ContinuousRefreshPlanInputs {
    algebraic: RefreshPlan,
    derivative: RefreshPlan,
    root: RefreshPlan,
    event: RefreshPlan,
    clock_events: Vec<RefreshPlan>,
}

impl ContinuousRefreshPlanInputs {
    #[must_use]
    pub fn new(
        algebraic: RefreshPlan,
        derivative: RefreshPlan,
        root: RefreshPlan,
        event: RefreshPlan,
        clock_events: Vec<RefreshPlan>,
    ) -> Self {
        Self {
            algebraic,
            derivative,
            root,
            event,
            clock_events,
        }
    }

    /// Explicit absence of continuous refresh work for checked fixtures and
    /// genuinely empty continuous systems.
    #[must_use]
    pub fn empty() -> Self {
        Self::new(
            RefreshPlan::empty(),
            RefreshPlan::empty(),
            RefreshPlan::empty(),
            RefreshPlan::empty(),
            Vec::new(),
        )
    }
}

fn validate_canonical_row_owners<'a>(
    plans: impl Iterator<Item = &'a RefreshPlan>,
) -> Result<(), ContinuousRefreshConstructionError> {
    let mut rows = BTreeMap::new();
    for row in plans.flat_map(|plan| plan.rows.iter()) {
        if let Some(existing) = rows.insert(row.owner_id, row)
            && existing != row
        {
            return refresh_error(
                "continuous refresh plans disagree on a canonical row owner".to_string(),
            );
        }
    }
    Ok(())
}

fn validate_row_assignment_program(
    row: &AlgebraicRefreshRow,
    programs: &[ExactRefreshAssignmentProgram],
) -> Result<(), ContinuousRefreshConstructionError> {
    let program = programs
        .iter()
        .find(|program| program.row_owners.contains(&row.owner_id));
    if !row.exact_assignment_certified {
        if program.is_some() {
            return refresh_error(
                "non-exact continuous refresh row owns an exact assignment program".to_string(),
            );
        }
        return Ok(());
    }
    let Some(program) = program else {
        return refresh_error(
            "exact continuous refresh row has no constructed assignment program".to_string(),
        );
    };
    let Some(position) = program
        .row_owners
        .iter()
        .position(|owner| *owner == row.owner_id)
    else {
        return refresh_error("exact continuous refresh program lost its row owner".to_string());
    };
    if program.source != row.source
        || program.target_indices.get(position) != Some(&row.target_index)
    {
        return refresh_error(
            "exact continuous refresh program does not replay its row owner".to_string(),
        );
    }
    Ok(())
}

struct ContinuousRefreshDrafts {
    algebraic: RefreshPlan,
    derivative: RefreshPlan,
    root: RefreshPlan,
    event: RefreshPlan,
    clock_events: Vec<RefreshPlan>,
}

struct PreparedContinuousRefreshDrafts {
    drafts: ContinuousRefreshDrafts,
    static_parameter_indices: Box<[usize]>,
}

impl ContinuousRefreshDrafts {
    fn prepare(
        mut self,
        implicit_rhs: &ComputeBlock,
        implicit_row_targets: &[Option<crate::ScalarSlot>],
        algebraic_projection_plan: &crate::AlgebraicProjectionPlan,
        solve_layout: &crate::SolveLayout,
    ) -> Result<PreparedContinuousRefreshDrafts, ContinuousRefreshConstructionError> {
        for (label, plan) in [
            ("algebraic", &self.algebraic),
            ("derivative", &self.derivative),
            ("root", &self.root),
            ("event", &self.event),
        ] {
            validate_refresh_plan(label, plan)?;
            validate_refresh_sources(
                label,
                plan,
                implicit_rhs,
                implicit_row_targets,
                algebraic_projection_plan,
                solve_layout,
            )?;
        }
        for (clock, plan) in self.clock_events.iter().enumerate() {
            let label = format!("clock event {clock}");
            validate_refresh_plan(&label, plan)?;
            validate_refresh_sources(
                &label,
                plan,
                implicit_rhs,
                implicit_row_targets,
                algebraic_projection_plan,
                solve_layout,
            )?;
        }
        for plan in [
            &mut self.algebraic,
            &mut self.derivative,
            &mut self.root,
            &mut self.event,
        ] {
            derive_static_refresh_partitions(plan, implicit_rhs, solve_layout)?;
        }
        for plan in &mut self.clock_events {
            derive_static_refresh_partitions(plan, implicit_rhs, solve_layout)?;
        }
        let plans = [&self.algebraic, &self.derivative, &self.root, &self.event]
            .into_iter()
            .chain(self.clock_events.iter());
        let static_parameter_indices =
            collect_static_refresh_parameter_indices(plans.clone(), implicit_rhs)?;
        validate_canonical_row_owners(plans)?;
        Ok(PreparedContinuousRefreshDrafts {
            drafts: self,
            static_parameter_indices,
        })
    }
}

#[derive(Default)]
struct ExactAssignmentEvidence {
    programs: Vec<ExactRefreshAssignmentProgram>,
    schedules: Vec<ExactRefreshAssignmentSchedule>,
    inventory: BTreeMap<Vec<RefreshRowOwnerId>, ExactRefreshAssignmentProgramId>,
}

impl ExactAssignmentEvidence {
    fn append(
        &mut self,
        implicit_rhs: &ComputeBlock,
        plan: &PendingRefreshPlan,
    ) -> Result<(), ContinuousRefreshConstructionError> {
        append_plan_assignment_schedules(
            implicit_rhs,
            &mut self.programs,
            &mut self.schedules,
            &mut self.inventory,
            plan,
        )
    }

    fn certify(&self, plan: &PendingRefreshPlan) -> bool {
        causal_assignment_plan_is_certified(plan, &self.programs, &self.schedules)
    }
}

struct IssuedContinuousRefreshBase {
    algebraic: IssuedRefreshPlan,
    derivative: IssuedRefreshPlan,
    root: IssuedRefreshPlan,
    event: IssuedRefreshPlan,
    clock_events: Vec<IssuedRefreshPlan>,
    static_parameter_indices: Box<[usize]>,
    evidence: ExactAssignmentEvidence,
}

impl PreparedContinuousRefreshDrafts {
    fn issue(
        self,
        implicit_rhs: &ComputeBlock,
    ) -> Result<IssuedContinuousRefreshBase, ContinuousRefreshConstructionError> {
        let clock_count = self.drafts.clock_events.len();
        let algebraic =
            RefreshSequenceAllocator::for_owner(0)?.issue_plan(self.drafts.algebraic)?;
        let derivative =
            RefreshSequenceAllocator::for_owner(1)?.issue_plan(self.drafts.derivative)?;
        let root = RefreshSequenceAllocator::for_owner(2)?.issue_plan(self.drafts.root)?;
        let event = RefreshSequenceAllocator::for_owner(3)?.issue_plan(self.drafts.event)?;
        let mut clocks = Vec::with_capacity(clock_count);
        for (clock, plan) in self.drafts.clock_events.into_iter().enumerate() {
            let owner = clock.checked_add(5).ok_or_else(relation_owner_overflow)?;
            clocks.push(RefreshSequenceAllocator::for_owner(owner)?.issue_plan(plan)?);
        }
        let mut evidence = ExactAssignmentEvidence::default();
        for plan in [&algebraic, &derivative, &root, &event]
            .into_iter()
            .chain(clocks.iter())
        {
            evidence.append(implicit_rhs, plan)?;
        }
        if !exact_assignment_stages_are_causal(&algebraic, &evidence.programs, &evidence.schedules)
        {
            return refresh_error("algebraic exact-assignment stages are non-causal".to_string());
        }
        let algebraic = seal_with_evidence(algebraic, &evidence);
        let derivative = seal_with_evidence(derivative, &evidence);
        let root = seal_with_evidence(root, &evidence);
        let event = seal_with_evidence(event, &evidence);
        let clock_events = clocks
            .into_iter()
            .map(|plan| seal_with_evidence(plan, &evidence))
            .collect();
        Ok(IssuedContinuousRefreshBase {
            algebraic,
            derivative,
            root,
            event,
            clock_events,
            static_parameter_indices: self.static_parameter_indices,
            evidence,
        })
    }
}

fn seal_with_evidence(
    plan: PendingRefreshPlan,
    evidence: &ExactAssignmentEvidence,
) -> IssuedRefreshPlan {
    let certified = evidence.certify(&plan);
    plan.seal(certified)
}

struct IssuedRefreshRemainders {
    root_after_derivative: RefreshRemainderRelation,
    algebraic_after_derivative: RefreshRemainderRelation,
    clock_events_after_event: Vec<RefreshRemainderRelation>,
}

impl IssuedContinuousRefreshBase {
    fn issue_remainders(
        &mut self,
        implicit_rhs: &ComputeBlock,
    ) -> Result<IssuedRefreshRemainders, ContinuousRefreshConstructionError> {
        let clock_count = self.clock_events.len();
        let relation_owner_start = 5usize
            .checked_add(clock_count)
            .ok_or_else(relation_owner_overflow)?;
        let root_after_derivative = RefreshSequenceAllocator::for_owner(4)?
            .issue_plan(self.root.value_remainder_after(&self.derivative))?;
        let mut clock_events_after_event = Vec::with_capacity(clock_count);
        for (clock, plan) in self.clock_events.iter().enumerate() {
            let owner = relation_owner_start
                .checked_add(clock)
                .ok_or_else(relation_owner_overflow)?;
            clock_events_after_event.push(
                RefreshSequenceAllocator::for_owner(owner)?
                    .issue_plan(plan.value_remainder_after(&self.event))?,
            );
        }
        let algebraic_owner = relation_owner_start
            .checked_add(clock_count)
            .ok_or_else(relation_owner_overflow)?;
        let algebraic_after_derivative = RefreshSequenceAllocator::for_owner(algebraic_owner)?
            .issue_plan(self.algebraic.value_remainder_after(&self.derivative))?;
        for plan in [&root_after_derivative, &algebraic_after_derivative]
            .into_iter()
            .chain(clock_events_after_event.iter())
        {
            self.evidence.append(implicit_rhs, plan)?;
        }
        Ok(IssuedRefreshRemainders {
            root_after_derivative: seal_remainder(root_after_derivative, &self.evidence),
            algebraic_after_derivative: seal_remainder(algebraic_after_derivative, &self.evidence),
            clock_events_after_event: clock_events_after_event
                .into_iter()
                .map(|plan| seal_remainder(plan, &self.evidence))
                .collect(),
        })
    }

    fn validate_assignment_programs(&self) -> Result<(), ContinuousRefreshConstructionError> {
        for plan in [&self.algebraic, &self.derivative, &self.root, &self.event]
            .into_iter()
            .chain(self.clock_events.iter())
        {
            for row in plan.rows() {
                validate_row_assignment_program(row, &self.evidence.programs)?;
            }
        }
        Ok(())
    }
}

fn relation_owner_overflow() -> ContinuousRefreshConstructionError {
    ContinuousRefreshConstructionError {
        reason: "continuous refresh relation owner count overflows".to_string(),
    }
}

fn seal_remainder(
    plan: PendingRefreshPlan,
    evidence: &ExactAssignmentEvidence,
) -> RefreshRemainderRelation {
    RefreshRemainderRelation {
        remainder: seal_with_evidence(plan, evidence),
    }
}

impl ContinuousRefreshOwners {
    pub(crate) fn exact_assignment_programs(&self) -> &[ExactRefreshAssignmentProgram] {
        &self.exact_assignment_programs
    }

    pub(crate) fn checked_for_source(
        implicit_rhs: &ComputeBlock,
        implicit_row_targets: &[Option<crate::ScalarSlot>],
        algebraic_projection_plan: &crate::AlgebraicProjectionPlan,
        solve_layout: &crate::SolveLayout,
        inputs: ContinuousRefreshPlanInputs,
    ) -> Result<Self, ContinuousRefreshConstructionError> {
        let ContinuousRefreshPlanInputs {
            algebraic,
            derivative,
            root,
            event,
            clock_events,
        } = inputs;
        let drafts = ContinuousRefreshDrafts {
            algebraic,
            derivative,
            root,
            event,
            clock_events,
        };
        let mut base = drafts
            .prepare(
                implicit_rhs,
                implicit_row_targets,
                algebraic_projection_plan,
                solve_layout,
            )?
            .issue(implicit_rhs)?;
        let remainders = base.issue_remainders(implicit_rhs)?;
        base.validate_assignment_programs()?;
        Ok(Self {
            algebraic: base.algebraic,
            derivative: base.derivative,
            root: base.root,
            event: base.event,
            clock_events: base.clock_events,
            static_parameter_indices: base.static_parameter_indices,
            exact_assignment_programs: base.evidence.programs,
            exact_assignment_schedules: base.evidence.schedules,
            root_after_derivative: remainders.root_after_derivative,
            algebraic_after_derivative: remainders.algebraic_after_derivative,
            clock_events_after_event: remainders.clock_events_after_event,
        })
    }

    pub fn exact_assignment_program(
        &self,
        id: ExactRefreshAssignmentProgramId,
    ) -> Option<&ExactRefreshAssignmentProgram> {
        self.exact_assignment_programs
            .iter()
            .find(|program| program.id == id)
    }

    pub fn exact_assignment_schedule(
        &self,
        sequence: RefreshSequenceId,
    ) -> Option<&ExactRefreshAssignmentSchedule> {
        self.exact_assignment_schedules
            .iter()
            .find(|schedule| schedule.sequence_id == sequence)
    }

    #[must_use]
    pub const fn algebraic(&self) -> &IssuedRefreshPlan {
        &self.algebraic
    }

    /// Whether the algebraic owner executes every one of its canonical BLT
    /// blocks through its construction-issued exact-assignment schedules.
    ///
    /// This is a local Solve-owner fact. It deliberately says nothing about a
    /// backend profile or the root solver layout; target preparation combines
    /// it with the authoritative root inventory it consumes.
    #[must_use]
    pub fn algebraic_exact_assignment_stages_cover(&self) -> bool {
        exact_assignment_stages_cover(
            &self.algebraic.plan,
            &self.exact_assignment_programs,
            &self.exact_assignment_schedules,
        )
    }

    #[must_use]
    pub const fn derivative(&self) -> &IssuedRefreshPlan {
        &self.derivative
    }

    #[must_use]
    pub const fn root(&self) -> &IssuedRefreshPlan {
        &self.root
    }

    #[must_use]
    pub const fn event(&self) -> &IssuedRefreshPlan {
        &self.event
    }

    #[must_use]
    pub fn clock_events(&self) -> &[IssuedRefreshPlan] {
        &self.clock_events
    }

    /// Exact P slots read by every construction-certified static refresh row.
    #[must_use]
    pub fn static_parameter_indices(&self) -> &[usize] {
        &self.static_parameter_indices
    }

    #[must_use]
    pub const fn root_after_derivative(&self) -> &RefreshRemainderRelation {
        &self.root_after_derivative
    }

    #[must_use]
    pub const fn algebraic_after_derivative(&self) -> &RefreshRemainderRelation {
        &self.algebraic_after_derivative
    }

    #[must_use]
    pub fn clock_events_after_event(&self) -> &[RefreshRemainderRelation] {
        &self.clock_events_after_event
    }
}

fn validate_refresh_sources(
    label: &str,
    plan: &RefreshPlan,
    implicit_rhs: &ComputeBlock,
    implicit_row_targets: &[Option<crate::ScalarSlot>],
    algebraic_projection_plan: &crate::AlgebraicProjectionPlan,
    solve_layout: &crate::SolveLayout,
) -> Result<(), ContinuousRefreshConstructionError> {
    for row in &plan.rows {
        let Some(equation) =
            scalar_source_output_index(implicit_rhs, row.source, row.output_offset)?
        else {
            return refresh_error(format!(
                "{label} refresh row refers to a missing canonical scalar-program output"
            ));
        };
        if equation != row.equation_index {
            return refresh_error(format!(
                "{label} refresh row source output {equation} does not own equation {}",
                row.equation_index
            ));
        }
        let Some(Some(crate::ScalarSlot::Y {
            index: canonical_target,
            ..
        })) = implicit_row_targets.get(equation)
        else {
            return refresh_error(format!(
                "{label} refresh row has no canonical implicit Y target"
            ));
        };
        if *canonical_target < solve_layout.state_scalar_count()
            || *canonical_target >= solve_layout.solver_scalar_count()
        {
            return refresh_error(format!(
                "{label} refresh row canonical target {canonical_target} leaves algebraic solver Y range {}..{}",
                solve_layout.state_scalar_count(),
                solve_layout.solver_scalar_count()
            ));
        }
        if row.target_index != *canonical_target {
            return refresh_error(format!(
                "{label} refresh row target {} disagrees with canonical implicit target {canonical_target}",
                row.target_index
            ));
        }
        let projection_owns_pair = algebraic_projection_plan.blocks.iter().any(|block| {
            block.rows.contains(&equation) && block.y_indices.contains(canonical_target)
        });
        if !projection_owns_pair {
            return refresh_error(format!(
                "{label} refresh row target is not owned by its canonical algebraic projection"
            ));
        }
        validate_refresh_assignment_certificate(label, row, implicit_rhs)?;
    }
    Ok(())
}

struct ParameterStaticDependencies {
    y: Vec<std::collections::BTreeSet<usize>>,
    parameters: Vec<std::collections::BTreeSet<usize>>,
    time: Vec<bool>,
    seed: Vec<bool>,
    effect: Vec<bool>,
}

impl ParameterStaticDependencies {
    fn derive(program: &[LinearOp]) -> Option<Self> {
        Some(Self {
            y: StructuralPattern::derive_output_y_dependencies(program, None).ok()?,
            parameters: StructuralPattern::derive_output_p_dependencies(program, None).ok()?,
            time: StructuralPattern::derive_output_time_dependencies(program, None).ok()?,
            seed: StructuralPattern::derive_output_seed_dependencies(program, None).ok()?,
            effect: StructuralPattern::derive_output_effect_dependencies(program, None).ok()?,
        })
    }

    fn is_static(
        &self,
        output_offset: usize,
        target: usize,
        state_count: usize,
        static_targets: &std::collections::BTreeSet<usize>,
        static_parameter_prefix: usize,
        homotopy_endpoint: Option<usize>,
    ) -> bool {
        let Some(parameters) = self.parameters.get(output_offset) else {
            return false;
        };
        let parameters_are_static = parameters
            .iter()
            .all(|index| *index < static_parameter_prefix || homotopy_endpoint == Some(*index));
        let Some(solver_values) = self.y.get(output_offset) else {
            return false;
        };
        let solver_values_are_static = solver_values.iter().all(|index| {
            *index == target || (*index >= state_count && static_targets.contains(index))
        });
        parameters_are_static
            && solver_values_are_static
            && self.time.get(output_offset) == Some(&false)
            && self.seed.get(output_offset) == Some(&false)
            && self.effect.get(output_offset) == Some(&false)
    }
}

fn parameter_static_refresh_targets(
    plan: &RefreshPlan,
    block: &ComputeBlock,
    solve_layout: &crate::SolveLayout,
) -> Result<std::collections::BTreeSet<usize>, ContinuousRefreshConstructionError> {
    // The causal row order is the only authority that can make a preceding
    // algebraic value available to a later row. Start with no certified
    // targets and grow the set in that exact order. A mutually dependent pair
    // therefore cannot certify itself through a greatest fixed point.
    let mut static_targets = std::collections::BTreeSet::new();
    for row in plan.causal_rows().iter() {
        let dependencies = scalar_source_program(block, row.source)?
            .and_then(|(program, _)| ParameterStaticDependencies::derive(program));
        if dependencies.as_ref().is_some_and(|dependencies| {
            dependencies.is_static(
                row.output_offset,
                row.target_index,
                solve_layout.state_scalar_count,
                &static_targets,
                solve_layout.parameter_count,
                solve_layout.initial_homotopy_parameter_index,
            )
        }) {
            static_targets.insert(row.target_index);
        }
    }
    Ok(static_targets)
}

fn ordered_stage_rows(
    row_count: usize,
    static_rows: &RefreshRowSelection,
    dynamic_rows: &RefreshRowSelection,
) -> Result<RefreshRowSelection, ContinuousRefreshConstructionError> {
    RefreshRowSelection::checked(
        row_count,
        static_rows
            .indices()
            .iter()
            .chain(dynamic_rows.indices())
            .map(|index| *index as usize),
    )
}

fn partition_refresh_rows(
    row_targets: &[usize],
    rows: &RefreshRowSelection,
    static_targets: &std::collections::BTreeSet<usize>,
) -> Result<(RefreshRowSelection, RefreshRowSelection), ContinuousRefreshConstructionError> {
    let static_rows = RefreshRowSelection::checked(
        row_targets.len(),
        rows.indices()
            .iter()
            .map(|index| *index as usize)
            .filter(|index| static_targets.contains(&row_targets[*index])),
    )?;
    let dynamic_rows = RefreshRowSelection::checked(
        row_targets.len(),
        rows.indices()
            .iter()
            .map(|index| *index as usize)
            .filter(|index| !static_targets.contains(&row_targets[*index])),
    )?;
    Ok((static_rows, dynamic_rows))
}

fn derive_static_refresh_partitions(
    plan: &mut RefreshPlan,
    block: &ComputeBlock,
    solve_layout: &crate::SolveLayout,
) -> Result<(), ContinuousRefreshConstructionError> {
    let static_targets = parameter_static_refresh_targets(plan, block, solve_layout)?;
    let row_targets = plan
        .rows
        .iter()
        .map(AlgebraicRefreshRow::target_index)
        .collect::<Vec<_>>();
    (plan.static_causal_seed_rows, plan.dynamic_causal_seed_rows) =
        partition_refresh_rows(&row_targets, &plan.causal_seed_rows, &static_targets)?;
    for stage in &mut plan.value_stages {
        let (static_rows, dynamic_rows) = match stage {
            RefreshStage::CausalSeedSweep {
                static_rows,
                dynamic_rows,
            }
            | RefreshStage::ExactAssignments {
                static_rows,
                dynamic_rows,
            } => (static_rows, dynamic_rows),
            RefreshStage::ProjectionBlock { .. } => continue,
        };
        let ordered = ordered_stage_rows(row_targets.len(), static_rows, dynamic_rows)?;
        (*static_rows, *dynamic_rows) =
            partition_refresh_rows(&row_targets, &ordered, &static_targets)?;
    }
    Ok(())
}

fn collect_static_refresh_parameter_indices<'a>(
    plans: impl Iterator<Item = &'a RefreshPlan>,
    block: &ComputeBlock,
) -> Result<Box<[usize]>, ContinuousRefreshConstructionError> {
    let mut outputs = std::collections::BTreeSet::new();
    for plan in plans {
        for row in plan.static_causal_rows().iter() {
            outputs.insert((row.source(), row.output_offset()));
        }
        for stage in &plan.value_stages {
            let static_rows = match stage {
                RefreshStage::CausalSeedSweep { static_rows, .. }
                | RefreshStage::ExactAssignments { static_rows, .. } => static_rows,
                RefreshStage::ProjectionBlock { .. } => continue,
            };
            outputs.extend(
                plan.selected_rows(static_rows)
                    .iter()
                    .map(|row| (row.source(), row.output_offset())),
            );
        }
    }
    let mut parameters = std::collections::BTreeSet::new();
    for (source, output_offset) in outputs {
        let Some((program, _)) = scalar_source_program(block, source)? else {
            return refresh_error(
                "static continuous refresh row lost its canonical source".to_string(),
            );
        };
        let dependencies =
            StructuralPattern::derive_output_p_dependencies(program, None).map_err(|error| {
                ContinuousRefreshConstructionError {
                    reason: format!(
                        "static continuous refresh parameter dependency proof failed: {error}"
                    ),
                }
            })?;
        let Some(output_dependencies) = dependencies.get(output_offset) else {
            return refresh_error(
                "static continuous refresh row lost its canonical output dependency".to_string(),
            );
        };
        parameters.extend(output_dependencies.iter().copied());
    }
    Ok(parameters
        .into_iter()
        .collect::<Vec<_>>()
        .into_boxed_slice())
}

fn validate_refresh_assignment_certificate(
    label: &str,
    row: &AlgebraicRefreshRow,
    implicit_rhs: &ComputeBlock,
) -> Result<(), ContinuousRefreshConstructionError> {
    let (program, _) = scalar_source_program(implicit_rhs, row.source)?.ok_or_else(|| {
        ContinuousRefreshConstructionError {
            reason: format!("{label} refresh row refers to a missing canonical source program"),
        }
    })?;
    let derived =
        derive_target_assignment_shape_for_output(program, row.output_offset, row.target_index);
    if row.assignment_shape != derived {
        return refresh_error(format!(
            "{label} refresh row assignment certificate disagrees with its canonical source"
        ));
    }
    let exact = derived.is_some() && !program.iter().any(non_causal_assignment_operation);
    let direct = exact && matches!(derived, Some(TargetAssignmentShape::Direct { .. }));
    if row.exact_assignment_certified != exact || row.direct_assignment_certified != direct {
        return refresh_error(format!(
            "{label} refresh row exact/direct certificate disagrees with its canonical source"
        ));
    }
    Ok(())
}

fn scalar_source_output_index(
    block: &ComputeBlock,
    source: RefreshScalarProgramSource,
    output_offset: usize,
) -> Result<Option<usize>, ContinuousRefreshConstructionError> {
    let source_node = usize::try_from(source.node).map_err(|_| refresh_source_overflow("node"))?;
    let source_program =
        usize::try_from(source.program).map_err(|_| refresh_source_overflow("program"))?;
    let mut output_cursor = 0usize;
    for (node_index, node) in block.nodes.iter().enumerate() {
        if node_index == source_node {
            let ComputeNode::ScalarPrograms(programs) = node else {
                return Ok(None);
            };
            let Some(_program) = programs.programs().get(source_program) else {
                return Ok(None);
            };
            let Some(source_output_count) =
                programs.stored_output_count_for_program(source_program)
            else {
                return Ok(None);
            };
            if output_offset >= source_output_count {
                return Ok(None);
            }
            let preceding_outputs = (0..source_program)
                .try_fold(0usize, |count, program_index| {
                    count.checked_add(programs.stored_output_count_for_program(program_index)?)
                })
                .ok_or_else(|| refresh_source_overflow("output ordinal"))?;
            let ordinal = preceding_outputs
                .checked_add(output_offset)
                .ok_or_else(|| refresh_source_overflow("output ordinal"))?;
            let outputs = programs
                .compute_block_output_indices(
                    "continuous.refresh_owners",
                    node_index,
                    output_cursor,
                )
                .map_err(|error| ContinuousRefreshConstructionError {
                    reason: error.to_string(),
                })?;
            return Ok(outputs.get(ordinal).copied());
        }
        output_cursor = advance_output_cursor(node, node_index, output_cursor)?;
    }
    Ok(None)
}

fn construct_exact_assignment_program(
    block: &ComputeBlock,
    id: ExactRefreshAssignmentProgramId,
    rows: &[&AlgebraicRefreshRow],
) -> Result<ExactRefreshAssignmentProgram, ContinuousRefreshConstructionError> {
    let Some(first) = rows.first() else {
        return refresh_error("exact continuous refresh assignment group is empty".to_string());
    };
    if rows.iter().any(|row| row.source != first.source) {
        return refresh_error(
            "exact continuous refresh assignment group has multiple canonical sources".to_string(),
        );
    }
    let (source_program, span) = scalar_source_program(block, first.source)?.ok_or_else(|| {
        ContinuousRefreshConstructionError {
            reason: "exact continuous refresh source program is missing".to_string(),
        }
    })?;
    let shapes = rows
        .iter()
        .map(|row| {
            row.assignment_shape
                .ok_or_else(|| ContinuousRefreshConstructionError {
                    reason: "exact continuous refresh row has no assignment shape".to_string(),
                })
        })
        .collect::<Result<Vec<_>, _>>()?;
    let prefix_len = shapes
        .iter()
        .map(|shape| shape.expr_eval_len())
        .max()
        .unwrap_or(0);
    source_program
        .get(..prefix_len)
        .ok_or_else(|| ContinuousRefreshConstructionError {
            reason: "exact continuous refresh assignment prefix exceeds its canonical source"
                .to_string(),
        })?;
    let assignment_y_dependencies = assignment_y_dependencies_for_shapes(source_program, &shapes);
    let final_program = materialize_exact_assignment_program(source_program, span, &shapes)?;
    let target_indices = rows
        .iter()
        .map(|row| row.target_index)
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let final_execution = final_program.sole_execution_program().ok_or_else(|| {
        ContinuousRefreshConstructionError {
            reason: "exact continuous refresh assignment must issue one final program".to_string(),
        }
    })?;
    if final_execution.output_sources().len() != target_indices.len() {
        return refresh_error(
            "exact continuous refresh assignment output projection does not cover its targets"
                .to_string(),
        );
    }
    Ok(ExactRefreshAssignmentProgram {
        id,
        row_owners: rows
            .iter()
            .map(|row| row.owner_id)
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        source: first.source,
        target_indices,
        assignment_shapes: shapes.into_boxed_slice(),
        assignment_y_dependencies,
        final_program,
    })
}

fn materialize_exact_assignment_program(
    source_program: &[LinearOp],
    span: rumoca_core::Span,
    assignment_shapes: &[TargetAssignmentShape],
) -> Result<ScalarProgramBlock, ContinuousRefreshConstructionError> {
    let prefix_len = assignment_shapes
        .iter()
        .map(|shape| shape.expr_eval_len())
        .max()
        .unwrap_or(0);
    let prefix =
        source_program
            .get(..prefix_len)
            .ok_or_else(|| ContinuousRefreshConstructionError {
                reason: "exact continuous refresh assignment prefix exceeds its canonical source"
                    .to_string(),
            })?;
    let mut operations = prefix
        .iter()
        .filter(|operation| {
            !matches!(
                operation,
                LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. }
            )
        })
        .cloned()
        .collect::<Vec<_>>();
    let mut builder = ExactAssignmentProgramBuilder::new(&mut operations).ok_or_else(|| {
        ContinuousRefreshConstructionError {
            reason: "exact continuous refresh assignment program overflows registers".to_string(),
        }
    })?;
    for shape in assignment_shapes.iter().copied() {
        let result =
            builder
                .materialize(shape)
                .ok_or_else(|| ContinuousRefreshConstructionError {
                    reason: "exact continuous refresh assignment program overflows registers"
                        .to_string(),
                })?;
        builder
            .operations
            .push(LinearOp::StoreOutput { src: result });
    }
    let provenance = rumoca_core::ProvenanceSpan::new(span, "continuous refresh assignment")
        .map_err(|error| ContinuousRefreshConstructionError {
            reason: error.to_string(),
        })?;
    ScalarProgramBlock::with_source_span(vec![operations], provenance).map_err(|error| {
        ContinuousRefreshConstructionError {
            reason: error.to_string(),
        }
    })
}

fn scalar_source_program(
    block: &ComputeBlock,
    source: RefreshScalarProgramSource,
) -> Result<Option<(&[LinearOp], rumoca_core::Span)>, ContinuousRefreshConstructionError> {
    let node = usize::try_from(source.node).map_err(|_| refresh_source_overflow("node"))?;
    let program =
        usize::try_from(source.program).map_err(|_| refresh_source_overflow("program"))?;
    let Some(ComputeNode::ScalarPrograms(programs)) = block.nodes.get(node) else {
        return Ok(None);
    };
    let Some(operations) = programs.programs().get(program) else {
        return Ok(None);
    };
    let Some(span) = programs.program_span(program) else {
        return refresh_error("continuous refresh source program has no provenance".to_string());
    };
    Ok(Some((operations, span)))
}

fn exact_rows_can_commit_together(
    block: &ComputeBlock,
    rows: &[&AlgebraicRefreshRow],
) -> Result<bool, ContinuousRefreshConstructionError> {
    let Some(first) = rows.first() else {
        return Ok(false);
    };
    let Some((program, _)) = scalar_source_program(block, first.source)? else {
        return Ok(false);
    };
    let dependencies = ScalarProgramYDependency::new(program);
    for row in rows {
        let Some(shape) = row.assignment_shape else {
            return Ok(false);
        };
        for other in rows {
            if other.owner_id != row.owner_id
                && shape_value_registers(shape)
                    .into_iter()
                    .flatten()
                    .any(|register| dependencies.depends_on(register, other.target_index))
            {
                return Ok(false);
            }
        }
    }
    Ok(true)
}

struct ExactAssignmentProgramBuilder<'a> {
    operations: &'a mut Vec<LinearOp>,
    next_register: u32,
}

impl<'a> ExactAssignmentProgramBuilder<'a> {
    fn new(operations: &'a mut Vec<LinearOp>) -> Option<Self> {
        let next_register = operations
            .iter()
            .filter_map(LinearOp::dst_register)
            .max()
            .map_or(Some(0), |register| register.checked_add(1))?;
        Some(Self {
            operations,
            next_register,
        })
    }

    fn materialize(&mut self, shape: TargetAssignmentShape) -> Option<u32> {
        match shape {
            TargetAssignmentShape::Direct { expr_reg, .. } => Some(expr_reg),
            TargetAssignmentShape::Affine {
                offset_reg,
                coefficient_reg,
                offset_scale,
                coefficient_scale,
                ..
            } => self.affine(offset_reg, coefficient_reg, offset_scale, coefficient_scale),
            TargetAssignmentShape::AffineResidual {
                target_reg,
                residual_reg,
                coefficient,
                ..
            } => self.affine_residual(target_reg, residual_reg, coefficient),
        }
    }

    fn affine(
        &mut self,
        offset: u32,
        coefficient: Option<u32>,
        offset_scale: f64,
        coefficient_scale: f64,
    ) -> Option<u32> {
        let offset_scale_reg = self.allocate()?;
        let scaled_offset = self.allocate()?;
        let coefficient_scale_reg = self.allocate()?;
        let scaled_coefficient = self.allocate()?;
        let negated_offset = self.allocate()?;
        let result = self.allocate()?;
        self.operations.push(LinearOp::Const {
            dst: offset_scale_reg,
            value: offset_scale,
        });
        self.operations.push(LinearOp::Binary {
            dst: scaled_offset,
            op: BinaryOp::Mul,
            lhs: offset_scale_reg,
            rhs: offset,
        });
        self.operations.push(LinearOp::Const {
            dst: coefficient_scale_reg,
            value: coefficient_scale,
        });
        self.operations.push(match coefficient {
            Some(coefficient) => LinearOp::Binary {
                dst: scaled_coefficient,
                op: BinaryOp::Mul,
                lhs: coefficient_scale_reg,
                rhs: coefficient,
            },
            None => LinearOp::Move {
                dst: scaled_coefficient,
                src: coefficient_scale_reg,
            },
        });
        self.operations.push(LinearOp::Unary {
            dst: negated_offset,
            op: UnaryOp::Neg,
            arg: scaled_offset,
        });
        self.operations.push(LinearOp::Binary {
            dst: result,
            op: BinaryOp::Div,
            lhs: negated_offset,
            rhs: scaled_coefficient,
        });
        Some(result)
    }

    fn affine_residual(&mut self, target: u32, residual: u32, coefficient: f64) -> Option<u32> {
        let coefficient_reg = self.allocate()?;
        let correction = self.allocate()?;
        let result = self.allocate()?;
        self.operations.push(LinearOp::Const {
            dst: coefficient_reg,
            value: coefficient,
        });
        self.operations.push(LinearOp::Binary {
            dst: correction,
            op: BinaryOp::Div,
            lhs: residual,
            rhs: coefficient_reg,
        });
        self.operations.push(LinearOp::Binary {
            dst: result,
            op: BinaryOp::Sub,
            lhs: target,
            rhs: correction,
        });
        Some(result)
    }

    fn allocate(&mut self) -> Option<u32> {
        let register = self.next_register;
        self.next_register = self.next_register.checked_add(1)?;
        Some(register)
    }
}

fn advance_output_cursor(
    node: &ComputeNode,
    node_index: usize,
    output_cursor: usize,
) -> Result<usize, ContinuousRefreshConstructionError> {
    match node {
        ComputeNode::ScalarPrograms(programs) => {
            let outputs = programs
                .compute_block_output_indices(
                    "continuous.refresh_owners",
                    node_index,
                    output_cursor,
                )
                .map_err(|error| ContinuousRefreshConstructionError {
                    reason: error.to_string(),
                })?;
            let Some(last) = outputs.into_iter().max() else {
                return Ok(output_cursor);
            };
            let end = last
                .checked_add(1)
                .ok_or_else(|| refresh_source_overflow("scalar output range"))?;
            Ok(output_cursor.max(end))
        }
        ComputeNode::MatMul { m, n, .. } => m
            .checked_mul(*n)
            .and_then(|count| output_cursor.checked_add(count))
            .ok_or_else(|| refresh_source_overflow("matrix output range")),
        ComputeNode::LinSolve { n, .. } => output_cursor
            .checked_add(*n)
            .ok_or_else(|| refresh_source_overflow("linear-solve output range")),
        ComputeNode::Map {
            domain, output_map, ..
        }
        | ComputeNode::AffineStencil {
            domain, output_map, ..
        } => output_map
            .output_count(domain)
            .map(|count| output_cursor.max(count))
            .map_err(|error| ContinuousRefreshConstructionError {
                reason: format!("continuous refresh tensor output map is invalid: {error:?}"),
            }),
    }
}

fn refresh_source_overflow(part: &str) -> ContinuousRefreshConstructionError {
    ContinuousRefreshConstructionError {
        reason: format!("continuous refresh source {part} overflows"),
    }
}

fn append_plan_assignment_schedules(
    block: &ComputeBlock,
    programs: &mut Vec<ExactRefreshAssignmentProgram>,
    schedules: &mut Vec<ExactRefreshAssignmentSchedule>,
    inventory: &mut BTreeMap<Vec<RefreshRowOwnerId>, ExactRefreshAssignmentProgramId>,
    plan: &PendingRefreshPlan,
) -> Result<(), ContinuousRefreshConstructionError> {
    append_exact_assignment_schedule(
        block,
        programs,
        schedules,
        inventory,
        plan.static_causal_sequence,
        plan.static_causal_rows(),
    )?;
    append_exact_assignment_schedule(
        block,
        programs,
        schedules,
        inventory,
        plan.dynamic_causal_sequence,
        plan.dynamic_causal_rows(),
    )?;
    for stage in &plan.value_stages {
        match stage {
            IssuedRefreshStage::CausalSeedSweep {
                static_sequence,
                dynamic_sequence,
                static_rows,
                dynamic_rows,
            }
            | IssuedRefreshStage::ExactAssignments {
                static_sequence,
                dynamic_sequence,
                static_rows,
                dynamic_rows,
            } => {
                append_exact_assignment_schedule(
                    block,
                    programs,
                    schedules,
                    inventory,
                    *static_sequence,
                    plan.selected_rows(static_rows),
                )?;
                append_exact_assignment_schedule(
                    block,
                    programs,
                    schedules,
                    inventory,
                    *dynamic_sequence,
                    plan.selected_rows(dynamic_rows),
                )?;
            }
            IssuedRefreshStage::ProjectionBlock {
                seed_sequence,
                seed_rows,
                ..
            } => append_exact_assignment_schedule(
                block,
                programs,
                schedules,
                inventory,
                *seed_sequence,
                plan.selected_rows(seed_rows),
            )?,
        }
    }
    Ok(())
}

fn exact_assignment_stages_cover(
    plan: &PendingRefreshPlan,
    programs: &[ExactRefreshAssignmentProgram],
    schedules: &[ExactRefreshAssignmentSchedule],
) -> bool {
    if plan.rows.is_empty() || plan.requires_projection() {
        return false;
    }
    let Some(expected) = exact_singleton_row_owners(plan) else {
        return false;
    };
    exact_assignment_stage_coverage(plan, programs, schedules)
        .is_some_and(|covered| covered == expected)
}

fn causal_assignment_plan_is_certified(
    plan: &PendingRefreshPlan,
    programs: &[ExactRefreshAssignmentProgram],
    schedules: &[ExactRefreshAssignmentSchedule],
) -> bool {
    if plan.rows.is_empty() {
        return false;
    }
    let Some(expected) = exact_singleton_row_owners(plan) else {
        return false;
    };
    let mut coverage = ExactAssignmentCoverage::new(plan, programs, schedules);
    coverage.mark_selection(plan.static_causal_sequence, &plan.static_causal_seed_rows)
        && coverage.mark_selection(plan.dynamic_causal_sequence, &plan.dynamic_causal_seed_rows)
        && coverage.covered == expected
}

fn exact_assignment_stages_are_causal(
    plan: &PendingRefreshPlan,
    programs: &[ExactRefreshAssignmentProgram],
    schedules: &[ExactRefreshAssignmentSchedule],
) -> bool {
    exact_assignment_stage_coverage(plan, programs, schedules).is_some()
}

fn exact_assignment_stage_coverage(
    plan: &PendingRefreshPlan,
    programs: &[ExactRefreshAssignmentProgram],
    schedules: &[ExactRefreshAssignmentSchedule],
) -> Option<std::collections::BTreeSet<RefreshRowOwnerId>> {
    let mut coverage = ExactAssignmentCoverage::new(plan, programs, schedules);
    for stage in &plan.value_stages {
        if !coverage.mark_stage(stage) {
            return None;
        }
    }
    Some(coverage.covered)
}

fn exact_singleton_row_owners(
    plan: &PendingRefreshPlan,
) -> Option<std::collections::BTreeSet<RefreshRowOwnerId>> {
    let mut owners = std::collections::BTreeSet::new();
    for block in &plan.simultaneous_plan.blocks {
        let ([equation], [target]) = (block.rows.as_slice(), block.y_indices.as_slice()) else {
            return None;
        };
        let mut matches = plan
            .rows
            .iter()
            .filter(|row| row.equation_index == *equation && row.target_index == *target);
        let row = matches.next()?;
        if matches.next().is_some() || !owners.insert(row.owner_id) {
            return None;
        }
    }
    (owners.len() == plan.rows.len()).then_some(owners)
}

struct ExactAssignmentCoverage<'a> {
    plan: &'a PendingRefreshPlan,
    programs: &'a [ExactRefreshAssignmentProgram],
    schedules: &'a [ExactRefreshAssignmentSchedule],
    target_inventory: std::collections::BTreeSet<usize>,
    covered: std::collections::BTreeSet<RefreshRowOwnerId>,
    available: std::collections::BTreeSet<usize>,
}

impl<'a> ExactAssignmentCoverage<'a> {
    fn new(
        plan: &'a PendingRefreshPlan,
        programs: &'a [ExactRefreshAssignmentProgram],
        schedules: &'a [ExactRefreshAssignmentSchedule],
    ) -> Self {
        let target_inventory = plan
            .simultaneous_plan
            .blocks
            .iter()
            .flat_map(|block| block.y_indices.iter().copied())
            .collect();
        Self {
            plan,
            programs,
            schedules,
            target_inventory,
            covered: std::collections::BTreeSet::new(),
            available: std::collections::BTreeSet::new(),
        }
    }

    fn mark_stage(&mut self, stage: &IssuedRefreshStage) -> bool {
        match stage {
            IssuedRefreshStage::CausalSeedSweep { .. } => true,
            IssuedRefreshStage::ExactAssignments {
                static_sequence,
                dynamic_sequence,
                static_rows,
                dynamic_rows,
            } => [
                (*static_sequence, static_rows),
                (*dynamic_sequence, dynamic_rows),
            ]
            .into_iter()
            .all(|(sequence, selection)| self.mark_selection(sequence, selection)),
            IssuedRefreshStage::ProjectionBlock { plan, .. } => plan
                .blocks
                .iter()
                .flat_map(|block| block.y_indices.iter().copied())
                .all(|target| {
                    self.target_inventory.contains(&target) && self.available.insert(target)
                }),
        }
    }

    fn mark_selection(
        &mut self,
        sequence: RefreshSequenceId,
        selection: &RefreshRowSelection,
    ) -> bool {
        let schedule = self
            .schedules
            .iter()
            .find(|schedule| schedule.sequence_id() == sequence);
        if selection.is_empty() {
            return schedule.is_none();
        }
        let Some(schedule) = schedule else {
            return false;
        };
        let mut actual = Vec::new();
        for id in schedule.program_ids() {
            let Some(program) = self.programs.iter().find(|program| program.id() == *id) else {
                return false;
            };
            if !exact_assignment_program_is_causal(
                program,
                &self.target_inventory,
                &mut self.available,
            ) {
                return false;
            }
            actual.extend_from_slice(program.row_owners());
            if program
                .row_owners()
                .iter()
                .any(|owner| !self.covered.insert(*owner))
            {
                return false;
            }
        }
        let expected = self
            .plan
            .selected_rows(selection)
            .iter()
            .map(AlgebraicRefreshRow::owner_id)
            .collect::<Vec<_>>();
        actual == expected
    }
}

fn exact_assignment_program_is_causal(
    program: &ExactRefreshAssignmentProgram,
    target_inventory: &std::collections::BTreeSet<usize>,
    available: &mut std::collections::BTreeSet<usize>,
) -> bool {
    for (position, target) in program.target_indices().iter().copied().enumerate() {
        let Some(dependencies) = program.assignment_y_dependencies(position) else {
            return false;
        };
        if dependencies.iter().copied().any(|dependency| {
            dependency != target
                && target_inventory.contains(&dependency)
                && !available.contains(&dependency)
        }) {
            return false;
        }
    }
    program
        .target_indices()
        .iter()
        .all(|target| available.insert(*target))
}

fn append_exact_assignment_schedule(
    block: &ComputeBlock,
    programs: &mut Vec<ExactRefreshAssignmentProgram>,
    schedules: &mut Vec<ExactRefreshAssignmentSchedule>,
    inventory: &mut BTreeMap<Vec<RefreshRowOwnerId>, ExactRefreshAssignmentProgramId>,
    sequence_id: RefreshSequenceId,
    rows: RefreshRows<'_>,
) -> Result<(), ContinuousRefreshConstructionError> {
    if rows.is_empty() || rows.iter().any(|row| !row.exact_assignment_certified) {
        return Ok(());
    }
    if schedules
        .iter()
        .any(|schedule| schedule.sequence_id == sequence_id)
    {
        return refresh_error(
            "continuous refresh construction issued a duplicate assignment sequence".to_string(),
        );
    }
    let mut program_ids = Vec::new();
    program_ids
        .try_reserve_exact(rows.len())
        .map_err(|_| ContinuousRefreshConstructionError {
            reason: "continuous refresh assignment schedule exceeds memory".to_string(),
        })?;
    let mut position = 0usize;
    while position < rows.len() {
        let source = rows[position].source;
        let mut end = position + 1;
        while end < rows.len() && rows[end].source == source {
            end += 1;
        }
        let source_rows = (position..end)
            .map(|position| &rows[position])
            .collect::<Vec<_>>();
        if source_rows.len() > 1 && exact_rows_can_commit_together(block, &source_rows)? {
            append_exact_assignment_program(
                block,
                programs,
                inventory,
                &source_rows,
                &mut program_ids,
            )?;
        } else {
            for row in source_rows {
                append_exact_assignment_program(
                    block,
                    programs,
                    inventory,
                    &[row],
                    &mut program_ids,
                )?;
            }
        }
        position = end;
    }
    schedules.push(ExactRefreshAssignmentSchedule {
        sequence_id,
        program_ids: program_ids.into_boxed_slice(),
    });
    Ok(())
}

fn append_exact_assignment_program(
    block: &ComputeBlock,
    programs: &mut Vec<ExactRefreshAssignmentProgram>,
    inventory: &mut BTreeMap<Vec<RefreshRowOwnerId>, ExactRefreshAssignmentProgramId>,
    rows: &[&AlgebraicRefreshRow],
    schedule: &mut Vec<ExactRefreshAssignmentProgramId>,
) -> Result<(), ContinuousRefreshConstructionError> {
    let key = rows.iter().map(|row| row.owner_id).collect::<Vec<_>>();
    let id = if let Some(id) = inventory.get(&key).copied() {
        id
    } else {
        let id = ExactRefreshAssignmentProgramId(u32::try_from(programs.len()).map_err(|_| {
            ContinuousRefreshConstructionError {
                reason: "continuous refresh assignment program count exceeds u32".to_string(),
            }
        })?);
        programs.push(construct_exact_assignment_program(block, id, rows)?);
        inventory.insert(key, id);
        id
    };
    schedule.push(id);
    Ok(())
}

fn validate_refresh_plan(
    label: &str,
    plan: &RefreshPlan,
) -> Result<(), ContinuousRefreshConstructionError> {
    if plan.simultaneous_plan.blocks.len() != plan.simultaneous_block_indices.len() {
        return refresh_error(format!(
            "{label} refresh owner has {} BLT blocks but {} canonical block identities",
            plan.simultaneous_plan.blocks.len(),
            plan.simultaneous_block_indices.len()
        ));
    }
    let rows = plan
        .rows
        .iter()
        .map(|row| (row.owner_id, row))
        .collect::<BTreeMap<_, _>>();
    if rows.len() != plan.rows.len() {
        return refresh_error(format!(
            "{label} refresh owner repeats a canonical row identity"
        ));
    }
    for row in &plan.rows {
        validate_refresh_row(label, row)?;
    }
    validate_refresh_row_selection(label, "causal", &plan.causal_seed_rows, &plan.rows)?;
    validate_refresh_row_selection(label, "static", &plan.static_causal_seed_rows, &plan.rows)?;
    validate_refresh_row_selection(label, "dynamic", &plan.dynamic_causal_seed_rows, &plan.rows)?;
    for stage in &plan.value_stages {
        match stage {
            RefreshStage::CausalSeedSweep {
                static_rows,
                dynamic_rows,
                ..
            } => {
                validate_refresh_row_selection(label, "stage static", static_rows, &plan.rows)?;
                validate_refresh_row_selection(label, "stage dynamic", dynamic_rows, &plan.rows)?;
            }
            RefreshStage::ExactAssignments {
                static_rows,
                dynamic_rows,
                ..
            } => {
                validate_exact_assignment_stage(label, plan, static_rows, dynamic_rows)?;
            }
            RefreshStage::ProjectionBlock {
                block_index,
                plan: stage_plan,
                seed_rows,
                ..
            } => {
                let Some(local) = plan
                    .simultaneous_block_indices
                    .iter()
                    .position(|candidate| candidate == block_index)
                else {
                    return refresh_error(format!(
                        "{label} refresh stage refers to unowned BLT block {block_index}"
                    ));
                };
                if stage_plan.blocks.as_slice()
                    != std::slice::from_ref(&plan.simultaneous_plan.blocks[local])
                {
                    return refresh_error(format!(
                        "{label} refresh stage does not replay canonical BLT block {block_index}"
                    ));
                }
                validate_refresh_row_selection(label, "projection seed", seed_rows, &plan.rows)?;
            }
        }
    }
    Ok(())
}

fn validate_exact_assignment_stage(
    label: &str,
    plan: &RefreshPlan,
    static_rows: &RefreshRowSelection,
    dynamic_rows: &RefreshRowSelection,
) -> Result<(), ContinuousRefreshConstructionError> {
    validate_refresh_row_selection(label, "stage static", static_rows, &plan.rows)?;
    validate_refresh_row_selection(label, "stage dynamic", dynamic_rows, &plan.rows)?;
    let all_exact = plan
        .selected_rows(static_rows)
        .iter()
        .chain(plan.selected_rows(dynamic_rows).iter())
        .all(|row| row.exact_assignment_certified);
    if !all_exact {
        return refresh_error(format!(
            "{label} exact-assignment stage selects a non-exact row"
        ));
    }
    Ok(())
}

fn validate_refresh_row(
    label: &str,
    row: &AlgebraicRefreshRow,
) -> Result<(), ContinuousRefreshConstructionError> {
    if row
        .assignment_shape
        .is_some_and(|shape| shape.target_y_index() != row.target_index)
    {
        return refresh_error(format!(
            "{label} refresh row has an assignment certificate for another target"
        ));
    }
    Ok(())
}

fn validate_refresh_row_selection(
    label: &str,
    selection: &str,
    selected: &RefreshRowSelection,
    rows: &[AlgebraicRefreshRow],
) -> Result<(), ContinuousRefreshConstructionError> {
    let mut seen = vec![false; rows.len()];
    for &index in selected.indices() {
        let Ok(index) = usize::try_from(index) else {
            return refresh_error(format!(
                "{label} {selection} row refers to an unowned canonical identity"
            ));
        };
        let Some(seen) = seen.get_mut(index) else {
            return refresh_error(format!(
                "{label} {selection} row refers to an unowned canonical identity"
            ));
        };
        if std::mem::replace(seen, true) {
            return refresh_error(format!(
                "{label} {selection} row repeats its canonical identity"
            ));
        }
    }
    Ok(())
}

fn refresh_error<T>(reason: String) -> Result<T, ContinuousRefreshConstructionError> {
    Err(ContinuousRefreshConstructionError { reason })
}

impl RefreshRemainderRelation {
    #[must_use]
    pub const fn remainder(&self) -> &IssuedRefreshPlan {
        &self.remainder
    }
}

impl RefreshPlan {
    #[must_use]
    pub fn selected_rows<'a>(&'a self, selection: &'a RefreshRowSelection) -> RefreshRows<'a> {
        RefreshRows {
            catalog: &self.rows,
            indices: selection.indices(),
        }
    }

    #[must_use]
    pub fn causal_rows(&self) -> RefreshRows<'_> {
        self.selected_rows(&self.causal_seed_rows)
    }

    #[must_use]
    pub fn static_causal_rows(&self) -> RefreshRows<'_> {
        self.selected_rows(&self.static_causal_seed_rows)
    }

    #[must_use]
    pub fn dynamic_causal_rows(&self) -> RefreshRows<'_> {
        self.selected_rows(&self.dynamic_causal_seed_rows)
    }
}

impl PendingRefreshPlan {
    fn seal(self, causal_solution_certified: bool) -> IssuedRefreshPlan {
        IssuedRefreshPlan {
            plan: self,
            causal_solution_certified,
        }
    }

    fn requires_projection(&self) -> bool {
        self.value_stages
            .iter()
            .any(|stage| matches!(stage, IssuedRefreshStage::ProjectionBlock { .. }))
    }

    fn selected_rows<'a>(&'a self, selection: &'a RefreshRowSelection) -> RefreshRows<'a> {
        RefreshRows {
            catalog: &self.rows,
            indices: selection.indices(),
        }
    }

    fn static_causal_rows(&self) -> RefreshRows<'_> {
        self.selected_rows(&self.static_causal_seed_rows)
    }

    fn dynamic_causal_rows(&self) -> RefreshRows<'_> {
        self.selected_rows(&self.dynamic_causal_seed_rows)
    }
}

impl IssuedRefreshPlan {
    #[cfg(test)]
    fn requires_projection(&self) -> bool {
        self.plan.requires_projection()
    }

    #[must_use]
    pub const fn static_causal_sequence(&self) -> RefreshSequenceId {
        self.plan.static_causal_sequence
    }

    #[must_use]
    pub const fn dynamic_causal_sequence(&self) -> RefreshSequenceId {
        self.plan.dynamic_causal_sequence
    }

    #[must_use]
    pub const fn simultaneous_plan(&self) -> &AlgebraicProjectionPlan {
        &self.plan.simultaneous_plan
    }

    #[must_use]
    pub fn simultaneous_block_indices(&self) -> &[usize] {
        &self.plan.simultaneous_block_indices
    }

    #[must_use]
    pub const fn value_projection_plan(&self) -> &AlgebraicProjectionPlan {
        &self.plan.value_projection_plan
    }

    #[must_use]
    pub fn rows(&self) -> &[AlgebraicRefreshRow] {
        &self.plan.rows
    }

    #[must_use]
    pub const fn causal_seed_rows(&self) -> &RefreshRowSelection {
        &self.plan.causal_seed_rows
    }

    #[must_use]
    pub const fn static_causal_seed_rows(&self) -> &RefreshRowSelection {
        &self.plan.static_causal_seed_rows
    }

    #[must_use]
    pub const fn dynamic_causal_seed_rows(&self) -> &RefreshRowSelection {
        &self.plan.dynamic_causal_seed_rows
    }

    #[must_use]
    pub fn value_stages(&self) -> &[IssuedRefreshStage] {
        &self.plan.value_stages
    }

    #[must_use]
    pub const fn causal_solution_certified(&self) -> bool {
        self.causal_solution_certified
    }

    #[must_use]
    pub fn selected_rows<'a>(&'a self, selection: &'a RefreshRowSelection) -> RefreshRows<'a> {
        RefreshRows {
            catalog: &self.plan.rows,
            indices: selection.indices(),
        }
    }

    #[must_use]
    pub fn causal_rows(&self) -> RefreshRows<'_> {
        self.selected_rows(&self.plan.causal_seed_rows)
    }

    #[must_use]
    pub fn static_causal_rows(&self) -> RefreshRows<'_> {
        self.selected_rows(&self.plan.static_causal_seed_rows)
    }

    #[must_use]
    pub fn dynamic_causal_rows(&self) -> RefreshRows<'_> {
        self.selected_rows(&self.plan.dynamic_causal_seed_rows)
    }

    fn value_remainder_after(&self, settled: &Self) -> RefreshPlan {
        let settled_stages = refresh_stage_coverage(settled);
        let value_stages = self
            .plan
            .value_stages
            .iter()
            .filter_map(|stage| uncovered_refresh_stage(self, stage, &settled_stages))
            .collect();
        let mut remainder = RefreshPlan {
            simultaneous_plan: self.plan.simultaneous_plan.clone(),
            simultaneous_block_indices: self.plan.simultaneous_block_indices.clone(),
            value_projection_plan: self.plan.value_projection_plan.clone(),
            rows: self.plan.rows.clone(),
            causal_seed_rows: self.plan.causal_seed_rows.clone(),
            static_causal_seed_rows: self.plan.static_causal_seed_rows.clone(),
            dynamic_causal_seed_rows: self.plan.dynamic_causal_seed_rows.clone(),
            value_stages,
        };
        if self.causal_solution_certified && settled.causal_solution_certified {
            let settled_rows = settled
                .causal_rows()
                .iter()
                .map(|row| RefreshStageIdentity::ExactAssignment(row.owner_id))
                .collect::<Vec<_>>();
            remainder.causal_seed_rows =
                uncovered_refresh_rows(self, &self.plan.causal_seed_rows, &settled_rows);
            remainder.static_causal_seed_rows =
                uncovered_refresh_rows(self, &self.plan.static_causal_seed_rows, &settled_rows);
            remainder.dynamic_causal_seed_rows =
                uncovered_refresh_rows(self, &self.plan.dynamic_causal_seed_rows, &settled_rows);
        }
        remainder
    }
}

#[derive(PartialEq)]
enum RefreshStageIdentity {
    ExactAssignment(RefreshRowOwnerId),
    ProjectionBlock(usize),
}

fn refresh_stage_coverage(plan: &IssuedRefreshPlan) -> Vec<RefreshStageIdentity> {
    let mut identities = Vec::new();
    for stage in &plan.plan.value_stages {
        match stage {
            IssuedRefreshStage::CausalSeedSweep {
                static_rows,
                dynamic_rows,
                ..
            }
            | IssuedRefreshStage::ExactAssignments {
                static_rows,
                dynamic_rows,
                ..
            } => identities.extend(
                plan.selected_rows(static_rows)
                    .iter()
                    .chain(plan.selected_rows(dynamic_rows).iter())
                    .map(|row| RefreshStageIdentity::ExactAssignment(row.owner_id)),
            ),
            IssuedRefreshStage::ProjectionBlock { block_index, .. } => {
                identities.push(RefreshStageIdentity::ProjectionBlock(*block_index));
            }
        }
    }
    identities
}

fn uncovered_refresh_rows(
    plan: &IssuedRefreshPlan,
    rows: &RefreshRowSelection,
    settled: &[RefreshStageIdentity],
) -> RefreshRowSelection {
    RefreshRowSelection(
        rows.indices()
            .iter()
            .copied()
            .filter(|index| {
                let row = &plan.plan.rows[*index as usize];
                !settled.contains(&RefreshStageIdentity::ExactAssignment(row.owner_id))
            })
            .collect(),
    )
}

fn uncovered_refresh_stage(
    plan: &IssuedRefreshPlan,
    stage: &IssuedRefreshStage,
    settled: &[RefreshStageIdentity],
) -> Option<RefreshStage> {
    match stage {
        IssuedRefreshStage::CausalSeedSweep {
            static_rows,
            dynamic_rows,
            ..
        } => uncovered_row_stage(plan, static_rows, dynamic_rows, settled, true),
        IssuedRefreshStage::ExactAssignments {
            static_rows,
            dynamic_rows,
            ..
        } => uncovered_row_stage(plan, static_rows, dynamic_rows, settled, false),
        IssuedRefreshStage::ProjectionBlock {
            block_index,
            plan: projection,
            seed_rows,
            ..
        } => (!settled.contains(&RefreshStageIdentity::ProjectionBlock(*block_index))).then(|| {
            RefreshStage::ProjectionBlock {
                block_index: *block_index,
                plan: projection.clone(),
                seed_rows: seed_rows.clone(),
            }
        }),
    }
}

fn uncovered_row_stage(
    plan: &IssuedRefreshPlan,
    static_rows: &RefreshRowSelection,
    dynamic_rows: &RefreshRowSelection,
    settled: &[RefreshStageIdentity],
    causal: bool,
) -> Option<RefreshStage> {
    let static_rows = uncovered_refresh_rows(plan, static_rows, settled);
    let dynamic_rows = uncovered_refresh_rows(plan, dynamic_rows, settled);
    if static_rows.is_empty() && dynamic_rows.is_empty() {
        return None;
    }
    Some(if causal {
        RefreshStage::CausalSeedSweep {
            static_rows,
            dynamic_rows,
        }
    } else {
        RefreshStage::ExactAssignments {
            static_rows,
            dynamic_rows,
        }
    })
}

#[cfg(test)]
mod tests;
