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
    TargetAssignmentShape, UnaryOp,
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
#[derive(
    Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Deserialize, Serialize,
)]
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
#[derive(Clone, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
#[serde(transparent)]
pub struct RefreshRowSelection(Box<[u32]>);

impl RefreshRowSelection {
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
        self.indices.iter().map(|index| {
            &self.catalog
                [usize::try_from(*index).expect("checked continuous refresh row index fits usize")]
        })
    }
}

impl Index<usize> for RefreshRows<'_> {
    type Output = AlgebraicRefreshRow;

    fn index(&self, position: usize) -> &Self::Output {
        self.get(position)
            .expect("checked continuous refresh selection index is in bounds")
    }
}

/// Opaque construction-issued identity of one exact ordered row selection.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RefreshSequenceId(u64);

impl RefreshSequenceId {
    fn issued(owner: usize, sequence: usize) -> Result<Self, ContinuousRefreshConstructionError> {
        let owner = u32::try_from(owner).map_err(|_| ContinuousRefreshConstructionError {
            reason: "continuous refresh owner count exceeds u32".to_string(),
        })?;
        let sequence = u32::try_from(sequence)
            .ok()
            .and_then(|sequence| sequence.checked_add(1))
            .ok_or_else(|| ContinuousRefreshConstructionError {
                reason: "continuous refresh sequence count exceeds u32".to_string(),
            })?;
        Ok(Self((u64::from(owner) << 32) | u64::from(sequence)))
    }
}

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

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct AlgebraicRefreshRowWire {
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

    /// Whether one checked assignment value reads a solver-Y coordinate.
    ///
    /// The dependency is derived with the canonical assignment certificate at
    /// construction/wire replay. Target preparation can therefore check its
    /// execution profile without reinterpreting residual operations.
    #[must_use]
    pub fn assignment_reads_y_index(&self, position: usize, y_index: usize) -> Option<bool> {
        self.assignment_shapes
            .get(position)
            .zip(self.assignment_y_dependencies.get(position))
            .map(|(shape, dependencies)| {
                dependencies.contains(&y_index) && y_index != shape.target_y_index()
            })
    }

    #[must_use]
    pub fn assignment_y_dependencies(&self, position: usize) -> Option<&[usize]> {
        self.assignment_y_dependencies
            .get(position)
            .map(Box::as_ref)
    }

    /// Materialize the scalar execution view at a final backend boundary.
    ///
    /// The checked owner stores only canonical source identity and isolator
    /// shapes. It never retains an expanded assignment operation graph.
    pub fn final_scalar_program(
        &self,
        source: &ComputeBlock,
    ) -> Result<ScalarProgramBlock, ContinuousRefreshConstructionError> {
        materialize_exact_assignment_program(source, self)
    }
}

/// One construction-ordered stage in an algebraic value refresh.
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub enum RefreshStage {
    CausalSeedSweep {
        #[serde(skip)]
        static_sequence: RefreshSequenceId,
        #[serde(skip)]
        dynamic_sequence: RefreshSequenceId,
        static_rows: RefreshRowSelection,
        dynamic_rows: RefreshRowSelection,
    },
    ExactAssignments {
        #[serde(skip)]
        static_sequence: RefreshSequenceId,
        #[serde(skip)]
        dynamic_sequence: RefreshSequenceId,
        static_rows: RefreshRowSelection,
        dynamic_rows: RefreshRowSelection,
    },
    ProjectionBlock {
        #[serde(skip)]
        seed_sequence: RefreshSequenceId,
        block_index: usize,
        plan: AlgebraicProjectionPlan,
        seed_rows: RefreshRowSelection,
    },
}

/// Exact compiler-issued continuous refresh schedule.
#[derive(Clone, Debug, Default, PartialEq, Deserialize, Serialize)]
pub struct RefreshPlan {
    #[serde(skip)]
    pub static_causal_sequence: RefreshSequenceId,
    #[serde(skip)]
    pub dynamic_causal_sequence: RefreshSequenceId,
    pub simultaneous_plan: AlgebraicProjectionPlan,
    pub simultaneous_block_indices: Vec<usize>,
    pub value_projection_plan: AlgebraicProjectionPlan,
    pub rows: Vec<AlgebraicRefreshRow>,
    pub causal_seed_rows: RefreshRowSelection,
    pub static_causal_seed_rows: RefreshRowSelection,
    pub dynamic_causal_seed_rows: RefreshRowSelection,
    pub value_stages: Vec<RefreshStage>,
    pub causal_solution_certified: bool,
}

impl RefreshPlan {
    /// Whether value refresh still needs a simultaneous residual projection.
    ///
    /// Exact-assignment and causal-seed stages are executable schedules, not
    /// residual systems. A final backend needs a nonlinear/algebraic solver
    /// only when construction retained an explicit projection stage.
    fn requires_projection(&self) -> bool {
        self.value_stages
            .iter()
            .any(|stage| matches!(stage, RefreshStage::ProjectionBlock { .. }))
    }
}

/// Construction-issued proof that `remainder` is the exact ordered portion of
/// a required refresh not settled by another owner at the same coordinate.
#[derive(Clone, Debug)]
pub struct RefreshRemainderRelation {
    remainder: RefreshPlan,
}

/// Complete construction-issued continuous refresh inventory for one model.
#[derive(Clone, Debug, Default, Serialize)]
pub struct ContinuousRefreshOwners {
    algebraic: RefreshPlan,
    derivative: RefreshPlan,
    root: RefreshPlan,
    event: RefreshPlan,
    clock_events: Vec<RefreshPlan>,
    #[serde(skip)]
    exact_assignment_programs: Vec<ExactRefreshAssignmentProgram>,
    #[serde(skip)]
    exact_assignment_schedules: Vec<ExactRefreshAssignmentSchedule>,
    #[serde(skip)]
    root_after_derivative: Option<RefreshRemainderRelation>,
    #[serde(skip)]
    algebraic_after_derivative: Option<RefreshRemainderRelation>,
    #[serde(skip)]
    clock_events_after_event: Vec<RefreshRemainderRelation>,
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

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ContinuousRefreshOwnersWire {
    algebraic: RefreshPlan,
    derivative: RefreshPlan,
    root: RefreshPlan,
    event: RefreshPlan,
    clock_events: Vec<RefreshPlan>,
}

impl<'de> Deserialize<'de> for ContinuousRefreshOwners {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let wire = ContinuousRefreshOwnersWire::deserialize(deserializer)?;
        Self::checked(
            wire.algebraic,
            wire.derivative,
            wire.root,
            wire.event,
            wire.clock_events,
        )
        .map_err(serde::de::Error::custom)
    }
}

impl ContinuousRefreshOwners {
    #[must_use]
    pub const fn is_issued(&self) -> bool {
        self.root_after_derivative.is_some()
            && self.algebraic_after_derivative.is_some()
            && self.clock_events_after_event.len() == self.clock_events.len()
    }

    fn checked(
        mut algebraic: RefreshPlan,
        mut derivative: RefreshPlan,
        mut root: RefreshPlan,
        mut event: RefreshPlan,
        mut clock_events: Vec<RefreshPlan>,
    ) -> Result<Self, ContinuousRefreshConstructionError> {
        issue_refresh_sequence_ids(&mut algebraic, 0)?;
        issue_refresh_sequence_ids(&mut derivative, 1)?;
        issue_refresh_sequence_ids(&mut root, 2)?;
        issue_refresh_sequence_ids(&mut event, 3)?;
        for (clock, plan) in clock_events.iter_mut().enumerate() {
            issue_refresh_sequence_ids(plan, clock.saturating_add(5))?;
        }
        validate_refresh_plan("algebraic", &algebraic)?;
        validate_refresh_plan("derivative", &derivative)?;
        validate_refresh_plan("root", &root)?;
        validate_refresh_plan("event", &event)?;
        for (clock, plan) in clock_events.iter().enumerate() {
            validate_refresh_plan(&format!("clock event {clock}"), plan)?;
        }
        let mut root_after_derivative = root.issue_value_remainder_after(&derivative);
        issue_refresh_sequence_ids(&mut root_after_derivative.remainder, 4)?;
        let mut algebraic_after_derivative = algebraic.issue_value_remainder_after(&derivative);
        let relation_owner_start = 5usize.checked_add(clock_events.len()).ok_or_else(|| {
            ContinuousRefreshConstructionError {
                reason: "continuous refresh relation owner count overflows".to_string(),
            }
        })?;
        let mut clock_events_after_event = Vec::with_capacity(clock_events.len());
        for (clock, plan) in clock_events.iter().enumerate() {
            let mut relation = plan.issue_value_remainder_after(&event);
            let owner = relation_owner_start.checked_add(clock).ok_or_else(|| {
                ContinuousRefreshConstructionError {
                    reason: "continuous refresh relation owner count overflows".to_string(),
                }
            })?;
            issue_refresh_sequence_ids(&mut relation.remainder, owner)?;
            clock_events_after_event.push(relation);
        }
        let algebraic_remainder_owner = relation_owner_start
            .checked_add(clock_events.len())
            .ok_or_else(|| ContinuousRefreshConstructionError {
                reason: "continuous refresh relation owner count overflows".to_string(),
            })?;
        issue_refresh_sequence_ids(
            &mut algebraic_after_derivative.remainder,
            algebraic_remainder_owner,
        )?;
        Ok(Self {
            algebraic,
            derivative,
            root,
            event,
            clock_events,
            exact_assignment_programs: Vec::new(),
            exact_assignment_schedules: Vec::new(),
            root_after_derivative: Some(root_after_derivative),
            algebraic_after_derivative: Some(algebraic_after_derivative),
            clock_events_after_event,
        })
    }

    pub fn checked_for_source(
        implicit_rhs: &ComputeBlock,
        algebraic: RefreshPlan,
        derivative: RefreshPlan,
        root: RefreshPlan,
        event: RefreshPlan,
        clock_events: Vec<RefreshPlan>,
    ) -> Result<Self, ContinuousRefreshConstructionError> {
        let mut owners = Self::checked(algebraic, derivative, root, event, clock_events)?;
        owners.validate_sources_against(implicit_rhs)?;
        owners.rebuild_exact_assignment_programs(implicit_rhs)?;
        owners.validate_against(implicit_rhs)?;
        Ok(owners)
    }

    fn validate_sources_against(
        &self,
        implicit_rhs: &ComputeBlock,
    ) -> Result<(), ContinuousRefreshConstructionError> {
        for (label, plan) in [
            ("algebraic", &self.algebraic),
            ("derivative", &self.derivative),
            ("root", &self.root),
            ("event", &self.event),
        ] {
            validate_refresh_sources(label, plan, implicit_rhs)?;
        }
        for (clock, plan) in self.clock_events.iter().enumerate() {
            validate_refresh_sources(&format!("clock event {clock}"), plan, implicit_rhs)?;
        }
        Ok(())
    }

    pub(crate) fn validate_against(
        &self,
        implicit_rhs: &ComputeBlock,
    ) -> Result<(), ContinuousRefreshConstructionError> {
        if !self.is_issued() {
            return Ok(());
        }
        self.validate_sources_against(implicit_rhs)?;
        for plan in [&self.algebraic, &self.derivative, &self.root, &self.event]
            .into_iter()
            .chain(self.clock_events.iter())
        {
            for row in &plan.rows {
                self.validate_row_assignment_program(row)?;
            }
        }
        Ok(())
    }

    /// Checks that one refresh row and the exact assignment program inventory
    /// agree on whether the row replays as an exact assignment.
    fn validate_row_assignment_program(
        &self,
        row: &AlgebraicRefreshRow,
    ) -> Result<(), ContinuousRefreshConstructionError> {
        let program = self
            .exact_assignment_programs
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
            return refresh_error(
                "exact continuous refresh program lost its row owner".to_string(),
            );
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

    /// Checks that every plan carrying a given row owner id carries the exact
    /// same canonical row.
    fn validate_canonical_row_owners(&self) -> Result<(), ContinuousRefreshConstructionError> {
        let mut rows = BTreeMap::new();
        for row in [&self.algebraic, &self.derivative, &self.root, &self.event]
            .into_iter()
            .chain(self.clock_events.iter())
            .flat_map(|plan| plan.rows.iter())
        {
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

    pub(crate) fn rebuild_exact_assignment_programs(
        &mut self,
        implicit_rhs: &ComputeBlock,
    ) -> Result<(), ContinuousRefreshConstructionError> {
        self.validate_canonical_row_owners()?;
        let Self {
            algebraic,
            derivative,
            root,
            event,
            clock_events,
            exact_assignment_programs,
            exact_assignment_schedules,
            root_after_derivative,
            algebraic_after_derivative,
            clock_events_after_event,
        } = self;
        exact_assignment_programs.clear();
        exact_assignment_schedules.clear();
        let mut inventory = BTreeMap::new();
        for plan in [&*algebraic, &*derivative, &*root, &*event]
            .into_iter()
            .chain(clock_events.iter())
            .chain(
                root_after_derivative
                    .iter()
                    .map(|relation| relation.remainder()),
            )
            .chain(
                algebraic_after_derivative
                    .iter()
                    .map(|relation| relation.remainder()),
            )
            .chain(
                clock_events_after_event
                    .iter()
                    .map(|relation| relation.remainder()),
            )
        {
            append_plan_assignment_schedules(
                implicit_rhs,
                exact_assignment_programs,
                exact_assignment_schedules,
                &mut inventory,
                plan,
            )?;
        }
        if !exact_assignment_stages_are_causal(
            algebraic,
            exact_assignment_programs,
            exact_assignment_schedules,
        ) {
            return refresh_error("algebraic exact-assignment stages are non-causal".to_string());
        }
        Ok(())
    }

    #[must_use]
    pub const fn algebraic(&self) -> &RefreshPlan {
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
            &self.algebraic,
            &self.exact_assignment_programs,
            &self.exact_assignment_schedules,
        )
    }

    #[must_use]
    pub const fn derivative(&self) -> &RefreshPlan {
        &self.derivative
    }

    #[must_use]
    pub const fn root(&self) -> &RefreshPlan {
        &self.root
    }

    #[must_use]
    pub const fn event(&self) -> &RefreshPlan {
        &self.event
    }

    #[must_use]
    pub fn clock_events(&self) -> &[RefreshPlan] {
        &self.clock_events
    }

    #[must_use]
    pub const fn root_after_derivative(&self) -> Option<&RefreshRemainderRelation> {
        self.root_after_derivative.as_ref()
    }

    #[must_use]
    pub const fn algebraic_after_derivative(&self) -> Option<&RefreshRemainderRelation> {
        self.algebraic_after_derivative.as_ref()
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
        validate_refresh_assignment_certificate(label, row, implicit_rhs)?;
    }
    Ok(())
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
            let Some(program) = programs.programs().get(source_program) else {
                return Ok(None);
            };
            if output_offset >= crate::ScalarProgramBlock::program_output_count(program) {
                return Ok(None);
            }
            let preceding_outputs = programs
                .programs()
                .iter()
                .take(source_program)
                .try_fold(0usize, |count, program| {
                    count.checked_add(crate::ScalarProgramBlock::program_output_count(program))
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
    let (source_program, _) = scalar_source_program(block, first.source)?.ok_or_else(|| {
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
    let target_indices = rows
        .iter()
        .map(|row| row.target_index)
        .collect::<Vec<_>>()
        .into_boxed_slice();
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
    })
}

fn materialize_exact_assignment_program(
    block: &ComputeBlock,
    owner: &ExactRefreshAssignmentProgram,
) -> Result<ScalarProgramBlock, ContinuousRefreshConstructionError> {
    let (source_program, span) = scalar_source_program(block, owner.source)?.ok_or_else(|| {
        ContinuousRefreshConstructionError {
            reason: "exact continuous refresh source program is missing".to_string(),
        }
    })?;
    let prefix_len = owner
        .assignment_shapes
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
    for shape in owner.assignment_shapes.iter().copied() {
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
    plan: &RefreshPlan,
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
            RefreshStage::CausalSeedSweep {
                static_sequence,
                dynamic_sequence,
                static_rows,
                dynamic_rows,
            }
            | RefreshStage::ExactAssignments {
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
            RefreshStage::ProjectionBlock {
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
    plan: &RefreshPlan,
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

fn exact_assignment_stages_are_causal(
    plan: &RefreshPlan,
    programs: &[ExactRefreshAssignmentProgram],
    schedules: &[ExactRefreshAssignmentSchedule],
) -> bool {
    exact_assignment_stage_coverage(plan, programs, schedules).is_some()
}

fn exact_assignment_stage_coverage(
    plan: &RefreshPlan,
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
    plan: &RefreshPlan,
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
    plan: &'a RefreshPlan,
    programs: &'a [ExactRefreshAssignmentProgram],
    schedules: &'a [ExactRefreshAssignmentSchedule],
    target_inventory: std::collections::BTreeSet<usize>,
    covered: std::collections::BTreeSet<RefreshRowOwnerId>,
    available: std::collections::BTreeSet<usize>,
}

impl<'a> ExactAssignmentCoverage<'a> {
    fn new(
        plan: &'a RefreshPlan,
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

    fn mark_stage(&mut self, stage: &RefreshStage) -> bool {
        match stage {
            RefreshStage::CausalSeedSweep { .. } => true,
            RefreshStage::ExactAssignments {
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
            RefreshStage::ProjectionBlock { plan, .. } => plan
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

fn issue_refresh_sequence_ids(
    plan: &mut RefreshPlan,
    owner: usize,
) -> Result<(), ContinuousRefreshConstructionError> {
    let mut next = 0usize;
    plan.static_causal_sequence = RefreshSequenceId::issued(owner, next)?;
    next += 1;
    plan.dynamic_causal_sequence = RefreshSequenceId::issued(owner, next)?;
    next += 1;
    for stage in &mut plan.value_stages {
        match stage {
            RefreshStage::CausalSeedSweep {
                static_sequence,
                dynamic_sequence,
                ..
            }
            | RefreshStage::ExactAssignments {
                static_sequence,
                dynamic_sequence,
                ..
            } => {
                *static_sequence = RefreshSequenceId::issued(owner, next)?;
                next += 1;
                *dynamic_sequence = RefreshSequenceId::issued(owner, next)?;
                next += 1;
            }
            RefreshStage::ProjectionBlock { seed_sequence, .. } => {
                *seed_sequence = RefreshSequenceId::issued(owner, next)?;
                next += 1;
            }
        }
    }
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
    pub const fn remainder(&self) -> &RefreshPlan {
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

    fn issue_value_remainder_after(&self, settled: &Self) -> RefreshRemainderRelation {
        let settled_stages = refresh_stage_coverage(settled);
        let value_stages = self
            .value_stages
            .iter()
            .filter_map(|stage| uncovered_refresh_stage(self, stage, &settled_stages))
            .collect();
        let mut remainder = self.clone();
        remainder.value_stages = value_stages;
        if self.causal_solution_certified && settled.causal_solution_certified {
            let settled_rows = settled
                .causal_rows()
                .iter()
                .map(|row| RefreshStageIdentity::ExactAssignment(row.owner_id))
                .collect::<Vec<_>>();
            remainder.causal_seed_rows =
                uncovered_refresh_rows(self, &self.causal_seed_rows, &settled_rows);
            remainder.static_causal_seed_rows =
                uncovered_refresh_rows(self, &self.static_causal_seed_rows, &settled_rows);
            remainder.dynamic_causal_seed_rows =
                uncovered_refresh_rows(self, &self.dynamic_causal_seed_rows, &settled_rows);
        } else {
            // A staged remainder cannot inherit the complete plan's causal
            // certificate. Doing so would select an unfiltered causal schedule
            // and discard the construction-issued uncovered stages.
            remainder.causal_solution_certified = false;
        }
        RefreshRemainderRelation { remainder }
    }
}

#[derive(PartialEq)]
enum RefreshStageIdentity {
    ExactAssignment(RefreshRowOwnerId),
    ProjectionBlock(usize),
}

fn refresh_stage_coverage(plan: &RefreshPlan) -> Vec<RefreshStageIdentity> {
    let mut identities = Vec::new();
    for stage in &plan.value_stages {
        match stage {
            RefreshStage::CausalSeedSweep {
                static_rows,
                dynamic_rows,
                ..
            }
            | RefreshStage::ExactAssignments {
                static_rows,
                dynamic_rows,
                ..
            } => identities.extend(
                plan.selected_rows(static_rows)
                    .iter()
                    .chain(plan.selected_rows(dynamic_rows).iter())
                    .map(|row| RefreshStageIdentity::ExactAssignment(row.owner_id)),
            ),
            RefreshStage::ProjectionBlock { block_index, .. } => {
                identities.push(RefreshStageIdentity::ProjectionBlock(*block_index));
            }
        }
    }
    identities
}

fn uncovered_refresh_rows(
    plan: &RefreshPlan,
    rows: &RefreshRowSelection,
    settled: &[RefreshStageIdentity],
) -> RefreshRowSelection {
    RefreshRowSelection(
        rows.indices()
            .iter()
            .copied()
            .filter(|index| {
                let row = &plan.rows[usize::try_from(*index)
                    .expect("checked continuous refresh row index fits usize")];
                !settled.contains(&RefreshStageIdentity::ExactAssignment(row.owner_id))
            })
            .collect(),
    )
}

fn uncovered_refresh_stage(
    plan: &RefreshPlan,
    stage: &RefreshStage,
    settled: &[RefreshStageIdentity],
) -> Option<RefreshStage> {
    match stage {
        RefreshStage::CausalSeedSweep {
            static_rows,
            dynamic_rows,
            ..
        } => uncovered_row_stage(plan, static_rows, dynamic_rows, settled, true),
        RefreshStage::ExactAssignments {
            static_rows,
            dynamic_rows,
            ..
        } => uncovered_row_stage(plan, static_rows, dynamic_rows, settled, false),
        RefreshStage::ProjectionBlock { block_index, .. } => (!settled
            .contains(&RefreshStageIdentity::ProjectionBlock(*block_index)))
        .then(|| stage.clone()),
    }
}

fn uncovered_row_stage(
    plan: &RefreshPlan,
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
            static_sequence: RefreshSequenceId::default(),
            dynamic_sequence: RefreshSequenceId::default(),
            static_rows,
            dynamic_rows,
        }
    } else {
        RefreshStage::ExactAssignments {
            static_sequence: RefreshSequenceId::default(),
            dynamic_sequence: RefreshSequenceId::default(),
            static_rows,
            dynamic_rows,
        }
    })
}

#[cfg(test)]
mod tests;
