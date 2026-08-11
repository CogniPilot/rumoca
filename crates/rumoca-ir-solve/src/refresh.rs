use std::collections::BTreeMap;
use std::fmt;

use serde::{Deserialize, Deserializer, Serialize};

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
        static_rows: Box<[AlgebraicRefreshRow]>,
        dynamic_rows: Box<[AlgebraicRefreshRow]>,
    },
    ExactAssignments {
        #[serde(skip)]
        static_sequence: RefreshSequenceId,
        #[serde(skip)]
        dynamic_sequence: RefreshSequenceId,
        static_rows: Box<[AlgebraicRefreshRow]>,
        dynamic_rows: Box<[AlgebraicRefreshRow]>,
    },
    ProjectionBlock {
        #[serde(skip)]
        seed_sequence: RefreshSequenceId,
        block_index: usize,
        plan: AlgebraicProjectionPlan,
        seed_rows: Box<[AlgebraicRefreshRow]>,
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
    pub causal_seed_rows: Vec<AlgebraicRefreshRow>,
    pub static_causal_seed_rows: Vec<AlgebraicRefreshRow>,
    pub dynamic_causal_seed_rows: Vec<AlgebraicRefreshRow>,
    pub value_stages: Vec<RefreshStage>,
    pub causal_solution_certified: bool,
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
        Ok(Self {
            algebraic,
            derivative,
            root,
            event,
            clock_events,
            exact_assignment_programs: Vec::new(),
            exact_assignment_schedules: Vec::new(),
            root_after_derivative: Some(root_after_derivative),
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
                let program = self
                    .exact_assignment_programs
                    .iter()
                    .find(|program| program.row_owners.contains(&row.owner_id));
                if row.exact_assignment_certified {
                    let Some(program) = program else {
                        return refresh_error(
                            "exact continuous refresh row has no constructed assignment program"
                                .to_string(),
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
                            "exact continuous refresh program does not replay its row owner"
                                .to_string(),
                        );
                    }
                } else if program.is_some() {
                    return refresh_error(
                        "non-exact continuous refresh row owns an exact assignment program"
                            .to_string(),
                    );
                }
            }
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

    pub(crate) fn rebuild_exact_assignment_programs(
        &mut self,
        implicit_rhs: &ComputeBlock,
    ) -> Result<(), ContinuousRefreshConstructionError> {
        let mut rows = BTreeMap::new();
        for plan in [&self.algebraic, &self.derivative, &self.root, &self.event]
            .into_iter()
            .chain(self.clock_events.iter())
        {
            for row in &plan.rows {
                if let Some(existing) = rows.insert(row.owner_id, row)
                    && existing != row
                {
                    return refresh_error(
                        "continuous refresh plans disagree on a canonical row owner".to_string(),
                    );
                }
            }
        }
        let Self {
            algebraic,
            derivative,
            root,
            event,
            clock_events,
            exact_assignment_programs,
            exact_assignment_schedules,
            root_after_derivative,
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
        Ok(())
    }

    #[must_use]
    pub const fn algebraic(&self) -> &RefreshPlan {
        &self.algebraic
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
    rows: &[AlgebraicRefreshRow],
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
    Ok(ExactRefreshAssignmentProgram {
        id,
        row_owners: rows
            .iter()
            .map(|row| row.owner_id)
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        source: first.source,
        target_indices: rows
            .iter()
            .map(|row| row.target_index)
            .collect::<Vec<_>>()
            .into_boxed_slice(),
        assignment_shapes: shapes.into_boxed_slice(),
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
    rows: &[AlgebraicRefreshRow],
) -> Result<bool, ContinuousRefreshConstructionError> {
    let Some(first) = rows.first() else {
        return Ok(false);
    };
    let Some((program, _)) = scalar_source_program(block, first.source)? else {
        return Ok(false);
    };
    let mut dependencies = ScalarProgramYDependency::new(program);
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

fn shape_value_registers(shape: TargetAssignmentShape) -> [Option<u32>; 3] {
    match shape {
        TargetAssignmentShape::Direct { expr_reg, .. } => [Some(expr_reg), None, None],
        TargetAssignmentShape::Affine {
            offset_reg,
            coefficient_reg,
            ..
        } => [Some(offset_reg), coefficient_reg, None],
        TargetAssignmentShape::AffineResidual {
            target_reg,
            residual_reg,
            ..
        } => [Some(target_reg), Some(residual_reg), None],
    }
}

/// Checked, fail-closed dependence query for registers in one scalar-program view.
///
/// The query is owned by Solve IR so construction and final scalar adapters do
/// not maintain independent interpretations of [`LinearOp`] dependencies.
pub struct ScalarProgramYDependency<'a> {
    program: &'a [LinearOp],
    memo: BTreeMap<(u32, usize), bool>,
}

impl<'a> ScalarProgramYDependency<'a> {
    pub fn new(program: &'a [LinearOp]) -> Self {
        Self {
            program,
            memo: BTreeMap::new(),
        }
    }

    pub fn depends_on(&mut self, register: u32, target: usize) -> bool {
        if let Some(value) = self.memo.get(&(register, target)).copied() {
            return value;
        }
        self.memo.insert((register, target), true);
        let value = self
            .program
            .iter()
            .rev()
            .find(|operation| register_is_written_by(operation, register))
            .cloned()
            .is_none_or(|operation| self.operation_depends(register, target, operation));
        self.memo.insert((register, target), value);
        value
    }

    fn operation_depends(&mut self, output: u32, target: usize, operation: LinearOp) -> bool {
        match operation {
            LinearOp::LoadY { index, .. } => index == target,
            LinearOp::TensorLoad {
                dst_start,
                input: crate::TensorInputKind::Y,
                input_start,
                count,
                lanes,
                ..
            } => {
                let offset = output.saturating_sub(dst_start) as usize;
                lanes == 0
                    || offset >= count.saturating_mul(lanes)
                    || !offset.is_multiple_of(lanes)
                    || input_start.saturating_add(offset / lanes) == target
            }
            LinearOp::Move { src, .. }
            | LinearOp::Unary { arg: src, .. }
            | LinearOp::LoadIndexedP { index: src, .. }
            | LinearOp::LoadIndexedSeed { index: src, .. }
            | LinearOp::ImpureRandomInit { seed: src, .. }
            | LinearOp::ImpureRandom { id: src, .. }
            | LinearOp::TableBounds { table_id: src, .. } => self.depends_on(src, target),
            LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
                self.any([lhs, rhs], target)
            }
            LinearOp::Select {
                cond,
                if_true,
                if_false,
                ..
            } => self.any([cond, if_true, if_false], target),
            LinearOp::LoadIndexedRegister {
                base,
                stride,
                dimensions,
                indices,
                ..
            } => {
                dimensions
                    .iter()
                    .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
                    .is_none_or(|count| {
                        (0..count).any(|offset| {
                            u32::try_from(offset.saturating_mul(stride))
                                .ok()
                                .and_then(|offset| base.checked_add(offset))
                                .is_none_or(|register| self.depends_on(register, target))
                        })
                    })
                    || indices
                        .iter()
                        .any(|index| self.tensor_index_depends(index, target))
            }
            LinearOp::LoadIndexedFoldCarried { indices, .. }
            | LinearOp::LoadIndexedFoldCapture { indices, .. } => indices
                .iter()
                .any(|index| self.tensor_index_depends(index, target)),
            LinearOp::LinearSolveComponent {
                matrix_start,
                rhs_start,
                n,
                ..
            } => {
                n.checked_mul(n)
                    .is_none_or(|count| self.range_depends(matrix_start, count, target))
                    || self.range_depends(rhs_start, n, target)
            }
            LinearOp::DotProduct {
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                ..
            } => (0..count).any(|term| {
                self.offset_depends(lhs_start, term.saturating_mul(lhs_stride), target)
                    || self.offset_depends(rhs_start, term.saturating_mul(rhs_stride), target)
            }),
            LinearOp::MatrixMultiply {
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes,
                ..
            } => {
                rows.checked_mul(inner)
                    .and_then(|count| count.checked_mul(lanes))
                    .is_none_or(|count| self.range_depends(lhs_start, count, target))
                    || inner
                        .checked_mul(columns)
                        .and_then(|count| count.checked_mul(lanes))
                        .is_none_or(|count| self.range_depends(rhs_start, count, target))
            }
            LinearOp::TensorBinary {
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                lanes,
                ..
            } => (0..count).any(|element| {
                self.range_offset_depends(
                    lhs_start,
                    element.saturating_mul(lhs_stride).saturating_mul(lanes),
                    lanes,
                    target,
                ) || self.range_offset_depends(
                    rhs_start,
                    element.saturating_mul(rhs_stride).saturating_mul(lanes),
                    lanes,
                    target,
                )
            }),
            LinearOp::TensorCross {
                lhs_start,
                rhs_start,
                lanes,
                ..
            } => {
                lanes
                    .checked_mul(3)
                    .is_none_or(|count| self.range_depends(lhs_start, count, target))
                    || lanes
                        .checked_mul(3)
                        .is_none_or(|count| self.range_depends(rhs_start, count, target))
            }
            LinearOp::TensorTranspose {
                src_start,
                rows,
                columns,
                element_width,
                lanes,
                ..
            } => rows
                .checked_mul(columns)
                .and_then(|count| count.checked_mul(element_width))
                .and_then(|count| count.checked_mul(lanes))
                .is_none_or(|count| self.range_depends(src_start, count, target)),
            LinearOp::TensorConcatenate { sources, lanes, .. } => sources.iter().any(|source| {
                source
                    .dimensions
                    .iter()
                    .try_fold(lanes, |count, extent| count.checked_mul(*extent as usize))
                    .is_none_or(|count| self.range_depends(source.start, count, target))
            }),
            LinearOp::TensorFill {
                value_start, lanes, ..
            } => self.range_depends(value_start, lanes, target),
            LinearOp::PureCall {
                input_starts, site, ..
            } => input_starts
                .iter()
                .zip(site.inputs())
                .any(|(start, ty)| self.range_depends(*start, ty.scalar_count() as usize, target)),
            LinearOp::PureCallDirectional {
                input_starts, site, ..
            } => input_starts
                .iter()
                .zip(site.inputs())
                .any(|(start, ty)| self.range_depends(*start, ty.scalar_count() as usize, target)),
            LinearOp::FunctionFold {
                initial_start,
                capture_start,
                program,
                ..
            }
            | LinearOp::GuardedFunctionFold {
                initial_start,
                capture_start,
                program,
                ..
            } => {
                self.range_depends(initial_start, program.carried_count, target)
                    || self.range_depends(capture_start, program.capture_count, target)
                    || program_reads_y(&program.update, target)
            }
            LinearOp::FunctionConditional {
                capture_start,
                program,
                ..
            } => {
                self.range_depends(capture_start, program.capture_count, target)
                    || program.arms.iter().any(|arm| {
                        program_reads_y(&arm.condition, target)
                            || program_reads_y(&arm.result, target)
                    })
                    || program_reads_y(&program.fallback, target)
            }
            LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::LoadSeed { .. }
            | LinearOp::LoadFoldCarried { .. }
            | LinearOp::LoadFoldIndex { .. }
            | LinearOp::LoadFoldCapture { .. }
            | LinearOp::LoadFunctionConditionalCapture { .. }
            | LinearOp::LoadFunctionConditionalCaptureRange { .. }
            | LinearOp::TensorIdentity { .. }
            | LinearOp::TensorLoad { .. } => false,
            _ => true,
        }
    }

    fn any<const N: usize>(&mut self, registers: [u32; N], target: usize) -> bool {
        registers
            .into_iter()
            .any(|register| self.depends_on(register, target))
    }

    fn range_depends(&mut self, start: u32, count: usize, target: usize) -> bool {
        (0..count).any(|offset| self.offset_depends(start, offset, target))
    }

    fn offset_depends(&mut self, start: u32, offset: usize, target: usize) -> bool {
        u32::try_from(offset)
            .ok()
            .and_then(|offset| start.checked_add(offset))
            .is_none_or(|register| self.depends_on(register, target))
    }

    fn range_offset_depends(
        &mut self,
        start: u32,
        offset: usize,
        count: usize,
        target: usize,
    ) -> bool {
        u32::try_from(offset)
            .ok()
            .and_then(|offset| start.checked_add(offset))
            .is_none_or(|start| self.range_depends(start, count, target))
    }

    fn tensor_index_depends(&mut self, index: &crate::TensorIndex, target: usize) -> bool {
        matches!(index, crate::TensorIndex::Runtime(register) if self.depends_on(*register, target))
    }
}

fn register_is_written_by(operation: &LinearOp, register: u32) -> bool {
    operation.dst_register().is_some_and(|start| {
        u32::try_from(operation.dst_register_count())
            .ok()
            .and_then(|count| start.checked_add(count))
            .is_some_and(|end| register >= start && register < end)
    })
}

fn program_reads_y(program: &[LinearOp], target: usize) -> bool {
    program.iter().any(|operation| match operation {
        LinearOp::LoadY { index, .. } => *index == target,
        LinearOp::TensorLoad {
            input: crate::TensorInputKind::Y,
            input_start,
            count,
            ..
        } => (*input_start..input_start.saturating_add(*count)).contains(&target),
        LinearOp::FunctionFold { program, .. }
        | LinearOp::GuardedFunctionFold { program, .. }
        | LinearOp::StoreOutputFunctionFold { program, .. } => {
            program_reads_y(&program.update, target)
        }
        LinearOp::FunctionConditional { program, .. } => {
            program.arms.iter().any(|arm| {
                program_reads_y(&arm.condition, target) || program_reads_y(&arm.result, target)
            }) || program_reads_y(&program.fallback, target)
        }
        _ => false,
    })
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
        &plan.static_causal_seed_rows,
    )?;
    append_exact_assignment_schedule(
        block,
        programs,
        schedules,
        inventory,
        plan.dynamic_causal_sequence,
        &plan.dynamic_causal_seed_rows,
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
                    static_rows,
                )?;
                append_exact_assignment_schedule(
                    block,
                    programs,
                    schedules,
                    inventory,
                    *dynamic_sequence,
                    dynamic_rows,
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
                seed_rows,
            )?,
        }
    }
    Ok(())
}

fn append_exact_assignment_schedule(
    block: &ComputeBlock,
    programs: &mut Vec<ExactRefreshAssignmentProgram>,
    schedules: &mut Vec<ExactRefreshAssignmentSchedule>,
    inventory: &mut BTreeMap<Vec<RefreshRowOwnerId>, ExactRefreshAssignmentProgramId>,
    sequence_id: RefreshSequenceId,
    rows: &[AlgebraicRefreshRow],
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
        let source_rows = &rows[position..end];
        if source_rows.len() > 1 && exact_rows_can_commit_together(block, source_rows)? {
            append_exact_assignment_program(
                block,
                programs,
                inventory,
                source_rows,
                &mut program_ids,
            )?;
        } else {
            for row in source_rows {
                append_exact_assignment_program(
                    block,
                    programs,
                    inventory,
                    std::slice::from_ref(row),
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
    rows: &[AlgebraicRefreshRow],
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
    validate_refresh_row_selection(label, "causal", &plan.causal_seed_rows, &rows)?;
    validate_refresh_row_selection(label, "static", &plan.static_causal_seed_rows, &rows)?;
    validate_refresh_row_selection(label, "dynamic", &plan.dynamic_causal_seed_rows, &rows)?;
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
            } => {
                validate_refresh_row_selection(label, "stage static", static_rows, &rows)?;
                validate_refresh_row_selection(label, "stage dynamic", dynamic_rows, &rows)?;
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
                validate_refresh_row_selection(label, "projection seed", seed_rows, &rows)?;
            }
        }
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
    selected: &[AlgebraicRefreshRow],
    rows: &BTreeMap<RefreshRowOwnerId, &AlgebraicRefreshRow>,
) -> Result<(), ContinuousRefreshConstructionError> {
    for row in selected {
        let Some(owner) = rows.get(&row.owner_id) else {
            return refresh_error(format!(
                "{label} {selection} row refers to an unowned canonical identity"
            ));
        };
        if *owner != row {
            return refresh_error(format!(
                "{label} {selection} row does not replay its canonical owner"
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
    fn issue_value_remainder_after(&self, settled: &Self) -> RefreshRemainderRelation {
        let settled_stages = refresh_stage_coverage(&settled.value_stages);
        let value_stages = self
            .value_stages
            .iter()
            .filter_map(|stage| uncovered_refresh_stage(stage, &settled_stages))
            .collect();
        let mut remainder = self.clone();
        remainder.value_stages = value_stages;
        if self.causal_solution_certified && settled.causal_solution_certified {
            let settled_rows = settled
                .causal_seed_rows
                .iter()
                .map(|row| RefreshStageIdentity::ExactAssignment(row.owner_id))
                .collect::<Vec<_>>();
            remainder.causal_seed_rows =
                uncovered_refresh_rows(&self.causal_seed_rows, &settled_rows);
            remainder.static_causal_seed_rows =
                uncovered_refresh_rows(&self.static_causal_seed_rows, &settled_rows);
            remainder.dynamic_causal_seed_rows =
                uncovered_refresh_rows(&self.dynamic_causal_seed_rows, &settled_rows);
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

fn refresh_stage_coverage(stages: &[RefreshStage]) -> Vec<RefreshStageIdentity> {
    let mut identities = Vec::new();
    for stage in stages {
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
                static_rows
                    .iter()
                    .chain(dynamic_rows.iter())
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
    rows: &[AlgebraicRefreshRow],
    settled: &[RefreshStageIdentity],
) -> Vec<AlgebraicRefreshRow> {
    rows.iter()
        .filter(|row| !settled.contains(&RefreshStageIdentity::ExactAssignment(row.owner_id)))
        .cloned()
        .collect()
}

fn uncovered_refresh_stage(
    stage: &RefreshStage,
    settled: &[RefreshStageIdentity],
) -> Option<RefreshStage> {
    match stage {
        RefreshStage::CausalSeedSweep {
            static_rows,
            dynamic_rows,
            ..
        } => uncovered_row_stage(static_rows, dynamic_rows, settled, true),
        RefreshStage::ExactAssignments {
            static_rows,
            dynamic_rows,
            ..
        } => uncovered_row_stage(static_rows, dynamic_rows, settled, false),
        RefreshStage::ProjectionBlock { block_index, .. } => (!settled
            .contains(&RefreshStageIdentity::ProjectionBlock(*block_index)))
        .then(|| stage.clone()),
    }
}

fn uncovered_row_stage(
    static_rows: &[AlgebraicRefreshRow],
    dynamic_rows: &[AlgebraicRefreshRow],
    settled: &[RefreshStageIdentity],
    causal: bool,
) -> Option<RefreshStage> {
    let static_rows = uncovered_refresh_rows(static_rows, settled).into_boxed_slice();
    let dynamic_rows = uncovered_refresh_rows(dynamic_rows, settled).into_boxed_slice();
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
