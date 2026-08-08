use super::*;

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ContinuousSolveSystem {
    pub implicit_rhs: ComputeBlock,
    pub implicit_row_targets: Vec<Option<ScalarSlot>>,
    pub algebraic_projection_plan: AlgebraicProjectionPlan,
    pub residual: ComputeBlock,
    /// Lower-order holonomic and velocity residuals retained when structural
    /// index reduction replaces them with acceleration-level equations.
    pub manifold_residual: ComputeBlock,
    /// Connected state-coordinate blocks used to project accepted numerical
    /// steps onto `manifold_residual = 0`. Blocks may have more state
    /// coordinates than residual rows; runtimes use a minimum-norm correction.
    pub manifold_projection_plan: AlgebraicProjectionPlan,
    pub derivative_rhs: ComputeBlock,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AlgebraicProjectionPlan {
    pub blocks: Vec<AlgebraicProjectionBlock>,
}

impl AlgebraicProjectionPlan {
    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty()
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AlgebraicProjectionBlock {
    pub rows: Vec<usize>,
    pub y_indices: Vec<usize>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InitializationProjectionPlan {
    pub blocks: Vec<InitializationProjectionBlock>,
}

impl InitializationProjectionPlan {
    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty()
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InitializationProjectionBlock {
    pub rows: Vec<usize>,
    /// Initialization unknowns may reside in either solver Y storage or
    /// parameter P storage.  Time and constant slots are invalid here.
    pub unknowns: Vec<ScalarSlot>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveArtifacts {
    pub continuous: ContinuousSolveArtifacts,
    pub initialization: InitializationSolveArtifacts,
}

#[derive(Clone, Debug)]
pub struct JacobianStructure {
    pattern: StructuralPattern,
    coloring: ColumnColoring,
}

impl JacobianStructure {
    pub fn derived(pattern: StructuralPattern) -> Self {
        let coloring = pattern.column_coloring();
        Self { pattern, coloring }
    }

    pub const fn pattern(&self) -> &StructuralPattern {
        &self.pattern
    }

    pub const fn coloring(&self) -> &ColumnColoring {
        &self.coloring
    }
}

#[derive(Clone, Debug, Default)]
pub struct ContinuousStructuralArtifacts {
    implicit: Option<JacobianStructure>,
    algebraic_projection: Box<[JacobianStructure]>,
    algebraic_invalidates_earlier: Box<[bool]>,
    manifold: Option<JacobianStructure>,
    manifold_projection: Box<[JacobianStructure]>,
    derivative: Option<JacobianStructure>,
}

impl ContinuousStructuralArtifacts {
    pub fn derived(
        implicit: Option<StructuralPattern>,
        algebraic_projection: Vec<StructuralPattern>,
        algebraic_invalidates_earlier: Vec<bool>,
        manifold: Option<StructuralPattern>,
        manifold_projection: Vec<StructuralPattern>,
        derivative: Option<StructuralPattern>,
    ) -> Self {
        Self {
            implicit: implicit.map(JacobianStructure::derived),
            algebraic_projection: algebraic_projection
                .into_iter()
                .map(JacobianStructure::derived)
                .collect(),
            algebraic_invalidates_earlier: algebraic_invalidates_earlier.into_boxed_slice(),
            manifold: manifold.map(JacobianStructure::derived),
            manifold_projection: manifold_projection
                .into_iter()
                .map(JacobianStructure::derived)
                .collect(),
            derivative: derivative.map(JacobianStructure::derived),
        }
    }

    pub const fn implicit(&self) -> Option<&JacobianStructure> {
        self.implicit.as_ref()
    }

    pub fn algebraic_projection(&self) -> &[JacobianStructure] {
        &self.algebraic_projection
    }

    pub fn algebraic_invalidates_earlier(&self, block_index: usize) -> Option<bool> {
        self.algebraic_invalidates_earlier.get(block_index).copied()
    }

    pub const fn manifold(&self) -> Option<&JacobianStructure> {
        self.manifold.as_ref()
    }

    pub fn manifold_projection(&self) -> &[JacobianStructure] {
        &self.manifold_projection
    }

    pub const fn derivative(&self) -> Option<&JacobianStructure> {
        self.derivative.as_ref()
    }
}

#[derive(Clone, Debug, Default)]
pub struct InitializationStructuralArtifacts {
    residual: Option<JacobianStructure>,
    projection: Box<[JacobianStructure]>,
}

impl InitializationStructuralArtifacts {
    pub fn derived(
        residual: Option<StructuralPattern>,
        projection: Vec<StructuralPattern>,
    ) -> Self {
        Self {
            residual: residual.map(JacobianStructure::derived),
            projection: projection
                .into_iter()
                .map(JacobianStructure::derived)
                .collect(),
        }
    }

    pub const fn residual(&self) -> Option<&JacobianStructure> {
        self.residual.as_ref()
    }

    pub fn projection(&self) -> &[JacobianStructure] {
        &self.projection
    }
}

/// Compact solver-facing mass-matrix representation.
///
/// The matrix dimension is the state scalar count in the accompanying
/// [`SolveLayout`]. Identity therefore needs no payload, while general sparse
/// matrices retain only their nonzero entries.
#[derive(Clone, Debug, Default, Deserialize, PartialEq, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum MassMatrix {
    #[default]
    Identity,
    Diagonal {
        values: Vec<f64>,
    },
    Sparse {
        entries: Vec<MassMatrixEntry>,
    },
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Serialize)]
pub struct MassMatrixEntry {
    pub row: usize,
    pub column: usize,
    pub value: f64,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ContinuousSolveArtifacts {
    /// Constructor-derived metadata; canonical Solve wire reconstructs it.
    #[serde(skip)]
    pub structural: ContinuousStructuralArtifacts,
    pub mass_matrix: MassMatrix,
    pub implicit_jacobian_v: ComputeBlock,
    /// Per-row forward-mode AD JVP of the *scalarized* `implicit_rhs`, row-aligned
    /// with successful `to_scalar_program_block(implicit_rhs)` output (and hence
    /// with the algebraic refresh plan's `row_idx`). Used by the state-only path
    /// to propagate the state seed through the algebraic projection
    /// (`d(alg)/d(state)`). Distinct from the tensor `implicit_jacobian_v`, whose
    /// scalarization is not row-aligned when the system has linear
    /// (`LinSolve`/`MatMul`) blocks.
    pub implicit_jacobian_v_scalar: ScalarProgramBlock,
    /// Forward-mode state Jacobian-vector product for
    /// [`ContinuousSolveSystem::manifold_residual`].
    pub manifold_jacobian_v: ComputeBlock,
    pub full_jacobian_v: ScalarProgramBlock,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InitializationSolveArtifacts {
    /// Constructor-derived metadata; canonical Solve wire reconstructs it.
    #[serde(skip)]
    pub structural: InitializationStructuralArtifacts,
    pub residual_jacobian_v: ComputeBlock,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InitializationSolveSystem {
    pub residual: ComputeBlock,
    pub row_targets: Vec<Option<ScalarSlot>>,
    /// What the initialization projection does with each residual row, indexed by
    /// equation index alongside `row_targets`.
    ///
    /// A runtime that only knows "this row has no target" cannot tell a row the
    /// rest of the system already determined — a legal MLS §8.6 consistency check
    /// — from a row nothing solved because it reads a coordinate outside the
    /// planned unknown space. Reporting the first when it is the second names the
    /// wrong defect, so the planner records which it is.
    pub row_roles: Vec<InitializationRowRole>,
    pub projection_unknowns: Vec<ScalarSlot>,
    pub projection_plan: InitializationProjectionPlan,
    pub update_rhs: ScalarProgramBlock,
    pub update_targets: Vec<ScalarSlot>,
}

/// What the MLS §8.6 initialization projection does with one residual row.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub enum InitializationRowRole {
    /// A projection block solves this row for the coordinate `row_targets` names.
    Solved,
    /// Every coordinate the row reads is determined by something other than this
    /// row — a `fixed = true` start, a pin, a binding, or another block — so the
    /// row is a consistency check between values the rest of the system fixed.
    /// MLS §8.6 permits such a row; a failure of one is a contradiction between
    /// declarations, not an unsolved coordinate.
    #[default]
    SurplusCheck,
    /// The row reads a coordinate the projection does not own, so nothing solved
    /// it and the residual is a check over a value the row cannot control.
    UnownedCoordinate(InitializationCoordinateKind),
}

/// Which coordinate kind kept a residual row out of the planned unknown space.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum InitializationCoordinateKind {
    /// A continuous algebraic or output coordinate. The initialization residual
    /// is certified against a reconstructed value, but the reduced
    /// initialization projection does not yet own the coordinate or its total
    /// derivative through the continuous system.
    Algebraic,
    /// A discrete-time coordinate or its `pre` value.
    Discrete,
    /// A coordinate the lowering cannot read per scalar: an array state, a
    /// multi-scalar row, or a structured family point.
    Unreadable,
    /// A coordinate the projection could own, but whose component the planner
    /// could not make square.
    Unmatched,
    /// Any other coordinate outside the planned space — an input, a delay, a
    /// `previous`, a relation memory, a terminal.
    Other,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum EventIterationValueKind {
    Real,
    Integer,
    Boolean,
    /// Positive integral ordinal. The DAE currently erases the declared upper
    /// literal bound; restoring that bound is a tracked upstream obligation.
    Enumeration,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum EventIterationOwner {
    Hold,
    ScalarRows { start_row: usize },
    StructuredUpdate { update_index: usize },
}

#[derive(Clone, Copy, Debug, PartialEq, Deserialize, Serialize)]
pub struct EventIterationRun {
    /// Canonical typed variable-storage owner.
    pub variable: usize,
    pub pre_binding_start: usize,
    pub owner: EventIterationOwner,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct EventIterationPlan {
    pub runs: Vec<EventIterationRun>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct DiscreteSolveSystem {
    /// Compact compiler-owned Appendix-B iteration catalog.
    pub event_iteration_plan: EventIterationPlan,
    pub runtime_assignment_rhs: ScalarProgramBlock,
    pub runtime_assignment_targets: Vec<ScalarSlot>,
    /// Compiler certificate for whether each runtime assignment evaluates a
    /// relation (directly or through another runtime assignment).
    pub runtime_assignment_roles: Vec<RuntimeAssignmentRole>,
    /// Relation-free root-driven assignments that remain valid after event
    /// `pre` history has committed.
    pub post_commit_assignment_rhs: ScalarProgramBlock,
    pub post_commit_assignment_targets: Vec<ScalarSlot>,
    /// Runtime-row owner copied by each post-commit row. Shape validation
    /// proves the copy is exact and the owner is relation-free.
    pub post_commit_assignment_runtime_rows: Vec<usize>,
    pub rhs: ScalarProgramBlock,
    pub update_targets: Vec<ScalarSlot>,
    pub row_roles: Vec<DiscreteRowRole>,
    pub pre_modes: Vec<DiscreteEventPreMode>,
    pub observation_refresh: Vec<bool>,
    /// Compiler-derived effect of changing each scalar update target on an
    /// integrator's continuous multistep history.
    ///
    /// This vector is row-aligned with `rhs`. A runtime may join the effect
    /// with exact update changes, but must not recover it from row position or
    /// model identity.
    pub integrator_history_effects: Vec<IntegratorHistoryEffect>,
    /// Periodic activation owner for each discrete row.
    ///
    /// `None` denotes an ordinary event-iteration row. A clock-owned row is
    /// evaluated only when the referenced exact lattice ticks.
    pub clock_owners: Vec<Option<PeriodicClockId>>,
    /// Compact B.1c maps. Scalar owners remain in `rhs`; a structured owner is
    /// represented exactly once here and is scalarized only by evaluation or
    /// backend adapter APIs.
    #[serde(default)]
    pub structured_rhs: ComputeBlock,
    #[serde(default)]
    pub structured_updates: Vec<StructuredDiscreteUpdate>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub enum RuntimeAssignmentRole {
    /// The row consumes already-selected values without evaluating a relation.
    RelationFree,
    /// The row evaluates a relation, depends on such a row, or writes relation memory.
    #[default]
    RelationEvaluating,
}

/// Compact target projection and row policy for one structured B.1c map node.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct StructuredDiscreteUpdate {
    /// Absolute index into [`DiscreteSolveSystem::structured_rhs`] nodes.
    pub node_index: usize,
    pub target: StructuredDiscreteTargetMap,
    pub role: DiscreteRowRole,
    pub pre_mode: DiscreteEventPreMode,
    pub observation_refresh: bool,
    pub integrator_history_effect: IntegratorHistoryEffect,
    pub clock_owner: Option<PeriodicClockId>,
}

/// One compact affine projection from map points to consecutive Y/P storage.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct StructuredDiscreteTargetMap {
    pub base: ScalarSlot,
    pub map: TensorOutputMap,
}

impl DiscreteSolveSystem {
    /// Checked scalar adapter view for one compact structured update.
    ///
    /// Each pair is `(target slot, structured_rhs output lane)`. Backends use
    /// this at their scalar boundary; the Solve IR retains only the compact map.
    pub fn structured_assignments(
        &self,
        update_index: usize,
    ) -> Result<Vec<(ScalarSlot, usize)>, SolveProblemShapeContractError> {
        let update = self.structured_updates.get(update_index).ok_or(
            SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: usize::MAX,
                detail: "update index is out of bounds",
                span: None,
            },
        )?;
        let node = self.structured_rhs.nodes.get(update.node_index).ok_or(
            SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute node index is out of bounds",
                span: None,
            },
        )?;
        let ComputeNode::Map {
            domain,
            output_map,
            span,
            ..
        } = node
        else {
            return Err(SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute node is not a Map",
                span: None,
            });
        };
        let sources = output_map.output_indices(domain).map_err(|_| {
            SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute output projection is invalid",
                span: Some(*span),
            }
        })?;
        let targets = update.target.map.output_indices(domain).map_err(|_| {
            SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "target projection is invalid",
                span: Some(*span),
            }
        })?;
        if sources.len() != targets.len() {
            return Err(SolveProblemShapeContractError::StructuredDiscreteUpdate {
                update_index,
                node_index: update.node_index,
                detail: "compute and target projections have different cardinality",
                span: Some(*span),
            });
        }
        targets
            .into_iter()
            .zip(sources)
            .map(|(offset, source)| {
                offset_scalar_slot(update.target.base, offset)
                    .map(|target| (target, source))
                    .ok_or(SolveProblemShapeContractError::StructuredDiscreteUpdate {
                        update_index,
                        node_index: update.node_index,
                        detail: "target base is not Y/P storage or its offset overflows",
                        span: Some(*span),
                    })
            })
            .collect()
    }
}

fn offset_scalar_slot(base: ScalarSlot, offset: usize) -> Option<ScalarSlot> {
    match base {
        ScalarSlot::Y { index, .. } => index.checked_add(offset).map(scalar_slot_y),
        ScalarSlot::P { index, .. } => index.checked_add(offset).map(scalar_slot_p),
        ScalarSlot::Time | ScalarSlot::Constant(_) => None,
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveEventPartition {
    pub root_conditions: ScalarProgramBlock,
    pub root_relation_memory_targets: Vec<Option<ScalarSlot>>,
    pub root_zero_domains: Vec<RootZeroDomain>,
    /// Compiler certificate describing which root memories may participate in
    /// post-commit algebraic coupling.
    pub root_relation_refresh_roles: Vec<RootRelationRefreshRole>,
    /// Hidden P slots that retain the previous value of each DAE condition.
    ///
    /// Event-action programs read these slots to distinguish a rising edge
    /// from a condition that merely remains true across an unrelated event.
    pub condition_memory_parameter_indices: Vec<usize>,
    pub scheduled_root_conditions: Vec<ScheduledRootCondition>,
    pub scheduled_time_events: Vec<f64>,
    pub dynamic_time_event_names: Vec<String>,
    pub dynamic_time_event_rhs: ScalarProgramBlock,
    pub action_conditions: ScalarProgramBlock,
    pub actions: Vec<SolveEventAction>,
    pub has_terminal_event: bool,
    pub delays: SolveDelayPartition,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Deserialize, Serialize)]
pub enum RootRelationRefreshRole {
    #[default]
    Frozen,
    AlgebraicDependent,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveDelayPartition {
    /// Current-value expressions whose accepted values populate history.
    pub source_rhs: ScalarProgramBlock,
    /// Delay amount evaluated at the query time.
    pub delay_time_rhs: ScalarProgramBlock,
    /// Maximum retained history horizon. When source `delayMax` is omitted,
    /// this row is identical to the corresponding delay-time row.
    pub delay_max_rhs: ScalarProgramBlock,
    /// Runtime-managed P slot receiving the delayed value for each row.
    pub value_parameter_indices: Vec<usize>,
    /// Whether the source uses piecewise-constant history rather than linear
    /// interpolation between accepted points.
    pub source_is_discrete: Vec<bool>,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub enum RootZeroDomain {
    Positive,
    NonPositive,
    #[default]
    Previous,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct ScheduledRootCondition {
    pub root_index: usize,
    pub period_seconds: f64,
    pub phase_seconds: f64,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SolveEventAction {
    pub kind: SolveEventActionKind,
    pub message: SolveEventMessage,
    pub span: rumoca_core::Span,
    pub origin: String,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveEventMessage {
    pub parts: Vec<SolveEventMessagePart>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum SolveEventMessagePart {
    Text(String),
    Conversion {
        value: Vec<LinearOp>,
        source: SolveStringConversionSource,
        format: SolveStringConversionFormat,
    },
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum SolveStringConversionSource {
    Real,
    Integer,
    Boolean,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum SolveStringConversionFormat {
    Options {
        minimum_length: Option<Vec<LinearOp>>,
        left_justified: Option<Vec<LinearOp>>,
        significant_digits: Option<Vec<LinearOp>>,
    },
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub enum SolveEventActionKind {
    Assert,
    Terminate,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveClockPartition {
    pub periodic_event_schedules: Vec<PeriodicEventSchedule>,
    /// Hidden Boolean-as-Real P slot for each typed periodic clock.
    ///
    /// The runtime derives each value from the schedule at the current event
    /// instant. These lanes make clock leaves computable inside mixed
    /// condition DAGs without creating another clock or row owner.
    pub activation_parameter_indices: Vec<usize>,
}

impl SolveClockPartition {
    pub fn periodic_clock_id(&self, index: usize) -> Option<PeriodicClockId> {
        self.periodic_event_schedules
            .get(index)
            .and_then(|_| u32::try_from(index).ok())
            .map(PeriodicClockId)
    }

    pub fn periodic_schedule(&self, clock: PeriodicClockId) -> Option<&PeriodicEventSchedule> {
        self.periodic_event_schedules.get(clock.index())
    }
}

/// Typed identity of one periodic schedule in a [`SolveClockPartition`].
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(transparent)]
pub struct PeriodicClockId(u32);

impl PeriodicClockId {
    pub const fn index(self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
pub enum DiscreteEventPreMode {
    /// Use the value from the start of the current clock/event tick.
    EventEntry,
    /// Hold `pre(..)` fixed for one event-iteration pass.
    Fixed,
    /// Read the current event-iteration fixed-point state.
    #[default]
    FollowCurrent,
}

/// Whether changing one typed discrete owner can invalidate continuous
/// integrator history.
///
/// `Preserve` is positive compiler evidence. The fail-closed default is
/// `Restart`, used whenever lowering cannot prove the dependency absent.
#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum IntegratorHistoryEffect {
    Preserve,
    #[default]
    Restart,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum DiscreteRowRole {
    /// A B.1c equation that participates in initialization and event iteration.
    Equation,
    /// An assignment that executes only on its owning event edge.
    EventAction,
    /// Runtime memory for detecting a condition edge.
    ConditionMemory,
}

#[derive(Clone, Debug, Serialize)]
pub struct PeriodicEventSchedule {
    lattice: rumoca_core::ClockLattice,
    anchor: rumoca_core::ClockPhaseAnchor,
}

impl PeriodicEventSchedule {
    /// Construct an exact rational schedule from finite second values.
    ///
    /// This is the external-boundary constructor for solver fixtures and
    /// decoded configuration. Compiler lowering should pass its already proven
    /// [`rumoca_core::ClockLattice`] through [`Self::new`].
    pub fn from_seconds(
        period: f64,
        phase: f64,
    ) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        Self::new(rumoca_core::ClockLattice::from_seconds(period, phase)?)
    }

    pub fn new(
        lattice: rumoca_core::ClockLattice,
    ) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        Self::from_schedule(rumoca_core::PeriodicClockSchedule::absolute(lattice)?)
    }

    pub fn from_schedule(
        schedule: rumoca_core::PeriodicClockSchedule,
    ) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        let schedule = match schedule.anchor() {
            rumoca_core::ClockPhaseAnchor::Absolute => {
                rumoca_core::PeriodicClockSchedule::absolute(schedule.lattice())?
            }
            rumoca_core::ClockPhaseAnchor::SimulationStart => {
                rumoca_core::PeriodicClockSchedule::simulation_start_relative(schedule.lattice())?
            }
        };
        Ok(Self {
            lattice: schedule.lattice(),
            anchor: schedule.anchor(),
        })
    }

    /// The authoritative exact rational lattice (MLS §16.3/§16.5).
    pub const fn lattice(&self) -> rumoca_core::ClockLattice {
        self.lattice
    }

    pub const fn anchor(&self) -> rumoca_core::ClockPhaseAnchor {
        self.anchor
    }

    /// Resolve a simulation-start-relative phase for one ME instance.
    pub fn resolved_at(&self, start_time: f64) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        let schedule = match self.anchor {
            rumoca_core::ClockPhaseAnchor::Absolute => {
                rumoca_core::PeriodicClockSchedule::absolute(self.lattice)?
            }
            rumoca_core::ClockPhaseAnchor::SimulationStart => {
                rumoca_core::PeriodicClockSchedule::simulation_start_relative(self.lattice)?
            }
        };
        Self::from_schedule(schedule.resolve_at(start_time)?)
    }

    pub fn period_seconds(&self) -> f64 {
        self.lattice.period_seconds()
    }

    pub fn phase_seconds(&self) -> f64 {
        self.lattice.phase_seconds()
    }

    /// Instant of tick `index` in seconds, computed exactly then rounded once.
    ///
    /// A schedule with no rational form or a tick outside the exact integer
    /// representation reports the original lattice error. Authoritative
    /// schedulers must not replace that failure with floating-point arithmetic.
    pub fn exact_tick_time_seconds(
        &self,
        index: impl Into<i128>,
    ) -> Result<f64, rumoca_core::ClockLatticeErrorKind> {
        self.lattice.tick_time_seconds(index)
    }
}

impl Default for PeriodicEventSchedule {
    fn default() -> Self {
        Self::new(
            rumoca_core::ClockLattice::from_interval_counter(1, 1)
                .expect("one-second clock lattice is valid"),
        )
        .expect("one-second periodic schedule is valid")
    }
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PeriodicEventScheduleWire {
    lattice: rumoca_core::ClockLattice,
    #[serde(default)]
    anchor: rumoca_core::ClockPhaseAnchor,
}

impl<'de> Deserialize<'de> for PeriodicEventSchedule {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let wire = PeriodicEventScheduleWire::deserialize(deserializer)?;
        let schedule = match wire.anchor {
            rumoca_core::ClockPhaseAnchor::Absolute => {
                rumoca_core::PeriodicClockSchedule::absolute(wire.lattice)
            }
            rumoca_core::ClockPhaseAnchor::SimulationStart => {
                rumoca_core::PeriodicClockSchedule::simulation_start_relative(wire.lattice)
            }
        }
        .map_err(serde::de::Error::custom)?;
        Self::from_schedule(schedule).map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolverNameIndexMaps {
    pub names: Vec<String>,
    pub name_to_idx: IndexMap<String, usize>,
    pub base_to_indices: IndexMap<String, Vec<usize>>,
}

/// Source slot for a `__pre__.*` parameter binding.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub enum PreParamSource {
    /// Copy from `y[index]` at event entry.
    Y { index: usize },
    /// Copy from `p[index]` (snapshot) at event entry.
    P { index: usize },
}

/// Maps a `__pre__.*` parameter's P-slot to the source slot it should be
/// snapshot-copied from at event entry. Built by phase-solve-lower from the
/// VarLayout after DAE-IR pre_lowering has run.
#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PreParamBinding {
    pub dest_p_index: usize,
    pub source: PreParamSource,
    /// Owning periodic clock for MLS §16 `previous()` history.
    ///
    /// `None` denotes ordinary Modelica `pre()` history and is committed after
    /// every event. A scheduled binding is committed only when this clock
    /// ticks, so unrelated roots and other clocks cannot advance its history.
    pub clock_schedule: Option<PeriodicEventSchedule>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum SolveVariableStorageRole {
    Parameter,
    Constant,
    ExternalInput,
    State,
    Algebraic,
    Output,
    DiscreteReal,
    DiscreteValue,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum SolveVariableValueKind {
    Real,
    Integer,
    Boolean,
    Enumeration,
    String,
}

#[derive(Clone, Copy, Debug, PartialEq, Deserialize, Serialize)]
pub struct SolveVariableStorageRun {
    pub base: ScalarSlot,
    pub scalar_count: usize,
    pub role: SolveVariableStorageRole,
    pub value_kind: SolveVariableValueKind,
}

/// Immutable typed declaration replayed independently of storage projection.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub struct SolveVariableDeclaration {
    role: SolveVariableStorageRole,
    value_kind: SolveVariableValueKind,
}

impl SolveVariableDeclaration {
    pub const fn new(role: SolveVariableStorageRole, value_kind: SolveVariableValueKind) -> Self {
        Self { role, value_kind }
    }

    pub const fn role(self) -> SolveVariableStorageRole {
        self.role
    }

    pub const fn value_kind(self) -> SolveVariableValueKind {
        self.value_kind
    }
}

impl SolveVariableStorageRun {
    pub fn event_iteration_kind(self) -> Option<EventIterationValueKind> {
        match (self.role, self.value_kind) {
            (SolveVariableStorageRole::DiscreteReal, SolveVariableValueKind::Real) => {
                Some(EventIterationValueKind::Real)
            }
            (SolveVariableStorageRole::DiscreteValue, SolveVariableValueKind::Integer) => {
                Some(EventIterationValueKind::Integer)
            }
            (SolveVariableStorageRole::DiscreteValue, SolveVariableValueKind::Boolean) => {
                Some(EventIterationValueKind::Boolean)
            }
            (SolveVariableStorageRole::DiscreteValue, SolveVariableValueKind::Enumeration) => {
                Some(EventIterationValueKind::Enumeration)
            }
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveLayout {
    pub solver_maps: SolverNameIndexMaps,
    /// Dense DAE variable ordinal to its first Solve storage slot.
    ///
    /// Scalar `k` of a variable is stored at `base + k` in the same column.
    /// This is the canonical cross-phase coordinate map; display names are not
    /// used to recover compiler identity.
    pub variable_storage_runs: Vec<SolveVariableStorageRun>,
    /// Canonical typed DAE declarations in the same dense identity order.
    pub variable_declarations: Vec<SolveVariableDeclaration>,
    pub state_scalar_count: usize,
    pub algebraic_scalar_count: usize,
    pub output_scalar_count: usize,
    pub parameter_count: usize,
    pub compiled_parameter_len: usize,
    pub input_scalar_names: Vec<String>,
    pub discrete_real_scalar_names: Vec<String>,
    pub discrete_valued_scalar_names: Vec<String>,
    pub relation_memory_parameter_indices: Vec<usize>,
    pub initial_event_parameter_index: Option<usize>,
    /// P-slot that is true only while applying the final simulation event.
    pub terminal_event_parameter_index: Option<usize>,
    /// Hidden P-slot used by initialization residuals that contain
    /// `homotopy(actual, simplified)`.
    ///
    /// The initialization driver advances this value from zero to one. Models
    /// without homotopy expressions omit the slot entirely.
    pub initial_homotopy_parameter_index: Option<usize>,
    /// Snapshot bindings for `__pre__.*` parameters created by DAE-IR
    /// pre_lowering. At event entry the runtime copies each source slot into
    /// the corresponding dest P-slot before the event equations evaluate.
    pub pre_param_bindings: Vec<PreParamBinding>,
}

impl SolveLayout {
    pub fn solver_maps(&self) -> &SolverNameIndexMaps {
        &self.solver_maps
    }

    pub fn variable_scalar_slot(&self, variable: usize, scalar: usize) -> Option<ScalarSlot> {
        let run = self.variable_storage_runs.get(variable)?;
        if scalar >= run.scalar_count {
            return None;
        }
        match run.base {
            ScalarSlot::Y { index, .. } => index.checked_add(scalar).map(scalar_slot_y),
            ScalarSlot::P { index, .. } => index.checked_add(scalar).map(scalar_slot_p),
            ScalarSlot::Time | ScalarSlot::Constant(_) => None,
        }
    }

    pub fn state_scalar_count(&self) -> usize {
        self.state_scalar_count
    }

    pub fn algebraic_scalar_count(&self) -> usize {
        self.algebraic_scalar_count
    }

    pub fn output_scalar_count(&self) -> usize {
        self.output_scalar_count
    }

    pub fn solver_scalar_count(&self) -> usize {
        self.solver_maps.names.len()
    }

    pub fn input_scalar_names(&self) -> &[String] {
        &self.input_scalar_names
    }

    pub fn input_parameter_index(&self, name: &str) -> Option<usize> {
        self.input_scalar_names
            .iter()
            .position(|candidate| candidate == name)
            .map(|offset| self.parameter_count + offset)
    }

    pub fn discrete_real_parameter_index(&self, name: &str) -> Option<usize> {
        self.discrete_real_scalar_names
            .iter()
            .position(|candidate| candidate == name)
            .map(|offset| self.parameter_count + self.input_scalar_names.len() + offset)
    }

    pub fn discrete_valued_parameter_index(&self, name: &str) -> Option<usize> {
        self.discrete_valued_scalar_names
            .iter()
            .position(|candidate| candidate == name)
            .map(|offset| {
                self.parameter_count
                    + self.input_scalar_names.len()
                    + self.discrete_real_scalar_names.len()
                    + offset
            })
    }

    pub fn has_runtime_parameter_tail(&self) -> bool {
        !self.input_scalar_names.is_empty()
            || !self.discrete_real_scalar_names.is_empty()
            || !self.discrete_valued_scalar_names.is_empty()
    }

    pub fn solver_idx_for_target(&self, target: &str) -> Option<usize> {
        solver_idx_for_target(target, &self.solver_maps.name_to_idx)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SolveVariableMeta {
    pub name: String,
    pub source_span: Span,
    pub role: String,
    pub is_state: bool,
    pub value_type: Option<String>,
    pub variability: Option<String>,
    pub time_domain: Option<String>,
    pub unit: Option<String>,
    pub start: Option<String>,
    pub min: Option<String>,
    pub max: Option<String>,
    pub nominal: Option<String>,
    pub fixed: Option<bool>,
    pub description: Option<String>,
}

impl SolveVariableMeta {
    pub fn empty_with_span(source_span: Span) -> Self {
        Self {
            name: String::new(),
            source_span,
            role: String::new(),
            is_state: bool::default(),
            value_type: None,
            variability: None,
            time_domain: None,
            unit: None,
            start: None,
            min: None,
            max: None,
            nominal: None,
            fixed: None,
            description: None,
        }
    }
}

/// Solver-facing Solve IR package.
///
/// This is pure data. DAE inspection, scalarization, start evaluation, and
/// mass-matrix extraction happen before this value is constructed.
#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct SolveModel {
    pub problem: SolveProblem,
    pub artifacts: SolveArtifacts,
    pub initial_y: Vec<f64>,
    /// Positive nominal values aligned with solver `y` slots.
    pub solver_nominals: Vec<f64>,
    pub parameters: Vec<f64>,
    pub external_tables: ExternalTables,
    pub visible_names: Vec<String>,
    pub visible_value_rows: ScalarProgramBlock,
    pub variable_meta: Vec<SolveVariableMeta>,
}

impl SolveModel {
    /// Clone this compile-time model and resolve every periodic schedule at
    /// the FMI instance's simulation start instant.
    pub fn resolved_periodic_schedules_at(
        &self,
        start_time: f64,
    ) -> Result<Self, rumoca_core::ClockLatticeErrorKind> {
        let mut resolved = self.clone();
        for schedule in &mut resolved.problem.clocks.periodic_event_schedules {
            *schedule = schedule.resolved_at(start_time)?;
        }
        for binding in &mut resolved.problem.solve_layout.pre_param_bindings {
            if let Some(schedule) = &mut binding.clock_schedule {
                *schedule = schedule.resolved_at(start_time)?;
            }
        }
        Ok(resolved)
    }

    pub fn state_scalar_count(&self) -> usize {
        self.problem.solve_layout.state_scalar_count()
    }

    pub fn solver_scalar_count(&self) -> usize {
        self.problem.solve_layout.solver_scalar_count()
    }

    pub fn initialization_projection_unknowns(&self) -> &[ScalarSlot] {
        &self.problem.initialization.projection_unknowns
    }

    /// Return a finite positive scale for one solver variable.
    ///
    /// The declared nominal is the baseline. A larger start magnitude expands
    /// the scale so solver tolerances remain meaningful for large initial
    /// values and runtime start overrides.
    pub fn solver_variable_scale(&self, index: usize) -> f64 {
        let nominal = self
            .solver_nominals
            .get(index)
            .copied()
            .filter(|value| value.is_finite() && *value > 0.0)
            .unwrap_or(1.0);
        let start_magnitude = self
            .initial_y
            .get(index)
            .copied()
            .filter(|value| value.is_finite())
            .map_or(0.0, f64::abs);
        nominal.max(start_magnitude)
    }
}

pub fn solver_idx_for_target(target: &str, name_to_idx: &IndexMap<String, usize>) -> Option<usize> {
    if let Some(&idx) = name_to_idx.get(target) {
        return Some(idx);
    }
    if let Some(scalar) = rumoca_core::parse_scalar_name(target)
        && scalar.indices.iter().all(|index| *index == 1)
    {
        return name_to_idx.get(scalar.base).copied();
    }
    None
}
