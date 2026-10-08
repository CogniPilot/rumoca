//! The trajectory sensitivity construction of a Solve problem, with its proofs.
//!
//! A sensitivity problem is *decided* here, once, and consumed by every
//! executor (SOLVE-C74 to SOLVE-C77): which parameters may be differentiated,
//! whether the model's trajectory is covered by the variational equations at
//! all, the layout of the augmented system `[x | S_1 .. S_m | q_1 .. q_k]`, the
//! consistent initialization structure, and the checkpoint contract of the
//! adjoint. An executor (the RK45 driver today, a native backend later) only
//! evaluates the rows these proofs admit.
//!
//! The admissibility predicate has three parts. Events that could change the
//! trajectory are refused: time events, clocks, delays, a solver coordinate an
//! event resets, or a value an event sets that the continuous rows read. A
//! relation evaluated without an event (`noEvent`, or inside a function) is
//! refused when it compares a solver coordinate, or time against a requested
//! parameter: the right-hand side jumps across a surface the trajectory crosses
//! (or that moves with the parameter), and a one-sided sensitivity through it is
//! not constructed. A relation of parameters alone is constant along the
//! trajectory and admitted. Finally
//! the directional-derivative seed must span the solver vector.

use std::collections::{BTreeMap, BTreeSet};

use rumoca_core::Span;

use crate::{
    CompareOp, ExclusionReason, LinearOp, LinearOpSliceKind, ParameterClassification, Reg,
    ScalarSlot, SolveProblem, SolveVisitor, event_writes, read_continuous_parameter_slots,
};

/// Why a sensitivity problem cannot be constructed.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum SensitivityRefusal {
    #[error("`{0}` is not a parameter of this model")]
    UnknownParameter(String),
    #[error("`{name}` cannot be differentiated: {}", reason.describe())]
    NotDifferentiable {
        name: String,
        reason: ExclusionReason,
    },
    #[error(
        "the initial value of `{state}` is a constant its start expression had when the model was \
         lowered, and that expression reads the requested parameter `{parameter}`; its initial \
         sensitivity is not differentiated (declare the state `fixed = true` so the \
         initialization defines it)"
    )]
    BakedInitialState { parameter: String, state: String },
    #[error("`{0}` is requested twice")]
    DuplicateParameter(String),
    #[error("it has delay channels")]
    DelayChannels,
    #[error("it has periodic clocks")]
    PeriodicClocks,
    #[error("it has time events")]
    TimeEvents,
    #[error("it has event transactions or structured discrete updates")]
    OpaqueEvents,
    #[error("an event resets a solver variable")]
    EventResetsSolverVariable,
    #[error(
        "an event relation (an `if` or `when` on a state, algebraic, or parameter) switches a \
         value the continuous rows read; one-sided sensitivities across a switching surface are \
         not constructed, and only a `noEvent` relation of parameters or time that the request \
         does not differentiate is admitted"
    )]
    EventValueRead { span: Option<Span> },
    #[error(
        "a relation under `noEvent` (or inside a function) compares states or algebraics, or \
         time against a requested parameter, so the right-hand side jumps across a surface the \
         trajectory crosses; one-sided sensitivities across a switching surface are not \
         constructed, and only relations of parameters, or of time and unrequested parameters, \
         are admitted"
    )]
    SwitchingSurface { span: Option<Span> },
    #[error(
        "the directional-derivative seed spans {y_scalars} Y scalars but the solver vector has \
         {solver_count}"
    )]
    SeedLayout {
        y_scalars: usize,
        solver_count: usize,
    },
    #[error("the initial sensitivity is undefined: {0}")]
    Initialization(String),
    #[error(
        "the plugin declares no continuous-extension order, so the {nodes}-node checkpoint \
         contract of degree {degree} cannot be proved"
    )]
    UndeclaredExtensionOrder { nodes: usize, degree: u32 },
    #[error(
        "the plugin's continuous extension of order {order} exceeds the {nodes}-node checkpoint contract of degree {degree}"
    )]
    CheckpointContract {
        order: u32,
        nodes: usize,
        degree: u32,
    },
    #[error(
        "the forward trajectory needs {needed} stored values, above the budget of {budget}; \
         loosen the tolerance, shorten the horizon, or raise the checkpoint budget \
         (`--checkpoint-budget <bytes>`)"
    )]
    CheckpointCapacity { needed: usize, budget: usize },
}

impl SensitivityRefusal {
    /// The source span of the relation the refusal names, when it names one.
    #[must_use]
    pub const fn span(&self) -> Option<Span> {
        match self {
            Self::EventValueRead { span } | Self::SwitchingSurface { span } => *span,
            _ => None,
        }
    }

    /// Whether the refusal is a defect of the requested parameter set, not of
    /// the model.
    #[must_use]
    pub const fn is_request_error(&self) -> bool {
        matches!(
            self,
            Self::UnknownParameter(_)
                | Self::NotDifferentiable { .. }
                | Self::BakedInitialState { .. }
                | Self::DuplicateParameter(_)
        )
    }
}

/// One parameter a trajectory is differentiated with respect to.
#[derive(Debug, Clone, PartialEq)]
pub struct SensitivityParameter {
    pub name: String,
    /// Slot in the runtime `p[]` vector.
    pub slot: usize,
}

/// The layout of the augmented system `[x | S_1 .. S_m | q_1 .. q_k]`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SensitivityLayout {
    pub states: usize,
    pub parameters: usize,
    pub quadratures: usize,
}

impl SensitivityLayout {
    #[must_use]
    pub const fn width(self) -> usize {
        self.states * (1 + self.parameters) + self.quadratures
    }

    /// The range of the sensitivity column of parameter `j`.
    #[must_use]
    pub const fn sensitivity(self, j: usize) -> std::ops::Range<usize> {
        let start = self.states * (1 + j);
        start..start + self.states
    }

    #[must_use]
    pub const fn quadrature_start(self) -> usize {
        self.states * (1 + self.parameters)
    }
}

/// The checkpoint contract of the adjoint: the forward path is stored at
/// equispaced nodes of each accepted step, and the node polynomial reproduces
/// the plugin's continuous extension exactly when that extension has no more
/// than `degree` accuracy order.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CheckpointPolicy {
    /// Nodes per step, endpoints included.
    pub nodes: usize,
}

impl CheckpointPolicy {
    /// Five nodes: degree four, the Dormand-Prince continuous extension.
    pub const FIVE_NODE: Self = Self { nodes: 5 };

    /// A policy of `nodes` equispaced nodes, at least two (the endpoints).
    #[must_use]
    pub const fn new(nodes: usize) -> Option<Self> {
        if nodes < 2 {
            None
        } else {
            Some(Self { nodes })
        }
    }

    /// The polynomial degree the nodes determine.
    #[must_use]
    pub const fn degree(self) -> u32 {
        self.nodes.saturating_sub(1) as u32
    }

    /// The position of node `k` in the unit step: `k / degree`.
    #[must_use]
    pub fn node_fraction(self, k: usize) -> f64 {
        k as f64 / f64::from(self.degree().max(1))
    }

    /// Refuse a plugin whose declared continuous-extension order the nodes
    /// cannot reproduce, or that declares none. The order is declared before
    /// the first step, so this is proved once, not per accepted step.
    pub fn admit_extension_order(self, declared: Option<u32>) -> Result<(), SensitivityRefusal> {
        let Some(order) = declared else {
            return Err(SensitivityRefusal::UndeclaredExtensionOrder {
                nodes: self.nodes,
                degree: self.degree(),
            });
        };
        if order <= self.degree() {
            return Ok(());
        }
        Err(SensitivityRefusal::CheckpointContract {
            order,
            nodes: self.nodes,
            degree: self.degree(),
        })
    }

    /// Stored `f64` values a budget of `bytes` admits.
    #[must_use]
    pub const fn budget_values(bytes: u64) -> usize {
        (bytes / std::mem::size_of::<f64>() as u64) as usize
    }

    /// Refuse a stored size above the budget.
    pub fn admit_stored_values(
        needed: usize,
        budget_values: usize,
    ) -> Result<(), SensitivityRefusal> {
        if needed <= budget_values {
            return Ok(());
        }
        Err(SensitivityRefusal::CheckpointCapacity {
            needed,
            budget: budget_values,
        })
    }
}

/// The structure of the initialization system a consistent initial sensitivity
/// is derived from: unknowns `u` (the projection plan's own) and update targets
/// `w`, solved by the plan's residual rows and the update rows.
#[derive(Debug, Clone, PartialEq)]
pub struct InitialSensitivityPlan {
    /// Unknown slots, projection unknowns first, then update targets.
    pub slots: Vec<ScalarSlot>,
    /// Seed index of each slot over `[solver-y | parameter]`.
    pub seeds: Vec<usize>,
    /// Residual rows the projection plan solves.
    pub rows: Vec<usize>,
    /// Number of projection unknowns (the leading slots).
    pub unknown_count: usize,
}

/// Tolerances of the algebraic projection's fixed-point settle that every
/// trajectory evaluation uses, matching the simulation's own refresh.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SettlePolicy {
    pub tolerance: f64,
    pub max_iterations: usize,
}

impl SettlePolicy {
    /// The settle of a simulated run.
    pub const SIMULATION: Self = Self {
        tolerance: 1.0e-10,
        max_iterations: 64,
    };
}

/// One side of an admitted relation.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RelationOperand {
    /// A requested parameter, by runtime slot.
    Parameter(usize),
    Constant(f64),
}

/// A relation of a requested parameter and a constant or another requested
/// parameter, evaluated without an event and admitted because it is constant
/// along the trajectory. Its sensitivity is that of the active branch, and
/// exactly at the switching value the branch is one-sided, so a point on it is
/// reported (SOLVE-C75).
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct AdmittedRelation {
    pub span: Option<Span>,
    pub lhs: RelationOperand,
    pub rhs: RelationOperand,
    pub op: CompareOp,
}

/// A requested parameter sitting exactly on the switching value of an admitted
/// relation.
#[derive(Debug, Clone, PartialEq)]
pub struct SwitchingValueNote {
    pub parameter: String,
    pub value: f64,
    pub span: Option<Span>,
}

impl AdmittedRelation {
    fn value(operand: RelationOperand, parameters: &[f64]) -> Option<f64> {
        match operand {
            RelationOperand::Constant(value) => Some(value),
            RelationOperand::Parameter(slot) => parameters.get(slot).copied(),
        }
    }

    /// The common value of both sides at `parameters`, when they are equal: the
    /// relation sits exactly on its switching surface.
    fn switching_value(&self, parameters: &[f64]) -> Option<f64> {
        let (lhs, rhs) = (
            Self::value(self.lhs, parameters)?,
            Self::value(self.rhs, parameters)?,
        );
        (lhs == rhs).then_some(lhs)
    }

    fn requested_slot(&self) -> Option<usize> {
        [self.lhs, self.rhs]
            .into_iter()
            .find_map(|side| match side {
                RelationOperand::Parameter(slot) => Some(slot),
                RelationOperand::Constant(_) => None,
            })
    }
}

/// A sensitivity problem: every decision proved once.
#[derive(Debug, Clone, PartialEq)]
pub struct SensitivityProblem {
    parameters: Vec<SensitivityParameter>,
    initial: InitialSensitivityPlan,
    states: usize,
    checkpoint: CheckpointPolicy,
    settle: SettlePolicy,
    relations: Vec<AdmittedRelation>,
}

impl SensitivityProblem {
    /// Prove `problem` admits the variational equations for the parameters
    /// named in `requested`, judged against the parameter `classification` the
    /// lowering computed once, and derive the construction.
    pub fn construct(
        problem: &SolveProblem,
        requested: &[String],
        classification: &ParameterClassification,
    ) -> Result<Self, SensitivityRefusal> {
        prove_continuous(problem)?;
        let y_scalars = problem.layout.y_scalars();
        let solver_count = problem.solve_layout.solver_scalar_count();
        if y_scalars != solver_count {
            return Err(SensitivityRefusal::SeedLayout {
                y_scalars,
                solver_count,
            });
        }
        let parameters = resolve_parameters(problem, requested, classification)?;
        let chosen: BTreeSet<usize> = parameters.iter().map(|parameter| parameter.slot).collect();
        let relations = prove_no_switching_surface(problem, &chosen)?;
        Ok(Self {
            parameters,
            initial: initial_plan(problem)?,
            states: problem.solve_layout.state_scalar_count(),
            checkpoint: CheckpointPolicy::FIVE_NODE,
            settle: SettlePolicy::SIMULATION,
            relations,
        })
    }

    #[must_use]
    pub fn parameters(&self) -> &[SensitivityParameter] {
        &self.parameters
    }

    #[must_use]
    pub const fn initial(&self) -> &InitialSensitivityPlan {
        &self.initial
    }

    /// The checkpoint contract of the adjoint.
    #[must_use]
    pub const fn checkpoint_policy(&self) -> CheckpointPolicy {
        self.checkpoint
    }

    /// The same problem under another checkpoint contract.
    #[must_use]
    pub const fn with_checkpoint_policy(mut self, policy: CheckpointPolicy) -> Self {
        self.checkpoint = policy;
        self
    }

    /// The algebraic settle every evaluation of the problem uses.
    #[must_use]
    pub const fn settle(&self) -> SettlePolicy {
        self.settle
    }

    /// The augmented layout with `quadratures` objective quadratures.
    #[must_use]
    pub fn layout(&self, quadratures: usize) -> SensitivityLayout {
        SensitivityLayout {
            states: self.states,
            parameters: self.parameters.len(),
            quadratures,
        }
    }

    /// Per-entry tolerance scales of the augmented system: each state's own
    /// scale (`state_scales`), each sensitivity entry as its state over its
    /// parameter's magnitude (at least one) at `parameter_values`, and unit
    /// scale for every quadrature.
    #[must_use]
    pub fn nominals(
        &self,
        state_scales: &[f64],
        parameter_values: &[f64],
        quadratures: usize,
    ) -> Vec<f64> {
        let mut nominals = Vec::with_capacity(self.layout(quadratures).width());
        nominals.extend_from_slice(state_scales);
        for parameter in &self.parameters {
            let scale = parameter_values
                .get(parameter.slot)
                .map_or(1.0, |value| value.abs().max(1.0));
            nominals.extend(state_scales.iter().map(|state| state / scale));
        }
        nominals.extend(std::iter::repeat_n(1.0, quadratures));
        nominals
    }

    /// The requested parameters that sit exactly on the switching value of an
    /// admitted relation at `parameter_values`: there the sensitivity is that
    /// of one side of the switch only.
    #[must_use]
    pub fn switching_value_notes(&self, parameter_values: &[f64]) -> Vec<SwitchingValueNote> {
        self.relations
            .iter()
            .filter_map(|relation| {
                let value = relation.switching_value(parameter_values)?;
                let slot = relation.requested_slot()?;
                let parameter = self.parameters.iter().find(|p| p.slot == slot)?;
                Some(SwitchingValueNote {
                    parameter: parameter.name.clone(),
                    value,
                    span: relation.span,
                })
            })
            .collect()
    }
}

fn prove_continuous(problem: &SolveProblem) -> Result<(), SensitivityRefusal> {
    let events = &problem.events;
    if !events.delays.value_parameter_indices.is_empty() || !events.delays.source_rhs.is_empty() {
        return Err(SensitivityRefusal::DelayChannels);
    }
    if !problem.clocks.periodic_event_schedules.is_empty() {
        return Err(SensitivityRefusal::PeriodicClocks);
    }
    if !events.scheduled_time_events.is_empty()
        || !events.scheduled_root_conditions.is_empty()
        || !events.dynamic_time_event_names.is_empty()
        || events.has_terminal_event
    {
        return Err(SensitivityRefusal::TimeEvents);
    }
    let writes = event_writes(problem);
    if writes.opaque {
        return Err(SensitivityRefusal::OpaqueEvents);
    }
    if writes.solver_y {
        return Err(SensitivityRefusal::EventResetsSolverVariable);
    }
    let read = read_continuous_parameter_slots(problem);
    for (row, target) in events.root_relation_memory_targets.iter().enumerate() {
        if let Some(ScalarSlot::P { index, .. }) = target
            && read.contains(index)
        {
            return Err(SensitivityRefusal::EventValueRead {
                span: events.root_conditions.program_span(row),
            });
        }
    }
    if writes.parameters.iter().any(|index| read.contains(index)) {
        return Err(SensitivityRefusal::EventValueRead { span: None });
    }
    Ok(())
}

/// Resolve the requested names against the classification the lowering
/// computed once: an excluded parameter, one whose lowering-time initial state
/// would carry a wrong zero, and a duplicate are refused, each by name.
fn resolve_parameters(
    problem: &SolveProblem,
    names: &[String],
    classification: &ParameterClassification,
) -> Result<Vec<SensitivityParameter>, SensitivityRefusal> {
    let mut parameters: Vec<SensitivityParameter> = Vec::with_capacity(names.len());
    for name in names {
        match classification.verdict(name) {
            None => return Err(SensitivityRefusal::UnknownParameter(name.clone())),
            Some(Err(reason)) => {
                return Err(SensitivityRefusal::NotDifferentiable {
                    name: name.clone(),
                    reason,
                });
            }
            Some(Ok(())) => {}
        }
        let Some(ScalarSlot::P { index, .. }) = problem.layout.binding(name) else {
            return Err(SensitivityRefusal::UnknownParameter(name.clone()));
        };
        if parameters.iter().any(|parameter| parameter.slot == index) {
            return Err(SensitivityRefusal::DuplicateParameter(name.clone()));
        }
        if let Some(state) = classification.baked_start(name) {
            return Err(SensitivityRefusal::BakedInitialState {
                parameter: name.clone(),
                state: state.to_string(),
            });
        }
        parameters.push(SensitivityParameter {
            name: name.clone(),
            slot: index,
        });
    }
    Ok(parameters)
}

fn initial_plan(problem: &SolveProblem) -> Result<InitialSensitivityPlan, SensitivityRefusal> {
    let init = &problem.initialization;
    let y_scalars = problem.layout.y_scalars();
    let slots: Vec<ScalarSlot> = init
        .projection_unknowns()
        .iter()
        .chain(init.update_targets())
        .copied()
        .collect();
    let mut seeds = Vec::with_capacity(slots.len());
    for slot in &slots {
        seeds.push(match slot {
            ScalarSlot::Y { index, .. } => *index,
            ScalarSlot::P { index, .. } => y_scalars + *index,
            ScalarSlot::Time | ScalarSlot::Constant(_) => {
                return Err(SensitivityRefusal::Initialization(
                    "an initialization unknown is not a storage slot".to_string(),
                ));
            }
        });
    }
    if !init.update_targets().is_empty() && updates_read_algebraics(problem) {
        return Err(SensitivityRefusal::Initialization(
            "an initialization update row reads a continuous algebraic".to_string(),
        ));
    }
    let rows: Vec<usize> = init
        .row_targets()
        .iter()
        .enumerate()
        .filter_map(|(row, target)| target.map(|_| row))
        .collect();
    let unknown_count = init.projection_unknowns().len();
    if rows.len() != unknown_count {
        return Err(SensitivityRefusal::Initialization(format!(
            "the initialization solves {} rows for {unknown_count} unknowns",
            rows.len()
        )));
    }
    Ok(InitialSensitivityPlan {
        slots,
        seeds,
        rows,
        unknown_count,
    })
}

/// Whether an initialization update row loads a continuous algebraic.
#[must_use]
pub fn updates_read_algebraics(problem: &SolveProblem) -> bool {
    let states = problem.solve_layout.state_scalar_count();
    problem
        .initialization
        .update_rhs()
        .programs()
        .iter()
        .flatten()
        .any(|op| match op {
            LinearOp::LoadY { index, .. } => *index >= states,
            LinearOp::TensorLoad {
                input: crate::TensorInputKind::Y,
                input_start,
                count,
                ..
            } => input_start.saturating_add(*count) > states,
            _ => false,
        })
}

/// Prove no relation the continuous rows evaluate without an event compares a
/// solver coordinate, or time with a chosen parameter, and collect the
/// relations of chosen parameters that are admitted.
fn prove_no_switching_surface(
    problem: &SolveProblem,
    chosen: &BTreeSet<usize>,
) -> Result<Vec<AdmittedRelation>, SensitivityRefusal> {
    struct Scan<'a> {
        chosen: &'a BTreeSet<usize>,
        relations: Vec<AdmittedRelation>,
    }
    impl SolveVisitor for Scan<'_> {
        type Error = SensitivityRefusal;

        fn visit_linear_op_slice(
            &mut self,
            kind: LinearOpSliceKind,
            ops: &[LinearOp],
        ) -> Result<(), Self::Error> {
            Taint::new(self.chosen, slice_span(kind), &mut self.relations).scan(ops)
        }
    }
    let mut scan = Scan {
        chosen,
        relations: Vec::new(),
    };
    scan.visit_continuous_system(&problem.continuous)?;
    Ok(scan.relations)
}

fn slice_span(kind: LinearOpSliceKind) -> Option<Span> {
    match kind {
        LinearOpSliceKind::ScalarProgram { span, .. } => span,
        LinearOpSliceKind::GuardedAssignmentProgram { span, .. }
        | LinearOpSliceKind::MatMulLhs { span, .. }
        | LinearOpSliceKind::MatMulRhs { span, .. }
        | LinearOpSliceKind::LinSolveSetup { span, .. }
        | LinearOpSliceKind::MapBase { span, .. }
        | LinearOpSliceKind::AffineStencilBase { span, .. } => Some(span),
    }
}

/// What a value may depend on.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
struct Dependence(u8);

impl Dependence {
    /// A solver coordinate, or anything not provably free of one.
    const STATE: Self = Self(1);
    const TIME: Self = Self(2);
    /// A requested parameter.
    const CHOSEN: Self = Self(4);
    const NONE: Self = Self(0);

    const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    const fn has(self, other: Self) -> bool {
        self.0 & other.0 != 0
    }

    /// Whether a relation over this dependence switches the right-hand side at
    /// a surface the trajectory or the parameters' own derivative would cross.
    ///
    /// A solver coordinate crosses its surface during integration, and time
    /// together with a requested parameter moves the switching instant with the
    /// parameter, so both need a jump term the construction does not state. A
    /// relation of parameters alone is constant along the trajectory: its
    /// sensitivity is that of the active branch, undefined only at the
    /// surface's own parameter value.
    const fn switches(self) -> bool {
        self.has(Self::STATE) || (self.has(Self::TIME) && self.has(Self::CHOSEN))
    }
}

/// Register dataflow marking what each value may depend on.
struct Taint<'a> {
    chosen: &'a BTreeSet<usize>,
    span: Option<Span>,
    relations: &'a mut Vec<AdmittedRelation>,
    flags: BTreeMap<Reg, Dependence>,
    /// Registers that hold exactly a chosen parameter or a constant.
    exact: BTreeMap<Reg, RelationOperand>,
    /// Every register at or above this one is unknown (a compact op whose
    /// extent is not enumerable).
    poisoned_from: Option<Reg>,
}

impl<'a> Taint<'a> {
    fn new(
        chosen: &'a BTreeSet<usize>,
        span: Option<Span>,
        relations: &'a mut Vec<AdmittedRelation>,
    ) -> Self {
        Self {
            chosen,
            span,
            relations,
            flags: BTreeMap::new(),
            exact: BTreeMap::new(),
            poisoned_from: None,
        }
    }

    fn of(&self, reg: Reg) -> Dependence {
        if self.poisoned_from.is_some_and(|from| reg >= from) {
            return Dependence::STATE;
        }
        self.flags.get(&reg).copied().unwrap_or_default()
    }

    fn set(&mut self, dst: Reg, flags: Dependence) {
        self.flags.insert(dst, flags);
        self.exact.remove(&dst);
    }

    fn set_exact(&mut self, dst: Reg, flags: Dependence, operand: RelationOperand) {
        self.set(dst, flags);
        self.exact.insert(dst, operand);
    }

    /// Record a relation of a chosen parameter that the register dataflow shows
    /// is a direct comparison of a parameter with a constant or another one.
    fn record_relation(&mut self, op: CompareOp, lhs: Reg, rhs: Reg) {
        let (Some(&left), Some(&right)) = (self.exact.get(&lhs), self.exact.get(&rhs)) else {
            return;
        };
        let relation = AdmittedRelation {
            span: self.span,
            lhs: left,
            rhs: right,
            op,
        };
        if relation.requested_slot().is_some() && !self.relations.contains(&relation) {
            self.relations.push(relation);
        }
    }

    /// Scan one op sequence. In a nested region every captured or carried
    /// load is unknown and so a possible state dependence.
    fn scan(&mut self, ops: &[LinearOp]) -> Result<(), SensitivityRefusal> {
        for op in ops {
            self.step(op)?;
        }
        Ok(())
    }

    fn step(&mut self, op: &LinearOp) -> Result<(), SensitivityRefusal> {
        match op {
            LinearOp::Const { dst, value } => {
                self.set_exact(*dst, Dependence::NONE, RelationOperand::Constant(*value));
            }
            LinearOp::LoadTime { dst } => self.set(*dst, Dependence::TIME),
            LinearOp::LoadP { dst, index } if self.chosen.contains(index) => {
                self.set_exact(*dst, Dependence::CHOSEN, RelationOperand::Parameter(*index));
            }
            LinearOp::LoadP { dst, .. } => self.set(*dst, Dependence::NONE),
            LinearOp::LoadIndexedP {
                dst,
                base,
                count,
                index,
            } => {
                let hit = (*base..base + count).any(|slot| self.chosen.contains(&slot));
                let flags = if hit {
                    Dependence::CHOSEN
                } else {
                    Dependence::NONE
                };
                self.set(*dst, flags.union(self.of(*index)));
            }
            LinearOp::Move { dst, src } => {
                let exact = self.exact.get(src).copied();
                self.set(*dst, self.of(*src));
                if let Some(operand) = exact {
                    self.exact.insert(*dst, operand);
                }
            }
            LinearOp::Unary { dst, arg: src, .. } => self.set(*dst, self.of(*src)),
            LinearOp::Binary { dst, lhs, rhs, .. } => {
                self.set(*dst, self.of(*lhs).union(self.of(*rhs)));
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => self.set(
                *dst,
                self.of(*cond)
                    .union(self.of(*if_true))
                    .union(self.of(*if_false)),
            ),
            LinearOp::Compare { dst, op, lhs, rhs } => {
                let flags = self.of(*lhs).union(self.of(*rhs));
                if flags.switches() {
                    return Err(SensitivityRefusal::SwitchingSurface { span: self.span });
                }
                self.record_relation(*op, *lhs, *rhs);
                self.set(*dst, flags);
            }
            LinearOp::StoreOutput { .. }
            | LinearOp::StoreOutputRange { .. }
            | LinearOp::StoreOutputFoldTensorUpdate { .. } => {}
            _ => self.other(op)?,
        }
        Ok(())
    }

    /// Any other op: its results are unknown, so a possible state dependence;
    /// nested regions are scanned for relations of their own.
    fn other(&mut self, op: &LinearOp) -> Result<(), SensitivityRefusal> {
        match op {
            LinearOp::FunctionFold { program, .. }
            | LinearOp::GuardedFunctionFold { program, .. }
            | LinearOp::StoreOutputFunctionFold { program, .. } => {
                for region in program.regions() {
                    Taint::new(self.chosen, self.span, &mut *self.relations).scan(region)?;
                }
            }
            LinearOp::FunctionConditional { program, .. } => {
                for arm in &program.arms {
                    Taint::new(self.chosen, self.span, &mut *self.relations)
                        .scan(&arm.condition)?;
                    Taint::new(self.chosen, self.span, &mut *self.relations).scan(&arm.result)?;
                }
                Taint::new(self.chosen, self.span, &mut *self.relations).scan(&program.fallback)?;
            }
            _ => {}
        }
        let Some(dst) = op.dst_register() else {
            return Ok(());
        };
        let count = op.dst_register_count();
        if count > (1 << 20) {
            self.poisoned_from = Some(self.poisoned_from.map_or(dst, |from| from.min(dst)));
        } else {
            for offset in 0..count {
                self.set(dst + offset as Reg, Dependence::STATE);
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{CompareOp, UnaryOp};

    fn chosen(slots: &[usize]) -> BTreeSet<usize> {
        slots.iter().copied().collect()
    }

    fn scan(
        chosen: &BTreeSet<usize>,
        ops: &[LinearOp],
    ) -> Result<Vec<AdmittedRelation>, SensitivityRefusal> {
        let mut relations = Vec::new();
        Taint::new(chosen, None, &mut relations).scan(ops)?;
        Ok(relations)
    }

    fn compare(lhs: LinearOp, rhs: LinearOp) -> Vec<LinearOp> {
        vec![
            lhs,
            rhs,
            LinearOp::Compare {
                dst: 2,
                op: CompareOp::Gt,
                lhs: 0,
                rhs: 1,
            },
        ]
    }

    #[test]
    fn a_relation_of_a_state_is_a_switching_surface() {
        let ops = compare(
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Const { dst: 1, value: 0.5 },
        );
        let refusal = scan(&chosen(&[]), &ops);
        assert!(matches!(
            refusal,
            Err(SensitivityRefusal::SwitchingSurface { .. })
        ));
    }

    #[test]
    fn a_relation_of_parameters_alone_is_constant_along_the_trajectory_and_admitted() {
        let ops = compare(
            LinearOp::LoadP { dst: 0, index: 3 },
            LinearOp::Const { dst: 1, value: 0.5 },
        );
        assert!(scan(&chosen(&[3]), &ops).is_ok());
        assert!(scan(&chosen(&[4]), &ops).is_ok());
    }

    #[test]
    fn a_relation_of_time_and_a_requested_parameter_moves_its_switching_instant() {
        let ops = vec![
            LinearOp::LoadTime { dst: 0 },
            LinearOp::LoadP { dst: 1, index: 3 },
            LinearOp::Compare {
                dst: 2,
                op: CompareOp::Gt,
                lhs: 0,
                rhs: 1,
            },
        ];
        assert!(scan(&chosen(&[3]), &ops).is_err());
        assert!(scan(&chosen(&[4]), &ops).is_ok());
    }

    #[test]
    fn a_relation_of_time_and_constants_is_admitted_and_taint_flows_through_arithmetic() {
        let ops = compare(
            LinearOp::LoadTime { dst: 0 },
            LinearOp::Const { dst: 1, value: 1.0 },
        );
        assert!(scan(&chosen(&[]), &ops).is_ok());
        let through = vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Unary {
                dst: 1,
                op: UnaryOp::Neg,
                arg: 0,
            },
            LinearOp::Move { dst: 2, src: 1 },
            LinearOp::Const { dst: 3, value: 0.0 },
            LinearOp::Compare {
                dst: 4,
                op: CompareOp::Lt,
                lhs: 2,
                rhs: 3,
            },
        ];
        assert!(scan(&chosen(&[]), &through).is_err());
    }

    #[test]
    fn the_checkpoint_policy_states_its_degree_and_budget() {
        let policy = CheckpointPolicy::FIVE_NODE;
        assert_eq!(policy.degree(), 4);
        assert!(policy.admit_extension_order(Some(4)).is_ok());
        assert!(matches!(
            policy.admit_extension_order(Some(5)),
            Err(SensitivityRefusal::CheckpointContract { order: 5, .. })
        ));
        assert!(matches!(
            policy.admit_extension_order(None),
            Err(SensitivityRefusal::UndeclaredExtensionOrder {
                nodes: 5,
                degree: 4
            })
        ));
        assert!(CheckpointPolicy::new(1).is_none());
        let three = CheckpointPolicy::new(3).expect("three nodes");
        assert_eq!(three.degree(), 2);
        assert_eq!(three.node_fraction(1), 0.5);
        assert!(three.admit_extension_order(Some(3)).is_err());
        assert_eq!(CheckpointPolicy::budget_values(80), 10);
        assert!(CheckpointPolicy::admit_stored_values(10, 10).is_ok());
        assert!(CheckpointPolicy::admit_stored_values(11, 10).is_err());
    }

    #[test]
    fn the_layout_places_each_column_after_the_states() {
        let layout = SensitivityLayout {
            states: 3,
            parameters: 2,
            quadratures: 1,
        };
        assert_eq!(layout.width(), 10);
        assert_eq!(layout.sensitivity(0), 3..6);
        assert_eq!(layout.sensitivity(1), 6..9);
        assert_eq!(layout.quadrature_start(), 9);
    }

    #[test]
    fn a_refusal_reports_whether_the_request_or_the_model_is_at_fault() {
        assert!(SensitivityRefusal::UnknownParameter("p".into()).is_request_error());
        assert!(!SensitivityRefusal::TimeEvents.is_request_error());
        assert!(SensitivityRefusal::TimeEvents.span().is_none());
    }

    fn relation_ops(op: CompareOp) -> Vec<LinearOp> {
        vec![
            LinearOp::LoadP { dst: 0, index: 3 },
            LinearOp::Const { dst: 1, value: 0.0 },
            LinearOp::Compare {
                dst: 2,
                op,
                lhs: 0,
                rhs: 1,
            },
        ]
    }

    #[test]
    fn an_admitted_relation_of_a_requested_parameter_is_recorded_once() {
        let mut ops = relation_ops(CompareOp::Gt);
        ops.extend(relation_ops(CompareOp::Gt));
        let relations = scan(&chosen(&[3]), &ops).expect("admitted");
        assert_eq!(relations.len(), 1);
        assert_eq!(relations[0].lhs, RelationOperand::Parameter(3));
        assert_eq!(relations[0].rhs, RelationOperand::Constant(0.0));
        // A parameter nobody differentiates is not one-sided.
        assert!(scan(&chosen(&[4]), &ops).expect("admitted").is_empty());
    }

    #[test]
    fn a_computed_operand_is_not_a_direct_relation() {
        let ops = vec![
            LinearOp::LoadP { dst: 0, index: 3 },
            LinearOp::Unary {
                dst: 1,
                op: UnaryOp::Neg,
                arg: 0,
            },
            LinearOp::Const { dst: 2, value: 0.0 },
            LinearOp::Compare {
                dst: 3,
                op: CompareOp::Gt,
                lhs: 1,
                rhs: 2,
            },
        ];
        assert!(scan(&chosen(&[3]), &ops).expect("admitted").is_empty());
    }

    #[test]
    fn a_moved_parameter_stays_a_direct_operand() {
        let ops = vec![
            LinearOp::LoadP { dst: 0, index: 3 },
            LinearOp::Move { dst: 1, src: 0 },
            LinearOp::Const { dst: 2, value: 0.0 },
            LinearOp::Compare {
                dst: 3,
                op: CompareOp::Gt,
                lhs: 1,
                rhs: 2,
            },
        ];
        assert_eq!(scan(&chosen(&[3]), &ops).expect("admitted").len(), 1);
    }

    fn problem_with(relations: Vec<AdmittedRelation>) -> SensitivityProblem {
        SensitivityProblem {
            parameters: vec![SensitivityParameter {
                name: "C".to_string(),
                slot: 3,
            }],
            initial: InitialSensitivityPlan {
                slots: Vec::new(),
                seeds: Vec::new(),
                rows: Vec::new(),
                unknown_count: 0,
            },
            states: 2,
            checkpoint: CheckpointPolicy::FIVE_NODE,
            settle: SettlePolicy::SIMULATION,
            relations,
        }
    }

    #[test]
    fn a_requested_parameter_on_a_switching_value_is_reported() {
        let relation = scan(&chosen(&[3]), &relation_ops(CompareOp::Gt)).expect("admitted");
        let problem = problem_with(relation);
        let mut values = vec![0.0; 4];
        values[3] = 0.5;
        assert!(problem.switching_value_notes(&values).is_empty());
        values[3] = 0.0;
        let notes = problem.switching_value_notes(&values);
        assert_eq!(notes.len(), 1);
        assert_eq!(notes[0].parameter, "C");
        assert_eq!(notes[0].value, 0.0);
        assert!(problem.switching_value_notes(&[]).is_empty());
    }

    #[test]
    fn nominals_scale_each_sensitivity_by_its_parameter_magnitude() {
        let problem = problem_with(Vec::new());
        let mut values = vec![0.0; 4];
        values[3] = -4.0;
        let nominals = problem.nominals(&[1.0, 2.0], &values, 1);
        assert_eq!(nominals, vec![1.0, 2.0, 0.25, 0.5, 1.0]);
        assert_eq!(
            problem.nominals(&[1.0, 2.0], &[], 0),
            vec![1.0, 2.0, 1.0, 2.0]
        );
        assert_eq!(problem.settle(), SettlePolicy::SIMULATION);
        let three = CheckpointPolicy::new(3).expect("three nodes");
        assert_eq!(
            problem.with_checkpoint_policy(three).checkpoint_policy(),
            three
        );
    }
}
