//! Which coordinates the MLS 3.6 §8.6 initialization system solves, and which
//! rows solve them.
//!
//! MLS 3.6 §8.6: "Before any operation is carried out with a Modelica model
//! (e.g., simulation or linearization), initialization takes place to assign
//! consistent values for all variables present in the model. During this phase,
//! also the derivatives `der(...)` and the pre-variables […] are interpreted as
//! unknown algebraic variables. The initialization uses all equations and
//! algorithms that are utilized in the intended operation."
//!
//! ## The unknowns/equations accounting rule
//!
//! The system MLS states is over *every* coordinate at once. This phase solves
//! the reduced form of it, which is exact because the reduction is by
//! substitution rather than by dropping rows:
//!
//! * **`der(x)` is not a free unknown here.** The structural matching already
//!   named the continuous row that determines each state derivative, and the
//!   Solve lowering replaces every `der(x)` an initialization row reads by that
//!   row's right-hand side (`ScalarCompiler::derivative_value`). So the
//!   der-equations are *active* at the initial instant — exactly the §8.6
//!   reading — but they are folded into the rows that read them instead of
//!   standing as extra rows over extra unknowns. `initial equation der(x) = 0`
//!   is therefore a row over `x`, not over `der(x)`.
//! * **A `fixed = true` start determines its state.** A source start proved
//!   independent of initialization unknowns supplies a given coordinate that
//!   projection cannot write. A start that depends on an unknown parameter is
//!   only a seed until its `state = startExpression` residual joins the same
//!   projection as the initial equations and retained manifold.
//! * **A start with `fixed = false`, or with no `fixed` at all, is a guess.**
//!   MLS 3.6 §4.8.1 gives `fixed` the default `false` for everything that is
//!   not a parameter or constant, and §8.6 says of such a start only that "the
//!   start value is used as a guess value". So a state's seed is an iteration
//!   guess and the initialization equations are what determine it.
//! * **A `fixed = false` parameter is an unknown** (§8.6: "All variables
//!   declared as parameter having `fixed = false` are treated as unknowns during
//!   the initialization phase, i.e., there must be additional equations for
//!   them"). `initial_parameters` owns that half and its binding-substitution
//!   ordering; this module only reads the coordinates it published.
//!
//! The resulting unknowns are states without given values and unbound
//! `fixed = false` parameters. Dependent fixed starts and transferred pins
//! remain explicit equations, alongside initial equations and every retained
//! manifold row.
//! A rectangular system keeps its surplus equations as consistency checks;
//! a free state without a determining equation retains its start guess.
//!
//! ## What is deliberately not an unknown, and what that costs
//!
//! A row reaching a coordinate outside that space is left unplanned. It stays in
//! the residual block, so the complete-residual test at the end of the projection
//! still evaluates it — but *what that evaluation proves* differs by kind, and
//! [`solve::InitializationRowRole`] carries the difference to the runtime so a
//! failure names the right defect instead of the friendliest one.
//!
//! **Algebraic and output reads fail closed until the reduced solve owns them.**
//! The runtime reconstructs those coordinates before evaluating the complete
//! initialization residual, so declaration seeds cannot certify a wrong initial
//! state. The projection still cannot move an algebraic or account for its total
//! derivative through the continuous system, so a row that needs that coupled
//! capability remains typed as unowned rather than being admitted unsoundly.
//! The two historical failure modes were:
//!
//! * `a = 2*time + 5; der(x) = a - x;` with `initial equation der(x) = 0`
//!   *silently simulates* `x(0) = 0` where OpenModelica gives `5`: the seeds
//!   cancel, so the check passes and the stated initial condition vanishes.
//! * the consistent `initial equation x = 5; x = a;` on the same model is
//!   *refused* on the stale seed (`EX001`) where OpenModelica initializes.
//!
//! The evaluation-local refresh closes the false-certificate hole without
//! claiming the larger capability. The owner that makes the shape fully solvable
//! remains algebraic refresh joining the initialization solve — folded into the
//! projection loop, or the algebraics carried as unknowns over their own
//! continuous rows with a checked total derivative.
//!
//! **Discrete reads: the check is honest, and the refusal is an over-refusal.** A
//! discrete coordinate *is* at its §8.6 value when the residual runs — the runtime
//! seeds and settles the discrete values before `settle_initialization_system`. So
//! the row checks a real number; it simply has no way to solve for the state it
//! also reads. `x + q = 5; x = d + 2;` with `d(start = 0, fixed = true)` is
//! satisfiable at `x = 2, q = 3`, which is what OpenModelica returns, and rumoca
//! reports `EX001` instead. That is an over-refusal against OMC, not merely an
//! unplanned row, and admitting it needs discretes in the unknown space's
//! *determined* half — task #44's event-machinery territory.
//!
//! **Array coordinates retain their exact scalar identity.** Initial equations,
//! structured family points, substituted parameter bindings, and matched
//! derivative rows use the same checked scalar dependency projection as
//! structural incidence. A projection failure remains unowned. A fixed state
//! declaration or transferred pin contributes only its actual scalar equations.
//!
//! ## Two choices this phase makes that the model does not
//!
//! **Which under-determined state keeps its guess.** For `x + y = 5` over two
//! guessed states, one equation cannot fix two coordinates. OpenModelica warns
//! ("The initial conditions are not fully specified") and keeps `x`, solving
//! `y = 5`; this planner takes the first augmenting assignment in solver-slot
//! order and keeps `y`, solving `x = 2`. Nothing in §8.6 picks between them,
//! because the model states nothing about it. The choice is pinned by
//! `an_under_determined_state_component_keeps_the_remaining_start_guesses` so it
//! stays a recorded fact rather than drift.
//!
//! **Which root of a nonlinear initialization block.** §8.6 says the `start` value
//! "is used as a guess value", and Newton from that guess converges to whichever
//! root it is nearest: `q*q = 4` from `start = 3` gives `q = 2`, from `start = -3`
//! it gives `q = -2`. Both satisfy the model, both are legal §8.6 answers, and the
//! declared `start` is the only thing that decides — so a model that cares must
//! say so with its `start`, and a comparison against another tool is a comparison
//! of guesses as much as of equations.

use std::collections::{BTreeMap, BTreeSet, HashMap, VecDeque};

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::initial_parameters::InitializationParameterOwnership;
use super::{DerivativeRowIndex, ScalarRowSource, scalar_count, variable_scalar_slot};
use crate::LowerError;
use crate::layout::LoweredLayout;

/// One coordinate the initialization projection may own.
///
/// The ordering is the planner's priority as well as its determinism: a
/// `fixed = false` parameter has no value at all without an initialization
/// equation (MLS §8.6), while an unmatched state falls back to the guess its
/// `start` carries. Matching the parameters first is what makes the augmenting
/// search spend its rows on the coordinates that cannot do without one.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum InitialUnknown {
    /// A `fixed = false` parameter scalar, named by its P-slot index.
    Parameter(usize),
    /// A state scalar whose `start` §8.6 leaves as a guess, named by its Y-slot
    /// index.
    State(usize),
}

impl InitialUnknown {
    fn slot(self) -> solve::ScalarSlot {
        match self {
            Self::Parameter(index) => solve::scalar_slot_p(index),
            Self::State(index) => solve::scalar_slot_y(index),
        }
    }

    const fn is_parameter(self) -> bool {
        matches!(self, Self::Parameter(_))
    }
}

/// The coordinate space one model's initialization system solves over.
pub(super) struct InitializationUnknownSpace<'a, 'dae> {
    view: dae::DaeView<'dae>,
    ownership: &'a InitializationParameterOwnership<'dae>,
    derivatives: &'a DerivativeRowIndex<'dae>,
    states: HashMap<(u32, usize), usize>,
    given_state_indices: BTreeSet<usize>,
}

/// Everything the initialization unknown space is assembled from.
pub(super) struct InitializationUnknownInputs<'a, 'dae> {
    pub(super) view: dae::DaeView<'dae>,
    pub(super) layout: &'a LoweredLayout<'dae>,
    pub(super) ownership: &'a InitializationParameterOwnership<'dae>,
    pub(super) derivatives: &'a DerivativeRowIndex<'dae>,
    pub(super) given_state_indices: &'a [usize],
}

/// Decide, once per model, who owns every coordinate the §8.6 system touches.
pub(super) fn initialization_unknown_space<'a, 'dae>(
    inputs: InitializationUnknownInputs<'a, 'dae>,
) -> Result<InitializationUnknownSpace<'a, 'dae>, LowerError> {
    let InitializationUnknownInputs {
        view,
        layout,
        ownership,
        derivatives,
        given_state_indices,
    } = inputs;
    Ok(InitializationUnknownSpace {
        view,
        ownership,
        derivatives,
        states: state_initial_slots(view, layout)?,
        given_state_indices: given_state_indices.iter().copied().collect(),
    })
}

impl<'dae> InitializationUnknownSpace<'_, 'dae> {
    fn all_projection_unknowns(&self) -> BTreeSet<InitialUnknown> {
        self.ownership
            .all_projection_unknown_slots()
            .map(InitialUnknown::Parameter)
            .chain(
                self.states
                    .values()
                    .copied()
                    .filter(|index| !self.given_state_indices.contains(index))
                    .map(InitialUnknown::State),
            )
            .collect()
    }
}

/// Exact state coordinates of the simultaneous initialization system.
fn state_initial_slots(
    view: dae::DaeView<'_>,
    layout: &LoweredLayout<'_>,
) -> Result<HashMap<(u32, usize), usize>, LowerError> {
    let mut slots = HashMap::new();
    for (id, variable) in view.variables() {
        if variable.role() != dae::VariableRole::State {
            continue;
        }
        let span = variable.declaration().span();
        for scalar in 0..variable.scalar_count() {
            let solve::ScalarSlot::Y { index, .. } =
                variable_scalar_slot(layout, id.index(), scalar, span)?
            else {
                return Err(LowerError::contract(
                    "a state does not occupy solver storage",
                    span,
                ));
            };
            slots.insert((id.index(), scalar), index);
        }
    }
    Ok(slots)
}

/// Where one initialization row's coordinate incidence is read from.
///
/// The three forms differ in what the walk may start from, not in what it proves.
pub(super) enum InitialRowIncidence<'dae> {
    /// The same scalar and structured domain point the emitted residual reads.
    Residual(ScalarRowSource<'dae>),
    /// An explicit fixed-value equation about one exact state scalar.
    StateValue {
        index: usize,
        terms: Vec<(dae::ExprId<'dae>, usize)>,
    },
    /// A fixed algebraic/output checked after solving the simultaneous
    /// continuous algebraic system. Its exact total incidence is implicit in
    /// that solve, so it conservatively joins every initialization unknown;
    /// zero numerical sensitivity can make the block fail, never certify a
    /// wrong value.
    ImplicitAlgebraic,
}

pub(super) struct InitialProjection {
    pub(super) plan: solve::InitializationProjectionPlan,
    /// What the projection does with each row, positionally by equation index.
    pub(super) row_roles: Vec<solve::InitializationRowRole>,
}

/// Plan the initialization unknowns the initialization system itself determines.
///
/// A block the runtime can solve has one equation per unknown, and the matching
/// is what names those equations. A component with rows to spare is legal — MLS
/// §8.6 lets a coordinate be determined by a declaration *and* be read by another
/// initialization equation — so a surplus row whose every unknown some other row
/// matched stays a consistency check the complete-residual test still has to
/// satisfy. That reading holds only for such a row, and the module header records
/// what an *unplanned* row's residual does and does not prove; every row is
/// labelled with which case it is so the runtime never has to guess.
///
/// A component whose rows cannot cover every unknown is under-determined, and the
/// two kinds of unknown answer that differently. A state falls back to the guess
/// its `start` carries, which is the §8.6 default-`fixed` reading and the same
/// value the runtime already seeds, so the rest of the component is still planned
/// around it. A `fixed = false` parameter has no such fallback — §8.6 says "there
/// must be additional equations for them" — so a component that cannot determine
/// one is left entirely unplanned, keeping the typed residual failure rather than
/// silently shipping the parameter's guess as its value.
///
/// The matching is structural, so it is rank-blind: it takes the first augmenting
/// assignment, which can pick a block whose Jacobian is numerically singular while
/// a different assignment of the same rows would not be. The runtime reports that
/// as a failed solve rather than a wrong answer, and a minimum-degree ordering
/// would choose better; until then the risk is accepted and named here rather than
/// left implied.
pub(super) fn plan_initialization_projection<'dae>(
    space: &InitializationUnknownSpace<'_, 'dae>,
    rows: &[InitialRowIncidence<'dae>],
) -> Result<InitialProjection, LowerError> {
    // Every row starts as a check between values the rest of the system fixed,
    // and is downgraded or promoted below by what the walk and the matching find.
    let mut row_roles = vec![solve::InitializationRowRole::SurplusCheck; rows.len()];
    let mut incidence: Vec<(usize, BTreeSet<InitialUnknown>)> = Vec::new();
    let mut projection_cache = rumoca_eval_dae::ScalarCoordinateProjectionCache::default();
    for (row, source) in rows.iter().enumerate() {
        match row_unknowns(space, source, &mut projection_cache) {
            RowIncidence::Owned(unknowns) if !unknowns.is_empty() => {
                incidence.push((row, unknowns));
            }
            // Nothing this row reads is an unknown: it is exactly the §8.6
            // consistency check the default role already names.
            RowIncidence::Owned(_) => {}
            RowIncidence::Unowned(kind) => {
                row_roles[row] = solve::InitializationRowRole::UnownedCoordinate(kind);
            }
        }
    }
    let mut blocks = Vec::new();
    for component in connected_components(&incidence) {
        let matched = match_component(&component).unwrap_or_default();
        record_component_roles(&component, &matched, &mut row_roles);
        if matched.is_empty() {
            continue;
        }
        blocks.extend(ordered_projection_blocks(&component, &matched)?);
    }
    for (row, source) in rows.iter().enumerate() {
        if !matches!(source, InitialRowIncidence::ImplicitAlgebraic) {
            continue;
        }
        row_roles[row] = match row_roles[row] {
            solve::InitializationRowRole::Solved => {
                solve::InitializationRowRole::SolvedThroughAlgebraicRefresh
            }
            solve::InitializationRowRole::SurplusCheck => {
                solve::InitializationRowRole::SurplusAlgebraicCheck
            }
            role => role,
        };
    }
    Ok(InitialProjection {
        plan: solve::InitializationProjectionPlan { blocks },
        row_roles,
    })
}

/// Triangularize the selected matching before numerical projection.
///
/// Sharing an unknown does not imply mutual dependence. A row defining `q`
/// must run before a row defining `y` from `q`, particularly when the latter
/// changes a Boolean branch between the start guess and the solved value.
fn ordered_projection_blocks(
    component: &ProjectionComponent,
    matched: &[(usize, InitialUnknown)],
) -> Result<Vec<solve::InitializationProjectionBlock>, LowerError> {
    let producer: BTreeMap<_, _> = matched
        .iter()
        .enumerate()
        .map(|(position, (_, unknown))| (*unknown, position))
        .collect();
    let reads: BTreeMap<_, _> = component
        .rows
        .iter()
        .copied()
        .zip(&component.row_unknowns)
        .collect();
    let dependencies = matched
        .iter()
        .enumerate()
        .map(|(position, (row, _))| {
            reads[row]
                .iter()
                .filter_map(|unknown| producer.get(unknown).copied())
                .filter(|dependency| *dependency != position)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let components =
        rumoca_core::dependency_graph::dependency_first_sccs(&dependencies).map_err(|error| {
            LowerError::UnspannedContractViolation {
                reason: format!("initialization matching has invalid dependency indices: {error}"),
            }
        })?;
    Ok(components
        .into_iter()
        .map(|component| {
            let (rows, unknowns) = component
                .members
                .iter()
                .map(|position| {
                    let (row, unknown) = matched[*position];
                    (row, unknown.slot())
                })
                .unzip();
            solve::InitializationProjectionBlock { rows, unknowns }
        })
        .collect())
}

/// Say, per row of one component, what the projection ended up doing with it.
///
/// A matched row is solved. An unmatched row is a *surplus check* only when every
/// unknown it reads was matched by some other row — then the value it checks is
/// one the block determined, which is the §8.6 shape that legitimately has more
/// equations than unknowns. An unmatched row that still reads an unmatched
/// coordinate is checking a number nothing solved, and saying "surplus check"
/// about it would name the wrong defect.
fn record_component_roles(
    component: &ProjectionComponent,
    matched: &[(usize, InitialUnknown)],
    row_roles: &mut [solve::InitializationRowRole],
) {
    let solved_rows: BTreeSet<usize> = matched.iter().map(|(row, _)| *row).collect();
    let solved_unknowns: BTreeSet<InitialUnknown> =
        matched.iter().map(|(_, unknown)| *unknown).collect();
    for (position, row) in component.rows.iter().copied().enumerate() {
        if solved_rows.contains(&row) {
            row_roles[row] = solve::InitializationRowRole::Solved;
            continue;
        }
        if component.row_unknowns[position].is_subset(&solved_unknowns) {
            row_roles[row] = solve::InitializationRowRole::SurplusCheck;
            continue;
        }
        row_roles[row] = solve::InitializationRowRole::UnownedCoordinate(
            solve::InitializationCoordinateKind::Unmatched,
        );
    }
}

/// The projection coordinates a row reads, or `None` when it also reads a
/// coordinate the initialization projection cannot own.
///
/// A parameter a binding owns is not itself an unknown, but the binding the row
/// was lowered to recompute is: the walk follows that binding so the incidence
/// matches the program the projection actually evaluates. A `der(x)` is followed
/// the same way, into the continuous row the Solve lowering substitutes for it.
fn row_unknowns<'dae>(
    space: &InitializationUnknownSpace<'_, 'dae>,
    row: &InitialRowIncidence<'dae>,
    projection_cache: &mut rumoca_eval_dae::ScalarCoordinateProjectionCache<'dae>,
) -> RowIncidence {
    let pending = match row {
        InitialRowIncidence::Residual(residual) => vec![residual.clone()],
        InitialRowIncidence::StateValue { terms, .. } => terms
            .iter()
            .map(|(expression, scalar)| ScalarRowSource {
                expression: *expression,
                scalar: *scalar,
                domain_point: None,
            })
            .collect(),
        InitialRowIncidence::ImplicitAlgebraic => {
            return RowIncidence::Owned(space.all_projection_unknowns());
        }
    };
    let mut incidence = InitialIncidence {
        unknowns: match row {
            InitialRowIncidence::StateValue { index, .. }
                if !space.given_state_indices.contains(index) =>
            {
                BTreeSet::from([InitialUnknown::State(*index)])
            }
            _ => BTreeSet::new(),
        },
        excluded: None,
        substituted: BTreeSet::new(),
        expanded: BTreeSet::new(),
        pending,
    };
    // One coordinate the projection cannot own already disqualifies the row, but
    // *which* one decides what the runtime is told, and the algebraic reading is
    // the one worth reporting (see the module header). So the walk keeps going
    // until it has found an algebraic or run out of expressions to expand.
    while !matches!(
        incidence.excluded,
        Some(solve::InitializationCoordinateKind::Algebraic)
    ) && let Some(row) = incidence.pending.pop()
    {
        let projected = rumoca_eval_dae::for_each_scalar_coordinate_cached(
            space.view,
            row.expression,
            row.scalar,
            row.domain_point
                .as_ref()
                .map(|(domain, point)| (*domain, point.as_slice())),
            projection_cache,
            |coordinate, scalar| incidence.visit(space, coordinate, scalar),
        );
        if projected.is_err() {
            incidence.exclude(solve::InitializationCoordinateKind::Unreadable);
        }
    }
    match incidence.excluded {
        None => RowIncidence::Owned(incidence.unknowns),
        Some(kind) => RowIncidence::Unowned(kind),
    }
}

/// What one initialization row contributes to the plan.
enum RowIncidence {
    /// The projection coordinates the row reads. May be empty: the row is then a
    /// check over coordinates the initialization instant has already determined.
    Owned(BTreeSet<InitialUnknown>),
    /// The row reads a coordinate outside the planned unknown space, of this kind.
    Unowned(solve::InitializationCoordinateKind),
}

/// How loudly one exclusion kind deserves to be reported, largest first.
///
/// An algebraic read outranks the rest because it is the only kind whose residual
/// says nothing either way — the others leave a row the runtime still checks
/// against a determined value. Ranking also keeps the reported kind deterministic
/// when a row reaches several, since the expression walk order is.
const fn exclusion_rank(kind: solve::InitializationCoordinateKind) -> u8 {
    match kind {
        solve::InitializationCoordinateKind::Algebraic => 4,
        solve::InitializationCoordinateKind::Discrete => 3,
        solve::InitializationCoordinateKind::Unreadable => 2,
        solve::InitializationCoordinateKind::Other => 1,
        solve::InitializationCoordinateKind::Unmatched => 0,
    }
}

/// The projection unknowns one initialization residual program reaches.
struct InitialIncidence<'dae> {
    unknowns: BTreeSet<InitialUnknown>,
    excluded: Option<solve::InitializationCoordinateKind>,
    /// Parameter bindings already followed, so a diamond is walked once.
    substituted: BTreeSet<(u32, usize)>,
    /// State derivatives already followed, so a derivative that reads itself
    /// through its own defining row terminates.
    expanded: BTreeSet<(u32, usize)>,
    pending: Vec<ScalarRowSource<'dae>>,
}

impl<'dae> InitialIncidence<'dae> {
    fn visit(
        &mut self,
        space: &InitializationUnknownSpace<'_, 'dae>,
        coordinate: dae::CoordinateView<'dae>,
        scalar: usize,
    ) {
        match coordinate {
            dae::CoordinateView::Parameter(parameter) => self.visit_parameter(space, parameter, scalar),
            dae::CoordinateView::State(state) => self.visit_state(space, state, scalar),
            dae::CoordinateView::Derivative(state) => self.visit_derivative(space, state, scalar),
            // MLS §8.6.1: the environment supplies inputs as known values.
            // Complete Solve-model construction requires a checked binding or
            // an explicit host value for every scalar before initialization.
            // They are coefficients of this solve, never projection unknowns.
            dae::CoordinateView::Input(_) => {}
            // The runtime reconstructs an algebraic before it certifies the
            // complete residual (module header), but this planner cannot yet
            // differentiate through or solve the simultaneous continuous
            // system, so the row remains outside the admitted reduced solve.
            dae::CoordinateView::Algebraic(_) => {
                self.exclude(solve::InitializationCoordinateKind::Algebraic);
            }
            dae::CoordinateView::DiscreteReal(_)
            | dae::CoordinateView::DiscreteValue(_)
            | dae::CoordinateView::PreDiscreteReal(_)
            | dae::CoordinateView::PreDiscreteValue(_)
            // A `pre()` read holds a settled event-history value at
            // initialization, so it is a discrete input to the row rather than
            // the continuous coordinate it names. Grouping it here rather than
            // leaving it to the `_` arm below is the same planning outcome —
            // both exclude the row — and only changes the reported exclusion
            // kind, which `exclusion_rank` ranks Discrete above Other.
            | dae::CoordinateView::PreState(_)
            | dae::CoordinateView::PreAlgebraic(_) => {
                self.exclude(solve::InitializationCoordinateKind::Discrete);
            }
            // A domain binder and a clock interval are compile-time constants, and
            // `time` is the known initialization instant.
            dae::CoordinateView::Time
            | dae::CoordinateView::ClockInterval(_)
            | dae::CoordinateView::Binder(_) => {}
            _ => self.exclude(solve::InitializationCoordinateKind::Other),
        }
    }

    /// Record why the row cannot be planned, keeping the loudest reason found.
    fn exclude(&mut self, kind: solve::InitializationCoordinateKind) {
        if self
            .excluded
            .is_none_or(|held| exclusion_rank(kind) > exclusion_rank(held))
        {
            self.excluded = Some(kind);
        }
    }

    fn visit_parameter(
        &mut self,
        space: &InitializationUnknownSpace<'_, 'dae>,
        parameter: dae::ParameterId<'dae>,
        scalar: usize,
    ) {
        if let Some(indices) = space.ownership.projection_unknown_slots(parameter.index()) {
            match indices.get(scalar) {
                Some(index) => {
                    self.unknowns.insert(InitialUnknown::Parameter(*index));
                }
                None => self.exclude(solve::InitializationCoordinateKind::Unreadable),
            }
            return;
        }
        if let Some(binding) = space.ownership.substitution(parameter.index())
            && self.substituted.insert((parameter.index(), scalar))
        {
            self.pending.push(ScalarRowSource {
                expression: binding,
                scalar: if scalar_count(space.view, binding) == 1 {
                    0
                } else {
                    scalar
                },
                domain_point: None,
            });
        }
    }

    fn visit_state(
        &mut self,
        space: &InitializationUnknownSpace<'_, 'dae>,
        state: dae::StateId<'dae>,
        scalar: usize,
    ) {
        match space.states.get(&(state.index(), scalar)) {
            Some(index) if !space.given_state_indices.contains(index) => {
                self.unknowns.insert(InitialUnknown::State(*index));
            }
            Some(_) => {}
            None => self.exclude(solve::InitializationCoordinateKind::Unreadable),
        }
    }

    /// Follow `der(x)` into the continuous row that defines it.
    ///
    /// This mirrors `ScalarCompiler::derivative_value`, which is what the lowered
    /// row evaluates: the derivative is not a coordinate of its own, it is that
    /// row solved for `der(x)`. Reading its incidence from anywhere else would
    /// plan a block over unknowns the program does not actually depend on.
    fn visit_derivative(
        &mut self,
        space: &InitializationUnknownSpace<'_, 'dae>,
        state: dae::StateId<'dae>,
        scalar: usize,
    ) {
        let Some(definition) = space.derivatives.definition(state, scalar) else {
            self.exclude(solve::InitializationCoordinateKind::Other);
            return;
        };
        if self.expanded.insert((state.index(), scalar)) {
            self.pending.push(definition.clone());
        }
    }
}

/// Assign each unknown of a component a distinct row that reads it.
///
/// Returns the matched `(row, unknown)` pairs, or `None` when a `fixed = false`
/// parameter of the component is left without one. This is the Hungarian
/// augmenting-path search over the row/unknown bipartite graph: each round either
/// matches the next unknown to a free row or reroutes an already-matched row to
/// make one free, and an unknown that neither reaches is one no row can determine.
/// Augmenting never un-matches an unknown, so running the parameters first (their
/// `InitialUnknown` ordering) guarantees the maximum number of them is covered.
fn match_component(component: &ProjectionComponent) -> Option<Vec<(usize, InitialUnknown)>> {
    let row_count = component.rows.len();
    let unknown_count = component.unknowns.len();
    let position: HashMap<InitialUnknown, usize> = component
        .unknowns
        .iter()
        .enumerate()
        .map(|(position, unknown)| (*unknown, position))
        .collect();
    let mut adjacency: Vec<Vec<usize>> = vec![Vec::new(); unknown_count];
    for (row, unknowns) in component.row_unknowns.iter().enumerate() {
        for unknown in unknowns {
            if let Some(unknown) = position.get(unknown) {
                adjacency[*unknown].push(row);
            }
        }
    }
    let mut matching = RowUnknownMatching {
        row_of_unknown: vec![None; unknown_count],
        unknown_of_row: vec![None; row_count],
    };
    for start in 0..unknown_count {
        if !matching.augment_from(start, &adjacency) && component.unknowns[start].is_parameter() {
            return None;
        }
    }
    Some(
        matching
            .row_of_unknown
            .iter()
            .enumerate()
            .filter_map(|(unknown, row)| {
                Some((component.rows[(*row)?], component.unknowns[unknown]))
            })
            .collect(),
    )
}

/// A partial assignment of component rows to the unknowns they determine.
struct RowUnknownMatching {
    row_of_unknown: Vec<Option<usize>>,
    unknown_of_row: Vec<Option<usize>>,
}

impl RowUnknownMatching {
    /// Grow the matching by one, starting the alternating search at `start`.
    ///
    /// Returns whether an augmenting path was found. `adjacency` lists, per
    /// unknown, the component-local rows that read it.
    fn augment_from(&mut self, start: usize, adjacency: &[Vec<usize>]) -> bool {
        let mut search = AlternatingSearch {
            visited_row: vec![false; self.unknown_of_row.len()],
            visited_unknown: vec![false; self.row_of_unknown.len()],
            reached_from: vec![None; self.unknown_of_row.len()],
            queue: VecDeque::from([start]),
        };
        search.visited_unknown[start] = true;
        while let Some(unknown) = search.queue.pop_front() {
            let Some(free_row) = self.reach_rows_of(unknown, adjacency, &mut search) else {
                continue;
            };
            self.flip_path_to(free_row, &search.reached_from);
            return true;
        }
        false
    }

    /// Visit every not-yet-reached row this unknown occurs in.
    ///
    /// Returns the first row that no unknown holds yet — the end of an
    /// augmenting path. A row that is already matched instead enqueues the
    /// unknown holding it, which is how the search looks for a reroute.
    fn reach_rows_of(
        &self,
        unknown: usize,
        adjacency: &[Vec<usize>],
        search: &mut AlternatingSearch,
    ) -> Option<usize> {
        for row in adjacency[unknown].iter().copied() {
            if search.visited_row[row] {
                continue;
            }
            search.visited_row[row] = true;
            search.reached_from[row] = Some(unknown);
            let Some(matched) = self.unknown_of_row[row] else {
                return Some(row);
            };
            if !search.visited_unknown[matched] {
                search.visited_unknown[matched] = true;
                search.queue.push_back(matched);
            }
        }
        None
    }

    /// Walk the alternating path back to its free unknown, flipping each edge.
    ///
    /// Every row on the path is reassigned to the unknown that reached it, and
    /// the unknown it displaced takes the row *it* had reached — so the path
    /// ends at the search's starting unknown, which had no row, and the matching
    /// grows by exactly one.
    fn flip_path_to(&mut self, free_row: usize, reached_from: &[Option<usize>]) {
        let mut row = free_row;
        while let Some(unknown) = reached_from[row] {
            let displaced = self.row_of_unknown[unknown];
            self.row_of_unknown[unknown] = Some(row);
            self.unknown_of_row[row] = Some(unknown);
            let Some(displaced) = displaced else {
                return;
            };
            row = displaced;
        }
    }
}

/// Breadth-first state of one alternating-path search.
struct AlternatingSearch {
    visited_row: Vec<bool>,
    visited_unknown: Vec<bool>,
    reached_from: Vec<Option<usize>>,
    queue: VecDeque<usize>,
}

struct ProjectionComponent {
    rows: Vec<usize>,
    /// The unknowns each entry of `rows` reads, positionally paired with it.
    row_unknowns: Vec<BTreeSet<InitialUnknown>>,
    unknowns: Vec<InitialUnknown>,
}

/// Group rows that share an initialization unknown into one solvable component.
///
/// These components bound the matching problem. The selected matching later
/// determines which rows are mutually dependent and need a simultaneous solve.
fn connected_components(
    incidence: &[(usize, BTreeSet<InitialUnknown>)],
) -> Vec<ProjectionComponent> {
    let mut sets = DisjointSets::new(incidence.len());
    let mut owner: HashMap<InitialUnknown, usize> = HashMap::new();
    for (position, (_, unknowns)) in incidence.iter().enumerate() {
        for unknown in unknowns {
            let first = *owner.entry(*unknown).or_insert(position);
            sets.union(first, position);
        }
    }
    let mut grouped: Vec<ProjectionComponent> = Vec::new();
    let mut group_of: BTreeMap<usize, usize> = BTreeMap::new();
    for (position, (row, unknowns)) in incidence.iter().enumerate() {
        let root = sets.find(position);
        let group = *group_of.entry(root).or_insert_with(|| {
            grouped.push(ProjectionComponent {
                rows: Vec::new(),
                row_unknowns: Vec::new(),
                unknowns: Vec::new(),
            });
            grouped.len() - 1
        });
        grouped[group].rows.push(*row);
        grouped[group].row_unknowns.push(unknowns.clone());
        grouped[group].unknowns.extend(unknowns.iter().copied());
    }
    for component in &mut grouped {
        component.unknowns.sort_unstable();
        component.unknowns.dedup();
    }
    grouped
}

/// Union-find over the rows of one initialization incidence.
struct DisjointSets {
    parent: Vec<usize>,
}

impl DisjointSets {
    fn new(len: usize) -> Self {
        Self {
            parent: (0..len).collect(),
        }
    }

    fn find(&mut self, mut node: usize) -> usize {
        while self.parent[node] != node {
            self.parent[node] = self.parent[self.parent[node]];
            node = self.parent[node];
        }
        node
    }

    fn union(&mut self, left: usize, right: usize) {
        let (left, right) = (self.find(left), self.find(right));
        if left != right {
            self.parent[right] = left;
        }
    }
}
