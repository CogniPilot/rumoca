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
//! * **A `fixed = true` start is an equation, not an unknown.** MLS 3.6 §8.6:
//!   "For every Real variable `vc` with `fixed = true`, the equation
//!   `vc = startExpression` is added to the initialization equations." The
//!   runtime seeds that coordinate from its own declaration, which *is* that
//!   equation, so the coordinate is determined and must never also be offered
//!   to the projection — one storage slot with two owners is the seed and the
//!   projection fighting over one number.
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
//! What is left is therefore square by the same count MLS states: the unknowns
//! are the coordinates whose value nothing else fixes — unpinned states and
//! unbound `fixed = false` parameters — and the equations are the initialization
//! rows (initial equations, initial-algorithm residuals, and the transferred
//! §8.6 values `initial_pins` could only place beside another stated one).
//! Squareness is enforced in both directions by the matching itself: every
//! unknown must receive one owning row, and every mandatory row (an initial
//! equation or initial-algorithm residual) must receive one unknown to
//! determine, with the stated-value agreement checks a structural
//! equality-class proof minted as the only optional rows. A mandatory row no
//! assignment can cover is a second equation for already-determined values and
//! refuses construction at its own provenance ([`unmatched_row_role`]).
//!
//! ## What is refused rather than planned
//!
//! A row reaching a coordinate outside that space refuses Solve construction
//! at the row's own provenance. No executable product retains an unowned row:
//! the [`solve::InitializationRowRole`] vocabulary is closed over solved rows
//! and stated-value checks, so "retained but unproven" is not a representable
//! state, and the unowned classification survives only inside the construction
//! error ([`ExcludedCoordinate`]). Each refusal names the capability that
//! would admit the shape:
//!
//! **Algebraic and output reads** need the reduced solve to own the coordinate
//! and its total derivative through the simultaneous continuous system. Until
//! that owner exists, every row that reads one is refused, and so is the §8.6
//! equation of a `fixed = true` algebraic/output (`initial_pins`), whose exact
//! transitive incidence is likewise not yet computed: admitting it with an
//! assumed universal incidence let an unrelated fixed-algebraic row match a
//! `fixed = false` parameter and silently retain that parameter's start guess
//! as the answer. The historical silent failure in the row direction:
//! `a = 2*time + 5; der(x) = a - x;` with `initial equation der(x) = 0`
//! simulated `x(0) = 0` where OpenModelica gives `5`. The consistent
//! `initial equation x = 5; x = a;` on the same model is refused too, an
//! over-refusal against OMC accepted until the coupled owner exists.
//!
//! **Discrete reads** are refused as a named over-refusal: the coordinate
//! holds a settled §8.6 value when initialization runs, but the row has no way
//! to solve for the continuous coordinate it also reads. `x + q = 5;
//! x = d + 2;` with `d(start = 0, fixed = true)` is satisfiable at
//! `x = 2, q = 3`, which OpenModelica returns; admitting it needs discretes in
//! the unknown space's *determined* half, task #44's event-machinery
//! territory.
//!
//! **Shapes this walk cannot read per scalar** (an array state, a multi-scalar
//! initialization row, a derivative whose defining row is a structured family
//! point) are refused rather than claiming a coordinate the walk cannot prove
//! the row reads. `Real x[2]; initial equation x[1] = 3; x[2] = 4;` is
//! therefore refused where OpenModelica initializes it.
//!
//! **A `fixed = true` state start that transitively reads a projection-owned
//! parameter** is refused
//! ([`reject_projection_dependent_fixed_starts`]): the seed is evaluated
//! before the projection solves that parameter and nothing re-applies the
//! state's §8.6 equation afterward, so the stated initial value would silently
//! keep the pre-solve guess. A start over literals, constants, and bound
//! parameters that reach no projection unknown stays admitted.
//!
//! ## Numerical root selection after structural ownership
//!
//! §8.6 says the `start` value
//! "is used as a guess value", and Newton from that guess converges to whichever
//! root it is nearest: `q*q = 4` from `start = 3` gives `q = 2`, from `start = -3`
//! it gives `q = -2`. Both satisfy the model, both are legal §8.6 answers, and the
//! declared `start` is the only thing that decides — so a model that cares must
//! say so with its `start`, and a comparison against another tool is a comparison
//! of guesses as much as of equations. That numerical choice is available only
//! after structural matching proves that every initialization unknown has an
//! owning equation. A guess never substitutes for a missing equation.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use rumoca_core::{Fixity, Span};
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;
use rumoca_phase_structural::{InitialValuePin, InitialValueRole};

use super::initial_parameters::InitializationParameterOwnership;
use super::{DerivativeRowIndex, scalar_count, variable_scalar_slot};
use crate::LowerError;
use crate::layout::LoweredLayout;

/// One coordinate the initialization projection may own.
///
/// The ordering makes structural matching deterministic. Both variants require
/// a distinct owning equation; neither a parameter nor a state may retain its
/// numerical start guess as a substitute for one.
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
}

/// What determines one state coordinate at the initialization instant.
#[derive(Clone)]
enum StateInitialOwner {
    /// The projection: the declaration states only a guess, so the
    /// initialization rows are what determine it. Carries its Y-slot index.
    Projection(Vec<usize>),
    /// The declaration itself: MLS §8.6 turns a `fixed = true` start into the
    /// equation `vc = startExpression`, and the structural phase may also have
    /// proved another declaration's stated value *defines* this coordinate
    /// (`InitialValueRole::Definition`), which lowers to an update row. Either
    /// way the coordinate holds a determined value a row may read, and offering
    /// it to the projection as well would give one slot two owners.
    Stated,
}

/// The coordinate space one model's initialization system solves over.
pub(super) struct InitializationUnknownSpace<'a, 'dae> {
    view: dae::DaeView<'dae>,
    ownership: &'a InitializationParameterOwnership<'dae>,
    derivatives: &'a DerivativeRowIndex<'dae>,
    states: HashMap<u32, StateInitialOwner>,
    origins: BTreeMap<InitialUnknown, InitialUnknownOrigin<'dae>>,
}

#[derive(Clone, Copy)]
struct InitialUnknownOrigin<'dae> {
    variable: dae::VariableView<'dae>,
    scalar: usize,
}

/// Everything the initialization unknown space is assembled from.
pub(super) struct InitializationUnknownInputs<'a, 'dae> {
    pub(super) view: dae::DaeView<'dae>,
    pub(super) layout: &'a LoweredLayout<'dae>,
    pub(super) ownership: &'a InitializationParameterOwnership<'dae>,
    pub(super) derivatives: &'a DerivativeRowIndex<'dae>,
    pub(super) pins: &'a [InitialValuePin],
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
        pins,
    } = inputs;
    let states = state_initial_owners(view, layout, pins)?;
    let origins = initial_unknown_origins(view, ownership, &states)?;
    reject_projection_dependent_fixed_starts(view, ownership)?;
    Ok(InitializationUnknownSpace {
        view,
        ownership,
        derivatives,
        states,
        origins,
    })
}

/// Refuse a `fixed = true` state whose `start` transitively reads a
/// projection-owned parameter.
///
/// MLS 3.6 §8.6 turns such a start into the equation `x = startExpression`,
/// and the runtime answers it by evaluating the declaration seed once, before
/// the projection runs. A start that reads a `fixed = false` parameter, either
/// directly or through any chain of parameter bindings, is therefore evaluated
/// at that parameter's *guess*: the projection later solves the parameter and
/// nothing re-applies the state's equation, so the stated initial value would
/// silently keep the pre-solve number. Until seed evaluation joins the
/// initialization solve with a proved ordering, the shape is refused at the
/// declaration. A start over literals, constants, and bound parameters whose
/// closure reaches no projection unknown is a settled value and stays
/// admitted.
fn reject_projection_dependent_fixed_starts<'dae>(
    view: dae::DaeView<'dae>,
    ownership: &InitializationParameterOwnership<'dae>,
) -> Result<(), LowerError> {
    for (_, variable) in view.variables() {
        if variable.role() != dae::VariableRole::State || variable.fixed() != Fixity::Fixed {
            continue;
        }
        let Some(start) = variable.start() else {
            continue;
        };
        let Some(dependency) = projection_dependent_parameter(view, ownership, start) else {
            continue;
        };
        let parameter = view
            .variable_id(dependency as usize)
            .and_then(|id| view.variable(id))
            .map(|parameter| parameter.name().to_string());
        return Err(LowerError::non_computable(
            format!(
                "the `fixed = true` start of state `{}` transitively reads {}, whose value the \
                 initialization system itself solves (a `fixed = false` parameter, or a \
                 parameter bound to one; MLS 3.6 §8.6); the seed would be evaluated at the \
                 pre-solve guess and never re-applied after the solve, so the stated initial \
                 value cannot be honored and the model is refused",
                variable.name(),
                match parameter {
                    Some(name) => format!("`{name}`"),
                    None => format!("parameter {dependency}"),
                },
            ),
            variable.declaration().span(),
        ));
    }
    Ok(())
}

/// The first parameter in one expression whose value the projection determines.
///
/// [`InitializationParameterOwnership`] has already computed the transitive
/// closure this test needs: a parameter is projection-dependent exactly when
/// the projection owns its slots directly, or when it is a bound dependent,
/// which is precisely the parameters `substitution` carries a binding for.
fn projection_dependent_parameter<'dae>(
    view: dae::DaeView<'dae>,
    ownership: &InitializationParameterOwnership<'dae>,
    root: dae::ExprId<'dae>,
) -> Option<u32> {
    let mut found: Option<u32> = None;
    dae::for_each_expression(view, root, |_, node| {
        if found.is_some() {
            return;
        }
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(parameter)) =
            node.operation()
        else {
            return;
        };
        if ownership
            .projection_unknown_slots(parameter.index())
            .is_some()
            || ownership.substitution(parameter.index()).is_some()
        {
            found = Some(parameter.index());
        }
    });
    found
}

impl<'dae> InitializationUnknownSpace<'_, 'dae> {
    /// The declaration one coordinate identity names.
    ///
    /// Absent when the identity has no declaration, which the walk treats as a
    /// coordinate it cannot own rather than assuming a shape for it.
    fn variable(&self, variable: dae::VariableId<'dae>) -> Option<dae::VariableView<'dae>> {
        self.view.variable(variable)
    }

    fn all_projection_unknowns(&self) -> BTreeSet<InitialUnknown> {
        self.origins.keys().copied().collect()
    }

    fn unmatched_error(
        &self,
        unknown: InitialUnknown,
        source_rows: usize,
        usable_rows: usize,
        component_rows: usize,
        component_unknowns: usize,
    ) -> LowerError {
        let Some(origin) = self.origins.get(&unknown).copied() else {
            return LowerError::unspanned_non_computable(format!(
                "initialization matching produced an unknown without construction provenance: \
                 {unknown:?}"
            ));
        };
        let name = match origin.variable.scalar_name(origin.scalar) {
            Some(name) => name,
            None => origin.variable.name().to_string(),
        };
        LowerError::non_computable(
            format!(
                "initialization cannot issue executable Solve IR: unknown `{name}` has no \
                 distinct owning equation; structural matching found {component_rows} usable \
                 row(s) for {component_unknowns} unknown coordinate(s) in its component \
                 ({usable_rows} usable row(s) among {source_rows} initialization row(s) for {} \
                 unknown coordinate(s) model-wide); a `start` value with `fixed = false` is a \
                 numerical guess, not an equation",
                self.origins.len(),
            ),
            origin.variable.declaration().span(),
        )
    }
}

fn initial_unknown_origins<'dae>(
    view: dae::DaeView<'dae>,
    ownership: &InitializationParameterOwnership<'dae>,
    states: &HashMap<u32, StateInitialOwner>,
) -> Result<BTreeMap<InitialUnknown, InitialUnknownOrigin<'dae>>, LowerError> {
    let mut origins = BTreeMap::new();
    for (id, variable) in view.variables() {
        let unknowns: Vec<InitialUnknown> = match variable.role() {
            dae::VariableRole::Parameter => ownership
                .projection_unknown_slots(id.index())
                .into_iter()
                .flatten()
                .copied()
                .map(InitialUnknown::Parameter)
                .collect(),
            dae::VariableRole::State => match states.get(&id.index()) {
                Some(StateInitialOwner::Projection(indices)) => {
                    indices.iter().copied().map(InitialUnknown::State).collect()
                }
                Some(StateInitialOwner::Stated) | None => Vec::new(),
            },
            _ => Vec::new(),
        };
        for (scalar, unknown) in unknowns.into_iter().enumerate() {
            let previous = origins.insert(unknown, InitialUnknownOrigin { variable, scalar });
            if previous.is_some() {
                return Err(LowerError::contract(
                    format!("initialization unknown slot {unknown:?} has multiple declarations"),
                    variable.declaration().span(),
                ));
            }
        }
    }
    Ok(origins)
}

/// Who determines each state coordinate, keyed by variable index.
///
/// A state absent from the map is one this phase can neither own nor read: an
/// array whose scalars a whole-expression walk cannot separate, with nothing
/// stated about it. A row that reaches such a coordinate is left unplanned.
fn state_initial_owners(
    view: dae::DaeView<'_>,
    layout: &LoweredLayout<'_>,
    pins: &[InitialValuePin],
) -> Result<HashMap<u32, StateInitialOwner>, LowerError> {
    let defined: BTreeSet<u32> = pins
        .iter()
        .filter(|pin| matches!(pin.role, InitialValueRole::Definition))
        .map(|pin| pin.coordinate)
        .collect();
    let mut owners = HashMap::new();
    for (id, variable) in view.variables() {
        if variable.role() != dae::VariableRole::State {
            continue;
        }
        let span = variable.declaration().span();
        if variable.fixed() == Fixity::Fixed || defined.contains(&id.index()) {
            owners.insert(id.index(), StateInitialOwner::Stated);
            continue;
        }
        let mut indices = Vec::with_capacity(variable.scalar_count());
        for scalar in 0..variable.scalar_count() {
            let solve::ScalarSlot::Y { index, .. } =
                variable_scalar_slot(layout, id.index(), scalar, span)?
            else {
                return Err(LowerError::contract(
                    format!("state `{}` does not occupy solver storage", variable.name()),
                    span,
                ));
            };
            indices.push(index);
        }
        if !indices.is_empty() {
            owners.insert(id.index(), StateInitialOwner::Projection(indices));
        }
    }
    Ok(owners)
}

/// One initialization row as the planner sees it: what it reads, and where in
/// the source it came from.
///
/// The span is the row's own provenance, so a refusal names the equation or
/// declaration that overdetermines the system rather than the whole model.
pub(super) struct InitialRow<'dae> {
    pub(super) incidence: InitialRowIncidence<'dae>,
    pub(super) span: Span,
}

/// Where one initialization row's coordinate incidence is read from.
///
/// The three forms differ in what the walk may start from, not in what it proves.
pub(super) enum InitialRowIncidence<'dae> {
    /// Nothing this planner can read: a multi-scalar equation, whose per-scalar
    /// unknowns a whole-expression walk cannot separate.
    Opaque,
    /// One whole-model residual expression. Every coordinate it reaches is an
    /// unknown of the row, so a coordinate the projection cannot own disqualifies
    /// the row.
    Residual(dae::ExprId<'dae>),
    /// A stated initial value carried onto a coordinate that already holds one
    /// (`initial_pins`). The row is `coordinate - Σ terms`, and the coordinate is
    /// a state the initialization instant has already seeded, so only the terms
    /// carry unknowns — which is exactly what lets such a row determine a
    /// `fixed = false` parameter its displacement reads.
    ///
    /// The coordinate itself is never one of those unknowns, and that is a fact
    /// rather than a restriction: `initial_pins::class_pins` emits a `Check` only
    /// for a *second* stated value about a class whose state already carries the
    /// first — either as its own `fixed = true` start or as a `Definition` pin —
    /// so the coordinate is always one [`StateInitialOwner::Stated`] covers. A
    /// `Check` that failed to converge is therefore two declarations contradicting
    /// each other, not a coordinate nothing solved.
    CarriedValue(Vec<dae::ExprId<'dae>>),
}

pub(super) struct InitialProjection {
    pub(super) unknowns: Vec<solve::ScalarSlot>,
    pub(super) plan: solve::InitializationProjectionPlan,
    /// What the projection does with each row, positionally by equation index.
    pub(super) row_roles: Vec<solve::InitializationRowRole>,
}

/// Plan the initialization unknowns the initialization system itself determines.
///
/// A block the runtime can solve has one equation per unknown, and the matching
/// is what names those equations. The pairing is a constrained bijection: every
/// projection unknown gets exactly one owning row, and every mandatory row (an
/// initial equation or initial-algorithm residual) gets exactly one unknown to
/// determine, with the stated-value checks a structural proof minted as the
/// only rows the matching may leave out ([`match_component`]). A mandatory row
/// no assignment can cover is refused as overdetermination at its own
/// provenance ([`overdetermined_row`]), a row that reads a coordinate the
/// projection cannot own is refused with the capability that would admit it
/// ([`unowned_row_error`]), and an unknown no row can determine is refused at
/// its declaration.
///
/// A component whose rows cannot cover every unknown is not executable. Solve
/// construction rejects it here, before any start guess can become a selected
/// answer and before runtime receives an incomplete projection plan.
///
/// The matching is structural, so it is rank-blind: it takes the first augmenting
/// assignment, which can pick a block whose Jacobian is numerically singular while
/// a different assignment of the same rows would not be. The runtime reports that
/// as a failed solve rather than a wrong answer, and a minimum-degree ordering
/// would choose better; until then the risk is accepted and named here rather than
/// left implied.
pub(super) fn plan_initialization_projection<'dae>(
    space: &InitializationUnknownSpace<'_, 'dae>,
    rows: &[InitialRow<'dae>],
) -> Result<InitialProjection, LowerError> {
    // No row holds a role until this planner proves one: a role is the outcome
    // of the matching or the stated-value check a structural proof minted.
    // There is no default role, and a row the walk cannot own is refused right
    // here rather than typed.
    let mut row_roles: Vec<Option<solve::InitializationRowRole>> = vec![None; rows.len()];
    let mut incidence: Vec<IncidentRow> = Vec::new();
    for (row, source) in rows.iter().enumerate() {
        match row_unknowns(space, &source.incidence) {
            RowIncidence::Owned(unknowns) if !unknowns.is_empty() => {
                incidence.push(IncidentRow {
                    row,
                    mandatory: !matches!(source.incidence, InitialRowIncidence::CarriedValue(_)),
                    unknowns,
                });
            }
            // Nothing this row reads is an unknown, so no matching can ever
            // pair it: it stands or is refused as an unmatched row right here.
            RowIncidence::Owned(_) => {
                row_roles[row] = Some(unmatched_row_role(source)?);
            }
            RowIncidence::Unowned(kind) => {
                return Err(unowned_row_error(kind, source.span));
            }
        }
    }
    let all_unknowns = space.all_projection_unknowns();
    let incident_unknowns: BTreeSet<InitialUnknown> = incidence
        .iter()
        .flat_map(|entry| entry.unknowns.iter().copied())
        .collect();
    if let Some(unmatched) = all_unknowns.difference(&incident_unknowns).next().copied() {
        return Err(space.unmatched_error(unmatched, rows.len(), incidence.len(), 0, 1));
    }
    let mut blocks = Vec::new();
    let mut unknowns = Vec::new();
    for component in connected_components(&incidence) {
        let matched = match match_component(&component) {
            Ok(matched) => matched,
            Err(UnmatchedEntity::Row(row)) => {
                return Err(overdetermined_row(&rows[row]));
            }
            Err(UnmatchedEntity::Unknown(unmatched)) => {
                return Err(space.unmatched_error(
                    unmatched,
                    rows.len(),
                    incidence.len(),
                    component.rows.len(),
                    component.unknowns.len(),
                ));
            }
        };
        record_component_roles(&component, &matched, rows, &mut row_roles)?;
        let mut block_rows = Vec::with_capacity(matched.len());
        let mut block_unknowns = Vec::with_capacity(matched.len());
        for (row, unknown) in matched {
            block_rows.push(row);
            block_unknowns.push(unknown.slot());
        }
        unknowns.extend(block_unknowns.iter().copied());
        blocks.push(solve::InitializationProjectionBlock {
            rows: block_rows,
            unknowns: block_unknowns,
        });
    }
    let row_roles = complete_row_roles(row_roles, rows)?;
    Ok(InitialProjection {
        unknowns,
        plan: solve::InitializationProjectionPlan { blocks },
        row_roles,
    })
}

/// Require the planner to have decided every row, so no role arrives by
/// omission.
///
/// Every surviving row takes its role in exactly one place: the incidence loop
/// decides rows with nothing left to determine, and [`record_component_roles`]
/// decides every row of every component; rows the planner cannot own were
/// already refused. A `None` left here is a planner defect, and it is refused
/// rather than papered over with a role the planner never proved.
fn complete_row_roles(
    roles: Vec<Option<solve::InitializationRowRole>>,
    rows: &[InitialRow<'_>],
) -> Result<Vec<solve::InitializationRowRole>, LowerError> {
    roles
        .into_iter()
        .enumerate()
        .map(|(row, role)| match role {
            Some(role) => Ok(role),
            None => Err(LowerError::contract(
                format!("initialization planning decided no role for residual row {row}"),
                rows[row].span,
            )),
        })
        .collect()
}

/// The only role an unmatched row may hold, or the refusal that names it.
///
/// A carried stated value is the one legitimate unmatched row: the structural
/// phase minted it as a numeric agreement test between two declarations that
/// state one coordinate's initial value (`initial_pins::class_pins` emits a
/// `Check` only when `stated_agreement` is `Undecided`), so it restates a §8.6
/// equation another declaration already contributes rather than adding one. The
/// two stated values may still read parameters, so whether they coincide is a
/// question the initialization instant answers with numbers.
///
/// Every other row is a mandatory initialization equation, and one the
/// matching pairs with no unknown makes the §8.6 system overdetermined
/// ([`overdetermined_row`]).
fn unmatched_row_role(row: &InitialRow<'_>) -> Result<solve::InitializationRowRole, LowerError> {
    match row.incidence {
        InitialRowIncidence::CarriedValue(_) => Ok(solve::InitializationRowRole::StatedValueCheck),
        InitialRowIncidence::Residual(_) | InitialRowIncidence::Opaque => {
            Err(overdetermined_row(row))
        }
    }
}

/// Refuse one mandatory initialization row left without an unknown to
/// determine.
///
/// The row is a second equation for values other owners already fix: every
/// coordinate it reads is determined by a `fixed = true` start, a binding, or
/// another initialization row, or is claimed by the other rows of its
/// component under every possible assignment. That makes the §8.6 system
/// overdetermined, and it is refused here, at the row's own provenance, before
/// it can reach the runtime as a residual no block owns.
fn overdetermined_row(row: &InitialRow<'_>) -> LowerError {
    LowerError::non_computable(
        "initialization is overdetermined (MLS 3.6 §8.6): this initialization row has no \
         unknown coordinate left to determine; every coordinate it reads is either determined \
         by a `fixed = true` start, a binding, or another initialization row, or is claimed by \
         the other initialization rows under every assignment, so the row is a second equation \
         for already-determined values and cannot issue executable Solve IR",
        row.span,
    )
}

/// Refuse one initialization row that reads a coordinate the reduced
/// projection cannot own.
///
/// The unowned classification survives only inside this construction error:
/// the executable role vocabulary has no variant for a retained-but-unproven
/// row, so the refusal happens at the row's own provenance and names the
/// capability that would admit the shape (module header).
fn unowned_row_error(kind: ExcludedCoordinate, span: Span) -> LowerError {
    LowerError::non_computable(
        format!(
            "initialization cannot issue executable Solve IR: this initialization row reads {}, \
             so the reduced initialization projection cannot own the row, and no executable \
             product retains a row nothing proves",
            kind.description(),
        ),
        span,
    )
}

/// Say, per row of one component, what the projection ended up doing with it.
///
/// A matched row is solved. Reaching this function at all means
/// [`match_component`] covered every unknown and every mandatory row of the
/// component, so an unmatched row is always an optional carried stated value,
/// and [`unmatched_row_role`] types it as the stated-value check; its `Err`
/// arm survives only as defense against a matcher that broke that contract.
fn record_component_roles(
    component: &ProjectionComponent,
    matched: &[(usize, InitialUnknown)],
    rows: &[InitialRow<'_>],
    row_roles: &mut [Option<solve::InitializationRowRole>],
) -> Result<(), LowerError> {
    let solved_rows: BTreeSet<usize> = matched.iter().map(|(row, _)| *row).collect();
    for row in component.rows.iter().copied() {
        row_roles[row] = Some(if solved_rows.contains(&row) {
            solve::InitializationRowRole::Solved
        } else {
            unmatched_row_role(&rows[row])?
        });
    }
    Ok(())
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
) -> RowIncidence {
    let pending = match row {
        InitialRowIncidence::Opaque => {
            return RowIncidence::Unowned(ExcludedCoordinate::Unreadable);
        }
        InitialRowIncidence::Residual(residual) => vec![*residual],
        InitialRowIncidence::CarriedValue(terms) => terms.clone(),
    };
    let mut incidence = InitialIncidence {
        unknowns: BTreeSet::new(),
        excluded: None,
        substituted: BTreeSet::new(),
        expanded: BTreeSet::new(),
        pending,
    };
    // One coordinate the projection cannot own already refuses the row, but
    // *which* one decides what the refusal names, and the algebraic reading is
    // the one worth reporting (see the module header). So the walk keeps going
    // until it has found an algebraic or run out of expressions to expand.
    while !matches!(incidence.excluded, Some(ExcludedCoordinate::Algebraic))
        && let Some(expression) = incidence.pending.pop()
    {
        dae::for_each_expression(space.view, expression, |_, node| {
            let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
                return;
            };
            incidence.visit(space, coordinate);
        });
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
    /// The row reads a coordinate outside the planned unknown space, of this
    /// kind, so the row refuses Solve construction.
    Unowned(ExcludedCoordinate),
}

/// Why one initialization row cannot join the planned unknown space.
///
/// This classification survives only inside the construction error the planner
/// refuses the row with; executable Solve IR has no representation for an
/// unowned row. The rank orders how loudly a kind deserves to be reported: an
/// algebraic read outranks the rest because it names the missing reduced-solve
/// capability, and ranking keeps the reported kind deterministic when a row
/// reaches several, since the expression walk order is.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum ExcludedCoordinate {
    Algebraic,
    Discrete,
    Unreadable,
    Other,
}

impl ExcludedCoordinate {
    const fn rank(self) -> u8 {
        match self {
            Self::Algebraic => 3,
            Self::Discrete => 2,
            Self::Unreadable => 1,
            Self::Other => 0,
        }
    }

    const fn description(self) -> &'static str {
        match self {
            Self::Algebraic => {
                "a continuous algebraic/output coordinate, whose simultaneous dependency and \
                 total derivative through the continuous system the reduced initialization \
                 projection does not yet own"
            }
            Self::Discrete => {
                "a discrete-time coordinate or its `pre` value, which the initialization \
                 projection cannot solve a continuous coordinate through"
            }
            Self::Unreadable => {
                "a coordinate this lowering cannot read per scalar (an array state, an array \
                 `fixed = false` parameter awaiting subscript-aware incidence, a multi-scalar \
                 row, or a structured family point)"
            }
            Self::Other => {
                "a coordinate outside the planned initialization unknown space (an input, a \
                 delay, a `previous`, a relation memory, or a terminal)"
            }
        }
    }
}

/// The projection unknowns one initialization residual program reaches.
struct InitialIncidence<'dae> {
    unknowns: BTreeSet<InitialUnknown>,
    excluded: Option<ExcludedCoordinate>,
    /// Parameter bindings already followed, so a diamond is walked once.
    substituted: BTreeSet<u32>,
    /// State derivatives already followed, so a derivative that reads itself
    /// through its own defining row terminates.
    expanded: BTreeSet<u32>,
    pending: Vec<dae::ExprId<'dae>>,
}

impl<'dae> InitialIncidence<'dae> {
    fn visit(
        &mut self,
        space: &InitializationUnknownSpace<'_, 'dae>,
        coordinate: dae::CoordinateView<'dae>,
    ) {
        match coordinate {
            dae::CoordinateView::Parameter(parameter) => self.visit_parameter(space, parameter),
            dae::CoordinateView::State(state) => self.visit_state(space, state),
            dae::CoordinateView::Derivative(state) => self.visit_derivative(space, state),
            // The runtime reconstructs an algebraic before it certifies the
            // complete residual (module header), but this planner cannot yet
            // differentiate through or solve the simultaneous continuous
            // system, so the row remains outside the admitted reduced solve.
            dae::CoordinateView::Algebraic(_) => {
                self.exclude(ExcludedCoordinate::Algebraic);
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
            // kind, which the exclusion rank orders Discrete above Other.
            | dae::CoordinateView::PreState(_)
            | dae::CoordinateView::PreAlgebraic(_) => {
                self.exclude(ExcludedCoordinate::Discrete);
            }
            // A domain binder and a clock interval are compile-time constants, and
            // `time` is the known initialization instant.
            dae::CoordinateView::Time
            | dae::CoordinateView::ClockInterval(_)
            | dae::CoordinateView::Binder(_) => {}
            _ => self.exclude(ExcludedCoordinate::Other),
        }
    }

    /// Record why the row cannot be planned, keeping the loudest reason found.
    fn exclude(&mut self, kind: ExcludedCoordinate) {
        if self.excluded.is_none_or(|held| kind.rank() > held.rank()) {
            self.excluded = Some(kind);
        }
    }

    fn visit_parameter(
        &mut self,
        space: &InitializationUnknownSpace<'_, 'dae>,
        parameter: dae::ParameterId<'dae>,
    ) {
        if let Some(indices) = space.ownership.projection_unknown_slots(parameter.index()) {
            // A whole-expression walk cannot tell which scalar of an array
            // parameter one occurrence reads. Claiming every scalar for every
            // occurrence let two rows that both read `p[1]` appear to cover
            // `p[1]` and `p[2]` between them, so the matching believed an
            // unknown was determined when nothing read it. Until
            // subscript-aware scalar incidence exists, only a one-scalar
            // projection-owned parameter is readable; an array occurrence
            // refuses the row instead (the same fail-closed shape as the
            // multi-scalar state arm below).
            match indices {
                [index] => {
                    self.unknowns.insert(InitialUnknown::Parameter(*index));
                }
                _ => self.exclude(ExcludedCoordinate::Unreadable),
            }
            return;
        }
        if let Some(binding) = space.ownership.substitution(parameter.index())
            && self.substituted.insert(parameter.index())
        {
            self.pending.push(binding);
        }
    }

    fn visit_state(
        &mut self,
        space: &InitializationUnknownSpace<'_, 'dae>,
        state: dae::StateId<'dae>,
    ) {
        match space.states.get(&state.index()) {
            Some(StateInitialOwner::Projection(indices)) if indices.len() == 1 => {
                self.unknowns.insert(InitialUnknown::State(indices[0]));
            }
            // A stated value is a number the row may read, not an unknown.
            Some(StateInitialOwner::Projection(_)) => {
                self.exclude(ExcludedCoordinate::Unreadable);
            }
            Some(StateInitialOwner::Stated) => {}
            None => self.exclude(ExcludedCoordinate::Unreadable),
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
    ) {
        let scalar = space
            .variable(state.into())
            .map(dae::VariableView::scalar_count);
        if scalar != Some(1) {
            self.exclude(ExcludedCoordinate::Unreadable);
            return;
        }
        let Some(definition) = space.derivatives.definition(state, 0) else {
            self.exclude(ExcludedCoordinate::Other);
            return;
        };
        // A family point carries binder values this walk does not substitute, and
        // a multi-scalar residual mixes the coordinates of every scalar into one
        // expression, so neither can be read per scalar.
        if definition.domain_point.is_some() || scalar_count(space.view, definition.expression) != 1
        {
            self.exclude(ExcludedCoordinate::Unreadable);
            return;
        }
        if self.expanded.insert(state.index()) {
            self.pending.push(definition.expression);
        }
    }
}

/// Assign rows to unknowns so that every unknown and every mandatory row is
/// covered.
///
/// Two greedy phases of Kuhn's augmenting-path search over the row/unknown
/// bipartite graph. Phase one augments from each mandatory row in row order:
/// the simultaneously matchable row sets form a transversal matroid, so a
/// mandatory row with no augmenting path proves no assignment covers every
/// mandatory row, and the first such row is the refusal. Phase two augments
/// from each still-free unknown over every row, the optional stated-value
/// checks included. Augmenting reroutes but never unmatches a matched vertex
/// on either side, so the phase-one cover survives phase two, and an optional
/// row joins the plan only when some unknown has no other owner.
fn match_component(
    component: &ProjectionComponent,
) -> Result<Vec<(usize, InitialUnknown)>, UnmatchedEntity> {
    let row_count = component.rows.len();
    let unknown_count = component.unknowns.len();
    let position: HashMap<InitialUnknown, usize> = component
        .unknowns
        .iter()
        .enumerate()
        .map(|(position, unknown)| (*unknown, position))
        .collect();
    let mut rows_of_unknown: Vec<Vec<usize>> = vec![Vec::new(); unknown_count];
    let mut unknowns_of_row: Vec<Vec<usize>> = vec![Vec::new(); row_count];
    for (row, unknowns) in component.row_unknowns.iter().enumerate() {
        for unknown in unknowns {
            if let Some(unknown) = position.get(unknown) {
                rows_of_unknown[*unknown].push(row);
                unknowns_of_row[row].push(*unknown);
            }
        }
    }
    let mut matching = RowUnknownMatching {
        row_of_unknown: vec![None; unknown_count],
        unknown_of_row: vec![None; row_count],
    };
    for row in 0..row_count {
        if component.mandatory[row]
            && !matching.augment_row(row, &unknowns_of_row, &mut vec![false; unknown_count])
        {
            return Err(UnmatchedEntity::Row(component.rows[row]));
        }
    }
    for unknown in 0..unknown_count {
        if matching.row_of_unknown[unknown].is_none()
            && !matching.augment_unknown(unknown, &rows_of_unknown, &mut vec![false; row_count])
        {
            return Err(UnmatchedEntity::Unknown(component.unknowns[unknown]));
        }
    }
    Ok(matching
        .row_of_unknown
        .iter()
        .enumerate()
        .filter_map(|(unknown, row)| Some((component.rows[(*row)?], component.unknowns[unknown])))
        .collect())
}

/// Which side of the row/unknown matching could not be covered.
enum UnmatchedEntity {
    /// A mandatory row, by model-level equation index, that no assignment can
    /// give an unknown to determine.
    Row(usize),
    /// An unknown no row can determine.
    Unknown(InitialUnknown),
}

/// A partial assignment of component rows to the unknowns they determine.
struct RowUnknownMatching {
    row_of_unknown: Vec<Option<usize>>,
    unknown_of_row: Vec<Option<usize>>,
}

impl RowUnknownMatching {
    /// Give `row` an unknown, rerouting matched rows onto alternatives.
    ///
    /// One depth-first alternating search of Kuhn's algorithm; the visit set
    /// guards a single search. Returns whether `row` ends the search matched.
    fn augment_row(
        &mut self,
        row: usize,
        unknowns_of_row: &[Vec<usize>],
        visited_unknown: &mut [bool],
    ) -> bool {
        for unknown in unknowns_of_row[row].iter().copied() {
            if visited_unknown[unknown] {
                continue;
            }
            visited_unknown[unknown] = true;
            let claimable = match self.row_of_unknown[unknown] {
                None => true,
                Some(holder) => self.augment_row(holder, unknowns_of_row, visited_unknown),
            };
            if claimable {
                self.row_of_unknown[unknown] = Some(row);
                self.unknown_of_row[row] = Some(unknown);
                return true;
            }
        }
        false
    }

    /// Give `unknown` a row, rerouting matched unknowns onto alternatives.
    ///
    /// The mirror image of [`Self::augment_row`], searched over every row so
    /// an optional row can be drawn in when it is the only owner left.
    fn augment_unknown(
        &mut self,
        unknown: usize,
        rows_of_unknown: &[Vec<usize>],
        visited_row: &mut [bool],
    ) -> bool {
        for row in rows_of_unknown[unknown].iter().copied() {
            if visited_row[row] {
                continue;
            }
            visited_row[row] = true;
            let claimable = match self.unknown_of_row[row] {
                None => true,
                Some(holder) => self.augment_unknown(holder, rows_of_unknown, visited_row),
            };
            if claimable {
                self.unknown_of_row[row] = Some(unknown);
                self.row_of_unknown[unknown] = Some(row);
                return true;
            }
        }
        false
    }
}

/// One initialization row offered to the matching.
struct IncidentRow {
    /// Model-level equation index.
    row: usize,
    /// Whether the matching must give this row an unknown: true for an initial
    /// equation or initial-algorithm residual, false for an optional carried
    /// stated-value check.
    mandatory: bool,
    unknowns: BTreeSet<InitialUnknown>,
}

struct ProjectionComponent {
    rows: Vec<usize>,
    /// Whether each entry of `rows` must be matched, positionally paired.
    mandatory: Vec<bool>,
    /// The unknowns each entry of `rows` reads, positionally paired with it.
    row_unknowns: Vec<BTreeSet<InitialUnknown>>,
    unknowns: Vec<InitialUnknown>,
}

/// Group rows that share an initialization unknown into one solvable component.
///
/// Two rows that read the same unknown must be solved together, so the components
/// of the row/unknown bipartite graph are the coarsest blocks that stay
/// independent.
fn connected_components(incidence: &[IncidentRow]) -> Vec<ProjectionComponent> {
    let mut sets = DisjointSets::new(incidence.len());
    let mut owner: HashMap<InitialUnknown, usize> = HashMap::new();
    for (position, entry) in incidence.iter().enumerate() {
        for unknown in &entry.unknowns {
            let first = *owner.entry(*unknown).or_insert(position);
            sets.union(first, position);
        }
    }
    let mut grouped: Vec<ProjectionComponent> = Vec::new();
    let mut group_of: BTreeMap<usize, usize> = BTreeMap::new();
    for (position, entry) in incidence.iter().enumerate() {
        let root = sets.find(position);
        let group = *group_of.entry(root).or_insert_with(|| {
            grouped.push(ProjectionComponent {
                rows: Vec::new(),
                mandatory: Vec::new(),
                row_unknowns: Vec::new(),
                unknowns: Vec::new(),
            });
            grouped.len() - 1
        });
        grouped[group].rows.push(entry.row);
        grouped[group].mandatory.push(entry.mandatory);
        grouped[group].row_unknowns.push(entry.unknowns.clone());
        grouped[group]
            .unknowns
            .extend(entry.unknowns.iter().copied());
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
