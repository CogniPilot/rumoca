//! Recognize the singular-system constraints this phase is allowed to reduce.
//!
//! Nothing here rewrites a DAE. Each function reports what the source system
//! already proves: which states a residual defines directly, which residuals
//! are holonomic, and whether an expression survives the exact symbolic
//! differentiation the reconstruction will later perform. A candidate that
//! fails a preflight here is never handed to reconstruction, so every
//! `unreachable!` in the differentiation code stands on a check made here.
//!
//! Two facts let detection see past a single residual. [`SystemEqualities`]
//! closes the connector alias chains an MSL model writes as bare `a - b`
//! potential equalities, so a state that is provably the same quantity as
//! another state or a parameter is recognized as redundant. Explicit derivative
//! definitions are read in either orientation, so the `w - der(phi)` form MSL
//! components use supplies `d/dt phi` exactly as `der(phi) - w` would.

use rumoca_core::{Fixity, Span, StateSelect};
use rumoca_ir_dae as dae;

use crate::CausalDefinitions;

use super::equalities::{
    EqualityAnchor, EqualitySign, FunctionCallContext, SingletonRealProjection, SystemEqualities,
    forwarded_call_argument, is_time_invariant, singleton_real_projection,
};
use super::initial_pins::represented_initial_values;
use super::{DirectStateConstraint, HolonomicConstraint, HolonomicDifferentiationProof};
use crate::StructuralError;

/// The exact indirections reconstruction is allowed to follow while
/// differentiating, gathered once per source system.
pub(super) struct DifferentiationFacts {
    pub(super) equalities: SystemEqualities,
    pub(super) derivative_definitions: Vec<Option<u32>>,
    pub(super) algebraic_definitions: Vec<Option<u32>>,
}

impl DifferentiationFacts {
    pub(super) fn collect(view: dae::DaeView<'_>) -> Self {
        let causal = CausalDefinitions::derive(view);
        let algebraic_definitions = view
            .variables()
            .map(|(id, variable)| {
                (variable.role() == dae::VariableRole::Algebraic)
                    .then(|| causal.definition_for_variable(id))
                    .flatten()
                    .map(dae::ExprId::index)
            })
            .collect();
        Self {
            equalities: SystemEqualities::collect(view),
            derivative_definitions: explicit_derivative_definitions(view),
            algebraic_definitions,
        }
    }

    pub(super) fn algebraic_definition<'dae>(
        &self,
        view: dae::DaeView<'dae>,
        algebraic: dae::AlgebraicId<'dae>,
    ) -> Option<dae::ExprId<'dae>> {
        self.algebraic_definitions[algebraic.index() as usize]
            .and_then(|definition| view.expression_id(definition as usize))
    }

    /// Whether the finalized source proves this instantiated expression is the
    /// additive zero of its Real payload.
    pub(super) fn expression_is_zero<'dae>(
        &self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> bool {
        let scoped_context = context.scoped_to_expression(view, expression);
        let context = &scoped_context;
        if let Some((result, nested)) = context.call_result(view, expression) {
            return self.expression_is_zero(view, result, &nested);
        }
        let Some(node) = view.expression(expression) else {
            return false;
        };
        match node.operation() {
            dae::ExpressionOperation::Literal(
                dae::DaeLiteral::Real(0.0) | dae::DaeLiteral::Integer(0),
            )
            | dae::ExpressionOperation::Builtin {
                builtin: dae::PureBuiltin::Zeros,
                ..
            } => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => context
                .parameter_argument(parameter)
                .is_some_and(|argument| self.expression_is_zero(view, argument, context)),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => self
                .equalities
                .value_anchor_of(algebraic.index())
                .and_then(|(anchor, _)| self.equalities.anchor_expression(anchor))
                .and_then(|anchor| view.expression_id(anchor as usize))
                .or_else(|| self.algebraic_definition(view, algebraic))
                .is_some_and(|definition| self.expression_is_zero(view, definition, context)),
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => self.expression_is_zero(view, operand, context),
            dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Multiply,
                lhs,
                rhs,
            } => {
                self.expression_is_zero(view, lhs, context)
                    || self.expression_is_zero(view, rhs, context)
            }
            dae::ExpressionOperation::Field { base, field } => context
                .projected_field(view, base, field)
                .is_some_and(|(projected, projected_context)| {
                    self.expression_is_zero(view, projected, &projected_context)
                }),
            _ => false,
        }
    }
}

/// Where one expression stands in a differentiability walk.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Visit {
    Pending,
    InProgress,
    Differentiable,
}

/// Reusable visitation state for all holonomic roots in one finalized DAE.
///
/// Each expression packs the six `(order, context)` states into the low twelve
/// bits of one word and the root epoch into the rest. Advancing the epoch makes
/// every slot logically pending in O(1), while keeping storage to one word per
/// expression. The backing table is cleared only after the practically
/// unreachable epoch wraparound.
struct HolonomicProofScratch {
    epoch: u64,
    visited: Vec<u64>,
}

impl HolonomicProofScratch {
    const STATE_BITS: u32 = 2;
    const STATE_SLOT_COUNT: u32 = 6;
    const MAX_EPOCH: u64 = u64::MAX >> (Self::STATE_BITS * Self::STATE_SLOT_COUNT);

    fn new(expression_count: usize) -> Self {
        Self {
            epoch: 0,
            visited: vec![0; expression_count],
        }
    }

    fn begin_root(&mut self) {
        if self.epoch == Self::MAX_EPOCH {
            self.visited.fill(0);
            self.epoch = 1;
        } else {
            self.epoch += 1;
        }
    }

    fn state(&self, expression: usize, order: usize, context: usize) -> Visit {
        let packed = self.visited[expression];
        if packed >> (Self::STATE_BITS * Self::STATE_SLOT_COUNT) != self.epoch {
            return Visit::Pending;
        }
        match (packed >> Self::slot_shift(order, context)) & 0b11 {
            1 => Visit::InProgress,
            2 => Visit::Differentiable,
            _ => Visit::Pending,
        }
    }

    fn set_state(&mut self, expression: usize, order: usize, context: usize, state: Visit) {
        let shift = Self::slot_shift(order, context);
        let state = match state {
            Visit::Pending => 0,
            Visit::InProgress => 1,
            Visit::Differentiable => 2,
        };
        let packed = &mut self.visited[expression];
        if *packed >> (Self::STATE_BITS * Self::STATE_SLOT_COUNT) != self.epoch {
            *packed = self.epoch << (Self::STATE_BITS * Self::STATE_SLOT_COUNT);
        }
        *packed = (*packed & !(0b11 << shift)) | (state << shift);
    }

    const fn slot_shift(order: usize, context: usize) -> u32 {
        ((order * 2 + context) as u32) * Self::STATE_BITS
    }
}

/// Every state demotion one system offers, split by what taking it costs the
/// initial values the model states.
pub(super) struct StateDemotionCandidates {
    /// Demotions the source system alone proves keep every stated initial
    /// value, in the order the reduction should try them.
    pub(super) admissible: Vec<DirectStateConstraint>,
    /// Demotions that drop the MLS 3.6 §8.6 initial equation of the coordinate
    /// they demote, as the source system reads it. §8.6 states that equation
    /// about a *quantity*, so such a demotion is legal exactly when the system
    /// it produces still states the value about a coordinate the runtime
    /// answers — a question only the rebuilt system can settle, which
    /// [`discarded_stated_initial_value`] asks of it. Tried after every
    /// unconditional demotion, so a reduction never spends a stated value while
    /// another way forward is open.
    pub(super) conditional: Vec<DirectStateConstraint>,
}

/// One stated initial value a rebuilt system no longer states.
#[derive(Clone)]
pub(super) struct DiscardedInitialValue {
    /// The pinned variable, named as the model declares it.
    pub(super) variable: String,
    /// That variable's declaration.
    pub(super) span: Span,
}

pub(super) fn direct_state_constraints(view: dae::DaeView<'_>) -> StateDemotionCandidates {
    let facts = DifferentiationFacts::collect(view);
    let mut constraints = view
        .continuous_owners()
        .flat_map(|owner| match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                direct_state_constraint(view, &facts, equation.residual(), equation.provenance())
                    .into_iter()
                    .collect::<Vec<_>>()
            }
            dae::ContinuousOwnerView::Structured { family, .. }
                if family.scalar_view()
                    == rumoca_core::ComprehensionScalarView::RowMajorProjection =>
            {
                family
                    .bodies()
                    .iter()
                    .filter_map(|body| {
                        direct_state_constraint(view, &facts, body, family.provenance())
                    })
                    .collect()
            }
            dae::ContinuousOwnerView::Structured { .. } => Vec::new(),
        })
        .collect::<Vec<_>>();
    constraints.extend(redundant_state_constraints(view, &facts.equalities));
    constraints.sort_by_key(|candidate| {
        let selection = view
            .variable(
                view.variable_id(candidate.state as usize)
                    .expect("candidate state identity resolves"),
            )
            .expect("candidate state declaration resolves")
            .state_select();
        (
            state_demotion_priority(selection),
            candidate.state,
            candidate.rhs,
            usize::from(candidate.rhs_sign == EqualitySign::Opposite),
        )
    });
    // A state can have several exact definitions. They are distinct
    // reconstruction proofs because `der(state)` is replaced by the derivative
    // of the selected RHS; discarding all but the first can turn a useful
    // kinematic definition into a tautology. Only byte-identical substitutions
    // are duplicates.
    constraints.dedup_by_key(|candidate| (candidate.state, candidate.rhs, candidate.rhs_sign));
    debug_assert!(
        constraints
            .iter()
            .all(|candidate| carries_a_differentiable_definition(view, *candidate)),
        "a demotion candidate must satisfy the contract its RHS is consumed under"
    );
    // MLS 3.6 §8.6 turns every `fixed = true` start into an initialization
    // equation, and a demoted state has no initialization equation of its own
    // left. The split runs over the merged candidate list, so it covers the
    // residual path and the equality-closure path alike, and it reads only the
    // state each candidate demotes, so every exact definition for one state
    // lands on the same side of it.
    let (admissible, conditional) = constraints.into_iter().partition(|candidate| {
        keeps_stated_initial_value(view, &facts.equalities, candidate.state)
    });
    StateDemotionCandidates {
        admissible,
        conditional,
    }
}

/// The MLS 3.6 §8.6 initial equation `rebuilt` no longer states, out of the ones
/// `stated` records about the system it was built from.
///
/// This is the postcondition a state demotion is accepted under. The source
/// system's own reading decides *which* obligations have to survive, and the
/// rebuilt system decides whether they did: a demotion that turns a pinned
/// state into an algebraic is legal when the equalities carry the stated value
/// onto a coordinate the runtime still answers, and is a dropped initial
/// condition when they do not. Checking it on the rebuilt system is what makes
/// the answer a proof rather than a prediction — the roles, the classes and the
/// substituted derivatives are all the reduction's own output.
///
/// Both reduction routes are checked against this. A state demotion changes a
/// coordinate's role outright; a holonomic reduction *replaces* the residual it
/// differentiates (`reconstruction::rebuild_holonomic_constraint`), which can
/// take an equality that carried a stated value onto another coordinate out of
/// the system. Neither is trusted to preserve what it does not name.
pub(super) fn discarded_stated_initial_value(
    source: dae::DaeView<'_>,
    rebuilt: dae::DaeView<'_>,
    stated: &[u32],
) -> Result<Option<DiscardedInitialValue>, StructuralError> {
    if stated.is_empty() {
        return Ok(None);
    }
    let kept = represented_initial_values(rebuilt);
    let mut discarded = None;
    for variable in stated.iter().copied() {
        // The two systems are compared by variable ordinal, which reconstruction
        // preserves by reserving one target variable per source variable in
        // source order. Every ordinal this comparison rests on is checked, on
        // the path that finds a discarded value and on the path that finds none,
        // because a silent renumbering would make the second one a lie.
        let declaration = |view: dae::DaeView<'_>| {
            view.variable_id(variable as usize)
                .and_then(|id| view.variable(id))
                .map(|declaration| {
                    (
                        declaration.name().as_str().to_string(),
                        declaration.declaration().span(),
                    )
                })
        };
        let (Some((name, span)), Some((rebuilt_name, _))) =
            (declaration(source), declaration(rebuilt))
        else {
            return Err(renumbered(variable));
        };
        if name != rebuilt_name {
            return Err(renumbered(variable));
        }
        if !kept.contains(&variable) && discarded.is_none() {
            discarded = Some(DiscardedInitialValue {
                variable: name,
                span,
            });
        }
    }
    Ok(discarded)
}

fn renumbered(variable: u32) -> StructuralError {
    StructuralError::UnspannedContractViolation {
        reason: format!(
            "a structural reduction renumbered variable {variable}, so the stated initial \
             values proved about the source system name nothing in the rebuilt one"
        ),
    }
}

/// Whether `candidate` satisfies the one contract its RHS is consumed under.
///
/// [`DirectStateConstraint::rhs`] is read at exactly one place —
/// `ExpressionRebuilder::rebuild_coordinate`, where a `der(state)` coordinate is
/// replaced by `differentiate(rhs)`. It is never substituted for the state's
/// *value*: the demoted state stays an unknown of the rebuilt system and the
/// residual that proves the equality stays in it to define that unknown. Two
/// consequences are load-bearing:
///
///   * a time-invariant displacement between the state and `rhs` vanishes under
///     `d/dt`, so only the *value* readers of [`SystemEqualities`] need the
///     offset-free layer;
///   * a sign does **not** vanish under `d/dt`, so the equality proof carries
///     it on the candidate and reconstruction applies it to `differentiate(rhs)`.
///
/// What must hold at runtime is that differentiation can reach `rhs` and that
/// the substitution does not re-enter itself: `rhs` is a whole-model expression
/// that does not name the state being demoted.
fn carries_a_differentiable_definition(
    view: dae::DaeView<'_>,
    candidate: DirectStateConstraint,
) -> bool {
    let Some(rhs) = view.expression_id(candidate.rhs as usize) else {
        return false;
    };
    let Some(node) = view.expression(rhs) else {
        return false;
    };
    if node.function_scope().is_some() || node.binder_domain().is_some() {
        return false;
    }
    let Some(state) = view
        .variable_id(candidate.state as usize)
        .and_then(|id| view.variable(id))
    else {
        return false;
    };
    !dae::expr_contains_var(view, rhs, state.id())
}

fn state_demotion_priority(selection: StateSelect) -> u8 {
    match selection {
        StateSelect::Never => 0,
        StateSelect::Avoid => 1,
        StateSelect::Default => 2,
        StateSelect::Prefer => 3,
        StateSelect::Always => 4,
    }
}

/// States an asserted coordinate equality proves redundant.
///
/// The equality holds for all time, so the demoted state keeps the residual
/// that defines it and every `der` of it becomes the exact derivative of the
/// class anchor — zero when the class is pinned to a parameter.
fn redundant_state_constraints(
    view: dae::DaeView<'_>,
    equalities: &SystemEqualities,
) -> Vec<DirectStateConstraint> {
    equalities
        .redundant_states()
        .filter_map(|(state, anchor, rhs_sign)| {
            let variable = view.variable(view.variable_id(state as usize)?)?;
            if variable.state_select() == StateSelect::Always
                || variable.value_type().scalar_type() != dae::ScalarType::Real
            {
                return None;
            }
            Some(DirectStateConstraint {
                state,
                rhs: equalities.anchor_expression(anchor)?,
                rhs_sign,
                owner: equalities.witness(state)?,
            })
        })
        .collect()
}

/// Whether the source system alone proves that demoting `state` keeps every
/// initial value the model states about the quantity it names.
///
/// MLS 3.6 §8.6: "For every Real variable vc with fixed = true, the equation
/// vc = startExpression is added to the initialization equations." A demoted
/// state is no longer a state, so it has no initialization equation of its own
/// left; that stated equation survives only when an equation the reduction
/// keeps reproduces it. Exactly one such proof is available from the source
/// system by itself, read off the offset-free equality layer because it is the
/// only one that proves anything about a *value*: a class anchored on another
/// state keeps that state, so the obligation survives when the anchor carries
/// the same one — `fixed = true` with the same stated start, same-signed.
///
/// Everything else is not a refusal but a *question*, and this layer is the
/// wrong place to answer it. A state that anchors its own class, an
/// opposite-signed member, an unpinned anchor, a class pinned to a
/// time-invariant value, two pinned members that disagree: whether the value
/// survives depends on the classes, roles and displacements of the system the
/// demotion produces, none of which exist yet. Those candidates go to
/// [`StateDemotionCandidates::conditional`], where the rebuilt system answers
/// with [`discarded_stated_initial_value`].
///
/// In particular an [`EqualityAnchor::Invariant`] class proves only that the
/// class holds *some* constant — `EqualityAnchor::Invariant { value: None, .. }`
/// names no expression at all, and the anchor carries no displacement — so it
/// cannot show the constant is the stated start. Accepting it here is what let
/// `x(start = 1, fixed = true)` in a class the system pins to 5 initialize at 5.
fn keeps_stated_initial_value(
    view: dae::DaeView<'_>,
    equalities: &SystemEqualities,
    state: u32,
) -> bool {
    let Some(variable) = view
        .variable_id(state as usize)
        .and_then(|id| view.variable(id))
    else {
        return false;
    };
    if variable.fixed() != Fixity::Fixed {
        return true;
    }
    match equalities.value_anchor_of(state) {
        Some((EqualityAnchor::State(anchor), EqualitySign::Same)) if anchor != state => view
            .variable_id(anchor as usize)
            .and_then(|id| view.variable(id))
            .is_some_and(|anchor| {
                anchor.fixed() == Fixity::Fixed && states_the_same_start(view, variable, anchor)
            }),
        _ => false,
    }
}

/// Whether two declarations state the same initial value.
///
/// An absent `start` is the MLS 3.6 §4.8 attribute default of zero for a Real,
/// so an omitted start and an explicit `start = 0` state the same thing. Two
/// present starts are compared as written: they are time-invariant expressions,
/// and structural equality is the only equality this phase can prove about them
/// without evaluating them. Comparing conservatively can only refuse a demotion
/// that would have been safe; it can never accept one that drops a start.
fn states_the_same_start<'dae>(
    view: dae::DaeView<'dae>,
    left: dae::VariableView<'dae>,
    right: dae::VariableView<'dae>,
) -> bool {
    match (left.start(), right.start()) {
        (None, None) => true,
        (Some(start), None) | (None, Some(start)) => numeric_literal(view, start) == Some(0.0),
        (Some(left), Some(right)) => states_the_same_expression(view, left, right),
    }
}

/// Whether two time-invariant expressions are the same expression as written.
pub(super) fn states_the_same_expression<'dae>(
    view: dae::DaeView<'dae>,
    left: dae::ExprId<'dae>,
    right: dae::ExprId<'dae>,
) -> bool {
    if left == right {
        return true;
    }
    let (Some(left), Some(right)) = (view.expression(left), view.expression(right)) else {
        return false;
    };
    match (left.operation(), right.operation()) {
        // `start = 0` on a Real may be written as either literal kind, so the
        // two numeric literals are compared by value rather than by spelling.
        (dae::ExpressionOperation::Literal(left), dae::ExpressionOperation::Literal(right)) => {
            match (literal_value(left), literal_value(right)) {
                (Some(left), Some(right)) => left == right,
                _ => left == right,
            }
        }
        (
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(left)),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(right)),
        ) => left == right,
        (
            dae::ExpressionOperation::Unary {
                operator: left_operator,
                operand: left,
            },
            dae::ExpressionOperation::Unary {
                operator: right_operator,
                operand: right,
            },
        ) => left_operator == right_operator && states_the_same_expression(view, left, right),
        (
            dae::ExpressionOperation::Binary {
                operator: left_operator,
                lhs: left_lhs,
                rhs: left_rhs,
            },
            dae::ExpressionOperation::Binary {
                operator: right_operator,
                lhs: right_lhs,
                rhs: right_rhs,
            },
        ) => {
            left_operator == right_operator
                && states_the_same_expression(view, left_lhs, right_lhs)
                && states_the_same_expression(view, left_rhs, right_rhs)
        }
        _ => false,
    }
}

pub(super) fn numeric_literal<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<f64> {
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Literal(literal) => literal_value(literal),
        _ => None,
    }
}

/// The numeric value of a literal, for the two kinds a Real `start` can carry.
///
/// An `Integer` too wide to convert exactly reports nothing rather than a
/// rounded value, which leaves the comparison above to fall back on spelling
/// equality — the conservative answer.
fn literal_value(literal: &dae::DaeLiteral) -> Option<f64> {
    match literal {
        dae::DaeLiteral::Real(value) => Some(*value),
        dae::DaeLiteral::Integer(value) => i32::try_from(*value).ok().map(f64::from),
        _ => None,
    }
}

fn direct_state_constraint<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    residual_id: dae::ExprId<'dae>,
    owner: dae::DaeProvenance,
) -> Option<DirectStateConstraint> {
    let residual = view.expression(residual_id)?;
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = residual.operation()
    else {
        return None;
    };
    let dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) =
        view.expression(lhs)?.operation()
    else {
        return None;
    };
    let variable = view.variable(view.variable_id(state.index() as usize)?)?;
    if variable.state_select() == StateSelect::Always
        || variable.value_type().scalar_type() != dae::ScalarType::Real
        || dae::expr_contains_var(view, rhs, variable.id())
        || reaches_demoted_derivative(view, facts, rhs, state)
        || !is_differentiable(
            view,
            facts,
            rhs,
            state,
            &mut vec![Visit::Pending; view.expression_count()],
        )
    {
        return None;
    }
    Some(DirectStateConstraint {
        state: state.index(),
        rhs: rhs.index(),
        rhs_sign: EqualitySign::Same,
        owner,
    })
}

/// Whether the definition closure of `root` names `der(demoted)`.
///
/// Reconstruction replaces `der(demoted)` by the derivative of this very
/// expression, so a definition it reaches that names `der(demoted)` again would
/// make the substitution refer to itself. The closure follows exactly the
/// indirections differentiation follows — state and derivative definitions — so
/// a candidate that clears this check cannot re-enter its own substitution.
fn reaches_demoted_derivative<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    root: dae::ExprId<'dae>,
    demoted: dae::StateId<'dae>,
) -> bool {
    let mut pending = vec![root];
    let mut expanded = vec![false; view.expression_count()];
    while let Some(root) = pending.pop() {
        if std::mem::replace(&mut expanded[root.index() as usize], true) {
            continue;
        }
        let mut found = false;
        let mut definitions = Vec::new();
        dae::for_each_expression(view, root, |_, expression| {
            let dae::ExpressionOperation::Coordinate(coordinate) = expression.operation() else {
                return;
            };
            let state = match coordinate {
                dae::CoordinateView::Derivative(state) if state == demoted => {
                    found = true;
                    return;
                }
                dae::CoordinateView::State(state) | dae::CoordinateView::Derivative(state) => state,
                _ => return,
            };
            definitions.extend(facts.derivative_definitions[state.index() as usize]);
        });
        if found {
            return true;
        }
        pending.extend(
            definitions
                .into_iter()
                .filter_map(|definition| view.expression_id(definition as usize)),
        );
    }
    false
}

#[cfg(test)]
pub(super) fn holonomic_constraints(view: dae::DaeView<'_>) -> Vec<HolonomicConstraint> {
    index_reduction_constraints(view)
        .into_iter()
        .filter(|constraint| constraint.lifted_algebraic.is_none())
        .collect()
}

pub(super) fn index_reduction_constraints(view: dae::DaeView<'_>) -> Vec<HolonomicConstraint> {
    let facts = DifferentiationFacts::collect(view);
    let causal = CausalDefinitions::derive(view);
    let mut scratch = HolonomicProofScratch::new(view.expression_count());
    view.continuous_owners()
        .enumerate()
        .flat_map(|(owner_ordinal, owner)| {
            let (owner, residuals): (_, Box<dyn Iterator<Item = _>>) = match owner {
                dae::ContinuousOwnerView::Residual { equation, .. } => (
                    equation.provenance(),
                    Box::new(std::iter::once((None, equation.residual()))),
                ),
                dae::ContinuousOwnerView::Structured { family, .. }
                    if family.scalar_view()
                        == rumoca_core::ComprehensionScalarView::RowMajorProjection =>
                {
                    (
                        family.provenance(),
                        Box::new(
                            family
                                .bodies()
                                .iter()
                                .enumerate()
                                .map(|(body, residual)| (Some(body), residual)),
                        ),
                    )
                }
                dae::ContinuousOwnerView::Structured { .. } => {
                    return Vec::new().into_iter();
                }
            };
            residuals
                .flat_map(|(body_ordinal, residual)| {
                    let ordinary =
                        prove_holonomic_differentiation(view, &facts, &mut scratch, residual).map(
                            |proof| HolonomicConstraint {
                                owner_ordinal,
                                body_ordinal,
                                residual: residual.index(),
                                owner,
                                proof,
                                lifted_algebraic: None,
                            },
                        );
                    let lifted = causal_definition(view, &causal, residual).and_then(
                        |(algebraic, definition)| {
                            let proof = prove_algebraic_lift_differentiation(
                                view,
                                &facts,
                                &mut scratch,
                                definition,
                            );
                            proof.map(|proof| HolonomicConstraint {
                                owner_ordinal,
                                body_ordinal,
                                residual: residual.index(),
                                owner,
                                proof,
                                lifted_algebraic: Some(algebraic.index()),
                            })
                        },
                    );
                    ordinary.into_iter().chain(lifted)
                })
                .collect::<Vec<_>>()
                .into_iter()
        })
        .collect()
}

/// Prove that one unstructured residual is a state-manifold constraint whose
/// structurally useful derivative reconstruction can form exactly.
///
/// Algebraic coordinates are admitted only through the signed equality class
/// already asserted by the source system. A pinned class differentiates to
/// zero; a state-anchored class differentiates as that state. Requiring two
/// distinct state anchors whenever an algebraic is involved excludes ordinary
/// component aliases such as `state - connector = 0`: differentiating those
/// would only replace a defining equation with a tautology. The older direct
/// state-only form remains admissible with one state.
fn prove_holonomic_differentiation<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    scratch: &mut HolonomicProofScratch,
    residual: dae::ExprId<'dae>,
) -> Option<HolonomicDifferentiationProof> {
    scratch.begin_root();
    let mut walk = HolonomicProofWalk {
        view,
        facts,
        anchored_states: Vec::new(),
        saw_algebraic: false,
        function_context: FunctionCallContext::default(),
        scratch,
    };
    if !walk.can_differentiate_order(residual, 1, true) {
        return None;
    }
    let mut value_visited = vec![Visit::Pending; view.expression_count()];
    if !can_materialize_holonomic_value(view, facts, residual, &mut value_visited) {
        return None;
    }
    let second_order = walk.can_differentiate_order(residual, 2, true);
    let maximum_order = if second_order {
        let mut derivative_visited = vec![Visit::Pending; view.expression_count()];
        let mut derivative_states = vec![Visit::Pending; view.variable_count()];
        if has_state_only_first_derivative(
            view,
            facts,
            residual,
            &mut derivative_visited,
            &mut derivative_states,
            &mut value_visited,
        ) {
            2
        } else {
            1
        }
    } else {
        1
    };
    walk.anchored_states.sort_unstable();
    walk.anchored_states.dedup();
    if walk.anchored_states.is_empty() || (walk.saw_algebraic && walk.anchored_states.len() < 2) {
        return None;
    }
    Some(HolonomicDifferentiationProof {
        residual: residual.index(),
        maximum_order,
        anchored_states: walk.anchored_states.into_boxed_slice(),
    })
}

fn causal_definition<'dae>(
    view: dae::DaeView<'dae>,
    causal: &CausalDefinitions<'dae>,
    residual: dae::ExprId<'dae>,
) -> Option<(dae::AlgebraicId<'dae>, dae::ExprId<'dae>)> {
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = view.expression(residual)?.operation()
    else {
        return None;
    };
    [(lhs, rhs), (rhs, lhs)]
        .into_iter()
        .find_map(|(target, value)| {
            let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) =
                view.expression(target)?.operation()
            else {
                return None;
            };
            let variable = view.variable(algebraic.into())?;
            if variable.role() != dae::VariableRole::Algebraic
                || variable.variability() != dae::ExpressionVariability::Continuous
                || variable.value_type().scalar_type() != dae::ScalarType::Real
                || variable.fixed() == Fixity::Fixed
                || causal.event_holds_variable(algebraic.into())
            {
                return None;
            }
            (causal.definition(algebraic) == Some(value)).then_some((algebraic, value))
        })
}

fn prove_algebraic_lift_differentiation<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    scratch: &mut HolonomicProofScratch,
    definition: dae::ExprId<'dae>,
) -> Option<HolonomicDifferentiationProof> {
    scratch.begin_root();
    let mut walk = HolonomicProofWalk {
        view,
        facts,
        anchored_states: Vec::new(),
        saw_algebraic: false,
        function_context: FunctionCallContext::default(),
        scratch,
    };
    if !walk.can_differentiate_order(definition, 1, true) {
        return None;
    }
    let mut value_visited = vec![Visit::Pending; view.expression_count()];
    if !can_materialize_holonomic_value(view, facts, definition, &mut value_visited) {
        return None;
    }
    walk.anchored_states.sort_unstable();
    walk.anchored_states.dedup();
    if walk.anchored_states.is_empty() {
        return None;
    }
    Some(HolonomicDifferentiationProof {
        residual: definition.index(),
        maximum_order: 1,
        anchored_states: walk.anchored_states.into_boxed_slice(),
    })
}

/// Whether the retained position-level residual can be reconstructed entirely
/// from exact state/invariant value anchors.
fn can_materialize_holonomic_value<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExprId<'dae>,
    visited: &mut [Visit],
) -> bool {
    can_materialize_holonomic_value_in_context(
        view,
        facts,
        expression,
        visited,
        &FunctionCallContext::default(),
    )
}

fn can_materialize_holonomic_value_in_context<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExprId<'dae>,
    visited: &mut [Visit],
    context: &FunctionCallContext<'dae>,
) -> bool {
    let scoped_context = context.scoped_to_expression(view, expression);
    let context = &scoped_context;
    let index = expression.index() as usize;
    if context.is_empty() {
        match visited[index] {
            Visit::Differentiable => return true,
            Visit::InProgress => return false,
            Visit::Pending => visited[index] = Visit::InProgress,
        }
    }
    if let Some((result, nested)) = context.call_result(view, expression) {
        let materializable =
            can_materialize_holonomic_value_in_context(view, facts, result, visited, &nested);
        if context.is_empty() {
            visited[index] = if materializable {
                Visit::Differentiable
            } else {
                Visit::Pending
            };
        }
        return materializable;
    }
    if let Some(argument) = forwarded_call_argument(view, expression) {
        return can_materialize_holonomic_value_in_context(view, facts, argument, visited, context);
    }
    let Some(expression) = view.expression(expression) else {
        return false;
    };
    let materializable = match expression.operation() {
        dae::ExpressionOperation::Literal(_)
        | dae::ExpressionOperation::Coordinate(
            dae::CoordinateView::Parameter(_)
            | dae::CoordinateView::Time
            | dae::CoordinateView::State(_),
        ) => true,
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(parameter)) => {
            context
                .parameter_argument(parameter)
                .is_some_and(|argument| {
                    can_materialize_holonomic_value_in_context(
                        view, facts, argument, visited, context,
                    )
                })
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => facts
            .equalities
            .value_anchor_of(algebraic.index())
            .and_then(|(anchor, _)| facts.equalities.anchor_expression(anchor))
            .and_then(|anchor| view.expression_id(anchor as usize))
            .or_else(|| facts.algebraic_definition(view, algebraic))
            .is_some_and(|anchor| {
                can_materialize_holonomic_value_in_context(view, facts, anchor, visited, context)
            }),
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => can_materialize_holonomic_value_in_context(view, facts, operand, visited, context),
        dae::ExpressionOperation::Binary {
            operator:
                dae::BinaryOperator::Add | dae::BinaryOperator::Subtract | dae::BinaryOperator::Multiply,
            lhs,
            rhs,
        } => {
            can_materialize_holonomic_value_in_context(view, facts, lhs, visited, context)
                && can_materialize_holonomic_value_in_context(view, facts, rhs, visited, context)
        }
        dae::ExpressionOperation::Array(elements) => elements.iter().all(|element| {
            can_materialize_holonomic_value_in_context(view, facts, element, visited, context)
        }),
        dae::ExpressionOperation::Builtin {
            builtin: dae::PureBuiltin::Sin | dae::PureBuiltin::Cos | dae::PureBuiltin::Atan2,
            arguments,
        } => arguments.iter().all(|argument| {
            can_materialize_holonomic_value_in_context(view, facts, argument, visited, context)
        }),
        _ => false,
    };
    if context.is_empty() {
        visited[index] = if materializable {
            Visit::Differentiable
        } else {
            Visit::Pending
        };
    }
    materializable
}

/// Whether the retained velocity-level residual materializes through state
/// coordinates rather than derivative or algebraic coordinates.
fn has_state_only_first_derivative<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExprId<'dae>,
    visited: &mut [Visit],
    state_visited: &mut [Visit],
    value_visited: &mut [Visit],
) -> bool {
    let index = expression.index() as usize;
    match visited[index] {
        Visit::Differentiable => return true,
        Visit::InProgress => return false,
        Visit::Pending => visited[index] = Visit::InProgress,
    }
    if let Some(argument) = forwarded_call_argument(view, expression) {
        return has_state_only_first_derivative(
            view,
            facts,
            argument,
            visited,
            state_visited,
            value_visited,
        );
    }
    let Some(expression) = view.expression(expression) else {
        return false;
    };
    let materializable = match expression.operation() {
        dae::ExpressionOperation::Literal(_)
        | dae::ExpressionOperation::Coordinate(
            dae::CoordinateView::Parameter(_) | dae::CoordinateView::Time,
        ) => true,
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) => {
            state_has_materializable_derivative(
                view,
                facts,
                state.index(),
                state_visited,
                value_visited,
            )
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => {
            match facts.equalities.value_anchor_of(algebraic.index()) {
                Some((EqualityAnchor::Invariant { .. }, _)) => true,
                Some((EqualityAnchor::State(state), _)) => state_has_materializable_derivative(
                    view,
                    facts,
                    state,
                    state_visited,
                    value_visited,
                ),
                None => facts
                    .algebraic_definition(view, algebraic)
                    .is_some_and(|definition| {
                        has_state_only_first_derivative(
                            view,
                            facts,
                            definition,
                            visited,
                            state_visited,
                            value_visited,
                        )
                    }),
            }
        }
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => has_state_only_first_derivative(
            view,
            facts,
            operand,
            visited,
            state_visited,
            value_visited,
        ),
        dae::ExpressionOperation::Binary {
            operator:
                dae::BinaryOperator::Add | dae::BinaryOperator::Subtract | dae::BinaryOperator::Multiply,
            lhs,
            rhs,
        } => {
            has_state_only_first_derivative(view, facts, lhs, visited, state_visited, value_visited)
                && has_state_only_first_derivative(
                    view,
                    facts,
                    rhs,
                    visited,
                    state_visited,
                    value_visited,
                )
        }
        _ => false,
    };
    visited[index] = if materializable {
        Visit::Differentiable
    } else {
        Visit::Pending
    };
    materializable
}

fn state_has_materializable_derivative<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    state: u32,
    state_visited: &mut [Visit],
    value_visited: &mut [Visit],
) -> bool {
    match state_visited[state as usize] {
        Visit::Differentiable => return true,
        Visit::InProgress => return false,
        Visit::Pending => state_visited[state as usize] = Visit::InProgress,
    }
    let materializable = facts.derivative_definitions[state as usize]
        .and_then(|definition| view.expression_id(definition as usize))
        .is_some_and(|definition| {
            can_materialize_holonomic_value(view, facts, definition, value_visited)
        });
    state_visited[state as usize] = if materializable {
        Visit::Differentiable
    } else {
        Visit::Pending
    };
    materializable
}

struct HolonomicProofWalk<'facts, 'dae> {
    view: dae::DaeView<'dae>,
    facts: &'facts DifferentiationFacts,
    anchored_states: Vec<u32>,
    saw_algebraic: bool,
    function_context: FunctionCallContext<'dae>,
    scratch: &'facts mut HolonomicProofScratch,
}

impl<'facts, 'dae> HolonomicProofWalk<'facts, 'dae> {
    fn can_differentiate_order(
        &mut self,
        expression: dae::ExprId<'dae>,
        order: u8,
        on_residual: bool,
    ) -> bool {
        let scoped_context = self
            .function_context
            .scoped_to_expression(self.view, expression);
        let previous_context = std::mem::replace(&mut self.function_context, scoped_context);
        let differentiable = self.can_differentiate_order_scoped(expression, order, on_residual);
        self.function_context = previous_context;
        differentiable
    }

    fn can_differentiate_order_scoped(
        &mut self,
        expression: dae::ExprId<'dae>,
        order: u8,
        on_residual: bool,
    ) -> bool {
        let index = expression.index() as usize;
        let context = usize::from(on_residual);
        if self.function_context.is_empty() {
            match self.scratch.state(index, order as usize, context) {
                Visit::Differentiable => return true,
                Visit::InProgress => return false,
                Visit::Pending => {
                    self.scratch
                        .set_state(index, order as usize, context, Visit::InProgress);
                }
            }
        }
        if let Some((result, nested)) = self.function_context.call_result(self.view, expression) {
            let previous = std::mem::replace(&mut self.function_context, nested);
            let differentiable = self.can_differentiate_order(result, order, on_residual);
            self.function_context = previous;
            self.cache_differentiability(index, order, context, differentiable);
            return differentiable;
        }
        if self
            .facts
            .expression_is_zero(self.view, expression, &self.function_context)
        {
            self.cache_differentiability(index, order, context, true);
            return true;
        }
        if let Some(argument) = forwarded_call_argument(self.view, expression) {
            let differentiable = self.can_differentiate_order(argument, order, on_residual);
            self.cache_differentiability(index, order, context, differentiable);
            return differentiable;
        }
        let expression_id = expression;
        let expression = self
            .view
            .expression(expression)
            .expect("checked differentiability expression resolves");
        if self.function_context.is_empty() && is_time_invariant(self.view, expression_id) {
            self.cache_differentiability(index, order, context, true);
            return true;
        }
        let differentiable = match expression.operation() {
            dae::ExpressionOperation::Literal(_) => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self
                .function_context
                .parameter_argument(parameter)
                .is_some_and(|argument| self.can_differentiate_order(argument, order, on_residual)),
            dae::ExpressionOperation::Coordinate(coordinate) => {
                self.can_differentiate_coordinate(coordinate, order, on_residual)
            }
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => self.can_differentiate_order(operand, order, on_residual),
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                matches!(
                    operator,
                    dae::BinaryOperator::Add
                        | dae::BinaryOperator::Subtract
                        | dae::BinaryOperator::Multiply
                ) && self.can_differentiate_order(lhs, order, on_residual)
                    && self.can_differentiate_order(rhs, order, on_residual)
            }
            dae::ExpressionOperation::Builtin { builtin, arguments } if order == 1 => {
                matches!(
                    builtin,
                    dae::PureBuiltin::Zeros
                        | dae::PureBuiltin::Ones
                        | dae::PureBuiltin::Identity
                        | dae::PureBuiltin::Sin
                        | dae::PureBuiltin::Cos
                        | dae::PureBuiltin::Atan2
                        | dae::PureBuiltin::Vector
                        | dae::PureBuiltin::Transpose
                        | dae::PureBuiltin::Diagonal
                        | dae::PureBuiltin::Skew
                        | dae::PureBuiltin::Cross
                        | dae::PureBuiltin::OuterProduct
                ) && arguments
                    .iter()
                    .all(|argument| self.can_differentiate_order(argument, order, on_residual))
            }
            dae::ExpressionOperation::Array(elements) => elements
                .iter()
                .all(|element| self.can_differentiate_order(element, order, on_residual)),
            dae::ExpressionOperation::Field { base, field } => self
                .function_context
                .projected_field(self.view, base, field)
                .is_some_and(|(projected, projected_context)| {
                    let previous = std::mem::replace(&mut self.function_context, projected_context);
                    let differentiable =
                        self.can_differentiate_order(projected, order, on_residual);
                    self.function_context = previous;
                    differentiable
                }),
            _ => false,
        };
        self.cache_differentiability(index, order, context, differentiable);
        differentiable
    }

    fn cache_differentiability(
        &mut self,
        expression: usize,
        order: u8,
        context: usize,
        differentiable: bool,
    ) {
        if self.function_context.is_empty() {
            self.scratch.set_state(
                expression,
                order as usize,
                context,
                if differentiable {
                    Visit::Differentiable
                } else {
                    Visit::Pending
                },
            );
        }
    }

    fn can_differentiate_coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'dae>,
        order: u8,
        on_residual: bool,
    ) -> bool {
        match coordinate {
            dae::CoordinateView::Parameter(_) | dae::CoordinateView::Time => true,
            dae::CoordinateView::State(state) => {
                self.can_differentiate_state(state.index(), order, on_residual)
            }
            dae::CoordinateView::Algebraic(algebraic) => {
                self.saw_algebraic |= on_residual;
                match self.facts.equalities.anchor_of(algebraic.index()) {
                    Some((EqualityAnchor::Invariant { .. }, _)) => true,
                    Some((anchor @ EqualityAnchor::State(state), _)) => {
                        self.can_differentiate_equality_anchor(anchor, state, order, on_residual)
                    }
                    None => self
                        .facts
                        .algebraic_definition(self.view, algebraic)
                        .is_some_and(|definition| {
                            self.can_differentiate_order(definition, order, false)
                        }),
                }
            }
            _ => false,
        }
    }

    fn can_differentiate_equality_anchor(
        &mut self,
        anchor: EqualityAnchor,
        state: u32,
        order: u8,
        on_residual: bool,
    ) -> bool {
        let Some(expression) = self
            .facts
            .equalities
            .anchor_expression(anchor)
            .and_then(|expression| self.view.expression_id(expression as usize))
            .and_then(|expression| self.view.expression(expression))
        else {
            return false;
        };
        matches!(
            expression.operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(candidate))
                if candidate.index() == state
        ) && self.can_differentiate_state(state, order, on_residual)
    }

    fn can_differentiate_state(&mut self, state: u32, order: u8, on_residual: bool) -> bool {
        if on_residual {
            self.anchored_states.push(state);
        }
        order == 1
            || self.facts.derivative_definitions[state as usize].is_some_and(|definition| {
                self.view
                    .expression_id(definition as usize)
                    .is_some_and(|definition| {
                        self.can_differentiate_order(definition, order - 1, false)
                    })
            })
    }
}

/// Whether reconstruction can differentiate `expression` exactly.
///
/// Mirrors the differentiation walk one for one, including the two
/// indirections it is allowed to follow: an algebraic coordinate resolved to
/// its equality anchor, and a derivative coordinate resolved to its explicit
/// definition. Re-entering an expression that is still being walked is a cycle
/// the differentiator could not terminate on, so it is rejected outright.
fn is_differentiable<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExprId<'dae>,
    demoted: dae::StateId<'dae>,
    visited: &mut [Visit],
) -> bool {
    is_differentiable_in_context(
        view,
        facts,
        expression,
        demoted,
        visited,
        &FunctionCallContext::default(),
    )
}

fn is_differentiable_in_context<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    expression: dae::ExprId<'dae>,
    demoted: dae::StateId<'dae>,
    visited: &mut [Visit],
    context: &FunctionCallContext<'dae>,
) -> bool {
    let scoped_context = context.scoped_to_expression(view, expression);
    let context = &scoped_context;
    let index = expression.index() as usize;
    if context.is_empty() {
        match visited[index] {
            Visit::Differentiable => return true,
            Visit::InProgress => return false,
            Visit::Pending => visited[index] = Visit::InProgress,
        }
    }
    if let Some((result, nested)) = context.call_result(view, expression) {
        let differentiable =
            is_differentiable_in_context(view, facts, result, demoted, visited, &nested);
        if context.is_empty() {
            visited[index] = if differentiable {
                Visit::Differentiable
            } else {
                Visit::Pending
            };
        }
        return differentiable;
    }
    if facts.expression_is_zero(view, expression, context) {
        if context.is_empty() {
            visited[index] = Visit::Differentiable;
        }
        return true;
    }
    let Some(node) = view.expression(expression) else {
        return false;
    };
    if context.is_empty() && is_time_invariant(view, expression) {
        visited[index] = Visit::Differentiable;
        return true;
    }
    let differentiable = match node.operation() {
        dae::ExpressionOperation::Literal(_) => true,
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(parameter)) => {
            context
                .parameter_argument(parameter)
                .is_some_and(|argument| {
                    is_differentiable_in_context(view, facts, argument, demoted, visited, context)
                })
        }
        dae::ExpressionOperation::Coordinate(coordinate) => {
            is_differentiable_coordinate(view, facts, coordinate, demoted, visited)
        }
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => is_differentiable_in_context(view, facts, operand, demoted, visited, context),
        dae::ExpressionOperation::Binary {
            operator:
                dae::BinaryOperator::Add
                | dae::BinaryOperator::Subtract
                | dae::BinaryOperator::Multiply
                | dae::BinaryOperator::Divide,
            lhs,
            rhs,
        } => {
            is_differentiable_in_context(view, facts, lhs, demoted, visited, context)
                && is_differentiable_in_context(view, facts, rhs, demoted, visited, context)
        }
        dae::ExpressionOperation::Builtin { builtin, arguments } => {
            builtin_is_differentiable(view, facts, builtin, arguments, demoted, visited, context)
        }
        dae::ExpressionOperation::Array(elements) => elements.iter().all(|element| {
            is_differentiable_in_context(view, facts, element, demoted, visited, context)
        }),
        dae::ExpressionOperation::Field { base, field } => context
            .projected_field(view, base, field)
            .is_some_and(|(projected, projected_context)| {
                is_differentiable_in_context(
                    view,
                    facts,
                    projected,
                    demoted,
                    visited,
                    &projected_context,
                )
            }),
        dae::ExpressionOperation::Index { .. } => {
            match singleton_real_projection(view, expression) {
                Some(SingletonRealProjection::State(state)) => state != demoted.index(),
                _ => false,
            }
        }
        _ => false,
    };
    if context.is_empty() {
        visited[index] = if differentiable {
            Visit::Differentiable
        } else {
            Visit::Pending
        };
    }
    differentiable
}

/// Admit exactly the pure builtins whose first derivative has a closed DAE
/// representation. The checked DAE constructor already proves builtin arity
/// and tensor shapes; this walk proves only that each value operand can itself
/// be differentiated under the current function-call substitution.
fn builtin_is_differentiable<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    builtin: dae::PureBuiltin,
    arguments: dae::ExpressionOperands<'dae>,
    demoted: dae::StateId<'dae>,
    visited: &mut [Visit],
    context: &FunctionCallContext<'dae>,
) -> bool {
    use dae::PureBuiltin as Builtin;

    match builtin {
        Builtin::Zeros | Builtin::Ones | Builtin::Identity => true,
        Builtin::Sin
        | Builtin::Cos
        | Builtin::Atan2
        | Builtin::Vector
        | Builtin::Transpose
        | Builtin::Diagonal
        | Builtin::Skew
        | Builtin::Cross
        | Builtin::OuterProduct => arguments.iter().all(|argument| {
            is_differentiable_in_context(view, facts, argument, demoted, visited, context)
        }),
        _ => false,
    }
}

fn is_differentiable_coordinate<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    coordinate: dae::CoordinateView<'dae>,
    demoted: dae::StateId<'dae>,
    visited: &mut [Visit],
) -> bool {
    match coordinate {
        dae::CoordinateView::Parameter(_) | dae::CoordinateView::Time => true,
        dae::CoordinateView::State(state) => state != demoted,
        dae::CoordinateView::Algebraic(algebraic) => {
            match facts.equalities.anchor_of(algebraic.index()) {
                Some((EqualityAnchor::Invariant { .. }, _)) => true,
                Some((anchor @ EqualityAnchor::State(_), _)) => facts
                    .equalities
                    .anchor_expression(anchor)
                    .and_then(|anchor| view.expression_id(anchor as usize))
                    .is_some_and(|anchor| is_differentiable(view, facts, anchor, demoted, visited)),
                None => facts
                    .algebraic_definition(view, algebraic)
                    .is_some_and(|definition| {
                        is_differentiable(view, facts, definition, demoted, visited)
                    }),
            }
        }
        dae::CoordinateView::Derivative(state) => {
            state != demoted
                && facts.derivative_definitions[state.index() as usize].is_some_and(|definition| {
                    view.expression_id(definition as usize)
                        .is_some_and(|definition| {
                            is_differentiable(view, facts, definition, demoted, visited)
                        })
                })
        }
        _ => false,
    }
}

/// Residuals that state one state derivative outright, in either orientation.
///
/// MSL writes both `der(phi) = w` and the equally common `w = der(phi)`, so a
/// detector that reads only the first form loses `d/dt phi` on most mechanical
/// components. Two residuals defining the same derivative leave it undefined
/// here rather than picking one arbitrarily.
pub(super) fn explicit_derivative_definitions(view: dae::DaeView<'_>) -> Vec<Option<u32>> {
    let mut definitions = vec![None; view.variable_count()];
    let mut duplicate = vec![false; view.variable_count()];
    for owner in view.continuous_owners() {
        let residuals: Box<dyn Iterator<Item = _>> = match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                Box::new(std::iter::once(equation.residual()))
            }
            dae::ContinuousOwnerView::Structured { family, .. }
                if family.scalar_view()
                    == rumoca_core::ComprehensionScalarView::RowMajorProjection =>
            {
                Box::new(family.bodies().iter())
            }
            dae::ContinuousOwnerView::Structured { .. } => continue,
        };
        for residual in residuals {
            let Some(residual) = view.expression(residual) else {
                continue;
            };
            let dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Subtract,
                lhs,
                rhs,
            } = residual.operation()
            else {
                continue;
            };
            let Some((state, definition)) = derivative_definition(view, lhs, rhs) else {
                continue;
            };
            let index = state as usize;
            if definitions[index].replace(definition).is_some() {
                duplicate[index] = true;
            }
        }
    }
    for (definition, duplicate) in definitions.iter_mut().zip(duplicate) {
        if duplicate {
            *definition = None;
        }
    }
    definitions
}

/// The state whose derivative one side of `a - b` names, and the other side.
fn derivative_definition<'dae>(
    view: dae::DaeView<'dae>,
    lhs: dae::ExprId<'dae>,
    rhs: dae::ExprId<'dae>,
) -> Option<(u32, u32)> {
    let derivative = |side: dae::ExprId<'dae>| {
        let expression = view.expression(side)?;
        if expression.function_scope().is_some() || expression.binder_domain().is_some() {
            return None;
        }
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) =
            expression.operation()
        else {
            return None;
        };
        Some(state.index())
    };
    match (derivative(lhs), derivative(rhs)) {
        (Some(state), None) => Some((state, rhs.index())),
        (None, Some(state)) => Some((state, lhs.index())),
        _ => None,
    }
}

#[cfg(test)]
mod proof_scratch_tests {
    use super::*;

    #[test]
    fn proof_scratch_reuses_one_expression_sized_table_across_many_roots() {
        const EXPRESSION_COUNT: usize = 16_384;
        const ROOT_COUNT: usize = 4_096;
        let mut scratch = HolonomicProofScratch::new(EXPRESSION_COUNT);
        let storage = scratch.visited.as_ptr();

        for root in 0..ROOT_COUNT {
            scratch.begin_root();
            let expression = root % EXPRESSION_COUNT;
            scratch.set_state(expression, 2, 1, Visit::Differentiable);
            assert_eq!(scratch.state(expression, 2, 1), Visit::Differentiable);
            if expression > 0 {
                assert_eq!(scratch.state(expression - 1, 2, 1), Visit::Pending);
            }
        }

        assert_eq!(scratch.visited.len(), EXPRESSION_COUNT);
        assert_eq!(scratch.visited.as_ptr(), storage);
    }
}
