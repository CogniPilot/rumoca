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

use rumoca_core::{Span, StateSelect};
use rumoca_ir_dae as dae;

use super::equalities::{EqualityAnchor, EqualitySign, SystemEqualities};
use super::initial_pins::represented_initial_values;
use super::{DirectStateConstraint, HolonomicConstraint};
use crate::StructuralError;

/// The exact indirections reconstruction is allowed to follow while
/// differentiating, gathered once per source system.
pub(super) struct DifferentiationFacts {
    pub(super) equalities: SystemEqualities,
    pub(super) derivative_definitions: Vec<Option<u32>>,
}

impl DifferentiationFacts {
    pub(super) fn collect(view: dae::DaeView<'_>) -> Self {
        Self {
            equalities: SystemEqualities::collect(view),
            derivative_definitions: explicit_derivative_definitions(view),
        }
    }
}

/// Where one expression stands in a differentiability walk.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Visit {
    Pending,
    InProgress,
    Differentiable,
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
        .filter_map(|owner| match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                direct_state_constraint(view, &facts, equation)
            }
            dae::ContinuousOwnerView::Structured { .. } => None,
        })
        .collect::<Vec<_>>();
    constraints.extend(redundant_state_constraints(view, &facts.equalities));
    let mut claimed = vec![false; view.variable_count()];
    constraints
        .retain(|candidate| !std::mem::replace(&mut claimed[candidate.state as usize], true));
    constraints.sort_by_key(|candidate| {
        let selection = view
            .variable(
                view.variable_id(candidate.state as usize)
                    .expect("candidate state identity resolves"),
            )
            .expect("candidate state declaration resolves")
            .state_select();
        (state_demotion_priority(selection), candidate.state)
    });
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
    // state each candidate demotes, so every candidate for one state lands on
    // the same side of it however that candidate was detected.
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
///   * a sign does **not** vanish under `d/dt`, so an opposite-signed class
///     member would need a negated `rhs` and is not a candidate at all — see
///     [`SystemEqualities::redundant_states`].
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
        .filter_map(|(state, anchor)| {
            let variable = view.variable(view.variable_id(state as usize)?)?;
            if variable.state_select() == StateSelect::Always {
                return None;
            }
            Some(DirectStateConstraint {
                state,
                rhs: equalities.anchor_expression(anchor)?,
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
    if variable.fixed() != Some(true) {
        return true;
    }
    match equalities.value_anchor_of(state) {
        Some((EqualityAnchor::State(anchor), EqualitySign::Same)) if anchor != state => view
            .variable_id(anchor as usize)
            .and_then(|id| view.variable(id))
            .is_some_and(|anchor| {
                anchor.fixed() == Some(true) && states_the_same_start(view, variable, anchor)
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
    equation: dae::ResidualEquationView<'dae>,
) -> Option<DirectStateConstraint> {
    let residual = view.expression(equation.residual())?;
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
        || !variable.value_type().is_scalar()
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
        owner: equation.provenance(),
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

pub(super) fn holonomic_constraints(view: dae::DaeView<'_>) -> Vec<HolonomicConstraint> {
    let definitions = explicit_derivative_definitions(view);
    view.continuous_owners()
        .filter_map(|owner| {
            let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
                return None;
            };
            let residual = equation.residual();
            let mut has_state = false;
            let mut forbidden = false;
            dae::for_each_expression(view, residual, |_, expression| {
                let dae::ExpressionOperation::Coordinate(coordinate) = expression.operation()
                else {
                    return;
                };
                match coordinate {
                    dae::CoordinateView::State(_) => has_state = true,
                    dae::CoordinateView::Derivative(_) | dae::CoordinateView::Algebraic(_) => {
                        forbidden = true;
                    }
                    _ => {}
                }
            });
            (has_state && !forbidden && can_differentiate_order(view, residual, 2, &definitions))
                .then_some(HolonomicConstraint {
                    residual: residual.index(),
                    owner: equation.provenance(),
                })
        })
        .collect()
}

fn can_differentiate_order<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    order: u8,
    definitions: &[Option<u32>],
) -> bool {
    let expression = view
        .expression(expression)
        .expect("checked differentiability expression resolves");
    match expression.operation() {
        dae::ExpressionOperation::Literal(_) => true,
        dae::ExpressionOperation::Coordinate(coordinate) => match coordinate {
            dae::CoordinateView::Parameter(_) | dae::CoordinateView::Time => true,
            dae::CoordinateView::State(state) => {
                definitions[state.index() as usize].is_some_and(|definition| {
                    order == 1
                        || can_differentiate_order(
                            view,
                            view.expression_id(definition as usize)
                                .expect("derivative definition resolves"),
                            order - 1,
                            definitions,
                        )
                })
            }
            _ => false,
        },
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => can_differentiate_order(view, operand, order, definitions),
        dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
            matches!(
                operator,
                dae::BinaryOperator::Add
                    | dae::BinaryOperator::Subtract
                    | dae::BinaryOperator::Multiply
            ) && can_differentiate_order(view, lhs, order, definitions)
                && can_differentiate_order(view, rhs, order, definitions)
        }
        _ => false,
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
    let index = expression.index() as usize;
    match visited[index] {
        Visit::Differentiable => return true,
        Visit::InProgress => return false,
        Visit::Pending => visited[index] = Visit::InProgress,
    }
    let Some(node) = view.expression(expression) else {
        return false;
    };
    let differentiable = match node.operation() {
        dae::ExpressionOperation::Literal(_) => true,
        dae::ExpressionOperation::Coordinate(coordinate) => {
            is_differentiable_coordinate(view, facts, coordinate, demoted, visited)
        }
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => is_differentiable(view, facts, operand, demoted, visited),
        dae::ExpressionOperation::Binary {
            operator:
                dae::BinaryOperator::Add
                | dae::BinaryOperator::Subtract
                | dae::BinaryOperator::Multiply
                | dae::BinaryOperator::Divide,
            lhs,
            rhs,
        } => {
            is_differentiable(view, facts, lhs, demoted, visited)
                && is_differentiable(view, facts, rhs, demoted, visited)
        }
        _ => false,
    };
    visited[index] = if differentiable {
        Visit::Differentiable
    } else {
        Visit::Pending
    };
    differentiable
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
                Some((EqualityAnchor::State(anchor), _)) => anchor != demoted.index(),
                None => false,
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
        let dae::ContinuousOwnerView::Residual { equation, .. } = owner else {
            continue;
        };
        let Some(residual) = view.expression(equation.residual()) else {
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
