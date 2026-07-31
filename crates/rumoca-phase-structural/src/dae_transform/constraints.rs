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

use rumoca_core::StateSelect;
use rumoca_ir_dae as dae;

use super::equalities::{EqualityAnchor, SystemEqualities};
use super::{DirectStateConstraint, HolonomicConstraint};

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

pub(super) fn direct_state_constraints(view: dae::DaeView<'_>) -> Vec<DirectStateConstraint> {
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
    constraints
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
            if variable.state_select() == StateSelect::Always
                || !keeps_stated_initial_value(view, variable, anchor)
            {
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

/// Whether demoting `state` onto `anchor` keeps every initial value the model
/// states about the shared quantity.
///
/// A demoted state is no longer initialized in its own right, so a `fixed`
/// start on it would simply vanish unless the surviving anchor carries the same
/// obligation. A time-invariant anchor determines the class outright and needs
/// no initial value at all; a state anchor must be pinned itself.
fn keeps_stated_initial_value(
    view: dae::DaeView<'_>,
    state: dae::VariableView<'_>,
    anchor: EqualityAnchor,
) -> bool {
    if state.fixed() != Some(true) {
        return true;
    }
    match anchor {
        EqualityAnchor::Invariant(_) => true,
        EqualityAnchor::State(anchor) => view
            .variable_id(anchor as usize)
            .and_then(|id| view.variable(id))
            .is_some_and(|anchor| anchor.fixed() == Some(true)),
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
                Some((EqualityAnchor::Invariant(_), _)) => true,
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
