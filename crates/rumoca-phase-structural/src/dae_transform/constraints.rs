//! Recognize the singular-system constraints this phase is allowed to reduce.
//!
//! Nothing here rewrites a DAE. Each function reports what the source system
//! already proves: which states a residual defines directly, which residuals
//! are holonomic, and whether an expression survives the exact symbolic
//! differentiation the reconstruction will later perform. A candidate that
//! fails a preflight here is never handed to reconstruction, so every
//! `unreachable!` in the differentiation code stands on a check made here.

use rumoca_core::StateSelect;
use rumoca_ir_dae as dae;

use super::{DirectStateConstraint, HolonomicConstraint};

pub(super) fn direct_state_constraints(view: dae::DaeView<'_>) -> Vec<DirectStateConstraint> {
    let mut constraints = view
        .continuous_owners()
        .filter_map(|owner| match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                direct_state_constraint(view, equation)
            }
            dae::ContinuousOwnerView::Structured { .. } => None,
        })
        .collect::<Vec<_>>();
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

fn direct_state_constraint<'dae>(
    view: dae::DaeView<'dae>,
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
        || !is_differentiable(view, rhs, state, &mut vec![false; view.expression_count()])
    {
        return None;
    }
    Some(DirectStateConstraint {
        state: state.index(),
        rhs: rhs.index(),
        owner: equation.provenance(),
    })
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

fn is_differentiable<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    demoted: dae::StateId<'dae>,
    visited: &mut [bool],
) -> bool {
    let index = expression.index() as usize;
    if visited[index] {
        return true;
    }
    visited[index] = true;
    let Some(expression) = view.expression(expression) else {
        return false;
    };
    match expression.operation() {
        dae::ExpressionOperation::Literal(_) => true,
        dae::ExpressionOperation::Coordinate(coordinate) => match coordinate {
            dae::CoordinateView::Parameter(_) | dae::CoordinateView::Time => true,
            dae::CoordinateView::State(state) => state != demoted,
            _ => false,
        },
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => is_differentiable(view, operand, demoted, visited),
        dae::ExpressionOperation::Binary {
            operator:
                dae::BinaryOperator::Add
                | dae::BinaryOperator::Subtract
                | dae::BinaryOperator::Multiply
                | dae::BinaryOperator::Divide,
            lhs,
            rhs,
        } => {
            is_differentiable(view, lhs, demoted, visited)
                && is_differentiable(view, rhs, demoted, visited)
        }
        _ => false,
    }
}

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
        let Some(lhs) = view.expression(lhs) else {
            continue;
        };
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) =
            lhs.operation()
        else {
            continue;
        };
        let index = state.index() as usize;
        if definitions[index].replace(rhs.index()).is_some() {
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
