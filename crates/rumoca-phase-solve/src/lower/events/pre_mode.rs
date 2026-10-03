//! Which `pre` snapshot an event-time discrete row reads (MLS §3.7.5).

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

pub(super) fn expression_pre_mode<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    sampled: bool,
) -> solve::DiscreteEventPreMode {
    if sampled {
        return solve::DiscreteEventPreMode::EventEntry;
    }
    let mut mode = solve::DiscreteEventPreMode::FollowCurrent;
    dae::for_each_expression(view, expression, |_, expression| {
        let found = match expression.operation() {
            dae::ExpressionOperation::Coordinate(
                dae::CoordinateView::PreDiscreteReal(_) | dae::CoordinateView::PreDiscreteValue(_),
            ) => solve::DiscreteEventPreMode::Fixed,
            dae::ExpressionOperation::Coordinate(
                dae::CoordinateView::PreState(_)
                | dae::CoordinateView::PreAlgebraic(_)
                | dae::CoordinateView::Previous(_),
            ) => solve::DiscreteEventPreMode::EventEntry,
            _ => solve::DiscreteEventPreMode::FollowCurrent,
        };
        mode = merge_pre_mode(mode, found);
    });
    mode
}

pub(super) fn condition_pre_mode<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
) -> solve::DiscreteEventPreMode {
    let mut pending = vec![root];
    let mut visited = vec![false; view.condition_count()];
    let mut mode = solve::DiscreteEventPreMode::FollowCurrent;
    while let Some(condition) = pending.pop() {
        let index = condition.index() as usize;
        if visited[index] {
            continue;
        }
        visited[index] = true;
        let condition = view
            .condition(condition)
            .expect("checked condition identity resolves");
        match condition.operation() {
            dae::ConditionOperation::Initial => {}
            dae::ConditionOperation::Relation(relation) => {
                let expression = view
                    .relation(relation)
                    .expect("checked relation identity resolves")
                    .expression();
                mode = merge_pre_mode(mode, expression_pre_mode(view, expression, false));
            }
            dae::ConditionOperation::Discrete(expression) => {
                mode = merge_pre_mode(mode, expression_pre_mode(view, expression, false));
            }
            dae::ConditionOperation::Clock(_) | dae::ConditionOperation::Always => {}
            dae::ConditionOperation::Not(operand) => pending.push(operand),
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                pending.push(rhs);
                pending.push(lhs);
            }
        }
    }
    mode
}

pub(super) fn merge_pre_mode(
    lhs: solve::DiscreteEventPreMode,
    rhs: solve::DiscreteEventPreMode,
) -> solve::DiscreteEventPreMode {
    match (lhs, rhs) {
        (solve::DiscreteEventPreMode::EventEntry, _)
        | (_, solve::DiscreteEventPreMode::EventEntry) => solve::DiscreteEventPreMode::EventEntry,
        (solve::DiscreteEventPreMode::Fixed, _) | (_, solve::DiscreteEventPreMode::Fixed) => {
            solve::DiscreteEventPreMode::Fixed
        }
        (
            solve::DiscreteEventPreMode::FollowCurrent,
            solve::DiscreteEventPreMode::FollowCurrent,
        ) => solve::DiscreteEventPreMode::FollowCurrent,
    }
}
