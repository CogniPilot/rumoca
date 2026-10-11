use rumoca_ir_dae as dae;
use std::collections::BTreeSet;

/// Visit each data/activation expression and condition once. The expression
/// grammar remains in the existing IR walker; condition coordinates expand
/// through their issued condition owners rather than being opaque leaves.
pub(crate) fn for_each_dependency<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    mut visit: impl FnMut(dae::ExpressionView<'dae>),
) {
    let mut expressions = vec![expression];
    let mut conditions = Vec::new();
    let mut seen_expressions = BTreeSet::new();
    let mut seen_conditions = BTreeSet::new();
    let mut traversal = dae::ExpressionTraversal::new();
    while !expressions.is_empty() || !conditions.is_empty() {
        if !expressions.is_empty() {
            let roots = std::mem::take(&mut expressions);
            visit_dependency_expressions(
                view,
                &mut traversal,
                roots,
                &mut seen_expressions,
                &mut expressions,
                &mut conditions,
                &mut visit,
            );
        }
        while let Some(id) = conditions.pop() {
            if !seen_conditions.insert(id) {
                continue;
            }
            let node = view
                .condition(id)
                .expect("checked condition identity resolves");
            match node.operation() {
                dae::ConditionOperation::Relation(id) => expressions.push(
                    view.relation(id)
                        .expect("checked relation identity resolves")
                        .expression(),
                ),
                dae::ConditionOperation::Discrete(expression) => expressions.push(expression),
                dae::ConditionOperation::Not(inner) => conditions.push(inner),
                dae::ConditionOperation::And(lhs, rhs)
                | dae::ConditionOperation::Or(lhs, rhs)
                | dae::ConditionOperation::AnyRise(lhs, rhs) => conditions.extend([lhs, rhs]),
                dae::ConditionOperation::Initial
                | dae::ConditionOperation::Always
                | dae::ConditionOperation::Clock(_) => {}
            }
        }
    }
}

fn visit_dependency_expressions<'dae>(
    view: dae::DaeView<'dae>,
    traversal: &mut dae::ExpressionTraversal<'dae>,
    roots: Vec<dae::ExprId<'dae>>,
    seen: &mut BTreeSet<dae::ExprId<'dae>>,
    expressions: &mut Vec<dae::ExprId<'dae>>,
    conditions: &mut Vec<dae::ConditionId<'dae>>,
    visit: &mut impl FnMut(dae::ExpressionView<'dae>),
) {
    traversal.visit_pruned(view, roots, |id, node| {
        if !seen.insert(id) {
            return false;
        }
        visit(node);
        expressions.extend(view.model_activation_guard(id));
        if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Condition(id)) =
            node.operation()
        {
            conditions.push(id);
        }
        true
    });
}

pub(crate) fn for_each_expression_clock_owner<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    variable_clock: impl Fn(dae::VariableId<'dae>) -> Option<dae::ClockId<'dae>>,
    mut visit: impl FnMut(dae::ClockId<'dae>),
) {
    for_each_dependency(view, expression, |node| {
        let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
            return;
        };
        let owner = if let dae::CoordinateView::Previous(previous) = coordinate {
            Some(
                view.previous(previous)
                    .expect("checked previous identity")
                    .clock(),
            )
        } else {
            super::coordinate_variable(coordinate)
                .or_else(|| super::pre_coordinate_variable(coordinate))
                .and_then(|index| view.variable_id(index as usize))
                .and_then(&variable_clock)
        };
        if let Some(owner) = owner {
            visit(owner);
        }
    });
}

pub(crate) fn expression_clock_owner<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    variable_clock: impl Fn(dae::VariableId<'dae>) -> Option<dae::ClockId<'dae>>,
) -> Option<dae::ClockId<'dae>> {
    let mut owner = None;
    for_each_expression_clock_owner(view, expression, variable_clock, |found| {
        owner.get_or_insert(found);
    });
    owner
}

/// The clock whose partition owns the relation `expression`, or `None` when it
/// is a continuous-time relation.
///
/// A relation of a clocked equation reads only its partition's values, which
/// change on ticks alone, so it is evaluated on those ticks and owns no root
/// (MLS §16.1). A relation that also reads `time` or an unclocked continuous
/// coordinate (a state, an algebraic, or an input) changes during integration
/// even when it reads a clocked value that is held between ticks (`time >=
/// t_i + t_width` with `t_i` written by `when sample(..)`), so MLS §8.5 makes
/// it an event-generating relation whose crossing must be located.
pub(crate) fn relation_clock_owner<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    variable_clock: impl Fn(dae::VariableId<'dae>) -> Option<dae::ClockId<'dae>>,
) -> Option<dae::ClockId<'dae>> {
    let owner = expression_clock_owner(view, expression, &variable_clock)?;
    let mut continuous = false;
    for_each_dependency(view, expression, |node| {
        let dae::ExpressionOperation::Coordinate(coordinate) = node.operation() else {
            return;
        };
        let variable = match coordinate {
            dae::CoordinateView::Time => {
                continuous = true;
                return;
            }
            dae::CoordinateView::State(id) | dae::CoordinateView::Derivative(id) => id.index(),
            dae::CoordinateView::Algebraic(id) => id.index(),
            dae::CoordinateView::Input(id) => id.index(),
            _ => return,
        };
        continuous |= view
            .variable_id(variable as usize)
            .is_some_and(|variable| variable_clock(variable).is_none());
    });
    (!continuous).then_some(owner)
}
