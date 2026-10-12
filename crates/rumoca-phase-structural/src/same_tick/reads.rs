//! Same-instant source reads and matched derivative dependency projection.

use super::*;

/// Collect the variables an expression reads *at the same instant*.
///
/// `pre(..)`, `previous(..)`, and delay coordinates carry values from a
/// strictly earlier instant (SDO-002's exception set) and are excluded; every
/// current-lane coordinate kind is included, mirroring the proven GALEC
/// `collect_current_reads`.
pub fn collect_same_instant_reads<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    reads: &mut BTreeSet<u32>,
) {
    collect_expression_reads(view, expression, &BTreeMap::new(), &mut |_| false, reads);
}

pub(super) fn collect_expression_reads<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    sources: &BTreeMap<u32, BTreeSet<dae::ExprId<'dae>>>,
    projected: &mut impl FnMut(dae::ExprId<'dae>) -> bool,
    reads: &mut BTreeSet<u32>,
) {
    let mut traversal = dae::ExpressionTraversal::new();
    let mut pending = vec![expression];
    let mut seen = BTreeSet::new();
    while let Some(root) = pending.pop() {
        traversal.visit_pruned(view, [root], |id, node| {
            if !seen.insert(id) {
                return false;
            }
            if projected(id) {
                return false;
            }
            if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state)) =
                node.operation()
                && let Some(rows) = sources.get(&state.index())
            {
                pending.extend(rows.iter().copied());
                return false;
            }
            let id = match node.operation() {
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(id)) => {
                    Some(dae::VariableId::from(id))
                }
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Input(id)) => {
                    Some(dae::VariableId::from(id))
                }
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(id)) => {
                    Some(dae::VariableId::from(id))
                }
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) => {
                    Some(dae::VariableId::from(id))
                }
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(id)) => {
                    Some(dae::VariableId::from(id))
                }
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteValue(id)) => {
                    Some(dae::VariableId::from(id))
                }
                _ => None,
            };
            if let Some(id) = id {
                reads.insert(id.index());
            }
            true
        });
    }
}

/// Collect the same-instant reads of one condition DAG.
pub fn collect_condition_same_instant_reads<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    reads: &mut BTreeSet<u32>,
) {
    collect_condition_same_instant_reads_except(view, root, &BTreeSet::new(), reads);
}

/// Collect body-guard reads, omitting source activation subtrees by identity.
/// A coordinate read by both a trigger and a body guard stays a body read.
pub fn collect_condition_same_instant_reads_except<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    excluded: &BTreeSet<dae::ConditionId<'dae>>,
    reads: &mut BTreeSet<u32>,
) {
    collect_condition_same_instant_reads_pruned(view, root, excluded, |_| false, reads);
}

/// Collect condition reads while stopping at construction-issued storage projections.
/// The consumer supplies the exact projected expression identities; their source
/// operands are not executable reads of that projected context.
pub fn collect_condition_same_instant_reads_pruned<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    excluded: &BTreeSet<dae::ConditionId<'dae>>,
    mut projected: impl FnMut(dae::ExprId<'dae>) -> bool,
    reads: &mut BTreeSet<u32>,
) {
    collect_condition_reads(
        view,
        root,
        excluded,
        &BTreeMap::new(),
        &mut projected,
        reads,
    );
}

fn collect_condition_reads<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    excluded: &BTreeSet<dae::ConditionId<'dae>>,
    sources: &BTreeMap<u32, BTreeSet<dae::ExprId<'dae>>>,
    projected: &mut impl FnMut(dae::ExprId<'dae>) -> bool,
    reads: &mut BTreeSet<u32>,
) {
    let mut pending = vec![root];
    let mut seen = BTreeSet::new();
    while let Some(condition) = pending.pop() {
        if excluded.contains(&condition) || !seen.insert(condition.index()) {
            continue;
        }
        match view
            .condition(condition)
            .expect("checked condition identity resolves")
            .operation()
        {
            dae::ConditionOperation::Initial
            | dae::ConditionOperation::Always
            | dae::ConditionOperation::Clock(_) => {}
            dae::ConditionOperation::Relation(relation) => {
                let expression = view
                    .relation(relation)
                    .expect("checked relation identity resolves")
                    .expression();
                collect_expression_reads(view, expression, sources, projected, reads);
            }
            dae::ConditionOperation::Discrete(expression) => {
                collect_expression_reads(view, expression, sources, projected, reads);
            }
            dae::ConditionOperation::Not(inner) => pending.push(inner),
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                pending.extend([lhs, rhs]);
            }
        }
    }
}

impl<'dae> SameTickDefinitions<'dae> {
    /// Current expression reads after substituting the issued derivative rows.
    pub fn collect_expression_reads(
        &self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        reads: &mut BTreeSet<u32>,
    ) {
        collect_expression_reads(
            view,
            expression,
            &self.derivative_sources,
            &mut |_| false,
            reads,
        );
    }

    /// Current condition reads, retaining the caller's exact entry/storage exclusions.
    pub fn collect_condition_reads(
        &self,
        view: dae::DaeView<'dae>,
        root: dae::ConditionId<'dae>,
        excluded: &BTreeSet<dae::ConditionId<'dae>>,
        mut projected: impl FnMut(dae::ExprId<'dae>) -> bool,
        reads: &mut BTreeSet<u32>,
    ) {
        collect_condition_reads(
            view,
            root,
            excluded,
            &self.derivative_sources,
            &mut projected,
            reads,
        );
    }
}
