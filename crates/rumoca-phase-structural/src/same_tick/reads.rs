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
    collect_raw_reads(view, expression, &mut |_| false, reads);
}

fn collect_raw_reads<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    projected: &mut impl FnMut(dae::ExprId<'dae>) -> bool,
    reads: &mut BTreeSet<u32>,
) {
    dae::ExpressionTraversal::new().visit_pruned(view, [expression], |id, node| {
        if projected(id) {
            return false;
        }
        if let Some(variable) = current_variable(node.operation()) {
            reads.insert(variable.index());
        }
        true
    });
}

fn current_variable(operation: dae::ExpressionOperation<'_>) -> Option<dae::VariableId<'_>> {
    match operation {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(id)) => Some(id.into()),
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Input(id)) => Some(id.into()),
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(id)) => Some(id.into()),
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(id)) => Some(id.into()),
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(id)) => {
            Some(id.into())
        }
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteValue(id)) => {
            Some(id.into())
        }
        _ => None,
    }
}

pub(super) fn collect_expression_reads<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    sources: &DerivativeSources<'dae>,
    projected: &mut impl FnMut(dae::ExprId<'dae>) -> bool,
    reads: &mut BTreeSet<u32>,
) -> Result<(), SameTickOrderError> {
    if sources.is_empty() {
        collect_raw_reads(view, expression, projected, reads);
        return Ok(());
    }
    let mut omitted = BTreeSet::new();
    dae::ExpressionTraversal::new().visit_pruned(view, [expression], |id, _| {
        if projected(id) {
            omitted.insert(id);
            return false;
        }
        true
    });
    let count = view
        .expression(expression)
        .expect("checked expression resolves")
        .value_type()
        .scalar_count()
        .expect("checked expression scalar capacity");
    let mut pending = (0..count)
        .map(|scalar| (expression, scalar, None))
        .collect::<Vec<_>>();
    let mut seen = BTreeSet::new();
    while let Some((root, scalar, point)) = pending.pop() {
        if !seen.insert((root, scalar, point.clone())) {
            continue;
        }
        let mut derivatives = Vec::new();
        rumoca_eval_dae::for_each_scalar_coordinate_pruned(
            view,
            root,
            scalar,
            point
                .as_ref()
                .map(|(domain, indices): &(dae::DomainId<'dae>, Vec<i64>)| {
                    (*domain, indices.as_slice())
                }),
            &omitted,
            |coordinate, scalar| {
                if let dae::CoordinateView::Derivative(state) = coordinate {
                    derivatives.push((state.index(), scalar));
                } else if let Some(variable) =
                    current_variable(dae::ExpressionOperation::Coordinate(coordinate))
                {
                    reads.insert(variable.index());
                }
            },
        )?;
        for derivative in derivatives {
            let source =
                sources
                    .get(&derivative)
                    .ok_or_else(|| SameTickOrderError::MissingDerivative {
                        span: view
                            .expression(root)
                            .expect("checked expression resolves")
                            .provenance()
                            .span(),
                    })?;
            pending.push((
                source.expression,
                source.expression_scalar,
                source.domain_point.clone(),
            ));
        }
    }
    Ok(())
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
    visit_condition_expressions(view, root, excluded, |expression| {
        collect_raw_reads(view, expression, &mut projected, reads);
        Ok::<(), std::convert::Infallible>(())
    })
    .unwrap_or_else(|error| match error {});
}

fn collect_condition_reads<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    excluded: &BTreeSet<dae::ConditionId<'dae>>,
    sources: &DerivativeSources<'dae>,
    projected: &mut impl FnMut(dae::ExprId<'dae>) -> bool,
    reads: &mut BTreeSet<u32>,
) -> Result<(), SameTickOrderError> {
    visit_condition_expressions(view, root, excluded, |expression| {
        collect_expression_reads(view, expression, sources, projected, reads)
    })
}

fn visit_condition_expressions<'dae, E>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    excluded: &BTreeSet<dae::ConditionId<'dae>>,
    mut visit: impl FnMut(dae::ExprId<'dae>) -> Result<(), E>,
) -> Result<(), E> {
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
                visit(expression)?;
            }
            dae::ConditionOperation::Discrete(expression) => {
                visit(expression)?;
            }
            dae::ConditionOperation::Not(inner) => pending.push(inner),
            dae::ConditionOperation::And(lhs, rhs)
            | dae::ConditionOperation::Or(lhs, rhs)
            | dae::ConditionOperation::AnyRise(lhs, rhs) => {
                pending.extend([lhs, rhs]);
            }
        }
    }
    Ok(())
}

impl<'dae> SameTickDefinitions<'dae> {
    /// Current expression reads after substituting the issued derivative rows.
    pub fn collect_expression_reads(
        &self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        reads: &mut BTreeSet<u32>,
    ) -> Result<(), SameTickOrderError> {
        collect_expression_reads(
            view,
            expression,
            &self.derivative_sources,
            &mut |_| false,
            reads,
        )
    }

    /// Current condition reads, retaining the caller's exact entry/storage exclusions.
    pub fn collect_condition_reads(
        &self,
        view: dae::DaeView<'dae>,
        root: dae::ConditionId<'dae>,
        excluded: &BTreeSet<dae::ConditionId<'dae>>,
        mut projected: impl FnMut(dae::ExprId<'dae>) -> bool,
        reads: &mut BTreeSet<u32>,
    ) -> Result<(), SameTickOrderError> {
        collect_condition_reads(
            view,
            root,
            excluded,
            &self.derivative_sources,
            &mut projected,
            reads,
        )
    }
}
