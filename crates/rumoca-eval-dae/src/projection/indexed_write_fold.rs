//! Exact projection of a single owned-binder array write, not RNG evaluation.
use super::*;
use crate::projection::fold_graph::FoldNode;

/// Positive certificate for one checked, unconditional, scalar indexed write.
/// Other update families retain the authoritative ordinary point traversal.
struct IndexedWrite<'dae> {
    domain: dae::DomainId<'dae>,
    value: dae::ExprId<'dae>,
    point: Option<Vec<i64>>,
    ordinal: Option<usize>,
    points: usize,
    #[cfg(debug_assertions)]
    extent: u32,
}

impl<'dae> Projection<'_, 'dae> {
    pub(super) fn project_indexed_write_fold(
        &mut self,
        node: &FoldNode<'dae>,
    ) -> Result<bool, ProjectionError> {
        // The reference path remains independently executable for differential
        // controls. Initial-value projection is owned by project_fold_node.
        #[cfg(test)]
        if self.cache.uncached_fold_reference || self.cache.uncached_indexed_write_folds {
            return Ok(false);
        }
        let Some(checked) = indexed_write(self.view, node) else {
            return Ok(false);
        };
        #[cfg(test)]
        {
            self.cache.indexed_write_folds += 1;
        }
        #[cfg(debug_assertions)]
        profile::indexed_write(
            node,
            checked.value,
            checked.ordinal,
            checked.points,
            checked.extent,
        );
        // Preserve first-occurrence graph edge order. A passthrough precedes
        // the selected write iff the selected point is not ordinal zero.
        if checked.points > 0 && checked.ordinal != Some(0) {
            self.enqueue_fold(node.fold, node.carried, None, node.scalar)?;
        }
        if let Some(point) = checked.point {
            self.domain_contexts.push(checked.domain, point);
            let projected = self.expression(checked.value, 0);
            self.domain_contexts.pop();
            projected?;
        }
        if checked.ordinal == Some(0) && checked.points > 1 {
            self.enqueue_fold(node.fold, node.carried, None, node.scalar)?;
        }
        Ok(true)
    }
}

fn indexed_write<'dae>(
    view: dae::DaeView<'dae>,
    node: &FoldNode<'dae>,
) -> Option<IndexedWrite<'dae>> {
    if node.field.is_some() {
        return None;
    }
    let update = transparent(view, node.update);
    let dae::ExpressionOperation::ArrayUpdate {
        base,
        value,
        subscripts,
    } = view.expression(update)?.operation()
    else {
        return None;
    };
    let base = view.expression(transparent(view, base))?;
    if !matches!(base.operation(), dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. }
        if fold == node.fold && carried == node.carried)
    {
        return None;
    }
    if base.value_type().is_record() || view.expression(value)?.value_type().is_record() {
        return None;
    }
    let [extent] = base.value_type().dimensions() else {
        return None;
    };
    if !view.expression(value)?.value_type().dimensions().is_empty() || subscripts.len() != 1 {
        return None;
    }
    let Some(dae::SubscriptView::Index { expression, .. }) = subscripts.get(0) else {
        return None;
    };
    prove_write_domain(view, node, value, expression, *extent)
}

fn prove_write_domain<'dae>(
    view: dae::DaeView<'dae>,
    node: &FoldNode<'dae>,
    value: dae::ExprId<'dae>,
    address: dae::ExprId<'dae>,
    extent: u32,
) -> Option<IndexedWrite<'dae>> {
    let address = view.expression(address)?;
    // Match the ordinary array-update owner's exact-address eligibility. A
    // nonconstant selector instead walks all replacement scalars and must not
    // silently become an exact-selection shortcut.
    if address.variability() != dae::ExpressionVariability::Constant {
        return None;
    }
    let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) =
        address.operation()
    else {
        return None;
    };
    let fold = view.function_fold(node.fold)?;
    if binder.domain() != fold.domain() || binder.ordinal() != 0 {
        return None;
    }
    let domain = view.domain(fold.domain())?.structured();
    let [axis] = domain.binders.as_slice() else {
        return None;
    };
    let points = domain.scalar_count().ok()?;
    // Positive whole-domain address proof, including source direction/stride.
    // Empty domains never execute an address; nonempty source endpoints must
    // both be in-range. Conservative for strides whose terminal endpoint is
    // outside extent even when their last reached coordinate would be valid.
    if points > 0
        && (axis.lower < 1
            || axis.upper < 1
            || axis.lower > i64::from(extent)
            || axis.upper > i64::from(extent))
    {
        return None;
    }
    let selected = i64::try_from(node.scalar).ok()?.checked_add(1)?;
    if selected > i64::from(extent) {
        return None;
    }
    let point = vec![selected];
    let ordinal = domain.ordinal_of(&point).ok()?;
    Some(IndexedWrite {
        domain: fold.domain(),
        value,
        point: ordinal.map(|_| point),
        ordinal,
        points,
        #[cfg(debug_assertions)]
        extent,
    })
}

fn transparent<'dae>(view: dae::DaeView<'dae>, mut value: dae::ExprId<'dae>) -> dae::ExprId<'dae> {
    while let dae::ExpressionOperation::FunctionValue { definition, .. } = view
        .expression(value)
        .expect("construction-owned expression resolves")
        .operation()
    {
        value = definition.rhs();
    }
    value
}
