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
    collect_raw_reads(view, expression, &|_| false, reads);
}

fn collect_raw_reads<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    projected: &impl Fn(dae::ExprId<'dae>) -> bool,
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
    projected: &impl Fn(dae::ExprId<'dae>) -> bool,
    reads: &mut BTreeSet<u32>,
) -> Result<(), SameTickOrderError> {
    if sources.is_empty() || !has_derivative_demand(view, expression, projected) {
        collect_raw_reads(view, expression, projected, reads);
        return Ok(());
    }
    let node = view
        .expression(expression)
        .expect("checked expression resolves");
    let count =
        node.value_type()
            .scalar_count()
            .ok_or(SameTickOrderError::UnsupportedDerivativeValue {
                span: node.provenance().span(),
            })?;
    let mut projection = rumoca_eval_dae::PrunedScalarCoordinateProjection::new(view, projected);
    let mut pending = (0..count)
        .map(|scalar| (expression, scalar, None))
        .collect::<Vec<_>>();
    let mut seen = BTreeSet::new();
    while let Some((root, scalar, point)) = pending.pop() {
        if !seen.insert((root, scalar, point.clone())) {
            continue;
        }
        let mut derivatives = Vec::new();
        projection.project(
            root,
            scalar,
            point
                .as_ref()
                .map(|(domain, indices): &(dae::DomainId<'dae>, Vec<i64>)| {
                    (*domain, indices.as_slice())
                }),
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

fn has_derivative_demand<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    projected: &impl Fn(dae::ExprId<'dae>) -> bool,
) -> bool {
    let mut demanded = false;
    dae::ExpressionTraversal::new().visit_pruned(view, [expression], |id, node| {
        if projected(id) {
            return false;
        }
        demanded |= matches!(
            node.operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(_))
        );
        true
    });
    demanded
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
    projected: impl Fn(dae::ExprId<'dae>) -> bool,
    reads: &mut BTreeSet<u32>,
) {
    visit_condition_expressions(view, root, excluded, |expression| {
        collect_raw_reads(view, expression, &projected, reads);
        Ok::<(), std::convert::Infallible>(())
    })
    .unwrap_or_else(|error| match error {});
}

fn collect_condition_reads<'dae>(
    view: dae::DaeView<'dae>,
    root: dae::ConditionId<'dae>,
    excluded: &BTreeSet<dae::ConditionId<'dae>>,
    sources: &DerivativeSources<'dae>,
    projected: &impl Fn(dae::ExprId<'dae>) -> bool,
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

/// Whole residuals whose every scalar row is already assigned to a derivative.
/// These are not additional algebraic writers merely because their opposite
/// operand is algebraic. Partial or mixed matching never proves this exclusion.
pub(super) fn complete_derivative_residuals<'dae>(
    view: dae::DaeView<'dae>,
    sources: &DerivativeSources<'dae>,
) -> BTreeSet<dae::ExprId<'dae>> {
    let mut scalars = BTreeMap::<_, BTreeSet<_>>::new();
    for source in sources.values().filter(|source| {
        source.domain_point.is_none()
            || view
                .expression(source.expression)
                .is_some_and(|node| node.binder_domain().is_none())
    }) {
        scalars
            .entry(source.expression)
            .or_default()
            .insert(source.expression_scalar);
    }
    scalars
        .into_iter()
        .filter_map(|(expression, rows)| {
            let count = view.expression(expression)?.value_type().scalar_count()?;
            (rows.len() == count && rows.iter().copied().eq(0..count)).then_some(expression)
        })
        .collect()
}

impl<'dae> SameTickDefinitions<'dae> {
    /// The complete set of coordinates one prospective producer observes at its
    /// tick, alias-followed through the exact definitions.
    ///
    /// Solve lowering uses this to decide *program granularity*: two targets
    /// may share one fused program only when neither can observe the other on
    /// the tick, so restoring fusion never reintroduces a stale same-tick read.
    pub fn read_closure(
        &self,
        view: dae::DaeView<'dae>,
        value_reads: &[dae::ExprId<'dae>],
        condition_reads: &[dae::ConditionId<'dae>],
    ) -> Result<BTreeSet<u32>, SameTickOrderError> {
        self.read_closure_with_role(view, value_reads, condition_reads, ReadRole::Current)
    }

    /// Clock-partition dependency closure: state derivatives retain SOLVE-C28.
    pub fn clock_partition_read_closure(
        &self,
        view: dae::DaeView<'dae>,
        value_reads: &[dae::ExprId<'dae>],
        condition_reads: &[dae::ConditionId<'dae>],
    ) -> Result<BTreeSet<u32>, SameTickOrderError> {
        self.read_closure_with_role(view, value_reads, condition_reads, ReadRole::ClockPartition)
    }

    fn read_closure_with_role(
        &self,
        view: dae::DaeView<'dae>,
        value_reads: &[dae::ExprId<'dae>],
        condition_reads: &[dae::ConditionId<'dae>],
        role: ReadRole,
    ) -> Result<BTreeSet<u32>, SameTickOrderError> {
        let mut direct = BTreeSet::new();
        for &value in value_reads {
            self.expression_reads_with_role(view, value, role, &mut direct)?;
        }
        for &condition in condition_reads {
            match role {
                ReadRole::Current => self.collect_condition_reads(
                    view,
                    condition,
                    &BTreeSet::new(),
                    |_| false,
                    &mut direct,
                )?,
                ReadRole::ClockPartition => {
                    collect_condition_same_instant_reads(view, condition, &mut direct)
                }
            }
        }
        self.variable_read_closure_with_role(view, direct, role)
    }

    fn expression_reads_with_role(
        &self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        role: ReadRole,
        reads: &mut BTreeSet<u32>,
    ) -> Result<(), SameTickOrderError> {
        match role {
            ReadRole::Current => self.collect_expression_reads(view, expression, reads),
            ReadRole::ClockPartition => {
                collect_same_instant_reads(view, expression, reads);
                Ok(())
            }
        }
    }

    /// Alias-followed current reads of declaration seeds.
    pub fn variable_read_closure(
        &self,
        view: dae::DaeView<'dae>,
        seeds: BTreeSet<u32>,
    ) -> Result<BTreeSet<u32>, SameTickOrderError> {
        self.variable_read_closure_with_role(view, seeds, ReadRole::Current)
    }

    /// Alias-followed entry reads, including opaque owners, at a clock boundary.
    pub fn clock_partition_variable_read_closure(
        &self,
        view: dae::DaeView<'dae>,
        seeds: BTreeSet<u32>,
    ) -> Result<BTreeSet<u32>, SameTickOrderError> {
        self.variable_read_closure_with_role(view, seeds, ReadRole::ClockPartition)
    }

    fn variable_read_closure_with_role(
        &self,
        view: dae::DaeView<'dae>,
        seeds: BTreeSet<u32>,
        role: ReadRole,
    ) -> Result<BTreeSet<u32>, SameTickOrderError> {
        let mut pending = seeds.into_iter().collect::<Vec<_>>();
        let mut closure = BTreeSet::new();
        while let Some(variable) = pending.pop() {
            if !closure.insert(variable) {
                continue;
            }
            if let Some(definition) = self.definition(variable) {
                let mut reads = BTreeSet::new();
                self.expression_reads_with_role(view, definition, role, &mut reads)?;
                pending.extend(reads);
            } else if let Some(reads) = self.opaque.get(&variable) {
                let reads = match role {
                    ReadRole::Current => &reads.current,
                    ReadRole::ClockPartition => &reads.clock_partition,
                };
                pending.extend(reads.iter().copied());
            }
        }
        Ok(closure)
    }

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
            &|_| false,
            reads,
        )
    }

    /// Current condition reads, retaining the caller's exact entry/storage exclusions.
    pub fn collect_condition_reads(
        &self,
        view: dae::DaeView<'dae>,
        root: dae::ConditionId<'dae>,
        excluded: &BTreeSet<dae::ConditionId<'dae>>,
        projected: impl Fn(dae::ExprId<'dae>) -> bool,
        reads: &mut BTreeSet<u32>,
    ) -> Result<(), SameTickOrderError> {
        collect_condition_reads(
            view,
            root,
            excluded,
            &self.derivative_sources,
            &projected,
            reads,
        )
    }
}

#[cfg(test)]
mod family_tests;
#[cfg(test)]
mod tests;
