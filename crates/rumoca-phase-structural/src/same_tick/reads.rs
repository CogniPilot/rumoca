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

#[derive(Default)]
pub(super) struct ReadSeeds {
    variables: BTreeSet<u32>,
    raw: BTreeSet<u32>,
    aliases: BTreeMap<u32, BTreeSet<u32>>,
}

impl ReadSeeds {
    pub(super) fn raw(variables: BTreeSet<u32>) -> Self {
        Self {
            raw: variables.clone(),
            variables,
            aliases: BTreeMap::new(),
        }
    }

    pub(super) fn merge(&mut self, other: Self) -> Vec<u32> {
        let mut changed = other.variables.iter().copied().collect::<Vec<_>>();
        changed.extend(other.raw.iter().copied());
        self.variables.extend(other.variables);
        self.raw.extend(other.raw);
        for (variable, reads) in other.aliases {
            let entry = self.aliases.entry(variable).or_default();
            let previous = entry.len();
            entry.extend(reads);
            if entry.len() != previous {
                changed.push(variable);
            }
        }
        changed
    }
}

pub(super) fn collect_expression_reads<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    sources: &DerivativeSources<'dae>,
    projected: &impl Fn(dae::ExprId<'dae>) -> bool,
    reads: &mut BTreeSet<u32>,
) -> Result<(), SameTickOrderError> {
    reads.extend(expression_read_seeds(view, expression, sources, None, projected)?.variables);
    Ok(())
}

pub(super) fn expression_read_seeds<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    sources: &DerivativeSources<'dae>,
    definitions: Option<&SameTickDefinitions<'dae>>,
    projected: &impl Fn(dae::ExprId<'dae>) -> bool,
) -> Result<ReadSeeds, SameTickOrderError> {
    if sources.is_empty() || !has_derivative_demand(view, expression, projected) {
        let mut reads = BTreeSet::new();
        collect_raw_reads(view, expression, projected, &mut reads);
        return Ok(ReadSeeds::raw(reads));
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
        .map(|scalar| (expression, scalar, None, None))
        .collect::<Vec<_>>();
    let mut seen = BTreeSet::new();
    let mut reads = ReadSeeds::default();
    while let Some((root, scalar, point, parent)) = pending.pop() {
        if !seen.insert((root, scalar, point.clone(), parent)) {
            continue;
        }
        let mut coordinates = Vec::new();
        projection.project(
            root,
            scalar,
            point
                .as_ref()
                .map(|(domain, indices): &(dae::DomainId<'dae>, Vec<i64>)| {
                    (*domain, indices.as_slice())
                }),
            |coordinate, scalar| coordinates.push((coordinate, scalar)),
        )?;
        let mut derivatives = Vec::new();
        for (coordinate, scalar) in coordinates {
            if let dae::CoordinateView::Derivative(state) = coordinate {
                derivatives.push((state.index(), scalar));
                continue;
            }
            if let Some((variable, definition)) =
                record_projected_read(&mut reads, definitions, coordinate, parent)
            {
                pending.push((definition, scalar, point.clone(), Some(variable)));
            }
        }
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
                parent,
            ));
        }
    }
    Ok(reads)
}

fn record_projected_read<'dae>(
    reads: &mut ReadSeeds,
    definitions: Option<&SameTickDefinitions<'dae>>,
    coordinate: dae::CoordinateView<'dae>,
    parent: Option<u32>,
) -> Option<(u32, dae::ExprId<'dae>)> {
    let variable = current_variable(dae::ExpressionOperation::Coordinate(coordinate))?.index();
    reads.variables.insert(variable);
    if let Some(parent) = parent {
        reads.aliases.entry(parent).or_default().insert(variable);
    }
    if let dae::CoordinateView::Algebraic(_) = coordinate
        && let Some(definition) = definitions.and_then(|owner| owner.definition(variable))
    {
        reads.aliases.entry(variable).or_default();
        return Some((variable, definition));
    }
    reads.raw.insert(variable);
    None
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
pub(super) fn collect_condition_same_instant_reads_except<'dae>(
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
pub(super) fn collect_condition_same_instant_reads_pruned<'dae>(
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

pub(super) fn visit_condition_expressions<'dae, E>(
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
        let mut seeds = ReadSeeds::default();
        for &value in value_reads {
            seeds.merge(self.expression_seeds_with_role(view, value, role)?);
        }
        for &condition in condition_reads {
            seeds.merge(self.condition_seeds_with_role(view, condition, role)?);
        }
        self.visit_read_seed_closure(view, seeds, role, |_, _| {})
    }

    fn condition_seeds_with_role(
        &self,
        view: dae::DaeView<'dae>,
        condition: dae::ConditionId<'dae>,
        role: ReadRole,
    ) -> Result<ReadSeeds, SameTickOrderError> {
        let mut seeds = ReadSeeds::default();
        match role {
            ReadRole::Current => {
                visit_condition_expressions(view, condition, &BTreeSet::new(), |expression| {
                    seeds.merge(self.expression_seeds_with_role(view, expression, role)?);
                    Ok::<(), SameTickOrderError>(())
                })?;
            }
            ReadRole::ClockPartition => {
                let mut reads = BTreeSet::new();
                collect_condition_same_instant_reads(view, condition, &mut reads);
                seeds.merge(ReadSeeds::raw(reads));
            }
        }
        Ok(seeds)
    }

    fn expression_seeds_with_role(
        &self,
        view: dae::DaeView<'dae>,
        expression: dae::ExprId<'dae>,
        role: ReadRole,
    ) -> Result<ReadSeeds, SameTickOrderError> {
        match role {
            ReadRole::Current => expression_read_seeds(
                view,
                expression,
                &self.derivative_sources,
                Some(self),
                &|_| false,
            ),
            ReadRole::ClockPartition => {
                let mut reads = BTreeSet::new();
                collect_same_instant_reads(view, expression, &mut reads);
                Ok(ReadSeeds::raw(reads))
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
        self.visit_variable_read_closure(view, seeds, role, |_, _| {})
    }

    pub(super) fn visit_variable_read_closure(
        &self,
        view: dae::DaeView<'dae>,
        seeds: BTreeSet<u32>,
        role: ReadRole,
        visit: impl FnMut(u32, &BTreeSet<u32>),
    ) -> Result<BTreeSet<u32>, SameTickOrderError> {
        self.visit_read_seed_closure(view, ReadSeeds::raw(seeds), role, visit)
    }

    pub(super) fn visit_read_seed_closure(
        &self,
        view: dae::DaeView<'dae>,
        mut seeds: ReadSeeds,
        role: ReadRole,
        mut visit: impl FnMut(u32, &BTreeSet<u32>),
    ) -> Result<BTreeSet<u32>, SameTickOrderError> {
        let mut pending = seeds.variables.iter().copied().collect::<Vec<_>>();
        let mut processed = BTreeMap::new();
        let mut closure = BTreeSet::new();
        while let Some(variable) = pending.pop() {
            let projected = (!seeds.raw.contains(&variable))
                .then(|| seeds.aliases.get(&variable))
                .flatten()
                .cloned();
            if processed.get(&variable) == Some(&projected) {
                continue;
            }
            processed.insert(variable, projected.clone());
            closure.insert(variable);
            if let Some(reads) = projected {
                visit(variable, &reads);
                pending.extend(reads);
            } else if let Some(definition) = self.definition(variable) {
                let next = self.expression_seeds_with_role(view, definition, role)?;
                visit(variable, &next.variables);
                pending.extend(seeds.merge(next));
            } else if let Some(reads) = self.opaque.get(&variable) {
                let reads = match role {
                    ReadRole::Current => &reads.current,
                    ReadRole::ClockPartition => &reads.clock_partition,
                };
                visit(variable, reads);
                pending.extend(seeds.merge(ReadSeeds::raw(reads.clone())));
            }
        }
        Ok(closure)
    }
}

#[cfg(test)]
mod alias_merge_tests;
#[cfg(test)]
mod family_tests;
#[cfg(test)]
mod tests;
