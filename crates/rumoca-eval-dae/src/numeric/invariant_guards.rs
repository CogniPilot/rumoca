//! Constant current-value selectors for continuous equations (SPEC_0043 §4).

use super::*;
use rumoca_core::ComprehensionScalarView;

/// Prove only the indexed Boolean conditions reached from continuous owners.
/// Checked B.1c targets have disjoint coverage, including element assignments.
pub fn invariant_boolean_guards<'dae>(
    view: dae::DaeView<'dae>,
    roots: impl IntoIterator<Item = dae::ExprId<'dae>>,
) -> Vec<(dae::ExprId<'dae>, SourceValues)> {
    if view.discrete_value_owner_count() == 0 {
        return Vec::new();
    }
    let mut reads = Vec::new();
    dae::ExpressionTraversal::new().visit_pruned(view, roots, |_, node| {
        if let dae::ExpressionOperation::Conditional(operands) = node.operation()
            && let Some(conditional) = operands.conditional()
        {
            reads.extend(
                conditional
                    .branches()
                    .filter_map(|(guard, _)| guard_read(view, guard)),
            );
        }
        true
    });
    if reads.is_empty() {
        return Vec::new();
    }
    let definitions = unconditional_definitions(view);
    let mut evaluator = NumericEvaluator::new(view);
    evaluator.guard_bindings = Some(crate::projection::owner_bindings(view));
    reads
        .into_iter()
        .filter_map(|(guard, variable, scalar)| {
            let element = u32::try_from(scalar).ok()?;
            let (definition, offset) = definitions
                .get(&(variable, Some(element)))
                .map(|def| (def, 0))
                .or_else(|| definitions.get(&(variable, None)).map(|def| (def, scalar)))?;
            let values = definition.element(&mut evaluator, offset)?;
            Some((guard, values))
        })
        .collect()
}

type GuardRead<'dae> = (dae::ExprId<'dae>, u32, usize);
fn guard_read<'dae>(view: dae::DaeView<'dae>, guard: dae::ExprId<'dae>) -> Option<GuardRead<'dae>> {
    let node = view.expression(guard)?;
    let dae::ExpressionOperation::Index { base, subscripts } = node.operation() else {
        return None;
    };
    let base_node = view.expression(base)?;
    let dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteValue(variable)) =
        base_node.operation()
    else {
        return None;
    };
    let indices = crate::literal_tensor_selection(view, base, subscripts)?
        .into_iter()
        .map(|index| u32::try_from(index?.checked_sub(1)?).ok())
        .collect::<Option<Vec<_>>>()?;
    let scalar = flatten_coordinates(base_node.value_type().dimensions(), &indices)?;
    Some((guard, variable.index(), scalar))
}

struct Definition<'dae> {
    value: dae::ExprId<'dae>,
    structure: Option<dae::StructuredDiscreteValueView<'dae>>,
}

impl<'dae> Definition<'dae> {
    fn element(
        &self,
        evaluator: &mut NumericEvaluator<'dae>,
        offset: usize,
    ) -> Option<SourceValues> {
        if let Some(structure) = self.structure
            && structure.scalar_view() == ComprehensionScalarView::BinderSubstitution
        {
            let domain = structure.domain();
            let point = evaluator
                .view
                .domain(domain)?
                .structured()
                .index_tuple_at(offset)
                .ok()??;
            return evaluator
                .values_at_point(domain, point, self.value)
                .ok()
                .filter(|v| v.len() == 1);
        }
        evaluator.source_values(self.value).ok()?.slice(offset, 1)
    }
}

fn unconditional_definitions(
    view: dae::DaeView<'_>,
) -> FxHashMap<(u32, Option<u32>), Definition<'_>> {
    let mut excluded = rustc_hash::FxHashSet::default();
    for (_, transaction) in view.model_event_transactions() {
        excluded.extend(transaction.targets().map(|target| target.variable()));
    }
    for ownership in (0..view.clock_ownership_count())
        .filter_map(|i| view.clock_ownership(view.clock_ownership_id(i)?))
    {
        excluded.insert(ownership.variable().index());
    }
    let mut definitions = FxHashMap::default();
    for owner in (0..view.discrete_value_owner_count())
        .filter_map(|i| view.discrete_value_owner(view.discrete_value_owner_id(i)?))
    {
        let Some(branch) = owner.branches().get(0) else {
            continue;
        };
        if owner.branches().len() != 1
            || !matches!(branch.activation(), dae::DiscreteBranchActivation::Always)
        {
            continue;
        }
        for (target, (value, _)) in owner.targets().iter().zip(branch.values().iter()) {
            if !excluded.contains(&target.index()) && arithmetic_definition(view, value) {
                definitions.insert(
                    (target.index(), target.scalar_selection()),
                    Definition {
                        value,
                        structure: owner.structure(),
                    },
                );
            }
        }
    }
    definitions
}

/// A clock transfer, temporal operator or opaque call is not an arithmetic definition.
fn arithmetic_definition<'dae>(view: dae::DaeView<'dae>, value: dae::ExprId<'dae>) -> bool {
    let mut admitted = true;
    dae::for_each_expression_pruned(view, value, |_, node| {
        admitted &= matches!(
            node.operation(),
            dae::ExpressionOperation::Literal(_)
                | dae::ExpressionOperation::Array(_)
                | dae::ExpressionOperation::Index { .. }
                | dae::ExpressionOperation::Unary { .. }
                | dae::ExpressionOperation::Binary { .. }
                | dae::ExpressionOperation::Builtin { .. }
                | dae::ExpressionOperation::Coordinate(
                    dae::CoordinateView::Binder(_) | dae::CoordinateView::Algebraic(_)
                )
        );
        admitted
    });
    admitted
}

/// Resolve a current algebraic read solely through a closed constant owner.
pub(super) fn constant_coordinate<'dae>(
    view: dae::DaeView<'dae>,
    coordinate: dae::CoordinateView<'dae>,
    bindings: &[Option<crate::LiteralBinding<'dae>>],
) -> Option<dae::ExprId<'dae>> {
    let dae::CoordinateView::Algebraic(variable) = coordinate else {
        return None;
    };
    let value = bindings[variable.index() as usize]?.value;
    let node = view.expression(value)?;
    (node.variability() == dae::ExpressionVariability::Constant && node.binder_domain().is_none())
        .then_some(value)
}
