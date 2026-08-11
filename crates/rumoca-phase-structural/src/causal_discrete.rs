//! Shared checked orientation of causal discrete DAE owners.

use std::collections::{BTreeMap, BTreeSet};

use rumoca_core::Span;
use rumoca_ir_dae as dae;

use crate::CausalDefinitions;

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum CausalDiscreteError {
    #[error("discrete Real residual is not an explicit computable definition")]
    NonComputable { span: Span },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DiscreteRealDefinition<'dae> {
    equation: u32,
    target: dae::DiscreteRealId<'dae>,
    value: dae::ExprId<'dae>,
}

impl<'dae> DiscreteRealDefinition<'dae> {
    #[must_use]
    pub const fn equation(self) -> u32 {
        self.equation
    }

    #[must_use]
    pub const fn target(self) -> dae::DiscreteRealId<'dae> {
        self.target
    }

    #[must_use]
    pub const fn value(self) -> dae::ExprId<'dae> {
        self.value
    }
}

/// Immutable causalization evidence tied to one branded DAE view.
pub struct CausalDiscretePlan<'dae> {
    causal: CausalDefinitions<'dae>,
    discrete_real: Vec<Option<DiscreteRealDefinition<'dae>>>,
    discrete_real_order: Vec<dae::DiscreteRealId<'dae>>,
}

impl<'dae> CausalDiscretePlan<'dae> {
    pub fn derive(view: dae::DaeView<'dae>) -> Result<Self, CausalDiscreteError> {
        let causal = CausalDefinitions::derive(view);
        let discrete_real = orient_discrete_real(view, &causal)?;
        let discrete_real_order = discrete_real_order(view, &discrete_real, &causal)?;
        Ok(Self {
            causal,
            discrete_real,
            discrete_real_order,
        })
    }

    #[must_use]
    pub const fn causal_definitions(&self) -> &CausalDefinitions<'dae> {
        &self.causal
    }

    #[must_use]
    pub fn discrete_real_definition(
        &self,
        equation: usize,
    ) -> Option<DiscreteRealDefinition<'dae>> {
        self.discrete_real.get(equation).copied().flatten()
    }

    #[must_use]
    pub fn discrete_real_order(&self) -> &[dae::DiscreteRealId<'dae>] {
        &self.discrete_real_order
    }
}

type Candidate<'dae> = (dae::DiscreteRealId<'dae>, dae::ExprId<'dae>);

fn orient_discrete_real<'dae>(
    view: dae::DaeView<'dae>,
    causal: &CausalDefinitions<'dae>,
) -> Result<Vec<Option<DiscreteRealDefinition<'dae>>>, CausalDiscreteError> {
    let count = view.discrete_real_equation_count();
    let mut candidates = Vec::with_capacity(count);
    let mut spans = Vec::with_capacity(count);
    let mut resolved = vec![None; count];
    let mut pending = 0usize;
    for index in 0..count {
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        spans.push(equation.provenance().span());
        if causal.consumes_discrete_real_equation(index) {
            candidates.push(Vec::new());
        } else {
            candidates.push(definition_candidates(view, equation.residual()));
            pending += 1;
        }
    }

    let mut defined = BTreeSet::new();
    for (index, row) in candidates.iter().enumerate() {
        if let [definition] = row.as_slice() {
            defined.insert(definition.0.index());
            resolved[index] = Some(*definition);
            pending -= 1;
        }
    }
    while pending != 0 {
        let forced = force_rows(&candidates, &mut resolved, &mut defined);
        if forced == 0 {
            let unresolved = resolved
                .iter()
                .enumerate()
                .find(|(index, value)| {
                    value.is_none() && !causal.consumes_discrete_real_equation(*index)
                })
                .expect("pending orientation has an unresolved row")
                .0;
            return Err(CausalDiscreteError::NonComputable {
                span: spans[unresolved],
            });
        }
        pending -= forced;
    }

    Ok(resolved
        .into_iter()
        .enumerate()
        .map(|(equation, definition)| {
            definition.map(|(target, value)| DiscreteRealDefinition {
                equation: u32::try_from(equation)
                    .expect("checked DAE equation capacity is bounded by u32"),
                target,
                value,
            })
        })
        .collect())
}

fn force_rows<'dae>(
    candidates: &[Vec<Candidate<'dae>>],
    resolved: &mut [Option<Candidate<'dae>>],
    defined: &mut BTreeSet<u32>,
) -> usize {
    let mut forced = 0;
    for (index, row) in candidates.iter().enumerate() {
        let mut open = row
            .iter()
            .filter(|(target, _)| !defined.contains(&target.index()));
        let (None, Some(definition), None) = (resolved[index], open.next(), open.next()) else {
            continue;
        };
        defined.insert(definition.0.index());
        resolved[index] = Some(*definition);
        forced += 1;
    }
    forced
}

fn discrete_real_order<'dae>(
    view: dae::DaeView<'dae>,
    definitions: &[Option<DiscreteRealDefinition<'dae>>],
    causal: &CausalDefinitions<'dae>,
) -> Result<Vec<dae::DiscreteRealId<'dae>>, CausalDiscreteError> {
    let targets = definitions
        .iter()
        .flatten()
        .map(|definition| definition.target.index())
        .collect::<BTreeSet<_>>();
    let mut dependencies = targets
        .iter()
        .map(|target| (*target, BTreeSet::new()))
        .collect::<BTreeMap<_, _>>();
    for definition in definitions.iter().flatten() {
        collect_current_discrete_reads(view, definition.value, |dependency| {
            if dependency != definition.target.index() && targets.contains(&dependency) {
                dependencies
                    .get_mut(&definition.target.index())
                    .expect("definition target owns a dependency row")
                    .insert(dependency);
            }
        });
    }

    let mut emitted = BTreeSet::new();
    let mut order = Vec::with_capacity(targets.len());
    while order.len() != targets.len() {
        let Some(target) = targets.iter().copied().find(|target| {
            !emitted.contains(target)
                && dependencies[target]
                    .iter()
                    .all(|dependency| emitted.contains(dependency))
        }) else {
            let blocked = definitions
                .iter()
                .flatten()
                .find(|definition| !emitted.contains(&definition.target.index()))
                .expect("a cyclic dependency graph has a blocked definition");
            let equation =
                usize::try_from(blocked.equation).expect("checked equation ordinal fits usize");
            let span = view
                .discrete_real_equation(equation)
                .expect("checked discrete Real definition resolves")
                .provenance()
                .span();
            return Err(CausalDiscreteError::NonComputable { span });
        };
        emitted.insert(target);
        order.push(
            definitions
                .iter()
                .flatten()
                .find(|definition| definition.target.index() == target)
                .expect("scheduled target has a definition")
                .target,
        );
    }

    debug_assert!(
        definitions
            .iter()
            .enumerate()
            .all(|(index, definition)| definition.is_some()
                || causal.consumes_discrete_real_equation(index))
    );
    Ok(order)
}

fn collect_current_discrete_reads<'dae>(
    view: dae::DaeView<'dae>,
    value: dae::ExprId<'dae>,
    mut read: impl FnMut(u32),
) {
    dae::for_each_expression(view, value, |_, expression| {
        if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(value)) =
            expression.operation()
        {
            read(value.index());
        }
    });
}

fn definition_candidates<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Vec<Candidate<'dae>> {
    let Some(residual) = view.expression(residual) else {
        return Vec::new();
    };
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = residual.operation()
    else {
        return Vec::new();
    };
    [(lhs, rhs), (rhs, lhs)]
        .into_iter()
        .filter_map(|(side, value)| {
            compatible_definition(view, whole_discrete_real(view, side)?, value)
        })
        .collect()
}

fn compatible_definition<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::DiscreteRealId<'dae>,
    value: dae::ExprId<'dae>,
) -> Option<Candidate<'dae>> {
    let variable = view.variable(dae::VariableId::from(target))?;
    let expression = view.expression(value)?;
    (variable.value_type().dimensions() == expression.value_type().dimensions()
        && variable.value_type().scalar_type() == dae::ScalarType::Real
        && matches!(
            expression.value_type().scalar_type(),
            dae::ScalarType::Real | dae::ScalarType::Integer
        )
        && !reads_current_target(view, value, target))
    .then_some((target, value))
}

fn reads_current_target<'dae>(
    view: dae::DaeView<'dae>,
    value: dae::ExprId<'dae>,
    target: dae::DiscreteRealId<'dae>,
) -> bool {
    let mut found = false;
    dae::for_each_expression(view, value, |_, expression| {
        found |= matches!(
            expression.operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(candidate))
                if candidate == target
        );
    });
    found
}

fn whole_discrete_real<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<dae::DiscreteRealId<'dae>> {
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(variable)) => {
            Some(variable)
        }
        _ => None,
    }
}
