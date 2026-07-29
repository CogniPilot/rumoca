//! Proof that a subset of scalar/array residual owners are acyclic algebraic
//! definitions.
//!
//! A candidate must have the exact checked form `target - value = 0` or
//! `value - target = 0`, with one whole algebraic coordinate on exactly one
//! side. Ambiguous aliases, duplicate targets, self-dependencies, structured
//! families, and cycles remain residual owners.

use std::collections::{HashMap, HashSet};

use rumoca_ir_dae as dae;

/// Immutable elimination evidence tied to one branded DAE view.
pub struct CausalDefinitions<'dae> {
    definitions: HashMap<u32, dae::ExprId<'dae>>,
    consumed_equations: HashSet<u32>,
    order: Vec<dae::AlgebraicId<'dae>>,
    remaining_owners: usize,
}

impl<'dae> CausalDefinitions<'dae> {
    pub fn derive(view: dae::DaeView<'dae>) -> Self {
        let mut candidates = Vec::new();
        let mut target_counts = HashMap::<u32, usize>::new();
        let mut total_owners = 0usize;
        for owner in view.continuous_owners() {
            total_owners += 1;
            let dae::ContinuousOwnerView::Residual { id, equation } = owner else {
                continue;
            };
            let Some((target, value)) = direct_definition(view, equation.residual()) else {
                continue;
            };
            if expression_references(view, value, target) {
                continue;
            }
            *target_counts.entry(target.index()).or_default() += 1;
            candidates.push((id, target, value));
        }
        candidates.retain(|(_, target, _)| target_counts[&target.index()] == 1);

        let candidate_targets = candidates
            .iter()
            .map(|(_, target, _)| target.index())
            .collect::<HashSet<_>>();
        let dependencies = candidates
            .iter()
            .map(|(_, target, value)| {
                (
                    target.index(),
                    algebraic_dependencies(view, *value)
                        .into_iter()
                        .filter(|dependency| candidate_targets.contains(dependency))
                        .collect::<HashSet<_>>(),
                )
            })
            .collect::<HashMap<_, _>>();
        let mut emitted = HashSet::new();
        let mut order = Vec::with_capacity(candidates.len());
        while order.len() < candidates.len() {
            let Some((_, target, _)) = candidates.iter().find(|(_, target, _)| {
                !emitted.contains(&target.index())
                    && dependencies[&target.index()]
                        .iter()
                        .all(|dependency| emitted.contains(dependency))
            }) else {
                break;
            };
            emitted.insert(target.index());
            order.push(*target);
        }

        let definitions = candidates
            .iter()
            .filter(|(_, target, _)| emitted.contains(&target.index()))
            .map(|(_, target, value)| (target.index(), *value))
            .collect();
        let consumed_equations = candidates
            .iter()
            .filter(|(_, target, _)| emitted.contains(&target.index()))
            .map(|(equation, _, _)| equation.index())
            .collect::<HashSet<_>>();
        Self {
            definitions,
            remaining_owners: total_owners - consumed_equations.len(),
            consumed_equations,
            order,
        }
    }

    pub fn definition(&self, variable: dae::AlgebraicId<'dae>) -> Option<dae::ExprId<'dae>> {
        self.definitions.get(&variable.index()).copied()
    }

    pub fn definition_for_variable(
        &self,
        variable: dae::VariableId<'dae>,
    ) -> Option<dae::ExprId<'dae>> {
        self.definitions.get(&variable.index()).copied()
    }

    pub fn consumes(&self, equation: dae::ContinuousEquationId<'dae>) -> bool {
        self.consumed_equations.contains(&equation.index())
    }

    pub fn order(&self) -> &[dae::AlgebraicId<'dae>] {
        &self.order
    }

    pub const fn remaining_owner_count(&self) -> usize {
        self.remaining_owners
    }
}

fn direct_definition<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Option<(dae::AlgebraicId<'dae>, dae::ExprId<'dae>)> {
    let residual = view.expression(residual)?;
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = residual.operation()
    else {
        return None;
    };
    match (whole_algebraic(view, lhs), whole_algebraic(view, rhs)) {
        (Some(target), None) => compatible_definition(view, target, rhs),
        (None, Some(target)) => compatible_definition(view, target, lhs),
        (None, None) | (Some(_), Some(_)) => None,
    }
}

fn compatible_definition<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::AlgebraicId<'dae>,
    value: dae::ExprId<'dae>,
) -> Option<(dae::AlgebraicId<'dae>, dae::ExprId<'dae>)> {
    let variable = view.variable(dae::VariableId::from(target))?;
    let expression = view.expression(value)?;
    (variable.value_type() == expression.value_type()).then_some((target, value))
}

fn whole_algebraic<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<dae::AlgebraicId<'dae>> {
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable)) => {
            Some(variable)
        }
        _ => None,
    }
}

fn expression_references<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    target: dae::AlgebraicId<'dae>,
) -> bool {
    let mut found = false;
    dae::for_each_expression(view, expression, |_, node| {
        found |= matches!(
            node.operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable))
                if variable.index() == target.index()
        );
    });
    found
}

fn algebraic_dependencies<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> HashSet<u32> {
    let mut dependencies = HashSet::new();
    dae::for_each_expression(view, expression, |_, node| {
        if let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable)) =
            node.operation()
        {
            dependencies.insert(variable.index());
        }
    });
    dependencies
}

#[cfg(test)]
mod tests {
    use rumoca_core::{SourceMap, Span, TypeId, VarName};

    use super::*;

    #[derive(Clone, Copy)]
    enum Fixture {
        Direct,
        Duplicate,
        Cycle,
    }

    fn fixture_residuals<'dae>(
        expressions: &mut dae::Expressions<'_, 'dae>,
        fixture: Fixture,
        x: dae::AlgebraicId<'dae>,
        y: dae::AlgebraicId<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
        let x_value = expressions
            .at(provenance)
            .coordinate(dae::CoordinateInput::Algebraic(x))?;
        let y_value = expressions
            .at(provenance)
            .coordinate(dae::CoordinateInput::Algebraic(y))?;
        let one = expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Real(1.0))?;
        let two = expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Real(2.0))?;
        match fixture {
            Fixture::Direct => Ok(vec![expressions.at(provenance).binary(
                dae::BinaryOperator::Subtract,
                x_value,
                one,
            )?]),
            Fixture::Duplicate => Ok(vec![
                expressions
                    .at(provenance)
                    .binary(dae::BinaryOperator::Subtract, x_value, one)?,
                expressions
                    .at(provenance)
                    .binary(dae::BinaryOperator::Subtract, x_value, two)?,
            ]),
            Fixture::Cycle => cyclic_residuals(expressions, x_value, y_value, one, provenance),
        }
    }

    fn cyclic_residuals<'dae>(
        expressions: &mut dae::Expressions<'_, 'dae>,
        x: dae::ExprId<'dae>,
        y: dae::ExprId<'dae>,
        one: dae::ExprId<'dae>,
        provenance: dae::DaeProvenance,
    ) -> Result<Vec<dae::ExprId<'dae>>, dae::DaeConstructionError> {
        let y_plus_one = expressions
            .at(provenance)
            .binary(dae::BinaryOperator::Add, y, one)?;
        let x_plus_one = expressions
            .at(provenance)
            .binary(dae::BinaryOperator::Add, x, one)?;
        Ok(vec![
            expressions
                .at(provenance)
                .binary(dae::BinaryOperator::Subtract, x, y_plus_one)?,
            expressions
                .at(provenance)
                .binary(dae::BinaryOperator::Subtract, y, x_plus_one)?,
        ])
    }

    fn add_residuals<'dae>(
        continuous: &mut dae::ContinuousEquations<'_, 'dae>,
        residuals: Vec<dae::ExprId<'dae>>,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        for residual in residuals {
            continuous.equation(provenance, |equation| equation.residual(residual))?;
        }
        Ok(())
    }

    fn model(fixture: Fixture) -> dae::Dae {
        let mut sources = SourceMap::new();
        let text = "Real x; Real y; equation definitions;";
        let source = sources.add("definitions.mo", text);
        let span = Span::from_offsets(source, 0, text.len());
        let provenance = dae::DaeProvenance::source(span).unwrap();
        dae::Dae::construct(sources, |dae| {
            let real = dae.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    provenance,
                )
            })?;
            let (x, y) = dae.variables(|variables| {
                Ok((
                    variables.algebraic(
                        VarName::new("x"),
                        real,
                        provenance,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.algebraic(
                        VarName::new("y"),
                        real,
                        provenance,
                        dae::VariableAttributes::default(),
                    )?,
                ))
            })?;
            let residuals = dae.expressions(|expressions| {
                fixture_residuals(expressions, fixture, x, y, provenance)
            })?;
            dae.continuous(|continuous| add_residuals(continuous, residuals, provenance))
        })
        .unwrap()
    }

    #[test]
    fn exact_direct_definition_has_elimination_evidence() {
        model(Fixture::Direct).inspect(|view| {
            let proof = CausalDefinitions::derive(view);
            assert_eq!(proof.order().len(), 1);
            assert_eq!(proof.remaining_owner_count(), 0);
            let target = view
                .variable(proof.order()[0].into())
                .expect("proof target is branded to this DAE");
            assert_eq!(target.name().as_str(), "x");
            let dae::ContinuousOwnerView::Residual { id, .. } =
                view.continuous_owners().next().unwrap()
            else {
                panic!("fixture contains one residual owner");
            };
            assert!(proof.consumes(id));
        });
    }

    #[test]
    fn duplicate_target_definitions_remain_residual() {
        model(Fixture::Duplicate).inspect(|view| {
            let proof = CausalDefinitions::derive(view);
            assert!(proof.order().is_empty());
            assert_eq!(proof.remaining_owner_count(), 2);
            let retained = view
                .continuous_owners()
                .filter_map(|owner| match owner {
                    dae::ContinuousOwnerView::Residual { id, .. } => Some(id),
                    dae::ContinuousOwnerView::Structured { .. } => None,
                })
                .filter(|id| !proof.consumes(*id))
                .count();
            assert_eq!(retained, 2);
        });
    }

    #[test]
    fn cyclic_definitions_remain_residual() {
        model(Fixture::Cycle).inspect(|view| {
            let proof = CausalDefinitions::derive(view);
            assert!(proof.order().is_empty());
            assert_eq!(proof.remaining_owner_count(), 2);
        });
    }
}
