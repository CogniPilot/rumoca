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
    scalar_definitions: HashMap<(u32, u32), dae::ExprId<'dae>>,
    fully_scalar_defined: HashSet<u32>,
    consumed_equations: HashSet<u32>,
    consumed_families: HashSet<u32>,
    order: Vec<dae::AlgebraicId<'dae>>,
    remaining_owners: usize,
}

impl<'dae> CausalDefinitions<'dae> {
    pub fn derive(view: dae::DaeView<'dae>) -> Self {
        let mut candidates = Vec::new();
        let mut aliases = Vec::new();
        let mut target_counts = HashMap::<u32, usize>::new();
        let mut total_owners = 0usize;
        for owner in view.continuous_owners() {
            total_owners += 1;
            let Some((owner, residual)) = definition_owner_residual(view, owner) else {
                continue;
            };
            if let Some(alias) = direct_alias(view, residual) {
                aliases.push((owner, alias));
                continue;
            }
            let Some((target, value)) = direct_definition(view, residual) else {
                continue;
            };
            if expression_references(view, value, target) {
                continue;
            }
            *target_counts.entry(target.index()).or_default() += 1;
            candidates.push((owner, target, value));
        }
        candidates.retain(|(_, target, _)| target_counts[&target.index()] == 1);
        append_oriented_aliases(&mut candidates, aliases);

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

        let definitions: HashMap<_, _> = candidates
            .iter()
            .filter(|(_, target, _)| emitted.contains(&target.index()))
            .map(|(_, target, value)| (target.index(), *value))
            .collect();
        let mut consumed_equations = candidates
            .iter()
            .filter(|(_, target, _)| emitted.contains(&target.index()))
            .filter_map(|(owner, _, _)| match owner {
                DefinitionOwner::Residual(equation) => Some(*equation),
                DefinitionOwner::Structured(_) => None,
            })
            .collect::<HashSet<_>>();
        let consumed_families = candidates
            .iter()
            .filter(|(_, target, _)| emitted.contains(&target.index()))
            .filter_map(|(owner, _, _)| match owner {
                DefinitionOwner::Structured(family) => Some(*family),
                DefinitionOwner::Residual(_) => None,
            })
            .collect::<HashSet<_>>();
        let (scalar_definitions, fully_scalar_defined, scalar_equations) =
            derive_scalar_definitions(view, &definitions, &consumed_equations);
        consumed_equations.extend(scalar_equations);
        Self {
            definitions,
            scalar_definitions,
            fully_scalar_defined,
            remaining_owners: total_owners - consumed_equations.len() - consumed_families.len(),
            consumed_equations,
            consumed_families,
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

    /// Exact scalar boundary definition for one array coordinate.
    pub fn scalar_definition_for_variable(
        &self,
        variable: dae::VariableId<'dae>,
        scalar: u32,
    ) -> Option<dae::ExprId<'dae>> {
        self.scalar_definitions
            .get(&(variable.index(), scalar))
            .copied()
    }

    /// Whether every scalar of this aggregate has one non-cyclic definition.
    pub fn fully_defines_variable(&self, variable: dae::VariableId<'dae>) -> bool {
        self.fully_scalar_defined.contains(&variable.index())
    }

    pub fn consumes(&self, equation: dae::ContinuousEquationId<'dae>) -> bool {
        self.consumed_equations.contains(&equation.index())
    }

    pub fn consumes_family(&self, family: dae::ContinuousFamilyId<'dae>) -> bool {
        self.consumed_families.contains(&family.index())
    }

    pub fn order(&self) -> &[dae::AlgebraicId<'dae>] {
        &self.order
    }

    pub const fn remaining_owner_count(&self) -> usize {
        self.remaining_owners
    }
}

fn definition_owner_residual<'dae>(
    view: dae::DaeView<'dae>,
    owner: dae::ContinuousOwnerView<'dae>,
) -> Option<(DefinitionOwner, dae::ExprId<'dae>)> {
    match owner {
        dae::ContinuousOwnerView::Residual { id, equation } => {
            Some((DefinitionOwner::Residual(id.index()), equation.residual()))
        }
        dae::ContinuousOwnerView::Structured { id, family } => {
            exact_row_major_family_body(view, family)
                .map(|residual| (DefinitionOwner::Structured(id.index()), residual))
        }
    }
}

#[derive(Clone, Copy)]
enum DefinitionOwner {
    Residual(u32),
    Structured(u32),
}

type DefinitionCandidate<'dae> = (DefinitionOwner, dae::AlgebraicId<'dae>, dae::ExprId<'dae>);

type AliasCandidate<'dae> = (
    DefinitionOwner,
    (
        dae::AlgebraicId<'dae>,
        dae::ExprId<'dae>,
        dae::AlgebraicId<'dae>,
        dae::ExprId<'dae>,
    ),
);

fn exact_row_major_family_body<'dae>(
    view: dae::DaeView<'dae>,
    family: dae::StructuredFamilyView<'dae>,
) -> Option<dae::ExprId<'dae>> {
    if family.scalar_view() != rumoca_core::ComprehensionScalarView::RowMajorProjection {
        return None;
    }
    let mut bodies = family.bodies().iter();
    let body = bodies.next()?;
    if bodies.next().is_some() || expression_references_binder(view, body) {
        return None;
    }
    Some(body)
}

fn expression_references_binder<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    let mut found = false;
    dae::for_each_expression(view, expression, |_, node| {
        found |= matches!(
            node.operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(_))
        );
    });
    found
}

/// Orient exact `x - y = 0` aliases only while constructing a unique target
/// graph. Existing definitions seed the direction; an unseeded component gets
/// one deterministic root, and the later dependency proof rejects cycles.
fn append_oriented_aliases<'dae>(
    candidates: &mut Vec<DefinitionCandidate<'dae>>,
    mut aliases: Vec<AliasCandidate<'dae>>,
) {
    let mut claimed = candidates
        .iter()
        .map(|(_, target, _)| target.index())
        .collect::<HashSet<_>>();
    while !aliases.is_empty() {
        let oriented = aliases.iter().position(|(_, (lhs, _, rhs, _))| {
            claimed.contains(&lhs.index()) != claimed.contains(&rhs.index())
        });
        let index = oriented.unwrap_or_else(|| {
            aliases
                .iter()
                .position(|(_, (lhs, _, rhs, _))| {
                    !claimed.contains(&lhs.index()) && !claimed.contains(&rhs.index())
                })
                .unwrap_or(0)
        });
        let (equation, (lhs, lhs_expression, rhs, rhs_expression)) = aliases.swap_remove(index);
        let lhs_claimed = claimed.contains(&lhs.index());
        let rhs_claimed = claimed.contains(&rhs.index());
        let candidate = match (lhs_claimed, rhs_claimed) {
            (true, false) => Some((equation, rhs, lhs_expression)),
            (false, true) | (false, false) => Some((equation, lhs, rhs_expression)),
            (true, true) => None,
        };
        if let Some(candidate) = candidate {
            claimed.insert(candidate.1.index());
            candidates.push(candidate);
        }
    }
}

type ScalarDefinitionMap<'dae> = HashMap<(u32, u32), dae::ExprId<'dae>>;

fn derive_scalar_definitions<'dae>(
    view: dae::DaeView<'dae>,
    whole: &HashMap<u32, dae::ExprId<'dae>>,
    already_consumed: &HashSet<u32>,
) -> (ScalarDefinitionMap<'dae>, HashSet<u32>, HashSet<u32>) {
    let mut candidates = Vec::new();
    let mut counts = HashMap::<(u32, u32), usize>::new();
    for owner in view.continuous_owners() {
        let dae::ContinuousOwnerView::Residual { id, equation } = owner else {
            continue;
        };
        if already_consumed.contains(&id.index()) {
            continue;
        }
        let Some((variable, scalar, value)) = scalar_direct_definition(view, equation.residual())
        else {
            continue;
        };
        if whole.contains_key(&variable.index()) || expression_references_algebraic(view, value) {
            continue;
        }
        *counts.entry((variable.index(), scalar)).or_default() += 1;
        candidates.push((id, variable, scalar, value));
    }
    candidates.retain(|(_, variable, scalar, _)| counts[&(variable.index(), *scalar)] == 1);
    let candidate_definitions = candidates
        .iter()
        .map(|(_, variable, scalar, value)| ((variable.index(), *scalar), *value))
        .collect::<HashMap<_, _>>();
    let fully_defined = view
        .variables()
        .filter(|(variable, entry)| {
            let Ok(scalar_count) = u32::try_from(entry.scalar_count()) else {
                return false;
            };
            entry.role() == dae::VariableRole::Algebraic
                && scalar_count > 0
                && (0..scalar_count)
                    .all(|scalar| candidate_definitions.contains_key(&(variable.index(), scalar)))
        })
        .map(|(variable, _)| variable.index())
        .collect::<HashSet<_>>();
    let definitions = candidate_definitions
        .into_iter()
        .filter(|((variable, _), _)| fully_defined.contains(variable))
        .collect();
    let consumed = candidates
        .iter()
        .filter(|(_, variable, _, _)| fully_defined.contains(&variable.index()))
        .map(|(equation, _, _, _)| equation.index())
        .collect::<HashSet<_>>();
    (definitions, fully_defined, consumed)
}

fn scalar_direct_definition<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Option<(dae::AlgebraicId<'dae>, u32, dae::ExprId<'dae>)> {
    let residual = view.expression(residual)?;
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = residual.operation()
    else {
        return None;
    };
    match (scalar_algebraic(view, lhs), scalar_algebraic(view, rhs)) {
        (Some((target, scalar)), None) => scalar_compatible_definition(view, target, scalar, rhs),
        (None, Some((target, scalar))) => scalar_compatible_definition(view, target, scalar, lhs),
        _ => None,
    }
}

fn scalar_compatible_definition<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::AlgebraicId<'dae>,
    scalar: u32,
    value: dae::ExprId<'dae>,
) -> Option<(dae::AlgebraicId<'dae>, u32, dae::ExprId<'dae>)> {
    let variable = view.variable(dae::VariableId::from(target))?;
    let value_type = view.expression(value)?.value_type();
    (value_type.is_scalar() && variable.value_type().scalar_type() == value_type.scalar_type())
        .then_some((target, scalar, value))
}

fn scalar_algebraic<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<(dae::AlgebraicId<'dae>, u32)> {
    let node = view.expression(expression)?;
    let dae::ExpressionOperation::Index { base, subscripts } = node.operation() else {
        return None;
    };
    let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable)) =
        view.expression(base)?.operation()
    else {
        return None;
    };
    let dimensions = view
        .variable(dae::VariableId::from(variable))?
        .value_type()
        .dimensions();
    if dimensions.len() != subscripts.len() {
        return None;
    }
    let mut scalar = 0_u32;
    for (subscript, extent) in subscripts.iter().zip(dimensions) {
        let dae::SubscriptView::Index { expression, .. } = subscript else {
            return None;
        };
        let dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(index)) =
            view.expression(expression)?.operation()
        else {
            return None;
        };
        let coordinate = u32::try_from(*index).ok()?.checked_sub(1)?;
        if coordinate >= *extent {
            return None;
        }
        scalar = scalar.checked_mul(*extent)?.checked_add(coordinate)?;
    }
    Some((variable, scalar))
}

fn expression_references_algebraic<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    let mut found = false;
    dae::for_each_expression(view, expression, |_, node| {
        found |= matches!(
            node.operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(_))
        );
    });
    found
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

fn direct_alias<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Option<(
    dae::AlgebraicId<'dae>,
    dae::ExprId<'dae>,
    dae::AlgebraicId<'dae>,
    dae::ExprId<'dae>,
)> {
    let residual = view.expression(residual)?;
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = residual.operation()
    else {
        return None;
    };
    let lhs_target = whole_algebraic(view, lhs)?;
    let rhs_target = whole_algebraic(view, rhs)?;
    (lhs_target != rhs_target).then_some((lhs_target, lhs, rhs_target, rhs))
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
    use rumoca_core::{
        SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, TypeId, VarName,
    };

    use super::*;

    #[derive(Clone, Copy)]
    enum Fixture {
        Direct,
        AliasChain,
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
            Fixture::AliasChain => Ok(vec![
                expressions
                    .at(provenance)
                    .binary(dae::BinaryOperator::Subtract, x_value, one)?,
                expressions.at(provenance).binary(
                    dae::BinaryOperator::Subtract,
                    y_value,
                    x_value,
                )?,
            ]),
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

    fn indexed_array_model(complete: bool) -> dae::Dae {
        let mut sources = SourceMap::new();
        let text = "input Real u[2]; Real x[2]; equation x[1] = u[1]; x[2] = u[2];";
        let source = sources.add("indexed-definitions.mo", text);
        let span = Span::from_offsets(source, 0, text.len());
        let provenance = dae::DaeProvenance::source(span).unwrap();
        let coordinate_count = i64::from(complete) + 1;
        dae::Dae::construct(sources, |dae| {
            let array = dae.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [2]),
                    provenance,
                )
            })?;
            let (u, x) = dae.variables(|variables| {
                Ok((
                    variables.input(
                        VarName::new("u"),
                        array,
                        dae::InputVariability::Continuous,
                        provenance,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.algebraic(
                        VarName::new("x"),
                        array,
                        provenance,
                        dae::VariableAttributes::default(),
                    )?,
                ))
            })?;
            let residuals = dae.expressions(|expressions| {
                let u = expressions
                    .at(provenance)
                    .coordinate(dae::CoordinateInput::Input(u))?;
                let x = expressions
                    .at(provenance)
                    .coordinate(dae::CoordinateInput::Algebraic(x))?;
                (1..=coordinate_count)
                    .map(|coordinate| indexed_residual(expressions, x, u, coordinate, provenance))
                    .collect::<Result<Vec<_>, _>>()
            })?;
            dae.continuous(|continuous| add_residuals(continuous, residuals, provenance))
        })
        .unwrap()
    }

    fn structured_array_model() -> dae::Dae {
        let mut sources = SourceMap::new();
        let text = "input Real u[2]; Real x[2]; equation x = u;";
        let source = sources.add("structured-definitions.mo", text);
        let span = Span::from_offsets(source, 0, text.len());
        let provenance = dae::DaeProvenance::source(span).unwrap();
        dae::Dae::construct(sources, |dae| {
            let array = dae.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [2]),
                    provenance,
                )
            })?;
            let domain = dae.domains(|domains| {
                domains.structured(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            id: 0,
                            display_name: "i".to_owned(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    provenance,
                )
            })?;
            let (u, x) = dae.variables(|variables| {
                Ok((
                    variables.input(
                        VarName::new("u"),
                        array,
                        dae::InputVariability::Continuous,
                        provenance,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.algebraic(
                        VarName::new("x"),
                        array,
                        provenance,
                        dae::VariableAttributes::default(),
                    )?,
                ))
            })?;
            let residual = dae.expressions(|expressions| {
                let u = expressions
                    .at(provenance)
                    .coordinate(dae::CoordinateInput::Input(u))?;
                let x = expressions
                    .at(provenance)
                    .coordinate(dae::CoordinateInput::Algebraic(x))?;
                expressions
                    .at(provenance)
                    .binary(dae::BinaryOperator::Subtract, x, u)
            })?;
            dae.continuous(|continuous| {
                continuous.structured_family(
                    provenance,
                    domain,
                    rumoca_core::ComprehensionScalarView::RowMajorProjection,
                    |family| family.body(residual),
                )?;
                Ok(())
            })
        })
        .unwrap()
    }

    fn indexed_residual<'dae>(
        expressions: &mut dae::Expressions<'_, 'dae>,
        x: dae::ExprId<'dae>,
        u: dae::ExprId<'dae>,
        coordinate: i64,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
        let index = expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Integer(coordinate))?;
        let subscript = dae::Subscript::Index {
            expression: index,
            provenance,
        };
        let target = expressions.at(provenance).index(x, [subscript])?;
        let value = expressions.at(provenance).index(u, [subscript])?;
        expressions
            .at(provenance)
            .binary(dae::BinaryOperator::Subtract, target, value)
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
    fn exact_alias_chain_is_oriented_from_its_defined_source() {
        model(Fixture::AliasChain).inspect(|view| {
            let proof = CausalDefinitions::derive(view);
            let names = proof
                .order()
                .iter()
                .map(|target| view.variable((*target).into()).unwrap().name().as_str())
                .collect::<Vec<_>>();
            assert_eq!(names, ["x", "y"]);
            assert_eq!(proof.remaining_owner_count(), 0);
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

    #[test]
    fn exact_indexed_coverage_has_scalar_elimination_evidence() {
        indexed_array_model(true).inspect(|view| {
            let proof = CausalDefinitions::derive(view);
            let (x, _) = view
                .variables()
                .find(|(_, variable)| variable.name().as_str() == "x")
                .unwrap();
            assert!(proof.fully_defines_variable(x));
            assert!(proof.scalar_definition_for_variable(x, 0).is_some());
            assert!(proof.scalar_definition_for_variable(x, 1).is_some());
            assert_eq!(proof.remaining_owner_count(), 0);
        });
    }

    #[test]
    fn exact_row_major_family_has_aggregate_elimination_evidence() {
        structured_array_model().inspect(|view| {
            let proof = CausalDefinitions::derive(view);
            assert_eq!(proof.order().len(), 1);
            assert_eq!(proof.remaining_owner_count(), 0);
            let dae::ContinuousOwnerView::Structured { id, .. } =
                view.continuous_owners().next().unwrap()
            else {
                panic!("fixture contains one structured owner");
            };
            assert!(proof.consumes_family(id));
        });
    }

    #[test]
    fn partial_indexed_coverage_remains_a_residual_owner() {
        indexed_array_model(false).inspect(|view| {
            let proof = CausalDefinitions::derive(view);
            let (x, _) = view
                .variables()
                .find(|(_, variable)| variable.name().as_str() == "x")
                .unwrap();
            assert!(!proof.fully_defines_variable(x));
            assert!(proof.scalar_definition_for_variable(x, 0).is_none());
            assert_eq!(proof.remaining_owner_count(), 1);
        });
    }
}
