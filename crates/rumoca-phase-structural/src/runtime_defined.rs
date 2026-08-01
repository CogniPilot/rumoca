//! Runtime-defined unknown discovery over checked DAE identities.
//!
//! SPEC_0029 §3b assigns this shared query to the structural phase. The DAE
//! grammar has already classified event and clock coordinates, so this module
//! never parses names or guesses semantics from function-call spelling.

use std::collections::HashSet;

use indexmap::IndexSet;
use rumoca_ir_dae as dae;

/// Names of unknowns defined by discrete, event, or clock evaluation.
pub fn runtime_defined_unknown_names(dae_model: &dae::Dae) -> IndexSet<String> {
    runtime_defined_impl(dae_model, true)
}

/// Names of continuous unknowns whose defining residual depends on event or
/// clock state.
pub fn runtime_defined_continuous_unknown_names(dae_model: &dae::Dae) -> IndexSet<String> {
    runtime_defined_impl(dae_model, false)
}

fn runtime_defined_impl(dae_model: &dae::Dae, include_discrete: bool) -> IndexSet<String> {
    dae_model.inspect(|view| {
        let mut defined = HashSet::new();
        collect_discrete_definitions(view, &mut defined);
        collect_condition_dependencies(view, &mut defined);
        collect_event_dependent_continuous_definitions(view, &mut defined);

        view.variables()
            .filter(|(id, variable)| {
                defined.contains(id) && included_role(variable.role(), include_discrete)
            })
            .map(|(_, variable)| variable.name().to_string())
            .collect()
    })
}

fn collect_discrete_definitions<'dae>(
    view: dae::DaeView<'dae>,
    defined: &mut HashSet<dae::VariableId<'dae>>,
) {
    for index in 0..view.discrete_value_owner_count() {
        let owner = view
            .discrete_value_owner(
                view.discrete_value_owner_id(index)
                    .expect("dense B.1c owner identity"),
            )
            .expect("dense B.1c owner resolves");
        for target in owner.targets().iter() {
            defined.insert(view.variable(target.into()).unwrap().id());
        }
        for branch in owner.branches().iter() {
            for (value, _) in branch.values().iter() {
                collect_expression_variables(view, value, defined);
            }
        }
    }
    for index in 0..view.discrete_real_equation_count() {
        let equation = view
            .discrete_real_equation(index)
            .expect("dense B.1b equation resolves");
        collect_expression_variables(view, equation.residual(), defined);
    }
}

fn collect_condition_dependencies<'dae>(
    view: dae::DaeView<'dae>,
    defined: &mut HashSet<dae::VariableId<'dae>>,
) {
    for index in 0..view.relation_count() {
        let relation = view
            .relation(view.relation_id(index).expect("dense relation identity"))
            .expect("dense relation resolves");
        collect_expression_variables(view, relation.expression(), defined);
    }
    for index in 0..view.condition_count() {
        let condition = view
            .condition(view.condition_id(index).expect("dense condition identity"))
            .expect("dense condition resolves");
        if let dae::ConditionOperation::Discrete(expression) = condition.operation() {
            collect_expression_variables(view, expression, defined);
        }
    }
}

fn collect_event_dependent_continuous_definitions<'dae>(
    view: dae::DaeView<'dae>,
    defined: &mut HashSet<dae::VariableId<'dae>>,
) {
    for index in 0..view.continuous_equation_count() {
        let residual = view
            .continuous_equation(index)
            .expect("dense continuous equation resolves")
            .residual();
        let Some((target, solution)) = solved_coordinate(view, residual) else {
            continue;
        };
        if contains_event_coordinate(view, solution) {
            defined.insert(target);
        }
    }
}

fn solved_coordinate<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Option<(dae::VariableId<'dae>, dae::ExprId<'dae>)> {
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = view.expression(residual)?.operation()
    else {
        return None;
    };
    referenced_variable(view, lhs)
        .map(|target| (target, rhs))
        .or_else(|| referenced_variable(view, rhs).map(|target| (target, lhs)))
}

fn referenced_variable<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<dae::VariableId<'dae>> {
    let node = view.expression(expression)?;
    node.variable_coordinate()
        .or_else(|| match node.operation() {
            dae::ExpressionOperation::Index { base, .. } => referenced_variable(view, base),
            _ => None,
        })
}

fn contains_event_coordinate<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> bool {
    let mut found = false;
    dae::for_each_expression(view, expression, |_, node| {
        found |= matches!(
            node.operation(),
            dae::ExpressionOperation::Coordinate(
                dae::CoordinateView::PreDiscreteReal(_)
                    | dae::CoordinateView::PreDiscreteValue(_)
                    // `pre()` of a continuous coordinate holds its left limit
                    // from the last event, so a residual that reads one is
                    // event-defined exactly like a discrete `pre()` read.
                    | dae::CoordinateView::PreState(_)
                    | dae::CoordinateView::PreAlgebraic(_)
                    | dae::CoordinateView::Condition(_)
                    | dae::CoordinateView::Previous(_)
                    | dae::CoordinateView::Terminal(_)
            )
        );
    });
    found
}

fn collect_expression_variables<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
    variables: &mut HashSet<dae::VariableId<'dae>>,
) {
    dae::for_each_expression(view, expression, |_, node| {
        if let Some(variable) = node.variable_coordinate() {
            variables.insert(variable);
        }
    });
}

fn included_role(role: dae::VariableRole, include_discrete: bool) -> bool {
    match role {
        dae::VariableRole::State | dae::VariableRole::Algebraic | dae::VariableRole::Output => true,
        dae::VariableRole::DiscreteReal | dae::VariableRole::DiscreteValue => include_discrete,
        dae::VariableRole::Parameter | dae::VariableRole::Constant | dae::VariableRole::Input => {
            false
        }
    }
}

#[cfg(test)]
fn define_test_discrete_value<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    target: dae::DiscreteValueId<'dae>,
    value: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    model.b1c([target], |topology| {
        topology.owner(provenance, [target], |owner| {
            owner.always(provenance, [(value, provenance)])
        })?;
        Ok(())
    })
}

#[cfg(test)]
mod tests {
    use rumoca_core::{SourceMap, Span, TypeId, VarName};

    use super::*;

    #[test]
    fn runtime_defined_queries_use_checked_roles_and_targets() {
        let mut sources = SourceMap::new();
        let source = sources.add("runtime_defined.mo", "Real a; discrete Boolean enable;");
        let a_at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 6)).unwrap();
        let enable_at = dae::DaeProvenance::source(Span::from_offsets(source, 8, 31)).unwrap();
        let model = dae::Dae::construct(sources, |model| {
            let real = model.types(|types| {
                types.intern(
                    TypeId::new(0),
                    dae::ValueType::scalar(dae::ScalarType::Real),
                    a_at,
                )
            })?;
            let boolean = model.types(|types| {
                types.intern(
                    TypeId::new(1),
                    dae::ValueType::scalar(dae::ScalarType::Boolean),
                    enable_at,
                )
            })?;
            let (a, enable) = model.variables(|variables| {
                Ok((
                    variables.algebraic(
                        VarName::new("a"),
                        real,
                        a_at,
                        dae::VariableAttributes::default(),
                    )?,
                    variables.discrete_value(
                        VarName::new("enable"),
                        boolean,
                        enable_at,
                        dae::VariableAttributes::default(),
                    )?,
                ))
            })?;
            let (a_use, enabled) = model.expressions(|expressions| {
                Ok((
                    expressions
                        .at(a_at)
                        .coordinate(dae::CoordinateInput::Algebraic(a))?,
                    expressions
                        .at(enable_at)
                        .literal(dae::DaeLiteral::Boolean(true))?,
                ))
            })?;
            define_test_discrete_value(model, enable, enabled, enable_at)?;
            let condition = model.conditions(|conditions| conditions.reserve(enable_at))?;
            model.conditions(|conditions| {
                conditions.define(condition, dae::ConditionInput::Discrete(enabled), enable_at)
            })?;
            model.expressions(|expressions| {
                expressions
                    .at(enable_at)
                    .binary(dae::BinaryOperator::Equal, a_use, a_use)?;
                Ok(())
            })
        })
        .unwrap();

        let all = runtime_defined_unknown_names(&model);
        assert!(all.contains("enable"));
        assert!(!all.contains("a"));
        let continuous = runtime_defined_continuous_unknown_names(&model);
        assert!(!continuous.contains("enable"));
    }
}
