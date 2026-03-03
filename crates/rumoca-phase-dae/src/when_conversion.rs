//! Conversion of flattened when-equations into DAE when clauses.

use std::collections::HashSet;

use indexmap::IndexMap;
use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use crate::{ToDaeError, compute_var_size, resolve_embedded_subscript_size};

/// Compute scalar count for a when-equation target variable.
fn when_target_scalar_count(target: &flat::VarName, flat: &flat::Model) -> usize {
    if let Some(var) = flat.variables.get(target) {
        return compute_var_size(&var.dims);
    }
    if let Some(size) = resolve_embedded_subscript_size(target.as_str(), flat, &HashSet::default())
    {
        return size.max(1);
    }
    1
}

/// Build `pre(target)` expression used for conditional when branches without assignment.
fn pre_of_target(target: &flat::VarName) -> flat::Expression {
    flat::Expression::BuiltinCall {
        function: flat::BuiltinFunction::Pre,
        args: vec![flat::Expression::VarRef {
            name: target.clone(),
            subscripts: vec![],
        }],
    }
}

/// Convert an explicit when assignment/reinit to a DAE equation with scalar count.
fn build_when_assignment_eq(
    target: &flat::VarName,
    rhs: &flat::Expression,
    span: Span,
    origin: &str,
    flat: &flat::Model,
) -> Option<dae::Equation> {
    let scalar_count = when_target_scalar_count(target, flat);
    if scalar_count == 0 {
        return None;
    }
    Some(dae::Equation::explicit_with_scalar_count(
        target.clone(),
        rhs.clone(),
        span,
        origin.to_string(),
        scalar_count,
    ))
}

/// Insert a converted when-equation into a target map, rejecting duplicate targets.
fn insert_when_assignment(
    assignments: &mut IndexMap<flat::VarName, dae::Equation>,
    equation: dae::Equation,
) -> Result<(), ToDaeError> {
    let Some(target) = equation.lhs.clone() else {
        return Err(ToDaeError::internal(
            "internal error: when-equation conversion produced equation without LHS",
        ));
    };

    if assignments.insert(target.clone(), equation).is_some() {
        return Err(ToDaeError::internal(format!(
            "MLS §8.3.5 violation: duplicate assignment to '{}' in when-equation branch",
            target
        )));
    }
    Ok(())
}

/// Convert a list of nested when-equations into a target->equation map.
fn collect_when_assignments(
    equations: &[flat::WhenEquation],
    state_vars: &HashSet<flat::VarName>,
    flat: &flat::Model,
) -> Result<IndexMap<flat::VarName, dae::Equation>, ToDaeError> {
    let mut assignments = IndexMap::new();
    for equation in equations {
        for converted in convert_when_equation(equation, state_vars, flat)? {
            insert_when_assignment(&mut assignments, converted)?;
        }
    }
    Ok(assignments)
}

/// Build conditional RHS for one target from when-if branch assignments.
fn build_conditional_when_rhs(
    target: &flat::VarName,
    branches: &[(flat::Expression, IndexMap<flat::VarName, dae::Equation>)],
    else_branch: &IndexMap<flat::VarName, dae::Equation>,
) -> flat::Expression {
    let branches = branches
        .iter()
        .map(|(condition, assignments)| {
            let rhs = assignments
                .get(target)
                .map(|eq| eq.rhs.clone())
                .unwrap_or_else(|| pre_of_target(target));
            (condition.clone(), rhs)
        })
        .collect::<Vec<_>>();

    let else_rhs = else_branch
        .get(target)
        .map(|eq| eq.rhs.clone())
        .unwrap_or_else(|| pre_of_target(target));

    flat::Expression::If {
        branches,
        else_branch: Box::new(else_rhs),
    }
}

/// Convert a conditional when-equation to explicit per-target assignments.
fn convert_conditional_when_equation(
    branches: &[(flat::Expression, Vec<flat::WhenEquation>)],
    else_branch: &[flat::WhenEquation],
    span: Span,
    origin: &str,
    state_vars: &HashSet<flat::VarName>,
    flat: &flat::Model,
) -> Result<Vec<dae::Equation>, ToDaeError> {
    let converted_branches = branches
        .iter()
        .map(|(condition, equations)| {
            let assignments = collect_when_assignments(equations, state_vars, flat)?;
            Ok::<_, ToDaeError>((condition.clone(), assignments))
        })
        .collect::<Result<Vec<_>, _>>()?;

    let converted_else = collect_when_assignments(else_branch, state_vars, flat)?;

    // Preserve deterministic target order.
    let mut target_order = IndexMap::<flat::VarName, ()>::new();
    for (_, assignments) in &converted_branches {
        for target in assignments.keys() {
            target_order.insert(target.clone(), ());
        }
    }
    for target in converted_else.keys() {
        target_order.insert(target.clone(), ());
    }

    let mut converted = Vec::new();
    for target in target_order.keys() {
        let rhs = build_conditional_when_rhs(target, &converted_branches, &converted_else);
        let conditional_origin = format!("conditional when assignment to {} ({})", target, origin);
        if let Some(eq) = build_when_assignment_eq(target, &rhs, span, &conditional_origin, flat) {
            converted.push(eq);
        }
    }

    Ok(converted)
}

/// Convert a WhenClause to a WhenClause.
pub(crate) fn convert_when_clause(
    when: &rumoca_ir_flat::WhenClause,
    state_vars: &HashSet<flat::VarName>,
    flat: &flat::Model,
) -> Result<dae::WhenClause, ToDaeError> {
    let mut dae_when =
        dae::WhenClause::new(when.condition.clone(), when.span, "when clause".to_string());

    for weq in &when.equations {
        for dae_eq in convert_when_equation(weq, state_vars, flat)? {
            dae_when.equations.push(dae_eq);
        }
    }

    Ok(dae_when)
}

/// Convert a single flat::WhenEquation to zero or more DAE equations.
///
/// MLS §10.5: When-clause equations targeting zero-size array variables
/// (e.g., `x = expr` where `x` has dims `[0]`) are eliminated since they
/// produce no scalar equations.
fn convert_when_equation(
    weq: &flat::WhenEquation,
    state_vars: &HashSet<flat::VarName>,
    flat: &flat::Model,
) -> Result<Vec<dae::Equation>, ToDaeError> {
    match weq {
        flat::WhenEquation::Assign {
            target,
            value,
            span,
            origin,
        } => Ok(build_when_assignment_eq(target, value, *span, origin, flat)
            .into_iter()
            .collect()),
        flat::WhenEquation::Reinit {
            state,
            value,
            span,
            origin,
        } => {
            // EQN-016: Validate that reinit target is a state variable
            if !state_vars.contains(state) {
                return Err(ToDaeError::reinit_non_state(state.as_str(), *span));
            }
            Ok(build_when_assignment_eq(state, value, *span, origin, flat)
                .into_iter()
                .collect())
        }
        flat::WhenEquation::Assert { .. } | flat::WhenEquation::Terminate { .. } => {
            // Assert and Terminate don't produce equations
            // They are runtime checks/actions
            Ok(vec![])
        }
        flat::WhenEquation::Conditional {
            branches,
            else_branch,
            span,
            origin,
            ..
        } => convert_conditional_when_equation(
            branches,
            else_branch,
            *span,
            origin,
            state_vars,
            flat,
        ),
        flat::WhenEquation::FunctionCallOutputs { .. } => {
            // Multi-output function calls are kept in the flat::Model
            // and handled by code generators. They don't produce standard
            // DAE equations - the outputs are set by the function call.
            Ok(vec![])
        }
    }
}

#[cfg(test)]
mod tests {
    use rumoca_core::Span;

    use super::*;

    #[test]
    fn test_insert_when_assignment_rejects_duplicate_targets() {
        let mut assignments = IndexMap::new();
        let target = flat::VarName::new("x");
        let eq = dae::Equation::explicit(
            target.clone(),
            flat::Expression::Literal(rumoca_ir_flat::Literal::Integer(1)),
            Span::default(),
            "test".to_string(),
        );
        insert_when_assignment(&mut assignments, eq.clone()).expect("first insert should succeed");
        let err =
            insert_when_assignment(&mut assignments, eq).expect_err("duplicate insert should fail");
        assert!(err.to_string().contains("duplicate assignment"));
    }

    #[test]
    fn test_build_when_assignment_eq_uses_target_scalar_size() {
        let target = flat::VarName::new("x");
        let rhs = flat::Expression::Literal(rumoca_ir_flat::Literal::Integer(1));
        let mut flat = flat::Model::new();
        flat.variables.insert(
            target.clone(),
            flat::Variable {
                name: target.clone(),
                dims: vec![3],
                ..Default::default()
            },
        );

        let eq = build_when_assignment_eq(&target, &rhs, Span::default(), "test", &flat)
            .expect("array target should produce explicit equation");
        assert_eq!(eq.scalar_count, 3);
    }
}
