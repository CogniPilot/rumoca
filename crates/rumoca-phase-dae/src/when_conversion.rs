//! Conversion of flattened when-equations into DAE when clauses.

use std::collections::HashSet;

use indexmap::{IndexMap, IndexSet};
use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use crate::{
    ToDaeError, compute_var_size, dae_to_flat_expression, dae_to_flat_var_name,
    flat_to_dae_expression, flat_to_dae_var_name, resolve_embedded_subscript_size,
};

/// Compute scalar count for a when-equation target variable.
fn when_target_scalar_count(target: &rumoca_core::VarName, flat: &flat::Model) -> usize {
    if let Some(var) = flat.variables.get(target) {
        return compute_var_size(&var.dims);
    }
    if let Some(size) = resolve_embedded_subscript_size(target.as_str(), flat, &HashSet::default())
    {
        return size.max(1);
    }
    1
}

fn target_ref(target: &rumoca_core::VarName) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: target.clone().into(),
        subscripts: vec![],
        span: rumoca_core::Span::DUMMY,
    }
}

/// Build `pre(target)` expression used for conditional when branches without assignment.
fn pre_of_target(target: &rumoca_core::VarName) -> rumoca_core::Expression {
    rumoca_core::Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Pre,
        args: vec![target_ref(target)],
        span: rumoca_core::Span::DUMMY,
    }
}

fn missing_when_branch_rhs(
    target: &rumoca_core::VarName,
    state_vars: &IndexSet<rumoca_core::VarName>,
    flat: &flat::Model,
) -> Result<rumoca_core::Expression, ToDaeError> {
    if state_vars.contains(target) {
        return Ok(target_ref(target));
    }
    if flat.variables.get(target).is_some_and(|var| {
        matches!(var.variability, rumoca_core::Variability::Discrete(_)) || var.is_discrete_type
    }) {
        return Ok(pre_of_target(target));
    }
    Err(ToDaeError::internal(format!(
        "MLS §8.3.5 violation: conditional when-equation target '{}' is not assigned in every branch and is not a discrete variable",
        target
    )))
}

/// Convert an explicit when assignment/reinit to a DAE equation with scalar count.
fn build_when_assignment_eq(
    target: &rumoca_core::VarName,
    rhs: &rumoca_core::Expression,
    span: Span,
    origin: &str,
    flat: &flat::Model,
) -> Option<dae::Equation> {
    let scalar_count = when_target_scalar_count(target, flat);
    if scalar_count == 0 {
        return None;
    }
    Some(dae::Equation::explicit_with_scalar_count(
        flat_to_dae_var_name(target),
        flat_to_dae_expression(rhs),
        span,
        origin.to_string(),
        scalar_count,
    ))
}

/// Insert a converted when-equation into a target map, rejecting duplicate targets.
fn insert_when_assignment(
    assignments: &mut IndexMap<rumoca_core::VarName, dae::Equation>,
    equation: dae::Equation,
) -> Result<(), ToDaeError> {
    let Some(target) = equation.lhs.clone() else {
        return Err(ToDaeError::internal(
            "internal error: when-equation conversion produced equation without LHS",
        ));
    };
    let target = dae_to_flat_var_name(&target);

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
    state_vars: &IndexSet<rumoca_core::VarName>,
    flat: &flat::Model,
) -> Result<IndexMap<rumoca_core::VarName, dae::Equation>, ToDaeError> {
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
    target: &rumoca_core::VarName,
    branches: &[(
        rumoca_core::Expression,
        IndexMap<rumoca_core::VarName, dae::Equation>,
    )],
    else_branch: &IndexMap<rumoca_core::VarName, dae::Equation>,
    state_vars: &IndexSet<rumoca_core::VarName>,
    flat: &flat::Model,
) -> Result<rumoca_core::Expression, ToDaeError> {
    let fallback = missing_when_branch_rhs(target, state_vars, flat)?;
    let branches = branches
        .iter()
        .map(|(condition, assignments)| {
            let rhs = assignments
                .get(target)
                .map(|eq| dae_to_flat_expression(&eq.rhs))
                .unwrap_or_else(|| fallback.clone());
            (condition.clone(), rhs)
        })
        .collect::<Vec<_>>();

    let else_rhs = else_branch
        .get(target)
        .map(|eq| dae_to_flat_expression(&eq.rhs))
        .unwrap_or(fallback);

    Ok(rumoca_core::Expression::If {
        branches,
        else_branch: Box::new(else_rhs),
        span: rumoca_core::Span::DUMMY,
    })
}

/// Convert a conditional when-equation to explicit per-target assignments.
fn convert_conditional_when_equation(
    branches: &[(rumoca_core::Expression, Vec<flat::WhenEquation>)],
    else_branch: &[flat::WhenEquation],
    span: Span,
    origin: &str,
    state_vars: &IndexSet<rumoca_core::VarName>,
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
    let mut target_order = IndexMap::<rumoca_core::VarName, ()>::new();
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
        let rhs = build_conditional_when_rhs(
            target,
            &converted_branches,
            &converted_else,
            state_vars,
            flat,
        )?;
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
    state_vars: &IndexSet<rumoca_core::VarName>,
    flat: &flat::Model,
) -> Result<dae::WhenClause, ToDaeError> {
    let mut dae_when = dae::WhenClause::new(
        flat_to_dae_expression(&when.condition),
        when.span,
        "when clause".to_string(),
    );

    for weq in &when.equations {
        for dae_eq in convert_when_equation(weq, state_vars, flat)? {
            dae_when.equations.push(dae_eq);
        }
    }
    collect_when_actions(&when.equations, &when.condition, &mut dae_when.actions)?;

    Ok(dae_when)
}

fn collect_when_actions(
    equations: &[flat::WhenEquation],
    guard: &rumoca_core::Expression,
    actions: &mut Vec<dae::DaeEventAction>,
) -> Result<(), ToDaeError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Reinit { .. } => {}
            flat::WhenEquation::Assert {
                condition,
                message,
                span,
                origin,
            } => {
                actions.push(dae::DaeEventAction {
                    condition: flat_to_dae_expression(&and_expr(
                        guard.clone(),
                        not_expr(condition.clone(), *span),
                        *span,
                    )),
                    kind: dae::DaeEventActionKind::Assert {
                        message: message.clone(),
                    },
                    span: *span,
                    origin: origin.clone(),
                });
            }
            flat::WhenEquation::Terminate {
                message,
                span,
                origin,
            } => actions.push(dae::DaeEventAction {
                condition: flat_to_dae_expression(guard),
                kind: dae::DaeEventActionKind::Terminate {
                    message: message.clone(),
                },
                span: *span,
                origin: origin.clone(),
            }),
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                span,
                ..
            } => {
                let mut branch_conditions = Vec::with_capacity(branches.len());
                for (condition, branch_equations) in branches {
                    branch_conditions.push(condition.clone());
                    collect_when_actions(
                        branch_equations,
                        &and_expr(guard.clone(), condition.clone(), *span),
                        actions,
                    )?;
                }
                let else_guard = branch_conditions
                    .into_iter()
                    .fold(guard.clone(), |acc, cond| {
                        and_expr(acc, not_expr(cond, *span), *span)
                    });
                collect_when_actions(else_branch, &else_guard, actions)?;
            }
            flat::WhenEquation::Assign { .. } | flat::WhenEquation::FunctionCallOutputs { .. } => {}
        }
    }
    Ok(())
}

fn and_expr(
    lhs: rumoca_core::Expression,
    rhs: rumoca_core::Expression,
    span: Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::And,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn not_expr(expr: rumoca_core::Expression, span: Span) -> rumoca_core::Expression {
    rumoca_core::Expression::Unary {
        op: rumoca_core::OpUnary::Not,
        rhs: Box::new(expr),
        span,
    }
}

/// Convert a single flat::WhenEquation to zero or more DAE equations.
///
/// MLS §10.5: When-clause equations targeting zero-size array variables
/// (e.g., `x = expr` where `x` has dims `[0]`) are eliminated since they
/// produce no scalar equations.
fn convert_when_equation(
    weq: &flat::WhenEquation,
    state_vars: &IndexSet<rumoca_core::VarName>,
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
        flat::WhenEquation::FunctionCallOutputs { span, origin, .. } => {
            Err(ToDaeError::unsupported_algorithm(
                "when-equation",
                format!("{origin}: multi-output function call assignment"),
                *span,
            ))
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
        let target = rumoca_core::VarName::new("x");
        let eq = dae::Equation::explicit(
            flat_to_dae_var_name(&target),
            flat_to_dae_expression(&rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            }),
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
        let target = rumoca_core::VarName::new("x");
        let rhs = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: rumoca_core::Span::DUMMY,
        };
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

    #[test]
    fn missing_conditional_when_branch_uses_pre_only_for_discrete_targets() {
        let target = rumoca_core::VarName::new("d");
        let mut flat = flat::Model::new();
        flat.variables.insert(
            target.clone(),
            flat::Variable {
                name: target.clone(),
                variability: rumoca_core::Variability::Discrete(rumoca_core::Token::default()),
                ..Default::default()
            },
        );

        let rhs = missing_when_branch_rhs(&target, &IndexSet::new(), &flat)
            .expect("discrete targets may hold their previous value");

        assert!(matches!(
            rhs,
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Pre,
                ..
            }
        ));
    }

    #[test]
    fn missing_conditional_reinit_branch_uses_current_state_not_pre() {
        let target = rumoca_core::VarName::new("x");
        let mut state_vars = IndexSet::new();
        state_vars.insert(target.clone());

        let rhs = missing_when_branch_rhs(&target, &state_vars, &flat::Model::new())
            .expect("state reinit branches may retain current state");

        assert!(matches!(
            rhs,
            rumoca_core::Expression::VarRef { name, .. } if name.var_name() == &target
        ));
    }

    #[test]
    fn missing_conditional_when_branch_rejects_continuous_non_state_targets() {
        let target = rumoca_core::VarName::new("y");
        let mut flat = flat::Model::new();
        flat.variables.insert(
            target.clone(),
            flat::Variable {
                name: target.clone(),
                variability: rumoca_core::Variability::Continuous(rumoca_core::Token::default()),
                ..Default::default()
            },
        );

        let err = missing_when_branch_rhs(&target, &IndexSet::new(), &flat)
            .expect_err("continuous non-state target must not synthesize pre(target)");

        assert!(err.to_string().contains("not assigned in every branch"));
    }
}
