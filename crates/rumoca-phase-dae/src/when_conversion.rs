//! Conversion of flattened when-equations into DAE when clauses.

use std::collections::HashSet;

use indexmap::{IndexMap, IndexSet};
use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_flat as flat;

use crate::{
    ToDaeError,
    analysis::{
        discrete_partition,
        variable_analysis::{self, resolve_flat_function},
    },
    compute_var_size, dae_to_flat_expression, dae_to_flat_var_name,
    flat_to_dae_expression_with_refs, flat_to_dae_var_name, resolve_embedded_subscript_size,
};

#[cfg(test)]
use crate::flat_to_dae_expression;

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
        name: crate::convert::structured_target_reference(target),
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
    if flat
        .variables
        .get(target)
        .is_some_and(is_discrete_when_memory_target)
        || is_clocked_assignment_target(target, flat)
        || is_when_only_memory_target(target, flat)
    {
        return Ok(pre_of_target(target));
    }
    Err(ToDaeError::internal(format!(
        "MLS §8.3.5 violation: conditional when-equation target '{}' is not assigned in every branch and is not a discrete variable",
        target
    )))
}

fn is_when_only_memory_target(target: &rumoca_core::VarName, flat: &flat::Model) -> bool {
    variable_analysis::find_when_only_vars(flat, &Default::default()).contains(target)
}

fn is_clocked_assignment_target(target: &rumoca_core::VarName, flat: &flat::Model) -> bool {
    flat.equations.iter().any(|equation| {
        discrete_partition::is_clocked_assignment_equation(equation)
            && discrete_partition::residual_lhs_targets(&equation.residual)
                .into_iter()
                .any(|candidate| candidate == *target)
    })
}

fn is_discrete_when_memory_target(var: &flat::Variable) -> bool {
    matches!(var.variability, rumoca_core::Variability::Discrete(_))
        || var.is_discrete_type
        || var
            .binding
            .as_ref()
            .is_some_and(discrete_partition::expression_contains_clocked_operators)
}

/// Convert an explicit when assignment/reinit to a DAE equation with scalar count.
#[derive(Debug)]
struct ConvertedWhenEquation {
    equation: dae::Equation,
    inactive_rhs: dae::WhenEquationInactiveRhs,
}

fn build_when_assignment_eq(
    target: &rumoca_core::VarName,
    rhs: &rumoca_core::Expression,
    span: Span,
    origin: &str,
    inactive_rhs: dae::WhenEquationInactiveRhs,
    flat: &flat::Model,
) -> Option<ConvertedWhenEquation> {
    let scalar_count = when_target_scalar_count(target, flat);
    if scalar_count == 0 {
        return None;
    }
    Some(ConvertedWhenEquation {
        equation: dae::Equation::explicit_with_scalar_count(
            crate::convert::structured_target_reference(&flat_to_dae_var_name(target)),
            flat_to_dae_expression_with_refs(rhs, flat),
            span,
            origin.to_string(),
            scalar_count,
        ),
        inactive_rhs,
    })
}

fn build_when_function_call_output_eqs(
    outputs: &[rumoca_core::VarName],
    function: &rumoca_core::Expression,
    span: Span,
    origin: &str,
    flat: &flat::Model,
) -> Result<Vec<ConvertedWhenEquation>, ToDaeError> {
    let rumoca_core::Expression::FunctionCall {
        name,
        args,
        is_constructor,
        ..
    } = function
    else {
        return Err(ToDaeError::unsupported_algorithm(
            "when-equation",
            format!("{origin}: multi-output assignment rhs is not a function call"),
            span,
        ));
    };
    if *is_constructor {
        return Err(ToDaeError::unsupported_algorithm(
            "when-equation",
            format!("{origin}: constructor multi-output assignment"),
            span,
        ));
    }

    let Some(function_def) = resolve_flat_function(name.as_str(), flat) else {
        return Err(ToDaeError::unresolved_function_call(name.as_str(), span));
    };
    if function_def.outputs.len() < outputs.len() {
        return Err(ToDaeError::internal(format!(
            "when-equation multi-output assignment expects {} outputs from '{}', but function has {}",
            outputs.len(),
            name.as_str(),
            function_def.outputs.len()
        )));
    }

    let mut equations = Vec::with_capacity(outputs.len());
    for (target, function_output) in outputs.iter().zip(function_def.outputs.iter()) {
        let selection_name = rumoca_core::VarName::new(format!(
            "{}.{}",
            name.as_str(),
            function_output.name.as_str()
        ));
        let rhs = rumoca_core::Expression::FunctionCall {
            name: selection_name.into(),
            args: args.clone(),
            is_constructor: false,
            span,
        };
        let output_origin = format!("{origin}: multi-output assignment to {target}");
        if let Some(eq) = build_when_assignment_eq(
            target,
            &rhs,
            span,
            &output_origin,
            dae::WhenEquationInactiveRhs::Pre,
            flat,
        ) {
            equations.push(eq);
        }
    }
    Ok(equations)
}

/// Insert a converted when-equation into a target map, rejecting duplicate targets.
fn insert_when_assignment(
    assignments: &mut IndexMap<rumoca_core::VarName, ConvertedWhenEquation>,
    converted: ConvertedWhenEquation,
) -> Result<(), ToDaeError> {
    let Some(target) = converted.equation.lhs.clone() else {
        return Err(ToDaeError::internal(
            "internal error: when-equation conversion produced equation without LHS",
        ));
    };
    let target = dae_to_flat_var_name(target.var_name());

    if assignments.insert(target.clone(), converted).is_some() {
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
) -> Result<IndexMap<rumoca_core::VarName, ConvertedWhenEquation>, ToDaeError> {
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
        IndexMap<rumoca_core::VarName, ConvertedWhenEquation>,
    )],
    else_branch: &IndexMap<rumoca_core::VarName, ConvertedWhenEquation>,
    state_vars: &IndexSet<rumoca_core::VarName>,
    flat: &flat::Model,
) -> Result<rumoca_core::Expression, ToDaeError> {
    let fallback = missing_when_branch_rhs(target, state_vars, flat)?;
    let branches = branches
        .iter()
        .map(|(condition, assignments)| {
            let rhs = assignments
                .get(target)
                .map(|converted| dae_to_flat_expression(&converted.equation.rhs))
                .unwrap_or_else(|| fallback.clone());
            (condition.clone(), rhs)
        })
        .collect::<Vec<_>>();

    let else_rhs = else_branch
        .get(target)
        .map(|converted| dae_to_flat_expression(&converted.equation.rhs))
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
) -> Result<Vec<ConvertedWhenEquation>, ToDaeError> {
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
        let inactive_rhs = if state_vars.contains(target) {
            dae::WhenEquationInactiveRhs::Current
        } else {
            dae::WhenEquationInactiveRhs::Pre
        };
        if let Some(converted_eq) =
            build_when_assignment_eq(target, &rhs, span, &conditional_origin, inactive_rhs, flat)
        {
            converted.push(converted_eq);
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
        flat_to_dae_expression_with_refs(&when.condition, flat),
        when.span,
        "when clause".to_string(),
    );

    for weq in &when.equations {
        for dae_eq in convert_when_equation(weq, state_vars, flat)? {
            dae_when.equations.push(dae_eq.equation);
            dae_when.equation_inactive_rhs.push(dae_eq.inactive_rhs);
        }
    }
    collect_when_actions(
        &when.equations,
        &when.condition,
        flat,
        &mut dae_when.actions,
    )?;

    Ok(dae_when)
}

fn collect_when_actions(
    equations: &[flat::WhenEquation],
    guard: &rumoca_core::Expression,
    flat: &flat::Model,
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
                    condition: flat_to_dae_expression_with_refs(
                        &and_expr(guard.clone(), not_expr(condition.clone(), *span), *span),
                        flat,
                    ),
                    kind: dae::DaeEventActionKind::Assert {
                        message: flat_to_dae_expression_with_refs(message, flat),
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
                condition: flat_to_dae_expression_with_refs(guard, flat),
                kind: dae::DaeEventActionKind::Terminate {
                    message: flat_to_dae_expression_with_refs(message, flat),
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
                        flat,
                        actions,
                    )?;
                }
                let else_guard = branch_conditions
                    .into_iter()
                    .fold(guard.clone(), |acc, cond| {
                        and_expr(acc, not_expr(cond, *span), *span)
                    });
                collect_when_actions(else_branch, &else_guard, flat, actions)?;
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
) -> Result<Vec<ConvertedWhenEquation>, ToDaeError> {
    match weq {
        flat::WhenEquation::Assign {
            target,
            value,
            span,
            origin,
        } => Ok(build_when_assignment_eq(
            target,
            value,
            *span,
            origin,
            dae::WhenEquationInactiveRhs::Pre,
            flat,
        )
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
            Ok(build_when_assignment_eq(
                state,
                value,
                *span,
                origin,
                dae::WhenEquationInactiveRhs::Current,
                flat,
            )
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
        flat::WhenEquation::FunctionCallOutputs {
            outputs,
            function,
            span,
            origin,
        } => build_when_function_call_output_eqs(outputs, function, *span, origin, flat),
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
            crate::convert::structured_target_reference(&flat_to_dae_var_name(&target)),
            flat_to_dae_expression(&rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Integer(1),
                span: rumoca_core::Span::DUMMY,
            }),
            Span::default(),
            "test".to_string(),
        );
        insert_when_assignment(
            &mut assignments,
            ConvertedWhenEquation {
                equation: eq.clone(),
                inactive_rhs: dae::WhenEquationInactiveRhs::Pre,
            },
        )
        .expect("first insert should succeed");
        let err = insert_when_assignment(
            &mut assignments,
            ConvertedWhenEquation {
                equation: eq,
                inactive_rhs: dae::WhenEquationInactiveRhs::Pre,
            },
        )
        .expect_err("duplicate insert should fail");
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

        let eq = build_when_assignment_eq(
            &target,
            &rhs,
            Span::default(),
            "test",
            dae::WhenEquationInactiveRhs::Pre,
            &flat,
        )
        .expect("array target should produce explicit equation");
        assert_eq!(eq.equation.scalar_count, 3);
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
    fn missing_conditional_when_branch_uses_pre_for_clocked_binding_targets() {
        let target = rumoca_core::VarName::new("u_super");
        let mut flat = flat::Model::new();
        flat.variables.insert(
            target.clone(),
            flat::Variable {
                name: target.clone(),
                binding: Some(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("superSample").into(),
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("u").into(),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                variability: rumoca_core::Variability::Continuous(rumoca_core::Token::default()),
                is_discrete_type: false,
                ..Default::default()
            },
        );

        let rhs = missing_when_branch_rhs(&target, &IndexSet::new(), &flat)
            .expect("clocked binding targets may hold their previous value");

        assert!(matches!(
            rhs,
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Pre,
                ..
            }
        ));
    }

    #[test]
    fn missing_conditional_when_branch_uses_pre_for_clocked_equation_targets() {
        let target = rumoca_core::VarName::new("u_super");
        let mut flat = flat::Model::new();
        flat.variables.insert(
            target.clone(),
            flat::Variable {
                name: target.clone(),
                variability: rumoca_core::Variability::Continuous(rumoca_core::Token::default()),
                is_discrete_type: false,
                ..Default::default()
            },
        );
        flat.equations.push(flat::Equation {
            residual: rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs: Box::new(target_ref(&target)),
                rhs: Box::new(rumoca_core::Expression::FunctionCall {
                    name: rumoca_core::VarName::new("superSample").into(),
                    args: vec![rumoca_core::Expression::VarRef {
                        name: rumoca_core::VarName::new("u").into(),
                        subscripts: vec![],
                        span: rumoca_core::Span::DUMMY,
                    }],
                    is_constructor: false,
                    span: rumoca_core::Span::DUMMY,
                }),
                span: rumoca_core::Span::DUMMY,
            },
            span: rumoca_core::Span::DUMMY,
            origin: flat::EquationOrigin::ComponentEquation {
                component: "clocked".to_string(),
            },
            scalar_count: 1,
        });

        let rhs = missing_when_branch_rhs(&target, &IndexSet::new(), &flat)
            .expect("clocked equation targets may hold their previous value");

        assert!(matches!(
            rhs,
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Pre,
                ..
            }
        ));
    }

    #[test]
    fn missing_conditional_when_branch_uses_pre_for_when_only_targets() {
        let target = rumoca_core::VarName::new("u_super");
        let mut flat = flat::Model::new();
        flat.variables.insert(
            target.clone(),
            flat::Variable {
                name: target.clone(),
                variability: rumoca_core::Variability::Continuous(rumoca_core::Token::default()),
                is_discrete_type: false,
                ..Default::default()
            },
        );
        let mut when_clause = flat::WhenClause::new(
            rumoca_core::Expression::Literal {
                value: rumoca_core::Literal::Boolean(true),
                span: rumoca_core::Span::DUMMY,
            },
            Span::DUMMY,
        );
        when_clause.add_equation(flat::WhenEquation::Conditional {
            branches: vec![(
                rumoca_core::Expression::Literal {
                    value: rumoca_core::Literal::Boolean(true),
                    span: rumoca_core::Span::DUMMY,
                },
                vec![flat::WhenEquation::assign(
                    target.clone(),
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Real(1.0),
                        span: rumoca_core::Span::DUMMY,
                    },
                    Span::DUMMY,
                    "clocked branch",
                )],
            )],
            else_branch: vec![],
            span: Span::DUMMY,
            origin: "conditional clocked memory".to_string(),
        });
        flat.when_clauses.push(when_clause);

        let rhs = missing_when_branch_rhs(&target, &IndexSet::new(), &flat)
            .expect("when-only targets may hold their previous value");

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
