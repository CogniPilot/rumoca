use super::*;
use std::borrow::Cow;

#[derive(Clone)]
pub(in crate::construction) enum EquationPartition<'flat> {
    Continuous,
    DiscreteReal { target: &'flat VarName },
    DiscreteValue(DiscreteValueAssignmentPlan<'flat>),
}

#[derive(Clone)]
pub(in crate::construction) struct DiscreteValueAssignmentPlan<'flat> {
    pub(in crate::construction) target: &'flat VarName,
    pub(in crate::construction) value: Cow<'flat, Expression>,
    pub(in crate::construction) generated: bool,
}

pub(in crate::construction) fn equation_partition<'flat>(
    flat: &'flat flat::Model,
    equation: &'flat flat::Equation,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<EquationPartition<'flat>, ToDaeError> {
    if let Some(plan) = discrete_connection_assignment(flat, equation, roles) {
        return Ok(EquationPartition::DiscreteValue(plan));
    }
    if let Some(plan) = discrete_value_assignment(&equation.residual, roles, equation.span)? {
        return Ok(EquationPartition::DiscreteValue(plan));
    }
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs: _,
        ..
    } = &equation.residual
    else {
        return Ok(EquationPartition::Continuous);
    };
    let targets = assignment_target_names(lhs);
    let discrete_targets = targets
        .iter()
        .filter(|target| {
            matches!(
                roles.get(*target),
                Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
            )
        })
        .collect::<Vec<_>>();
    if discrete_targets.is_empty() {
        return Ok(EquationPartition::Continuous);
    }
    if discrete_targets.len() != targets.len() || targets.len() != 1 {
        return Err(ToDaeError::unsupported_flat(
            "mixed discrete equation",
            "one equation owner cannot mix continuous and discrete targets or define multiple discrete coordinates",
            equation.span,
        ));
    }
    let target = discrete_targets[0];
    match roles[target] {
        PlannedRole::DiscreteReal => Ok(EquationPartition::DiscreteReal { target }),
        PlannedRole::DiscreteValue => {
            unreachable!("discrete-value equations are classified before residual equations")
        }
        PlannedRole::Parameter
        | PlannedRole::Constant
        | PlannedRole::Input
        | PlannedRole::State
        | PlannedRole::Algebraic
        | PlannedRole::Output
        | PlannedRole::Clock
        | PlannedRole::EnumerationLiteral
        | PlannedRole::Aggregate => {
            unreachable!("the target was selected as a discrete coordinate")
        }
    }
}

fn discrete_connection_assignment<'flat>(
    flat: &'flat flat::Model,
    equation: &'flat flat::Equation,
    roles: &HashMap<VarName, PlannedRole>,
) -> Option<DiscreteValueAssignmentPlan<'flat>> {
    if !matches!(equation.origin, flat::EquationOrigin::Connection { .. }) {
        return None;
    }
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.residual
    else {
        return None;
    };
    let lhs_name = scalar_discrete_value_reference(lhs, roles)?;
    let rhs_name = scalar_discrete_value_reference(rhs, roles)?;
    let lhs_causality = &flat.variables.get(lhs_name)?.causality;
    let rhs_causality = &flat.variables.get(rhs_name)?.causality;
    match (lhs_causality, rhs_causality) {
        (Causality::Output(_), Causality::Input(_)) => Some(DiscreteValueAssignmentPlan {
            target: rhs_name,
            value: Cow::Borrowed(lhs),
            generated: false,
        }),
        (Causality::Input(_), Causality::Output(_)) => Some(DiscreteValueAssignmentPlan {
            target: lhs_name,
            value: Cow::Borrowed(rhs),
            generated: false,
        }),
        _ => None,
    }
}

fn scalar_discrete_value_reference<'flat>(
    expression: &'flat Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> Option<&'flat VarName> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    let name = name.var_name();
    (subscripts.is_empty() && matches!(roles.get(name), Some(PlannedRole::DiscreteValue)))
        .then_some(name)
}

fn discrete_value_assignment<'flat>(
    expression: &'flat Expression,
    roles: &HashMap<VarName, PlannedRole>,
    owner: Span,
) -> Result<Option<DiscreteValueAssignmentPlan<'flat>>, ToDaeError> {
    match expression {
        Expression::Binary {
            op: OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => {
            let mut references = Vec::new();
            lhs.collect_var_refs(&mut references);
            if !references
                .iter()
                .any(|name| matches!(roles.get(name), Some(PlannedRole::DiscreteValue)))
            {
                return Ok(None);
            }
            let Expression::VarRef {
                name, subscripts, ..
            } = lhs.as_ref()
            else {
                return Err(invalid_discrete_lhs(owner));
            };
            if !subscripts.is_empty()
                || !matches!(roles.get(name.var_name()), Some(PlannedRole::DiscreteValue))
            {
                return Err(invalid_discrete_lhs(owner));
            }
            Ok(Some(DiscreteValueAssignmentPlan {
                target: name.var_name(),
                value: Cow::Borrowed(rhs),
                generated: false,
            }))
        }
        Expression::If {
            branches,
            else_branch,
            span,
        } => {
            let Some(fallback) = discrete_value_assignment(else_branch, roles, owner)? else {
                return if expression_mentions_discrete_value(expression, roles) {
                    Err(invalid_discrete_lhs(owner))
                } else {
                    Ok(None)
                };
            };
            let mut values = Vec::with_capacity(branches.len());
            for (condition, branch) in branches {
                let Some(branch) = discrete_value_assignment(branch, roles, owner)? else {
                    return Err(invalid_discrete_lhs(owner));
                };
                if branch.target != fallback.target {
                    return Err(ToDaeError::discrete_solved_form_violation(
                        "all branches of a discrete-valued equation must assign the same coordinate",
                        owner,
                    ));
                }
                values.push((condition.clone(), branch.value.into_owned()));
            }
            Ok(Some(DiscreteValueAssignmentPlan {
                target: fallback.target,
                value: Cow::Owned(Expression::If {
                    branches: values,
                    else_branch: Box::new(fallback.value.into_owned()),
                    span: *span,
                }),
                generated: true,
            }))
        }
        _ if expression_mentions_discrete_value(expression, roles) => {
            Err(invalid_discrete_lhs(owner))
        }
        _ => Ok(None),
    }
}

fn expression_mentions_discrete_value(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> bool {
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    references
        .iter()
        .any(|name| matches!(roles.get(name), Some(PlannedRole::DiscreteValue)))
}

fn invalid_discrete_lhs(span: Span) -> ToDaeError {
    ToDaeError::discrete_solved_form_violation(
        "a discrete-valued equation must have one unsubscripted resolved coordinate as its left-hand side",
        span,
    )
}

fn assignment_target_names(expression: &Expression) -> Vec<&VarName> {
    let mut targets = Vec::new();
    collect_assignment_target_names(expression, &mut targets);
    targets
}

fn collect_assignment_target_names<'flat>(
    expression: &'flat Expression,
    targets: &mut Vec<&'flat VarName>,
) {
    match expression {
        Expression::VarRef { name, .. } => targets.push(name.var_name()),
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args,
            ..
        } if args.len() == 1 => collect_assignment_target_names(&args[0], targets),
        Expression::Unary { rhs, .. } | Expression::Index { base: rhs, .. } => {
            collect_assignment_target_names(rhs, targets);
        }
        Expression::Tuple { elements, .. } | Expression::Array { elements, .. } => {
            for element in elements {
                collect_assignment_target_names(element, targets);
            }
        }
        Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (_, value) in branches {
                collect_assignment_target_names(value, targets);
            }
            collect_assignment_target_names(else_branch, targets);
        }
        Expression::Literal { .. }
        | Expression::Binary { .. }
        | Expression::BuiltinCall { .. }
        | Expression::FunctionCall { .. }
        | Expression::StringConversion { .. }
        | Expression::ArrayComprehension { .. }
        | Expression::Range { .. }
        | Expression::FieldAccess { .. }
        | Expression::Empty { .. } => {}
    }
}

pub(super) fn defined_discrete_targets(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
) -> Result<HashSet<VarName>, ToDaeError> {
    let mut targets = event_targets(flat);
    targets.extend(algorithm_targets(flat).into_iter().filter(|target| {
        matches!(
            roles.get(target),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        )
    }));
    for equation in &flat.equations {
        match equation_partition(flat, equation, roles)? {
            EquationPartition::Continuous => continue,
            EquationPartition::DiscreteReal { target } => {
                targets.insert(target.clone());
            }
            EquationPartition::DiscreteValue(plan) => {
                targets.insert(plan.target.clone());
            }
        }
        // Every discrete coordinate the equation touches is an unknown of the
        // discrete system it belongs to, not a held value. The classified
        // target is only the side the solved form assigns; a connection row
        // (`a - b`) defines the other side just as much, and dropping it would
        // count the equation without counting its unknown.
        targets.extend(discrete_references(&equation.residual, roles));
    }
    Ok(targets)
}

fn discrete_references(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
) -> Vec<VarName> {
    let mut references = Vec::new();
    expression.collect_var_refs(&mut references);
    references.retain(|name| {
        matches!(
            roles.get(name),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        )
    });
    references
}
