use super::*;
use rumoca_core::ExpressionRewriter;

pub(super) fn validate_when_chains(
    chains: &[flat::WhenChain],
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
) -> Result<HashSet<Span>, ToDaeError> {
    let mut reinit_state_pre = HashSet::new();
    for chain in chains {
        for branch in chain.branches() {
            validate_condition_expression(
                &branch.condition,
                roles,
                states,
                constants,
                sample_lattices,
            )?;
            let clocked = matches!(
                &branch.condition,
                Expression::VarRef { name, .. }
                    if matches!(roles.get(name.var_name()), Some(PlannedRole::Clock))
            );
            validate_when_equations(
                &branch.equations,
                roles,
                states,
                constants,
                sample_lattices,
                &mut reinit_state_pre,
                clocked,
            )?;
        }
    }
    Ok(reinit_state_pre)
}

fn validate_when_equations(
    equations: &[flat::WhenEquation],
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
    reinit_state_pre: &mut HashSet<Span>,
    clocked: bool,
) -> Result<(), ToDaeError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign {
                target,
                value,
                span,
                ..
            } => validate_assignment(target, value, *span, roles, states, clocked)?,
            flat::WhenEquation::Reinit {
                state, value, span, ..
            } => validate_reinitialization(state, value, *span, roles, states, reinit_state_pre)?,
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                validate_condition_expression(
                    condition,
                    roles,
                    states,
                    constants,
                    sample_lattices,
                )?;
                validate_expression(message, roles, states)?;
                if let Some(level) = level {
                    validate_expression(level, roles, states)?;
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                validate_expression(message, roles, states)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                span,
                ..
            } => {
                validate_non_real_branch_targets(branches, else_branch, roles, *span)?;
                for (condition, equations) in branches {
                    validate_condition_expression(
                        condition,
                        roles,
                        states,
                        constants,
                        sample_lattices,
                    )?;
                    validate_when_equations(
                        equations,
                        roles,
                        states,
                        constants,
                        sample_lattices,
                        reinit_state_pre,
                        clocked,
                    )?;
                }
                if let Some(else_branch) = else_branch {
                    validate_when_equations(
                        else_branch,
                        roles,
                        states,
                        constants,
                        sample_lattices,
                        reinit_state_pre,
                        clocked,
                    )?;
                }
            }
            flat::WhenEquation::FunctionCallOutputs { span, .. } => {
                return Err(ToDaeError::unsupported_flat(
                    "when function-call outputs",
                    "multi-output event calls require a checked function-action owner",
                    *span,
                ));
            }
        }
    }
    Ok(())
}

fn validate_non_real_branch_targets(
    branches: &[(Expression, Vec<flat::WhenEquation>)],
    else_branch: &Option<Vec<flat::WhenEquation>>,
    roles: &HashMap<VarName, PlannedRole>,
    span: Span,
) -> Result<(), ToDaeError> {
    let expected = branches
        .first()
        .map(|(_, equations)| non_real_targets(equations, roles))
        .unwrap_or_default();
    if branches
        .iter()
        .skip(1)
        .map(|(_, equations)| non_real_targets(equations, roles))
        .chain(
            else_branch
                .iter()
                .map(|equations| non_real_targets(equations, roles)),
        )
        .all(|targets| targets == expected)
    {
        return Ok(());
    }
    Err(ToDaeError::discrete_solved_form_violation(
        "all branches of a non-Real if-equation must assign the same resolved coordinates",
        span,
    ))
}

fn non_real_targets(
    equations: &[flat::WhenEquation],
    roles: &HashMap<VarName, PlannedRole>,
) -> HashSet<VarName> {
    let mut targets = HashSet::new();
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { target, .. }
                if matches!(roles.get(target), Some(PlannedRole::DiscreteValue)) =>
            {
                targets.insert(target.clone());
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_, equations) in branches {
                    targets.extend(non_real_targets(equations, roles));
                }
                if let Some(else_branch) = else_branch {
                    targets.extend(non_real_targets(else_branch, roles));
                }
            }
            flat::WhenEquation::Assign { .. }
            | flat::WhenEquation::Reinit { .. }
            | flat::WhenEquation::Assert { .. }
            | flat::WhenEquation::Terminate { .. }
            | flat::WhenEquation::FunctionCallOutputs { .. } => {}
        }
    }
    targets
}

fn validate_assignment(
    target: &VarName,
    value: &Expression,
    span: Span,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    clocked: bool,
) -> Result<(), ToDaeError> {
    if !matches!(
        roles.get(target),
        Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
    ) {
        return Err(ToDaeError::unsupported_flat(
            "when assignment",
            format!("`{target}` is not a discrete coordinate"),
            span,
        ));
    }
    validate_clocked_temporal_expressions(value, roles, clocked)?;
    validate_expression(value, roles, states)
}

fn validate_clocked_temporal_expressions(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    clocked: bool,
) -> Result<(), ToDaeError> {
    if let Expression::BuiltinCall {
        function: BuiltinFunction::Previous,
        args,
        span,
    } = expression
    {
        if !clocked {
            return Err(ToDaeError::unsupported_runtime_operator(
                "previous",
                "requires an owning clocked when-clause",
                *span,
            ));
        }
        let [argument] = args.as_slice() else {
            return Err(ToDaeError::unsupported_runtime_operator(
                "previous",
                "requires exactly one coordinate operand",
                *span,
            ));
        };
        let Some((coordinate, _)) = derivative_reference(argument) else {
            return Err(ToDaeError::unsupported_runtime_operator(
                "previous",
                "requires a direct component expression",
                *span,
            ));
        };
        if !matches!(
            roles.get(coordinate.var_name()),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        ) {
            return Err(ToDaeError::unsupported_runtime_operator(
                "previous",
                "requires a clocked discrete coordinate",
                *span,
            ));
        }
        return Ok(());
    }
    for child in expression_children(expression) {
        validate_clocked_temporal_expressions(child, roles, clocked)?;
    }
    Ok(())
}

fn validate_reinitialization(
    state: &VarName,
    value: &Expression,
    span: Span,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    reinit_state_pre: &mut HashSet<Span>,
) -> Result<(), ToDaeError> {
    if !matches!(roles.get(state), Some(PlannedRole::State)) {
        return Err(ToDaeError::reinit_non_state(state.as_str(), span));
    }
    collect_state_pre(value, roles, reinit_state_pre);
    let validation = StatePreEraser {
        spans: reinit_state_pre,
    }
    .rewrite_expression(value);
    validate_expression(&validation, roles, states)
}

fn collect_state_pre(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    reinit_state_pre: &mut HashSet<Span>,
) {
    if let Expression::BuiltinCall {
        function: BuiltinFunction::Pre,
        args,
        span,
    } = expression
        && let [argument] = args.as_slice()
        && let Some((name, _)) = derivative_reference(argument)
        && matches!(roles.get(name.var_name()), Some(PlannedRole::State))
    {
        reinit_state_pre.insert(*span);
        return;
    }
    for child in expression_children(expression) {
        collect_state_pre(child, roles, reinit_state_pre);
    }
}

struct StatePreEraser<'spans> {
    spans: &'spans HashSet<Span>,
}

impl ExpressionRewriter for StatePreEraser<'_> {
    fn rewrite_expression(&mut self, expression: &Expression) -> Expression {
        if let Expression::BuiltinCall {
            function: BuiltinFunction::Pre,
            args,
            span,
        } = expression
            && self.spans.contains(span)
        {
            return args[0].clone();
        }
        self.walk_expression(expression)
    }
}
