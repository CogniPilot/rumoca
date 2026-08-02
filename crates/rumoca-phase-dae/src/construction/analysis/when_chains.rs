use super::*;

pub(super) fn validate_when_chains(
    chains: &[flat::WhenChain],
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    for chain in chains {
        for branch in chain.branches() {
            // A when-clause's own activation condition is evaluated to decide
            // whether the event happens, so it is not yet inside the event it
            // guards: `pre()` of a continuous coordinate has no left limit to
            // read there. OMC rejects the shape for the same reason
            // ("Argument 1 of pre must be a discrete expression").
            validate_condition_expression(
                &branch.condition,
                roles,
                states,
                constants,
                sample_lattices,
            )?;
            // MLS §16.5.1: a branch is clocked when it names a `Clock`
            // coordinate or uses the inferred-clock constructor `Clock()`; the
            // clock-domain analysis proves the inferred branch's owner.
            let clocked = matches!(
                &branch.condition,
                Expression::VarRef { name, .. }
                    if matches!(roles.get(name.var_name()), Some(PlannedRole::Clock))
            ) || is_inferred_clock_condition(&branch.condition);
            validate_when_equations(
                &branch.equations,
                roles,
                states,
                constants,
                sample_lattices,
                clocked,
            )?;
        }
    }
    Ok(())
}

fn validate_when_equations(
    equations: &[flat::WhenEquation],
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
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
            } => validate_reinitialization(state, value, *span, roles, states, clocked)?,
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                validate_when_condition_expression(
                    condition,
                    roles,
                    states,
                    constants,
                    sample_lattices,
                    clocked,
                )?;
                validate_when_expression(message, roles, states, clocked)?;
                if let Some(level) = level {
                    validate_when_expression(level, roles, states, clocked)?;
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                validate_when_expression(message, roles, states, clocked)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                span,
                ..
            } => {
                validate_non_real_branch_targets(branches, else_branch, roles, *span)?;
                for (condition, equations) in branches {
                    validate_when_condition_expression(
                        condition,
                        roles,
                        states,
                        constants,
                        sample_lattices,
                        clocked,
                    )?;
                    validate_when_equations(
                        equations,
                        roles,
                        states,
                        constants,
                        sample_lattices,
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
    validate_when_expression(value, roles, states, clocked)
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

/// MLS §8.3.6 `reinit(x, expr)`.
///
/// The value expression is evaluated at the event instant, so `pre(x)` inside
/// it is the ordinary left limit `x(t^pre)` and lowers through the same
/// event-entry lane as every other continuous `pre()` read. It used to be
/// rewritten to a plain `x` here, which read the wrong value once the lane
/// existed and turned `reinit(x, pre(x) + 1)` into the unsolvable
/// `reinit(x, x + 1)`.
fn validate_reinitialization(
    state: &VarName,
    value: &Expression,
    span: Span,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    clocked: bool,
) -> Result<(), ToDaeError> {
    if !matches!(roles.get(state), Some(PlannedRole::State)) {
        return Err(ToDaeError::reinit_non_state(state.as_str(), span));
    }
    validate_when_expression(value, roles, states, clocked)
}
