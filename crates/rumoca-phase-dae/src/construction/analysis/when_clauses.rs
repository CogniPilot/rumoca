use super::*;
use rumoca_core::ExpressionRewriter;

pub(super) fn validate_when_clauses(
    clauses: &[flat::WhenClause],
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
) -> Result<HashSet<Span>, ToDaeError> {
    let mut reinit_state_pre = HashSet::new();
    for clause in clauses {
        require_span(clause.span, "when clause")?;
        validate_condition_expression(
            &clause.condition,
            roles,
            states,
            constants,
            sample_lattices,
        )?;
        validate_when_equations(
            &clause.equations,
            roles,
            states,
            constants,
            sample_lattices,
            &mut reinit_state_pre,
        )?;
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
) -> Result<(), ToDaeError> {
    for equation in equations {
        require_span(equation.span(), "when equation")?;
        match equation {
            flat::WhenEquation::Assign {
                target,
                value,
                span,
                ..
            } => validate_assignment(target, value, *span, roles, states)?,
            flat::WhenEquation::Reinit {
                state, value, span, ..
            } => validate_reinitialization(state, value, *span, roles, states, reinit_state_pre)?,
            flat::WhenEquation::Assert {
                condition, message, ..
            } => {
                validate_condition_expression(
                    condition,
                    roles,
                    states,
                    constants,
                    sample_lattices,
                )?;
                validate_expression(message, roles, states)?;
            }
            flat::WhenEquation::Terminate { message, .. } => {
                validate_expression(message, roles, states)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
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
                    )?;
                }
                validate_when_equations(
                    else_branch,
                    roles,
                    states,
                    constants,
                    sample_lattices,
                    reinit_state_pre,
                )?;
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

fn validate_assignment(
    target: &VarName,
    value: &Expression,
    span: Span,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
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
    validate_expression(value, roles, states)
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
        return Err(ToDaeError::unsupported_flat(
            "reinit",
            format!("`{state}` is not a continuous state"),
            span,
        ));
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
