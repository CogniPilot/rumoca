use super::*;
use rumoca_core::ExpressionRewriter;

type DefinitionMap = flat::VarNameIndexMap<Span>;

pub(super) fn validate_when_chains(
    chains: &[flat::WhenChain],
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
) -> Result<HashSet<Span>, ToDaeError> {
    validate_unique_when_owners(chains)?;
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

fn validate_unique_when_owners(chains: &[flat::WhenChain]) -> Result<(), ToDaeError> {
    let mut owners = DefinitionMap::default();
    for chain in chains {
        require_span(chain.span(), "when chain")?;
        let mut chain_definitions = DefinitionMap::default();
        for branch in chain.branches() {
            require_span(branch.span, "when branch")?;
            merge_alternative_definitions(
                &mut chain_definitions,
                summarize_when_definitions(&branch.equations)?,
            );
        }
        for (target, _) in chain_definitions {
            insert_when_owner(&mut owners, target, chain.span())?;
        }
    }
    Ok(())
}

fn summarize_when_definitions(
    equations: &[flat::WhenEquation],
) -> Result<DefinitionMap, ToDaeError> {
    let mut definitions = DefinitionMap::default();
    for equation in equations {
        require_span(equation.span(), "when equation")?;
        match equation {
            flat::WhenEquation::Assign { target, span, .. } => {
                insert_when_definition(&mut definitions, target.clone(), *span)?;
            }
            flat::WhenEquation::Reinit { state, span, .. } => {
                insert_when_definition(&mut definitions, state.clone(), *span)?;
            }
            flat::WhenEquation::FunctionCallOutputs { outputs, span, .. } => {
                for output in outputs {
                    insert_when_definition(&mut definitions, output.clone(), *span)?;
                }
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                let mut alternatives = DefinitionMap::default();
                for (_, branch) in branches {
                    merge_alternative_definitions(
                        &mut alternatives,
                        summarize_when_definitions(branch)?,
                    );
                }
                if let Some(else_branch) = else_branch {
                    merge_alternative_definitions(
                        &mut alternatives,
                        summarize_when_definitions(else_branch)?,
                    );
                }
                for (target, span) in alternatives {
                    insert_when_definition(&mut definitions, target, span)?;
                }
            }
            flat::WhenEquation::Assert { .. } | flat::WhenEquation::Terminate { .. } => {}
        }
    }
    Ok(definitions)
}

fn merge_alternative_definitions(definitions: &mut DefinitionMap, alternative: DefinitionMap) {
    for (target, span) in alternative {
        definitions.entry(target).or_insert(span);
    }
}

fn insert_when_definition(
    definitions: &mut DefinitionMap,
    target: VarName,
    span: Span,
) -> Result<(), ToDaeError> {
    if definitions.contains_key(&target) {
        return Err(ToDaeError::discrete_solved_form_violation(
            format!("when branch target `{target}` is defined more than once"),
            span,
        ));
    }
    definitions.insert(target, span);
    Ok(())
}

fn insert_when_owner(
    owners: &mut DefinitionMap,
    target: VarName,
    owner_span: Span,
) -> Result<(), ToDaeError> {
    if owners.contains_key(&target) {
        return Err(ToDaeError::discrete_solved_form_violation(
            format!("when target `{target}` is defined by more than one source when owner"),
            owner_span,
        ));
    }
    owners.insert(target, owner_span);
    Ok(())
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
    if let Expression::FunctionCall {
        name, args, span, ..
    } = expression
        && name.as_str() == "previous"
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
